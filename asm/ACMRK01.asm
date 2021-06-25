*          DATA SET ACMRK01    AT LEVEL 040 AS OF 03/28/14                      
*PHASE T61601A                                                                  
ACMRK01  TITLE 'WIP - HOLD'                                                     
* JFOX 029 YEAR 2000 FIX                                                        
* JFOX 030 REV OPTION TO FILTER FILE READING (U.S. ONLY)                        
ACMRK01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MRK1**,RA                                                    
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         L     R8,AOVERWRK                                                      
         USING OVRWRKD,R8                                                       
         USING SAVED,R7                                                         
         USING TWAD,R6                                                          
*                                  SET INPUT SCREEN DISPS FOR ROOT              
         LH    R1,=Y(INPHEDH-TWAD)                                              
         STH   R1,DISPHED          DISPLACEMENT OF INPUT HEADLINE               
         AR    R1,R6                                                            
         ST    R1,ADISHEAD         A(INPUT HEADLINE)                            
         LH    R1,=Y(INPHD2H-TWAD)                                              
         STH   R1,DISPHED2         DISPLACEMENT OF INPUT HEADLINE               
         AR    R1,R6                                                            
         ST    R1,ADISHEA2         A(INPUT HEADLINE)                            
         LH    R1,=Y(INPDETH-TWAD)                                              
         STH   R1,DISPDET          DISPLACEMENT OF 1ST DETAIL LINE              
         AR    R1,R6                                                            
         ST    R1,ADISDET1         A(1ST DETAIL LINE)                           
         LH    R1,=Y(INPTOTH-TWAD)                                              
         STH   R1,DISPTOT          DISPLACEMENT OF TOTALS LINE                  
         AR    R1,R6                                                            
         ST    R1,ADISTOTS         A(TOTALS LINE)                               
         LH    R1,=Y(INPPFKH-TWAD)                                              
         STH   R1,DISPPFK          DISPLACEMENT OF PF KEY LINE                  
         AR    R1,R6                                                            
         ST    R1,ADISPFKS         A(PF KEY LINE)                               
                                                                                
                                                                                
         CLI   BYTE,ACTIPRVL                                                    
         BE    PREVAL              PRE-VALIDATE HEADER                          
         TM    TWAMODE2,TWAM2NXA                                                
         BO    NXTACC              SPECIAL ROUTINE TO SET NEXT ACCOUNT          
         CLI   XACTION,ACTUPDT                                                  
         BE    UPDATE              UPDATE                                       
         CLI   XACTION,ACTDRFT                                                  
         BE    UPDATE              DRAFT (UPDATE WITHOUT UPDATE)                
         CLI   XACTION,ACTQUIT                                                  
         BE    QUIT                QUIT                                         
         CLI   XACTION,ACTHOLD                                                  
         BE    *+6                 HOLD                                         
         DC    H'0'                                                             
         CLI   TWASCROV,WHSCR1                                                  
         BE    VALHED              HOLD - VALIDATE HEADER                       
         CLI   TWASCROV,WHSCR2                                                  
         BE    VALINP              HOLD - VALIDATE INPUT                        
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* PRE-VALIDATE HEADER                                                 *         
***********************************************************************         
                                                                                
         USING ACTRECD,R1                                                       
PREVAL   LA    R1,LACCOUNT                                                      
         USING ACTRECD,R1                                                       
         CLC   ACTKCULA,SPACES     TEST LAST TYPE/ACTION ACCOUNT                
         BNH   PREVAL10                                                         
         CLI   LTYPE,TYPWIP        TEST WIP ACTION LAST                         
         BNE   PREVAL10                                                         
*                                                                               
         OI    HLDCLIH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R2,ACTKACT          R2=A(CLIENT CODE)                            
         XR    RF,RF                                                            
         IC    RF,PRODALEN         RF=L'(CLIENT LEVEL)                          
         LR    RE,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   HLDCLI(0),0(R2)     DISPLAY CLIENT CODE                          
*                                                                               
         OI    HLDPROH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R2,1(RF,R2)         R2=A(JOB CODE)                               
         XR    RF,RF                                                            
         IC    RF,PRODBLEN         RF=L'(CLIENT/PRODUCT LEVEL)                  
         SR    RF,RE               RF=L'(PRODUCT CODE)                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   HLDPRO(0),0(R2)                                                  
         LA    R2,1(RF,R2)         DISPLAY PRODUCT CODE                         
*                                                                               
         OI    HLDJOBH+(FVOIND-FVIHDR),FVOXMT                                   
         XR    RF,RF                                                            
         IC    RF,PRODCLEN         RF=L'(CLIENT/PRODUCT/JOB LEVEL)              
         XR    RE,RE                                                            
         IC    RE,PRODBLEN         RE=L'(CLIENT/PRODUCT CODE)                   
         SR    RF,RE               RF=L'(JOB CODE)                              
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   HLDJOB(0),0(R2)     DISPLAY JOB CODE                             
*                                                                               
PREVAL10 XC    LACCOUNT,LACCOUNT                                                
                                                                                
PREVALX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SET NEXT ACCOUNT IN RELEVANT SCREEN FIELD(S)                        *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
NXTACC   OC    TWASKEY,TWASKEY     TEST KEY SAVED IN TWA                        
         BZ    NXTACC2                                                          
         CLI   HLDCLIH+(FVILEN-FVIHDR),0  TEST INPUT TO CLIENT                  
         BNE   *+14                                                             
         XC    TWASKEY,TWASKEY     NO - CLEAR KEY SAVED IN TWA                  
         B     NXTACC4             ALWAYS REBUILD CLI/PRO/JOB ACCOUNT           
         SR    RF,RF                                                            
         IC    RF,PRODCLEN         RF=FULL LENGTH OF JOB                        
         BCTR  RF,0                MINUS ONE FOR EXECUTE                        
         SR    R1,R1                                                            
         IC    R1,PRODALEN         SET INDEX TO PRODUCT/JOB IN TWASKEY          
         CLI   HLDPROH+(FVILEN-FVIHDR),0  TEST INPUT TO PRODUCT                 
         BE    NXTACC0                                                          
         CLI   HLDJOBH+(FVILEN-FVIHDR),0  TEST INPUT TO JOB                     
         BNE   NXTACC2                                                          
         IC    R1,PRODBLEN         SET INDEX TO JOB IN TWASKEY                  
NXTACC0  SR    RF,R1               SET RF TO CLEAR PRODUCT/JOB OR JOB           
         LA    R1,TWASKEY+L'PRODUL(R1)  R1=A(PRODUCT OR JOB) IN TWASKEY         
         EX    RF,*+4                                                           
         XC    0(0,R1),0(R1)       CLEAR PRODUCT/JOB OR JOB IN TWASKEY          
         MVC   ACCOUNT(L'ACTKCPY),COMPANY  REBUILD ACCOUNT FROM TWASKEY         
         MVC   ACCOUNT+L'ACTKCPY(L'ACCOUNT-L'ACTKCPY),TWASKEY                   
         B     NXTACC6                                                          
                                                                                
NXTACC2  OC    ACCOUNT,ACCOUNT     TEST FIRST TIME FOR TWAM2NXA                 
         BNZ   NXTACC6                                                          
NXTACC4  LA    R2,ACCOUNT          YES - REBUILD ACCOUNT                        
         MVC   ACCOUNT,SPACES                                                   
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         OC    TWASKEY,TWASKEY     TEST ACCOUNT SAVED IN TWA                    
         BZ    NXTACC8                                                          
         MVC   ACTKUNT(L'ACTKCULA-1),TWASKEY  RESTORE ACCOUNT                   
                                                                                
NXTACC6  LA    R2,KEY              BUILD KEY FOR IO (VIA GETACC)                
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,ACCOUNT                                                 
         B     NXTACC12                                                         
                                                                                
NXTACC8  LA    R2,KEY              BUILD KEY FOR IO ROUTINE                     
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,ACCOUNT                                                 
NXTACC10 SR    RF,RF               BUMP KEY FOR NEXT JOB/PRO/CLI                
         IC    RF,ACTKACT+L'ACTKACT-1                                           
         LA    RF,1(RF)                                                         
         STC   RF,ACTKACT+L'ACTKACT-1                                           
         GOTO1 AIOEXEC,IOHI+IOACCDIR+IO1Q                                       
         CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BNE   NXTACC16            END OF PRODUCTION LEDGER                     
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AACCELS                                                          
         OC    RECABLEL,RECABLEL   TEST JOB LEVEL RECORD                        
         BZ    NXTACC10            NO - TRY AGAIN                               
         MVC   ACCOUNT,ACTKCULA                                                 
         B     NXTACC14                                                         
                                                                                
NXTACC12 SR    RF,RF               BUMP KEY FOR NEXT JOB/PRO/CLI                
         IC    RF,ACTKCULA+L'ACTKCULA-1                                         
         LA    RF,1(RF)                                                         
         STC   RF,ACTKCULA+L'ACTKCULA-1                                         
         MVI   GETIND,GETIABLQ+GETICCBQ                                         
         GOTO1 AGETACC,0                                                        
         BNE   NXTACC14            NOT FOUND/NOT VALID                          
         CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BE    NXTACC18            ELSE TEST PRODUCTION LEDGER                  
         B     NXTACC16            NO - FINISHED                                
                                                                                
NXTACC14 CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BNE   NXTACC16            PAST PRODUCTION LEDGER                       
         MVI   GETIND,GETIABLQ+GETICCBQ  RE-/READ ACCOUNT                       
         GOTO1 AGETACC,0                                                        
         BE    NXTACC18            VALID THIS TIME                              
         B     NXTACC12            STILL NO GOOD - TRY NEXT                     
                                                                                
NXTACC16 MVC   FVMSGNO,=AL2(EANOJOBS)                                           
         MVC   FVXTRA,SPACES                                                    
         LA    R1,HLDCLIH                                                       
         B     NXTACCX                                                          
                                                                                
NXTACC18 L     RF,RECALDGT                                                      
         USING LEDGTABD,RF                                                      
         GOTO1 VACSRCHC,DMCB,HLDJOBH,TWAD,(LEDGTLVC,LEDGTLVB),         X        
               (X'C0',JOBNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         DROP  RF                                                               
         MVC   TWASKEY,ACTKCULA+(ACTKUNT-ACTKEY)                                
         MVC   ACCOUNT,ACTKCULA                                                 
         MVC   ACTKEY,SPACES       RESET KEY                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         SR    R3,R3                                                            
         IC    R3,PRODALEN         READ CLIENT RECORD FOR NAME                  
NXTACC20 BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   ACTKACT(0),ACCOUNT+(ACTKACT-ACTKEY)                              
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AACCELS                                                          
         CLM   R3,1,PRODALEN       TEST CLIENT READ                             
         BNL   NXTACC22                                                         
         L     RF,RECALDGT                                                      
         USING LEDGTABD,RF                                                      
         GOTO1 VACSRCHC,DMCB,HLDCLIH,TWAD,(LEDGTLVA,0),                X        
               (X'C0',CLINDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         DROP  RF                                                               
         IC    R3,PRODBLEN         AND READ FOR PRODUCT NAME                    
         B     NXTACC20                                                         
                                                                                
NXTACC22 L     RF,RECALDGT                                                      
         USING LEDGTABD,RF                                                      
         GOTO1 VACSRCHC,DMCB,HLDPROH,TWAD,(LEDGTLVB,LEDGTLVA),         X        
               (X'C0',PRONDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         DROP  RF                                                               
         MVC   FVMSGNO,=AL2(IAEPJP1N)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,HLDJOBH                                                       
                                                                                
NXTACCX  ST    R1,FVADDR                                                        
         NI    TWAMODE2,255-TWAM2NXA                                            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER SCREEN FIELDS                                       *         
***********************************************************************         
                                                                                
VALHED   DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CLIENT                                                     *         
***********************************************************************         
                                                                                
VALCLI   TM    HLDCLIH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCLIX                                                          
         MVI   FVMINL,1                                                         
         GOTO1 AVALCLI,HLDCLIH                                                  
         BNE   EXIT                EXIT ON CLIENT ERROR                         
                                                                                
VALCLI2  MVC   TWASKEY,ACCOUNT+(ACTKUNT-ACTKEY)  SAVE CLIENT                    
         L     RF,RECALDGT                                                      
         USING LEDGTABD,RF                                                      
         GOTO1 VACSRCHC,DMCB,HLDCLIH,TWAD,(LEDGTLVA,0),                X        
               (X'C0',CLINDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         DROP  RF                                                               
VALCLIX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE PRODUCT                                                    *         
***********************************************************************         
                                                                                
VALPRO   TM    HLDPROH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALPROX                                                          
         MVI   FVMINL,1                                                         
         GOTO1 AVALPRO,HLDPROH                                                  
         BNE   EXIT                EXIT ON PRODUCT ERROR                        
                                                                                
VALPRO2  MVC   TWASKEY,ACCOUNT+(ACTKUNT-ACTKEY)  SAVE CLIENT/PRODUCT            
         L     RF,RECALDGT                                                      
         USING LEDGTABD,RF                                                      
         GOTO1 VACSRCHC,DMCB,HLDPROH,TWAD,(LEDGTLVB,LEDGTLVA),         X        
               (X'C0',PRONDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         DROP  RF                                                               
VALPROX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE JOB                                                        *         
***********************************************************************         
                                                                                
VALJOB   TM    HLDJOBH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALJOBX                                                          
         MVC   ACCNAME,SPACES                                                   
         MVI   FVMINL,1                                                         
         MVI   GETIND,GETICCBQ     GETACC WILL CHECK CLIENT BILLING             
         GOTO1 AVALJOB,HLDJOBH                                                  
         BH    EXIT                                                             
         L     RF,RECALDGT                                                      
         USING LEDGTABD,RF                                                      
         GOTO1 VACSRCHC,DMCB,HLDJOBH,TWAD,(LEDGTLVC,LEDGTLVB),         X        
               (X'C0',JOBNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         DROP  RF                                                               
         MVC   ACCNAME,RECNAME     SAVE NAME FOR LATER                          
         ZAP   ACCBAL,RECBAL       SAVE BALANCE FOR LATER                       
         MVC   TWASKEY,ACCOUNT+(ACTKUNT-ACTKEY)  SAVE CLIENT/PRO/JOB            
VALJOBX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE WORK CODE                                                  *         
***********************************************************************         
                                                                                
VALWRK   TM    HLDWRKH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALWRKX                                                          
         XC    HLDWRKN,HLDWRKN                                                  
         OI    HLDWRKNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALWRK,HLDWRKH                                                  
         BL    VALWRKX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   HLDWRKN,RECNAME                                                  
         OI    HLDWRKNH+(FVOIND-FVIHDR),FVOXMT                                  
VALWRKX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CONTRA ACCOUNT                                             *         
***********************************************************************         
                                                                                
VALCON   TM    HLDCONH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCONX                                                          
         XC    HLDCONN,HLDCONN                                                  
         OI    HLDCONNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALCON,HLDCONH                                                  
         BL    VALCONX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   HLDCONN,RECNAME                                                  
         OI    HLDCONNH+(FVOIND-FVIHDR),FVOXMT                                  
VALCONX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE REFERENCE NUMBER RANGE                                     *         
***********************************************************************         
                                                                                
VALREF   TM    HLDREFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALREFX                                                          
         GOTO1 AVALREF,HLDREFH                                                  
         BH    EXIT                                                             
VALREFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE TRANSACTION DATE PERIOD                                    *         
***********************************************************************         
                                                                                
VALPER   TM    HLDPERH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALPERX                                                          
         GOTO1 AVALPER,HLDPERH                                                  
         BH    EXIT                                                             
VALPERX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE ACTIVITY DATE PERIOD                                       *         
***********************************************************************         
                                                                                
VALADA   TM    HLDADAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALADAX                                                          
         GOTO1 AVALADA,HLDADAH                                                  
         BH    EXIT                                                             
VALADAX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE MOS RANGE                                                  *         
***********************************************************************         
                                                                                
VALMOS   TM    HLDMOAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALMOSX                                                          
         GOTO1 AVALMOS,HLDMOAH                                                  
         BH    EXIT                                                             
VALMOSX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE INCLUDE HELD                                               *         
***********************************************************************         
                                                                                
VALHLD   TM    HLDHLDH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALHLDX                                                          
         GOTO1 AVALICL,HLDHLDH                                                  
         BH    EXIT                                                             
VALHLDX  DS    0H                                                               
                                                                                
         B     READTRN             READ AND FILTER TRANSACTIONS                 
         EJECT                                                                  
***********************************************************************         
* READ AND FILTER TRANSACTIONS.  PUT QUALIFYING TRANSACTIONS TO TSAR  *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
READTRN  OI    DISIND,DISIOFLO     SET OVERFLOW (WHICH IS ALLOWED)              
                                                                                
         LA    R1,TOTALS           CLEAR TOTALS ACCUMULATORS                    
         LA    R0,TOTALSN                                                       
         ZAP   0(L'TOTALS,R1),PZERO                                             
         LA    R1,L'TOTALS(R1)                                                  
         BCT   R0,*-10                                                          
                                                                                
         LA    R2,KEY              BUILD START KEY                              
         GOTO1 SETKEY,SETALL                                                    
         MVI   TSARLEN+1,TSARWHL   SET TSAR RECORD LENGTH                       
         LA    R1,IOHIGH+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BNE   READTRNX                                                         
         B     READ04                                                           
READ02   LA    R2,KEY              R2=A(KEY)                                    
         LA    R1,IOSEQ+IOACCDIR+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BNE   READTRNX                                                         
READ04   TM    TRNKSTAT,TRNSDELT+TRNSDRFT                                       
         BNZ   READ02                                                           
*&&US                                                                           
         CLI   OPTREV,INCLUDE      DDS-ONLY - INCLUDE REVERSALS?                
         BE    *+12                                                             
*&&                                                                             
         TM    TRNKSTAT,TRNSREVS                                                
         BNZ   READ02                                                           
         TM    TRNKSTA2,TRNSUSED+TRNSPEEL                                       
         BNZ   READ02                                                           
         CLC   TRNKCULA,ACCOUNT                                                 
         BNE   READTRNX                                                         
         OC    WORKCODE,WORKCODE                                                
         BZ    *+14                                                             
         CLC   TRNKWORK,WORKCODE                                                
         BNE   READTRNX                                                         
         CLC   TRNKWORK,ORDER      NOT PRODUCTION ORDERS                        
         BE    READ02                                                           
         OC    CONTRA,CONTRA                                                    
         BZ    READ06                                                           
         IC    RF,CONTRAXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKCULC(0),CONTRA                                               
         BE    READ06                                                           
         OC    WORKCODE,WORKCODE                                                
         BNZ   READTRNX                                                         
         GOTO1 SETKEY,SETCON+NXTWRK                                             
         B     READ02                                                           
READ06   CLC   TRNKDATE,PERSTA                                                  
         BNL   READ08                                                           
         GOTO1 SETKEY,SETSDT                                                    
         B     READ02                                                           
READ08   CLC   TRNKDATE,PEREND                                                  
         BNH   READ12                                                           
         TM    CONTIND,CONTILOQ    TEST LOW LEVEL CONTRA FILTER                 
         BNZ   READ10                                                           
         GOTO1 SETKEY,SETSDT+NXTCON                                             
         B     READ02                                                           
READ10   OC    WORKCODE,WORKCODE                                                
         BNZ   READTRNX                                                         
         GOTO1 SETKEY,SETSDT+NXTWRK                                             
         B     READ02                                                           
READ12   OC    REFSTA,REFSTA                                                    
         BZ    READ14                                                           
         IC    RF,REFSTAXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKREF(0),REFSTA                                                
         BE    READ14                                                           
         BL    *+14                                                             
         OC    REFEND,REFEND       TEST END REFERENCE PRESENT                   
         BNZ   READ16                                                           
         GOTO1 SETKEY,SETREF                                                    
         B     READ02                                                           
READ14   OC    REFEND,REFEND                                                    
         BZ    READ18                                                           
READ16   IC    RF,REFENDXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKREF(0),REFEND                                                
         BNH   READ18                                                           
         GOTO1 SETKEY,SETREF+NXTSDT                                             
         B     READ02                                                           
READ18   CLC   TRNKSMOS,MOSSTA                                                  
         BL    READ02                                                           
         CLC   TRNKSMOS,MOSEND                                                  
         BH    READ02                                                           
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         TM    TRNKSTAT,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETELAD,AIOBUFF                                                 
         BAS   RE,RFILTER          OVERLAY SPECIFIC FILTERING                   
         BNE   READ02                                                           
                                                                                
         L     R2,AIOSAVE          R2=A(SAVED DIRECTORY VALUES)                 
         MVC   TSARDADR,0(R2)      EXTRACT DATA RECORD DISK ADDRESS             
         L     R2,AIOBUFF          R2=A(DATA RECORD)                            
         MVC   TSAROFF,TRNKWORK                                                 
         MVC   TSARCON,TRNKCULC                                                 
         MVC   TSARDAT,TRNKDATE                                                 
         MVC   TSARREF,TRNKREF                                                  
         MVC   TSARSBR,TRNKSBR                                                  
         MVC   TSARMOS,TRNRSMOS                                                 
         MVI   TSARRSTA,0                                                       
         TM    TRNRSTAT,TRNSARCH                                                
         BZ    *+8                                                              
         OI    TSARRSTA,TRNSARCH   SET RECORD ON ARCHIVE                        
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        R2=A(TRNEL)                                  
         MVC   TSARBAT,TRNBTCH                                                  
         MVC   TSARBTY,TRNTYPE                                                  
         MVC   TSARSTA,TRNSTAT                                                  
         XC    TSARVAR,TSARVAR     CLEAR VARIABLE KEY BYTE                      
         OI    TSARINDS,TSARDRQ                                                 
         TM    TRNSTAT,TRNSHOLD                                                 
         BNO   *+12                                                             
         MVI   TSARVAR,TRNSHOLD    SET VARIABLE KEY BYTE                        
         OI    TSARINDS,TSARMKQ+TSARINMQ                                        
         ZAP   TSARAMNT,TRNAMNT                                                 
         USING OTHELD,R2                                                        
         ICM   R2,15,AOTHEL                                                     
         BZ    *+10                                                             
         MVC   TSARFOTH,OTHNUM                                                  
         USING TRSELD,R2                                                        
         ICM   R2,15,ATRSEL                                                     
         MVC   TSARADAT,TRSDATE                                                 
         GOTO1 ATSARADD                                                         
         BNE   READ20                                                           
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        R2=A(TRNEL)                                  
         AP    TOTDRS,TRNAMNT                                                   
         LA    RF,TOTBAL           RF=A(BALANCE TOTAL)                          
         TM    TRNSTAT,TRNSHOLD                                                 
         BNO   *+8                                                              
         LA    RF,TOTMRK           RF=A(HELD TOTAL)                             
         AP    0(L'TOTALS,RF),TRNAMNT                                           
         MVI   ANYADD,1            SET TRANSACTION(S) ADDED                     
         B     READ02              READ SEQUENTIAL                              
                                                                                
READ20   TM    DISIND,DISIOFLO     TEST ERROR WAS OVERFLOW                      
         BNO   EXIT                NO - EXIT WITH ROOT ERROR SET                
         B     DISPTRN             QUIT READ LEAVING OVERFLOW SET               
                                                                                
READTRNX NI    DISIND,255-DISIOFLO  RESET OVERFLOW (DID NOT OCCUR)              
         CLI   ANYADD,1            TEST ANYTHING ADDED                          
         BE    DISPTRN                                                          
         LA    R1,HLDCLIH          NO - SET CURSOR TO CLIENT FIELD              
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IANOTRAN)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
DISPTRN  GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(1,0),MRKOLAYH               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TWAMODE,TWAMHDRS    INDICATE WE HAVE A HEADER SAVED              
         GOTO1 AOVRSCR,WHSCR2      OVERLAY ACTION INPUT SCREEN                  
         OI    MRKTYPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    MRKACTH+(FVATRB-FVIHDR),FVAPROT                                  
                                                                                
         GOTO1 ABLDBAL,INPACCH     BUILD ACCOUNT CODE/NAME/BALANCE LINE         
                                                                                
         OI    DISIND,DISIRST      SET START FROM BEGINNING                     
         GOTO1 ADISPLAY                                                         
         TM    DISIND,DISIOFLO     TEST BUFER OVERFLOW                          
         BNO   *+10                                                             
         MVC   FVMSGNO,=AL2(IATRNWRN)  WARN USER                                
         GOTO1 ABLDTOT,INPTOTH                                                  
                                                                                
DISPTRNX B     EXIT                                                             
         EJECT                                                                  
         USING DISLINED,R2                                                      
VALINP   CLI   OPTALL,0            TEST GLOBAL MARK/UNMARK                      
         BE    VALINP2                                                          
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BO    VALINP14            YES - CALL DISPLAY                           
         BAS   RE,MRKALL           MARK ALL TRANSACTIONS                        
         OI    TWAMODE2,TWAM2SKP   SET SKIP VALIDATION                          
         OI    DISIND,DISIRST      SET TO RESTART DISPLAY                       
         B     VALINP14                                                         
                                                                                
VALINP2  MVI   ANYMARK,0                                                        
         LA    R3,DISLIST          R3=A(LIST OF TSAR RECDS ON DISPLAY)          
         L     R2,ADISDET1         R2=A(1ST DETAIL LINE)                        
         SR    R0,R0                                                            
         ICM   R0,3,DISLCNT        NUMBER OF DISPLAY LINES                      
         BZ    VALINP14            NO RECORDS TO DISPLAY                        
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BZ    VALINP4                                                          
         NI    TWAMODE2,255-TWAM2SKP  RESET SKIP VALIDATION                     
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BNE   VALINP14            SKIP VALIDATION AND CALL DISPLAY             
                                                                                
VALINP4  GOTO1 AVALMRK,DISLHDR2                                                 
         BH    VALINPX             EXIT WITH ERROR SET                          
         BL    VALINP12            NO INPUT - NEXT SCREEN LINE                  
         MVC   HALF,0(R3)                                                       
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   BYTE,TSARCHGQ       TEST ONLY WANT TO RE-DISPLAY                 
         BE    VALINP10                                                         
         CLI   BYTE,TSARMKQ        TEST IF USER IS MARKING                      
         BNE   VALINP6                                                          
         TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BO    VALINP12                                                         
         OI    TSARINDS,TSARMKQ                                                 
         AP    TOTMRK,TSARAMNT     ADD TO MARKED                                
         SP    TOTBAL,TSARAMNT     SUBTRACT FROM BALANCE                        
         TM    TSARINDS,TSARINMQ   IS TRANS MARKED ON FILE?                     
         BO    *+14                                                             
         AP    REPMRK,TSARAMNT     NO, ADD TO MARKED THIS SESSION               
         B     VALINP8                                                          
         SP    REPUMK,TSARAMNT     YES, MUST HAVE BEEN ADDED TO UNMARKD         
         B     VALINP8             THIS SESSION, SO SUBTRACT IT                 
                                                                                
VALINP6  TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BZ    VALINP12                                                         
         NI    TSARINDS,255-TSARMKQ                                             
         SP    TOTMRK,TSARAMNT     SUBTRACT FROM MARKED                         
         AP    TOTBAL,TSARAMNT     ADD TO BALANCE                               
         TM    TSARINDS,TSARINMQ   IS TRANS MARKED ON FILE?                     
         BO    *+14                                                             
         SP    REPMRK,TSARAMNT     NO, MUST HAVE BEEN ADDED TO MARKED           
         B     VALINP8             THIS SESSION, SO SUBTRACT IT                 
         AP    REPUMK,TSARAMNT     YES, ADD TO UNMARKED THIS SESSION            
         B     VALINP8                                                          
                                                                                
VALINP8  L     RF,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ANYMARK,1                                                        
         DROP  RF                                                               
VALINP10 GOTO1 ABLDLIN,DISLHDR1    REBUILD LHS, TRANSMIT, (UN)HIGHLIGHT         
                                                                                
VALINP12 LA    R2,DISLINEL(R2)     R2=A(NEXT INPUT LINE)                        
         LA    R3,L'DISLIST(R3)    R3=A(NEXT TSAR RECORD NUMBER)                
         BCT   R0,VALINP4                                                       
                                                                                
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   *+16                                                             
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BE    *+8                                                              
         OI    TWAMODE2,TWAM2SKP   YES - SET SKIP VALIDATION                    
         TM    DISIND,DISINCOL     TEST NEW COLUMN DISPLAY                      
         BNZ   VALINP14            DISPLAY NEW COLUMNS                          
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   VALINP14                                                         
         OI    DISIND,DISIFFLT     FORCE FILTERING OF DISLIST NEXT TIME         
         LA    R1,MRKSCRH          SET CURSOR TO SCROLL FIELD                   
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAMKTEPA)                                           
         TM    DISIND,DISIEOF+DISIBOF                                           
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IAMKTNOM)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         B     VALINPX                                                          
                                                                                
VALINP14 GOTO1 ADISPLAY                                                         
                                                                                
VALINPX  GOTO1 ABLDTOT,INPTOTH                                                  
         CLI   ANYMARK,1                                                        
         BNE   *+8                                                              
         OI    TWAMODE2,TWAM2CHG   SET CHANGES MADE BIT                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE TRANSACTIONS                                                 *         
*                                                                     *         
***********************************************************************         
                                                                                
         USING TSARD,RF                                                         
         USING REPD,R3                                                          
UPDATE   TM    TWAMODE2,TWAM2CHG   TEST CHANGES MADE                            
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(EANOTHIN)  NOTHING DONE YET                         
         B     UPDATEX                                                          
         OC    PRTSUB,PRTSUB                                                    
         BZ    UPD02                                                            
         L     R3,AREPWRK          R3=A(REPORT W/S)                             
         GOTO1 APRTINI             INITIALISE AND PRINT FRONT PAGE              
         MVC   REPH5+L'DISLLINE+1(L'LC@HELD),LC@HELD                            
         LA    R1,REPH5+L'DISLLINE+1+L'LC@HELD-1                                
         CLI   0(R1),C' '          SEEK FIRST NON-BLANK                         
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'?'                                                       
                                                                                
UPD02    LA    R1,INPMRKH                                                       
         ST    R1,FVADDR                                                        
         LA    R1,1                                                             
         STCM  R1,3,TEMP                                                        
                                                                                
UPD04    GOTO1 ATSARGET,TEMP                                                    
         BE    UPD06                                                            
         L     RF,ATSARBLK                                                      
         TM    TSERRS,TSEEOF                                                    
         BO    UPD28                                                            
         DC    H'0'                                                             
                                                                                
UPD06    TM    TSARINDS,TSARMKQ    TEST IF USER IS MARKING                      
         BZ    UPD08                                                            
         TM    TSARSTA,TRNSHOLD    TEST ALREADY HELD                            
         BO    UPD26                                                            
         MVI   NEWSTA,TRNSHOLD                                                  
         B     UPD10                                                            
                                                                                
UPD08    TM    TSARSTA,TRNSHOLD    TEST ALREADY HELD                            
         BZ    UPD26                                                            
         MVI   NEWSTA,0                                                         
                                                                                
UPD10    OC    PRTSUB,PRTSUB       PRINT REPORT IF REQUIRED                     
         BZ    UPD12               MUST BE LIVE IF NO REPORT                    
         LA    R1,REPP1-1                                                       
         ICM   R1,8,=C'R'                                                       
         GOTO1 ABLDLIN             BUILD PRINT LINE USING REPDISP               
         MVC   REPP1+L'DISLLINE+1(L'LC4YES),LC4YES                              
         TM    TSARINDS,TSARMKQ    WAS IT HELD OR UNHELD                        
         BO    *+10                                                             
         MVC   REPP1+L'DISLLINE+1(L'LC4NO),LC4NO                                
         GOTO1 VREPORT,REPD        PRINT IT                                     
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    UPD26               YES - GET NEXT TSAR RECORD                   
                                                                                
UPD12    MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGETRUP+IOACCMST+IO1Q                                        
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETELAD,AIOBUFF                                                 
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRNSTAT,TSARSTA     TEST SOMEONE AMENDING ELSEWHERE              
         BE    UPD14                                                            
         NI    TWAMODE2,255-TWAM2CHG  RESET CHANGES MADE BIT                    
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         MVC   FVMSGNO,=AL2(IAUPDCUT)  UPDATE CUT SHORT                         
         OC    PRTSUB,PRTSUB                                                    
         BZ    UPDATEX                                                          
                                                                                
         LA    R1,PARM                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,=AL2(RARAEUCS)                                           
         MVI   GTMTYP,GTMREP       REPORT MESSAGE                               
         MVI   GTMAXL,L'REPP1                                                   
         OI    GT1INDS,GT1NOREF+GT1OWRK                                         
         LA    R0,REPP1                                                         
         STCM  R0,7,GTAOUT                                                      
         GOTO1 VGETTXT                                                          
         GOTO1 VREPORT,REPD                                                     
         GOTO1 APRTCLO                                                          
         MVC   FVMSGNO,=AL2(IAUPCUTR)  UPDATE CUT SHORT, REPORT SPOOLED         
         B     UPDATEX                                                          
         DROP  R1                                                               
                                                                                
UPD14    NI    TRNSTAT,255-TRNSHOLD     UNHOLD                                  
         CLI   NEWSTA,0                 TEST UNHOLDING                          
         BE    *+8                                                              
         OI    TRNSTAT,TRNSHOLD         HOLD                                    
         USING TRSELD,R2                                                        
         ICM   R2,15,ATRSEL                                                     
         CLI   TRSLN,TRSLNQ        TEST SHORT ELEMENT                           
         BNL   UPD16                                                            
         GOTO1 AEXTRSL             YES - EXTEND IT                              
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
         ICM   R2,15,ATRSEL                                                     
UPD16    MVI   TRSMARK,TRSMWHQ     SET MARKER TYPE/ACTION IN ELEMENT            
         CLI   NEWSTA,0                                                         
         BNE   *+8                                                              
         OI    TRSMARK,TRSMUMQ     SET ACTION IS NEGATIVE                       
*&&US                                                                           
         L     R2,AIOBUFF                                                       
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         USING GDAELD,R2           SEARCH FOR AND DELETE GDAELS                 
         SR    R0,R0                                                            
UPD18    CLI   GDAEL,0                                                          
         BE    UPD22                                                            
         CLI   GDAEL,GDAELQ                                                     
         BNE   UPD20                                                            
         CLI   GDATYPE,GDATMHLD    TEST HELD BY MARKER                          
         BE    *+12                                                             
         CLI   GDATYPE,GDATAHLD    TEST AUTO-HELD                               
         BNE   UPD20                                                            
         MVI   GDAEL,X'FF'         SET TO DELETE                                
                                                                                
UPD20    IC    R0,GDALN                                                         
         AR    R2,R0                                                            
         B     UPD18                                                            
                                                                                
UPD22    GOTO1 VHELLO,DMCB,(C'D',OVACCMST),(X'FF',AIOBUFF),0                    
         CLI   NEWSTA,TRNSHOLD     TEST HOLDING                                 
         BNE   UPD24                                                            
         LA    R2,OVELEM           ADD A MARKER GDAEL                           
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATMHLD                                                 
         MVC   GDADATE,TODAYP                                                   
         GOTO1 VHELLO,DMCB,(C'P',OVACCMST),AIOBUFF,GDAEL,=C'ADD=END'            
*&&                                                                             
UPD24    L     R2,AIOBUFF               R2=A(DATA RECORD)                       
         USING TRNRECD,R2                                                       
         LA    R1,IOPUT+IOACCMST+IO1Q   PUT BACK TO ACCMST                      
         TM    TRNRSTAT,TRNSARCH        TEST TRANSACTION ON ACCARC              
         BNO   *+12                                                             
         NI    TRNRSTAT,255-TRNSARCH    CLEAR ACCARC INDICATOR                  
         LA    R1,IOADFR+IOACCMST+IO1Q  RE-ADD ACCARC RECORD TO ACCMST          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    IOCTCOMM,IOADFR          TEST RECORD RE-ADDED TO ACCMST          
         BNO   UPD26                                                            
         MVC   KEY(L'TRNKEY),TRNKEY     YES - AMEND DIRECTORY RECORD            
         LA    R1,IORDUP+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+(TRNKSTA-TRNRECD)(L'TRNRSTA),TRNRSTA                         
         L     R2,AIOSAVE               R2=A(SAVED DATA RECORD VALUES)          
         MVC   KEY+(TRNKDA-TRNRECD)(L'TRNKDA),0(R2)                             
         LA    R1,IOWRITE+IOACCDIR+IO1Q                                         
         GOTO1 AIOEXEC                                                          
         BE    UPD26                                                            
         DC    H'0'                                                             
                                                                                
UPD26    ICM   R1,3,TEMP                                                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,TEMP                                                        
         B     UPD04                                                            
                                                                                
UPD28    OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    UPD30               NO - MUST BE LIVE UPDATE                     
         GOTO1 APRTCLO             CLOSE REPORT, BUILD SPOOL-ID MESSAGE         
         CLI   XACTION,ACTDRFT     TEST REPORT WAS DRAFT                        
         BE    UPDATEX             PRTCLO HAS SET MESSAGE                       
                                                                                
UPD30    NI    TWAMODE2,255-TWAM2CHG  RESET CHANGES MADE BIT                    
         MVC   FVMSGNO,=AL2(IATRNUPS)                                           
         OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IATRNUPR)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         LA    R1,MRKACTH          R1=A(ACTION FIELD)                           
         B     *+8                                                              
                                                                                
UPDATEX  LA    R1,MRKSCRH          R1=A(SCROLL FIELD)                           
         ST    R1,FVADDR           STORE FIELD ADDRESS                          
         XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
         B     EXIT                                                             
         DROP  R2,R3,RF                                                         
         EJECT                                                                  
***********************************************************************         
* QUIT                                                                *         
***********************************************************************         
                                                                                
QUIT     XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
         LA    R1,MRKACTH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAHDRECH)                                           
         NI    TWAMODE2,255-TWAM2CHG                                            
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OVERLAY ROUTINES                                                    *         
***********************************************************************         
                                                                                
***********************************************************************         
* ROUTINE TO HOLD/UNHOLD ALL TRANSACTIONS                             *         
***********************************************************************         
                                                                                
MRKALL   NTR1  ,                                                                
         LA    R1,1                                                             
MRKALL2  STH   R1,HALF                                                          
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TSARINDS,TSARDISQ   TEST DISPLAY ONLY TRANSACTION                
         BO    MRKALL8                                                          
         GOTO1 AFILTER             FILTER TRANSACTION                           
         BNE   MRKALL8             NOT REQUIRED - GET NEXT                      
         CLC   OPTALL,AC@YES       TEST IF USER IS MARKING                      
         BNE   MRKALL4                                                          
         TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BO    MRKALL8                                                          
         OI    TSARINDS,TSARMKQ                                                 
         AP    TOTMRK,TSARAMNT     ADD TO MARKED                                
         SP    TOTBAL,TSARAMNT     SUBTRACT FROM BALANCE                        
         TM    TSARINDS,TSARINMQ   IS TRANS MARKED ON FILE?                     
         BO    *+14                                                             
         AP    REPMRK,TSARAMNT     NO, ADD TO MARKED THIS SESSION               
         B     MRKALL6                                                          
         SP    REPUMK,TSARAMNT     YES, MUST HAVE BEEN ADDED TO UNMARKD         
         B     MRKALL6                                                          
                                                                                
MRKALL4  TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BZ    MRKALL8                                                          
         NI    TSARINDS,255-TSARMKQ                                             
         SP    TOTMRK,TSARAMNT     SUBTRACT FROM MARKED                         
         AP    TOTBAL,TSARAMNT     ADD TO BALANCE                               
         TM    TSARINDS,TSARINMQ   IS TRANS MARKED ON FILE?                     
         BO    *+14                                                             
         SP    REPMRK,TSARAMNT     NO, MUST HAVE BEEN ADDED TO MARKED           
         B     MRKALL6             THIS SESSION, SO SUBTRACT IT                 
         AP    REPUMK,TSARAMNT     YES, ADD TO UNMARKED THIS SESSION            
         B     MRKALL6                                                          
                                                                                
MRKALL6  L     RF,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TWAMODE2,TWAM2CHG   SET CHANGES MADE BIT                         
         B     MRKALL8                                                          
         DROP  RF                                                               
                                                                                
MRKALL8  LH    R1,HALF                                                          
         LA    R1,1(R1)                                                         
         CH    R1,DISMAX                                                        
         BNH   MRKALL2                                                          
                                                                                
MRKALLX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OVERLAY SPECIFIC FILTERING                                          *         
***********************************************************************         
                                                                                
         USING TRNELD,R2                                                        
RFILTER  NTR1  ,                                                                
         ICM   R2,15,ATRNEL                                                     
         BZ    RFILTNEQ                                                         
         GOTO1 ATCLOSE             TEST CLOSED/LIMIT ACCESS USING OFFAL         
         BNE   RFILTERX            EXIT WITH CC NEQ                             
         TM    TRNSTAT,TRNSDR                                                   
         BNO   RFILTNEQ                                                         
         CLI   TRNTYPE,49          TEST BT49 (BILLABLE TIME)                    
         BNE   *+14                                                             
         CP    TRNAMNT,PZERO                                                    
         BE    RFILTNEQ            DROP ZERO POSTINGS FROM BT49                 
         CLI   ICLMARK,ICLMYES     INCLUDE HELD                                 
         BE    RFILT2              CARRY ON                                     
         LA    RF,X'10'            BO IF EXCLUDING HELD                         
         CLI   ICLMARK,ICLMNO                                                   
         BE    *+8                                                              
         LA    RF,X'80'            BZ IF HELD ONLY                              
         TM    TRNSTAT,TRNSHOLD                                                 
         EX    RF,*+4                                                           
         NOP   RFILTNEQ            BO OR BZ                                     
                                                                                
         USING TRSELD,R2                                                        
RFILT2   ICM   R2,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO STATUS ELEMENT                            
         CLC   TRSDATE,ADASTA                                                   
         BL    RFILTNEQ                                                         
         CLC   TRSDATE,ADAEND                                                   
         BH    RFILTNEQ                                                         
                                                                                
RFILTEQU CR    RB,RB                                                            
         B     RFILTERX                                                         
                                                                                
RFILTNEQ LTR   RB,RB                                                            
         B     RFILTERX                                                         
                                                                                
RFILTERX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A KEY FOR TRANSACTION READING                      *         
*                                                                     *         
* NTRY - R1=KEY BUILD MASK (SEE SETXXX EQUATES)                       *         
*        R2=A(TRANSACTION KEY)                                        *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
SETKEY   NTR1  ,                                                                
         STC   R1,WORK                                                          
                                                                                
         TM    WORK,SETACC         SET ACCOUNT                                  
         BZ    SETKEY0                                                          
         MVC   TRNKEY,SPACES                                                    
         OC    ACCOUNT,ACCOUNT                                                  
         BNZ   *+6                                                              
         DC    H'0'                JOB MISSING                                  
         MVC   TRNKCULA,ACCOUNT                                                 
                                                                                
SETKEY0  TM    WORK,SETWRK         SET WORKCODE                                 
         BZ    SETKEY1                                                          
         MVC   TRNKWORK,SPACES                                                  
         MVI   TRNKWORK+L'TRNKWORK-1,X'41'                                      
         OC    WORKCODE,WORKCODE                                                
         BZ    SETKEY1                                                          
         MVC   TRNKWORK,WORKCODE                                                
                                                                                
SETKEY1  TM    WORK,SETCON         SET CONTRA                                   
         BZ    SETKEY2                                                          
         MVC   TRNKCULC,SPACES                                                  
         MVI   TRNKCULC+L'TRNKCULC-1,X'41'                                      
         OC    CONTRA,CONTRA                                                    
         BZ    SETKEY2                                                          
         MVC   TRNKCULC,CONTRA                                                  
                                                                                
SETKEY2  TM    WORK,SETSDT         SET TRANSACTION DATE                         
         BZ    SETKEY3                                                          
         MVC   TRNKDATE,PERSTA                                                  
                                                                                
SETKEY3  TM    WORK,SETREF         SET REFERENCE (BILL NUMBER)                  
         BZ    SETKEY4                                                          
         MVC   TRNKREF,SPACES                                                   
         MVI   TRNKREF+L'TRNKREF-1,X'41'                                        
         OC    REFSTA,REFSTA                                                    
         BZ    SETKEY4                                                          
         MVC   TRNKREF,REFSTA                                                   
                                                                                
SETKEY4  TM    WORK,NXTWRK         BUMP WORKCODE                                
         BZ    SETKEY5                                                          
         IC    RE,TRNKWORK+(L'TRNKWORK-1)                                       
         LA    RE,1(RE)                                                         
         STC   RE,TRNKWORK+(L'TRNKWORK-1)                                       
                                                                                
SETKEY5  TM    WORK,NXTCON         BUMP CONTRA                                  
         BZ    SETKEY6                                                          
         IC    RE,TRNKCACT+(L'TRNKCACT-1)                                       
         LA    RE,1(RE)                                                         
         STC   RE,TRNKCACT+(L'TRNKCACT-1)                                       
                                                                                
SETKEY6  TM    WORK,NXTSDT         BUMP DATE                                    
         BZ    SETKEY7                                                          
         IC    RE,TRNKDATE+(L'TRNKDATE-1)                                       
         LA    RE,1(RE)                                                         
         STC   RE,TRNKDATE+(L'TRNKDATE-1)                                       
                                                                                
SETKEY7  DS    0H                                                               
                                                                                
SETKEYX  MVI   TRNKSBR,0           ALWAYS CLEAR SUB-REFERENCE                   
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL EXIT                                                        *         
***********************************************************************         
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
ORDER    DC    C'**'                                                            
OVACCMST DC    C'ACCMST '                                                       
CLINDSP  EQU   19                                                               
PRONDSP  EQU   19                                                               
JOBNDSP  EQU   19                                                               
         EJECT                                                                  
OVRWRKD  DSECT                                                                  
NEWSTA   DS    XL1                 NEW TRANSACTION STATUS                       
OVELEM   DS    XL255               ELEMENT AREA                                 
                                                                                
                                                                                
* ACMRKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMRKWRK                                                       
         PRINT ON                                                               
SAVED    DSECT                                                                  
         ORG   TOTALS                                                           
TOTDRS   DS    PL8                                                              
TOTMRK   DS    PL8                                                              
TOTBAL   DS    PL8                                                              
REPMRK   DS    PL8                                                              
REPUMK   DS    PL8                                                              
                                                                                
TWAD     DSECT                                                                  
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKF1D                                                       
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKE1D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040ACMRK01   03/28/14'                                      
         END                                                                    
