*          DATA SET ACMRK12    AT LEVEL 043 AS OF 11/09/18                      
*PHASE T61612B                                                                  
*INCLUDE AUTOAPM                                                                
ACMRK12  TITLE 'VENDOR - MATCH'                                                 
**********************************************************************          
* RGUP 042 28JUN18  <SPEC-20692> ADDITIONAL MEDIA FOR DIGIAL AUDIO   *          
**********************************************************************          
ACMRK12  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*MRK12**,RA,RR=RE                                              
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         USING SAVED,R7                                                         
         USING TWAD,R6                                                          
         L     R8,AOVERWRK                                                      
         USING OVRWRKD,R8                                                       
         ST    RE,OVRELO                                                        
*                                  SET INPUT SCREEN DISPS FOR ROOT              
         LHI   R1,GRDDAT1H-TWAD                                                 
         STH   R1,DISPHED          DISPLACEMENT OF INPUT HEADLINE               
         AR    R1,R6                                                            
         ST    R1,ADISHEAD         A(INPUT HEADLINE)                            
         LHI   R1,GRDDAT2H-TWAD                                                 
         STH   R1,DISPHED2         DISPLACEMENT OF INPUT 2ND HEADLINE           
         AR    R1,R6                                                            
         ST    R1,ADISHEA2         A(INPUT 2ND HEADLINE)                        
         LHI   R1,GRDDAT3H-TWAD                                                 
         STH   R1,DISPDET          DISPLACEMENT OF 1ST DETAIL LINE              
         AR    R1,R6                                                            
         ST    R1,ADISDET1         A(1ST DETAIL LINE)                           
         LHI   R1,GRDCONFH-TWAD                                                 
         STH   R1,DISPTOT          DISPLACEMENT OF TOTALS LINE                  
         AR    R1,R6                                                            
         ST    R1,ADISTOTS         A(TOTALS LINE)                               
         LHI   R1,GRDPFAH-TWAD                                                  
         STH   R1,DISPPFK          DISPLACEMENT OF PF KEY LINE                  
         AR    R1,R6                                                            
         ST    R1,ADISPFKS         A(PF KEY LINE)                               
*                                                                               
*NIT02   CLI   BYTE,ACTIPRVL                                                    
*        BE    PREVAL              PRE-VALIDATE HEADER SCREEN                   
*        TM    TWAMODE2,TWAM2NXA                                                
*        BO    NXTACC              SET NEXT ACCOUNT                             
         CLI   XACTION,ACTUPDT                                                  
         BE    UPDATE              UPDATE                                       
         CLI   XACTION,ACTDRFT                                                  
         BE    UPDATE              DRAFT (UPDATE WITHOUT UPDATE)                
         CLI   XACTION,ACTQUIT                                                  
         BE    QUIT                QUIT                                         
         CLI   XACTION,ACTSELC     APPROVE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TWASCROV,VTSCR1                                                  
         BE    VALHED              MATCH - VALIDATE HEADER                      
         CLI   TWASCROV,GRDSCR                                                  
         BE    VALINP              GRID SCREEN - VALIDATE FURTHER               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER SCREEN FIELDS                                                 
***********************************************************************         
VALHED   DS    0H                                                               
*----------------------------------------------------------------------         
* VALIDATE UNIT & LEDGER                                                        
*----------------------------------------------------------------------         
VALLDG   DS    0H                                                               
         GOTO1 APROFILE                                                         
         TM    MATLDGH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALLDGX                                                          
         XC    MATLDGN,MATLDGN                                                  
         OI    MATLDGNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R1,MATLDGH                                                       
         ST    R1,FVADDR           SET A(LEDGER FIELD) FOR FVERR                
*                                                                               
         MVI   FVMINL,1            REQUIRED FIELD                               
         GOTO1 AVALLDG,MATLDGH                                                  
         BH    VALLERR                                                          
*                                                                               
         LA    R1,LDGLIST          R1=A(LIST OF VALID LEDGERS)                  
VALLDG2  CLC   MATLDG,0(R1)                                                     
         BE    VALLDG4             LEDGER IN LIST - VALIDATE IT                 
         CLI   0(R1),EOT                                                        
         BE    *+12                                                             
         LA    R1,L'LDGLIST(R1)                                                 
         B     VALLDG2                                                          
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         MVC   FVXTRA(L'LEDGTUL),KEY+(ACTKUNT-ACTKEY)                           
         B     VALLERR             ERROR - NOT IN VALID LEDGER LIST             
                                                                                
VALLDG4  DS    0H                                                               
         MVC   AUTLDGR,FVIFLD                                                   
         MVC   MATLDGN,RECNAME     NAME EXTRACTED BY GETLDG                     
         B     VALLDGX                                                          
                                                                                
VALLERR  B     EXIT                NO - EXIT TO ROOT WITH MESSAGE SET           
                                                                                
VALLDGX  DS    0H                                                               
*----------------------------------------------------------------------         
* VALIDATE MEDIA OR SYSTEM                                                      
* IT SHOULD BE IN THE FORM MEDIA OR +SYSTEM                                     
*----------------------------------------------------------------------         
VALMED   DS    0H                                                               
         TM    MATMEDH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALMEDX                                                          
         GOTO1 AFLDVAL,MATMEDH                                                  
         LA    R1,MATMEDH                                                       
         ST    R1,FVADDR           SET A(MEDIA SYSTM FIELD) FOR FVERR           
*                                                                               
         USING PROFKEYD,RF                                                      
         XC    WORK,WORK           READ AGENCY LEVEL PROFILE PAGE 2             
         LA    RF,WORK             R1=A(PROFILE KEY)                            
         MVI   PROFKSYS,C'A'       SYSTEM ACCOUNTING                            
         MVC   PROFKPGM,=C'0AA'    PROGRAM CODE                                 
         MVC   PROFKPAG+1(1),COMPANY AGENCY CODE                                
         MVC   PROFKAGY,COMPALFA   ALPHA ID                                     
         GOTO1 VGETPROF,DMCB,PROFKEYD,AUTPROF,VDATAMGR                          
         DROP  RF                                                               
*                                                                               
         MVC   AUTMEDIA,FVIFLD                                                  
         MVC   AUTSYSTM,FVIFLD+1                                                
*                                                                               
         MVC   FVMSGNO,=AL2(EGIFNOTV) INVALID INPUT FIELD                       
         CLI   AUTMEDIA,C'+'          +SYSTEM                                   
         BNE   VALMED10                                                         
         CLI   AUTSYSTM,C' '                                                    
         BNH   VALMERR                                                          
         MVI   AUTMEDIA,C' '          CLEAR MEDIA FIELD                         
         B     VALMEDX                                                          
*                                                                               
VALMED10 CLI   AUTSYSTM,C' '           IS ANYTHING IN SYSTEM                    
         BH    VALMERR                 AUTMEDIA MUST BE '+'                     
         CLI   AUTPRF2,C'S'            RUN BY SYSTEM PROFILE SET?               
         BNE   VALMEDX                                                          
         CLI   AUTMEDIA,C' '                                                    
         BNH   VALMEDX                                                          
*                                                                               
VALMERR  B     EXIT                NO - EXIT TO ROOT WITH MESSAGE SET           
VALMEDX  DS    0H                                                               
***********************************************************************         
*----------------------------------------------------------------------         
* VALIDATE CLIENT                                                               
*----------------------------------------------------------------------         
VALCLT   DS    0H                                                               
         XC    MATCLTN,MATCLTN                                                  
         OI    MATCLTNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R1,MATCLTH                                                       
         ST    R1,FVADDR           SET A(LEDGER FIELD) FOR FVERR                
*                                                                               
         MVI   FVMINL,1            REQUIRED FIELD                               
         GOTO1 AVALCLI,MATCLTH                                                  
         BH    VALCERR                                                          
         MVC   AUTCLNT,FVIFLD                                                   
*                                                                               
         MVC   MATCLTN,RECNAME     CLIENT NAME                                  
         B     VALCLTX                                                          
                                                                                
VALCERR  B     EXIT                NO - EXIT TO ROOT WITH MESSAGE SET           
VALCLTX  DS    0H                                                               
*----------------------------------------------------------------------         
* VALIDATE PRODUCT                                                              
*----------------------------------------------------------------------         
VALPRD   DS    0H                                                               
*                                                                               
         GOTO1 AFLDVAL,MATPRDH                                                  
         MVC   AUTPROD,FVIFLD                                                   
VALEST   MVI   FVNUMER,1           TREAT AS NUMERIC IF FOUND AS NUMERIC         
         GOTO1 AFLDVAL,MATESTH                                                  
         MVC   AUTEST,SPACES                                                    
         CLI   FVILEN,0            ANYTHING INPUTTED?                           
         BE    VALESTX                                                          
         LA    R1,MATPRDH           POINT TO PRODUCT FIELD                      
         CLI   5(R1),0             ANYTHING INPUTTED?                           
         BH    VALEST10                                                         
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EAPRDREQ) PRODUCT REQUIRED WITH ESTIMATE            
         B     EXIT                                                             
VALEST10 MVC   DUB(4),FULL                                                      
         L     R0,DUB                                                           
         CVD   R0,DUB                                                           
         MVC   WORK(17),=X'F0F0F0F0F0F02020202020202020202020'                  
         ED    WORK(17),DUB+2                                                   
         MVC   AUTEST(3),WORK+17-(3)                                            
VALESTX  DS    0H                                                               
***********************************************************************         
* VALIDATE SUPPLIER                                                   *         
***********************************************************************         
         MVC   ACCNAME,SPACES      SAVE NAME FOR LATER                          
         ZAP   ACCBAL,=P'0'        SAVE BALANCE FOR LATER                       
*----------------------------------------------------------------------         
* VALIDATE MOS RANGE                                                            
*----------------------------------------------------------------------         
VALMOS   TM    MATMOSH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALMOSX                                                          
         LA    R1,MATMOSH                                                       
         ST    R1,FVADDR           SET A(LEDGER FIELD) FOR FVERR                
*                                                                               
         MVI   FVMINL,1            REQUIRED FIELD                               
         GOTO1 AVALMOS,MATMOSH                                                  
         BH    EXIT                                                             
VALMOSX  DS    0H                                                               
*----------------------------------------------------------------------         
* VALIDATE INCLUDE SELECTED                                                     
*----------------------------------------------------------------------         
VALSEL   TM    MATSELH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSELX                                                          
         GOTO1 AVALICL,MATSELH                                                  
         BH    EXIT                                                             
VALSELX  DS    0H                                                               
*----------------------------------------------------------------------         
         B     READTRN             READ AND FILTER TRANSACTIONS                 
         EJECT                                                                  
***********************************************************************         
* READ AND FILTER TRANSACTIONS.  PUT QUALIFYING TRANSACTIONS TO TSAR            
***********************************************************************         
         USING TRNRECD,R2                                                       
READTRN  OI    DISIND,DISIOFLO     PRESET OVERFLOW (WHICH IS ALLOWED)           
*                                                                               
         LA    R1,TOTALS           CLEAR TOTALS ACCUMULATORS                    
         LA    R0,TOTALSN                                                       
         ZAP   0(L'TOTALS,R1),PZERO                                             
         LA    R1,L'TOTALS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         MVC   AUTCPY,COMPANY       COMPANY                                     
         MVC   AUTALPHA,COMPALFA    ALPHA ID                                    
         MVC   AUTSMOS(L'MOSSTA),MOSSTA       START MOS                         
         MVI   AUTSMOS+L'MOSSTA,1                                               
         MVC   AUTEMOS(L'MOSEND),MOSEND       END MOS                           
         MVI   AUTEMOS+L'MOSEND,1                                               
         MVI   AUTOPT2,C'Y'         EXPAND EACH TRANS FOR DISK ADDR             
         MVI   AUTOPT6,C'Y'         CALCULATE CASH BY MOS                       
         MVC   AUTCOMF,ACOM                                                     
         MVC   XTSAR,VTSAR                                                      
*                                                                               
         MVC   TSRMODE,TSARMODE                                                 
         MVC   TSRFLDS2,TSARFLD2                                                
         GOTO1 =V(AUTOAPM),AUTBLK,RR=OVRELO                                     
         MVC   TSARMODE,TSRMODE                                                 
         MVC   TSARFLD2,TSRFLDS2                                                
*                                                                               
         CLI   AUTERR,0             DO WE NEED TO LIMIT THE REQUEST             
         BE    READ05                                                           
*                                                                               
         TM    AUTERR,AUTEOF        TSAR2/PAYBL TAB OVERFLOW?                   
         BNO   READ03                                                           
         LA    R1,MATPRDH           POINT TO PRODUCT FIELD                      
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATRNMAX) TSAR2/PAYBL TABL IS FULL ENTR PRD         
         B     EXIT                                                             
*                                                                               
READ03   DS    0H                                                               
         TM    AUTERR,AUTMRNG       MOS RANGE > 12 MONTHS                       
         BNO   READ05                                                           
         LA    R1,MATMOSH           POINT TO MOS RANGE FIELD                    
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EARNGGYR) MOS RANGE IS GREATER THAN ONE YR.         
         B     EXIT                                                             
*                                                                               
READ05   BRAS  RE,REINTSR           RE-INITIALIZE MARKER TSAR DISPLAY           
         LHI   R1,1                                                             
         STCM  R1,3,OVTS2N          OVTS2N - TSAR RECORD NUMBER                 
*                                                                               
READ10   BRAS  RE,GETSAR2           GET AUTOAPP TSAR RECORD                     
         BNE   READTRNX                                                         
*                                                                               
         OC    TSARAAVN,TSARAAVN   ANY VENDOR CODE?                             
         BZ    READ12                                                           
*                                                                               
         USING ACTRECD,R1                                                       
         LA    R1,KEY                                                           
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKULA,TSARAAVN    READ FOR ACCOUNT                             
         GOTO1 AIOEXEC,IOHI+IOACCDIR+IO1Q                                       
         CLC   KEY(ACTKEND),KEYSAVE                                             
         BNE   READ12              NO ACCOUNT FOUND                             
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
*                                                                               
         GOTO1 ASETELAD,AIOBUFF    SET A(ELEMENTS)                              
*                                                                               
         USING NAMELD,R2                                                        
         ICM   R2,15,ANAMEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMEREC+1-NAMELD)                                          
         MVC   TSARVNME(0),NAMEREC                                              
         EX    RF,*-6                                                           
*                                                                               
READ12   MVI   TSARLEN+1,TSARVTL                                                
         OC    TSARDADR,TSARDADR                                                
         BZ    READ50                                                           
*                                                                               
         MVC   IODA,TSARDADR                                                    
*                                                                               
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
         GOTO1 AGENFILT,AIOBUFF    FILTERS                                      
         BNE   READ10                                                           
*                                                                               
         L     R1,AIOSAVE          R1=A(SAVED DIRECTORY VALUES)                 
         MVC   TSARDADR,0(R1)      EXTRACT DATA RECORD DISK ADDRESS             
*                                                                               
         USING TRNRECD,R1          R1=A(DATA RECORD KEY)                        
         L     R1,AIOBUFF          EXTRACT TRNKEY VALUES                        
         MVC   TSARCON,TRNKCULC                                                 
         MVC   TSARDAT,TRNKDATE                                                 
         MVC   TSARREF,TRNKREF                                                  
         MVC   TSARSBR,TRNKSBR                                                  
*        MVC   TSARMOS,TRNRSMOS                                                 
         MVI   TSARRSTA,0                                                       
         TM    TRNRSTAT,TRNSARCH                                                
         BZ    *+8                                                              
         OI    TSARRSTA,TRNSARCH   SET RECORD IS ON ARCHIVE                     
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        EXTRACT TRNEL VALUES                         
         TM    COMPSTAT,CPYSOROE   TEST OFFICES IN USE                          
         BZ    *+10                NO - DON'T SET TSAROFF                       
         MVC   TSAROFF,TRNOFFC                                                  
         MVC   TSARBAT,TRNBTCH                                                  
         MVC   TSARBTY,TRNTYPE                                                  
         MVC   TSARSTA,TRNSTAT                                                  
         XC    TSARVAR,TSARVAR     CLEAR VARIABLE KEY BYTE                      
         ZAP   TSARAMNT,TRNAMNT                                                 
         TM    TRNSTAT,TRNSHOLD    TEST ALREADY HELD FROM PAYING                
         BO    READ22                                                           
         TM    COMPSTA4,CPYSIREG   TEST AGENCY USES INVOICE REGISTER            
         BNO   READ24                                                           
         CLC   SUPPUL,TRNKUNT      TEST PRODUCTION SUPPLIER                     
         BE    *+14                                                             
         CLC   SUPXUL,TRNKUNT      TEST HOUSE SUPPLIER                          
         BNE   READ24                                                           
         TM    TRNSTAT,TRNSAUTH                                                 
         BO    READ24                                                           
READ22   OI    TSARINDS,TSARDISQ   UNAUTH/HELD TRANS - DISPLAY ONLY             
         MVI   TSARCHA,C'*'                                                     
READ24   TM    TRNSTAT,TRNSAPPR                                                 
         BNO   *+12                                                             
         MVI   TSARVAR,TRNSAPPR    SET VARIABLE KEY BYTE FOR SORT SEQ.          
         OI    TSARINDS,TSARMKQ+TSARINMQ                                        
                                                                                
         USING SCIELD,R2           EXTRACT SCIEL VALUES                         
         ICM   R2,15,ASCIEL                                                     
         BZ    READ30                                                           
         CLI   SCITYPE,SCITCDSC    TEST CASH DISCOUNT TYPE                      
         BNE   READ30                                                           
         OI    TSARIND2,TSARLDSC   SET LIVE DISCOUNT                            
         CLI   PROFDISC,C'Y'       TEST ADDING ANY CASH DISCOUNT                
         BNE   READ30                                                           
         AP    TSARAMNT,SCIAMNT    ADD TO TRANSACTION AMOUNT                    
                                                                                
         USING TRSELD,R2           EXTRACT TRSEL VALUES                         
READ30   ICM   R2,15,ATRSEL        R2=A(TRANSACTION STATUS ELEMENT)             
         MVC   TSARADAT,TRSDATE    EXTRACT TRANSACTION ACTIVITY DATE            
         MVC   TSARSSTA,TRSSTAT    EXTRACT STATUS BYTE                          
                                                                                
         OC    ANOTELS,ANOTELS     TEST MEMO'S ATTACHED                         
         BZ    *+8                                                              
         OI    TSARIND2,TSARMEMO                                                
                                                                                
*&&UK*&& GOTO1 AVAL1FC             VALIDATE ONE FOREIGN CURRENCY                
*&&US                                                                           
         USING XPYELD,R2                                                        
         ICM   R2,15,AXPYEL                                                     
         BZ    READ40                                                           
*        MVC   TSARFINV(L'XPYINV),XPYINV                                        
         CP    XPYCD,PZERO         TEST CASH DISCOUNT                           
         BE    READ40                                                           
*        ZAP   TSARFDIS,XPYCD                                                   
         OI    TSARIND2,TSARLDSC   SET LIVE DISCOUNT                            
         CLI   PROFDISC,C'Y'       TEST ADDING ANY CASH DISCOUNT                
         BNE   READ40                                                           
         AP    TSARAMNT,XPYCD      ADD TO TRANSACTION AMOUNT                    
                                                                                
         USING FFTELD,R2                                                        
READ40   ICM   R2,15,AFFTLEL                                                    
         BZ    READ50                                                           
*&&                                                                             
READ50   GOTO1 ATSARADD            PUT RECORD TO TSAR                           
         BNE   READ80                                                           
                                                                                
         OC    TSARDADR,TSARDADR    ANY DISK ADDRESS?                           
         BZ    READ70               . NO                                        
                                                                                
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
*&&UK                                                                           
         USING AFCELD,R3                                                        
         ICM   R3,15,AAFCEL                                                     
         BZ    *+10                                                             
         AP    CURCRS,AFCAMNT                                                   
*&&                                                                             
         AP    TOTCRS,TRNAMNT                                                   
         LA    RF,TOTBAL           RF=A(BALANCE TOTAL)                          
         TM    TRNSTAT,TRNSAPPR    TEST SELECTED FOR PAYMENT                    
         BNO   *+8                                                              
         LA    RF,TOTMRK           RF=A(SELECTED TOTAL)                         
*&&UK                                                                           
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         AP    CURTOTS-TOTALS(L'TOTALS,RF),AFCAMNT                              
*&&                                                                             
         AP    0(L'TOTALS,RF),TRNAMNT                                           
*                                                                               
READ70   MVI   ANYADD,1            SET TRANSACTION ADDED                        
*                                                                               
         ICM   R0,3,OVTS2N                                                      
         AHI   R0,1                                                             
         STCM  R0,3,OVTS2N         READ THE NEXT TSAR2 RECORD                   
*                                                                               
         B     READ10              READ SEQUENTIAL                              
         DROP  R2                                                               
*&&UK*&& DROP  R3                                                               
                                                                                
READ80   TM    DISIND,DISIOFLO     TEST ERROR WAS OVERFLOW                      
         BNO   EXIT                                                             
         B     DISPTRN                                                          
*                                                                               
READTRNX NI    DISIND,255-DISIOFLO  CLEAR OVERFLOW (DID NOT OCCUR)              
         CLI   ANYADD,1            TEST ANYTHING IN BUFFER                      
         BE    DISPTRN                                                          
         LA    R1,MATSELH          NO - SET CURSOR TO SUPPLIER FIELD            
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IANOTRAN)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
*        DROP  R1                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
DISPTRN  GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(1,0),MRKOLAYH               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*&&UK                                                                           
*        GOTO1 ABLDCURR            BUILD CURRENCY ENTRIES                       
*        BNH   *+6                                                              
*        DC    H'0'                                                             
*        GOTO1 ASETFORE            SET UP SINGLE FOREIGN CURRENCY               
*&&                                                                             
         OI    TWAMODE,TWAMHDRS    INDICATE WE HAVE A HEADER SAVED              
         GOTO1 AOVRSCR,GRDSCR      OVERLAY ACTION INPUT SCREEN                  
         OI    MRKTYPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    MRKACTH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
DTRN02   OI    DISIND,DISIRST      SET START FROM BEGINNING                     
         GOTO1 ADISGRID            YES                                          
*        NI    DISIND,X'FF'-DISIRST                                             
         L     R2,ADISTOTS                                                      
         GOTO1 ABLDTOT,(R2)                                                     
DISPTRNX B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
* VALIDATE INPUT                                                                
*----------------------------------------------------------------------         
         USING DISLINED,R2                                                      
VALINP   LA    RF,MRKOPTH          SET A(OPTION FIELD) FOR EARLY EXIT           
         ST    RF,FVADDR                                                        
         CLI   OPTALL,0            TEST GLOBAL MARK/UNMARK                      
         BE    VALINP02                                                         
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BO    VALINP16            YES - CALL DISPLAY                           
         BAS   RE,MRKALL           MARK ALL TRANSACTIONS                        
         OI    TWAMODE2,TWAM2SKP   SET SKIP VALIDATION                          
         OI    DISIND,DISIRST      SET TO RESTART DISPLAY                       
         NI    DISIND,X'FF'-DISINIT  REINITIALISE FOR GRIDS                     
         B     VALINP16                                                         
*                                                                               
VALINP02 MVI   ANYMARK,0                                                        
VALINP16 GOTO1 ADISGRID            YES                                          
*                                                                               
         L     R2,ADISTOTS                                                      
         GOTO1 ABLDTOT,(R2)                                                     
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   *+8                                                              
         OI    TWAMODE2,TWAM2CHG   SET CHANGES MADE BIT                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE TRANSACTIONS                                                 *         
***********************************************************************         
         USING TSARD,RF                                                         
         USING REPD,R3                                                          
UPDATE   ZAP   REPMRK,PZERO        INIT MARKED TOTALS                           
         ZAP   REPUMK,PZERO        INIT UNMARKED TOTALS                         
         LA    R1,MRKOPTH                                                       
         GOTO1 AUPDGRID            READ IN DATA AND CLEAR SCREEN                
         BNE   UPDATEX             HAVEN'T COME TO END OF DATA                  
                                                                                
         TM    TWAMODE2,TWAM2CHG                                                
         BO    UPD010                                                           
         MVC   FVMSGNO,=AL2(EANOTHIN)  NOTHING DONE YET                         
         LA    R1,MRKOPTH          YES - SET OPTION FIELD                       
         B     UPDATEX                                                          
                                                                                
UPD010   OC    PRTSUB,PRTSUB                                                    
         BZ    UPD020                                                           
         L     R3,AREPWRK          R3=A(REPORT W/S)                             
         GOTO1 ABLDDIS             GET DISPLACEMENTS FROM PROFILE               
         GOTO1 APRTINI             INITIALISE AND PRINT FRONT PAGE              
         MVC   REPH5+L'DISLLINE+1(L'LC@APRVD),LC@APRVD                          
         LA    R1,REPH5+L'DISLLINE+1+L'LC@APRVD-1                               
         CLI   0(R1),C' '          SEEK FIRST NON-BLANK                         
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'?'                                                       
UPD020   LA    R1,MRKOPTH                                                       
         ST    R1,FVADDR                                                        
         LA    R1,1                                                             
         STCM  R1,3,TEMP                                                        
                                                                                
UPD030   GOTO1 ATSARGET,TEMP                                                    
         BE    UPD040                                                           
         L     RF,ATSARBLK                                                      
         TM    TSERRS,TSEEOF                                                    
         BO    UPD430                                                           
         DC    H'0'                                                             
                                                                                
UPD040   TM    TSARINDS,TSARDISQ   TEST DISPLAY ONLY TRANSACTION                
         BO    UPD060                                                           
         TM    TSARINDS,TSARMKQ    TEST IF USER IS MARKING                      
         BZ    UPD050                                                           
         TM    TSARSTA,TRNSAPPR    TEST ALREADY SELECTED                        
         BO    UPD060                                                           
         MVI   BYTE,TRNSAPPR                                                    
         AP    REPMRK,TSARAMNT     ADD TO MARKED TOTALS                         
         B     UPD070                                                           
                                                                                
UPD050   TM    TSARSTA,TRNSAPPR    TEST ALREADY SELECTED                        
         BZ    UPD420                                                           
         MVI   BYTE,0                                                           
         AP    REPUMK,TSARAMNT      ADD TO UNMARKED TOTALS                      
         B     UPD070                                                           
*                                                                               
UPD060   TM    TSARIND3,TSARZEPD+TSARZDUE                                       
         BZ    UPD420                                                           
         MVI   BYTE,X'FF'                                                       
         B     UPD080                                                           
*                                                                               
UPD070   OC    PRTSUB,PRTSUB       PRINT REPORT IF REQUIRED                     
         BZ    UPD080              MUST BE LIVE IF NO REPORT                    
         LA    R1,REPP1-1                                                       
         ICM   R1,8,=C'R'                                                       
         GOTO1 ABLDLIN             BUILD PRINT LINE USING REPDISP               
         MVC   REPP1+L'DISLLINE+1(L'LC4YES),LC4YES                              
         TM    TSARINDS,TSARMKQ                                                 
         BO    *+10                                                             
         MVC   REPP1+L'DISLLINE+1(L'LC4NO),LC4NO                                
         GOTO1 VREPORT,REPD        PRINT IT                                     
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    UPD420              YES - GET NEXT TSAR RECORD                   
*                                                                               
UPD080   MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGETRUP+IOACCMST+IO1Q                                        
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETELAD,AIOBUFF    SET A(ELEMENTS)                              
*                                                                               
         USING TRSELD,R2                                                        
         ICM   R2,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRSSTAT,TSARSSTA    TEST SOMEONE AMENDING ELSEWHERE              
         BNE   UPD090                                                           
*                                                                               
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRNSTAT,TSARSTA     TEST SOMEONE AMENDING ELSEWHERE              
         BE    UPD100                                                           
*                                                                               
UPD090   NI    TWAMODE2,255-TWAM2CHG  RESET CHANGES MADE BIT                    
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         MVC   FVMSGNO,=AL2(IAUPDCUT)  UPDATE CUT SHORT                         
         LA    R1,MRKACTH                                                       
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
         LA    R1,MRKACTH                                                       
         B     UPDATEX                                                          
         DROP  R1                                                               
*                                                                               
UPD100   CLI   BYTE,X'FF'                                                       
         BE    UPD110                                                           
         NI    TRNSTAT,255-TRNSAPPR     DESELECT                                
         CLI   BYTE,0                   TEST DESELECTING                        
         BE    *+8                                                              
         OI    TRNSTAT,TRNSAPPR         SELECT                                  
         USING TRSELD,R2                                                        
UPD110   ICM   R2,15,ATRSEL                                                     
         CLI   TRSLN,TRSLNQ        TEST SHORT ELEMENT                           
         BNL   UPD120                                                           
         GOTO1 AEXTRSL             YES - EXTEND IT                              
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
         ICM   R2,15,ATRSEL                                                     
UPD120   CLI   BYTE,X'FF'                                                       
         BNE   UPD130                                                           
         MVI   TRSMARK,TRSZOOMQ    SET MARKER TYPE/ACTION                       
         B     UPD140                                                           
UPD130   MVI   TRSMARK,TRSMCSQ     SET MARKER TYPE/ACTION                       
         CLI   BYTE,0              TEST SELECT/DESELECT                         
         BNE   UPD132                                                           
         OI    TRSMARK,TRSMUMQ     DESELECT - SET ACTION IS NEGATIVE            
         B     UPD190                                                           
*                                                                               
         USING GDAELD,R2                                                        
UPD132   ICM   R2,15,AGDAAPPR      TEST APPROVAL ELEMENT CARRIED                
         BZ    UPD137                                                           
         CLI   BYTE,TRNSAPPR       ARE WE APPROVING?                            
         BE    UPD134                                                           
         MVI   0(R2),X'FF'         DELETE OLD GDAELD                            
         GOTO1 VHELLO,DMCB,(C'D',LACCMST),(X'FF',AIOBUFF),0                     
         B     UPD140                                                           
*                                                                               
UPD134   TIME  DEC                                                              
         SRL   R0,8                SHIFT OUT TENTHS & HUNDREDTHS                
         SLL   R0,4                MAKE ROOM FOR SIGN                           
         XC    DUB,DUB                                                          
         STCM  R0,15,DUB+4                                                      
         OI    DUB+7,X'0F'                                                      
         AP    DUB,=P'60000'       BUMP UP HOURS FROM DDS TO ACTUAL             
         ICM   R0,15,DUB+4                                                      
         SRL   R0,4                SHIFT OUT SIGN                               
         STCM  R0,7,GDAAPTIM       SAVE OFF CURRENT TIME                        
         MVC   GDAAPPDT,TODAYP     SET DATE FROM HEADER SCREEN                  
         B     UPD140                                                           
UPD137   CLI   BYTE,TRNSAPPR       ARE WE APPROVING?                            
         BNE   UPD140                                                           
         LA    R2,ELEMT                                                         
         XC    GDAELD(GDALNQ),GDAELD                                            
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALN3Q                                                    
         MVI   GDATYPE,GDAAPP                                                   
         MVI   GDATSUB,GDAAPPMK                                                 
         MVC   GDAAPPDT,TODAYP     SET DATE FROM HEADER SCREEN                  
         TIME  DEC                                                              
         SRL   R0,8                SHIFT OUT TENTHS & HUNDREDTHS                
         SLL   R0,4                MAKE ROOM FOR SIGN                           
         XC    DUB,DUB                                                          
         STCM  R0,15,DUB+4                                                      
         OI    DUB+7,X'0F'                                                      
         AP    DUB,=P'60000'       BUMP UP HOURS FROM DDS TO ACTUAL             
         ICM   R0,15,DUB+4                                                      
         SRL   R0,4                SHIFT OUT SIGN                               
         STCM  R0,7,GDAAPTIM       SAVE OFF CURRENT TIME                        
         GOTO1 VHELLO,DMCB,(C'P',LACCMST),AIOBUFF,GDAELD                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
*                                                                               
UPD140   TM    TSARIND3,TSARZEPD   IF YES WAS THE EARLIEST PAYMENT DATE         
         BNZ   UPD160                                 CHANGED                   
UPD150   OC    ERPDSTA,ERPDSTA     SELECT - TEST EARLIEST PAYMENT DATE          
         BZ    UPD200                                                           
         TM    TSARIND3,TSARZEPD   TEST ZOOM ADDED/CHANGED/DELETED DATE         
         BO    UPD240                                                           
*                                                                               
UPD160   ICM   R2,15,AGDAERPD      TEST EARLIEST PAYMENT DATE CARRIED           
         BZ    UPD170                                                           
         MVC   GDADATE,ERPDSTA     SET DATE FROM HEADER SCREEN                  
         TM    TSARIND3,TSARZEPD   IF YES WAS THE EARLIEST PAYMENT DATE         
         BZ    UPD200                                 CHANGED                   
*&&US*&& B     UPD190                                                           
*&&UK*&& OC    TSARERPD,TSARERPD                                                
*&&UK*&& BZ    UPD190                                                           
*&&UK*&& MVC   GDADATE,TSARERPD    SET DATE FROM HEADER SCREEN                  
*&&UK*&& B     UPD200                                                           
UPD170   LA    R2,ELEMT                                                         
         XC    GDAELD(GDALNQ),GDAELD                                            
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATERPD                                                 
         MVC   GDADATE,ERPDSTA     SET DATE FROM HEADER SCREEN                  
         TM    TSARIND3,TSARZEPD   IF YES WAS THE EARLIEST PAYMENT DATE         
         BZ    UPD180                                 CHANGED                   
*&&US*&& B     UPD200                                                           
*&&UK*&& OC    TSARERPD,TSARERPD                                                
*&&UK*&& BZ    UPD200                                                           
*&&UK*&& MVC   GDADATE,TSARERPD    SET DATE FROM HEADER SCREEN                  
UPD180   GOTO1 VHELLO,DMCB,(C'P',LACCMST),AIOBUFF,GDAELD                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
         B     UPD200                                                           
UPD190   ICM   R2,15,AGDAERPD      TEST EARLIEST PAYMENT DATE CARRIED           
         BZ    UPD200              NO                                           
         GOTO1 VHELLO,DMCB,(C'D',LACCMST),('GDAELQ',AIOBUFF),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
*                                                                               
UPD200   TM    TSARIND3,TSARZDUE   WAS THE DUE DATE CHANGED                     
         BZ    UPD240              NO                                           
         L     R2,AIOBUFF               R2=A(DATA RECORD)                       
         USING TRNRECD,R2                                                       
         MVC   TRNRSDUE,TSARFDUE                                                
         USING DUEELD,R2                                                        
         ICM   R2,15,ADUEEL                                                     
         BZ    UPD220                                                           
         OC    TSARFDUE,TSARFDUE                                                
         BNZ   UPD210                                                           
         GOTO1 VHELLO,DMCB,(C'D',LACCMST),('DUEELQ',AIOBUFF),0                  
         CLI   12(R1),0                                                         
         BE    UPD230                                                           
         DC    H'0'                DIE ON ANY ERROR                             
UPD210   MVC   DUEDATE,TSARFDUE                                                 
         B     UPD240                                                           
*                                                                               
UPD220   LA    R2,ELEMT                                                         
         MVI   DUEEL,DUEELQ                                                     
         MVI   DUELN,DUELNQ                                                     
         MVC   DUEDATE,TSARFDUE                                                 
         GOTO1 VHELLO,DMCB,(C'P',LACCMST),AIOBUFF,(R2)                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
UPD230   GOTO1 ASETELAD,AIOBUFF    AND REFRESH ELEMENT ADDRESSES                
*                                                                               
UPD240   L     R2,AIOBUFF          R2=A(DATA RECORD)                            
         USING TRNRECD,R2                                                       
         LA    R1,IOPUT+IOACCMST+IO1Q   PUT BACK TO ACCMST                      
*&&UK                                                                           
         TM    TRNRSTAT,TRNSARCH        TEST TRANSACTION ON ACCARC              
         BNO   UPD250                                                           
         GOTO1 VPROMOTE,DMCB,AIOBUFF,ACOM  PROMOTE TO ACCMST AND                
         B     UPD260                   CHANGE ACCDIR POINTER                   
*&&                                                                             
UPD250   GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'TRNKEY),TRNKEY  EXTRACT TRANSACTION KEY                    
         LA    R1,IORDUP+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+(TRNKSTA-TRNRECD)(L'TRNKSTA),TRNRSTA                         
         LA    R1,IOWRITE+IOACCDIR+IO1Q                                         
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
UPD260   MVC   KEY(L'TRNKEY),TRNKEY     EXTRACT TRANSACTION KEY                 
*                                                                               
* MARK AUTO APPROVAL POINTERS AS APPROVED(UNAPPROVED)                           
*                                                                               
UPD270   MVC   SVKEY,KEY           SAVE OFF KEY FOR LATER RESTORE               
         USING TRNRECD,R2                                                       
         L     R2,AIOBUFF                                                       
*                                                                               
         LA    R4,TRNRFST          FIND TRSEL                                   
         LA    RF,0                                                             
         XC    SVMOS,SVMOS                                                      
         MVC   SVEST,SPACES                                                     
         MVC   SVOFF,SPACES                                                     
         MVI   SVSYS,0                                                          
UPD280   CLI   0(R4),0             END OF RECORD                                
         BE    UPD380                                                           
         CLI   0(R4),X'1A'         MEDIA TRANSFER ELEMENT                       
         BE    UPD300                                                           
         CLI   0(R4),X'23'         OTHERS ELEMENT                               
         BE    UPD310                                                           
         CLI   0(R4),X'44'         TRANSACTION ELEMENT                          
         BE    UPD320                                                           
         CLI   0(R4),X'46'         EXTRA PAYMENT ELEMENT                        
         BE    UPD330                                                           
         CLI   0(R4),X'6A'         MEDIA TRANSFER (PACKED DATA)                 
         BE    UPD300                                                           
         CLI   0(R4),X'E5'         GENERAL DATE ELEMENT                         
         BE    UPD360                                                           
         CLI   0(R4),X'F3'         NEW BILLING XFER(ONLY) ELEMENT               
         BE    UPD370                                                           
UPD290   SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     UPD280                                                           
*                                                                               
         USING MDTELD,R4                                                        
UPD300   CLI   MDTSYS,C'J'         IS SYSTEM PRODUCTION?                        
         BE    UPD420              YES NO POINTER NEEDED                        
*                                                                               
         MVC   SVSYS,MDTSYS        SYSTEM                                       
*                                                                               
         LA    R1,PSYSTAB          TABLE OF EQUIVALENT SYSTEMS-PRINT            
UPD302   CLI   0(R1),X'FF'                                                      
         BE    UPD305                                                           
         CLC   MDTSYS,0(R1)        IF MATCH ON SYSTEM THAN THE ACTUAL           
         BE    UPD304              SYSTEM MUST BE PRINT                         
         LA    R1,1(R1)                                                         
         B     UPD302                                                           
*                                                                               
UPD304   MVI   SVSYS,C'P'          YES SO SAVE AS PRINT IN SVSYS                
UPD305   MVC   SVMOS,MDTMOS        MOVE IN MONTH OF SERVICE                     
         MVC   SVEST,MDTEST        SAVE CHARACTER ESTIMATE                      
         OC    SVEST,SPACES                                                     
         B     UPD290                                                           
         DROP  R4                                                               
*                                                                               
         USING OTHELD,R4                                                        
UPD310   CHI   RF,23                                                            
         BH    UPD290                                                           
         CLI   OTHPROF-3,C' '      IF SOMETHING IN THIS FIELD THAN NO           
         BH    UPD290              MOS IN THIS ELEMENT                          
         LA    RF,23                                                            
         MVC   SVMOS,OTHDATE                                                    
         B     UPD290                                                           
         DROP  R4                                                               
*                                                                               
         USING TRNELD,R4                                                        
UPD320   MVC   SVOFF,TRNOFFC                                                    
         B     UPD290                                                           
         DROP  R4                                                               
*                                                                               
         USING XPYELD,R4                                                        
UPD330   OC    SVMOS,SVMOS                                                      
         BNZ   UPD350                                                           
         CLC   XPYPER,SPACES         CHECK FOR ANY PERIOD DATE(S)               
         BNH   UPD350                                                           
         CLI   TRNKCULA+3,C'N'       IF NETWORK                                 
         BE    *+8                                                              
         OI    FLAG,FLGBRD           GET DATE FROM BROADCAST CAL                
         MVC   WORK(L'XPYPER),XPYPER                                            
         LA    R5,WORK                                                          
         CLI   WORK+6,C' '                                                      
         BNH   *+8                                                              
         LA    R5,WORK+6                                                        
         CLI   WORK+6,C'-'                                                      
         BNE   *+8                                                              
         LA    R5,WORK+7                                                        
         TM    FLAG,FLGBRD                                                      
         BNO   UPD340                                                           
         GOTO1 VGETBRD,DMCB,(1,(R5)),WORK+20,VGETDAY,VADDAY                     
         LA    R5,WORK+20                                                       
UPD340   GOTO1 VDATCON,DMCB,(0,(R5)),(1,SVMOS)                                  
*                                                                               
UPD350   CLC   XPYEST,SPACES       IS THERE AN ESTIMATE?                        
         BE    UPD290                                                           
         OC    XPYEST,XPYEST                                                    
         BZ    UPD290                                                           
         LH    RE,XPYEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVEST(3),DUB           UNPACK THE ESTIMATE                       
         B     UPD290                                                           
         DROP  R4                                                               
*                                                                               
         USING GDAELD,R4                                                        
UPD360   CLI   GDATYPE,GDAMMOS     IS THIS AN MOS DATE?                         
         BNE   UPD290                                                           
         LA    RF,X'E5'                                                         
         MVC   SVMOS,GDAYYMM                                                    
         B     UPD290                                                           
         DROP  R4                                                               
*                                                                               
         USING MBIELD,R4                                                        
UPD370   CHI   RF,X'E5'                                                         
         BE    *+10                                                             
         MVC   SVMOS,MBIMOS                                                     
         MVC   SVEST,MBIEST                                                     
         B     UPD290                                                           
         DROP  R4                                                               
*                                                                               
         USING AAVPASD,R4                                                       
UPD380   LA    R4,KEY                                                           
         MVC   AAVPKEY,SPACES                                                   
         MVI   AAVPTYP,AAVPTYPQ    X'24'                                        
         MVI   AAVPSUB,AAVPSUBQ    X'01'                                        
         MVC   AAVPCPY,TRNKCPY                                                  
         MVC   AAVPCLT,TRNKCACT+9  LAST 3 CHAR OF CONTRA IS CLIENT              
         MVC   AAVPPRD,TRNKREF     1ST 3 CHAR OF REFERENCE IS PRODUCT           
         MVC   AAVPEST,SVEST       ESTIMATE                                     
         MVC   AAVPMOS,SVMOS       MOS                                          
         LA    RF,LDSYTAB          TABLE OF LEDGER/SYSTEM                       
UPD390   CLI   0(RF),X'FF'         IF NOT IN TABLE THAN NO PASSIVE FOR          
         BE    UPD410              THIS LEDGER.                                 
         CLC   TRNKLDG,0(RF)                                                    
         BE    UPD400                                                           
         LA    RF,2(RF)                                                         
         B     UPD390                                                           
*                                                                               
UPD400   MVC   AAVPSYS,1(RF)       SYSTEM                                       
         CLI   AAVPSYS,C'S'        IF IT'S SPOT CHECK TO SEE IF IT'S            
         BNE   *+16                REALLY NET BY CHECKING THE 1ST CHAR          
         CLI   TRNKACT,C'N'        OF THE ACCOUNT FOR 'N' (SSN)                 
         BNE   *+8                                                              
         MVI   AAVPSYS,C'N'                                                     
         MVC   AAVPOFF,SVOFF       OFFICE                                       
         MVC   AAVPACCT,TRNKLDG    STATION (SS ACCOUNT)                         
         MVC   AAVPKDA,IODA        DISK ADDRESS                                 
         MVI   AAVPFLG1,0          SET FOR GOOD ESTIMATE AS DEFAULT             
         GOTO1 AIOEXEC,IOHI+IOACCDIR+IO1Q                                       
         CLC   AAVPKEY(AAVPFLG1-AAVPKEY),KEYSAVE                                
         BNE   UPD410              END OF LEDGER                                
*                                                                               
         NI    AAVPSTAT,X'FF'-AAVPAPP   DESELECT                                
         CLI   BYTE,0                   TEST DESELECTING                        
         BE    *+8                                                              
         OI    AAVPSTAT,AAVPAPP          SELECT                                 
*                                                                               
         GOTO1 AIOEXEC,IOWRITE+IOACCDIR+IO1Q                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPD410   MVC   KEY,SVKEY                                                        
         GOTO1 AIOEXEC,IORD+IOACCDIR+IO1Q                                       
         BE    *+6                 RE-ESTABLISH SEQUENCE                        
         DC    H'0'                                                             
*                                                                               
                                                                                
UPD420   ICM   R1,3,TEMP                                                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,TEMP                                                        
         B     UPD030                                                           
                                                                                
UPD430   OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    UPD440              NO - MUST BE LIVE UPDATE                     
         GOTO1 APRTCLO             CLOSE REPORT, BUILD SPOOL-ID MESSAGE         
         LA    R1,MRKOPTH          YES - SET OPTION FIELD                       
         CLI   XACTION,ACTDRFT     TEST REPORT WAS DRAFT                        
         BE    UPDATEX             PRTCLO HAS SET MESSAGE                       
                                                                                
UPD440   LA    R1,MRKACTH                                                       
         NI    TWAMODE2,255-TWAM2CHG  RESET CHANGES MADE BIT                    
         MVC   FVMSGNO,=AL2(IATRNUPS)                                           
         OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IATRNUPR)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
                                                                                
UPDATEX  ST    R1,FVADDR           STORE FIELD ADDRESS                          
         XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
         B     EXIT                                                             
         DROP  R2,R3,RF                                                         
*                                  TABLE OF LEDGER AND SYSTEM                   
LDSYTAB  DC    C'SS'               LEDGER S - SYS SPOT (BUT MAYBE NET)          
         DC    C'TS'               LEDGER T - SYS SPOT                          
         DC    C'PP'               LEDGER P - SYS PRINT                         
         DC    C'QP'               LEDGER Q - SYS PRINT                         
         DC    C'UN'               LEDGER U - SYS NET                           
         DC    X'FF'                                                            
*                                   TABLE OF PRINT SYSTEMS                      
PSYSTAB  DC    C'I'                 INTERACTIVE                                 
         DC    C'L'                 SOCIAL                                      
         DC    C'B'                 MOBILE                                      
         DC    C'V'                 NATIONAL VIDEO                              
         DC    C'W'                 LOCAL VIDEO                                 
         DC    C'D'                 DIGITAL AUDIO                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* QUIT                                                                          
***********************************************************************         
QUIT     XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
         LA    R1,MRKACTH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAHDRECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         NI    TWAMODE2,255-TWAM2CHG                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SELECT/DESELECT ALL TRANSACTIONS                                   
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
*&&UK*&& AP    CURMRK,TSARAFCA     ADD TO MARKED (AFC)                          
*&&UK*&& SP    CURBAL,TSARAFCA     SUBTRACT FROM BALANCE (AFC)                  
         TM    TSARINDS,TSARINMQ   IS TRANS MARKED ON FILE?                     
*&&UK*&& BO    *+20                                                             
*&&US*&& BO    *+14                                                             
         AP    REPMRK,TSARAMNT     NO, ADD TO MARKED THIS SESSION               
*&&UK*&& AP    RCPMRK,TSARAFCA     NO, ADD TO MARKED (AFC)                      
         B     MRKALL6                                                          
         SP    REPUMK,TSARAMNT     YES, MUST HAVE BEEN ADDED TO UNMARKD         
*&&UK*&& SP    RCPUMK,TSARAFCA     YES, MUST HAVE BEEN ADDED (AFC)              
         B     MRKALL6                                                          
                                                                                
MRKALL4  TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BZ    MRKALL8                                                          
         NI    TSARINDS,255-TSARMKQ                                             
         SP    TOTMRK,TSARAMNT     SUBTRACT FROM MARKED                         
         AP    TOTBAL,TSARAMNT     ADD TO BALANCE                               
*&&UK*&& SP    CURMRK,TSARAFCA     SUBTRACT FROM MARKED (AFC)                   
*&&UK*&& AP    CURBAL,TSARAFCA     ADD TO BALANCE (AFC)                         
         TM    TSARINDS,TSARINMQ   IS TRANS MARKED ON FILE?                     
*&&UK*&& BO    *+20                                                             
*&&US*&& BO    *+14                                                             
         SP    REPMRK,TSARAMNT     NO, MUST HAVE BEEN ADDED TO MARKED           
*&&UK*&& SP    RCPMRK,TSARAFCA     NO, MUST HAVE BEEN ADDED (AFC)               
         B     MRKALL6             THIS SESSION, SO SUBTRACT IT                 
         AP    REPUMK,TSARAMNT     YES, ADD TO UNMARKED THIS SESSION            
*&&UK*&& AP    RCPUMK,TSARAFCA     YES, ADD TO UNMARKED (AFC)                   
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
                                                                                
         LA    R1,MRKOPTH          SET CURSOR TO OPTIONS FIELD                  
         ST    R1,FVADDR                                                        
                                                                                
MRKALLX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OVERLAY SPECIFIC FILTERING                                                    
***********************************************************************         
RFILTER  NTR1  ,                                                                
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        TEST THIS IS A TRANSACTION                   
         BZ    RFILTNEQ                                                         
         GOTO1 ATCLOSE             TEST TRANS IS CLOSED (USING OFFAL)           
         BNE   RFILTERX                                                         
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BO    RFILTNEQ                                                         
         CLI   ICLMARK,ICLMYES     INCLUDE SELECTED                             
         BE    RFILT02                                                          
         LA    RF,X'10'            BO IF EXCLUDING SELECTED                     
         CLI   ICLMARK,ICLMNO                                                   
         BE    *+8                                                              
         LA    RF,X'80'            BZ IF SELECTED ONLY                          
         TM    TRNSTAT,TRNSAPPR                                                 
         EX    RF,*+4                                                           
         NOP   RFILTNEQ            BO OR BZ                                     
                                                                                
RFILT02  OC    OFFICE,OFFICE       TEST OFFICE FILTER SET                       
         BZ    RFILT04                                                          
         IC    RF,OFFICEXL                                                      
         EX    RF,*+8                                                           
         BNE   RFILTERX            WRONG OFFICE                                 
         CLC   TRNOFFC(0),OFFICE                                                
                                                                                
RFILT04  GOTO1 ABLDSRC             BUILD SOURCE A/C IN SRCWORK                  
                                                                                
         OC    SRCACC,SRCACC       TEST FILTER FOR SOURCE A/C                   
         BZ    RFILT06                                                          
         IC    RF,SRCACCXL                                                      
         EX    RF,*+8                                                           
         BNE   RFILTERX            SOURCE ACCOUNT DOES NOT MATCH                
         CLC   SRCWORK(0),SRCACC                                                
                                                                                
         USING TRSELD,R2                                                        
RFILT06  ICM   R2,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO STATUS ELEMENT                            
         CLC   TRSDATE,ADASTA      TEST ACTIVITY DATE                           
         BL    RFILTNEQ                                                         
         CLC   TRSDATE,ADAEND                                                   
         BH    RFILTNEQ                                                         
*&&UK                                                                           
         USING GDAELD,R2                                                        
         CLI   ICLMARK,ICLMONLY    TEST EXCLUSIVELY DESELECTING                 
         BNE   RFILT10                                                          
         OC    ERPDSTA,ERPDSTA     IF START=X'000000'                           
         BNZ   RFILT08                                                          
         OC    ERPDEND,ERPDEND     AND END=X'FFFFFF'                            
         BO    RFILT10             NO FILTER                                    
RFILT08  ICM   R2,15,AGDAERPD                                                   
         BZ    RFILTNEQ            EARLIEST PAYMENT DATE MISSING                
         CLC   GDADATE,ERPDSTA     TEST ACTIVITY DATE                           
         BL    RFILTNEQ                                                         
         CLC   GDADATE,ERPDEND                                                  
         BH    RFILTNEQ                                                         
                                                                                
RFILT10  DS    0H                                                               
*&&                                                                             
RFILTEQU CR    RB,RB                                                            
         B     RFILTERX                                                         
                                                                                
RFILTNEQ LTR   RB,RB                                                            
         B     RFILTERX                                                         
                                                                                
RFILTERX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A KEY FOR TRANSACTION READING                                
*                                                                               
* NTRY - R1=KEY BUILD MASK (SEE SETXXX EQUATES)                                 
*        R2=A(TRANSACTION KEY)                                                  
***********************************************************************         
         USING TRNRECD,R2                                                       
SETKEY   NTR1  ,                                                                
         STC   R1,WORK                                                          
                                                                                
         TM    WORK,SETACC         SET ACCOUNT                                  
         BZ    SETKEY0                                                          
         MVC   TRNKEY,SPACES                                                    
         OC    ACCOUNT,ACCOUNT                                                  
         BNZ   *+6                                                              
         DC    H'0'                SUPPLIER MISSING                             
         MVC   TRNKCULA,ACCOUNT                                                 
                                                                                
SETKEY0  TM    WORK,SETOFF         SET OFFICE                                   
         BZ    SETKEY1                                                          
         MVC   TRNKOFF,SPACES                                                   
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    SETKEY1             NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    SETKEY1             YES - OFFICE NOT IN KEY                      
         OC    OFFICE,OFFICE                                                    
         BZ    SETKEY1                                                          
         MVC   TRNKOFF,OFFICE                                                   
                                                                                
SETKEY1  TM    WORK,SETCON         SET CONTRA                                   
         BZ    SETKEY2                                                          
         MVC   TRNKCULC,SPACES                                                  
         MVI   TRNKCULC+L'TRNKCULC-1,X'41'                                      
         OC    CONTRA,CONTRA                                                    
         BZ    SETKEY2                                                          
         MVC   TRNKCULC,CONTRA                                                  
                                                                                
SETKEY2  TM    WORK,SETSDT         SET MIN TRANSACTION DATE                     
         BZ    SETKEY3                                                          
         MVC   TRNKDATE,PERSTA                                                  
                                                                                
SETKEY3  TM    WORK,SETREF         SET REFERENCE (BILL NUMBER)                  
         BZ    SETKEY4                                                          
         MVC   TRNKREF,SPACES                                                   
         MVI   TRNKREF+L'TRNKREF-1,X'41'                                        
         OC    REFSTA,REFSTA                                                    
         BZ    SETKEY4                                                          
         MVC   TRNKREF,REFSTA                                                   
                                                                                
SETKEY4  TM    WORK,NXTOFF         BUMP OFFICE (NEW OFFICES ONLY)               
         BZ    SETKEY5                                                          
         IC    RE,TRNKOFF+(L'TRNKOFF-1)                                         
         LA    RE,1(RE)                                                         
         STC   RE,TRNKOFF+(L'TRNKOFF-1)                                         
                                                                                
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
* GENERAL EXIT                                                                  
***********************************************************************         
OKXIT    CR    RB,RB                                                            
         J     EXIT                                                             
ERXIT    LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
LDGLIST  DS    0CL1                VALID LEDGERS IN UNIT S                      
*&&UK*&& DC    C'FTVX'                                                          
*&&US*&& DC    C'STUPQ'                                                         
         DC    AL1(EOT)                                                         
*                                                                               
DUMCON   DC    C'SJ999'                                                         
OACCMST  DC    C'ACCMST  '                                                      
         EJECT                                                                  
*                                                                               
SUPNDSP  EQU   19                                                               
***********************************************************************         
* RE-INIT TSAR BUFFER 1 TO WORK WITH MARKER DISPLAY TSAR RECORDS                
***********************************************************************         
         USING TSARD,R1            R1=A(TSAR BLOCK)                             
REINTSR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,ATSARBLK                                                      
         MVC   TSINDS,TWATSARI     SET INDICATORS                               
         OI    TSINDS,TSIREUSE     SET TO RE-USE PREVIOUS ALLOCATION            
         MVC   TSPAGL,TWALOWPG     SET LOW PAGE NUMBER                          
         MVC   TSPAGN,TWANUMPG     SET NUMBER OF PAGES ALLOCATED                
         MVI   TSACTN,TSAINI       SET INITIALISE                               
         TM    TWAMODE,TWAMINIT                                                 
         BZ    *+8                                                              
         MVI   TSACTN,TSARES       SET RESTORE                                  
         GOTO1 VTSAR               CALL TO INITIALISE/RESTORE                   
         BE    REINTSRX                                                         
         TM    TWAMODE,TWAMRSRV    TEST FIRST TIME ALLOCATION                   
         BZ    *+6                                                              
         DC    H'0'                KILL IF RESTORE (INITIALISED)                
         NI    TSINDS,255-TSIALLOC RESET TEMPEST ALLOCATION                     
         MVI   TSPAGL,2            SET TO USE TEMPSTR PAGES 2-4                 
         MVI   TSPAGN,3                                                         
         BASR  RE,RF               INITIALISE TEMPSTR PAGES                     
         BE    *+6                                                              
         DC    H'0'                KILL IF CAN'T INITIALISE TEMPSTR             
         MVC   TWALOWPG,TSPAGL     SAVE LOW TSAR PAGE NUMBER                    
         MVC   TWANUMPG,TSPAGN     SAVE NUMBER OF PAGES ALLOCATED               
         MVC   TWATSARI,TSINDS     SAVE TEMPSTR/TEMPEST INDICATOR               
         NI    TWATSARI,TSIALLOC+TSIXTTWA                                       
         OI    TWAMODE,TWAMINIT+TWAMRSRV                                        
*                                                                               
REINTSRX J     OKXIT                                                            
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET AUTOAPP TSAR RECORDS                                                      
*        ON ENTRY - OVTS2N=TSAR RECORD NUMBER                                   
***********************************************************************         
         USING TSARD,R1             R1=A(TSAR BLOCK)                            
GETSAR2  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
GETSAR10 LA    R1,TSRBLK2                                                       
         MVI   TSACTN,TSAGET                                                    
         MVC   TSRNUM,OVTS2N                                                    
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0             SET CC FOR CALLER                           
         BNE   CONER                                                            
*                                                                               
         USING PSSAD,R3                                                         
         L     R3,TSAREC                                                        
*        CLI   PSSKTYP,X'00'        IS THIS A GOOD RECORD ?                     
*        BNE   CONER                NO, GO TO ERROR.                            
*                                                                               
         CLI   PSSKRTYP,5           CHECK FOR DETAIL LINES                      
         BE    GETSAR20                                                         
         B     GETSAR23                                                         
GETSAR15 SR    R0,R0                                                            
         ICM   R0,3,OVTS2N                                                      
         AHI   R0,1                                                             
         STCM  R0,3,OVTS2N                                                      
         B     GETSAR10                                                         
*                                                                               
GETSAR20 DS    0H                                                               
         CLI   ICLMARK,ICLMONLY     SHOW PREVIOUSLY APPROVED ITEMS ONLY         
         BNE   GETSAR2A                                                         
         CLI   PSSKSTAT,PSSKPREV                                                
         BNE   GETSAR15             SKIP IT                                     
         B     GETSAR22                                                         
*                                                                               
GETSAR2A DS    0H                                                               
         CLI   ICLMARK,ICLMNO                                                   
         BNE   GETSAR21             EXCLUDE PREVIOUSLY APPROVED ONES            
         CLI   PSSKSTAT,PSSKPREV    TRANSACTION PREVIOUSLY APPROVED?            
         BE    GETSAR15             . YES                                       
GETSAR21 CLI   PSSKSTAT,PSSKUNAP    TRANSACTION UNAPPROVED                      
         BE    GETSAR22                                                         
         CLI   PSSKSTAT,PSSKPREV    TRAN PREVIOUSLY APPROVED                    
         BNE   GETSAR23                                                         
GETSAR22 MVC   TSARDADR,PSSKDA      DISK ADDRESS                                
         B     GETSAR24                                                         
*                                                                               
GETSAR23 XC    TSARDADR,TSARDADR                                                
         OI    TSARINDS,TSARDISQ    DISPLAY ONLY TRANSACTION                    
GETSAR24 DS    0H                                                               
         MVC   TSARAASY,PSSKSYS     SYSTEM                                      
         MVC   TSARAAMD,PSSKMED     MEDIA                                       
         MVC   TSARAACL,PSSKCLI     CLIENT                                      
         MVC   TSARAAPR,PSSKPRO     PRODUCT                                     
         MVC   TSARAAES,PSSKEST     ESTIMATE                                    
         MVC   TSARAAVN,PSSKSTN     STATIONS                                    
         MVC   TSARAAVI,PSSKINV     STATION INVOICE NUMBER                      
         ZAP   TSARAAAB,PSSBILLB    AMOUNT BILLED                               
         ZAP   TSARAACR,PSSRCVDB    CASH RECEIVED                               
         ZAP   TSARAACD,PSSDISBB    CHECKS DISBURSED                            
         ZAP   TSARAACP,PSSCASHB    CASH POSITION (RECEIVED-DISBURSED)          
         ZAP   TSARAATC,PSSCLRB     TOTAL CLEARED                               
         ZAP   TSARAACU,PSSUDISB    CLEARED BUT UNDISBURSED                     
         ZAP   TSARAACA,PSSAVLB     CASH AVAILABLE                              
                                                                                
         MVC   TSARAALT,SPACES                                                  
         CLI   PSSKRTYP,0                                                       
         BNE   *+10                                                             
         MVC   TSARAALT(3),=C'ALL'                                              
         CLI   PSSKRTYP,1                                                       
         BNE   *+10                                                             
         MVC   TSARAALT(5),=C'PRIOR'                                            
         CLI   PSSKRTYP,4                                                       
         BNE   *+10                                                             
         MVC   TSARAALT(6),=C'FUTURE'                                           
         CLI   PSSKRTYP,3                                                       
         BNE   *+14                                                             
         MVC   TSARAALT+7(5),=C'TOTAL'                                          
         B     *+12                                                             
         CLI   PSSKRTYP,5                                                       
         BNE   CONOK                                                            
         MVC   WORK(L'PSSKMOS),PSSKMOS                                          
         MVI   WORK+L'PSSKMOS,1                                                 
         GOTO1 VDATCON,DMCB,(1,WORK),(14,TSARAALT)                              
*                                                                               
CONOK    J     OKXIT                                                            
CONER    J     ERXIT                                                            
         DROP  R1,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
* ACMRKWRK                                                                      
       ++INCLUDE ACMRKWRK                                                       
*                                                                               
OVRWRKD  DSECT                                                                  
OVRELO   DS    A                                                                
OVTS2N   DS    H                                                                
SVEST    DS    CL6                 SAVED ESTIMATE                               
SVMOS    DS    XL2                 SAVED MOS                                    
         DS    CL1                 SPARE                                        
SVOFF    DS    CL2                 SAVED OFFICE                                 
SVSYS    DS    CL1                 SAVED SYSTEM                                 
SVKEY    DS    XL(L'KEY)                                                        
* FLAG BYTE                                                                     
FLGBRD   EQU   X'40'               GET DATE FROM BRAODCAST CALENDAR             
       ++INCLUDE ACAUTOAPD                                                      
*                                                                               
SAVED    DSECT                                                                  
         ORG   SOVRWRK             ** OVERLAY SAVED W/S REDEFINED **            
         ORG   SOVRWRK+L'SOVRWRK                                                
         ORG   TOTALS                                                           
TOTCRS   DS    PL8                                                              
TOTMRK   DS    PL8                                                              
TOTBAL   DS    PL8                                                              
REPMRK   DS    PL8                                                              
REPUMK   DS    PL8                                                              
*&&UK                                                                           
         ORG   CURTOTS                                                          
CURCRS   DS    PL8                                                              
CURMRK   DS    PL8                                                              
CURBAL   DS    PL8                                                              
RCPMRK   DS    PL8                                                              
RCPUMK   DS    PL8                                                              
*&&                                                                             
*                                                                               
TWAD     DSECT                                                                  
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKD7D                                                       
         ORG   TWAUSER                                                          
TSARMODE DS    CL1                                                              
TSARFLD2 DS    CL3                                                              
*                                                                               
* ACAUTOD                                                                       
       ++INCLUDE ACAUTOD                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043ACMRK12   11/09/18'                                      
         END                                                                    
