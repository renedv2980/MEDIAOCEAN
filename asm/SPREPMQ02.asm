*          DATA SET SPREPMQ02  AT LEVEL 072 AS OF 08/29/00                      
*PHASE SPMQ02A                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE EZMMPCDA                                                               
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE CLPACK                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE NUMVAL                                                                 
*INCLUDE QSORT                                                                  
*INCLUDE SORTER                                                                 
*        TITLE 'SPREPMQ02 - CONVERT MMP DATA TO EASI INPUT'                     
        TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT - MACROS'         
***********************************************************************         
*                                                                     *         
*        MACROS TO FILL IN END OF FIELD AND END OF RECORD CHARACTERS  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
*                                                                               
         MACRO                                                                  
*                                                                               
         SETEF &FIELD                                                           
*                                                                               
         MVI   &FIELD+L'&FIELD,EOF                                              
*                                                                               
         MEND                                                                   
*                                                                               
         MACRO                                                                  
*                                                                               
         SETER &FIELD                                                           
*                                                                               
         MVI   &FIELD+L'&FIELD,EOR                                              
*                                                                               
         MEND                                                                   
*                                                                               
*        TEST IF FIELD IS NUMERIC                                               
*                                                                               
         MACRO                                                                  
*                                                                               
         TSTNM &FIELD              TEST FIELD AS NUMERIC                        
*                                                                               
         MVC   NUMWORK(L'&FIELD),&FIELD                                         
         NC    NUMWORK(L'&FIELD),ZEROS                                          
         CLC   NUMWORK(L'&FIELD),ZEROS                                          
*                                                                               
         MEND                                                                   
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMP DATA TO EASI INPUT'                     
***********************************************************************         
*                                                                     *         
*        SPREPMQ02 - CONVERT MMP DATA TO EASI INPUT                   *         
*                                                                     *         
*        MMPLUS INPUT TAPE CONVERT TO INPUT FOR EZLOAD                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPMQ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPMQ02                                                         
*                                                                               
         L     RA,0(R1)            ESTABLISH SPWORKD                            
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         L     R8,=A(WORKSECT)     ESTABLISH WORKAREA                           
         USING WORKSECT,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        ONLY INTERESTED IN 1ST TIME CALL             
         BE    MMPCNV                                                           
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMP DATA TO EASI INPUT- MMPCNV'             
***********************************************************************         
*                                                                     *         
*        MAIN LINE                                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MMPCNV   DS    0H                                                               
*                                                                               
         BRAS  RE,INIT             INITIALIZATION                               
*                                                                               
         BRAS  RE,SORTINI          SORT INITIALIZATION                          
*                                                                               
         CLI   SWITCH,0            IF NO RECORDS TO SORT                        
         BE    RECDONE                SKIP RECORD PROCESSING                    
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMP DATA TO EASI INPUT- RECLOOP'            
***********************************************************************         
*                                                                     *         
*        PROCESS RECORDS FROM SORT                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         MVI   SORTSW,0            RESET SORT SWITCH                            
*                                                                               
RECLOOP  DS    0H                                                               
*                                                                               
         GOTO1 SORTER,DMCB,=C'GET',0   GET FIRST/NEXT HEADER 1 RECORD           
*                                                                               
         ICM   R7,15,DMCB+4        GET A(RETURNED RECORD)                       
         BZ    RECDONE             END OF FILE                                  
*                                                                               
RECH110  DS    0H                                                               
*                                  SAVE SEQUENCE NUMBER                         
         BRAS  RE,RECH1            PROCESS HEADER RECORD 1                      
         BNZ   RECCONT             SKIP RECORD                                  
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT - RECH2'         
***********************************************************************         
*                                                                     *         
*        PROCESS HEADER RECORD 2                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RECH2    DS    0H                                                               
*                                                                               
         LA    RE,SSEQCTR-SORTREC(R7)                                           
         ZAP   SSEQCTR,0(L'SSEQCTR,RE)                                          
*                                                                               
         USING SORTREC,R7          ESTABLISH RETURNED RECORD                    
*                                                                               
         CLI   SORTSW,SSWDROP      IF INVOICE BEING DROPPED                     
         BE    RECH2X                 DO NOT PROCESS THIS RECORD                
*                                                                               
         MVC   MH2REC(MH2RECLQ),SREC         COPY TO HEADER 2 AREA              
*                                                                               
         CLC   MH2RECID,=C'H2'     HEADER 2 RECORD                              
         BE    RECH210             MUST FIND IT                                 
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGRIDNF,MH2REC,                        X        
               (L'MH2RECID,MH2RECID-MH2REC),=C'H2'                              
*                                                                               
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RECH2X                                                           
*                                                                               
MSGRIDNF DC    CL50'H2 RECORD TYPE NOT FOUND. INVOICE DROPPED.'                 
*                                                                               
RECH210  DS    0H                                                               
*                                                                               
*        PRODUCE INVOICE HEADER RECORD                                          
*                                                                               
         XC    E31REC(256),E31REC INIT INVOICE HEADER RECORD                    
         XC    E31REC+256(E31RECLQ-256),E31REC+256                              
*                                                                               
         LA    RF,E31RECLQ         INIT RECORD LENGTH                           
         STCM  RF,3,E31RECLN                                                    
*                                                                               
         MVC   E31RECID,=C'31'     SET RECORD ID                                
*                                                                               
         SETEF E31RECID            SET EOF                                      
*                                                                               
         MVC   E31REPNM(L'MH2STARP),MH2STARP    REPNAME                         
         SETEF E31REPNM                                                         
*                                                                               
         SETEF E31SLSNM                                                         
*                                                                               
         MVC   E31ADVNM,MH1ADVNM   ADVERTISER NAME                              
         SETEF E31ADVNM                                                         
*                                                                               
         MVC   E31PRDNM(L'MH1PRDNM),MH1PRDNM   PRODUCT NAME                     
         SETEF E31PRDNM                                                         
*                                                                               
         GOTO1 DATVAL,DMCB,(0,MH2INVDT),E31INVDT                                
*                                                                               
         OC    DMCB(4),DMCB        IF DATE NOT VALID                            
         BNZ   *+10                                                             
         MVC   E31INVDT,TODAYC        USE TODAY'S DATE                          
*                                                                               
         SETEF E31INVDT                                                         
*                                                                               
         SETEF E31ORDTP                                                         
*                                                                               
*        FIND DDS CLT/PRD/EST CODES                                             
*                                                                               
         GOTO1 =V(EZMMPCDS),DMCB,MH1ESTNM,DDSCDS,DATAMGR                        
*                                                                               
         MVC   E31AGEST(L'DDSEST),DDSEST     ESTIMATE NUMBER                    
         SETEF E31AGEST                                                         
*                                                                               
         MVC   E31INV#,MH2INV#     INVOICE NUMBER                               
*                                                                               
         CLC   E31INV#,SPACES      IF NO NUMBER PROVIDED                        
         BH    *+26                                                             
         MVC   E31INV#(6),TODAYC      MAKE # FROM TODAY'S DATE                  
         AP    WRKINV#,=P'1'          (BUMP WORK INVOICE #)                     
         OI    WRKINV#+7,X'0F'        AND                                       
         UNPK  E31INV#+6(4),WRKINV#   WORK INVOICE NUMBER                       
*                                                                               
         SETEF E31INV#                                                          
*                                                                               
         MVC   E31SCHST,=X'040400010203'   MMDDY TO YYMMDD                      
         TR    E31SCHST,MH1FLTST                                                
         SETEF E31SCHST                                                         
*                                                                               
         MVC   E31SCHEN,=X'040400010203'   MMDDY TO YYMMDD                      
         TR    E31SCHEN,MH1FLTEN                                                
         MVI   E31SCHST,C'9'     SET DECADE                                     
         MVI   E31SCHEN,C'9'     SET DECADE                                     
         CLC   E31SCHEN+5(1),E31SCHST+5  CHECK FOR CENTURY                      
         BL    *+12                                                             
         MVI   E31SCHST,C'0'     SET DECADE                                     
         MVI   E31SCHEN,C'0'     SET DECADE                                     
*                                                                               
         SETEF E31SCHEN                                                         
*                                                                               
         SETEF E31CONST                                                         
*                                                                               
         SETEF E31CONEN                                                         
*                                                                               
         SETEF E31BLLIN                                                         
*                                                                               
         SETEF E31RCRD#                                                         
*                                                                               
         MVI   E31AGFLG,C'Y'       ASSUME AGENCY COMMISSION                     
*                                                                               
         CLC   MH2AGNET,=C'1.00'   IF NET = GROSS                               
         BNE   *+8                                                              
         MVI   E31AGFLG,C'N'          FLAG AS  NO AGENCY COMMISSION             
*                                                                               
         SETEF E31AGFLG                                                         
*                                                                               
         MVC   E31STXPC(L'MH2TAXRT),MH2TAXRT    SALES TAX                       
         SETEF E31STXPC                                                         
*                                                                               
         SETEF E31AUDPC                                                         
*                                                                               
         SETEF E31REP#                                                          
         SETEF E31STA#                                                          
         SETEF E31STADV                                                         
         MVC   E31AGADV(3),MA1DDSCL   AGENCY CLIENT CODE                        
         SETEF E31AGADV                                                         
         SETEF E31STPRD                                                         
*                                                                               
         MVC   E31AGPRD(L'DDSPRD),DDSPRD   DDS PRODUCT CODE                     
         SETEF E31AGPRD                                                         
*                                                                               
         SETEF E31STCON                                                         
         SETEF E31AGCON                                                         
         SETEF E31IDUE                                                          
         SETEF E31NTWK                                                          
*                                                                               
         MVC   E31ACN(5),MH1ESTNM+8          ACN NUMBER                         
         SETER E31ACN                                                           
*                                                                               
*        BUILD TABLE OF BROADCAST MONTHS COVERED                                
*                                                                               
         LA    R0,12               MAX 12 MONTHS                                
         LA    R2,MH2BLLEN         POINT TO BILLING END DATES                   
         LA    R3,WRKBMONS         POINT TO BROADCAST MONTHS SAVE               
         XC    WRKBMONS(84),WRKBMONS   INIT BROADCAST MONTHS SAVE               
*                                                                               
RH2BMNLP DS    0H                                                               
*                                                                               
         CLC   0(6,R2),=6C'0'      DONE IF NO DATE PRESENT                      
         B     RH2BMNDN                                                         
*******  BNH   RH2BMNDN                                                         
*                                                                               
         MVC   WRKENDDT,=X'040500010203'                                        
         TR    WRKENDDT,0(R2)      GET BILL END DATE IN YYMMDD                  
*                                                                               
         GOTO1 =V(GETBROAD),DMCB,(1,WRKENDDT),0(R3),  GET BROADCAST MN X        
               V(GETDAY),ADDAY                        GET BROADCAST MN          
*                                                                               
         CLI   0(R1),X'FF'         TEST FOR ERRORS                              
         BNE   RH2BMN30                                                         
*                                                                               
         LA    RF,MH2REC           START OF RECORD                              
         SR    R2,RF               DISPLACEMENT OF FIELD INTO RECORD            
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGDATNV,MH2REC,                        X        
               (L'MH2BLLEN,0(R2)),=C'H2'                                        
*                                                                               
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RH2BMNDN              SHOW ONLY ONE ERROR                        
*                                                                               
MSGDATNV DC    CL50'DATE NOT VALID. INVOICE DROPPED.'                           
*                                                                               
RH2BMN30 DS    0H                                                               
*                                                                               
RH2BMNCN DS    0H                                                               
*                                                                               
         LA    R3,12(R3)           NEXT SAVEAREA                                
         LA    R2,6(R2)            NEXT ENDDATE                                 
         BCT   R0,RH2BMNLP                                                      
*                                                                               
RH2BMNDN DS    0H                                                               
*                                                                               
RECH2X   DS    0H                                                               
*                                                                               
         DROP  R7                                                               
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT-RECI1RD'         
***********************************************************************         
*                                                                     *         
*        READ INVOICE DETAIL RECORDS AND STORE IN BUFFER              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RECI1RD  DS    0H                                                               
*                                                                               
         CLI   SORTSW,SSWDROP      SKIP IF INVOICE BEING DROPPED                
         BE    RECI1HDX                                                         
*                                                                               
         MVC   WRKFLTST+2(4),MH1FLTST  FLIGHT START MMDD                        
         MVC   WRKFLTST(2),WRKFLTYR    FLIGHT START YY                          
*                                                                               
         GOTO1 DATCON,DMCB,WRKFLTST,WRKFLTST       DDS INTERNAL FORMAT          
*                                                                               
         MVC   WRKFLTYR,WRKFLTST   DDS INTERNAL FORMAT                          
*                                                                               
*        HAVE TRUE FLIGHT STARTING DATE                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,WRKFLTST),(3,DUB)      BINARY                     
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,1,DUB            BUMP YEAR BY 1                               
         LA    R2,1(R2)                                                         
         STC   R2,DUB                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(0,WRKDATE)       YYMMDD                     
*                                                                               
         MVC   WRKNXTYY,WRKDATE    SAVE NEXT YEAR                               
*                                                                               
         BCTR  R2,0                GET LAST YEAR                                
         BCTR  R2,0                                                             
         STC   R2,DUB                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(0,WRKDATE)                                  
*                                                                               
         MVC   WRKLSTYY,WRKDATE    SAVE LAST YEAR                               
*                                                                               
*        EXPAND FLIGHT PERIOD BY 2 MONTHS EITHER WAY                            
*                                                                               
         LA    R2,1(R2)                                                         
         STC   R2,DUB              ORIGINAL FLIGHT START DATE                   
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DUB+1            START MONTH                                  
*                                                                               
         BCT   RF,*+10             BACK UP A MONTH                              
         BCTR  R2,0                BACK IN PREVIOUS YEAR                        
         LA    RF,12               DECEMBER                                     
*                                                                               
         BCT   RF,*+10             BACK UP A MONTH                              
         BCTR  R2,0                BACK IN PREVIOUS YEAR                        
         LA    RF,12               DECEMBER                                     
*                                                                               
         STC   R2,DUB              NEW FLIGHT START DATE                        
         STC   RF,DUB+1                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),WRKFLTST                                     
*                                                                               
         MVC   WRKFLTEN+2(4),MH1FLTEN  FLIGHT END MMDD                          
         MVC   WRKFLTEN(2),WRKFLTYR    FLIGHT END YY                            
*                                                                               
         GOTO1 DATCON,DMCB,WRKFLTEN,WRKFLTEN       DDS INTERNAL FORMAT          
*                                                                               
         GOTO1 DATCON,DMCB,(0,WRKFLTEN),(3,DUB)      BINARY                     
*                                                                               
         SR    R2,R2                                                            
*                                                                               
         CLC   WRKFLTEN,WRKFLTST   IF END BEFORE START                          
         BH    RECI110                                                          
*                                                                               
         ICM   R2,1,DUB               BUMP YEAR BY 1                            
         LA    R2,1(R2)                                                         
         STC   R2,DUB                                                           
*                                                                               
RECI110  DS    0H                                                               
*                                                                               
         IC    R2,DUB              END YY                                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DUB+1            END MONTH                                    
*                                                                               
         LA    RF,1(RF)            UP A MONTH                                   
         CH    RF,=H'12'                                                        
         BNH   *+12                                                             
         LA    R2,1(R2)            INTO NEXT YEAR                               
         LA    RF,1                JANUARY                                      
*                                                                               
         LA    RF,1(RF)            UP A MONTH                                   
         CH    RF,=H'12'                                                        
         BNH   *+12                                                             
         LA    R2,1(R2)            INTO NEXT YEAR                               
         LA    RF,1                JANUARY                                      
*                                                                               
         STC   R2,DUB              NEW FLIGHT END DATE                          
         STC   RF,DUB+1                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),WRKFLTEN                                     
*                                                                               
RECI1HDX DS    0H                                                               
*                                                                               
         L     R6,BUFFSTRT         ESTABLISH DETAIL BUFFER                      
         USING BUFFD,R6                                                         
*                                                                               
         LA    R3,DTLSTRT          POINT TO START OF DETAIL BUFFER              
         LA    R4,MI1RECLQ         ENTRY LENGTH                                 
         LR    R5,R3                                                            
         BCTR  R5,0                END OF CURRENT BUFFER                        
*                                                                               
         STM   R3,R5,DTLBXLE       SAVE BXLE REGISTERS                          
*                                                                               
RECI1RDL DS    0H                                                               
*                                                                               
         GOTO1 SORTER,DMCB,=C'GET'   GET INVOICE DETAIL RECORD                  
*                                                                               
         ICM   R7,15,DMCB+4        GET A(RETURNED RECORD)                       
         BZ    RECDONE             END OF FILE                                  
*                                                                               
         LA    RE,SSEQCTR-SORTREC(R7)                                           
         ZAP   SSEQCTR,0(L'SSEQCTR,RE)                                          
*                                                                               
         USING SORTREC,R7          ESTABLISH RETURNED RECORD                    
*                                                                               
         MVC   MI1REC(MI1RECLQ),SREC     COPY TO INVOICE DETAIL AREA            
*                                                                               
         CLC   MI1RECID,=C'S3'     DONE IF SUMMARY FOUND                        
         BE    RECI1RDD                                                         
*                                                                               
         CLC   MI1RECID,=C'H1'     DONE IF NEXT HEADER FOUND                    
         BE    RECI1RDD                                                         
*                                                                               
         CLC   MI1RECID,=C'A1'     DONE IF NEXT AGENCY RECORD FOUND             
         BE    RECI1RDD                                                         
*                                                                               
         CLC   MI1RECID,=C'I1'     MUST BE INVOICE DETAIL                       
         BE    RECI1RD1                                                         
*                                  UNKNOWN RECORD TYPE                          
         GOTO1 =A(PUTERR),DMCB,MSGNOTID,MI1REC,                        X        
               (L'MI1RECID,MI1RECID-MI1REC),=C'I1'                              
*                                                                               
         B     RECI1RDC                                                         
*                                                                               
MSGNOTID DC    CL50'UNKNOWN RECORD TYPE. RECORD IGNORED'                        
*                                                                               
RECI1RD1 DS    0H                                                               
*                                                                               
         CLI   SORTSW,SSWDROP      SKIP IF INVOICE BEING DROPPED                
         BE    RECI1RDC                                                         
*                                                                               
         MVC   WRKDATE,=X'040500010203'   ADD YY TO RUN DATE                    
         TR    WRKDATE,MI1DATE                                                  
*                                                                               
         MVC   WRKDATE(2),WRKNXTYY TRY NEXT YEAR                                
*                                                                               
         CLC   WRKDATE,WRKFLTEN    OKAY IF BEFORE FLIGHT END                    
         BNH   RECI1R10                                                         
*                                                                               
         MVC   WRKDATE(2),WRKFLTYR TRY FLIGHT START YEAR                        
*                                                                               
         CLC   WRKDATE,WRKFLTEN    OKAY IF BEFORE FLIGHT END                    
         BNH   RECI1R10                                                         
*                                                                               
         MVC   WRKDATE(2),WRKLSTYY MUST BE LAST YEAR                            
*                                                                               
RECI1R10 DS    0H                                                               
*                                                                               
         CLC   WRKDATE,WRKFLTST    MUST NOT BE BEFORE FLIGHT START              
         BNL   RECI1R11                                                         
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGDTEFX,MI1REC,                        X        
               (L'MI1DATE,MI1DATE-MI1REC),=C'I1'                                
*                                                                               
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RECI1RDC                                                         
*                                                                               
MSGDTEFX DC    CL50'RUN DATE NOT IN FLIGHT PERIOD. INVOICE DROPPED.'            
*                                                                               
RECI1R11 DS    0H                                                               
*                                                                               
         MVI   WRKDATE+6,C'/'      FOR TRANSLATION                              
         MVC   WRKDATE1,=X'0203060405060001'  FORMAT AS DATE                    
         TR    WRKDATE1,WRKDATE                                                 
*                                                                               
         GOTO1 DATVAL,DMCB,(0,WRKDATE1),WRKDATE   VALIDATE DATE                 
*                                                                               
         OC    DMCB(4),DMCB        IF DATE NOT VALID                            
         BNZ   RECI1RD2                                                         
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGDTENV,MI1REC,                        X        
               (L'MI1DATE,MI1DATE-MI1REC),=C'I1'                                
*                                                                               
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RECI1RDC                                                         
*                                                                               
MSGDTENV DC    CL50'RUN DATE NOT VALID. INVOICE DROPPED.'                       
*                                                                               
RECI1RD2 DS    0H                                                               
*                                                                               
         MVC   MI1DATE,WRKDATE     UPDATE RUN DATE                              
*                                                                               
         LA    RF,L'MI1TIME        MAX FIELD LENGTH                             
         LA    RE,MI1TIME+L'MI1TIME-1  LAST BYTE OF FIELD                       
*                                                                               
         CLI   0(RE),C' '          FIND TRUE FIELD LENGTH                       
         BH    *+14                                                             
         BCTR  RE,0                BACK UP A BYTE                               
         BCT   RF,*-10                                                          
         B     RECI1TME            INVALID TIME                                 
*                                                                               
         GOTO1 VTIMVAL,DMCB,((RF),MI1TIME),FULL                                 
*                                                                               
         CLI   DMCB,X'FF'          TIME MUST BE VALID                           
         BNE   RECI1RD4                                                         
*                                                                               
RECI1TME DS    0H                                                               
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGTIMNV,MI1REC,                        X        
               (L'MI1TIME,MI1TIME-MI1REC),=C'I1'                                
*                                                                               
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RECI1RDC                                                         
*                                                                               
MSGTIMNV DC    CL50'RUN TIME NOT VALID. INVOICE DROPPED.'                       
*                                                                               
RECI1RD4 DS    0H                                                               
*                                                                               
         LA    R0,L'MI1LEN         FIELD LENGTH                                 
         LA    R1,MI1LEN+L'MI1LEN-1 LAST BYTE OF FIELD                          
*                                                                               
         CLI   0(R1),C' '          FIND LAST BYTE                               
         BH    *+14                                                             
         BCTR  R1,0                BACK UP A BYTE                               
         BCT   R0,*-10                                                          
         B     RECI1RDE            ERROR - ALL BLANKS                           
*                                                                               
         LA    R1,MI1LEN           START OF FIELD                               
*                                                                               
         CLI   0(R1),C' '          ELIMINATE LEADING BLANKS                     
         BH    *+16                                                             
         LA    R1,1(R1)            NEXT BYTE                                    
         BCT   R0,*-12                                                          
         B     RECI1RDE            ERROR - ALL BLANKS                           
*                                                                               
         LR    RF,R0               FIELD LENGTH                                 
         BZ    RECI1RDE            ERROR - ALL BLANKS                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+16                                                          
         EX    RF,*+18                                                          
         EX    RF,*+20                                                          
         B     *+22                                                             
         MVC   DUB(0),=8C'0'       TEST FIELD IS NUMERIC                        
         NC    DUB(0),0(R1)                                                     
         CLC   DUB(0),=8C'0'                                                    
*                                                                               
         BE    RECI1RD5            SPOT LENGTH IS NUMERIC                       
*                                                                               
RECI1RDE DS    0H                  INVALID SPOT LENGTH                          
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGNOTNM,MI1REC,                        X        
               (L'MI1LEN,MI1LEN-MI1REC),=C'I1'                                  
*                                                                               
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RECI1RDC                                                         
*                                                                               
MSGNOTNM DC    CL50'FIELD MUST BE NUMERIC. INVOICE DROPPED'                     
*                                                                               
RECI1RD5 DS    0H                                                               
*                                                                               
         GOTO1 =A(NUMTRAN),DMCB,(L'MI1APRGR,MI1APRGR)                           
         BE    RECI1RD6            NO ERRORS                                    
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGNOTNM,MI1REC,                        X        
               (L'MI1APRGR,MI1APRGR-MI1REC),=C'I1'                              
*                                                                               
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RECI1RDC                                                         
*                                                                               
RECI1RD6 DS    0H                                                               
*                                                                               
         GOTO1 =A(NUMTRAN),DMCB,(L'MI1APRNT,MI1APRNT)                           
         BE    RECI1RD7            NO ERRORS                                    
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGNOTNM,MI1REC,                        X        
               (L'MI1APRNT,MI1APRNT-MI1REC),=C'I1'                              
*                                                                               
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RECI1RDC                                                         
*                                                                               
RECI1RD7 DS    0H                                                               
*                                                                               
RECI1RD9 DS    0H                                                               
*                                                                               
         CLI   SORTSW,SSWDROP      IF INVOICE BEING DROPPED                     
         BE    RECI1RDC               DO NOT PROCESS THIS RECORD                
*                                                                               
         LA    R5,1(R5)            A(NEXT AVAILABLE SLOT)                       
*                                                                               
         MVC   0(MI1RECLQ,R5),MI1REC  ADD DETAIL TO BUFFER                      
*                                                                               
         LA    R5,0(R4,R5)         NEW END OF BUFFER                            
         BCTR  R5,0                                                             
*                                                                               
RECI1RDC DS    0H                                                               
*                                                                               
         B     RECI1RDL                                                         
*                                                                               
RECI1RDD DS    0H                                                               
*                                                                               
         CLI   SORTSW,SSWDROP      IF INVOICE BEING DROPPED                     
         BE    RECI1X                 SKIP OUTPUT                               
*                                                                               
         STM   R3,R5,DTLBXLE       SAVE BXLE REGISTERS                          
*                                                                               
*        SORT BUFFER BY RUN DATE                                                
*                                                                               
         SR    RE,RE                                                            
         LR    RF,R5               BUFFER END                                   
         LA    RF,1(R5)                                                         
         SR    RF,R3               BUFFER LENGTH                                
         BNP   RI1DTLD1            NO DETAILS IN BUFFER                         
         DR    RE,R4               NUMBER OF ENTRIES IN BUFFER                  
*                                                                               
         GOTO1 =V(QSORT),DMCB,(0,0(R3)),(RF),(R4),L'MI1DATE,           X        
               MI1DATE-MI1REC                                                   
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT-RI1DTL'          
***********************************************************************         
*                                                                     *         
*        ADD INVOICE HEADER/DETAILS/TOTAL TO FILE                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RI1DTL   DS    0H                                                               
*                                                                               
         LA    R0,12               MAX 12 MONTHS                                
         LM    R3,R5,DTLBXLE       BXLE REGISTERS FOR BUFFER                    
         LA    R2,WRKBMONS-12      LIST OF BROADMONS IN INVOICE                 
         XC    0(12,R2),0(R2)      INIT FOR START OF LOOPS                      
*                                                                               
RI1DTLLP DS    0H                                                               
*                                                                               
         MVC   MI1REC(MI1RECLQ),0(R3)   NEXT RECORD IN BUFFER                   
*                                                                               
         CLC   MI1DATE,6(R2)       SKIP IF NEXT DETAIL IN SAME MONTH            
         BNH   RI1DTL10                                                         
*                                                                               
         OC    0(12,R2),0(R2)      SKIP TOTALS IF FIRST TIME                    
         BZ    RI1DTL05                                                         
*                                                                               
         BRAS  RE,TOTALS              GO PUT OUT TOTAL RECORD                   
*                                                                               
RI1DTL05 DS    0H                                                               
*                                                                               
         OC    12(12,R2),12(R2)    IF END OF LIST REACHED                       
         BZ    RI1DTL06               CREATE NEW ENTRY                          
         LA    R2,12(R2)           BUMP TO NEXT BROADCAST MONTH                 
         CLC   MI1DATE,6(R2)       RUN DATE MUST LIE IN MONTH                   
         BH    *-20                                                             
*                                                                               
         B     RI1DTL09                                                         
*                                                                               
*        ADD NEW BROADCAST MONTH TO LIST                                        
*                                                                               
RI1DTL06 DS    0H                                                               
*                                                                               
         LA    R2,12(R2)           BUMP TO NEXT BROADCAST MONTH                 
*                                                                               
         GOTO1 =V(GETBROAD),DMCB,(1,MI1DATE),0(R2),  GET BROADCAST MN  X        
               V(GETDAY),ADDAY                       GET BROADCAST MN           
*                                                                               
*        FILL IN BROADCAST MONTH AND ADD INVOICE HEADER                         
*                                                                               
RI1DTL09 DS    0H                                                               
*                                                                               
         MVC   E31BRDMN,6(R2)      BROADCAST MONTH IS YYMM OF END DATE          
         SETEF E31BRDMN                                                         
*                                                                               
         MVC   E31PERST,0(R2)      PERIOD IS BROADCAST MONTH                    
         SETEF E31PERST                                                         
*                                                                               
         MVC   E31PEREN,6(R2)                                                   
         SETEF E31PEREN                                                         
*                                                                               
         MVC   ESAVEREC(256),E31REC           SAVE CURRENT RECORD               
         MVC   ESAVEREC+256(256),E31REC+256                                     
*                                                                               
         GOTO1 =A(COLLAPSE),DMCB,E31REC   COLLAPSE RECORD                       
*                                                                               
         L     RF,=A(EZOUT)                                                     
         PUT   (RF),E31REC        PUT OUT RECORD                                
*                                                                               
         MVC   E31REC(256),ESAVEREC         RESTORE CURRENT RECORD              
         MVC   E31REC+256(256),ESAVEREC+256                                     
*                                                                               
RI1DTL10 DS    0H                                                               
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT - RECI1'         
***********************************************************************         
*                                                                     *         
*        PUT OUT INVOICE DETAIL                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RECI1    DS    0H                                                               
*                                                                               
         CLC   MI1RECID,=C'I1'      INVOICE DETAIL RECORD                       
         BNE   RI1DTLCN               MUST FIND ONE                             
*                                                                               
*        PRODUCE INVOICE DETAIL RECORD                                          
*                                                                               
         XC    E51REC(E51RECLQ),E51REC INIT INVOICE DETAIL RECORD               
*                                                                               
         LA    RF,E51RECLQ         INIT RECORD LENGTH                           
         STCM  RF,3,E51RECLN                                                    
*                                                                               
         MVC   E51RECID,=C'51'     SET RECORD ID                                
*                                                                               
         SETEF E51RECID            SET EOF                                      
*                                                                               
         MVI   E51RUNCD,C'Y'       ASSUME SPOT RAN                              
         SETEF E51RUNCD                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,MI1DATE),(X'20',E51RUNDT) NON-FUNNY YRS           
*                                                                               
*******  MVC   E51RUNDT,MI1DATE    CHANGED BEFORE PUT IN BUFFER                 
         SETEF E51RUNDT                                                         
*                                                                               
         TSTNM MI1DAYCD            DAY CODE SHOULD BE NUMERIC                   
         BNE   RECI1DY1            IGNORE IF NOT NUMERIC                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,MI1DAYCD         GET DAY CODE 0-6                             
         LA    RF,1(RF)            BUMP TO 1-7                                  
         STC   RF,E51RUNDY         SET DAY OF WEEK                              
*                                                                               
RECI1DY1 DS    0H                                                               
*                                                                               
         SETEF E51RUNDY                                                         
*                                                                               
*                                  CONVERT TIME TO MILITARY FORMAT              
*                                                                               
         LA    RF,L'MI1TIME        MAX FIELD LENGTH                             
         LA    RE,MI1TIME+L'MI1TIME-1  LAST BYTE OF FIELD                       
*                                                                               
         CLI   0(RE),C' '          FIND TRUE FIELD LENGTH                       
         BH    *+12                                                             
         BCTR  RE,0                BACK UP A BYTE                               
         BCT   RF,*-10                                                          
         DC    H'0'                INVALID TIME                                 
*                                                                               
         GOTO1 VTIMVAL,DMCB,((RF),MI1TIME),FULL                                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,FULL           TIME IN BINARY                               
         CVD   RF,DUB                                                           
*                                                                               
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         UNPK  E51RUNTM,DUB        PASS ON TIME                                 
         SETEF E51RUNTM                                                         
*                                                                               
         MVC   E51SPTLN,MI1LEN     SET SPOT LENGTH                              
         SETEF E51SPTLN                                                         
*                                                                               
         CLC   MI1ISCI,=4C'.'      SKIP IF NO ISCI CODE                         
         BE    *+16                                                             
         MVC   E51CPYID(4),MI1ISCI SET ISCI CODE                                
         MVC   E51CPYID+4(4),MI1COM#  SET COMMERCIAL NUMBER                     
*                                                                               
         SETEF E51CPYID                                                         
*                                                                               
         GOTO1 =A(NUMTRAN),DMCB,(L'MI1APRGR,MI1APRGR)                           
         AP    WRKGRS,DUB          INCREMENT GROSS ACCUMULATOR                  
         EDIT  (P8,DUB),(11,E51RATE),FLOAT=-,ALIGN=LEFT                         
*                                                                               
         SETEF E51RATE                                                          
*                                                                               
         GOTO1 =A(NUMTRAN),DMCB,(L'MI1APRNT,MI1APRNT)                           
         AP    WRKNET,DUB          INCREMENT NET ACCUMULATOR                    
*                                                                               
         SETEF E51CLASS                                                         
*                                                                               
         SETEF E51PGY                                                           
         SETEF E51MGDT1                                                         
         SETEF E51MGTM1                                                         
         SETEF E51MGDT2                                                         
         SETEF E51MGTM2                                                         
*                                                                               
         SETEF E51ADJDR                                                         
         SETEF E51ADJCR                                                         
         SETEF E51PROG                                                          
         SETEF E51BLBID                                                         
         SETEF E51BLBLN                                                         
         SETEF E51BLBVI                                                         
         SETER E51BLBAI                                                         
*                                                                               
         GOTO1 =A(COLLAPSE),DMCB,E51REC   COLLAPSE RECORD                       
*                                                                               
         L     RF,=A(EZOUT)                                                     
         PUT   (RF),E51REC                                                      
*                                                                               
         AP    WRKSPTS,=P'1'       BUMP SPOTS COUNTER                           
*                                                                               
RI1DTLCN DS    0H                                                               
*                                                                               
         BXLE  R3,R4,RI1DTLLP      NEXT INVOICE DETAIL                          
*                                                                               
RI1DTLDN DS    0H                                                               
*                                                                               
         BRAS  RE,TOTALS           FINAL TOTALS RECORD                          
*                                                                               
RI1DTLD1 DS    0H                                                               
*                                                                               
RECI1X   DS    0H                                                               
*                                                                               
         CLC   MI1RECID,=C'H2'     CHECK FOR HEADER RECORD FOUND                
         BE    RECH110                                                          
*                                                                               
RECCONT  DS    0H                                                               
*                                                                               
         B     RECLOOP                                                          
*                                                                               
RECDONE  DS    0H                                                               
*                                                                               
         DROP  R7                                                               
*                                                                               
         CLOSE MMPIN                                                            
         CLOSE EZOUT                                                            
*                                                                               
*        FREE UP STORAGE FOR INVOICE DETAIL BUFFER                              
*                                                                               
         ICM   R3,15,BUFFLEN       GET SIZE OF GETMAIN AREA                     
*                                                                               
         LA    R4,BUFFSTRT         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)  GET CORE                                   
*                                                                               
MMPCNVX  DS    0H                                                               
*                                                                               
         CLI   SWITCH,0            IF NO RECORDS WERE SORTED                    
         BNE   INVNFDN                                                          
*                                                                               
         L     R3,=A(PRINTOUT)     POINT TO MESSAGE DCB                         
         OPEN  ((R3),(OUTPUT))     OPEN MESSAGE PRINT FACILITY                  
*                                                                               
         MVC   P,SPACES            INIT PRINT LINE                              
         MVC   P+1(40),=CL40'NO INVOICE DATA FOUND IN DATASET'                  
*                                                                               
         LA    R2,20               INIT COUNTER                                 
*                                                                               
INVNFDLP DS    0H                                                               
*                                                                               
         PUT   (R3),P              PRINT MESSAGE                                
*                                                                               
INVNFDCN DS    0H                                                               
*                                                                               
         BCT   R2,INVNFDLP                                                      
*                                                                               
INVNFDDN DS    0H                                                               
*                                                                               
INVNFDN  DS    0H                                                               
*                                                                               
         CP    ERRCTR,=P'0'        IF THERE ARE NO ERRORS                       
         BNP   INVERRN                                                          
*                                                                               
         CLI   SWITCH,0            IF NO INVOICE MISSING MESSAGE                
         BE    INVERR1                                                          
*                                                                               
         L     R3,=A(PRINTOUT)                                                  
         OPEN  ((R3),(OUTPUT))        OPEN MESSAGE PRINT FACILITY               
*                                                                               
INVERR1  DS    0H                                                               
*                                                                               
         MVC   P,SPACES            INIT PRINT LINE                              
         MVC   P+1(40),=CL40'ERRORS FOUND. REPORT ON PRINT QUEUE'               
*                                                                               
         LA    R2,20               INIT COUNTER                                 
*                                                                               
INVERRLP DS    0H                                                               
*                                                                               
         PUT   (R3),P              PRINT MESSAGE                                
*                                                                               
INVERRCN DS    0H                                                               
*                                                                               
         BCT   R2,INVERRLP                                                      
*                                                                               
INVERRDN DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
INVERRN  DS    0H                                                               
*                                                                               
         CP    ERRCTR,=P'0'        IF THERE ARE ERRORS                          
         BP    *+12                                                             
         CLI   SWITCH,0            OR NO INVOICE MISSING MESSAGE                
         BNE   MMPCNVXX                                                         
*                                                                               
         CLOSE ((R3))                 CLOSE MESSAGE DCB                         
*                                                                               
MMPCNVXX DS    0H                                                               
*                                                                               
         GOTO1 AENDREQ             STOP REQUEST                                 
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMP DATA TO EASI INPUT- INIT'               
***********************************************************************         
*                                                                     *         
*        INITIALIZATION - OPEN FILES - READ FIRST RECORD              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9                                                    
         USING WORKSECT,R8                                                      
*                                                                               
*        GET STORAGE FOR INVOICE DETAIL BUFFER                                  
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(1000*MH1RECLQ+32)   1000 ELEMENT BUFFER                      
*                                                                               
         LA    R4,BUFFSTRT         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         ST    R3,BUFFLEN          SAVE RETURNED BUFFER LENGTH                  
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         L     R6,BUFFSTRT         ESTABLISH DETAIL BUFFER                      
         USING BUFFD,R6                                                         
*                                                                               
         LA    R3,DTLSTRT          POINT TO START OF DETAIL BUFFER              
         LA    R4,MI1RECLQ         ENTRY LENGTH                                 
         LR    R5,R3                                                            
         BCTR  R5,0                END OF CURRENT BUFFER                        
*                                                                               
         STM   R3,R5,DTLBXLE       SAVE BXLE REGISTERS                          
*                                                                               
         OPEN  (MMPIN,(INPUT))                                                  
         OPEN  (EZOUT,(OUTPUT))                                                 
*                                                                               
         ZAP   WRKGRS,=P'0'        INIT ACCUMULATORS                            
         ZAP   WRKNET,=P'0'                                                     
         ZAP   WRKAC,=P'0'                                                      
         ZAP   WRKSPTS,=P'0'                                                    
         ZAP   WRKINV#,=P'0'       INIT INVOICE NUMBER                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,TODAYDDS)                                   
*                                                                               
         GOTO1 DATCON,DMCB,(0,TODAYDDS),(X'20',TODAYC)  PRINTABLE FORM          
*                                                                               
INITX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT-SORTINI'         
***********************************************************************         
*                                                                     *         
*        INPUT MAY HAVE DUPLICATE COPIES OF AN INVOICE                *         
*        SORT INPUT FILE TO FIND LAST COPY OF EACH INVOICE            *         
*        ON ADDING TO SORT ADD TO COUNTERS                            *         
*              ONE COUNTS HEADER RECORDS                              *         
*              ONE COUNTS RECORDS WITHIN INVOICE                      *         
*              FIRST IS SORTED IN DESCENDING ORDER SO FIRST           *         
*                ON OUTPUT IS VERSION WE WANT                         *         
*              SECOND PRESERVES ORIGINAL INPUT ORDER                  *         
*                                                                     *         
*        ALSO WE DROP ANY INVOICES THAT ARE PURELY BUY INFORMATION    *         
*                                                                     *         
*        ALSO WE DROP ANY INVOICES THAT ARE PRIOR TO 1996             *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SORTINI  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9                                                    
         USING WORKSECT,R8                                                      
*                                                                               
         ZAP   SSEQCTR,=P'0'       INIT RECORD COUNTER                          
         ZAP   SHDRCTR,=P'0'       INIT HEADER COUNTER                          
         ZAP   SRECCTR,=P'0'       INIT RECORD COUNTER                          
         MVI   SORTSW,0            INIT SORT SWITCH                             
         ZAP   ERRCTR,=P'0'        INIT ERROR COUNTER                           
*                                                                               
         MVI   SWITCH,0            INIT SORT SWITCH                             
*                                                                               
SORTLOOP DS    0H                                                               
*                                                                               
         L     R2,=A(MMPIN)                                                     
         GET   (R2),SREC           GET FIRST/NEXT RECORD                        
*                                                                               
         AP    SSEQCTR,=P'1'       BUMP SEQUENCE COUNTER                        
*                                                                               
         CLC   =C'A1',SREC         IF AN AGENCY RECORD                          
         BNE   SORTA1N                                                          
*                                                                               
         MVC   SAGY,SREC              SAVE AGENCY INFO                          
         XC    SHDR,SHDR              INIT SORT RECORD                          
         XC    SHDR2,SHDR2                                                      
         MVI   SORTSW,0               INIT SORT SWITCH                          
*                                                                               
         B     SORTCONT               DROP FROM SORT                            
*                                                                               
SORTA1N  DS    0H                                                               
*                                                                               
         CLC   =C'H1',SREC         IF A HEADER RECORD                           
         BNE   SORTH1N                                                          
*                                                                               
         MVI   SORTSW,0               INIT SORT SWITCH                          
*                                                                               
         LHI   RF,MH1NXTRI-MH1REC                                               
         LA    R1,SREC(RF)                                                      
         CLI   0(R1),C'I'                 DROP IF NOT INVOICE HEADER            
         BE    *+12                                                             
         MVI   SORTSW,SSWDROP               SET TO DROP INVOICE                 
         B     SORTCONT                     DROP THIS RECORD                    
*                                                                               
*        FIND DDS CLT/PRD/EST CODES                                             
*                                                                               
         GOTO1 =V(EZMMPCDS),DMCB,MH1ESTNM,DDSCDS,DATAMGR                        
*                                                                               
         MVC   SHDR,SREC              COPY HEADER RECORD                        
         XC    SHDR2,SHDR2                                                      
         AP    SHDRCTR,=P'1'          BUMP HEADER COUNTER                       
         ZAP   SRECCTR,=P'0'          RESET RECORD COUNTER                      
*                                                                               
         B     SORTCONT                                                         
*                                                                               
SORTH1N  DS    0H                                                               
*                                                                               
         CLC   =C'H2',SREC         IF A HEADER RECORD                           
         BNE   SORTH2N                                                          
*                                                                               
         MVC   SHDR2,SREC          COPY HEADER 2 RECORD                         
*                                                                               
SORTH2N  DS    0H                                                               
*                                                                               
         CLI   SORTSW,SSWDROP      IF DROPPING INVOICE                          
         BE    SORTCONT               DON'T ADD TO SORT                         
*                                                                               
         AP    SRECCTR,=P'1'       BUMP RECORD COUNTER                          
*                                                                               
         CLI   SWITCH,0            IF FIRST RECORD TO SORT                      
         BNE   SORTINI1                                                         
*                                                                               
         MVI   SWITCH,X'FF'           INDICATE SOMETHING ADDED TO SORT          
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0   INIT SORT                       
*                                                                               
SORTINI1 DS    0H                                                               
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC    ADD TO SORT                       
*                                                                               
SORTCONT DS    0H                                                               
*                                                                               
         B     SORTLOOP                                                         
*                                                                               
*        INPUT TO SORT COMPLETED                                                
*        NOW START READING SORT OUTPUT                                          
*                                                                               
SORTDONE DS    0H                                                               
*                                                                               
SORTINIX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT - RECH1'         
***********************************************************************         
*                                                                     *         
*        PROCESS HEADER RECORD 1                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RECH1    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9                                                    
         USING WORKSECT,R8                                                      
*                                                                               
         LA    RE,SSEQCTR-SORTREC(R7)                                           
         ZAP   SSEQCTR,0(L'SSEQCTR,RE)                                          
*                                                                               
         USING SORTREC,R7          ESTABLISH RETURNED RECORD                    
*                                                                               
         MVC   MA1REC(L'SAGY),SAGY SAVE AGENCY DATA                             
*                                                                               
         CLC   =C'H1',SHDR         MUST HAVE AN 'H1' RECORD                     
         BE    RECH120                                                          
*                                                                               
         XC    MH1REC(MH1RECLQ),MH1REC  CLEAR OUT HEADER AREA                   
         XC    MH2REC(MH2RECLQ),MH2REC  CLEAR OUT HEADER AREA                   
         MVC   MI1REC(MI1RECLQ),SREC    COPY INPUT RECORD                       
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGH1NFD,MI1REC,                        X        
               (L'MI1RECID,MI1RECID-MI1REC),=C'I1'                              
*                                                                               
         XC    MI1REC(MI1RECLQ),MI1REC  CLEAR OUT RECORD AREA                   
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RECH1ER                                                          
*                                                                               
MSGH1NFD DC    CL50'H1 TYPE HEADER RECORD MISSING. INVOICE DROPPED.'            
*                                                                               
RECH120  DS    0H                                                               
*                                                                               
         CLC   SHDR,MH1REC         IF A DUPLICATE OF PREVIOUS INVOICE           
         BNE   *+10                                                             
         CLC   SHDR2,MH2REC                                                     
         BNE   *+12                                                             
         MVI   SORTSW,SSWDROP         INDICATE INVOICE TO BE DROPPED            
         B     RECH1X                 DROP THIS RECORD                          
*                                                                               
         MVI   SORTSW,0            RESET SORT SWITCH                            
*                                                                               
         MVC   MH1REC(MH1RECLQ),SHDR  COPY TO HEADER AREA                       
         MVC   MH2REC(MH2RECLQ),SHDR2 COPY TO HEADER AREA                       
*                                                                               
         CLC   MH2RECID,=C'H2'     HEADER 2 RECORD                              
         BE    RECH220                                                          
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGH2NFD,MH2REC,                        X        
               (L'MH2RECID,MH2RECID-MH2REC),=C'H2'                              
*                                                                               
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RECH1ER             MUST BE A HEADER 2 RECORD                    
*                                                                               
MSGH2NFD DC    CL50'H2 TYPE HEADER RECORD MISSING. INVOICE DROPPED.'            
*                                                                               
RECH220  DS    0H                                                               
*                                  SAVE SEQUENCE NUMBER                         
*        PRODUCE AGENCY RECORD IF NEEDED                                        
*                                                                               
         CLC   WRKAGY,MH1AGYNM     SKIP IF ADVERTISER NOT CHANGED               
         BE    RH1AGYX                                                          
*                                                                               
         MVC   WRKAGY,MH1AGYNM     SAVE NEW ADVERTISER NAME                     
*                                                                               
         XC    E21REC(E21RECLQ),E21REC INIT AGENCY RECORD                       
*                                                                               
         LA    RF,E21RECLQ         INIT RECORD LENGTH                           
         STCM  RF,3,E21RECLN                                                    
*                                                                               
         MVC   E21RECID,=C'21'     SET RECORD ID                                
*                                                                               
         SETEF E21RECID                   SET FIELD SEPARATOR                   
*                                                                               
         SETEF E21AGYID                   SET FIELD SEPARATOR                   
*                                                                               
         MVC   E21AGYNM(L'MH1AGYNM),MH1AGYNM  SET AGENCY NAME                   
         SETEF E21AGYNM                   SET FIELD SEPARATOR                   
*                                                                               
         SETEF E21AGAD1                   SET FIELD SEPARATOR                   
*                                                                               
         SETEF E21AGAD2                   SET FIELD SEPARATOR                   
*                                                                               
         SETEF E21AGAD3                   SET FIELD SEPARATOR                   
*                                                                               
         SETER E21AGAD4                   SET END OF RECORD                     
*                                                                               
         GOTO1 =A(COLLAPSE),DMCB,E21REC   COLLAPSE RECORD                       
*                                                                               
         L     R2,=A(EZOUT)                                                     
         PUT   (R2),E21REC         WRITE RECORD                                 
*                                                                               
RH1AGYX  DS    0H                                                               
*                                                                               
*        PRODUCE STATION RECORD IF NEEDED                                       
*                                                                               
         CLC   WRKSTA,MH1STA       SKIP IF STATION NOT CHANGED                  
         BE    RH1STAX                                                          
*                                                                               
         MVC   WRKSTA,MH1STA       SAVE NEW STATION ID                          
*                                                                               
         XC    E22REC(E22RECLQ),E22REC INIT STATION RECORD                      
*                                                                               
         LA    RF,E22RECLQ         INIT RECORD LENGTH                           
         STCM  RF,3,E22RECLN                                                    
*                                                                               
         MVC   E22RECID,=C'22'     SET RECORD ID                                
*                                                                               
         SETEF E22RECID                   SET FIELD SEPARATOR                   
*                                                                               
*        FIND CALL LETTERS - FORMAT IS KXXX-TV                                  
*                                                                               
         LA    R1,MH1STA           INPUT - KXXX-TV                              
         SR    RF,RF               CHARACTER COUNTER                            
         LA    R0,L'MH1STA         MAX LENGTH STATION ID                        
*                                                                               
         CLI   0(R1),C' '          FIND ' ' SEPARATOR                           
         BE    *+8                                                              
         CLI   0(R1),C'-'          FIND '-' SEPARATOR                           
         BE    *+16                                                             
         LA    R1,1(R1)            BUMP COUNTERS                                
         LA    RF,1(RF)                                                         
         BCT   R0,*-24                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   E22STA(0),MH1STA    PASS STATION CALL LETTERS                    
*                                                                               
         CLI   E22STA,C'!'         SKIP IF NORMAL STATION                       
         BNE   RH1CLLX                                                          
*                                                                               
         L     RE,=A(STATAB)       POINT TO STATION TABLE                       
         BCTR  RF,0                DECREMENT CALL LETTERS LENGTH AGAIN          
*                                                                               
RH1CLLLP DS    0H                                                               
*                                                                               
         CLI   0(RE),X'FF'         DONE AT END OF TABLE                         
         BE    RH1CLLDN                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   E22STA+1(0),1(RE)   MATCH CALL LETTERS                           
         BE    RH1CLLFD                                                         
*                                                                               
RH1CLLCN DS    0H                                                               
*                                                                               
         LA    RE,STATABLQ(RE)     BUMP TO NEXT ENTRY IN TABLE                  
         B     RH1CLLLP                                                         
*                                                                               
RH1CLLFD DS    0H                                                               
*                                                                               
         LA    RF,1(RF)            RESTORE EXECUTE LENGTH                       
         MVC   E22STA(1),0(RE)     REPLACE CALL LETTER                          
         B     RH1LWPX                                                          
*                                                                               
RH1CLLDN DS    0H                  UNKNOWN SPILL STATION                        
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGSTANF,MH1REC,                        X        
               (L'MH1STA,MH1STA-MH1REC),=C'H1'                                  
*                                                                               
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RH1LWPX                                                          
*                                                                               
MSGSTANF DC    CL50'SPILL STATION NOT KNOWN. INVOICE DROPPED.'                  
*                                                                               
RH1CLLX  DS    0H                                                               
*                                                                               
*        CHECK FOR LOW POWER STATIONS                                           
*                                                                               
         L     RE,=A(STATB2)       POINT TO LOW POWER STATION TABLE             
*                                                                               
RH1LWPLP DS    0H                                                               
*                                                                               
         CLI   0(RE),X'FF'         DONE AT END OF TABLE                         
         BE    RH1LWPDN                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   E22STA(0),0(RE)     MATCH CALL LETTERS                           
         BE    RH1LWPFD                                                         
*                                                                               
RH1LWPCN DS    0H                                                               
*                                                                               
         LA    RE,STATB2LQ(RE)     BUMP TO NEXT ENTRY IN TABLE                  
         B     RH1LWPLP                                                         
*                                                                               
RH1LWPFD DS    0H                                                               
*                                                                               
         MVC   E22STA(4),4(RE)     REPLACE CALL LETTERS                         
         LA    R1,8(RE)            POINT TO C'-',MEDIA                          
*                                                                               
RH1LWPDN DS    0H                                                               
*                                                                               
RH1LWPX  DS    0H                                                               
*                                                                               
         SETEF E22STA                     SET FIELD SEPARATOR                   
*                                                                               
         CLI   0(R1),C'-'          NO BAND IF NO '-' FOUND                      
         BNE   RH1MEDBN                                                         
*                                                                               
         CLC   =C'L ',1(R1)        IF LOW POWER TV                              
         BE    *+10                                                             
         CLC   =C'TV',1(R1)        IF TV                                        
         BNE   RH1MEDTN                                                         
*                                                                               
         MVC   E22MED,=C'TV'          SET MEDIA TV                              
         SETEF E22MED                                                           
*                                                                               
         CLC   =C'TV',1(R1)        IF NOT TV                                    
         BE    RH1MEDT1                                                         
*                                                                               
         MVC   E22BAND,1(R1)          SET BAND                                  
*                                                                               
RH1MEDT1 DS    0H                                                               
*                                                                               
         SETEF E22BAND                NO BAND                                   
*                                                                               
         B     RH1MEDX                                                          
*                                                                               
RH1MEDTN DS    0H                                                               
*                                                                               
         CLC   =C'AM',1(R1)        IF AM                                        
         BE    *+10                                                             
         CLC   =C'FM',1(R1)        OR FM                                        
         BE    *+10                                                             
         CLC   =C'CO',1(R1)        OR FM                                        
         BE    *+10                                                             
         CLC   =C'FF',1(R1)        OR FM                                        
         BE    *+10                                                             
         CLC   =C'AF',1(R1)        OR FM                                        
         BNE   RH1MEDRN                                                         
*                                                                               
         MVC   E22MED,=C'R '          SET MEDIA RADIO                           
         SETEF E22MED                                                           
*                                                                               
         MVC   E22BAND,1(R1)          SET BAND                                  
*                                                                               
         CLC   =C'CO',1(R1)        IF CO                                        
         BE    *+10                                                             
         CLC   =C'AF',1(R1)        OR AF                                        
         BE    *+10                                                             
         CLC   =C'FF',1(R1)        OR FF                                        
         BNE   *+10                                                             
         MVC   E22BAND,=C'FM'        ASSUME FM                                  
*                                                                               
         SETEF E22BAND                                                          
*                                                                               
         B     RH1MEDX                                                          
*                                                                               
RH1MEDRN DS    0H                                                               
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGBBDNV,MH1REC,                        X        
               (L'MH1STA,MH1STA-MH1REC),=C'H1'                                  
*                                                                               
         B     RH1MEDBN                                                         
*                                                                               
MSGBBDNV DC    CL50'STATION BROADCAST BAND NOT VALID. ASSUMING TV'              
*                                                                               
RH1MEDBN DS    0H                  NO BAND SPECIFIED - DEFAULT TO TV            
*                                                                               
         MVC   E22MED,=C'TV'          SET MEDIA TV AS DEFAULT                   
         SETEF E22MED                                                           
*                                                                               
         SETEF E22BAND                NO BAND                                   
*                                                                               
RH1MEDX  DS    0H                                                               
*                                                                               
         SETEF E22STANM                   SET FIELD SEPARATOR                   
*                                                                               
         SETEF E22STAD1                   SET FIELD SEPARATOR                   
*                                                                               
         SETEF E22STAD2                   SET FIELD SEPARATOR                   
*                                                                               
         SETEF E22STAD3                   SET FIELD SEPARATOR                   
*                                                                               
         SETEF E22STAD4                   SET FIELD SEPARATOR                   
*                                                                               
         SETER E22STSYS                   SET FIELD SEPARATOR                   
*                                                                               
         GOTO1 =A(COLLAPSE),DMCB,E22REC   COLLAPSE RECORD                       
*                                                                               
         L     R2,=A(EZOUT)                                                     
         PUT   (R2),E22REC         WRITE RECORD                                 
*                                                                               
RH1STAX  DS    0H                                                               
*                                                                               
*        YEAR IS OFTEN MISSING OR SENT AS ONLY LAST DIGIT OF YEAR               
*        IN FORMER CASE USE FLIGHT START YEAR (COMES AS LAST DIGIT)             
*        IF LAST DIGIT THEN ADD IN TENS DIGIT                                   
*                                                                               
         MVC   WRKFLTYR,SPACES         INIT FLIGHT START YEAR                   
*                                                                               
         MVC   WRKFLTYR(1),MH1FLTST+4  USE  FLIGHT START YEAR                   
*                                                                               
         CLI   WRKFLTYR,C' '       USE IT IF PRESENT                            
         BH    *+10                                                             
         MVC   WRKFLTYR,MH1FLTYR      ELSE COPY FLIGHT YEAR                     
*                                                                               
         TSTNM WRKFLTYR            TEST NUMERIC FIELD                           
         BE    RECH1YRX            OKAY                                         
*                                                                               
         CLC   WRKFLTYR,=C'  '     IF NO DATA                                   
*****    BH    *+10                                                             
         MVC   WRKFLTYR(1),MH1FLTST+4  USE FLIGHT START YEAR                    
*                                                                               
         CLI   WRKFLTYR,C' '       IF FIRST CHARACTER IS SPACE                  
         BH    *+14                                                             
         MVC   WRKFLTYR(1),WRKFLTYR+1   USE SECOND CHARACTER                    
         MVI   WRKFLTYR+1,C' '                                                  
*                                                                               
         CLI   WRKFLTYR+1,C' '     IF SECOND POSITION NOT NOW A SPACE           
         BH    ERRFLTYR               CONSIDER IT AN ERROR                      
*                                                                               
         MVC   WRKFLTYR+1(1),WRKFLTYR  MOVE TO ONES DIGIT                       
*                                                                               
         CLI   WRKFLTYR+1,C'0'     ERROR IF NOT NUMERIC                         
         BL    ERRFLTYR                                                         
*                                                                               
         MVI   WRKFLTYR,C'9'          ASSUME NINETIES                           
*                                                                               
         CLI   WRKFLTYR+1,C'6'     CHECK FOR NEXT CENTURY                       
         BNL   *+8                                                              
         MVI   WRKFLTYR,C'0'          NEXT CENTURY                              
*                                                                               
         TSTNM WRKFLTYR            TEST NUMERIC FIELD                           
         BE    RECH1YRX            FINALLY OK                                   
*                                                                               
ERRFLTYR DS    0H                  FLIGHT START YEAR IN ERROR                   
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGFLTYR,MH1REC,                        X        
               (L'MH1FLTYR,MH1FLTYR-MH1REC),=C'H1'                              
*                                                                               
         MVC   WRKFLTYR,=C'00'     MAKE SURE WE HAVE NUMERIC VALUE              
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RECH1YRX                                                         
*                                                                               
MSGFLTYR DC    CL50'FLIGHT START YEAR INVALID. INVOICE DROPPED.'                
*                                                                               
RECH1YRX DS    0H                                                               
*                                                                               
         TSTNM MH1FLTEN            TEST NUMERIC FIELD                           
         BE    RECH1ENX            FINALLY OK                                   
*                                                                               
         GOTO1 =A(PUTERR),DMCB,MSGFLTEN,MH1REC,                        X        
               (L'MH1FLTEN,MH1FLTEN-MH1REC),=C'H1'                              
*                                                                               
         MVI   SORTSW,SSWDROP      DROP INVOICE                                 
*                                                                               
         B     RECH1ENX                                                         
*                                                                               
MSGFLTEN DC    CL50'FLIGHT END DATE INVALID. INVOICE DROPPED.'                  
*                                                                               
RECH1ENX DS    0H                                                               
*                                                                               
RECH1X   DS    0H                                                               
         CR    RB,RB               SET EQUAL CC                                 
         XIT1                                                                   
*                                                                               
RECH1ER  DS    0H                                                               
         LTR   RB,RB               UNEQUSL CC                                   
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
        TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT-COLLAPSE'         
***********************************************************************         
*                                                                     *         
*        STRIP EXCESS NULLS AND SPACES FROM RECORD                    *         
*                                                                     *         
*NTRY    P0    A(RECORD TO BE COLLAPSED                               *         
*                                                                     *         
*                                                                     *         
*        COLLAPSE TO RIGHT AND THEN REPOSITION ON LEFT                *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
COLLAPSE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9                                                    
         USING WORKSECT,R8                                                      
*                                                                               
         LR    R6,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         L     R5,0(R6)            POINT TO RECORD                              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,0(R5)          RECORD LENGTH                                
         BZ    COLLAPSX            NOTHING TO COLLAPSE                          
*                                                                               
         SH    RF,=H'4'            VARIABLE PART LENGTH                         
         BNP   COLLAPSX            NOTHING TO COLLAPSE                          
*                                                                               
         LR    R0,RF               SAVE VARIABLE LENGTH                         
         LA    R2,3(RF,R5)         POINT TO END OF RECORD                       
         LR    R3,R2               COPY POINTER                                 
         SR    R4,R4               RETAIN DATA SWITCH                           
*                                                                               
COLLLOOP DS    0H                                                               
*                                                                               
         CLI   0(R2),EOF           IF END OF FIELD                              
         BE    *+8                                                              
         CLI   0(R2),EOR           OR END OF RECORD                             
         BNE   *+10                                                             
         SR    R4,R4                  STOP MOVING ALL DATA                      
         B     COLLKEEP               KEEP THIS DATA IN RECORD                  
*                                                                               
         LTR   R4,R4               SKIP IF KEEPING ALL DATA                     
         BNZ   COLLKEEP                                                         
*                                                                               
         CLI   0(R2),0             IF NULLS                                     
         BE    *+8                                                              
         CLI   0(R2),C' '          OR SPACE                                     
         BE    COLLCONT            DROP DATA                                    
*                                                                               
*                                  ACTUAL DATA                                  
*                                                                               
         LA    R4,1                SET MOVE ALL DATA SWITCH                     
*                                                                               
COLLKEEP DS    0H                  MOVE DATA TO NEW RECORD                      
*                                                                               
         MVC   0(1,R3),0(R2)       KEEP DATA IN RECORD                          
         BCTR  R3,0                DECREMENT POINTER                            
*                                                                               
COLLCONT DS    0H                                                               
*                                                                               
         BCTR  R2,0                DECREMENT POINTER                            
         BCT   R0,COLLLOOP                                                      
*                                                                               
COLLDONE DS    0H                                                               
*                                                                               
         LA    RF,3(RF,R5)         POINT TO END OF RECORD                       
         SR    RF,R3               LENGTH OF NEW RECORD                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R5),1(R3)       LEFT JUSTIFY NEW RECORD                      
*                                                                               
         LA    RF,5(RF)            NEW RECORD LENGTH                            
         STCM  RF,3,0(R5)          SET NEW RECORD LENGTH                        
*                                                                               
COLLAPSX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT-TOTALS'          
***********************************************************************         
*                                                                     *         
*        PUT OUT A TOTALS RECORD                                      *         
*                                                                     *         
*NTRY    WORK FIELDS HAVE DATA FOR SUMMARY RECORDS                    *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TOTALS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9                                                    
         USING WORKSECT,R8                                                      
*                                                                               
*        PRODUCE INVOICE DETAIL RECORD                                          
*                                                                               
         XC    E34REC(E34RECLQ),E34REC INIT INVOICE DETAIL RECORD               
*                                                                               
         LA    RF,E34RECLQ         INIT RECORD LENGTH                           
         STCM  RF,3,E34RECLN                                                    
*                                                                               
         MVC   E34RECID,=C'34'     SET RECORD ID                                
*                                                                               
         SETEF E34RECID            SET EOF                                      
*                                                                               
         SETEF E34INVCF                                                         
*                                                                               
         EDIT  (P8,WRKGRS),(11,E34INVGR),0,ALIGN=LEFT,FLOAT=-                   
         SETEF E34INVGR                                                         
*                                                                               
         ZAP   WRKAC,WRKGRS        GET AGENCY COMMISSION                        
         SP    WRKAC,WRKNET                                                     
         EDIT  (P8,WRKAC),(11,E34INVAC),0,ALIGN=LEFT,FLOAT=-                    
         SETEF E34INVAC                                                         
*                                                                               
         EDIT  (P8,WRKNET),(11,E34INVNT),0,ALIGN=LEFT,FLOAT=-                   
         SETEF E34INVNT                                                         
*                                                                               
         SETEF E34RECDR                                                         
*                                                                               
         SETEF E34RECCR                                                         
*                                                                               
         SETEF E34RECTL                                                         
*                                                                               
         SETEF E34STTAX                                                         
*                                                                               
         SETEF E34LCTAX                                                         
*                                                                               
         SETEF E34PRRGR                                                         
*                                                                               
         SETEF E34PRRNT                                                         
*                                                                               
         EDIT  (P8,WRKSPTS),(11,E34SPTS),0,ALIGN=LEFT,FLOAT=-                   
         SETEF E34SPTS                                                          
*                                                                               
         SETER E34GST                                                           
*                                                                               
         GOTO1 =A(COLLAPSE),DMCB,E34REC   COLLAPSE RECORD                       
*                                                                               
         L     R2,=A(EZOUT)                                                     
         PUT   (R2),E34REC                                                      
*                                                                               
         ZAP   WRKGRS,=P'0'        RESET ACCUMULATORS                           
         ZAP   WRKNET,=P'0'                                                     
         ZAP   WRKAC,=P'0'                                                      
         ZAP   WRKSPTS,=P'0'                                                    
*                                                                               
TOTALSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT-NUMTRAN'         
***********************************************************************         
*                                                                     *         
*        CONVERT CHARACTER NUMBER TO PACKED                           *         
*                                                                     *         
*NTRY    P0+0  AL1(LENGTH OF INPUT)                                   *         
*        P0    A(FIELD TO BE CONVERTD)                                *         
*              2 DECIMALS ASSUMED                                     *         
*                                                                     *         
*EXIT    DUB   PACKED NUMBER                                          *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NUMTRAN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9                                                    
         USING WORKSECT,R8                                                      
*                                                                               
         ZAP   DUB,=P'0'           INIT OUTPUT                                  
         MVI   SIGN,0              INIT SIGN                                    
         XC    DATASTRT,DATASTRT   INIT DATA START SWITCH                       
*                                                                               
         L     RF,0(R1)            START OF INPUT                               
         LA    RF,0(RF)            CLEARS HIGH ORDER BYTE                       
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)          LENGTH OF INPUT                              
         BZ    NTRERR                                                           
*                                                                               
NTRLOOP  DS    0H                                                               
*                                                                               
         CLI   0(RF),C'-'          CHECK FOR MINUS SIGN                         
         BNE   *+8                                                              
         MVI   SIGN,C'-'                                                        
*                                                                               
         OC    DATASTRT,DATASTRT   IF NO DATA FOUND YET                         
         BNZ   *+16                                                             
         CLI   0(RF),C' '             LOOK FOR NON-SPACE                        
         BNH   *+8                                                              
         ST    RF,DATASTRT            SET START OF DATA                         
*                                                                               
         CLI   0(RF),C'.'          FIND DECIMAL POINT                           
         BE    NTRDONE1                                                         
         CLI   0(RF),C' '          OR END OF FIELD                              
         BNH   NTRDONE1                                                         
*                                                                               
         CLI   0(RF),C'0'          MUST BE A NUMBER                             
         BL    NTRERR                                                           
         CLI   0(RF),C'9'                                                       
         BH    NTRERR                                                           
*                                                                               
NTRCONT1 DS    0H                                                               
*                                                                               
         LA    RF,1(RF)                                                         
         BCT   R0,NTRLOOP                                                       
*                                                                               
NTRDONE1 DS    0H                                                               
*                                                                               
         OC    DATASTRT,DATASTRT   DONE IF NO DATA FOUND                        
         BZ    NTRDONE4                                                         
*                                                                               
         LR    RE,RF               COPY POINTER                                 
         L     R4,DATASTRT         GET FIELD START                              
         LA    R4,0(R4)            CLEARS HIGH ORDER BYTE                       
*                                                                               
         SR    RE,R4               LENGTH OF DOLLARS                            
         BNP   NTRDONE2            NO DOLLARS                                   
*                                                                               
         BCTR  RE,0                DECREMENT FOR EXECUTE                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)         DOLLARS                                      
*                                                                               
         SRP   DUB,2,5             *100                                         
*                                                                               
NTRDONE2 DS    0H                                                               
*                                                                               
         CLI   0(RF),C'.'          DONE IF NOT A DECIMAL POINT                  
         BNE   NTRDONE3                                                         
*                                                                               
         MVC   HALF,1(RF)          GET PENNIES                                  
         OC    HALF,=C'00'         FORCE TO BE NUMERIC                          
         PACK  FULL,HALF                                                        
         AP    DUB,FULL            ADD IN PENNIES                               
*                                                                               
NTRDONE3 DS    0H                                                               
*                                                                               
         CLI   SIGN,C'-'           IF MINUS SIGN FOUND                          
         BNE   NTRDONE4                                                         
*                                                                               
         ZAP   DUB2,DUB               COPY NUMBER                               
         SP    DUB,DUB2                                                         
         SP    DUB,DUB2               MAKE NUMBER NEGATIVE                      
*                                                                               
NTRDONE4 DS    0H                                                               
*                                                                               
         CR    RB,RB               SET =CC                                      
*                                                                               
         B     NUMTRANX                                                         
*                                                                               
NTRERR   DS    0H                  NOT NUMERIC                                  
         LTR   RB,RB               SET NE CC                                    
*                                                                               
NUMTRANX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT-PUTERR'          
***********************************************************************         
*                                                                     *         
*        PUT OUT RECORDS TO ERROR FILE                                *         
*                                                                     *         
*NTRY    P0    A(ERROR MESSAGE)                                       *         
*        P1    A(RECORD)                                              *         
*        P2+0  FIELD LENGTH                                           *         
*        P2    DISPLACEMENT TO FIELD IN ERROR                         *         
*        P3    ID OF RECORD IN ERROR                                  *         
*                                                                     *         
*EXIT    ERROR RECORD PRINTED                                         *         
*        COPY OF RECORD IN ERROR PRINTED                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
PUTERR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9                                                    
         USING WORKSECT,R8                                                      
*                                                                               
         LR    R3,R1               COPY PARAMETER LIST POINTER                  
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT-PEREMOT'         
***********************************************************************         
*                                                                     *         
*        INITIALIZE REMOTE AREA                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PEREMOT  EQU   *                                                                
*                                                                               
         CP    ERRCTR,=P'0'        SKIP IF NOT FIRST ERROR                      
         BNE   PEREMOTX                                                         
*                                                                               
         ICM   RF,15,VMASTC        POINT TO MASTC                               
*                                                                               
         ICM   R1,15,MCVREMOT-MASTD(RF) ESTABLISH REMOTE AREA                   
         USING REMOTED,R1                                                       
*                                                                               
         MVC   REMOTDSC(L'TITLE),TITLE    SET REPORT ID                         
*                                                                               
         MVC   REMOTJID,=C'ERR'                                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
PEREMOTX EQU   *                                                                
*                                                                               
         AP    ERRCTR,=P'1'        BUMP ERROR COUNTER                           
*                                                                               
         XC    ERRREC,ERRREC       INIT ERROR RECORD                            
*                                                                               
*        COLLECT ERROR DATA                                                     
*                                                                               
         L     RF,0(R3)            POINT TO ERROR MESSAGE                       
         MVC   ERRMSG,0(RF)        GET ERROR MESSAGE                            
*                                                                               
         MVC   ERRFLDD,10(R3)      FIELD DISPLACEMENT                           
         MVC   ERRFLDL,8(R3)       FIELD LENGTH                                 
*                                                                               
         ZAP   ERRSEQ,SSEQCTR      RECORD SEQUENCE NUMBER                       
*                                                                               
         L     R4,8(R3)            POINT TO RECORD ID                           
         MVC   ERRID,0(R4)         PASS ID OF RECORD IN ERROR                   
*                                                                               
*        PRINT ERROR MESSAGE AND DETAILS                                        
*                                                                               
         CLI   LINE,45             CHECK FOR ROOM ON THE PAGE                   
         BNH   *+8                                                              
         MVI   LINE,100            FORCE NEW PAGE                               
*                                                                               
         MVC   PERRT,=C'**ERROR**'                                              
         MVC   PERRMSG,ERRMSG      PRINT ERROR MESSAGE                          
         MVC   PSQNT,=C'RECORD NO. ='                                           
         EDIT  (P8,ERRSEQ),(8,PSQN),ALIGN=LEFT  PRINT SQN                       
*                                                                               
         GOTO1 REPORT              PRINT IT                                     
*                                                                               
         MVC   PERRREC,MA1REC      PRINT FIRST HALF OF A1 RECORD                
*                                                                               
         GOTO1 REPORT              PRINT IT                                     
*                                                                               
         CLC   ERRID,=C'H1'        SKIP IF ERROR IN H1 RECORD                   
         BE    PERPTERR                                                         
*                                                                               
         MVC   PERRREC,MH1REC      PRINT FIRST HALF OF H1 RECORD                
*                                                                               
         GOTO1 REPORT              PRINT IT                                     
*                                                                               
         MVC   PERRREC,MH1REC+128  PRINT SECOND HALF OF H1 RECORD               
*                                                                               
         GOTO1 REPORT              PRINT IT                                     
*                                                                               
         CLC   ERRID,=C'H2'        SKIP IF ERROR IN H2 RECORD                   
         BE    PERPTERR                                                         
*                                                                               
         MVC   PERRREC,MH2REC      PRINT FIRST HALF OF H2 RECORD                
*                                                                               
         GOTO1 REPORT              PRINT IT                                     
*                                                                               
         MVC   PERRREC,MH2REC+128  PRINT SECOND HALF OF H2 RECORD               
*                                                                               
         GOTO1 REPORT              PRINT IT                                     
*                                                                               
PERPTERR DS    0H                                                               
*                                                                               
         MVC   ERRSPACS,BLNKS      INIT PRINT AREA                              
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,ERRFLDD           GET FIELD DISPLACEMENT                    
         LA    RE,ERRSPACS(RE)        POINT TO SPOT IN RECORD                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,ERRFLDL           GET FIELD LENGTH                          
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),=64C'X'        HIGHLIGHT ERROR                           
*                                                                               
         L     R4,4(R3)            POINT TO RECORD IN ERROR                     
         MVC   PERRREC,0(R4)       PRINT FIRST HALF OF RECORD                   
*                                                                               
         GOTO1 REPORT              PRINT IT                                     
*                                                                               
         CLC   ERRSPACS(L'PERRREC),BLNKS SKIP LINE IF ALL SPACES                
         BE    PERR20                                                           
*                                                                               
         MVC   PERRREC,ERRSPACS    PRINT IDENTIFIER FOR ERROR                   
*                                                                               
         GOTO1 REPORT              PRINT IT                                     
*                                                                               
PERR20   DS    0H                                                               
*                                                                               
         MVC   PERRREC,128(R4)     PRINT SECOND HALF OF RECORD                  
*                                                                               
         GOTO1 REPORT              PRINT IT                                     
*                                                                               
         CLC   ERRSPACS+128(L'PERRREC),BLNKS SKIP LINE IF ALL SPACES            
         BE    PERR30                                                           
*                                                                               
         MVC   PERRREC,ERRSPACS+128    PRINT IDENTIFIER FOR ERROR               
*                                                                               
         GOTO1 REPORT              PRINT IT                                     
*                                                                               
PERR30   DS    0H                                                               
*                                                                               
         XC    P,P                                                              
         MVI   P,X'41'             FORCE A SEPARATION LINE                      
         GOTO1 REPORT              PRINT IT                                     
*                                                                               
PUTERRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
TITLE    DC    C'COKE ERRORS'                                                   
BLNKS    DC    CL256' '            SPACES                                       
*                                                                               
         LTORG                                                                  
*                                                                               
ERRREC   DS    0C                  ERROR RECORD                                 
ERRMSG   DS    CL50                ERROR MESSAGE                                
ERRFLDD  DS    XL2                 FIELD DISPLACEMENT                           
ERRFLDL  DS    XL1                 FIELD LENGTH                                 
ERRSEQ   DS    PL8                 RECORD SEQUENCE NUMBER                       
ERRID    DS    CL2                 ID OF RECORD IN ERROR                        
         DS    XL(256-(*-ERRREC))  SPARE                                        
*                                                                               
ERRSPACS DS    CL256               ERROR WORKAREA                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT-DCBS'            
***********************************************************************         
*                                                                     *         
*        DCBS                                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MMPIN    DCB   DDNAME=MMPIN,                                           X        
               DSORG=PS,                                               X        
               MACRF=GM,                                               X        
               EODAD=SORTDONE                                                   
EZOUT    DCB   DDNAME=EZOUT,                                           X        
               DSORG=PS,                                               X        
               MACRF=PT                                                         
PRINTOUT DCB   DDNAME=PRINTOUT,                                        X        
               DSORG=PS,                                               X        
               MACRF=PT                                                         
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT-WORKC'           
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKSECT DS    0D                                                               
*                                                                               
         TITLE 'SPREPMQ02-CONVERT MMPLUS DATA TO EASI FORMAT-CONSTANTS'         
***********************************************************************         
*                                                                     *         
*        CONSTANTS                                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EOF      EQU   X'5E'              END OF FIELD  CHARACTER - SEMI COLON          
EOR      EQU   X'15'              END OF RECORD CHARACTER                       
DATVAL   DC    V(DATVAL)                                                        
SORTER   DC    V(SORTER)                                                        
VTIMVAL  DC    V(TIMVAL)                                                        
VNUMVAL  DC    V(NUMVAL)                                                        
SORTCARD DC    CL80'SORT FIELDS=(1,544,BI,A,545,8,BI,D,553,8,BI,A)'             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=824'                                   
*                                                                               
ZEROS    DC    16C'0'                                                           
REGSAVE  DS    18F                 REGISTER SAVEAREA                            
*                                                                               
*                                                                               
DUB2     DS    D                                                                
SIGN     DS    XL1                 MINUS SIGN HOLDER                            
SWITCH   DS    XL1                 X'FF' - SOMETHING SENT TO SORT               
PRINTSW  DS    XL1                 X'FF' - PRINTOUT OPENED                      
         DS    XL5                 SPARE                                        
NUMWORK  DS    XL16                NUMERIC TEST WORKAREA                        
*                                                                               
DATASTRT DS    A                   START OF DATA IN NUMTRAN                     
*                                                                               
DDSCDS   DS    0C                  DDS CLT/PRD/EST CDS                          
*                                  RETURNED BY EZMMPCDS                         
DDSCLT   DS    CL3                 CLIENT                                       
DDSPRD   DS    CL3                 PRODUCT                                      
DDSEST   DS    CL3                 ESTIMATE                                     
*                                                                               
BUFFSTRT DS    A                   A(INVOICE DETAIL BUFFER)                     
BUFFLEN  DS    F                   BUFFER LENGTH                                
*                                                                               
SORTSW   DS    X                   SORT SWITCH                                  
SSWDROP  EQU   1                   DROP INVOICE FROM SORT                       
*                                                                               
WRKAGY   DS    CL(L'MH1AGYNM)      AGENCY NAME SAVEAREA                         
WRKSTA   DS    CL(L'MH1STA)        STATION CALL LETTERS SAVEAREA                
*                                                                               
WRKBMNXT DS    XL12                EXTRA BROADCAST MONTH TO DRIVE LOGIC         
WRKBMONS DS    14CL12              BROADCAST MONTHS - START AND END             
*                                                                               
WRKNUM   DS    CL16                WORK AREA FOR NUMBERS                        
WRKENDDT DS    CL6                 WORK AREA FOR DATES                          
WRKFLTST DS    CL6                 WORK AREA FOR DATES                          
WRKFLTEN DS    CL6                 WORK AREA FOR DATES                          
WRKNXTYY DS    CL2                 WORK AREA FOR DATES-NEXT YEAR                
WRKLSTYY DS    CL2                 WORK AREA FOR DATES-LAST YEAR                
WRKDATE  DS    CL6                 WORK AREA FOR DATES                          
         DS    C'/'                FOR TRANSLATION                              
WRKDATE1 DS    CL8                 WORK AREA FOR DATES MM/DD/YY                 
WRKSTRT  DS    CL6                 WORK AREA FOR DATES                          
WRKFLTYR DS    CL2                 WORK AREA FOR FLIGHT YEAR                    
*                                                                               
WRKGRS   DS    PL8                 GROSS ACCUMULATOR                            
WRKNET   DS    PL8                 NET   ACCUMULATOR                            
WRKAC    DS    PL8                 AGENCY COMMISSION ACCUMULATOR                
WRKSTTAX DS    PL8                 STATE TAX         ACCUMULATOR                
WRKSPTS  DS    PL8                 SPOTS             ACCUMULATOR                
*                                                                               
ERRCTR   DS    PL8                 ERROR COUNTER                                
*                                                                               
WRKINV#  DS    PL8                 WORK INVOICE NUMBER                          
TODAYDDS DS    CL6                 TODAY YYMMDD- DDS INTERNAL FORMAT            
TODAYC   DS    CL6                 TODAY YYMMDD- PRINTABLE FORMAT               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        MMPLUS INPUT RECORDS SAVEAREAS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
*        AGENCY RECORD                                                          
*                                                                               
MA1REC   DS    0X                  AGENCY RECORD                                
MA1RECID DS    CL2'A1'             AGENCY IDENTIFIER                            
MA1DDSCL DS    CL3                 DDS CLIENT CODE                              
MA1XXXX  DS    CL2                 SOME OTHER FIELD                             
         DS    CL247               SPARE                                        
*                                                                               
*        FIRST HEADER RECORD                                                    
*                                                                               
MH1REC   DS    0X                  HEADER 1 RECORD                              
MH1RECID DS    CL2'H1'             HEADER 1 IDENTIFIER                          
MH1MKTNM DS    CL36                MARKET NAME                                  
MH1MKT   DS    CL4                 MARKET NUMBER (NNNC) NUMBER PLUS CH          
MH1ADVNM DS    CL30                ADVERTISER NAME                              
MH1ADVCD DS    CL10                ADVERTISER CODE                              
MH1PRDNM DS    CL20                PRODUCT NAME                                 
MH1PRDCD DS    CL5                 PRODUCT CODE                                 
MH1CMPNM DS    CL20                CAMPAIGN                                     
MH1STA   DS    CL8                 STATION KXXX-TV                              
MH1FLTST DS    CL5                 FLIGHT START DATE - MMDDY                    
MH1FLTEN DS    CL5                 FLIGHT END   DATE - MMDDY                    
MH1FLTYR DS    CL2                 FLIGHT START YEAR - YY                       
MH1FLTTY DS    CL1                 FLIGHT TYPE - LONG OR SHORT                  
MH1ESTNM DS    CL16                ESTIMATE NUMBER                              
MH1CONNM DS    CL16                CONTRACT NUMBER                              
MH1JOBNM DS    CL16                JOB ORDER NUMBER                             
MH1FILRF DS    CL4                 FILE REFERENCE NUMBER                        
MH1DELFG DS    CL2                 DELETED FLAG - 99                            
MH1FLTRV DS    CL2                 FLIGHT REVISION NUMBER                       
MH1NXTRI DS    CL1                 NEXT RECORD TYPE FOLLOWING B OR I            
MH1AGYNM DS    CL25                AGENCY NAME                                  
MH1BUYER DS    CL25                BUYER                                        
MH1FLTSD DS    CL1                 FLIGHT START DAY M=0, SU=6                   
MH1RECLQ EQU   *-MH1REC            H1 RECORD LENGTH                             
*                                                                               
*        SECOND HEADER RECORD                                                   
*                                                                               
MH2REC   DS    0X                  HEADER 2 RECORD                              
MH2RECID DS    CL2'H2'             HEADER 2 IDENTIFIER                          
MH2PER#  DS    CL2                 NUMBER OF PERIODS                            
MH2BLLEN DS    12CL6               BILLING PERIODS END DATES - MMDDYY           
MH2STARP DS    CL24                STATION REP                                  
MH2DEMNM DS    6CL8                DEMO NAMES- CCCCCCCT T=R/O                   
MH2MMWKS DS    12ZL1               NUMBER OF WEEKS IN MONTH                     
MH2TAXRT DS    CL6                 TAX RATE - INCLUDES DECIMAL                  
MH2CLDSC DS    CL6                 CLIENT DISCOUNT RATE                         
MH2AGNET DS    CL6                 AGENCY NET      RATE                         
MH2INVRV DS    CL24                INVOICE REVISION NUMBER                      
MH2INV#  DS    CL10                INVOICE NUMBER                               
MH2INVDT DS    CL10                INVOICE DATE MM-DD-YY                        
MH2FSCDT DS    CL10                FISCAL DATE  MM-DD-YY                        
MH2MONTH DS    CL6                 INVOICE MONTH AND YEAR    MMM'YY             
MH2ESHR  DS    CL6                 ESTIMATED SHARE SWEEP                        
MH2ASHR  DS    CL6                 ACTUAL    SHARE SWEEP                        
MH2NUSER DS    CL2                 NETWORK USER NUMBER                          
         DS    CL4                 NOT USED                                     
MH2RECLQ EQU   *-MH2REC            SECOND HEADER LENGTH                         
*                                                                               
*        I1 - INVOICE DETAIL RECORD                                             
*                                                                               
MI1REC   DS    0X                  INVOICE DETAIL RECORD                        
MI1RECID DS    CL2'I1'             INVOICE DETAIL IDENTIFIER                    
MI1BUY#  DS    CL3                 BUY ITEM NUMBER                              
MI1DAY   DS    CL3                 DAY OF WEEK                                  
MI1DATE  DS    CL6                 DATE - MMDD                                  
MI1BRDWK DS    CL1                 BROADCAST WEEK OF MONTH                      
MI1DAYCD DS    CL1                 DAY OF WEEK CODE 0-6                         
MI1TIME  DS    CL5                 TIME HHMMA/P                                 
MI1ISCI  DS    CL4                 ISCI CODE                                    
MI1COM#  DS    CL4                 COMMERCIAL NUMBER                            
MI1PRDCD DS    CL2                 PRODUCR CODE                                 
MI1TAG   DS    CL3                 TAG                                          
MI1LEN   DS    ZL3                 LENGTH                                       
MI1BLLGR DS    CL10                BILLED GROSS'                                
MI1SCHDV DS    CL2                 SCHEDULE DEVIATION CODE                      
MI1APRGR DS    CL10                APPROVED GROSS                               
MI1RMKS  DS    CL20                REMARKS                                      
MI1REV#  DS    CL2                 RECORD REVISION NUMBER                       
MI1APRNT DS    CL10                APPROVED NET                                 
         DS    CL165               NOT USED                                     
MI1RECLQ EQU   *-MI1REC            INVOICE DETAIL RECORD LENGTH                 
*                                                                               
*        S3 - SUMMARY RECORD                                                    
*                                                                               
MS3REC   DS    0X                  SUMMARY RECORD                               
MS3RECID DS    CL2'S3'             SUMMARY IDENTIFIER                           
MS3SSPTS DS    CL4                 NUMBER OF SPOTS IN SCHEDULE                  
MS3ISPTS DS    CL4                 NUMBER OF SPOTS ON INVOICE                   
MS3SCTOT DS    CL12                TOTAL DOLLARS IN BUY SCHEDULE                
MS3BLLGR DS    CL12                TOTAL BILLED GROSS                           
MS3APPGR DS    CL12                TOTAL APPROVED GROSS                         
MS3APPNT DS    CL12                TOTAL APPROVED NET                           
MS3MKBDG DS    CL12                BUDGET DOLLARS (MARKET)                      
         DS    CL186               NOT USED                                     
MS3RECLQ EQU   *-MS3REC            SUMMARY RECORD LENGTH                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        EASI RECORD LAYOUTS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
*        AGENCY RECORD - 21                                                     
*                                                                               
E21REC   DS    0X                  AGENCY RECORD                                
E21RECLN DS    XL2                 RECORD LENGTH                                
         DS    XL2                 CONVENTIONAL                                 
E21RECID DS    CL2'21'             AGENCY IDENTIFIER                            
         DS    XL1                 EOF                                          
E21AGYID DS    CL2                 AGENCY ID                                    
         DS    XL1                 EOF                                          
E21AGYNM DS    CL25                AGENCY NAME                                  
         DS    XL1                 EOF                                          
E21AGAD1 DS    CL30                AGENCY ADDRESS LINE 1                        
         DS    XL1                 EOF                                          
E21AGAD2 DS    CL30                AGENCY ADDRESS LINE 2                        
         DS    XL1                 EOF                                          
E21AGAD3 DS    CL30                AGENCY ADDRESS LINE 3                        
         DS    XL1                 EOF                                          
E21AGAD4 DS    CL30                AGENCY ADDRESS LINE 4                        
         DS    XL1                 EOR                                          
E21RECLQ EQU   *-E21REC            AGENCY RECORD LENGTH                         
*                                                                               
*        STATION RECORD - 22                                                    
*                                                                               
E22REC   DS    0X                  STATION RECORD                               
E22RECLN DS    XL2                 RECORD LENGTH                                
         DS    XL2                 CONVENTIONAL                                 
E22RECID DS    CL2'22'             STATION IDENTIFIER                           
         DS    XL1                 EOF                                          
E22STA   DS    CL4                 STATION CALL LETTERS                         
         DS    XL1                 EOF                                          
E22MED   DS    CL2                 MEDIA                                        
         DS    XL1                 EOF                                          
E22BAND  DS    CL2                 BAND - AM/FM                                 
         DS    XL1                 EOF                                          
E22STANM DS    CL30                STATION NAME                                 
         DS    XL1                 EOF                                          
E22STAD1 DS    CL30                STATION ADDRESS LINE 1                       
         DS    XL1                 EOF                                          
E22STAD2 DS    CL30                STATION ADDRESS LINE 2                       
         DS    XL1                 EOF                                          
E22STAD3 DS    CL30                STATION ADDRESS LINE 3                       
         DS    XL1                 EOF                                          
E22STAD4 DS    CL30                STATION ADDRESS LINE 4                       
         DS    XL1                 EOR                                          
E22STSYS DS    CL15                STATION COMPUTER SYSTEM                      
         DS    XL1                 EOR                                          
*                                                                               
E22RECLQ EQU   *-E22REC            STATION RECORD LENGTH                        
*                                                                               
*                                                                               
*        STATION RECORD - 22                                                    
*                                                                               
E23REC   DS    0X                  PAYEE RECORD                                 
E23RECLN DS    XL2                 RECORD LENGTH                                
         DS    XL2                 CONVENTIONAL                                 
E23RECID DS    CL2'23'             PAYEE IDENTIFIER                             
         DS    XL1                 EOF                                          
E23PAYNM DS    CL30                PAYEE NAME                                   
         DS    XL1                 EOF                                          
E23PYAD1 DS    CL30                PAYEE ADDRESS LINE 1                         
         DS    XL1                 EOF                                          
E23PYAD2 DS    CL30                PAYEE ADDRESS LINE 2                         
         DS    XL1                 EOF                                          
E23PYAD3 DS    CL30                PAYEE ADDRESS LINE 3                         
         DS    XL1                 EOF                                          
E23PYAD4 DS    CL30                PAYEE ADDRESS LINE 4                         
         DS    XL1                 EOR                                          
*                                                                               
E23RECLQ EQU   *-E23REC            PAYEE RECORD LENGTH                          
*                                                                               
*        INVOICE HEADER RECORD - 31                                             
*                                                                               
E31REC   DS    0X                  INVOICE HEADER RECORD                        
E31RECLN DS    XL2                 RECORD LENGTH                                
         DS    XL2                 CONVENTIONAL                                 
E31RECID DS    CL2'31'             INVOICE HEADER IDENTIFIER                    
         DS    XL1                 EOF                                          
E31REPNM DS    CL25                REP NAME                                     
         DS    XL1                 EOF                                          
E31SLSNM DS    CL25                SALESPERSON NAME                             
         DS    XL1                 EOF                                          
E31ADVNM DS    CL25                ADVERTISER NAME                              
         DS    XL1                 EOF                                          
E31PRDNM DS    CL25                PRODUCT NAME                                 
         DS    XL1                 EOF                                          
E31INVDT DS    CL6                 INVOICE DATE YYMMDD                          
         DS    XL1                 EOF                                          
E31ORDTP DS    CL15                ORDER TYPE                                   
         DS    XL1                 EOF                                          
E31AGEST DS    CL10                AGENCY ESTIMATE CODE                         
         DS    XL1                 EOF                                          
E31INV#  DS    CL10                INVOICE NUMBER                               
         DS    XL1                 EOF                                          
E31BRDMN DS    CL4                 BROADCAST MONTH YYMM                         
         DS    XL1                 EOF                                          
E31PERST DS    CL6                 PERIOD START - YYMMDD                        
         DS    XL1                 EOF                                          
E31PEREN DS    CL6                 PERIOD END   - YYMMDD                        
         DS    XL1                 EOF                                          
E31SCHST DS    CL6                 SCHEDULE START - YYMMDD                      
         DS    XL1                 EOF                                          
E31SCHEN DS    CL6                 SCHEDULE END   - YYMMDD                      
         DS    XL1                 EOF                                          
E31CONST DS    CL6                 CONTRACT START - YYMMDD                      
         DS    XL1                 EOF                                          
E31CONEN DS    CL6                 CONTRACT END   - YMMDD                       
         DS    XL1                 EOF                                          
E31BLLIN DS    CL25                BILLING INSTRUCTIONS                         
         DS    XL1                 EOF                                          
E31RCRD# DS    CL10                RATE CARD NUMBER                             
         DS    XL1                 EOF                                          
E31AGFLG DS    CL1                 AGENCY COMMISSION FLAG                       
         DS    XL1                 EOF                                          
E31STXPC DS    CL10                SALES TAX PER CENT                           
         DS    XL1                 EOF                                          
E31AUDPC DS    CL10                AUDIENCE PER CENT FOR TAX                    
         DS    XL1                 EOF                                          
E31REP#  DS    CL10                REP ORDER NUMBER                             
         DS    XL1                 EOF                                          
E31STA#  DS    CL10                STATION ORDER NUMBER                         
         DS    XL1                 EOF                                          
E31STADV DS    CL8                 STATION ADVERTISER NUMBER                    
         DS    XL1                 EOF                                          
E31AGADV DS    CL8                 AGENCY ADVERTISER NUMBER                     
         DS    XL1                 EOF                                          
E31STPRD DS    CL8                 STATION PRODUCT CODE                         
         DS    XL1                 EOF                                          
E31AGPRD DS    CL8                 AGENCY PRODUCT CODE                          
         DS    XL1                 EOF                                          
E31STCON DS    CL25                STATION CONTACT PERSON                       
         DS    XL1                 EOF                                          
E31AGCON DS    CL25                AGENCY CONTACT PERSON                        
         DS    XL1                 EOF                                          
E31IDUE  DS    CL6                 AGENCY DUE DATE - YYMMDD                     
         DS    XL1                 EOF                                          
E31NTWK  DS    CL4                 NETWORK FOR LOCAL CABLE                      
         DS    XL1                 EOF                                          
E31ACN   DS    CL8                 ACN NUMBER                                   
         DS    XL1                 EOR                                          
*                                                                               
E31RECLQ EQU   *-E31REC            RECORD LENGTH                                
*                                                                               
*        INVOICE DETAIL RECORD - 31                                             
*                                                                               
E51REC   DS    0X                  INVOICE DETAIL RECORD                        
E51RECLN DS    XL2                 RECORD LENGTH                                
         DS    XL2                 CONVENTIONAL                                 
E51RECID DS    CL2'51'             INVOICE DETAIL IDENTIFIER                    
         DS    XL1                 EOF                                          
E51RUNCD DS    CL1                 Y/N SPOT RAN                                 
         DS    XL1                 EOF                                          
E51RUNDT DS    CL6                 RUN DATE  YYMMDD                             
         DS    XL1                 EOF                                          
E51RUNDY DS    CL1                 RUN DAY OF WEEK   1-7                        
         DS    XL1                 EOF                                          
E51RUNTM DS    CL4                 RUN TIME HHMM MILITARY                       
         DS    XL1                 EOF                                          
E51SPTLN DS    ZL3                 SPOT LENGTH                                  
         DS    XL1                 EOF                                          
E51CPYID DS    CL30                ISCI CODES                                   
         DS    XL1                 EOF                                          
E51RATE  DS    CL11                RATE                                         
         DS    XL1                 EOF                                          
E51CLASS DS    CL3                 CLASS                                        
         DS    XL1                 EOF                                          
E51PGY   DS    CL6                 PIGGYBACK SECONDS SPLIT                      
         DS    XL1                 EOF                                          
E51MGDT1 DS    CL6                 MAKEGOOD DATE 1                              
         DS    XL1                 EOF                                          
E51MGDT2 DS    CL6                 MAKEGOOD DATE 2                              
         DS    XL1                 EOF                                          
E51MGTM1 DS    CL4                 MAKEGOOD TIME 1                              
         DS    XL1                 EOF                                          
E51MGTM2 DS    CL4                 MAKEGOOD TIME 2                              
         DS    XL1                 EOF                                          
E51ADJDR DS    CL11                ADJUSTMENT DEBIT                             
         DS    XL1                 EOF                                          
E51ADJCR DS    CL11                ADJUSTMENT CREDIT                            
         DS    XL1                 EOF                                          
E51PROG  DS    CL40                PROGRAM DESCRIPTION                          
         DS    XL1                 EOF                                          
E51BLBID DS    CL1                 BILLBOARD INDICATOR Y/N                      
         DS    XL1                 EOF                                          
E51BLBLN DS    CL3                 BILLBOARD SECONDS                            
         DS    XL1                 EOF                                          
E51BLBVI DS    CL30                BILLBOARD VIDEO ISCI CODES                   
         DS    XL1                 EOF                                          
E51BLBAI DS    CL30                BILLBOARD AUDIO ISCI CODES                   
         DS    XL1                 EOR                                          
*                                                                               
E51RECLQ EQU   *-E51REC            PAYEE RECORD LENGTH                          
*                                                                               
*        INVOICE TOTAL RECORD - 34                                              
*                                                                               
E34REC   DS    0X                  INVOICE TOTAL RECORD                         
E34RECLN DS    XL2                 RECORD LENGTH                                
         DS    XL2                 CONVENTIONAL                                 
E34RECID DS    CL2'54'             INVOICE TOTAL IDENTIFIER                     
         DS    XL1                 EOF                                          
E34INVCF DS    CL11                INVOICE CONFIRMED COST                       
         DS    XL1                 EOF                                          
E34INVGR DS    CL11                INVOICE GROSS                                
         DS    XL1                 EOF                                          
E34INVAC DS    CL11                INVOICE AGENCY COMMISSION                    
         DS    XL1                 EOF                                          
E34INVNT DS    CL11                NET DUE                                      
         DS    XL1                 EOF                                          
E34RECDR DS    CL11                RECONCILIATION DEBIT                         
         DS    XL1                 EOF                                          
E34RECCR DS    CL11                RECONCILIATON CREDIT                         
         DS    XL1                 EOF                                          
E34RECTL DS    CL11                RECONCILIATION TOTAL                         
         DS    XL1                 EOF                                          
E34STTAX DS    CL11                STATE TAX                                    
         DS    XL1                 EOF                                          
E34LCTAX DS    CL11                LOCAL TAX                                    
         DS    XL1                 EOF                                          
E34PRRGR DS    CL11                GROSS PRIOR BALANCE                          
         DS    XL1                 EOF                                          
E34PRRNT DS    CL11                NET   PRIOR BALANCE                          
         DS    XL1                 EOF                                          
E34SPTS  DS    CL5                 NUMBER OF SPOTS                              
         DS    XL1                 EOF                                          
E34GST   DS    CL11                GST                                          
         DS    XL1                 EOR                                          
*                                                                               
E34RECLQ EQU   *-E34REC            PAYEE RECORD LENGTH                          
*                                                                               
ESAVEREC DS    XL2                 E-TYPE RECORD SAVEAREA                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        SORT RECORD                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
SORTREC  DS    0C                  SORT RECORD                                  
SAGY     DS    CL32                1ST 32 BYTES OF AGENCY RECORD                
SHDR     DS    CL256               HEADER RECORD                                
SHDR2    DS    CL256               HEADER2  RECORD                              
SHDRCTR  DS    PL8                 HEADER COUNTER                               
SRECCTR  DS    PL8                 RECORD COUNTER                               
SSEQCTR  DS    PL8                 RECORD SEQUENCE COUNTER                      
SREC     DS    CL512               SORT RECORD                                  
*                                                                               
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT-STATAB'          
***********************************************************************         
*                                                                     *         
*        TABLE OF SPILL STATIONS                                      *         
*        STATIONS COME IN AS !XXX-TV AND ARE CONVERTD HERE            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0H                                                               
*                                                                               
*        TABLE OF SPILL STATIONS                                                
*        STATIONS COME IN AS !XXX-TV AND ARE CONVERTD HERE                      
*                                                                               
STATAB   DC    C'KBMY-TV '                                                      
STATABLQ EQU   *-STATAB            LENGTH OF TABLE ENTRY                        
         DC    C'KFYR-TV '                                                      
         DC    C'KXMC-TV '                                                      
         DC    C'KPOM-TV '                                                      
         DC    C'KHBS-TV '                                                      
         DC    C'KREX-TV '                                                      
         DC    C'KOTA-TV '                                                      
         DC    C'KOAT-TV '                                                      
         DC    C'KOB-TV  '                                                      
         DC    C'KSVI-TV '                                                      
         DC    C'KECI-TV '                                                      
         DC    C'KPAX-TV '                                                      
         DC    C'KELO-TV '                                                      
         DC    C'KCAU-TV '                                                      
         DC    C'KSFY-TV '                                                      
         DC    C'KTTW-TV '                                                      
         DC    C'KPVI-TV '                                                      
         DC    C'WXLV-TV '                                                      
         DC    C'WFXI-TV '                                                      
         DC    C'WSAZ-TV '                                                      
         DC    C'WFXR-TV '                                                      
         DC    C'KPBI-TV '                                                      
         DC    C'KFSM-TV '                                                      
         DC    X'FF'                                                            
*                                                                               
*        TABLE OF LOW POWER STATIONS                                            
*                                                                               
STATB2   DC    C'KM58',C'KMPH-L '                                               
STATB2LQ EQU   *-STATB2            LENGTH OF TABLE ENTRY                        
         DC    X'FF'                                                            
         TITLE 'SPREPMQ02 - CONVERT MMPLUS DATA TO EASI FORMAT-BUFFD'           
***********************************************************************         
*                                                                     *         
*        INVOICE DETAIL BUFFER                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUFFD    DSECT                                                                  
DTLBXLE  DS    0XL12               BXLE REGISTERS SAVEAREA                      
DTLSTA   DS    A                   A(BUFFER START)                              
DTLLEN   DS    F                   LENGTH OF ENTRY IN BUFFER                    
DTLENDA  DS    A                   A(CURRENT END OF BUFFER)                     
         DS    5A                  SPARE                                        
DTLSTRT  DS    0A                  START OF BUFFER                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
*                                                                               
         TITLE 'SPREPMQ02 - SPMMPCNV ERRORS - REPORT LINE'                      
***********************************************************************         
*                                                                     *         
*        LAYOUT OF REPORT LINE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPWORKD  DSECT                                                                  
         ORG   P                                                                
PERRT    DS    C'**ERROR**'                                                     
         DS    CL1                                                              
PERRMSG  DS    CL50                ERROR MESSAGE                                
         DS    CL1                                                              
PSQNT    DS    CL12                'RECORD NO, ='                               
         DS    CL1                                                              
PSQN     DS    CL8                 SEQUENCE NUMBER                              
         DS    CL(132-(P-*))       SPARE                                        
         ORG   P                                                                
PERRREC  DS    CL128               RECORD IN ERROR                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072SPREPMQ02 08/29/00'                                      
         END                                                                    
