*          DATA SET ACREPPL02  AT LEVEL 022 AS OF 09/03/20                      
*PHASE ACPL02C                                                                  
*---------------------------------------------------------------------*         
*  ! !                                                           ! !  *         
*                 *-----------*     *-------*     *-----------*       *         
*  **** NOTE: *** º ACREPPL02 º *** º CALLS º *** º ACREPRL02 º ****  *         
*                 *-----------*     *-------*     *-----------*       *         
*  ! !                                                           ! !  *         
*---------------------------------------------------------------------*         
* JFOS 017 01OCT13 <DSPCA110> SET TSRECL TO MAX TRNRLEN(!)                      
* YNGX 018 30SEP14 <PCA01185> RELINK TO INCLUDE NEW ACREPRLWRK                  
* VGUP 021 07JUN19 <SPEC-35904> Fix the CR/DR pairing issue due to '*'          
*                               key reference number                            
* GHOA 022 23Jul20 SPEC-46231 Relink due to new ACREPRLWRK                      
         SPACE 3                                                                
ACPL02   CSECT                                                                  
         TITLE 'SCRIBE PAYABLES REPORT GENERATOR'                               
         PRINT NOGEN                                                            
         USING ACWORKD,RC                                                       
         USING ACRLD,RA                                                         
         USING ACPLD,R7                                                         
         NMOD1 ACPLQ-ACPLD,**ACPL**,CLEAR=YES                                   
         LR    R7,RC                                                            
         L     RC,0(,R1)                                                        
         LA    RA,SPACEND                                                       
         L     R2,=A(IOTRANS)                                                   
         ST    R2,AIOTRANS                                                      
*                                                                               
         MVC   SVMODE,MODE                                                      
         MVC   SVADTRAN,ADTRANS                                                 
         CLI   MODE,PROCSPCL       Specail call from ACPRINT                    
         BNE   *+10                                                             
         MVC   SVREG,4(R1)         A(request card data)                         
         CLI   MODE,RUNFRST                                                     
         BE    RUNF10                                                           
         CLI   MODE,REQFRST                                                     
         BE    REQF10                                                           
         CLI   MODE,PROCTRNS                                                    
         BE    PROCT10                                                          
         CLI   MODE,SBACLAST                                                    
         BE    SBACL10                                                          
         OC    MAINPROG,MAINPROG                                                
         BZ    ACPL02X                                                          
         GOTO1 MAINPROG,DMCB,ACWORKD                                            
*                                                                               
ACPL02X  MVC   MODE,SVMODE                                                      
         MVC   ADTRANS,SVADTRAN                                                 
         XMOD1                                                                  
         EJECT                                                                  
         USING MASTD,R6                                                         
RUNF10   L     R6,ADMASTC                                                       
         L     R2,=A(PHASETAB)     LOAD PHASES 02,03,04,05                      
RUNF12   MVC   DUB,0(R2)                                                        
         MVC   DUB+6(1),MCTEST2    TEST PHASE?                                  
         OI    DUB+6,X'40'         MAKE UPPER CASE                              
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         ICM   RF,15,DMCB+4        IS IT OK?                                    
         BNZ   RUNF15              YES A TEST PHASE                             
         MVC   DUB,0(R2)                                                        
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         ICM   RF,15,DMCB+4        IS IT OK?                                    
         BNZ   *+6                 NON-TEST PHASE                               
         DC    H'00'                                                            
RUNF15   L     RE,8(,R2)                                                        
         AR    RE,RA               ADD BASE OF ACRL DSECT                       
         ST    RF,0(,RE)           SAVE OFF PHASE BEGINING                      
         OC    MCUSRDMP,MCUSRDMP                                                
         BNZ   *+8                                                              
         ST    RF,MCUSRDMP                                                      
         C     RF,MCUSRDMP                                                      
         BNL   *+8                                                              
         ST    RF,MCUSRDMP                                                      
         LA    R2,12(,R2)          BUMP TO NEXT PHASE                           
         CLI   0(R2),EOT           END OF TABLE?                                
         BNE   RUNF12                                                           
*                                                                               
         MVC   DUB,SPACES                                                       
         MVC   DUB(4),=C'T00A'                                                  
         MVI   WORK,QTSAR                                                       
         GOTO1 HEXOUT,DMCB,WORK,DUB+4,1                                         
         GOTOR LOADER,DMCB,DUB,0,0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TSARDINE,4(R1)                                                   
*                                                                               
         MVC   UPSI,MCUPSI                                                      
         MVC   MCAPHAS3,FRMTBLD    SET SO PATCH=03 PHASE WORKS                  
         MVC   MCAPHAS4,PRINTREP   SET SO PATCH=04 PHASE WORKS                  
         TM    UPSI,X'04'                                                       
         BZ    RUNF25                                                           
         MVC   MCAPHAS3,TABINIT    SET SO PATCH=05 PHASE WORKS                  
         MVC   MCAPHAS4,ROUTINE    SET SO PATCH=06 PHASE WORKS                  
*                                                                               
*UNF20   TM    UPSI,UPSIPT7                                                     
*        BZ    RUNF25                                                           
*        MVC   MCAPHAS3,RQPRINT    SET SO PATCH=07 PHASE WORKS                  
*                                                                               
RUNF25   A     RF,DMCB             LENGTH OF PHASE + RF=(PHASE 05 BASE)         
         C     RF,MCUSRDMP+4                                                    
         BNH   *+8                                                              
         ST    RF,MCUSRDMP+4       NEW END                                      
         MVC   CMPDATE,=X'B644'    FEB04/91                                     
         CLC   ALPHAID,=C'*B'      DDSB                                         
         BNE   *+10                                                             
         MVC   CMPDATE,=X'B675'    MAR02/91                                     
         GOTO1 MAINPROG,DMCB,ACWORKD                                            
         B     ACPL02X                                                          
         DROP  R6                                                               
         EJECT                                                                  
         USING TSARD,R1                                                         
REQF10   GOTO1 MAINPROG,DMCB,ACWORKD                                            
                                                                                
         LA    R1,TSARBLK2         Got storage in MAINPROG                      
         XC    TSARD(TSARDL),TSARD CLEAR TSAR BLOCK                             
         MVC   TSABUF,ASTACK       A(TSAR BUFF)                                 
         MVC   TSAREC,=A(TBUFLEN)  SIZE OF STACK                                
         MVI   TSKEYL,TRKLNQ       KEY LENGTH                                   
*&&UK*&& MVC   TSRECL,=H'2000'     RECORD LENGTH                                
*&&US*&& MVC   TSRECL,=H'1000'     RECORD LENGTH                                
         MVI   TSOFFACT,TSAINI     ACTION(INIT) IS HOB OF BUFFER                
         OI    TSIND2,TSI2MANY     USE FULL WORD COUNTERS                       
         OI    TSRECI,TSRVAR       VARIBABLE LEN RECORDS                        
         GOTO1 ATSAROFF                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         XC    SVACCT,SVACCT                                                    
         XC    CRAMNT,CRAMNT                                                    
*&&US                                                                           
         XC    SVREF,SVREF                                                      
         XC    SVSEQ,SVSEQ                                                      
*&&                                                                             
         B     ACPL02X                                                          
         EJECT                                                                  
***********************************************************************         
*  STORE UP TRANSACTIONS                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING ACMD,R5                                                          
         USING TRKEYD,R3                                                        
         USING TRNRECD,R2                                                       
PROCT10  L     R5,AMONACC                                                       
         MVC   SVTKOFF,ACMTKOFF    TRANSACTION KEY OFFICE                       
         MVC   SVTKSTA,ACMTKSTA    TRANSACTION KEY STATUS                       
         DROP  R5                                                               
*                                                                               
         L     R2,SVADTRAN         A(ADTRANS)                                   
         SH    R2,DATADISP                                                      
*&&US*&& BAS   RE,FIXREF                                                        
         CLC   TRNKEY(TRNKSBR-TRNRECD),SVACCT                                   
         BE    PROCT15                                                          
         OC    SVACCT,SVACCT       FIRST TIME THROUGH                           
         BZ    *+8                                                              
         BAS   RE,FLUSH                                                         
         MVC   SVACCT,TRNKEY                                                    
*                                                                               
PROCT15  MVC   ACPUSED,TRNRECD+ACCOUSED                                         
         L     R4,AIOTRANS         BUILD TSAR KEY, RECORD                       
         XC    0(2,R4),0(R4)       CLEAR LENGTH                                 
         LA    R4,2+TRKLNQ(,R4)    POINT TO DATA AREA                           
         LA    R5,2048-(TRKLNQ+2)  AREA SIZE TO MOVE INTO                       
         LR    RE,R2               A(SOURCE)                                    
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN        RECORD LENGTH                                
         LA    R1,2+TRKLNQ(,RF)    LENGTH OF TSAR RECORD                        
         MVCL  R4,RE                                                            
         L     R4,AIOTRANS                                                      
         STH   R1,0(,R4)           SAVE SIZE OR TSAR RECORD                     
         DROP  R2                                                               
*                                                                               
         USING TRNELD,R2                                                        
         L     R2,SVADTRAN         BUILD KEY OF TSAR RECORD                     
         LA    R3,2(,R4)           POINT TO KEY  AREA                           
         CLI   0(R2),TRNELQ        X'44'                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         XC    TRKEYD(TRKLNQ),TRKEYD                                            
         TM    TRNSTAT,TRNSDR            DEBIT OR CREDIT                        
         BNO   *+10                                                             
         MVC   TRKSEQ,TRNSUB       TRANSACTION SUBREF                           
         ZAP   TRKAMT,TRNAMNT            TRANSACTION AMOUNT                     
         NI    TRKAMT+L'TRKAMT-1,X'F0'   REMOVE SIGN OF AMOUNT                  
         MVC   TRKTKOFF,SVTKOFF    TRANSACTION KEY OFFICE                       
         MVC   TRKTKSTA,SVTKSTA    TRANSACTION KEY STATUS                       
*&&US                                                                           
         CLI   REFCHG,C'Y'         WAS REF# CHANGED?                            
         BNE   *+10                NO, DON'T PLUG IN NEW SEQUENCE               
         MVC   TRKNSEQ,SVSEQ       PUT NEW SEQUENCE IF USED                     
*&&                                                                             
         TM    TRNSTAT,TRNSDR            DEBIT OR CREDIT                        
         BO    PROCT22                                                          
         MVC   TRKSUB,TRNSUB       REAL CREDIT SUB SEQ                          
         XC    CRAMNT,CRAMNT                                                    
         MVI   CRSUBREF,0                                                       
*&&US*&& CLC   ACPUSED,CMPDATE     FEB04/91                                     
*&&US*&& BNL   PROCT55                                                          
         MVC   CRAMNT,TRNAMNT                                                   
         MVC   CRSUBREF,TRNSUB     SAVE FOR DEBIT                               
         B     PROCT55                                                          
         DROP  R2                                                               
*                                                                               
         USING MPYELD,R2                                                        
PROCT22  CLI   0(R2),0                                                          
         BE    PROCT30             THIS IS A PAIN IF WE BRANCH                  
         CLI   0(R2),MPYELQ        X'64' MEDIA PAYMENT ELEMENT                  
         BNE   PROCT24                                                          
         MVC   TRKSUB,MPYSUB       CREDIT SUB SEQUENSE NUMBER                   
         B     PROCT55                                                          
PROCT24  SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     PROCT22                                                          
         DROP  R2                                                               
*                                                                               
         USING TRNELD,R2                                                        
PROCT30  L     R2,SVADTRAN         BUILD KEY OF TSAR RECORD                     
*&&US                                                                           
         CLC   ACPUSED,CMPDATE     FEB04/91                                     
         BL    *+6                                                              
         DC    H'00'               BEFORE WE HAD THE MPYELD                     
*&&                                                                             
         OC    CRAMNT,CRAMNT       DID WE EVER GET A CREDIT?                    
         BNZ   PROCT35             NO                                           
         MVI   TRKSUB,X'FF'        MAKE THESE COME OUT LAST                     
         B     PROCT55                                                          
*                                                                               
PROCT35  MVC   TRKSUB,CRSUBREF     USE CREDIT SUB REF                           
         CLC   CRAMNT,TRNAMNT      MATCH TO CREDIT AMOUNT                       
         BE    PROCT55                                                          
         MVI   TRKSUB,X'FF'        MAKE THESE COME OUT LAST                     
         DROP  R2                                                               
*                                                                               
PROCT55  CLI   QOPT3,C''                                                       
         BNE   PROCT60                                                          
         LA    R0,TRKLNQ+2                                                      
         GOTO1 PRNTBL,DMCB,=C'TRNS TSAR',(R4),C'DUMP',(R0),=C'2D'               
*                                                                               
         USING TSARD,R1                                                         
PROCT60  LA    R1,TSARBLK2                                                      
         MVC   TSAREC,AIOTRANS     A(RECORD TO BE ADDED)                        
         MVI   TSOFFACT,TSAADD     ADD RECORD TO TSAR                           
         GOTO1 ATSAROFF                                                         
         CLI   TSERRS,0                                                         
         BE    PROCT90                                                          
         DC    H'00'               TSAR ERROR                                   
PROCT90  B     ACPL02X                                                          
         EJECT                                                                  
***********************************************************************         
*  PASS TRANSACTION BACK IN CORRECT ORDER                             *         
***********************************************************************         
         SPACE 1                                                                
         USING ACMD,R5                                                          
SBACL10  L     R5,AMONACC                                                       
         MVC   SVTKOFF,ACMTKOFF     TRANSACTION KEY OFFICE                      
         MVC   SVTKSTA,ACMTKSTA     TRANSACTION KEY STATUS                      
         DROP  R5                                                               
*                                                                               
         BAS   RE,FLUSH                                                         
         MVC   MODE,SVMODE         CALL ORIGINAL MODE                           
         MVC   ADTRANS,SVADTRAN                                                 
         GOTO1 MAINPROG,DMCB,ACWORKD                                            
         B     ACPL02X                                                          
         EJECT                                                                  
***********************************************************************         
*  FLUSH OUT TSAR AND CALL PROCTRNS                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TSARD,R1                                                         
FLUSH    NTR1                                                                   
         XC    CRAMNT,CRAMNT                                                    
         L     RE,AIOTRANS                                                      
         LA    RF,2048                                                          
         XCEF                                                                   
*                                                                               
         LA    R1,TSARBLK2                                                      
         MVI   TSOFFACT,TSARDH     GET FIRST RECORD                             
FLUSH20  MVC   TSAREC,AIOTRANS                                                  
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSEEOF       END OF RECORDS?                              
         BO    FLUSH50                                                          
*                                                                               
         USING ACMD,R5                                                          
         USING TRKEYD,R3                                                        
         L     R5,AMONACC                                                       
         L     R3,AIOTRANS                                                      
         LA    R3,2(,R3)           BUMP PAST RECORD LENGTH                      
         MVC   ACMTKOFF,TRKTKOFF   RESET FOR THIS TRANACTION                    
         MVC   ACMTKSTA,TRKTKSTA                                                
         LA    R3,TRKLNQ(,R3)      BUMP PAST TSAR KEY                           
         LA    R1,ACCORFST(,R3)    BUMP PAST TRANSACTION KEY TO X'44'           
         ST    R1,ADTRANS          SAVE ADDRESS IN MONACCS A(TRANSACT)          
         MVI   MODE,PROCTRNS                                                    
         GOTO1 MAINPROG,DMCB,ACWORKD                                            
         LA    R1,TSARBLK2         RESET R1                                     
         MVI   TSOFFACT,TSANXT     GET NEXT TSAR RECORD                         
         MVC   ACMTKOFF,SVTKOFF    RESET TO ORIGINAL                            
         MVC   ACMTKSTA,SVTKSTA                                                 
         B     FLUSH20                                                          
         DROP  R5                                                               
*                                                                               
FLUSH50  DS    0H                                                               
         LA    R1,TSARBLK2                                                      
         XC    TSARD(TSARDL),TSARD CLEAR TSAR BLOCK                             
         MVC   TSABUF,ASTACK       A(TSAR BUFF)                                 
         MVC   TSAREC,=A(TBUFLEN)  SIZE OF STACK                                
         MVI   TSKEYL,TRKLNQ       KEY LENGTH                                   
*&&UK*&& MVC   TSRECL,=H'2000'     RECORD LENGTH                                
*&&US*&& MVC   TSRECL,=H'1000'     RECORD LENGTH                                
         OI    TSIND2,TSI2MANY     USE FULL WORD COUNTERS                       
         OI    TSRECI,TSRVAR       VARIBABLE LEN RECORDS                        
         MVI   TSOFFACT,TSAINI     RESET TO CLEAR                               
         GOTO1 ATSAROFF                                                         
         XIT1                                                                   
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* Fix reference in key/44 if DB key reference elem found (255 issue)            
***********************************************************************         
*&&US                                                                           
         USING FFTELD,R4                                                        
         USING TRNRECD,R2                                                       
FIXREF   NTR1                                                                   
         MVI   REFCHG,C'N'         No reference number change                   
         L     R4,SVADTRAN                                                      
         CLI   TRNKREF+3,C'*'                                                   
         BNE   FXREFX                                                           
*                                                                               
FXREF05  CLI   0(R4),0                                                          
         BE    FXREFX                                                           
         CLI   0(R4),FFTELQ                                                     
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTKREF                                                 
         BE    FXREF10                                                          
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     FXREF05                                                          
*                                                                               
         USING TRNRECD,R2                                                       
FXREF10  CLI   FFTDATA+3,C'*'                                                   
         BE    FXREFX                                                           
         MVC   TRNKREF,FFTDATA    Fix reference in key                          
         DROP  R2                                                               
*                                                                               
         USING TRNELD,R2                                                        
         L     R2,SVADTRAN                                                      
         MVC   TRNREF,FFTDATA     Fix reference in 44 elem                      
*                                                                               
         LH    R1,SVSEQ                                                         
         CLC   SVREF,TRNREF                                                     
         MVC   SVREF,TRNREF                                                     
         BE    *+8                                                              
         LHI   R1,0                                                             
         AHI   R1,1                                                             
         STH   R1,SVSEQ                                                         
         MVI   REFCHG,C'Y'         Indicate we made a change                    
*                                                                               
FXREFX   XIT1                                                                   
         DROP  R2,R4                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
*        WORK AREA SAVED ACROSS CALLS                                 *         
***********************************************************************         
         SPACE ,                                                                
SVACCT   DS    CL42                CURRENT TRANSACTION KEY                      
CRAMNT   DS    PL(L'TRNAMNT)       CREDIT  TRANSACTION AMOUNT                   
CRSUBREF DS    XL1                 CREDIT  TRANSACTION SUBREFERENCE             
CMPDATE  DS    XL2                 DATE    TO COMPARE TO                        
*&&US                                                                           
SVSEQ    DS    H                   NEW SEQUENCE FROM FIXREF                     
SVREF    DS    CL6                 SAVE LAST REFERENCED USED                    
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
*  PHASES TO LOAD TO RUN MAIN PROGRAM                                 *         
***********************************************************************         
PHASETAB DS    0F                                                               
         DC    CL8'ACRL02  ',A(MAINPROG-ACRLD)                                  
         DC    CL8'ACRL03  ',A(FRMTBLD-ACRLD)                                   
         DC    CL8'ACRL04  ',A(PRINTREP-ACRLD)                                  
         DC    CL8'ACRL05  ',A(TABINIT-ACRLD)                                   
         DC    CL8'ACRL06  ',A(ROUTINE-ACRLD)                                   
         DC    CL8'ACRL07  ',A(RQPRINT-ACRLD)                                   
         DC    AL1(EOT)                                                         
         EJECT                                                                  
         DS    0A                                                               
         DC    C'**IOTR**'                                                      
IOTRANS  DS    XL2048                                                           
         EJECT                                                                  
TRKEYD   DSECT                                                                  
TRKAMT   DS    PL8                 TRANSACTION AMOUNT                           
TRKSUB   DS    XL1                 CREDIT      SUBREF (MPYSUB)                  
*&&US                                                                           
TRKNSEQ  DS    XL2                 TRANSACTION SUBREF EXTENSTION                
*&&                                                                             
TRKSEQ   DS    XL1                 TRANSACTION SUBREF TRNKSBR                   
TRKTKOFF DS    CL(L'ACMTKOFF)      TRANSACTION KEY OFFICE                       
TRKTKSTA DS    XL(L'ACMTKSTA)      TRANSACTION KEY STATUS                       
TRKLNQ   EQU   *-TRKEYD                                                         
         EJECT                                                                  
***********************************************************************         
*  NOT SAVED BETWEEN CALLS                                            *         
***********************************************************************         
ACPLD    DSECT                                                                  
AIOTRANS DS    A                                                                
SVMODE   DS    XL1                 SAVE MONACC MODE                             
SVADTRAN DS    A                   SAVE A(ADTRANS)                              
SVTKOFF  DS    CL(L'ACMTKOFF)      TRANSACTION KEY OFFICE                       
SVTKSTA  DS    XL(L'ACMTKSTA)      TRANSACTION KEY STATUS                       
*&&US                                                                           
REFCHG   DS    CL1                 INDICATOR FOR REF# CHANGES                   
*&&                                                                             
ACPWORK  DS    CL64                                                             
ACPUSED  DS    XL2                                                              
ACPLQ    DS    0XL1                                                             
         EJECT                                                                  
*ACREPRLWRK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPRLWRK                                                     
         PRINT ON                                                               
*DDCOREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACREPPL02 09/03/20'                                      
         END                                                                    
