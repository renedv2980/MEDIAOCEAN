*          DATA SET SPREPM102  AT LEVEL 069 AS OF 09/15/06                      
*PHASE SPM102A                                                                  
*INCLUDE GETSUPER                                                               
*INCLUDE GETSUPRC                                                               
*INCLUDE GETSUPTB                                                               
*INCLUDE GETSUPR4                                                               
         TITLE 'SPREPM102 - CPP GUIDE INTERFACE TAPE'                           
****************************CHANGE LOG ***************************              
* SEP15/06   USE SWEEP TABLE FROM DEMTABS, NOT HARD-CODED TABLE. *              
*            GET MARKET NAMES FROM DEMTABOF, NOT DELDCREC.       *              
*            MISCELLANEOUS (TRANSPARENT) CLEANUP.                *              
*                                                                *              
* DEC07/05   TAKE OUT AGY HARDCODE AND REMOVE USELESS CODE       *              
*            INCLUDING AGYOVR ROUTINE (IT DID NOTHING)           *              
*                                                                *              
* JUL13/05   FORCE TO 1 DECIMAL                                  *              
*                                                                *              
* SEPT10/87  TABLE 39 FOR 1987/88 BANK EXTRACTS                  *              
*            SWEEP DATES FOR 1988 ARB NSI                        *              
*                                                                *              
* APR22/88   CHANGES FOR 1988 SEASON                             *              
*             1. NEW SUPER DEMO TABLE                            *              
*             2. HANDLE SPECIAL RATES                            *              
*             3. FORCE OLD 25-54 STYLE DEMO                      *              
******************************************************************              
         EJECT                                                                  
         PRINT NOGEN                                                            
SPM102   CSECT                                                                  
         NMOD1 0,SPM102,R2                                                      
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPM1RA                                                     
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         ST    R3,SPM1R3                                                        
         L     R8,MEDBUFF                                                       
         USING MEDBLOCK,R8                                                      
         SPACE 2                                                                
         CLI   MODE,ESTFRST        MAY HAVE EXTENDED DATES                      
         BL    *+10                                                             
         MVC   QSTART(12),SVQDATE                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTBOF-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,MRKTNAMT  GET A(MARKET NAME TABLE)                     
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
*                                                                               
         MVC   MTLENGTH,4(R1)      LENGTH OF EACH ENTRY                         
SETMNTAB CLC   0(2,RE),=C'NT'      GET THE NSI LIST                             
         BE    SETMNTBX                                                         
         ICM   RE,7,2(RE)                                                       
         B     SETMNTAB                                                         
*                                                                               
SETMNTBX LA    RE,5(RE)            SET TO LIST START                            
         ST    RE,MTADDR                                                        
*                                                                               
         MVI   TOTPASS,1           SET TO DO DOLLAR TOTALS                      
*                                                                               
         CLI   MODE,PROCBUY        FORCE TO START AT NSI FOR BANK               
         BNE   M10                                                              
         OI    RQOPTS,RQOPTS_1DEC  ALWAYS FORCE TO 1 DECIMAL RATINGS            
         L     RE,ADBUY            SAVE BUY RECORD AGENCY CODE                  
         USING BUYREC,RE                                                        
         MVC   HLDAGY,BUYREC+20                                                 
         MVC   HLDSPC,BDPROGT-1                                                 
         DROP  RE                                                               
*        CLI   PROGPROF+15,C'Y'    IS IT A BANK AGENCY                          
*        BNE   M10                                                              
*        FORCE TO NSI                                                           
         L     RE,ADCLT            OVERRIDE NORMAL RATING SERVICE               
         USING CLTHDR,RE                                                        
         MVI   CPROF+3,C'0'                                                     
         MVI   RTGSRV,C'N'                                                      
         DROP  RE                                                               
         EJECT                                                                  
M10      CLI   MODE,REQFRST                                                     
         BNE   M20                                                              
         MVC   OVRAGY,QAGY                                                      
* OVERRIDE MINDSHARE OFFICES                                                    
*                                                                               
         MVC   M1QCLT,QCLT         FORCE ALL CLIENT IF OFFICE REQ               
         CLI   QCLT,C'$'                                                        
         BNE   *+10                                                             
         MVC   M1QCLT,=C'ALL'                                                   
*                                                                               
         CLC   QAGY,=C'H7'         MINDSHARE                                    
         BNE ENDMSOF                                                            
         CLC   QCLT(2),=C'$C'      CLIENT LIST C IS CENTRAL/OM                  
         BNE   *+10                                                             
         MVC   OVRAGY,=C'OM'                                                    
         CLC   QCLT(2),=C'$R'      CLIENT LIST R IS REGIONAL/JW                 
         BNE   *+10                                                             
         MVC   OVRAGY,=C'JW'                                                    
ENDMSOF  DS    0C                                                               
         MVI   RCSUBPRG,1                                                       
         GOTOR REQFRSTC,DMCB,(RA)                                               
         B     EXIT                                                             
         SPACE 2                                                                
M20      CLI   MODE,CLTFRST                                                     
         BNE   M30                                                              
         XC    TDCNT,TDCNT                                                      
         GOTOR CFRST,DMCB,(RA),BFREC                                            
         B     EXIT                                                             
         SPACE 2                                                                
M30      CLI   MODE,ESTFRST                                                     
         BNE   M40                                                              
*                                                                               
         MVC   PROGPROF(16),SVPROF                                              
         GOTOR EFRSTC,DMCB,(RA),BFREC                                           
         MVC   SPOTPROF+3(1),SVPROF+4                                           
         SPACE 2                                                                
         B     EXIT                                                             
         EJECT                                                                  
M40      CLI   MODE,PROCBUY                                                     
         BNE   M50                                                              
*                                                                               
         MVI   BYPSW,8                                                          
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         CLI   CCPPRS,C'N'                                                      
         BE    EXCLUDE                                                          
         DROP  RE                                                               
*                                                                               
         MVI   BYPSW,9                                                          
         L     RE,ADPRD                                                         
         USING PRDHDR,RE                                                        
         CLI   PCPPRS,C'N'                                                      
         BE    EXCLUDE                                                          
         DROP  RE                                                               
*                                                                               
         MVI   MEDEXTAC,C'G'       PUT NET IN GOAL DOLLARS                      
         MVI   BEFAFTSW,1                                                       
         MVI   BYPSW,6             UNKNOWN MARKET FOR STATION                   
         OC    SVRSMKT,SVRSMKT                                                  
         BZ    EXCLUDE                                                          
         MVI   BYPSW,1             SPECIAL REP                                  
         MVI   SPOTTYPE,C'R'       SET TO REGULAR SPOT                          
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         BAS   R9,GDEMEL1                                                       
         CLI   0(R7),2             FOUND DEMOS ?                                
         BNE   *+8                                                              
         BAS   R9,LMTDEM           YES - LIMIT RATING VALUES                    
         LA    R7,BDELEM                                                        
         USING BDELEM,R7                                                        
*                                                                               
         MVC   BDPROGT-1(1),HLDSPC                                              
         MVC   SVSPTLN,BDSEC                                                    
         DROP  R7                                                               
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    M40A1                                                            
         TM    BDCIND2,BDC2NEG     ADD NEGATIVES INTO EXCLUDED                  
         BO    EXCLUDE                                                          
         B     M40A2                                                            
M40A1    TM    BDCIND,X'01'        ADD NEGATIVES INTO EXCLUDED                  
         BO    EXCLUDE                                                          
M40A2    MVC   CURREST,BUYKEST                                                  
         MVC   BUYREC+20(2),=C'11' FORCE CONSISTANT RULES                       
         SPACE 1                                                                
         CLC   OVRAGY,=C'JW'       BYPASS JWT SPECIAL REPS                      
         BNE   M40NOJW                                                          
         CLC   BDREP,=H'899'                                                    
         BH    EXCLUDE             REP GT 900= SPECIAL REP                      
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         CLI   COFFICE,C'Z'        OR OFFICE "Z"                                
         BE    EXCLUDE                                                          
         DROP  RE                                                               
         SPACE 1                                                                
M40NOJW  MVI   BYPSW,2             SPECIAL                                      
         XC    SPDEMLST,SPDEMLST   RESET SPECIAL OVERRIDES                      
         CLI   BDPROGT-1,0         SPECIAL                                      
         BNE   M40SPC5              NO                                          
         MVI   SPOTTYPE,C'S'       SET TO SPECIAL                               
         CLI   PROGPROF,C'O'       OMIT                                         
         BE    EXCLUDE              YES - EXIT                                  
         EJECT                                                                  
* BUILD A LIST OF THE OVERRIDEN DEMOS FOR SPECIALS                              
* THIS LIST IS USED LATER ON TO DETERMINE IF WE WANT TO INCLUDE                 
* A PARTICULAR DEMO ON THE OUTPUT FILE                                          
         LA    R7,BDELEM           BUILD A LIST OF OVERRIDEN DEMOS              
         USING NDELEM,R7                                                        
M40SPC   CLI   0(R7),X'00'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R7),X'02'         FIND THE DEMO ELEMENT                        
         BE    M40SPC1                                                          
         ZIC   RE,1(R7)                                                         
         AR    R7,RE                                                            
         B     M40SPC                                                           
*                                                                               
M40SPC1  LA    RE,SPDEMLST                                                      
         LA    RF,NDEMNO           SET TO START OF DEMOS                        
         ZIC   R1,1(R7)                                                         
         AR    R7,R1               SET TO END OF ELEMENT                        
M40SPC3  CR    RF,R7                                                            
         BNL   M40SPC5                                                          
         TM    4(RF),X'80'         CHECK FOR OVERRIDE                           
         BZ    *+14                                                             
         MVC   0(3,RE),0(RF)       SET OVERRIDE IN LIST                         
         LA    RE,3(RE)                                                         
         LA    RF,8(RF)                                                         
         B     M40SPC3                                                          
*                                                                               
M40SPC5  DS    0C                                                               
         CLI   QMED,C'T'           USTV                                         
         BNE   M40BNKX                                                          
         CLI   PROGPROF+15,C'Y'    BANK AGENCY                                  
         BNE   M40BNKX                                                          
         L     RE,ASV1W            HAS PROFILE BEEN READ                        
         LTR   RE,RE                                                            
         BNZ   M40BNK1                                                          
         MVI   MEDBRAND,X'FF'                                                   
         GOTO1 MEDGETBY,DMCB,(RA),3    NO-LET MEDGETBY READ IT                  
         L     RE,ASV1W                                                         
         LTR   RE,RE               THIS HAPPENS IF NO ACTIVE SPOTS              
         BZ    M40BNKX             SO TAKE CARE OF IT                           
M40BNK1  L     RE,ASV1W                                                         
         MVC   0(3,RE),=C'1NN'     FORCE 1W PROFILE                             
         L     RE,ASVD0                                                         
         MVC   0(3,RE),=C'YY1'     FORCE D0 PROFILE                             
         ZIC   RF,BDDAY                                                         
         SR    R1,R1                                                            
         SLDL  RE,24                                                            
M40BNK2  SR    RE,RE               ONLY DO WEEKLIES FOR NON - ROTATORS          
         SLDL  RE,1                                                             
         AR    R1,RE                                                            
         LTR   RF,RF                                                            
         BNZ   M40BNK2                                                          
         CHI   R1,1                                                             
         BE    *+12                                                             
         L     RE,ASV1W                                                         
         MVI   0(RE),C'N'                                                       
*                                                                               
         GOTOR CHKSWP,DMCB,(RA)                                                 
         MVC   PROGPROF(1),BYTE                                                 
M40BNKX  DS    0C                                                               
*                                                                               
         CLI   PROGPROF,C'N'       NORMAL                                       
         BE    NOTSP                                                            
         MVI   BDPROGT-1,64        RESET SPECIAL                                
         XC    SPDEMLST,SPDEMLST   RESET SPECIAL OVERRIDES                      
         EJECT                                                                  
NOTSP    DS    0H                                                               
         SPACE 2                                                                
AYERATTX DS    0C                                                               
         SPACE 2                                                                
* CHECK FOR BONUS SPOTS                                                         
         MVI   BYPSW,3             BONUS                                        
         OC    BDCOST,BDCOST       ZERO COST SPOTS ONLY                         
         BNZ   NOTBNS                                                           
         LA    R7,BDELEM                                                        
         USING PKGELEM,R7                                                       
BONUS1   CLI   0(R7),0                                                          
         BE    BONUS3              BYPASS REGULAR BUYS                          
         CLI   0(R7),5             PACKAGE ELEMENT                              
         BE    BONUS2                                                           
         ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     BONUS1                                                           
*                                                                               
BONUS2   CLI   PKGIND,1            ALLOW PACKAGE BUYS                           
         BE    NOTBNS                                                           
         CLI   PKGIND,2            ALLOW PACKAGE BUYS                           
         BE    NOTBNS                                                           
         CLI   PKGIND,8            ALLOW MAKEGOOD SLAVES                        
         BE    NOTBNS                                                           
BONUS3   MVI   SPOTTYPE,C'B'       SET TO BONUS                                 
         CLI   PROGPROF+6,C'Y'     TEST FOR EXCLUDE BONUS SPOTS                 
         BNE   NOTBNS                                                           
         B     EXCLUDE             BYPASS ALL OTHER BONUS SPOTS                 
NOTBNS   DS    0C                                                               
         SPACE 2                                                                
         GOTOR SETDP,DMCB,(RA)                                                  
*********GOTOR PRNTRC,DMCB,(RA)    USE ONLY FOR TESTING                         
         MVC   BDTIMEND,SVETIM     RESTORE END TIME                             
         L     R8,MEDBUFF                                                       
         SPACE 2                                                                
         XC    PSLIST,PSLIST       BUILD ACTIVE PRODUCT/SPOT LEN LIST           
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
         LA    RE,PSLIST                                                        
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     *-12                                                             
         MVC   0(2,RE),=X'FFFF'                                                 
M401     LA    RE,PSLIST                                                        
M401A    CLC   0(2,RE),=X'FFFF'    CHECK FOR END                                
         BE    M402                                                             
         CLI   BPRD,X'FF'                                                       
         BE    M402                                                             
         CLC   0(1,RE),BPRD        PRODUCT OK                                   
         BNE   *+12                 NO - DELETE                                 
         LA    RE,2(RE)                                                         
         B     M401A                                                            
         XC    0(2,RE),0(RE)                                                    
         LA    RE,2(RE)                                                         
         B     M401A                                                            
         SPACE 2                                                                
M402     DS    0H                  SET DEMO LOOKUP CODE                         
         LA    RF,PSLIST                                                        
         ST    RF,PSLPTR                                                        
M402A    CLC   0(2,RF),=X'FFFF'    END                                          
         BE    M404                CHECK FOR TRUE END                           
         CLI   0(RF),0                                                          
         BNE   *+20                                                             
M402B    L     RF,PSLPTR                                                        
         LA    RF,2(RF)                                                         
         ST    RF,PSLPTR                                                        
         B     M402A                                                            
         MVC   PSLPRD,0(RF)                                                     
         MVC   PSLSLN,1(RF)                                                     
         MVI   BEFAFTSW,1                                                       
         NI    TOTPASS,B'11111101' RESET TOTALING                               
         MVI   BYPSW,7             UNALLOCATED                                  
         CLI   PSLPRD,219                                                       
         BE    M402B1B                                                          
         MVI   BYPSW,4             NO DEMOS                                     
* SET PRODUCT AND SPOT LENGTH                                                   
         MVC   MEDBRAND,PSLPRD                                                  
         MVC   MEDSPTLN,PSLSLN                                                  
         ZIC   RF,MEDBRAND         POINT RF TO BRAND SLOT                       
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         LA    R7,WORK                                                          
         USING ETABD,R7                                                         
         MVC   ETPRD,1(RF)                                                      
         MVC   ETEST,CURREST                                                    
*                                                                               
         MVC   SVPRI,28(RF)        SAVE THE PRIMARY DEMO                        
*                                                                               
         OC    PREVETAB,PREVETAB                                                
*        BZ    M402B0                                                           
         B     M402B0                                                           
         L     RE,PREVETAB                                                      
         STM   RE,RF,SVRE                                                       
         CLC   WORK(4),0(RE)       SAME SLOT AS PREV                            
         BNE   M402B0               NO                                          
         MVC   DEMAREA,SVDMAREA    RESTORE PREVIOUS DEMO AREA                   
         B     M402B1A                                                          
M402B0   DS    0H                                                               
         ST    RF,CURRBRND                                                      
         L     R5,ESCNTR                                                        
         L     R6,VESAVEC                                                       
         GOTO1 BINSRCH,DMCB,WORK,(R6),(R5),ETABLN,4,5000                        
         XC    PREVETAB,PREVETAB                                                
         MVI   BYPSW,4             ESTIMATE ERROR                               
         CLI   DMCB,1                                                           
         BE    M402B1B             ESTIMATE NOT IN TABLE-BYPASSED               
         MVC   PREVETAB,DMCB                                                    
M402B1   L     RF,CURRBRND                                                      
         L     RE,PREVETAB                                                      
         MVC   SVDEMS,28(RF)       SAVE DEMOS                                   
         MVC   28(60,RF),4(RE)                                                  
         LA    R4,WORK                                                          
         USING ERULESK,R4                                                       
         MVC   ERKCLT,CLIENT                                                    
         CLC   QUESTOR(3),=C'CTS'                                               
         BE    *+10                                                             
         CLC   M1QCLT,=C'ALL'                                                   
         BE    *+14                                                             
         MVC   ERKCLT(2),PROGPROF+2                                             
         MVI   ERKCLT+2,C'*'                                                    
         MVC   ERKPRD,PSLPRD                                                    
         MVC   ERKEST,CURREST                                                   
         STM   RE,RF,SVRE                                                       
         GOTO1 VGETDEM,DMCB,BFREC                                               
         MVC   SVDMAREA,DEMAREA    SAVE DEMO AREA                               
M402B1A  DS    0H                                                               
         CLI   DM1AREA+1,0         ANY DEMOS                                    
         BNE   M402B1C             YES PROCESS THEM                             
M402B1B  MVC   MEDBRAND,PSLPRD     SUM EXCLUDED DOLLARS                         
         MVC   MEDSPTLN,PSLSLN                                                  
         OI    TOTPASS,X'02'       SET TO TOTAL                                 
         BAS   R9,EXCLUDE1                                                      
         NI    TOTPASS,B'11111101' RESET TOTALING                               
         B     M402B                                                            
         SPACE 2                                                                
M402B1C  MVC   CURRTGT,DM1AREA                                                  
M402B2   LM    RE,RF,SVRE                                                       
         MVC   28(60,RF),SVDEMS    RESTOR DEMOS                                 
         LA    R9,DM1AREA                                                       
M402B3   CLI   1(R9),0             END OF DEMOS                                 
         BE    M402B                YES - NEXT PRODUCT                          
         CLI   0(R9),X'FF'         PREVIOUSLY USED                              
         BNE   M402B4                                                           
         LA    R9,60(R9)            YES-TRY NEXT SLOT                           
         B     M402B3                                                           
M402B4   MVC   28(60,RF),0(R9)                                                  
         XC    86(14,RF),86(RF)    CLEAR WEIGHTS                                
         MVI   0(R9),X'FF'                                                      
         L     R5,DLUPCOD                                                       
*                                                                               
         CLI   QMED,C'C'           CANADIAN ARB USES E INSTEAD OF R             
         BNE   M402B4B                                                          
         MVI   SPOTPROF+3,28       SET CSI ACTUAL BOOK TABLE                    
         CLI   RTGSRV,C'A'                                                      
         BNE   M402B4B                                                          
         MVI   SPOTPROF+3,40       SET BBM ACTUAL BOOK TABLE                    
         LA    RF,28(RF)           POINT TO DEMO LIST                           
M402B4A  CLI   1(RF),0                                                          
         BE    M402B4B                                                          
         CLI   1(RF),C'R'                                                       
         BNE   *+8                                                              
         MVI   1(RF),C'E'                                                       
         LA    RF,3(RF)                                                         
         B     M402B4A                                                          
*                                                                               
M402B4B  DS    0C                                                               
*                                                                               
         L     R1,ACOMFACS                                                      
         LA    RF,CDEMAND-COMFACSD(R1)                                          
         OC    MDEMAND,MDEMAND                                                  
         BNZ   *+10                                                             
         MVC   MDEMAND,0(RF)                                                    
         LA    RE,TDEMAND                                                       
         ST    RE,0(RF)                                                         
         LA    RE,TDEMHOK                                                       
         ST    RE,ATDEMHOK                                                      
         STM   R2,RC,M1REGS                                                     
         XC    TOTQH(6),TOTQH                                                   
*        XC    NWSQH,NWSQH                                                      
*        XC    SPTQH,SPTQH                                                      
         GOTO1 MEDGETBY,DMCB,(RA),(R5)                                          
*        CLC   QUESTOR(4),=C'NWXZ'                                              
         B     NOTEST                                                           
*        MVC   P+20(6),=C'GETBUY'                                               
*        EDIT  (B2,TOTQH),(4,P)                                                 
*        EDIT  (B2,NWSQH),(4,P+5)                                               
*        EDIT  (B2,SPTQH),(4,P+10)                                              
*        GOTO1 REPORT                                                           
*        B     NOTEST                                                           
*---->   GOTOR BUYTEST,DMCB,(RA)                                                
         USING *,RF                                                             
TDEMAND  OC    4(4,R1),4(R1)                                                    
         BZ    TDEMAND2                                                         
         MVC   MROOTRD,4(RD)                                                    
         MVC   MROOTHK,4(R1)                                                    
         MVC   4(4,R1),ATDEMHOK                                                 
TDEMAND2 L     RF,MDEMAND                                                       
         BR    RF                                                               
         DROP  RF                                                               
*        DC    H'0'                                                             
*        DC    C'THIS TRAPPED THE DEMAND CALL'                                  
*        DS    0H                                                               
         PRINT GEN                                                              
ATDEMHOK DC    F'0'                                                             
         DROP  R2                                                               
         USING *,RF                                                             
TDEMHOK  NTR1                                                                   
         L     RF,MROOTHK                                                       
*        STM   R0,RC,HKREGS                                                     
         DROP  RF                                                               
         BASR  RE,RF                                                            
         USING *,RE                                                             
         LM    R2,RC,M1REGS                                                     
         DROP  RE                                                               
         USING SPM102+4096,R2                                                   
         L     RE,0(R1)                                                         
         ST    RE,MDMCB+4                                                       
         GOTOR PROCHK,MDMCB,(RA)                                                
*        LM    R0,RC,HKREGS                                                     
         XIT1  ,                                                                
MROOTRD  DC    F'0'                                                             
MROOTHK  DC    F'0'                                                             
*        DC    H'0'                                                             
*        DC    C'THIS TRAPPED THE DEMAND HOOK'                                  
         DS    0H                                                               
         PRINT NOGEN                                                            
*                                                                               
NOTEST   CLI   TOTPASS,1                                                        
         BNE   *+16                                                             
         CLI   BEFAFTSW,1                                                       
         BNE   *+8                                                              
         BAS   RE,EXCLUDE6                                                      
         MVI   BEFAFTSW,2                                                       
         CLI   MEDSPILL,C'Y'       BYPASS IF SPILL                              
         BE    EXIT                                                             
*                                                                               
* EXTRACT AND POST ROUTINES                                                     
*                                                                               
         L     R5,MEDAFRST                                                      
         XC    CURRDEM,CURRDEM                                                  
         CLI   DEMPASS,0           SKIP TARGET ON PASS 2                        
         BE    *+8                                                              
         MVI   CURRDEM,2                                                        
         MVI   CPPPROG,C'O'                                                     
         L     RE,VDPTSVC                                                       
M402B5   CLI   0(RE),0             ANY DAYPART PROGRAM TYPES                    
         BE    M402B6                                                           
         CLC   MEDDPART,0(RE)                                                   
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     M402B5                                                           
         MVC   CPPPROG,3(RE)       SET PROGTYP FROM DAYPART                     
M402B6   L     RE,VPTYPSVC                                                      
M402B7   CLI   0(RE),0                                                          
         BE    M402C                                                            
         CLC   BDPROGT,0(RE)                                                    
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     M402B7                                                           
         MVC   CPPPROG,2(RE)       SET PROGTYP FROM PROGTYP                     
*                                                                               
M402C    CLI   CPPPROG,C'O'                                                     
         BNE   M402C10                                                          
         LH    RE,TOTQH                                                         
         SH    RE,NWSQH                                                         
         CH    RE,NWSQH                                                         
         BH    *+8                                                              
         MVI   CPPPROG,C'N'                                                     
         LH    RE,TOTQH                                                         
         SH    RE,SPTQH                                                         
         CH    RE,SPTQH                                                         
         BH    *+8                                                              
         MVI   CPPPROG,C'S'                                                     
*                                                                               
M402C10  CLC   QUESTOR(4),=C'NWXZ'                                              
         BNE   M402CPX                                                          
         L     R6,ADBUY                                                         
         MVC   P1(7),=C'BUYLINE'                                                
         MVC   P+9(1),CPPPROG                                                   
         MVC   P+11(1),CPPDP                                                    
         L     RE,=A(DPTNAM2)                                                   
         CLC   CPPDP(1),0(RE)                                                   
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     *-14                                                             
         MVC   P+11(3),1(RE)                                                    
*                                                                               
         CLC   1(3,RE),=C'LTE'                                                  
         BE    *+8                                                              
         CLC   1(3,RE),=C'PAC'                                                  
         BE    *+8                                                              
         CLC   1(3,RE),=C'PRI'                                                  
         BNE   M402CPX                                                          
                                                                                
         EDIT  (B4,TOTQH),(4,P+17)                                              
         EDIT  (B4,SPTQH),(4,P+22)                                              
         EDIT  (B4,NWSQH),(4,P+27)                                              
         MVC   P+30(14),BDPROG+4                                                
         EDIT  (B2,BDTIMST),(4,P+52)                                            
         EDIT  (B2,BDTIMEND),(4,P+57)                                           
         EDIT  (B1,BUYKEST),(3,P+62)                                            
         EDIT  (B1,BUYKBUY),(3,P+66)                                            
         EDIT  (B1,BDDAY),(3,P+70)                                              
         MVC   P+74(4),STA                                                      
         GOTO1 REPORT                                                           
         B     M402CPX                                                          
*                                                                               
M402CPX  L     R6,MEDALAST                                                      
         OI    TOTPASS,X'02'       ADD ON FIRST DEMO ONLY                       
         CR    R5,R6               END                                          
         BH    M402B2               YES - GET ANOTHER SET OF DEMOS              
         CLI   0(R5),0                                                          
         BNE   *+12                                                             
M402D    LA    R5,12(R5)                                                        
         B     M402C                                                            
M403     L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYSPT(12),MEDBYSPT                                            
         BZ    M402D                                                            
         XC    BFREC,BFREC                                                      
         SPACE 2                                                                
M403A    L     RF,CURRBRND                                                      
         ZIC   RE,CURRDEM                                                       
         MHI   RE,3                SET TO NEXT DEMO CODE                        
         LA    R9,28(RE,RF)                                                     
         MVC   BFDEMO,0(R9)                                                     
         CLI   BFDEMO+1,0                                                       
         BE    M403B                                                            
         CLI   BFDEMO,59                                                        
         BNE   *+10                                                             
         MVC   BFDEMO,1(R9)                                                     
         MVC   BFTARGET,CURRTGT                                                 
         CLI   BFTARGET+1,0                                                     
         BE    M402B                                                            
         GOTO1 DATCON,DMCB,(X'02',2(R5)),(X'03',WORK)                           
         L     RF,ADBUY                                                         
         MVC   BYTE,0(RF)                                                       
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'01'          SET UP CANADIAN NETWORKS                     
         BE    *+8                                                              
         MVI   BFSTYP,C'N'                                                      
         MVC   BFRTGSRV,RTGSRV                                                  
         MVC   BFYEAR(2),WORK                                                   
*                                                                               
         CLC   QEND+2(2),=C'10'    WHEN DOING 4TH QTR                           
         BL    SEPTOK                                                           
         CLI   PROGPROF+15,C'Y'    CHANGE SEPT TO OCT                           
         BNE   SEPTOK              FOR BANK AGENCIES                            
         CLI   QMED,C'T'                                                        
         BNE   SEPTOK                                                           
         CLI   BFYEAR+1,9                                                       
         BNE   SEPTOK                                                           
         MVI   BFYEAR+1,10                                                      
*                                                                               
SEPTOK   MVC   BFSL,PSLSLN                                                      
         MVC   BFDP,CPPDP                                                       
         MVC   BFPROG,CPPPROG                                                   
         MVI   BFAFFL,X'00'                                                     
*                                                                               
         CLI   LCLPROG,C'Y'        LOCAL PROGRAMMING IS NOT NETWORK             
         BE    *+12                                                             
         CLI   BFDP,C'N'           OTHERWISE PRIME IS NETWORK                   
         BE    WBUPN1                                                           
         CLI   SVRSAFF2,C'W'       WB/UPN ARE IND IN OTHER DPTS                 
         BE    *+8                                                              
         CLI   SVRSAFF2,C'U'                                                    
         BNE   *+8                                                              
         MVI   BFAFFL,C'I'                                                      
WBUPN1   DS    0C                                                               
*                                                                               
         MVC   BFEQIV,MEDEQFAC                                                  
         TM    PROGPROF+1,X'F0'                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   BFEQIV,=H'1000'     DONT EQUIV CANADIAN                          
         CLI   QMED,C'C'                                                        
         BE    M403AB                                                           
         CLI   PROGPROF+1,X'01'                                                 
         BNE   M403AA              USE AGENCY EQUIVALENCE FACTORS               
         ZIC   RE,BFSL             USE STANDARD EQUIVALENCE FACTORS             
         MHI   RE,1000                                                          
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,=F'30'                                                        
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         STH   RF,HALF                                                          
         MVC   BFEQIV,HALF                                                      
M403AA   CLI   PROGPROF+1,X'02'                                                 
         BL    M403AB                                                           
         GOTOR GETEQU,DMCB,(2,BFREC)                                            
         MVI   BYPSW,5             UNUSED SPOT LENGTH                           
         CLI   BFSL,15                                                          
         BNE   *+10                                                             
         CLC   MEDSPTLN,SVSPTLN                                                 
         BNE   *+10                                                             
         XC    BFEQIV,BFEQIV                                                    
         OC    BFEQIV,BFEQIV                                                    
         BZ    M402B1B             EXIT IF NO EQUIVALENCE FOUND                 
M403AB   DS    0H                                                               
         MVC   BFSPOT,MEDBYSPT                                                  
*                                                                               
         MVC   BFCASH,MEDBYD                                                    
         CLI   PROGPROF+15,C'Y'    BANK AGENCY                                  
         BNE   M403AB0             NO - USE MEDIA DOLLARS                       
         SR    R0,R0               YES - GET THE NET DOLLARS                    
         L     R1,MEDGLD           AND GROSS THEM UP                            
         M     R0,=F'100'                                                       
         AHI   R1,42                                                            
         D     R0,=F'85'                                                        
         ST    R1,BFCASH                                                        
*                                                                               
M403AB0  CLI   TOTPASS,3           BYPASS IF NOT TOTAL PASS                     
         BNE   M403AB1                                                          
         CLI   DEMPASS,0                                                        
         BNE   M403AB1                                                          
*                                  SUMMARIZE SUPER/TARGET INFO                  
         ZIC   R1,SVPRI+2                                                       
         SLL   R1,4                *16 FOR SPOTS/DOLS                           
         A     R1,=A(PRIBUF)                                                    
         L     RE,BFCASH           KILL THE PENNIES                             
         AHI   RE,50                                                            
         SRDA  RE,32                                                            
         D     RE,=F'100'                                                       
         ST    RF,FULL                                                          
         L     RE,0(R1)            ADD PRIMARY DEMO SPOTS                       
         A     RE,MEDBYSPT                                                      
         ST    RE,0(R1)                                                         
         L     RE,4(R1)                                                         
         A     RE,FULL             ADD PRIMARY DEMO DOLLARS                     
         ST    RE,4(R1)                                                         
         L     RE,8(R1)            IMPRESSIONS                                  
         A     RE,MEDBY1                                                        
         ST    RE,8(R1)                                                         
         L     RE,12(R1)           RATINGS                                      
         A     RE,MEDBY2                                                        
         ST    RE,12(R1)                                                        
*                                                                               
         LA    RE,MEDBFORE                                                      
         CR    R5,RE                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         L     RE,MEDBYD           ADD TO TOTAL COUNTERS                        
         CVD   RE,DUB                                                           
         AP    CLTIND,DUB                                                       
         AP    AGYIND,DUB                                                       
         L     RE,MEDBYSPT                                                      
         CVD   RE,DUB                                                           
         AP    CLTINS,DUB                                                       
         AP    AGYINS,DUB                                                       
*                                                                               
         NI    TOTPASS,B'11111101'                                              
*                                                                               
M403AB1  ZIC   RE,CURRDEM                                                       
         MHI   RE,8                                                             
         LA    RF,MEDBY2(RE)                                                    
         MVC   BFPNTS,0(RF)                                                     
         CLC   BFDEMO+2(1),SUPRDEM CHECK FOR TOTAL DEMO                         
         BNE   *+8                 AND ADD UP POINTS IF IT IS                   
         BAS   R9,SUMDEMS                                                       
         LA    RF,MEDBY1(RE)                                                    
         MVC   BFIMPS,0(RF)                                                     
         CLI   SPOTTYPE,C'B'       SET BONUS SPOTS FOR BONUS                    
         BNE   *+10                                                             
         MVC   BFBONUS,BFSPOT                                                   
         CLI   SPOTTYPE,C'S'       SET DOLLARS FOR SPECIALS                     
         BNE   *+10                                                             
         MVC   BFSPEC,BFCASH                                                    
*                                                                               
         OC    SPDEMLST,SPDEMLST   CHECK FOR SPECIAL WITH OVERRIDE              
         BZ    M403AD                                                           
         CLI   DEMPASS,0                                                        
         BNE   M403AE                                                           
         LA    RE,SPDEMLST         BYPASS IF SPECIAL AND THIS DEMO IS           
M403AC   CLI   1(RE),0             NOT OVERRIDEN                                
         BE    M403AE                                                           
         CLC   BFDEMO,0(RE)        IS THIS DEMO OVERRIDEN                       
         BE    M403AD               YES - USE IT                                
         LA    RE,3(RE)             NO - TRY NEXT                               
         B     M403AC                                                           
M403AD   DS    0C                                                               
         L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',(R7),BFREC                                  
M403AE   ZIC   RE,CURRDEM                                                       
         LA    RE,2(RE)            GET NEXT DEMO PAIR                           
         CHI   RE,42                                                            
         BE    M403B                                                            
         STC   RE,CURRDEM                                                       
         B     M403A                                                            
M403B    XC    CURRDEM,CURRDEM                                                  
         CLI   DEMPASS,0                                                        
         BE    M402D                                                            
         MVI   CURRDEM,2                                                        
         B     M402D                                                            
         EJECT                                                                  
SUMDEMS  L     R1,BFPNTS           ACCUMULATE ARB OR NSI SUPR  RATINGS          
         CVD   R1,DUB                                                           
         CLI   RTGSRV,C'N'                                                      
         BNE   *+10                                                             
         AP    AGYNSI,DUB                                                       
         CLI   RTGSRV,C'A'                                                      
         BNE   *+10                                                             
         AP    AGYARB,DUB                                                       
         BR    R9                                                               
         EJECT                                                                  
EXCLUDE  MVC   MEDBRAND,BPRD       TOTAL EXCLUDED SPOT/DOLLARS                  
         MVI   MEDSPTLN,0                                                       
         MVI   TOTPASS,3                                                        
         BAS   R9,EXCLUDE1         SUM EXCLUDED DOLLARS                         
         B     EXIT                                                             
         SPACE 2                                                                
EXCLUDE1 CLI   TOTPASS,3           RETURN IF NOT TOTAL PASS                     
         BNER  R9                                                               
         CLI   DEMPASS,0                                                        
         BNER  R9                                                               
         GOTO1 MEDGETBY,DMCB,(RA),2                                             
         CLI   MEDSPILL,C'Y'                                                    
         BE    EXIT                                                             
         L     R5,MEDAFRST                                                      
EXCLUDE2 L     R6,MEDALAST         FIND SLOTS                                   
         CR    R5,R6                                                            
         BH    EXCLUDE4                                                         
         CLI   0(R5),0                                                          
         BNE   *+12                                                             
         LA    R5,12(R5)                                                        
         B     EXCLUDE2                                                         
         SPACE 2                                                                
         L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
         BAS   RE,EXCLADD                                                       
         LA    R5,12(R5)                                                        
         B     EXCLUDE2                                                         
         SPACE 2                                                                
EXCLUDE4 CLI   BEFAFTSW,2          MAY HAVE BEEN ADDED ALREADY                  
         BE    *+8                                                              
         BAS   RE,EXCLUDE6         ADD BEFORE AND AFTER                         
         BR    R9                                                               
*                                                                               
EXCLUDE6 NTR1                                                                   
         CLI   DEMPASS,0                                                        
         BNE   EXCL6EX                                                          
         L     R4,MEDBFORE+4       BEFORE                                       
         CLC   BEFORE(4),MEDBFORE                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         LTR   R4,R4                                                            
         BZ    *+8                                                              
         BAS   RE,EXCLADD2                                                      
         L     R4,MEDAFTER+4       AFTER                                        
         CLC   AFTER(4),MEDAFTER                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         LTR   R4,R4                                                            
         BZ    *+8                                                              
         BAS   RE,EXCLADD2                                                      
EXCL6EX  XIT1                                                                   
EXCLADD  L     RF,MEDBYSPT         ADD SPOTS                                    
         CVD   RF,DUB                                                           
         AP    CLTBYPS,DUB                                                      
         AP    AGYBYPS,DUB                                                      
         ZIC   RF,BYPSW                                                         
         SLL   RF,4                *16                                          
         LA    RF,EXCLSUM-16(RF)                                                
         AP    0(8,RF),DUB                                                      
         L     RF,MEDBYD           ADD DOLLARS                                  
         CVD   RF,DUB                                                           
         AP    CLTBYPD,DUB                                                      
         AP    AGYBYPD,DUB                                                      
         ZIC   RF,BYPSW                                                         
         SLL   RF,4                *16                                          
         LA    RF,EXCLSUM-16(RF)                                                
         AP    8(8,RF),DUB                                                      
         BR    RE                                                               
*                                                                               
EXCLADD2 L     RF,MEDBYSPT         ADD OUTSIDE DATES SPOTS                      
         CVD   RF,DUB                                                           
         AP    CLTBYOS,DUB                                                      
         AP    AGYBYOS,DUB                                                      
         L     RF,MEDBYD           ADD OUTSIDE DATES DOLLARS                    
         CVD   RF,DUB                                                           
         AP    CLTBYOD,DUB                                                      
         AP    AGYBYOD,DUB                                                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
M404     B     EXIT           <----DISABLE ARB                                  
*404     CLI   PROGPROF+15,C'Y'    BANK AGENCY                                  
*        BNE   EXIT                NO THEN EXIT                                 
*        L     RE,ADCLT            CHECK IF ARB HAS BEEN DONE                   
*        USING CLTHDR,RE                                                        
*        B     EXIT           <----DISABLE ARB                                  
*        CLI   CPROF+3,C'1'                                                     
*        BE    EXIT                YES - EXIT                                   
*        MVI   CPROF+3,C'1'        NO - DO ARB NOW                              
*        MVI   RTGSRV,C'A'                                                      
*        MVI   TOTPASS,0                                                        
*        B     M40                                                              
         DROP  R7                                                               
*        DROP  RE                                                               
         EJECT                                                                  
M50      CLI   MODE,STALAST                                                     
         BNE   M60                                                              
         GOTOR STLAST,DMCB,(RA)                                                 
         CLI   DEMPASS,0                                                        
         BNE   *+20                                                             
         MVI   DEMPASS,1                                                        
         MVI   MODE,REREAD                                                      
         MVI   TOTPASS,0                                                        
         B     M71                                                              
*                                                                               
         MVI   DEMPASS,0                                                        
         B     EXIT                                                             
*                                                                               
M60      CLI   MODE,RUNLAST                                                     
         BNE   M70                                                              
         CLI   TAPEOPEN,0                                                       
         BE    EXIT                                                             
*                                                                               
         CLOSE (OUT)                                                            
         B     EXIT                                                             
         SPACE 2                                                                
         EJECT                                                                  
M70      CLI   MODE,STAFRST                                                     
         BNE   M80                                                              
M71      LA    R9,RECORD                                                        
         USING CPIDREC,R9                                                       
         GOTOR CPPMRKT,DMCB,STA,CPIDMKT                                         
         DROP  R9                                                               
         B     EXIT                                                             
*                                                                               
M80      CLI   MODE,REQLAST                                                     
         BNE   M90                                                              
         GOTOR RQLASTC,DMCB,(RA)                                                
         B     EXIT                                                             
         SPACE 2                                                                
M90      CLI   MODE,CLTLAST                                                     
         BNE   MA10                                                             
         GOTOR CLLASTC,DMCB,(RA)                                                
         CHI   R7,2                                                             
         BL    EXIT                                                             
         GOTOR ARPRINT,DMCB,(RA)                                                
         B     EXIT                                                             
         SPACE 2                                                                
MA10     DS    0H                                                               
EXIT     CLI   MODE,REQFRST                                                     
         BL    *+10                                                             
         MVC   QSTART(12),SVEDATE                                               
         XMOD1                                                                  
         EJECT                                                                  
         USING *,RF                                                             
WRREC    NTR1  BASE=SPM1RB                                                      
         DROP  RF                                                               
         LR    R2,RB                                                            
         AHI   R2,4096                                                          
         LM    RA,RC,SPM1RA                                                     
         L     R3,SPM1R3                                                        
         L     R9,RECCNT                                                        
         LA    R9,1(R9)                                                         
         ST    R9,RECCNT                                                        
         L     R9,CLTRCNT                                                       
         LA    R9,1(R9)                                                         
         ST    R9,CLTRCNT                                                       
         CLC   QUESTOR(3),=C'CTS'                                               
         BE    WRRECX                                                           
*        CLC   QUESTOR(4),=C'TST1'                                              
*        BE    WRREC1                                                           
         ST    R9,FULL                                                          
         L     R9,=A(OUT)                                                       
         PUT   (R9),RECORD                                                      
         L     R9,FULL                                                          
*RREC1   CLI   RECORD+12,C'1'      NOOP TO PRINT OUTPUT RECORDS                 
*        B     WRRECX                                                           
*        MVC   P(36),RECORD                                                     
*        GOTO1 HEXOUT,DMCB,RECORD,P2,40,0,0                                     
*        GOTO1 REPORT                                                           
WRRECX   XIT1                                                                   
         SPACE 2                                                                
         USING BUYREC,R6                                                        
GDEMEL1  LA    R7,BDELEM                                                        
         USING NDELEM,R7                                                        
GDEMEL2  CLI   0(R7),0             EOR                                          
         BER   R9                                                               
         CLI   0(R7),2             DEMO ELEMENT                                 
         BER   R9                                                               
         ZIC   RE,1(R7)                                                         
         AR    R7,RE                                                            
         B     GDEMEL2                                                          
         SPACE 2                                                                
************LIMIT RATING OVERRIDES TO MAX VALUE ********                        
*                                  R6 POINTS TO BUY                             
LMTDEM   LR    RF,R7               R7 POINTS TO DEMO ELEMENT                    
         ZIC   R1,1(R7)                                                         
         AR    RF,R1                                                            
         LA    R1,NDEMNO-NDELEM                                                 
         SR    RF,R1                                                            
LMTDEM1  CR    R7,RF                                                            
         BNLR  R9                                                               
         CLI   NDEMNO+1,C'R'                                                    
         BNE   LMTDEM2                                                          
         CLI   BDPROGT-1,0                                                      
         BNE   *+14                                                             
         CLC   NDEMRAW+1(3),=X'0003EC' 100.0                                    
         B     *+10                                                             
         CLC   NDEMRAW+1(3),=X'0001F1'  50.1                                    
         BL    *+10                                                             
         XC    NDEMRAW,NDEMRAW      CLEAR VALUE                                 
LMTDEM2  LA    R7,8(R7)                                                         
         B     LMTDEM1                                                          
         EJECT                                                                  
SPM1RA   DC    F'0'                                                             
SPM1RB   DC    F'0'                                                             
SPM1RC   DC    F'0'                                                             
SPM1R3   DC    F'0'                                                             
MDEMAND  DC    F'0'                                                             
M1REGS   DS    11F                                                              
*KREGS   DS    16F                                                              
         EJECT                                                                  
DPTNAM2  DC    C'A',C'EAM'                                                      
         DC    C'C',C'DAY'                                                      
         DC    C'E',C'WEM'                                                      
         DC    C'G',C'WEA'                                                      
         DC    C'J',C'ELY'                                                      
         DC    C'K',C'SUP'                                                      
         DC    C'L',C'PAC'                                                      
         DC    C'N',C'PRI'                                                      
         DC    C'P',C'LTE'                                                      
         DC    C'R',C'LLT'                                                      
         DC    X'00'                                                            
         DS    0H                                                               
         LTORG                                                                  
*          DATA SET SPREPM102S AT LEVEL 163 AS OF 12/19/96                      
*                                                                               
TIMTAB   DS    0C                  DAYPARTS BY TIME                             
         DC    B'01111100',AL2(0600,0857),C'A'    M-F 6A,857A                   
         DC    B'01111100',AL2(0858,1627),C'C'    M-F 858A-427P                 
         DC    B'00000011',AL2(0600,1257),C'E'   S-S 6A-1257P                   
         DC    B'00000011',AL2(1258,1627),C'G'   S-S 1258-427P                  
         DC    B'01111110',AL2(1628,1927),C'J'    M-SA 428-727P                 
         DC    B'01111110',AL2(1928,1957),C'L'   M-SA 728-757P                  
         DC    B'00000001',AL2(1628,1857),C'J'    SUN 428-657P                  
         DC    B'00000001',AL2(1858,1957),C'K'    SUN  658-757P                 
         DC    B'01111111',AL2(1958,2257),C'N'   M-S 758-1057P                  
         DC    B'01111111',AL2(2258,3159),C'P'     M-S 1058P-SIGN OFF           
         DC    A(0)                                                             
TIMTABC  DS    0C                  CENT AND MOUNT DAYPARTS BY TIME              
         DC    B'01111100',AL2(0600,0757),C'A'    M-F 6A-757A                   
         DC    B'01111100',AL2(0758,1527),C'C'    M-F 757A-327P                 
         DC    B'00000011',AL2(0600,1157),C'E'   S-S 6A-1157P                   
         DC    B'00000011',AL2(1158,1527),C'G'   S-S 1157A-327P                 
         DC    B'01111110',AL2(1528,1827),C'J'    M-SA 328-627P                 
         DC    B'01111110',AL2(1828,1857),C'L'   M-SA 628-657P                  
         DC    B'00000001',AL2(1529,1757),C'J'    SUN  328-557P                 
         DC    B'00000001',AL2(1758,1857),C'K'    SUN  558-657P                 
         DC    B'01111111',AL2(1858,2157),C'N'   M-S 657P-1057P                 
         DC    B'01111111',AL2(2158,3159),C'P'    M-S 958P-SIGN OFF             
         DC    A(0)                                                             
         EJECT                                                                  
* 3Q/1996 CHANGE DPT J FROM M-SA 358-727P                                       
* 2Q/2001 CHANGE DPT A/E FROM TO 5A START FROM 6A START                         
EPBANK   DS    0C       EASTERN AND PACIFIC FOR BANK                            
         DC    B'01111100',AL2(0500,0857),C'A'   M-F 5A,857A                    
         DC    B'01111100',AL2(0858,1557),C'C'   M-F 858A-357P                  
         DC    B'00000011',AL2(0500,1257),C'E'   S-S 5A-1257P                   
         DC    B'00000011',AL2(1258,1557),C'G'   S-S 1258-357P                  
         DC    B'01111110',AL2(1558,1857),C'J'   M-SA 358-657P                  
         DC    B'01111110',AL2(1858,1957),C'L'   M-SA 628-757P                  
         DC    B'00000001',AL2(1558,1757),C'J'   SUN 358-557P *                 
         DC    B'00000001',AL2(1758,1857),C'L'   SUN  558-657P *                
         DC    B'00000001',AL2(1858,1957),C'K'   SUN 658-757P *                 
         DC    B'01111111',AL2(1958,2257),C'N'   M-S 758-1057P                  
         DC    B'01111111',AL2(2258,2457),C'P'   M-S 1058P-1257A                
         DC    B'01111111',AL2(2458,3159),C'R'   M-S 1258P-2A                   
         DC    A(0)                                                             
CMBANK   DS    0C       CENTRAL AND MOUNTIAN FOR BANK                           
         DC    B'01111100',AL2(0500,0857),C'A'   M-F 5A-857A                    
         DC    B'01111100',AL2(0858,1457),C'C'   M-F 858A-257P                  
         DC    B'00000011',AL2(0500,1157),C'E'   S-S 5A-1157P                   
         DC    B'00000011',AL2(1158,1457),C'G'   S-S 1158-257P                  
         DC    B'01111110',AL2(1458,1757),C'J'   M-SA 258-557P                  
         DC    B'01111110',AL2(1758,1857),C'L'   M-SA 558-657P                  
         DC    B'00000001',AL2(1458,1657),C'J'   SUN 258-457P *                 
         DC    B'00000001',AL2(1658,1757),C'K'   SUN 458-557P *                 
         DC    B'00000001',AL2(1758,1857),C'L'   SUN  558-657P *                
         DC    B'01111111',AL2(1858,2157),C'N'   M-SU 658-957P                  
         DC    B'01111111',AL2(2158,2457),C'P'   M-S 958P-1257A                 
         DC    B'01111111',AL2(2458,3159),C'R'   M-S 1258P-2A                   
         DC    A(0)                                                             
* USE FOR KOVR ONLY 8/9/01                                                      
SSBANK   DS    0C       SACREMENT SAN FRANCISCO NBC/CBS                         
         DC    B'01111100',AL2(0500,0857),C'A'   M-F 5A,857A                    
         DC    B'01111100',AL2(0858,1557),C'C'   M-F 858A-357P                  
         DC    B'00000011',AL2(0500,1257),C'E'   S-S 5A-1257P                   
         DC    B'00000011',AL2(1258,1557),C'G'   S-S 1258-357P                  
         DC    B'01111100',AL2(1558,1827),C'J'   M-F 358-627P                   
         DC    B'00000010',AL2(1558,1927),C'J'   SAT 358-727P                   
         DC    B'01111110',AL2(1828,1857),C'L'   M-F 628-657P                   
         DC    B'00000010',AL2(1928,1957),C'L'   SAT 728-757P                   
         DC    B'00000001',AL2(1558,1757),C'J'   SUN 358-558P                   
         DC    B'00000001',AL2(1758,1957),C'K'   SUN 558-757P                   
         DC    B'01111100',AL2(1858,2157),C'N'   M-F 658-957P                   
         DC    B'00000011',AL2(1958,2257),C'N'   S-S 758-1057P                  
         DC    B'01111100',AL2(2158,2457),C'P'   M-S  958P-1257A                
         DC    B'00000011',AL2(2258,2457),C'P'   S-S 1058P-1257A                
         DC    B'01111111',AL2(2458,3159),C'R'   M-S 1258P-2A                   
         DC    A(0)                                                             
         EJECT                                                                  
*  BANK DAYPARTS FOR FOX AFFILIATES                                             
EPBANKF  DS    0C       EASTERN AND PACIFIC FOR BANK                            
         DC    B'01111100',AL2(0500,0857),C'A'   M-F 5A,857A                    
         DC    B'01111100',AL2(0858,1557),C'C'   M-F 858A-357P                  
         DC    B'00000011',AL2(0500,1257),C'E'   S-S 5A-1257P                   
         DC    B'00000011',AL2(1258,1557),C'G'   S-S 1258-357P                  
         DC    B'01111110',AL2(1558,1857),C'J'   M-SA 358-657P                  
         DC    B'01111110',AL2(1858,1957),C'L'   M-SA 658-757P                  
         DC    B'00000001',AL2(1558,1757),C'J'   SUN 358-557P                   
         DC    B'00000001',AL2(1758,1857),C'L'   SUN  558-657P                  
         DC    B'00000001',AL2(1858,1957),C'K'   SUN 658-757P                   
*        DC    B'00000001',AL2(1558,1757),C'J'   SUN 358-557P                   
*        DC    B'00000001',AL2(1758,1957),C'K'   SUN 558-757P                   
* OUT 3/17/03  B'01111111',AL2(1958,2227),C'N'   M-S 758-1027P PER              
         DC    B'01111111',AL2(1958,2157),C'N'   M-S 758-957P                   
         DC    B'01111111',AL2(2158,2457),C'P'   M-S 958P-1257A                 
         DC    B'01111111',AL2(2458,3159),C'R'   M-S 1258P-2A                   
         DC    A(0)                                                             
CMBANKF  DS    0C       CENTRAL AND MOUNTIAN FOR BANK                           
         DC    B'01111100',AL2(0500,0857),C'A'   M-F 5A-857A                    
         DC    B'01111100',AL2(0858,1457),C'C'   M-F 858A-257P                  
         DC    B'00000011',AL2(0500,1157),C'E'   S-S 5A-1157P                   
         DC    B'00000011',AL2(1158,1457),C'G'   S-S 1158-257P                  
         DC    B'01111110',AL2(1458,1757),C'J'   M-SA 258-557P                  
         DC    B'01111110',AL2(1758,1857),C'L'   M-SA 558-657P                  
         DC    B'00000001',AL2(1458,1657),C'J'   SUN 258-457P                   
         DC    B'00000001',AL2(1658,1757),C'K'   SUN 458-557P                   
         DC    B'00000001',AL2(1758,1857),C'L'   SUN  558-657P                  
*        DC    B'00000001',AL2(1458,1657),C'J'   SUN 258-457P                   
*        DC    B'00000001',AL2(1658,1857),C'K'   SUN 458-657P                   
         DC    B'01111111',AL2(1858,2057),C'N'   M-SU 658-858P                  
         DC    B'01111111',AL2(2058,2457),C'P'   M-S 858P-1257A                 
         DC    B'01111111',AL2(2458,3159),C'R'   M-S 1258P-2A                   
         DC    A(0)                                                             
SSBANKF  DS    0C       SACREMENT SAN FRANCISCO NBC/CBS                         
         DC    B'01111100',AL2(0500,0857),C'A'   M-F 5A,857A                    
         DC    B'01111100',AL2(0858,1557),C'C'   M-F 858A-357P                  
         DC    B'00000011',AL2(0500,1257),C'E'   S-S 5A-1257P                   
         DC    B'00000011',AL2(1258,1557),C'G'   S-S 1258-357P                  
         DC    B'01111100',AL2(1558,1827),C'J'   M-F 358-627P                   
         DC    B'00000010',AL2(1558,1927),C'J'   SAT 358-727P                   
         DC    B'01111110',AL2(1828,1857),C'L'   M-F 628-657P                   
         DC    B'00000010',AL2(1928,1957),C'L'   SAT 728-757P                   
         DC    B'00000001',AL2(1558,1757),C'J'   SUN 358-558P                   
         DC    B'00000001',AL2(1758,1957),C'K'   SUN 558-757P                   
         DC    B'01111100',AL2(1858,2157),C'N'   M-F 658-957P                   
         DC    B'00000011',AL2(1958,2257),C'N'   S-S 758-1057P                  
         DC    B'01111100',AL2(2158,2457),C'P'   M-S  958P-1257A                
         DC    B'00000011',AL2(2258,2457),C'P'   S-S 1058P-1257A                
         DC    B'01111111',AL2(2458,3159),C'R'   M-S 1258P-2A                   
         DC    A(0)                                                             
         SPACE 2                                                                
* CANADIAN DAYPART DEFINITIONS                                                  
CBANK    DS    0C       ALL TIME ZONES                                          
         DC    B'01111100',AL2(0600,1557),C'C'   M-F 6A-357P                    
         DC    B'01111100',AL2(1558,1757),C'J'   M-F 358P-557P                  
         DC    B'00000011',AL2(0600,1257),C'E'   S-S 6A-1257P                   
         DC    B'00000011',AL2(1258,1857),C'G'   S-S 1258-657P                  
         DC    B'01111100',AL2(1758,2257),C'N'   M-F 558P-1057P                 
         DC    B'00000011',AL2(1858,2257),C'N'   S-S 658P-1057P                 
         DC    B'01111111',AL2(2258,3159),C'P'   M-SU 1058P-CONCL               
         DC    A(0)                                                             
         EJECT                                                                  
         EJECT                                                                  
TIFTBC   DS    0C                  DAYPARTS BY TIME                             
         DC    B'01111100',AL2(0700,0857),C'A'    M-F 7-9A                      
         DC    B'01111100',AL2(0858,1557),C'C'    M-F 9A-4P                     
         DC    B'01111100',AL2(1558,1927),C'J'    M-F 4-730P                    
         DC    B'00000011',AL2(0658,1157),C'E'    S-S 7A-12N                    
         DC    B'00000011',AL2(1158,1257),C'F'    S-S 12N-1P                    
         DC    B'00000010',AL2(1258,1857),C'G'    SAT 1P-730P                   
         DC    B'00000001',AL2(1258,1857),C'G'    SUN 1P-7P                     
         DC    B'01111110',AL2(1928,1957),C'L'    M-SA 728-757P                 
         DC    B'01111110',AL2(1958,2257),C'N'    M-SA 8-11P                    
         DC    B'00000001',AL2(1858,2257),C'N'    SUN 7-11P                     
         DC    B'01111111',AL2(2258,2500),C'P'    M-S 11P-1A                    
         DC    A(0)                                                             
TIMFBCC  DS    0C                  CENT AND MOUNT DAYPARTS BY TIME              
         DC    B'01111100',AL2(0700,0857),C'A'    M-F 7-9A                      
         DC    B'01111100',AL2(0858,1457),C'C'    M-F 9A-3P                     
         DC    B'01111100',AL2(1458,1827),C'J'    M-F 3-630P                    
         DC    B'00000011',AL2(0658,1157),C'E'    S-S 7A-12N                    
         DC    B'00000011',AL2(1158,1258),C'E'    S-S 7A-1P                     
         DC    B'00000010',AL2(1258,1857),C'G'    SAT 1P-7P                     
         DC    B'00000001',AL2(1258,1857),C'G'    SUN 1P-7P                     
         DC    B'01111110',AL2(1828,1857),C'L'    M-SA 630-7P                   
         DC    B'01111110',AL2(1858,2157),C'N'    M-SA 7-10P                    
         DC    B'00000001',AL2(1858,2157),C'N'    SUN 7-10P                     
         DC    B'01111111',AL2(2158,2400),C'P'    M-S 10P-12M                   
         DC    A(0)                                                             
         DROP  R2                                                               
         EJECT                                                                  
PROCHK   NMOD1 0,PROCHK                                                         
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         USING SPM102+4096,R2                                                   
         L     RE,MDMCB+4                                                       
         USING DBLOCK,RE                                                        
         L     RF,DBAQUART                                                      
         ZIC   R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLI   1(RF),4                                                          
         BNE   *+16                                                             
         ICM   RF,3,2(RF)                                                       
         N     RF,=X'00007FFF'                                                  
         A     RF,DBAREC                                                        
         XC    WORK,WORK                                                        
         MVC   WORK(2),QIPRSRC-QIELEM(RF)                                       
         MVC   WORK+3(2),QIPTYPE-QIELEM(RF)                                     
         MVC   WORK+6(5),QIAFFIL-QIELEM(RF)                                     
         MVC   WORK+12(3),QIPNUM-QIELEM(RF)                                     
         MVI   NWSFLAG,0                                                        
         MVI   LCLPROG,0                                                        
         CLC   WORK+2,=C'LM'        LOCAL MOVIES                                
         BNE   *+8                                                              
         MVI   LCLPROG,C'Y'                                                     
         CLC   WORK(2),=C'LN'      LOCAL NEWS                                   
         BNE   *+8                                                              
         MVI   NWSFLAG,C'N'                                                     
         LA    RF,NWSPROG          NETWORK NEWS PROGRAMS                        
FLAGNWS  CLI   0(RF),X'FF'                                                      
         BE    CNTNWS                                                           
         CLC   WORK+12(3),0(RF)                                                 
         BNE   FLAGNWS2                                                         
         MVI   NWSFLAG,C'N'                                                     
         B     CNTNWS                                                           
FLAGNWS2 LA    RF,3(RF)                                                         
         B     FLAGNWS                                                          
*                                                                               
CNTNWS   LH    RF,DBFACTOR                                                      
         AH    RF,TOTQH                                                         
         STH   RF,TOTQH                                                         
         CLI   WORK+3,C' '                                                      
         BE    *+16                                                             
         LH    RF,DBFACTOR                                                      
         AH    RF,SPTQH                                                         
         STH   RF,SPTQH                                                         
         CLI   NWSFLAG,C'N'                                                     
         BNE   FLAGNWSX                                                         
         LH    RF,DBFACTOR                                                      
         AH    RF,NWSQH                                                         
         STH   RF,NWSQH                                                         
FLAGNWSX DS    0H                                                               
         CLC   QUESTOR(5),=C'NWXZ '                                             
         BNE   FLAGNWX2                                                         
*        MVC   P(25),WORK                                                       
*        MVC   P+43(1),NWSFLAG                                                  
*        GOTO1 VDEFINE,MDMCB,=C'PROGRAM',,P+26                                  
*        EDIT  (B3,WORK+12),(7,P+16)                                            
*        L     RE,WXZDBLK                                                       
*        L     R6,DBAREC                                                        
*        GOTO1 HEXOUT,DMCB,(R6),P2,40,0,0                                       
*        GOTO1 REPORT                                                           
         DROP  RE                                                               
FLAGNWX2 XMOD1                                                                  
WXZDBLK  DS    F                                                                
         LTORG                                                                  
NWSPROG  DC    AL3(2041)           ABC NWS-SUN                                  
         DC    AL3(2062)           ABC-NWS SAT                                  
         DC    AL3(2458)           ABC-WORLD NWS                                
         DC    AL3(3057)           CBS EVE NWS                                  
         DC    AL3(3159)           CBS NWS-SUN                                  
         DC    AL3(3013)           CBS SAT NWS                                  
         DC    AL3(88455)          CBS SAT NWS                                  
         DC    AL3(3004)           CBS SUNDAY NWS                               
         DC    AL3(2487)           FOX NWS SUNDAY                               
         DC    AL3(1025)           NBC NITELY NWS                               
         DC    AL3(1026)           NBC-NWS SAT                                  
         DC    AL3(1027)           NBC-NWS SUN                                  
         DC    X'FF'                                                            
         EJECT                                                                  
STLAST   NMOD1 0,STLAST                                                         
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         XC    BFREC,BFREC                                                      
         MVI   FRSMPRT,1                                                        
         L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R7),BFREC,1                               
         B     M502                                                             
M501     GOTO1 BUFFALO,DMCB,=C'SEQ',(R7),BFREC,1                                
M502     TM    DMCB+8,X'80'                                                     
         BO    M504                                                             
         L     R4,ADSTAT                                                        
         USING STAREC,R4                                                        
         LA    R2,RECORD                                                        
         USING CPIDREC,R2                                                       
         MVI   CPIDTYPE,C'D'                                                    
         MVC   CPIDAGY,OVRAGY                                                   
         MVC   CPIDMED,QMED                                                     
         MVC   CPIDCLT,CLT                                                      
         MVC   CPIDSERV,BFRTGSRV                                                
         MVC   CPIDEQIV,BFEQIV                                                  
         MVC   CPIDYEAR,BFYEAR                                                  
         MVC   CPIDMON,BFMON                                                    
         MVC   CPIDDP,BFDP                                                      
         MVC   CPIDAFFL,BFAFFL                                                  
         MVC   CPIDSL,BFSL                                                      
         MVC   CPIDPROG,BFPROG                                                  
*                                                                               
         CLC   QUESTOR(3),=C'CTS'                                               
         BE    *+10                                                             
         CLC   M1QCLT,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   CPIDAGY,PROGPROF+2                                               
         CLC   QUESTOR(3),=C'CTS'                                               
         BE    *+14                                                             
         CLC   M1QCLT,=C'ALL'                                                   
         BE    *+14                                                             
         MVC   CPIDCLT(2),OVRAGY                                                
         MVI   CPIDCLT+2,C' '                                                   
*                                                                               
         CLI   CPIDAFFL,C' '       DP/PROG SPECIFIC AFFILIATION SET             
         BH    AFFOVR               YES - LEAVE IT ALONE                        
         MVI   CPIDAFFL,C'N'                                                    
         CLI   SVRSAFF,C' '                                                     
         BNE   *+8                                                              
         MVI   CPIDAFFL,C'I'                                                    
         CLI   SVRSAFF,C'I'                                                     
         BNE   *+8                                                              
         MVI   CPIDAFFL,C'I'                                                    
         CLI   SVRSAFF,0                                                        
         BNE   *+8                                                              
         MVI   CPIDAFFL,C'I'                                                    
         CLI   SVRSAFF,C'0'                                                     
         BNE   *+8                                                              
         MVI   CPIDAFFL,C'I'                                                    
         CLI   SVRSAFF,C'O'                                                     
         BNE   *+8                                                              
         MVI   CPIDAFFL,C'I'                                                    
         CLI   SVRSAFF,C'H'                                                     
         BNE   *+8                                                              
         MVI   CPIDAFFL,C'H'                                                    
         MVC   BFAFFL,CPIDAFFL                                                  
*                                                                               
AFFOVR   CLI   QMED,C'C'           SET MEDIA FOR CANADIAN                       
         BNE   FRSMKT                                                           
         CLI   BFSTYP,C'N'                                                      
         BE    *+8                                                              
         MVI   BFSTYP,C'T'                                                      
         MVC   CPIDAFFL,BFSTYP                                                  
*                                                                               
FRSMKT   MVC   CPIDMKT,SVRSMKT                                                  
         DROP  R4                                                               
         L     RE,=A(CPMWGHT)                                                   
         CLI   QMED,C'C'                                                        
         BNE   *+8                                                              
         L     RE,=A(CPMWGHTC)                                                  
         MVC   CPIDMKWT,=H'1'                                                   
         SR    R1,R1                                                            
         ICM   R1,3,CPIDMKT                                                     
         LTR   R1,R1                                                            
         BZ    M504                                                             
         CLI   QMED,C'C'                                                        
         BE    *+8                                                              
         SHI   R1,400              ADJUST FOR REGULAR NSI NUMBER                
         STH   R1,HALF                                                          
M503     CLI   0(RE),X'FF'                                                      
         BE    M503A               DEFAULT WEIGHT FOR UNKNOWNS                  
         CLC   HALF,0(RE)                                                       
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     M503                                                             
         MVC   CPIDMKWT,2(RE)                                                   
M503A    DS    0H                                                               
*                                                                               
         MVC   CPIDTARG,BFTARGET+2 SET TARGET CATAGORY                          
         CLI   CPIDTARG,0          INVALID DEMO BYPASS                          
         BE    M501                                                             
         MVC   CPIDDEMO,BFDEMO+2   SET DEMO CATAGORY                            
         CLI   CPIDDEMO,0          INVALID DEMO BYPASS                          
         BE    M501                                                             
*                                                                               
         MVC   CPIDSPOT,BFSPOT                                                  
*                                ************TEST**************                 
         CLI   FRSMPRT,1                                                        
         BNE   M503B                                                            
         MVI   FRSMPRT,0                                                        
         MVI   P,C'*'                                                           
         MVC   P+1(5),STA                                                       
         MVC   P+6(1),SVRSAFF                                                   
         EDIT  (B2,HALF),(4,P+12)                                               
         LA    RE,TZALPHA                                                       
PMNTZ    CLI   0(RE),X'FF'                                                      
         BE    PMNTZ2                                                           
         CLC   0(1,RE),TIMZON                                                   
         BE    PMNTZ2                                                           
         LA    RE,4(RE)                                                         
         B     PMNTZ                                                            
PMNTZ2   MVC   P+8(3),1(RE)                                                     
         L     RE,MTADDR                                                        
PMN1     CLC   0(2,RE),HALF                                                     
         BE    PMN2                                                             
         L     RF,MTLENGTH                                                      
         AR    RE,RF                                                            
         OC    0(2,RE),0(RE)                                                    
         BNZ   PMN1                                                             
         MVC   P+17(20),=CL20'UNKNOWN'                                          
         B     *+10                                                             
PMN2     MVC   P+17(20),2(RE)                                                   
*                                                                               
         GOTO1 REPORT                                                           
*                               ********************************                
M503B    ICM   RE,15,BFCASH                                                     
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,=F'100'                                                       
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,FULL                                                          
         MVC   CPIDCASH,FULL                                                    
         MVC   CPIDPNTS,BFPNTS                                                  
         ICM   RE,15,BFIMPS                                                     
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,FULL                                                          
         MVC   CPIDIMPS,FULL                                                    
         GOTO1 VWRREC                                                           
         GOTOR BLDRPT,DMCB,(RA)                                                 
         B     M501                                                             
M504     L     R2,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R2)                                      
STLASTX  XMOD1                                                                  
*                                                                               
         SPACE 2                                                                
TZALPHA  DC    X'01',C'EP '                                                     
         DC    X'02',C'CM '                                                     
         DC    X'03',C'SF '                                                     
         DC    X'11',C'EPF'                                                     
         DC    X'12',C'CMF'                                                     
         DC    X'13',C'SFF'                                                     
         DC    X'FF',C'???'                                                     
         SPACE 2                                                                
FRSMPRT  DS    C                                                                
         LTORG                                                                  
         EJECT                                                                  
CLLASTC  NMOD1 0,CLLASTC                                                        
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         MVI   RCSUBPRG,2                                                       
         L     R7,CLTRCNT                                                       
         CHI   R7,2                FORCE HEADLINE IF SIGNIFICANT DATA           
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(7),=C'CLIENT='                                                 
         MVC   P+7(3),CLT                                                       
         MVC   P+12(8),=C'RECORDS='                                             
         EDIT  CLTRCNT,(7,P+20),,ALIGN=LEFT                                     
         GOTOR PRTDOLT,DMCB,(RA)     CLIENT DOLLAR TOTALS                       
         XC    CLTRCNT,CLTRCNT                                                  
         XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
SETDP    NMOD1 0,SETDP                                                          
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         MVC   SVETIM,BDTIMEND     SAVE END TIME                                
         OC    BDTIMEND,BDTIMEND                                                
         BNZ   *+10                                                             
         MVC   BDTIMEND,BDTIMST                                                 
         CLC   BDTIMST,=H'459'     CORRECT TIMES LT 0500                        
         BH    M4A                                                              
         MVC   HALF,BDTIMST                                                     
         LH    RE,HALF                                                          
         AHI   RE,2400                                                          
         STH   RE,HALF                                                          
         MVC   BDTIMST,HALF                                                     
M4A      CLC   BDTIMEND,=H'459'                                                 
         BH    M4B                                                              
         MVC   HALF,BDTIMEND                                                    
         LH    RE,HALF                                                          
         AHI   RE,2400                                                          
         STH   RE,HALF                                                          
         MVC   BDTIMEND,HALF                                                    
M4B      DS    0H                                                               
         XC    DPTHOLD,DPTHOLD                                                  
         CLC   BDTIMEND,BDTIMST    FIX TIMES WHICH CROSS DAYS                   
         BNL   *+10                                                             
         MVC   BDTIMST,=H'500'                                                  
         L     RF,ATIMTAB                                                       
         CLI   TIMZON,2                                                         
         BNE   *+8                                                              
         L     RF,ATIMTABC                                                      
         CLI   TIMZON,3                                                         
         BNE   *+8                                                              
         L     RF,ATIMTABS                                                      
         TM    TIMZON,X'10'                                                     
         BZ    M40A                                                             
         L     RF,ATIMTBF                                                       
         CLI   TIMZON,X'12'                                                     
         BNE   *+8                                                              
         L     RF,ATIMTBCF                                                      
         CLI   TIMZON,X'13'                                                     
         BNE   M40A                                                             
         L     RF,ATIMTBSF                                                      
M40A     ZIC   RE,0(RF)            GET CPP DAYPART                              
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    BDDAY,0                                                          
         BZ    M40B                                                             
         CLC   BDTIMEND,1(RF)      ET LT. ST                                    
         BL    M40B                                                             
         CLC   BDTIMST,3(RF)       ST GT. ET                                    
         BH    M40B                                                             
         MVC   CPPDP,5(RF)                                                      
         CLC   BDTIMEND,3(RF)      CROSS DAYPARTS                               
         BNH   M40C                                                             
         B     M40BA                                                            
M40B     LA    RF,6(RF)                                                         
         CLI   0(RF),0                                                          
         BNE   M40A                                                             
         B     M40B4                                                            
M40BA    LA    R1,DPTHOLD                                                       
         XC    DPTHOLD,DPTHOLD                                                  
         MVC   SVSTIM,BDTIMST                                                   
M40B1    CLC   SVSTIM,BDTIMEND     IS DAYPART WITHIN BUY                        
         BNL   M40B3                                                            
         MVC   HALF,3(RF)                                                       
         CLC   HALF,BDTIMEND                                                    
         BL    *+10                                                             
         MVC   HALF,BDTIMEND                                                    
         CLC   SVSTIM,HALF         IS THIS TIME WITHIN CURRENT DPT              
         BNL   M40B2               NO - TRY NEXT                                
         MVC   DPST,SVSTIM                                                      
         MVC   DPET,HALF                                                        
         LH    R8,HALF                                                          
         MVC   HALF,SVSTIM                                                      
         LH    R9,HALF                                                          
         SR    R8,R9                                                            
         STH   R8,DPT1                                                          
         STH   R8,DPT1             SAVE MINUTES                                 
         LH    R8,DPST             GET ST HOUR                                  
         SRDA  R8,32                                                            
         D     R8,=F'100'                                                       
         STH   R9,DPST                                                          
         LH    R8,DPET             GET ET HOUR                                  
         SRDA  R8,32                                                            
         D     R8,=F'100'                                                       
         SH    R9,DPST             GET HOUR DIFFERENCE                          
         MHI   R9,40               DECREMENT BY 40 X HOURS                      
         LH    R8,DPT1                                                          
         SR    R8,R9                                                            
         MVC   0(1,R1),5(RF)       SAVE DAYPART CODE                            
         STH   R8,DPT1                                                          
         MVC   1(2,R1),DPT1        SAVE NUMBER OF MINUTES IN DAYPART            
         LA    R1,3(R1)                                                         
         MVC   SVSTIM,3(RF)        SET NEW START TIME                           
         LH    R8,SVSTIM                                                        
         LA    R8,1(R8)                                                         
         STH   R8,SVSTIM                                                        
M40B2    LA    RF,6(RF)            FIND NEXT QUALIFING DAYPART                  
         CLI   0(RF),0                                                          
         BE    M40B3                                                            
         ZIC   RE,0(RF)                                                         
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    BDDAY,0                                                          
         BZ    M40B2                                                            
         B     M40B1                                                            
M40B3    LA    R1,DPTHOLD          SET DAYPART WITH HIGHEST MINUTES             
         XC    DPT1,DPT1                                                        
M40B31   CLC   DPT1,1(R1)                                                       
         BNL   M40B32                                                           
         MVC   CPPDP,0(R1)                                                      
         MVC   DPT1,1(R1)                                                       
M40B32   LA    R1,3(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   M40B31                                                           
         B     M40C                                                             
M40B4    MVI   CPPDP,X'FF'                                                      
M40C     DS    0H                                                               
         CLI   CPPDP,C'P'                                                       
         BNE   M40C1                                                            
         CLC   BDTIMST,=H'2500'                                                 
         BL    *+8                                                              
         MVI   CPPDP,C'R'                                                       
M40C1    DS    0C                                                               
         XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
PRNTRC   NMOD1 0,PRNTRC                                                         
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         ZIC   R9,9(R6)                                                         
         EDIT  (R9),(3,P)                                                       
         ZIC   R9,10(R6)                                                        
         EDIT  (R9),(3,P+4)                                                     
         EDIT  (B1,BDDAY),(3,P+9)                                               
         EDIT  (B2,BDTIMST),(4,P+12)                                            
         EDIT  (B2,BDTIMEND),(4,P+17)                                           
         MVC   P+22(1),CPPDP                                                    
         LA    R8,8                                                             
         LA    R7,DPTHOLD                                                       
         LA    R6,P+24                                                          
PRNTRC2  MVC   0(1,R6),0(R7)                                                    
         SR    R9,R9                                                            
         ICM   R9,3,1(R7)                                                       
         EDIT  (R9),(3,1(R6))                                                   
         LA    R7,3(R7)                                                         
         LA    R6,5(R6)                                                         
         BCT   R8,PRNTRC2                                                       
         GOTO1 REPORT                                                           
         XC    P(132),P                                                         
         XMOD1                                                                  
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
BUYTEST  NMOD1 0,TSTBUY                                                         
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         ZIC   R9,9(R6)                                                         
         EDIT  (R9),(3,P)                                                       
         ZIC   R9,10(R6)                                                        
         EDIT  (R9),(3,P+4)                                                     
         L     R5,MEDAFRST                                                      
BUYTEST1 L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYSPT,MEDBYSPT                                                
         BZ    BUYTEST2                                                         
         L     R9,MEDBYD                                                        
         EDIT  (R9),(8,P+10)                                                    
         L     R9,MEDBYSPT                                                      
         EDIT  (R9),(8,P+20)                                                    
         L     R9,MEDBY1                                                        
         EDIT  (R9),(8,P+30)                                                    
         MVC   P+40(4),PROGPROF                                                 
         MVC   P+46(7),STAPRINT                                                 
         MVC   P+60(1),PROGPROF                                                 
         L     RE,ASV1W                                                         
         MVC   P+62(3),0(RE)                                                    
         MVC   P+66(1),SPOTTYPE                                                 
         GOTO1 REPORT                                                           
BUYTEST2 LA    R5,12(R5)                                                        
         OC    0(4,R5),0(R5)                                                    
         BNZ   BUYTEST1                                                         
         XMOD1                                                                  
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
RQLASTC  NMOD1 0,RQLASTC                                                        
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         MVC   CLTIND,AGYIND       AGENCY TO CLIENT BUCKETS                     
         MVC   CLTBYOD,AGYBYOD     AGENCY TO CLIENT BUCKETS                     
         MVC   CLTBYPD,AGYBYPD     PRINT AGENCY DOL/DEM TOTALS                  
         GOTOR PRTDOLT,DMCB,(RA)                                                
         L     RE,=A(SUPRTAB)                                                   
         LHI   RF,512                                                           
         XCEF                                                                   
         LA    R8,0                                                             
REQLPH   LA    R8,1(R8)            HOUSEKEEPING                                 
         STC   R8,FULL             BUILD A SUPER DEMO TABLE                     
         CHI   R8,255                                                           
         BH    REQLPH1                                                          
         GOTO1 AGETSUPR,DMCB,(X'01',FULL)                                       
         LR    RE,R8                                                            
         SLL   RE,1                                                             
         A     RE,=A(SUPRTAB)                                                   
         MVC   0(1,RE),FULL        SET THE SUPER DEMO                           
         STC   R8,1(RE)            SET THE ACTUAL DEMO                          
         B     REQLPH                                                           
REQLPH1  L     R8,=A(SUPRTAB)                                                   
         GOTO1 XSORT,DMCB,(R8),256,2,2,0                                        
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
         MVC   P+34(6),=C' CPPRS'                                               
         MVC   P+47(5),=C'SUPER'                                                
         MVC   P+58(5),=C'SUPER'                                                
         MVC   P2(5),=C'SUPER'                                                  
         MVC   P2+7(6),=C'ACTUAL'                                               
         MVC   P2+21(5),=C'SPOTS'                                               
         MVC   P2+34(7),=C'DOLLARS'                                             
         MVC   P2+47(5),=C' IMPS'                                               
         MVC   P2+58(5),=C' RTGS'                                               
         MVC   P3(5),=C'-----'                                                  
         MVC   P3+7(6),=C'------'                                               
         MVC   P3+21(5),=C'-----'                                               
         MVC   P3+34(7),=C'-------'                                             
         MVC   P3+47(5),=C'-----'                                               
         MVC   P3+58(5),=C'-----'                                               
         MVI   P4,0                                                             
         GOTO1 REPORT                                                           
         L     R8,=A(SUPRTAB)      PRINT PRIMARY DEMO REPORT                    
         SR    R9,R9                                                            
REQLPP   CLI   0(R8),0                                                          
         BE    REQLPP2                                                          
         ZIC   R7,1(R8)                                                         
         SLL   R7,4                * 16                                         
         A     R7,=A(PRIBUF)                                                    
         OC    0(4,R7),0(R7)                                                    
         BZ    REQLPP2                                                          
         XC    FULL,FULL                                                        
         MVI   FULL+1,C'R'                                                      
         MVC   FULL+2(1),0(R8)                                                  
         L     RE,ADBLOCK                                                       
         USING DBLOCK,RE                                                        
         XC    0(256,RE),0(RE)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         GOTO1 DEMOCON,DMCB,(1,FULL),(2,WORK),(C'S',ADBLOCK),          X        
               (SPOTPROF+9,ADEST)                                               
         MVC   P(6),WORK+1                                                      
         XC    FULL,FULL                                                        
         MVI   FULL+1,C'R'                                                      
         MVC   FULL+2(1),1(R8)                                                  
         L     RE,ADBLOCK                                                       
         USING DBLOCK,RE                                                        
         XC    0(256,RE),0(RE)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         GOTO1 DEMOCON,DMCB,(1,FULL),(2,WORK),(C'S',ADBLOCK),          X        
               (SPOTPROF+9,ADEST)                                               
         MVC   P+7(6),WORK+1                                                    
         EDIT  (B4,0(R7)),(9,P+17)                                              
         EDIT  (B4,4(R7)),(14,P+27),,COMMAS=YES                                 
         EDIT  (B4,8(R7)),(10,P+42),1                                           
         EDIT  (B4,12(R7)),(10,P+53),1                                          
         GOTO1 REPORT                                                           
*                                                                               
         CLI   PROGPROF+15,C'Y'                                                 
         BNE   REQLPP2                                                          
         LA    R2,RECORD           SAVE THESE TOTALS TO THE FILE                
         USING CPIAREC,R2                                                       
         MVI   CPIDTYPE,C'D'                                                    
         MVC   CPIDAGY,OVRAGY                                                   
         MVC   CPIDMED,QMED                                                     
         MVC   CPIDCLT,=C'TOT'     FUDGE CLIENT                                 
         MVI   CPIDSERV,C'1'             SERVICE                                
         MVC   CPIDEQIV,=H'1000'                                                
*                                  SET THE DATE                                 
         GOTO1 DATCON,DMCB,(0,QEND),(3,DUB)                                     
         MVC   CPIDYEAR(1),DUB                                                  
         MVI   CPIDYEAR+1,1        Q1                                           
         CLC   QEND+2(2),=C'04'    Q2                                           
         BL    *+8                                                              
         MVI   CPIDYEAR+1,4                                                     
         CLC   QEND+2(2),=C'07'    Q3                                           
         BL    *+8                                                              
         MVI   CPIDYEAR+1,7                                                     
         CLC   QEND+2(2),=C'10'    Q4                                           
         BL    *+8                                                              
         MVI   CPIDYEAR+1,10                                                    
*                                                                               
         MVI   CPIDDP,C'Z'               DAYPART                                
         MVI   CPIDSL,30                 SPOT LENGTH                            
         MVI   CPIDPROG,C' '             PROGRAM                                
         MVI   CPIDAFFL,C' '                                                    
         MVC   CPIDMKT,=H'1'                                                    
         MVC   CPIDMKWT,=H'1'                                                   
         MVC   CPIDTARG,0(R8)                                                   
         MVC   CPIDDEMO,1(R8)                                                   
         MVC   CPIDSPOT,0(R7)                                                   
         MVC   CPIDCASH,4(R7)      NOTE - IN DOLLARS                            
         MVC   CPIDPNTS,12(R7)                                                  
         MVC   CPIDIMPS,8(R7)                                                   
         GOTO1 VWRREC                                                           
         L     RE,RECCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,RECCNT                                                        
*                                                                               
REQLPP2  LA    R8,2(R8)                                                         
         LA    R9,1(R9)                                                         
         CHI   R9,255                                                           
         BNH   REQLPP                                                           
         MVC   P(18),=C'EXTRACTED RECORDS='                                     
         EDIT  RECCNT,(7,P+18),,ALIGN=LEFT                                      
         GOTO1 REPORT                                                           
         XC    RECCNT,RECCNT                                                    
         L     R8,VSTATABC                                                      
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
STTABPR  OC    0(5,R8),0(R8)                                                    
         BZ    ENDIT                                                            
         MVC   P(5),0(R8)                                                       
         MVC   P+10(5),5(R8)                                                    
         MVC   P+20(1),10(R8)                                                   
         LA    R8,STTABLN(R8)                                                   
         GOTO1 REPORT                                                           
         B     STTABPR                                                          
         SPACE 1                                                                
ENDIT    XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
PRTDOLT  NMOD1 0,PRTDOLT                                                        
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         MVC   P+30(17),=C'INCLUDED DOLLARS='                                   
         EDIT  (P8,CLTIND),(16,P+47),2,COMMAS=YES,ALIGN=LEFT,CR=YES             
         MVC   P+64(17),=C'EXCLUDED DOLLARS='                                   
         EDIT  (P8,CLTBYPD),(16,P+82),2,COMMAS=YES,ALIGN=LEFT,CR=YES            
         MVC   P+99(14),=C'TOTAL DOLLARS='                                      
         AP    CLTBYPD,CLTIND                                                   
         AP    CLTBYPD,CLTBYOD                                                  
         EDIT  (P8,CLTBYPD),(16,P+114),2,COMMAS=YES,ALIGN=LEFT,CR=YES           
         MVC   P2+38(39),=C'OUTSIDE REQUEST DATES EXCLUDED DOLLARS='            
         EDIT  (P8,CLTBYOD),(16,P2+80),2,COMMAS=YES,ALIGN=LEFT,CR=YES           
         GOTO1 REPORT                                                           
         L     R5,=A(EXCLCAPS)     PRINT EXCLUDED WHY                           
         LA    R9,9                                                             
         LA    R7,EXCLSUM                                                       
         PRINT GEN                                                              
PRTEXCL  CP    0(8,R7),=PL8'0'     CHECK FOR DATA                               
*        BNE   *+10                                                             
         CP    8(8,R7),=PL8'0'                                                  
         BE    PRTEXCL1                                                         
         MVC   P1+61(19),0(R5)                                                  
         EDIT  (P8,8(R7)),(16,P+80),2,COMMAS=YES,ALIGN=LEFT                     
         GOTO1 REPORT                                                           
PRTEXCL1 LA    R5,19(R5)                                                        
         LA    R7,16(R7)                                                        
         BCT   R9,PRTEXCL                                                       
         PRINT NOGEN                                                            
*                                                                               
         MVC   CLTIND,=PL8'0'                                                   
         MVC   CLTINS,=PL8'0'                                                   
         MVC   CLTBYPD,=PL8'0'                                                  
         MVC   CLTBYOD,=PL8'0'                                                  
         MVC   CLTBYPS,=PL8'0'                                                  
         MVC   CLTBYOS,=PL8'0'                                                  
         MVC   EXCLSUM,=PL8'0'                                                  
         MVC   EXCLSUM+8(8*8*2+8),EXCLSUM                                       
         CLI   MODE,REQLAST                                                     
         BNE   PRTDOLTX                                                         
*        MVC   P+30(17),=C'ARB HOMES RATING='                                   
         CLI   QMED,C'C'                                                        
         BNE   *+10                                                             
         MVC   P+30(17),=C'BBM V18+  RATING='                                   
*        EDIT  (P8,AGYARB),(14,P+47),1,COMMAS=YES,ALIGN=LEFT                    
         MVC   P+63(17),=C'NSI HOMES RATING='                                   
         CLI   QMED,C'C'                                                        
         BNE   *+10                                                             
         MVC   P+63(17),=C'NSI V18+  RATING='                                   
         EDIT  (P8,AGYNSI),(14,P+80),1,COMMAS=YES,ALIGN=LEFT                    
         GOTO1 REPORT                                                           
         MVC   AGYARB,=PL8'0'                                                   
         MVC   AGYNSI,=PL8'0'                                                   
PRTDOLTX XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
REQFRSTC NMOD1 0,REQFRSTC                                                       
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         L     R2,=A(BLDEST)                                                    
         ST    R2,VBLDEST                                                       
         L     R2,=A(GETER)                                                     
         ST    R2,VGETER                                                        
         L     R2,=A(ESAVEC)                                                    
         ST    R2,VESAVEC                                                       
         L     R2,=A(STATABC)                                                   
         ST    R2,VSTATABC                                                      
         L     R2,=A(ERULESKC)                                                  
         ST    R2,VERKC                                                         
         L     R2,=A(ERULESLC)                                                  
         ST    R2,VERLC                                                         
         L     R2,=A(ERULESC)                                                   
         ST    R2,VERC                                                          
         L     R2,=A(GETDEM)                                                    
         ST    R2,VGETDEM                                                       
         L     R2,=A(DPTSVC)                                                    
         ST    R2,VDPTSVC                                                       
         L     R2,=A(PTYPSVC)                                                   
         ST    R2,VPTYPSVC                                                      
         L     R2,=A(GRYDPT)                                                    
         ST    R2,VGRYDPT                                                       
         L     R2,=V(GETSUPER)                                                  
         ST    R2,AGETSUPR                                                      
         L     R2,=V(GETSUPR4)     NEW FOR 1991                                 
         L     RE,=A(STATABC)                                                   
         LHI   RF,10000                                                         
         XCEF                                                                   
         XC    STACNTR,STACNTR                                                  
*                                                                               
         CLC   QEND(2),=C'88'      NEW TABLE FOR 88                             
         BL    *+8                                                              
         ST    R2,AGETSUPR                                                      
         L     R2,=V(GETSUPRC)                                                  
*                                                                               
         MVI   SUPRDEM,1           TOTAL HOMES FOR US                           
         CLI   QMED,C'C'           CANADIAN RUNS UNDER MEDIA C                  
         BNE   *+8                                                              
         ST    R2,AGETSUPR                                                      
*                                                                               
** WHEN REMOVING HARDCODE FOR AGY TB, NEXT 3 LINES LOOK PRETTY USELESS          
*&&DO                                                                           
         MVI   SUPRDEM,145         TOTAL V18+ FOR CANADA                        
*                                                                               
         L     R2,=V(GETSUPTB)     SUPERDEMOS FOR TBC                           
*                                                                               
         MVI   SUPRDEM,1           TOTAL HOMES FOR US                           
         CLC   OVRAGY,=C'TB'       CANADIAN RUNS UNDER MEDIA C                  
         BNE   *+8                                                              
         ST    R2,AGETSUPR                                                      
*&&                                                                             
*                                                                               
         MVC   CLTINS,=PL8'0'                                                   
         MVC   CLTIND,=PL8'0'                                                   
         MVC   CLTBYPS,=PL8'0'                                                  
         MVC   CLTBYOS,=PL8'0'                                                  
         MVC   CLTBYPD,=PL8'0'                                                  
         MVC   CLTBYOD,=PL8'0'                                                  
         MVC   AGYBYPS,=PL8'0'                                                  
         MVC   AGYBYOS,=PL8'0'                                                  
         MVC   AGYBYPD,=PL8'0'                                                  
         MVC   AGYBYOD,=PL8'0'                                                  
         MVC   AGYINS,=PL8'0'                                                   
         MVC   AGYIND,=PL8'0'                                                   
         MVC   AGYARB,=PL8'0'                                                   
         MVC   AGYNSI,=PL8'0'                                                   
         L     RE,=A(PRIBUF)                                                    
         LHI   RF,4096                                                          
         XCEF                                                                   
         MVC   EXCLSUM(8),=PL8'0'                                               
         MVC   EXCLSUM+8(8*8*2+8),EXCLSUM                                       
* READ MEDIA SUMMARY PROFILE                                                    
         MVC   WORK(10),=CL10'S000'                                             
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+2(2),=C'M1'                                                 
         CLC   QUESTOR(3),=C'CTS'                                               
         BE    *+10                                                             
         CLC   M1QCLT,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   WORK+7(3),QCLT                                                   
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         MVC   PROGPROF(16),SVPROF                                              
*                                                                               
         MVC   ATIMTAB,=A(TIMTAB)  SET DEFAULT DAYPART DEFS                     
         MVC   ATIMTABC,=A(TIMTABC)                                             
         CLI   PROGPROF+15,C'Y'    IS THIS A BANK AGENCY                        
         BNE   RFNOBANK                                                         
         MVC   ATIMTAB,=A(EPBANK)  SET BANK DAYPART DEFS                        
         MVC   ATIMTABC,=A(CMBANK)                                              
         MVC   ATIMTABS,=A(SSBANK)                                              
         MVC   ATIMTBF,=A(EPBANKF)  SET FOX BANK DAYPART DEFS                   
         MVC   ATIMTBCF,=A(CMBANKF)                                             
         MVC   ATIMTBSF,=A(SSBANKF)                                             
         CLC   QBOOK1(4),=C'0107'     ALLOW JUL/01 TO BE FORCED                 
         BNE   *+10                                                             
         MVC   QBOOK1(7),=C'ACT NOP'                                            
*        MVC   QBOOK1(7),=C'9607NOI'   PATCH JUL/96                             
         MVI   BANKPROF+4,38                                                    
         CLC   QSTART,=C'8706'                                                  
         BL    *+8                                                              
         MVI   BANKPROF+4,39       SEPT SPLIT FOR CPP 87/88                     
         MVC   PROGPROF(16),BANKPROF                                            
         MVC   SVPROF(16),BANKPROF                                              
         CLI   QMED,C'C'           NEED A BOOK FOR CANADIAN                     
         BNE   RFNOBANK                                                         
         MVC   ATIMTAB,=A(CBANK)                                                
         MVC   ATIMTABC,=A(CBANK)                                               
         MVC   PROGPROF(16),CANBPROF                                            
         MVC   SVPROF(16),CANBPROF                                              
RFNOBANK DS    0C                                                               
*  MUST CREATE BROADCAST MONTHS FOR EST FILTERING                               
         MVC   SVQDATE,QSTART                                                   
         MVI   SPOTPROF+2,0                                                     
         MVI   SPOTPROF+6,0                                                     
         MVI   SPOTPROF+7,0                                                     
         MVI   SPOTPROF+8,1                                                     
         GOTO1 MEDDATE,DMCB,(RA)                                                
RQFEND   GOTO1 ADDAY,DMCB,QEND,WORK,7                                           
         CLC   QEND(4),WORK        BROADMON END DATE                            
         BL    *+14                                                             
         MVC   QEND(6),WORK                                                     
         B     RQFEND                                                           
         MVC   SVEDATE(12),QSTART                                               
         XC    RECCNT,RECCNT                                                    
         GOTO1 DATCON,DMCB,(0,QSTART),(1,PWOSSTR)                               
         GOTO1 DATCON,DMCB,(0,QEND),(1,PWOSEND)                                 
         GOTOR PRRULE,DMCB,(RA)                                                 
         GOTOR PRPROF,DMCB,(RA)                                                 
         CLC   QUESTOR(7),=C'NORERAT'                                           
         BNE   *+14                                                             
         MVI   QRERATE,C' '                                                     
         MVC   QBOOK1(6),=C'      '                                             
         LA    RE,2                SET MDGTBY LOOKUP CODE                       
         CLI   QRERATE,C' '                                                     
         BE    SETDLU2                                                          
         CLI   QRERATE,C'A'        ADJUST ONLY                                  
         BNE   SETDLU1                                                          
         LA    RE,5                                                             
         B     SETDLU2                                                          
SETDLU1  LA    RE,3                SET FOR PURCHASE RERATED                     
         CLC   QHUT1,=C'NO'                                                     
         BE    *+8                                                              
         LA    RE,1(RE)            SET FOR ADJUSTMENT                           
         CLI   QRERATE,C'I'        RERATE BASED ON AFFID                        
         BNE   *+8                                                              
         LA    RE,3(RE)                                                         
SETDLU2  ST    RE,DLUPCOD                                                       
         CLI   REQSW,0                                                          
         BNE   SETDLU3                                                          
         MVI   REQSW,1                                                          
         MVC   VWRREC,=A(WRREC)                                                 
         MVC   MEDLCHNK,=F'200'                                                 
         MVI   RQEQUIV,C'Y'                                                     
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDNUMMO,=F'12'                                                  
         MVI   MEDEXTDM,14                                                      
         MVI   RQDAYPT,C'Y'                                                     
         L     R2,=A(OUT)                                                       
         LA    RE,=A(OUT)                                                       
         ST    R2,0(RE)                                                         
         L     R2,=A(RECORD)                                                    
         LA    RE,=A(RECORD)                                                    
         ST    R2,0(RE)                                                         
         L     R2,=A(BUFFALOC)                                                  
         ST    R2,BUFFBUFF                                                      
         LA    RE,BFREC                                                         
         ST    RE,BUFFIO                                                        
         CLC   QUESTOR(3),=C'CTS'                                               
         BE    *+10                                                             
         CLC   QUESTOR(4),=C'TST1'                                              
         BE    SETDLU3                                                          
         MVI   TAPEOPEN,1                                                       
*                                                                               
         MVC   DSNM1+13(2),OVRAGY                                               
         GOTO1 DYNALLOC,DMCB,DDM1,DSNM1                                         
         OPEN  (OUT,(OUTPUT))                                                   
         B     SETDLU3                                                          
DDM1     DC    CL8'OUT'                                                         
DSNM1    DC    CL20'SPTTAPE.SP0M1XX1'                                           
*                                                                               
SETDLU3  LA    R2,RECORD                                                        
         USING CPIAREC,R2                                                       
         CLC   QUESTOR(3),=C'CTS'                                               
         BE    *+10                                                             
         CLC   M1QCLT,=C'ALL'                                                   
         BNE   REQFEXIT                                                         
         MVC   RECORD,SPACES                                                    
         MVI   CPIATYPE,C'A'                                                    
         MVC   CPIACODE,OVRAGY                                                  
         MVC   CPIANAME,AGYNM                                                   
         MVI   CPIAWICH,C'1'                                                    
         GOTO1 VWRREC                                                           
         MVI   CPIATYPE,C'A'                                                    
         MVC   CPIACODE,OVRAGY                                                  
         MVC   CPIANAME,AGYADR                                                  
         MVI   CPIAWICH,C'2'                                                    
         GOTO1 VWRREC                                                           
         DROP  R2                                                               
         USING CPIBREC,R2                                                       
         MVC   RECORD,SPACES                                                    
         MVI   CPIBTYPE,C'B'                                                    
         MVC   CPIBAGY,OVRAGY                                                   
         MVC   CPIBSTRT,QSTART                                                  
         MVC   CPIBEND,QEND                                                     
         GOTO1 VWRREC                                                           
         DROP  R2                                                               
REQFEXIT XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
CFRST    NMOD1 0,CFRST                                                          
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         L     RE,VDPTSVC                                                       
         LHI   RF,400                                                           
         XCEF                                                                   
         L     RE,VPTYPSVC                                                      
         LHI   RF,400                                                           
         XCEF                                                                   
         LA    R2,RECORD                                                        
         USING CPICREC,R2                                                       
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         CLI   PROGPROF+5,C'N'     FORCE NSI                                    
         BNE   *+8                                                              
         MVI   CPROF+3,C'0'                                                     
         CLI   PROGPROF+5,C'A'     FORCE ARB                                    
         BNE   *+8                                                              
         MVI   CPROF+3,C'1'                                                     
         MVI   SPOTPROF+2,0                                                     
         MVI   SPOTPROF+6,0                                                     
         MVI   SPOTPROF+7,0                                                     
         MVI   SPOTPROF+8,1                                                     
         MVC   RECORD,SPACES                                                    
         MVI   CPICTYPE,C'C'                                                    
         MVC   CPICOFF,COFFICE                                                  
         MVC   CPICCODE,CLT                                                     
         MVC   CPICNAME,CLTNM                                                   
         MVI   RTGSRV,C'N'                                                      
         MVI   RQRDPOL,C'Y'                                                     
         MVI   SVONEPRD,0                                                       
         XC    CLTRCNT,CLTRCNT                                                  
*        CLI   CPROF+3,C'0'                                                     
*        BE    *+8                                                              
*        MVI   RTGSRV,C'A'                                                      
         CLC   QUESTOR(3),=C'CTS'                                               
         BE    *+10                                                             
         CLC   M1QCLT,=C'ALL'                                                   
         BNE   ADVSET                                                           
         GOTO1 VWRREC                                                           
         B     CFXIT                                                            
         SPACE 2                                                                
ADVSET   DS    0H                                                               
         DROP  R2                                                               
         USING CPIAREC,R2                                                       
         MVC   RECORD,SPACES                                                    
         MVI   CPIATYPE,C'A'                                                    
         MVC   CPIACODE,PROGPROF+2                                              
         MVC   CPIANAME(24),CLTNM                                               
         MVI   CPIAWICH,C'1'                                                    
         GOTO1 VWRREC                                                           
         MVC   RECORD,SPACES                                                    
         MVI   CPIATYPE,C'A'                                                    
         MVC   CPIACODE,PROGPROF+2                                              
         MVI   CPIAWICH,C'2'                                                    
         GOTO1 VWRREC                                                           
         DROP  R2                                                               
         USING CPIBREC,R2                                                       
         MVC   RECORD,SPACES                                                    
         MVI   CPIBTYPE,C'B'                                                    
         MVC   CPIBAGY,PROGPROF+2                                               
         MVC   CPIBSTRT,QSTART                                                  
         MVC   CPIBEND,QEND                                                     
         GOTO1 VWRREC                                                           
         DROP  R2                                                               
         USING CPICREC,R2                                                       
         MVI   CPICTYPE,C'C'                                                    
         MVI   CPICOFF,C'0'                                                     
         MVC   CPICCODE,OVRAGY                                                  
         MVI   CPICCODE+2,C' '                                                  
         MVC   CPICNAME(7),=C'AGENCY '                                          
         MVC   CPICNAME+7(2),OVRAGY                                             
         LA    RE,ANAMTAB                                                       
GAN1     CLI   0(RE),0                                                          
         BE    GAN3                                                             
         CLC   OVRAGY,0(RE)                                                     
         BE    GAN2                                                             
         LA    RE,26(RE)                                                        
         B     GAN1                                                             
GAN2     MVC   CPICNAME,2(RE)                                                   
GAN3     DS    0H                                                               
         GOTO1 VWRREC                                                           
CFXIT    XMOD1                                                                  
         LTORG                                                                  
         DROP  R3                                                               
         DROP  R2                                                               
         DROP  RE                                                               
ANAMTAB  DC    CL2'HC',CL24'NEEDHAM HARPER-CHICAGO'                             
         DC    CL2'NE',CL24'NEEDHAM HARPER'                                     
         DC    CL2'NH',CL24'NEEDHAM HARPER-NY'                                  
         DC    CL2'DF',CL24'DFS, INC.'                                          
         DC    CL2'CA',CL24'CAMPBELL-MITHUN'                                    
         DC    X'0000'                                                          
         EJECT                                                                  
BLDEST   NMOD1 PRDRDWDQ,BLDEST                                                  
         LR    R2,RC                                                            
         USING PRDRDWD,R2                                                       
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         L     RE,VESAVEC                                                       
         L     RF,=F'441000'                                                    
         XCEF                                                                   
         MVC   LASTEST,VESAVEC                                                  
         XC    ESCNTR,ESCNTR                                                    
         LA    R5,EIOAREA                                                       
         ST    R5,AREC                                                          
         XC    EKEYSAVE,EKEYSAVE                                                
         MVI   ESTFT,1                                                          
         L     R1,LASTEST                                                       
         USING ETABD,R1                                                         
READEST  BAS   RE,GETES                                                         
         CLI   RECFLAG,1                                                        
         BE    BLDEEXIT                                                         
         L     R4,AREC                                                          
         USING ESTHDR,R4                                                        
         L     R1,LASTEST                                                       
         MVC   ETPRD,EKEYPRD                                                    
         MVC   ETEST,EKEYEST                                                    
         SPACE 2                                                                
SETDEM1  LA    R5,ETDEMO                                                        
         LA    R6,ETWGHT                                                        
         LA    R7,EDEMOS                                                        
         LA    R8,20                                                            
SETDEM   MVC   0(3,R5),0(R7)                                                    
         MVC   0(1,R6),60(R7)                                                   
         LA    R5,3(R5)                                                         
         LA    R6,1(R6)                                                         
         LA    R7,3(R7)                                                         
         BCT   R8,SETDEM                                                        
         DROP  R4                                                               
         LA    R1,ETABLN(R1)                                                    
         ST    R1,LASTEST                                                       
         L     R1,ESCNTR                                                        
         LA    R1,1(R1)                                                         
         CHI   R1,5000                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         ST    R1,ESCNTR                                                        
         B     READEST                                                          
         SPACE 2                                                                
BLDEEXIT XMOD1                                                                  
         DROP  R1                                                               
         EJECT                                                                  
* READ ESTIMATES AND FILTER THEM                                                
GETES    NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ESTHDR,R4                                                        
         CLI   ESTFT,1                                                          
         BNE   GESEQ                                                            
         BAS   R9,POLEX                                                         
         MVI   RECFLAG,0                                                        
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         CLC   EKEYPRD,=C'ALL'                                                  
         BNE   *+10                                                             
         XC    EKEYPRD,EKEYPRD                                                  
         CLC   EKEYPRD,=C'POL'                                                  
         BNE   *+10                                                             
         XC    EKEYPRD,EKEYPRD                                                  
         MVC   EKEYSAVE,KEY                                                     
GEHIGH   GOTO1 HIGH                                                             
         B     GEREC                                                            
*                                                                               
GESEQ    GOTO1 SEQ                                                              
*                                                                               
GEREC    LA    R4,KEY                                                           
         MVI   ESTFT,0                                                          
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+12                                                             
         MVI   RECFLAG,1                                                        
         B     GEEXIT                                                           
         CLC   QPRD,=C'ALL'                                                     
         BE    GEFEST                                                           
         CLC   QPRD,=C'POL'                                                     
         BE    GEFEST                                                           
         CLC   EKEYPRD,QPRD        CHECK PRODUCT                                
         BE    GEFEST                                                           
         MVI   RECFLAG,1                                                        
         B     GEEXIT                                                           
*                                                                               
* FILTER KEY ON ESTIMATE NUMBER                                                 
GEFEST   CLI   EKEYEST,0                                                        
         BNE   GEFESTA                                                          
         MVI   EKEYEST,1                                                        
         B     GEHIGH                                                           
GEFESTA  DS    0H                                                               
         CLI   EKEYEST+1,0                                                      
         BE    GEFEST2                                                          
GEFEST1  MVI   EKEYEST+1,X'FF'     SET NEXT PRODUCT                             
         MVC   EKEYEST+2(4),EKEYEST+1                                           
         B     GEHIGH                                                           
*                                                                               
GEFEST2  CLI   BEST,0                                                           
         BE    GEFEST5                                                          
         CLC   EKEYEST,BEST        ESTIMATE LOW                                 
         BNL   GEFEST3                                                          
         MVC   EKEYEST,BEST         YES - SET FIRST ESTIMATE                    
         B     GEHIGH                                                           
*                                                                               
GEFEST3  CLI   BESTEND,0           END ESTIMATE GIVEN                           
         BNE   GEFEST4              YES - CHECK IT                              
         CLC   EKEYEST,BEST         NO - MUST BE EQUAL                          
         BE    GEFEST5                                                          
         B     GEFEST1                                                          
*                                                                               
GEFEST4  CLC   EKEYEST,BESTEND     ESTIMATE WITHIN RANGE                        
         BH    GEFEST1              NO - NEXT PRODUCT                           
*                                                                               
GEFEST5  GOTO1 GET                 KEY FILTERS PASSED - GET RECORD              
         L     R4,AREC                                                          
         CLC   EEND(6),SVEDATE     DATE FILTERS                                 
         BL    GESEQ                                                            
         CLC   ESTART(6),SVEDATE+6                                              
         BH    GESEQ                                                            
         CLI   QMED,C'C'           BYPASS ORDERED CHECK FOR CANADA              
         BE    GEEXIT                                                           
         ZIC   RF,EKEYEST                                                       
         L     RE,=A(POLEXCL)                                                   
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         CLI   0(RE),1             POL EXCLUDED                                 
         BE    *+12                                                             
         CLI   ECPPRS,C'N'         ESTIMATE IS EXCLUDED                         
         BNE   GEEXIT                                                           
         CLI   0(RE),1                                                          
         BNE   *+10                                                             
         MVC   P+27(6),=C'BY POL'                                               
         MVC   P(17),=C'ESTIMATE EXCLUDED'                                      
         MVC   P+18(3),EKEYPRD                                                  
         ZIC   RE,EKEYEST                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+22(3),DUB+6(2)                                                 
         GOTO1 REPORT                                                           
         B     GESEQ                                                            
*        OC    EORDN,EORDN         TAKEN OUT 10/23/01*                          
*        BNZ   GEEXIT                                *                          
*        OC    EPAIDN,EPAIDN                         *                          
*        BZ    GESEQ                                 *                          
*                                                                               
GEEXIT   XIT1                                                                   
         EJECT                                                                  
POLEX    MVI   RECFLAG,0                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         L     RE,=A(POLEXCL)                                                   
         XC    0(255,RE),0(RE)                                                  
         MVC   EKEYSAVE,KEY                                                     
PXHIGH   GOTO1 HIGH                                                             
         B     PXREC                                                            
*                                                                               
PXSEQ    GOTO1 SEQ                                                              
*                                                                               
PXREC    LA    R4,KEY                                                           
         MVI   ESTFT,0                                                          
         CLC   KEY(6),KEYSAVE      ONLY POL KEYS                                
         BNER  R9                                                               
         L     R4,AREC                                                          
         CLI   ECPPRS,C'N'         ESTIMATE IS EXCLUDED                         
         BNE   PXSEQ                                                            
         L     RE,=A(POLEXCL)                                                   
         ZIC   RF,EKEYEST                                                       
         AR    RE,RF                                                            
         MVI   0(RE),1                                                          
         B     PXSEQ                                                            
                                                                                
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
EFRSTC   NMOD1 0,M1EFRSTC                                                       
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R8,MEDBUFF                                                       
         USING MEDBLOCK,R8                                                      
         L     R4,4(R1)                                                         
         USING BFREC,R4                                                         
         MVC   PAGE,=H'1'                                                       
         L     R2,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',(R2)                                        
         GOTO1 VBLDEST,DMCB,BFREC                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',(R2)                                      
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         GOTO1 VGETER,DMCB,BFREC                                                
*                                                                               
         XC    BEFORE,BEFORE       SETUP DEFAULTS                               
         XC    AFTER,AFTER                                                      
         MVI   MEDNYBEF,C'N'                                                    
         MVI   MEDNYAFT,C'N'                                                    
         CLI   QMED,C'T'           USTV                                         
         BNE   *+8                                                              
         CLI   PROGPROF+15,C'Y'    AND BANK AGENCY                              
         BNE   *+10                                                             
         CLC   QSTART+2(2),=C'09'  AND SEPT START                               
         BNE   *+8                                                              
         MVI   SPOTPROF+2,1        SET TO MONTH START                           
         MVI   SPOTPROF+6,0                                                     
         MVI   SPOTPROF+7,0                                                     
         MVI   SPOTPROF+8,1                                                     
         MVI   MEDEXTAX,C'N'       FORCE TO INCLUDE TAX                         
         XC    PREVETAB,PREVETAB                                                
*                                                                               
         MVC   QSTART,SVQDATE                                                   
         GOTO1 DATCON,DMCB,(0,QSTART),(2,SVBSTART)                              
         GOTO1 DATCON,DMCB,(0,QEND),(2,SVBEND)                                  
         GOTO1 MEDDATE,DMCB,(RA)                                                
*        B     BA2                                                              
FIXEND   GOTO1 ADDAY,DMCB,QEND,WORK,7                                           
         CLC   QEND(4),WORK        BROADMON END DATE                            
         BL    *+14                                                             
         MVC   QEND(6),WORK                                                     
         B     FIXEND                                                           
*                                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   QSTART(12),SVQDATE                                               
         L     R9,MEDAFRST                                                      
         CLC   SVBSTART,0(R9)      FIX THE START DATE                           
         BNH   M30A                                                             
         MVC   BEFORE(2),0(R9)     SAVE BROADCAST START                         
         MVC   0(2,R9),SVBSTART    SET REAL START                               
         SPACE 2                                                                
*                          SET UP BEFORE DATES                                  
         GOTO1 DATCON,DMCB,(2,SVBSTART),(0,WORK)                                
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'-1'                                     
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,HALF)                                  
         MVC   BEFORE+2(2),HALF                                                 
         L     R9,MEDAFRST                                                      
*                                                                               
M30A     CLI   0(R9),0             CLEAR TOTAL POINTERS                         
         BE    M30B                                                             
         XC    8(4,R9),8(R9)       CLEAR THE TOTAL POINTER                      
*                                                                               
         CLC   SVBEND,2(R9)        FIX THE END DATE                             
         BNL   M30A1                                                            
*                          SET UP AFTER DATES                                   
         MVC   AFTER(4),0(R9)     SAVE BROADCAST END                            
         GOTO1 DATCON,DMCB,(2,SVBEND),(0,WORK)                                  
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,HALF)                                  
         MVC   AFTER(2),HALF                                                    
*                                                                               
         MVC   2(2,R9),SVBEND                                                   
M30A1    LA    R9,12(R9)                                                        
         B     M30A                                                             
M30B     DS    0H                                                               
         SPACE 2                                                                
         OC    BEFORE,BEFORE       LIMITED DATES                                
         BZ    BA1                                                              
         MVC   MEDBFORE,BEFORE     SET TO ACCUM EXCLUDE DATES                   
         MVI   MEDNYBEF,C'Y'                                                    
         L     RE,MEDPERD+4        SET BUFFER ADDRESSES                         
         A     RE,MEDLCHNK                                                      
         ST    RE,MEDBFORE+4                                                    
         XC    MEDBFORE+8(4),MEDBFORE+8                                         
BA1      OC    AFTER,AFTER                                                      
         BZ    BA2                                                              
         MVC   MEDAFTER,AFTER                                                   
         MVI   MEDNYAFT,C'Y'                                                    
         L     RE,MEDBFORE+4                                                    
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         L     RE,MEDPERD+4                                                     
         A     RE,MEDLCHNK                                                      
         ST    RE,MEDAFTER+4                                                    
         XC    MEDAFTER+8(4),MEDAFTER+8                                         
BA2      L     RE,MEDAFRST         SET PERIOD DATES                             
         MVC   MEDPERD(2),0(RE)                                                 
         L     RE,MEDALAST                                                      
         MVC   MEDPERD+2(2),2(RE)                                               
*                                                                               
M30B1    L     RE,PRDBUFF                                                       
         LA    RF,220                                                           
         LA    RE,28(RE)                                                        
M301     XC    3(57,RE),3(RE)      CLEAR SUBORDINATE DEMOS                      
         AH    RE,PRDBUFLN                                                      
         BCT   RF,M301                                                          
         XMOD1                                                                  
         LTORG                                                                  
         DROP  R8                                                               
         DROP  R4                                                               
         EJECT                                                                  
GETER    NMOD1 0,GETER                                                          
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R2,0(R1)                                                         
         USING BFREC,R2                                                         
         LA    R5,REC                                                           
         ST    R5,AREC                                                          
         MVC   GERKSAVE,KEY                                                     
         MVC   GERRSAVE,AREC                                                    
         L     RE,VERKC                                                         
         LHI   RF,5000                                                          
         XCEF                                                                   
         L     RE,VERLC                                                         
         LHI   RF,5000                                                          
         XCEF                                                                   
         L     RE,VERC                                                          
         LHI   RF,20000                                                         
         XCEF                                                                   
         MVC   LASTLINK,VERLC                                                   
         MVC   LASTE,VERC                                                       
         XC    RULESKCT,RULESKCT                                                
         CLC   QUESTOR,=C'CTS'                                                  
         BE    *+10                                                             
         CLC   M1QCLT,=C'ALL'                                                   
         BNE   GERCLT                                                           
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING CTXREC,R5                                                        
         MVI   CTXKTYP,C'X'                                                     
         MVC   CTXKAGY,RCAGENCY                                                 
         MVC   KEYSAVE,KEY                                                      
         L     R8,AREC                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,(R8),        X        
               (0,DMWORK)                                                       
         L     R5,AREC                                                          
         MVC   KEY,0(R8)                                                        
         CLC   KEY(12),KEYSAVE                                                  
         BNE   GEREXIT                                                          
         B     GERAGY                                                           
GERASEQ  GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,(R8),        X        
               (0,DMWORK)                                                       
GERAGY   MVC   KEY,0(R8)                                                        
         CLC   KEY(15),KEYSAVE                                                  
         BNE   GERCLT                                                           
         L     R5,AREC                                                          
         CLI   CTXKSTRT,0          ANY START DATE                               
         BE    GERAGY1                                                          
         CLC   CTXKSTRT,PWOSEND     YES - FILTER THEM                           
         BH    GERASEQ                                                          
GERAGY1  CLI   CTXKEND,0           ANY END DATE                                 
         BE    GERAGY2                                                          
         CLC   CTXKEND,PWOSSTR                                                  
         BL    GERASEQ                                                          
GERAGY2  DS    0H                                                               
         BAS   R9,GERSTR           SAVE AGENCY DEFAULTS                         
         B     GERASEQ                                                          
         SPACE 2                                                                
GERCLT   XC    KEY,KEY                                                          
         B     GEROFF                                                           
         LA    R5,KEY                                                           
         MVI   CTXKTYP,C'X'                                                     
         MVC   CTXKAGY,RCAGENCY                                                 
         MVC   CTXKCLT,CLT                                                      
         CLC   QUESTOR(3),=C'CTS'                                               
         BE    *+10                                                             
         CLC   M1QCLT,=C'ALL'                                                   
         BE    *+14                                                             
         MVC   CTXKCLT(2),PROGPROF+2                                            
         MVI   CTXKCLT+2,C'*'                                                   
         MVC   KEYSAVE,KEY                                                      
         L     R8,AREC                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,(R8),        X        
               (0,DMWORK)                                                       
         B     GERHAVC                                                          
         SPACE 2                                                                
GERCSEQ  GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,(R8),        X        
               (0,DMWORK)                                                       
         SPACE 2                                                                
GERHAVC  MVC   KEY,0(R8)                                                        
         CLC   KEY(15),KEYSAVE                                                  
         BNE   GEROFF                                                           
         L     R5,AREC                                                          
         CLI   CTXKSTRT,0                                                       
         BE    GERHAVC1                                                         
         CLC   CTXKSTRT,PWOSEND                                                 
         BH    GERCSEQ                                                          
GERHAVC1 CLI   CTXKEND,0                                                        
         BE    GERHAVC2                                                         
         CLC   CTXKEND,PWOSSTR                                                  
         BL    GERCSEQ                                                          
GERHAVC2 DS    0H                                                               
         BAS   R9,GERSTR                                                        
         B     GERCSEQ                                                          
         SPACE 2                                                                
GEROFF   XC    KEY,KEY                                                          
         B     GEREXIT                                                          
         CLC   QUESTOR(3),=C'CTS'                                               
         BE    *+10                                                             
         CLC   M1QCLT,=C'ALL'                                                   
         BNE   GEREXIT                                                          
         LA    R5,KEY                                                           
         MVI   CTXKTYP,C'X'                                                     
         MVC   CTXKAGY,RCAGENCY                                                 
         L     R8,ADCLT                                                         
         USING CLTHDR,R8                                                        
         MVC   CTXKCLT,=C'*  '                                                  
         MVC   CTXKCLT+1(1),COFFICE                                             
         MVC   KEYSAVE,KEY                                                      
         L     R8,AREC                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,(R8),        X        
               (0,DMWORK)                                                       
         B     GERHAVO                                                          
         SPACE 2                                                                
GEROSEQ  GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,(R8),        X        
               (0,DMWORK)                                                       
         SPACE 2                                                                
GERHAVO  MVC   KEY,0(R8)                                                        
         B     GEREXIT                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   GEREXIT                                                          
         L     R5,AREC                                                          
         CLI   CTXKSTRT,0                                                       
         BE    GERHAVO1                                                         
         CLC   CTXKSTRT,PWOSEND                                                 
         BH    GEROSEQ                                                          
GERHAVO1 CLI   CTXKEND,0                                                        
         BE    GERHAVO2                                                         
         CLC   CTXKEND,PWOSSTR                                                  
         BL    GEROSEQ                                                          
GERHAVO2 DS    0H                                                               
         BAS   R9,GERSTR                                                        
         B     GEROSEQ                                                          
GEREXIT  XMOD1                                                                  
         DROP  R8                                                               
         EJECT                                                                  
GERSTR   LA    R4,WORK             SET UP KEY ELEMENT                           
         USING ERULESK,R4                                                       
         XC    WORK,WORK                                                        
         CLC   QUESTOR(3),=C'CTS'                                               
         BE    *+10                                                             
         CLC   M1QCLT,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   ERKCLT,QCLT                                                      
         MVC   ERKCLT,CTXKCLT                                                   
         L     RE,PRDBUFF                                                       
         LA    RF,220                                                           
         CLC   CTXKPRD,=C'POL'                                                  
         BE    GERSTRB                                                          
         OC    CTXKPRD,CTXKPRD                                                  
         BZ    GERSTRB                                                          
GERSTRA  CLC   CTXKPRD,1(RE)       FIND PRODUCT CODE                            
         BE    GERSTRA1                                                         
         AH    RE,PRDBUFLN                                                      
         BCT   RF,GERSTRA                                                       
         BR    R9                  INVALID PRODUCT - IGNORE                     
GERSTRA1 MVC   ERKPRD,0(RE)                                                     
GERSTRB  DS    0H                                                               
         MVC   ERKEST,CTXKEST                                                   
         MVC   ERKDLNK,LASTLINK                                                 
         L     R7,VERKC                                                         
         L     R6,RULESKCT                                                      
         GOTO1 BINSRCH,DMCB,(1,WORK),(R7),(R6),12,(0,05),416                    
         MVC   RULESKCT,DMCB+8                                                  
         OC    DMCB+1(3),DMCB+1                                                 
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL - TO MANY RULES                   
         CLI   DMCB,0              BYPASS DATE ENTRIES                          
         BNE   *+8                                                              
         L     R4,DMCB                                                          
         L     R6,ERKDLNK                                                       
         USING ERULESL,R6                                                       
         XC    ERLDATE,ERLDATE                                                  
         MVC   ERLEDTE,=X'FFFF'                                                 
         XC    ERLNXT,ERLNXT                                                    
* SEARCH FOR DEMO ELEMENT                                                       
         LA    R7,CTXDATA                                                       
         XC    ERLDEM,ERLDEM                                                    
GERSTR1  CLI   0(R7),0                                                          
         BE    GERSTR2A                                                         
         CLI   0(R7),X'74'                                                      
         BE    GERSTR2                                                          
         ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     GERSTR1                                                          
GERSTR2  BAS   RE,CNVERD           CONVERT CTFILE DEMOS TO NEW FORMAT           
         LA    R7,SVCTELM                                                       
         L     R1,LASTE                                                         
         ZIC   RE,1(R7)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVELEM                                                        
         ST    R1,ERLDEM                                                        
         ZIC   R0,1(R7)                                                         
         AR    R1,R0                                                            
         XC    0(3,R1),0(R1)                                                    
         LA    R1,3(R1)                                                         
         ST    R1,LASTE                                                         
         B     GERSTR2A                                                         
         SPACE 2                                                                
STRNOD   L     R1,LASTLINK         NO DEMO ELEMENT-SAVE CONTROL                 
         AHI   R1,16                               EXIT                         
         ST    R1,LASTLINK                                                      
         B     GERSTRX                                                          
         SPACE 2                                                                
* SAVE PROGRAM TYPE/DAYPART ELEMENTS                                            
GERSTR2A LA    R7,CTXDATA                                                       
         XC    ERLPTYP,ERLPTYP                                                  
         OC    CTXKCLT(7),CTXKCLT                                               
         BNZ   STRNOD                                                           
GERSTR3  CLI   0(R7),0                                                          
         BE    STRNOD                                                           
         CLI   0(R7),X'76'         FIND PRG TYPE ELEMENTS                       
         BE    GERSTR4                                                          
GERSTR3A ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     GERSTR3                                                          
GERSTR4  CLI   4(R7),C' '                                                       
         BE    GESTR4B                                                          
         L     RF,VDPTSVC                                                       
GERSTR4A CLC   2(3,R7),0(RF)                                                    
         BE    GERSTR3A                                                         
         CLI   0(RF),0                                                          
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     GERSTR4A                                                         
         MVC   0(3,RF),2(R7)                                                    
         MVC   3(1,RF),5(R7)                                                    
         B     GERSTR3A                                                         
* SAVE PROGRAM TYPES                                                            
GESTR4B  L     RF,VPTYPSVC                                                      
GESTR4C  CLI   0(RF),0                                                          
         BE    *+12                                                             
         LA    RF,3(RF)                                                         
         B     GESTR4C                                                          
         MVC   0(2,RF),2(R7)                                                    
         MVC   2(1,RF),5(R7)                                                    
         B     GERSTR3A                                                         
GERSTRX  BR    R9                                                               
         SPACE 2                                                                
MVELEM   MVC   0(0,R1),0(R7)                                                    
         SPACE 2                                                                
         EJECT                                                                  
CNVERD   NTR1                                                                   
         ZIC   RE,1(R7)            GET LENGTH OF ELEMENT                        
         SHI   RE,2                DETERMINE NUMBER OF DEMOS                    
         ST    RE,FULL                                                          
         MHI   RE,3                                                             
         XC    SVCTELM(100),SVCTELM                                             
         MVI   SVCTELM,X'74'                                                    
         LA    RE,2(RE)            ADJUST FOR CODE AND LENGTH                   
         STC   RE,SVCTELM+1                                                     
         L     RE,FULL                                                          
         L     RF,=A(SPDEMTAB)     POINT TO CONVERT TABLE                       
         LA    R7,2(R7)            SET TO FIRST OLD DEMO                        
         LA    R6,SVCTELM+2        SET TO FIRST NEW DEMO                        
CNVERD1  ZIC   R1,0(R7)                                                         
         LTR   R1,R1               CHECK FOR NONE                               
         BZ    *+12                                                             
         TM    0(R7),X'F0'         CHECK FOR OVERRIDE                           
         BNO   CNVERD2                                                          
         STC   R1,1(R6)            YES - SAVE IT                                
         B     CNVERD3                                                          
*NVERD2  BCTR  R1,0                                                             
*        SLL   R1,1                                                             
*        L     RF,=A(SPDEMTAB)                                                  
*        LA    RF,0(R1,RF)                                                      
*        MVC   1(2,R6),0(RF)                                                    
CNVERD2  MVI   1(R6),C'R'          REFORMAT INTO A RATING                       
         STC   R1,2(R6)                                                         
CNVERD3  LA    R7,1(R7)                                                         
         LA    R6,3(R6)                                                         
         BCT   RE,CNVERD1                                                       
         XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
         DROP  R4                                                               
         EJECT                                                                  
GETDEM   NMOD1 0,GTDEM             EXTRACT DEMOS FROM ESTIMATE HEADER           
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R2,0(R1)                                                         
         USING BFREC,R2                                                         
         LA    R4,WORK                                                          
         USING ERULESK,R4                                                       
         XC    DMXAREA,DMXAREA                                                  
         XC    DEMAREA(126),DEMAREA                                             
         L     RE,SVRE             CHECK FOR DEMO SEEDED                        
         USING ETABD,RE                                                         
         CLI   ETFLAG,1            SEE IF DEMOS ALREADY SEEDED                  
         BNE   GETDEM1                                                          
         MVC   DMXAREA,ETDEMO                                                   
         CLI   32(RE),0                                                         
         BE    GETDEM1                                                          
         MVC   DMXAREA,32(RE)      SEEDED - MOVE FROM ESTIMATE                  
         B     GETDEMX                                                          
GETDEM1  DS    0H                                                               
         BAS   R9,GETRULE          SCAN FOR CLT/PRD/EST                         
         CLI   DMCB,0                                                           
         BE    GETDEM2                                                          
         MVC   SVKEST,ERKEST                                                    
         XC    ERKEST,ERKEST                                                    
         BAS   R9,GETRULE          SCAN FOR CLT/PRD                             
         CLI   DMCB,0                                                           
         BE    GETDEM2                                                          
         XC    ERKPRD,ERKPRD                                                    
         BAS   R9,GETRULE          SCAN FOR CLIENT                              
         CLI   DMCB,0                                                           
         BE    GETDEM2                                                          
         MVC   ERKEST,SVKEST                                                    
         BAS   R9,GETRULE          SCAN FOR CLIENT EST                          
         CLI   DMCB,0                                                           
         BE    GETDEM2                                                          
         XC    ERKEST,ERKEST                                                    
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVI   ERKCLT,C'*'                                                      
         MVC   ERKCLT+1(1),COFFICE                                              
         MVI   ERKCLT+2,C' '                                                    
         BAS   R9,GETRULE          SCAN FOR OFFICE                              
         CLI   DMCB,0                                                           
         BE    GETDEM2                                                          
         DROP  RE                                                               
         XC    ERKEST,ERKEST                                                    
         XC    ERKCLT,ERKCLT                                                    
         BAS   R9,GETRULE          SCAN FOR AGENCY                              
         CLI   DMCB,0                                                           
         BE    GETDEM2                                                          
         L     RE,SVRF                                                          
         MVC   DMXAREA(3),28(RE)                                                
         B     GETDEMX                                                          
         SPACE 2                                                                
GETDEM2  L     R4,DMCB             BUILD DEMO LOOKUP AREAS                      
         L     R5,ERKDLNK                                                       
         USING ERULESL,R5                                                       
         L     R6,ERLDEM                                                        
         USING CTDXD,R6                                                         
         LA    R9,DMXAREA                                                       
GETDEM3  CLI   CTDXLIST+1,0        END                                          
         BE    GETDEM6              YES - SEED DEMOS                            
         TM    CTDXLIST+1,X'F0'    EXTRACT FROM ESTIMATE                        
         BO    GETDEM4                                                          
         MVC   0(3,R9),CTDXLIST     NO - MOVE IN DEMO                           
         B     GETDEM5                                                          
         SPACE 2                                                                
GETDEM4  L     RE,SVRF             GET ESTIMATE START                           
         LA    RE,28(RE)                                                        
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),CTDXLIST+1  SET FOR EST HDR DEMO                       
         NI    HALF+1,X'0F'                                                     
         LH    R7,HALF                                                          
         BCTR  R7,0                                                             
         MHI   R7,3                                                             
         AR    RE,R7                                                            
         CLI   1(RE),0             ANY DEMO THERE                               
         BE    GETDEM5A             NO - BUMP INPUT LIST ONLY                   
         MVC   0(3,R9),0(RE)       MOVE DEMO FROM ESTIMATE RECORD               
         SPACE 2                                                                
GETDEM5  LA    R9,3(R9)            BUMP OUTPUT LIST                             
GETDEM5A LA    R6,3(R6)            BUMP INPUT LIST                              
         B     GETDEM3                                                          
         DROP  R4                                                               
         DROP  R5                                                               
         DROP  R6                                                               
         USING ETABD,RE                                                         
GETDEM6  L     RE,SVRE                                                          
         MVI   ETFLAG,1                                                         
         MVC   ETDEMO,DMXAREA                                                   
         EJECT                                                                  
GETDEMX  LA    R6,DMXAREA          FIND CORRESPONDING PNTS AND IMPS             
         CLC   SVDXAREA,DMXAREA                                                 
*        BE    GDEXIT                                                           
*                                                                               
*&&DO                                                                           
         CLC   OVRAGY,=C'TB'       BATES                                        
         BNE   GDXTBC                                                           
         GOTO1 AGETSUPR,DMCB,(X'01',DMXAREA+2)                                  
         CLI   DMXAREA+2,X'FF'                                                  
         BNE   *+6                 INVALID DEMO                                 
         DC    H'0'                                                             
         XC    DMXAREA+3(57),DMXAREA+3                                          
         MVC   DMXAREA+3(LNTBCDEM),TBCDEMS                                      
         CLI   DMXAREA+2,125       V1217 FORCE EXTRA DEMO                       
         BNE   GDXTBC                                                           
         MVC   DMXAREA+6(3),DMXAREA                                             
         MVI   DMXAREA+8,128       V1224                                        
*&&                                                                             
GDXTBC   CLI   PROGPROF+15,C'Y'    BANK AGENCY                                  
         BNE   GDXNOBNK                                                         
         GOTO1 AGETSUPR,DMCB,(X'01',DMXAREA+2)                                  
         CLI   DMXAREA+2,X'FF'                                                  
         BNE   *+6                 INVALID DEMO                                 
         DC    H'0'                                                             
         XC    DMXAREA+3(57),DMXAREA+3                                          
*        MVC   DMXAREA+3(LNBNKDEM),BANKDEMS                                     
         MVC   DMXAREA+3(LNBNKD95),BANKD95S                                     
         CLI   DEMPASS,1                                                        
         BNE   *+10                                                             
         MVC   DMXAREA+3(LNBN2D95),BAN2D95S                                     
*        MVC   DMXAREA+3(LNBNKD88),BANKD88S                                     
         CLI   QMED,C'C'           CANADIAN RUNS UNDER MEDIA C                  
         BNE   GDXNOBNK                                                         
         XC    DMXAREA+3(57),DMXAREA+3                                          
         MVC   DMXAREA+3(LNCNBNK),CNBNK                                         
GDXNOBNK DS    0C                                                               
*                                                                               
         LA    R5,DM1AREA                                                       
         XC    DEMDUP,DEMDUP                                                    
         LA    R6,DMXAREA                                                       
         LA    R1,1                SEQUENCE CONTROL                             
         SPACE 2                                                                
GETDEMX1 CLI   1(R6),0             FORCE ALL DEMOS TO RATINGS                   
         BE    GETDEMX2                                                         
         MVI   1(R6),C'R'                                                       
         STC   R1,0(R6)                                                         
         LA    R1,1(R1)                                                         
         LA    R6,3(R6)                                                         
         B     GETDEMX1                                                         
GETDEMX2 LA    R4,DMXAREA                                                       
         LR    R7,R6                                                            
GETDEMX3 CR    R4,R7               EXPLODE TO LOOKUP IMPS ALSO                  
         BNL   GETDEMX4                                                         
         MVC   0(3,R6),0(R4)                                                    
         MVI   1(R6),C'I'                                                       
         LA    R4,3(R4)                                                         
         LA    R6,3(R6)                                                         
         B     GETDEMX3                                                         
GETDEMX4 LA    R4,DMXAREA          NOW ELIMINATE DUPLICATES                     
         LA    R6,0                                                             
GETDEMX5 CLI   1(R4),0                                                          
         BE    GETDEMX6                                                         
         LA    R6,1(R6)                                                         
         ZIC   R7,2(R4)                                                         
         LA    R7,DEMDUP(R7)                                                    
         CLC   1(1,R4),0(R7)                                                    
         BNE   *+8                                                              
         MVI   0(R4),X'FF'                                                      
         MVC   0(1,R7),1(R4)       SET MODIFIER JUST PROCESSED                  
         LA    R4,3(R4)                                                         
         B     GETDEMX5                                                         
         SPACE 2                                                                
* GROUP BY DEMO NUMBER                                                          
GETDEMX6 GOTO1 XSORT,DMCB,DMXAREA,(R6),3,3,0                                    
         LA    R1,DMXAREA                                                       
GETDMX6A CLI   1(R1),0                                                          
         BE    GETDEMX7                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   GETDMX6B                                                         
         XC    0(3,R1),0(R1)                                                    
GETDMX6B MVI   0(R1),0             RESET DEMO NUMBER                            
         MVC   0(3,R5),0(R1)                                                    
         LA    R1,3(R1)                                                         
         LA    R5,3(R5)                                                         
         B     GETDMX6A                                                         
GETDEMX7 B     GETDEMXX                                                         
         SPACE 2                                                                
GETDEMXX MVC   SVDMAREA,DEMAREA                                                 
         MVC   SVDXAREA,DMXAREA                                                 
GDEXIT   MVC   DEMAREA,SVDMAREA                                                 
         XMOD1                                                                  
         SPACE 2                                                                
* GET EXTRACT RULE FROM TABLE                                                   
GETRULE  L     R6,RULESKCT                                                      
         L     R7,VERKC                                                         
         GOTO1 BINSRCH,DMCB,(0,WORK),(R7),(R6),12,(0,05),416                    
         BR    R9                                                               
SVDXAREA DS    CL63                                                             
SVKEST   DS    C                                                                
         LTORG                                                                  
DEMDUP   DS    CL255                                                            
         EJECT                                                                  
GETEQU   NMOD1 0,GETEQU                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         MVC   SVMEN,DMCB                                                       
         L     R2,0(R1)                                                         
         LA    R2,0(R2)                                                         
         USING BFREC,R2                                                         
         MVC   BFEQIV,=H'0000'    SET DEFAULT EQIVALENCE                        
         LA    RE,MENUS                                                         
GETEQU1  CLC   SVMEN,0(RE)         GET MENU                                     
         BE    GETEQU2                                                          
         CLI   0(RE),X'FF'                                                      
         BE    GETEQUX                                                          
         MVC   HALF,1(RE)                                                       
         AH    RE,HALF                                                          
         B     GETEQU1                                                          
         SPACE 2                                                                
GETEQU2  LA    RE,3(RE)            FIND SPOT LENGHT                             
         CLI   0(RE),0                                                          
         BE    GETEQUX                                                          
         CLC   BFSL,0(RE)                                                       
         BNE   GETEQU2                                                          
         MVC   BFEQIV,1(RE)                                                     
GETEQUX  XMOD1                                                                  
         SPACE 2                                                                
MENUS    DS    0C                                                               
M1ST     DC    X'02',AL2(M1EN-M1ST)                                             
         DC    AL1(10),AL2(500)                                                 
         DC    AL1(15),AL2(500)                                                 
         DC    AL1(30),AL2(1000)                                                
         DC    AL1(60),AL2(2000)                                                
         DC    X'00'                                                            
M1EN     DS    0C                                                               
M3ST     DC    X'03',AL2(M3EN-M3ST)                                             
         DC    AL1(10),AL2(500)                                                 
         DC    AL1(30),AL2(1000)                                                
         DC    AL1(45),AL2(1500)                                                
         DC    AL1(60),AL2(2000)                                                
         DC    AL1(90),AL2(3000)                                                
         DC    X'00'                                                            
M3EN     DS    0C                                                               
         DC    X'FF'                                                            
SVMEN    DS    C                                                                
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
BLDRPT   NMOD1 0,BLDRPT            BUILD AUDIT TRAIL REPORT                     
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         XC    REC(100),REC                                                     
         LA    R5,REC                                                           
         USING TDD,R5                                                           
         MVC   TDTRGT,BFTARGET     SET UP INPUT AREA                            
         MVC   TDRTGSRV,BFRTGSRV                                                
         MVC   TDDEM,BFDEMO                                                     
         MVC   TDDPT,BFDP                                                       
         MVC   TDPROG,BFPROG                                                    
*                                                                               
         MVI   TDNORI,C'I'         FLAG INDIES                                  
*        L     R4,ADSTAT                                                        
*        USING STAREC,R4                                                        
         CLI   SVRSAFF,C'A'                                                     
         BE    *+8                                                              
         CLI   SVRSAFF,C'F'                                                     
         BE    *+8                                                              
         CLI   SVRSAFF,C'N'                                                     
         BE    *+8                                                              
         CLI   SVRSAFF,C'C'                                                     
         BNE   *+8                                                              
         MVI   TDNORI,C'N'         FLAG NETWORKS                                
*                                                                               
         CLI   SVRSAFF,C'H'                                                     
         BNE   *+8                                                              
         MVI   TDNORI,C'H'         FLAG HISPANIC                                
*                                                                               
         CLI   QMED,C'C'                                                        
         BNE   *+10                                                             
         MVC   TDNORI,BFSTYP                                                    
         MVC   TDNORI,BFAFFL                                                    
         MVC   TDSPOTS,BFSPOT                                                   
         MVC   TDDOLS,BFCASH                                                    
         MVC   TDRTG,BFPNTS                                                     
         MVC   TDIMPS,BFIMPS                                                    
         MVC   TDBONUS,BFBONUS                                                  
         MVC   TDSPEC,BFSPEC                                                    
         GOTO1 BINSRCH,DMCB,(X'01',REC),TDTAB,TDCNT,TDLN,TDKLN,TDMAX            
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   TDCNT,8(R1)         SAVE COUNTER                                 
         CLI   0(R1),1                                                          
         BE    BLDRPTX                                                          
         L     R5,0(R1)            SET TO TABLE SLOT                            
         LA    RE,TDSPOTS                                                       
         LA    R5,REC              SET TO INPUT                                 
         LA    R5,TDSPOTS                                                       
         LA    R0,6                SET LOOP FOR NUMBER OF COUNTERS              
BLDRPT2  ICM   RF,15,0(R5)         AND ADD THEM UP                              
         ICM   R9,15,0(RE)                                                      
         AR    RF,R9                                                            
         STCM  RF,15,0(RE)                                                      
         LA    RE,4(RE)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,BLDRPT2                                                       
BLDRPTX  XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
CPPMRKT  NMOD1 0,CPPMRKT                                                        
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         MVC   SVCPARM,0(R1)                                                    
         ST    R1,STORER1                                                       
         LA    R2,RECORD                                                        
         USING CPIDREC,R2                                                       
         L     R5,ADBLOCK                                                       
         USING DBLOCK,R5                                                        
         XC    0(256,R5),0(R5)                                                  
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELSRC,C'N'       FORCE TO LOOKUP NSI                          
*                                                                               
         CLI   QMED,C'C'           CANADIAN                                     
         BNE   *+12                                                             
         MVI   DBSELMED,C'C'                                                    
         MVI   DBSELSRC,C'A'                                                    
*                                                                               
         XC    SVRMKT,SVRMKT                                                    
         L     RE,SVCPARM                                                       
         MVC   DBSELSTA,0(RE)                                                   
         MVI   DBSELSTA+4,C'T'                                                  
         TM    DBSELSTA,X'F0'                                                   
         BO    INVMKT                                                           
         MVI   DBFUNCT,DBGETMB                                                  
         LA    RE,DEMREC                                                        
         ST    RE,DBAREC                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 DEMAND,DMCB,ADBLOCK,SETMKT                                       
         CLI   DBERROR,NOTFOUND                                                 
         BE    INVMKT                                                           
*        MVC   DBSELBK,=X'620B'                                                 
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBSELDAY,X'40'                                                   
         MVC   DBSELTIM(2),=H'2000'                                             
         MVC   DBSELTIM+2(2),=H'2014'                                           
         MVC   DBSELAGY,=C'11'                                                  
         GOTO1 DEMAND,DMCB,ADBLOCK,SETAFF                                       
*                                                                               
CPPMRKT3 LH    RE,SVRMKT                                                        
         LTR   RE,RE                                                            
         BZ    INVMKT                                                           
         CLI   QMED,C'C'                                                        
         BE    *+8                                                              
         AHI   RE,400                                                           
         STH   RE,HALF                                                          
*        MVC   P(5),DBSELSTA                                                    
*        EDIT  (B2,HALF),(4,P+6)                                                
*        MVC   P+12(1),SVAFFL                                                   
*        MVI   P+13,C'$'                                                        
*        GOTO1 REPORT                                                           
         LH    RE,HALF                                                          
*                                                                               
         CLC   OVRAGY,=C'JW'       J WALTER                                     
         BNE   CPPMRKT4            DOESNT WANT HONOLULU FOR JULY                
         CLC   QSTART+2(2),=C'07'  THIRD QUARTER                                
         BH    CPPMRKT4                                                         
         CLC   QEND+2(2),=C'07'                                                 
         BL    CPPMRKT4                                                         
         CHI   RE,744              HONOLULU FOR JULY                            
         BE    INVMKT                                                           
*                                                                               
CPPMRKT4 L     R1,STORER1                                                       
         L     RF,SVCPARM+4                                                     
         STH   RE,0(RF)                                                         
CPPMRKTX L     R1,STORER1                                                       
         MVC   0(8,R1),SVCPARM                                                  
         MVC   0(2,R1),SVAFFL                                                   
* 93.04.08                                                                      
         MVC   SVRSAFF(2),0(R1)                                                 
         MVC   SVRSMKT,CPIDMKT                                                  
         MVC   HALF,CPIDMKT                                                     
         LH    RF,HALF                                                          
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         STC   RF,HALF                                                          
         MVI   TIMZON,1                                                         
         CLI   HALF,6                                                           
         BNE   *+8                                                              
         MVI   TIMZON,2                                                         
         CLI   HALF,7                                                           
         BNE   *+8                                                              
         MVI   TIMZON,2                                                         
         CLI   QMED,C'T'           ENSURE THAT WE ARE DOING USTV                
         BNE   M70X                                                             
*  COMPLETELY OUT 3/06/01                                                       
*        CLC   STA(4),=C'KPIX'     SPECIAL SACR. SAN FRAN SHIT                  
*        BE    *+10                                                             
*        CLC   STA(4),=C'KRON'     OUT 93/11/15***                              
*        BE    *+10                *             *                              
*        CLC   STA(4),=C'KCRA'     *             *                              
*        BE    *+10                ***************                              
*        CLC   STA(4),=C'KXTV'                                                  
*        BNE   *+8                                                              
*        MVI   TIMZON,3            M-F PRIME TO 7P-10                           
* THIS ONE BACK IN 8/9/01                                                       
         CLC   STA(4),=C'KOVR'                                                  
         BNE   *+8                                                              
         MVI   TIMZON,3            M-F PRIME TO 7P-10                           
*                                                                               
         CLC   CPIDMKT,=H'509'     FT. WAYNE                                    
         BE    *+10                                                             
         CLC   CPIDMKT,=H'581'     TERRE HAUTE                                  
         BE    *+10                                                             
         CLC   CPIDMKT,=H'588'     SOUTH BEND                                   
         BNE   M70X                                                             
         CLC   QEND+2(2),=C'05'    SWITCH TO CENTRAL FOR SUMMER                 
         BL    M70X                (FARMERS DON'T USE EDT IN INDIANA)           
         CLC   QEND+2(2),=C'09'                                                 
         BH    M70X                                                             
         MVI   TIMZON,2                                                         
M70X     CLI   SVRSAFF,C'F'        SPECIAL DPT FOR FOX                          
         BNE   *+8                                                              
         OI    TIMZON,X'10'                                                     
         CLI   SVRSAFF2,C'U'       SPECIAL DPT FOR UPN                          
         BE    *+8                                                              
         CLI   SVRSAFF2,C'W'       SPECIAL DPT FOR WB                           
         BNE   *+8                                                              
         OI    TIMZON,X'10'                                                     
* 93.04.08                                                                      
         XMOD1                                                                  
         EJECT                                                                  
INVMKT   LA    RE,GLBLTAB          GLOBAL STATIONS WILL BE INVALID              
INVMKT2  CLI   0(RE),X'FF'         SO CHECK THE TABLE                           
         BE    INVMKT3                                                          
         CLC   DBSELSTA(4),0(RE)                                                
         BE    *+12                                                             
         LA    RE,8(RE)                                                         
         B     INVMKT2                                                          
         MVC   SVRMKT,4(RE)        SET GLOBAL MARKET                            
         B     CPPMRKT3                                                         
         SPACE 2                                                                
INVMKT3  L     R1,STORER1                                                       
         MVC   0(8,R1),SVCPARM                                                  
         L     RF,4(R1)                                                         
         XC    0(2,RF),0(RF)                                                    
         MVC   P(22),=C'      NOT IN DEMO FILE'                                 
         MVC   P(5),DBSELSTA                                                    
         GOTO1 REPORT                                                           
         B     CPPMRKTX                                                         
         SPACE 2                                                                
* DEMAND COMES TO HERE WITH MARKET SET                                          
SETMKT   LA    RF,DBKEY                                                         
         USING SBKEY,RF                                                         
         CLC   SBBOOK,=X'6602'     BYPASS SOME OLD BOOKS                        
         BLR   RE                                                               
         CLI   SBBTYP,0            REJECT SPECIAL BOOKS                         
         BNER  RE                                                               
         OC    SBKMKT,SBKMKT       AND SPILL MARKETS                            
         BNZR  RE                                                               
         MVC   SVRMKT,SBRMKT       ELSE SET A MARKET NUMBER                     
         BR    RE                                                               
*                                                                               
SETAFF   NTR1                                                                   
         SR    RF,RF                                                            
         MVI   SVAFFL,C'I'                                                      
         L     RE,DBAREC                                                        
         USING DRKEY,RE                                                         
         XC    WORK,WORK                                                        
         MVC   WORK+5(5),DBACTSTA                                               
         LA    RE,DRFRSTEL                                                      
         USING QIELEM,RE                                                        
SETAFF1  CLI   0(RE),QICODEQ                                                    
         BH    SETAFFX                                                          
         BE    SETAFF2                                                          
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     SETAFF1                                                          
*                                                                               
SETAFF2  MVC   WORK(5),QIAFFIL                                                  
         MVC   SVAFFL(2),=C'  '                                                 
         CLI   QIAFFIL,C'A'                                                     
         BE    *+8                                                              
         CLI   QIAFFIL,C'C'                                                     
         BE    *+8                                                              
         CLI   QIAFFIL,C'N'                                                     
         BNE   *+10                                                             
         MVC   SVAFFL(2),=C'NN'                                                 
         CLI   QIAFFIL,C'F'        FOX(SPECIAL)                                 
         BNE   *+10                                                             
         MVC   SVAFFL(2),=C'FN'                                                 
         CLI   QIAFFIL,C'W'        WARNER                                       
         BNE   *+10                                                             
         MVC   SVAFFL(2),=C'NW'                                                 
         CLI   QIAFFIL,C'I'                                                     
         BNE   *+10                                                             
         MVC   SVAFFL(2),=C'II'                                                 
*                                                                               
         CLC   QIAFFIL(2),=C'UP'      UPN                                       
         BE    *+10                                                             
         CLC   QIAFFIL(2),=C'IU'      UPN                                       
         BNE   *+14                                                             
         MVC   SVAFFL(2),=C'NU'                                                 
         B     SETAFF3                                                          
*                                                                               
         CLI   QIAFFIL+1,C'F'                                                   
         BNE   *+10                                                             
         MVC   SVAFFL(2),=C'FN'                                                 
         CLI   QIAFFIL,C'T'                                                     
         BE    *+8                                                              
         CLI   QIAFFIL,C'U'                                                     
         BNE   *+10                                                             
         MVC   SVAFFL(2),=C'HH'                                                 
SETAFF3  MVC   WORK+10(1),SVAFFL                                                
SETAFFX  DS    0C                                                               
         L     R6,VSTATABC                                                      
         L     R5,STACNTR                                                       
         PRINT GEN                                                              
         GOTO1 BINSRCH,DMCB,(1,WORK),(R6),(R5),STTABLN,(0,10),2000              
         PRINT NOGEN                                                            
         MVC   STACNTR,DMCB+8                                                   
         OC    DMCB+1(3),DMCB+1                                                 
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL - TO MANY RULES                   
         XIT1                                                                   
         EJECT                                                                  
GLBLTAB  DS    0CL8                TABLE OF GLOBAL STATIONS/MARKETS             
*                                  CALL LETTERS                                 
*                                  BBM MARKET NUMBER                            
*                                  CSI MARKET NUMBER                            
*                                                                               
         DC    C'GBAR',AL2(5243),AL2(0457)                                      
         DC    C'GHAM',AL2(5269),AL2(0447)                                      
         DC    C'GKIN',AL2(5109),AL2(0454)                                      
         DC    C'GKIT',AL2(5339),AL2(0447)                                      
         DC    C'GLON',AL2(5369),AL2(0445)                                      
         DC    C'GOTT',AL2(5069),AL2(0444)                                      
         DC    C'GPET',AL2(5159),AL2(0456)                                      
         DC    C'GTOR',AL2(5199),AL2(0000)                                      
         DC    C'GWIN',AL2(5441),AL2(0458)                                      
         DC    C'GWND',AL2(9999),AL2(0459)                                      
         DC    C'GSUD',AL2(5479),AL2(9999)                                      
         DC    C'GTIM',AL2(9999),AL2(0446)                                      
         DC    X'FF'                                                            
         EJECT                                                                  
SVCPARM  DS    D                                                                
SVRMKT   DS    CL2                                                              
SVAFFL   DS    C                                                                
SVAFFL2  DS    C                                                                
         EJECT                                                                  
PARAM    DS    6D                                                               
STORER1  DS    F                                                                
DEMREC   DS    CL1800                                                           
         LTORG                                                                  
         EJECT                                                                  
PRRULE   NMOD1 0,PRRULE                                                         
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         MVI   FORCEHED,C'Y'                                                    
         LA    R5,REC                                                           
         ST    R5,AREC                                                          
         MVC   GERKSAVE,KEY                                                     
         MVC   GERRSAVE,AREC                                                    
         LA    R6,P1                                                            
         USING ZRPL,R6                                                          
         XC    KEY,KEY             SET TO READ RULES                            
         LA    R5,KEY                                                           
         USING CTXREC,R5                                                        
         MVI   CTXKTYP,C'X'                                                     
         MVC   CTXKAGY,RCAGENCY                                                 
         MVC   KEYSAVE,KEY                                                      
         L     R2,AREC                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,(R2),        X        
               (0,DMWORK)                                                       
         B     PRRFLTR                                                          
PRRSEQ   L     R2,AREC                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,(R2),        X        
               (0,DMWORK)                                                       
PRRFLTR  L     R5,AREC                                                          
         MVC   KEY,0(R2)                                                        
*        CLC   KEY(12),KEYSAVE     SAME AGENCY                                  
         CLC   KEY(15),KEYSAVE     SAME AGENCY - NO CLIENT                      
         BNE   PRREX               NO - EXIT                                    
         L     R5,AREC                                                          
         CLI   CTXKSTRT,0          FILTER ON REQUEST START                      
         BE    PRRFLTR1                                                         
         CLC   CTXKSTRT,PWOSEND                                                 
         BH    PRRSEQ                                                           
PRRFLTR1 CLI   CTXKEND,0           FILTER ON REQUEST END                        
         BE    PRRPRT                                                           
         CLC   CTXKEND,PWOSSTR                                                  
         BL    PRRSEQ                                                           
PRRPRT   MVC   ZRCLT,CTXKCLT       PRINT CLIENT                                 
         MVC   ZRPRD,CTXKPRD                                                    
         EDIT  CTXKEST,(3,ZREST)                                                
         OC    CTXKSTRT,CTXKSTRT   PRINT DATES                                  
         BZ    PRRPRT02                                                         
         GOTO1 DATCON,DMCB,(X'01',CTXKSTRT),(X'05',ZRSDTE)                      
PRRPRT02 OC    CTXKEND,CTXKEND                                                  
         BZ    PRRPRT4                                                          
         CLI   CTXKEND,X'FF'                                                    
         BE    PRRPRT4                                                          
         GOTO1 DATCON,DMCB,(X'01',CTXKEND),(X'05',ZREDTE)                       
         SPACE 2                                                                
PRRPRT4  LA    R7,CTXDATA          PRINT DEMOS                                  
PRRPRT4A CLI   0(R7),0                                                          
         BE    PRRPRT4B                                                         
         CLI   0(R7),X'74'                                                      
         BE    PRRPRT4B                                                         
         ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     PRRPRT4A                                                         
PRRPRT4B ZIC   R1,1(R7)                                                         
         SHI   R1,2                                                             
         LA    R7,2(R7)                                                         
         LA    RF,ZRDEM                                                         
PRRPRT4C LTR   R1,R1                                                            
         BZ    PRRPRT5                                                          
         MVI   3(RF),C','                                                       
         ZIC   RE,0(R7)                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,RF),DUB+6(2)                                                 
         TM    0(R7),X'F0'         POSITIONAL OVERRIDE                          
         BNO   PRRPT4D                                                          
         SHI   RE,240              DROP HIGH ORDER BITS                         
         MHI   RE,7                PRINT EXPANSION                              
         LA    RE,ZRDEMTAB(RE)                                                  
         MVC   0(7,RF),0(RE)                                                    
         LA    RF,4(RF)                                                         
PRRPT4D  LA    R7,1(R7)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,PRRPRT4C                                                      
         SPACE 2                                                                
PRRPRT5  LA    R7,CTXDATA          PRINT PROGTYPS AND DAYPARTS                  
         LA    RF,ZRPTYP                                                        
PRRPRT5A CLI   0(R7),0                                                          
         BE    PRRPRT6                                                          
         CLI   0(R7),X'76'                                                      
         BE    PRRPRT5C                                                         
PRRPRT5B ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     PRRPRT5A                                                         
PRRPRT5C MVI   1(RF),C'='                                                       
         MVI   5(RF),C','                                                       
         MVC   0(1,RF),5(R7)                                                    
         MVC   2(3,RF),2(R7)                                                    
         LA    RF,6(RF)                                                         
         B     PRRPRT5B                                                         
         SPACE 2                                                                
PRRPRT6  GOTO1 REPORT                                                           
         B     PRRSEQ              NEXT RECORD                                  
         SPACE 2                                                                
PRREX    MVI   FORCEHED,C'Y'                                                    
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
ZRDEMTAB DC    CL7'NONE'                                                        
         DC    CL7'FIRST'                                                       
         DC    CL7'SECOND'                                                      
         DC    CL7'THIRD'                                                       
         DC    CL7'FOURTH'                                                      
         EJECT                                                                  
CHKSWP   NMOD1 0,CHKSWP            IS SPECIAL IN A SWEEP WEEK                   
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         CLI   SPOTTYPE,C'S'       ONLY DO FOR SPECIALS                         
         BNE   CHKSWPX                                                          
         GOTO1 MEDGETBY,DMCB,(RA),3    GET ACTUAL BOOKS                         
         L     R0,4(R1)            SAVE ACTUAL BOOK LIST                        
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         MVI   BYTE,C'N'           PRESET TO NORMAL                             
         GOTO1 DATCON,DMCB,(3,BDSTART),(2,FULL)                                 
         LR    RE,R0               FIND THE BOOK USED                           
         CLC   FULL(2),2(RE)                                                    
         BNH   *+12                                                             
         LA    RE,6(RE)                                                         
         B     *-14                                                             
         MVC   HALF,4(RE)          SAVE THE BOOK                                
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SWEEPTBL  GET A(SWEEP TABLE)                           
         ICM   R5,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
         USING SWPTABLD,R5                                                      
CHKSWP1  CLI   0(R5),0             NOW CHECK AGAINST BUY DATES                  
         BE    CHKSWPX                                                          
         CLC   HALF,SWPTBOOK       FIND SWEEP DATES                             
         BE    *+10                                                             
         AR    R5,R0                                                            
         B     CHKSWP1                                                          
         CLC   RTGSRV,SWPTSRC      FOR THIS RATING SERVICE                      
         BNE   *-12                                                             
         GOTO1 DATCON,DMCB,(10,SWPTST),WORK   GET END DATE                      
         DROP  R5                                                               
*                                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+6,28                                        
         GOTO1 DATCON,DMCB,WORK,(3,DUB)                                         
         GOTO1 DATCON,DMCB,WORK+6,(3,DUB+3)                                     
         CLC   BDEND,DUB+3         ENTIRE PERIOD WITHIN BOOK                    
         BH    *+8                                                              
         MVI   BYTE,C'R'           SET TO RERATE                                
*                                                                               
CHKSWPX  XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
PRPROF   NMOD1 0,PRPROF                                                         
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         LA    R1,P1               INITIALIZE PRINT ADDRESS                     
         ST    R1,APL                                                           
         LA    R6,PROGPROF         SET TO PROGRAM PROFILE                       
         LA    R7,PRFTAB           INTIALIZE DESCRIPTION ADDRESS                
         ST    R7,AFDESC                                                        
         MVC   TAB1,2(R7)          SAVE FIRST LENGTH                            
PRPROF1  SR    RE,RE               FIND LONGEST CONSTANT AND                    
         ICM   RE,3,0(R7)                                                       
         AR    R7,RE               AND SAVE AS TAB POSITION                     
         OC    0(2,R7),0(R7)       END                                          
         BZ    PRPROF2                                                          
         CLC   TAB1,2(R7)          REPLACE IF CURRENT IS MORE                   
         BH    *+10                                                             
         MVC   TAB1,2(R7)                                                       
         B     PRPROF1                                                          
         SPACE 2                                                                
PRPROF2  L     R7,AFDESC           GET PROFILE BYTE                             
         MVC   BYTE,0(R6)                                                       
         BAS   RE,PRDESC           PRINT THE FIELD DESC AND VALUE               
         LA    R6,1(R6)            NEXT PROFILE BYTE                            
         SR    R1,R1                                                            
         ICM   R1,3,0(R7)          NEXT DESCRIPTION                             
         AR    R7,R1                                                            
         ST    R7,AFDESC           SAVE ADDRESS OF NEXT DESC.                   
         L     R1,APL                                                           
         LA    R1,132(R1)          NEXT PRINT LINE                              
         ST    R1,APL                                                           
         OC    0(2,R7),0(R7)                                                    
         BNZ   PRPROF2                                                          
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
         XMOD1                                                                  
         EJECT                                                                  
PRDESC   NTR1  ,                   PRINT FIELD DESCRIPTION AND VALUE            
         L     RE,AFDESC                                                        
         L     RF,APL                                                           
         ZIC   R6,2(RE)            GET FIELD CAPTION                            
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),3(RE)       MOVE IN FIELD DESCRIPTION                    
         ZIC   R6,TAB1                                                          
         LA    RF,2(RF,R6)         SET TO VALUE POSITION                        
         ZIC   R6,2(RE)                                                         
         LA    RE,3(RE,R6)                                                      
         MVC   0(1,RF),BYTE                                                     
         TM    BYTE,X'C0'          QUICK TEST FOR ALPHA                         
         BNZ   PRDESC2                                                          
         CLI   BYTE,0                                                           
         BE    PRDESC2                                                          
         ZIC   R1,BYTE                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,RF),DUB+6(2)    PRINT AS NUMERIC VALUE                       
PRDESC2  CLI   0(RE),0             PRINT                                        
         BE    PRDESCX                                                          
         CLC   BYTE,1(RE)          CHECK FOR EXPANSION                          
         BE    PRDESC3             EXPAND IT                                    
         ZIC   R6,0(RE)            TRY THE NEXT VALUE                           
         AR    RE,R6                                                            
         B     PRDESC2                                                          
PRDESC3  ZIC   R6,0(RE)            GET EXPANSION LENGTH                         
         SHI   R6,3                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),2(RE)                                                    
PRDESCX  XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
AFDESC   DC    F'0'                                                             
APL      DC    F'0'                                                             
TAB1     DC    X'00'                                                            
*                                                                               
*  M1 PROFILE FIELD DESCRIPTIONS                                                
PRFTAB   DS    0C                                                               
PRFF1ST  DC    AL2(PRFF1EN-PRFF1ST+1)                                           
         DC    AL1(15),CL15'SPECIAL CONTROL'                                    
         DC    AL1(06),C'O',CL4'OMIT'                                           
         DC    AL1(08),C'N',CL6'NORMAL'                                         
         DC    AL1(08),C'R',CL6'RERATE'                                         
PRFF1EN  DC    X'00'                                                            
PRFF2ST  DC    AL2(PRFF2EN-PRFF2ST+1)                                           
         DC    AL1(19),CL19'EQUIVALENCE CONTROL'                                
PRFF2EN  DC    X'00'                                                            
PRFF3ST  DC    AL2(PRFF3EN-PRFF3ST+1)                                           
         DC    AL1(14),CL14'ADVETSR CODE 1'                                     
PRFF3EN  DC    X'00'                                                            
PRFF4ST  DC    AL2(PRFF4EN-PRFF4ST+1)                                           
         DC    AL1(14),CL14'ADVETSR CODE 2'                                     
PRFF4EN  DC    X'00'                                                            
PRFF5ST  DC    AL2(PRFF5EN-PRFF5ST+1)                                           
         DC    AL1(19),CL19'ACTUAL BOOK CONTROL'                                
PRFF5EN  DC    X'00'                                                            
PRFF6ST  DC    AL2(PRFF6EN-PRFF6ST+1)                                           
         DC    AL1(20),CL20'RATING SERVICE FORCE'                               
PRFF6EN  DC    X'00'                                                            
PRFF7ST  DC    AL2(PRFF7EN-PRFF7ST+1)                                           
         DC    AL1(19),CL19'EXCLUDE BONUS SPOTS'                                
PRFF7EN  DC    X'00'                                                            
PRFF8ST  DC    AL2(PRFF8EN-PRFF8ST+1)                                           
         DC    AL1(08),CL08'NOT USED'                                           
PRFF8EN  DC    X'00'                                                            
PRFF9ST  DC    AL2(PRFF9EN-PRFF9ST+1)                                           
         DC    AL1(08),CL08'NOT USED'                                           
PRFF9EN  DC    X'00'                                                            
PRFFAST  DC    AL2(PRFFAEN-PRFFAST+1)                                           
         DC    AL1(08),CL08'NOT USED'                                           
PRFFAEN  DC    X'00'                                                            
PRFFBST  DC    AL2(PRFFBEN-PRFFBST+1)                                           
         DC    AL1(08),CL08'NOT USED'                                           
PRFFBEN  DC    X'00'                                                            
PRFFCST  DC    AL2(PRFFCEN-PRFFCST+1)                                           
         DC    AL1(08),CL08'NOT USED'                                           
PRFFCEN  DC    X'00'                                                            
PRFFDST  DC    AL2(PRFFDEN-PRFFDST+1)                                           
         DC    AL1(08),CL08'NOT USED'                                           
PRFFDEN  DC    X'00'                                                            
PRFFEST  DC    AL2(PRFFEEN-PRFFEST+1)                                           
         DC    AL1(08),CL08'NOT USED'                                           
PRFFEEN  DC    X'00'                                                            
PRFFFST  DC    AL2(PRFFFEN-PRFFFST+1)                                           
         DC    AL1(11),CL11'BANK AGENCY'                                        
PRFFFEN  DC    X'00'                                                            
         DC    X'0000'                                                          
         EJECT                                                                  
* PRINT THE AUDIT REPORT                                                        
ARPRINT  NMOD1 0,ARPRINT                                                        
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         USING SPWORKD,RA,RC                                                    
         L     R3,=A(SPM1WK)                                                    
         USING SPM1WK,R3                                                        
         CLI   QMED,C'C'                                                        
         BNE   *+10                                                             
         MVC   ARBCAP,=C'BBM'                                                   
         L     R4,TDCNT            GET NUMBER OF ITEMS                          
         L     R5,TDTAB            POINT TO START OF TABLE                      
         USING TDD,R5                                                           
         LTR   R4,R4               CHECK FOR ACTIVITY                           
         BZ    ARPEXIT                                                          
         LA    R6,P1               POINT TO PRINT LINE                          
         USING ARPRINTD,R6                                                      
ARPRT2   MVC   FULL(3),TDTRGT      RETURN HERE FOR NEXT LINE                    
         BAS   RE,GETDNAM          GET TARGET DEMO NAME                         
         MVC   ARTRGT,WORK                                                      
         MVC   FULL,TDDEM          GET REPORT DEMO NAME                         
         BAS   RE,GETDNAM                                                       
         MVC   ARDEM,WORK                                                       
         MVC   ARRTGSRV(1),TDRTGSRV SET UP R/S                                  
         CLI   TDRTGSRV,C'A'                                                    
         BNE   *+10                                                             
         MVC   ARRTGSRV,ARBCAP                                                  
         CLI   TDRTGSRV,C'N'                                                    
         BNE   *+10                                                             
         MVC   ARRTGSRV,NSICAP                                                  
         SPACE 2                                                                
         MVC   ARDPT(1),TDDPT                                                   
         LA    RE,DPTNAM           GET THE DAYPART EXPANSION                    
ARPRT4   CLI   0(RE),X'00'                                                      
         BE    ARPRT4X                                                          
         CLC   TDDPT,0(RE)                                                      
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     ARPRT4                                                           
         MVC   ARDPT(3),1(RE)                                                   
ARPRT4X  DS    0H                                                               
         SPACE 2                                                                
         MVC   ARPROG(1),TDPROG                                                 
         LA    RE,PROGNAM          GET THE DAYPART EXPANSION                    
ARPRT6   CLI   0(RE),X'00'                                                      
         BE    ARPRT6X                                                          
         CLC   TDPROG,0(RE)                                                     
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     ARPRT6                                                           
         MVC   ARPROG(3),1(RE)                                                  
ARPRT6X  DS    0H                                                               
         SPACE 2                                                                
         MVC   ARNORI(1),TDNORI                                                 
         CLI   TDNORI,C'I'                                                      
         BNE   *+10                                                             
         MVC   ARNORI,=C'IND'                                                   
         CLI   TDNORI,C'N'                                                      
         BNE   *+10                                                             
         MVC   ARNORI,=C'NET'                                                   
         CLI   TDNORI,C'H'                                                      
         BNE   *+10                                                             
         MVC   ARNORI,=C'HSP'                                                   
         SPACE 2                                                                
         EDIT  TDSPOTS,(6,ARSPOTS)                                              
         EDIT  TDDOLS,(11,ARDOLS),2                                             
         EDIT  TDRTG,(8,ARRTG),1                                                
         EDIT  TDIMPS,(11,ARIMPS),1                                             
         EDIT  TDSPEC,(11,ARSPEC),2                                             
         EDIT  TDBONUS,(6,ARBONUS)                                              
         GOTO1 REPORT                                                           
         CLC   TDRTGSRV(1),TDLN(R5) RTG SRV CHANGE                              
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         LA    R5,TDLN(R5)         GET NEXT LINE                                
         BCT   R4,ARPRT2           LOOP UNTIL DONE                              
ARPEXIT  MVI   FORCEHED,C'Y'                                                    
         XMOD1                                                                  
         EJECT                                                                  
GETDNAM  NTR1                      GET THE DEMO NAMES                           
         L     RE,ADBLOCK                                                       
         USING DBLOCK,RE                                                        
         XC    0(256,RE),0(RE)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         GOTO1 DEMOCON,DMCB,(1,FULL),(2,WORK),(C'S',ADBLOCK),          X        
               (SPOTPROF+9,ADEST)                                               
         XIT1                                                                   
         EJECT                                                                  
DPTNAM   DC    C'A',C'EAM'                                                      
         DC    C'C',C'DAY'                                                      
         DC    C'E',C'WEM'                                                      
         DC    C'G',C'WEA'                                                      
         DC    C'J',C'ELY'                                                      
         DC    C'K',C'SUP'                                                      
         DC    C'L',C'PAC'                                                      
         DC    C'N',C'PRI'                                                      
         DC    C'P',C'LTE'                                                      
         DC    C'R',C'LLT'                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
PROGNAM  DC    C'N',C'NWS'                                                      
         DC    C'S',C'SPO'                                                      
         DC    C'K',C'KID'                                                      
         DC    C'F',C'FAM'                                                      
         DC    C'M',C'MOV'                                                      
         DC    C'O',C'REG'                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
ARBCAP   DC    C'ARB'                                                           
NSICAP   DC    C'NSI'                                                           
         LTORG                                                                  
         EJECT                                                                  
CPMWGHT  DS    0D                                                               
         DC    AL2(101),AL2(7694)  NEW YORK                                     
         DC    AL2(403),AL2(5184)  LOS ANGELES                                  
         DC    AL2(202),AL2(3518)  CHICAGO                                      
         DC    AL2(104),AL2(2889)  PHILADELPHIA                                 
         DC    AL2(407),AL2(2419)  SAN FRANCISCO                                
         DC    AL2(106),AL2(2260)  BOSTON                                       
         DC    AL2(105),AL2(1913)  DETRIOT                                      
         DC    AL2(111),AL2(1776)  WASHINGTON,DC                                
         DC    AL2(110),AL2(1668)  CLEVELAND                                    
         DC    AL2(223),AL2(1767)  DALLAS-FORT WORTH                            
         DC    AL2(218),AL2(1649)  HOUSTON                                      
         DC    AL2(108),AL2(1424)  PITTSBURG                                    
         DC    AL2(128),AL2(1339)  MIAMI-FT. LAUDERDALE                         
         DC    AL2(419),AL2(1323)  SEATTLE-TACOMA                               
         DC    AL2(168),AL2(1332)  ATLANTA                                      
         DC    AL2(213),AL2(1332)  MINNEAPOLIS-ST. PAUL                         
         DC    AL2(139),AL2(1301)  TAMPA-ST PETERSBURG                          
         DC    AL2(209),AL2(1227)  ST. LOUIS                                    
         DC    AL2(351),AL2(1124)  DENVER                                       
         DC    AL2(112),AL2(1002)  BALTIMORE                                    
         DC    AL2(462),AL2(1057)  SACRAMENTO-STOCKTON                          
         DC    AL2(127),AL2(0982)  INDIANAPOLIS                                 
         DC    AL2(420),AL2(0935)  PORTLAND,0R                                  
         DC    AL2(353),AL2(0943)  PHOENIX                                      
         DC    AL2(425),AL2(0879)  SAN DIEGO                                    
         DC    AL2(133),AL2(0980)  HARTFORD-NEW HAVEN                           
         DC    AL2(115),AL2(0835)  CINCINNATI                                   
         DC    AL2(259),AL2(0809)  NASHVILLE                                    
         DC    AL2(217),AL2(0804)  MILWAUKEE                                    
         DC    AL2(216),AL2(0825)  KANSAS CITY                                  
         DC    AL2(134),AL2(0778)  ORLANDO-DAYTONA BEACH                        
         DC    AL2(114),AL2(0722)  BUFFALO                                      
         DC    AL2(121),AL2(0620)  PROVIDENCE                                   
         DC    AL2(222),AL2(0744)  NEW ORLEANS                                  
         DC    AL2(117),AL2(0738)  CHARLOTTE                                    
         DC    AL2(135),AL2(0701)  COLUMBUS,OH                                  
         DC    AL2(167),AL2(0710)  GRNVLL-SPART-ASHEVILL                        
         DC    AL2(163),AL2(0658)  GRAND RAPIDS-KALAMAZOO                       
         DC    AL2(240),AL2(0641)  MEMPHIS                                      
         DC    AL2(230),AL2(0677)  BIRMINGHAM                                   
         DC    AL2(160),AL2(0667)  RALEIGH-DURHAM                               
         DC    AL2(370),AL2(0667)  SALT LAKE CITY                               
         DC    AL2(250),AL2(0697)  OKLAHOMA CITY                                
         DC    AL2(129),AL2(0631)  LOUISVILLE                                   
         DC    AL2(241),AL2(0603)  SAN ANTONIO                                  
         DC    AL2(164),AL2(0583)  CHARLESTON-HUNTINGTON                        
         DC    AL2(144),AL2(0584)  NORFOLK-PORTSMTH-NWPRT NWS                   
         DC    AL2(166),AL2(0601)  HRRSBRG-LANCSTR-LEB-YRK                      
         DC    AL2(177),AL2(0582)  WILKES BARRE-SCRANTON                        
         DC    AL2(132),AL2(0562)  ALBANY-SCHNCTADY-TROY                        
         DC    AL2(142),AL2(0565)  DAYTON                                       
         DC    AL2(118),AL2(0552)  GRNSBORO-H.PNT-W.SALEM                       
         DC    AL2(113),AL2(0511)  FLINT-SAGINAW-BAY CITY                       
         DC    AL2(293),AL2(0507)  LITTLE ROCK-PINE BUFF                        
         DC    AL2(156),AL2(0513)  RICHMOND-PETERSBURG                          
         DC    AL2(212),AL2(0533)  SHREVEPORT                                   
         DC    AL2(155),AL2(0518)  SYRACUSE                                     
         DC    AL2(271),AL2(0537)  TULSA                                        
         DC    AL2(278),AL2(0492)  WICHITA-HUTCHINSON                           
         DC    AL2(147),AL2(0480)  TOLEDO                                       
         DC    AL2(157),AL2(0467)  KNOXVILLE                                    
         DC    AL2(286),AL2(0468)  MOBILE-PENSACOLA                             
         DC    AL2(390),AL2(0464)  ALBUQUERQUE                                  
         DC    AL2(466),AL2(0460)  FRESNO(VISALIA)                              
         DC    AL2(161),AL2(0450)  JACKSONVILLE                                 
         DC    AL2(173),AL2(0410)  ROANOKE-LYNCHBURG                            
         DC    AL2(148),AL2(0458)  WEST PALM BEACH                              
         DC    AL2(258),AL2(0420)  GREEN BAY                                    
         DC    AL2(279),AL2(0412)  DES MOINES AMES                              
         DC    AL2(252),AL2(0402)  OMAHA                                        
         DC    AL2(481),AL2(0395)  SPOKANE                                      
         DC    AL2(138),AL2(0407)  ROCHESTER                                    
         DC    AL2(248),AL2(0395)  CHMPAGN-SPRNGFLD-DECATUR                     
         DC    AL2(100),AL2(0390)  PORTLAND-POLAND SPRING                       
         DC    AL2(185),AL2(0387)  AKRON                                        
         DC    AL2(282),AL2(0386)  DAVENPRT-R.ISL-MOLINE                        
         DC    AL2(237),AL2(0378)  CEDAR RAPIDS-WATERLOO                        
         DC    AL2(232),AL2(0378)  PADUCAH-C.GIRARDEAU-HRRBRG                   
         DC    AL2(174),AL2(0333)  JOHNSTOWN-ALTOONA                            
         DC    AL2(344),AL2(0379)  HONOLULU                                     
         DC    AL2(175),AL2(0331)  CHATTANOOGA                                  
         DC    AL2(131),AL2(0321)  TRI CITIES, TN-VA                            
         DC    AL2(188),AL2(0321)  SOUTH BEND-ELKHART                           
         DC    AL2(141),AL2(0338)  LEXINGTON                                    
         DC    AL2(219),AL2(0320)  SPRINGFIELD,MO                               
         DC    AL2(318),AL2(0321)  JACKSON,MS                                   
         DC    AL2(389),AL2(0320)  TUCSON                                       
         DC    AL2(322),AL2(0298)  LINCOLN-HSTINGS-KEARNEY                      
         DC    AL2(235),AL2(0337)  AUSTIN,TX                                    
         DC    AL2(249),AL2(0294)  EVANSVILLE                                   
         DC    AL2(291),AL2(0292)  HUNTSVILLE-DECATUR                           
         DC    AL2(316),AL2(0300)  BATON ROUGE                                  
         DC    AL2(136),AL2(0277)  YOUNGSTOWN                                   
         DC    AL2(146),AL2(0289)  COLUMBIA,SC                                  
         DC    AL2(109),AL2(0269)  FT. WAYNE                                    
         DC    AL2(143),AL2(0272)  SPRINGFIELD-HOLYOKE                          
         DC    AL2(123),AL2(0270)  BURLINGTON-PLATTSBURG                        
         DC    AL2(145),AL2(0269)  GRNVLLE-N.BERN-WASHGTN                       
         DC    AL2(325),AL2(0263)  SIOUX FALLS                                  
         DC    AL2(225),AL2(0267)  WACO-TEMPLE                                  
         DC    AL2(151),AL2(0246)  LANSING                                      
         DC    AL2(275),AL2(0254)  PEORIA                                       
         DC    AL2(324),AL2(0246)  FARGO-VALLEY CITY                            
         DC    AL2(352),AL2(0245)  COLORADO SPRINGS-PUEBLO                      
         DC    AL2(269),AL2(0241)  MADISON                                      
         DC    AL2(439),AL2(0256)  LAS VEGAS                                    
         DC    AL2(120),AL2(0244)  AUGUSTA,GA                                   
         DC    AL2(242),AL2(0228)  LAFAYETTE,LA                                 
         DC    AL2(365),AL2(0240)  EL PASO                                      
         DC    AL2(107),AL2(0220)  SAVANNAH                                     
         DC    AL2(210),AL2(0224)  ROCKFORD                                     
         DC    AL2(154),AL2(0211)  WHEELING-STUBNVILLE                          
         DC    AL2(428),AL2(0229)  MONTEREY-SALINAS                             
         DC    AL2(298),AL2(0219)  MONTGOMERY                                   
         DC    AL2(119),AL2(0230)  CHARLESTON,SC                                
         DC    AL2(228),AL2(0217)  MONROE-EL DORADO                             
         DC    AL2(122),AL2(0211)  COLUMBUS,GA                                  
         DC    AL2(102),AL2(0212)  BINGHAMTON                                   
         DC    AL2(455),AL2(0216)  SANTA BARBARA-SANTA MARIA                    
         DC    AL2(276),AL2(0202)  DULUTH-SUPERIOR                              
         DC    AL2(181),AL2(0205)  TERRE HAUTE                                  
         DC    AL2(234),AL2(0209)  AMARILLO                                     
         DC    AL2(171),AL2(0222)  FT. MEYERS                                   
         DC    AL2(410),AL2(0202)  YAKIMA                                       
         DC    AL2(203),AL2(0203)  JOPLIN-PITTSBURG                             
         DC    AL2(401),AL2(0189)  EUGENE                                       
         DC    AL2(227),AL2(0186)  WICHITA FALLS-LAWTON                         
         DC    AL2(292),AL2(0206)  BEAUMONT-PORT ARTHUR                         
         DC    AL2(130),AL2(0183)  TALLAHASSEE-THOMASVILLE                      
         DC    AL2(411),AL2(0188)  RENO                                         
         DC    AL2(236),AL2(0199)  HARLINGTON-WESLACO                           
         DC    AL2(204),AL2(0166)  COLUMBIA-JEFFERSON CITY                      
         DC    AL2(200),AL2(0196)  CORPUS CHRISTI                               
         DC    AL2(305),AL2(0194)  WAUSAU                                       
         DC    AL2(170),AL2(0124)  FLORENCE,SC                                  
         DC    AL2(140),AL2(0176)  TRAVERSE CITY-CADILLAC                       
         DC    AL2(302),AL2(0178)  LA CROSS-EAU CLAIR                           
         DC    AL2(224),AL2(0174)  SIOUX CITY                                   
         DC    AL2(116),AL2(0177)  ERIE                                         
         DC    AL2(103),AL2(0176)  MACON                                        
         DC    AL2(357),AL2(0177)  BOISE                                        
         DC    AL2(251),AL2(0177)  LUBBOCK                                      
         DC    AL2(211),AL2(0169)  MASON CITY-AUSTIN-ROCHESTER                  
         DC    AL2(205),AL2(0157)  TOPEKA                                       
         DC    AL2(468),AL2(0173)  CHICO-REDDING                                
         DC    AL2(273),AL2(0168)  COLUMBUS-TUPELO                              
         DC    AL2(287),AL2(0168)  MINOT-BSMRK-DICKINSON                        
         DC    AL2(159),AL2(0162)  BKLY-BLUEFIELD-OAK HILL                      
         DC    AL2(270),AL2(0154)  FT. SMITH                                    
         DC    AL2(400),AL2(0162)  BAKERSFIELD                                  
         DC    AL2(233),AL2(0159)  ODESSA-MIDLAND-MONAHANS                      
         DC    AL2(317),AL2(0145)  QUINCY-HANNIBLE-KEOKUK                       
         DC    AL2(137),AL2(0144)  BANGOR                                       
         DC    AL2(150),AL2(0197)  WILMINGTON                                   
         DC    AL2(125),AL2(0137)  ALBANY,GA                                    
         DC    AL2(413),AL2(0132)  MEDFORD-KALMATH FALLS                        
         DC    AL2(194),AL2(0130)  HAGERSTOWN                                   
         DC    AL2(262),AL2(0135)  ABILENE-SWEETWATER                           
         DC    AL2(199),AL2(0118)  SARASOTA  *******                            
         DC    AL2(126),AL2(0114)  UTICA                                        
         DC    AL2(309),AL2(0118)  TYLER                                        
         DC    AL2(358),AL2(0109)  IDAHO FALLS-POCATELLO                        
         DC    AL2(206),AL2(0116)  DOTHAN                                       
         DC    AL2(364),AL2(0110)  RAPID CITY                                   
         DC    AL2(297),AL2(0057)  ALEXANDRIA,MN                                
         DC    AL2(310),AL2(0099)  HATTIESBURG-LAUREL                           
         DC    AL2(405),AL2(0091)  PALM SPRINGS                                 
         DC    AL2(343),AL2(0117)  ANCHORAGE                                    
         DC    AL2(176),AL2(0095)  SALISBURY                                    
         DC    AL2(260),AL2(0094)  ANNISTON                                     
         DC    AL2(198),AL2(0093)  CLARKSBURG-WESTON                            
         DC    AL2(243),AL2(0090)  LAKE CHARLES                                 
         DC    AL2(356),AL2(0097)  BILLINGS                                     
         DC    AL2(192),AL2(0087)  GAINSVILLE                                   
         DC    AL2(311),AL2(0083)  MERIDAN                                      
         DC    AL2(256),AL2(0076)  PANAMA CITY                                  
         DC    AL2(362),AL2(0083)  MISSOULA                                     
         DC    AL2(257),AL2(0084)  ADA-ARDMORE                                  
         DC    AL2(244),AL2(0103)  ALEXANDRIA,LA                                
         DC    AL2(334),AL2(0077)  JONESBORO                                    
         DC    AL2(247),AL2(0089)  GREENWOOD                                    
         DC    AL2(355),AL2(0076)  GREAT FALLS                                  
         DC    AL2(149),AL2(0076)  WATERTOWN                                    
         DC    AL2(354),AL2(0077)  BUTTE                                        
         DC    AL2(238),AL2(0069)  ST. JOSEPH                                   
         DC    AL2(371),AL2(0075)  YUMA-EL CENTRO                               
         DC    AL2(359),AL2(0069)  CHYENN-SCOTTSBLUF-STERLING                   
         DC    AL2(197),AL2(0072)  PARKERSBURG                                  
         DC    AL2(346),AL2(0074)  BILOXI                                       
         DC    AL2(367),AL2(0066)  CASPER-RIVERTON                              
         DC    AL2(337),AL2(0060)  MANKATO                                      
         DC    AL2(402),AL2(0067)  EUREKA                                       
         DC    AL2(361),AL2(0066)  ROSWELL                                      
         DC    AL2(153),AL2(0057)  MARQUETTE                                    
         DC    AL2(373),AL2(0066)  GRAND JUCTION-MONTROSE                       
         DC    AL2(239),AL2(0051)  JACKSON,TN                                   
         DC    AL2(231),AL2(0052)  OTTUMWA-KIRKSVILLE                           
         DC    AL2(338),AL2(0059)  BOWLING GREEN                                
         DC    AL2(182),AL2(0051)  LAFAYETTE,IN ******                          
         DC    AL2(158),AL2(0048)  LIMA                                         
         DC    AL2(350),AL2(0048)  FLAGSTAFF *********                          
         DC    AL2(261),AL2(0045)  SAN ANGELO                                   
         DC    AL2(169),AL2(0042)  HARRISONBURG                                 
         DC    AL2(360),AL2(0030)  TWIN FALLS                                   
         DC    AL2(196),AL2(0036)  ZANESVILLE                                   
         DC    AL2(152),AL2(0034)  PRESQUE ISLE                                 
         DC    AL2(349),AL2(0038)  LAREDO                                       
         DC    AL2(421),AL2(0030)  BEND OR                                      
         DC    AL2(345),AL2(0028)  FAIRBANKS                                    
         DC    AL2(340),AL2(0019)  N.PLATTE-HAYES-MC COOK                       
         DC    AL2(183),AL2(0018)  ALPENA                                       
         DC    AL2(398),AL2(0006)  GLENDIVE                                     
         DC    AL2(165),AL2(0000)  ELMIRA                                       
         DC    AL2(342),AL2(0000)  ENSIGN-GARDEN CITY                           
         DC    AL2(333),AL2(0000)  FLORENCE,AL                                  
         DC    AL2(290),AL2(0000)  GREAT BEND                                   
         DC    AL2(341),AL2(0000)  HAYS-GOODLAND                                
         DC    AL2(172),AL2(0000)  MANCHESTER                                   
         DC    AL2(226),AL2(0000)  VICTORIA                                     
         DC    AL2(193),AL2(0000)  WORCESTER                                    
         DC    AL2(406),AL2(0154)  UNKNOWN                                      
         DC    AL2(215),AL2(0048)  UNKNOWN                                      
         DC    AL2(368),AL2(0021)  UNKNOWN                                      
         DC    X'FFFF'                                                          
CPMWGHTC DS    0D                  CANADIAN MARKETS                             
         DC    AL2(0009),AL2(1)       ST JOHNS CMA                              
         DC    AL2(0041),AL2(1)       CORNRBR C.AREA                            
         DC    AL2(0060),AL2(1)       CD6                                       
         DC    AL2(1021),AL2(1)       CHARTWN C.AREA                            
         DC    AL2(1031),AL2(1)       ATLANTIC                                  
         DC    AL2(2009),AL2(1)       SYD-GLAC,SYD-M **PR NOV/84**              
         DC    AL2(2010),AL2(1)       CAPE BRETON **EFF NOV/84**                
         DC    AL2(2079),AL2(1)       HALIFAX,CMA ** PR NOV/84**                
         DC    AL2(2080),AL2(1)       HALIFAX CO **EFF NOV/84**                 
         DC    AL2(3011),AL2(1)       STJN-MON C.AREA                           
         DC    AL2(3111),AL2(1)       CARLTON C.AREA                            
         DC    AL2(4041),AL2(1)       MATANE C.AREA                             
         DC    AL2(4061),AL2(1)       RIM+MAT C.AREA                            
         DC    AL2(4071),AL2(1)       RIMOUSK C.AREA                            
         DC    AL2(4091),AL2(1)       SEPT ILES EM                              
         DC    AL2(4101),AL2(1)       RIVLOUP C.AREA                            
         DC    AL2(4120),AL2(1)       CHICOUTI-JONQU                            
         DC    AL2(4119),AL2(1)                                                 
         DC    AL2(4199),AL2(1)       QUEBEC CMA                                
         DC    AL2(4351),AL2(1)       SHERBRK C.AREA                            
         DC    AL2(4479),AL2(1)       MONTREAL CMA                              
         DC    AL2(4661),AL2(1)       TR RIV. C.AREA                            
         DC    AL2(4667),AL2(1)       SHER-TR RIV EM                            
         DC    AL2(4723),AL2(1)       ROUYN EM                                  
         DC    AL2(5069),AL2(1)       OTTAWA CMA                                
         DC    AL2(5109),AL2(1)       KINGSTON CA                               
         DC    AL2(5159),AL2(1)       PETERBOROUGH CA                           
         DC    AL2(5199),AL2(1)       TORONTO CMA                               
         DC    AL2(5243),AL2(1)       BARRIE C.AREA                             
         DC    AL2(5269),AL2(1)       HAMILTON CMA                              
         DC    AL2(5339),AL2(1)       KITCHENER CMA                             
         DC    AL2(5369),AL2(1)       LONDON CMA                                
         DC    AL2(5409),AL2(1)       WINDSOR CMA                               
         DC    AL2(5441),AL2(1)       WINGHAM C.AREA                            
         DC    AL2(5469),AL2(1)       NORTH BAY CA                              
         DC    AL2(5479),AL2(1)       SUDBURY 8011                              
         DC    AL2(5499),AL2(1)       TIMMINS C                                 
         DC    AL2(5531),AL2(1)       ALGOMA WEST                               
         DC    AL2(5539),AL2(1)       THUNDER BAY                               
         DC    AL2(6061),AL2(1)       BRANDON C AREA                            
         DC    AL2(6119),AL2(1)       WINNIPEG CMA                              
         DC    AL2(7011),AL2(1)       YORKTON C.AREA                            
         DC    AL2(7045),AL2(1)       CD4,CD8 (SWIFT CURRENT)                   
         DC    AL2(7071),AL2(1)       REG-MOOS CAREA                            
         DC    AL2(7109),AL2(1)       SASKATOON CMA                             
         DC    AL2(7153),AL2(1)       PR ALBER C.AREA                           
         DC    AL2(8010),AL2(1)       MEDICINE HAT                              
         DC    AL2(8020),AL2(1)       CD2                                       
         DC    AL2(8069),AL2(1)       CALGARY CMA                               
         DC    AL2(8080),AL2(1)       RED DEER                                  
         DC    AL2(8078),AL2(1)       RED DEER 8011                             
         DC    AL2(8091),AL2(1)       LLOYDMIN C AREA                           
         DC    AL2(8119),AL2(1)       EDMONTON CMA                              
         DC    AL2(9071),AL2(1)       OKAN-KAM CAREA                            
         DC    AL2(9109),AL2(1)       VANCOUVER CMA                             
         DC    AL2(9119),AL2(1)       VICTORIA CMA                              
         DC    AL2(9301),AL2(1)       TER-KIT C.AREA                            
         DC    AL2(9341),AL2(1)       PR G/TK C.AREA                            
         DC    AL2(9350),AL2(1)       FRASER-FT GEOR                            
         DC    AL2(9363),AL2(1)       DAWSON CREEK                              
         DC    X'FFFF'                                                          
         EJECT                                                                  
OUT      DCB   DDNAME=OUT,             DOS SYS005                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00040,                                            X        
               BLKSIZE=04000,          DOS BLKSIZE=04000               X        
               MACRF=PM                                                         
         EJECT                                                                  
GRYDPT   DS    0D                                                               
         DC    C'EAMA' DAYPART CODE M                                           
         DC    C'ERMA' DAYPART CODE M                                           
         DC    C'DAYC' DAYPART CODE D                                           
         DC    C'WKKE' DAYPART CODE W                                           
         DC    C'AFTJ' DAYPART CODE B                                           
         DC    C'WAFJ' DAYPART CODE C                                           
         DC    C'ELYJ' DAYPART CODE E                                           
         DC    C'EFRJ' DAYPART CODE E                                           
         DC    C'ACCL' DAYPART CODE X                                           
         DC    C'PRIN' DAYPART CODE P                                           
         DC    C'FRGP' DAYPART CODE F                                           
         DC    C'LTEP' DAYPART CODE L                                           
         DC    C'LFRP' DAYAPRT CODE L                                           
         DC    4X'00'                                                           
         EJECT                                                                  
SPM1WK   DS    0D                                                               
BANKDEMS DC    X'00',C'R',AL1(1)   HOMES                                        
         DC    X'00',C'R',AL1(42)  W1849                                        
         DC    X'00',C'R',AL1(92)  M1849                                        
         DC    X'00',C'R',AL1(142) V1849                                        
         DC    X'00',C'R',AL1(48)  W2554                                        
         DC    X'00',C'R',AL1(098) M2554                                        
         DC    X'00',C'R',AL1(148) V2554                                        
         DC    X'00',C'R',AL1(053) W3564                                        
         DC    X'00',C'R',AL1(103) M3564                                        
*        DC    X'00',C'R',AL1(153) V3564                                        
*        DC    X'00',C'R',AL1(025) W1217                                        
*        DC    X'00',C'R',AL1(075) M1217                                        
*        DC    X'00',C'R',AL1(125) TN1217                                       
BNKDEMEN DS    0C                                                               
LNBNKDEM EQU   BNKDEMEN-BANKDEMS                                                
         SPACE 2                                                                
BANKD95S DC    X'00',C'R',AL1(1)   HOMES                                        
         DC    X'00',C'R',AL1(42)  W1849                                        
         DC    X'00',C'R',AL1(48)  W2554                                        
         DC    X'00',C'R',AL1(92)  M1849                                        
         DC    X'00',C'R',AL1(142) V1849                                        
         DC    X'00',C'R',AL1(148) V2554                                        
BNKD95EN DS    0C                                                               
LNBNKD95 EQU   BNKD95EN-BANKD95S                                                
         SPACE 2                                                                
BAN2D95S DC    X'00',C'R',AL1(098) M2554                                        
         DC    X'00',C'R',AL1(053) W3564                                        
         DC    X'00',C'R',AL1(103) M3564                                        
         DC    X'00',C'R',AL1(153) V3564                                        
         DC    X'00',C'R',AL1(125) TN1217                                       
         DC    X'00',C'R',AL1(025) WM1217                                       
BN2D95EN DS    0C                                                               
LNBN2D95 EQU   BN2D95EN-BAN2D95S                                                
         SPACE 2                                                                
BANKD88S DC    X'00',C'R',AL1(1)   HOMES                                        
         DC    X'00',C'R',AL1(42)  W1849                                        
         DC    X'00',C'R',AL1(92)  M1849                                        
         DC    X'00',C'R',AL1(142) V1849                                        
         DC    X'00',C'R',AL1(48)  W2554                                        
         DC    X'00',C'R',AL1(098) M2554                                        
         DC    X'00',C'R',AL1(148) V2554                                        
         DC    X'00',C'R',AL1(053) W3564                                        
         DC    X'00',C'R',AL1(103) M3564                                        
         DC    X'00',C'R',AL1(153) V3564                                        
BNKD88EN DS    0C                                                               
LNBNKD88 EQU   BNKD88EN-BANKD88S                                                
         SPACE 2                                                                
TBCDEMS  DC    X'00',C'R',AL1(1)   HOMES                                        
TBCDEMEN DS    0C                                                               
LNTBCDEM EQU   TBCDEMEN-TBCDEMS                                                 
         SPACE 2                                                                
* CANADIAN RECLASSIFIED                                                         
CNBNK    DC    X'00',C'R',AL1(142) V1849                                        
         DC    X'00',C'R',AL1(145) V18+                                         
CNBNKEN  DS    0C                                                               
LNCNBNK  EQU   CNBNKEN-CNBNK                                                    
*                                                                               
BANKPROF DC    C'R'                SPECIAL CONTROL                              
         DC    AL1(3)              EQUIVALENCE CODE                             
         DC    X'00'               ADVERTISER CODE 1                            
         DC    X'00'               ADVERTISER CODE 2                            
         DC    AL1(38)             ACTUAL BOOK CONTROL                          
         DC    X'00'               RATING SERVICE FORCE                         
         DC    C'N'                EXCLUDE BONUS SPOTS                          
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    C'Y'                BANK AGENCY                                  
*                                                                               
* PROFILE FOR CANADIAN BANK AGENCIES                                            
CANBPROF DC    C'N'                SPECIAL CONTROL                              
         DC    AL1(3)              EQUIVALENCE CODE                             
         DC    X'00'               ADVERTISER CODE 1                            
         DC    X'00'               ADVERTISER CODE 2                            
         DC    AL1(28)             ACTUAL BOOK CONTROL                          
         DC    X'00'               RATING SERVICE FORCE                         
         DC    C'N'                EXCLUDE BONUS SPOTS                          
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    X'00'               SPARE                                        
         DC    C'Y'                BANK AGENCY                                  
         EJECT                                                                  
NOTFOUND EQU   X'10'               DEMAND - RECORD NOT FOUND                    
DEMPASS  DC    X'00'                                                            
LCLPROG  DC    X'00'                                                            
NWSFLAG  DC    X'00'                                                            
MDMCB    DS    6F                                                               
TOTQH    DC    H'0'                TOTAL PROGRAM QH COUNT                       
NWSQH    DS    H'0'                NWS PORTION QH COUNT                         
SPTQH    DS    H'0'                SPORTS PORTION COUNT                         
ATIMTAB  DC    F'0'                EASTERN AND PACIFIC DAYPARTS                 
ATIMTABC DC    F'0'                CENTRAL AND MOUNTIAN DAYPARTS                
ATIMTABS DC    F'0'                SACREMENTO SAN/FRAN DAYPARTS                 
ATIMTBF  DC    F'0'           FOX  EASTERN AND PACIFIC DAYPARTS                 
ATIMTBCF DC    F'0'           FOX  CENTRAL AND MOUNTIAN DAYPARTS                
ATIMTBSF DC    F'0'           FOX  SACREMENTO SAN/FRAN DAYPARTS                 
AGETSUPR DC    A(0)                GETSUPER ROUTINE (CAN. OR US)                
VWRREC   DC    F'0'                                                             
VGETER   DC    F'0'                                                             
VBLDEST  DC    F'0'                                                             
VERKC    DC    F'0'                                                             
VERLC    DC    F'0'                                                             
VERC     DC    F'0'                                                             
VESAVEC  DC    F'0'                                                             
VSTATABC DC    F'0'                                                             
VGETDEM  DC    F'0'                                                             
RULESKCT DC    F'0'                                                             
LASTLINK DC    F'0'                                                             
LASTE    DC    F'0'                                                             
LASTEST  DC    F'0'                                                             
CURRBRND DC    F'0'                A(CURRENT PRDBUF)                            
PREVETAB DC    F'0'                A(PREV ETAB)                                 
ESCNTR   DC    F'0'                ESTIMATE COUNTER                             
STACNTR  DC    F'0'                STATION COUNTER                              
BEFORE   DC    F'0'                                                             
AFTER    DC    F'0'                                                             
CLTBYPS  DC    PL8'0'                                                           
CLTBYOS  DC    PL8'0'                                                           
CLTINS   DC    PL8'0'                                                           
CLTBYPD  DC    PL8'0'                                                           
CLTBYOD  DC    PL8'0'                                                           
CLTIND   DC    PL8'0'                                                           
AGYBYPS  DC    PL8'0'                                                           
AGYBYOS  DC    PL8'0'                                                           
AGYINS   DC    PL8'0'                                                           
AGYBYPD  DC    PL8'0'                                                           
AGYBYOD  DC    PL8'0'                                                           
AGYIND   DC    PL8'0'                                                           
AGYARB   DC    PL8'0'                                                           
AGYNSI   DC    PL8'0'                                                           
EXCLSUM  DC    PL8'0'              9 SETS OF EXCLUDE TOTALS                     
         DS    CL8                                                              
         DS    CL128                                                            
TDTAB    DC    A(TDTABC)                                                        
TDCNT    DC    A(0)                                                             
TDMAX    DC    A(TDCLEN/TDLN)                                                   
SPOTTYPE DC    C'X'                                                             
REQSW    DC    X'00'                                                            
TAPEOPEN DC    X'00'                                                            
TOTPASS  DC    X'01'                                                            
BYPSW    DC    X'00'                                                            
SUPRDEM  DC    X'01'                                                            
OVRAGY   DS    CL2                                                              
M1QCLT   DS    CL3                                                              
SVRSMKT  DS    CL2                                                              
SVRSAFF  DS    C                                                                
SVRSAFF2 DS    C                                                                
SVSPTLN  DS    C                                                                
SVPRI    DS    CL3                                                              
SPDEMLST DS    CL63                DEMO OVERRIDES FOR SPECIALS                  
PSLPTR   DS    F                   PSLIST CURRENT POINTER                       
PSLPRD   DS    C                   PSL CURRENT PRODUCT                          
PSLSLN   DS    C                   PSL CURRENT SPOT LENGTH                      
PSLIST   DS    CL100                                                            
RECORD   DS    CL40                                                             
         DS    0F                                                               
BFREC    DS    0CL40                                                            
BFRTGSRV DS    C                   RATING SERVICE                               
BFTARGET DS    CL3                 TARGET DEMO                                  
BFDEMO   DS    CL3                 DEMO CODE (IMPRESSIONS)                      
BFDP     DS    CL1                 DAYPART (CPP SYSTEM)                         
BFSL     DS    CL1                 SPOT LENGTH                                  
BFPROG   DS    CL1                 PROGRAM TYPE (CPP SYSTEM)                    
BFAFFL   DS    CL1                 AFFILIATE                                    
BFEQIV   DS    CL2                 EQUIVALENCE                                  
BFYEAR   DS    CL1                 YEAR                                         
BFMON    DS    CL1                 MONTH                                        
BFSTYP   DS    CL1                 MEDIA FOR CANADIAN                           
BFSPOT   DS    CL4                 SPOTS                                        
BFCASH   DS    CL4                 DOLLARS                                      
BFPNTS   DS    CL4                 POINTS                                       
BFIMPS   DS    CL4                 IMPRESSIONS                                  
BFSPEC   DS    CL4                 SPECIAL TOTAL DOLLARS                        
BFBONUS  DS    CL4                 BONUS TOTAL SPOTS                            
DLUPCOD  DS    F                   DEMO LOOKUP CODE                             
RECCNT   DS    F                                                                
CLTRCNT  DS    F                                                                
SVRE     DS    F                                                                
SVRF     DS    F                                                                
DPT1     DS    H                                                                
DPT2     DS    H                                                                
DPST     DS    H                                                                
DPET     DS    H                                                                
ESTFT    DS    C                                                                
CURREST  DS    C                                                                
RTGSRV   DS    C                                                                
BEFAFTSW DS    C                                                                
CPPPROG  DS    CL1                 CPP PROGRAM CODE                             
CPPDP    DS    CL2                 CPP DAYPART CODE                             
HLDAGY   DS    CL2                                                              
HLDSPC   DS    C                                                                
PWOSSTR  DS    CL3                                                              
PWOSEND  DS    CL3                                                              
CURRTGT  DS    CL3                                                              
CURRDEM  DS    C                                                                
DEMAREA  DS    0CL180                                                           
DM1AREA  DS    CL60                                                             
DM2AREA  DS    CL60                                                             
DM3AREA  DS    CL60                                                             
         DC    X'00'                                                            
DMXAREA  DS    CL63                                                             
TIMZON   DS    CL1                 TIME ZONE 1=A/P 2=M/C                        
SVEDATE  DS    CL12                EXPANDED REQUEST DATES                       
SVQDATE  DS    CL12                ACTUAL REQUEST DATES                         
SVBSTART DS    CL2                 ACTUAL REQUEST START (COMPRESSED)            
SVBEND   DS    CL2                 ACTUAL REQUEST END (COMPRESSED)              
SVDMAREA DS    CL180                                                            
SVDEMS   DS    CL60                                                             
DPTHOLD  DS    CL30                                                             
SVPROF   DS    CL16                                                             
SVSTIM   DS    H                                                                
SVETIM   DS    H                                                                
VGRYDPT  DS    F                                                                
VDPTSVC  DS    F                                                                
VPTYPSVC DS    F                                                                
MTLENGTH DS    F                   LENGTH OF MARKET NAME ENTRY                  
MTADDR   DS    F                   A(NSI MARKET NAMES)                          
GERKSAVE DS    CL20                                                             
GERRSAVE DS    F                                                                
SVCTELM  DS    100C                                                             
REC      DS    CL500                                                            
*                                                                               
EXCLCAPS DC    C'       SPECIAL REP='                                           
         DC    C'           SPECIAL='                                           
         DC    C'             BONUS='                                           
         DC    C' ESTIMATE EXCLUDED='                                           
         DC    C'UNUSED SPOT LENGTH='                                           
         DC    C'    UNKNOWN MARKET='                                           
         DC    C'       UNALLOCATED='                                           
         DC    C'   CLIENT EXCLUDED='                                           
         DC    C'  PRODUCT EXCLUDED='                                           
         DC    X'00'                                                            
*                                                                               
PRIBUF   DS    512D                                                             
SUPRTAB  DS    256H                                                             
POLEXCL  DS    CL255                                                            
TDTABC   DS    0C                                                               
TDTABCST DS    0C                                                               
         DS    100000C                                                          
TDTABCEN DS    0C                                                               
TDCLEN   EQU   TDTABCEN-TDTABCST                                                
         LTORG                                                                  
         SPACE 3                                                                
         BUFF  LINES=2000,ROWS=1,COLUMNS=6,KEYLIST=(16,A)                       
         EJECT                                                                  
SPM102   CSECT                                                                  
ERULESKC DS    0D                                                               
         DS    5000C                                                            
ERULESLC DS    0D                                                               
         DS    5000C                                                            
ERULESC  DS    0D                                                               
         DS    20000C                                                           
ESAVEC   DS    0D                                                               
         DS    441000C                                                          
DPTSVC   DS    0D                                                               
         DS    400C                                                             
PTYPSVC  DS    0D                                                               
         DS    400C                                                             
STATABC  DS    0D                                                               
         DS    (2000*STTABLN)C                                                  
         EJECT                                                                  
PTABD    DSECT                                                                  
PTPRDN   DS    CL1                 NUMERIC PRODUCT CODE                         
PTPRDA   DS    CL3                 ALPHA PRODUCT CODE                           
PTNAME   DS    CL20                PRODUCT NAME                                 
         DS    CL4                                                              
PTDEMO   DS    CL14                DEMO NUMBER                                  
PTWGHT   DS    CL14                DEMO WIEGHT                                  
         SPACE 2                                                                
STATABD  DSECT                                                                  
STTABBEG DS    0C                                                               
STTAFFR  DS    CL5                 RATING SERVICE AFFILIATE                     
STTCALL  DS    CL5                 CALL LETTERS                                 
STTAFFC  DS    CL1                 CPPRS AFFILIATE                              
STTAFFD  DS    CL1                 DAYPART AFFILIATE                            
STTABEND DS    0C                                                               
STTABLN  EQU   STTABEND-STTABBEG                                                
         SPACE 2                                                                
PRDRDWD  DSECT                                                                  
EIOAREA  DS    1500C                                                            
RECFLAG  DS    C                   END OF RECORDS                               
EKEYSAVE DS    CL13                                                             
PRDRDWDQ EQU   *-PRDRDWD                                                        
         SPACE 2                                                                
*                                                                               
*                                                                               
* ESTIMATE TABLE DSECT                                                          
*                                                                               
ETABD    DSECT                                                                  
ETPRD    DS    CL3                                                              
ETEST    DS    CL1                                                              
ETDEMO   DS    CL60                                                             
ETWGHT   DS    CL20                                                             
ETFLAG   DS    CL1                                                              
ETABEND  DS    0C                                                               
ETABLN   EQU   ETABEND-ETPRD                                                    
         SPACE 2                                                                
* EXTRACT RULES KEY TABLE                                                       
*                                                                               
ERULESK  DSECT                                                                  
ERKCLT   DS    CL3                 CLIENT CODE                                  
ERKPRD   DS    CL1                 PRODUCT CODE                                 
ERKEST   DS    CL1                 ESTIMATE NUMBER                              
         DS    CL3                 SPARE                                        
ERKDLNK  DS    CL4                 LINK TO DATE TABLES                          
         SPACE 2                                                                
ERULESL  DSECT                                                                  
ERLDATE  DS    0CL4                DATES                                        
ERLSDTE  DS    CL2                 START DATE                                   
ERLEDTE  DS    CL2                 END DATE                                     
ERLNXT   DS    CL4                 NEXT DATES (0 IF END)                        
ERLDEM   DS    CL4                 DEMO ELEMENT LINK (0 IF NONE)                
ERLPTYP  DS    CL4                 PROG TYPE ELEMENT LINK                       
         SPACE 2                                                                
TDD      DSECT AUDIT TRAIL REPORT                                               
TDTABST  DS    0C                                                               
TDRTGSRV DS    C                   RATING SERVICE                               
TDTRGT   DS    CL3                 TARGET DEMO                                  
TDDEM    DS    CL3                 REPORTING DEMO                               
TDDPT    DS    C                   DAY/TIME DAYPART                             
TDPROG   DS    C                   PROGRAM TYPE                                 
TDNORI   DS    C                   NETWORK OR INDEPENDENT                       
TDSPOTS  DS    CL4                 SPOTS                                        
TDDOLS   DS    CL4                 DOLLARS                                      
TDRTG    DS    CL4                 RATING POINTS (REPORTING DEMO)               
TDIMPS   DS    CL4                 IMPRESSIONS   (REPORTING DEMO)               
TDSPEC   DS    CL4                 TOTAL SPECIAL PROGRAMMING DOLLARS            
TDBONUS  DS    CL4                 TOTAL BONUS SPOTS                            
TDTABEN  DS    0C                                                               
TDKLN    EQU   TDSPOTS-TDTABST                                                  
TDLN     EQU   TDTABEN-TDTABST                                                  
         SPACE 2                                                                
ZRPL     DSECT                                                                  
ZRCLT    DS    CL3                 CLIENT                                       
         DS    CL2                                                              
ZRPRD    DS    CL3                 PRODUCT                                      
         DS    CL2                                                              
ZREST    DS    CL3                 ESTIMATE                                     
         DS    CL2                                                              
ZRSDTE   DS    CL8                 START DATE                                   
         DS    CL2                                                              
ZREDTE   DS    CL8                 END DATE                                     
         DS    CL2                                                              
ZRDEM    DS    CL54                DEMOS                                        
         DS    CL2                                                              
ZRPTYP   DS    CL40                PROGRAM TYPES                                
         SPACE 2                                                                
ARPRINTD DSECT                     AUDIT RPORT DSCT                             
ARRTGSRV DS    CL3                                                              
         DS    C                                                                
ARTRGT   DS    CL7                 TARGET DEMO                                  
         DS    CL1                                                              
ARDEM    DS    CL7                 REPORT DEMO                                  
         DS    CL1                                                              
ARDPT    DS    CL3                 DAYPART                                      
         DS    CL1                                                              
ARPROG   DS    CL3                 PROGRAM TYPE                                 
         DS    C                                                                
ARNORI   DS    CL3                 NETWORK OR IND.                              
         DS    C                                                                
ARSPOTS  DS    CL6                 SPOTS                                        
         DS    C                                                                
ARDOLS   DS    CL11                DOLLARS                                      
         DS    C                                                                
ARRTG    DS    CL8                 RATINGS                                      
         DS    C                                                                
ARIMPS   DS    CL11                IMPRESSIONS                                  
         DS    C                                                                
ARSPEC   DS    CL11                SPECIAL                                      
         DS    C                                                                
ARBONUS  DS    CL6                                                              
         EJECT                                                                  
SPM102   CSECT                                                                  
       ++INCLUDE DEEQUIVOLD                                                     
       ++INCLUDE DEDBLOCK                                                       
         DS    CL80                                                             
       ++INCLUDE DDCOMFACS                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
       ++INCLUDE CPGENINTER                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069SPREPM102 09/15/06'                                      
         END                                                                    
