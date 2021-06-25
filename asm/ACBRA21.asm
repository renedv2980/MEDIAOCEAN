*          DATA SET ACBRA21    AT LEVEL 040 AS OF 05/15/20                      
*PHASE T62421A                                                                  
                                                                                
ACBRA21  TITLE '- BrandOcean General + Jobs Upload Server'                      
                                                                                
* Level change comments                                                         
* ---------------------                                                         
* UK Levels                                                                     
* ---------                                                                     
* TKLU 001 28MAY06 First version for 'General approvals overview'               
*          23OCT06 FINAL RENAME FROM MCS TO BRA                                 
* TKLU 002 24OCT06 GAOV call fix for orders and call for InvAppr                
* TKLU 003 30OCT06 GAOV call fix for finance versus normal approver             
* TKLU 004 22NOV06 <UKCR00009903> expense GAOV call bug fix                     
* TKLU 005 07DEC06 GAOV call to TimeSheets - parameter fix                      
* NSHE 006 07NOV07 <LO01-6995> Set FACPAK code for analysis                     
* TKLU 007 21NOV07 <UKCR00010576> expense GAOV call parameter bug fix           
* TKLU 008 11DEC07 US merger (JSHA, comments only)                              
* NSHE 009 30APR08 Add invoices                                                 
* TKLU 011 06AUG08 <DU01-7687> New Job/Status upload request                    
* TKLU 014 16OCT08 <LO01-8207> 'Internal Approvals' implementation              
* MPEN 015 05NOV08 <DU01-7334> AVOID POSS TO CLOSE JOBS IT TIME EXISTS          
* TKLU 016 08JAN09 Job Status Upload bug fix (no ASTELD)                        
* NSHE 017 20JAN09 Merge US and UK code                                         
* NSHE 018 03AUG09 Change finance approver call for expenses                    
* SMAN 019 09OCT09 <UKCR00024866> Fix to level 014                              
* NSHE 021 05AUG10 Change CONPIDC to CCTPID                                     
* NSHE 022 26APR11 Remove TACPASD and change to using TSJPASD                   
* YNGX 023 23NOV11 <PR002442> UPDATE ESTSJBCL ON ESTIMATE RECORDS               
* NSHE 024 03SEP12 Change IO routine for auto switching system                  
* MPEN     25FEB13 <BR54230L> Check order fully billed first!                   
*NRAK 025 15JAN14 <DSBO-618> Check for outstanding invoices on close            
* TKLU 026 27JUN14 <RD003081> Job added date (PIDKADAT) for PIDKJSA/SQ          
* JSHA 027 22SEP14 Deal with passive already existing                           
* JFOS 028 28JAN15 <PCA-1234> New BACS Email status upload call                 
* NSHE 029 11May15 US fixes from Jim Shea                                       
* JFOS 030 07Sep15 <PCA01983> Errmsg if short TRSEL on email sts update         
* TKLU 031 27Jan15 <RD010160> Use AGOBLOCB not AGOBLOCK                         
* TKLU 032 16Jan17 DSPCA-2818 Add GM Accounting Person Upload                   
*                  DSPCA-2819 and support name change action                    
* TKLU 032 27Apr17 DSPCA-2818 Person upload fixes                               
* TKLU 033 08May18 DSPCA-2844 RNSPASD Adjustments                               
* NSHE 034 08May18 DSMU-120 Add account lock/unlock to upload tool              
* MPEN 035 23Oct18 DSRD-20447 Relink for DPAPASD changes                        
* NSHE 036 08Feb19 DSRD-20646 Security API bug for US                           
* SGAV 037 14FEB20 DSPCA-3080 Update RSTLN from RSTLN3Q To RSTLN4Q              
* MPEN 038 06Mar20 DSRD-25763 Fix for MIM cost person upload                    
* NSHE 039 26Mar20 DSRD-25885 Another fix for MIM person upload 2P              
* DPUR             DSPCA-3122 TO ADD 2 NEW FIELD LIMIT LIST PID                 
*                                                                               
                                                                                
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=2000,REQUEST=*,WORKERKEY=ACBO,   *        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=ACCSYSQ                   
                                                                                
ENTRY    NMOD1 0,**BO21**,RR=RE                                                 
         LR    RC,R1                                                            
         USING LP_D,RC                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         ICM   R8,15,RSVRSAVE      R8=A(4K SAVE AREA)                           
         ST    R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
         L     R5,ATWA                                                          
         USING TWAD,R5             RA=A(TWA)                                    
                                                                                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
***********************************************************************         
* Initialise Upload                                                   *         
***********************************************************************         
                                                                                
INITUL   CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         JE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JE    INPUT                                                            
         J     EXITY                                                            
                                                                                
INIT     LA    R0,SAVED            CLEAR SAVE STORAGE                           
         LHI   R1,SAVEL                                                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   GIND2,GI2EBUY       (default to ebuyer)                          
         GOTOR (#CPYINI,ACPYINI)   INITIALISE COMPANY VALUES                    
         USING CPXELD,SCPXEL                                                    
         USING CPYELD,SCPYEL                                                    
                                                                                
         L     R1,ALP              RESTORE A(LP_D)                              
         MVC   AALIOB,LP_ALIOB     EXTRACT A(LIOB) FROM LP_D                    
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   AALINKIO,CLINKIO-COMFACSD(RF)                                    
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Process and Upload Record                                           *         
***********************************************************************         
                                                                                
INPUT    BASR  RF,0                                                             
         AHI   RF,RECTAB-*                                                      
         USING RECTABD,RF                                                       
         LHI   R0,RECTABN                                                       
         BASR  RE,0                                                             
         CLC   RECTMAP#,LP_QMAPN   Look up record map code                      
         JE    INPUT02                                                          
         AHI   RF,RECTABL                                                       
         BCTR  R0,RE                                                            
         DC    H'0'                -> unknown record type                       
                                                                                
INPUT02  MVC   RECTYPE,RECTTYPE    -> set known record type                     
                                                                                
                                                                                
         GOTOR UPDREC              PROCESS THE INPUT RECORD                     
                                                                                
         J     EXITY               EXIT BACK TO DDLINK                          
         DROP  RB                                                               
                                                                                
***********************************************************************         
* Go to Upload Record handling routine                                *         
***********************************************************************         
                                                                                
UPDREC   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR (#SYSCHK,ASYSCHK)                                                
         JE    UPDREC2                                                          
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$FLRD                                                   
         J     EXITN                                                            
                                                                                
UPDREC2  LLC   RF,RECTYPE                                                       
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         CHI   RF,UPDTABL                                                       
         JNL   *+2                                                              
         B     UPDTAB(RF)                                                       
                                                                                
UPDTAB   DS    0XL4                                                             
         J     UPDPER              Accounting Person                            
*&&UK*&& J     UPDALK              Account lock/unlock                          
*&&UK*&& J     UPD5PE              BACS email status                            
UPDTABL  EQU   *-UPDTAB                                                         
                                                                                
UPDRECY  J     EXITY                                                            
                                                                                
UPDRECN  J     EXITN                                                            
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Account lock/unlock upload                                          *         
***********************************************************************         
                                                                                
*&&UK                                                                           
UPDALK   BASE  ,                                                                
                                                                                
         GOTOR VALULA              validate and update account                  
         BNE   UPDALKER                                                         
                                                                                
         GOTOR UPDACA              additional updates and write                 
         BNE   UPDALKER                                                         
                                                                                
         GOTOR VDATAMGR,DMCB,DMCOMMIT,0  save each change                       
                                                                                
         B     UPDALKY                                                          
                                                                                
UPDALKER OI    TWAMODE,TWAMEDP                                                  
         MVC   XERRTXT(L'RQ_ULA),RQ_ULA                                         
         ICM   R1,3,ROUERRV                                                     
         GOTOR PUTERR                                                           
*        B     UPDALKN                                                          
                                                                                
         B     UPDALKY                                                          
                                                                                
*** Global exists for account lock/unlock upload **********************         
                                                                                
UPDALKY  LA    R0,RQUPVAL                                                       
         LA    R1,RQUPLNQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     UPDRECY                                                          
                                                                                
UPDALKN  LA    R0,RQUPVAL                                                       
         LA    R1,RQUPLNQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         TM    TWAMODE,TWAMUWD     unwind required?                             
         JZ    EXITN                                                            
         NI    TWAMODE,FF-TWAMUWD                                               
         OI    GIND1,GIUNWIND      DDLINK will unwind/abend                     
         J     EXITN                                                            
                                                                                
         DROP  RB                                                               
                                                                                
         EJECT                                                                  
                                                                                
*&&                                                                             
***********************************************************************         
* Accounting Person Upload                                            *         
* ------------------------                                            *         
* => to add person, payroll history, 1R, 2P and SX accounts as well   *         
*    adding to approvers for time and expenses                        *         
* => to change above records (if applicable) for a name change        *         
* => to terminate cost person and 1R records                          *         
***********************************************************************         
UPDPER   BASE  ,                                                                
                                                                                
* 1. Initialisation                                                             
                                                                                
         LAY   RE,ERRTAB                                                        
         MVI   0(RE),ET_EOTQ       Initialise error table                       
                                                                                
         LAY   RE,RETTAB                                                        
         MVI   0(RE),RT_EOTQ       Initialise return table                      
                                                                                
         TM    TWAMODE,TWAMEDP     exit on all critical errors                  
         JNZ   UPDPERN             (will not apply here)                        
                                                                                
         LA    RE,RV_BLOCK         Clear retrieved values                       
         LHI   RF,RV_BLENQ                                                      
         XCEFL                                                                  
                                                                                
* 2. Global validation                                                          
                                                                                
         GOTOR VAL_ACT             validate action                              
         JNE   UPDPER80                                                         
                                                                                
         GOTOR VAL_PID             validate PID                                 
         JNE   UPDPER80                                                         
                                                                                
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,IS_1R        set ledger 1R, 2P and SX/Y details           
         GOTOR (#SETLDG,ASETLDG)                                                
         CLI   LDGAOP,LDGONKHI                                                  
         BNH   *+8                                                              
         MVI   LDGAOP,0                                                         
         MVC   RV_1ROP,LDGAOP                                                   
         NI    RV_1ROP,X'FF'-LDGOKEY2                                           
         MVC   RV_1RL1,LDGAL1                                                   
         MVC   RV_1RL2,LDGAL2                                                   
         MVC   RV_1RL3,LDGAL3                                                   
         MVC   RV_1RL4,LDGAL4                                                   
         GOTOR GET_AAD,RV_1RAD                                                  
         GOTOR GET_DPC                                                          
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,IS_2P                                                     
         GOTOR (#SETLDG,ASETLDG)                                                
         CLI   LDGAOP,LDGONKHI                                                  
         BNH   *+8                                                              
         MVI   LDGAOP,0                                                         
         MVC   RV_2POP,LDGAOP                                                   
         NI    RV_2POP,X'FF'-LDGOKEY2                                           
         MVC   RV_2PL1,LDGAL1                                                   
         MVC   RV_2PL2,LDGAL2                                                   
         MVC   RV_2PL3,LDGAL3                                                   
         MVC   RV_2PL4,LDGAL4                                                   
         GOTOR GET_AAD,RV_2PAD                                                  
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,IS_SX        In US this is SY                             
         GOTOR (#SETLDG,ASETLDG)                                                
         CLI   LDGAOP,LDGONKHI                                                  
         BNH   *+8                                                              
         MVI   LDGAOP,0                                                         
         MVC   RV_SXOP,LDGAOP                                                   
         NI    RV_SXOP,X'FF'-LDGOKEY2                                           
         MVC   RV_SXL1,LDGAL1                                                   
         MVC   RV_SXL2,LDGAL2                                                   
         MVC   RV_SXL3,LDGAL3                                                   
         MVC   RV_SXL4,LDGAL4                                                   
         GOTOR GET_AAD,RV_SXAD                                                  
                                                                                
         GOTOR VAL_PCL             validate PID code length                     
         JNE   UPDPER80                                                         
                                                                                
* 3. Data validation first                                                      
                                                                                
         CLI   RQP_ACT,RQP_ADQ     Action: add a new person                     
         JE    UPDPER10                                                         
         CLI   RQP_ACT,RQP_CHQ     Action: change a person record               
         JE    UPDPER28                                                         
         J     *+2                 (unknown - see VAL_ACT)                      
                                                                                
* a) Cost person record                                                         
                                                                                
UPDPER10 GOTOR VAL_CPR             validate cost person record                  
         JNE   UPDPER80            (some acceptable errors)                     
                                                                                
         GOTOR VAL_NAM             validate names                               
         JNE   UPDPER80                                                         
                                                                                
         GOTOR VAL_HIR             validate hire date                           
         JNE   UPDPER80            (some acceptable errors)                     
                                                                                
* b) Cost history for person                                                    
                                                                                
         CLI   RV_DPCD,0           auto history set?                            
         JE    UPDPER11                                                         
                                                                                
         GOTOR VAL_HIS             validate for history data                    
         JNE   UPDPER80                                                         
                                                                                
         GOTOR VAL_HRO             validate hourly rate override                
         DS    0H                  (does not return error)                      
                                                                                
* c) 1R account                                                                 
                                                                                
UPDPER11 GOTOR VAL_ODS             validate off/dep/sub data                    
         JNE   UPDPER80                                                         
                                                                                
         DS    0H                                                               
         MVC   RV_ULA,SPACES                                                    
         MVC   RV_ULA(2),IS_1R                                                  
         LA    RE,RV_ULA+2                                                      
         LLC   R1,RV_1RL1          set level 1                                  
         SHI   R1,1                                                             
         MVC   0(0,RE),RQP_OFF                                                  
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RE,R1                                                            
         LLC   R1,RV_1RL2          set level 2                                  
         LLC   RF,RV_1RL1                                                       
         SR    R1,RF                                                            
         SHI   R1,1                                                             
         MVC   0(0,RE),RQP_DEP                                                  
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RE,R1                                                            
         LLC   R1,RV_1RL3          set level 3                                  
         LLC   RF,RV_1RL2                                                       
         SR    R1,RF                                                            
         SHI   R1,1                                                             
         MVC   0(0,RE),RQP_SUB                                                  
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RE,R1                                                            
         LLC   R1,RV_1RL4          set level 4                                  
         LLC   RF,RV_1RL3                                                       
         SR    R1,RF                                                            
         SHI   R1,1                                                             
         MVC   0(0,RE),RV_PIDC                                                  
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
                                                                                
         MVC   RV_ULA1R,RV_ULA                                                  
                                                                                
         GOTOR VAL_LVL             validate high level accounts                 
         JNE   UPDPER80            (may also return acceptable error)           
                                                                                
         GOTOR VAL_AOF,RQP_PERQ    validate account not on file yet             
         JNE   UPDPER80            (may also return acceptable error)           
                                                                                
         GOTOR VAL_1RN             validate and set names                       
         DS    0H                  (does not return error)                      
                                                                                
         GOTOR VAL_1RV             validate subsidiary 1R values                
         JNE   UPDPER80            (may also return acceptable error)           
                                                                                
* d) 2P account                                                                 
                                                                                
         MVC   RV_ULA,SPACES                                                    
         MVC   RV_ULA(2),IS_2P                                                  
         MVC   RV_ULA+2(L'RQP_2PC),RQP_2PC                                      
         CLC   RQP_2PC,SPACES      code override passed?                        
         JH    UPDPER16                                                         
         MVC   RV_ULA+2(L'RQP_2PC),SPACES                                       
         LA    R1,RV_ULA+(L'ACTKUNT+L'ACTKLDG)                                  
         CLI   RV_2POP,0                                                        
         JE    UPDPER12                                                         
         CLI   RV_2POP,1                                                        
         JNE   *+2                                                              
         LLC   RF,RV_2PL1                                                       
         BCTR  RF,0                                                             
         MVC   0(0,R1),RQP_OFF                                                  
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         AR    R1,RF                                                            
                                                                                
UPDPER12 DS    0H                                                               
*&&UK                                                                           
         LLC   RF,RV_2PL1          get dept length assume level 1               
         CLC   CUAALF,=C'A1'       GroupM                                       
         JE    UPDPER13                                                         
         CLC   CUAALF,=C'T1'       GroupM test file                             
         JE    UPDPER13                                                         
*&&                                                                             
         CLI   RV_2PDP,0           Assume if dept position non zero             
         JE    UPDPER14             it's either position 1 or after off         
         LLC   RF,RV_2PDL          get dept length                              
UPDPER13 BCTR  RF,0                                                             
         MVC   0(0,R1),RQP_DEP                                                  
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         AR    R1,RF                                                            
                                                                                
UPDPER14 MVC   0(L'RV_PIDC,R1),RV_PIDC                                          
                                                                                
UPDPER16 MVC   RV_ULA2P,RV_ULA                                                  
                                                                                
         GOTOR VAL_LVL             validate high level accounts                 
         JNE   UPDPER80            (may also return acceptable error)           
                                                                                
         GOTOR VAL_AOF,RQP_2PCQ    validate account not on file yet             
         JNE   UPDPER80            (may also return acceptable error)           
                                                                                
         GOTOR VAL_2PV             validate subsidiary 2P values                
         JNE   UPDPER80            (may also return acceptable error)           
                                                                                
* e) SX account                                                                 
                                                                                
*&&UK                                                                           
         GOTOR GET_MAP,0                                                        
*&&                                                                             
                                                                                
         MVC   RV_ULA,SPACES                                                    
         MVC   RV_ULA(2),IS_SX                                                  
         MVC   RV_ULA+2(L'RQP_SXC),RQP_SXC                                      
         CLC   RQP_SXC,SPACES      code override passed?                        
         JH    UPDPER26                                                         
         MVC   RV_ULA+2(L'RQP_SXC),SPACES                                       
         LA    R1,RV_ULA+(L'ACTKUNT+L'ACTKLDG)                                  
*&&US                                                                           
         CLC   CUAALF,=C'GP'       Grey NY                                      
         JE    UPDPER18                                                         
*&&                                                                             
         CLI   RV_SXOP,0                                                        
         JE    UPDPER20                                                         
         CLI   RV_SXOP,1                                                        
         JNE   *+2                                                              
UPDPER18 LLC   RF,RV_SXL1                                                       
         BCTR  RF,0                                                             
         MVC   0(0,R1),RQP_OFF                                                  
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         AR    R1,RF                                                            
                                                                                
UPDPER20 CLI   RV_SXDP,0           Assume if dept position non zero             
         JE    UPDPER21             it's either position 1 or after off         
         LLC   RF,RV_SXDL          get dept length                              
         BCTR  RF,0                                                             
         MVC   0(0,R1),RQP_DEP                                                  
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         AR    R1,RF                                                            
                                                                                
UPDPER21 DS    0H                                                               
*&&UK                                                                           
         CLC   CUAALF,=C'A1'       GroupM                                       
         JE    UPDPER22                                                         
         CLC   CUAALF,=C'T1'       GroupM test file                             
         JNE   UPDPER24                                                         
UPDPER22 MVC   RV_ULA+2(L'RQP_SXC),SPACES                                       
         MVC   RV_ULA+2(L'RV_1ROCM),RV_1ROCM                                    
         MVC   RV_ULA+2+L'RV_1ROCM(L'RV_PIDC),RV_PIDC                           
         J     UPDPER26                                                         
*&&                                                                             
                                                                                
UPDPER24 MVC   0(L'RV_PIDC,R1),RV_PIDC                                          
                                                                                
UPDPER26 MVC   RV_ULASX,RV_ULA                                                  
                                                                                
         GOTOR VAL_LVL             validate high level accounts                 
         JNE   UPDPER80            (may also return acceptable error)           
                                                                                
         GOTOR VAL_AOF,RQP_SXCQ    validate account not on file yet             
         JNE   UPDPER80            (may also return acceptable error)           
                                                                                
         GOTOR VAL_SXV             validate subsidiary SX values                
         JNE   UPDPER80            (may also return acceptable error)           
                                                                                
* f. Time and Expenses approver entries                                         
                                                                                
         GOTOR VAL_APS             validate t/s and exp approver                
         DS    0H                  (does not return error)                      
                                                                                
         GOTOR VAL_ROF             validate records on file                     
         DS    0H                  (does not return error)                      
                                                                                
* g. Limit List PID/PIN entries validation                                      
                                                                                
         GOTOR VAL_LLS             validate Limit list entries                  
         DS    0H                  (does not return error)                      
                                                                                
         J     UPDPER40                                                         
                                                                                
* h. Change validation                                                          
                                                                                
UPDPER28 DS    0H                                                               
                                                                                
         GOTOR VAL_CHA             validate type of change                      
         JNE   UPDPER80                                                         
                                                                                
* i. Name change validation                                                     
                                                                                
         TM    RV_CHIND,RV_CHNCQ   name change?                                 
         JZ    UPDPER30                                                         
                                                                                
         GOTOR VAL_NAM             validate (new) names                         
         JNE   UPDPER80                                                         
                                                                                
         GOTOR VAL_1RN             validate and set names                       
         DS    0H                  (does not return error)                      
                                                                                
         GOTOR TST_PER             test for cost person record                  
         JNE   UPDPER80            (existing and name change)                   
                                                                                
         GOTOR TST_1RS             test for 1R records                          
         DS    0H                  (does not return error)                      
                                                                                
         GOTOR TST_2PA             test for 2P account                          
         DS    0H                  (does not return error)                      
                                                                                
         GOTOR TST_SXA             test for SX account                          
         DS    0H                  (does not return error)                      
                                                                                
* j. Person termination validation                                              
                                                                                
UPDPER30 DS    0H                                                               
                                                                                
         TM    RV_CHIND,RV_CHPTQ   terminate person change?                     
         JZ    UPDPER35                                                         
                                                                                
* Note: no payroll history checks/deletion, no 2P nor SX changes                
                                                                                
         GOTOR VAL_TRM             validate termination action                  
         JNE   UPDPER80                                                         
                                                                                
* k. Limit List PID/PIN entries validation                                      
                                                                                
UPDPER35 DS    0H                                                               
                                                                                
         GOTOR VAL_LLS             validate Limit list entries                  
                                                                                
                                                                                
* 4. Updates now ('die not error' or 'critical error' from here on)             
                                                                                
* Note: No PTREL/RAPRECD maintenance done here                                  
                                                                                
UPDPER40 DS    0H                                                               
                                                                                
         CLI   RQP_ACT,RQP_ADQ     Action: add a new person                     
         JE    UPDPER50                                                         
         CLI   RQP_ACT,RQP_CHQ     Action: change a person                      
         JE    UPDPER60                                                         
         J     *+2                 (unknown)                                    
                                                                                
* a) Cost person record                                                         
                                                                                
UPDPER50 TM    RV_ERIND,RV_ERPEQ+RV_ERHDQ                                       
         JNZ   UPDPER54            skip on person or hire date error            
         TM    RV_ERIN2,RV_ERH1R                                                
         JNZ   UPDPER54            skip on 1R level error                       
                                                                                
         GOTOR BLD_PER             build basic person record and add            
         GOTOR ADD_GPN             data elements                                
         GOTOR ADD_LOC                                                          
         GOTOR ADD_PID                                                          
         GOTOR ADD_DDA                                                          
         GOTOR ADD_REC             add record and passives                      
         GOTOR SAVRET,RT_CPQ                                                    
                                                                                
* b) Cost history for person                                                    
                                                                                
         CLI   RV_DPCD,0           auto history set?                            
         JE    UPDPER52                                                         
                                                                                
         TM    RV_ERIND,RV_ERHSE   skip if some already exists                  
         JNZ   UPDPER52                                                         
                                                                                
         GOTOR ADD_HIS             add history rates                            
                                                                                
* c) 1R account                                                                 
                                                                                
UPDPER52 TM    RV_ERIN2,RV_ERH1R+RV_ERE1R                                       
         JNZ   UPDPER54            skip 1R if any 2P error                      
                                                                                
         MVC   RV_ULA,RV_ULA1R                                                  
         GOTOR SET_KEY,RV_1RAD     add basic record and data elements           
         GOTOR ADD_RST             (1R has no ASTEL)                            
         GOTOR ADD_ABL                                                          
         GOTOR ADD_APO                                                          
         GOTOR ADD_EMP                                                          
         GOTOR ADD_GPN                                                          
*        GOTOR ADD_GDA             > not required and not fully coded           
         GOTOR ADD_DDA                                                          
         GOTOR ADD_REC             add record and passives                      
         GOTOR SAVRET,RT_1RQ                                                    
                                                                                
* d) 2P account                                                                 
                                                                                
UPDPER54 TM    RV_ERIN2,RV_ERH2P+RV_ERE2P                                       
         JNZ   UPDPER56            skip 2P if any 2P error                      
                                                                                
         MVC   RV_ULA,RV_ULA2P                                                  
         GOTOR SET_KEY,RV_2PAD     add basic record and data elements           
         GOTOR ADD_RST                                                          
         GOTOR ADD_ABL                                                          
         GOTOR ADD_AST                                                          
         GOTOR ADD_APO                                                          
         GOTOR ADD_PID                                                          
         GOTOR ADD_RAC                                                          
         GOTOR ADD_REC             add record and passives                      
         GOTOR SAVRET,RT_2PQ                                                    
                                                                                
* e) SX account                                                                 
                                                                                
UPDPER56 TM    RV_ERIN2,RV_ERHSX+RV_ERESX+RV_ERMSX                              
         JNZ   UPDPER58            skip SX if any 2P error                      
                                                                                
         MVC   RV_ULA,RV_ULASX                                                  
         GOTOR SET_KEY,RV_SXAD     add basic record and data elements           
         GOTOR ADD_RST                                                          
         GOTOR ADD_PID                                                          
         GOTOR ADD_ABL                                                          
         GOTOR ADD_AST                                                          
         GOTOR ADD_APO                                                          
         GOTOR ADD_BAC                                                          
         GOTOR ADD_SPA                                                          
         GOTOR ADD_EMA                                                          
         GOTOR ADD_RAC                                                          
         GOTOR ADD_REC             add record and passives                      
         GOTOR SAVRET,RT_SXQ                                                    
                                                                                
* f. Time and Expenses approver entries                                         
                                                                                
UPDPER58 TM    RV_ERIND,RV_ERTAQ+RV_ERXAQ                                       
         JNZ   UPDPER59            skip both on any error                       
         TM    RV_ERIN2,RV_ERH1R                                                
         JNZ   UPDPER59                                                         
                                                                                
         GOTOR UPD_APS             add/update t/s and exp approver(s)           
                                                                                
* g. Limit List update                                                          
                                                                                
UPDPER59 TM    RV_ERIN3,RV_ERLLS                                                
         JNZ   UPDPER80            skip on any error                            
                                                                                
         GOTOR UPD_LLS             update Limit List record                     
                                                                                
         J     UPDPER80                                                         
                                                                                
* h. Change updates                                                             
                                                                                
UPDPER60 DS    0H                                                               
                                                                                
* i. Name change updates                                                        
                                                                                
         TM    RV_CHIND,RV_CHNCQ   person name change?                          
         JZ    UPDPER70                                                         
                                                                                
         GOTOR UPD_PER                                                          
         GOTOR SAVRET,RT_CPQ                                                    
                                                                                
         GOTOR UPD_1RS                                                          
                                                                                
         TM    RV_ERIND,RV_ER2PQ   skip 2P?                                     
         JNZ   UPDPER62                                                         
                                                                                
         CLC   RV_ULA2P,SPACES                                                  
         JNH   UPDPER62                                                         
                                                                                
         GOTOR UPD_ACT,RV_ULA2P                                                 
         GOTOR SAVRET,RT_2PQ                                                    
                                                                                
UPDPER62 TM    RV_ERIND,RV_ERSXQ   skip SX?                                     
         JNZ   UPDPER64                                                         
                                                                                
         TM    RV_ERIN2,RV_ERMSX                                                
         JNZ   UPDPER64                                                         
                                                                                
         CLC   RV_ULASX,SPACES                                                  
         JNH   UPDPER64                                                         
                                                                                
         GOTOR UPD_ACT,RV_ULASX                                                 
         GOTOR SAVRET,RT_SXQ                                                    
                                                                                
UPDPER64 DS    0H                                                               
         J     UPDPER75                                                         
                                                                                
* j. Person termination updates                                                 
                                                                                
UPDPER70 DS    0H                                                               
                                                                                
         TM    RV_CHIND,RV_CHPTQ   terminate person change?                     
         JZ    UPDPER75                                                         
                                                                                
         TM    RV_CHIND,RV_CHNCQ   already done above?                          
         JNZ   UPDPER80                                                         
                                                                                
         GOTOR UPD_PER                                                          
         GOTOR SAVRET,RT_CPQ                                                    
                                                                                
         TM    RV_ERIN3,RV_ERARM                                                
         JNZ   UPDPER80                                                         
                                                                                
         GOTOR UPD_A1R                                                          
                                                                                
* k. Limit List updates                                                         
                                                                                
UPDPER75 TM    RV_ERIN3,RV_ERLLS                                                
         JNZ   UPDPER80            skip on any error                            
                                                                                
         GOTOR UPD_LLS             update Limit List record                     
                                                                                
* 5. Common data and error return                                               
                                                                                
         USING LIOBD,R4                                                         
R        USING RT_D,RF                                                          
UPDPER80 L     R4,AALIOB           after updates pass return data               
         LAY   RF,RETTAB                                                        
         CLI   R.RT_TYPE,RT_EOTQ   anything left?                               
         JNE   UPDPER81                                                         
         LAY   RF,ERRTAB                                                        
         CLI   R.RT_TYPE,RT_EOTQ   anything left?                               
         JNE   UPDPER81                                                         
*                                                                               
         XR    R0,R0               BUILD DOWNLOAD MAP ELEMENT                   
         ICM   R0,3,LP_QMAPN                                                    
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',(R0))                
         J     UPDPER98                                                         
         DROP  R                                                                
                                                                                
         USING RT_D,R2                                                          
UPDPER81 LAY   R2,RETTAB                                                        
                                                                                
UPDPER82 CLI   RT_TYPE,RT_EOTQ     anything left?                               
         JE    UPDPER90                                                         
                                                                                
         XR    R0,R0               Build download map element                   
         ICM   R0,B'0011',LP_QMAPN                                              
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TYPE),     +        
               ('LD_CHARQ',RT_TYPE),(L'RT_TYPE,0)                               
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CODE),     +        
               ('LD_CHARQ',RT_CODE),(L'RT_CODE,0)                               
                                                                                
         AHI   R2,RT_LNQ                                                        
         J     UPDPER82                                                         
                                                                                
         USING ET_D,R2                                                          
UPDPER90 LAY   R2,ERRTAB           Send errors                                  
                                                                                
UPDPER92 CLI   ET_D,ET_EOTQ        Test end of error table                      
         JE    UPDPER98                                                         
                                                                                
         LLC   RF,ERRCNT           Check for maximum number of error            
         CHI   RF,ERRMAX           messages                                     
         JNL   UPDPER98                                                         
                                                                                
         XR    R0,R0               Build download map element                   
         ICM   R0,B'0011',LP_QMAPN                                              
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
                                                                                
         USING GETTXTD,R1                                                       
         LA    R1,DMCB             Build error text string in ELEMENT           
         XC    GTBLOCK,GTBLOCK     Resolve error text                           
         MVI   GTMAXL,80                                                        
         MVC   GTMSGNO,ET_ERRNO                                                 
         LA    R0,ELEMENT                                                       
         STCM  R0,B'0111',GTAOUT                                                
         MVI   GTMTYP,GTMERR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LLC   R0,ET_LN                                                         
         SHI   R0,ET_LN1Q                                                       
         LTR   R0,R0                                                            
         JZ    UPDPER94                                                         
         STC   R0,GTLTXT           Set length of extra text                     
         LA    R0,ET_EXTRA                                                      
         STCM  R0,B'0111',GTATXT   Set A(Extra text)                            
                                                                                
UPDPER94 GOTOR VGETTXT,(R1)                                                     
         LLC   R0,4(R1)            Length of text just added                    
                                                                                
*        LA    RF,ELEMENT          Put out error number - not required          
*        AR    RF,R0                                                            
*        MVI   1(RF),BRKOPNQ                                                    
*        MVI   2(RF),NUMBERQ                                                    
*        LLH   RE,ET_FLDNM                                                      
*        CVD   RE,DUB                                                           
*        UNPK  3(6,RF),DUB+5(3)                                                 
*        OI    8(RF),X'F0'                                                      
*        MVI   9(RF),BRKCLOQ                                                    
*        AHI   R0,10                                                            
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERR),      +        
               ('LD_CHARQ',ELEMENT),((R0),0)                                    
         OC    ET_FLDNM,ET_FLDNM                                                
         JZ    UPDPER96                                                         
         GOTOR AALINKIO,(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#ERROW),    +        
               ('LD_LBINQ',ET_FLDNM),(L'ET_FLDNM,0)                             
                                                                                
UPDPER96 LLC   R0,ET_LN            Bump to next error table entry               
         AR    R2,R0                                                            
                                                                                
         LLC   RF,ERRCNT           Error counter                                
         AHI   RF,1                                                             
         STC   RF,ERRCNT                                                        
                                                                                
         J     UPDPER92                                                         
         DROP  R1,R2                                                            
                                                                                
UPDPER98 TM    TWAMODE,TWAMUWD     unwind?                                      
         JZ    UPDPERY                                                          
         MVI   LP_RMODE,LP_RERRR                                                
         MVI   LP_EMSYS,6                                                       
         J     UPDPERN                                                          
                                                                                
UPDPERCE OI    TWAMODE,TWAMEDP     critical error => don't proceed              
         J     UPDPEREX                                                         
                                                                                
UPDPERER OI    TWAMODE,TWAMERP     regular error => proceed                     
                                                                                
UPDPEREX ICM   R1,B'0011',ROUERRV                                               
         GOTOR PUTERR                                                           
         J     UPDPERN                                                          
                                                                                
*** Global exists for Accounting Person Upload ************************         
                                                                                
UPDPERY  LA    R0,RQUPVAL                                                       
         LA    R1,RQUPLNQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     UPDRECY                                                          
                                                                                
UPDPERN  LA    R0,RQUPVAL                                                       
         LA    R1,RQUPLNQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         TM    TWAMODE,TWAMUWD     unwind required?                             
         JZ    EXITN                                                            
         NI    TWAMODE,FF-TWAMUWD                                               
         OI    GIND1,GIUNWIND      DDLINK will unwind/abend                     
         J     EXITN                                                            
                                                                                
         DROP  RB                                                               
                                                                                
***********************************************************************         
* Upload: Update BACS email sent status                               *         
***********************************************************************         
*&&UK                                                                           
UPD5PE   BASE  ,                                                                
                                                                                
         CLC   RQ5PSUL,=C'SF'      Supplier ledgers only                        
         JE    UPD5P06                                                          
         CLC   RQ5PSUL,=C'SV'                                                   
         JE    UPD5P06                                                          
         CLC   RQ5PSUL,=C'SX'                                                   
         JE    UPD5P06                                                          
         CLC   RQ5PSUL,=C'ST'                                                   
         JE    UPD5P06                                                          
                                                                                
         MVC   XERRTXT(2),RQ5PSUL                                               
         GOTOR PUTERR,AE$INLDG                                                  
         B     UPD5PEN                                                          
                                                                                
UPD5P06  CLC   RQ5PACT,SPACES      Supplier account                             
         BH    UPD5P10                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$MIACC                                                  
         B     UPD5PEN                                                          
                                                                                
UPD5P10  CLC   RQ5PPRF,SPACES      Payment ref                                  
         JH    UPD5P14                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVCDD XERRTXT(12),AC#PAYRF                                             
         GOTOR PUTERR,AE$MISIF                                                  
         J     UPD5PEN                                                          
                                                                                
UPD5P14  CLC   RQ5PDAT,SPACES      Payment date                                 
         JH    UPD5P18                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVCDD XERRTXT(12),AC#PAYDT                                             
         GOTOR PUTERR,AE$MISIF                                                  
         J     UPD5PEN                                                          
                                                                                
UPD5P18  CLC   RQ5PEML,SPACES      Email address                                
         JH    UPD5P20                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVCDD XERRTXT(12),AC#EMAIL                                             
         GOTOR PUTERR,AE$MISIF                                                  
         J     UPD5PEN                                                          
                                                                                
UPD5P20  CLC   RQ5PEMDT,SPACES     Email sent date                              
         JH    UPD5P22                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVCDD XERRTXT(12),AC#EHBST                                             
         GOTOR PUTERR,AE$MISIF                                                  
         J     UPD5PEN                                                          
                                                                                
UPD5P22  CLC   RQ5PEMTM,SPACES     Email sent time                              
         JH    UPD5P24                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVCDD XERRTXT(12),AC#EHBST                                             
         GOTOR PUTERR,AE$MISIF                                                  
         J     UPD5PEN                                                          
                                                                                
UPD5P24  ZAP   DUB2,PZERO                                                       
         USING RNSPASD,IOKEY       Use ref passives to find the SC trx          
         XC    RNSPKEY,RNSPKEY                                                  
         MVI   RNSPTYP,RNSPTYPQ                                                 
         MVI   RNSPSUB,RNSPSUBQ                                                 
         MVC   RNSPCPY,CUXCPY                                                   
         MVI   RNSPIND,RNSPRFQ                                                  
         MVC   RNSPREF,RQ5PPRF                                                  
         MVI   RNSPBTY,TRNTPAYS                                                 
         MVC   RNSPUL,=C'SC'                                                    
         MVC   RNSPDAT,RQ5PDAT                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
         J     UPD5P28                                                          
                                                                                
UPD5P26  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
UPD5P28  CLC   RNSPKEY(RNSPFIL-RNSPKEY),IOKEYSAV                                
         JNE   UPD5PEX             Finished/none                                
                                                                                
T        USING TRNKSTA,RNSPSTA                                                  
         TM    T.TRNKSTAT,TRNSDELT+TRNSDRFT+TRNSREVS+TRNSARCH                   
         JNZ   UPD5P26                                                          
         DROP  T                                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         MVI   BYTE4,1             Set '1st pass' = read without update         
         USING TRNRECD,R2                                                       
         L     R2,AIO1                                                          
***      CLC   TRNKULA(2),=C'SC'   Match SC                                     
***      JNE   UPD5P26                                                          
         CLC   TRNKULC,RQ5PSUP     Supplier code is contra                      
         JNE   UPD5P26                                                          
***      CLC   TRNKDATE,RQ5PDAT    Date                                         
***      JNE   UPD5P26                                                          
         USING TRNELD,RF                                                        
         LA    RF,TRNRFST                                                       
         CLI   TRNEL,TRNELQ        Safety                                       
         JNE   UPD5P26                                                          
**       CLI   TRNTYPE,60          Payment                                      
**       JNE   UPD5P26                                                          
         J     UPD5P32                                                          
                                                                                
UPD5P30  MVI   BYTE4,2             2nd pass - reread for update                 
         MVC   WORK2(L'IOKEY),IOKEY  Save passive key                           
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'TRNKEY),TRNKEY                                           
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         USING TRNRECD,R2                                                       
         L     R2,AIO1                                                          
         USING TRSELD,RF                                                        
         LA    RF,TRNRFST                                                       
                                                                                
UPD5P32  SR    R0,R0                                                            
         CLI   TRSEL,0                                                          
         JE    UPD5PEX2            Short/no TRSEL - exit with errmsg            
         CLI   TRSEL,TRSELQ                                                     
         JNE   *+12                                                             
         CLI   TRSLN,TRSLN2Q       Want long element                            
         JNL   *+14                                                             
         IC    R0,TRSLN                                                         
         AR    RF,R0                                                            
         J     UPD5P32                                                          
                                                                                
         TM    TRSSTAT6,TRSSEMP    Expect 'email pending' sts                   
         JZ    UPD5P26                                                          
         CLI   BYTE4,2             Test 2nd pass                                
         JNE   UPD5P30             Reread for update                            
                                                                                
*                                  Update record:                               
         NI    TRSSTAT6,255-TRSSEMP                                             
         OI    TRSSTAT6,TRSSEMS    Set 'email sent' sts                         
                                                                                
         USING FFTELD,RF                                                        
         LA    RF,TRNRFST                                                       
UPD5P38  CLI   FFTEL,0             Find possible email address el               
         JE    UPD5P40             (if repeat request to send email)            
         CLI   FFTEL,FFTELQ                                                     
         JNE   *+8                                                              
         CLI   FFTTYPE,FFTTREML                                                 
         JE    *+14                                                             
         IC    R0,FFTLN                                                         
         AR    RF,R0                                                            
         J     UPD5P38                                                          
                                                                                
         MVI   FFTEL,X'FF'         Delete existing element                      
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('FF',TRNRECD),0                       
                                                                                
UPD5P40  LA    RF,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     Build a new element                          
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTREML                                                 
         MVC   FFTREMDT,RQ5PEMDT                                                
         PACK  DUB,RQ5PEMTM        Convert HHMM time to packed                  
         MVO   DUB,DUB                                                          
         MVC   FFTREMTM,DUB+5                                                   
         MVC   FFTREMLA,RQ5PEML                                                 
         LA    RE,FFTREMLA+L'FFTREMLA-1                                         
         CLI   0(RE),C' '                                                       
         JH    *+8                                                              
         JCT   RE,*-8                                                           
         LA    R1,FFTREMLA                                                      
         SR    RE,R1                                                            
         AHI   RE,2+L'FFTREMDT+L'FFTREMTM                                       
         STC   RE,FFTDLEN                                                       
         AHI   RE,FFTLN1Q                                                       
         STC   RE,FFTLN                                                         
         LA    RF,=C'ADD=END'                                                   
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TRNRECD,ELEMENT,(RF),0                 
         CLI   12(R1),0                                                         
         JNE   *+2                 Die - can't add element                      
         DROP  RF                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JNE   *+2                                                              
                                                                                
         MVC   IOKEY,WORK2         Restore passive key + read sequence          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
         AP    DUB2,PONE                                                        
         J     UPD5P26        Go again, payment may be split by office          
                                                                                
UPD5PEX  CP    DUB2,PZERO          Test any postings found                      
         JH    UPDRECY             Yes                                          
                                                                                
UPD5PEX2 MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'RQ5PPRF),RQ5PPRF                                       
         MVC   XERRTXT+L'RQ5PPRF+1(L'XERRTXT-(L'RQ5PPRF+1)),RQ5PSUP             
         GOTOR PUTERR,AE$PNFUS     'Payment n/f or unexpected status'           
                                                                                
UPD5PEN  J     EXITN                                                            
*&&                                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* CALL LINKIO TO BUILD ERROR RETURN ELEMENT                           *         
***********************************************************************         
                                                                                
PUTERR   NTR1  LABEL=NO                                                         
         STCM  R1,3,WORK                                                        
                                                                                
         CLC   XERRTXT,SPACES                                                   
         JNH   PUTERR2                                                          
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTERR',D#UPLERR),  *        
               WORK,(L'XERRTXT,XERRTXT)                                         
         J     PUTERR4                                                          
                                                                                
PUTERR2  GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTERR',D#UPLERR),  *        
               WORK,0                                                           
                                                                                
PUTERR4  MVC   XERRTXT,SPACES                                                   
                                                                                
PUTERRX  J     EXITY                                                            
                                                                                
***********************************************************************         
* GENERAL EXITS HERE                                                  *         
***********************************************************************         
                                                                                
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Routines within this server                                         *         
***********************************************************************         
                                                                                
* Dummy routine                                                                 
DUMROU   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*DUMROU*'                                                      
                                                                                
         J     DUMROUY                                                          
                                                                                
DUMROUY  J     EXITY                                                            
                                                                                
DUMROUN  J     EXITN                                                            
                                                                                
                                                                                
***********************************************************************         
* Account lock/unlock - validate unit ledger account                  *         
***********************************************************************         
         SPACE 1                                                                
*&&UK                                                                           
VALULA   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*VALULA*'                                                      
                                                                                
         OC    RQ_ULA,SPACES                                                    
         CLC   RQ_ULA,SPACES                                                    
         JH    VULA002                                                          
         MVC   ROUERRV,=AL2(AE$MIACC)                                           
         J     VALULAN                                                          
                                                                                
VULA002  XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,RQ_ULA                                                    
         GOTOR (#SETLDG,ASETLDG)                                                
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$INLDG)                                           
         J     VALULAN                                                          
                                                                                
         L     R2,AIO1                                                          
         USING LDGRECD,R2                                                       
         LA    R2,LDGRFST          LOCATE LEVEL ELEMENT                         
         USING LDGELD,R2                                                        
         XR    R0,R0                                                            
VULA003  CLI   LDGEL,0                                                          
         JE    VULA004                                                          
         CLI   LDGEL,LDGELQ                                                     
         JE    VULA003B                                                         
VUAL003A IC    R0,LDGLN                                                         
         AR    R2,R0                                                            
         J     VULA003                                                          
                                                                                
VULA003B MVC   X#LST2,LDGSTAT2                                                  
                                                                                
         USING ACTRECD,R2                                                       
VULA004  LA    R2,IOKEY            build account key                            
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,RQ_ULA                                                   
         MVC   CSVKEY1,ACTKEY                                                   
                                                                                
         CLC   PRODUL,RQ_ULA       Are we dealing with productn ledger          
         JNE   VULA016                                                          
                                                                                
         LLC   R1,PCLILEN          read for client                              
         LA    R1,ACTKACT(R1)                                                   
         MVC   0(L'ACTKACT,R1),SPACES                                           
         MVC   ROUERRV,=AL2(AE$INCLI)                                           
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   VALULAN                                                          
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING PPRELD,R3                                                        
         L     R2,AIO2                                                          
         LA    R3,ACTRFST                                                       
         XR    R0,R0                                                            
                                                                                
VULA006  CLI   PPREL,0             retrieve office code                         
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PPREL,PPRELQ                                                     
         JE    VULA008                                                          
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         J     VULA006                                                          
                                                                                
VULA008  MVC   X#OFFC,PPRGAOFF                                                  
         OC    X#OFFC,SPACES                                                    
         DROP  R3                                                               
                                                                                
         LA    R2,IOKEY            read for product                             
         MVC   ACTKEY,CSVKEY1                                                   
         LLC   RE,PCLILEN                                                       
         LA    RE,ACTKACT(RE)                                                   
         CLC   0(L'ACTKACT,RE),SPACES    Is account client level                
         JNH   VUAL030                                                          
         MVC   0(L'ACTKACT,RE),SPACES                                           
                                                                                
         MVC   ROUERRV,=AL2(AE$INPRO)                                           
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   VALULAN                                                          
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO2                                                          
         LA    R3,ACTRFST                                                       
         USING PPRELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
VULA010  CLI   PPREL,0             check for office override                    
         JE    VULA014                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    VULA012                                                          
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         J     VULA010                                                          
                                                                                
VULA012  CLC   PPRGAOFF,SPACES                                                  
         BNH   VULA014                                                          
         MVC   X#OFFC,PPRGAOFF                                                  
         OC    X#OFFC,SPACES                                                    
         DROP  R3                                                               
                                                                                
VULA014  LA    R2,IOKEY            read for job updative                        
         MVC   ACTKEY,CSVKEY1                                                   
         LLC   RE,PPROLEN                                                       
         LA    RE,ACTKACT(RE)                                                   
         CLC   0(L'ACTKACT,RE),SPACES    Is account product level               
         JNH   VUAL030                                                          
                                                                                
VULA016  MVC   ROUERRV,=AL2(AE$INJOB)                                           
         CLC   PRODUL,RQ_ULA       Are we dealing with productn ledger          
         JE    *+10                                                             
         MVC   ROUERRV,=AL2(AE$INACC)                                           
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   VALULAN                                                          
                                                                                
                                                                                
         MVC   ROUERRV,=AL2(AE$ACCNA)                                           
         TM    ACTKSTAT,ACTSDRFT                                                
         JNZ   VALULAN             disallow on draft jobs                       
                                                                                
         MVC   CSVKEY1,ACTKEY                                                   
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   PRODUL,RQ_ULA       Are we dealing with productn ledger          
         JNE   VULA024             No - ignore office look up                   
                                                                                
         USING PPRELD,R3                                                        
         L     R2,AIO2                                                          
         LA    R3,ACTRFST                                                       
         XR    R0,R0                                                            
                                                                                
VULA020  CLI   PPREL,0             check for office override                    
         JE    VUAL030                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    VULA022                                                          
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         J     VULA020                                                          
                                                                                
VULA022  CLC   PPRGAOFF,SPACES                                                  
         BNH   VUAL030                                                          
         MVC   X#OFFC,PPRGAOFF                                                  
         OC    X#OFFC,SPACES                                                    
         J     VUAL030                                                          
         DROP  R3                                                               
                                                                                
VULA024  CLI   LDGAOP,0            No office                                    
         JE    VULA032                                                          
         CLI   LDGAOP,LDGOTRAN     Office in transaction                        
         JE    VULA032                                                          
         NI    LDGAOP,X'FF'-LDGOKEY2                                            
         CLI   LDGAOP,LDGOKEY      Is office in key of account                  
         JH    VULA032             No - could be filters                        
         MVC   X#OFFC,SPACES        Yes - extract office                        
         L     R1,AIO2                                                          
         LLC   RE,LDGAOP                                                        
         SHI   RE,1                                                             
         LA    RF,ACTKACT-ACTRECD(R1)                                           
         AR    RE,RF               A(office code)                               
         SR    RF,RF                                                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    VULA026                                                          
         AHI   RF,1                L'office code                                
VULA026  BASR  R2,0                                                             
         MVC   X#OFFC(0),0(RE)                                                  
         EX    RF,0(R2)                                                         
                                                                                
         USING OFFALD,R3                                                        
VUAL030  L     R3,AOFFAREA         check office access                          
         MVC   OFFAOFFC,X#OFFC                                                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL,OFFALD                                                    
         JE    VULA032                                                          
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     VALULAN                                                          
         DROP  R3                                                               
                                                                                
         USING RSTELD,R3                                                        
VULA032  L     R2,AIO2             get element addresses                        
         LA    R3,ACTRFST                                                       
         XR    R0,R0                                                            
                                                                                
VULA034  CLI   RSTEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   RSTEL,RSTELQ                                                     
         JE    VULA038                                                          
VULA036  IC    R0,RSTLN                                                         
         AR    R3,R0                                                            
         J     VULA034                                                          
                                                                                
VULA038  CLI   RQ_STA,NOQ          lock action?                                 
         JE    VULA040             No - must be unlock                          
                                                                                
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         TM    RSTSTAT1,RSTSACIL   locked already?                              
         JNZ   VALULAN                                                          
                                                                                
         OI    RSTSTAT1,RSTSACIL         Set lock status on element             
         OI    ACTRSTAT,ACTSLOCK              and record key                    
         OI    ACTKSTAT-ACTRECD+CSVKEY1,ACTSLOCK                                
         J     VULA042                                                          
                                                                                
VULA040  MVC   ROUERRV,=AL2(AE$ACINL)                                           
         TM    RSTSTAT1,RSTSACIL   not locked?                                  
         JZ    VALULAN                                                          
                                                                                
         NI    RSTSTAT1,FF-RSTSACIL     Set unlock status on element            
         NI    ACTRSTAT,FF-ACTSLOCK             and record key                  
         NI    ACTKSTAT-ACTRECD+CSVKEY1,FF-ACTSLOCK                             
         J     VALULAY                                                          
                                                                                
         USING REBPASD,R4                                                       
VULA042  CLC   PRODUL,RQ_ULA                                                    
         JNE   VALULAY                                                          
                                                                                
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         MVI   REBPTYP,REBPTYPQ                                                 
         MVI   REBPSUB,REBPSUBQ                                                 
         MVC   REBPCPY,CUXCPY                                                   
         MVI   REBPMOD,REBPMCPJ                                                 
         MVC   REBPCPJ(L'PRODUL),PRODUL                                         
         MVC   REBPCPJ+L'PRODUL(L'RQ_ACC),RQ_ACC                                
         OC    REBPCPJ,SPACES                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     VULA046                                                          
                                                                                
VULA044  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
VULA046  CLI   REBPTYP,REBPTYPQ      ARE WE STILL READING INVOICE PASS?         
         BNE   VULA060                                                          
         CLI   REBPSUB,REBPSUBQ                                                 
         BNE   VULA060                                                          
         CLC   REBPCPY,CUXCPY                                                   
         JNE   VULA060                                                          
         CLI   REBPMOD,REBPMCPJ                                                 
         JNE   VULA060                                                          
         CLC   PRODUL,REBPCPJ                                                   
         JNE   VULA044                                                          
         CLC   RQ_ACC,REBPCPJ+L'PRODUL                                          
         JNE   VULA044                                                          
         TM    REBPSTA1,REBSACRB     SKIP IF DELETED BY ACRB OR LOGICAL         
         JNZ   VULA044               DELETED                                    
         TM    REBPSTA2,REBSBLDE                                                
         JNZ   VULA044                                                          
         TM    REBPSTA1,REBSBRAN     BRANDOCEAN?                                
         JZ    VULA048                                                          
         TM    REBPSTA2,REBSBREG     ERROR IF INV IS REGISTERED                 
         JNZ   VULA050                                                          
         J     VULA044                                                          
*                                                                               
VULA048  TM    REBPSTA2,REBSMDEL     POSTMAN SKIP MARKED TO BE DELETED          
         JNZ   VULA044                                                          
         TM    REBPSTA2,REBSOPEN     ERROR IF INV IS OPEN                       
         JZ    VULA044                                                          
VULA050  MVC   ROUERRV,=AL2(AE$DRFL)                                            
         J     VALULAN                                                          
*                                                                               
         USING TSJPASD,R3                                                       
VULA060  LA    R3,IOKEY            Check for unapproved time                    
         XC    TSJPAS,TSJPAS                                                    
         MVC   TSJPCPY,CUXCPY                                                   
         MVI   TSJPTYP,TSJPTYPQ                                                 
         MVI   TSJPSUB,TSJPSUBQ                                                 
         MVI   TSJPVIEW,TSJPSJAQ                                                
         MVC   TSJPCOFF,X#OFFC                                                  
         MVC   TSJPACT,RQ_ACC                                                   
         MVC   CSVKEY2,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     VULA064                                                          
                                                                                
VULA062  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
VULA064  CLC   IOKEY(TSJPMED-TSJPAS),CSVKEY2   test time passive                
         JNE   VALULAY             Finished                                     
         TM    TSJPSTAT,TIMSDELT   Is it deleted                                
         JNZ   VULA062                                                          
         TM    TSJPSTAT,TIMSFAPP   Is it approved                               
         JNZ   VULA062                                                          
         MVC   ROUERRV,=AL2(AE$DRFC) Unapproved timesheets exist                
         J     VALULAN                                                          
                                                                                
VALULAY  J     EXITY                                                            
                                                                                
VALULAN  J     EXITN                                                            
         DROP  R2,R3,R4                                                         
**********************************************************************          
* Account lock/unlock - updates to account, audit and passives       *          
**********************************************************************          
         SPACE 1                                                                
UPDACA   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*UPDACA*'                                                      
                                                                                
         USING ACTRECD,R2                                                       
         L     R2,AIO2             R2=account record                            
                                                                                
         CLC   PRODUL,RQ_UNIT      Only update audit record                     
         JNE   UPACA70                 for production ledger and jobs           
         LLC   RF,PJOBLEN                                                       
         LA    RF,ACTKACT(RF)                                                   
         CLI   0(RF),C' '        Check first character of job code              
         JNH   UPACA70              if space must be product or client          
                                                                                
         USING STCELD,R3                                                        
         LA    R3,ELEMENT           Build audit element                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   STCEL,STCELQ                                                     
         MVI   STCLN,STCLN8Q                                                    
         MVI   STCIND,STCIACT                                                   
         MVC   STCUSER,CUUSER                                                   
         MVC   STCPERS,CCTPID                                                   
         MVC   STCTERM,CUTERM                                                   
         MVC   STCDATE,TODAYP                                                   
         MVC   STCTIME,CTIME                                                    
         MVI   STCATYP,STCALOCK                                                 
         MVI   STCASTAT,STCACHG                                                 
         MVI   STCASTA,STCANOQ                                                  
         CLI   RQ_STA,NOQ                                                       
         JE    *+8                                                              
         MVI   STCASTA,STCAYESQ                                                 
         OI    STCASTA,STCAACUP                                                 
                                                                                
         LA    R4,IOKEY                                                         
         USING AUDRECD,R4                                                       
         XC    AUDKEY,AUDKEY       Read for audit record                        
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,CUXCPY                                                   
         MVI   AUDKAUDT,AUDKACC                                                 
         MVC   AUDKULA,ACTKULA                                                  
         MVI   AUDKSEQ,0                                                        
         MVC   CSVKEY3,AUDKEY                                                   
         L     R1,=AL4(IORDUP+IODIR+IO3)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   UPACA56                                                          
UPACA10  MVC   BYTE1,AUDKSEQ                                                    
                                                                                
         MVC   CSVKEY2,AUDKEY                                                   
         L     R1,=AL4(IOGETRUP+IOMST+IO3)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPACA20                                                          
         DC    H'0'                                                             
UPACA20  L     R4,AIO3                                                          
         MVC   AC_INDEX,AUDRINDX                                                
*                                                                               
         CLI   STCEL,0                                                          
         JE    UPACA40                                                          
         SR    RE,RE                                                            
         ICM   RE,3,AUDRLEN                                                     
         SR    RF,RF                                                            
         IC    RF,STCLN                                                         
         AR    RE,RF                                                            
         CH    RE,=H'2000'                                                      
         JH    UPACA40                                                          
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AUDRECD,STCELD,=C'ADD=END'             
         CLI   12(R1),0                                                         
         JE    UPACA30                                                          
         CLI   12(R1),5                                                         
         JE    UPACA40                                                          
         DC    H'0'                                                             
UPACA30  SR    RE,RE                                                            
         IC    RE,STCLN                                                         
         AR    R3,RE                                                            
*                                                                               
UPACA40  GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO3'                           
         JE    UPACA42                                                          
         DC    H'0'                                                             
UPACA42  LA    R4,IOKEY                                                         
         MVC   AUDKINDX,AC_INDEX                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO3'                            
         JE    UPACA44                                                          
         DC    H'0'                                                             
UPACA44  MVC   IOKEY,CSVKEY2                                                    
         L     R1,=AL4(IORDUPD+IODIR+IO3)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPACA46                                                          
         DC    H'0'                                                             
UPACA46  L     R1,=AL4(IOSQUP+IODIR+IO3)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   CSVKEY3(AUDKSEQ-AUDKEY),AUDKEY Do any records exist              
         JE    UPACA10             yes                                          
UPACA54  SR    RE,RE               No - build new record                        
         IC    RE,BYTE1                                                         
         AHI   RE,1                                                             
         STC   RE,BYTE1                                                         
UPACA56  CLI   STCEL,0                                                          
         JE    UPACA70                                                          
         L     R4,AIO3                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO3                                        
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,CUXCPY                                                   
         MVI   AUDKAUDT,AUDKACC                                                 
         MVC   AUDKULA,RQ_ULA                                                   
         MVC   AUDKSEQ,BYTE1                                                    
         MVC   AUDRLEN,=Y(RAURFST-RAUKEY)                                       
         XC    AUDRSTA,AUDRSTA                                                  
         MVC   AUDRSTAT,ACTRSTAT                                                
         MVC   AUDRINDX,AC_INDEX                                                
UPACA58  CLI   STCEL,0                                                          
         JE    UPACA62                                                          
         SR    RE,RE                                                            
         ICM   RE,3,AUDRLEN                                                     
         SR    RF,RF                                                            
         IC    RF,STCLN                                                         
         AR    RE,RF                                                            
         CH    RE,=H'2000'                                                      
         JH    UPACA62                                                          
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AUDRECD,STCELD,=C'ADD=END'             
         CLI   12(R1),0                                                         
         JE    UPACA60                                                          
         CLI   12(R1),5                                                         
         JE    UPACA62                                                          
         DC    H'0'                                                             
*                                                                               
UPACA60  SR    RE,RE                                                            
         IC    RE,STCLN                                                         
         AR    R3,RE                                                            
         J     UPACA58                                                          
*                                                                               
UPACA62  MVC   IOKEY,AUDKEY                                                     
         L     R1,=AL4(IORDUPD+IODIR+IO3)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPACA64                                                          
         CLI   IOERR,IOEDEL                                                     
         JE    UPACA64                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO3'                           
         JE    UPACA54                                                          
         DC    H'0'                                                             
*                                                                               
UPACA64  DS    0H                  update existing records                      
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO7                                        
         L     R0,AIO7             copy new record to IO7                       
         LA    R1,IOLENQ                                                        
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R1,=AL4(IOGETRUP+IOMST+IO3)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPACA66                                                          
         DC    H'0'                                                             
*                                                                               
UPACA66  L     R0,AIO3             copy new record to IO3                       
         LA    R1,2000                                                          
         L     RE,AIO7                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO3'                           
         JE    UPACA68                                                          
         DC    H'0'                                                             
UPACA68  LA    RE,IOKEY            pass new status                              
         MVC   AUDKSTA-AUDRECD(8,RE),AUDRSTA                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO3'                            
         JE    UPACA54                                                          
         DC    H'0'                                                             
         DROP  R3,R4                                                            
                                                                                
UPACA70  GOTO1 VHELLO,DMCB,(C'G',ACCMST),('RACELQ',ACTRECD),           +        
               (1,=AL1(RACTCHA))                                                
         CLI   12(R1),0                                                         
         JNE   UPACA80                                                          
         L     R3,12(R1)                                                        
         USING RACELD,R3                                                        
         MVC   RACUSER,CUUSER                                                   
         MVC   RACPERS,CCTPID                                                   
         MVC   RACTERM,CUTERM                                                   
         MVC   RACDATE,TODAYP                                                   
         MVC   RACTIME,CTIME                                                    
         MVI   RACAPPL,RACAURAQ                                                 
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether BrandOcean or Aura             
         CHI   RF,XPBROCNQ          is connected                                
         JNE   *+8                                                              
         MVI   RACAPPL,RACBRONQ                                                 
         CHI   RF,XPACCUPQ         Check whether AccUpload is connected         
         JNE   *+8                                                              
         MVI   RACAPPL,RACAUPLQ                                                 
         J     UPACA82                                                          
                                                                                
UPACA80  LA    R3,ELEMENT                                                       
         USING RACELD,R3                                                        
         MVI   RACEL,RACELQ                                                     
         MVI   RACLN,RACLNQ                                                     
         MVI   RACTYPE,RACTCHA                                                  
         MVC   RACUSER,CUUSER                                                   
         MVC   RACPERS,CCTPID                                                   
         MVC   RACTERM,CUTERM                                                   
         MVC   RACDATE,TODAYP                                                   
         MVC   RACTIME,CTIME                                                    
         MVI   RACAPPL,RACAURAQ                                                 
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether BrandOcean or Aura             
         CHI   RF,XPBROCNQ          is connected                                
         JNE   *+8                                                              
         MVI   RACAPPL,RACBRONQ                                                 
         CHI   RF,XPACCUPQ         Check whether AccUpload is connected         
         JNE   *+8                                                              
         MVI   RACAPPL,RACAUPLQ                                                 
                                                                                
         GOTOR VHELLO,DMCB,(C'P',ACCMST),ACTRECD,RACELD                         
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
UPACA82  GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   IOKEY,CSVKEY1       and directory                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING CPTRBLK,R4                                                       
         L     R4,AGENAXTN         get list of passives                         
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         MVC   CPYSTA6,CPYSTAT6                                                 
         MVC   CPYSTA7,CPYSTAT7                                                 
         MVC   CPYSTA9,CPYSTAT9                                                 
         MVC   CPYSTAC,CPYSTATC                                                 
         MVC   LDGSTA2,X#LST2                                                   
         MVC   LDGLVALN,LDGAL1                                                  
         MVC   LDGLVBLN,LDGAL2                                                  
         MVC   LDGLVCLN,LDGAL3                                                  
         MVC   LDGLVDLN,LDGAL4                                                  
         MVC   CLIOFFC,X#OFFC                                                   
         MVC   TODAY,TODAYF                                                     
         GOTO1 VPADDLE,DMCB,(C'B',AIO2),CPTRBLK,IODA,AELEAREA,ACOMFACS          
         DROP  R4                                                               
                                                                                
         DS    0H                  loop through table and update                
         L     R3,AELEAREA         status of passives                           
         XR    R1,R1                                                            
         MVC   BYTE2,CSVKEY1+ACTKSTAT-ACTRECD                                   
                                                                                
UPACA90  AHI   R3,ACCKLEN          ignore primary pointer                       
         CLI   0(R3),0             end of table?                                
         JE    UPACA92                                                          
                                                                                
         MVC   IOKEY(L'ACTKEY),0(R3)                                            
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO3)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   UPACA90                                                          
                                                                                
         MVC   IOKEY+ACTKSTAT-ACTRECD(L'ACTKSTAT),BYTE2                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO3'                            
         JE    UPACA90                                                          
         DC    H'0'                                                             
                                                                                
UPACA92  DS    0H                                                               
                                                                                
UPDACAY  J     EXITY                                                            
         DROP  R2,R3                                                            
*&&                                                                             
                                                                                
***********************************************************************         
* Acc Person Upload: validate action                                  *         
***********************************************************************         
                                                                                
VAL_ACT  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_ACT*'                                                      
                                                                                
         LA    RE,RQP_PER          ensure printable                             
         LHI   RF,L'RQP_PER                                                     
                                                                                
VACT02   CLI   0(RE),SPACEQ                                                     
         JNL   VACT04                                                           
         MVI   0(RE),SPACEQ                                                     
                                                                                
VACT04   AHI   RE,1                                                             
         JCT   RF,VACT02                                                        
                                                                                
         LA    RE,RQP_OFF          ensure printable                             
         LHI   RF,L'RQP_OFF                                                     
                                                                                
VACT06   CLI   0(RE),SPACEQ                                                     
         JNL   VACT08                                                           
         MVI   0(RE),SPACEQ                                                     
                                                                                
VACT08   AHI   RE,1                                                             
         JCT   RF,VACT06                                                        
                                                                                
         LA    RE,RQP_DEP          ensure printable                             
         LHI   RF,L'RQP_DEP                                                     
                                                                                
VACT10   CLI   0(RE),SPACEQ                                                     
         JNL   VACT12                                                           
         MVI   0(RE),SPACEQ                                                     
                                                                                
VACT12   AHI   RE,1                                                             
         JCT   RF,VACT10                                                        
                                                                                
         LA    RE,RQP_SUB          ensure printable                             
         LHI   RF,L'RQP_SUB                                                     
                                                                                
VACT14   CLI   0(RE),SPACEQ                                                     
         JNL   VACT16                                                           
         MVI   0(RE),SPACEQ                                                     
                                                                                
VACT16   AHI   RE,1                                                             
         JCT   RF,VACT14                                                        
                                                                                
         CLI   RQP_ACT,RQP_ADQ     Add a new person                             
         JE    VACT18                                                           
         CLI   RQP_ACT,RQP_CHQ     Change a person                              
         JE    VACT18                                                           
                                                                                
         LHI   RF,AE$INACT                                                      
         LHI   R0,RQP_ACTQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
VACT18   OC    CCTPID,CCTPID                                                    
         JNZ   VACT22                                                           
                                                                                
VACT20   LHI   RF,AE$NCPID                                                      
         LHI   R0,RQP_ACTQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
VACT22   MVC   TEMP2(2),CCTPID                                                  
         GOTOR (#GETPID,AGETPID)                                                
         JNE   VACT20                                                           
                                                                                
         MVC   RV_CCTPC,TEMP2                                                   
                                                                                
         ICM   RF,B'1111',AFILQ    get maximum record length from HELEN         
         GOTOX VHELLO,DMCB,(RF)                                                 
         L     RE,0(R1)                                                         
                                                                                
         USING HELEND,RE                                                        
VACT24   CLI   HELFLEN,FF                                                       
         JE    *+2                 (die if not in table)                        
                                                                                
         LLC   RF,HELFLEN                                                       
         BASR  R1,0                                                             
         EX    RF,8(R1)                                                         
         JE    VACT26                                                           
         CLC   HELNAME(0),ACCMST                                                
         AHI   RE,L'HELENL                                                      
         J     VACT24                                                           
                                                                                
VACT26   MVC   RV_MAXAR,HELMSIZE   save maximal acc record size                 
         DROP  RE                                                               
                                                                                
         USING CPYVALS,RE                                                       
         LA    RE,RV_CPYV                                                       
         MVC   CPYSTA1,SCPYEL+CPYSTAT1-CPYELD                                   
         MVC   CPYSTA2,SCPYEL+CPYSTAT2-CPYELD                                   
         MVC   CPYSTA3,SCPYEL+CPYSTAT3-CPYELD                                   
         MVC   CPYSTA4,SCPYEL+CPYSTAT4-CPYELD                                   
         MVC   CPYSTA5,SCPYEL+CPYSTAT5-CPYELD                                   
         MVC   CPYSTA6,SCPYEL+CPYSTAT6-CPYELD                                   
         MVC   CPYSTA7,SCPYEL+CPYSTAT7-CPYELD                                   
         MVC   CPYSTA9,SCPYEL+CPYSTAT9-CPYELD                                   
         MVC   CPYSTAC,SCPYEL+CPYSTATC-CPYELD                                   
         MVC   CPYLANG,CULANG                                                   
         MVC   CPYCTRY,CUCTRY                                                   
         DROP  RE                                                               
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Acc Person Upload: validate achange type                            *         
***********************************************************************         
                                                                                
VAL_CHA  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_CHA*'                                                      
                                                                                
         CLC   RQP_FNA,SPACES                                                   
         JH    VCHA02                                                           
         CLC   RQP_LNA,SPACES                                                   
         JNH   VCHA04                                                           
                                                                                
VCHA02   OI    RV_CHIND,RV_CHNCQ                                                
                                                                                
VCHA04   OC    RQP_TRM,RQP_TRM                                                  
         JZ    VCHA06                                                           
                                                                                
         OI    RV_CHIND,RV_CHPTQ                                                
                                                                                
VCHA06   DS    0H                  no other fields cross checked                
                                                                                
         CLI   RV_CHIND,0          any change found?                            
         JNE   VCHA08                                                           
         LHI   RF,AE$NOCHA                                                      
         LHI   R0,RQP_ACTQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
VCHA08   DS    0H                                                               
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate for history data                        *         
***********************************************************************         
                                                                                
VAL_HIS  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_HIS*'                                                      
                                                                                
         USING PHIRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ                                                 
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CUXCPY                                                   
         MVC   PHIKOFC,RQP_OFF                                                  
         MVC   PHIKDPT,RQP_DEP                                                  
         MVC   PHIKSBD,RQP_SUB                                                  
         MVC   PHIKPER,RV_PIDC                                                  
         XC    PHIKMOA,PHIKMOA                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
         CLC   PHIKEY(PHIKMOA-PHIKEY),IOKEYSAV                                  
         JNE   VHIS02                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'AC_HISR),AC_HISR                                       
         LHI   RF,AE$RECAE                                                      
         LHI   R0,RQP_HIRQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         OI    RV_ERIND,RV_ERHSE                                                
                                                                                
VHIS02   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate hourly rate override                    *         
***********************************************************************         
                                                                                
VAL_HRO  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_HRO*'                                                      
                                                                                
         OC    RQP_HRO,RQP_HRO                                                  
         JZ    VHRO10                                                           
         CP    RQP_HRO,PZERO                                                    
         JE    VHRO10                                                           
                                                                                
         ZAP   RV_HRO,RQP_HRO      set for use                                  
                                                                                
VHRO10   DS    0H                                                               
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: add history rates for person/year                *         
***********************************************************************         
                                                                                
ADD_HIS  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_HIS*'                                                      
                                                                                
         MVC   HALF1,RQP_HIR       determine start and end mos                  
         MVC   HALF2,RQP_HIR                                                    
         MVI   HALF2+1,X'12'                                                    
                                                                                
         USING PHIRECD,R2                                                       
ADD_HIS2 L     R2,AIO1                                                          
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ                                                 
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CUXCPY                                                   
         MVC   PHIKOFC,RQP_OFF                                                  
         MVC   PHIKDPT,RQP_DEP                                                  
         MVC   PHIKSBD,RQP_SUB                                                  
         MVC   PHIKPER,RV_PIDC                                                  
         XR    R1,R1                                                            
         ICM   R1,B'0011',HALF1                                                 
         LNR   R1,R1                                                            
         STCM  R1,B'0011',PHIKMOA                                               
         MVI   PHIKSEQ,0                                                        
                                                                                
         XC    PHIRSTA,PHIRSTA                                                  
         XC    PHIRLNK,PHIRLNK                                                  
                                                                                
         USING PDEELD,R3                                                        
         LA    R3,PHIRFST                                                       
         XC    PDEEL(PDELNQ+1),PDEEL                                            
         MVI   PDEEL,PDEELQ                                                     
         MVI   PDELN,PDELNQ                                                     
         MVC   PDEDTE(2),HALF1                                                  
         MVI   PDEDTE+2,X'01'                                                   
         MVC   PDENUM,RV_DPCD                                                   
         ZAP   PDEAMT,PONE                                                      
         OC    RV_HRO,RV_HRO                                                    
         JZ    ADD_HIS4                                                         
         ZAP   PDEAMT,RV_HRO                                                    
                                                                                
ADD_HIS4 ZAP   PDEADJ,PZERO                                                     
         MVI   PDESTAT2,PDESHRTE                                                
                                                                                
         LHI   RF,PHIRFST-PHIRECD                                               
         AHI   RF,PDELNQ+1                                                      
         STCM  RF,B'0011',PHIRLEN                                               
                                                                                
         GOTOR ADD_DDA                                                          
                                                                                
         GOTOR ADD_REC                                                          
         GOTOR SAVRET,RT_HSQ                                                    
                                                                                
         CLC   HALF1,HALF2         all done?                                    
         JE    ADD_HIS8                                                         
         LLC   R1,HALF1+1                                                       
         AHI   R1,1                                                             
         CHI   R1,X'0A'            (Sep to Oct format flip)                     
         JNE   ADD_HIS6                                                         
         LHI   R1,X'10'                                                         
                                                                                
ADD_HIS6 STC   R1,HALF1+1                                                       
         J     ADD_HIS2                                                         
                                                                                
ADD_HIS8 DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: get RV_1ROCM mapping from 1R office              *         
***********************************************************************         
                                                                                
GET_MAP  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**G_MAP*'                                                      
                                                                                
GM_1ROFQ EQU   X'00'                                                            
         CLC   CUAALF,=C'A1'       GroupM                                       
         JE    GMAP00                                                           
         CLC   CUAALF,=C'T1'       GroupM test file                             
         JNE   EXITY                                                            
                                                                                
GMAP00   LTR   R1,R1               called from TST_1RS                          
         JZ    GMAP02                                                           
                                                                                
         MVC   RV_1ROCM,0(R1)      ... default until data set up ...            
         J     GMAP14              ... if so enable and check below ...         
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(L'IS_1R),IS_1R                                           
         MVC   ACTKACT(2),0(R1)                                                 
         J     GMAP04                                                           
                                                                                
GMAP02   CLC   RQP_SXC,SPACES      not required if account override set         
         JH    GMAP14                                                           
                                                                                
         MVC   RV_1ROCM,RQP_OFF    ... default until data set up ...            
         J     GMAP14              ... if so enable and check below ...         
                                                                                
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         LLC   R1,RV_1RL1                                                       
         AHI   R1,L'ACTKUNT+L'ACTKLDG-1                                         
         MVC   ACTKULA(0),RV_ULA1R                                              
         EX    R1,*-6                                                           
                                                                                
GMAP04   GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   GMAP12                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
         USING FFTELD,R3                                                        
                                                                                
GMAP06   CLI   FFTEL,FFTELQ                                                     
         JE    GMAP10                                                           
         CLI   FFTEL,0                                                          
         JE    GMAP12                                                           
                                                                                
GMAP08   LLC   R1,FFTLN                                                         
         AR    R3,R1                                                            
         J     GMAP06                                                           
                                                                                
GMAP10   CLI   FFTTYPE,FFTTEPTR                                                 
         JNE   GMAP08                                                           
         CLI   FFTSEQ,GM_1ROFQ     GroupM 1R office mapping rule                
         JNE   GMAP08                                                           
         CLC   FFTDLEN,RV_SXL1     GroupM must match SX level 1 length          
         JNE   GMAP12                                                           
         MVC   RV_1ROCM,FFTDATA                                                 
         J     GMAP14                                                           
                                                                                
GMAP12   LHI   RF,AE$OFEQV                                                      
         LHI   R0,RQP_OFFQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    RV_ERIN2,RV_ERMSX                                                
                                                                                
GMAP14   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: retrive 'add account as draft flag'              *         
***********************************************************************         
                                                                                
GET_AAD  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**G_AAD*'                                                      
                                                                                
         LR    R4,R1                                                            
         MVI   0(R4),NOQ                                                        
                                                                                
         USING LDGRECD,R2                                                       
         L     R2,AIO1                                                          
         USING LDGELD,R3                                                        
         LA    R3,LDGRFST                                                       
                                                                                
GAAD02   CLI   LDGEL,LDGELQ                                                     
         JE    GAAD04                                                           
         CLI   LDGEL,0                                                          
         JE    GAAD10                                                           
                                                                                
         LLC   R1,LDGLN                                                         
         AR    R3,R1                                                            
         J     GAAD02                                                           
                                                                                
GAAD04   MVC   1(1,R4),LDGSTAT2                                                 
         MVC   2(1,R4),LDGDPOS                                                  
         MVC   3(1,R4),LDGDLEN                                                  
         TM    LDGSTAT2,LDGSDRFT                                                
         JZ    GAAD10                                                           
         MVI   0(R4),YESQ                                                       
                                                                                
GAAD10   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: get default pay code from 1R ledger              *         
***********************************************************************         
                                                                                
GET_DPC  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**G_DPC*'                                                      
                                                                                
         USING LDGRECD,R2                                                       
         L     R2,AIO1                                                          
         USING LDGELD,R3                                                        
         LA    R3,LDGRFST                                                       
                                                                                
GDPC02   CLI   LDGEL,LDGELQ                                                     
         JE    GDPC04                                                           
         CLI   LDGEL,0                                                          
         JE    GDPC06                                                           
                                                                                
         LLC   R1,LDGLN                                                         
         AR    R3,R1                                                            
         J     GDPC02                                                           
                                                                                
GDPC04   CLI   LDGLN,LDGLNXQ                                                    
         JL    GDPC06                                                           
         CLI   LDGDEFPC,0                                                       
         JE    GDPC06                                                           
         MVC   RV_DPCD,LDGDEFPC                                                 
                                                                                
GDPC06   CLI   RV_DPCD,0           if nothing found read for first              
         JH    GDPC20              payroll on file with hourly rates            
                                                                                
         USING PAYRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    PAYKEY,PAYKEY                                                    
         MVI   PAYKTYP,PAYKTYPQ                                                 
         MVI   PAYKSUB,PAYKSUBQ                                                 
         MVC   PAYKCPY,CUXCPY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     GDPC10                                                           
                                                                                
GDPC08   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
                                                                                
GDPC10   JNE   *+2                 (I/O error)                                  
         CLC   PAYKEY(PAYKSEQ-PAYKEY),IOKEYSAV                                  
         JNE   GDPC20                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         L     R3,AIO1                                                          
         AHI   R3,PAYRFST-PAYRECD                                               
         USING PAYELD,R3                                                        
                                                                                
GDPC12   CLI   PAYEL,PAYELQ                                                     
         JE    GDPC14                                                           
         CLI   PAYEL,0                                                          
         JE    GDPC08                                                           
         LLC   R1,PAYLN                                                         
         AR    R3,R1                                                            
         J     GDPC12                                                           
                                                                                
GDPC14   TM    PAYSTAT,PAYSHRTE                                                 
         JZ    GDPC08                                                           
         TM    PAYSTAT,PAYADJRT                                                 
         JNZ   GDPC08                                                           
         MVC   RV_DPCD,PAYNUM      take it                                      
                                                                                
GDPC20   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: update cost person record                        *         
***********************************************************************         
                                                                                
UPD_PER  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**U_PER*'                                                      
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,RV_PIDC                                                 
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         MVC   RV_DA,PERKDA                                                     
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R0,AIO2             Copy record to IO2                           
         L     RE,AIO1                                                          
         LA    R1,IODDWQ                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
*** delete passives via PADDLE here - not required                              
                                                                                
         L     R2,AIO1                                                          
         USING NAMELD,R3                                                        
         LA    R3,PERRFST                                                       
         MVI   BYTE1,NOQ                                                        
                                                                                
UPER02   CLI   NAMEL,NAMELQ        find and adjust elements                     
         JE    UPER06                                                           
         CLI   NAMEL,ACTVELQ                                                    
         JE    UPER08                                                           
         CLI   NAMEL,GPNELQ                                                     
         JE    UPER10                                                           
         CLI   NAMEL,EMPELQ                                                     
         JE    UPER20                                                           
         CLI   NAMEL,LOCELQ                                                     
         JE    UPER30                                                           
         CLI   NAMEL,0                                                          
         JE    UPER50                                                           
                                                                                
UPER04   LLC   R1,NAMLN                                                         
         AR    R3,R1                                                            
         J     UPER02                                                           
                                                                                
UPER06   TM    RV_CHIND,RV_CHNCQ   name change only                             
         JZ    UPER04                                                           
         MVI   NAMEL,FF                                                         
         MVI   BYTE1,YESQ                                                       
         J     UPER04                                                           
                                                                                
         USING ACTVD,R3                                                         
UPER08   GOTO1 VDATCON,DMCB,(1,TODAYP),(3,ACTVCHDT)                             
         MVC   ACTVCHID,CCTPID                                                  
         MVI   ACTVCHFL,ACTVPPQ                                                 
         AI    ACTVCHNM,1                                                       
         J     UPER04                                                           
                                                                                
         USING GPNELD,R3                                                        
UPER10   TM    RV_CHIND,RV_CHNCQ   name change only                             
         JZ    UPER04                                                           
         CLI   GPNTYP,GPNTLST                                                   
         JE    UPER12                                                           
         CLI   GPNTYP,GPNTFST                                                   
         JNE   UPER04                                                           
                                                                                
UPER12   MVI   GPNEL,FF                                                         
         MVI   BYTE1,YESQ                                                       
         J     UPER04                                                           
                                                                                
         USING EMPELD,R3                                                        
UPER20   TM    RV_CHIND,RV_CHPTQ   termination only                             
         JZ    UPER04                                                           
         MVC   EMPTRM,RQP_TRM      set                                          
         MVI   EMPCSTAT,EMPCTRM                                                 
         J     UPER04                                                           
                                                                                
         USING LOCELD,R3                                                        
UPER30   TM    RV_CHIND,RV_CHPTQ   termination only                             
         JZ    UPER04                                                           
         OC    LOCEND,LOCEND                                                    
         JNZ   UPER04                                                           
         MVC   LOCEND,RQP_TRM                                                   
         MVI   LOCSTAT,LOCSTRM                                                  
         J     UPER04                                                           
                                                                                
UPER50   CLI   BYTE1,YESQ          delete elements                              
         JNE   UPER52                                                           
                                                                                
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('FF',PERRECD),0                       
                                                                                
UPER52   TM    RV_CHIND,RV_CHNCQ   name change only                             
         JZ    UPER60                                                           
                                                                                
         GOTOR ADD_GPN                                                          
                                                                                
         USING NAMELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   NAMEL,NAMELQ                                                     
         LLC   R1,RV_NAML                                                       
         SHI   R1,1                                                             
         MVC   NAMEREC(0),RV_NAME                                               
         EX    R1,*-6                                                           
         AHI   R1,NAMEREC-NAMELD+1                                              
         STC   R1,NAMLN                                                         
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),PERRECD,NAMELD,0,0                     
         CLI   12(R1),0                                                         
         JNE   *+2                 die - can't add element                      
                                                                                
UPER60   GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JNE   *+2                                                              
                                                                                
         DS    0H                  no directory write required                  
                                                                                
*** add passives via PADDLE here - not required                                 
                                                                                
UPER70   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: update active 1R account if present              *         
***********************************************************************         
                                                                                
UPD_A1R  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**U_A1R*'                                                      
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,RV_ULA1R                                                 
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         MVC   RV_DA,ACTKDA                                                     
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
*** delete passives via PADDLE here - not required                              
                                                                                
         L     R2,AIO1                                                          
         USING EMPELD,R3                                                        
         LA    R3,ACTRFST                                                       
                                                                                
UA1R02   CLI   EMPEL,EMPELQ        find and adjust elements                     
         JE    UA1R06                                                           
         CLI   EMPEL,ACTVELQ                                                    
         JE    UA1R08                                                           
         CLI   EMPEL,0                                                          
         JE    UA1R20                                                           
                                                                                
UA1R04   LLC   R1,EMPLN                                                         
         AR    R3,R1                                                            
         J     UA1R02                                                           
                                                                                
UA1R06   MVC   EMPTRM,RQP_TRM      set termination                              
         MVI   EMPCSTAT,EMPCTRM                                                 
         J     UA1R04                                                           
                                                                                
         USING ACTVD,R3                                                         
UA1R08   GOTO1 VDATCON,DMCB,(1,TODAYP),(3,ACTVCHDT)                             
         MVC   ACTVCHID,CCTPID                                                  
         MVI   ACTVCHFL,ACTVPPQ                                                 
         AI    ACTVCHNM,1                                                       
         J     UA1R04                                                           
                                                                                
UA1R20   GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JNE   *+2                                                              
                                                                                
         GOTOR SAVRET,RT_1RQ                                                    
                                                                                
         DS    0H                  no directory write required                  
                                                                                
*** add passives via PADDLE here - not required                                 
                                                                                
UA1R30   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: update or add approver records                   *         
***********************************************************************         
                                                                                
UPD_APS  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**U_APS*'                                                      
                                                                                
         MVI   BYTE1,0                                                          
         MVI   BYTE2,0                                                          
                                                                                
         OC    RQP_TAN,RQP_TAN     timesheet approver requested?                
         JZ    UAPS40                                                           
                                                                                
         TM    RV_APIND,RV_APTSK   skip APPRECD change as already ok            
         JNZ   UAPS40                                                           
                                                                                
         TM    RV_APIND,RV_APTOF   change APPRECD if on file                    
         JNZ   UAPS10                                                           
                                                                                
         USING APPRECD,R2                                                       
         L     R2,AIO1             build key                                    
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,CUXCPY                                                   
         MVC   APPKPIDB,RQP_TAN                                                 
         XC    RV_DA,RV_DA                                                      
                                                                                
         TM    RV_APIND,RV_APTID   read for it if deleted                       
         JZ    UAPS02                                                           
                                                                                
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'APPKEY),APPKEY                                           
                                                                                
         L     R1,=AL4(IORDUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,IOEDEL        must be deleted still                        
         JNE   *+2                                                              
                                                                                
         MVC   RV_DA,IOKEY+APPKDA-APPRECD                                       
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
UAPS02   XC    APPRSTA,APPRSTA     (re)build the record                         
         XC    APPRLNK,APPRLNK                                                  
         LHI   RF,APPRFST-APPRECD                                               
         STCM  RF,B'0011',APPRLEN                                               
                                                                                
         USING LIDELD,R3                                                        
         LA    R3,APPRFST                                                       
         XC    0(255,R3),0(R3)                                                  
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDTYPE,LIDTAP1R                                                 
         MVI   LIDITLN,LID1RLNQ                                                 
         MVI   LIDAPDTY,LIDAPDTI                                                
                                                                                
         TM    RV_APIND,RV_APTEX   expense approver, too?                       
         JZ    UAPS04                                                           
         TM    RV_APIND,RV_APTSK   but not if to skip|                          
         JNZ   UAPS04                                                           
         OI    LIDAPDTY,LIDAPDEX   set it                                       
                                                                                
UAPS04   MVC   LIDAPACC,RV_ULA1R+2                                              
         ZAP   LIDAPEXV,PZERO                                                   
         LHI   RF,LID1RLNQ                                                      
         AHI   RF,LIDDATA-LIDELD                                                
         STC   RF,LIDLN                                                         
         AR    R3,RF                                                            
         MVI   LIDEL,0                                                          
         ICM   RE,B'0011',APPRLEN                                               
         AR    RF,RE                                                            
         AHI   RF,1                                                             
         STCM  RF,B'0011',APPRLEN  set new length                               
                                                                                
         GOTOR ADD_RAC                                                          
                                                                                
         CLI   BYTE1,ADDITQ        add sequential?                              
         JE    UAPS06                                                           
         TM    RV_APIND,RV_APTID                                                
         JNZ   UAPS08                                                           
                                                                                
UAPS06   GOTOR ADD_REC             add the record incl. passives                
                                                                                
         GOTOR SAVRET,RT_TAQ                                                    
         TM    RV_APIND,RV_APTEX   expense approver, too?                       
         JZ    UAPS40                                                           
         TM    RV_APIND,RV_APTSK   but not if to skip|                          
         JNZ   UAPS40                                                           
         GOTOR SAVRET,RT_XAQ                                                    
         J     UAPS40                                                           
                                                                                
UAPS08   GOTOR PUT_REC,'PR_PASQ+PR_NOWQ'                                        
                                                                                
         GOTOR SAVRET,RT_TAQ                                                    
         TM    RV_APIND,RV_APTEX   expense approver, too?                       
         JZ    UAPS40                                                           
         TM    RV_APIND,RV_APTSK   but not if to skip|                          
         JNZ   UAPS40                                                           
         GOTOR SAVRET,RT_XAQ                                                    
         J     UAPS40              put the record incl. passives                
                                                                                
         USING APPRECD,R2                                                       
UAPS10   LA    R2,IOKEY            build key and get record                     
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,CUXCPY                                                   
         MVC   APPKPIDB,RQP_TAN                                                 
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         MVC   RV_DA,APPKDA                                                     
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
UAPS12   L     R2,AIO1                                                          
                                                                                
         XR    R1,R1               does LIDEL fit onto record?                  
         ICM   R1,B'0011',APPRLEN                                               
         AHI   R1,LID1RLNQ+LIDDATA-LIDELD+1                                     
         CLM   R1,B'0011',RV_MAXAR                                              
         JNH   UAPS22                                                           
                                                                                
         GOTOR UPD_RAC             update RACEL                                 
                                                                                
         GOTOR PUT_REC,'PR_NOWQ'   put main record back for RACEL               
                                                                                
UAPS14   LA    R2,IOKEY                                                         
         MVC   BYTE2,APPKSEQ                                                    
                                                                                
         L     R1,=AL4(IOSQUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,0                                                          
         JE    UAPS16                                                           
         MVI   BYTE1,ISDELQ                                                     
         CLI   IOERR,IOEDEL                                                     
         JNE   *+2                 (die on any other error)                     
                                                                                
ISDELQ   EQU   C'D'                                                             
ADDITQ   EQU   C'A'                                                             
                                                                                
UAPS16   CLC   APPKEY(APPKSEQ-APPRECD),IOKEYSAV                                 
         JE    UAPS18                                                           
                                                                                
         MVI   BYTE1,ADDITQ                                                     
         LLC   RE,BYTE2                                                         
         AHI   RE,1                                                             
         STC   RE,BYTE2                                                         
                                                                                
         L     R2,AIO1             rebuild key for add sequential               
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,CUXCPY                                                   
         MVC   APPKPIDB,RQP_TAN                                                 
         MVC   APPKSEQ,BYTE2                                                    
                                                                                
         J     UAPS02                                                           
                                                                                
UAPS18   MVC   RV_DA,APPKDA                                                     
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R2,AIO1                                                          
                                                                                
         CLI   BYTE1,ISDELQ        if deleted rebuild record first              
         JNE   UAPS20                                                           
                                                                                
         XC    APPRSTA,APPRSTA                                                  
         XC    APPRLNK,APPRLNK                                                  
         LHI   RF,APPRFST-APPRECD+2                                             
         STCM  RF,B'0011',APPRLEN                                               
         LA    R3,APPRFST                                                       
         XC    0(2,R3),0(R3)       clear all elements                           
         J     UAPS22                                                           
                                                                                
UAPS20   XR    R1,R1               does LIDEL fit onto record?                  
         ICM   R1,B'0011',APPRLEN                                               
         AHI   R1,LID1RLNQ+LIDDATA-LIDELD+1                                     
         CLM   R1,B'0011',RV_MAXAR                                              
         JH    UAPS14              try next one                                 
                                                                                
         USING LIDELD,R3                                                        
UAPS22   LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDTYPE,LIDTAP1R                                                 
         MVI   LIDITLN,LID1RLNQ                                                 
         MVI   LIDAPDTY,LIDAPDTI                                                
                                                                                
         TM    RV_APIND,RV_APTEX   expense approver, too?                       
         JZ    UAPS24                                                           
         TM    RV_APIND,RV_APTSK   but not if to skip|                          
         JNZ   UAPS24                                                           
         OI    LIDAPDTY,LIDAPDEX   set it                                       
                                                                                
UAPS24   MVC   LIDAPACC,RV_ULA1R+2                                              
         ZAP   LIDAPEXV,PZERO                                                   
         LHI   RF,LID1RLNQ                                                      
         AHI   RF,LIDDATA-LIDELD                                                
         STC   RF,LIDLN                                                         
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),APPRECD,LIDELD,0,0                     
         CLI   12(R1),0                                                         
         JNE   *+2                 die - can't add element                      
         DROP  R3                                                               
                                                                                
         LHI   R1,PR_NOWQ                                                       
         CLI   BYTE1,ISDELQ        if deleted need to write directory           
         JNE   UAPS26                                                           
         XR    R1,R1                                                            
                                                                                
         GOTOR UPD_RAC                                                          
                                                                                
UAPS26   GOTOR PUT_REC                                                          
                                                                                
         GOTOR ADD_DPA,DMCB,('DPAPATIM',APPRECD),RV_ULA1R                       
                                                                                
         GOTOR SAVRET,RT_TAQ                                                    
         TM    RV_APIND,RV_APTEX   expense approver, too?                       
         JZ    UAPS40                                                           
         TM    RV_APIND,RV_APTSK   but not if to skip|                          
         JNZ   UAPS40                                                           
         GOTOR SAVRET,RT_XAQ                                                    
                                                                                
         GOTOR ADD_DPA,DMCB,('DPAPAEXP',APPRECD),RV_ULA1R                       
                                                                                
UAPS40   TM    RV_APIND,RV_APTEX   if expense = t/s approver: done              
         JNZ   UAPS90                                                           
                                                                                
         OC    RQP_XAN,RQP_XAN     expense approver requested?                  
         JZ    UAPS90                                                           
                                                                                
         TM    RV_APIND,RV_APXSK   skip APPRECD change as already ok            
         JNZ   UAPS90                                                           
                                                                                
         MVI   BYTE1,0                                                          
         MVI   BYTE2,0                                                          
                                                                                
         TM    RV_APIND,RV_APXOF   change APPRECD if on file                    
         JNZ   UAPS50                                                           
                                                                                
         USING APPRECD,R2                                                       
         L     R2,AIO1             build key                                    
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,CUXCPY                                                   
         MVC   APPKPIDB,RQP_XAN                                                 
         XC    RV_DA,RV_DA                                                      
                                                                                
         TM    RV_APIND,RV_APXID   read for it if deleted                       
         JZ    UAPS42                                                           
                                                                                
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'APPKEY),APPKEY                                           
                                                                                
         L     R1,=AL4(IORDUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,IOEDEL        must be deleted still                        
         JNE   *+2                                                              
                                                                                
         MVC   RV_DA,IOKEY+APPKDA-APPRECD                                       
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
UAPS42   XC    APPRSTA,APPRSTA     (re)build the record                         
         XC    APPRLNK,APPRLNK                                                  
         LHI   RF,APPRFST-APPRECD                                               
         STCM  RF,B'0011',APPRLEN                                               
                                                                                
         USING LIDELD,R3                                                        
         LA    R3,APPRFST                                                       
         XC    0(255,R3),0(R3)                                                  
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDTYPE,LIDTAP1R                                                 
         MVI   LIDITLN,LID1RLNQ                                                 
         MVI   LIDAPDTY,LIDAPDEX                                                
         MVC   LIDAPACC,RV_ULA1R+2                                              
         ZAP   LIDAPEXV,PZERO                                                   
         LHI   RF,LID1RLNQ                                                      
         AHI   RF,LIDDATA-LIDELD                                                
         STC   RF,LIDLN                                                         
         AR    R3,RF                                                            
         MVI   LIDEL,0                                                          
         ICM   RE,B'0011',APPRLEN                                               
         AR    RF,RE                                                            
         AHI   RF,1                                                             
         STCM  RF,B'0011',APPRLEN  set new length                               
                                                                                
         GOTOR ADD_RAC                                                          
                                                                                
         TM    RV_APIND,RV_APXID                                                
         JNZ   UAPS46                                                           
                                                                                
         GOTOR ADD_REC             add the record incl. passives                
                                                                                
         GOTOR SAVRET,RT_XAQ                                                    
         J     UAPS90                                                           
                                                                                
UAPS46   GOTOR PUT_REC,'PR_PASQ+PR_NOWQ'                                        
                                                                                
         GOTOR SAVRET,RT_XAQ                                                    
         J     UAPS90              put the record incl. passives                
                                                                                
         USING APPRECD,R2                                                       
UAPS50   LA    R2,IOKEY            build key and get record                     
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,CUXCPY                                                   
         MVC   APPKPIDB,RQP_XAN                                                 
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         MVC   RV_DA,APPKDA                                                     
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
UAPS56   L     R2,AIO1                                                          
                                                                                
         TM    RV_APIND,RV_APXUT   use existing entry?                          
         JZ    UAPS58                                                           
         CLI   RV_APSEQ,0                                                       
         JNE   UAPS60                                                           
         J     UAPS74                                                           
                                                                                
UAPS58   XR    R1,R1               does LIDEL fit onto record?                  
         ICM   R1,B'0011',APPRLEN                                               
         AHI   R1,LID1RLNQ+LIDDATA-LIDELD+1                                     
         CLM   R1,B'0011',RV_MAXAR                                              
         JNH   UAPS72                                                           
                                                                                
UAPS60   GOTOR UPD_RAC             update RACEL                                 
                                                                                
         GOTOR PUT_REC,'PR_NOWQ'   put main record back for RACEL               
                                                                                
UAPS62   LA    R2,IOKEY                                                         
         L     R1,=AL4(IOSQUPD+IODIR+IO1)                                       
                                                                                
         TM    RV_APIND,RV_APXUT   use existing entry?                          
         JZ    UAPS64                                                           
         MVC   APPKSEQ,RV_APSEQ    get sequence                                 
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
                                                                                
UAPS64   MVC   BYTE2,APPKSEQ                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,0                                                          
         JE    UAPS66                                                           
         TM    RV_APIND,RV_APXUT   use existing entry?                          
         JNZ   *+2                 (must be ok to read)                         
         MVI   BYTE1,ISDELQ                                                     
         CLI   IOERR,IOEDEL                                                     
         JNE   *+2                 (die on any other error)                     
                                                                                
UAPS66   CLC   APPKEY(APPKSEQ-APPRECD),IOKEYSAV                                 
         JE    UAPS68                                                           
                                                                                
         MVI   BYTE1,ADDITQ                                                     
         LLC   RE,BYTE2                                                         
         AHI   RE,1                                                             
         STC   RE,BYTE2                                                         
                                                                                
         L     R2,AIO1             rebuild key for add sequential               
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,CUXCPY                                                   
         MVC   APPKPIDB,RQP_TAN                                                 
         MVC   APPKSEQ,BYTE2                                                    
                                                                                
         J     UAPS42                                                           
                                                                                
UAPS68   MVC   RV_DA,APPKDA                                                     
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R2,AIO1                                                          
                                                                                
         TM    RV_APIND,RV_APXUT   use existing entry?                          
         JNZ   UAPS74                                                           
                                                                                
         CLI   BYTE1,ISDELQ        if deleted rebuild record first              
         JNE   UAPS70                                                           
                                                                                
         XC    APPRSTA,APPRSTA                                                  
         XC    APPRLNK,APPRLNK                                                  
         LHI   RF,APPRFST-APPRECD+2                                             
         STCM  RF,B'0011',APPRLEN                                               
         LA    R3,APPRFST                                                       
         XC    0(2,R3),0(R3)       clear all elements                           
         J     UAPS72                                                           
                                                                                
UAPS70   XR    R1,R1               does LIDEL fit onto record?                  
         ICM   R1,B'0011',APPRLEN                                               
         AHI   R1,LID1RLNQ+LIDDATA-LIDELD+1                                     
         CLM   R1,B'0011',RV_MAXAR                                              
         JH    UAPS62              try next one                                 
                                                                                
         USING LIDELD,R3                                                        
UAPS72   LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDTYPE,LIDTAP1R                                                 
         MVI   LIDITLN,LID1RLNQ                                                 
         MVI   LIDAPDTY,LIDAPDTI                                                
         MVC   LIDAPACC,RV_ULA1R+2                                              
         ZAP   LIDAPEXV,PZERO                                                   
         LHI   RF,LID1RLNQ                                                      
         AHI   RF,LIDDATA-LIDELD                                                
         STC   RF,LIDLN                                                         
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),APPRECD,LIDELD,0,0                     
         CLI   12(R1),0                                                         
         JNE   *+2                 die - can't add element                      
         J     UAPS86                                                           
                                                                                
UAPS74   LA    R3,APPRFST          use existing time entry                      
         J     UAPS78                                                           
                                                                                
UAPS76   LLC   R1,LIDLN                                                         
         AR    R3,R1                                                            
                                                                                
UAPS78   CLI   LIDEL,0                                                          
         JE    *+2                 (bad VAL_ROF or UPD_APS code)                
         CLI   LIDEL,LIDELQ                                                     
         JNE   UAPS76                                                           
         CLI   LIDTYPE,LIDTAP1R                                                 
         JNE   UAPS76                                                           
         LLC   RF,LIDLN                                                         
         AR    RF,R3                                                            
                                                                                
         LA    RE,LIDDATA                                                       
TE       USING LIDAPDTY,RE                                                      
UAPS80   TM    TE.LIDAPDTY,LIDAPDTI                                             
         JZ    UAPS82                                                           
         CLC   TE.LIDAPACC,RV_ULA1R+2                                           
         JE    UAPS84                                                           
                                                                                
UAPS82   AHI   RE,LID1RLNQ                                                      
         CR    RE,RF                                                            
         JL    UAPS80                                                           
         J     *+2                 (bad VAL_ROF or UPD_APS code)                
                                                                                
UAPS84   OI    TE.LIDAPDTY,LIDAPDEX                                             
         DROP  TE                                                               
                                                                                
UAPS86   LHI   R1,PR_NOWQ                                                       
         CLI   BYTE1,ISDELQ        if deleted need to write directory           
         JNE   UAPS88                                                           
         XR    R1,R1                                                            
                                                                                
         GOTOR UPD_RAC                                                          
                                                                                
UAPS88   GOTOR PUT_REC                                                          
                                                                                
         GOTOR ADD_DPA,DMCB,('DPAPAEXP',APPRECD),RV_ULA1R                       
         GOTOR SAVRET,RT_XAQ                                                    
                                                                                
UAPS90   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: Add/update Limit List record                     *         
***********************************************************************         
                                                                                
UPD_LLS  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**U_LLS*'                                                      
                                                                                
         OC    RQP_LLN,RQP_LLN     Limit List  update requested?                
         JZ    ULLS100                                                          
                                                                                
         XC    WORK,WORK                                                        
         XC    WORK2,WORK2                                                      
         XC    RS_IND,RS_IND                                                    
         USING LLSRECD,R2                                                       
ULLS20   LA    R2,IOKEY            build key and get LimLst PID record          
         XC    LLSKEY,LLSKEY                                                    
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
         MVC   LLSKCPY,CUXCPY                                                   
         MVC   LLSKPIDB,RQP_LLN    Field 19 PID                                 
         MVC   WORK(L'IOKEY),IOKEY Save the key                                 
         L     R1,=AL4(IOHI+IODIR+IO1)                                          
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   IOKEY(LLSKSEQ-LLSKEY),WORK                                       
         JE    ULLS40                                                           
         J     ULLS100             Exit                                         
                                                                                
ULLS30   MVC   IOKEY,WORK          Restore key + read sequence                  
         LA    R2,IOKEY                                                         
         L     R1,=AL4(IORD+IODIR+IO1)                                          
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                 Die                                          
                                                                                
         L     R1,=AL4(IOSQ+IODIR+IO1)                                          
         GOTOR (#IOEXEC,AIOEXEC)                                                
                                                                                
         CLC   IOKEY(LLSKSEQ-LLSKEY),WORK                                       
         JE    ULLS40              Rec found for Fld19 PID                      
         CLI   RQP_ACT,RQP_ADQ     Rec not found and Action: Add                
         JE    ULLS100             Yes, then exit                               
         OI    RS_IND,RS_PID19     Set EOR for Fld19 PID                        
         J     ULLS50              Read Field 2 PID record                      
                                                                                
ULLS40   MVC   WORK(L'IOKEY),IOKEY Save key for seq read                        
         MVC   RV_DA,LLSKDA                                                     
         L     R1,=AL4(IOGET+IOMST+IO1)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         CLI   RQP_ACT,RQP_ADQ     Action: ADD LimLst for Fld2 PID              
         JE    ULLS95              Yes, then go and add record                  
                                                                                
         USING LLSRECD,R2                                                       
ULLS50   OC    WORK2,WORK2         Do we already have key                       
         JNZ   ULLS60              Yes, read sequence                           
         LA    R2,IOKEY            build key and get LimLst PID record          
         XC    LLSKEY,LLSKEY                                                    
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
         MVC   LLSKCPY,CUXCPY                                                   
         MVC   LLSKPIDB,RQP_PIN    Field 2 PID                                  
         MVC   WORK2(L'IOKEY),IOKEY                                             
         L     R1,=AL4(IOHIUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         J     ULLS70                                                           
                                                                                
ULLS60   MVC   IOKEY,WORK2         Restore key + read sequence                  
         LA    R2,IOKEY                                                         
         L     R1,=AL4(IORDD+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+12                                                             
         CLI   IOERR,IOEDEL        Record marked as deleted?                    
         JNE   ULLS90              yes, read next seq record                    
                                                                                
         L     R1,=AL4(IOSQUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
                                                                                
ULLS70   CLC   IOKEY(LLSKSEQ-LLSKEY),WORK2                                      
         JNE   ULLS90                                                           
         TM    RS_IND,RS_PID02     EOR Field 2  PID?                            
         JO    ULLS90              Yes, then go to add directly                 
                                                                                
         MVC   WORK2(L'IOKEY),IOKEY  Save key for seq read                      
         MVC   RV_DA,LLSKDA                                                     
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         TM    RS_IND,RS_PID19     EOR Field 19 PID?                            
         JNO   ULLS80              No, update the Field 2 PID rec               
                                                                                
         L     R2,AIO2                                                          
         OI    LLSRSTAT,LLSSDELT   update the status to deleted                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JNE   *+2                                                              
                                                                                
         LA    R2,IOKEY                                                         
         OI    LLSKSTAT,LLSSDELT   update the status to deleted                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    ULLS60               Read next seq record                        
         DC    H'0'                                                             
                                                                                
ULLS80   L     R0,AIO2             field 2 rec                                  
         L     RE,AIO1             field 19 rec                                 
         LA    RF,IODDWQ           Record length                                
         LR    R1,RF                                                            
         MVCL  R0,RE               Copy Fld19 rec to Fld2 rec                   
                                                                                
         L     R2,AIO2                                                          
         MVC   0(L'LLSKEY,R2),WORK2  Replace original key                       
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    ULLS30              Read next Fld 19 seq record                  
         DC    H'0'                Die                                          
                                                                                
ULLS90   OI    RS_IND,RS_PID02     Set EOR for Fld2  PID                        
         TM    RS_IND,RS_PID19     Is field19 record over                       
         JO    ULLS100             Yes, exit the process                        
ULLS95   L     R2,AIO1                                                          
         MVC   LLSKPIDB,RQP_PIN    Update the LimLst record for Fld2PIN         
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO1'                           
         JE    ULLS30              Read next Fld 19 seq record                  
         DC    H'0'                Die                                          
                                                                                
ULLS100  DS    0H                                                               
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Acc Person Upload: update account record                            *         
***********************************************************************         
                                                                                
UPD_ACT  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**U_ACT*'                                                      
                                                                                
         MVC   RV_ULA,0(R1)                                                     
         CLC   IS_2P,RV_ULA                                                     
         JE    UACT02                                                           
         CLC   IS_SX,RV_ULA                                                     
         JNE   *+2                                                              
                                                                                
         TM    RV_NCIND,RV_NCSXQ                                                
         JZ    UACT40                                                           
         J     UACT04                                                           
                                                                                
UACT02   TM    RV_NCIND,RV_NC2PQ                                                
         JZ    UACT40                                                           
                                                                                
         USING ACTRECD,R2                                                       
UACT04   LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,RV_ULA                                                   
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         MVC   RV_DA,ACTKDA                                                     
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         USING CPTRBLK,R4                                                       
         L     R4,AGENAXTN                                                      
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         MVC   LDGLVALN(4),RV_2PL1                                              
         MVC   LDGSTA2,RV_2PS2                                                  
         CLC   IS_2P,RV_ULA                                                     
         JE    UACT06                                                           
         MVC   LDGLVALN(4),RV_SXL1                                              
         MVC   LDGSTA2,RV_SXS2                                                  
         CLC   IS_SX,RV_ULA                                                     
         JNE   *+2                                                              
                                                                                
UACT06   MVC   CPYVALS,RV_CPYV                                                  
         GOTO1 VPADDLE,DMCB,(C'D',AIO1),(C'K',CPTRBLK),0,0,ACOMFACS             
         DROP  R4                                                               
                                                                                
         L     R2,AIO1                                                          
         USING NAMELD,R3                                                        
         LA    R3,ACTRFST                                                       
         MVI   BYTE1,NOQ                                                        
         XR    R0,R0                                                            
         XC    TEMP,TEMP                                                        
                                                                                
UACT12   CLI   NAMEL,NAMELQ        find and adjust elements                     
         JE    UACT16                                                           
         CLI   NAMEL,0                                                          
         JE    UACT20                                                           
                                                                                
UACT14   LLC   R1,NAMLN                                                         
         AR    R3,R1                                                            
         J     UACT12                                                           
                                                                                
UACT16   MVI   NAMEL,FF                                                         
         MVI   BYTE1,YESQ                                                       
         J     UACT14                                                           
                                                                                
UACT20   CLI   BYTE1,YESQ          delete elements                              
         JNE   UACT22                                                           
                                                                                
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('FF',ACTRECD),0                       
                                                                                
         USING NAMELD,R3                                                        
UACT22   LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   NAMEL,NAMELQ                                                     
         LLC   R1,RV_NAML                                                       
         SHI   R1,1                                                             
         MVC   NAMEREC(0),RV_NAME                                               
         EX    R1,*-6                                                           
         AHI   R1,NAMEREC-NAMELD+1                                              
         STC   R1,NAMLN                                                         
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ACTRECD,NAMELD,0,0                     
         CLI   12(R1),0                                                         
         JNE   *+2                 die - can't add element                      
                                                                                
         GOTOR UPD_RAC             RACTCHA element handling                     
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JNE   *+2                                                              
                                                                                
         DS    0H                  no directory write required                  
                                                                                
         USING CPTRBLK,R4                                                       
         L     R4,AGENAXTN                                                      
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         MVC   LDGLVALN(4),RV_2PL1                                              
         MVC   LDGSTA2,RV_2PS2                                                  
         CLC   IS_2P,RV_ULA                                                     
         JE    UACT30                                                           
         MVC   LDGLVALN(4),RV_SXL1                                              
         MVC   LDGSTA2,RV_SXS2                                                  
         CLC   IS_SX,RV_ULA                                                     
         JNE   *+2                                                              
                                                                                
UACT30   MVC   CPYVALS,RV_CPYV                                                  
         GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,RV_DA,0,ACOMFACS                
         DROP  R4                                                               
                                                                                
UACT40   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: set up a basic key                               *         
***********************************************************************         
                                                                                
SET_KEY  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**S_KEY*'                                                      
                                                                                
         MVC   BYTE1,0(R1)         save 'add as draft' flag                     
                                                                                
         USING ACTRECD,R2                                                       
         L     R2,AIO1             build record key                             
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,RV_ULA                                                   
                                                                                
         XC    ACTRLNK,ACTRLNK     clear linkage area and set to no             
         MVI   ACTRFST,0           element                                      
                                                                                
         XC    ACTRSTA,ACTRSTA     build status area                            
         MVI   ACTRSAF1,SPACEQ                                                  
         MVI   ACTRSAF2,SPACEQ                                                  
         MVI   ACTRSAF3,SPACEQ                                                  
         MVI   ACTRSAF4,SPACEQ                                                  
         MVI   ACTRSAF5,SPACEQ                                                  
         OI    ACTRSTAT,ACTSABLP                                                
                                                                                
         CLI   BYTE1,YESQ          'add as draft'?                              
         JNE   SKEY02                                                           
         OI    ACTRSTAT,ACTSDRFT                                                
                                                                                
         USING NAMELD,R3                                                        
SKEY02   LA    R3,ACTRFST                                                       
         MVI   NAMEL,NAMELQ                                                     
         LLC   R1,RV_NAML                                                       
         LR    RE,R1                                                            
         AHI   RE,NAMEREC-NAMELD                                                
         STC   RE,NAMLN                                                         
         AR    RE,R3                                                            
         XC    0(2,RE),0(RE)       clear EoR                                    
         SHI   R1,1                                                             
         MVC   NAMEREC(0),RV_NAME                                               
         EX    R1,*-6                                                           
                                                                                
         LLC   R1,NAMLN            set record length                            
         AHI   R1,ACTRFST-ACTRECD+1                                             
         STCM  R1,B'0011',ACTRLEN                                               
                                                                                
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate office/department/subdepartment         *         
***********************************************************************         
                                                                                
VAL_ODS  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_ODS*'                                                      
                                                                                
         CLC   RQP_OFF,SPACES                                                   
         JH    V_ODS02                                                          
         LHI   RF,AE$IOFCQ                                                      
         LHI   R0,RQP_OFFQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
V_ODS02  CLC   RQP_DEP,SPACES                                                   
         JH    V_ODS04                                                          
         LHI   RF,AE$IDEPT                                                      
         LHI   R0,RQP_DEPQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
V_ODS04  CLC   RQP_SUB,SPACES                                                   
         JH    V_ODS06                                                          
         LHI   RF,AE$ISDPT                                                      
         LHI   R0,RQP_SUBQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
         USING OFFALD,R2                                                        
V_ODS06  L     R2,AOFFAREA                                                      
         MVC   OFFAOFFC,RQP_OFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL,OFFALD                                                    
         JE    V_ODS08                                                          
         DROP  R2                                                               
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(2),RQP_OFF                                               
         LHI   RF,AE$SECLK                                                      
         LHI   R0,RQP_OFFQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         J     EXITN                                                            
                                                                                
V_ODS08  DS    0H                                                               
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate PID                                     *         
***********************************************************************         
                                                                                
VAL_PID  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_PID*'                                                      
                                                                                
         OC    RQP_PIN,RQP_PIN                                                  
         JNZ   VPID02                                                           
         LHI   RF,AE$MIPER                                                      
         LHI   R0,RQP_PINQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
VPID02   MVC   TEMP2(L'RQP_PIN),RQP_PIN                                         
         GOTOR (#GETPID,AGETPID)                                                
         JE    VPID04                                                           
         LHI   RF,AE$INPID                                                      
         LHI   R0,RQP_PINQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
VPID04   MVC   RV_SPIDC,TEMP2                                                   
         MVC   RV_PIDC,RV_SPIDC    default is security person code              
         CLC   RQP_PER,SPACES                                                   
         JNH   VPID06                                                           
         MVC   RV_PIDC,RQP_PER     but may be overridden with user data         
         J     VPID10                                                           
                                                                                
VPID06   CLI   RQP_ACT,RQP_CHQ     Get PID passive if action is change          
         JNE   VPID10                                                           
                                                                                
         USING PIDRECD,R2                                                       
         LA    R2,IOKEY            get person code via PID                      
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,RQP_PIN                                                  
         MVI   PIDKSTYP,PIDKPERQ                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
         CLC   PIDKEY(PIDKPER-PIDKEY),IOKEYSAV                                  
         JE    VPID08                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$PIDPR                                                      
         LHI   R0,RQP_PINQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
VPID08   MVC   RV_PIDC,PIDKPER                                                  
                                                                                
VPID10   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate PID code length                         *         
***********************************************************************         
                                                                                
VAL_PCL  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_PCL*'                                                      
                                                                                
         LLC   RE,RV_1RL4                                                       
         LLC   RF,RV_1RL3                                                       
         SR    RE,RF               length of 1R level 4                         
                                                                                
         MVC   TEMP2(L'RV_PIDC),RV_PIDC                                         
         MVC   TEMP2+L'RV_PIDC(L'ACTKACT),SPACES                                
                                                                                
         LA    RE,TEMP2(RE)        point to PID after 1R lvl 4                  
         CLC   0(L'ACTKACT,RE),SPACES                                           
         JNH   VPCL02              error if anything in there                   
                                                                                
         LHI   RF,AE$ACLNG                                                      
         LHI   R0,RQP_PINQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
VPCL02   DS    0H                                                               
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate t/s and exp approvers                   *         
***********************************************************************         
                                                                                
VAL_APS  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_APS*'                                                      
                                                                                
         MVI   RV_APIND,0                                                       
                                                                                
         OC    RQP_TAN,RQP_TAN                                                  
         JZ    VAPS02                                                           
         MVC   TEMP2(L'RQP_TAN),RQP_TAN                                         
         GOTOR (#GETPID,AGETPID)                                                
         JNE   VAPS01                                                           
         MVC   RQP_TAC,TEMP2                                                    
         J     VAPS06                                                           
                                                                                
VAPS01   MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TIMEQ),TIMEQ                                           
         LHI   RF,AE$INAPP                                                      
         LHI   R0,RQP_TANQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         OI    RV_ERIND,RV_ERTAQ                                                
         J     VAPS06                                                           
                                                                                
VAPS02   CLC   RQP_TAC,SPACES                                                   
         JNH   VAPS06                                                           
         MVC   TEMP2(L'RQP_TAC),RQP_TAC                                         
         GOTOR (#GETPIN,AGETPIN)                                                
         JE    VAPS04                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TIMEQ),TIMEQ                                           
         LHI   RF,AE$INAPP                                                      
         LHI   R0,RQP_TACQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         OI    RV_ERIND,RV_ERTAQ                                                
         J     VAPS06                                                           
                                                                                
VAPS04   MVC   RQP_TAN,TEMP2+50                                                 
                                                                                
VAPS06   OC    RQP_XAN,RQP_XAN                                                  
         JZ    VAPS08                                                           
         MVC   TEMP2(L'RQP_XAN),RQP_XAN                                         
         GOTOR (#GETPID,AGETPID)                                                
         JNE   VAPS07                                                           
         MVC   RQP_XAC,TEMP2                                                    
         J     VAPS12                                                           
                                                                                
VAPS07   MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'EXPENSEQ),EXPENSEQ                                     
         LHI   RF,AE$INAPP                                                      
         LHI   R0,RQP_XANQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         OI    RV_ERIND,RV_ERXAQ                                                
         J     VAPS14                                                           
                                                                                
VAPS08   CLC   RQP_XAC,SPACES                                                   
         JNH   VAPS12                                                           
         MVC   TEMP2(L'RQP_XAC),RQP_XAC                                         
         GOTOR (#GETPIN,AGETPIN)                                                
         JE    VAPS10                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TIMEQ),TIMEQ                                           
         LHI   RF,AE$INAPP                                                      
         LHI   R0,RQP_XACQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         OI    RV_ERIND,RV_ERXAQ                                                
         J     VAPS14                                                           
                                                                                
VAPS10   MVC   RQP_XAN,TEMP2+50                                                 
                                                                                
VAPS12   CLC   RQP_XAN,RQP_TAN                                                  
         JNE   VAPS14                                                           
         OI    RV_APIND,RV_APTEX                                                
                                                                                
VAPS14   DS    0H                                                               
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate approver record(s) on file              *         
***********************************************************************         
                                                                                
VAL_ROF  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_ROF*'                                                      
                                                                                
         OC    RQP_TAN,RQP_TAN     any t/s approver passed?                     
         JZ    VROF10                                                           
                                                                                
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLAPPR',SPACES),('GAPLTDTE',GAPLPARM),              +        
               ('QTIME',RQP_TAN)   No time approval entries or approver         
         JNE   VROF06              found for passed PIN                         
                                                                                
GAP      USING GAPTABD,GAPAREA                                                  
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   VROF06              end of buffer = no applicable entry          
         J     VROF04                                                           
                                                                                
VROF02   GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   VROF06              end of buffer = no applicable entry          
                                                                                
VROF04   TM    GAP.GAPTSTA,GAPTSMQ look for main entry                          
         JZ    VROF02                                                           
                                                                                
         LLC   RE,GAP.GAPTLEN      check it matches 1R account                  
         SHI   RE,1                                                             
         CLC   GAP.GAPTACT(0),RV_ULA1R+2                                        
         EX    RE,*-6                                                           
         JNE   VROF02                                                           
         OI    RV_APIND,RV_APTSK   can skip APPRECD handling for time           
         J     VROF10                                                           
                                                                                
VROF06   GOTOR TT1RVGT,RV_ULA1R+2  test against table for approved              
         JE    VROF08              elsewhere                                    
         OI    RV_APIND,RV_APTSK   can skip APPRECD handling for time           
         J     VROF10                                                           
                                                                                
VROF08   DS    0H                  can add to time approver                     
                                                                                
VROF10   OC    RQP_XAN,RQP_XAN     any expenses approver passed?                
         JZ    VROF20                                                           
                                                                                
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLAPPR',SPACES),('GAPLTDTE',GAPLPARM),              +        
               ('QEXPCLM',RQP_XAN) No time approval entries or approver         
         JNE   VROF16              found for passed PIN                         
                                                                                
GAP      USING GAPTABD,GAPAREA                                                  
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   VROF16              end of buffer = no applicable entry          
         J     VROF14                                                           
                                                                                
VROF12   GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   VROF16              end of buffer = no applicable entry          
                                                                                
VROF14   TM    GAP.GAPTSTA,GAPTSMQ look for main entry                          
         JZ    VROF12                                                           
                                                                                
         LLC   RE,GAP.GAPTLEN      check it matches 1R account                  
         SHI   RE,1                                                             
         CLC   GAP.GAPTACT(0),RV_ULA1R+2                                        
         EX    RE,*-6                                                           
         JNE   VROF12                                                           
         OI    RV_APIND,RV_APXSK   can skip APPRECD handling for exp.           
         J     VROF20                                                           
                                                                                
VROF16   GOTOR TT1RVGT,RV_ULA1R+2  test against table for approved              
         JE    VROF18              elsewhere                                    
         OI    RV_APIND,RV_APXSK   can skip APPRECD handling for exp.           
         J     VROF20                                                           
                                                                                
VROF18   DS    0H                  can add to time approver                     
                                                                                
VROF20   OC    RQP_TAN,RQP_TAN     any time approver passed?                    
         JZ    VROF24                                                           
                                                                                
         USING APPRECD,R2                                                       
         LA    R2,IOKEY            look up t/s approver                         
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,CUXCPY                                                   
         MVC   APPKPIDB,RQP_TAN                                                 
                                                                                
         L     R1,=AL4(IORDD+IODIR+IO1)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,0                                                          
         JE    VROF22                                                           
         CLI   IOERR,IOEDEL                                                     
         JNE   VROF24                                                           
         OI    RV_APIND,RV_APTID   deleted                                      
         J     VROF24                                                           
                                                                                
VROF22   OI    RV_APIND,RV_APTOF   approver on file                             
                                                                                
VROF24   OC    RQP_XAN,RQP_XAN     any exp approver passed?                     
         JZ    VROF50                                                           
                                                                                
         TM    RV_APIND,RV_APTEX   time=expense approver                        
         JZ    VROF28                                                           
                                                                                
         TM    RV_APIND,RV_APTID   deleted?                                     
         JZ    VROF26                                                           
         OI    RV_APIND,RV_APXID   deleted                                      
         J     VROF50                                                           
                                                                                
VROF26   TM    RV_APIND,RV_APTOF   record on file?                              
         JZ    VROF50                                                           
         OI    RV_APIND,RV_APXOF   on file                                      
         J     VROF32                                                           
                                                                                
         USING APPRECD,R2                                                       
VROF28   LA    R2,IOKEY            look up exp approver                         
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,CUXCPY                                                   
         MVC   APPKPIDB,RQP_XAN                                                 
                                                                                
         L     R1,=AL4(IORDD+IODIR+IO1)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,0                                                          
         JE    VROF30                                                           
         CLI   IOERR,IOEDEL                                                     
         JNE   VROF50                                                           
         OI    RV_APIND,RV_APXID   deleted                                      
         J     VROF50                                                           
                                                                                
VROF30   OI    RV_APIND,RV_APXOF   approver on file                             
         J     VROF50                                                           
                                                                                
VROF32   TM    RV_APIND,RV_APXSK   expense approver to be skipped?              
         JNZ   VROF50                                                           
         TM    RV_APIND,RV_APTSK   time approver to be skipped?                 
         JZ    VROF50              (if so will do in one go anywway)            
         MVC   CSVKEY1,APPKEY      check for 1R low level entry match           
         J     VROF36                                                           
                                                                                
VROF34   LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
         CLC   APPKEY(APPKSEQ-APPRECD),CSVKEY1                                  
         JNE   VROF50                                                           
                                                                                
VROF36   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         L     R2,AIO1                                                          
         USING LIDELD,R3                                                        
         LA    R3,APPRFST                                                       
         J     VROF40                                                           
                                                                                
VROF38   LLC   R1,LIDLN                                                         
         AR    R3,R1                                                            
                                                                                
VROF40   CLI   LIDEL,0                                                          
         JE    VROF34                                                           
         CLI   LIDEL,LIDELQ                                                     
         JNE   VROF38                                                           
         CLI   LIDTYPE,LIDTAP1R                                                 
         JNE   VROF38                                                           
         LLC   RF,LIDLN                                                         
         AR    RF,R3                                                            
                                                                                
         LA    RE,LIDDATA                                                       
TE       USING LIDAPDTY,RE                                                      
VROF42   TM    TE.LIDAPDTY,LIDAPDTI                                             
         JZ    VROF44                                                           
         CLC   TE.LIDAPACC,RV_ULA1R+2                                           
         JE    VROF46                                                           
                                                                                
VROF44   AHI   RE,LID1RLNQ                                                      
         CR    RE,RF                                                            
         JL    VROF42                                                           
         J     VROF38                                                           
                                                                                
VROF46   OI    RV_APIND,RV_APXUT   set to use existing time entry               
         MVC   RV_APSEQ,APPKSEQ                                                 
                                                                                
VROF50   DS    0H                                                               
         J     EXITY                                                            
         DROP  GAP,TE                                                           
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Test current 1R account versus GAPTAB entries                       *         
***********************************************************************         
                                                                                
         USING GAPTABD,R4                                                       
TT1RVGT  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*TT1RVGT'                                                      
                                                                                
         LR    R3,R1               pointo to account code passed                
                                                                                
         LA    R4,GAPAREA2         point to main/office entry                   
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
                                                                                
P        USING ACTKACT,R3                                                       
TT1RV08  LLC   RF,GAPTLEN                                                       
         AR    RF,R3                                                            
         CLI   0(RF),C' '          Is the account any longer than entry         
         JE    TT1RVGLY            No - so keep account                         
                                                                                
TT1RV10  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF     End of buffer                                
         JNZ   TT1RVGLY            Yes                                          
         TM    GAPTSTA,GAPTSMQ     next group/office                            
         JO    TT1RVGLY                                                         
                                                                                
TT1RV12  LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   GAPTACT(0),P.ACTKACT                                             
         EX    RF,0(RE)                                                         
         JNE   TT1RV10                                                          
         J     TT1RVGLN                                                         
                                                                                
TT1RVGLY GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF            reread GAPAREA entry on exit                 
         CR    RB,RB                                                            
         J     TT1RVGLX                                                         
                                                                                
TT1RVGLN GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         LTR   RB,RB                                                            
                                                                                
TT1RVGLX XIT1                                                                   
         DROP  P,R4                                                             
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate SX/1R/2P levels                         *         
***********************************************************************         
                                                                                
VAL_LVL  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_LVL*'                                                      
                                                                                
         LHI   R0,RQP_SXCQ                                                      
         CLC   IS_SX,RV_ULA                                                     
         JE    VLVL02                                                           
         LHI   R0,RQP_2PCQ                                                      
         CLC   IS_2P,RV_ULA                                                     
         JE    VLVL04                                                           
         LHI   R0,RQP_OFFQ                                                      
         CLC   IS_1R,RV_ULA                                                     
         JE    VLVL06                                                           
         J     *+2                 (?)                                          
                                                                                
VLVL02   LA    R4,RV_SXOP          Point to SX ledger block                     
         MVI   BYTE1,NOQ           Offpos check                                 
         J     VLVL20                                                           
                                                                                
VLVL04   LA    R4,RV_2POP          Point to 2P ledger block                     
         MVI   BYTE1,NOQ           Offpos check                                 
         J     VLVL20                                                           
                                                                                
VLVL06   LA    R4,RV_1ROP          Point to 1R ledger block                     
         MVI   BYTE1,NOQ           Offpos check                                 
                                                                                
VLVL20   CLI   BYTE1,YESQ          Offpos check?                                
         JNE   VLVL22                                                           
         CLI   0(R4),LDGOTRAN                                                   
         JE    VLVL22                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(2),RV_ULA                                                
*&&UK*&& LHI   RF,AE$MISOP                                                      
*&&US*&& LHI   RF,AE$MOPOS                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         J     EXITN                                                            
                                                                                
         USING ACTRECD,R2                                                       
VLVL22   LA    R2,IOKEY                                                         
                                                                                
         AHI   R4,RV_SXL1-RV_SXOP  Point to level 1 length                      
         LHI   R3,4                (max of 4 levels)                            
                                                                                
VLVL30   CLI   0(R4),L'ACTKACT     current level is low level - done            
         JE    VLVL90                                                           
                                                                                
         LLC   R1,0(R4)            extract current level                        
         AHI   R1,L'IS_SX-1                                                     
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(0),RV_ULA                                                
         EX    R1,*-6                                                           
         MVC   TEMP(14),ACTKULA                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VLVL34                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),TEMP                                                 
         LHI   RF,AE$HLACM                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         MVI   BYTE2,RV_ERH1R                                                   
         CLC   IS_1R,RV_ULA                                                     
         JE    VLVL32                                                           
         MVI   BYTE2,RV_ERH2P                                                   
         CLC   IS_2P,RV_ULA                                                     
         JE    VLVL32                                                           
         MVI   BYTE2,RV_ERHSX                                                   
         CLC   IS_SX,RV_ULA                                                     
         JNE   *+2                 (ledger not supported here yet)              
                                                                                
VLVL32   OC    RV_ERIN2,BYTE2                                                   
         J     VLVL90                                                           
                                                                                
VLVL34   AHI   R4,1                                                             
         JCT   R3,VLVL30                                                        
                                                                                
VLVL90   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate low level account not on file yet       *         
***********************************************************************         
                                                                                
VAL_AOF  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_AOF*'                                                      
                                                                                
         LR    R4,R1                                                            
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,RV_ULA                                                   
                                                                                
         L     R1,=AL4(IORDD+IODIR+IO1)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,0                                                          
         JE    VAOF02                                                           
         CLI   IOERR,IOEDEL                                                     
         JNE   VAOF06              (not found at all)                           
                                                                                
VAOF02   MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),RV_ULA                                               
         LHI   RF,AE$RECAE                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R4)                        
         MVI   BYTE2,RV_ERE1R                                                   
         CLC   IS_1R,RV_ULA                                                     
         JE    VAOF04                                                           
         MVI   BYTE2,RV_ERESX                                                   
         CLC   IS_SX,RV_ULA                                                     
         JE    VAOF04                                                           
         MVI   BYTE2,RV_ERE2P                                                   
         CLC   IS_2P,RV_ULA                                                     
         JNE   *+2                 (not supported ledger here yet)              
                                                                                
VAOF04   OC    RV_ERIN2,BYTE2                                                   
                                                                                
VAOF06   DS    0H                                                               
                                                                                
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: check acc cost person record not existing yet    *         
***********************************************************************         
                                                                                
VAL_CPR  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_CPR*'                                                      
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,RV_PIDC                                                 
                                                                                
         L     R1,=AL4(IORDD+IODIR+IO1)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,0                                                          
         JE    VCPR02                                                           
         CLI   IOERR,IOEDEL                                                     
         JNE   VCPR04              (not found at all)                           
                                                                                
VCPR02   MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(5),AC_COST                                               
         MVC   XERRTXT+5+1(8),RV_PIDC                                           
         LHI   RF,AE$RECAE                                                      
         LHI   R0,RQP_PINQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         OI    RV_ERIND,RV_ERPEQ                                                
         J     VCPR06                                                           
                                                                                
         USING PIDRECD,R2                                                       
VCPR04   LA    R2,IOKEY            ensure PID not used elsewhere                
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,RQP_PIN                                                  
         MVI   PIDKSTYP,PIDKPERQ                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
         CLC   PIDKEY(PIDKPER-PIDKEY),IOKEYSAV                                  
         JNE   VCPR06                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(8),PIDKPER                                               
         LHI   RF,AE$IPID                                                       
         LHI   R0,RQP_PINQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         J     EXITN                                                            
                                                                                
VCPR06   DS    0H                  on add no PIDKPER vs RV_PIDC check           
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate cost person record for termination      *         
***********************************************************************         
                                                                                
VAL_TRM  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_TRM*'                                                      
                                                                                
         OC    RQP_TRM,RQP_TRM                                                  
         JNZ   VTRM02                                                           
                                                                                
         LHI   RF,AE$MISDA                                                      
         LHI   R0,RQP_TRMQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
         USING PERRECD,R2                                                       
VTRM02   LA    R2,IOKEY            ensure cost person record exists             
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,RV_PIDC                                                 
                                                                                
         L     R1,=AL4(IORD+IODIR+IO1)                                          
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    VTRM04                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(8),RV_PIDC                                               
         LHI   RF,AE$PIDPR                                                      
         LHI   R0,RQP_TRMQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         J     EXITN                                                            
                                                                                
VTRM04   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         L     R2,AIO1                                                          
         USING EMPELD,R3                                                        
         LA    R3,PERRFST                                                       
                                                                                
VTRM06   CLI   EMPEL,0                                                          
         JE    VTRM40                                                           
         CLI   EMPEL,EMPELQ                                                     
         JE    VTRM10                                                           
         CLI   EMPEL,LOCELQ                                                     
         JE    VTRM20                                                           
                                                                                
VTRM08   LLC   R1,EMPLN                                                         
         AR    R3,R1                                                            
         J     VTRM06                                                           
                                                                                
VTRM10   OC    EMPTRM,EMPTRM       term date already set or person              
         JNZ   VTRM12              terminated?                                  
         CLI   EMPCSTAT,EMPCTRM                                                 
         JNE   VTRM14                                                           
                                                                                
VTRM12   LHI   RF,AE$PRSNT                                                      
         LHI   R0,RQP_TRMQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
VTRM14   CLC   RQP_TRM,EMPHIR      ensure term dat not before hire date         
         JNL   VTRM16                                                           
                                                                                
         LHI   RF,AE$TDBHD                                                      
         LHI   R0,RQP_TRMQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
VTRM16   DS    0H                  no further checks                            
         J     VTRM08                                                           
                                                                                
         USING LOCELD,R3                                                        
VTRM20   CLI   LOCSTAT,LOCSACT     active location for 2P/SX                    
         JNE   VTRM08                                                           
                                                                                
         MVC   TEMP(16),SPACES                                                  
         MVC   TEMP(2),IS_1R                                                    
         LA    RF,TEMP+2                                                        
                                                                                
         LLC   R1,RV_1RL1                                                       
         SHI   R1,1                                                             
         MVC   0(0,RF),LOCOFF                                                   
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
                                                                                
         LLC   RE,RV_1RL1                                                       
         LLC   R1,RV_1RL2                                                       
         SR    R1,RE                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),LOCDEPT                                                  
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
                                                                                
         LLC   RE,RV_1RL2                                                       
         LLC   R1,RV_1RL3                                                       
         SR    R1,RE                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),LOCSUB                                                   
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
                                                                                
         MVC   0(L'RV_PIDC,RF),RV_PIDC                                          
         MVC   RV_ULA1R,TEMP                                                    
                                                                                
         J     VTRM08                                                           
                                                                                
* Note: no 'payroll exists in future' checks carried out nor any                
*       future payroll history deleted                                          
                                                                                
VTRM40   CLC   RV_ULA1R,SPACES                                                  
         JNH   VTRM50                                                           
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,RV_ULA1R                                                 
                                                                                
         L     R1,=AL4(IORD+IODIR+IO1)                                          
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    VTRM50                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),RV_ULA1R                                             
         LHI   RF,AE$MIACC                                                      
         LHI   R0,RQP_PINQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         OI    RV_ERIN3,RV_ERARM                                                
                                                                                
VTRM50   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: test person on file and check name               *         
*                    Record in AIO2 for multiple locations            *         
***********************************************************************         
                                                                                
TST_PER  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**T_PER*'                                                      
                                                                                
         MVI   RV_NCIND,0                                                       
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,RV_PIDC                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    TPER02                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(5),AC_COST                                               
         MVC   XERRTXT+5+1(8),RV_PIDC                                           
         LHI   RF,AE$RECNF                                                      
         LHI   R0,RQP_PINQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         J     EXITN                                                            
                                                                                
TPER02   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JNE   *+2                                                              
                                                                                
         USING GPNELD,R3                                                        
         L     R2,AIO2                                                          
         LA    R3,PERRFST                                                       
         MVI   BYTE1,0                                                          
                                                                                
TPER04   CLI   GPNEL,0                                                          
         JE    TPER20                                                           
         CLI   GPNEL,GPNELQ                                                     
         JE    TPER08                                                           
                                                                                
TPER06   LLC   R1,GPNLN                                                         
         AR    R3,R1                                                            
         J     TPER04                                                           
                                                                                
TPER08   CLI   GPNTYP,GPNTLST                                                   
         JE    TPER12                                                           
         CLI   GPNTYP,GPNTFST                                                   
         JNE   TPER06                                                           
                                                                                
         OI    BYTE1,RV_NCFNQ                                                   
         LLC   R1,GPNLN                                                         
         SHI   R1,GPNLNQ                                                        
         LLC   RE,RV_FNLN                                                       
         CR    R1,RE                                                            
         JNE   TPER10                                                           
         LA    RF,RQP_FNA                                                       
         SHI   R1,1                                                             
         EX    R1,TPEREX                                                        
         JE    TPER06                                                           
                                                                                
TPER10   OI    RV_NCIND,RV_NCFNQ                                                
         J     TPER06                                                           
                                                                                
TPER12   OI    BYTE1,RV_NCLNQ                                                   
         LLC   R1,GPNLN                                                         
         SHI   R1,GPNLNQ                                                        
         LLC   RE,RV_LNLN                                                       
         CR    R1,RE                                                            
         JNE   TPER14                                                           
         LA    RF,RQP_LNA                                                       
         SHI   R1,1                                                             
         EX    R1,TPEREX                                                        
         JE    TPER06                                                           
                                                                                
TPER14   OI    RV_NCIND,RV_NCLNQ                                                
         J     TPER06                                                           
                                                                                
TPER20   TM    BYTE1,RV_NCLNQ+RV_NCFNQ                                          
         JO    TPER24                                                           
         TM    BYTE1,RV_NCLNQ                                                   
         JZ    TPER22                                                           
         OI    RV_NCIND,RV_NCLNQ                                                
                                                                                
TPER22   TM    BYTE1,RV_NCFNQ                                                   
         JZ    TPER24                                                           
         OI    RV_NCIND,RV_NCFNQ                                                
                                                                                
TPER24   DS    0H                                                               
         J     EXITY                                                            
                                                                                
TPEREX   CLC   GPNNME(0),0(RF)                                                  
                                                                                
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: test 1R location accounts                        *         
*                    Person record in AIO2 for multiple locations     *         
***********************************************************************         
                                                                                
TST_1RS  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**T_1RS*'                                                      
                                                                                
         XC    HALF1,HALF1                                                      
                                                                                
         MVC   RV_ULA2P,SPACES                                                  
         CLC   RQP_2PC,SPACES      code override passed?                        
         JNH   T1RS02                                                           
         MVC   RV_ULA2P(2),IS_2P                                                
         MVC   RV_ULA2P+2(L'RQP_2PC),RQP_2PC                                    
                                                                                
T1RS02   MVC   RV_ULASX,SPACES                                                  
         CLC   RQP_SXC,SPACES      code override passed?                        
         JNH   T1RS04                                                           
         MVC   RV_ULASX(2),IS_SX                                                
         MVC   RV_ULASX+2(L'RQP_SXC),RQP_SXC                                    
                                                                                
         USING PERRECD,R4                                                       
T1RS04   L     R4,AIO2                                                          
         AHI   R4,PERRFST-PERRECD                                               
         USING LOCELD,R4                                                        
                                                                                
T1RS06   CLI   LOCEL,0                                                          
         JE    T1RS60                                                           
         CLI   LOCEL,LOCELQ                                                     
         JE    T1RS10                                                           
                                                                                
T1RS08   LLC   R1,LOCLN                                                         
         AR    R4,R1                                                            
         J     T1RS06                                                           
                                                                                
T1RS10   CLI   LOCSTAT,LOCSACT     active location for 2P/SX                    
         JNE   T1RS48                                                           
                                                                                
         CLC   RV_ULASX,SPACES     resolved?                                    
         JH    T1RS20                                                           
         MVC   RV_ULASX(2),IS_SX                                                
         LA    R1,RV_ULASX+(L'ACTKUNT+L'ACTKLDG)                                
*&&US                                                                           
         CLC   CUAALF,=C'GP'       Grey NY                                      
         JE    T1RS12                                                           
*&&                                                                             
         CLI   RV_SXOP,0                                                        
         JE    T1RS14                                                           
         CLI   RV_SXOP,1                                                        
         JNE   *+2                                                              
T1RS12   LLC   RF,RV_SXL1                                                       
         BCTR  RF,0                                                             
         MVC   0(0,R1),LOCOFF                                                   
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         AR    R1,RF                                                            
                                                                                
T1RS14   CLI   RV_SXDP,0           Assume if dept position non zero             
         JE    T1RS15               it's either position 1 or after off         
         LLC   RF,RV_SXDL          get dept length                              
         BCTR  RF,0                                                             
         MVC   0(0,R1),LOCDEPT                                                  
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         AR    R1,RF                                                            
                                                                                
T1RS15   DS    0H                                                               
*&&UK                                                                           
         CLC   CUAALF,=C'A1'       GroupM                                       
         JE    T1RS16                                                           
         CLC   CUAALF,=C'T1'       GroupM test file                             
         JNE   T1RS18                                                           
T1RS16   MVC   RV_ULASX+2(L'RQP_SXC),SPACES                                     
         MVC   HALF1,LOCOFF                                                     
         MVC   RV_ULASX+2+L'RV_1ROCM(L'RV_PIDC),RV_PIDC                         
         J     T1RS20                                                           
*&&                                                                             
                                                                                
T1RS18   MVC   0(L'RV_PIDC,R1),RV_PIDC                                          
                                                                                
T1RS20   CLC   RV_ULA2P,SPACES     resolved?                                    
         JH    T1RS48                                                           
         MVC   RV_ULA2P(2),IS_2P                                                
         LA    R1,RV_ULA2P+(L'ACTKUNT+L'ACTKLDG)                                
         CLI   RV_2POP,0                                                        
         JE    T1RS22                                                           
         CLI   RV_2POP,1                                                        
         JNE   *+2                                                              
         LLC   RF,RV_2PL1                                                       
         BCTR  RF,0                                                             
         MVC   0(0,R1),LOCOFF                                                   
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         AR    R1,RF                                                            
                                                                                
T1RS22   DS    0H                                                               
*&&UK                                                                           
         LLC   RF,RV_2PL1          get dept length                              
         CLC   CUAALF,=C'A1'       GroupM                                       
         JE    T1RS23                                                           
         CLC   CUAALF,=C'T1'       GroupM test file                             
         JE    T1RS23                                                           
*&&                                                                             
         CLI   RV_2PDP,0           Assume if dept position non zero             
         JE    T1RS24               it's either position 1 or after off         
         LLC   RF,RV_2PDL          get dept length                              
T1RS23   BCTR  RF,0                                                             
         MVC   0(0,R1),LOCDEPT                                                  
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         AR    R1,RF                                                            
T1RS24   MVC   0(L'RV_PIDC,R1),RV_PIDC                                          
                                                                                
         USING ACTRECD,R2                                                       
T1RS48   LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),IS_1R                                                 
         LA    RF,ACTKACT                                                       
                                                                                
         LLC   R1,RV_1RL1                                                       
         SHI   R1,1                                                             
         MVC   0(0,RF),LOCOFF                                                   
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
                                                                                
         LLC   RE,RV_1RL1                                                       
         LLC   R1,RV_1RL2                                                       
         SR    R1,RE                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),LOCDEPT                                                  
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
                                                                                
         LLC   RE,RV_1RL2                                                       
         LLC   R1,RV_1RL3                                                       
         SR    R1,RE                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),LOCSUB                                                   
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
                                                                                
         MVC   0(L'RV_PIDC,RF),RV_PIDC                                          
         MVC   TEMP(L'ACTKULA),ACTKULA                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    T1RS50                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),TEMP                                                 
         LHI   RF,AE$RECNF                                                      
         LHI   R0,RQP_PERQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         OI    RV_ERIND,RV_ER1RQ                                                
                                                                                
T1RS50   DS    0H                                                               
         J     T1RS08                                                           
                                                                                
T1RS60   OC    HALF1,HALF1         resolve office mapping?                      
         JZ    T1RS70                                                           
*&&UK                                                                           
         CLC   CUAALF,=C'A1'       GroupM                                       
         JE    T1RS62                                                           
         CLC   CUAALF,=C'T1'       GroupM test file                             
         JNE   T1RS70                                                           
T1RS62   GOTOR GET_MAP,HALF1                                                    
         MVC   RV_ULASX+2(L'RV_1ROCM),RV_1ROCM                                  
*&&                                                                             
                                                                                
T1RS70   DS    0H                                                               
         J     EXITY                                                            
                                                                                
         DROP  R2,R4                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: update 1R location accounts                      *         
*                    Person record in AIO2 for multiple locations     *         
***********************************************************************         
                                                                                
UPD_1RS  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**U_1RS*'                                                      
                                                                                
         USING PERRECD,R4                                                       
         L     R4,AIO2                                                          
         AHI   R4,PERRFST-PERRECD                                               
         USING LOCELD,R4                                                        
                                                                                
U1RS02   CLI   LOCEL,0             get each location                            
         JE    U1RS50                                                           
         CLI   LOCEL,LOCELQ                                                     
         JE    U1RS10                                                           
                                                                                
U1RS04   LLC   R1,LOCLN                                                         
         AR    R4,R1                                                            
         J     U1RS02                                                           
                                                                                
         USING ACTRECD,R2                                                       
U1RS10   LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),IS_1R                                                 
         LA    RF,ACTKACT                                                       
                                                                                
         LLC   R1,RV_1RL1                                                       
         SHI   R1,1                                                             
         MVC   0(0,RF),LOCOFF                                                   
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
                                                                                
         LLC   RE,RV_1RL1                                                       
         LLC   R1,RV_1RL2                                                       
         SR    R1,RE                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),LOCDEPT                                                  
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
                                                                                
         LLC   RE,RV_1RL2                                                       
         LLC   R1,RV_1RL3                                                       
         SR    R1,RE                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),LOCSUB                                                   
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
                                                                                
         MVC   0(L'RV_PIDC,RF),RV_PIDC                                          
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   U1RS48              skip if cannot be found                      
                                                                                
         MVC   RV_DA,ACTKDA                                                     
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         TM    RV_CHIND,RV_CHNCQ   person name change?                          
         JZ    U1RS11                                                           
                                                                                
         USING CPTRBLK,R3                                                       
         L     R3,AGENAXTN                                                      
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         MVC   LDGLVALN(4),RV_1RL1                                              
         MVC   LDGSTA2,RV_1RS2                                                  
         MVC   CPYVALS,RV_CPYV                                                  
         GOTO1 VPADDLE,DMCB,(C'D',AIO1),(C'K',CPTRBLK),0,0,ACOMFACS             
         DROP  R3                                                               
                                                                                
U1RS11   L     R2,AIO1                                                          
         USING NAMELD,R3                                                        
         LA    R3,ACTRFST                                                       
         MVI   BYTE1,NOQ                                                        
                                                                                
U1RS12   CLI   NAMEL,NAMELQ        find and adjust elements                     
         JE    U1RS16                                                           
         CLI   NAMEL,GPNELQ                                                     
         JE    U1RS18                                                           
         CLI   NAMEL,EMPELQ                                                     
         JE    U1RS22                                                           
         CLI   NAMEL,ACTVELQ                                                    
         JE    U1RS24                                                           
         CLI   NAMEL,0                                                          
         JE    U1RS30                                                           
                                                                                
U1RS14   LLC   R1,NAMLN                                                         
         AR    R3,R1                                                            
         J     U1RS12                                                           
                                                                                
U1RS16   TM    RV_CHIND,RV_CHNCQ   person name change?                          
         JZ    U1RS14                                                           
         MVI   NAMEL,FF                                                         
         MVI   BYTE1,YESQ                                                       
         J     U1RS14                                                           
                                                                                
         USING GPNELD,R3                                                        
U1RS18   CLI   GPNTYP,GPNTLST                                                   
         JE    U1RS20                                                           
         CLI   GPNTYP,GPNTFST                                                   
         JNE   U1RS14                                                           
                                                                                
U1RS20   TM    RV_CHIND,RV_CHNCQ   person name change?                          
         JZ    U1RS14                                                           
         MVI   GPNEL,FF                                                         
         MVI   BYTE1,YESQ                                                       
         J     U1RS14                                                           
                                                                                
         USING EMPELD,R3                                                        
U1RS22   CLI   LOCSTAT,LOCSACT     active PERRECD location?                     
         JNE   U1RS14                                                           
         TM    RV_CHIND,RV_CHPTQ   terminate person change?                     
         JZ    U1RS14                                                           
         MVC   EMPTRM,RQP_TRM      set termination                              
         MVI   EMPCSTAT,EMPCTRM                                                 
         J     U1RS14                                                           
                                                                                
         USING ACTVD,R3                                                         
U1RS24   GOTO1 VDATCON,DMCB,(1,TODAYP),(3,ACTVCHDT)                             
         MVC   ACTVCHID,CCTPID                                                  
         MVI   ACTVCHFL,ACTVPPQ                                                 
         AI    ACTVCHNM,1                                                       
         J     U1RS14                                                           
                                                                                
U1RS30   CLI   BYTE1,YESQ          delete elements                              
         JNE   U1RS32                                                           
                                                                                
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('FF',ACTRECD),0                       
                                                                                
U1RS32   TM    RV_CHIND,RV_CHNCQ   person name change?                          
         JZ    U1RS40                                                           
                                                                                
         GOTOR ADD_GPN                                                          
                                                                                
         USING NAMELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   NAMEL,NAMELQ                                                     
         LLC   R1,RV_NAML                                                       
         SHI   R1,1                                                             
         MVC   NAMEREC(0),RV_NAME                                               
         EX    R1,*-6                                                           
         AHI   R1,NAMEREC-NAMELD+1                                              
         STC   R1,NAMLN                                                         
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ACTRECD,NAMELD,0,0                     
         CLI   12(R1),0                                                         
         JNE   *+2                 die - can't add element                      
                                                                                
U1RS40   LHI   R1,PR_PASQ+PR_NOWQ  person name change?                          
         TM    RV_CHIND,RV_CHNCQ                                                
         JNZ   U1RS42                                                           
         LHI   R1,PR_NOWQ                                                       
                                                                                
U1RS42   GOTOR PUT_REC                                                          
         GOTOR SAVRET,RT_1RQ                                                    
                                                                                
U1RS48   DS    0H                  next location                                
         J     U1RS04                                                           
                                                                                
U1RS50   DS    0H                                                               
         J     EXITY                                                            
                                                                                
         DROP  R2,R3,R4                                                         
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: Test 2P account record and check name            *         
***********************************************************************         
                                                                                
TST_2PA  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**T_2PA*'                                                      
                                                                                
         CLC   RV_ULA2P,SPACES                                                  
         JNH   T2PA12                                                           
                                                                                
         USING ACTRECD,R2                                                       
T2PA00   LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,RV_ULA2P                                                 
         MVC   TEMP(L'ACTKULA),ACTKULA                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   T2PA12                                                           
                                                                                
T2PA02   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING NAMELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
                                                                                
T2PA04   CLI   NAMEL,0                                                          
         JE    T2PA10                                                           
         CLI   NAMEL,NAMELQ                                                     
         JE    T2PA06                                                           
         LLC   R1,NAMLN                                                         
         AR    R3,R1                                                            
         J     T2PA04                                                           
                                                                                
T2PA06   LLC   R1,NAMLN                                                         
         SHI   R1,NAMLN1Q                                                       
         LLC   RE,RV_NAML                                                       
         CR    RE,R1                                                            
         JNE   T2PA10                                                           
         SHI   R1,1                                                             
         EX    R1,T2PAEX                                                        
         JE    T2PA20                                                           
                                                                                
T2PA10   OI    RV_NCIND,RV_NC2PQ   2P a/c name changed                          
         J     T2PA20                                                           
                                                                                
         USING PIDRECD,R2                                                       
T2PA12   LA    R2,IOKEY            get account code via PID                     
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,RQP_PIN                                                  
         MVI   PIDKSTYP,PIDKPSNQ                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
         CLC   PIDKEY(PIDKPER-PIDKEY),IOKEYSAV                                  
         JE    T2PA14                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),TEMP                                                 
         LHI   RF,AE$RECNF                                                      
         LHI   R0,RQP_2PCQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         OI    RV_ERIND,RV_ER2PQ                                                
         J     T2PA20                                                           
                                                                                
T2PA14   MVC   RV_ULA2P,PIDKULA                                                 
         J     T2PA00              try this one now                             
                                                                                
T2PA20   DS    0H                                                               
         J     EXITY                                                            
                                                                                
T2PAEX   CLC   NAMEREC(0),RV_NAME                                               
                                                                                
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: Test SX account record and check name            *         
***********************************************************************         
                                                                                
TST_SXA  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**T_SXA*'                                                      
                                                                                
         CLC   RV_ULASX,SPACES                                                  
         JNH   TSXA12                                                           
                                                                                
         USING ACTRECD,R2                                                       
TSXA00   LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,RV_ULASX                                                 
         MVC   TEMP(L'ACTKULA),ACTKULA                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   TSXA12                                                           
                                                                                
TSXA02   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING NAMELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
                                                                                
TSXA04   CLI   NAMEL,0                                                          
         JE    TSXA10                                                           
         CLI   NAMEL,NAMELQ                                                     
         JE    TSXA06                                                           
         LLC   R1,NAMLN                                                         
         AR    R3,R1                                                            
         J     TSXA04                                                           
                                                                                
TSXA06   LLC   R1,NAMLN                                                         
         SHI   R1,NAMLN1Q                                                       
         LLC   RE,RV_NAML                                                       
         CR    RE,R1                                                            
         JNE   TSXA10                                                           
         SHI   R1,1                                                             
         EX    R1,TSXAEX                                                        
         JE    TSXA20                                                           
                                                                                
TSXA10   OI    RV_NCIND,RV_NCSXQ   SX a/c name changed                          
         J     TSXA20                                                           
                                                                                
         USING PIDRECD,R2                                                       
TSXA12   LA    R2,IOKEY            get account code via PID                     
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,RQP_PIN                                                  
         MVI   PIDKSTYP,PIDKCRDQ                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
         CLC   PIDKEY(PIDKPER-PIDKEY),IOKEYSAV                                  
         JE    TSXA14                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),TEMP                                                 
         LHI   RF,AE$RECNF                                                      
         LHI   R0,RQP_SXCQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         OI    RV_ERIND,RV_ERSXQ                                                
         J     TSXA20                                                           
                                                                                
TSXA14   MVC   RV_ULASX,PIDKULA                                                 
         J     TSXA00              try this one now                             
                                                                                
TSXA20   DS    0H                                                               
         J     EXITY                                                            
                                                                                
TSXAEX   CLC   NAMEREC(0),RV_NAME                                               
                                                                                
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate hire date                               *         
***********************************************************************         
                                                                                
VAL_HIR  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_HIR*'                                                      
                                                                                
         OC    RQP_HIR,RQP_HIR                                                  
         JNZ   VHIR02                                                           
                                                                                
         LHI   RF,AE$MISDA                                                      
         LHI   R0,RQP_HIRQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    RV_ERIND,RV_ERHDQ                                                
         J     VHIR04                                                           
                                                                                
VHIR02   DS    0H                  no AE$DOPSP or AE$IVDNC checks               
                                                                                
VHIR04   DS    0H                                                               
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate names                                   *         
***********************************************************************         
                                                                                
VAL_NAM  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_NAM*'                                                      
                                                                                
         CLC   RQP_LNA,SPACES                                                   
         JH    VNAM02                                                           
                                                                                
         LHI   RF,AE$MISIF                                                      
         LHI   R0,RQP_LNAQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
VNAM02   LA    RE,RQP_LNA+L'RQP_LNA-1                                           
         LHI   RF,L'RQP_LNA                                                     
                                                                                
VNAM04   CLI   0(RE),SPACEQ                                                     
         JH    VNAM06                                                           
         SHI   RE,1                                                             
         JCT   RF,VNAM04                                                        
         J     *+2                 (VAL_NAM bug)                                
                                                                                
VNAM06   STC   RF,RV_LNLN                                                       
                                                                                
VNAM08   CLC   RQP_FNA,SPACES                                                   
         JH    VNAM10                                                           
                                                                                
         LHI   RF,AE$MISIF                                                      
         LHI   R0,RQP_FNAQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITN                                                            
                                                                                
VNAM10   LA    RE,RQP_FNA+L'RQP_FNA-1                                           
         LHI   RF,L'RQP_FNA                                                     
                                                                                
VNAM12   CLI   0(RE),SPACEQ                                                     
         JH    VNAM14                                                           
         SHI   RE,1                                                             
         JCT   RF,VNAM12                                                        
         J     *+2                 (VAL_NAM bug)                                
                                                                                
VNAM14   STC   RF,RV_FNLN                                                       
                                                                                
VNAM16   DS    0H                                                               
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate 1R names                                *         
***********************************************************************         
                                                                                
VAL_1RN  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_1RN*'                                                      
                                                                                
         MVC   RV_NAME,SPACES                                                   
         MVI   RV_NAML,1                                                        
                                                                                
         TM    SCPXEL+CPXSTAT6-CPXELD,CPXCNAMO                                  
         JNZ   V1RN06                                                           
                                                                                
* Last name then first name                                                     
                                                                                
         MVC   TEMP(60),SPACES                                                  
         MVC   TEMP(L'RQP_LNA),RQP_LNA                                          
         OC    TEMP(L'RQP_LNA),SPACES                                           
                                                                                
         LA    RE,TEMP+L'RQP_LNA-1                                              
         LHI   RF,L'RQP_LNA                                                     
                                                                                
V1RN02   CLI   0(RE),SPACEQ        find end of name                             
         JH    V1RN04                                                           
         SHI   RE,1                                                             
         JCT   RF,V1RN02                                                        
         J     *+2                 (bad code/no name)                           
                                                                                
V1RN04   AHI   RE,2                point to next name                           
         MVC   0(L'RQP_FNA,RE),RQP_FNA                                          
         OC    0(L'RQP_FNA,RE),SPACES                                           
         J     V1RN12                                                           
                                                                                
* First name then last name                                                     
                                                                                
V1RN06   MVC   TEMP(60),SPACES                                                  
         MVC   TEMP(L'RQP_FNA),RQP_FNA-1                                        
         OC    TEMP(L'RQP_FNA),SPACES                                           
                                                                                
         LA    RE,TEMP+L'RQP_FNA                                                
         LHI   RF,L'RQP_FNA                                                     
                                                                                
V1RN08   CLI   0(RE),SPACEQ        find end of name                             
         JH    V1RN10                                                           
         SHI   RE,1                                                             
         JCT   RF,V1RN08                                                        
         J     *+2                 (bad code/no name)                           
                                                                                
V1RN10   AHI   RE,2                point to next name                           
         MVC   0(L'RQP_LNA,RE),RQP_LNA                                          
         OC    0(L'RQP_LNA,RE),SPACES                                           
                                                                                
* Common name handling again                                                    
                                                                                
V1RN12   MVC   RV_NAME,TEMP                                                     
                                                                                
         LA    RE,RV_NAME+L'RV_NAME-1                                           
         LHI   R1,L'RV_NAME                                                     
                                                                                
V1RN14   CLI   0(RE),SPACEQ                                                     
         JH    V1RN16                                                           
         SHI   RE,1                                                             
         JCT   R1,V1RN14                                                        
         J     *+2                 (see above)                                  
                                                                                
V1RN16   STC   R1,RV_NAML                                                       
                                                                                
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate subsidiary data fields for 1R           *         
***********************************************************************         
                                                                                
VAL_1RV  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_1RV*'                                                      
                                                                                
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate subsidiary data fields for SX           *         
***********************************************************************         
                                                                                
VAL_SXV  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_SXV*'                                                      
                                                                                
         MVI   RV_EMALN,0                                                       
         CLC   RQP_EMA,SPACES                                                   
         JNH   VSXV20                                                           
                                                                                
         LA    RE,RQP_EMA+L'RQP_EMA-1                                           
         LHI   R1,L'RQP_EMA-1                                                   
                                                                                
VSXV02   CLI   0(RE),SPACEQ                                                     
         JH    VSXV04                                                           
         SHI   RE,1                                                             
         JCT   R1,VSXV02                                                        
         J     *+2                 (bug in SPACES check)                        
                                                                                
VSXV04   AHI   R1,1                                                             
         STC   R1,RV_EMALN                                                      
                                                                                
* ACFIL0.VALEML based code                                                      
                                                                                
         LA    R3,RQP_EMA          point R3 to @ in mail address                
         LLC   RE,RV_EMALN                                                      
                                                                                
VSXV06   CLI   0(R3),SPACEQ                                                     
         JNH   VSXS14                                                           
         CLI   0(R3),ATSIGNQ       mail address must have @                     
         JE    VSXV08                                                           
         CLI   0(R3),COMMAQ        ',' is invalid                               
         JE    VSXS14                                                           
         AHI   R3,1                                                             
         JCT   RE,VSXV06                                                        
         J     VSXS14                                                           
                                                                                
VSXV08   LR    RF,R3               check mail domain has at least one           
         XR    RE,RE               '.' dot                                      
                                                                                
VSXS10   CLI   0(RF),SPACEQ                                                     
         JNH   VSXS12                                                           
         CLI   0(RF),DOTQ                                                       
         JNE   *+8                                                              
         AHI   RE,1                                                             
         CLI   0(RF),COMMAQ        ',' is invalid                               
         JE    VSXS14                                                           
         AHI   RF,1                                                             
         J     VSXS10                                                           
                                                                                
VSXS12   CHI   RE,1                check at least one '.' dot                   
         JNL   VSXV20                                                           
                                                                                
VSXS14   LHI   RF,AE$INVEM                                                      
         LHI   R0,RQP_EMAQ                                                      
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    RV_ERIN2,RV_ERSXE                                                
                                                                                
VSXV20   DS    0H                                                               
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate subsidiary data fields for 2P           *         
***********************************************************************         
                                                                                
VAL_2PV  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_2PV*'                                                      
                                                                                
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: validate Limit List record                       *         
***********************************************************************         
                                                                                
VAL_LLS  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**V_LLS*'                                                      
                                                                                
         OC    RQP_LLN,RQP_LLN                                                  
         JZ    VLLS04                                                           
         MVC   TEMP2(L'RQP_LLN),RQP_LLN                                         
         GOTOR (#GETPID,AGETPID)                                                
         JNE   VLLS02                                                           
         MVC   RQP_LLC,TEMP2                                                    
         J     VLLS10                                                           
                                                                                
VLLS02   MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'LLSTPIN),LLSTPIN                                       
         LHI   RF,AE$INPID                                                      
         LHI   R0,RQP_LLNQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
         OI    RV_ERIN3,RV_ERLLS                                                
         J     VLLS10                                                           
                                                                                
VLLS04   CLC   RQP_LLC,SPACES                                                   
         JNH   VLLS06                                                           
         MVC   TEMP2(L'RQP_LLC),RQP_LLC                                         
         GOTOR (#GETPIN,AGETPIN)                                                
         JE    VLLS08                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'LLSTPID),LLSTPID                                       
         LHI   RF,AE$INPID                                                      
         LHI   R0,RQP_LLCQ                                                      
         GOTOR SAVERR,DMCB,(RF),(L'XERRTXT,XERRTXT),(R0)                        
                                                                                
VLLS06   OI    RV_ERIN3,RV_ERLLS                                                
         J     VLLS10                                                           
                                                                                
VLLS08   MVC   RQP_LLN,TEMP2+50                                                 
                                                                                
VLLS10   DS    0H                                                               
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Acc Person Upload: build basic person record                        *         
***********************************************************************         
                                                                                
BLD_PER  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**B_PER*'                                                      
                                                                                
         USING PERRECD,R2                                                       
         L     R2,AIO1             build record key                             
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,RV_PIDC                                                 
                                                                                
         XC    PERRLNK,PERRLNK     clear linkage area                           
                                                                                
         XC    PERRSTA,PERRSTA     clear status area                            
                                                                                
         USING EMPELD,R3                                                        
         LA    R3,PERRFST          add first element                            
         XC    EMPEL(EMPLNQ+2),EMPEL                                            
         MVI   EMPEL,EMPELQ                                                     
         MVI   EMPLN,EMPLNQ                                                     
         MVC   EMPHIR,RQP_HIR                                                   
                                                                                
         LHI   RF,PERRFST-PERKEY+EMPLNQ+1                                       
         STCM  RF,B'0011',PERRLEN                                               
                                                                                
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Acc Person Upload: add GDA element                                  *         
*  Note: not required not fully coded for dates                       *         
***********************************************************************         
                                                                                
ADD_GDA  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_GDA*'                                                      
                                                                                
         USING GDAELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALN2Q                                                    
         MVI   GDATYPE,GDATMCST                                                 
         XC    GDADATE,GDADATE                                                  
         XC    GDADATE2,GDADATE2                                                
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO1,GDAELD,(RF),0                     
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Acc Person Upload: add PID element                                  *         
***********************************************************************         
                                                                                
ADD_PID  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_PID*'                                                      
                                                                                
         USING PIDELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   PIDEL,PIDELQ                                                     
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,RQP_PIN                                                    
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO1,PIDELD,(RF),0                     
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Acc Person Upload: add 2P SPA element                               *         
***********************************************************************         
                                                                                
ADD_SPA  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_SPA*'                                                      
                                                                                
         USING SPAELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATPERS                                                 
         MVC   SPAAULA,RV_ULA2P                                                 
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO1,SPAELD,(RF),0                     
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Acc Person Upload: add RAC elements                                 *         
***********************************************************************         
                                                                                
ADD_RAC  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_RAC*'                                                      
                                                                                
         MVI   BYTE1,RACTADD                                                    
                                                                                
         USING RACELD,R3                                                        
ADD_RAC2 LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   RACEL,RACELQ                                                     
         MVI   RACLN,RACLNQ                                                     
         MVC   RACTYPE,BYTE1                                                    
         MVC   RACUSER,CUUSER                                                   
         MVC   RACPERS,CCTPID                                                   
         MVC   RACTERM,CUTERM                                                   
         MVC   RACDATE,TODAYP                                                   
         MVC   RACTIME,CTIME                                                    
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO1,RACELD,(RF),0                     
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         CLI   BYTE1,RACTCHA                                                    
         JE    ADD_RAC4                                                         
         MVI   BYTE1,RACTCHA                                                    
         J     ADD_RAC2                                                         
                                                                                
ADD_RAC4 DS    0H                                                               
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Acc Person Upload: update RACEL element                             *         
***********************************************************************         
                                                                                
UPD_RAC  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**U_RAC*'                                                      
                                                                                
         USING ACCRECD,R2                                                       
         L     R2,AIO1                                                          
         LA    R3,ACCRFST                                                       
                                                                                
         USING RACELD,R3                                                        
         XR    R0,R0                                                            
         XC    TEMP(4),TEMP                                                     
         MVC   TEMP+4(L'RACDATE+L'RACTIME),FFS                                  
                                                                                
URAC02   CLI   RACEL,0                                                          
         JE    URAC08                                                           
         CLI   RACEL,RACELQ                                                     
         JE    URAC06                                                           
                                                                                
URAC04   LLC   R1,RACLN                                                         
         AR    R3,R1                                                            
         J     URAC02                                                           
                                                                                
URAC06   CLI   RACTYPE,RACTCHA                                                  
         JNE   URAC04                                                           
         AHI   R0,1                                                             
         CLC   RACDATE(L'RACDATE+L'RACTIME),TEMP+4                              
         JNL   URAC04                                                           
         MVC   TEMP+4(L'RACDATE+L'RACTIME),RACDATE                              
         STCM  R3,B'1111',TEMP                                                  
         J     URAC04                                                           
                                                                                
URAC08   CHI   R0,RACMAXQ                                                       
         JL    URAC12                                                           
                                                                                
URAC10   ICM   R3,B'1111',TEMP     replace earliest element                     
         MVC   RACUSER,CUUSER                                                   
         MVC   RACPERS,CCTPID                                                   
         MVC   RACTERM,CUTERM                                                   
         MVC   RACDATE,TODAYP                                                   
         MVC   RACTIME,CTIME                                                    
         J     URAC20                                                           
                                                                                
URAC12   XR    R1,R1               does RACEL fit onto record?                  
         ICM   R1,B'0011',ACCRLEN                                               
         AHI   R1,RACLNQ+1                                                      
         CLM   R1,B'0011',RV_MAXAR yes so add change element                    
         JNH   URAC14                                                           
         CHI   R0,0                any RACEL to replace?                        
         JH    URAC10                                                           
         J     *+2                 (? could skip but no RACEL then)             
                                                                                
URAC14   LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   RACEL,RACELQ                                                     
         MVI   RACLN,RACLNQ                                                     
         MVI   RACTYPE,RACTCHA                                                  
         MVC   RACUSER,CUUSER                                                   
         MVC   RACPERS,CCTPID                                                   
         MVC   RACTERM,CUTERM                                                   
         MVC   RACDATE,TODAYP                                                   
         MVC   RACTIME,CTIME                                                    
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),(R2),RACELD,(RF),0                     
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
URAC20   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Acc Person Upload: add ABL element                                  *         
***********************************************************************         
                                                                                
ADD_ABL  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_ABL*'                                                      
                                                                                
         USING ABLELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ABLEL,ABLELQ                                                     
         MVI   ABLLN,ABLLN3Q                                                    
         ZAP   ABLFRWD,PZERO                                                    
         ZAP   ABLDR,PZERO                                                      
         ZAP   ABLCR,PZERO                                                      
         ZAP   ABLURG,PZERO                                                     
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO1,ABLELD,(RF),0                     
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Acc Person Upload: add APO element                                  *         
***********************************************************************         
                                                                                
ADD_APO  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_APO*'                                                      
                                                                                
         USING APOELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   APOEL,APOELQ                                                     
         MVI   APOLN,APOLN2Q                                                    
         ZAP   APODR,PZERO                                                      
         ZAP   APOCR,PZERO                                                      
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO1,APOELD,(RF),0                     
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Acc Person Upload: add EMP element                                  *         
***********************************************************************         
                                                                                
ADD_EMP  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_EMP*'                                                      
                                                                                
         USING EMPELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   EMPEL,EMPELQ                                                     
         MVI   EMPLN,EMPLNQ                                                     
         MVC   EMPHIR,RQP_HIR                                                   
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO1,EMPELD,(RF),0                     
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Acc Person Upload: add RST element                                  *         
***********************************************************************         
                                                                                
ADD_RST  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_RST*'                                                      
                                                                                
         USING ACTRECD,R2                                                       
         L     R2,AIO1                                                          
                                                                                
         USING RSTELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   RSTEL,RSTELQ                                                     
*&&UK*&& MVI   RSTLN,RSTLN4Q                                                    
*&&US*&& MVI   RSTLN,RSTLN3Q                                                    
         MVC   RSTBDATE,TODAYP                                                  
         MVC   RSTTDATE,TODAYP                                                  
         MVC   RSTDFTSK,SPACES                                                  
         MVC   RSTFILT1,ACTRSAF1                                                
         MVC   RSTFILT2,ACTRSAF2                                                
         MVC   RSTFILT3,ACTRSAF3                                                
         MVC   RSTFILT4,ACTRSAF4                                                
         MVC   RSTFILT5,ACTRSAF5                                                
         MVC   RSTCOSTG,SPACES                                                  
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),ACTRECD,RSTELD,(RF),0                  
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Acc Person Upload: add AST element                                  *         
***********************************************************************         
                                                                                
ADD_AST  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_AST*'                                                      
                                                                                
         USING ACTRECD,R2                                                       
         L     R2,AIO1                                                          
                                                                                
         USING ASTELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ASTEL,ASTELQ                                                     
         MVI   ASTLN,ASTLN1Q                                                    
         CLC   IS_SX,ACTKULA                                                    
         JNE   AAST02                                                           
         MVC   ASTCUR,SCPYEL+CPYCURR-CPYELD                                     
                                                                                
AAST02   LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),ACTRECD,ASTELD,(RF),0                  
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Acc Person Upload: add BAC element                                  *         
***********************************************************************         
                                                                                
ADD_BAC  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_BAC*'                                                      
                                                                                
         USING ACTRECD,R2                                                       
         L     R2,AIO1                                                          
                                                                                
         USING BACELD,R3                                                        
         LA    R3,ELEMENT          add basic BACEL only                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   BACEL,BACELQ                                                     
         MVI   BACLN,BACLNQ                                                     
         MVI   BACSTAT,BACSTMCH                                                 
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),ACTRECD,BACELD,(RF),0                  
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Acc Person Upload: add Email element                                *         
***********************************************************************         
                                                                                
ADD_EMA  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_EMA*'                                                      
                                                                                
         USING ACTRECD,R2                                                       
         L     R2,AIO1                                                          
                                                                                
         TM    RV_ERIN2,RV_ERSXE   invalid mail?                                
         JNZ   AEMA02                                                           
                                                                                
         CLI   RV_EMALN,0          skip if no mail passes                       
         JE    AEMA02                                                           
                                                                                
         USING FFTELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ                                                     
*&&UK*&& MVI   FFTTYPE,FFTTPEML                                                 
*&&US*&& MVI   FFTTYPE,FFTTEML                                                  
         MVC   FFTDLEN,RV_EMALN                                                 
         LLC   R1,RV_EMALN                                                      
         SHI   R1,1                                                             
         MVC   FFTDATA(0),RQP_EMA                                               
         EX    R1,*-6                                                           
         AHI   R1,1+FFTLN1Q+L'FFTDLEN                                           
         STC   R1,FFTLN                                                         
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),ACTRECD,FFTELD,(RF),0                  
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
AEMA02   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Acc Person Upload: add DD activity element                          *         
***********************************************************************         
                                                                                
ADD_DDA  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_DDA*'                                                      
                                                                                
         USING ACTVD,R3                                                         
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACTVEL,ACTVELQ                                                   
         MVI   ACTVLEN,ACTVLENQ                                                 
         MVC   ACTVADDT,TODAYP                                                  
         GOTO1 VDATCON,DMCB,(1,TODAYP),(3,ACTVADDT)                             
         MVC   ACTVADID,CCTPID                                                  
         MVI   ACTVADFL,ACTVPPQ                                                 
         MVC   ACTVCHDT,ACTVADDT                                                
         MVC   ACTVSCID,RV_CCTPC                                                
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO1,ACTVD,(RF),0                      
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Acc Person Upload: add GPN elements                                 *         
***********************************************************************         
                                                                                
ADD_GPN  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_GPN*'                                                      
                                                                                
         LA    R4,RQP_LNA                                                       
         LLC   R1,RV_LNLN                                                       
         LHI   R0,GPNTLST                                                       
                                                                                
         USING GPNELD,R3                                                        
AGPN02   LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   GPNEL,GPNELQ                                                     
         STC   R0,GPNTYP                                                        
                                                                                
         SHI   R1,1                                                             
         MVC   GPNNME(0),0(R4)                                                  
         EX    R1,*-6                                                           
         OC    GPNNME(0),0(R4)                                                  
         EX    R1,*-6                                                           
         AHI   R1,1+GPNLNQ                                                      
         STC   R1,GPNLN                                                         
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO1,GPNELD,(RF),0                     
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         CHI   R0,GPNTLST                                                       
         JNE   AGPN04                                                           
         LA    R4,RQP_FNA                                                       
         LLC   R1,RV_FNLN                                                       
         LHI   R0,GPNTFST                                                       
         J     AGPN02                                                           
                                                                                
AGPN04   DS    0H                                                               
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Acc Person Upload: add location element                             *         
***********************************************************************         
                                                                                
ADD_LOC  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_LOC*'                                                      
                                                                                
         USING LOCELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   LOCEL,LOCELQ                                                     
         MVI   LOCLN,LOCLNQ                                                     
         MVC   LOCSTART,RQP_HIR                                                 
         MVC   LOCOFF,RQP_OFF                                                   
         MVC   LOCDEPT,SPACES                                                   
         MVC   LOCDEPT(L'RQP_DEP),RQP_DEP                                       
         MVC   LOCSUB,SPACES                                                    
         MVC   LOCSUB(L'RQP_SUB),RQP_SUB                                        
         MVI   LOCSTAT,LOCSACT                                                  
                                                                                
         LA    RF,ADDEND                                                        
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO1,LOCELD,(RF),0                     
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Acc Person Upload: add a record incl. passives                      *         
***********************************************************************         
                                                                                
ADD_REC  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_REC*'                                                      
                                                                                
         USING ACCRECD,R2                                                       
         L     R2,AIO1                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO1'                           
         JNE   *+2                                                              
                                                                                
         CLI   ACCKEY+PHIKTYP-PHIRECD,PHIKTYPQ                                  
         JNE   ADD_REC0                                                         
         CLI   ACCKEY+PHIKSUB-PHIRECD,PHIKSUBQ                                  
         JE    ADD_RECX                                                         
                                                                                
         USING CPTRBLK,R4                                                       
ADD_REC0 L     R4,AGENAXTN                                                      
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
                                                                                
         CLC   IS_1R,ACCKEY+ACTKULA-ACTKEY                                      
         JNE   ADD_REC2                                                         
         MVC   LDGLVALN(4),RV_1RL1                                              
         MVC   LDGSTA2,RV_1RS2                                                  
         J     ADD_REC8                                                         
                                                                                
ADD_REC2 CLC   IS_2P,ACCKEY+ACTKULA-ACTKEY                                      
         JNE   ADD_REC4                                                         
         MVC   LDGLVALN(4),RV_2PL1                                              
         MVC   LDGSTA2,RV_2PS2                                                  
         J     ADD_REC8                                                         
                                                                                
ADD_REC4 CLC   IS_SX,ACCKEY+ACTKULA-ACTKEY                                      
         JNE   ADD_REC6                                                         
         MVC   LDGLVALN(4),RV_SXL1                                              
         MVC   LDGSTA2,RV_SXS2                                                  
         J     ADD_REC8                                                         
                                                                                
ADD_REC6 DS    0H                                                               
                                                                                
ADD_REC8 MVC   CPYVALS,RV_CPYV                                                  
         GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,IODA,0,ACOMFACS                 
         DROP  R4                                                               
                                                                                
ADD_RECX DS    0H                                                               
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Acc Person Upload: add a DPA passive                                *         
***********************************************************************         
                                                                                
ADD_DPA  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**A_DPA*'                                                      
                                                                                
         USING APPRECD,R2                                                       
         XR    R2,R2               base record                                  
         ICM   R2,B'0111',1(R1)                                                 
         MVC   BYTE1,0(R1)         application type                             
         L     R4,4(R1)            1R account code                              
                                                                                
         USING DPAPASD,R3                                                       
         LA    R3,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,APPKCPY                                                  
         MVC   DPAPAPPL,BYTE1                                                   
         ZAP   DPAPXVAL,PZERO                                                   
         MVC   DPAP1RAC,2(R4)                                                   
         MVC   DPAPPIDB,APPKPIDB                                                
         MVC   DPAPSEQ,APPKSEQ                                                  
                                                                                
         MVC   DPAPDA,IODA                                                      
         CLC   RV_DA,IODA                                                       
         JNE   *+2                 (code integrity)                             
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IODIR'                                  
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Acc Person Upload: put a record incl. passives                      *         
***********************************************************************         
                                                                                
PR_PASQ  EQU   X'80'               do passives via paddle                       
PR_NOWQ  EQU   X'08'               no directory write requierd                  
                                                                                
PUT_REC  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'**P_REC*'                                                      
                                                                                
         STC   R1,BYTE1                                                         
                                                                                
         USING ACCRECD,R2                                                       
         L     R2,AIO1                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JNE   *+2                                                              
                                                                                
         TM    BYTE1,PR_NOWQ       skip write of directory?                     
         JNZ   PUT_REC0                                                         
                                                                                
KY       USING ACCRECD,R1                                                       
         LA    R1,IOKEY                                                         
         MVC   KY.ACCKSTA,ACCKSTA                                               
         DROP  KY                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1'                            
         JNE   *+2                                                              
                                                                                
PUT_REC0 TM    BYTE1,PR_PASQ       do passives?                                 
         JZ    PUT_RECX                                                         
                                                                                
         USING CPTRBLK,R4                                                       
         L     R4,AGENAXTN                                                      
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
                                                                                
         CLC   IS_1R,ACCKEY+ACTKULA-ACTKEY                                      
         JNE   PUT_REC2                                                         
         MVC   LDGLVALN(4),RV_1RL1                                              
         MVC   LDGSTA2,RV_1RS2                                                  
         J     PUT_REC8                                                         
                                                                                
PUT_REC2 CLC   IS_2P,ACCKEY+ACTKULA-ACTKEY                                      
         JNE   PUT_REC4                                                         
         MVC   LDGLVALN(4),RV_2PL1                                              
         MVC   LDGSTA2,RV_2PS2                                                  
         J     PUT_REC8                                                         
                                                                                
PUT_REC4 CLC   IS_SX,ACCKEY+ACTKULA-ACTKEY                                      
         JNE   PUT_REC6                                                         
         MVC   LDGLVALN(4),RV_SXL1                                              
         MVC   LDGSTA2,RV_SXS2                                                  
         J     PUT_REC8                                                         
                                                                                
PUT_REC6 CLI   ACCKEY+APPKTYP-APPRECD,APPKTYPQ                                  
         JNE   PUT_REC8                                                         
         CLI   ACCKEY+APPKSUB-APPRECD,APPKSUBQ                                  
         JNE   PUT_REC8                                                         
         MVC   LDGLVALN(4),RV_1RL1                                              
         MVC   LDGSTA2,RV_1RS2                                                  
                                                                                
PUT_REC8 MVC   CPYVALS,RV_CPYV                                                  
         GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,IODA,0,ACOMFACS                 
         DROP  R4                                                               
                                                                                
PUT_RECX DS    0H                                                               
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
                                                                                
***********************************************************************         
* Get job's office                                                    *         
***********************************************************************         
         USING ACTRECD,R2                                                       
GETJOF   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*GETJOF*'                                                      
                                                                                
         LA    R2,IOKEY                                                         
         MVC   CSVKEY1,ACTKEY                                                   
                                                                                
         MVC   ROUERRV,=AL2(AE$INCLI)                                           
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         LA    RE,ACTKACT(RE)                                                   
         MVC   0(12,RE),SPACES                                                  
         MVC   CSVKEY2,ACTKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITN                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
         USING PPRELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
GETJOF05 CLI   PPREL,0                                                          
         BE    GETJOF15                                                         
         CLI   PPREL,PPRELQ                                                     
         BE    GETJOF10                                                         
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         B     GETJOF05                                                         
                                                                                
GETJOF10 MVC   XL#COFF,PPRGAOFF                                                 
         OC    XL#COFF,SPACES                                                   
                                                                                
GETJOF15 LA    R2,IOKEY                                                         
         MVC   ACTKEY,CSVKEY2                                                   
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         LR    RF,RE                                                            
         LA    RE,ACTKACT(RE)                                                   
         LA    RF,CSVKEY1+ACTKACT-ACTRECD(RF)                                   
*&&UK*&& MVC   0(2,RE),0(RF)                                                    
*&&US*&& MVC   0(3,RE),0(RF)                                                    
                                                                                
         MVC   ROUERRV,=AL2(AE$INPRO)                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITN                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
         USING PPRELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
GETJOF20 CLI   PPREL,0                                                          
         BE    GETJOF30                                                         
         CLI   PPREL,PPRELQ                                                     
         BE    GETJOF25                                                         
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         B     GETJOF20                                                         
                                                                                
GETJOF25 CLC   PPRGAOFF,SPACES                                                  
         BNH   GETJOF30                                                         
         MVC   XL#COFF,PPRGAOFF                                                 
         OC    XL#COFF,SPACES                                                   
                                                                                
GETJOF30 LA    R2,IOKEY                                                         
         MVC   IOKEY,CSVKEY1                                                    
                                                                                
         USING OFFALD,R3                                                        
         L     R3,AOFFAREA                                                      
         MVC   OFFAOFFC,XL#COFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL,OFFALD       VALIDATE OFFICE                              
         JE    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
***********************************************************************         
* Save return data (R1 with type, AIO1 with record)                   *         
***********************************************************************         
                                                                                
SAVRET   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SAVRET*'                                                      
                                                                                
         USING RT_D,R2                                                          
         LAY   R2,RETTAB                                                        
         LHI   R4,RT_MAXQ                                                       
                                                                                
SRET02   CLI   RT_TYPE,RT_EOTQ     Find next slot                               
         JE    SRET04                                                           
         AHI   R2,RT_LNQ                                                        
         JCT   R4,SRET02                                                        
         J     *+2                 (Incease RT_MAXQ, but why?)                  
                                                                                
SRET04   STC   R1,RT_TYPE                                                       
         XC    RT_CODE,RT_CODE                                                  
                                                                                
         L     R3,AIO1                                                          
         CLI   RT_TYPE,RT_CPQ                                                   
         JNE   SRET06                                                           
         MVC   RT_CODE(L'PERKCODE),PERKCODE-PERRECD(R3)                         
         J     SRET30                                                           
                                                                                
SRET06   CLI   RT_TYPE,RT_HSQ                                                   
         JNE   SRET08                                                           
         MVC   RT_CODE(PHIKMOA-PHIKOFC),PHIKOFC-PHIRECD(R3)                     
         XR    R1,R1                                                            
         ICM   R1,B'0011',PHIKMOA-PHIRECD(R3)                                   
         LNR   R1,R1                                                            
         STCM  R1,B'0011',FULL1                                                 
         MVI   FULL1+2,X'01'                                                    
         GOTO1 VDATCON,DMCB,(1,FULL1),(9,RT_CODE+PHIKMOA-PHIKOFC+1)             
         J     SRET30                                                           
                                                                                
SRET08   CLI   RT_TYPE,RT_1RQ                                                   
         JE    SRET10                                                           
         CLI   RT_TYPE,RT_2PQ                                                   
         JE    SRET10                                                           
         CLI   RT_TYPE,RT_SXQ                                                   
         JNE   SRET12                                                           
                                                                                
SRET10   MVC   RT_CODE(L'ACTKULA),ACTKULA-ACTRECD(R3)                           
         J     SRET30                                                           
                                                                                
SRET12   CLI   RT_TYPE,RT_TAQ                                                   
         JNE   SRET14                                                           
         MVC   RT_CODE(L'RQP_TAC),RQP_TAC                                       
         J     SRET30                                                           
                                                                                
SRET14   CLI   RT_TYPE,RT_XAQ                                                   
         JNE   *+2                 (SAVRET call bug)                            
         MVC   RT_CODE(L'RQP_XAC),RQP_XAC                                       
                                                                                
SRET30   AHI   R2,RT_LNQ                                                        
         MVI   RT_TYPE,RT_EOTQ                                                  
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Save error messages in an area so we can send all errors together   *         
***********************************************************************         
                                                                                
SAVERR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SAVERR*'                                                      
                                                                                
         L     RF,ANXTERR          Address of next error in table               
         LTR   RF,RF                                                            
         JZ    SAVERR02                                                         
         LAY   RE,ERRTAB                                                        
         CLI   0(RE),ET_EOTQ       If table is empty                            
         JNE   SAVERR10                                                         
                                                                                
SAVERR02 LAY   RF,ERRTAB           Point to start of table                      
         XC    ALSTERR,ALSTERR                                                  
                                                                                
         USING ET_D,RF                                                          
SAVERR10 LLC   R0,4(R1)                                                         
         AHI   R0,ET_LN1Q          R0=Length of new entry                       
         AR    R0,RF                                                            
         LAY   RE,ERRTAB+L'ERRTAB                                               
         CR    R0,RE                                                            
         JH    SAVERR30            No room left                                 
         SR    R0,RF                                                            
         STC   R0,0(RF)            Set length of new entry                      
         OI    TWAMODE,TWAMERP     Set we have validation error                 
                                                                                
         LM    R2,R4,0(R1)                                                      
         STCM  R2,B'0011',ET_ERRNO                                              
         STCM  R4,B'0011',ET_FLDNM                                              
         SHI   R0,ET_LN1Q                                                       
         LTR   R1,R0               Test any extra text to be added              
         JZ    SAVERR20            No                                           
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   ET_EXTRA(0),0(R3)   Move extra text into entry                   
         EX    R1,0(RE)                                                         
                                                                                
SAVERR20 ST    RF,ALSTERR          REMEMBER PREVIOUS ERROR SLOT                 
         LA    RF,ET_D+ET_LN1Q     Point to next error slot                     
         AR    RF,R0               Add length of extra text                     
         ST    RF,ANXTERR          Set A(Next entry to be added)                
         MVI   ET_D,ET_EOTQ        Set new end of table                         
         J     EXITY                                                            
                                                                                
SAVERR30 DS    0H                  Too many errors for DDLINKIO                 
         L     RF,ALSTERR          Truncate it                                  
         LA    R0,ET_LN1Q                                                       
         AR    R0,RF                                                            
         LAY   RE,ERRTAB+L'ERRTAB                                               
         CR    R0,RE                                                            
         JH    *+2                 Room left to warn user                       
         SR    R0,RF               table still too long                         
         STC   R0,0(RF)            Set length of new entry                      
         OI    TWAMODE,TWAMEDP     Give up now                                  
                                                                                
         LHI   R1,AE$TMERR                                                      
         STCM  R1,B'0011',ET_ERRNO                                              
         MVI   ET_LN1Q(RF),ET_EOTQ                                              
         J     EXITY                                                            
         DROP  RF                                                               
                                                                                
* next routine to go in here                                                    
                                                                                
***********************************************************************         
* REQUEST MAP FOR ACCOUNT LOCK/UNLOCK UPLOAD                          *         
***********************************************************************         
                                                                                
JSTAUPL  LKREQ H,A#ALCK,NEWREC=Y                                                
ULA      LKREQ F,01,(D,B#SAVED,RQ_ULA),CHAR,TEXT=AC#ACC                         
Locked   LKREQ F,02,(D,B#SAVED,RQ_STA),CHAR,TEXT=AC#LCKED                       
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR ACCOUNTING PERSON UPLOAD                            *         
***********************************************************************         
                                                                                
PERUPL   LKREQ H,A#PERUPL,NEWREC=Y                                              
Action   LKREQ F,RQP_ACTQ,(D,B#SAVED,RQP_ACT),CHAR,TEXT=AC#ACT                  
RQP_ACTQ EQU   01                                                               
PIN      LKREQ F,RQP_PINQ,(D,B#SAVED,RQP_PIN),UBIN,TEXT=AC#RSPID                
RQP_PINQ EQU   02                                                               
PersonC  LKREQ F,RQP_PERQ,(D,B#SAVED,RQP_PER),CHAR,TEXT=AC#RSPSC                
RQP_PERQ EQU   03                                                               
Office   LKREQ F,RQP_OFFQ,(D,B#SAVED,RQP_OFF),CHAR,TEXT=AC#OFFC                 
RQP_OFFQ EQU   04                                                               
Departm  LKREQ F,RQP_DEPQ,(D,B#SAVED,RQP_DEP),CHAR,TEXT=AC#DPT                  
RQP_DEPQ EQU   05                                                               
SubDepm  LKREQ F,RQP_SUBQ,(D,B#SAVED,RQP_SUB),CHAR,TEXT=AC#SUBDP                
RQP_SUBQ EQU   06                                                               
HireDate LKREQ F,RQP_HIRQ,(D,B#SAVED,RQP_HIR),PDAT,TEXT=AC#RSHDT                
RQP_HIRQ EQU   07                                                               
FrstName LKREQ F,RQP_FNAQ,(D,B#SAVED,RQP_FNA),CHAR,TEXT=AC#CFNAM                
RQP_FNAQ EQU   08                                                               
LastName LKREQ F,RQP_LNAQ,(D,B#SAVED,RQP_LNA),CHAR,TEXT=AC#CLNAM                
RQP_LNAQ EQU   09                                                               
TS_Appr  LKREQ F,RQP_TANQ,(D,B#SAVED,RQP_TAN),UBIN,TEXT=AC#APRVR                
RQP_TANQ EQU   10                                                               
TS_AppC  LKREQ F,RQP_TACQ,(D,B#SAVED,RQP_TAC),CHAR,TEXT=AC#APRVR                
RQP_TACQ EQU   11                                                               
EXP_Appr LKREQ F,RQP_XANQ,(D,B#SAVED,RQP_XAN),UBIN,TEXT=AC#FINAP                
RQP_XANQ EQU   12                                                               
EXP_AppC LKREQ F,RQP_XACQ,(D,B#SAVED,RQP_XAC),CHAR,TEXT=AC#FINAP                
RQP_XACQ EQU   13                                                               
EMail    LKREQ F,RQP_EMAQ,(D,B#SAVED,RQP_EMA),CHAR,TEXT=AC#EMAIL,      +        
               LOWERCASE=Y                                                      
RQP_EMAQ EQU   14                                                               
2P_Act   LKREQ F,RQP_2PCQ,(D,B#SAVED,RQP_2PC),CHAR,TEXT=(*,TWOPLIT)             
RQP_2PCQ EQU   15                                                               
SX_Act   LKREQ F,RQP_SXCQ,(D,B#SAVED,RQP_SXC),CHAR,TEXT=(*,SXSYLIT)             
RQP_SXCQ EQU   16                                                               
TermDate LKREQ F,RQP_TRMQ,(D,B#SAVED,RQP_TRM),PDAT,TEXT=AC#TRMDT                
RQP_TRMQ EQU   17                                                               
HrlyRate LKREQ F,RQP_HROQ,(D,B#SAVED,RQP_HRO),SPAK,TEXT=AC#HRRT                 
RQP_HROQ EQU   18                                                               
Lls_PIN  LKREQ F,RQP_LLNQ,(D,B#SAVED,RQP_LLN),UBIN,TEXT=(*,LLSTPIN)             
RQP_LLNQ EQU   19                                                               
Lls_PID  LKREQ F,RQP_LLCQ,(D,B#SAVED,RQP_LLC),CHAR,TEXT=(*,LLSTPID)             
RQP_LLCQ EQU   20                                                               
                                                                                
D#TYPE   EQU   1                   Return: type                                 
D#CODE   EQU   2                   Return: code (key)                           
D#ERR    EQU   3                   Error: is error                              
D#ERROW  EQU   4                   Error: error row number                      
                                                                                
         LKREQ E                                                                
                                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR UPDATE BACS EMAIL STATUS                            *         
***********************************************************************         
                                                                                
A5PUPL   LKREQ H,A#5PEM,NEWREC=Y                                                
Supplier LKREQ F,01,(D,B#SAVED,RQ5PSUP),CHAR,TEXT=AC#SUP                        
PayRef   LKREQ F,02,(D,B#SAVED,RQ5PPRF),CHAR,TEXT=AC#PAYRF                      
PayDate  LKREQ F,03,(D,B#SAVED,RQ5PDAT),PDAT,TEXT=AC#PAYDT                      
EmailAdr LKREQ F,04,(D,B#SAVED,RQ5PEML),CHAR,TEXT=AC#EMAIL                      
EmailSDT LKREQ F,05,(D,B#SAVED,RQ5PEMDT),PDAT,TEXT=AC#DATE                      
EmailSTM LKREQ F,06,(D,B#SAVED,RQ5PEMTM),CHAR,TEXT=AC#TIME                      
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* END OF REQUEST MAP TABLES                                          *          
***********************************************************************         
                                                                                
         LKMAP X                                                                
                                                                                
*** GENERAL UPLOAD RESPONSE DATA MAP NUMBERS                                    
                                                                                
* general equates *                                                             
D#UPLERR EQU   255                                                              
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
SPACEQ   EQU   C' '                                                             
ATSIGNQ  EQU   C'@'                                                             
DOTQ     EQU   C'.'                                                             
COMMAQ   EQU   C','                                                             
BRKOPNQ  EQU   C'('                                                             
BRKCLOQ  EQU   C')'                                                             
NUMBERQ  EQU   C'#'                                                             
                                                                                
IS_1R    DC    CL2'1R'                                                          
IS_2P    DC    CL2'2P'                                                          
*&&UK                                                                           
IS_SX    DC    CL2'SX'                                                          
*&&                                                                             
*&&US                                                                           
IS_SX    DC    CL2'SY'                                                          
*&&                                                                             
                                                                                
DMCOMMIT DC    C'COMMIT  '         Recovery checkpoint command                  
AFILQ    DC    C'AFIL'                                                          
ACCMST   DC    C'ACCMST  '                                                      
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
LSPACES  DC    255C' '                                                          
ADDEND   DC    C'ADD=END'                                                       
AC_COST  DC    C'=Cost'                                                         
AC_HISR  DC    C'History rate'                                                  
TIMEQ    DC    C'Time'                                                          
TIMAQ    DC    C'Time Appr'                                                     
EXPENSEQ DC    C'Expenses'                                                      
EXPENSAQ DC    C'Exp. Appr'                                                     
TWOPLIT  DC    C'2P Account'                                                    
SXSYLIT  DC    C'SX/SY account'                                                 
LLSTPIN  DC    C'Limlist equivalent PIN'                                        
LLSTPID  DC    C'Limlist equivalent PID'                                        
                                                                                
RS_IND   DS    XL1                 End Of record indicator                      
RS_PID19 EQU   X'80'               - FIELD 19 PID EOR INDICATOR                 
RS_PID02 EQU   X'40'               - FIELD 02 PID EOR INDICATOR                 
                                                                                
FFS      DC    16X'FF'                                                          
                                                                                
RECTAB   DS    0XL(RECTABL)        ** RECORD TABLE **                           
         DC    AL2(A#ALCK),AL1(RECTALCK)                                        
         DC    AL2(A#5PEM),AL1(RECT5PEM)                                        
         DC    AL2(A#PERUPL),AL1(RECTAPER)                                      
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
                                                                                
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTTYPE DS    AL1                 ** RECORD TYPE **                            
RECTAPER EQU   1                   Accounting Person upload                     
RECTALCK EQU   2                   Account lock/unlock Upload                   
RECT5PEM EQU   3                   BACS email status upload                     
RECTABL  EQU   *-RECTABD                                                        
                                                                                
SAVED    DSECT                                                                  
                                                                                
ANXTERR  DS    A                   A(Next error entry)                          
ALSTERR  DS    A                   A(Previous error entry in errtab)            
ERRCNT   DS    XL1                                                              
ERRMAX   EQU   25                  Maximum no. of error messages                
                                                                                
TSARABUF DS    XL(TSPXTNL)         TSAR block for GAPLST                        
                                                                                
X#OFFC   DS    CL2                                                              
AC_INDEX DS    XL2                                                              
XL#COFF  DS    CL2                                                              
X#LST2   DS    XL1                                                              
XL#JOBDA DS    XL4                                                              
XL#OJS2  DS    XL1                                                              
XL#ESLNO DS    XL1                                                              
                                                                                
RV_BLOCK DS    0H                                                               
RV_CHIND DS    XL1                 change type indicator                        
RV_CHNCQ EQU   X'80'               - name change                                
RV_CHPTQ EQU   X'08'               - person termination                         
RV_MAXAR DS    XL2                                                              
RV_ERIND DS    XL1                 error indicator                              
RV_ER1RQ EQU   X'80'               - change: missing 1R account record          
RV_ER2PQ EQU   X'40'               - change: missing 2P account record          
RV_ERSXQ EQU   X'20'               - change: missing SX account record          
RV_ERPEQ EQU   X'10'               - add: person record error                   
RV_ERHDQ EQU   X'08'               - add: hire date invalid/missing             
RV_ERHSE EQU   X'04'               - add: (some) payroll history exists         
RV_ERTAQ EQU   X'02'               - add: invalid time approver                 
RV_ERXAQ EQU   X'01'               - add: invalid expenses approver             
RV_ERIN2 DS    XL1                 error indicator 2                            
RV_ERH1R EQU   X'80'               - high level 1R missing                      
RV_ERHSX EQU   X'40'               - high level SX missing                      
RV_ERH2P EQU   X'20'               - high level 2P missing                      
RV_ERSXE EQU   X'10'               - SX email invalid so skip                   
RV_ERE1R EQU   X'08'               - 1R already exists                          
RV_ERESX EQU   X'04'               - SX already exists                          
RV_ERE2P EQU   X'02'               - 2P already exists                          
RV_ERMSX EQU   X'01'               - no 1R off mapping for SX                   
RV_ERIN3 DS    XL1                 error indicator 3                            
RV_ERARM EQU   X'80'               - 1R active account missing                  
RV_ERLLS EQU   X'40'               - Invalid/Missing Limit list                 
RV_DA    DS    XL4                                                              
RV_1ROP  DS    XL1                                                              
RV_1RL1  DS    XL1                                                              
RV_1RL2  DS    XL1                                                              
RV_1RL3  DS    XL1                                                              
RV_1RL4  DS    XL1                                                              
RV_1RAD  DS    XL1                 Ledger allows draft                          
RV_1RS2  DS    XL1                 Ledger element status byte 2                 
RV_1RDP  DS    XL1                 Ledger dept position                         
RV_1RDL  DS    XL1                 Ledger dept length                           
RV_2POP  DS    XL1                 Ledger 2P office position                    
RV_2PL1  DS    XL1                 Ledger 2P level 1 length                     
RV_2PL2  DS    XL1                 Ledger 2P level 2 length                     
RV_2PL3  DS    XL1                 Ledger 2P level 3 length                     
RV_2PL4  DS    XL1                 Ledger 2P level 4 length                     
RV_2PAD  DS    XL1                 Ledger 2P allows draft                       
RV_2PS2  DS    XL1                 Ledger 2P status byte 2                      
RV_2PDP  DS    XL1                 Ledger 2P dept position                      
RV_2PDL  DS    XL1                 Ledger 2P dept length                        
RV_SXOP  DS    XL1                                                              
RV_SXL1  DS    XL1                                                              
RV_SXL2  DS    XL1                                                              
RV_SXL3  DS    XL1                                                              
RV_SXL4  DS    XL1                                                              
RV_SXAD  DS    XL1                                                              
RV_SXS2  DS    XL1                                                              
RV_SXDP  DS    XL1                 Ledger dept position                         
RV_SXDL  DS    XL1                 Ledger dept length                           
RV_SPIDC DS    CL8                                                              
RV_PIDC  DS    CL8                                                              
RV_CCTPC DS    CL8                                                              
RV_1ROCM DS    CL1                 1R office code mapped                        
RV_ULA   DS    CL14                                                             
RV_ULASX DS    CL14                                                             
RV_ULA2P DS    CL14                                                             
RV_ULA1R DS    CL14                                                             
RV_NAME  DS    CL36                                                             
RV_NAML  DS    XL1                                                              
RV_EMALN DS    XL1                                                              
RV_LNLN  DS    XL1                                                              
RV_FNLN  DS    XL1                                                              
RV_APIND DS    XL1                 approver record indicator                    
RV_APTEX EQU   X'80'               - timesheet = expenses approver              
RV_APTOF EQU   X'40'               - timesheet approver on file                 
RV_APTID EQU   X'20'               - timesheet approver is deleted              
RV_APTSK EQU   X'10'               - timesheet approver to be skipped           
RV_APXOF EQU   X'08'               - expenses approver on file                  
RV_APXSK EQU   X'04'               - expenses approver is deleted               
RV_APXID EQU   X'02'               - expenses approver to be skipped            
RV_APXUT EQU   X'01'               - expenses approver use time entry           
RV_APSEQ DS    XL1                                                              
RV_NCIND DS    XL1                 name change indicator                        
RV_NCLNQ EQU   X'80'               - last name changed                          
RV_NCFNQ EQU   X'40'               - first name changed                         
RV_NC1RQ EQU   X'20'               - 1R NAMELQ change                           
RV_NC2PQ EQU   X'10'               - 2P NAMELQ change                           
RV_NCSXQ EQU   X'08'               - SX NAMELQ change                           
RV_DPCD  DS    XL1                 Default pay code                             
RV_HRO   DS    PL6                 Hourly rate override or zeroes               
RV_CPYV  DS    XL(CPYVALSL)                                                     
RV_BLENQ EQU   *-RV_BLOCK                                                       
                                                                                
RECTYPE  DS    AL(L'RECTTYPE)      RECORD TYPE                                  
                                                                                
RQUPVAL  DS    2000X               see SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
         ORG   RQUPVAL                                                          
* Accout lock/unlock upload                                                     
RQ_ULA   DS    0CL14                                                            
RQ_UNIT  DS    CL1                                                              
RQ_LDGR  DS    CL1                                                              
RQ_ACC   DS    CL12                                                             
RQ_STA   DS    CL1                 Status type                                  
RQ_LOCK  EQU   C'Y'                - locked (Default if blank)                  
RQ_UNLK  EQU   C'N'                - unlocked                                   
         DS    XL(RQUPLNQ-(*-RQUPVAL))                                          
                                                                                
* Accounting Person Upload                                                      
*                                                                               
         ORG   RQUPVAL                                                          
RQP_ACT  DS    CL1                 Action                                       
RQP_ADQ  EQU   C'A'                - add                                        
RQP_CHQ  EQU   C'C'                - change                                     
RQP_PIN  DS    XL2                 PIN                                          
RQP_PER  DS    CL8                 Person code (Cost)                           
RQP_OFF  DS    CL2                 (1R) office code                             
RQP_DEP  DS    CL3                 (1R) department code                         
RQP_SUB  DS    CL3                 (1R) sub department code                     
RQP_HIR  DS    PL3                 Hire date                                    
RQP_FNA  DS    CL15                First name                                   
RQP_LNA  DS    CL36                Last name                                    
RQP_TAN  DS    XL2                 Timesheet approver PIN                       
RQP_TAC  DS    CL8                 same but as PID code                         
RQP_XAN  DS    XL2                 Expenses approver                            
RQP_XAC  DS    CL8                 same but as PID code                         
RQP_EMA  DS    CL58                EMail address                                
RQP_2PC  DS    CL12                2P account code override                     
RQP_SXC  DS    CL12                SX account code override                     
RQP_TRM  DS    PL3                 Termination date                             
RQP_HRO  DS    PL8                 Hourly rate override                         
RQP_LLN  DS    XL2                 Limlist equivalent PIN                       
RQP_LLC  DS    CL8                 Limlist equivalent PID                       
         DS    XL(RQUPLNQ-(*-RQUPVAL))                                          
*                                                                               
* BACS email status upload                                                      
         ORG   RQUPVAL                                                          
RQ5PSUP  DS    0CL(L'ACTKULA)      - Supplier                                   
RQ5PSUL  DS    CL2                                                              
RQ5PACT  DS    CL12                                                             
RQ5PPRF  DS    CL(L'TRNKREF)       - Payment reference                          
RQ5PDAT  DS    PL(L'TRNKDATE)      - Payment date                               
RQ5PEML  DS    CL50                - Supplier email address used                
RQ5PEMDT DS    PL(L'FFTREMDT)      - Email sent date                            
RQ5PEMTM DS    CL4                 - Email sent time  HHMM                      
RQ5PLNQ  EQU   *-RQ5PSUP                                                        
         DS    XL(RQUPLNQ-(*-RQUPVAL))                                          
                                                                                
ATSRERRS DS    XL1                 error area for TSAR errors                   
GAPLPARM DS    CL1                 GAPLST parameter                             
                                                                                
         DS    0H                  TSAR buffrec read areas                      
GAPAREA  DS    XL(GAPTLNQ)                                                      
                                                                                
         DS    0H                  TSAR buffrec read areas                      
GAPAREA2 DS    XL(GAPTLNQ)                                                      
                                                                                
         DS    0H                  Saved return data table                      
RETTAB   DS    XL(RT_LNQ*RT_MAXQ+1)                                             
                                                                                
         DS    0H                  Saved error messages and data                
ERRTAB   DS    XL500                                                            
                                                                                
SAVEL    EQU   *-SAVED                                                          
                                                                                
***********************************************************************         
* Included DSects                                                     *         
***********************************************************************         
                                                                                
       ++INCLUDE ACBRAWRKD                                                      
                                                                                
       ++INCLUDE DDACTIVD                                                       
ACTVELQ  EQU   X'F1'                                                            
ACTVPPQ  EQU   X'80'                                                            
                                                                                
***********************************************************************         
* Error table                                                         *         
***********************************************************************         
                                                                                
ET_D     DSECT                                                                  
ET_EOTQ  EQU   FF                  End of table indiciator                      
ET_LN    DS    X                   Length of this entry                         
ET_ERRNO DS    XL(L'ROUERRV)       Error number                                 
ET_FLDNM DS    XL2                 Field number error applies                   
ET_LN1Q  EQU   *-ET_D                                                           
ET_EXTRA DS    0C                  Extra error text                             
                                                                                
***********************************************************************         
* Return table                                                        *         
***********************************************************************         
                                                                                
RT_D     DSECT                                                                  
RT_TYPE  DS    CL1                 Type                                         
RT_EOTQ  EQU   FF                  End of table indiciator                      
RT_CPQ   EQU   C'1'                Cost person record                           
RT_HSQ   EQU   C'2'                Payroll history record                       
RT_1RQ   EQU   C'3'                1R account record                            
RT_2PQ   EQU   C'4'                2P account record                            
RT_SXQ   EQU   C'5'                SX account record                            
RT_TAQ   EQU   C'6'                Time approver record                         
RT_XAQ   EQU   C'7'                Expense approver record                      
RT_CODE  DS    CL23                Code key                                     
RT_LNQ   EQU   *-RT_D                                                           
RT_MAXQ  EQU   25                                                               
                                                                                
***********************************************************************         
* HELEN coverage                                                      *         
***********************************************************************         
                                                                                
HELEND   DSECT                                                                  
HELENL   DS    0CL13                                                            
HELFLEN  DS    CL1                 L'FILE NAME - 1                              
HELNAME  DS    CL8                 NAME                                         
HELMSIZE DS    CL2                 MAXIMUM RECORD SIZE                          
HELEDIS  DS    CL1                 DISPLACEMENT OF FIRST ELEMENT                
HELLDIS  DS    CL1                 DISPLACEMENT OF LENGTH                       
                                                                                
***********************************************************************         
* Others & End                                                        *         
***********************************************************************         
                                                                                
TWAD     DSECT                                                                  
         ORG   TWAUSER                                                          
SVVALS   DS    0X                  ** SAVED VALUES **                           
SVVALL   EQU   *-SVVALS                                                         
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040ACBRA21   05/15/20'                                      
         END                                                                    
