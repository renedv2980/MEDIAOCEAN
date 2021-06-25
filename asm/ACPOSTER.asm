*          DATA SET ACPOSTER   AT LEVEL 076 AS OF 03/28/13                      
*PHASE T62210A,*                                                                
         TITLE 'ACPOSTER - POSTING MODULE FOR MEDIA BILLING TRANSFER'           
         SPACE 2                                                                
*********************************************************************           
*        PARM 1  BYTE 0     NOT USED                                *           
*                BYTE 1-3   A(CONTROL DSECT) SEE ACPOSTD            *           
*                                                                   *           
*  ***** NOTE -- CALLING PROGRAM MUST SWITCH TO ACC SYSTEM BEFORE   *           
*        GOING TO ACPOSTER                                          *           
*                                                                   *           
* ACPOSTER HAS 3 MODES                                              *           
* 1. READS PROF RECS ONLY                                           *           
* 2. READS POST RECS ONLY                                           *           
* 3. READ POST RECORDS AND CREATES POSTINGS (MUST HAVE BILL RECORD  *           
*    AND MUST ASK FOR AMOUNTS)                                      *           
*  ACPOSTER ATTEMPTS TO READ THE FOLLOWING RECORDS                  *           
*  SJ,MI FOLLOWED BY VARIOUS COMBINATIONS OF THE POST MAINT RECS    *           
*  1- SYSTEM ONLY , 2 - SYS/MEDIA ONLY , 3- SYS,MEDIA,OFFICE GROUP  *           
*  4- SYS,MEDIA,OFF CODE 5- SYS,MEDIA,CLIENT 6- SYS,MEDIA,CLI,PROD  *           
*                                                                   *           
*********************************************************************           
         SPACE                                                                  
ACPOSTER CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKDL+POSTL,*POST,R9,R7,CLEAR=YES                               
         USING WORKD,RC                                                         
*                                                                               
         L     RA,0(R1)            A(CONTROL DSECT)                             
         USING ACPOSTD,RA                                                       
*                                                                               
         LR    R1,RC                                                            
         LA    R1,WORKDL(R1)                                                    
         ST    R1,APOSTS          A(EXPANDED POSTINGS)                          
*                                                                               
         L     RF,ACPACOM         A(COMFACS)                                    
         MVC   VDATAMGR,CDATAMGR-COMFACSD(RF)                                   
         MVC   VDATCON,CDATCON-COMFACSD(RF)                                     
         OC    ACPDTCN,ACPDTCN    NEED TO DO THIS BECAUSE PRINT'S               
         BZ    *+10               COMFACS LIST IS NOT RIGHT                     
         MVC   VDATCON,ACPDTCN                                                  
         RELOC RELO                                                             
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         EJECT                                                                  
         MVI   READ2,0            CLEAR READ INDICATOR 2                        
         MVI   READ3,0            CLEAR READ INDICATOR 3                        
         MVI   SVREAD,0           CLEAR SAVED READ INDICATOR                    
         MVI   RULREC,C'N'        NO POST RULES RECORD FOUND                    
         MVI   BPCRULE,C'N'       NO BPOST RULES RECORD FOUND                   
         MVI   LRTN,C'N'          DON'T INCLUDE ACCOUNT NAME                    
         ICM   RE,15,ACPSVACC     AREA FOR ACCOUNTS                             
         BZ    PST10              JUST READ ACC                                 
         ICM   RE,15,ACPNUM3      LENGTH OF ACPVACC                             
         BZ    PST10              JUST READ ACC                                 
         MVI   LRTN,C'Y'          INCLUDE ACCOUNT NAME IN RETURN                
*                                                                               
PST10    OC    ACPBILL,ACPBILL    IF NO BILL RECORD PRESENT                     
         BNZ   *+14                                                             
         MVC   BYTE,ACPTYPE2      SET RECORD TYPE REQUESTED                     
         B     PST20                                                            
         L     R6,ACPBILL         ELSE, R6=A(SPOT/NET BILL RECORD)              
         USING BILLRECD,R6                                                      
         BAS   RE,GETTYPE         SETS BYTE,READ3,ACPBTYPE,&PRTTYPE             
*                                                                               
PST20    CLI   BYTE,MPRKRTL       IF BILL IS RETAIL (FOR POST & PROF)           
         BE    PST25                                                            
         CLI   BYTE,MPRKRLMG      IF BILL IS LMG/REGIONAL                       
         BE    PST25                                                            
         CLI   BYTE,MPRKTRAD      IF BILL IS TRADE                              
         BE    PST25                                                            
         CLI   BYTE,MPRKTRAA      IF BILL IS AOR TRADE                          
         BE    PST25                                                            
         CLI   BYTE,MPRKMTRA      IF BILL IS MIDAS TRADE                        
         BE    PST25                                                            
         CLI   BYTE,MPRKPPB       IF BILL IS SPECIAL PRINT (PROF ONLY)          
         BNE   PST30                                                            
         CLI   ACPTYPE,0                                                        
         BE    PST30                                                            
PST25    MVC   READ2,BYTE         SAVE FOR SECOND READ                          
         MVI   BYTE,MPRKREG       READ REGULAR RECORD FIRST                     
*                                                                               
PST30    CLI   ACPTYPE,0          POST RECORDS?                                 
         BNE   *+8                NO - GO READ PROF RECORDS REQUESTED           
         BAS   RE,SETDFLT         SET DEFAULTS                                  
         BAS   RE,RDREC           READ POST/PROF RECORDS                        
*                                                                               
         CLI   ACPTYPE,0          SET SPOST ACCOUNTS FOR FINANCIAL              
         BNE   PST60                                                            
         CLI   BYTE,MPRKPPB                                                     
         BNE   *+8                                                              
         BAS   RE,SETSACC         SET SPOST ACCS (SUSP SR & REB SR)             
*                                                                               
PST60    TM    ACPIND,ACPEXP      IF ONLY WANT EXPRESSIONS                      
         BO    EXIT               DONE - EXIT                                   
         OC    ACPBILL,ACPBILL    CAN'T CONTINUE WITHOUT BILL                   
         BZ    EXIT                                                             
         CLI   ACPSYS,C'P'         LMG ONLY FOR SPOT/NET                        
         BE    PST70                                                            
         TM    BILSTAT3,BSTLMGQ                                                 
         BZ    PST70                                                            
         CLI   BPCRULE,C'Y'                                                     
         BE    PST70                                                            
         OI    ACPERRC,ACPNOBPR                                                 
PST70    CLI   RULREC,C'Y'        WERE THERE ANY POST RECORDS READ?             
         BE    PST80                                                            
         OI    ACPERR,ACPNOREC    NO POST RECORDS                               
         B     EXIT                                                             
*                                                                               
PST80    CLI   ACPTYPE,0          SET SPOST ACCOUNTS FOR FINANCIAL              
         BNE   PST85                                                            
         CLI   ACPSYS,C'P'                                                      
         BNE   *+8                                                              
         BAS   RE,SETCRCV         CHECK IF CLIENT RCVBL IS SR OR SJ             
*                                                                               
PST85    BAS   RE,SETCC           SET CC INCOME OVERRIDES IF NECESS             
         CLI   BYTE,MPRKRTL       RETAIL POST RECORDS                           
         BNE   *+8                                                              
         BAS   RE,SETRACC         SET RCV & COSTING ACCOUNTS                    
*                                                                               
         GOTO1 =A(EXPAND),DMCB,(RC),RR=RELO                                     
         GOTO1 =A(RESORT),DMCB,(RC),RR=RELO                                     
         CLC   =C'AOR',ACPBTYPE                                                 
         BNE   PST90                                                            
         GOTO1 =A(FIXPST),DMCB,(RC),RR=RELO                                     
PST90    GOTO1 =A(DBCR),DMCB,(RC),RR=RELO                                       
         GOTO1 =A(RTNPSTS),DMCB,RR=RELO                                         
*                                                                               
         CLI   ACPAFLG,C'T'        IF MAIN BILL POSTINGS                        
         BE    EXIT                                                             
         OC    ACPIOR,ACPIOR       AND INTERNAL PERCENTAGE SPECIFIED            
         BZ    EXIT                                                             
         CLI   INTERNAL,C'Y'       AND NO INTERNAL POSTINGS MADE                
         BE    EXIT                                                             
         OI    ACPERRC,ACPINT      SET ERROR                                    
EXIT     XIT1                                                                   
         SPACE                                                                  
YES      CR    RB,RB                                                            
         B     *+6                                                              
NO       LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*============================================================*                  
* GETTYPE - SETS   ACPBTYPE = MAIN BILL TYPES FOR POSTING    *                  
*                  (AOR/RET/REG/PRINT FOR PRINT PRTTYPE CAN  *                  
*                  BE SET TO FIN IF FINANCIAL BILL           *                  
*           SETS   BYTE = BINARY TYPE FOR POST MAINT RECORDS *                  
*============================================================*                  
*                                                                               
GETTYPE  NTR1                                                                   
         CLI   ACPTYPE,0           IF READING POSTING RECORDS                   
         BNE   *+8                                                              
         MVI   READ3,MPRKPST       SET 3RD READ = TPOST(ALL BILL TYPES)         
         XC    PRTTYPE,PRTTYPE     CLEAR OUT PRINT BILL TYPE                    
         CLI   ACPSYS,C'P'         IF PRINT SYSTEM?                             
         BE    GETTP40             YES                                          
*                                                                               
         CLI   BRETAIL,0           RETAIL BILL?                                 
         BE    GETTP10                                                          
         MVC   ACPBTYPE,=C'RET'                                                 
         MVI   BYTE,3                                                           
         B     GETTPX                                                           
GETTP10  TM    BILSTAT,X'20'       AOR CLIENT BILL?                             
         BO    *+12                                                             
         TM    BILSTAT,X'10'       TRUE AOR CLIENT                              
         BNO   GETTP20                                                          
         MVC   ACPBTYPE,=C'AOR'                                                 
         MVI   BYTE,8              AOR UFN BILL (READ UANPOST)                  
         TM    BILSTAT,BSTSNETQ                                                 
         BO    GETTPX                                                           
         MVI   BYTE,7              AOR UFC BILL (READ UACPOST)                  
         TM    BILSTAT,BSTSCOMQ                                                 
         BO    GETTPX                                                           
         MVI   BYTE,13             AOR TRADE (XAPOST)                           
         CLI   ACPTYPE,1           IF READING PROFILE RECORDS                   
         BNE   *+8                 THEN READ XPROF BECAUSE XAPROF DOES          
         MVI   BYTE,12             NOT EXIST                                    
         TM    BILSTAT3,BSTTRCNQ                                                
         BO    GETTPX                                                           
         MVI   BYTE,2              JUST AOR MAIN BILL (READ APOST)              
         B     GETTPX                                                           
*                                                                               
GETTP20  MVC   ACPBTYPE,=C'REG'    REGULAR BILLING FOR NOW                      
         MVI   BYTE,6              REG UFN BILL (READ UNPOST)                   
         TM    BILSTAT,BSTSNETQ                                                 
         BO    GETTPX                                                           
         MVI   BYTE,5              REG UFC BILL (READ UCPOST)                   
         TM    BILSTAT,BSTSCOMQ                                                 
         BO    GETTPX                                                           
         MVI   BYTE,10             REGULAR DIFF BILL (READ DPOST)               
         TM    BILSTAT2,BSTAMAQ                                                 
         BO    GETTPX                                                           
         MVI   BYTE,11             REGIONAL/LMG RETAIL BILLING                  
         TM    BILSTAT3,BSTLMGQ                                                 
         BO    GETTPX                                                           
         MVI   BYTE,12             TRADE BILL                                   
         TM    BILSTAT3,BSTTRCNQ                                                
         BO    GETTPX                                                           
         MVI   BYTE,14             MIDAS TRADE (GROUPM)                         
         TM    BILSTAT3,BSTMBARQ                                                
         BO    GETTPX                                                           
         MVI   BYTE,1              REGULAR BIL (READ POST)                      
         B     GETTPX                                                           
         EJECT                                                                  
GETTP40  CLI   PBRETAIL,0           RETAIL?                                     
         BE    GETTP50                                                          
         MVC   ACPBTYPE,=C'RET'                                                 
         MVI   BYTE,3                                                           
         B     GETTP70                                                          
GETTP50  TM    PBILCMSW,X'20'       AOR CLIENT BILL?                            
         BO    *+12                                                             
         TM    PBILCMSW,X'10'       TRUE AOR CLIENT                             
         BNO   GETTP60                                                          
         MVC   ACPBTYPE,=C'AOR'                                                 
         MVI   BYTE,8              AOR UFN BILL (READ UANPOST)                  
         TM    PBILCMSW,X'08'                                                   
         BO    GETTPX                                                           
         MVI   BYTE,7              AOR UFC BILL (READ UACPOST)                  
         TM    PBILCMSW,X'01'                                                   
         BO    GETTPX                                                           
         MVI   BYTE,2              JUST AOR MAIN BILL (READ APOST)              
         B     GETTP70                                                          
*                                                                               
GETTP60  MVC   ACPBTYPE,=C'REG'                                                 
         MVI   BYTE,6              REG UFN BILL (READ UNPOST)                   
         TM    PBILCMSW,X'08'                                                   
         BO    GETTP70                                                          
         MVI   BYTE,5              REG UFC BILL (READ UCPOST)                   
         TM    PBILCMSW,X'01'                                                   
         BO    GETTP70                                                          
         MVI   BYTE,14             MIDAS TRADE (GROUPM)                         
         TM    PBILSTAT,X'20'                                                   
         BO    GETTP70                                                          
         MVI   BYTE,1              REG BILL                                     
*                                                                               
GETTP70  LA    R1,PBILLEL           ELEMENTS                                    
         SR    R0,R0                                                            
GETTP78  CLI   0(R1),0                                                          
         BE    GETTPX                                                           
         CLI   0(R1),X'09'          FINANCIAL ELEMENT?                          
         BE    GETTP80                                                          
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     GETTP78                                                          
*                                                                               
GETTP80  TM    PBILLIND,X'01'       FINNANCIAL - PRINT                          
         BNO   GETTPX                                                           
         MVC   PRTTYPE,=C'FIN'                                                  
         MVI   BYTE,4                                                           
*                                                                               
GETTPX   B     EXIT                                                             
         EJECT                                                                  
*========================================================*                      
* SETDFLT - READING POST RECORDS - FIRST SET DEFAULTS    *                      
*========================================================*                      
*                                                                               
SETDFLT  NTR1                                                                   
         OC    ACPCLT,ACPCLT      CAN'T READ PROD UNIT/LEDGER REC               
         BZ    *+8                UNLESS CLIENT SPECIFIED                       
         BAS   RE,PROD                                                          
         OC    ACPSYS,ACPSYS      NEED SYSTEM                                   
         BZ    SETDF10                                                          
         OC    ACPMED,ACPMED      AND MEDIA                                     
         BZ    *+8                                                              
         BAS   RE,MI              INORDER TO READ MI RECORD                     
*                                                                               
SETDF10  XC    PRVCODE,PRVCODE    DON'T READ PROV LEVEL FOR GST                 
         OC    ACPBILL,ACPBILL    IF BILL PRESENT - USE GST FROM BILL           
         BNZ   SETDF15                                                          
         MVI   OUTCODE,C'S'       OTHERWISE DEFAULT TO S                        
         CLI   ACPGSTO,C' '       IF NO GST CODE (FROM CLI/PRD)                 
         BNH   *+10                                                             
SETDF15  MVC   OUTCODE,ACPGSTO                                                  
         MVI   RULTYPE,C'O'       READ FOR OUTPUT ACCOUNT                       
         MVI   SVTBYTE,MBTTGST                                                  
         BAS   RE,GETVAT          GET DEFAULT GST OUTPUT ACCOUNT                
*                                                                               
         CLI   BYTE,MPRKAOR       FOR AOR POST RECORDS                          
         BNE   SETDF18                                                          
         MVI   RULTYPE,C'I'       READ FOR INPUT ACCOUNT                        
         MVI   SVTBYTE,MBTTGSTI                                                 
         BAS   RE,GETVAT          SET AOR GST INPUT ACCOUNT                     
*                                                                               
SETDF18  OC    ACPBILL,ACPBILL    IF BILL PRESENT                               
         BNZ   *+12                                                             
         CLI   BYTE,MPRKPST       OR IF READING TPOST MAINT RECORDS             
         BNE   SETDFX                                                           
*                                                                               
         MVI   RULTYPE,C'O'       READ FOR OUTPUT ACCOUNT                       
         LA    R0,ACPMCODE        R0=(N'ENTRIES)                                
         LA    R1,ACPPSTO         R1=A(PST OUTPUT VALUES)                       
SETDF20  CLI   0(R1),0            TEST PROVIDENCE                               
         BE    SETDF25                                                          
         MVC   PRVCODE,0(R1)      SET PROVIDENCE                                
         MVC   SVTBYTE,2(R1)      SET EQUATED ELEMENT TYPE                      
         MVC   OUTCODE,3(R1)      SET TAX CODE                                  
         BAS   RE,GETVAT          GET DEFAULT PST OUTPUT ACCOUNT                
SETDF25  LA    R1,L'ACPPSTO(R1)                                                 
         BCT   R0,SETDF20                                                       
*                                                                               
SETDF30  LA    R1,ACPPSTI         R1=A(PST INPUT VALUES)                        
         LA    R0,ACPMCODE        R0=(N'ENTRIES)                                
         MVI   RULTYPE,C'I'       READ FOR INPUT ACCOUNT                        
SETDF35  CLI   0(R1),0            TEST PROVIDENCE                               
         BE    SETDF40                                                          
         MVC   PRVCODE,0(R1)      SET PROVIDENCE                                
         MVC   SVTBYTE,2(R1)      SET EQUATED ELEMENT CODE                      
         MVC   OUTCODE,3(R1)      SET TAX CODE                                  
         BAS   RE,GETVAT          GET DEFAULT PST INPUT ACCOUNT                 
SETDF40  LA    R1,L'ACPPSTI(R1)                                                 
         BCT   R0,SETDF35                                                       
*                                                                               
SETDFX   B     EXIT                                                             
         EJECT                                                                  
*========================================================*                      
* RDREC - READ POST/PROF MAINT RECORDS                   *                      
*========================================================*                      
*                                                                               
RDREC    NTR1                                                                   
         MVC   SVREAD,BYTE        SAVE READ INDICATOR                           
*                                                                               
RDREC5   LA    R3,1               FIRST KEY COMPONENT                           
         OC    ACPCMPC,ACPCMPC    MUST HAVE BOTH COMPANY CODES                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OC    ACPCMPC2,ACPCMPC2  IF SPLIT-FILES CMPC2 WILL BE DIFF             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R3,1(R3)           TEST NEXT KEY COMPONENT                       
*                                                                               
         XC    KEY,KEY            BUILD POST MAINT KEY                          
         LA    R4,KEY                                                           
         USING MPRRECD,R4                                                       
         MVI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         MVI   MPRKSUB,MPRKSUBQ   X'01'                                         
         MVC   MPRKCPY,ACPCMPC    COMPANY CODE FOR NATIVE FILE                  
         MVC   MPRKPRO,BYTE       BILLING TYPE                                  
*                                                                               
RDREC10  CH    R3,=H'2'           WORKING ON 2ND KEY COMPONENT?                 
         BNE   RDREC20                                                          
         OC    ACPSYS,ACPSYS      YES - SYSTEM GIVEN?                           
         BNZ   *+6                                                              
         DC    H'0'               MUST HAVE SYSTEM                              
         MVC   MPRKALPH,ACPALPH   AGENCY FOR MEDIA SPLIT FILES                  
         MVC   MPRKSYS,ACPSYS     YES- READ POST MAINT REC                      
         MVC   LEVEL,=C'SYS'                                                    
         MVC   ABUFF,ACPSYSBF     SYSTEM BUFFER                                 
         B     PSTIT                                                            
*                                                                               
RDREC20  CH    R3,=H'3'           WORKING ON 3RD KEY COMPONENT?                 
         BNE   RDREC30                                                          
         OC    ACPMED,ACPMED      YES- MEDIA CODE GIVEN?                        
         BZ    PSTNXT             NO - TRY NEXT KEY FIELD                       
         MVC   MPRKMED,ACPMED     YES- READ POST MAINT REC                      
         MVC   LEVEL,=C'MED'                                                    
         MVC   ABUFF,ACPMEDBF     MEDIA BUFFER                                  
         B     PSTIT                                                            
*                                                                               
RDREC30  CH    R3,=H'4'           WORKING ON 4TH KEY COMPONENT?                 
         BNE   RDREC40                                                          
         OC    ACPOFG,ACPOFG      YES - OFFICE GROUP GIVEN?                     
         BZ    PSTNXT             NO - TRY NEXT KEY FIELD                       
         MVC   MPRKOFC,ACPOFG     YES- READ POST MAINT REC                      
         MVC   ABUFF,ACPOFGBF     OFFICE GROUP BUFFER                           
         MVC   LEVEL,=C'OFG'                                                    
         B     PSTIT                                                            
*                                                                               
RDREC40  CH    R3,=H'5'           WORKING ON 5TH KEY COMPONENT?                 
         BNE   RDREC50                                                          
         OC    ACPOFC,ACPOFC      YES- OFFICE CODE GIVEN?                       
         BZ    PSTNXT             NO - TRY NEXT KEY FIELD                       
         MVC   MPRKOFC,=C'  '                                                   
         MVC   MPRKOFC(1),ACPOFC  YES- READ POST MAINT REC                      
         MVC   ABUFF,ACPOFCBF     OFFICE BUFFER                                 
         MVC   LEVEL,=C'OFC'                                                    
         B     PSTIT                                                            
*                                                                               
RDREC50  CH    R3,=H'6'           WORKING ON 6TH KEY COMPONENT?                 
         BNE   RDREC60                                                          
         OC    ACPCLT,ACPCLT      YES- CLIENT GIVEN?                            
         BZ    PSTNXT             NO - TRY NEXT KEY FIELD                       
         MVC   MPRKCLI,ACPCLT     YES- READ POST MAINT REC                      
         XC    MPRKOFC,MPRKOFC    CAN'T HAVE OFC OR OFG                         
         MVC   ABUFF,ACPCLTBF     CLIENT BUFFER                                 
         MVC   LEVEL,=C'CLT'                                                    
         B     PSTIT                                                            
*                                                                               
RDREC60  CH    R3,=H'7'           WORKING ON 7TH KEY COMPONENT?                 
         BNE   PSTNXT                                                           
         OC    ACPPRD,ACPPRD      YES- PRODUCT GIVEN?                           
         BZ    PSTNXT             NO - TRY NEXT KEY FIELD                       
         MVC   MPRKPRD,ACPPRD     YES- READ POST MAINT REC                      
         MVC   ABUFF,ACPPRDBF     PRODUCT BUFFER                                
         MVC   LEVEL,=C'PRD'                                                    
         B     PSTIT                                                            
*                                                                               
PSTIT    BAS   RE,PSTREC          READ POST MAINT RECORD FOR GIVEN LVL          
*                                                                               
PSTNXT   LA    R3,1(R3)           GET NEXT KEY COMPONENT                        
         LA    R1,ACPNUM          MAX NUMBER OF KEY COMPONENTS                  
         CR    R3,R1                                                            
         BNH   RDREC10                                                          
*                                                                               
         CLI   READ2,0            ANY OTHER READS?                              
         BE    PSTNXT3            NO - GO EXPAND TO POSTINGS                    
         MVC   BYTE,READ2         SET SECOND TYPE TO READ                       
         MVC   SVREAD,READ2       SAVE OFF READ                                 
         MVI   READ2,0            TO END LOOP                                   
         B     RDREC5             GO READ RECORDS                               
*                                                                               
PSTNXT3  CLI   READ3,0            ANY OTHER READS?                              
         BE    RDRECX                                                           
         MVC   BYTE,READ3         SET THIRD TYPE TO READ                        
         MVI   READ3,0            TO END LOOP                                   
         B     RDREC5                                                           
*                                                                               
RDRECX   MVC   BYTE,SVREAD         RESET READ INDICATOR (1ST OR 2ND)            
         B     EXIT                                                             
         EJECT                                                                  
*========================================================*                      
* PROD - READS PRODUCTION LEDGER CLIENT/PRODUCT RECORD   *                      
*========================================================*                      
*                                                                               
PROD     NTR1                                                                   
         MVI   WORK,C'C'          READING FOR CLIENT LEVEL FIRST                
         MVI   WORK+1,C'N'        NO ACCOUNTS FOUND                             
         MVI   PRDREC,C'N'        PRODUCT LEVEL RECORD DOESN'T EXIST            
         MVI   PRD1C,C'N'         PRD LVL 1C DOESN'T EXIST                      
PROD2    MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACPCMPC),ACPCMPC2  COMPANY CODE FROM OTHER ACCFILE         
         MVC   KEY+1(L'ACPSPROD),ACPSPROD                                       
         MVC   KEY+3(L'ACPCLT),ACPCLT                                           
         CLI   WORK,C'C'          IF READING AT CLIENT LEVEL                    
         BE    *+10               SKIP MOVING PRODUCT IN KEY                    
         MVC   KEY+6(L'ACPPRD),ACPPRD                                           
         MVI   RDSE,2             READ FROM OTHER SE IF APPLICABLE              
         GOTO1 =A(READ),DMCB,(RC),RR=RELO                                       
         BNE   PROD30                                                           
         CLI   WORK,C'P'                                                        
         BNE   *+8                                                              
         MVI   PRDREC,C'Y'        PRODUCT LEVEL RECORD EXISTS                   
         LA    R4,IOAREA                                                        
         LA    R1,ACCORFST                                                      
         AR    R4,R1                                                            
         USING PPRELD,R4                                                        
         SR    R0,R0                                                            
PROD5    CLI   0(R4),0            END OF RECORD?                                
         BE    PROD30                                                           
         CLI   0(R4),PPRELQ       X'24'                                         
         BE    PROD10                                                           
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     PROD5                                                            
*                                                                               
         USING ACPRTND,R5                                                       
PROD10   LA    R2,PRODTBL         RECEIVABLES ROW                               
         MVC   TBYTE,0(R2)                                                      
         BAS   RE,GETROW          GET R5 TO PT TO CORRECT ROW                   
         CLI   ACPSYS,C'P'        IF PRINT BILL                                 
         BNE   PROD15                                                           
         CLI   ACPRCLT,C'Y'       POST TO CLT LVL RCVBL ONLY                    
         BNE   PROD15                                                           
         CLI   WORK,C'P'          YES - ARE WE CLT LVL?                         
         BE    PROD20             NO - SKIP THIS LVL                            
PROD15   CLC   PPRRECV+1(L'PPRRECV-1),SPACES                                    
         BNH   PROD20                                                           
         MVC   TEMPACC(L'PPRRECV-1),PPRRECV+1                                   
         BAS   RE,GETDOC          GET CREDIT OR DEBIT FROM TBYTE                
         MVC   ACPSTAT,TBYTE                                                    
         MVC   ACPBTYP,0(R2)                                                    
         MVI   WORK+1,C'Y'        ACCOUNT FOUND                                 
         MVC   ACPACC,TEMPACC                                                   
         MVC   ACPLVL(L'ACPSPROD),ACPSPROD                                      
*                                                                               
PROD20   LA    R2,1(R2)           COSTING ROW                                   
         CLI   ACPCOST,C'Y'       COSTING ALLOWED?                              
         BNE   PROD25                                                           
         MVC   TBYTE,0(R2)                                                      
         BAS   RE,GETROW          GET CORRECT ROW                               
         MVC   ACPBTYP,0(R2)                                                    
         BAS   RE,GETDOC          GET CREDIT OR DEBIT FROM TBYTE                
         MVC   ACPSTAT,TBYTE                                                    
         CLC   PPRCOST+1(L'PPRCOST-1),SPACES                                    
         BNH   PROD23                                                           
         CLI   WORK,C'P'                                                        
         BNE   *+8                                                              
         MVI   PRD1C,C'Y'         PRODUCT LEVEL 1C  EXISTS                      
         MVI   WORK+1,C'Y'        ACCOUNT FOUND                                 
         MVC   ACPACC,PPRCOST+1                                                 
         MVC   ACPLVL(L'ACPSPROD),ACPSPROD                                      
*                                                                               
PROD23   LA    R2,1(R2)            INTERNAL COSTING ROW                         
         MVC   TBYTE,0(R2)                                                      
         BAS   RE,GETROW                                                        
         MVC   ACPBTYP,0(R2)                                                    
         BAS   RE,GETDOC           SET CREDIT OR DEBIT                          
         MVC   ACPSTAT,TBYTE                                                    
         CLC   PPRCOST+1(L'PPRCOST-1),SPACES                                    
         BNH   PROD25                                                           
         MVC   ACPACC,PPRCOST+1                                                 
         MVC   ACPLVL(L'ACPSPROD),ACPSPROD                                      
*                                                                               
PROD25   CLC   PPRGAOFF,SPACES    ACC OFFICE                                    
         BNH   PROD30                                                           
         MVC   ACCOFF,PPRGAOFF                                                  
*                                                                               
PROD30   CLI   WORK,C'C'          IF CLIENT LEVEL?                              
         BNE   PROD40                                                           
         MVI   WORK,C'P'          TRY PRODUCT LEVEL NEXT                        
         B     PROD2                                                            
*                                                                               
PROD40   CLI   WORK+1,C'Y'        ANYTHING FOUND                                
         BE    PROD50                                                           
         OI    ACPERR,X'80'       NO SJ RECORD OR X'24' ELEM                    
*                                                                               
PROD50   CLI   ACPSJPRD,C'Y'      CHECK FOR PRD RECORD EXISTANCE                
         BNE   PROD55                                                           
         CLI   PRDREC,C'Y'        DOES IT EXIST?                                
         BE    *+8                                                              
         OI    ACPERR,ACPNOPRD    NOPE                                          
*                                                                               
PROD55   CLI   ACP1CPRD,C'Y'      CHECK FOR PRODUCT LEVEL 1C EXISTANCE          
         BNE   PRODX                                                            
         CLI   PRD1C,C'Y'         DOES IT EXIST?                                
         BE    *+8                                                              
         OI    ACPERR,ACPNOP1C    NOPE                                          
PRODX    XIT1                                                                   
         DROP  R4                                                               
         SPACE                                                                  
PRODTBL  DC    AL1(MBTTRCV,MBTTCOS,MBTTINTC)                                    
         EJECT                                                                  
*==========================================================*                    
* MI   - READS MEDIA INTERFACE RECORD -                    *                    
*        IF NONE - USES DEFAULT VALUES                     *                    
*==========================================================*                    
*                                                                               
MI       NTR1                                                                   
         CLI   ACPMI,C'Y'         DO THEY USE MI RECORDS?                       
         BNE   MI10               NO - USE DEFAULTS                             
         LA    R4,KEY                                                           
         USING MINRECD,R4                                                       
         MVC   MINKEY,SPACES                                                    
         MVI   MINKTYP,MINKTYPQ    X'08'                                        
         MVC   MINKCPY,ACPCMPC2    COMPANY CODE FROM OTHER ACCFILE              
         MVC   MINKMED(1),ACPSYS   SYSTEM                                       
         MVC   MINKMED+1(1),ACPMED MEDIA                                        
         MVI   RDSE,2             READ FROM OTHER SE IF APPLICABLE              
         GOTO1 =A(READ),DMCB,(RC),RR=RELO                                       
         BE    *+12                                                             
         OI    ACPERR,ACPNOMI     ERROR ON MI BUT NO RECORD                     
         B     MIX                                                              
         DROP  R4                                                               
         LA    R4,IOAREA                                                        
         LA    R1,ACCORFST                                                      
         AR    R4,R1                                                            
         USING MDIELD,R4                                                        
         SR    R0,R0                                                            
MI5      CLI   0(R4),0             END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),MDIELQ       X'19'                                         
         BE    MI10                                                             
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     MI5                                                              
*                                                                               
         USING ACPRTND,R5                                                       
MI10     LA    R2,MITBL           MI TABLE OF ROW VALUES                        
         MVC   TBYTE,0(R2)        INCOME ROW                                    
         BAS   RE,GETROW          PT R5 TO CORRECT ROW IN TABLE                 
         MVC   ACPBTYP,0(R2)                                                    
         BAS   RE,GETDOC          GET CREDIT OR DEBIT FROM TBYTE                
         MVC   ACPSTAT,TBYTE                                                    
         CLI   ACPMI,C'Y'                                                       
         BE    MI15                                                             
         MVC   ACPLVL(2),=C'DF'                                                 
         MVC   ACPACC(2),=C'SI'                                                 
         MVC   ACPACC+2(1),ACPSYS                                               
         MVC   ACPACC+3(1),ACPMED                                               
         B     MI20                                                             
MI15     CLC   MDICOMM,SPACES     IF NO INCOME DON'T MOVE IN MI                 
         BNH   MI20                                                             
         MVC   ACPLVL(2),=C'MI'                                                 
         MVC   ACPACC,MDICOMM                                                   
*                                                                               
MI20     LA    R2,1(R2)           NET ROW                                       
         MVC   TBYTE,0(R2)                                                      
         BAS   RE,GETROW          PT R5 TO CORRECT ROW IN TABLE                 
         MVC   ACPBTYP,0(R2)                                                    
         BAS   RE,GETDOC          GET CREDIT OR DEBIT FROM TBYTE                
         MVC   ACPSTAT,TBYTE                                                    
         CLI   ACPMI,C'Y'                                                       
         BE    MI25                                                             
         MVC   ACPLVL(2),=C'DF'                                                 
         MVC   ACPACC(2),=C'SZ'                                                 
         MVC   ACPACC+2(1),ACPSYS                                               
         MVC   ACPACC+3(1),ACPMED                                               
         B     MI30                                                             
MI25     CLC   MDICNTL,SPACES                                                   
         BNH   MI30                                                             
         MVC   ACPLVL(2),=C'MI'                                                 
         MVC   ACPACC,MDICNTL                                                   
         B     MI30                                                             
*                                                                               
MI30     LA    R2,1(R2)           CASH DISC ROW                                 
         CLI   ACPSYS,C'S'                                                      
         BE    MI40               NO CD FOR FOR SPOT                            
         CLI   ACPSYS,C'N'                                                      
         BE    MI40               AND NO CD FOR NET                             
         MVC   TBYTE,0(R2)                                                      
         BAS   RE,GETROW          PT R5 TO CORRECT ROW IN TABLE                 
         MVC   ACPBTYP,0(R2)                                                    
         BAS   RE,GETDOC          GET CREDIT OR DEBIT FROM TBYTE                
         MVC   ACPSTAT,TBYTE                                                    
         CLI   ACPMI,C'Y'                                                       
         BE    MI35                                                             
         MVC   ACPLVL(2),=C'DF'                                                 
         MVC   ACPACC(4),=C'SIMP'                                               
         B     MI40                                                             
MI35     CLC   MDICSHD,SPACES                                                   
         BNH   MI40                                                             
         MVC   ACPLVL(2),=C'MI'                                                 
         MVC   ACPACC,MDICSHD                                                   
         B     MI40                                                             
*                                                                               
MI40     LA    R2,1(R2)           BILLING ROW                                   
         MVC   TBYTE,0(R2)                                                      
         BAS   RE,GETROW          PT R5 TO CORRECT ROW IN TABLE                 
         MVC   ACPBTYP,0(R2)                                                    
         CLI   ACPCOST,C'Y'       BILLING ALLOWED?                              
         BNE   MI50                                                             
         BAS   RE,GETDOC          GET CREDIT OR DEBIT FROM TBYTE                
         MVC   ACPSTAT,TBYTE                                                    
         MVC   ACPACC(2),=C'11'                                                 
         CLI   ACPMI,C'Y'                                                       
         BE    MI45                                                             
         BAS   RE,GETFILT         GETFILT SETS TBYTE                            
         MVC   ACPACC+2(1),TBYTE                                                
         MVC   ACPLVL(2),=C'DF'                                                 
         B     MI50                                                             
MI45     CLC   MDICOST,SPACES                                                   
         BNH   MI50                                                             
         MVC   ACPLVL(2),=C'MI'                                                 
         MVC   ACPACC+2(L'MDICOST),MDICOST                                      
         B     MI50                                                             
*                                                                               
MI50     LA    R2,1(R2)           REVENUE ROW                                   
         MVC   TBYTE,0(R2)                                                      
         BAS   RE,GETROW          PT R5 TO CORRECT ROW IN TABLE                 
         MVC   ACPBTYP,0(R2)                                                    
         CLI   ACPCOST,C'Y'       REVENUE ALLOWED                               
         BNE   MIX                                                              
         BAS   RE,GETDOC          GET CREDIT OR DEBIT FROM TBYTE                
         MVC   ACPSTAT,TBYTE                                                    
         MVC   ACPACC(2),=C'12'                                                 
         CLI   ACPMI,C'Y'                                                       
         BE    MI55                                                             
         BAS   RE,GETFILT                                                       
         MVC   ACPACC+2(1),TBYTE                                                
         MVC   ACPLVL(2),=C'DF'                                                 
         B     MIX                                                              
MI55     CLC   MDICOST,SPACES                                                   
         BNH   MIX                                                              
         MVC   ACPLVL(2),=C'MI'                                                 
         MVC   ACPACC+2(L'MDICOST),MDICOST                                      
MIX      XIT1                                                                   
         SPACE 1                                                                
MITBL    DC    AL1(MBTTINC,MBTTNET,MBTTCD,MBTTBIL,MBTTREV)                      
         SPACE 1                                                                
         EJECT                                                                  
*=======================================================*                       
* GETFILT - GETS RSTFILT3 FOR DEFAULT IN BILLING & REV  *                       
*     XIT - TBYTE SET                                   *                       
*=======================================================*                       
*                                                                               
GETFILT  NTR1                                                                   
         MVI   TBYTE,C' '                                                       
         LA    R4,KEY                                                           
         USING ACTKEY,R4                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,ACPCMPC2     COMPANY CODE FROM OTHER ACCFILE             
         MVC   ACTKUNT(2),=C'SI'    *** USE INCOME UNIT/LEDGER ***              
         MVC   ACTKACT(1),ACPSYS    SYSTEM                                      
         MVC   ACTKACT+1(1),ACPMED  MEDIA                                       
         MVI   RDSE,2               READ FROM OTHER SE IF APPLICABLE            
         GOTO1 =A(READ),DMCB,(RC),RR=RELO                                       
         BNE   GETFILTX                                                         
         DROP  R4                                                               
         LA    R4,IOAREA                                                        
         LA    R1,ACCORFST                                                      
         AR    R4,R1                                                            
         USING RSTELD,R4                                                        
GETFILT3 CLI   0(R4),0            END OF RECORD                                 
         BE    GETFILTX                                                         
         CLI   0(R4),RSTELQ       RECORD STATUS ELE?                            
         BE    GETFILT5                                                         
         SR    R0,R0                                                            
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     GETFILT3                                                         
*                                                                               
GETFILT5 MVC   TBYTE,RSTCOSTG     COSTING GROUP                                 
GETFILTX XIT1                                                                   
         SPACE                                                                  
*=======================================================*                       
* GETROW - GIVEN ROW NUMBER PTS R5 TO CORRECT POSITION  *                       
*=======================================================*                       
*                                                                               
GETROW   NTR1                                                                   
         ZIC   R1,TBYTE           ROW NUMBER                                    
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         LA    RF,ACPRTNL         LENGTH OF TABLE                               
         MR    RE,R1              GET TOTAL DISPLACEMENT FROM START             
         L     R5,ACPPOST                                                       
         AR    R5,RF              R5 = CORRECT ROW                              
         XIT1  REGS=(R5)                                                        
         EJECT                                                                  
*============================================================*                  
* GETVAT  - SET GST/PST INPUT AND OUTPUT ELEMENTS            *                  
*============================================================*                  
*                                                                               
GETVAT   NTR1                                                                   
         OC    PRVCODE,PRVCODE     IF PROVIDENCE PROVIDED                       
         BZ    GETVAT10                                                         
         MVI   RULES,C'P'          READ BY PROVINCE/OFFICE/EFFECT DT            
         B     GETVAT20                                                         
*                                                                               
GETVAT5  MVI   RULES,C'V'          READ BY PROVINCE/EFFECTIVE DATE              
         B     GETVAT20                                                         
*                                                                               
GETVAT10 MVI   RULES,C'O'          READ BY OFFICE/EFFECTIVE DATE                
         B     GETVAT20                                                         
*                                                                               
GETVAT15 MVI   RULES,C'E'          READ BY EFFECTIVE DATE                       
*                                                                               
GETVAT20 BAS   RE,RDRULES          READ ACCOUNTING RECORD                       
         LA    R4,IOAREA                                                        
         USING TAXRECD,R4                                                       
         CLC   KEY(TAXKDATE-TAXKEY),0(R4)                                       
         BNE   GETVAT50            CHECK MATCH FOUND                            
*                                                                               
         LA    R1,ACCORFST                                                      
         AR    R4,R1                                                            
         USING TAXELD,R4                                                        
         BAS   RE,FILTEL           FIND ELEMENT/SET ACCOUNT                     
         BE    GETVATX                                                          
*                                                                               
GETVAT50 CLI   RULES,C'P'         IF NO LUCK WITH PROV/OFFICE/DATE              
         BE    GETVAT5            TRY PROV/DATE NEXT                            
         CLI   RULES,C'V'         IF NO LUCK WITH PROV/DATE                     
         BE    GETVAT10           TRY OFFICE/DATE NEXT                          
         CLI   RULES,C'O'         IF NO LUCK WITH OFFICE/DATE                   
         BE    GETVAT15           TRY DATE                                      
*                                                                               
GETVATX  B     EXIT                                                             
         EJECT                                                                  
* FILTEL - ROUTINES LOOPS THROUGH ACC RECORD LOOKING FOR ACCOUNT                
*                                                                               
         USING TAXELD,R4                                                        
FILTEL   NTR1                                                                   
*                                                                               
FILTEL5  CLI   0(R4),0            END OF RECORD?                                
         BE    NO                                                               
         CLI   RULTYPE,C'O'                                                     
         BNE   FILTEL10                                                         
         CLI   0(R4),TAXOELQ      X'DF' - ELEMENT FOR OUTPUT ACCOUNT            
         BNE   FILTEL50                                                         
         CLC   TAXCODE,OUTCODE    MATCH ON TAX CODE                             
         BNE   FILTEL50                                                         
         BAS   RE,SETVAT                                                        
         B     YES                                                              
*                                                                               
FILTEL10 CLI   0(R4),TAXIELQ       X'DE' - ELEMENT FOR INPUT ACCOUNT            
         BNE   FILTEL50                                                         
         OC    ACPBILL,ACPBILL     IF BILL PRESENT MATCH ON TAX CODE            
         BZ    FILTEL15                                                         
         CLC   TAXCODE,OUTCODE                                                  
         BNE   FILTEL50                                                         
         B     *+12                                                             
FILTEL15 TM    TAXINDS,TAXIDFLT    ELSE, GET DEFAULT TAX CODE                   
         BZ    FILTEL50                                                         
         BAS   RE,SETVAT                                                        
         B     YES                                                              
*                                                                               
FILTEL50 XR    R0,R0                                                            
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     FILTEL5                                                          
         DROP  R4                                                               
         EJECT                                                                  
* SETVAT - ROUTINE SETS DEFAULT ACCOUNT INFO INTO AREA                          
*                                                                               
         USING ACPRTND,R5                                                       
         USING TAXELD,R4                                                        
SETVAT   NTR1                                                                   
         MVC   TBYTE,SVTBYTE      SET EQUATED ACCOUNT ELEMENT                   
         BAS   RE,GETROW          GET R5 TO PT TO CORRECT ROW                   
         MVC   ACPBTYP,TBYTE      SET EQUATED ACCOUNT NUMBER                    
         MVC   ACPACC,TAXACT      SET DEFAULT ACCOUNT                           
         MVC   ACPLVL,=C'GST'     SET DEFAULT LEVEL                             
         OC    PRVCODE,PRVCODE    IF PROVIDENCE PROVIDED                        
         BZ    *+10                                                             
         MVC   ACPLVL,=C'PST'                                                   
         MVI   ACPSTAT,ACPCR      SET ACCOUNT IS A CREDIT                       
         CLI   RULTYPE,C'O'       IF INPUT ACCOUNT                              
         BE    *+8                                                              
         MVI   ACPSTAT,ACPDEB     SET ACCOUNT IS A DEBIT                        
         B     EXIT                                                             
         DROP  R5,R4                                                            
         EJECT                                                                  
*============================================================*                  
* RDRULES - READS GSTRULES RECORD TO GET GST/PST ACCOUNT     *                  
*           READ  PROVINCE/ACC OFFICE/EFFECTIVE DATE         *                  
*                 PROVINCE/EFFECTIVE DATE                    *                  
*                 ACC OFFICE/EFFECTIVE DATE                  *                  
*                 EFFECTIVE DATE                             *                  
*   NTRY- RULES  - RECORD TO RECORD                          *                  
*   XIT - CC CODE SET                                        *                  
*============================================================*                  
*                                                                               
RDRULES  NTR1                                                                   
         OC    ACPBILL,ACPBILL                                                  
         BZ    RDRULES5                                                         
         MVC   TEMPDATE,BQDATE     SPOT/NET BILL INVOICE DATE                   
         CLI   ACPSYS,C'P'         IF PRINT SYSTEM                              
         BNE   RDRULES5                                                         
         GOTO1 VDATCON,DMCB,(3,PBILINVD),(0,TEMPDATE) USE PRINT INVDT           
*                                                                               
RDRULES5 XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TAXRECD,R4                                                       
         MVI   TAXKTYP,TAXKTYPQ   X'05'                                         
         MVC   TAXKCPY,ACPCMPC2   COMPANY CODE- FROM OTHER ACCFILE              
         MVC   TAXKOFF,FFS                                                      
         CLI   RULES,C'P'         IF READING BY PROV/OFF/DATE                   
         BNE   RDRULES6                                                         
         MVC   TAXKPRV,PRVCODE    SET PROVIDENCE CODE                           
         MVC   TAXKOFF,ACCOFF     SET ACC OFFICE CODE                           
         B     RDRULES7           GO SET DATE                                   
*                                                                               
RDRULES6 CLI   RULES,C'V'         IF READING BY PROV/DATE                       
         BNE   *+14                                                             
         MVC   TAXKPRV,PRVCODE    SET PROVIDENCE CODE                           
         B     RDRULES7           GO SET DATE                                   
*                                                                               
         CLI   RULES,C'O'         IF READING BY OFFICE/DATE                     
         BNE   *+10                                                             
         MVC   TAXKOFF,ACCOFF     SET ACC OFFICE CODE                           
*                                                                               
RDRULES7 OC    ACPBILL,ACPBILL    IF BILL NOT AVAILABLE                         
         BNZ   RDRULES8                                                         
         GOTO1 VDATCON,DMCB,(5,FULL),(0,TEMPDATE) USE TODAYS DATE               
*                                                                               
RDRULES8 GOTO1 VDATCON,DMCB,(0,TEMPDATE),(1,TAXKDATE)                           
         XC    TAXKDATE,FFS       GET COMPLEMENT                                
         MVI   RDSE,2             READ FROM OTHER SE IF APPLICABLE              
         GOTO1 =A(READ),DMCB,(RC),RR=RELO                                       
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*============================================================*                  
* PSTREC  - GETS POST MAINT RECORD INTO IOAREA               *                  
*         - AND ADDS POSTINGS ELEMENTS TO AREA PROVIDED      *                  
*============================================================*                  
*                                                                               
PSTREC   NTR1                                                                   
         MVI   UPDBUFF,C'N'       DON'T UPDATE THE BUFFER                       
         OC    ABUFF,ABUFF        BUFFER GIVEN OF LAST RECORD READ?             
         BZ    PSTREC3                                                          
         L     R4,ABUFF           PT TO BUFFER RECORD                           
         CLC   KEY,0(R4)          HAS KEY CHANGED?                              
         BE    PSTREC4            NO - GET INFO FROM BUFFER                     
         MVI   UPDBUFF,C'Y'       YES - UPDATE BUFFER AFTER READ                
*                                                                               
PSTREC3  LA    R4,IOAREA                                                        
         MVI   RDSE,1             READ FROM NATIVE SE                           
         GOTO1 =A(READ),DMCB,(RC),RR=RELO                                       
         BNE   PSTREC80                                                         
*                                                                               
PSTREC4  LA    R2,ACCORFST        DISPLACEMENT TO FIRST ELEMENT                 
         AR    R2,R4              PT TO FIRST ELEMENT                           
         CLI   ACPTYPE,0          READ POST OR PROFILE?                         
         BE    PSTREC30           POST                                          
*                                                                               
         USING MTPELD,R2                                                        
PSTREC5  CLI   0(R2),0            END OF RECORD                                 
         BE    PSTREC80           YES                                           
         CLI   0(R2),MTPELQ       MEDIA PROFILE ELEMENT?                        
         BNE   PSTREC20              NO  - TRY NEXT ELEMENT                     
*                                    YES - NEED TO ADD IT TO POSTINGS           
         USING ACPRTND,R5                                                       
         MVC   TBYTE,MTPFNUM                                                    
         BAS   RE,GETROW          PTS R5 TO CORRECT ROW                         
         MVC   ACPBTYP,MTPFNUM    ELEMENT TYPE                                  
         MVC   ACPFLVL,LEVEL      LEVEL                                         
         XC    ACPFVAL,ACPFVAL                                                  
         ZIC   RE,MTPLN                                                         
         LA    RF,MTPFDATA-MTPEL  DATA                                          
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACPFVAL(0),MTPFDATA                                              
         MVC   ACPFACT(L'MTPFCHID),MTPFCHID                                     
         GOTO1 VDATCON,DMCB,(3,MTPFCHDT),(8,ACPFACT+4)                          
PSTREC20 SR    R0,R0                                                            
         ICM   R0,1,1(R2)         BUMP TO NEXT ELEMENT                          
         BNZ   *+6                IN RECORD                                     
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     PSTREC5            AND TRY AGAIN                                 
         DROP  R2                                                               
         EJECT                                                                  
*                                 POST RECORDS                                  
         USING MBTELD,R2                                                        
PSTREC30 CLI   0(R2),0            END OF RECORD                                 
         BE    PSTREC80                                                         
         CLI   0(R2),MBTELQ         MEDIA TRANSFER ELEMENT?                     
         BNE   PSTREC60              NO  - TRY NEXT ELEMENT                     
         MVI   RULREC,C'Y'         READ AT LEAST ONE RECORD                     
         TM    BILSTAT3,BSTLMGQ                                                 
         BZ    *+16                                                             
         CLI   BYTE,MPRKRLMG                                                    
         BNE   *+8                                                              
         MVI   BPCRULE,C'Y'                                                     
*                                                                               
         USING ACPRTND,R5                                                       
         MVC   TBYTE,MBTTYP                                                     
         BAS   RE,GETROW          PTS R5 TO CORRECT ROW                         
*                                 REPLACE/ADD CURRENT ELEMENT                   
         MVC   ACPBTYP,MBTTYP     BILLING ELEMENT TYPE                          
         OC    MBTULA(14),MBTULA  IF NO ACCOUNT IN THIS REC                     
         BZ    PSTREC50           DON'T OVERRIDE WITH ZEROS                     
         MVC   TBYTE,MBTTYP                                                     
         BAS   RE,GETDOC          GET CREDIT OR DEBIT FROM TBYTE                
         MVC   ACPSTAT,TBYTE                                                    
         CLI   ACPSTAT,0          IF STATUS NOT SET                             
         BNE   PSTREC40                                                         
         CLI   ACPBTYP,MBTTAPR    AND AOR PAY/RCVBL ROW                         
         BNE   PSTREC40                                                         
         MVI   ACPSTAT,ACPDEB     SET IT BY LOOKING AT ACCOUNT                  
         CLC   MBTULA(2),=C'SR'                                                 
         BE    *+8                                                              
         MVI   ACPSTAT,ACPCR                                                    
PSTREC40 MVC   ACPACC,MBTULA       UNIT/LEDGER/ACCOUNT                          
         MVC   ACPLVL,LEVEL       SET NEW LEVEL                                 
PSTREC50 OC    MBTAMTX,MBTAMTX    ANYTHING IN CURRENT AMOUNT                    
         BNZ   PSTREC55           YES - OVERRIDE PREVIOUS LEVEL                 
         OC    MBTMEMOX,MBTMEMOX  ANYTHING IN CURRENT MEMO                      
         BZ    PSTREC58           NOTHING IN AMT&MEMO - DON'T OVERRIDE          
*                            CHANGE IN AMT OR MEMO - USE CURRENT VALUES         
PSTREC55 MVC   ACPAMT,MBTAMTX       AMOUNT                                      
         MVC   ACPMEMO,MBTMEMOX     MEMO AMOUNT                                 
         MVC   ACPLVL2,LEVEL      LEVEL                                         
*                                                                               
PSTREC58 MVC   ACPID,MBTPERID     PERSON'S INITIALS                             
         MVC   ACPCHDT,MBTCHNG   LAST CHANGE DATE                               
PSTREC60 SR    R0,R0                                                            
         ICM   R0,1,1(R2)         BUMP TO NEXT ELEMENT                          
         BNZ   *+6                IN RECORD                                     
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     PSTREC30           AND TRY AGAIN                                 
*                                                                               
PSTREC80 CLI   UPDBUFF,C'Y'       NEED TO UPDATE THE BUFFER?                    
         BNE   PSTRECX                                                          
         LA    RE,IOAREA          PT TO IOAREA TO SAVE                          
         LA    RF,L'IOAREA                                                      
         L     R0,ABUFF                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     PSTRECX                                                          
*                                                                               
PSTRECX  B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
*================================*                                              
* GETDOC - GIVEN ROW NUMBER FINDS*                                              
*          IF ACC IS DR OR CR    *                                              
*          NTRY - TBYTE          *                                              
*          XIT  - TBYTE          *                                              
*================================*                                              
*                                                                               
GETDOC   NTR1                                                                   
         LA    RE,DOCTBL          CREDIT OR DEBIT TABLE                         
         ZIC   RF,TBYTE           ROW NUMBER                                    
         BCTR  RF,0                                                             
         AR    RE,RF              PT TO CORRECT DR/CR FOR ROW                   
         MVC   TBYTE,0(RE)        MOVE IN CR OR DR                              
         XIT1                                                                   
         SPACE 2                                                                
* TABLE ARRANGED BY MBTTYP -(ELE TYPE) -                                        
* EX 1ST ONE (01=RCVL) IS DEBIT                                                 
*    4TH ONE (04=CASH DISC) IS CREDIT                                           
*    0 = ALREADY TAKEN CARE OF                                                  
*                                                                               
DOCTBL   DC    X'80,40,40,40,80,40,40,40,40,40,40,0,80,80,40,40,40,80'          
         DC    X'40,80,40,40,80,40,40,80,40,80,40,80,40,80,40,80,40,40'         
         DC    X'40,40,40,40,40,40,80,40,40,40,40,80,40'                        
         EJECT                                                                  
*============================================================*                  
* SETSACC - SETS SPOST ACCOUNTS SUSPENSE RCVBL & REBATE RCVBL*                  
*         - ACCORDING TO PROFILE VALUES PASSED               *                  
*============================================================*                  
*                                                                               
SETSACC  NTR1                                                                   
         USING ACPRTND,R5                                                       
*                                                                               
         MVI   TBYTE,MBTTRCV      CREATE SUS & REB OFF OF RECEIVABLES           
         BAS   RE,GETROW          PT R5 TO RECEIVABLE ACCOUNT                   
         MVC   SVRCACC,ACPACC                                                   
         OC    SVRCACC,SPACES                                                   
         CLC   SVRCACC,SPACES     IF NO RCVBL ACC                               
         BE    SETSACCX           - SKIP SUSP & REB ACCOUNTS                    
*                                                                               
         MVI   TBYTE,MBTTRCS                                                    
         BAS   RE,GETROW          PT R5 TO SUSPENSE RCVBL ROW                   
         BAS   RE,GETSUSP         GET SUSPENSE ACCOUNT                          
         MVC   ACPACC,WORK                                                      
         MVC   ACPLVL,=C'SM '     INDICATE SPROF MAINT DRIVEN                   
         MVI   ACPSTAT,ACPDEB                                                   
*                                                                               
         MVI   TBYTE,MBTTRCR                                                    
         BAS   RE,GETROW          PT R5 TO REBATE RCVBL ROW                     
         BAS   RE,GETREB          GET REBATE ACCOUNT                            
         MVI   ACPSTAT,ACPDEB                                                   
         MVC   ACPLVL,=C'SM '     INDICATE SPROF MAINT DRIVEN                   
         MVC   ACPACC,WORK                                                      
*                                                                               
SETSACCX XIT1                                                                   
         EJECT                                                                  
*-------------------------------------*                                         
* GETSUSP - CREATES SUSPENSE ACC BY   *                                         
*           CHECKING PROF FOR SUFFIX  *                                         
* XIT       WORK(14) IS SUSPENSE ACC  *                                         
*-------------------------------------*                                         
*                                                                               
GETSUSP  NTR1                                                                   
         MVC   WORK(14),SVRCACC   RECEIVABLE ACCOUNT                            
         MVC   SUFFIX,=C'OSUS '   SUSPENSE DEFAULT SUFFIX                       
         OC    ACPSUFX,ACPSUFX                                                  
         BZ    *+10                                                             
         MVC   SUFFIX,ACPSUFX                                                   
         OC    SUFFIX,SPACES                                                    
         LA    RF,12              LENGTH OF ACC NAME                            
         LA    R2,WORK+13                                                       
GETSUS5  CLI   0(R2),C' '                                                       
         BH    GETSUS10                                                         
         BCTR  R2,0                                                             
         BCT   RF,GETSUS5                                                       
         B     GETSERR            SUFFIX ERROR - NOT ENOUGHT ROOM               
GETSUS10 LA    RF,WORK+13                                                       
         SR    RF,R2              (RF) = ROOM LEFT FOR SUFFIX                   
*                                 CALCULATE LENGTH OF SUFFIX                    
         LA    R1,0                                                             
         LA    RE,SUFFIX                                                        
         LA    R0,L'ACPSUFX                                                     
GETSUS12 CLI   0(RE),C' '                                                       
         BE    GETSUS14                                                         
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,GETSUS12                                                      
*                                 R1 = LENGTH (ACPSUFX)                         
GETSUS14 CR    RF,R1              ENOUGH ROOM IN RCVBL ACC?                     
         BL    GETSERR                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),SUFFIX     MOVE IN SUFFIX (AFTER SPACE)                  
*                                                                               
GETSUSX  B     EXIT                                                             
*                                                                               
GETSERR  OI    ACPERR2,ACPESFX                                                  
         B     GETSUSX                                                          
         EJECT                                                                  
*-------------------------------------*                                         
* GETREB  - CREATES REBATE ACC BY     *                                         
*           CHECKING PROF FOR SUFFIX  *                                         
* XIT       WORK(14) IS REBATE ACCOUNT*                                         
*-------------------------------------*                                         
*                                                                               
GETREB   NTR1                                                                   
         MVC   WORK(14),SVRCACC   RECEIVABLE ACCOUNT                            
         MVC   SUFFIX,=C'OREB '   REBATE SUFFIX                                 
         OC    ACPRBFX,ACPRBFX                                                  
         BZ    *+10                                                             
         MVC   SUFFIX,ACPRBFX                                                   
         OC    SUFFIX,SPACES                                                    
         LA    RF,12              LENGTH OF ACC NAME                            
         LA    R2,WORK+13                                                       
GETREB5  CLI   0(R2),C' '                                                       
         BH    GETREB10                                                         
         BCTR  R2,0               BACK UP ONE SPACE IN ACC NAME                 
         BCT   RF,GETREB5                                                       
         B     GETRERR            ERROR IN SUFFIX                               
GETREB10 LA    RF,WORK+13                                                       
         SR    RF,R2              (RF) = # OF SPACES LEFT FOR SUFFIX            
*                                                                               
         LA    R1,0                                                             
         LA    RE,SUFFIX          SUFFIX TO ADD                                 
         LA    R0,L'ACPRBFX                                                     
GETREB12 CLI   0(RE),C' '                                                       
         BE    GETREB14                                                         
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,GETREB12                                                      
*                                 R1 = LENGTH OF SUFFIX                         
GETREB14 CR    RF,R1              ENOUGH ROOM FOR SUFFIX                        
         BL    GETRERR            ERROR WITH SUFFIX                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),SUFFIX     MOVE IN SUFFIX AFTER SPACE                    
GETREBX  B     EXIT                                                             
*                                                                               
GETRERR  OI    ACPERR2,ACPESFX    ERROR WITH SUFFIX                             
         B     GETREBX                                                          
         DROP  R5                                                               
         EJECT                                                                  
*============================================================*                  
* SETCRCV - SET CLIENT RCVBL ACCOUNT                         *                  
*============================================================*                  
*                                                                               
SETCRCV  NTR1                                                                   
         CLI   ACPRVSJ,C'Y'       YES - OVERRIDE SR WITH SJ                     
         BNE   SETCRCVX                                                         
         MVI   TBYTE,MBTTRCV                                                    
         BAS   RE,GETROW                                                        
         USING ACPRTND,R5                                                       
         BAS   RE,GETADCD            SETS ERROR IF NO ADCD                      
         MVC   ACPACC(14),TEMPACC    ACCOUNT SET BY GETADCD                     
         BAS   RE,CHKWKSJ            CHECK WORK CODE                            
         BE    SETCRCVX                                                         
         OI    ACPERRC,ACPNOWK       ERROR WITH WORK CODE                       
SETCRCVX B     EXIT                                                             
         SPACE                                                                  
*------------------------------------------------------------------*            
* GETADCD - BLDS SJ ACCOUNT SJCLTPRD + ADCODE IF REGULAR BILL - IF *            
*         - CD BILL SJCLTPRD + SEPCD CODE   - TEMPACC SET ON EXIT  *            
*------------------------------------------------------------------*            
GETADCD  NTR1                                                                   
         XC    TEMPACC,TEMPACC                                                  
         MVC   TEMPACC(2),=C'SJ'                                                
         MVC   TEMPACC+2(3),ACPCLT   CLIENT                                     
         MVC   TEMPACC+5(3),ACPPRD   PRODUCT                                    
         LA    R1,PBILLEL                                                       
         USING PBILOTH,R1                                                       
         SR    R0,R0                                                            
GETAD10  CLI   0(R1),0                                                          
         BE    GETAD30            NO ADCODE ,CHECK JOB CODE                     
         CLI   0(R1),9                                                          
         BE    GETAD20                                                          
         ICM   R0,1,1(R1)                                                       
         AR    R1,R0                                                            
         B     GETAD10                                                          
GETAD20  MVC   TEMPACC+8(6),PBILLJOB ADCODE                                     
*                                                                               
GETAD30  CLI   PBILCDSW,C'S'      CD TYPE BILLS?                                
         BNE   GETADX                                                           
         CLI   PBILSEP,C'C'       YES - CD BILL?                                
         BNE   GETADX             NO - MUST BE REGULAR CD OR ADJ CD             
         OC    ACPJBCD,ACPJBCD                                                  
         BZ    GETADX                                                           
         MVC   TEMPACC+8(L'ACPJBCD),ACPJBCD                                     
GETADX   OC    TEMPACC,SPACES                                                   
         CLC   TEMPACC+8(6),SPACES                                              
         BNE   *+8                                                              
         OI    ACPERR2,ACPEADCD      ERROR WITH ADCODE                          
         B     EXIT                                                             
         DROP  R1                                                               
         DROP  R5                                                               
         EJECT                                                                  
*============================================================*                  
* CHKWKSJ - MAKES SURE WORK CODE STILL VALID                 *                  
*============================================================*                  
*                                                                               
CHKWKSJ  NTR1                                                                   
         OC    ACPWKSJ,ACPWKSJ                                                  
         BZ    NO                 NO WORK CODE                                  
         MVC   KEY,SPACES                                                       
         LA    R1,KEY                                                           
         USING WCOKEY,R1                                                        
         MVI   WCOKTYP,WCOKTYPQ   X'0A'                                         
         MVC   WCOKCPY,ACPCMPC2   COMPANY CODE FROM OTHER FILE                  
         MVC   WCOKUNT(2),ACPSPROD UNIT/LEDGER                                  
         MVC   WCOKWRK,ACPWKSJ    WORK CODE                                     
         GOTO1 =A(READ),DMCB,(RC),RR=RELO                                       
         B     EXIT               CC CODE SET                                   
         DROP  R1                                                               
         EJECT                                                                  
*============================================================*                  
* SETCC   - GET CC= OVERRIDES ON ANY INCOME ACCOUNTS         *                  
*============================================================*                  
*                                                                               
SETCC    NTR1                                                                   
         XC    CCMEDINC,CCMEDINC                                                
         XC    CCAORINC,CCAORINC                                                
         XC    CCSELINC,CCSELINC                                                
         XC    CCINTINC,CCINTINC                                                
         CLI   ACPCC,C'N'         ANY CC OVERRIDES?                             
         BE    SETCCX                                                           
         USING ACPRTND,R5                                                       
         MVI   TBYTE,MBTTINC      GET MEDIA INCOME ACCOUNT                      
         BAS   RE,GETROW                                                        
         BAS   RE,GETCC           GET CC =                                      
         MVC   CCMEDPOS,WORK      POSITION TO START REPLACEMENT                 
         MVC   CCMEDINC,WORK+1    CHARACTERS TO REPLACE                         
         OC    CCMEDINC,SPACES                                                  
*                                                                               
         MVI   TBYTE,MBTTARI      GET AOR INCOME ACCOUNT                        
         BAS   RE,GETROW                                                        
         BAS   RE,GETCC           GET CC =                                      
         MVC   CCAORPOS,WORK      POSITION TO START REPLACEMENT                 
         MVC   CCAORINC,WORK+1    CHARACTERS TO REPLACE                         
         OC    CCAORINC,SPACES                                                  
*                                                                               
         MVI   TBYTE,MBTTSEL      GET SELLOFF INCOME ACCOUNT                    
         BAS   RE,GETROW                                                        
         BAS   RE,GETCC           GET CC =                                      
         MVC   CCSELPOS,WORK      POSITION TO START REPLACEMENT                 
         MVC   CCSELINC,WORK+1    CHARACTERS TO REPLACE                         
         OC    CCSELINC,SPACES                                                  
*                                                                               
         MVI   TBYTE,MBTTINTI     GET INTERNAL INCOME ACCOUNT                   
         BAS   RE,GETROW                                                        
         BAS   RE,GETCC           GET CC =                                      
         MVC   CCINTPOS,WORK      POSITION TO START REPLACEMENT                 
         MVC   CCINTINC,WORK+1    CHARACTERS TO REPLACE                         
         OC    CCINTINC,SPACES                                                  
*                                                                               
SETCCX   B     EXIT                                                             
         EJECT                                                                  
*============================================================*                  
* GETCC - GIVEN AN INCOME ACCOUNT - READS RECORD TO SEE IF   *                  
*         - COSTING CENTER ATTACHED TO IT                    *                  
*      XIT- WORK = POSITION TO REPLACE, WORK+1=COSTING CENTER*                  
*============================================================*                  
*                                                                               
GETCC    NTR1                                                                   
         MVI   WORK,0                                                           
         MVC   WORK+1(L'WORK-1),SPACES                                          
         OC    ACPACC,ACPACC      IF INCOME ACCOUNT                             
         BZ    GETCCX                                                           
*                                 READ ACCOUNT RECORD                           
         LA    R2,KEY                                                           
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,ACPCMPC2   COMPANY CODE FROM OTHER ACCFILE               
         MVC   ACTKUNT(L'ACPACC),ACPACC                                         
         OC    ACTKUNT(L'ACPACC),SPACES                                         
         MVI   RDSE,2             READ FROM OTHER SE IF APPLICABLE              
         GOTO1 =A(READ),DMCB,(RC),RR=RELO                                       
         BNE   GETCCX                                                           
         LA    R4,IOAREA                                                        
         LA    R1,ACCORFST                                                      
         AR    R4,R1                                                            
         USING RSTELD,R4                                                        
         SR    R0,R0                                                            
GETCC10  CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),RSTELQ       RECORD STATUS ELE X'30'                       
         BE    GETCC20                                                          
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     GETCC10                                                          
*                                                                               
GETCC20  MVC   WORK(1),RSTCCTRR   POSTION TO REPLACE                            
         MVC   WORK+1(L'RSTCCTR),RSTCCTR                                        
*                                                                               
GETCCX   B     EXIT                                                             
         DROP  R5,R4,R2                                                         
         EJECT                                                                  
*============================================================*                  
* SETRACC - SET RCV & COSTING ACCOUNTS FOR RETAIL BILLS ONLY *                  
*         - ACCORDING TO PROFILE VALUES PASSED               *                  
*============================================================*                  
*                                                                               
         USING ACPRTND,R5                                                       
SETRACC  NTR1                                                                   
*        TM    BILSTAT3,BSTREGQ+BSTLMGQ   REGIONL OR LMG RTL BILLING?           
*        BNZ   SETRACX                    THESE ACCTS ARE NOT SETUP             
                                                                                
         CLI   ACPSYS,C'P'                                                      
         BE    SETRAC1                                                          
         MVC   RETACC,BRETACCT    RETAIL ACCOUNT FOR SPOT/NET                   
         OC    RETACC,SPACES      (AKA PARTICIPANT/DISTRIBUTOR ACC)             
         CLI   BRETAIL,X'41'      IF CORP SUMMARY                               
         BNE   SETRAC4                                                          
         B     SETRAC2            CHECK MARKET GROUP OVERRIDE                   
*                                                                               
SETRAC1  MVC   RETACC,PBRACCT     RETAIL ACCOUNT FOR PRINT                      
         OC    RETACC,SPACES                                                    
         CLI   PBRETAIL,X'41'     IF CORP SUMMARY                               
         BNE   SETRAC4                                                          
*                                                                               
SETRAC2  OC    ACPMGROV,ACPMGROV  SET MARKET GROUP OVERRIDE                     
         BZ    SETRAC4                                                          
         MVC   RETACC(L'ACPMGROV),ACPMGROV                                      
*                                                                               
SETRAC4  MVI   TBYTE,MBTTRCV                                                    
         BAS   RE,GETROW          PT R5 TO RECEIVABLE ROW                       
         MVI   FLAG,C'R'          RECV OVERRIDE                                 
*                                                                               
         CLI   ACPRCV,C'Y'        RECV = OVERRIDE                               
         BNE   SETRAC5                                                          
         BAS   RE,CHKUNT          READ UNIT 3 LEDGER                            
         B     SETRAC10           NOW GO DO COSTING ROW                         
*                                                                               
SETRAC5  CLI   ACPLDG,C' '        LEDGER SUPPLIED ?                             
         BNH   SETRACER                                                         
         BAS   RE,CHKPART         CHECK FOR + SIGN                              
*                                                                               
SETRAC10 MVI   TBYTE,MBTTCOS                                                    
         BAS   RE,GETROW          PT R5 TO COSTING ROW                          
         MVI   FLAG,C'C'                                                        
*                                                                               
         CLI   ACPCST,C'Y'        COST = OVERRIDE                               
         BNE   SETRAC15                                                         
         BAS   RE,CHKUNT          READ UNIT 3 LEDGER                            
         B     SETRACX                                                          
*                                                                               
SETRAC15 CLI   ACPLDG,C' '        LEDGER SUPPLIED  ?                            
         BNH   SETRACER                                                         
         BAS   RE,CHKPART         CHECK FOR + SIGN                              
         B     SETRACX                                                          
*                                                                               
SETRACER OI    ACPERRC,ACPIRET    RETAIL RPROF NOT SET UP CORRECTLY             
*                                                                               
SETRACX  XIT1                                                                   
         EJECT                                                                  
*============================================================*                  
* CHKUNT  - READS UNIT 3 LEDGER TO SEE IF ACCOUNT OVERRIDE   *                  
*         - SPECIFIED                                        *                  
*============================================================*                  
*                                                                               
CHKUNT   NTR1                                                                   
         MVC   KEY,SPACES                                                       
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKCPY,ACPCMPC2   COMPANY CODE - FROM OTHER ACCFILE             
         MVI   ACTKUNT,C'3'       UNIT 3                                        
         MVC   ACTKLDG,ACPLDG     LEDGER REQUESTED                              
         MVC   ACTKACT,RETACC     RETAIL ACCOUNT FROM BILL                      
         MVI   RDSE,2             READ FROM OTHER SE IF APPLICABLE              
         GOTO1 =A(READ),DMCB,(RC),RR=RELO                                       
         BNE   UNT3ERR            ERROR - SHOULD BE THERE                       
         DROP  R4                                                               
         LA    R4,IOAREA                                                        
         LA    R1,ACCORFST                                                      
         AR    R4,R1                                                            
         USING RBRELD,R4                                                        
         SR    R0,R0                                                            
CHKUNT5  CLI   0(R4),0                                                          
         BE    UNT3ERR2           SHOULD BE THERE                               
         CLI   0(R4),RBRELQ       X'2B'                                         
         BE    CHKUNT10                                                         
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     CHKUNT5                                                          
*                                                                               
CHKUNT10 CLI   FLAG,C'R'        OVERRIDE FOR RECEIVABLES?                       
         BNE   CHKUNT15                                                         
         CLC   RBRRECB,SPACES                                                   
         BNH   UNT3ERR2                                                         
         MVC   ACPACC,RBRRECB                                                   
         B     CHKUNTX                                                          
*                                                                               
CHKUNT15 CLI   FLAG,C'C'        OVERRIDE FOR COSTING?                           
         BNE   CHKUNTX                                                          
         CLC   RBRCOST,SPACES                                                   
         BNH   UNT3ERR2                                                         
         MVC   ACPACC,RBRCOST                                                   
CHKUNTX  B     EXIT                                                             
*                                                                               
UNT3ERR  OI    ACPERR2,ACPEUNT3   UNIT 3 ERROR                                  
         B     EXIT                                                             
*                                                                               
UNT3ERR2 OI    ACPERR2,ACPE3ROC   RECV= AND/OR COST= MISSING                    
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*============================================================*                  
* CHKPART -1) CHECKS ACCOUNT FOR + LEVELS                    *                  
*          2) READS UNIT 3 LEDG TO FIND DISP & LENGTH TO MOVE*                  
*          3) SETS ACCOUNT INTO ACPACC                       *                  
* LEVELS : 1(LEV), 1(DISP INTO RETACC), 1(LEN MOVE), 1(SPARE)*                  
*============================================================*                  
*                                                                               
CHKPART  NTR1                                                                   
         BAS   RE,SETLEV          SET LEVELS IF ANY                             
         OC    LEVELS,LEVELS                                                    
         BZ    CHKPX              USING DETAULT ACCOUNT                         
         BAS   RE,SETFLEN         SET FROM LEN,DISP FOR LEVELS                  
         BNE   CHKPX                                                            
*                                                                               
         LA    R4,LEVELS          LEVEL TABLE                                   
         XC    WORK,WORK                                                        
         LA    R3,WORK            BUILD NEW ACCOUNT                             
         LA    R2,ACPACC          ACCOUNT WITH +LEVELS                          
         LA    R1,L'ACPACC                                                      
CHKP5    CLI   0(R2),C'+'         CHECK FOR PART SIGN                           
         BE    CHKP10                                                           
         MVC   0(1,R3),0(R2)                                                    
         LA    R3,1(R3)                                                         
         LA    R2,1(R2)                                                         
         BCT   R1,CHKP5                                                         
         B     CHKP20                                                           
*                                                                               
CHKP10   LA    RF,RETACC                                                        
         ZIC   RE,1(R4)           DISP INTO RETACC                              
         AR    RF,RE                                                            
         ZIC   RE,2(R4)           LENGTH TO MOVE FROM RETACC                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RF)      MOVE TO ACPACC                                
         LA    RE,1(RE)           RESTORE LENGTH                                
         AR    R3,RE              BUMP PAST LEVEL JUST ADDED                    
         LA    R4,4(R4)           BUMP TO NEXT LEVEL                            
         LA    R2,2(R2)           BUMP PAST +LEVEL IN ACPACC                    
         SH    R1,=H'2'           SUBTRACT +LEVEL FROM LENGTH                   
         LTR   R1,R1                                                            
         BNZ   CHKP5                                                            
*                                                                               
CHKP20   MVC   ACPACC,WORK        MOVE WHATEVER ACCOUNT HAS BEEN BUILT          
*                                                                               
CHKPX    XIT1                                                                   
         EJECT                                                                  
SETLEV   NTR1                                                                   
         XC    LEVELS,LEVELS                                                    
         LA    R3,ACPACC                                                        
         LA    RF,L'ACPACC                                                      
         LA    RE,LEVELS                                                        
*                                                                               
SETLEV5  CLI   0(R3),C'+'         CHECK FOR PART SIGN                           
         BNE   SETLEV10                                                         
         PACK  DUB,1(1,R3)                                                      
         CVB   R1,DUB                                                           
         STC   R1,0(RE)           SAVE LEVEL                                    
         LA    RE,4(RE)                                                         
*                                                                               
SETLEV10 LA    R3,1(R3)                                                         
         BCT   RF,SETLEV5                                                       
         B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------*                      
* SETFLEN - READS UNIT 3 (LEDGER-RPROF/MAINT) TO GET     *                      
*          STRUCTURE OF LEVEL REQUESTED (DISP AND LENGTH)*                      
*--------------------------------------------------------*                      
*                                                                               
SETFLEN  NTR1                                                                   
         LA    R4,KEY                                                           
         MVC   KEY,SPACES                                                       
         USING LDGKEY,R4                                                        
         MVC   LDGKCPY,ACPCMPC2   COMPANY CODE - FROM OTHER ACCFILE             
         MVI   LDGKUNT,C'3'       UNIT 3                                        
         MVC   LDGKLDG,ACPLDG     LEDGER REQUESTED                              
         MVI   RDSE,2             READ FROM OTHER SE IF APPLICABLE              
         GOTO1 =A(READ),DMCB,(RC),RR=RELO                                       
         BE    SETFLEN5           ERROR                                         
         OI    ACPERR2,ACPEUNT3   UNIT 3 ERROR                                  
         B     NO                                                               
*                                                                               
SETFLEN5 BAS   RE,GETLEN          CC CODE SET                                   
SETFLENX B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*============================================================*                  
* GETLEN - LED REC- ELE HAS 04,0C  IMPLIES LVL STRUCT OF 4,8 *                  
*      LVL REQUESTED =1  TAKE FIRST 4 CHARS                  *                  
*      IF LVL REQUESTED =2 TAKE +4 FOR 8 CHARS               *                  
*============================================================*                  
GETLEN   NTR1                                                                   
         LA    R3,LEVELS                                                        
GETLEN5  LA    R4,IOAREA                                                        
         LA    R1,ACCORFST                                                      
         AR    R4,R1                                                            
         USING ACLELD,R4                                                        
*                                                                               
         SR    R0,R0                                                            
GETLEN10 CLI   0(R4),0                                                          
         BE    GETLEN70           PART ERROR                                    
         CLI   0(R4),ACLELQ       X'16'                                         
         BE    GETLEN20                                                         
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     GETLEN10                                                         
*                                                                               
GETLEN20 ZIC   RF,1(R4)           GET LENGTH OF ELEMENT                         
         SH    RF,=Y(ACLLN1Q)     RF = LENGTH OF DATA                           
         LA    R1,L'ACLVALS       R1 = LENGTH OF EACH LVL OF DATA               
         SR    RE,RE                                                            
         DR    RE,R1              RF = # OF LEVELS IN ELEMENT                   
         CLM   RF,1,0(R3)         REQUESTED LVL IN RPROF/MAINT                  
         BL    GETLEN70           NOT IN ELE - PART ERROR                       
*                                                                               
         LA    R4,ACLLN1Q(R4)     PT TO DATA                                    
         MVI   BYTE,0             DISPLACEMENT INTO BRETAIL                     
         LA    R2,0               START AT FIRST LEVEL                          
         ZIC   RE,0(R3)           LEVEL REQUESTED                               
         BCTR  RE,0                                                             
*                                                                               
GETLEN30 CR    R2,RE              MATCH ON LEVEL                                
         BE    GETLEN50                                                         
         LA    R2,1(R2)           NEXT LEVEL COUNTER                            
         MVC   BYTE,0(R4)         SAVE PREVIOUS LENGTH                          
         LA    R4,L'ACLVALS(R4)                                                 
         BCT   RF,GETLEN30        NUMBER OF LEVELS                              
         B     GETLEN70           LEVEL REQUESTED NOT IN ELE                    
*                                                                               
GETLEN50 CLI   0(R4),C' '         IS THIS A REAL LEVEL?                         
         BE    GETLEN70                                                         
         ZIC   RE,0(R4)           THIS LEVELS DISP                              
         ZIC   R1,BYTE            DISP INTO RETACC                              
         SR    RE,R1              LENGTH OF LEVEL WANTED FROM BRETAIL           
         STC   R1,1(R3)           DISP TO MOVE FROM                             
         STC   RE,2(R3)           LENGTH TO MOVE FROM                           
         LA    R3,4(R3)                                                         
         CLI   0(R3),0            ANY MORE LEVELS                               
         BNE   GETLEN5                                                          
         B     YES                                                              
*                                                                               
GETLEN70 OI    ACPERR2,ACPEPAR    PART ERROR                                    
         B     NO                                                               
         DROP  R4,R5                                                            
         EJECT                                                                  
FFS      DC    X'FFFFFF'                                                        
         SPACE 2                                                                
*====================*                                                          
* LITERAL POOL       *                                                          
*====================*                                                          
*                                                                               
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB,R9,R7                                                         
         EJECT                                                                  
*============================================================*                  
* EXPAND  - EXPANDS ACCOUNTS TO ACTUAL POSTINGS              *                  
*============================================================*                  
*                                                                               
         DS    0D                                                               
EXPAND   NMOD1 0,**EXPD**,R9,R7                                                 
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
*                                                                               
         L     R5,ACPPOST         R5=A(ACCOUNTS TO EXPAND)                      
         USING ACPRTND,R5                                                       
         L     R4,APOSTS          R4=A(RETURN POSTINGS)                         
         MVI   NAPOSTS,0          PRE-CLEAR # OF POSTINGS IN APOSTS             
         ZIC   R3,ACPNUMA         R3=NUMBER OF ACCOUNTS                         
         XC    SVCOST,SVCOST      CLEAR OUT SAVED 1C LINES                      
         XC    SVCOSTI,SVCOSTI                                                  
         BAS   RE,CALCIOR         CALCULATE IOR AMOUNT (% OF GROSS)             
*                                                                               
         CLI   ACPSYS,C'P'        IF PRINT SYSTEM                               
         BNE   EXPAND10                                                         
         CLI   PBILLTYP,C'R'      &  REBATE BILL                                
         BNE   EXPAND10                                                         
         BAS   RE,GENRBPST        GEN REBATE PST'G FOR ALL BILL TYPES           
         B     EXPAND90                                                         
*                                                                               
EXPAND10 CLC   =C'REG',ACPBTYPE                                                 
         BE    EXPAND20                                                         
         CLC   =C'RET',ACPBTYPE                                                 
         BNE   EXPAND40                                                         
EXPAND20 BAS   RE,GENRPOST        GENERATE REG/RET BILLING POSTINGS             
         B     EXPAND90                                                         
*                                                                               
EXPAND40 CLC   =C'AOR',ACPBTYPE                                                 
         BNE   EXPANDX                                                          
         BAS   RE,GENAPOST        GENERATE AOR BILLING POSTINGS                 
*                                                                               
EXPAND90 DS    0H                 CHECK ALL ACCOUNTS                            
         MVI   ACCERROR,C'N'      NO PROBLEM WITH ANY ACCOUNT                   
         GOTO1 =A(CHKACC),DMCB,(RC),RR=RELO                                     
         CLI   ACCERROR,C'Y'                                                    
         BNE   *+8                                                              
         OI    ACPERR,ACPAERR     ERROR WITH ONE OF THE ACCOUNTS                
*                                                                               
EXPANDX  XIT1                                                                   
*                                                                               
EXPYES   CR    RB,RB                                                            
         B     *+6                                                              
EXPNO    LTR   RB,RB                                                            
         B     EXPANDX                                                          
         EJECT                                                                  
*----------------------*                                                        
* CALCIOR - % OF GROSS *                                                        
*----------------------*                                                        
*                                                                               
CALCIOR  NTR1                                                                   
         CLI   ACPSYS,C'P'                                                      
         BNE   *+14                                                             
         ZAP   IORAMT,=P'0'                                                     
         B     *+10                                                             
         XC    IORAMT,IORAMT                                                    
         ICM   RF,15,ACPIOR        PERCENTAGE WITH 3 DECIMAL PLACES             
         LTR   RF,RF                                                            
         BZ    CALCIORX                                                         
         CVD   RF,DUB                                                           
         ZAP   PNUM(16),DUB                                                     
*                                                                               
         CLI   ACPSYS,C'P'                                                      
         BNE   *+14                                                             
         ZAP   DUB,PBILLGRS                                                     
         B     *+10                                                             
         ZAP   DUB,BGRSP                                                        
         MP    PNUM(16),DUB                                                     
         SRP   PNUM,64-5,5         DIVIDE AND ROUND                             
*                                                                               
         ZAP   DUB,PNUM+8(8)                                                    
         CLI   ACPSYS,C'P'                                                      
         BNE   *+14                                                             
         ZAP   IORAMT,DUB                                                       
         B     CALCIORX                                                         
         CVB   RF,DUB                                                           
         STCM  RF,15,IORAMT+2                                                   
CALCIORX B     EXPANDX                                                          
         SPACE 2                                                                
BUMP4NXT DS    0H                  BUMP R4 TO NEXT POSTING ROW                  
         CLI   LRTN,C'Y'                                                        
         BE    *+12                                                             
         LA    R4,ACPRTNL(R4)                                                   
         B     *+8                                                              
         LA    R4,ACPRTNL2(R4)                                                  
         BR    RE                                                               
         SPACE 2                                                                
BUMPCNT  DS    0H                  BUMP NUMBER OF POSTINGS                      
         ZIC   R1,NAPOSTS          NUMBER POSTINGS IN APOSTS                    
         LA    R1,1(R1)                                                         
         STC   R1,NAPOSTS                                                       
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------*                                
* GENRBPST - GENERATE PRINT REBATE BILL POSTING*                                
*----------------------------------------------*                                
*                                                                               
GENRBPST NTR1                                                                   
*                                                                               
GENRB10  OC    0(ACPRTNL,R5),0(R5)                                              
         BZ    GENRB80                                                          
         CLC   =C'NONE',ACPACC                                                  
         BE    GENRB80            SKIP THIS POSTING                             
*                                                                               
         CLI   0(R5),MBTTRCS      SUSPENSE ROW                                  
         BE    GENRB30                                                          
         CLI   0(R5),MBTTRCR      REBATE ROW                                    
         BE    GENRB30                                                          
*                                                                               
         CLI   0(R5),MBTTGST      OUTPUT GST ROW                                
         BNE   GENRB20                                                          
         BAS   RE,GETPGST         GET PRINT GST -- IN FULL                      
         BNE   GENRB80            NONE - SKIP POSTING                           
         B     GENRB30                                                          
*                                                                               
GENRB20  CLI   0(R5),MBTTPQO      OUTPUT PST ROW FOR PQ                         
         BE    GENRB25                                                          
         CLI   0(R5),MBTTNBO      OUTPUT PST ROW FOR NB                         
         BE    GENRB25                                                          
         CLI   0(R5),MBTTNSO      OUTPUT PST ROW FOR NS                         
         BE    GENRB25                                                          
         CLI   0(R5),MBTTBCO      OUTPUT PST ROW FOR BC                         
         BE    GENRB25                                                          
         CLI   0(R5),MBTTONO      OUTPUT PST ROW FOR ON                         
         BE    GENRB25                                                          
         CLI   0(R5),MBTTPEO      OUTPUT PST ROW FOR PE                         
         BE    GENRB25                                                          
         CLI   0(R5),MBTTNFO      OUTPUT PST ROW FOR NF                         
         BNE   GENRB80            NO OTHER POSTINGS FOR REBATE BILLING          
GENRB25  BAS   RE,CHKPSTO         GET PST -- IN FULL                            
         BNE   GENRB80                                                          
*                                                                               
GENRB30  MVC   0(ACPRTNL,R4),0(R5)                                              
         BAS   RE,BUMPCNT          BUMP POSTING COUNT                           
         LA    R1,ACPMEMO2-ACPBTYP                                              
         AR    R1,R4                                                            
         XC    0(L'ACPMEMO2,R1),0(R1) NO MEMO AMOUNT                            
         LA    R1,ACPAMT2-ACPBTYP                                               
         AR    R1,R4                                                            
         CLI   0(R5),MBTTRCR      REBATE ROW                                    
         BNE   GENRB35                                                          
         ZAP   0(L'ACPAMT2,R1),PBILLRCV                                         
         B     GENRB50                                                          
GENRB35  CLI   0(R5),MBTTRCS      SUSPENSE ROW                                  
         BNE   GENRB40                                                          
*        ZAP   0(L'ACPAMT2,R1),PBILLRCV                                         
*        MP    0(L'ACPAMT2,R1),=P'-1'                                           
         ZAP   DUB,PBILLRCV                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   0(L'ACPAMT2,R1),DUB+2(6)                                         
         B     GENRB50                                                          
*                                                                               
GENRB40  ICM   R1,15,FULL         GST/PST AMOUNT                                
         CVD   R1,DUB                                                           
         ZAP   0(L'ACPAMT2,R1),DUB+2(6)                                         
*                                                                               
GENRB50  BAS   RE,BUMP4NXT      BUMP R4 TO NEXT POSTING ROW                     
*                                                                               
GENRB80  LA    R5,ACPRTNL(R5)   NEXT ACCOUNT                                    
         BCT   R3,GENRB10                                                       
GENRBX   XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------*                 
*  GENRPOST - EXPANDS POSTINGS FOR REGULAR BILLING ACCOUNTS   *                 
*-------------------------------------------------------------*                 
*                                                                               
GENRPOST NTR1                                                                   
*                                                                               
GENR10   OC    0(ACPRTNL,R5),0(R5) IF NO ACCOUNT LINE                           
         BZ    GENR100            SKIP TO NEXT ACCOUNT LINE                     
         CLI   ACPBTYP,MBTTPQI    SKIP IF INPUT PST FOR PQ                      
         BE    GENR100                                                          
         CLI   ACPBTYP,MBTTNBI    SKIP IF INPUT PST FOR NB                      
         BE    GENR100                                                          
         CLI   ACPBTYP,MBTTNSI    SKIP IF INPUT PST FOR NS                      
         BE    GENR100                                                          
         CLI   ACPBTYP,MBTTNFI    SKIP IF INPUT PST FOR NF                      
         BE    GENR100                                                          
         CLI   ACPBTYP,MBTTBCI    SKIP IF INPUT PST FOR BC                      
         BE    GENR100                                                          
         CLI   ACPBTYP,MBTTONI    SKIP IF INPUT PST FOR ON                      
         BE    GENR100                                                          
         CLI   ACPBTYP,MBTTPEI    SKIP IF INPUT PST FOR PE                      
         BE    GENR100                                                          
         CLI   ACPBTYP,MBTTCD     IF ACCOUNT = CASH DISC ROW                    
         BNE   GENR20                                                           
         CLI   ACPSYS,C'P'        IF SPOT & NET                                 
         BNE   GENR100            SKIP POSTING                                  
         CLI   PBILCDSW,C'S'      IF PRINT CD TYPE (REG CD,ADJCD,CD CD)         
         BE    GENR100            SKIP POSTING                                  
*                                                                               
GENR20   CLC   =C'NONE',ACPACC    SKIP THIS POSTING                             
         BE    GENR100                                                          
         CLI   ACPBTYP,MBTTRCR    IF ACCOUNT =  REBATE RCVBL ROW                
         BE    GENR100            SKIP IT (TAKEN CARE OF W/REBATE BILL)         
*                                                                               
         CLI   ACPBTYP,MBTTINTI   IF ACCOUNTS FOR INTERNAL                      
         BE    GENR25                                                           
         CLI   ACPBTYP,MBTTINTC                                                 
         BE    GENR25                                                           
         CLI   ACPBTYP,MBTTINTB                                                 
         BE    GENR25                                                           
         CLI   ACPBTYP,MBTTINTR                                                 
         BNE   GENR28                                                           
GENR25   OC    ACPIOR,ACPIOR      SKIP IF NO PERCENTAGE DEFINED                 
         BZ    GENR100                                                          
*                                                                               
GENR28   CLI   ACPBTYP,MBTTCOS    IF ACCOUNT =  COSTING                         
         BNE   *+14                                                             
         MVC   SVCOST,0(R5)       SAVE COSTING LINE                             
         B     GENR100            GO TO NEXT POST ACCOUNT                       
*                                                                               
         CLI   ACPBTYP,MBTTINTC   IF ACCOUNT =  INTERNAL COSTING                
         BNE   GENR30                                                           
         MVC   SVCOSTI,0(R5)      SAVE INTERNAL COSTING LINE                    
         B     GENR100            GO TO NEXT POST ACCOUNT                       
*                                                                               
GENR30   CLI   ACPBTYP,MBTTGST    IF ACCOUNT = GST OUTPUT                       
         BNE   GENR40                                                           
         CLI   ACPSYS,C'P'                                                      
         BNE   GENR35                                                           
         BAS   RE,GETPGST                                                       
         BNE   GENR100            NO GST -- SKIP POSTING                        
         B     GENR48                                                           
*                                                                               
GENR35   CLC   BLEN,=H'90'        IF RECORD NOT LONG ENOUGH                     
         BNH   GENR100            CAN'T BE ANY  GST IN BILL                     
         CLI   BVATCOD,0          OR IF LONG ENOUGH                             
         BE    GENR100            BUT NO GST CODE - REALLY NO GST               
         B     GENR48                                                           
*                                                                               
GENR40   CLI   ACPBTYP,MBTTPQO    IF OUTPUT PST FOR PQ                          
         BE    GENR42                                                           
         CLI   ACPBTYP,MBTTNBO    OR OUTPUT PST FOR NB                          
         BE    GENR42                                                           
         CLI   ACPBTYP,MBTTNSO    OR OUTPUT PST FOR NS                          
         BE    GENR42                                                           
         CLI   ACPBTYP,MBTTBCO    OR OUTPUT PST FOR BC                          
         BE    GENR42                                                           
         CLI   ACPBTYP,MBTTONO    OR OUTPUT PST FOR ON                          
         BE    GENR42                                                           
         CLI   ACPBTYP,MBTTPEO    OR OUTPUT PST FOR PE                          
         BE    GENR42                                                           
         CLI   ACPBTYP,MBTTNFO    OR OUTPUT PST FOR NF                          
         BNE   GENR48                                                           
GENR42   BAS   RE,CHKPSTO         MAKE SURE PST AMOUNT IN BILL                  
         BNE   GENR100                                                          
*                                                                               
GENR48   OC    ACPMEMO,ACPMEMO    IF NO AMOUNT OR MEMO                          
         BNZ   *+14                                                             
         OC    ACPAMT,ACPAMT                                                    
         BZ    GENR100            GO TO NEXT POST ACCOUNT                       
         MVC   0(ACPRTNL,R4),0(R5)                                              
         BAS   RE,BUMPCNT         BUMP POSTING COUNT                            
         BAS   RE,SETAMTS         SET AMOUNT AND MEMO                           
*                                                                               
         CLI   ACPBTYP,MBTTBIL    BILLING ROW                                   
         BE    *+12                                                             
         CLI   ACPBTYP,MBTTREV    REVENUE ROW                                   
         BNE   GENR60                                                           
         MVI   ROBFLAG,C'R'       REGULAR BILLING/REVENUE                       
         B     GENR65                                                           
*                                                                               
GENR60   CLI   ACPBTYP,MBTTINTB   INTERNAL BILLING ROW                          
         BE    *+12                                                             
         CLI   ACPBTYP,MBTTINTR   INTERNAL REVENUE ROW                          
         BNE   GENR62                                                           
         MVI   ROBFLAG,C'I'       INTERNAL BILLING/REVENUE                      
         B     GENR65                                                           
*                                                                               
GENR62   CLI   ACPBTYP,MBTTRBI    TRADE INCOME                                  
         BE    GENR65                                                           
         CLI   ACPBTYP,MBTTRRV    TRADE REVENUE                                 
         BE    GENR65                                                           
*        MVI   ROBFLAG,C'I'       TRADE BILLING/REVENUE                         
*                                                                               
GENR63   CLI   ACPBTYP,MBTTOBIL   OTHER TRADE BILLING (MIDAS)                   
         BE    GENR65                                                           
         CLI   ACPBTYP,MBTTOREV   OTHER TRADE REVENUE (MIDAS)                   
         BE    GENR65                                                           
***      CLI   ACPBTYP,MBTTIRCV   INTERCOMPANY RECIEVABLE                       
***      BE    GENR65                                                           
***      CLI   ACPBTYP,MBTTIPBL   INTERCOMPANY PAYABLE                          
***      BE    GENR65                                                           
***      CLI   ACPBTYP,MBTTOPIN   OPCO INCOME                                   
***      BE    GENR65                                                           
         CLI   ACPBTYP,MBTTOPBL   OPCO BILLINGS                                 
         BE    GENR65                                                           
         CLI   ACPBTYP,MBTTOPRV   OPCO REVENUE                                  
         BNE   GENR80                                                           
*                                                                               
GENR65   MVI   ADJSTAT,0          SET MINIMAL ADJUSTMENT                        
         BAS   RE,DO1C            CREATE 1C POSTING                             
         MVI   ADJSTAT,ADJCOST    SET ADJUST COST POSTINGS IF NECESS.           
         BAS   RE,CHKADJ          ADJUST BILLING/REV/COSTING                    
         B     GENR90                                                           
*                                                                               
GENR80   MVI   ADJSTAT,0          SET MINIMAL AJUSTMENT                         
         BAS   RE,CHKADJ          ADJUST POSTING                                
*                                                                               
GENR90   BAS   RE,BUMP4NXT        BUMP R4 TO NEXT POSTING ROW                   
*                                                                               
GENR100  LA    R5,ACPRTNL(R5)     NEXT ACCOUNT                                  
         BCT   R3,GENR10                                                        
*                                                                               
GENRX    XIT1                                                                   
         EJECT                                                                  
*--------------------------------------------------------*                      
*  GENAPOST - EXPANDS POSTINGS FOR AOR BILLING ACCOUNTS  *                      
*--------------------------------------------------------*                      
*                                                                               
GENAPOST NTR1                                                                   
*                                                                               
GENA10   OC    0(ACPRTNL,R5),0(R5) IF NO ACCOUNT LINE                           
         BZ    GENA120             SKIP TO NEXT ACCOUNT LINE                    
*                                                                               
         CLI   ACPBTYP,MBTTCD     IF ACCOUNT = CASH DISC ROW                    
         BNE   GENA15                                                           
         CLI   ACPSYS,C'P'        IF SPOT & NET                                 
         BNE   GENA120            SKIP POSTING                                  
         CLI   PBILCDSW,C'S'      IF PRINT CD TYPE (REG CD,ADJCD,CD CD)         
         BE    GENA120            SKIP POSTING                                  
*                                                                               
GENA15   CLC   =C'NONE',ACPACC    SKIP THIS POSTING                             
         BE    GENA120                                                          
         CLI   ACPBTYP,MBTTRCR   IF ACCOUNT =  REBATE RCVBL ROW                 
         BE    GENA120           SKIP IT (TAKEN CARE OF W/REBATE BILL)          
*                                                                               
         CLI   ACPBTYP,MBTTINTI   IF ACCOUNTS FOR INTERNAL                      
         BE    GENA18                                                           
         CLI   ACPBTYP,MBTTINTC                                                 
         BE    GENA18                                                           
         CLI   ACPBTYP,MBTTINTB                                                 
         BE    GENA18                                                           
         CLI   ACPBTYP,MBTTINTR                                                 
         BNE   *+14                                                             
GENA18   OC    ACPIOR,ACPIOR     SKIP IF NO PERCENTAGE DEFINED                  
         BZ    GENA120                                                          
*                                                                               
         CLI   ACPBTYP,MBTTCOS   IF ACCOUNT =  COSTING                          
         BNE   *+14                                                             
         MVC   SVCOST,0(R5)      SAVE COSTING LINE                              
         B     GENA120           GO TO NEXT POST ACCOUNT                        
*                                                                               
         CLI   ACPBTYP,MBTTINTC  IF ACCOUNT =  INTERNAL COSTING                 
         BNE   *+14                                                             
         MVC   SVCOSTI,0(R5)     SAVE INTERNAL COSTING LINE                     
         B     GENA120           GO TO NEXT POST ACCOUNT                        
*                                                                               
         CLI   ACPBTYP,MBTTPQO   IF OUTPUT PST FOR PQ                           
         BE    GENA20                                                           
         CLI   ACPBTYP,MBTTNBO   OR OUTPUT PST FOR NB                           
         BE    GENA20                                                           
         CLI   ACPBTYP,MBTTNSO   OR OUTPUT PST FOR NS                           
         BE    GENA20                                                           
         CLI   ACPBTYP,MBTTBCO   IF OUTPUT PST FOR BC                           
         BE    GENA20                                                           
         CLI   ACPBTYP,MBTTONO   OR OUTPUT PST FOR ON                           
         BE    GENA20                                                           
         CLI   ACPBTYP,MBTTPEO   OR OUTPUT PST FOR PE                           
         BE    GENA20                                                           
         CLI   ACPBTYP,MBTTNFO   OR OUTPUT PST FOR NF                           
         BNE   GENA21                                                           
GENA20   BAS   RE,CHKPSTO        MAKE SURE PST IN BILL                          
         BNE   GENA120                                                          
         B     GENA30                                                           
*                                                                               
GENA21   CLI   ACPBTYP,MBTTPQI    IF INPUT PST FOR PQ                           
         BE    GENA21A                                                          
         CLI   ACPBTYP,MBTTNBI    OR INPUT PST FOR NB                           
         BE    GENA21A                                                          
         CLI   ACPBTYP,MBTTNSI    OR INPUT PST FOR NS                           
         BE    GENA21A                                                          
         CLI   ACPBTYP,MBTTBCI    OR INPUT PST FOR BC                           
         BE    GENA21A                                                          
         CLI   ACPBTYP,MBTTONI    OR INPUT PST FOR ON                           
         BE    GENA21A                                                          
         CLI   ACPBTYP,MBTTPEI    OR INPUT PST FOR PE                           
         BE    GENA21A                                                          
         CLI   ACPBTYP,MBTTNFI    OR INPUT PST FOR NF                           
         BNE   GENA22                                                           
GENA21A  BAS   RE,CHKAPR          CHECK VALID AOR PAY/RECEIVABLE ACC            
         BNE   GENA120                                                          
         BAS   RE,CHKPSTI         AND MAKE SURE INPUT PST TO POST               
         BNE   GENA120                                                          
         B     GENA30                                                           
*                                                                               
GENA22   CLI   ACPBTYP,MBTTGSTI   IF ACCOUNT = INPUT GST                        
         BNE   GENA23                                                           
         BAS   RE,CHKAPR          CHECK VALID AOR PAY/RECEIVABLE ACC            
         BNE   GENA120                                                          
         B     GENA25                                                           
*                                                                               
GENA23   CLI   ACPBTYP,MBTTGST    IF ACCOUNT = GST OUTPUT                       
         BNE   GENA30                                                           
GENA25   CLI   ACPSYS,C'P'                                                      
         BNE   GENA26                                                           
         BAS   RE,GETPGST         GET PRINT GST - IN FULL                       
         BNE   GENA120            NO GST -- SKIP POSTING                        
         B     GENA30                                                           
*                                                                               
GENA26   CLC   BLEN,=H'90'        IF RECORD NOT LONG ENOUGH                     
         BNH   GENA120            CAN'T BE ANY  GST IN BILL                     
         CLI   BVATCOD,0          OR IF LONG ENOUGH                             
         BE    GENA120            BUT NO GST CODE - REALLY NO GST               
*                                                                               
GENA30   OC    ACPMEMO,ACPMEMO    IF NO AMOUNT OR MEMO                          
         BNZ   *+14                                                             
         OC    ACPAMT,ACPAMT                                                    
         BZ    GENA120            GO TO NEXT POST ACCOUNT                       
         MVC   0(ACPRTNL,R4),0(R5)                                              
         BAS   RE,BUMPCNT         BUMP POSTING COUNT                            
         BAS   RE,SETAMTS         SET AMOUNT AND MEMO TO AMOUNTS                
*                                                                               
         CLI   ACPBTYP,MBTTSRV    SELLOFF REVENUE?                              
         BE    *+12                                                             
         CLI   ACPBTYP,MBTTSBL    SELLOFF BILLING                               
         BNE   GENA43                                                           
         BAS   RE,INDBILL2        INDICATE BILL2 POSTINGS(FOR BILL/REV)         
         MVI   ROBFLAG,C'S'       SELLOFF BILLING/REVENUE                       
         MVI   ADJSTAT,ADJBILL2   AND BILL2 FOR COSTINGS                        
         B     GENA60                                                           
*                                                                               
GENA43   CLI   ACPBTYP,MBTTARR    AOR REVENUE 1C/12                             
         BE    *+12                                                             
         CLI   ACPBTYP,MBTTABL    AOR BILLING  1C/11                            
         BNE   GENA48                                                           
         MVI   ROBFLAG,C'A'       AOR BILLING/REVENUE                           
         MVI   ADJSTAT,0          COSTING POSTINGS NOT BILL2                    
         BAS   RE,CHKINC          IF MEDIA INCOME ACCOUNT EXISTS                
         BNE   GENA60                                                           
         BAS   RE,INDBILL2        INDICATE BILL2 PST'GS FOR BILL/REV            
         MVI   ADJSTAT,ADJBILL2   AND BILL2 FOR COSTING                         
         B     GENA60                                                           
*                                                                               
GENA48   CLI   ACPBTYP,MBTTBIL    MEDIA BILLING  1C/11                          
         BE    *+12                                                             
         CLI   ACPBTYP,MBTTREV    MEDIA REVENUE 1C/12                           
         BNE   GENA50                                                           
         MVI   ROBFLAG,C'R'       REGULAR BILLING/REVENUE                       
         MVI   ADJSTAT,0          COSTING POSTINGS NOT BILL2                    
         B     GENA60                                                           
*                                                                               
GENA50   CLI   ACPBTYP,MBTTINTB    INTERNAL BILLING 1C/11                       
         BE    *+12                                                             
         CLI   ACPBTYP,MBTTINTR    INTERNAL REVENUE 1C/12                       
         BNE   GENA55                                                           
         MVI   ROBFLAG,C'I'        INTERNAL BILLING/REVENUE                     
         MVI   ADJSTAT,0           COSTING POSTINGS NOT BILL2                   
         B     GENA60                                                           
*                                                                               
GENA55   CLI   ACPBTYP,MBTTRBI    TRADE BILLING                                 
         BE    GENA57                                                           
         CLI   ACPBTYP,MBTTRRV    TRADE REVENUE                                 
         BE    GENA57                                                           
         CLI   ACPBTYP,MBTTOBIL   OTHER BILLING                                 
         BE    GENA57                                                           
         CLI   ACPBTYP,MBTTOREV   OTHER REVENUE                                 
         BNE   GENA70                                                           
GENA57   BAS   RE,INDBILL2        INDICATE BILL2 POSTINGS(FOR BILL/REV)         
*        MVI   ROBFLAG,C'T'       TRADE BILLING/REVENUE                         
*                                                                               
GENA60   BAS   RE,DO1C            DO 1C POSTING                                 
         MVI   ADJSTAT,ADJCOST+ADJBILL2                                         
         CLI   PSTMFLG,C'I'       IF BACKING OUT IOR                            
         BE    *+12                                                             
         CLI   PSTAFLG,C'I'                                                     
         BNE   *+8                                                              
         MVI   ADJSTAT,ADJCOST    MINIMAL ADJUSTMENTS                           
         BAS   RE,CHKADJ                                                        
         CLI   FLAG2,C'N'          IF CREATING ADDITIONAL POSTINGS              
         BE    GENA100                                                          
         MVC   PSTAFLG,FLAG2                                                    
         CLI   FLAG2,C'A'          IF BACK OUT IS AOR                           
         BNE   *+12                                                             
         OI    ADJSTAT,ADJBILL2    MAKE SURE BILL2                              
         B     *+8                                                              
         NI    ADJSTAT,X'FF'-ADJBILL2  ELSE, MAKE SURE ITS NOT                  
         BAS   RE,CHKADJ                                                        
         B     GENA100                                                          
*                                                                               
GENA70   CLI   ACPBTYP,MBTTAPR    AOR PAY/RCVB ACCOUNT                          
         BNE   GENA80                                                           
         BAS   RE,INDBILL2        INDICATE BILL2 POSTING                        
         CLC   ACPACC(2),=C'SR'   IF RCVBL'S ACCOUNT                            
         BNE   GENA100            NO - PAYABLE THAT'S IT                        
         BAS   RE,MINUSIT         YES - MINUS OUT AMOUNT & MEMO                 
         B     GENA100                                                          
*                                                                               
GENA80   MVI   ADJSTAT,ADJBILL2   REGULAR POSTING - BILL2/NO COST               
         CLI   PSTMFLG,C'I'       IF BACKING OUT IOR                            
         BE    *+12                                                             
         CLI   PSTAFLG,C'I'                                                     
         BNE   *+8                                                              
         MVI   ADJSTAT,0          MINIMAL ADJUSTMENTS                           
         BAS   RE,CHKADJ          CHECK FOR POSTING AJUSTMENTS?                 
         CLI   FLAG2,C'N'         IF CREATING ADDITIONAL POSTINGS               
         BE    GENA85                                                           
         MVC   PSTAFLG,FLAG2                                                    
         CLI   FLAG2,C'A'          IF BACK OUT IS AOR                           
         BNE   *+12                                                             
         OI    ADJSTAT,ADJBILL2       MAKE SURE BILL2                           
         B     *+8                                                              
         NI    ADJSTAT,X'FF'-ADJBILL2 ELSE, MAKE SURE ITS NOT                   
         BAS   RE,CHKADJ                                                        
*                                                                               
GENA85   CLI   ACPBTYP,MBTTGSTI   INPUT GST ACCOUNT                             
         BE    GENA98                                                           
         CLI   ACPBTYP,MBTTPQI    IF INPUT PQ PST ACCOUNT                       
         BE    GENA98                                                           
         CLI   ACPBTYP,MBTTNSI    IF INPUT NS PST ACCOUNT                       
         BE    GENA98                                                           
         CLI   ACPBTYP,MBTTNBI    IF INPUT NB PST ACCOUNT                       
         BE    GENA98                                                           
         CLI   ACPBTYP,MBTTNFI    IF INPUT NF PST ACCOUNT                       
         BE    GENA98                                                           
         CLI   ACPBTYP,MBTTBCI    IF INPUT BC PST ACCOUNT                       
         BE    GENA98                                                           
         CLI   ACPBTYP,MBTTONI    IF INPUT ON PST ACCOUNT                       
         BE    GENA98                                                           
         CLI   ACPBTYP,MBTTPEI    IF INPUT PE PST ACCOUNT                       
         BE    GENA98                                                           
         CLI   ACPBTYP,MBTTSEL    SELLOFF INCOME?                               
         BE    GENA98                                                           
         CLI   ACPBTYP,MBTTARI    IF AOR INCOME                                 
         BNE   GENA100                                                          
         BAS   RE,CHKINC          AND IF MEDIA INCOME ACCOUNT EXISTS            
         BNE   GENA100                                                          
GENA98   BAS   RE,INDBILL2        INDICATE BILL2                                
*                                                                               
GENA100  BAS   RE,BUMP4NXT        BUMP R4 TO NEXT POSTING ROW                   
*                                                                               
GENA120  LA    R5,ACPRTNL(R5)     NEXT ACCOUNT                                  
         BCT   R3,GENA10                                                        
*                                                                               
GENAX    XIT1                                                                   
         EJECT                                                                  
* CHKPSTO - CHECK ANY PST IN BILL FOR PROVINCE                                  
*                                                                               
CHKPSTO  NTR1                                                                   
         XC    FULL,FULL                                                        
         LA    R0,ACPMCODE                                                      
         LA    RE,ACPPSTO          FIND PROVINCE POSITION IN TABLE              
CHKPSTO5 CLC   2(1,RE),ACPBTYP     IF IN TABLE                                  
         BE    CHKPSTO8                                                         
         LA    RE,L'ACPPSTO(RE)                                                 
         BCT   R0,CHKPSTO5                                                      
         B     EXPNO                                                            
*                                                                               
CHKPSTO8 CLI   3(RE),0             IF TAX CODE                                  
         BE    EXPNO                                                            
         MVC   FULL,8(RE)          SET PST AMOUNT                               
         B     EXPYES              GENERATE POSTING                             
         SPACE 2                                                                
* CHKPSTI - CHECK ANY PST IN BILL FOR PROVINCE AND OKAY TO POST                 
*                                                                               
CHKPSTI  NTR1                                                                   
         LA    R0,ACPMCODE                                                      
         LA    RE,ACPPSTI          FIND PROVINCE POSITION IN TABLE              
CHKPSTI5 CLC   2(1,RE),ACPBTYP     IF IN TABLE                                  
         BE    CHKPSTI8                                                         
         LA    RE,L'ACPPSTI(RE)                                                 
         BCT   R0,CHKPSTI5                                                      
         B     EXPNO                                                            
*                                                                               
CHKPSTI8 CLI   3(RE),0             AND IF TAX CODE                              
         BE    EXPNO                                                            
         B     EXPYES                                                           
         EJECT                                                                  
*---------------------------------------------------------------*               
* CHKADJ  - CHECKS PSTAFLG FOR ADDITIONAL POSTINGS TO MAKE      *               
*---------------------------------------------------------------*               
*                                                                               
CHKADJ   NTR1                                                                   
         CLI   PSTMFLG,C'N'                                                     
         BE    CHKADJ2                                                          
         CLI   PSTMFLG,C'M'                                                     
         BNE   CHKADJ4                                                          
*                                                                               
CHKADJ2  CLI   PSTAFLG,C'N'                                                     
         BE    CHKADJX                                                          
         CLI   PSTAFLG,C'M'                                                     
         BE    CHKADJX                                                          
*                                 TAKE CARE OF SPECIAL ADJUSTS                  
CHKADJ4  CLI   PSTMFLG,C'I'       IF BACK OUT POSTING WITH IOR                  
         BE    *+12                                                             
         CLI   PSTAFLG,C'I'                                                     
         BNE   CHKADJ10                                                         
         OC    ACPIOR,ACPIOR       AND NO IOR PERCENTAGE                        
         BZ    CHKADJX                                                          
*                                                                               
CHKADJ10 BAS   RE,BUMP4NXT         BUMP R4 TO NEXT POSTING ROW                  
         MVC   0(ACPRTNL,R4),0(R5) COPY OVER POSTING                            
         BAS   RE,BUMPCNT          BUMP POSTING COUNT                           
         LA    R1,ACPAMT-ACPBTYP  CLEAR OUT EXPRESSIONS                         
         AR    R1,R4                                                            
         XC    0(16,R1),0(R1)                                                   
*                                                                               
         TM    ADJSTAT,ADJBILL2                                                 
         BZ    *+8                                                              
         BAS   RE,INDBILL2        INDICATE BILL # 2 POSTING                     
         BAS   RE,ADJAMT          ADJ AMOUNT IF NECESSARY                       
         BAS   RE,ADJMEMO         ADJ MEMO IF NECESSARY                         
*                                                                               
         TM    ADJSTAT,ADJCOST    IF COSTING POSTINGS TOO                       
         BZ    CHKADJX                                                          
         BAS   RE,BUMP4NXT         BUMP R4 TO NEXT POSTING ROW                  
         MVC   0(ACPRTNL,R4),SVCOST                                             
         CLI   ROBFLAG,C'I'                                                     
         BNE   *+10                                                             
         MVC   0(ACPRTNL,R4),SVCOSTI                                            
         BAS   RE,BUMPCNT          BUMP POSTING COUNT                           
         TM    ADJSTAT,ADJBILL2                                                 
         BZ    *+8                                                              
         BAS   RE,INDBILL2        INDICATE BILL # 2 POSTING                     
*                                                                               
         LA    R1,ACPWHC-ACPBTYP                                                
         AR    R1,R4                                                            
         MVC   0(L'ACPBTYP,R1),ACPBTYP ELE TYPE WHICH COST IS FOR               
*                                                                               
         BAS   RE,ADJAMT                                                        
         BAS   RE,ADJMEMO                                                       
*                                                                               
CHKADJX  XIT1  REGS=(R4)                                                        
         EJECT                                                                  
*---------------------------------------------*                                 
* ADJAMT - ADJUST AMOUNT ACCORDING TO PSTAFLG *                                 
*---------------------------------------------*                                 
*                                                                               
ADJAMT   NTR1                                                                   
         CLI   PSTAFLG,C'0'        ZERO MEMO                                    
         BNE   ADJA10                                                           
         ZAP   DUB,=P'0'                                                        
         LA    R1,ACPAMT2-ACPBTYP                                               
         AR    R1,R4                                                            
         MVC   0(L'ACPAMT2,R1),DUB+2                                            
         B     ADJAX                                                            
*                                                                               
ADJA10   CLI   PSTAFLG,C'Z'       - MEMO AMOUNT OUT                             
         BNE   ADJA20                                                           
         ZAP   DUB,AMOUNT                                                       
         MP    DUB,=P'-1'                                                       
         LA    R1,ACPAMT2-ACPBTYP                                               
         AR    R1,R4                                                            
         MVC   0(L'ACPAMT2,R1),DUB+2                                            
         B     ADJAX                                                            
*                                                                               
ADJA20   CLI   PSTAFLG,C'A'       -AOR MEMO AMOUNT OUT                          
         BNE   ADJA30                                                           
         ZAP   DUB,ACPTACT                                                      
         MP    DUB,=P'-1'        AMOUNT = AOR                                   
         MP    DUB,=P'-1'        NOW MINUS IT                                   
         LA    R1,ACPAMT2-ACPBTYP                                               
         AR    R1,R4                                                            
         MVC   0(L'ACPAMT2,R1),DUB+2                                            
*                                                                               
ADJA30   CLI   PSTAFLG,C'I'       -IOR MEMO AMOUNT OUT                          
         BNE   ADJAX                                                            
         CLI   ACPSYS,C'P'         IORAMT IS NOW 6 BYTES LONG                   
         BNE   *+14                PRINT USES ALL 6(PACKED)                     
         ZAP   DUB,IORAMT          SPOT/NET USES 4 BYTES(HEX)                   
         B     *+12                                                             
         ICM   R1,15,IORAMT+2                                                   
         CVD   R1,DUB                                                           
         MP    DUB,=P'-1'        NOW MINUS IT                                   
         LA    R1,ACPAMT2-ACPBTYP                                               
         AR    R1,R4                                                            
         MVC   0(L'ACPAMT2,R1),DUB+2                                            
*                                                                               
ADJAX    B     EXPANDX                                                          
         EJECT                                                                  
*--------------------------------------------*                                  
* ADJMEMO - ADJUST MEMO ACCORDING TO PSTMFLG *                                  
*--------------------------------------------*                                  
*                                                                               
ADJMEMO  NTR1                                                                   
         CLI   PSTMFLG,C'0'        ZERO MEMO                                    
         BNE   ADJM10                                                           
         ZAP   DUB,=P'0'                                                        
         LA    R1,ACPMEMO2-ACPBTYP                                              
         AR    R1,R4                                                            
         MVC   0(L'ACPMEMO2,R1),DUB+2                                           
         B     ADJMX                                                            
*                                                                               
ADJM10   CLI   PSTMFLG,C'Z'       - MEMO AMOUNT OUT                             
         BNE   ADJM20                                                           
         ZAP   DUB,MEMO                                                         
         MP    DUB,=P'-1'                                                       
         LA    R1,ACPMEMO2-ACPBTYP                                              
         AR    R1,R4                                                            
         MVC   0(L'ACPMEMO2,R1),DUB+2                                           
         B     ADJMX                                                            
*                                                                               
ADJM20   CLI   PSTMFLG,C'A'       -AOR MEMO AMOUNT OUT                          
         BNE   ADJM30                                                           
         ZAP   DUB,ACPTACT                                                      
         MP    DUB,=P'-1'          MEMO = AOR                                   
         MP    DUB,=P'-1'          NOW MINUS IT                                 
         LA    R1,ACPMEMO2-ACPBTYP                                              
         AR    R1,R4                                                            
         MVC   0(L'ACPMEMO2,R1),DUB+2                                           
*                                                                               
ADJM30   CLI   PSTMFLG,C'I'       -IOR MEMO AMOUNT OUT                          
         BNE   ADJMX                                                            
         CLI   ACPSYS,C'P'         IORAMT IS NOW 6 BYTES LONG                   
         BNE   *+14                PRINT USES ALL 6(PACKED)                     
         ZAP   DUB,IORAMT          SPOT/NET USES 4 BYTES(HEX)                   
         B     *+12                                                             
         ICM   R1,15,IORAMT+2                                                   
         CVD   R1,DUB                                                           
         MP    DUB,=P'-1'        NOW MINUS IT                                   
         LA    R1,ACPMEMO2-ACPBTYP                                              
         AR    R1,R4                                                            
         MVC   0(L'ACPMEMO2,R1),DUB+2                                           
*                                                                               
ADJMX    B     EXPANDX                                                          
         EJECT                                                                  
*---------------------------------------*                                       
* DO1C - CREATE 1C POSTING              *                                       
*---------------------------------------*                                       
*                                                                               
DO1C     NTR1                                                                   
         BAS   RE,BUMP4NXT         BUMP R4 TO NEXT POSTING ROW                  
         BAS   RE,DOCCADJ            DO CC ADJUSTMENT IF ANY                    
         CLI   ROBFLAG,C'I'          IF COSTING FOR INTERNAL BILL/REV           
         BNE   *+14                                                             
         MVC   0(ACPRTNL,R4),SVCOSTI NOW MOVE IN INTERNAL COSTING               
         B     *+10                                                             
         MVC   0(ACPRTNL,R4),SVCOST  ELSE, USE REGULAR COSTING                  
         BAS   RE,BUMPCNT          BUMP POSTING COUNT                           
         OC    0(ACPRTNL,R4),0(R4)   IF NO COST LINE                            
         BNZ   DO1C20                                                           
         MVI   0(R4),MBTTCOS         FILL IN NECESSARY DETAILS                  
         CLI   ROBFLAG,C'I'                                                     
         BNE   *+8                                                              
         MVI   0(R4),MBTTINTC                                                   
         LA    R1,ACPSTAT-ACPBTYP                                               
         AR    R1,R4                                                            
         MVI   0(R1),ACPDEB          ALWAYS A DEBIT                             
*                                                                               
DO1C20   LA    R1,ACPWHC-ACPBTYP                                                
         AR    R1,R4                                                            
         MVC   0(L'ACPBTYP,R1),ACPBTYP ELE TYPE WHICH COST IS FOR               
*                                                                               
         LA    R1,ACPAMT2-ACPBTYP                                               
         AR    R1,R4                                                            
         MVC   0(L'AMOUNT,R1),AMOUNT                                            
         LA    R1,ACPMEMO2-ACPBTYP                                              
         AR    R1,R4                                                            
         MVC   0(L'MEMO,R1),MEMO                                                
*                                                                               
         TM    ADJSTAT,ADJBILL2    IF INDICATING BILL2                          
         BZ    *+8                                                              
         BAS   RE,INDBILL2         SET BILL2 ON                                 
         XIT1  REGS=(R4)                                                        
         EJECT                                                                  
*---------------------------------------*                                       
* DOCCADJ - DO COSTING CENTER ADJUSTMENT*                                       
*---------------------------------------*                                       
*                                                                               
DOCCADJ  NTR1                                                                   
         CLI   ROBFLAG,C'R'       REGULAR BIL/REV 1C ROW                        
         BNE   DOCC10                                                           
         MVC   WORK(1),CCMEDPOS                                                 
         MVC   WORK+1(L'CCMEDINC),CCMEDINC                                      
         BAS   RE,REPLCC          REPLACE ACCOUNT WITH CC OVERRIDE              
         B     DOCCX                                                            
*                                                                               
DOCC10   CLI   ROBFLAG,C'A'       AOR BILL/REV 1C ROW                           
         BNE   DOCC20                                                           
         MVC   WORK(1),CCAORPOS                                                 
         MVC   WORK+1(L'CCAORINC),CCAORINC                                      
         BAS   RE,REPLCC          REPLACE ACCOUNT WITH CC OVERRIDE              
         B     DOCCX                                                            
*                                                                               
DOCC20   CLI   ROBFLAG,C'S'       SELLOFF BILL/REV 1C ROW                       
         BNE   DOCC30                                                           
         MVC   WORK(1),CCSELPOS                                                 
         MVC   WORK+1(L'CCSELINC),CCSELINC                                      
         BAS   RE,REPLCC          REPLACE ACCOUNT WITH CC OVERRIDE              
*                                                                               
DOCC30   CLI   ROBFLAG,C'I'       INTERNAL BILL/REV 1C ROW                      
         BNE   DOCCX                                                            
         MVC   WORK(1),CCINTPOS                                                 
         MVC   WORK+1(L'CCINTINC),CCINTINC                                      
         BAS   RE,REPLCC          REPLACE ACCOUNT WITH CC OVERRIDE              
*                                                                               
DOCCX    B     EXPANDX                                                          
         EJECT                                                                  
*---------------------------------------------*                                 
* REPLCC - MOVES COSTING CENTER OVERRIDE      *                                 
*          INTO 1C ACCOUNT AT POSTION GIVEN   *                                 
* ENTRY - WORK(1) - POSTION TO INSERT         *                                 
*         WORK+1(3) - CHARACTERS FOR OVERRIDE *                                 
* IF WORK+1(3)=SPACES DOESN'T OVERRIDE 1C     *                                 
*---------------------------------------------*                                 
*                                                                               
REPLCC   NTR1                                                                   
         LA    R1,ACPACC-ACPBTYP                                                
         LA    R1,2(R1)           PT PASSED UNIT/LEDGER                         
         CLI   ROBFLAG,C'I'       IF COSTING FOR INTERNAL BILL/REV              
         BNE   *+12                                                             
         LA    R3,SVCOSTI         USE INTERNAL COSTING                          
         B     *+8                                                              
         LA    R3,SVCOST          ELSE, USE REGULAR COSTING ROW                 
         AR    R3,R1                                                            
*                                                                               
         CLI   WORK,0             ANY PARTICULAR POSTION TO OVERRIDE            
         BNE   *+12                                                             
         LA    R3,7(R3)           OLD WAY -OVERRIDE AT +7                       
         B     REPLCC10                                                         
         SR    R0,R0                                                            
         IC    R0,WORK                                                          
         AR    R3,R0              PT TO POSTION IN ACCOUNT TO OVERRIDE          
*                                                                               
REPLCC10 BCTR  R3,0                                                             
*                                                                               
REPLCC20 LA    RE,3               3 POSITIONS MAX TO BE OVERRIDEN               
         LA    R1,WORK+1          PT TO OVERRIDE CHARACTERS                     
         OC    WORK+1,SPACES                                                    
REPLCC25 CLI   0(R1),C' '         CANNOT OVERRIDE WITH A SPACE                  
         BE    *+10                                                             
         MVC   0(1,R3),0(R1)      MOVE IN OVERRIDE CHARACTERS                   
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,REPLCC25                                                      
         B     EXPANDX                                                          
         EJECT                                                                  
*---------------------------------------*                                       
* MINUSIT - MINUS AMOUNT AND MEMO       *                                       
*---------------------------------------*                                       
*                                                                               
MINUSIT  NTR1                                                                   
         OC    AMOUNT,AMOUNT                                                    
         BZ    MINUS5                                                           
         XI    AMOUNT+L'AMOUNT-1,X'01'  MULTIPLY BY -1 TO FLIP THE SIGN         
MINUS5   LA    R1,ACPAMT2-ACPBTYP                                               
         AR    R1,R4                                                            
         MVC   0(L'AMOUNT,R1),AMOUNT                                            
*                                                                               
         OC    MEMO,MEMO                                                        
         BZ    MINUS10                                                          
         XI    MEMO+L'MEMO-1,X'01'      MULTIPLY BY -1 TO FLIP THE SIGN         
MINUS10  LA    R1,ACPMEMO2-ACPBTYP                                              
         AR    R1,R4                                                            
         MVC   0(L'MEMO,R1),MEMO                                                
         B     EXPANDX                                                          
         SPACE 2                                                                
*---------------------------------*                                             
* - MARKS POSTING BILL2 POSTING   *                                             
*---------------------------------*                                             
*                                                                               
INDBILL2 NTR1                                                                   
         LA    R1,ACPBIL2-ACPBTYP                                               
         AR    R1,R4                                                            
         MVI   0(R1),X'02'        INDICATE BILL2 POSTING                        
         B     EXPANDX                                                          
         EJECT                                                                  
*============================================================*                  
* SETAMTS - CONVERTS AMT EXPRESSION TO VALID AMOUNT          *                  
*           MEMO FIRST--THEN AMOUNT (IMPORTANT FOR INPUT GST)*                  
*============================================================*                  
*                                                                               
SETAMTS  NTR1                                                                   
         MVI   PSTMFLG,C'N'       DEFAULT FLAG - NO ADJUSTS                     
         MVI   PSTAFLG,C'N'       DEFAULT FLAG - NO ADJUSTS                     
         LA    R1,ACPAMT-ACPBTYP                                                
         AR    R1,R4                                                            
         XC    0(16,R1),0(R1)     CLEAR OUT AMT EXPRESSIONS                     
*                                                                               
         XC    MEMO,MEMO         ACTUAL AMOUNT                                  
         MVI   AMTSW,C'M'        CONVERT MEMO EXP TO VALID MEMO AMOUNT          
         BAS   RE,GETAMT                                                        
         CLI   ACPBTYP,MBTTRCV   IF MEMO TO RCVBL ROW                           
         BNE   SETAMT2                                                          
         CLC   =C'SJ',ACPACC     AND IF ACC = SJ & PROF =NO                     
         BNE   SETAMT1A                                                         
         CLI   ACPNMEM,C'N'                                                     
         BNE   SETAMT1A                                                         
         XC    MEMO,MEMO         SHOW NO MEMO                                   
         B     SETAMT8                                                          
*                                                                               
SETAMT1A CLI   ACPMEMO,C'C'      OTHERWISE, CALCULATE MEMO IF C                 
         BNE   SETAMT2                                                          
         CLI   PBILCDSW,C'S'                                                    
         BNE   SETAMT1B                                                         
         CLI   PBILSEP,C'C'                                                     
         BNE   SETAMT1B                                                         
         ZAP   MEMO,PBILLRCV      FOR CD BILL CD=RCVBL AMOUNT                   
         B     SETAMT2                                                          
*                                                                               
SETAMT1B XI    MEMO+L'MEMO-1,X'01' CD IS (-) UNLESS  *FLIP THE SIGN             
         TM    PBILBASA,X'04'     BILL FORM LESS CD AND                         
         BNO   SETAMT2                                                          
         MVC   DUB,ACPAMT                                                       
         OC    DUB,SPACES                                                       
         CLC   =CL8'B',DUB        AMOUNT IS B OR B+T                            
         BE    SETAMT1                                                          
         CLC   =CL8'B+T',DUB                                                    
         BNE   SETAMT2                                                          
SETAMT1  XI    MEMO+L'MEMO-1,X'01'  THAN CD MEMO IS + *FLIP THE SIGN            
*                                                                               
SETAMT2  CLI   ACPBTYP,MBTTAPR   IF MEMO TO AOR PAY/RCVBL                       
         BNE   *+8                                                              
         B     *+4                                                              
         CLI   PSTMFLG,C'M'      MINUS AMOUNT?                                  
         BE    SETAMT4                                                          
         BAS   RE,CHKFNPST       CHECK FINANCIAL POSTINGS NEED MINUS            
         BNE   SETAMT8                                                          
SETAMT4  OC    MEMO,MEMO                                                        
         BZ    SETAMT8                                                          
         XI    MEMO+L'MEMO-1,X'01'  SAME AS MP BY -1 TO FLIP THE SIGN           
*                                                                               
SETAMT8  LA    R1,ACPMEMO2-ACPBTYP                                              
         AR    R1,R4                                                            
         MVC   0(L'MEMO,R1),MEMO                                                
*                                                                               
         XC    AMOUNT,AMOUNT      ACTUAL AMOUNT                                 
         MVI   AMTSW,C'A'         CONVERT AMT EXP TO VALID AMOUNT               
         BAS   RE,GETAMT                                                        
         CLI   PSTAFLG,C'M'       MINUS AMOUNT?                                 
         BE    SETAMT10                                                         
         BAS   RE,CHKFNPST       CHECK FINANCIAL POSTINGS NEED MINUS            
         BNE   SETAMT15                                                         
SETAMT10 OC    AMOUNT,AMOUNT                                                    
         BZ    SETAMT15                                                         
         XI    AMOUNT+L'AMOUNT-1,X'01'  SAME AS MP BY -1 TO FLIP THE            
*                                       SIGN                                    
SETAMT15 LA    R1,ACPAMT2-ACPBTYP                                               
         AR    R1,R4                                                            
         MVC   0(L'AMOUNT,R1),AMOUNT                                            
*                                                                               
         B     EXPANDX                                                          
         EJECT                                                                  
*=======================================*                                       
* GETAMT - LOOKS UP EXPRESSION IN TABLE *                                       
*=======================================*                                       
*                                                                               
GETAMT   NTR1                                                                   
         CLI   AMTSW,C'A'                                                       
         BNE   GETAMT2                                                          
         MVC   EXP,ACPAMT                                                       
         B     GETAMT5                                                          
*                                                                               
GETAMT2  MVC   EXP,ACPMEMO                                                      
*                                                                               
GETAMT5  OC    EXP,SPACES                                                       
         CLC   EXP,SPACES                                                       
         BE    GETAMTX                                                          
*                                                                               
         MVC   EXPEQU,ACPBTYP      SET EQUATED ACCOUNT                          
*                                                                               
         L     R1,=A(EXPTBL)      PT TO TABLE OF AMOUNT EXPRESSIONS             
         A     R1,RELO                                                          
         USING EXPD,R1                                                          
GETAMT8  CLI   0(R1),X'FF'        END OF TABLE?                                 
         BNE   *+6                                                              
         DC    H'0'               MUST HAVE MATCH                               
         CLC   EXP,0(R1)          MATCH ON EXPRESSION                           
         BE    GETAMT10           YES                                           
         LA    R1,EXPL(R1)                                                      
         B     GETAMT8                                                          
*                                                                               
GETAMT10 DS    0H                                                               
         MVC   FLAG2,EXPFLG2      SET EXTRA POSTING FLAG                        
         MVC   FLAG3,EXPFLG3      SET ADD/SUBTRACE VALUE FLAG                   
         ICM   RF,15,EXPRTN       ROUTINE ADDRESS                               
         A     RF,RELO                                                          
         BASR  RE,RF              REAL AMOUNT BACK IN AMOUNT/MEMO               
*                                                                               
         CLI   AMTSW,C'A'                                                       
         BNE   *+14                                                             
         MVC   PSTAFLG,EXPFLG     SET AMOUNT FLAG                               
         B     *+10                                                             
         MVC   PSTMFLG,EXPFLG     SET MEMO FLAG                                 
*                                                                               
GETAMTX  B     EXPANDX                                                          
         DROP  R5                                                               
         EJECT                                                                  
*--------------------------------------------------*                            
* CHKFNPST-CHECKS FINANCIAL POSTINGS 'DIFF' KEYWORD*                            
*          BUT DEPENDING ON IF B1 OR R1 BILLING    *                            
*          MIGHT BE -DIFF                          *                            
*          CC SET = IF - DIFF NEEDED               *                            
*--------------------------------------------------*                            
*                                                                               
CHKFNPST NTR1                                                                   
         CLI   0(R4),MBTTRCR      IF REBATE ROW?                                
         BE    EXPYES             NEED -                                        
         CLI   0(R4),MBTTRCS      IF SUSPENSE ROW                               
         BNE   EXPNO              DON'T NEED -                                  
         CLI   PBILLTYP,C'R'      R1 (NO -), B1 (NEED -)                        
         BE    EXPNO                                                            
         B     EXPYES                                                           
         SPACE 2                                                                
*---------------------------------------------------------*                     
* CHKINC   - CHECKS FOR EXISTANCE OF MEDIA INCOME ACCOUNT *                     
*---------------------------------------------------------*                     
*                                                                               
CHKINC   NTR1                                                                   
         LA    R1,MBTTINC         MEDIA INCOME ACCOUNT                          
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         LA    RF,ACPRTNL                                                       
         MR    RE,R1                                                            
         L     RE,ACPPOST         POSTINGS                                      
         AR    RE,RF                                                            
         USING ACPRTND,RE                                                       
         OC    ACPACC,ACPACC      NO INCOME ROW                                 
         BZ    EXPNO                                                            
         OC    ACPAMT,ACPAMT      INCOME ROW EXISTS - BUT ANY AMOUNTS?          
         BNZ   EXPYES                                                           
         OC    ACPMEMO,ACPMEMO                                                  
         BZ    EXPNO              NO AMOUNTS                                    
         B     EXPYES                                                           
         DROP  RE                                                               
         EJECT                                                                  
* CHKAPR - CHECK OKAY TO POST TO INPUT GST/PST ACCOUNT                          
*                                                                               
CHKAPR   NTR1                                                                   
         LA    R1,MBTTAPR          AOR PAYABLE/RECEIVABLE ACCOUNT               
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         LA    RF,ACPRTNL                                                       
         MR    RE,R1                                                            
         L     RE,ACPPOST          POSTINGS                                     
         AR    RE,RF                                                            
         USING ACPRTND,RE                                                       
         OC    ACPACC,ACPACC       IF POSTING TO AOR PAY/RECEIV                 
         BZ    EXPNO                                                            
         OC    ACPAMT,ACPAMT                                                    
         BNZ   *+14                                                             
         OC    ACPMEMO,ACPMEMO                                                  
         BZ    EXPNO                                                            
*                                                                               
         LA    R1,PAYTBL           AND UNIT/LEDGER IN TABLE                     
CHKAPR8  CLC   =C'FF',0(R1)                                                     
         BE    EXPNO                                                            
         CLC   ACPACC(2),0(R1)                                                  
         BE    EXPYES              OKAY TO POST INPUT GST/PST                   
         LA    R1,2(R1)                                                         
         B     CHKAPR8                                                          
         DROP  RE                                                               
         SPACE 2                                                                
PAYTBL   DC    C'SRSPSQSSSTSUSVSXSWSYFF'                                        
         EJECT                                                                  
*---------------------------------------*                                       
* CALCULATING ROUTINES - ALL RTNS       *                                       
* XIT -THROUGH EXPX WHICH CHECKS FLAG3  *                                       
*---------------------------------------*                                       
*                                                                               
BILL     NTR1                     CALCULATE ACTUAL AMOUNT                       
         CLI   ACPSYS,C'P'                                                      
         BNE   BIL10                                                            
         ZAP   DUB,PBILLRCV       PRINT AMOUNT                                  
         B     BIL15                                                            
BIL10    ZAP   DUB,BACTP          SPOT/NET AMOUNT                               
*                                                                               
BIL15    CLI   AMTSW,C'A'                                                       
         BNE   BIL20                                                            
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     BILX                                                             
BIL20    ZAP   MEMO,DUB+2(6)                                                    
BILX     B     EXPX               CHECK IF ANY AMOUNT TO +/-                    
*                                                                               
*                                                                               
*                                                                               
NET      NTR1                     CALCULATE NET AMT                             
         CLI   ACPSYS,C'P'                                                      
         BNE   NET10                                                            
         ZAP   DUB,=P'0'          NET = 0                                       
         TM    PBILCMSW,X'02'     FOR COMM ONLY BILLS                           
         BO    NET2                                                             
         TM    PBILCMSW,X'01'     AND FOR UPFRONT COMMISSION BILLS              
         BO    NET2                                                             
*                                                                               
         TM    PBILCMSW,X'08'      IF UPFRONT NET (AOR OR REGULAR)              
         BZ    NET1                                                             
         ZAP   DUB,PBILLNET        USE NET AMOUNT                               
         CLC   =X'0202',PBILBASA   UNLESS BILL FORMULA                          
         BNE   NET15                                                            
         ZAP   DUB,PBILLRCV        SAYS TO USE RCVBL AMOUNT                     
         B     NET15                                                            
*                                                                               
NET1     ZAP   DUB,PBILLNET       NET= NET AMT FOR ALL EXCEPT CD BILLS          
NET2     CLI   PBILCDSW,C'S'      CD TYPE BILLS?                                
         BNE   NET15                                                            
         CLI   PBILSEP,C'C'       YES                                           
         BNE   *+14                                                             
         ZAP   DUB,PBILLRCV       NET AMT=RCV AMT FOR CD BILL                   
         B     NET15                                                            
         BAS   RE,CALCD                                                         
         AP    DUB,CDAMT          NET=NET+CD FOR MAIN CD & ADJ CD BILLS         
         B     NET15                                                            
*                                                                               
NET10    ZAP   DUB,=P'0'                                                        
         TM    BILSTAT,X'02'      FOR COMM ONLY BILLS                           
         BO    NET15                                                            
         TM    BILSTAT,BSTSCOMQ   OR UPFRONT COMMISSION-> NET=0                 
         BO    NET15                                                            
         ZAP   DUB,BNETP          SPOT/NET AMOUNT                               
NET15    CLI   AMTSW,C'A'                                                       
         BNE   NET20                                                            
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     NETX                                                             
NET20    ZAP   MEMO,DUB+2(6)                                                    
NETX     B     EXPX               CHECK IF ANY AMOUNT TO +/-                    
         EJECT                                                                  
*                                                                               
*                                                                               
AOR      NTR1                     CALC ACTUAL AMT(FROM TRUE AOR BILL)           
         ZAP   DUB,=P'0'                                                        
         OC    ACPTACT,ACPTACT                                                  
         BZ    *+10                                                             
         ZAP   DUB,ACPTACT                                                      
         CLI   AMTSW,C'A'                                                       
         BNE   AOR20                                                            
         ZAP   AMOUNT,DUB+2(6)                                                  
         XI    AMOUNT+L'AMOUNT-1,X'01' SAME AS MP TO FLIP THE SIGN              
         B     AORX                                                             
*                                                                               
AOR20    ZAP   MEMO,DUB+2(6)                                                    
         XI    MEMO+L'MEMO-1,X'01'     SAME AS MP TO FLIP THE SIGN              
AORX     B     EXPX               CHECK IF ANY AMOUNT TO +/-                    
*                                                                               
*                                                                               
*                                                                               
AORIVAT  NTR1                     CALC ACTUAL AMT(FROM TRUE AOR BILL)           
         ZAP   DUB,ACPTACT                                                      
         MP    DUB,=P'-1'         ALWAYS                                        
         ICM   R1,15,ACPTGST      TRUE AOR GST                                  
         CVD   R1,DUB2                                                          
         MP    DUB2,=P'-1'        ALWAYS                                        
         AP    DUB,DUB2                                                         
*                                                                               
         LA    R1,ACPPSTI                                                       
         LA    R0,ACPMCODE                                                      
AORIVAT5 ICM   RE,15,8(R1)         TRUE AOR PST                                 
         CVD   RE,DUB2                                                          
         MP    DUB2,=P'-1'         ALWAYS                                       
         AP    DUB,DUB2                                                         
         LA    R1,L'ACPPSTI(R1)                                                 
         BCT   R0,AORIVAT5                                                      
*                                                                               
         CLI   AMTSW,C'A'                                                       
         BNE   AORIV20                                                          
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     AORIVX                                                           
AORIV20  ZAP   MEMO,DUB+2(6)                                                    
AORIVX   B     EXPX               CHECK IF ANY AMOUNT TO +/-                    
         EJECT                                                                  
*                                                                               
* INC FOR REG BILLS IS RCVBL-NET (THEN -CD DEPENDING ON BILL FORM)              
* FOR SEP CD-CD BILL=0 , ADJ CD & MAIN CD = RCVBL-NET-CD ALWAYS                 
*                                                                               
INC      NTR1                     CALCULATE AGENCY COMMISION                    
         CLI   ACPSYS,C'P'                                                      
         BNE   INC10                                                            
         CLI   PBILCDSW,C'S'      CD TYPE BILLS?                                
         BNE   INC1                                                             
         CLI   PBILSEP,C'C'       YES                                           
         BNE   INC1                                                             
         ZAP   DUB2,=P'0'         INC=0 FOR CD BILL                             
         B     INC18                                                            
*                                                                               
INC1     ZAP   DUB2,PBILLRCV      INC = RCVBL-NET                               
         ZAP   DUB,=P'0'          NET = 0                                       
         TM    PBILCMSW,X'01'     FOR UPFRONT COMMISSION BILLS                  
         BO    INC2                                                             
         TM    PBILCMSW,X'02'     AND FOR COMM ONLY BILLS                       
         BO    INC2                                                             
         ZAP   DUB,PBILLNET                                                     
INC2     SP    DUB2,DUB                                                         
         CLC   =C'FIN',PRTTYPE                                                  
         BNE   INC3                                                             
         BAS   RE,CALDIFF         TAKE DIFF OUT OF INC FOR FIN BILLS            
         SP    DUB2,DIFFAMT                                                     
INC3     CLI   PBILCDSW,C'S'      CD BILL TYPES?                                
         BE    INC9               YES-ADJ & MAIN CD ALWAYS -CD                  
         TM    PBILBASA,X'04'     OTHER BILLS - TEST BILL FORM LESS CD          
         BO    INC18                                                            
INC9     BAS   RE,CALCD                                                         
         SP    DUB2,CDAMT         TAKE CD OUT OF INC                            
         B     INC18                                                            
*                                                                               
INC10    ZAP   DUB2,BACTP         SPOT/NET ACTUAL BILL AMOUNT                   
         ZAP   DUB,=P'0'                                                        
         TM    BILSTAT,X'02'      FOR COMM ONLY BILLS                           
         BO    INC15                                                            
         TM    BILSTAT3,BSTTRCNQ  TRADE W/ CALC NET                             
         BNO   INC12                                                            
         SR    RF,RF                                                            
         ICM   RF,15,BCLDNET                                                    
         CVD   RF,DUB                                                           
         B     INC15                                                            
INC12    TM    BILSTAT,BSTSCOMQ   OR UPFRONT COMM -> NET=0                      
         BO    INC15                                                            
         ZAP   DUB,BNETP          SPOT/NET NET BILL AMOUNT                      
INC15    SP    DUB2,DUB                                                         
*                                                                               
INC18    CLI   AMTSW,C'A'                                                       
         BNE   INC20                                                            
         ZAP   AMOUNT,DUB2+2(6)                                                 
         B     INCX                                                             
INC20    ZAP   MEMO,DUB2+2(6)                                                   
INCX     B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
         EJECT                                                                  
*                                                                               
*                                                                               
XINC     NTR1                     BCNET - CALCULATED NET FOR SPOT               
         CLI   ACPSYS,C'S'                                                      
         BNE   EXPX                                                             
         SR    RF,RF                                                            
         ICM   RF,15,BCLDNET                                                    
         CVD   RF,DUB                                                           
         CLI   AMTSW,C'A'                                                       
         BNE   XINC10                                                           
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     XINCX                                                            
XINC10   ZAP   MEMO,DUB+2(6)                                                    
XINCX    B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
*                                                                               
TRA      NTR1                      XPROF % BASIS                                
         ZAP   DUB,=P'0'                                                        
         ICM   RF,15,ACPTPCT       TRADE % WITH 3 DECIMAL PLACES                
         LTR   RF,RF                                                            
         BZ    TRA15                                                            
         CVD   RF,DUB                                                           
         ZAP   PNUM(16),DUB                                                     
*                                                                               
         ZAP   DUB,=P'0'                                                        
         CLI   ACPTBAS,C'G'         USE GROSS AS THE BASIS?                     
         BNE   *+14                                                             
         ZAP   DUB,BGRSP                                                        
         B     TRA10                                                            
         CLI   ACPTBAS,C'N'         USE NET AS THE BASIS?                       
         BNE   TRA10                                                            
         SR    RF,RF                                                            
         ICM   RF,15,BCLDNET                                                    
         CVD   RF,DUB                                                           
*                                                                               
TRA10    MP    PNUM(16),DUB                                                     
         SRP   PNUM,64-5,5         DIVIDE AND ROUND                             
         ZAP   DUB,PNUM+8(8)                                                    
TRA15    CLI   AMTSW,C'A'                                                       
         BNE   TRA20                                                            
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     TRAX                                                             
TRA20    ZAP   MEMO,DUB+2(6)                                                    
TRAX     B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
*                                                                               
OINC     NTR1                                                                   
         CLI   ACPSYS,C'P'        MIDAS COMMISSION                              
         BE    OINC20             BNET2P-BNETP                                  
         ZAP   DUB,BNET2P                                                       
         SP    DUB,BNETP                                                        
         B     OINC100                                                          
*                                                                               
OINC20   ZAP   DUB,=P'0'          PBILLOPN-PBILLNET                             
         LA    RE,PBILLEL                                                       
         USING PBILOTH,RE                                                       
         SR    R0,R0                                                            
OINC25   CLI   0(RE),0                                                          
         BE    OINC35                                                           
         CLI   0(RE),X'09'                                                      
         BE    OINC30                                                           
         ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     OINC25                                                           
OINC30   ZAP   DUB,PBILLOPN       NET FOR COS2 BILLS                            
OINC35   SP    DUB,PBILLNET                                                     
*                                                                               
OINC100  CLI   AMTSW,C'A'                                                       
         BNE   OINC110                                                          
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     OINCX                                                            
OINC110  ZAP   MEMO,DUB+2(6)                                                    
OINCX    B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
         DROP  RE                                                               
*                                                                               
*                                                                               
AINC     NTR1                                                                   
         CLI   ACPSYS,C'P'        MAXUS COMMISSION                              
         BE    AINC20             (BACTP-BNETP)-(BNET2P-BNETP)                  
         ZAP   DUB,BACTP                                                        
         SP    DUB,BNETP                                                        
         ZAP   DUB2,BNET2P                                                      
         SP    DUB2,BNETP                                                       
         SP    DUB,DUB2                                                         
         B     AINC100                                                          
*                                                                               
AINC20   ZAP   DUB,PBILLRCV    (PBILLRCV-PBILLNET)-(PBILLOPN-PBILLNET)          
         SP    DUB,PBILLNET                                                     
         ZAP   DUB2,=P'0'                                                       
         LA    RE,PBILLEL                                                       
         USING PBILOTH,RE                                                       
         SR    R0,R0                                                            
AINC25   CLI   0(RE),0                                                          
         BE    AINC35                                                           
         CLI   0(RE),X'09'                                                      
         BE    AINC30                                                           
         ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     AINC25                                                           
AINC30   ZAP   DUB2,PBILLOPN       NET FOR COS2 BILLS                           
AINC35   SP    DUB2,PBILLNET                                                    
         SP    DUB,DUB2                                                         
*                                                                               
AINC100  CLI   AMTSW,C'A'                                                       
         BNE   AINC110                                                          
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     AINCX                                                            
AINC110  ZAP   MEMO,DUB+2(6)                                                    
AINCX    B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
         DROP  RE                                                               
*                                                                               
NETB     NTR1                                                                   
         CLI   ACPSYS,C'P'        NET BUDGET (MEMO)                             
         BE    NETB20             BNET2P+(-BCLDNET)                             
         ZAP   DUB,BNET2P                                                       
         SR    RF,RF                                                            
         ICM   RF,15,BCLDNET      TRADE CREDIT (ON REGM BILLS)                  
         CVD   RF,DUB2                                                          
         SP    DUB,DUB2           ADD TO GET MIDAS EXCHANGE AMT                 
         B     NETB100                                                          
*                                                                               
NETB20   ZAP   DUB,=P'0'                                                        
         LA    RE,PBILLEL         PBILLOPN+(-PBILTRCD)                          
         USING PBILOTH,RE                                                       
         SR    R0,R0                                                            
NETB25   CLI   0(RE),0                                                          
         BE    NETB35                                                           
         CLI   0(RE),X'09'                                                      
         BE    NETB30                                                           
         ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     NETB25                                                           
NETB30   ZAP   DUB,PBILLOPN       NET FOR COS2 BILLS                            
NETB35   SR    RF,RF                                                            
         ICM   RF,15,PBILTRCD     TRADE CREDIT (ON REGM BILLS)                  
         CVD   RF,DUB2                                                          
         SP    DUB,DUB2           ADD TO GET MIDAS EXCHANGE AMT                 
*                                                                               
NETB100  CLI   AMTSW,C'A'                                                       
         BNE   NETB110                                                          
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     NETBX                                                            
NETB110  ZAP   MEMO,DUB+2(6)                                                    
NETBX    B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
         DROP  RE                                                               
*                                                                               
TRDCR    NTR1                                                                   
         CLI   ACPSYS,C'P'        TRADE CREDIT (MEMO)                           
         BE    TRDC20                                                           
         ZAP   DUB,BCLDNET                                                      
         B     TRDC100                                                          
*                                                                               
TRDC20   ZAP   DUB,PBILTRCD                                                     
*                                                                               
TRDC100  CLI   AMTSW,C'A'                                                       
         BNE   TRDC110                                                          
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     TRDCX                                                            
TRDC110  ZAP   MEMO,DUB+2(6)                                                    
TRDCX    B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
*                                                                               
AFEE     NTR1                     INC-AOR                                       
         CLI   ACPSYS,C'P'                                                      
         BNE   AFEE10                                                           
         CLI   PBILCDSW,C'S'      CD TYPE BILLS?                                
         BNE   AFEE2                                                            
         CLI   PBILSEP,C'C'                                                     
         BNE   AFEE2                                                            
         ZAP   DUB2,=P'0'         INC = 0 FOR CD BILL                           
         B     AFEE18                                                           
*                                                                               
AFEE2    ZAP   DUB2,PBILLRCV      INC = RCVBL - NET                             
         ZAP   DUB,=P'0'          NET = 0                                       
         TM    PBILCMSW,X'01'     FOR UPFRONT COMMISSION BILLS                  
         BO    AFEE3                                                            
         TM    PBILCMSW,X'02'     AND FOR COMM ONLY BILLS                       
         BO    AFEE3                                                            
         ZAP   DUB,PBILLNET                                                     
AFEE3    SP    DUB2,DUB                                                         
         CLC   =C'FIN',PRTTYPE                                                  
         BNE   AFEE5                                                            
         BAS   RE,CALDIFF         TAKE DIFF OUT OF INC                          
         SP    DUB2,DIFFAMT                                                     
AFEE5    CLI   PBILCDSW,C'S'      CD BILL TYPES?                                
         BE    AFEE08             YES - ALWAYS TAKE OUT CD                      
         TM    PBILBASA,X'04'     NO -  TEST BILL FORM LESS CD                  
         BO    AFEE18                                                           
AFEE08   BAS   RE,CALCD           TAKE CD OUT OF INC                            
         SP    DUB2,CDAMT                                                       
         B     AFEE18                                                           
*                                                                               
AFEE10   ZAP   DUB2,BACTP         SPOT/NET BILL AMOUNT                          
         ZAP   DUB,=P'0'                                                        
         TM    BILSTAT,X'02'      FOR COMM ONLY BILLS                           
         BO    AFEE15                                                           
         TM    BILSTAT3,BSTTRCNQ  TRADE W/ CALC NET                             
         BNO   AFEE12                                                           
         SR    RF,RF                                                            
         ICM   RF,15,BCLDNET                                                    
         CVD   RF,DUB                                                           
         B     AFEE15                                                           
AFEE12   TM    BILSTAT,BSTSCOMQ   OR UPFRONT COMM -> NET=0                      
         BO    AFEE15                                                           
         ZAP   DUB,BNETP          SPOT/NET NET AMOUNT                           
AFEE15   SP    DUB2,DUB           INC = DUB2+2(6)                               
*                                                                               
AFEE18   ZAP   DUB,=P'0'                                                        
         OC    ACPTACT,ACPTACT                                                  
         BZ    *+10                                                             
         ZAP   DUB,ACPTACT                                                      
         MP    DUB,=P'-1'         ALWAYS                                        
         SP    DUB2+2(6),DUB+2(6)                                               
         CLI   AMTSW,C'A'                                                       
         BNE   AFEE20                                                           
         ZAP   AMOUNT,DUB2+2(6)    AMOUNT =INC - AOR                            
         B     AFEEX                                                            
AFEE20   ZAP   MEMO,DUB2+2(6)      AMOUNT = INC - AOR                           
AFEEX    B     EXPX               CHECK IF ANY AMOUNT TO +/-                    
         EJECT                                                                  
*                                                                               
*                                                                               
GRS      NTR1                                                                   
         CLI   ACPSYS,C'P'                                                      
         BNE   GRS10                                                            
         ZAP   DUB,PBILLGRS       PRINT GROSS AMOUNT                            
         B     *+10                                                             
GRS10    ZAP   DUB,BGRSP          PRINT GROSS AMOUNT                            
         CLI   AMTSW,C'A'                                                       
         BNE   GRS20                                                            
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     GRSX                                                             
GRS20    ZAP   MEMO,DUB+2(6)                                                    
GRSX     B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
*                                                                               
*                                                                               
IOR      NTR1                                                                   
         CLI   ACPSYS,C'P'         IORAMT IS NOW 6 BYTES LONG                   
         BNE   *+14                PRINT USES ALL 6(PACKED)                     
         ZAP   DUB,IORAMT          SPOT/NET USES 4 BYTES(HEX)                   
         B     *+12                                                             
         ICM   R1,15,IORAMT+2      % OF GROSS                                   
         CVD   R1,DUB                                                           
         CLI   AMTSW,C'A'                                                       
         BNE   IOR20                                                            
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     IORX                                                             
IOR20    ZAP   MEMO,DUB+2(6)                                                    
IORX     B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
*                                                                               
*                                                                               
GX       NTR1                                                                   
         CLI   ACPSYS,C'P'                                                      
         BNE   GX10                                                             
         ZAP   DUB,PBILLGRS       PRINT GROSS AMOUNT                            
         BAS   RE,GETPTAX                                                       
         ICM   RE,15,FULL                                                       
         CVD   RE,DUB2                                                          
         B     GX15                                                             
GX10     ZAP   DUB,BGRSP          SPOT/NET GROSS AMOUNT                         
         XR    RE,RE              SALES TAX = 0                                 
         TM    BILSTAT,BSTSCOMQ   FOR UPFRONT COMM                              
         BO    *+8                                                              
         ICM   RE,15,BTAXAMT                                                    
         CVD   RE,DUB2                                                          
*                                                                               
GX15     SP    DUB,DUB2                                                         
         CLI   AMTSW,C'A'                                                       
         BNE   GX20                                                             
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     GSX                                                              
GX20     ZAP   MEMO,DUB+2(6)                                                    
GSX      B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
         EJECT                                                                  
*                                                                               
*                                                                               
ZERO     NTR1                                                                   
         CLI   AMTSW,C'A'                                                       
         BNE   ZERO20                                                           
         ZAP   AMOUNT,=P'0'                                                     
         B     ZEROX                                                            
*                                                                               
ZERO20   ZAP   MEMO,=P'0'                                                       
ZEROX    B     EXPX               CHECK IF ANY AMOUNT TO +/-                    
*                                                                               
*                                                                               
*                                                                               
CD       NTR1                     CD AMOUNT                                     
         TM    PBILCMSW,X'01'     FOR UPFRONT COMMISSION BILLS                  
         BO    *+12                                                             
         CLI   PBILCDSW,C'S'      AND FOR CD TYPE BILL(MAIN, ADJ & CD)          
         BNE   *+14                                                             
         ZAP   DUB2,=P'0'         CD=0                                          
         B     CD10                                                             
         ZAP   DUB2,PBILLGRS      GROSS AMOUNT                                  
         ZAP   DUB,PBILLBIL       GROSS AMOUNT - CD                             
         SP    DUB2,DUB           DUB2 = CD AMOUNT IF ANY                       
*                                                                               
CD10     CLI   AMTSW,C'A'                                                       
         BNE   CD20                                                             
         ZAP   AMOUNT,DUB2+2(6)                                                 
         B     CDX                                                              
CD20     ZAP   MEMO,DUB2+2(6)                                                   
CDX      B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
         EJECT                                                                  
*              VAT - OUTPUT GST IF ACCOUNT IS GST, ELSE OUTPUT PST              
*                                                                               
VAT      NTR1                                                                   
         CLI   EXPEQU,MBTTGST      IF NOT GST ACCOUNT                           
         BE    VAT5                                                             
         BAS   RE,VATPST           SHOW PST AMOUNT FOR PROVINCE                 
         B     VAT15               DUB = PST AMOUNT                             
*                                                                               
VAT5     BAS   RE,VATGST           ELSE, SHOW GST AMOUNT                        
*                                  DUB = GST AMOUNT                             
VAT15    CLI   AMTSW,C'A'                                                       
         BNE   VAT20                                                            
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     VATX                                                             
VAT20    ZAP   MEMO,DUB+2(6)                                                    
VATX     B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
         SPACE 2                                                                
*              SET GST AMOUNT IN DUB                                            
*                                                                               
VATGST   NTR1                                                                   
         CLI   ACPSYS,C'P'                                                      
         BNE   VATGST10                                                         
         BAS   RE,GETPGST         LOOK FOR PRINT GST                            
         ICM   R1,15,FULL                                                       
         CVD   R1,DUB                                                           
         B     VATGSTX                                                          
*                                                                               
VATGST10 SR    R1,R1              SET GST TO ZERO                               
         CLC   BLEN,=H'90'                                                      
         BNH   VATGST12           NO GST                                        
         CLI   BVATCOD,0                                                        
         BE    VATGST12           NO GST                                        
         ICM   R1,15,BVATAMT                                                    
VATGST12 CVD   R1,DUB                                                           
VATGSTX  B     EXPANDX                                                          
         SPACE 2                                                                
*              SET PST AMOUNT IN DUB                                            
*                                                                               
VATPST   NTR1                                                                   
         XR    R1,R1                                                            
         OC    ACPPSTO(ACPMCODE*L'ACPPSTO),ACPPSTO                              
         BZ    VATPST9                                                          
         LA    RE,ACPPSTO          RE=A(OUTPUT PST INFO)                        
         LA    R0,ACPMCODE         R0=(N'ENTRIES)                               
VATPST5  CLC   EXPEQU,2(RE)                                                     
         BE    VATPST8                                                          
         LA    RE,L'ACPPSTO(RE)                                                 
         BCT   R0,VATPST5                                                       
         DC    H'0'                                                             
*                                                                               
VATPST8  ICM   R1,15,8(RE)         GET PST AMOUNT                               
VATPST9  CVD   R1,DUB                                                           
         B     EXPANDX                                                          
         EJECT                                                                  
*                                                                               
*             IVAT - INPUT GST IF ACCOUNT IS GST, ELSE INPUT PST                
*                                                                               
IVAT     NTR1                                                                   
         CLI   EXPEQU,MBTTGSTI     IF NOT GST INPUT ACOCUNT                     
         BE    IVAT5                                                            
         BAS   RE,VATIPST          SHOW PST INPUT AMOUNT FOR PROVINCE           
         B     IVAT15              DUB = PST INPUT AMOUNT                       
*                                                                               
IVAT5    BAS   RE,VATIGST          ELSE, SHOW GST INPUT AMOUNT                  
*                                                                               
IVAT15   CLI   AMTSW,C'A'                                                       
         BNE   IVAT20                                                           
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     IVATX                                                            
IVAT20   ZAP   MEMO,DUB+2(6)                                                    
IVATX    B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
         SPACE 2                                                                
*              SET INPUT PST AMOUNT IN DUB                                      
*                                                                               
VATIPST  NTR1                                                                   
         XR    R1,R1                                                            
         OC    ACPPSTI(ACPMCODE*L'ACPPSTI),ACPPSTI                              
         BZ    VATIPST9                                                         
         LA    RE,ACPPSTI          RE=A(INPUT PST INFO)                         
         LA    R0,ACPMCODE         R0=(N'ENTRIES)                               
VATIPST5 CLC   EXPEQU,2(RE)                                                     
         BE    VATIPST8                                                         
         LA    RE,L'ACPPSTI(RE)                                                 
         BCT   R0,VATIPST5                                                      
         DC    H'0'                                                             
*                                                                               
VATIPST8 ICM   R1,15,8(RE)         GET PST INPUT AMOUNT                         
VATIPST9 CVD   R1,DUB                                                           
         MP    DUB,=P'-1'          ALWAYS SHOW AS POSITIVE AMOUNT               
         B     EXPANDX                                                          
         SPACE 2                                                                
*              SET INPUT GST AMOUNT IN DUB                                      
*                                                                               
VATIGST  NTR1                                                                   
         ICM   R1,15,ACPTGST      TRUE AOR GST AMOUNT                           
         CVD   R1,DUB                                                           
         MP    DUB,=P'-1'                                                       
         B     EXPANDX                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
OBAS     NTR1                                                                   
         XR    RE,RE               RE=BASIS AMOUNT                              
         LA    R0,ACPMCODE         R0=(N'ENTRIES)                               
*                                                                               
         LA    R1,ACPPSTI          R1=A(INPUT PST VALUES)                       
         LA    RF,1                RF=1 - NEGATE AMOUNT                         
         CLI   EXPEQU,MBTTPQI                                                   
         BE    OBAS2                                                            
         CLI   EXPEQU,MBTTNBI                                                   
         BE    OBAS2                                                            
         CLI   EXPEQU,MBTTNSI                                                   
         BE    OBAS2                                                            
         CLI   EXPEQU,MBTTNFI                                                   
         BE    OBAS2                                                            
         CLI   EXPEQU,MBTTBCI                                                   
         BE    OBAS2                                                            
         CLI   EXPEQU,MBTTONI                                                   
         BE    OBAS2                                                            
         CLI   EXPEQU,MBTTPEI                                                   
         BE    OBAS2                                                            
         LA    R1,ACPPSTO          R1=A(OUTPUT PST VALUES)                      
         XR    RF,RF               RF=0 DON'T NEGATE AMOUNT                     
         CLI   EXPEQU,MBTTPQO                                                   
         BE    OBAS2                                                            
         CLI   EXPEQU,MBTTNBO                                                   
         BE    OBAS2                                                            
         CLI   EXPEQU,MBTTNSO                                                   
         BE    OBAS2                                                            
         CLI   EXPEQU,MBTTBCO                                                   
         BE    OBAS2                                                            
         CLI   EXPEQU,MBTTONO                                                   
         BE    OBAS2                                                            
         CLI   EXPEQU,MBTTPEO                                                   
         BE    OBAS2                                                            
         CLI   EXPEQU,MBTTNFO                                                   
         BNE   OBAS3                                                            
OBAS2    CLC   EXPEQU,2(R1)                                                     
         BE    *+16                                                             
         LA    R1,L'ACPPSTO(R1)                                                 
         BCT   R0,OBAS2                                                         
         B     OBAS30                                                           
         ICM   RE,15,4(R1)         BASIS AMOUNT                                 
         CVD   RE,DUB                                                           
         LTR   RF,RF               IF INPUT PST ACCOUNT                         
         BZ    *+10                                                             
         MP    DUB,=P'-1'          ALWAYS SHOW POSITIVE AMOUNT                  
         B     OBAS30                                                           
*                                                                               
OBAS3    CLI   EXPEQU,MBTTGSTI     IF INPUT GST ACCOUNT                         
         BNE   OBAS4                                                            
         ZAP   DUB,ACPTACT                                                      
         MP    DUB,=P'-1'          ALWAYS SHOW POSITIVE AMOUNT                  
         B     OBAS30                                                           
*                                                                               
OBAS4    CLI   ACPSYS,C'P'                                                      
         BNE   OBAS20                                                           
         ZAP   DUB,=P'0'                                                        
         SR    R0,R0                                                            
         LA    R1,PBILLEL          GET GST BASIS FOR PRINT                      
         USING PBILVEL,R1                                                       
OBAS5    CLI   0(R1),0                                                          
         BE    OBAS30                                                           
         CLI   0(R1),X'0A'                                                      
         BE    OBAS10                                                           
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     OBAS5                                                            
*                                                                               
OBAS10   ICM   RE,15,PBILLVBS                                                   
         CVD   RE,DUB                                                           
         B     OBAS30                                                           
         DROP  R1                                                               
*                                                                               
OBAS20   TM    BILSTAT2,BSTVATBQ   GET GST BASIS FOR SPOT/NET                   
         BZ    *+12                                                             
         ICM   RE,15,BVATBAS                                                    
         B     OBAS25                                                           
         ZAP   DUB,BACTP           GET GST BASIS FOR SPOT/NET                   
         CVB   RE,DUB                                                           
         ICM   RF,15,BTAXAMT                                                    
         SR    RE,RF                                                            
OBAS25   CVD   RE,DUB                                                           
*                                                                               
OBAS30   CLI   AMTSW,C'A'                                                       
         BNE   OBAS35                                                           
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     OBASX                                                            
OBAS35   ZAP   MEMO,DUB+2(6)                                                    
OBASX    B     EXPX                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
DIFF     NTR1                     DIFF = RECVBL AMOUNT - CONTRACT RATE          
         ZAP   DUB,=P'0'                                                        
         CLI   ACPSYS,C'P'                                                      
         BNE   DIFF10                                                           
         ZAP   DUB,PBILLRCV       RECEIVABLE AMOUNT                             
         SP    DUB,PBILLCNR       - CONTRACT RATE                               
DIFF10   CLI   AMTSW,C'A'                                                       
         BNE   DIFF20                                                           
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     DIFFX                                                            
DIFF20   ZAP   MEMO,DUB+2(6)                                                    
DIFFX    B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
*                                                                               
*                                                                               
*                                                                               
CON      NTR1                     CONTRACT RATE AMOUNT                          
         ZAP   DUB,=P'0'                                                        
         CLI   ACPSYS,C'P'                                                      
         BNE   CON10                                                            
         ZAP   DUB,PBILLCNR       CONTRACT RATE                                 
CON10    CLI   AMTSW,C'A'                                                       
         BNE   CON20                                                            
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     CONX                                                             
CON20    ZAP   MEMO,DUB+2(6)                                                    
CONX     B     EXPX               CHECKS IF ANY AMOUNT TO +/-                   
         EJECT                                                                  
*                                                                               
* ---- GENERAL EXIT FOR CALCULATING ROUTINES  ----                              
* CHECKS FLAG3 TO SEE WHAT ADDITIONAL AMTS NEED TO BE +/-                       
*                                                                               
EXPX     CLI   FLAG3,0            ANYTHING ADDITIONAL TO DO?                    
         BE    EXPANDX            NOPE                                          
         TM    FLAG3,EXPF3GST     IF NEED TO ADD GST                            
         BNO   *+8                                                              
         BAS   RE,ADDGST          ADD IT                                        
         TM    FLAG3,EXPF3PST     IF NEED TO ADD PST                            
         BNO   *+8                                                              
         BAS   RE,ADDPST          ADD IT                                        
*                                                                               
         TM    FLAG3,EXPF3CDP     IF NEED TO ADD CD                             
         BNO   EXP30                                                            
         CLI   PBILCDSW,C'S'      IF CD ON SEP BILL                             
         BE    EXP40              IGNORE CD + OR -                              
         TM    PBILBASA,X'04'     TEST LESS CD                                  
         BNO   EXP30              NEVER TAKEN OUT - SO DON'T READD              
         BAS   RE,ADDCD                                                         
EXP30    TM    FLAG3,EXPF3CDM     IF NEED TO SUBTRACT CD                        
         BNO   EXP40                                                            
         BAS   RE,SUBCD           SUBTRACT IT                                   
EXP40    TM    FLAG3,EXPF3TRM     IF NEED TO SUBTRACT TRADE                     
         BNO   *+8                                                              
         BAS   RE,SUBTRD                                                        
*                                                                               
         TM    FLAG3,EXPF3POI     TAKE % OF OINC OR XINC                        
         BNO   *+8                                                              
         BAS   RE,INCPCT                                                        
*                                                                               
EXP50    B     EXPANDX                                                          
         EJECT                                                                  
*-------------------------------*                                               
* ADDGST - ADDS GST AMOUNT TO   *                                               
*          CURRENT AMT/MEMO     *                                               
*-------------------------------*                                               
*                                                                               
ADDGST   NTR1                                                                   
         XC    FULL,FULL                                                        
         CLI   ACPSYS,C'P'                                                      
         BNE   ADDG10                                                           
         BAS   RE,GETPGST                                                       
         B     ADDG15                                                           
*                                                                               
ADDG10   CLC   BLEN,=H'90'                                                      
         BNH   ADDG15                                                           
         CLI   BVATCOD,0                                                        
         BE    ADDG15                                                           
         MVC   FULL,BVATAMT      GET SPOT/NET GST AMOUNT                        
*                                                                               
ADDG15   ICM   R1,15,FULL                                                       
         CVD   R1,DUB2                                                          
         CLI   AMTSW,C'A'                                                       
         BNE   ADDG18                                                           
         ZAP   DUB,AMOUNT                                                       
         AP    DUB,DUB2                                                         
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     ADDGX              CHECK FOR OTHER FLAG +/-                      
*                                                                               
ADDG18   ZAP   DUB,MEMO                                                         
         AP    DUB,DUB2                                                         
         ZAP   MEMO,DUB+2(6)                                                    
*                                                                               
ADDGX    B     EXPANDX                                                          
         EJECT                                                                  
*-------------------------------------------*                                   
* ADDPST- ADDS PST TO CURRENT AMOUNT OR MEMO*                                   
*         AMOUNT OR MEMO                    *                                   
*-------------------------------------------*                                   
*                                                                               
ADDPST   NTR1                                                                   
         XC    FULL,FULL           ACCUMULATED PST AMOUNT                       
         CLI   ACPSYS,C'P'         IF PRINT SYSTEM                              
         BNE   *+12                                                             
         BAS   RE,GETPPST          GET PRINT PST                                
         B     ADDPST8                                                          
*                                                                               
         CLC   BLEN,=H'130'        IF NEW BILL LENGTH                           
         BNH   ADDPSTX                                                          
         CLI   BILNPVTS,0          IF ANY PST ON BILL                           
         BE    ADDPSTX                                                          
         ZIC   R0,BILNPVTS         R0=(NUMBER OF ELEMENTS)                      
         LA    R1,BILPVELD         R1=A(FIRST VAT ELEMENT)                      
         USING BILPVELD,R1                                                      
*                                                                               
ADDPST5  ICM   RE,15,FULL                                                       
         ICM   RF,15,BILPVAMT                                                   
         AR    RE,RF                                                            
         STCM  RE,15,FULL                                                       
         LA    R1,BILPVLEN(R1)                                                  
         BCT   R0,ADDPST5                                                       
*                                                                               
ADDPST8  ICM   R1,15,FULL          ACCUMULATED PST IN BILL                      
         CVD   R1,DUB2                                                          
         CLI   AMTSW,C'A'                                                       
         BNE   ADDPST10                                                         
         ZAP   DUB,AMOUNT          ADD PST TO POSTING AMOUNT                    
         AP    DUB,DUB2                                                         
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     ADDPSTX                                                          
*                                                                               
ADDPST10 ZAP   DUB,MEMO            ADD PST TO MEMO AMOUNT                       
         AP    DUB,DUB2                                                         
         ZAP   MEMO,DUB+2(6)                                                    
*                                                                               
ADDPSTX  B     EXPANDX                                                          
         DROP  R1                                                               
         EJECT                                                                  
*-------------------------------------*                                         
* ADDCD - ADDS CD TO CURRENT          *                                         
*         AMOUNT OR MEMO              *                                         
*-------------------------------------*                                         
*                                                                               
ADDCD    NTR1                                                                   
         TM    PBILCMSW,X'01'   FOR UPFRONT COMMISSION BILLS                    
         BO    ADDCDX           CD=0                                            
         ZAP   DUB2,PBILLGRS    GROSS AMOUNT                                    
         ZAP   DUB,PBILLBIL     GROSS AMOUNT - CD                               
         SP    DUB2,DUB         DUB2 = CD AMOUNT IF ANY                         
*                                                                               
         CLI   AMTSW,C'A'                                                       
         BNE   ADDCD10                                                          
         ZAP   DUB,AMOUNT                                                       
         AP    DUB,DUB2           ADD CD TO AMOUNT                              
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     ADDCDX                                                           
*                                                                               
ADDCD10  ZAP   DUB,MEMO                                                         
         AP    DUB,DUB2           ADDS CD TO MEMO                               
         ZAP   MEMO,DUB+2(6)                                                    
*                                                                               
ADDCDX   B     EXPANDX                                                          
         EJECT                                                                  
*-------------------------------------*                                         
* SUBCD - SUBTRACTS CD FROM CURRENT   *                                         
*         AMOUNT OR MEMO              *                                         
*-------------------------------------*                                         
*                                                                               
SUBCD    NTR1                                                                   
         TM    PBILCMSW,X'01'   FOR UPFRONT COMMISSION BILLS                    
         BO    SUBCDX           CD=0                                            
         ZAP   DUB2,PBILLGRS    GROSS AMOUNT                                    
         ZAP   DUB,PBILLBIL     GROSS AMOUNT - CD                               
         SP    DUB2,DUB         DUB2 = CD AMOUNT IF ANY                         
         CLI   AMTSW,C'A'                                                       
         BNE   SUBCD10                                                          
         ZAP   DUB,AMOUNT                                                       
         SP    DUB,DUB2           SUBTRACT CD FROM AMOUNT                       
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     SUBCDX                                                           
SUBCD10  ZAP   DUB,MEMO                                                         
         SP    DUB,DUB2           SUBTRACT CD FROM MEMO                         
         ZAP   MEMO,DUB+2(6)                                                    
SUBCDX   B     EXPANDX                                                          
         SPACE                                                                  
*-----------------------------------------*                                     
* SUBTRD - SUBTRACTS TRADE PORTION FROM   *                                     
*         AMOUNT OR MEMO                  *                                     
*-----------------------------------------*                                     
*                                                                               
SUBTRD   NTR1                                                                   
         ICM   RF,15,ACPTPCT       TRADE % WITH 3 DECIMAL PLACES                
         LTR   RF,RF                                                            
         BZ    SUBTRDX                                                          
         CVD   RF,DUB                                                           
         ZAP   PNUM(16),DUB                                                     
*                                                                               
         ZAP   DUB,=P'0'                                                        
         CLI   ACPTBAS,C'G'         USE GROSS AS THE BASIS?                     
         BNE   *+14                                                             
         ZAP   DUB,BGRSP                                                        
         B     SUBTR10                                                          
         CLI   ACPTBAS,C'N'         USE NET AS THE BASIS?                       
         BNE   *+10                                                             
         SR    RF,RF                                                            
         ICM   RF,15,BCLDNET                                                    
         CVD   RF,DUB                                                           
*                                                                               
SUBTR10  MP    PNUM(16),DUB                                                     
         SRP   PNUM,64-5,5         DIVIDE AND ROUND                             
         ZAP   DUB,PNUM+8(8)                                                    
         CLI   AMTSW,C'A'                                                       
         BNE   SUBTR20                                                          
         ZAP   DUB2,AMOUNT                                                      
         SP    DUB2,DUB                                                         
         ZAP   AMOUNT,DUB2+2(6)                                                 
         B     SUBTRDX            CHECK FOR OTHER FLAG +/-                      
*                                                                               
SUBTR20  ZAP   DUB2,MEMO                                                        
         SP    DUB2,DUB                                                         
         ZAP   MEMO,DUB2+2(6)                                                   
*                                                                               
SUBTRDX  B     EXPANDX                                                          
*-------------------------------------------*                                   
* CALCD - CALCULATE CD AMOUNT AND SET CDAMT*                                    
*-------------------------------------------*                                   
*                                                                               
CALCD    NTR1                                                                   
         ZAP   CDAMT,=P'0'       CD=0                                           
         TM    PBILCMSW,X'01'    FOR UPFRONT COMMISSION BILLS                   
         BO    EXPANDX                                                          
         ZAP   PAMT1,PBILLGRS    GROSS AMOUNT                                   
         ZAP   PAMT2,PBILLBIL    GROSS AMOUNT - CD                              
         SP    PAMT1,PAMT2       PAMT1=CD AMOUNT IF ANY                         
         ZAP   CDAMT,PAMT1                                                      
         B     EXPANDX                                                          
         SPACE                                                                  
*-----------------------------------------------*                               
* CALDIFF - CALCULATE DIFF AMOUNT & SET DIFFAMT *                               
*-----------------------------------------------*                               
*                                                                               
CALDIFF  NTR1                                                                   
         ZAP   PAMT1,PBILLRCV     RECEIVABLE AMOUNT                             
         ZAP   PAMT2,PBILLCNR     - CONTRACT RATE                               
         SP    PAMT1,PAMT2                                                      
         ZAP   DIFFAMT,PAMT1                                                    
         B     EXPANDX                                                          
         EJECT                                                                  
*--------------------------------------*                                        
* GETPGST - GETS GST AMOUNT INTO FULL  *                                        
*           SETS CC <> IF NO GST       *                                        
*--------------------------------------*                                        
*                                                                               
GETPGST  NTR1                                                                   
         XC    FULL,FULL                                                        
         SR    R0,R0                                                            
         LA    R1,PBILLEL                                                       
         USING PBILVEL,R1                                                       
GETPGST5 CLI   0(R1),0                                                          
         BE    EXPNO              NO GST                                        
         CLI   0(R1),X'0A'        GST ELEMENT                                   
         BE    GETPGST8                                                         
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     GETPGST5                                                         
*                                                                               
GETPGST8 MVC   FULL,PBILLVAT      GST AMOUNT                                    
         B     EXPYES                                                           
         DROP  R1                                                               
         EJECT                                                                  
*--------------------------------------*                                        
* GETPPST - SETS IN FULL TOTAL PST     *                                        
*--------------------------------------*                                        
*                                                                               
GETPPST  NTR1                                                                   
         SR    R0,R0                                                            
         LA    R1,PBILLEL          R1=A(FIRST ELEMENT)                          
         SR    RE,RE               RE=(ACCUMULATED PST AMOUNT)                  
         USING PBLPSTEL,R1                                                      
GETPPST5 CLI   0(R1),0             TEST END OF RECORD                           
         BE    GETPPSTX                                                         
         CLI   0(R1),X'84'         TEST PST ELEMENT                             
         BNE   GETPPST8                                                         
         ICM   RF,15,PBLPVAMT                                                   
         AR    RE,RF               ADD TO TOTAL                                 
*                                                                               
GETPPST8 ICM   R0,1,1(R1)          BUMP TO NEXT ELEMENT IN RECORD               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     GETPPST5                                                         
*                                                                               
GETPPSTX STCM  RE,15,FULL                                                       
         B     EXPANDX                                                          
         DROP  R1                                                               
         EJECT                                                                  
*--------------------------------------*                                        
* GETPTAX - GETS TAX AMOUNT INTO FULL  *                                        
*--------------------------------------*                                        
*                                                                               
GETPTAX  NTR1                                                                   
         XC    FULL,FULL          SET SALES TAX = 0                             
         TM    PBILCMSW,X'01'     FOR UPFRONT COMMISSION BILLS                  
         BO    GETPTAXX                                                         
         SR    R0,R0                                                            
         LA    R1,PBILLEL                                                       
         USING PBILVEL,R1                                                       
GETPTAX5 CLI   0(R1),0                                                          
         BE    GETPTAXX                                                         
         CLI   0(R1),X'09'                                                      
         BE    GETPTAX8                                                         
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     GETPTAX5                                                         
*                                                                               
GETPTAX8 MVC   FULL,PBILLTAX      TAX AMOUNT                                    
GETPTAXX B     EXPANDX                                                          
         DROP  R1                                                               
         EJECT                                                                  
*--------------------------------------*                                        
* INCPCT - CALC % OF OINC             *                                         
*--------------------------------------*                                        
*                                                                               
INCPCT   NTR1                                                                   
*                                                                               
         ZAP   DUB,=P'0'                                                        
         ICM   RF,15,ACPTMPCT      MIDAS SPLIT % WITH 3 DECIMAL PLACES          
         LTR   RF,RF                                                            
         BZ    INCPCTX             NO %, DONE                                   
         CVD   RF,DUB                                                           
         ZAP   PNUM(16),DUB                                                     
*                                                                               
         ZAP   DUB,AMOUNT                                                       
         CLI   AMTSW,C'A'                                                       
         BE    *+10                                                             
         ZAP   DUB,MEMO                                                         
*                                                                               
         ZAP   DUB2,DUB            SAVE FOR LATER                               
         MP    PNUM(16),DUB                                                     
         SRP   PNUM,64-5,5         DIVIDE AND ROUND                             
         ZAP   DUB,PNUM+8(8)                                                    
         CLI   EXP,C'%'            IF IT'S %OINC                                
         BE    *+16                                                             
         SP    DUB2,DUB            ELSE, OINC%                                  
         ZAP   DUB,DUB2                                                         
*                                                                               
         CLI   AMTSW,C'A'                                                       
         BNE   INCPCT20                                                         
         ZAP   AMOUNT,DUB+2(6)                                                  
         B     INCPCTX                                                          
INCPCT20 ZAP   MEMO,DUB+2(6)                                                    
*                                                                               
INCPCTX  B     EXPANDX                                                          
         EJECT                                                                  
*====================*                                                          
* LITERAL POOL       *                                                          
*====================*                                                          
*                                                                               
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB,R9,R7                                                         
         SPACE 2                                                                
       ++INCLUDE ACTRAPRV                                                       
         EJECT                                                                  
*---------------------------------------------------------------------*         
* EXPTBL - TABLE OF EXPRESSIONS                                       *         
*          EXP (8), ROUTINE (4), FLAG (1), FLAG,(1), FLAG(1)          *         
*          2) ADJ FLAG M(MINUS),Z(RESULT ZERO), 0(ZER OUT) N(NOTHING) *         
*---------------------------------------------------------------------*         
*                                                                               
EXPTBL   DC    CL8'B',AL4(BILL),C'N',C'N',X'00'                                 
         DC    CL8'B+T',AL4(BILL),C'N',C'N',X'90'                               
         DC    CL8'B+C',AL4(BILL),C'N',C'N',X'40'                               
         DC    CL8'B+C+T',AL4(BILL),C'N',C'N',X'D0'                             
         DC    CL8'0B',AL4(BILL),C'Z',C'N',X'00'                                
         DC    CL8'0(B+T)',AL4(BILL),C'Z',C'N',X'90'                            
         DC    CL8'0(B+C)',AL4(BILL),C'Z',C'N',X'40'                            
         DC    CL8'0(B+C+T)',AL4(BILL),C'Z',C'N',X'D0'                          
         DC    CL8'-B',AL4(BILL),C'M',C'N',X'00'                                
         DC    CL8'-(B+T)',AL4(BILL),C'M',C'N',X'90'                            
         DC    CL8'-(B+C)',AL4(BILL),C'M',C'N',X'40'                            
         DC    CL8'-(B+C+T)',AL4(BILL),C'M',C'N',X'D0'                          
         DC    CL8'B,Z',AL4(BILL),C'0',C'N',X'00'                               
         DC    CL8'B+T,Z',AL4(BILL),C'0',C'N',X'90'                             
         DC    CL8'B+C,Z',AL4(BILL),C'0',C'N',X'40'                             
         DC    CL8'B+C+T,Z',AL4(BILL),C'0',C'N',X'D0'                           
         DC    CL8'B,Z,Z',AL4(BILL),C'0',C'0',X'00'                             
         DC    CL8'B+T,Z,Z',AL4(BILL),C'0',C'0',X'90'                           
         DC    CL8'B+C,Z,Z',AL4(BILL),C'0',C'0',X'40'                           
*                                                                               
         DC    CL8'Z',AL4(ZERO),C'N',C'N',X'00'                                 
         DC    CL8'Z,Z',AL4(ZERO),C'0',C'N',X'00'                               
         DC    CL8'T',AL4(VAT),C'N',C'N',X'00'                                  
         DC    CL8'IT',AL4(IVAT),C'N',C'N',X'00'                                
         DC    CL8'C',AL4(CD),C'N',C'N',X'00'                                   
         DC    CL8'NET',AL4(NET),C'N',C'N',X'00'                                
         DC    CL8'AOR',AL4(AOR),C'N',C'N',X'00'                                
         DC    CL8'-AOR',AL4(AOR),C'M',C'N',X'00'                               
         DC    CL8'AOR+IT',AL4(AORIVAT),C'N',C'N',X'00'                         
         DC    CL8'INC',AL4(INC),C'N',C'N',X'00'                                
         DC    CL8'0INC',AL4(INC),C'Z',C'N',X'00'                               
         DC    CL8'-INC',AL4(INC),C'M',C'N',X'00'                               
         DC    CL8'AFEE',AL4(AFEE),C'N',C'N',X'00'                              
         DC    CL8'AFEE,Z',AL4(AFEE),C'0',C'N',X'00'                            
         DC    CL8'INC,-AOR',AL4(INC),C'A',C'N',X'00'                           
         DC    CL8'INC,-IOR',AL4(INC),C'I',C'N',X'00'                           
         DC    CL8'IN,-I,-A',AL4(INC),C'I',C'A',X'00'                           
         DC    CL8'IOR',AL4(IOR),C'N',C'N',X'00'                                
         DC    CL8'OBASIS',AL4(OBAS),C'N',C'N',X'00'                            
         DC    CL8'IBASIS',AL4(OBAS),C'N',C'N',X'00'                            
         DC    CL8'TR',AL4(TRA),C'N',C'N',X'00'                                 
         DC    CL8'XINC',AL4(XINC),C'N',C'N',X'00'                              
         DC    CL8'XINC,-TR',AL4(XINC),C'N',C'N',X'08'                          
         DC    CL8'%XINC-TR',AL4(XINC),C'N',C'N',X'0C'                          
         DC    CL8'XINC-TR%',AL4(XINC),C'N',C'N',X'0C'                          
         DC    CL8'OINC',AL4(OINC),C'N',C'N',X'00'                              
         DC    CL8'%OINC',AL4(OINC),C'N',C'N',X'04'                             
         DC    CL8'OINC%',AL4(OINC),C'N',C'N',X'04'                             
         DC    CL8'AINC',AL4(AINC),C'N',C'N',X'00'                              
         DC    CL8'NB',AL4(NETB),C'N',C'N',X'00'                                
         DC    CL8'TC',AL4(TRDCR),C'N',C'N',X'00'                               
*                                                                               
         DC    CL8'DIFF',AL4(DIFF),C'N',C'N',X'00'                              
         DC    CL8'CON',AL4(CON),C'N',C'N',X'00'                                
         DC    CL8'CON+C',AL4(CON),C'N',C'N',X'40'                              
         DC    CL8'CON+T',AL4(CON),C'N',C'N',X'90'                              
         DC    CL8'CON+C+T',AL4(CON),C'N',C'N',X'D0'                            
*                                                                               
         DC    CL8'G',AL4(GRS),C'N',C'N',X'00'                                  
         DC    CL8'G+T',AL4(GRS),C'N',C'N',X'90'                                
         DC    CL8'G-C',AL4(GRS),C'N',C'N',X'20'                                
         DC    CL8'G-C+T',AL4(GRS),C'N',C'N',X'B0'                              
         DC    CL8'G-X',AL4(GX),C'N',C'N',X'00'                                 
         DC    CL8'0G',AL4(GRS),C'Z',C'N',X'00'                                 
         DC    CL8'0(G+T)',AL4(GRS),C'Z',C'N',X'90'                             
         DC    CL8'0(G-C)',AL4(GRS),C'Z',C'N',X'20'                             
         DC    CL8'0(G-C+T)',AL4(GRS),C'Z',C'N',X'B0'                           
         DC    CL8'-G',AL4(GRS),C'M',C'N',X'00'                                 
         DC    CL8'-(G+T)',AL4(GRS),C'M',C'N',X'90'                             
         DC    CL8'-(G-C)',AL4(GRS),C'M',C'N',X'20'                             
         DC    CL8'-(G-C+T)',AL4(GRS),C'M',C'N',X'B0'                           
         DC    CL8'G,Z',AL4(GRS),C'0',C'N',X'00'                                
         DC    CL8'G+T,Z',AL4(GRS),C'0',C'N',X'90'                              
         DC    CL8'G-C,Z',AL4(GRS),C'0',C'N',X'20'                              
         DC    CL8'G-C+T,Z',AL4(GRS),C'0',C'N',X'B0'                            
         DC    X'FF'                                                            
         EJECT                                                                  
*-------------------------------------------------------------*                 
*  CHKACCS - READS THROUGH ACCOUNTS IN ACPPOST -              *                 
*       XIT- IF NOT VALID SETS ACPERR2  &                     *                 
*            IF LRTN =Y SETS ACPACCN & TRYS TO BE I/O CONCIOUS*                 
*-------------------------------------------------------------*                 
*                                                                               
         DS    0D                                                               
CHKACC   NMOD1 0,**CHACC*                                                       
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
*                                                                               
         L     R4,APOSTS          R4=A(EXPANDED POSTINGS)                       
         USING ACPRTND,R4                                                       
         ZIC   R3,NAPOSTS         R3=# OF POSTINGS                              
         LTR   R3,R3                                                            
         BZ    CHKACCX                                                          
*                                                                               
CHKACC1  MVC   ACCNAME,SPACES                                                   
         CLI   ACPERR2,0          ERROR WITH ACCOUNT                            
         BE    CHKACC3                                                          
         MVI   ACCERROR,C'Y'                                                    
         B     CHKACC20                                                         
*                                                                               
CHKACC3  CLI   LRTN,C'Y'          PASS BACK ACCOUT NAME                         
         BNE   CHKACC15                                                         
         OC    0(ACPRTNL2,R4),0(R4)  IF NO ACCOUNT LINE                         
         BZ    CHKACC20           SKIP                                          
         ICM   R2,15,ACPSVACC     AREA FOR ACCOUNTS                             
         USING ACCD,R2                                                          
         SR    RE,RE                                                            
         ICM   RF,15,ACPNUM3      LENGTH OF AREA                                
         LA    R1,ACCL            LENGTH OF EACH ROW                            
         DR    RE,R1                                                            
*                                                                               
         LR    R1,RF              # OF ROWS                                     
         MVI   WORK,C'N'          NOT FULL                                      
*                                                                               
CHKACC4  OC    ACCACT,ACCACT      ANY ACCOUNT THERE?                            
         BZ    CHKACC5            NO - GO READ NEW ACCOUNT                      
         CLC   ACCACT,ACPACC      MATCH ON ACCOUNT                              
         BE    CHKACC8                                                          
         LA    R2,ACCL(R2)        NEXT ACCOUNT IN SAVED AREA                    
         BCT   R1,CHKACC4                                                       
*                                 COULDN'T FIND ACC & NO MORE ROOM              
         MVI   WORK,C'F'          SAVE AREA FULL                                
         B     CHKACC5            SO JUST READ FOR ACCOUNT                      
*                                                                               
CHKACC8  MVC   ACPACCN,ACCACTN    MOVE IN NAME                                  
         B     CHKACC20                                                         
*                                                                               
CHKACC5  GOTO1 =A(GETNAME),DMCB,(RC),RR=RELO                                    
         BNE   CHKACCN            INDICATE ERROR WITH ACCOUNT                   
         MVC   ACPACCN,ACCNAME    MOVE IN NAME                                  
*                                                                               
         CLI   WORK,C'F'          IS SAVED AREA FULL                            
         BE    CHKACC20                                                         
         MVC   ACCACT,ACPACC      NO - SET IN SAVED AREA                        
         MVC   ACCACTN,ACCNAME                                                  
         B     CHKACC20                                                         
*                                                                               
CHKACC15 OC    0(ACPRTNL,R4),0(R4)                                              
         BZ    CHKACC20                                                         
         GOTO1 =A(GETNAME),DMCB,(RC),RR=RELO                                    
         BE    CHKACC20                                                         
*                                                                               
CHKACCN  MVC   ACPERR2,BYTE         INDICATE ERROR                              
         MVI   ACCERROR,C'Y'                                                    
*                                                                               
CHKACC20 CLI   LRTN,C'Y'                                                        
         BNE   CHKACC25                                                         
         LA    R4,ACPRTNL2(R4)                                                  
         B     CHKACC30                                                         
CHKACC25 LA    R4,ACPRTNL(R4)                                                   
*                                                                               
CHKACC30 BCT   R3,CHKACC1                                                       
CHKACCX  XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*====================*                                                          
* LITERAL POOL       *                                                          
*====================*                                                          
*                                                                               
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*---------------------------------------------------------------*               
* GETNAME - CHECKS ACPACC TO SEE IF IT IS VALID FOR POSTING     *               
*     XIT - IF ACCOUNT NOT VALID -SETS ACPERR2                  *               
*           IF ACCOUNT VALIDS -- SETS ACCNAME                   *               
*---------------------------------------------------------------*               
*                                                                               
         DS    0D                                                               
GETNAME  NMOD1 0,**GTNA**                                                       
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
*                                                                               
         MVI   BYTE,0                                                           
         OC    ACPACC,ACPACC      IF NO ACCOUNT                                 
         BNZ   *+12                                                             
         OI    BYTE,ACPEINV       ACCOUNT INVALID                               
         B     GETNMNO                                                          
*                                                                               
         CLI   ACPERR2,0          ERROR FROM PREVIOUS VALIDATION ?              
         BE    GETNM1                                                           
         MVC   BYTE,ACPERR2                                                     
         B     GETNMNO                                                          
*                                                                               
GETNM1   LA    R2,KEY                                                           
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,ACPCMPC2        COMPANY CODE- FROM OTHER ACCFILE         
         MVC   ACTKUNT(L'ACPACC),ACPACC   UNIT/LEDGER/ACC                       
         OC    ACTKUNT(L'ACPACC),SPACES                                         
         DROP  R2                                                               
         MVI   RDSE,2             READ FROM OTHER SE IF APPLICABLE              
         GOTO1 =A(READ),DMCB,(RC),RR=RELO                                       
         BE    *+12                                                             
         OI    BYTE,ACPEINV       ACCOUNT INVALID                               
         B     GETNMNO                                                          
*                                                                               
         LA    R2,IOAREA                                                        
         LA    R1,ACCORFST                                                      
         AR    R2,R1                                                            
         USING ABLELD,R2          ACCOUNT BALANCE ELEMENT                       
GETNM5   CLI   ABLEL,0            END OF RECORD?                                
         BNE   GETNM8                                                           
         OI    BYTE,ACPEINP       ACCOUNT NOT VALID FOR POSTING                 
         B     GETNMNO                                                          
*                                                                               
GETNM8   CLI   ABLEL,ABLELQ       BALANCE ELEMENT?                              
         BE    GETNM10                                                          
         SR    R0,R0                                                            
         ICM   R0,1,ABLLN                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     GETNM5                                                           
         DROP  R2                                                               
GETNM10  LA    R2,IOAREA                                                        
         LA    R1,ACCORFST                                                      
         AR    R2,R1                                                            
         USING NAMELD,R2                                                        
GETNM15  CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),NAMELQ       X'20'                                         
         BE    GETNM20                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     GETNM15                                                          
*                                                                               
GETNM20  SR    R1,R1              SET CONTRA ACCOUNT NAME                       
         IC    R1,NAMLN           LENGTH OF NAME ELEMENT                        
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACCNAME(0),NAMEREC SET CONTRA ACCOUNT NAME                       
         CR    RB,RB                                                            
         B     GETNMX                                                           
*                                                                               
GETNMNO  LTR   RB,RB                                                            
GETNMX   XIT1                                                                   
         DROP  R4,R2                                                            
         EJECT                                                                  
*====================*                                                          
* LITERAL POOL       *                                                          
*====================*                                                          
*                                                                               
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*==============================================================*                
* RESORT - GIVEN POSTING TABLE - RESORT POSTINGS SO THAT       *                
*      1) 1C BEFORE 11 & 1C BEFORE 12 ,ETC..(ALWAYS SHOW CR 1ST*                
*      2) AND AT SAME TIME FORCE ALL COSTING POSTINGS TO BOTTOM*                
*      3) FOR AOR SITUATIONS ALL BILL1 APPEAR FIRST BEFORE BILL2                
*==============================================================*                
*                                                                               
         DS    0D                                                               
RESORT   NMOD1 0,**SORT**                                                       
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
*                                                                               
         L     R4,APOSTS           R4=A(EXPANDED POSTINGS)                      
         USING ACPRTND,R4                                                       
         ZIC   R3,NAPOSTS          R3=NUMBER OF POSTINGS                        
         LTR   R3,R3                                                            
         BZ    RESORTX                                                          
RESORT2  CLI   LRTN,C'Y'                                                        
         BNE   *+14                                                             
         OC    0(ACPRTNL2,R4),0(R4)                                             
         B     *+10                                                             
         OC    0(ACPRTNL,R4),0(R4)                                              
         BZ    RESORT80                                                         
*                                                                               
         XC    SVCRROW,SVCRROW    CLEAR OUT COSTING ROW TO SWITCH               
         XC    SVDRROW,SVDRROW    CLEAR OUT PARTNER ROW TO SWITCH               
         MVI   BYTE,2                                                           
         CLI   0(R4),MBTTBIL      IF BILLING ROW                                
         BE    RESORT15                                                         
         MVI   BYTE,3                                                           
         CLI   0(R4),MBTTREV      OR REVENUE ROW                                
         BE    RESORT15                                                         
         MVI   BYTE,4                                                           
         CLI   0(R4),MBTTABL      OR AOR BILLING ROW                            
         BE    RESORT15                                                         
         MVI   BYTE,5                                                           
         CLI   0(R4),MBTTARR      OR AOR REVENUE ROW                            
         BE    RESORT15                                                         
         MVI   BYTE,6                                                           
         CLI   0(R4),MBTTSBL      OR SELLOFF BILLING ROW                        
         BE    RESORT15                                                         
         MVI   BYTE,7                                                           
         CLI   0(R4),MBTTSRV      OR SELLOFF REVENUE ROW                        
         BE    RESORT15                                                         
         MVI   BYTE,8                                                           
         CLI   0(R4),MBTTINTI     OR INTERNAL INCOME ROW                        
         BE    RESORT15                                                         
         MVI   BYTE,9                                                           
         CLI   0(R4),MBTTINTB     OR INTERNAL BILLING ROW                       
         BE    RESORT15                                                         
         MVI   BYTE,10                                                          
         CLI   0(R4),MBTTINTR     OR INTERNAL REVENUE ROW                       
         BE    RESORT15                                                         
         MVI   BYTE,11                                                          
         CLI   0(R4),MBTTRBI      OR TRADE BILLING                              
         BE    RESORT15                                                         
         MVI   BYTE,12                                                          
         CLI   0(R4),MBTTRRV      OR TRADE REVENUE                              
         BE    RESORT15                                                         
         MVI   BYTE,13                                                          
         CLI   0(R4),MBTTOBIL     OR OTHER TRADE BILLING (MIDAS)                
         BE    RESORT15                                                         
         MVI   BYTE,14                                                          
         CLI   0(R4),MBTTOREV     OR OTHER TRADE REVENUE (MIDAS)                
         BE    RESORT15                                                         
         MVI   BYTE,15                                                          
         CLI   0(R4),MBTTIRCV     INTERCOMPANY RECIEVABLE                       
         BE    RESORT15                                                         
         MVI   BYTE,16                                                          
         CLI   0(R4),MBTTIPBL     INTERCOMPANY PAYABLE                          
         BE    RESORT15                                                         
         MVI   BYTE,17                                                          
         CLI   0(R4),MBTTOPIN     OPCO INCOME                                   
         BE    RESORT15                                                         
         MVI   BYTE,18                                                          
         CLI   0(R4),MBTTOPBL     OPCO BILLINGS                                 
         BE    RESORT15                                                         
         MVI   BYTE,19                                                          
         CLI   0(R4),MBTTOPRV     OPCO REVENUE                                  
         BNE   RESORT80                                                         
RESORT15 LA    RE,ACPLAST-ACPBTYP                                               
         AR    RE,R4                                                            
         MVC   0(1,RE),BYTE       FORCE COSTING POSTINGS TO BOTTOM              
         LR    R1,R4              SAVE POSITION                                 
         CLI   LRTN,C'Y'                                                        
         BNE   *+12                                                             
         LA    R1,ACPRTNL2(R1)                                                  
         B     *+8                                                              
         LA    R1,ACPRTNL(R1)     NEXT POSTING                                  
*                                                                               
         CLI   0(R1),MBTTCOS      MAKE SURE NEXT POSTING IS COSTING             
         BE    *+12                                                             
         CLI   0(R1),MBTTINTC                                                   
         BNE   RESORT80                                                         
         LA    RE,ACPLAST-ACPBTYP                                               
         AR    RE,R1                                                            
         MVC   0(1,RE),BYTE       FORCE COSTING POSTINGS TO BOTTOM              
         LA    RE,ACPWHC-ACPBTYP                                                
         AR    RE,R1                                                            
         CLC   0(1,R4),0(RE)      MAKE SURE IT'S THE CORRECT COSTING            
         BNE   RESORT80           NEXT ROW- IF NOT SKIP (PROBLEM)               
         BAS   RE,SAVECR          SAVE COSTING ROW                              
         BAS   RE,SAVEDR          SAVE ITS PARTNER                              
         BAS   RE,SWITCH          SWITCH THE TWO IN THE TABLE                   
*                                                                               
RESORT80 CLI   LRTN,C'Y'                                                        
         BNE   RESORT85                                                         
         LA    R4,ACPRTNL2(R4)                                                  
         B     *+8                                                              
RESORT85 LA    R4,ACPRTNL(R4)     NEXT POSTING                                  
         BCT   R3,RESORT2                                                       
*                                                                               
         ZIC   R3,NAPOSTS         R3=NUMBER OF POSTINGS                         
         LA    R2,ACPRTNL2        R2=LENGTH OF EACH POSTING ENTRY               
         CLI   LRTN,C'Y'                                                        
         BE    *+8                                                              
         LA    R2,ACPRTNL                                                       
         LA    R4,ACPBIL2-ACPBTYP                                               
         GOTO1 ACPXSORT,DMCB,APOSTS,(R3),(R2),2,(R4)                            
RESORTX  XIT1                                                                   
         EJECT                                                                  
SAVECR   LA    R2,ACPRTNL         R1 PSTS TO COSTING (CR) ROW                   
         CLI   LRTN,C'Y'                                                        
         BNE   *+8                                                              
         LA    R2,ACPRTNL2                                                      
         BCTR  R2,0                                                             
         EX    R2,SVCR                                                          
         B     *+10                                                             
SVCR     MVC   SVCRROW(0),0(R1)   SAVE CR ROW                                   
         BR    RE                                                               
         SPACE                                                                  
SAVEDR   LA    R2,ACPRTNL         R4 PTS TO PARTNER ROW (DR)                    
         CLI   LRTN,C'Y'                                                        
         BNE   *+8                                                              
         LA    R2,ACPRTNL2                                                      
         BCTR  R2,0                                                             
         EX    R2,SVDR                                                          
         B     *+10                                                             
SVDR     MVC   SVDRROW(0),0(R4)   SAVE DR ROW                                   
         BR    RE                                                               
         SPACE                                                                  
SWITCH   LA    R2,ACPRTNL     R4=ROW TO MOVE CR TOO, R1=ROW TO MOVE DR          
         CLI   LRTN,C'Y'                                                        
         BNE   *+8                                                              
         LA    R2,ACPRTNL2                                                      
         BCTR  R2,0                                                             
         EX    R2,CLRROW1                                                       
         EX    R2,CLRROW2                                                       
         EX    R2,MOVECR                                                        
         EX    R2,MOVEDR                                                        
         BR    RE                                                               
         SPACE                                                                  
CLRROW1  XC    0(0,R1),0(R1)                                                    
CLRROW2  XC    0(0,R4),0(R4)                                                    
MOVECR   MVC   0(0,R4),SVCRROW                                                  
MOVEDR   XC    0(0,R1),SVDRROW                                                  
         EJECT                                                                  
*====================*                                                          
* LITERAL POOL       *                                                          
*====================*                                                          
*                                                                               
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*====================================================================*          
* FIXPST - FOR AOR BILLS - FIX POSTINGS SO SENDING BACK ONLY MAIN OR *          
*          ONLY TRUE - ALSO FIX GENERAL ERROR MSG - FROM POSTINGS    *          
*====================================================================*          
*                                                                               
         DS    0D                                                               
FIXPST   NMOD1 0,**FPST**                                                       
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
*                                                                               
         NI    ACPERR,255-X'10'   TURN OFF ACCOUNT ERROR                        
         L     R4,APOSTS           R4=A(EXPANDED POSTINGS)                      
         USING ACPRTND,R4                                                       
         ZIC   R3,NAPOSTS         R3=NUMBER OF POSTINGS                         
         LTR   R3,R3                                                            
         BZ    FIXPSTX                                                          
FIXPST5  CLI   LRTN,C'Y'                                                        
         BNE   FIXPST10                                                         
         OC    0(ACPRTNL2,R4),0(R4)                                             
         B     FIXPST15                                                         
FIXPST10 OC    0(ACPRTNL,R4),0(R4)                                              
FIXPST15 BZ    FIXPST50                                                         
         CLI   ACPAFLG,C'M'                                                     
         BE    FIXPST30                                                         
         CLI   ACPBIL2,2          LOOKING FOR TRUE POSTINGS ONLY                
         BE    FIXPST50                                                         
         BAS   RE,CLRPST          CLEAR THIS POSTING ROW                        
         B     FIXPST50                                                         
*                                                                               
FIXPST30 CLI   ACPBIL2,2          LOOKING FOR MAIN BILL POSTINGS ONLY           
         BNE   FIXPST50                                                         
         BAS   RE,CLRPST                                                        
*                                                                               
FIXPST50 CLI   ACPERR2,0                                                        
         BE    *+8                                                              
         OI    ACPERR,ACPAERR     ERROR WITH ACCOUNT- TURN ON                   
         CLI   LRTN,C'Y'                                                        
         BNE   FIXPST55                                                         
         LA    R4,ACPRTNL2(R4)                                                  
         B     *+8                                                              
FIXPST55 LA    R4,ACPRTNL(R4)     NEXT POSTING                                  
         BCT   R3,FIXPST5                                                       
FIXPSTX  XIT1                                                                   
         EJECT                                                                  
CLRPST   NTR1                                                                   
         LA    R2,ACPRTNL         CLEAR THIS POSTING ROW                        
         CLI   LRTN,C'Y'                                                        
         BNE   *+8                                                              
         LA    R2,ACPRTNL2                                                      
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)                                                    
         XIT1                                                                   
         DROP  R4                                                               
*====================*                                                          
* LITERAL POOL       *                                                          
*====================*                                                          
*                                                                               
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*============================================*                                  
* DBCR - GO THROUGH POSTINGS TO CHECK THAT   *                                  
*        CREDITS EQUAL DEBITS AND FOR EACH   *                                  
*        ACCOUNT THERE IS A ACPAMT2          *                                  
*============================================*                                  
*                                                                               
         DS    0D                                                               
DBCR     NMOD1 0,**DBCR**                                                       
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
*                                                                               
         ZAP   CREDS,=P'0'                                                      
         ZAP   DEBS,=P'0'                                                       
         MVI   BYTE,C'N'          NO POSTINGS                                   
         MVI   INTERNAL,C'N'      NO INTERNAL POSTINGS                          
         ZIC   R3,NAPOSTS          NUMBER OF POSTINGS                           
         LTR   R3,R3                                                            
         BZ    DBCR30                                                           
         L     R4,APOSTS           R4=A(EXPANDED POSTINGS)                      
         USING ACPRTND,R4                                                       
*                                                                               
DBCR5    CLI   LRTN,C'Y'                                                        
         BNE   DBCR6                                                            
         OC    0(ACPRTNL2,R4),0(R4)                                             
         B     DBCR8                                                            
DBCR6    OC    0(ACPRTNL,R4),0(R4)                                              
DBCR8    BZ    DBCR20                                                           
         MVI   BYTE,C'Y'          POSTING FOUND                                 
         CLI   0(R4),MBTTINTI                                                   
         BL    DBCR10                                                           
         CLI   0(R4),MBTTINTR                                                   
         BH    DBCR10                                                           
         MVI   INTERNAL,C'Y'                                                    
*                                                                               
DBCR10   OC    ACPAMT2,ACPAMT2                                                  
         BNZ   *+12                                                             
         OI    ACPERR,ACPMISSA    ERROR MISSING AN AMOUNT                       
         B     DBCR20                                                           
         TM    ACPSTAT,ACPCR                                                    
         BNO   DBCR15                                                           
         AP    CREDS,ACPAMT2                                                    
         B     DBCR20                                                           
*                                                                               
DBCR15   AP    DEBS,ACPAMT2                                                     
*                                                                               
DBCR20   CLI   LRTN,C'Y'                                                        
         BNE   DBCR24                                                           
         LA    R4,ACPRTNL2(R4)                                                  
         B     DBCR28                                                           
DBCR24   LA    R4,ACPRTNL(R4)     NEXT POSTING                                  
DBCR28   BCT   R3,DBCR5                                                         
         CP    CREDS,DEBS         DEBITS = CREDITS?                             
         BE    DBCR30                                                           
         OI    ACPERR,ACPNOTEQ     ERROR NOT EQUAL                              
*                                                                               
DBCR30   CLI   BYTE,C'N'          ANY POSTING FOUND                             
         BNE   DBCRX                                                            
         OI    ACPERR,ACPMISSA    NO POSTING                                    
DBCRX    XIT1                                                                   
         SPACE 2                                                                
*====================*                                                          
* LITERAL POOL       *                                                          
*====================*                                                          
*                                                                               
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*===================================================*                           
* RTNPSTS - GET EXPANDED POSTINGS TO AREA REQUESTED *                           
*===================================================*                           
*                                                                               
         DS    0D                                                               
RTNPSTS  NMOD1 0,**RPST**                                                       
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
*                                                                               
         L     R1,ACPPOST2         R1=A(POSTING AREA REQUESTED)                 
         L     R4,APOSTS           R4=A(EXPANDED POSTINGS)                      
         USING ACPRTND,R4                                                       
         ZIC   R3,NAPOSTS          NUMBER OF POSTINGS                           
         LTR   R3,R3                                                            
         BZ    RTNPSTX                                                          
*                                                                               
         LA    RE,ACPRTNL2-1                                                    
         CLI   LRTN,C'Y'                                                        
         BE    *+8                                                              
         LA    RE,ACPRTNL-1                                                     
*                                                                               
RTNPST5  EX    RE,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),0(R4)                                                    
         BZ    RTNPST8                                                          
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R4)                                                    
*                                                                               
RTNPST8  LA    R1,1(RE,R1)                                                      
         LA    R4,1(RE,R4)                                                      
         BCT   R3,RTNPST5                                                       
RTNPSTX  XIT1                                                                   
         SPACE                                                                  
*LITERAL POOL                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*===============================*                                               
*        DATAMGR CALLS          *                                               
*===============================*                                               
         DS    0D                                                               
READ     NMOD1 0,**READ**                                                       
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
*                                                                               
         LA    R2,IOAREA                                                        
         MVC   KEYSAVE,KEY                                                      
         XC    DMCB(24),DMCB                                                    
         OC    ACPSW,ACPSW        SWITCH ADDRESS PROVIDED                       
         BNZ   READ10             YES - ONLINE                                  
         L     RF,ACPUTL          OFFLINE - SET SE # IN THE UTL                 
         MVC   4(1,RF),ACPSE1                                                   
         CLI   RDSE,1                                                           
         BE    READ40                                                           
         MVC   4(1,RF),ACPSE2                                                   
         B     READ40                                                           
*                                                                               
READ10   LA    R1,ACPSE1                                                        
         CLI   RDSE,1             READ FROM NATIVE SYSTEM                       
         BE    READ20                                                           
         CLI   ACPSE2,0           READ FROM OTHER SYSTEM-IF APPLICABLE          
         BE    READ20                                                           
         CLC   ACPSE1,ACPSE2                                                    
         BE    READ20                                                           
         LA    R1,ACPSE2                                                        
READ20   XC    DMCB(24),DMCB                                                    
         MVC   DMCB(1),0(R1)                                                    
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 ACPSW,DMCB         SWITCH TO OTHER SYSTEM                        
         CLI   4(R1),0                                                          
         BE    READ40                                                           
         DC    H'0'               SWITCH NOT SUCCESFUL                          
READ40   GOTO1 VDATAMGR,DMCB,=CL8'DMREAD',=C'ACCOUNT',KEY,(R2)                  
         CLI   DMCB+8,0           SET CC CODE                                   
         BNE   READX                                                            
         CLC   KEY,0(R2)          DID WE GET RECORD REQUESTED                   
READX    XIT1                                                                   
         SPACE                                                                  
* LITERAL POOL                                                                  
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
VDATAMGR DS    F                                                                
VDATCON  DS    F                                                                
AMTTBL   DS    F                                                                
MEMOTBL  DS    F                                                                
RELO     DS    F                                                                
ABUFF    DS    A                                                                
APOSTS   DS    A                   A(EXPANDED POSTINGS)                         
POSTL    EQU   6000                LENGTH OF APOSTS                             
*                                                                               
DMCB     DS    6F                                                               
WORK     DS    XL100                                                            
DUB      DS    D                                                                
DUB2     DS    D                                                                
FULL     DS    F                                                                
BYTE     DS    X                                                                
TBYTE    DS    X                                                                
SVTBYTE  DS    X                                                                
ADJSTAT  DS    X                                                                
ADJCOST  EQU   X'80'               INDICATE COSTING POSTINGS TOO                
ADJBILL2 EQU   X'40'               INDICATE BILL2 POSTING                       
FLAG     DS    C                                                                
NAPOSTS  DS    X                   ACTUAL NUMBER OF EXPANDED POSTINGS           
PRVCODE  DS    CL2                 PROVIDENCE CODE                              
OUTCODE  DS    C                   TAX TYPE CODE                                
RULTYPE  DS    C                   I=INPUT,O=OUTPUT                             
RULES    DS    C                   P,O,E - LEVELS OF RECORD READ                
GSTI     DS    C                                                                
ROBFLAG  DS    C                                                                
ACCERROR DS    C                                                                
UPDBUFF  DS    C                                                                
RULREC   DS    C                                                                
BPCRULE  DS    C                                                                
PSTAFLG  DS    C                                                                
PSTMFLG  DS    C                                                                
FLAG2    DS    X                                                                
FLAG3    DS    X                                                                
READ2    DS    X                   SECOND RECORD TYPE TO READ                   
SVREAD   DS    X                   SAVED RECORD TYPE (READ INDICATOR)           
READ3    DS    X                   THIRD RECORD TYPE TO READ                    
AMTSW    DS    X                                                                
LRTN     DS    X                                                                
INTERNAL DS    X                                                                
LEVELS   DS    XL9                2X4 WITH 00 END OF TABLE                      
RDSE     DS    XL1                SE NUMBER FOR READ CALL                       
PRDREC   DS    CL1                EXISTANCE OF PRODUCT RECORD                   
PRD1C    DS    CL1                EXISTANCE OF PRODUCT LVL 1C                   
SUFFIX   DS    CL(L'ACPSUFX)                                                    
CCMEDPOS DS    XL1                                                              
CCMEDINC DS    CL(L'RSTCCTR)                                                    
CCAORPOS DS    XL1                                                              
CCAORINC DS    CL(L'RSTCCTR)                                                    
CCSELPOS DS    XL1                                                              
CCSELINC DS    CL(L'RSTCCTR)                                                    
CCINTPOS DS    XL1                                                              
CCINTINC DS    CL(L'RSTCCTR)                                                    
ACCOFF   DS    CL2                                                              
TEMPACC  DS    CL(L'ACPACC)                                                     
SVRCACC  DS    CL(L'ACPACC)       SAVED SR ACCOUNT                              
SVCRROW  DS    CL(ACPRTNL2)       SAVED CR POSTING ROW                          
SVDRROW  DS    CL(ACPRTNL2)       SAVED DR POSTING ROW                          
TEMPDATE DS    CL6                                                              
LEVEL    DS    CL3                                                              
PRTTYPE  DS    CL3                                                              
EXP      DS    CL8                                                              
EXPEQU   DS    CL1                                                              
IORAMT   DS    PL6                                                              
AMOUNT   DS    PL6                                                              
MEMO     DS    PL6                                                              
TEMP     DS    PL6                                                              
DEBS     DS    PL6                                                              
CREDS    DS    PL6                                                              
PNUM     DS    0PL16                                                            
PAMT1    DS    PL8                                                              
PAMT2    DS    PL8                                                              
CDAMT    DS    PL8                                                              
DIFFAMT  DS    PL8                                                              
RETACC   DS    CL(L'BRETACCT)                                                   
KEY      DS    XL42                                                             
KEYSAVE  DS    XL42                                                             
SPACES   DS    CL42                                                             
ACCNAME  DS    CL36                                                             
SVCOST   DS    CL(ACPRTNL)         SAVED COSTING ROW                            
SVCOSTI  DS    CL(ACPRTNL)         SAVED INTERNAL COSTING ROW                   
*                                                                               
         DS    0D                                                               
IOAREA   DS    CL2000                                                           
WORKDL   EQU   *-WORKD                                                          
         SPACE 2                                                                
         EJECT                                                                  
* - EXPTBL DSECT                                                                
*                                                                               
EXPD     DSECT                                                                  
EXPNM    DS    CL8                EXPRESSION NAME                               
EXPRTN   DS    XL4                ADDRESS OF ROUTINE                            
*                                                                               
EXPFLG   DS    CL1                ADDITIONAL POSTINGS                           
EXPFMIN  EQU   C'M'               POST -AMOUNT                                  
EXPFNEG  EQU   C'Z'               POST BACK OUT AS -AMT/MEMO                    
EXPFZER  EQU   C'0'               POST BACK OUT AS ZERO                         
EXPFAOR  EQU   C'A'               POST BACK OUT WITH AOR AMOUNT                 
EXPFIOR  EQU   C'I'               POST BACK OUT WITH IOR AMOUNT                 
EXPFLG2  DS    CL1                ADDITIONAL POSTINGS                           
EXPF2AOR EQU   C'A'               POST BACK OUT WITH AOR AMOUNT                 
EXPF2ZER EQU   C'0'               POST BACK OUT AS ZERO                         
*                                                                               
EXPFLG3  DS    CL1                FLAG FOR EXTRA AMT(S) TO BE +/-               
EXPF3GST EQU   X'80'              ADD GST AMOUNT                                
EXPF3CDP EQU   X'40'              ADD CD AMOUNT                                 
EXPF3CDM EQU   X'20'              SUBTRACT CD AMOUNT                            
EXPF3PST EQU   X'10'              ADD PST AMOUNT                                
EXPF3TRM EQU   X'08'              SUBTRACT TRADE AMOUNT                         
EXPF3POI EQU   X'04'              % OF OINC OR XINC                             
*****EXPF3OIP EQU   X'02'              100%- %OF OINC                           
EXPL     EQU   *-EXPNM            LENGTH OF ONE EXPTBL ENTRY                    
         SPACE                                                                  
* - ACCD DSECT                                                                  
*                                                                               
ACCD     DSECT                                                                  
ACCACT   DS    CL14               ACCOUNT                                       
ACCACTN  DS    CL36               ACCOUNT NAME                                  
ACCL     EQU   *-ACCACT           LENGTH OF ONE ACCD ENTRY                      
         SPACE                                                                  
*===========================*                                                   
* OTHER VARIOUS DSECTS      *                                                   
*===========================*                                                   
*                                                                               
* ACPOSTD                                                                       
       ++INCLUDE ACPOSTD                                                        
         EJECT                                                                  
* DDCOMFACSD                                                                    
* ACGENFILE                                                                     
* SPGENBILL                                                                     
* PBILLREC                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE ACGENFILE                                                      
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         ORG BILLRECD                                                           
       ++INCLUDE PBILLREC                                                       
*                                                                               
       ++INCLUDE PBLPSTEL                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076ACPOSTER  03/28/13'                                      
         END                                                                    
