*          DATA SET TAGENAA    AT LEVEL 052 AS OF 10/11/16                      
*PHASE T702AAB,*                                                                
         TITLE 'T702AA - CLA/CBL MAINTENANCE'                                   
T702AA   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702AA                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=A(TWAHOLE)                                
         USING WORKD,R7                                                         
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         CLI   TWASCR,SCR73        IF ON VNR/LIST                               
         BNE   CLA0                                                             
         BRAS  RE,VNRLIST          GO TO COMPLETELY SEPARATE AREA               
         B     XIT                                                              
         SPACE 1                                                                
CLA0     GOTO1 INITIAL,DMCB,PFTAB                                               
         BAS   RE,DISHEADS                                                      
         SPACE 1                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   CLA14                                                            
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',CLAAGYH),CLAAGYNH  AGENCY             
         SPACE 1                                                                
         MVI   NWKREQ,C'N'         TURN OFF NETWORK REQ'D (CLA USES)            
         BAS   RE,GETRULES         SET NWKREQ FROM AGENCY RECORD                
         SPACE 1                                                                
         LA    R2,CLAINVH                                                       
         CLI   5(R2),0             IF NO INVOICE INPUT                          
         BNE   CLA5                                                             
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'24',CLAINVH) USE GLOBAL INVOICE           
         SPACE 1                                                                
         CLI   ERROR,MISSING       IF THERE IS A GLOBAL INVOICE                 
         BE    THEEND                                                           
         MVC   CINV,TGINV          COMPLEMENTED INVOICE NUMBER                  
         XC    CLAINV,=6X'FF'      UN-COMPLEMENT INVOICE NUMBER                 
         GOTO1 TINVCON,DMCB,CLAINV,INV,DATCON CONVERT FOR DISPLAY               
         MVC   CLAINV,INV                                                       
         CLI   0(R1),X'FF'         IF INVALID                                   
         BNE   CLA3                                                             
         XC    CLAINV,CLAINV       CLEAR FROM SCREEN                            
         B     FLDMISS             REQUIRE INVOICE INPUT                        
         SPACE                                                                  
CLA3     LA    R2,CLAAGYH                                                       
         CLI   ERROR,NOTFOUND                                                   
         BE    THEEND                                                           
         B     CLA9                                                             
         SPACE                                                                  
CLA5     GOTO1 TINVCON,DMCB,CLAINV,INV,DATCON CONVERT INV INPUT FOR KEY         
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    INV,=6X'FF'         COMPLEMENT INVOICE NUMBER                    
         MVC   CINV,INV            COMPLEMENTED INVOICE NUMBER                  
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',INV) BUILD THE KEY                    
         BNE   THEEND                                                           
         SPACE                                                                  
         USING TAIND,R4                                                         
CLA9     LA    R2,CLAINVH                                                       
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL            GET INVOICE STATUS EL                        
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         TM    TAINSTAT,TAINSPAY                                                
         BZ    NOTPD               MUST BE PAID ALREADY                         
         SPACE                                                                  
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ                                                   
         L     R4,AIO                                                           
         BAS   RE,GETEL            GET PAYMENT DETAILS EL                       
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE IF PAID ALREADY                
         SPACE 1                                                                
         CLI   RECNUM,P5F          IF NOT ON CABLE SCREEN                       
         BE    CLA9A                                                            
         CLI   RECNUM,PBB          OR WILDSPOT SCREEN                           
         BE    CLA9A                                                            
         CLI   RECNUM,P66          OR ADDENDUM WILDSPOT SCREEN                  
         BE    CLA9A                                                            
         CLI   RECNUM,P59          OR INDUSTRIAL SUPPLEMENTAL SCREEN            
         BE    CLA9A                                                            
*                                                                               
         LA    RE,NCLARTAB                                                      
CLA7     CLI   0(RE),X'FF'         AND USE CANNOT HAVE INTERNET/                
         BE    CLA8                NEWMEDIA ON IT                               
         CLC   RECNUM,0(RE)                                                     
         BE    CLA9A                                                            
         LA    RE,1(RE)                                                         
         B     CLA7                                                             
*                                                                               
CLA8     CLC   TAPDUSE,=C'CLA'     IF CLA INVOICE                               
         BNE   *+18                                                             
         CLC   =C'CLA',CONREC      AND RECORD TYPE IS NOT CLA                   
         BE    CLA12                                                            
         B     CLA10               GO CHANGE TO CORRECT RECORD TYPE             
*                                                                               
         CLC   TAPDUSE,=C'LNA'     IF LNA INVOICE                               
         BNE   *+18                                                             
         CLC   =C'LNA',CONREC      AND RECORD TYPE IS NOT LNA                   
         BE    CLA12                                                            
         B     CLA10               GO CHANGE TO CORRECT RECORD TYPE             
*                                                                               
         CLC   TAPDUSE,=C'VRE'     IF VRE INVOICE                               
         BNE   *+18                                                             
         CLC   =C'VRE',CONREC      AND RECORD TYPE IS NOT VRE                   
         BE    CLA12                                                            
         B     CLA10               GO CHANGE TO CORRECT RECORD TYPE             
*                                                                               
         CLC   TAPDUSE,=C'VNR'     IF VNR INVOICE                               
         BNE   *+18                                                             
         CLC   =C'VNR',CONREC      AND RECORD TYPE IS NOT VNR                   
         BE    CLA12                                                            
         B     CLA10               GO CHANGE TO CORRECT RECORD TYPE             
*                                                                               
         CLC   TAPDUSE,=C'LNN'     IF LNN INVOICE                               
         BNE   *+18                                                             
         CLC   =C'LNN',CONREC      AND RECORD TYPE IS NOT LNN                   
         BE    CLA12                                                            
         B     CLA10               GO CHANGE TO CORRECT RECORD TYPE             
*                                                                               
         CLC   TAPDUSE,=C'LNC'     IF LNC INVOICE                               
         BNE   *+18                                                             
         CLC   =C'LNC',CONREC      AND RECORD TYPE IS NOT LNC                   
         BE    CLA12                                                            
         B     CLA10               GO CHANGE TO CORRECT RECORD TYPE             
*                                                                               
         CLC   TAPDUSE,=C'LNF'     IF LNF INVOICE                               
         BNE   *+18                                                             
         CLC   =C'LNF',CONREC      AND RECORD TYPE IS NOT LNF                   
         BE    CLA12                                                            
         B     CLA10               GO CHANGE TO CORRECT RECORD TYPE             
*                                                                               
         CLC   TAPDUSE,=C'PAX'     IF PAX INVOICE                               
         BNE   CLA9A                                                            
         CLC   =C'PAX',CONREC      AND RECORD TYPE IS NOT PAX                   
         BE    CLA12                                                            
         B     CLA10                                                            
*                                                                               
CLA9A    MVC   TGUSCDE,TAPDUSE                                                  
*                                                                               
         CLC   TAPDUSE,=C'CBL'     IF CBL INVOICE                               
         BNE   *+18                                                             
         CLC   =C'CBL',CONREC      AND RECORD TYPE IS NOT CBL                   
         BE    CLA12                                                            
         B     CLA10               GO CHANGE TO CORRECT RECORD TYPE             
*                                                                               
         CLC   TAPDUSE,=C'SCB'     IF SCB INVOICE                               
         BNE   *+18                                                             
         CLC   =C'SCB',CONREC      AND RECORD TYPE IS NOT SCB                   
         BE    CLA12                                                            
         B     CLA10                                                            
*                                                                               
         CLC   TAPDUSE,=C'SWS'     IF SWS INVOICE                               
         BNE   *+18                                                             
         CLC   =C'SWS',CONREC      AND RECORD TYPE IS NOT SWS                   
         BE    CLA12                                                            
         B     CLA10                                                            
*                                                                               
         CLC   TAPDUSE,=C'WSC'     IF WSC INVOICE                               
         BNE   *+18                                                             
         CLC   =C'WSC',CONREC      AND RECORD TYPE IS NOT WSC                   
         BE    CLA12                                                            
         B     CLA10                                                            
*                                                                               
         CLC   TAPDUSE,=C'WSP'     IF WSP INVOICE                               
         BNE   *+18                                                             
         CLC   =C'WSP',CONREC      AND RECORD TYPE IS NOT WSP                   
         BE    CLA12                                                            
         B     CLA10                                                            
*                                                                               
         CLC   TAPDUSE,=C'ADW'     IF ADW INVOICE                               
         BNE   *+18                                                             
         CLC   =C'ADW',CONREC      AND RECORD TYPE IS NOT WSP                   
         BE    CLA12                                                            
         B     CLA10                                                            
*                                                                               
         CLC   TAPDUSE,=C'LCB'     IF LCB INVOICE                               
         BNE   *+18                                                             
         CLC   =C'LCB',CONREC      AND RECORD TYPE IS NOT LCB                   
         BE    CLA12                                                            
         B     CLA10                                                            
*                                                                               
         CLC   TAPDUSE,=C'ISU'     IF ISU INVOICE                               
         BNE   CLA9B                                                            
         CLC   =C'ISU',CONREC      AND RECORD TYPE IS NOT ISU                   
         BE    CLA12                                                            
         B     CLA10                                                            
*                                                                               
CLA9B    LA    RE,NCLAUTAB                                                      
CLA9C    CLI   0(RE),X'FF'                                                      
         BE    NOTCBL                                                           
         CLC   TAPDUSE,0(RE)       ELSE, IF RECORD TYPE DOES NOT                
         BE    CLA9D               MATCH SCREEN, GO CHANGE IT                   
         LA    RE,3(RE)                                                         
         B     CLA9C                                                            
CLA9D    CLC   CONREC(3),0(RE)                                                  
         BE    CLA13                                                            
*                                                                               
CLA10    BAS   RE,SETPFK           SET PFKEY TO CHANGE RECORD TYPE              
         GOTO1 INITIAL,DMCB,IPFTAB                                              
         DC    H'0'                                                             
         SPACE 3                                                                
CLA12    MVC   NUMUSE,TAPDUSES     SAVE TOTAL NUMBER OF USES PAID               
         MVC   PDOPT3,TAPDOPT3          PAYMENT OPTION 3                        
         MVC   PCYCS(6),TAPDCYCS        CYCLE DATES                             
         BAS   RE,RDCLI            READ CLIENT RECORD FOR NWKREQ                
         SPACE 3                                                                
CLA13    MVC   PCYCS(6),TAPDCYCS                                                
         B     XIT                                                              
         SPACE 3                                                                
CLA14    CLI   MODE,DISPREC                                                     
         BE    CLA15                                                            
         CLI   MODE,XRECPUT        IF RECORD HAS BEEN CHANGED                   
         BNE   CLA20                                                            
         SPACE 2                                                                
CLA15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 2                                                                
CLA20    CLI   MODE,VALREC         IF MODE IS VALIDATE RECORD                   
         BNE   XIT                                                              
         SPACE                                                                  
         BAS   RE,CHGREC           MUST BE CHANGE                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SWITCH TO CORRECT RECORD TYPE                         
         SPACE                                                                  
         USING TAPDD,R4                                                         
SETPFK   NTR1                                                                   
         OI    TRNSTAT,OKINTPFK    INTERNAL PFKEY                               
         MVI   PFAID,21                                                         
         CLC   TAPDUSE,=C'CLA'     CHANGE RECORD TYPE TO CLA                    
         BE    SPFKX                                                            
         MVI   PFAID,22                                                         
         CLC   TAPDUSE,=C'LNA'     CHANGE RECORD TYPE TO LNA                    
         BE    SPFKX                                                            
         MVI   PFAID,23                                                         
         CLC   TAPDUSE,=C'LNN'     CHANGE RECORD TYPE TO LNN                    
         BE    SPFKX                                                            
         MVI   PFAID,24                                                         
         CLC   TAPDUSE,=C'LNC'     CHANGE RECORD TYPE TO LNC                    
         BE    SPFKX                                                            
         MVI   PFAID,20                                                         
         CLC   TAPDUSE,=C'PAX'     CHANGE RECORD TYPE TO PAX                    
         BE    SPFKX                                                            
         SPACE 1                                                                
         MVI   PFAID,19                                                         
         MVC   IPFUSE(L'TAPDUSE),TAPDUSE                                        
         CLC   TAPDUSE,=C'CBL'     CHANGE RECORD TYPE TO CBL                    
         BE    SPFKX                                                            
         CLC   TAPDUSE,=C'SCB'     CHANGE RECORD TYPE TO SCB                    
         BE    SPFKX                                                            
         CLC   TAPDUSE,=C'SWS'     CHANGE RECORD TYPE TO SWS                    
         BE    SPFKX                                                            
         CLC   TAPDUSE,=C'WSC'     CHANGE RECORD TYPE TO WSC                    
         BE    SPFKX                                                            
         CLC   TAPDUSE,=C'WSP'     CHANGE RECORD TYPE TO WSP                    
         BE    SPFKX                                                            
         CLC   TAPDUSE,=C'ADW'     CHANGE RECORD TYPE TO ADW                    
         BE    SPFKX                                                            
         CLC   TAPDUSE,=C'LCB'     CHANGE RECORD TYPE TO LCB                    
         BE    SPFKX                                                            
         CLC   TAPDUSE,=C'ISU'     CHANGE RECORD TYPE TO ISU                    
         BE    SPFKX                                                            
         CLC   TAPDUSE,=C'VRE'     CHANGE RECORD TYPE TO VRE                    
         BE    SPFKX                                                            
         CLC   TAPDUSE,=C'VNR'     CHANGE RECORD TYPE TO VNR                    
         BE    SPFKX                                                            
         CLC   TAPDUSE,=C'LNF'     CHANGE RECORD TYPE TO LNF                    
         BE    SPFKX                                                            
         SPACE 1                                                                
         LA    RE,NCLAUTAB                                                      
SPFK10   CLI   0(RE),X'FF'                                                      
         BE    SPFK20                                                           
         CLC   TAPDUSE,0(RE)                                                    
         BE    SPFKX                                                            
         LA    RE,3(RE)                                                         
         B     SPFK10                                                           
*                                                                               
SPFK20   DC    H'0'                                                             
SPFKX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY SCREEN FIELD PROMPTS                          
DISHEADS NTR1                                                                   
         CLC   =C'CLA',CONREC      EXIT IF ON CLASS A SCREEN                    
         BE    DHEADSX                                                          
         CLC   =C'ISU',CONREC      OR ISU SCREEN                                
         BE    DHEADSX                                                          
         CLC   =C'VRE',CONREC      OR VRE SCREEN                                
         BE    DHEADSX                                                          
         CLC   =C'VNR',CONREC      OR VRN SCREEN                                
         BE    DHEADSX                                                          
         SPACE 1                                                                
         LA    RE,NCLARTAB                                                      
DHEADS10 CLI   0(RE),X'FF'         SKIP AHEAD IF USE HAS INTERNET/              
         BE    DHEADS20            NEWMEDIA                                     
         CLC   RECNUM,0(RE)                                                     
         BE    DHEADS50                                                         
         LA    RE,1(RE)                                                         
         B     DHEADS10                                                         
         SPACE 1                                                                
DHEADS20 GOTO1 FLDVAL,DMCB,(X'01',CBLOVHDH),CBLSUBH                             
         SPACE 1                                                                
         CLI   RECNUM,P5F          IF ON CBL/SCB/SWS/WSC/LCB SCREEN             
         BE    DHEADS30                                                         
         CLI   RECNUM,PBB          OR WSP SCREEN                                
         BE    DHEADS30                                                         
         CLI   RECNUM,P66          OR ADDENDUM SCREEN                           
         BNE   DHEADSX                                                          
         SPACE                                                                  
DHEADS30 MVC   CBLOVHD,OVHEAD      MOVE OVERRRIDE                               
         MVC   CBLAUHD,AUHEAD      AUTOMATIC                                    
         MVC   CBLUPHD,UPHEAD      AND UPGRADE HEADINGS TO SCREEN               
         SPACE 1                                                                
         LA    R2,CBLCOHDH         MOVE NTI/MKT HEADINGS TO SCREEN              
         LHI   RE,4                                                             
         SPACE 1                                                                
         LA    R1,CBLHEAD                                                       
         MVC   CBLSUHD,SUHEAD                                                   
         CLC   =C'CBL',CONREC                                                   
         BE    DHEADS40                                                         
         CLC   =C'SCB',CONREC                                                   
         BE    DHEADS40                                                         
         LA    R1,SYSHEAD                                                       
         MVC   CBLSUHD,SPACES                                                   
         CLC   =C'LCB',CONREC                                                   
         BE    DHEADS40                                                         
         LA    R1,MKTHEAD                                                       
         CLC   =C'WSP',CONREC                                                   
         BNE   DHEADS40                                                         
         MVC   CBLSUHD,MJHEAD                                                   
         SPACE 1                                                                
DHEADS40 MVC   8(L'CBLHEAD,R2),0(R1)                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   RE,DHEADS40                                                      
         B     DHEADSX                                                          
         SPACE 1                                                                
DHEADS50 LA    R2,NMRFRSTH                                                      
         LA    R3,NMRLSTH                                                       
         SPACE 1                                                                
DHEADS60 CR    R2,R3                                                            
         BH    DHEADSX                                                          
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         XC    8(4,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         XC    8(30,R2),8(R2)                                                   
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     DHEADS60                                                         
DHEADSX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET NWKREQ SWITCH                                     
         SPACE                                                                  
GETRULES NTR1                                                                   
         MVI   ELCODE,TABRELQ      GET BILLING RULES ELEMENT                    
         L     R4,AIO              R4=A(AGENCY/CLIENT RECORD)                   
         BAS   RE,GETEL                                                         
         BNE   GETRULEX                                                         
*                                                                               
         USING TABRD,R4                                                         
         TM    TABRSTAT,TABRSNWK   IF NETWORK REQUIRED                          
         BZ    GETRULEX                                                         
         MVI   NWKREQ,C'Y'         SET SWITCH ON                                
*                                                                               
GETRULEX B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO READ CLIENT RECORD AND SET NWKREQ                     
         SPACE                                                                  
RDCLI    NTR1                                                                   
         MVC   SVKEY,KEY           SAVE THE KEY                                 
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE BECAUSE PAID ALREADY           
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TAPDCOM)                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TACOD,R4                                                         
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGCID,TACOCID       SAVE CURRENT COMMERCIAL ID                   
         DROP  R4                                                               
*                                                                               
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A0',TLCOCLI)                              
         BNE   RDCLIX                                                           
         BAS   RE,GETRULES         SET NWKREQ FROM CLIENT RECORD                
*                                                                               
RDCLIX   MVC   AIO,AIO1            RESTORE THE IOAREA                           
         MVC   KEY,SVKEY           AND THE KEY                                  
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         BAS   RE,CLEARSCR         CLEAR SCREEN                                 
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TALFELQ      GET LIFT DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   DIS3                                                             
         USING TALFD,R4                                                         
         MVI   CLALFT,C'Y'         SET LFT TO Y                                 
         MVC   CLALID,TALFLID      LIFT ID                                      
         B     DIS5                                                             
         SPACE                                                                  
DIS3     L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ      GET VERSIONS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DIS5                                                             
         USING TAVRD,R4                                                         
         MVC   CLALFT(1),TAVRVERS  SET LFT TO VERSION CODE                      
         MVC   CLALID,TAVRCID      SET VERSION ID                               
         SPACE                                                                  
         TM    TGSYSTAT,TASYS3VR   IF VERSIONS ARE 3 CHARACTERS LONG            
         BZ    DIS5                DISPLAY 3 CHAR NUMERIC VERSION CODE          
         EDIT  TAVRVERS,CLALFT,ALIGN=LEFT                                       
         SPACE                                                                  
DIS5     MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE BECAUSE PAID ALREADY           
         USING TACOD,R4                                                         
         MVC   CLACID,TACOCID      COMMERCIAL ID                                
         GOTO1 CHAROUT,DMCB,TAFNELQ,CLACOMNH,TAFNTTTL COMMERCIAL TITLE          
         SPACE                                                                  
         OC    PCYCS,PCYCS         IF CYCLE DATES PRESENT                       
         BZ    DIS5A                                                            
         GOTO1 DATCON,DMCB,(X'11',PCYCS),(8,CLACYC)  CYCLE DATES                
         SPACE                                                                  
DIS5A    CLI   RECNUM,P5F                                                       
         BE    DIS10                                                            
         CLI   RECNUM,PBB                                                       
         BE    DIS10                                                            
         CLI   RECNUM,P66                                                       
         BE    DIS10                                                            
         CLI   RECNUM,P7A                                                       
         BE    DIS30                                                            
         CLI   RECNUM,P8E                                                       
         BE    DIS40                                                            
         SPACE                                                                  
         LA    RE,NCLARTAB                                                      
DIS5B    CLI   0(RE),X'FF'                                                      
         BE    DIS5C                                                            
         CLC   RECNUM,0(RE)                                                     
         BE    DIS20                                                            
         LA    RE,1(RE)                                                         
         B     DIS5B                                                            
         SPACE 1                                                                
DIS5C    BRAS  RE,DISUSE           DISPLAY NUMBER OF USES                       
         SPACE 1                                                                
         USING CLASCRND,R2                                                      
         LH    R0,NUMUSE           R0=TOTAL NUMBER OF USES PAID                 
         CH    R0,=H'20'           IF OVER MAX N'SCREEN ENTRIES                 
         BH    DIS6                NO USE INFO TO DISPLAY                       
         SPACE 1                                                                
         BAS   RE,DISTANP          DISPLAY TANPD ELEMENTS                       
DIS6     GOTO1 FLDVAL,DMCB,(X'20',CLAFRSTH),999  SET ALL USE INFO VALID         
         B     XIT                                                              
         SPACE 1                                                                
DIS10    BRAS  RE,DISMKT                                                        
         GOTO1 FLDVAL,DMCB,(X'20',CBLOVHDH),999  SET ALL USE INFO VALID         
         B     XIT                                                              
         SPACE 1                                                                
DIS20    BRAS  RE,DISMED                                                        
         B     XIT                                                              
         SPACE 1                                                                
DIS30    BRAS  RE,DISISU                                                        
         B     XIT                                                              
         SPACE 1                                                                
DIS40    BRAS  RE,DISVER                                                        
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CLEAR THE SCREEN                                      
         SPACE                                                                  
CLEARSCR NTR1                                                                   
         TWAXC CLACIDH                                                          
         XC    CLALID,CLALID       CLEAR AND TRANSMIT PROTECTED FIELDS          
         OI    CLALIDH+6,X'80'                                                  
         XC    CLACOMN,CLACOMN                                                  
         OI    CLACOMNH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY TANPD ELEMENTS                                
         SPACE                                                                  
DISTANP  NTR1                                                                   
         LA    R2,CLAFRSTH         LOOP THROUGH SCREEN USE ENTRIES              
         LA    R3,USETAB           R3=A(TABLE OF DATES/LIFT STATUS)             
*                                                                               
         MVI   ELCODE,TANPELQ      GET NETWORK PROGRAM DETAILS EL               
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
DISTANP4 BAS   RE,NEXTEL                                                        
         BNE   DISTANPX                                                         
         BAS   RE,DISDATE          DISPLAY USE DATE                             
         BAS   RE,DISPROG                  PROGGRAM NAME                        
         BAS   RE,DISLFT                   LIFT                                 
         BAS   RE,DISNWK                   NETWORK                              
         LA    R3,4(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         SPACE 1                                                                
         LA    R2,CLASNEXT         BUMP TO NEXT SET OF SCREEN FIELDS            
         BCT   R0,DISTANP4                                                      
         SPACE 1                                                                
DISTANPX B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY CLA USE DATES                                 
*              R3=USETAB ENTRY                                                  
         SPACE 1                                                                
         USING TANPD,R4            R4=A(TANP ELEMENT)                           
DISDATE  NTR1                                                                   
         CLC   TANPDATE,=C'TBA'    IF DATE IS UNKNOWN                           
         BNE   *+14                                                             
         MVC   CLASDTE(3),TANPDATE JUST MOVE TO SCREEN                          
         B     DDTEX                                                            
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(1,TANPDATE),(8,CLASDTE)  DISPLAY DATE               
         SPACE                                                                  
DDTEX    OI    CLASDTEH+6,X'80'                                                 
         MVC   0(3,R3),TANPDATE    SAVE PACKED DATE IN USETAB ENTRY             
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO DISPLAY CLA PROGRAM NAMES                             
         SPACE 1                                                                
DISPROG  DS    0H                                                               
         MVC   CLASPRG,TANPPNME    MOVE TO SCREEN                               
         OI    CLASPRGH+6,X'80'                                                 
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE TO DISPLAY CLA NETWORK FIELD                             
         SPACE 1                                                                
DISNWK   DS    0H                                                               
         MVC   CLASNWK,TANPNWK     MOVE TO SCREEN                               
         OI    CLASNWKH+6,X'80'                                                 
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE TO DISPLAY CLA LIFT FIELD                                
*              R3=USETAB ENTRY                                                  
         SPACE 1                                                                
DISLFT   DS    0H                                                               
         MVC   CLASLFT,TANPLFT     MOVE TO SCREEN                               
         OI    CLASLFTH+6,X'80'                                                 
         MVC   3(1,R3),TANPLFT     SAVE IN USETAB ENTRY                         
         BR    RE                                                               
         EJECT                                                                  
*              CHANGE THE RECORD                                                
         SPACE 1                                                                
CHGREC   NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'40',CLAFRSTH),999 TEST ALL USE INFO VALID         
         BE    XIT                 IF YES, DON'T BOTHER                         
         SPACE                                                                  
         LH    R0,NUMUSE           R0=TOTAL NUMBER OF USES PAID                 
         L     R2,8(R1)                                                         
         CH    R0,=H'20'           IF OVER MAX N'SCREEN ENTRIES                 
         BH    NOINPUT             INPUT NOT ALLOWED                            
         SPACE                                                                  
         USING TANPD,R4                                                         
CHGR5    MVI   ELCODE,TANPELQ      DELETE CURRENT NETWORK PROGRAM ELEMS         
         GOTO1 REMELEM                                                          
         XC    LASTDATE,LASTDATE                                                
         XC    LASTPROG,LASTPROG                                                
         GOTO1 DATCON,DMCB,(1,PCYCS),CYCSTART  CVT CYCLE TO EBCDIC              
         GOTO1 (RF),(R1),(1,PCYCE),CYCEND                                       
         SPACE                                                                  
         XC    ELEMENT,ELEMENT     SET TO BUILD NEW NETWORK PRG ELEMS           
         LA    R4,ELEMENT                                                       
         MVI   TANPEL,TANPELQ                                                   
         MVI   TANPLEN,TANPLNQ                                                  
         SPACE                                                                  
         USING CLASCRND,R2                                                      
         LA    R2,CLAFRSTH         LOOP THROUGH SCREEN USE ENTRIES              
         LA    R3,USETAB           R3=A(TABLE OF DATES/LIFT STATUS)             
         XR    R1,R1               R1=SEQUENCE NUMBER                           
         SPACE 1                                                                
CHGR10   BAS   RE,VALDATE          VALIDATE USE DATE                            
         BAS   RE,VALPROG          VALIDATE PROGRAM NAME                        
         BAS   RE,VALLFT           VALIDATE LFT                                 
         BAS   RE,VALNWK           VALIDATE NETWORK                             
         STC   R1,TANPSEQ          SEQUENCE NUMBER                              
         GOTO1 ADDL                ADD ELEMENT TO RECORD                        
         SPACE                                                                  
         LA    R3,4(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         LA    R1,1(R1)            INCREMENT SEQUENCE NUMBER                    
         LA    R2,CLASNEXT         BUMP TO NEXT SET OF SCREEN FIELDS            
         BCT   R0,CHGR10                                                        
         SPACE 1                                                                
         GOTO1 FLDVAL,DMCB,(X'80',(R2)),(X'80',999)                             
         BE    XIT                 TEST REST OF USE DETAILS EMPTY               
         L     R2,8(R1)                                                         
         B     NOINPUT             INPUT NOT ALLOWED                            
         EJECT                                                                  
*              ROUTINE TO VALIDATE CLA USE DATES                                
*              R3=A(USETAB ENTRY), R4=A(ELEMENT)                                
         SPACE 1                                                                
         USING TANPD,R4                                                         
         USING CLASCRND,R2         R2=A(SCREEN ENTRY)                           
VALDATE  NTR1                                                                   
         LA    R2,CLASDTEH                                                      
         CLI   5(R2),0             TEST USE DATE INPUT                          
         BE    FLDMISS                                                          
         TM    4(R2),X'20'         IF PREV. VALIDATED                           
         BZ    *+14                                                             
         MVC   TANPDATE,0(R3)      GET PACKED DATE FROM USETAB                  
         B     XIT                                                              
         SPACE 1                                                                
         CLI   8(R2),C'*'          ASTERISK MEANS SAME AS LAST                  
         BNE   VDTE8                                                            
         OC    LASTDATE,LASTDATE   TEST WE HAVE A LAST                          
         BZ    FLDINV                                                           
         MVC   TANPDATE,LASTDATE   PUT INTO ELEMENT                             
         B     VDTE20              AND CONTINUE                                 
         SPACE 1                                                                
VDTE8    CLC   =C'TBA',8(R2)       TEST FOR UNKNOWN DATE                        
         BNE   VDTE10                                                           
         MVC   TANPDATE,8(R2)      PUT INTO ELEMENT                             
         MVC   LASTDATE,8(R2)      SAVE MOST RECENT DATE                        
         B     VDTE20                                                           
         SPACE 1                                                                
VDTE10   CLI   5(R2),5             IF L'I/P LESS OR EQUAL TO FIVE               
         BH    VDTE12                                                           
         GOTO1 DATVAL,DMCB,(1,8(R2)),DUB   VALIDATE MONTH/DAY ONLY              
         OC    0(4,R1),0(R1)               TEST FOR INPUT ERROR                 
         BZ    DATEINV                                                          
         MVC   DUB(2),CYCSTART             CARRY YEAR FROM CYCLE START          
         CLC   CYCSTART(2),CYCEND          TEST CYCLE START YEAR <> END         
         BE    VDTE13                                                           
         CLC   DUB+2(4),CYCSTART+2         IF MM/DD IS LT CYCLE START           
         BNL   VDTE11                                                           
         CLC   DUB+2(4),CYCEND+2           & MM/DD IS NOT GT CYCLE END          
         BH    VDTE30                                                           
         MVC   DUB(2),CYCEND               CARRY YEAR FROM CYCLE END            
VDTE11   B     VDTE14                                                           
         SPACE 1                                                                
VDTE12   GOTO1 DATVAL,DMCB,8(R2),DUB       VALIDATE Y/M/D                       
         OC    0(4,R1),0(R1)               TEST FOR INPUT ERROR                 
         BZ    DATEINV                                                          
VDTE13   CLC   DUB(6),CYCSTART     IF USE DATE NOT BEFORE CYCLE START           
         BL    VDTE30                                                           
         CLC   DUB(6),CYCEND       AND NOT AFTER CYCLE END                      
         BH    VDTE30                                                           
         SPACE 1                                                                
VDTE14   GOTO1 DATCON,DMCB,(0,DUB),(1,TANPDATE) PUT INTO ELEMENT                
         SPACE 1                                                                
         MVC   LASTDATE,0(R3)               SAVE MOST RECENT DATE               
         SPACE 1                                                                
VDTE20   B     XIT                                                              
         SPACE 1                                                                
VDTE30   B     FLDINV              ERROR - USE DATE NOT WITHIN CYCLE            
         EJECT                                                                  
*              ROUTINE TO VALIDATE CLA PROGRAM NAMES                            
*              R3=A(USETAB ENTRY), R4=A(ELEMENT)                                
         SPACE 1                                                                
         USING TANPD,R4                                                         
         USING CLASCRND,R2         R2=A(SCREEN ENTRY)                           
VALPROG  NTR1                                                                   
         LA    R2,CLASPRGH         TEST FOR INPUT                               
         CLI   5(R2),0                                                          
         BE    VPRG5                                                            
         SPACE 1                                                                
         CLI   8(R2),C'*'          ASTERISK MEANS SAME AS LAST                  
         BNE   VPRG4                                                            
         OC    LASTPROG,LASTPROG   TEST WE HAVE A LAST                          
         BZ    FLDINV                                                           
         MVC   TANPPNME,LASTPROG   PUT INTO ELEMENT                             
         B     XIT                                                              
VPRG4    MVC   LASTPROG,8(R2)      SAVE NEW PROGRAM NAME                        
VPRG5    MVC   TANPPNME,8(R2)      PUT INTO EL (CLEARS OLD IF NO INPUT)         
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO VALIDATE CLA LFT FIELD                                
*              R3=A(USETAB ENTRY), R4=A(ELEMENT)                                
         SPACE 1                                                                
         USING TANPD,R4                                                         
VALLFT   DS    0H                                                               
         MVC   TANPLFT,3(R3)       GET LFT FROM USETAB                          
         TM    PDOPT3,TAPDONUS     IF NUMBER OF USES OVERRIDDEN                 
         BZR   RE                                                               
         MVC   TANPLFT,CLALFT      GET FROM SCREEN                              
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE TO VALIDATE CLA NETWORK FIELD                            
*              R3=A(USETAB ENTRY), R4=A(ELEMENT)                                
         SPACE 1                                                                
         USING TANPD,R4                                                         
         USING CLASCRND,R2         R2=A(SCREEN ENTRY)                           
VALNWK   NTR1                                                                   
         LA    R2,CLASNWKH                                                      
         CLI   5(R2),0             ACCEPT ANY INPUT                             
         BNE   *+12                                                             
         CLI   NWKREQ,C'Y'         TEST NETWORK REQUIRED ON CLA USES            
         BE    FLDMISS                                                          
         MVC   TANPNWK,8(R2)       PUT INOT ELEMENT                             
         B     XIT                                                              
         EJECT                                                                  
*              LOCAL ERROR/EXIT ROUTINES                                        
         SPACE                                                                  
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
DATEINV  MVI   ERROR,INVDATE       INVALID DATE                                 
         B     THEEND                                                           
         SPACE                                                                  
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
         SPACE                                                                  
NOTPD    MVI   ERROR,ERNOTPD       INVOICE NOT PAID                             
         B     THEEND                                                           
         SPACE                                                                  
NOTCLA   MVI   ERROR,ERNOTCLA      INVOICE NOT CLA                              
         B     THEEND                                                           
         SPACE                                                                  
NOTCBL   MVC   MYMSGNO,=Y(ERNOTCBL) INVOICE NOT CABLE                           
         B     NTHEEND                                                          
         SPACE                                                                  
ACTERR   MVI   ERROR,INVRCACT      RECORD/ACTION COMBINATION INVALID            
         LA    R2,CONACTH                                                       
         B     THEEND                                                           
         SPACE                                                                  
NTHEEND  OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
OVHEAD   DC    CL(L'CBLOVHD)'Override'                                          
AUHEAD   DC    CL(L'CBLAUHD)'Automatic'                                         
UPHEAD   DC    CL(L'CBLUPHD)'Upgrade?'                                          
SUHEAD   DC    CL(L'CBLSUHD)'Subscrbs'                                          
MJHEAD   DC    CL(L'CBLSUHD)'Majors'                                            
CBLHEAD  DC    CL(L'CBLCOHD)'CNet'                                              
MKTHEAD  DC    CL(L'CBLCOHD)'Mkt'                                               
SYSHEAD  DC    CL(L'CBLCOHD)'CSys'                                              
         SPACE 1                                                                
NCLARTAB DC    AL1(P81,P82,P8C,P64,P67,P6A,P70,P73,P75,PAF,PBE,PBF)             
         DC    X'FF'                                                            
         SPACE 1                                                                
NCLAUTAB DC    C'IRN'                                                           
         DC    C'NMR'                                                           
         DC    C'MVI'                                                           
         DC    C'MVN'                                                           
         DC    C'SIR'                                                           
         DC    C'SNM'                                                           
         DC    C'SMI'                                                           
         DC    C'SMN'                                                           
         DC    C'BSS'                                                           
         DC    C'SSS'                                                           
         DC    C'SCS'                                                           
         DC    C'BSC'                                                           
         DC    C'BSU'                                                           
         DC    C'DEM'                                                           
         DC    C'ADD'                                                           
         DC    C'SNA'                                                           
         DC    C'CDM'                                                           
         DC    C'CAU'                                                           
         DC    C'RRS'                                                           
         DC    C'ARS'                                                           
         DC    C'SRS'                                                           
         DC    C'ADC'                                                           
         DC    C'BSM'                                                           
         DC    C'IMS'                                                           
         DC    C'CMS'                                                           
         DC    C'ISS'                                                           
         DC    C'PRM'                                                           
         DC    C'IFS'                                                           
         DC    C'TAG'                                                           
         DC    C'VAR'                                                           
         DC    C'ADT'                                                           
         DC    C'AUD'                                                           
         DC    C'NBS'                                                           
         DC    C'CNL'                                                           
         DC    C'SCN'                                                           
         DC    C'INS'                                                           
         DC    C'FGS'                                                           
         DC    C'SFS'                                                           
         DC    C'PUB'                                                           
         DC    C'INU'                                                           
         DC    C'SIU'                                                           
         DC    C'NMU'                                                           
         DC    C'SNU'                                                           
         DC    X'FF'                                                            
         SPACE 1                                                                
PFTAB    DS    0C                  PF TABLE                                     
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'HISTORY',CL8'DISPLAY'                                 
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CYCLE  ',CL8'DISPLAY'                                 
PF14     DC    AL1(KEYTYTWA,L'CBLAGY-1),AL2(CBLAGY-T702FFD)                     
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYGLB,L'TGUSCDE-1),AL2(TGUSCDE-TGD)                       
         DC    AL1(0,0),AL2(0)                                                  
         DC    AL1(0,0),AL2(0)                                                  
         DC    AL1(KEYTYTWA,L'CBLINV-1),AL2(CBLINV-T702FFD)                     
PF14X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
IPFTAB   DS    0C                  INTERNAL PF TABLE                            
         DC    AL1(IPF21X-*,21,PFTINT,0,0)                                      
         DC    CL3' ',CL8'CLA    ',CL8'DISPLAY'                                 
IPF21X   EQU   *                                                                
*                                                                               
         DC    AL1(IPF22X-*,22,PFTINT,0,0)                                      
         DC    CL3' ',CL8'LNA    ',CL8'DISPLAY'                                 
IPF22X   EQU   *                                                                
*                                                                               
         DC    AL1(IPF23X-*,23,PFTINT,0,0)                                      
         DC    CL3' ',CL8'LNN    ',CL8'DISPLAY'                                 
IPF23X   EQU   *                                                                
*                                                                               
         DC    AL1(IPF24X-*,24,PFTINT,0,0)                                      
         DC    CL3' ',CL8'LNC    ',CL8'DISPLAY'                                 
IPF24X   EQU   *                                                                
*                                                                               
         DC    AL1(IPF20X-*,20,PFTINT,0,0)                                      
         DC    CL3' ',CL8'PAX    ',CL8'DISPLAY'                                 
IPF20X   EQU   *                                                                
*                                                                               
         DC    AL1(IPF19X-*,19,PFTINT,0,0)                                      
         DC    CL3' '                                                           
IPFUSE   DC    CL8'       '                                                     
         DC    CL8'DISPLAY'                                                     
IPF19X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY A SCREEN OF CNETS/MARKETS                     
         SPACE 1                                                                
DISMKT   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,CBLFRSTH                                                      
         SPACE 1                                                                
         USING TAMTD,R4                                                         
         L     R4,AIO            READ ALL CNET/MKT/CSYS ELEMENTS                
         MVI   ELCODE,TAMTELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DMKT10   BRAS  RE,NEXTEL                                                        
         BNE   DMKT20                                                           
         SPACE 1                                                                
         LA    RF,CBLLSTH        IF NOT PAST END OF DATE/CODE FIELDS            
         CR    R2,RF                                                            
         BH    DMKT20                                                           
         SPACE 1                                                                
         ZIC   RF,0(R2)          BUMP TO DATE FIELD                             
         AR    R2,RF                                                            
         SPACE 1                                                                
         OC    TAMTCYCS,TAMTCYCS IF DATE PRESENT, DISPLAY IT                    
         BZ    DMKT15                                                           
         GOTO1 DATCON,DMCB,(1,TAMTCYCS),(8,8(R2))                               
         SPACE 1                                                                
DMKT15   ZIC   RF,0(R2)          BUMP TO CODE FIELD                             
         AR    R2,RF             AND DISPLAY IT                                 
         MVC   8(L'TAMTCODE,R2),TAMTCODE                                        
         SPACE 1                                                                
         ZIC   RE,0(R2)          BUMP TO NEXT HEADER FIELD                      
         AR    R2,RE                                                            
         B     DMKT10                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         USING TLUHD,R4                                                         
DMKT20   LA    R4,KEY              GET USAGE HISTORY RECORD                     
         XC    TLUHKEY,TLUHKEY                                                  
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM                                                    
         MVC   TLUHUSE,TGUSCDE                                                  
         MVC   TLUHINV,TGINV                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLUHKEY),KEYSAVE                                           
         BNE   DMKTX                                                            
         GOTO1 GETREC                                                           
         DROP  R4                                                               
         SPACE                                                                  
         USING TAUHD,R4                                                         
         L     R4,AIO              GET USAGE HISTORY ELEMENT                    
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DMKTX                                                            
         SPACE 1                                                                
         CLC   =C'CBL',CONREC      FOR CABLE                                    
         BE    DMKT30                                                           
         CLC   =C'SCB',CONREC      AND SPANISH CABLE                            
         BNE   DMKT50                                                           
DMKT30   OC    TAUHICBU,TAUHICBU   INDICATE UPGRADE                             
         BZ    *+8                                                              
         MVI   CBLUPG,C'Y'                                                      
         TM    TAUHCSTA,TAUHSFUP   INDICATE FORCED UPGRADE                      
         BZ    *+8                                                              
         MVI   CBLUPG,C'F'                                                      
         TM    TAUHCSTA,TAUHSFIN   INDICATE FORCED INITIAL                      
         BZ    *+8                                                              
         MVI   CBLUPG,C'I'                                                      
         EDIT  TAUHCAUT,CBLAUT,ALIGN=LEFT                                       
         CLC   TAUHCAUT,TAUHCBUN                                                
         BE    DMKT40                                                           
         EDIT  TAUHCBUN,CBLOVR,ZERO=NOBLANK,ALIGN=LEFT                          
DMKT40   CLI   TAUHLEN,TAUHLNQ                                                  
         BL    DMKTX                                                            
         EDIT  TAUHCSUB,CBLSUB,ZERO=NOBLANK,ALIGN=LEFT                          
         B     DMKTX                                                            
         SPACE 1                                                                
DMKT50   CLC   =C'LCB',CONREC      FOR LOCAL CABLE                              
         BNE   DMKT60                                                           
         CLI   TAUHLEN,TAUHLNQ                                                  
         BL    DMKTX                                                            
         CLI   TAUHFRTY,0                                                       
         BE    *+8                                                              
         MVI   CBLUPG,C'Y'                                                      
         TM    TAUHLCST,TAUHLSFU                                                
         BZ    *+8                                                              
         MVI   CBLUPG,C'F'                                                      
         TM    TAUHLCST,TAUHLSFI                                                
         BZ    *+8                                                              
         MVI   CBLUPG,C'I'                                                      
         EDIT  TAUHASUB,CBLAUT,ALIGN=LEFT                                       
         CLC   TAUHASUB,TAUHSUBS                                                
         BE    DMKTX                                                            
         EDIT  TAUHSUBS,CBLOVR,ZERO=NOBLANK,ALIGN=LEFT                          
         B     DMKTX                                                            
         SPACE 1                                                                
DMKT60   OC    TAUHIUNT,TAUHIUNT   FOR WILDSPOT USES                            
         BZ    *+8                 INDICATE UPGRADE                             
         MVI   CBLUPG,C'Y'                                                      
         TM    TAUHSTAT,TAUHSFUP   INDICATE FORCED UPGRADE                      
         BZ    *+8                                                              
         MVI   CBLUPG,C'F'                                                      
         TM    TAUHSTAT,TAUHSFIN   INDICATE FORCED INITIAL                      
         BZ    *+8                                                              
         MVI   CBLUPG,C'I'                                                      
         EDIT  TAUHWAUT,CBLAUT,ALIGN=LEFT                                       
         CLC   TAUHWAUT,TAUHUNT                                                 
         BE    DMKT70                                                           
         EDIT  TAUHUNT,CBLOVR,ZERO=NOBLANK,ALIGN=LEFT                           
         SPACE 1                                                                
DMKT70   CLC   =C'WSP',CONREC      FOR WILDSPOT                                 
         BNE   DMKTX                                                            
         GOTO1 MAJVAL,DMCB,(X'80',TAUHMAJ)                                      
         MVC   CBLSUB,TGMACHAR                                                  
DMKTX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY INTERNET/NEW MEDIA                        *         
***********************************************************************         
                                                                                
DISMED   NTR1  BASE=*,LABEL=*                                                   
         USING TAMDD,R4                                                         
         L     R4,AIO1             ANY DISPLAY/NEW MEDIA ON INVOICE?            
         MVI   ELCODE,TAMDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
*                                                                               
         LA    R2,NMRFHEDH         R2=A(FIRST HEADER)                           
         LA    R3,NMRLHEDH         R3=A(LAST HEADER)                            
*                                                                               
DMED10   CR    R2,R3               SET THE HEADINGS                             
         JH    DMED20                                                           
         MVC   8(9,R2),=CL9'Internet'                                           
         MVI   TGMEEQU,INTERNET                                                 
         CLI   TAMDTYPE,INTERNET                                                
         JE    *+14                                                             
         MVC   8(9,R2),=CL9'New Media'                                          
         MVI   TGMEEQU,NEWMEDIA                                                 
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         J     DMED10                                                           
*                                                                               
DMED20   LA    R2,NMRFRSTH         R2=A(FIRST MEDIA FIELD)                      
         MVC   AIO,AIO2            READ INTERNET/NEW MEDIA INTO AIO2            
         J     DMED40                                                           
*                                                                               
DMED30   BRAS  RE,NEXTEL                                                        
         JNE   DMED50                                                           
*                                                                               
DMED40   ZIC   RE,0(R2)            COPY INTERNET/NEW MEDIA TO FIELD             
         AR    R2,RE                                                            
         MVC   8(L'TAMDCODE,R2),TAMDCODE                                        
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
         LR    R3,R4               SAVE ELEMENT ADDRESS                         
*                                                                               
         GOTO1 RECVAL,DMCB,TLMDCDQ,(X'AC',TAMDCODE),(R2)                        
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
         LR    R4,R3               RESTORE ELEMENT ADDRESS                      
         MVI   ELCODE,TAMDELQ      AND ELEMENT CODE                             
         J     DMED30                                                           
*                                                                               
DMED50   MVC   AIO,AIO1            RESTORE AIO TO INVOICE                       
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS THE NUMBER OF USES PAID                         
         SPACE                                                                  
         USING TANDD,R4                                                         
DISUSE   NTR1  BASE=*,LABEL=*                                                   
         XC    CLAUSE,CLAUSE                                                    
         MVI   ELCODE,TANDELQ      GET NETWORK/CLASS A DETAILS ELEMENT          
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   DUX                                                              
         SR    R2,R2                                                            
         ICM   R2,3,TANDSTUS       FIND TOTAL START USE NUMBER                  
         BNZ   *+8                                                              
         AHI   R2,1                + 1 IF 0 FROM CONVERSION                     
         SPACE                                                                  
         SR    R1,R1                                                            
         ICM   R1,3,TANDSTUL                                                    
         BNZ   *+8                                                              
         AHI   R1,1                                                             
         SPACE                                                                  
         AR    R2,R1                                                            
         BCTR  R2,0                R2=TOTAL START USE NUMBER                    
         LA    R1,CLAUSE                                                        
         EDIT  (R2),(5,(R1)),ALIGN=LEFT                                         
         LA    RF,CLAUSE+4         FIND END OF START USE NUMBER                 
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         AH    R2,TANDUSES         FIND TOTAL END USE NUMBER                    
         AH    R2,TANDUSEL                                                      
         BCTR  R2,0                                                             
         EDIT  (R2),(5,2(RF)),ALIGN=LEFT                                        
         SPACE                                                                  
         OC    TANDUSEL,TANDUSEL   IF THERE ARE LIFT USES                       
         BZ    DU10                                                             
         MVI   CLAUSE+10,C'L'      SHOW LIFT USES                               
         LA    R1,CLAUSE+11                                                     
         EDIT  (2,TANDSTUL),(5,(R1)),ALIGN=LEFT  LIFT START USE NUMBER          
         LA    RF,CLAUSE+11+4      FIND END OF LIFT START USE NUMBER            
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         LH    R2,TANDSTUL         FIND LIFT END USE NUMBER                     
         AH    R2,TANDUSEL                                                      
         BCTR  R2,0                                                             
         EDIT  (R2),(5,2(RF)),ALIGN=LEFT                                        
         SPACE                                                                  
DU10     GOTO1 SQUASHER,DMCB,CLAUSE,L'CLAUSE  SQUASH IT                         
DUX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY ISU DETAILS                                         
***********************************************************************         
                                                                                
         USING TLUHD,R4                                                         
DISISU   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,KEY              GET USAGE HISTORY RECORD                     
         XC    TLUHKEY,TLUHKEY                                                  
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM                                                    
         MVC   TLUHUSE,TGUSCDE                                                  
         MVC   TLUHINV,TGINV                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLUHKEY),KEYSAVE                                           
         JNE   XIT                                                              
         GOTO1 GETREC                                                           
         DROP  R4                                                               
                                                                                
         USING TAUHD,R4                                                         
         L     R4,AIO              GET USAGE HISTORY ELEMENT                    
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
                                                                                
         GOTO1 USEVAL,DMCB,TGUSCDE,TAUHTYPE                                     
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   ISUTYPE,TGUSNAME                                                 
                                                                                
         MVI   ISUBCA,C' '                                                      
         TM    TAUHISU1,TAIDTBCA                                                
         JZ    *+8                                                              
         MVI   ISUBCA,C'X'                                                      
                                                                                
         MVI   ISUNNW,C' '                                                      
         TM    TAUHISU1,TAIDTNNW                                                
         JZ    *+8                                                              
         MVI   ISUNNW,C'X'                                                      
                                                                                
         MVI   ISUTEX,C' '                                                      
         TM    TAUHISU1,TAIDTTEX                                                
         JZ    *+8                                                              
         MVI   ISUTEX,C'X'                                                      
                                                                                
         MVI   ISUFGR,C' '                                                      
         TM    TAUHISU1,TAIDTFGR                                                
         JZ    *+8                                                              
         MVI   ISUFGR,C'X'                                                      
                                                                                
         MVI   ISUIN5,C' '                                                      
         TM    TAUHISU1,TAIDTIN5                                                
         JZ    *+8                                                              
         MVI   ISUIN5,C'X'                                                      
                                                                                
         MVI   ISUICU,C' '                                                      
         TM    TAUHISU1,TAIDTICU                                                
         JZ    *+8                                                              
         MVI   ISUICU,C'X'                                                      
                                                                                
         MVI   ISUSRI,C' '                                                      
         TM    TAUHISU1,TAIDTSRI                                                
         JZ    *+8                                                              
         MVI   ISUSRI,C'X'                                                      
                                                                                
         MVI   ISUPAG,C' '                                                      
         TM    TAUHISU1,TAIDTPAG                                                
         JZ    *+8                                                              
         MVI   ISUPAG,C'X'                                                      
                                                                                
         MVI   ISUCT2,C' '                                                      
         TM    TAUHISU2,TAIDTCT2                                                
         JZ    *+8                                                              
         MVI   ISUCT2,C'X'                                                      
                                                                                
         MVI   ISUNWK,C' '                                                      
         TM    TAUHISU2,TAIDTNWK                                                
         JZ    *+8                                                              
         MVI   ISUNWK,C'X'                                                      
                                                                                
         MVI   ISUCAB,C' '                                                      
         TM    TAUHISU2,TAIDTCAB                                                
         JZ    *+8                                                              
         MVI   ISUCAB,C'X'                                                      
                                                                                
         MVI   ISUGPU,C' '                                                      
         TM    TAUHISU2,TAIDTGPU                                                
         JZ    *+8                                                              
         MVI   ISUGPU,C'X'                                                      
                                                                                
         MVI   ISUGOV,C' '                                                      
         TM    TAUHISU2,TAIDTGOV                                                
         JZ    *+8                                                              
         MVI   ISUGOV,C'X'                                                      
         DROP  R4                                                               
                                                                                
         GOTO1 FLDVAL,DMCB,(2,ISUTYPEH),ISUGOVH                                 
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY VRE/VNR VERSION DETAILS                   *         
***********************************************************************         
                                                                                
DISVER   NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVRE))                                     
         JE    DV00                                                             
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVNR))                                     
         JNE   XIT                                                              
                                                                                
DV00     XC    VERVARS(VVLNQ),VERVARS                                           
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         ZIC   R3,TAFNLEN                                                       
         SHI   R3,TAFNLNQ                                                       
         LA    R4,TAFNNAME                                                      
         DROP  R4                                                               
                                                                                
         LA    R5,VREVAR                                                        
         LA    R6,L'VREVAR-8(R5)                                                
         J     DV40                                                             
                                                                                
DV10     CR    R5,R6             IF NO MORE ROOM TO DISPLAY NEXT                
         JL    DV20              VERSION, INDICATE THERE ARE MORE               
         MVI   0(R5),C'+'        VERSIONS                                       
         J     XIT                                                              
                                                                                
DV20     CLC   RANGEX,0(R4)                                                     
         JE    DV50                                                             
                                                                                
         CLC   RANGEC,LASTDISP                                                  
         JE    DV30                                                             
                                                                                
         MVI   0(R5),C'-'                                                       
         AHI   R5,1                                                             
         EDIT  RANGEC,(3,0(R5)),ALIGN=LEFT                                      
         AR    R5,R0                                                            
                                                                                
DV30     MVI   0(R5),C','                                                       
         AHI   R5,1                                                             
                                                                                
DV40     EDIT  (1,0(R4)),(3,0(R5)),ALIGN=LEFT                                   
         MVC   LASTDISP,0(R4)                                                   
         AR    R5,R0                                                            
                                                                                
DV50     MVC   RANGEC,0(R4)                                                     
         ZIC   RE,0(R4)                                                         
         AHI   RE,1                                                             
         STC   RE,RANGEX                                                        
                                                                                
         LA    R4,1(R4)                                                         
         BCT   R3,DV10                                                          
                                                                                
         CLC   LASTDISP,RANGEC                                                  
         JE    XIT                                                              
         MVI   0(R5),C'-'                                                       
         EDIT  RANGEC,(3,1(R5)),ALIGN=LEFT                                      
         J     XIT                                                              
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO HANDLE VNR/LIST SCREEN                            *         
***********************************************************************         
                                                                                
VNRLIST  NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'VNR',CONREC                                                   
         JNE   VLRCACT                                                          
                                                                                
         BRAS  RE,VLINIT                                                        
                                                                                
         CLI   MODE,VALKEY                                                      
         JNE   *+12                                                             
         BRAS  RE,VLVK                                                          
         BRAS  RE,VLVR                                                          
                                                                                
         CLI   MODE,LISTRECS                                                    
         JNE   XIT                                                              
         BRAS  RE,VLLR                                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR MESSAGES                                               *         
***********************************************************************         
                                                                                
VLRCACT  LA    R2,CONACTH                                                       
         MVI   ERROR,INVRCACT                                                   
         B     VLEND                                                            
                                                                                
VLEND    GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE PROGRAM                                *         
***********************************************************************         
                                                                                
VLINIT   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 INITIAL,DMCB,0                                                   
                                                                                
         CLI   TGCTSTTY,TASTTYPP                                                
         JE    XIT                                                              
         CLI   TGCTSTTY,TASTTYP2                                                
         JE    XIT                                                              
         CLI   TGCTSTTY,TASTTYP3                                                
         JE    XIT                                                              
         CLI   TGCTSTTY,TASTTYP4                                                
         JE    XIT                                                              
         CLI   TGCTSTTY,TASTTYP5                                                
         JE    XIT                                                              
         CLI   TGCTSTTY,TASTTYP6                                                
         JE    XIT                                                              
         GOTO1 FLDVAL,DMCB,(X'0A',VNRAVHDH),(8,VNRDVERH)                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VLINIT ROUTINES                   *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VLVK     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(X'40',VNRAGYH),(X'80',VNRCIDH)                      
         JE    XIT                                                              
                                                                                
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
         MVC   TIUSERID,TWAORIG    INITIALIZE SYSIO VARIABLES                   
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLCACDQ                                                   
         OI    TIQFLAG2,TIQFNLIM                                                
                                                                                
         LA    R2,VNRAGYH                                                       
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',VNRAGYH)                              
         MVC   TIFAGY,TGAGY                                                     
                                                                                
         GOTO1 RECVAL,DMCB,TLCOICDQ,(8,VNRCIDH),VNRCIDNH                        
                                                                                
         USING TLCOPD,R3                                                        
         LA    R3,KEY                                                           
         MVC   TGCOM,TLCOICOM      SAVE INTERNAL COMMERCIAL NUMBER              
         MVC   TIFCOM,TLCOICOM     AND SET SYSIO FILTER                         
         DROP  R3                                                               
                                                                                
         BAS   RE,SVCOMVER         SAVE COMMERCIAL'S 26K VERSIONS               
                                                                                
         CLI   COMVERS,0           COMMERCIAL MUST HAVE AT LEAST                
         JE    VLVKCINV            1 26K VERSION                                
                                                                                
         GOTO1 FLDVAL,DMCB,(X'20',VNRAGYH),(X'80',VNRCIDH)                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE SAVES COMMERCIALS 26K VERSIONS                       *         
***********************************************************************         
                                                                                
SVCOMVER NTR1                                                                   
         LA    R2,COMVERS                                                       
                                                                                
         USING TLVRD,R3                                                         
         LA    R3,KEY                                                           
         XC    TLVRKEY,TLVRKEY   READ ALL VERSION KEYS ATTACHED TO              
         MVI   TLVRCD,TLVRCDQ    THE COMMERCIAL                                 
         MVC   TLVRCOM,TIFCOM                                                   
         GOTO1 HIGH                                                             
         J     BCOT20                                                           
BCOT10   GOTO1 SEQ                                                              
BCOT20   CLC   KEY(TLVRVER-TLVRD),KEYSAVE                                       
         JNE   BCOT30                                                           
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TACOSTA3,TACOS26K                                                
         JZ    BCOT10                                                           
         MVC   0(1,R2),TLVRVER   SAVE VERSION NUMBER                            
         LA    R2,1(R2)                                                         
         J     BCOT10                                                           
         DROP  R3                                                               
                                                                                
BCOT30   MVI   0(R2),0           MARK END OF 26K VERSIONS                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR MESSAGES FOR VLVK ROUTINES                             *         
***********************************************************************         
                                                                                
VLVKCINV LA    R2,VNRCIDH                                                       
         J     VLVKINV                                                          
                                                                                
VLVKINV  MVI   ERROR,INVALID                                                    
         B     VLVKEND                                                          
                                                                                
VLVKEND  GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VKEY ROUTINES                     *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE SCREEN                                   *         
***********************************************************************         
                                                                                
VLVR     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(X'80',VNRSEL1H),(X'80',VNRLSTH)                     
         JNE   VLVR10              IF SELECT FIELDS ARE EMPTY                   
         XC    FRSTKEY,FRSTKEY     SET TO CONTINUE LISTING                      
                                                                                
VLVR10   GOTO1 FLDVAL,DMCB,(X'80',VNRAVERH),(X'80',VNRDVERH)                    
         JNE   VLVR40                                                           
         LA    R2,VNRSEL1H         IF ADD/DELETE FIELDS DO NOT CONTAIN          
         LA    R3,VNRLSTH          INPUT, ENSURE NO SELECT FIELDS               
VLVR20   CR    R2,R3               CONTAIN "C"                                  
         JE    XIT                                                              
         TM    1(R2),X'20'                                                      
         JO    VLVR30                                                           
         CLI   5(R2),0                                                          
         JH    VLVRINV                                                          
VLVR30   ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         J     VLVR20                                                           
                                                                                
VLVR40   CLI   VNRAVERH+5,0        ENSURE THAT ADD AND DELETE FIELDS            
         JE    VLVR50              ARE NOT BOTH POPULATED                       
         CLI   VNRDVERH+5,0                                                     
         JNE   VLVRDINV                                                         
                                                                                
VLVR50   LA    R2,VNRAVERH         R2=A(POPULATED FIELD)                        
         CLI   VNRAVERH+5,0                                                     
         JNE   *+8                                                              
         LA    R2,VNRDVERH                                                      
         BAS   RE,SVINPVER         SAVE INPUTTED VERSIONS                       
                                                                                
         LA    R2,VNRSEL1H         IF AT LEAST ONE CAST MEMBER                  
         LA    R5,VNRLSTH          HAS BEEN SELECTED ...                        
         GOTO1 FLDVAL,DMCB,(X'80',(R2)),(X'80',(R5))                            
         JE    XIT                                                              
                                                                                
         LA    R3,KEY              R3=A(KEY)                                    
                                                                                
VLVR60   CR    R2,R5                                                            
         JH    XIT                                                              
         TM    1(R2),X'20'         BUMP THROUGH SELECT FIELDS                   
         JO    VLVR70              LOOKING FOR ONES WITH INPUT                  
         CLI   5(R2),0                                                          
         JE    VLVR70                                                           
         CLI   8(R2),C'C'          C IS ONLY ACCEPTABLE INPUT                   
         JNE   VLVRINV                                                          
         ST    R2,ASELFLD                                                       
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BAS   RE,PROCAST          PROCESS CAST RECORDS                         
VLVR70   ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         J     VLVR60                                                           
                                                                                
***********************************************************************         
*        ROUTINE SAVES INPUTTED 26K VERSIONS                          *         
*        ON ENTRY ... R2=A(VERSION INPUT FIELD)                       *         
***********************************************************************         
                                                                                
SVINPVER NTR1                                                                   
         MVI   INPVERS,X'FF'     INITIALIZE INPUT TABLE                         
         MVI   ELEMENT,X'FF'     AND WORK AREA                                  
                                                                                
         USING SCAND,R5                                                         
         LA    R5,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R5),0                                         
         CLI   4(R1),0                                                          
         JE    VLVRINV                                                          
         ZIC   R0,4(R1)                                                         
                                                                                
SIV10    CLC   SCDATA2,SPACES    ENTRY SHOULD NEVER INCLUDE AN                  
         JNE   VLVRINV           EQUALS SIGN                                    
                                                                                
         XC    RANGEH(L'RANGEH+L'RANGE),RANGEH                                  
                                                                                
         SR    R1,R1             USE R1 TO COUNT NUMBER OF DASHES               
         LA    RE,SCDATA1        IN ENTRY                                       
         LA    RF,L'SCDATA1                                                     
SIV20    CLI   0(RE),C'-'                                                       
         JNE   SIV30                                                            
         MVI   0(RE),C'='        REPLACE DASHES WITH EQUAL SIGNS                
         AHI   R1,1                                                             
SIV30    LA    RE,1(RE)                                                         
         BCT   RF,SIV20                                                         
                                                                                
         CHI   R1,0              NO DASHES MEANS ENTRY IS NOT A RANGE           
         JE    SIV50                                                            
         CHI   R1,1              MAXIMUM OF ONE DASH PERMITTED                  
         JNE   VLVRINV                                                          
                                                                                
         MVI   RANGEH,L'RANGEH+L'RANGE     IF ENTRY IS A RANGE                  
         MVC   RANGEH+5(1),SCLEN1          BUILD MOCK FIELD TO SCAN             
         MVC   RANGE,SCDATA1                                                    
         ST    R5,SVENTRY        AND SAVE A(CURR ENTRY IN SCAN BLOCK)           
         ST    R0,SVENCNT                                                       
                                                                                
         LA    R5,WORK                                                          
         GOTO1 SCANNER,DMCB,RANGEH,(R5),0                                       
         CLI   4(R1),0                                                          
         JE    VLVRINV                                                          
         ZIC   R0,4(R1)                                                         
                                                                                
         TM    SCVAL1,X'80'      FIRST ENTRY IN RANGE MUST BE NUMERIC           
         JZ    VLVRINV                                                          
         CLC   SCBIN1,=F'4'      BETWEEN 4                                      
         JL    VLVRINV                                                          
         CLC   SCBIN1,=F'249'    AND 249                                        
         JH    VLVRINV                                                          
                                                                                
         TM    SCVAL2,X'80'      SECOND ENTRY IN RANGE MUST BE NUMERIC          
         JZ    VLVRINV                                                          
         CLC   SCBIN2,=F'5'      BETWEEN 5                                      
         JL    VLVRINV                                                          
         CLC   SCBIN2,=F'250'    AND 250                                        
         JH    VLVRINV                                                          
                                                                                
         CLC   SCBIN1,SCBIN2     FIRST ENTRY MUST BE LESS THAN SECOND           
         JNL   VLVRINV                                                          
                                                                                
SIV40    CLC   SCBIN1,SCBIN2     PROCESS ALL NUMBERS IN RANGE UNTIL             
         JNH   SIV50             FIRST ENTRY EXCEEDS SECOND                     
         L     R5,SVENTRY                                                       
         L     R0,SVENCNT                                                       
         XC    RANGE,RANGE                                                      
         J     SIV120                                                           
                                                                                
SIV50    LA    R3,COMVERS        R3=A(26K VERSION TABLE)                        
                                                                                
         CLI   SCLEN1,3          IF FIELD CONTAINS A VERSION CODE               
         JH    VLVRINV           IT MUST BE 3 CHARACTERS OR LESS                
         TM    SCVAL1,X'80'      AND VALID NUMERIC                              
         JZ    VLVRINV                                                          
         MVC   ACTUAL,SCBIN1+3   SAVE CODE IN ACTUAL                            
SIV60    CLI   0(R3),X'FF'                                                      
         JE    SIV70                                                            
         CLC   ACTUAL,0(R3)                                                     
         JE    SIV80                                                            
         LA    R3,1(R3)                                                         
         J     SIV60                                                            
                                                                                
SIV70    OC    RANGE,RANGE       CODE MUST EXIST ON COMMERCIAL                  
         JZ    VLVRINV           UNLESS IT IS PART OF A RANGE                   
         J     SIV110                                                           
                                                                                
SIV80    LA    RE,ELEMENT                                                       
SIV90    CLI   0(RE),X'FF'       SAVE ACTUAL INTO WORK AREA'S FIRST             
         JE    SIV100            EMPTY SLOT                                     
         CLC   ACTUAL,0(RE)      DO NOT ALLOW VERSION TO BE DUPLICATED          
         JE    VLVRINV                                                          
         LA    RE,1(RE)                                                         
         J     SIV90                                                            
SIV100   MVC   0(1,RE),ACTUAL                                                   
         MVI   1(RE),X'FF'                                                      
                                                                                
         OC    RANGE,RANGE       IF VERSION IS PART OF RANGE                    
         JZ    SIV120                                                           
SIV110   ZIC   RE,SCBIN1+3       BUMP TO NEXT NUMBER IN RANGE                   
         AHI   RE,1              AND GO PROCESS                                 
         STC   RE,SCBIN1+3                                                      
         J     SIV40                                                            
                                                                                
SIV120   LA    R5,SCANNEXT       ELSE, BUMP TO NEXT ENTRY IN SCANNER            
         BCT   R0,SIV10          BLOCK                                          
         DROP  R5                                                               
                                                                                
         LA    R1,INPVERS                                                       
                                                                                
SIV130   LA    RE,ELEMENT                                                       
SIV140   CLI   0(RE),X'FF'       FIND FIRST MEANINGFUL VALUE IN                 
         JE    SIV180            WORK AREA                                      
         CLI   0(RE),0                                                          
         JNE   SIV150                                                           
         LA    RE,1(RE)                                                         
         J     SIV140                                                           
                                                                                
SIV150   LR    RF,RE                                                            
SIV160   LA    RF,1(RF)          COMPARE IT TO NEXT MEANINGFUL VALUE            
         CLI   0(RF),X'FF'       IN WORK AREA                                   
         JE    SIV170                                                           
         CLI   0(RF),0                                                          
         JE    SIV160                                                           
         CLC   0(1,RE),0(RF)                                                    
         JL    SIV160                                                           
         LR    RE,RF                                                            
         J     SIV160                                                           
                                                                                
SIV170   AHI   R0,1              ADD VERSIONS TO INPUT TABLE FROM               
         MVC   0(1,R1),0(RE)     SMALLEST TO LARGEST                            
         LA    R1,1(R1)                                                         
         MVI   0(RE),0                                                          
         J     SIV130                                                           
                                                                                
SIV180   MVI   0(R1),0           FINALIZE INPVERS                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE PROCESSES SELECTED CAST RECORD                       *         
*        ON ENTRY ... R2=A(SELECT FIELD)                              *         
*                     R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
PROCAST  NTR1                                                                   
         USING LISTD,R2                                                         
         AHI   R2,8                R2=A(LIST LINE)                              
                                                                                
         OC    LISSORT,LISSORT                                                  
         JZ    XIT                                                              
                                                                                
         USING TLCAD,R3                                                         
         XC    TLCAKEY,TLCAKEY                                                  
         MVI   TLCACD,TLCACDQ      READ CAST RECORD                             
         MVC   TLCACOM,TIFCOM                                                   
         GOTO1 HEXIN,DMCB,LISSORT+1,TLCASORT,L'LISSORT-1                        
         GOTO1 HIGH                                                             
         CLC   KEY(TLCASSN-TLCAD),KEYSAVE                                       
         JE    *+6                                                              
         DC    H'00'                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R2,R3                                                            
                                                                                
         L     R0,AIO1                                                          
         LHI   R1,4000             COPY RECORD INTO AIO2                        
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
*        SET CAST'S OCSTVERS WITH CAST'S ORIGINAL NON-RENDERED                  
*        VERSIONS. SET COUNT OF THE VERSIONS IN OCVCNT.                         
         GOTO1 SAVVERS,DMCB,('TAFNTVNR',OCSTVERS),OCVCNT                        
                                                                                
*        SET CAST'S OMANVERS WITH CAST'S ORIGINAL MANUAL VERSIONS               
*        VERSIONS. SET COUNT OF THE VERSIONS IN OMVCNT.                         
         GOTO1 (RF),(R1),('TAFNTVNM',OMANVERS),OMVCNT                           
                                                                                
*        UPDATE CAST'S NON-RENDERED VERSIONS                                    
         GOTO1 PROVERS,DMCB,('TAFNTVNR',OCSTVERS)                               
                                                                                
*        UPDATE CAST'S MANUALLY ADDED VERSIONS                                  
         GOTO1 (RF),(R1),('TAFNTVNM',OMANVERS)                                  
                                                                                
         L     R0,AIO1                                                          
         LHI   R1,4000             IF RECORD HAS NOT CHANGED                    
         L     RE,AIO2             RETURN ERROR                                 
         LR    RF,R1                                                            
         CLCL  RE,R0                                                            
         JE    VLVRSNC                                                          
                                                                                
         GOTO1 PUTREC              UPDATE CAST RECORD                           
                                                                                
         L     R2,ASELFLD                                                       
         MVI   5(R2),0                                                          
         XC    8(L'VNRSEL1,R2),8(R2)                                            
         GOTO1 FLDVAL,DMCB,(2,(R2)),2                                           
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         LA    R2,8(R2)                                                         
         BRAS  RE,DISLINE                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE SAVES VERSIONS FROM SPECIFIED ELEMENT INTO           *         
*        PROVIDED AREA                                                *         
*        ON ENTRY ... P1 BYTE 0 = TAFNTYPE FOR VERSION ELEMENT        *         
*                     P1        = AREA TO SAVE VERSIONS               *         
*                     P2        = AREA TO SAVE # OF VERSIONS          *         
***********************************************************************         
                                                                                
SAVVERS  NTR1                                                                   
         MVC   FNTYPE,0(R1)                                                     
                                                                                
         ZICM  R2,1(R1),3        R2=A(AREA TO SAVE VERSIONS)                    
         XC    0(L'OCSTVERS,R2),0(R2)                                           
                                                                                
         ZICM  R3,5(R1),3        R3=A(AREA TO SAVE # OF VERSIONS)               
         MVI   0(R3),0                                                          
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,FNTYPE)                                             
         JNE   XIT                                                              
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM         COPY VERSIONS INTO AREA                        
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,TAFNLNQ+1                                                     
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),TAFNNAME                                                 
         LA    RF,1(R2)                                                         
         AR    RF,RE                                                            
         MVI   0(RF),0                                                          
                                                                                
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,3                                                             
         STC   RE,0(R3)                                                         
         DROP  R4                                                               
                                                                                
SV20     MVI   0(R4),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ADDS/DELETES INPUTTED VERSIONS TO/FROM CAST RECORD   *         
*        ON ENTRY ... P1 BYTE 0 = TAFNTYPE FOR VERSION ELEMENT        *         
*                     P1        = A(ORIGINAL VERSIONS)                *         
***********************************************************************         
                                                                                
PROVERS  NTR1                                                                   
         ZICM  R2,1(R1),3        R2=A(ORIGINAL VERSIONS)                        
         L     RE,4(R1)                                                         
                                                                                
         USING TAFND,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT   INITIALIZE NON-RENDERED VERSIONS               
         MVI   TAFNEL,TAFNELQ    ELEMENT                                        
         MVI   TAFNLEN,TAFNLNQ                                                  
         MVC   TAFNTYPE,0(R1)                                                   
                                                                                
         BAS   RE,ADDVERS        ATTEMPT TO ADD                                 
         BAS   RE,DELVERS        OR DELETE NON-RENDERED VERSIONS                
                                                                                
         CLI   TAFNLEN,TAFNLNQ   IF ELEMENT HAS ANY VERSIONS IT IT              
         JE    XIT                                                              
         GOTO1 ADDELEM           ADD IT NOW                                     
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE ADDS INPUTTED VERSIONS TO ORIGINAL VERSIONS          *         
*        ON ENTRY ... R2        = ORIGINAL NON-RENDERED VERSIONS      *         
*                     R4        = A(IN PROGRESS NON-REND VERS ELEM)   *         
***********************************************************************         
                                                                                
         USING TAFND,R4                                                         
ADDVERS  NTR1                                                                   
         CLI   VNRAVERH+5,0      EXIT IF NOT ADDING VERSIONS                    
         JE    XIT                                                              
                                                                                
         CLI   TAFNTYPE,TAFNTVNR                                                
         JNE   AVER10                                                           
         CLI   OCVCNT,2          CANNOT ADD A PERFORMER'S 1ST OR 2ND            
         JL    VLVRSINV          NON-RENDERED VERSION MANUALLY                  
                                                                                
AVER10   LA    R5,TAFNNAME                                                      
                                                                                
         LA    R3,INPVERS                                                       
AVER20   CLI   0(R3),0           R3=A(INPUTTED VERSIONS)                        
         JE    AVER50                                                           
                                                                                
         BAS   RE,ONVER          IF PERFORMER IS NOT ON THIS VERSION            
         JE    AVER30            SKIP IT                                        
         LA    R3,1(R3)                                                         
         J     AVER20                                                           
                                                                                
AVER30   BAS   RE,ISREND         IF VERSION TO ADD IS ALREADY                   
         JNE   AVER40            RENDERED FOR THIS PERFORMER, SKIP IT           
         LA    R3,1(R3)                                                         
         J     AVER20                                                           
                                                                                
AVER40   BAS   RE,ISNREND        IF VERSION TO ADD IS ALREADY                   
         JNE   AVER50            NON-RENDERED FOR THIS PERFORMER,               
         LA    R3,1(R3)          SKIP IT                                        
         J     AVER20                                                           
                                                                                
AVER50   CLI   0(R2),0           IF WE'VE REACHED END OF ORIGINAL               
         JE    AVER70            VERSIONS, GO ADD INPUTTED VERSION              
                                                                                
         CLI   0(R3),0                                                          
         JE    AVER60                                                           
         CLC   0(1,R3),0(R2)     IF INPUTTED VERSION IS SAME OR HIGHER          
         JL    AVER70            THAN ORIGINAL VERSION, ADD ORIGINAL            
AVER60   MVC   0(1,R5),0(R2)     VERSION                                        
         LA    R2,1(R2)          BUMP PAST ORIGINAL VERSION                     
         CLC   0(1,R5),0(R3)     IF THEY'RE THE SAME, BUMP PAST                 
         JNE   AVER80            INPUTTED VERSION AS WELL                       
         LA    R3,1(R3)                                                         
         J     AVER80                                                           
                                                                                
AVER70   CLI   0(R3),0           IF WE'VE REACHED THE END OF CURRENT            
         JE    AVER90            VERSIONS, GO ADD ELEMENT                       
                                                                                
*                                IF INPUTTED VERSION IS LOWER THAN              
         MVC   0(1,R5),0(R3)     ORIGINAL VERSION, ADD INPUTTED VERS            
         LA    R3,1(R3)          BUMP PAST INPUTTED VERSION                     
                                                                                
AVER80   LA    R5,1(R5)          BUMP TO NEXT POSITION IN ELEMENT               
         ZIC   R0,TAFNLEN        AND BUMP UP ELEMENT LENGTH                     
         AHI   R0,1                                                             
         STC   R0,TAFNLEN                                                       
         J     AVER20                                                           
                                                                                
AVER90   CLI   TAFNTYPE,TAFNTVNR                                                
         JNE   XIT               IF ADDING NON-RENDERED VERSIONS                
         CLI   TAFNLEN,TAFNLNQ                                                  
         JE    XIT                                                              
                                                                                
         ZIC   RF,OCVCNT         RF=ORIGINAL # OF VERSIONS                      
         BCTR  RF,0              (NOT COUNTING THE 1ST VERSION)                 
                                                                                
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,TAFNLNQ        RE=UPDATED # OF VERSIONS                       
         BCTR  RE,0              (NOT COUNTING THE 1ST VERSION)                 
         DROP  R4                                                               
                                                                                
         XR    R4,R4                                                            
         LR    R5,RF                                                            
         D     R4,=F'4'                                                         
         CHI   R4,0                                                             
         JE    VLVRSINV                                                         
         LHI   R5,4                                                             
         SR    R5,R4             R5=#MAX VERSIONS TO ADD                        
                                                                                
         SR    RE,RF                                                            
         CR    R5,RE             IF UPDATED # OF VERSIONS EXCEEDS               
         JL    VLVRSINV          THE MAX TO ADD, RETURN ERROR                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE RETURNS POSITIVE CONDITION CODE IF PERFORMER IS      *         
*        ON THIS VERSION                                              *         
*        ON ENTRY ...  0(R3)    = VERSION TO CHECK                    *         
***********************************************************************         
                                                                                
ONVER    NTR1                                                                   
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         JNE   NO                                                               
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         CLI   TAFNNAME,251                                                     
         JE    YES                                                              
                                                                                
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,TAFNLNQ                                                       
         LA    R4,TAFNNAME                                                      
         DROP  R4                                                               
                                                                                
OV10     CLC   0(1,R3),0(R4)                                                    
         JE    YES                                                              
         LA    R4,1(R4)                                                         
         BCT   RE,OV10                                                          
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE RETURNS POSITIVE CONDITION CODE IF PERFORMER IS      *         
*        RENDERED FOR THIS VERSION                                    *         
*        ON ENTRY ...  0(R3)    = VERSION TO CHECK                    *         
***********************************************************************         
                                                                                
ISREND   NTR1                                                                   
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVRE))                                     
         JNE   NO                                                               
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,TAFNLNQ                                                       
         LA    R4,TAFNNAME                                                      
         DROP  R4                                                               
                                                                                
IR10     CLC   0(1,R3),0(R4)                                                    
         JE    YES                                                              
         LA    R4,1(R4)                                                         
         BCT   RE,IR10                                                          
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE RETURNS POSITIVE CONDITION CODE IF PERFORMER IS      *         
*        NON-RENDERED FOR THIS VERSION                                *         
*        ON ENTRY ... 0(R3)     = VERSION TO CHECK                    *         
***********************************************************************         
                                                                                
ISNREND  NTR1                                                                   
         LA    RE,OCSTVERS                                                      
INR10    CLI   0(RE),0                                                          
         JE    NO                                                               
         CLC   0(1,R3),0(RE)                                                    
         JE    YES                                                              
         LA    RE,1(RE)                                                         
         J     INR10                                                            
                                                                                
***********************************************************************         
*        ROUTINE DELETES INPUTTED VERSIONS FROM ORIGINAL VERSIONS     *         
*        ON ENTRY ... R2        = ORIGINAL NON-RENDERED VERSIONS      *         
*                     R4        = A(IN PROGRESS NON-REND VERS ELEM)   *         
***********************************************************************         
                                                                                
         USING TAFND,R4                                                         
DELVERS  NTR1                                                                   
         CLI   VNRDVERH+5,0      EXIT IF NOT DELETING VERSIONS                  
         JE    XIT                                                              
                                                                                
******** CLI   OCVCNT,3          PERFORMER MUST BE ON AT LEAST 3                
******** JL    VLVRSINV          NON-RENDERED VERSIONS                          
                                                                                
         LA    R5,TAFNNAME                                                      
                                                                                
DVER10   LA    R3,INPVERS                                                       
DVER20   CLI   0(R3),0           R3=LIST OF VERSIONS TO DELETE                  
         JE    DVER40                                                           
                                                                                
         CLI   TAFNTYPE,TAFNTVNR                                                
         JNE   DVER30                                                           
         BAS   RE,ISMAN          IF VERSION WAS NOT MANUALLY ADDED,             
         JE    DVER30            SKIP IT                                        
         LA    R3,1(R3)                                                         
         J     DVER20                                                           
                                                                                
DVER30   CLC   0(1,R3),0(R2)                                                    
         JE    DVER50                                                           
         LA    R3,1(R3)                                                         
         J     DVER20                                                           
DVER40   MVC   0(1,R5),0(R2)                                                    
         LA    R5,1(R5)                                                         
         ZIC   RE,TAFNLEN                                                       
         AHI   RE,1                                                             
         STC   RE,TAFNLEN                                                       
DVER50   CLI   1(R2),0                                                          
         JE    DVER60                                                           
         LA    R2,1(R2)                                                         
         J     DVER10                                                           
                                                                                
DVER60   CLI   TAFNTYPE,TAFNTVNR                                                
         JNE   XIT                                                              
                                                                                
*&&DO                                                                           
         ZIC   RF,OCSTVERS       RF=ORIGINAL # OF VERSIONS                      
         SHI   RF,1              (NOT COUNTING THE 1ST VERSION)                 
                                                                                
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,TAFNLNQ        RE=UPDATED # OF VERSIONS                       
         SHI   RE,1              (NOT COUNTING THE 1ST VERSION)                 
                                                                                
         XR    R4,R4                                                            
         LR    R5,RF                                                            
         D     R4,=F'4'                                                         
         LTR   R4,R4                                                            
         JNZ   DVER70                                                           
         LHI   R4,3                                                             
         J     DVER80                                                           
DVER70   SHI   R4,1              R4=MAX # OF VERSIONS TO DELETE                 
                                                                                
DVER80   SR    RF,RE                                                            
         CR    RF,R4             IF UPDATED # OF VERSIONS EXCEED                
         JH    VLVRSINV          THE MAX TO ADD, RETURN ERROR                   
*&&                                                                             
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE RETURNS POSITIVE CONDITION CODE IF PERFORMER WAS     *         
*        MANUALLY NON-RENDERED FOR THIS VERSION                       *         
*        ON ENTRY ... 0(R3)     = VERSION TO CHECK                    *         
***********************************************************************         
                                                                                
ISMAN    NTR1                                                                   
         LA    RE,OMANVERS                                                      
IM10     CLI   0(RE),0                                                          
         JE    NO                                                               
         CLC   0(1,R3),0(RE)                                                    
         JE    YES                                                              
         LA    RE,1(RE)                                                         
         J     IM10                                                             
                                                                                
***********************************************************************         
*        ERROR MESSAGES FOR VLVR ROUTINES                             *         
***********************************************************************         
                                                                                
VLVRDINV LA    R2,VNRDVERH                                                      
         J     VLVRINV                                                          
                                                                                
VLVRSINV L     R2,ASELFLD                                                       
         J     VLVRINV                                                          
                                                                                
VLVRINV  MVI   ERROR,INVALID                                                    
         B     VLVREND                                                          
                                                                                
VLVRSNC  L     R2,ASELFLD                                                       
         LHI   RE,ERRRECNC                                                      
         STH   RE,MYMSGNO                                                       
         J     VLVREEND                                                         
                                                                                
VLVREEND MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         J     VLVREND                                                          
                                                                                
VLVREND  GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VLVR ROUTINES                     *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO LIST RECORDS                                      *         
***********************************************************************         
                                                                                
VLLR     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(1,VNRCST1H),(X'40',VNRLSTH)                         
         OI    GLSTSTAT,RETEXTRA                                                
                                                                                
         LA    R2,LISTAR           SETUP SCREEN INFORMATION                     
         MVI   NLISTS,8                                                         
                                                                                
         XC    NAMEH(L'NAMEH+L'NAME),NAMEH                                      
         MVI   NAMEH,L'NAMEH+L'NAME                                             
         XC    NAMEX,NAMEX                                                      
                                                                                
         OC    TIKEY,TIKEY                                                      
         JZ    LR10                                                             
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
         MVC   TIQSKEY,KEY                                                      
                                                                                
LR10     OC    FRSTKEY,FRSTKEY                                                  
         JZ    LR20                                                             
         MVC   TIQSKEY,FRSTKEY                                                  
                                                                                
LR20     LA    R0,LRHOOK           SETUP SYSIO AND CALL IT                      
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
                                                                                
         NI    VNRAGYH+4,X'DF'                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO PROCESS SYSIO RECORDS                             *         
*        ON ENTRY ... R2=A(LIST LINE)                                 *         
***********************************************************************         
                                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC                                                   
         JNE   XIT                                                              
                                                                                
         MVC   AIO,TIAREC                                                       
                                                                                
         BAS   RE,FILTREC                                                       
         JNE   LH20                                                             
                                                                                
         XC    CSTVARS(CVLNQ),CSTVARS                                           
                                                                                
         USING LISTD,R2                                                         
         MVC   LISTAR,SPACES                                                    
         GOTO1 SSNPACK,DMCB,TISSN,LISPID                                        
         BRAS  RE,DISLINE                                                       
         DROP  R2                                                               
                                                                                
         GOTO1 LISTMON                                                          
                                                                                
         USING LISLINE2,R2                                                      
         L     R2,ATHISLST                                                      
                                                                                
         USING TLCAD,R4                                                         
         L     R4,TIAREC                                                        
         GOTO1 HEXOUT,DMCB,TLCASORT,LISSORT+1,6,0                               
         MVC   LISCAT,TICAT                                                     
         DROP  R4                                                               
                                                                                
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',TISSN),NAMEH                          
         MVC   LISNAME,NAME                                                     
                                                                                
         LA    R2,LIS2LNQ(R2)                                                   
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ST    R2,ATHISLST                                                      
         DROP  R2                                                               
                                                                                
         CLI   LISTNUM,1                                                        
         JNE   LH20                                                             
         MVC   FRSTKEY,TIKEY                                                    
                                                                                
LH20     MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO FILTER SYSIO RECORDS                              *         
*        ON ENTRY ... AIO=A(CAST RECORD)                              *         
***********************************************************************         
                                                                                
FILTREC  NTR1                                                                   
         USING TLCAD,R4                                                         
         L     R4,AIO                                                           
         GOTO1 CATVAL,DMCB,TLCACAT                                              
         TM    TGCATYPE,EXTRA                                                   
         JO    YES                                                              
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAYEAR,=C'13 '                                                 
         JNL   YES                                                              
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR LISTREC ROUTINES                  *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DISPLAYS VNR/LIST LINE                               *         
*        ON ENTRY ... R2 = A(LIST LINE)                               *         
***********************************************************************         
                                                                                
         USING LISTD,R2                                                         
DISLINE  NTR1  BASE=*,LABEL=*                                                   
         MVI   LIS1VR,C'N'                                                      
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVN1))                                     
         JNE   *+8                                                              
         MVI   LIS1VR,C'Y'                                                      
                                                                                
         MVC   LISVPD,=CL2' 0'                                                  
         MVI   LISVRE,C'0'                                                      
                                                                                
         USING TAFND,R1                                                         
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVNR))                                     
         JNE   XIT                                                              
         L     R1,TGELEM                                                        
                                                                                
         ZIC   R5,TAFNLEN                                                       
         SHI   R5,3                                                             
         CLI   LIS1VR,C'Y'                                                      
         JNE   *+8                                                              
         SHI   R5,1                                                             
         EDIT  (R5),LISVPD,ZERO=NOBLANK                                         
         DROP  R1                                                               
                                                                                
         CHI   R5,0                                                             
         JNH   XIT                                                              
                                                                                
         XR    R4,R4                                                            
         D     R4,=F'4'                                                         
         CHI   R4,0                                                             
         JZ    *+8                                                              
         AHI   R5,1                                                             
                                                                                
         CHI   R4,0                                                             
         JE    XIT                                                              
         LHI   R5,4                                                             
         SR    R5,R4                                                            
         EDIT  (R5),LISVRE,ZERO=NOBLANK                                         
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR LISLINE                           *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER CLASS A SCREEN USE ENTRIES                        
         SPACE 1                                                                
CLASCRND DSECT                                                                  
         DS    CL8                                                              
         DS    CL1                 ENTRY INDICATOR                              
CLASDTEH DS    CL8                                                              
CLASDTE  DS    CL8                 USE DATE                                     
CLASPRGH DS    CL8                                                              
CLASPRG  DS    CL15                PROGRAM NAME                                 
CLASLFTH DS    CL8                                                              
CLASLFT  DS    CL1                 LIFT                                         
CLASNWKH DS    CL8                                                              
CLASNWK  DS    CL1                 NETWORK                                      
CLASNEXT EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE FOR VNR/LIST                                 *         
***********************************************************************         
                                                                                
WORKD    DSECT                                                                  
FRSTKEY  DS    XL(L'TIKEY)       KEY OF FIRST DISPLAYED RECORD                  
                                                                                
NAMEH    DS    XL8               FAUX CAST NAME FIELD                           
NAME     DS    XL16                                                             
NAMEX    DS    XL8                                                              
                                                                                
SVENTRY  DS    F                 SAVED ADDRESS OF SCANNER BLOCK ENTRY           
SVENCNT  DS    F                 SAVED NUMBER OF ENTRIES IN SCAN BLOCK          
RANGEH   DS    XL8               FIELD FOR FAKING OUT SCANNER                   
RANGE    DS    XL7               INTO UNDERSTANDING RANGES                      
                                                                                
FNTYPE   DS    CL(L'TAFNTYPE)                                                   
                                                                                
OCVCNT   DS    X                 CAST ORIG NON-RENDERED VERSIONS COUNT          
OMVCNT   DS    X                 CAST ORIG MANUALLY ADDED VERS COUNT            
                                                                                
COMVERS  DS    XL251             COMMERCIAL'S 26K VERSIONS                      
INPVERS  DS    XL251             INPUTTED 26K VERSIONS                          
OCSTVERS DS    XL247             NON-RENDERED VERSION TABLE                     
OMANVERS DS    XL247             MANUALLY ADDED VERSION TABLE                   
                                                                                
ASELFLD  DS    A                 A(SELECT FIELD)                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
***********************************************************************         
*        DSECT FOR VNR/LIST CAST LINE                                 *         
***********************************************************************         
                                                                                
LISTD    DSECT                                                                  
LISPID   DS    CL6                                                              
         DS    CL11                                                             
LIS1VR   DS    CL1                                                              
         DS    CL14                                                             
LISVPD   DS    CL2                                                              
         DS    CL20                                                             
LISVRE   DS    CL1                                                              
LIS1LNQ  EQU   *-LISTD                                                          
LISLINE2 DS    CL8                                                              
LISNAME  DS    CL16                                                             
         DS    CL8                                                              
LISCAT   DS    CL3                                                              
         DS    CL8                                                              
LISSORT  DS    XL14                                                             
         DS    CL11                                                             
LIS2LNQ  EQU   *-LISLINE2                                                       
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR72D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR59D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR9CD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR52D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR73D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRAAD                                                       
         EJECT                                                                  
         ORG   CLAWORK                                                          
NUMUSE   DS    H                   TOTAL NUMBER OF USES PAID                    
INV      DS    CL6                 INVOICE NUMBER WORK SPACE                    
CINV     DS    CL6                 COMPLEMENTED INVOICE NUMBER                  
NWKREQ   DS    CL1                 Y=NETWORK REQUIRED FOR CLA USES              
LASTDATE DS    PL3                 LAST USE DATE INPUT                          
CYCSTART DS    CL6                 EBCDIC CYCLE START DATE                      
CYCEND   DS    CL6                              END DATE                        
PCYCS    DS    PL3                 PACKED CYCLE START DATE                      
PCYCE    DS    PL3                              END DATE                        
LASTPROG DS    CL15                LAST PROGRAM NAME INPUT                      
PDOPT3   DS    XL1                 3RD PAYMENT OPTION                           
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
                                                                                
VERVARS  DS    0X                                                               
CSTVARS  DS    0X                                                               
LASTDISP DS    X                 LAST DISPLAYED VERSION                         
RANGEC   DS    X                 RANGE CURRENT                                  
RANGEX   DS    X                 RANGE END                                      
CVLNQ    EQU   *-CSTVARS                                                        
VVLNQ    EQU   *-VERVARS                                                        
                                                                                
USETAB   DS    CL(20*(3+1))        USE DATE(3), LIFT?(1)                        
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052TAGENAA   10/11/16'                                      
         END                                                                    
