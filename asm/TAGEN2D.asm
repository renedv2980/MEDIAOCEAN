*          DATA SET TAGEN2D    AT LEVEL 033 AS OF 05/19/04                      
*PHASE T7022DA,*                                                                
         TITLE 'T7022D - CONTROL RECORD MAINTENANCE'                            
T7022D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7022D                                                         
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROL                                                     
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
         SPACE 2                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,VALREC         BUILD RECORD                                 
         BNE   *+12                                                             
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    MN10                                                             
         CLI   MODE,XRECADD                                                     
         BE    MN10                                                             
         CLI   MODE,XRECPUT                                                     
         BNE   XIT                                                              
MN10     BAS   RE,DISPLAY                                                       
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         XC    SCTAGYN,SCTAGYN     PRE-CLEAR NAMES                              
         OI    SCTAGYNH+6,X'80'                                                 
         XC    SCTCLIN,SCTCLIN                                                  
         OI    SCTCLINH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R3,KEY              R3=A(KEY)                                    
         USING TLCTD,R3                                                         
         SPACE 1                                                                
         LA    R2,SCTAGYH          VALIDATE AGENCY                              
         CLI   5(R2),3             AND L'INPUT IS 3                             
         BNE   VK0                                                              
         CLC   =C'ALL',8(R2)       AND INPUT IS 'ALL'                           
         BNE   VK0                                                              
         MVC   SVAGY,TGAGY         THEN SAVE GLOBAL AGENCY                      
         XC    TGAGY,TGAGY         AND CLEAR IT                                 
         CLI   SCTCLIH+5,0                                                      
         BE    VK1                                                              
         MVI   SCTCLIH+5,0         AND INSURE CLIENT IS EMPTY                   
         XC    SCTCLI,SCTCLI                                                    
         OI    SCTCLIH+6,X'80'                                                  
         B     VK4                                                              
         SPACE 1                                                                
VK0      GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SCTAGYNH  READ AGENCY           
         MVC   SVAGY,TGAGY         SAVE GLOBAL AGENCY                           
         SPACE 1                                                                
VK1      LA    R2,SCTCLIH          R2=A(CLIENT FIELD)                           
         CLI   5(R2),0             IF INPUT TO CLIENT, VALIDATE IT              
         BNE   VK2                                                              
         TM    SCRSTAT,SCRCHG      IF FIRST TIME FOR SCREEN                     
         BZ    VK4                                                              
         GOTO1 RECVAL,DMCB,TLCTCDQ,0  AND CONTROL RECORD EXISTS                 
         BNE   VK4                                                              
         OC    TLCTCLI,TLCTCLI     AND IT'S FOR CLIENT                          
         BZ    VK4                                                              
         MVC   SCTCLI,TLCTCLI      THEN DISPLAY CLIENT RECORD                   
         MVI   SCTCLIH+5,L'TLCTCLI                                              
         OI    SCTCLIH+6,X'80'                                                  
         SPACE 1                                                                
VK2      GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SCTCLINH  VAL. CLIENT           
         MVC   SVCLI,TGCLI         SAVE GLOBAL CLIENT                           
         B     VK6                                                              
         SPACE 1                                                                
VK4      MVC   SVCLI,TGCLI         SAVE GLOBAL CLIENT                           
         XC    TGCLI,TGCLI         AND CLEAR IT                                 
         SPACE 1                                                                
VK6      GOTO1 RECVAL,DMCB,TLCTCDQ,(X'40',0)  BUILD CONTROL RECORD KEY          
         MVC   SVKEY,KEY           SAVE IT                                      
         SPACE 1                                                                
         CLI   TWALACT,ACTSEL      SKIP MERGE IF LAST ACTION WAS SELECT         
         BE    VK8                                                              
         CLI   TWALACT,ACTADD      OR ADD                                       
         BE    VK8                                                              
         SPACE 1                                                                
         BAS   RE,MERGE            MERGE HIGH LEVEL RECORDS FOR DISPLAY         
         SPACE 1                                                                
         MVC   KEY,SVKEY           RESTORE THIS KEY                             
         MVC   TGAGY,SVAGY                 GLOBAL AGENCY                        
         MVC   TGCLI,SVCLI                        CLIENT                        
         SPACE 1                                                                
VK8      CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         BNE   VKX                                                              
         GOTO1 FLDVAL,DMCB,(X'80',AFRSTREC),999  AND NO INPUT YET               
         BNE   VKX                 (CHECKS 5(R2) WHICH WILL BE ZERO)            
         GOTO1 HIGH                LOOK FOR THIS RECORD                         
         CLC   TLCTKEY,KEYSAVE     IF NOT ON FILE YET                           
         BNE   DFLTDISP            GIVE DEFAULTS DISPLAYED MESSAGE              
         L     R4,AFRSTREC                                                      
         TWAXC (R4)                ELSE CLEAR EVERYTHING (WILL GET ERR)         
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MERGE IN HIGH LEVEL RECORDS FOR DISPLAY               
         SPACE 1                                                                
MERGE    NTR1                                                                   
         LA    R3,SVKEY            R3=A(SAVED KEY FOR CURRENT REC.)             
         USING TLCTD,R3                                                         
         SPACE 1                                                                
         OC    TLCTAGY,TLCTAGY     GET OUT IF PROCESSING ALL AGENCY             
         BZ    MERX                                                             
         XC    TGAGY,TGAGY         ELSE LOOK FOR ALL AGENCY RECORD              
         XC    TGCLI,TGCLI                                                      
         SPACE 1                                                                
         BAS   RE,GETIT            GET IT                                       
         BNE   MERX                                                             
         BAS   RE,DISPLAY          AND DISPLAY                                  
         SPACE 1                                                                
         OC    TLCTCLI,TLCTCLI     GET OUT IF PROCESSING AGENCY LEVEL           
         BZ    MERX                                                             
         MVC   TGAGY,SVAGY         ELSE LOOK FOR AGENCY RECORD                  
         SPACE 1                                                                
         BAS   RE,GETIT            GET IT                                       
         BNE   MERX                                                             
         BAS   RE,DISPLAY          AND DISPLAY                                  
         SPACE 1                                                                
MERX     B     XIT                                                              
         SPACE 3                                                                
GETIT    NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLCTCDQ,(X'20',0)  GET CONTROL RECORD                
         B     XIT                            RETURN CC                         
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 1                                                                
DKEY     NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO2            SET TO USE 2ND I/O AREA FOR READS            
         SPACE 1                                                                
         XC    SCTAGYN,SCTAGYN     PRE-CLEAR NAMES                              
         OI    SCTAGYNH+6,X'80'                                                 
         XC    SCTCLIN,SCTCLIN                                                  
         OI    SCTCLINH+6,X'80'                                                 
         SPACE 1                                                                
         L     R3,AIO1             R3=A(RECORD)                                 
         USING TLCTD,R3                                                         
         MVC   SCTAGY,=CL6'ALL'                                                 
         OI    SCTAGYH+6,X'80'                                                  
         OC    TLCTAGY,TLCTAGY     AGENCY                                       
         BZ    DKX                                                              
         MVC   SCTAGY,TLCTAGY                                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',SCTAGY),SCTAGYNH                      
         SPACE 1                                                                
         MVC   SCTCLI,TLCTCLI                                                   
         OI    SCTCLIH+6,X'80'                                                  
         OC    TLCTCLI,TLCTCLI     CLIENT                                       
         BZ    DKX                                                              
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'88',SCTCLI),SCTCLINH                      
         SPACE 1                                                                
DKX      MVC   SVAGY,TLCTAGY       SET SAVED AGENCY                             
         MVC   SVCLI,TLCTCLI                 CLIENT                             
         BAS   RE,MERGE            MERGE HIGH LEVEL RECORDS FOR DISPLAY         
         SPACE 1                                                                
         ST    R3,AIO              RESTORE I/O AREA                             
         MVC   KEY,SVKEY           AND KEY                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUILDS ESTIMATE CONTROL ELEMENT                          
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         LA    R3,KEY              R3=A(KEY)                                    
         USING TLCTD,R3                                                         
         OC    TLCTAGY,TLCTAGY     IF THIS IS ALL AGENCY RECORD                 
         BNZ   *+12                                                             
         TM    TGCTSTLV,X'C0'      AND NOT PROGRAMMER/SYSTEM MANAGER            
         BZ    LOCKOUT                                                          
         SPACE 1                                                                
         MVI   ELCODE,TAEPELQ      DELETE EXISTING ELEMENT                      
         GOTO1 REMELEM                                                          
         XC    SCTFMTN,SCTFMTN     CLEAR FORMAT NAME                            
         OI    SCTFMTNH+6,X'80'                                                 
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT          R3=A(ESTIMATE CONTROL ELEMENT)               
         USING TAEPD,R3                                                         
         MVI   TAEPEL,TAEPELQ      ELEMENT CODE                                 
         MVI   TAEPLEN,TAEPLNQ             LENGTH                               
         MVC   TAEPEL+2(TAEPLNQ-2),DFLTEPEL+2  SET DEFAULT VALUES               
         SPACE 1                                                                
         LA    R2,SCTMTHH          VALIDATE FISCAL MONTH                        
         CLI   5(R2),0                                                          
         BE    BLDR2                                                            
         GOTO1 VALINUM                                                          
         CLI   ACTUAL,12           INSURE NOT GREATER THAN 12 (DEC.)            
         BH    FLDINV                                                           
         MVC   TAEPMTH,ACTUAL                                                   
         SPACE 1                                                                
BLDR2    LA    R2,SCTRATEH         VALIDATE COMMISSION RATE                     
         CLI   5(R2),0                                                          
         BE    BLDR3                                                            
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         TM    4(R1),X'80'                                                      
         BO    FLDINV                                                           
         CLC   4(4,R1),=F'9999'                                                 
         BH    FLDINV                                                           
         MVC   TAEPRATE,6(R1)                                                   
         SPACE 1                                                                
BLDR3    LA    R4,CNTLTAB          R4=A(CONTROL TABLE)                          
         USING CNTLD,R4                                                         
BLDR4    CLI   0(R4),X'FF'                                                      
         BE    BLDRX                                                            
         LH    R2,CNTLFDSP                                                      
         AR    R2,RA               R2=A(FIELD HEADER)                           
         SPACE 1                                                                
         LA    RF,CNTLOPTS         RF=A(LIST OF CHOICES)                        
         CLI   5(R2),0                                                          
         BE    BLDR10                                                           
         SPACE 1                                                                
BLDR6    CLC   8(1,R2),0(RF)                                                    
         BE    BLDR8               INPUT IS VALID                               
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '          TRY NEXT IF NOT END OF LIST                  
         BH    BLDR6                                                            
         B     FLDINV              ELSE ERROR                                   
         SPACE 1                                                                
BLDR8    ZIC   R1,CNTLEDSP                                                      
         AR    R1,R3               R1=A(FIELD IN ELEMENT)                       
         MVC   0(1,R1),0(RF)       SAVE INPUT IN ELEMENT                        
         SPACE 1                                                                
BLDR10   LA    R4,1(RF)            BUMP TO NEXT ENTRY IN TABLE                  
         CLI   0(R4),C' '                                                       
         LA    R4,1(R4)            SCAN PAST FIRST BLANK/NUL                    
         BH    *-8                                                              
         B     BLDR4               AND CONTINUE                                 
         SPACE 1                                                                
BLDRX    LA    R2,SCTCASTH                                                      
         CLI   TAEPCAST,C'Y'       TEST USER WANTS INCLUDE CAST                 
         BNE   *+12                                                             
         CLI   TAEPFMT,TAEPFUSE    IT'S NOT VALID IF REPORT BY USE              
         BE    FLDINV                                                           
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY ELEMENT                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS ESTIMATE CONTROL ELEMENT                        
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         MVI   ELCODE,TAEPELQ                                                   
         L     R3,AIO                                                           
         BAS   RE,GETEL            GET CONTROL ELEMENT                          
         BNE   XIT                                                              
         USING TAEPD,R3            R3=A(ESTIMATE CONTROL ELEMENT)               
         MVC   DFLTEPEL,TAEPEL     SAVE IT                                      
         SPACE 1                                                                
         EDIT  (1,TAEPMTH),(2,SCTMTH),ALIGN=LEFT,ZERO=NOBLANK                   
         OI    SCTMTHH+6,X'80'     DISPLAY FISCAL MONTH                         
         SPACE 1                                                                
         EDIT  (2,TAEPRATE),(5,SCTRATE),2,ALIGN=LEFT,ZERO=NOBLANK               
         OI    SCTRATEH+6,X'80'    COMMISSION RATE                              
         SPACE 1                                                                
         USING CNTLD,R4                                                         
         LA    R4,CNTLTAB          R4=A(CONTROL TABLE)                          
DISP2    CLI   0(R4),X'FF'                                                      
         BE    DISPX                                                            
         LH    R2,CNTLFDSP                                                      
         AR    R2,RA               R2=A(FIELD HEADER)                           
         SPACE 1                                                                
         ZIC   R1,CNTLEDSP                                                      
         AR    R1,R3               R1=A(FIELD IN ELEMENT)                       
         SPACE 1                                                                
         CLI   0(R1),0             DON'T BOTHER IF NOTHING DEFINED              
         BE    *+14                                                             
         MVC   8(1,R2),0(R1)       DISPLAY FIELD                                
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R4,CNTLOPTS+1       BUMP TO NEXT ENTRY IN TABLE                  
         CLI   0(R4),C' '                                                       
         LA    R4,1(R4)            SCAN PAST FIRST BLANK/NUL                    
         BH    *-8                                                              
         B     DISP2               AND CONTINUE                                 
         SPACE 1                                                                
DISPX    BAS   RE,DISFMT           DISPLAY FORMAT NAME                          
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,SCTACTVH  DISPLAY LAST CHANGED DATA                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE REPORT FORMAT                                
         SPACE 1                                                                
DISFMT   NTR1                                                                   
         LA    R4,FORMTAB          LOOK IT UP IN TABLE                          
DISF2    CLI   0(R4),X'FF'                                                      
         BE    FLDINV                                                           
         CLC   SCTFMT,0(R4)        MATCH ON CODE                                
         BE    *+12                                                             
         LA    R4,L'FORMTAB(R4)                                                 
         B     DISF2                                                            
         SPACE 1                                                                
         MVC   SCTFMTN,1(R4)       DISPLAY FORMAT NAME                          
         OI    SCTFMTNH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS/EXITS                                                     
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
LOCKOUT  MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         L     R2,EFHACT                                                        
         B     THEEND                                                           
         SPACE 1                                                                
DFLTDISP MVI   MYMSGNO1,29         DEFAULTS DISPLAYED - ENTER CHANGES           
         L     R2,AFRSTREC                                                      
         B     INFEND                                                           
         SPACE 1                                                                
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
CNTLTAB  DS    0C                  CONTROL SETTING/VALIDATION TABLE             
*                                                                               
         DC    AL2(SCTEXPH-T702FFD),AL1(TAEPEXP-TAEPD),C'NY '                   
         DC    AL2(SCTFILTH-T702FFD),AL1(TAEPFILT-TAEPD)                        
         DC    AL1(TAEPFBIL,TAEPFCYC,TAEPFBES,TAEPFCES,0)                       
         DC    AL2(SCTMUSH-T702FFD),AL1(TAEPMUS-TAEPD),C'YN '                   
*                                                                               
         DC    AL2(SCTCOMMH-T702FFD),AL1(TAEPCOMM-TAEPD)                        
         DC    AL1(TAEPCYES,TAEPCPAY,TAEPCTNH,TAEPCNO,TAEPCCLI)                 
         DC    AL1(TAEPCHND,0)                                                  
*                                                                               
         DC    AL2(SCTFMTH-T702FFD),AL1(TAEPFMT-TAEPD)                          
         DC    AL1(TAEPFCOM,TAEPFUSE,TAEPFLB,TAEPFQTR,TAEPFCAT)                 
         DC    AL1(TAEPFV1,TAEPFA,TAEPFB,TAEPFDWN,TAEPFNOW)                     
         DC    AL1(TAEPFW,TAEPFRCD,0)                                           
*                                                                               
         DC    AL2(SCTRELH-T702FFD),AL1(TAEPREL-TAEPD),C'YN '                   
         DC    AL2(SCTPAGEH-T702FFD),AL1(TAEPPAGE-TAEPD),C'NY '                 
         DC    AL2(SCTSUPPH-T702FFD),AL1(TAEPSUPP-TAEPD),C'NY '                 
         DC    AL2(SCTTNHH-T702FFD),AL1(TAEPTNH-TAEPD),C'NY '                   
         DC    AL2(SCTHNWH-T702FFD),AL1(TAEPHNW-TAEPD),C'NY '                   
         DC    AL2(SCTTNHCH-T702FFD),AL1(TAEPTNHC-TAEPD),C'NY '                 
*                                                                               
         DC    AL2(SCTCASTH-T702FFD),AL1(TAEPCAST-TAEPD),C'YN '                 
         DC    AL2(SCTAUTOH-T702FFD),AL1(TAEPAUTO-TAEPD),C'YNMX123456 '         
         DC    AL2(SCTPBSSH-T702FFD),AL1(TAEPPBSS-TAEPD),C'YN '                 
         DC    AL2(SCTHORZH-T702FFD),AL1(TAEPHORZ-TAEPD),C'NYC '                
         DC    AL2(SCTCOMLH-T702FFD),AL1(TAEPCOML-TAEPD),C'YN '                 
*                                                                               
         DC    AL2(SCTRCAPH-T702FFD),AL1(TAEPRCAP-TAEPD),C'N'                   
         DC    AL1(TAEPRQTR,TAEPRMTH,TAEPRVQT,TAEPRUSE,TAEPRWC)                 
         DC    AL1(TAEPRXCM,TAEPRTTL,TAEPR1,TAEPRCO,TAEPRCOG)                   
         DC    AL1(TAEPRPRJ,TAEPR2,0)                                           
*                                                                               
         DC    AL2(SCTACTH-T702FFD),AL1(TAEPACTL-TAEPD),C'NYXPOUZ '             
         DC    AL2(SCTESTH-T702FFD),AL1(TAEPEST-TAEPD),C'NYP '                  
         DC    AL2(SCTLEFTH-T702FFD),AL1(TAEPLEFT-TAEPD),C'YN '                 
         DC    AL2(SCTNOBXH-T702FFD),AL1(TAEPNOBX-TAEPD),C'NY '                 
         DC    AL2(SCTDLRH-T702FFD),AL1(TAEPDLR-TAEPD),C'NY '                   
         DC    AL2(SCTFREEH-T702FFD),AL1(TAEPFREE-TAEPD),C'YN '                 
         DC    AL2(SCTSIGH-T702FFD),AL1(TAEPSIG-TAEPD),C'NY '                   
         DC    AL2(SCTCLAH-T702FFD),AL1(TAEPCLA-TAEPD),C'YN '                   
         DC    AL2(SCTDETH-T702FFD),AL1(TAEPDET-TAEPD),C'NY '                   
         DC    X'FF'                                                            
         SPACE 2                                                                
FORMTAB  DS    0CL16               FORMAT NAMES                                 
         DC    AL1(TAEPFCOM),CL15'BY COML/CYC/USE'                              
         DC    AL1(TAEPFUSE),CL15'BY USE/CYC/COML'                              
         DC    AL1(TAEPFLB),CL15'BY COML/USE/CYC'                               
         DC    AL1(TAEPFQTR),CL15'BY COML/USE/QTR'                              
         DC    AL1(TAEPFCAT),CL15'BY COML/USE/CAT'                              
         DC    AL1(TAEPFV1),CL15'VARIANCE BY QTR'                               
         DC    AL1(TAEPFA),CL15'BY COML/USE/DET'                                
         DC    AL1(TAEPFB),CL15'BY USE/COML/DET'                                
         DC    AL1(TAEPFDWN),CL15'DOWNLOAD'                                     
         DC    AL1(TAEPFNOW),CL15'NOW REPORT'                                   
         DC    AL1(TAEPFW),CL15'BY USE/COML/DET'                                
         DC    AL1(TAEPFRCD),CL15'RECAP AND D/L  '                              
         DC    X'FF'                                                            
         SPACE 2                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'CONTROL ',CL8'LIST  '                                 
PF13X    EQU   *                                                                
         DC    AL1(PF24X-*,24,0,0,PFTSETPR)                                     
         DC    CL3' ',CL8'ESTIMATE',CL8'REPORT'                                 
PF24X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER CONTROL TABLE                                     
         SPACE 1                                                                
CNTLD    DSECT                                                                  
CNTLFDSP DS    H                   DISP. TO SCREEN FIELD HEADER                 
CNTLEDSP DS    AL1                 DISP. TO ELEMENT FIELD                       
CNTLOPTS DS    0C                  VALID OPTIONS - ENDS W/ <=BLANK              
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR2DD                                                       
         SPACE 2                                                                
*              LOCAL SAVED STORAGE                                              
         SPACE 1                                                                
SVAGY    DS    CL(L'TGAGY)         SAVED GLOBAL AGENCY                          
SVCLI    DS    CL(L'TGCLI)         SAVED GLOBAL CLIENT                          
SVKEY    DS    CL(L'KEY)           SAVED KEY                                    
DFLTEPEL DS    CL(TAEPLNQ)         DEFAULT TAEPEL                               
         EJECT                                                                  
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033TAGEN2D   05/19/04'                                      
         END                                                                    
