*          DATA SET TAGENCB    AT LEVEL 022 AS OF 10/24/12                      
*PHASE T702CBA                                                                  
         TITLE 'T702CB - SOAP GUARANTEE MAINTENANCE'                            
T702CB   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702CB,R6                                                      
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=PROGRAM SAVED STORAGE                     
         USING SOAPD,R7                                                         
         EJECT                                                                  
*              MODE CONTROL                                                     
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
*                                                                               
         CLI   MODE,SETFILE        GUAR. TO GET THIS MODE DURING SELECT         
         BNE   SG5                                                              
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   SGX                                                              
         CLI   THISLSEL,CHASELQ    AND SELECTED FOR CHANGE                      
         BNE   SGX                                                              
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE = A(TRANSLATOR I/O BLOCK)                 
         USING TIOBD,RE                                                         
         LA    R1,SSGROLH                                                       
         SR    R1,RA                                                            
         STH   R1,TIOBLAST         FORCE GENCON TO THINK A FLD WAS I/P          
         B     SGX                                                              
*                                                                               
SG5      CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,VALREC         BUILD RECORD                                 
         BNE   *+12                                                             
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
*                                                                               
         CLI   MODE,XRECADD        IF RECORD ADDED                              
         BE    SG10                                                             
         CLI   MODE,XRECREST       OR RECORD RESTORED                           
         BE    SG10                                                             
         CLI   MODE,XRECDEL        OR RECORD DELETED                            
         BE    SG10                                                             
*                                                                               
         CLI   MODE,XRECPUT        IF RECORD CHANGED                            
         BNE   SG20                                                             
         CLI   ACTNUM,ACTSEL       AND ACTION IS SELECT                         
         BE    *+12                                                             
         CLI   TWALACT,ACTSEL      OR LAST ACTION WAS SELECT                    
         BNE   SG10                                                             
         CLC   THISPG,NPAGES       AND THIS WAS LAST PAGE                       
         BE    SGX                 THEN DON'T RE-DISPLAY                        
SG10     BAS   RE,DISPLAY          RE-DISPLAY THE RECORD                        
*                                                                               
         CLI   MODE,XRECPUT        TEST RECORD CHANGED                          
         BNE   SGX                                                              
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BNE   SG15                                                             
         CLI   TWALACT,ACTSEL      AND LAST ACTION WAS SELECT                   
         BNE   SG15                                                             
         MVC   CONACT(6),=C'SELECT' RETURN TO SELECT NEXT TIME                  
SG15     B     PGDSPMS2            GIVE MY OWN MESSAGE                          
*                                                                               
SG20     CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BNE   SG30                                                             
         CLI   THISLSEL,C'D'       AND SELECTED FOR DELETE                      
         BE    SGX                 THEN DON'T DISPLAY YET                       
         BAS   RE,DISPLAY          ELSE DISPLAY THE RECORD                      
*                                                                               
         CLI   ACTNUM,ACTDIS       IF ACTION IS DISPLAY                         
         BE    PGDSPMSG            GIVE MY OWN MESSAGE                          
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BE    PGDSPMSG            GIVE MY OWN MESSAGE                          
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   SGX                                                              
         CLC   THISPG,NPAGES       AND THIS IS NOT LAST PAGE                    
         BNE   PGDSPMSG            GIVE MY OWN MESSAGE                          
         B     SGX                                                              
*                                                                               
SG30     CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   SGX                                                              
         GOTO1 RECVAL,DMCB,TLSGCDQ,(X'20',0) GET THE RECORD                     
         BAS   RE,CLRSCRN          CLEAR THE SCREEN                             
         XC    CLS,CLS             CLEAR LAST ELEMENT KEYS                      
         BAS   RE,PREP             PRINT THE REPORT                             
         TM    WHEN,X'40'          IF SPOOLING NOW                              
         BZ    SGX                                                              
         XC    CONSERV,CONSERV                                                  
         MVC   CONSERV(4),=C'$DQU'                                              
         BAS   RE,CLRCYC           CLEAR CYCLE LINES                            
*                                                                               
SGX      B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         TM    SCRSTAT,SCRCHG+RECCHG IF SCREEN/RECORD CHANGED                   
         BZ    *+8                                                              
         NI    SSGSSNH+4,X'DF'     SET SSN FIELD CHANGED                        
*                                                                               
         TM    SSGSSNH+4,X'20'     IF SSN CHANGED                               
         BO    VKEY10                                                           
         NI    SSGGUAH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         LA    R1,W4IO             VALIDATE SSN                                 
         ST    R1,AIO                                                           
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SSGSSNH),SSGSSNNH                     
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
VKEY10   LA    R2,SSGGUAH          R2=A(SOAP GUARANTEE CODE)                    
         TM    4(R2),X'20'         IF CODE CHANGED                              
         BO    VKEY50                                                           
         CLI   ACTNUM,ACTADD       IF WE'RE ADDING                              
         BNE   *+12                                                             
         BAS   RE,GETCODE          GET NEXT SOAP GUARANTEE NUMBER               
         B     VKEY40                                                           
*                                                                               
         CLI   5(R2),0             OTHERWISE TEST FOR INPUT                     
         BNE   VKEY20                                                           
         OC    TGGUA,TGGUA         NONE - SOMETHING IN GLOBAL                   
         BZ    VKEY20                                                           
         MVC   8(4,R2),TGGUA       YES - MOVE TO FIELD                          
         XC    8(4,R2),HEXFFS      AND UNCOMPLEMENT IT                          
         OI    6(R2),X'80'                                                      
         B     VKEY40                                                           
VKEY20   GOTO1 ANY                 REQUIRE FIELD                                
         MVC   TGGUA,WORK                                                       
         XC    TGGUA,HEXFFS        SAVE COMPLEMENTED IN GLOBAL                  
VKEY40   OI    4(R2),X'20'         SET VALIDATED                                
         XC    CLS,CLS             CLEAR LAST ELEMENT KEYS                      
*                                                                               
VKEY50   GOTO1 RECVAL,DMCB,TLSGCDQ,(X'40',0)  BUILD KEY                         
         B     XIT                                                              
         EJECT                                                                  
*              GETS NEXT SOAP GUARANTEE CODE ON ADD                             
         SPACE 1                                                                
GETCODE  NTR1                                                                   
         XC    TGGUA,TGGUA         SET TO PICK UP NEXT CODE                     
         GOTO1 RECVAL,DMCB,TLSGCDQ,(X'C0',TGGUA)  BUILD DUMMY KEY               
         GOTO1 HIGH                READ FIRST RECORD ON FILE                    
         LA    R3,KEY                                                           
         USING TLSGD,R3                                                         
*                                                                               
         ZAP   DUB,=P'0'           DEFAULT GUARANTEE CODE                       
         CLC   TLSGKEY(TLSGGUA-TLSGD),KEYSAVE  IF WE FOUND A RECORD             
         BNE   GETCD20                                                          
         MVC   GUACODE,TLSGGUA     SAVE ITS CODE AND                            
         XC    GUACODE,HEXFFS      UNCOMPLEMENT IT                              
         PACK  DUB,GUACODE         PACK IT                                      
*                                                                               
GETCD20  AP    DUB,=P'1'           OLD CODE PLUS 1                              
         OI    DUB+7,X'0F'                                                      
         UNPK  GUACODE,DUB+5(3)    IS NEW CODE                                  
         MVC   TGGUA,GUACODE                                                    
         XC    TGGUA,HEXFFS        COMPLEMENT IT FOR GLOBAL                     
         SPACE 1                                                                
         MVC   8(L'GUACODE,R2),GUACODE  DISPLAY FOR USER                        
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 1                                                                
DKEY     NTR1                                                                   
         CLC   SVKEY,KEY           IF KEY CHANGED                               
         BE    *+10                                                             
         XC    CLS,CLS             CLEAR LAST ELEMENT KEYS                      
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
         L     R4,AIO              R4=A(RECORD)                                 
         USING TLSGD,R4                                                         
         MVC   SSGSSN,TLSGSSN      S/S NUMBER                                   
         OI    SSGSSNH+6,X'80'                                                  
*                                                                               
         LA    R1,W4IO             SET IO FOR SSN READ                          
         ST    R1,AIO                                                           
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',SSGSSN),SSGSSNNH                      
         MVC   AIO,AIO1            RETORE IO                                    
*                                                                               
         MVC   SSGGUA,TLSGGUA      GUARANTEE CODE                               
         XC    SSGGUA,HEXFFS                                                    
         OI    SSGGUAH+6,X'80'                                                  
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE BUILDS RECORD                                            
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                  DELETE/ADD ROLE ELEMENT                      
         GOTO1 NAMIN,DMCB,TAFNELQ,SSGROLH,TAFNTROL                              
*                                                                               
         L     R4,AIO              R4=A(RECORD)                                 
         MVI   ELCODE,TASGELQ                                                   
         GOTO1 REMELEM             REMOVE EXISTING SOAP GUARANTEE               
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT          BUILD NEW SOAP GUARANTEE DETAILS             
         USING TASGD,R4                                                         
         MVI   TASGEL,TASGELQ      SET ELEMENT CODE                             
         MVI   TASGLEN,TASGLNQ     SET ELEMENT LENGTH                           
         BAS   RE,VRCAT            VALIDATE CATEGORY                            
         BAS   RE,VRCORP           VALIDATE CORP CODE                           
         BAS   RE,VRAGNT           VALIDATE AGENT CODE                          
         BAS   RE,VRAGY            VALIDATE AGENCY CODE                         
         BAS   RE,VRPYMT           VALIDATE PAYMENT METHOD                      
         GOTO1 ADDELEM             ADD SOAP GUARANTEE DETAILS ELEMENT           
*                                                                               
*                                  DELETE/ADD COMMENT ELEMENT                   
         GOTO1 NAMIN,DMCB,(2,TACMELQ),(X'80',SSGCMNTH),TACMTYPG                 
*                                                                               
         BAS   RE,VRCYC            VALIDATE/ADD CYCLE ELEMENTS                  
*                                                                               
         GOTO1 ACTVIN,DMCB,0       UPDATE ACTIVITY INFO                         
*                                                                               
         MVC   KEY,SVKEY           RESTORE GUARANTEE KEY                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES CATEGORY                                       
         SPACE 1                                                                
VRCAT    NTR1                                                                   
         LA    R2,SSGCATH          CATEGORY CODE                                
         GOTO1 ANY                                                              
         GOTO1 CATVAL,DMCB,WORK    VALIDATE CATEGORY                            
         BNE   FLDINV                                                           
         MVC   TASGCAT,TGCAT                                                    
         B     XIT                                                              
         SPACE                                                                  
*              ROUTINE VALIDATES CORPORATION CODE (OPTIONAL)                    
         SPACE 1                                                                
VRCORP   NTR1                                                                   
         MVI   TASGCRP,0                                                        
         LA    R2,SSGCRPH          CORP CODE (OPTIONAL)                         
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    VRCORPX                                                          
         MVC   TASGCRP,8(R2)                                                    
         CLI   TASGCRP,C'Y'        IF FIELD HAD 'Y'                             
         BNE   *+8                                                              
         MVI   TASGCRP,C'1'        THEN SET TO '1' IN ELEMENT                   
*                                                                               
         MVI   ELCODE,TATIELQ      LOOK FOR TAX ID EL. FOR CORP.                
         MVI   HALF,TATITYCO       SET CORPORATION TYPE                         
         MVC   HALF+1(1),TASGCRP   AND CORP CODE                                
         LA    R1,W4IO             AND SET RECORD AREA                          
         ST    R1,AIO                                                           
         GOTO1 GETL,DMCB,(2,HALF)                                               
         MVC   AIO,AIO1            RESET I/O AREA                               
         BNE   FLDINV                                                           
*                                                                               
         CLI   8(R2),C'Y'          IF 'Y' WAS INPUT                             
         BNE   VRCORPX                                                          
         L     R4,TGELEM                                                        
         BAS   RE,NEXTEL           SEARCH FOR ADDL CORPS ON W4 REC              
         BE    ERRCRP              NEED PRECISE CORP CODE                       
VRCORPX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES AGENT CODE                                     
         SPACE 1                                                                
VRAGNT   NTR1                                                                   
         LA    R2,SSGNCDEH         AGENT CODE (OPTIONAL)                        
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    VRAGNTX                                                          
         GOTO1 RECVAL,DMCB,TLANCCDQ,(R2)                                        
         GOTO1 TRNSAGT,DMCB,(X'80',TGAGT),TASGNCDE                              
VRAGNTX  B     XIT                                                              
         SPACE                                                                  
*              ROUTINE VALIDATES AGENCY CODE                                    
         SPACE 1                                                                
VRAGY    NTR1                                                                   
         LA    R2,SSGAGYH                                                       
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',SSGAGYH)  VALIDATE AGENCY             
         MVC   TASGAGY,TGAGY       SET AGENCY CODE                              
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         B     XIT                                                              
         SPACE                                                                  
*              ROUTINE VALIDATES PAYMENT METHOD                                 
         SPACE 1                                                                
VRPYMT   NTR1                                                                   
         LA    R2,SSGPYMTH         VALIDATE PAYMENT METHOD                      
         GOTO1 ANY                                                              
         LA    RE,PYMTBL           RE = A(PAYMENT METHOD TABLE)                 
         USING PYMTBLD,RE                                                       
         ZIC   R1,5(R2)            COMPARE FOR LENGTH OF INPUT                  
         BCTR  R1,0                                                             
VRPYMT10 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PYMTTYP(0),WORK     CHECK FOR MATCH                              
         BE    VRPYMT15                                                         
         LA    RE,PYMTLNQ(RE)      BUMP TO NEXT ENTRY IN TABLE                  
         CLI   0(RE),X'FF'                                                      
         BNE   VRPYMT10            LOOP                                         
         B     FLDINV              INVALID PAYMENT METHOD                       
*                                                                               
VRPYMT15 MVC   TASGPYMT,PYMTST     SET PAYMENT METHOD                           
         MVC   SVPMNT,TASGPYMT     SAVE NEW PAYMENT METHOD                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE VALIDATES/ADDS SOAP CYCLE ELEMENTS                       
         SPACE 1                                                                
         USING TASLD,R4                                                         
VRCYC    NTR1                                                                   
         OC    CLS,CLS             IF SOMETHING ON SCREEN                       
         BZ    VRCYC8                                                           
         L     R4,AIO              R4=A(RECORD)                                 
         MVI   ELCODE,TASLELQ      SET TO DELETE CORRESPONDING ELEMENTS         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VRCYC5   CLC   TASLSPD,CLSPER      IF ELEMENT IS WITHIN DISPLAYED RANGE         
         BL    VRCYC7                                                           
         CLC   TASLSPD,CLEPER                                                   
         BH    VRCYC7                                                           
         MVI   TASLEL,X'FF'        SET TO DELETE                                
VRCYC7   BAS   RE,NEXTEL                                                        
         BE    VRCYC5                                                           
*                                                                               
         MVI   ELCODE,X'FF'        REMOVE MARKED ELEMENTS                       
         GOTO1 REMELEM                                                          
*                                                                               
VRCYC8   LA    R5,CYCLINES         NUMBER OF LINES                              
         LA    R3,SSGLL1H          R3=A(1ST LINE)                               
         USING LINED,R3                                                         
         LA    R4,ELEMENT          BUILD NEW SOAP GUAR CYCLE ELEMENTS           
*                                                                               
VRCYC10  GOTO1 FLDVAL,DMCB,(X'80',LAIRH),7    IF BLANK LINE                     
         BE    VRCYC20                        SKIP IT                           
*                                                                               
         XC    ELEMENT,ELEMENT     OTHERWISE CHECK FOR FIELD INPUT              
         MVI   TASLEL,TASLELQ      SET ELEMENT CODE                             
         MVI   TASLLEN,TASLLNQ     SET ELEMENT LENGTH                           
         BAS   RE,VRPER            VALIDATE AIR CYCLE PERIOD                    
         BAS   RE,VRNOT            VALIDATE NOTICE BY DATE                      
         BAS   RE,VRPRF            VALIDATE PERFORMANCE RATE                    
         BAS   RE,VRGRT            VALIDATE EPISODES GUARANTEED                 
         BAS   RE,VRWKS            VALIDATE WEEKS                               
         BAS   RE,CALAVG           CALCULATE AVERAGE PAY                        
         GOTO1 ADDELEM             ADD SOAP GUARANTEE CYCLE ELEMENT             
*                                                                               
VRCYC20  LA    R3,LINELNQ(R3)      BUMP TO NEXT DISPLAY LINE                    
         BCT   R5,VRCYC10          LOOP                                         
*                                                                               
VRCYCX   B     XIT                                                              
         EJECT                                                                  
*              VALIDATE AIR CYCLE PERIOD                                        
VRPER    NTR1                                                                   
         LA    R2,LAIRH            VALIDATE PERIOD                              
         LA    R5,BLOCK            R4=A(PERVAL BLOCK)                           
         GOTO1 PDVAL,DMCB,(R5)                                                  
         USING PERVALD,R5                                                       
         MVC   TASLSPD,PVALPSTA    SAVE PWOS PERIOD                             
         B     XIT                                                              
         SPACE                                                                  
*              VALIDATE NOTICE BY DATE                                          
VRNOT    NTR1                                                                   
         LA    R2,LNOTH            OPTIONAL NOTICE BY DATE                      
         GOTO1 DTVAL,DMCB,(X'80',TASLNTCD)                                      
         B     XIT                                                              
         SPACE                                                                  
*              VALIDATE PERFORMANCE RATE                                        
VRPRF    NTR1                                                                   
         LA    R2,LPERFH           PERFORMANCE RATE                             
         GOTO1 ANY                                                              
         ZIC   R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,WORK,(R5)                                           
         CLI   0(R1),X'FF'                                                      
         BE    AMTINV                                                           
         TM    4(R1),X'80'         NEGATIVE NOT ALLOWED                         
         BO    AMTINV                                                           
         MVC   TASLPERF,4(R1)                                                   
         B     XIT                                                              
         SPACE                                                                  
*              VALIDATE EPISODES GUARANTEED                                     
VRGRT    NTR1                                                                   
         LA    R2,LGRTH            NUMBER OF EPISODES GUARANTEED                
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         BZ    VRGRTX                                                           
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   0(R1),0                                                          
         BNE   FLDINV                                                           
         CLC   4(4,R1),=X'00007FFF'                                             
         BH    FLDINV                                                           
         MVC   TASLGRT,6(R1)                                                    
VRGRTX   B     XIT                                                              
         SPACE                                                                  
*              VALIDATE WEEKS                                                   
VRWKS    NTR1                                                                   
         LA    R2,LWEEKH           NUMBER OF WEEKS                              
         CLI   5(R2),0             IF NO WEEKS INPUT                            
         BNE   VRWKS5                                                           
         OC    TASLGRT,TASLGRT     THEN MUST BE NO GUARANTEE                    
         BNZ   FLDMISS                                                          
         B     VRWKSX                                                           
*                                                                               
VRWKS5   GOTO1 VALINUM                                                          
         MVC   TASLWKS,ACTUAL      SET WEEKS INPUT                              
         OC    TASLGRT,TASLGRT                                                  
         BNZ   VRWKSX              THEN MUST BE GUARANTEE                       
         LA    R2,LGRTH                                                         
         B     FLDMISS                                                          
VRWKSX   B     XIT                                                              
         EJECT                                                                  
*              CALCULATE AVERAGE                                                
CALAVG   NTR1                                                                   
         MVC   FULL,TASLPERF       PERFORMANCE RATE                             
         TM    SVPMNT,TASGPPER     IF PAY PER PERFORMANCE                       
         BO    CALAVG15            AVERAGE IS PERFORMANCE                       
         LH    RF,TASLGRT          RF=EPISODES GUARANTEED                       
         M     RE,FULL                XPERFORMANCE RATE                         
         ZIC   R1,TASLWKS          R1=NUMBER WEEKS GUARANTEED                   
         MH    R1,=H'100'          X100                                         
         LTR   R1,R1                                                            
         BZ    CALAVGX                                                          
         XR    RE,RE                                                            
         DR    RE,R1               (GRTXPERF)/(WKSX100)                         
         ST    RF,FULL                                                          
*                                                                               
CALAVG15 MVC   TASLAVG,FULL                                                     
CALAVGX  B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE DISPLAYS RECORD                                          
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         BAS   RE,CLRSCRN          CLEAR THE SCREEN                             
*                                                                               
         BAS   RE,DRHDR            DISPLAY HEADER PART OF RECORD                
         BAS   RE,DRCYC            DISPLAY CYCLE ELEMENTS                       
*                                                                               
         GOTO1 ACTVOUT,DMCB,SSGLCHGH   LAST CHANGED INFO                        
*                                                                               
         GOTO1 FLDVAL,DMCB,(X'20',AFRSTREC),999 MAKE ALL FIELDS VALID           
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         SPACE 2                                                                
*              DISPLAY CATEGORY CODE                                            
CLRSCRN  NTR1                                                                   
         XC    SSGCRPN,SSGCRPN     PRE-CLEAR CORP NAME                          
         OI    SSGCRPNH+6,X'80'                                                 
         XC    SSGNCDN,SSGNCDN               AGENT NAME                         
         OI    SSGNCDNH+6,X'80'                                                 
         XC    SSGAGYN,SSGAGYN               AGENCY NAME                        
         OI    SSGAGYNH+6,X'80'                                                 
         GOTO1 FLDVAL,DMCB,(X'01',SSGROLH),(X'80',SSGHD1H)                      
         BAS   RE,CLRCYC           CLEAR CYCLE LINES SKIP PROTECTED             
         B     XIT                                                              
         SPACE                                                                  
CLRCYC   NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'01',SSGLL1H),(X'80',SSGLSTH)                      
         LA    RE,CYCLINES         NUMBER OF CYCLE LINES                        
         LA    R3,SSGLL1H          A(FIRST CYCLE LINE)                          
         USING LINED,R3                                                         
CLRSCR5  XC    LAVG,LAVG           CLEAR AVERAGE (PROTECTED)                    
         OI    LAVGH+6,X'80'                                                    
         LA    R3,LINELNQ(R3)      BUMP TO NEXT LINE                            
         BCT   RE,CLRSCR5          LOOP                                         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              DISPLAY HEADER INFO TO SCREEN                                    
DRHDR    NTR1                                                                   
         GOTO1 CHAROUT,DMCB,TAFNELQ,SSGROLH,TAFNTROL ROLE                       
         L     R4,AIO              R4=A(RECORD)                                 
         MVI   ELCODE,TASGELQ      GET SOAP GUARANTEE DETAILS                   
         BAS   RE,GETEL                                                         
         BNE   DRHDR10                                                          
         USING TASGD,R4                                                         
         BAS   RE,DRCAT            DISPLAY CATEGORY CODE                        
         BAS   RE,DRCORP           DISPLAY CORP CODE                            
         BAS   RE,DRAGNT           DISPLAY AGENT CODE                           
         BAS   RE,DRAGY            DISPLAY AGENCY CODE                          
         BAS   RE,DRPYMT           DISPLAY PAYMENT METHOD                       
*                                                                               
DRHDR10  GOTO1 CHAROUT,DMCB,TACMELQ,(2,SSGCMNTH),TACMTYPG COMMENT               
         B     XIT                                                              
         SPACE 2                                                                
*              DISPLAY CATEGORY CODE                                            
DRCAT    NTR1                                                                   
         MVC   SSGCAT,TASGCAT      DISPLAY CATEGORY CODE                        
         B     XIT                                                              
         SPACE                                                                  
*              DISPLAY CORPORATION CODE                                         
DRCORP   NTR1                                                                   
         CLI   TASGCRP,0           IF WE HAVE CORP                              
         BE    DRCORPX                                                          
         MVC   SSGCRP,TASGCRP      DISPLAY IT                                   
         LA    R1,W4IO             LOOK UP TAX ID EL. ON W4 REC.                
         ST    R1,AIO                                                           
         MVI   HALF,TATITYCO       SET CORPORATION TYPE                         
         MVC   HALF+1(1),TASGCRP   AND CORP CODE                                
         MVI   ELCODE,TATIELQ      SET ELEMENT CODE                             
         GOTO1 GETL,DMCB,(2,HALF)                                               
         MVC   AIO,AIO1            RESTORE IOAREA                               
         BNE   DRCORPX                                                          
*                                                                               
         L     R4,TGELEM           R2=A(TAX ID ELEMENT FOR CORP)                
         USING TATID,R4                                                         
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',TATIID),SSGCRPNH GET CORP NME         
         MVC   AIO,AIO1                                                         
         MVC   TGSSN,SSGSSN        RESTORE ACTUAL SSN TO GLOBAL                 
DRCORPX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
         USING TASGD,R4                                                         
*              DISPLAY AGENT CODE & NAME                                        
DRAGNT   NTR1                                                                   
         OC    TASGNCDE,TASGNCDE   IF NO AGENT CODE                             
         BNZ   DRAGNT10                                                         
         LA    R4,W4IO             LOOK FOR PAYEE ELEMENT ON W4                 
         MVI   ELCODE,TAPEELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DRAGNTX                                                          
         MVC   SSGNCDN,=CL16'* PAYEE ON W4 *'                                   
         B     DRAGNTX                                                          
*                                                                               
DRAGNT10 GOTO1 TRNSAGT,DMCB,(X'40',TASGNCDE),SSGNCDE                            
         MVC   SSGNCDN,=CL16' * NOT FOUND * '                                   
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'8C',SSGNCDE),0                            
         BNE   *+10                                                             
         MVC   SSGNCDN,TGNAME                                                   
         MVC   AIO,AIO1                                                         
DRAGNTX  B     XIT                                                              
         SPACE                                                                  
*              DISPLAY AGENCY CODE & NAME                                       
DRAGY    NTR1                                                                   
         MVC   SSGAGY,TASGAGY      DISPLAY AGENCY                               
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',SSGAGY),SSGAGYNH                      
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         SPACE                                                                  
*              DISPLAY PAYMENT METHOD                                           
DRPYMT   NTR1                                                                   
         LA    RE,PYMTBL           RE = A(PAYMENT METHOD TABLE)                 
         USING PYMTBLD,RE                                                       
DRPYMT10 CLC   TASGPYMT,PYMTST     MATCH ON STATUS BYTE                         
         BE    DRPYMT15                                                         
         LA    RE,PYMTLNQ(RE)      BUMP TO NEXT ENTRY IN TABLE                  
         CLI   0(RE),X'FF'                                                      
         BNE   DRPYMT10            LOOP                                         
         DC    H'0'                                                             
*                                                                               
DRPYMT15 MVC   SSGPYMT(L'PYMTTYP),PYMTTYP   DISPLAY PAYMENT TYPE                
         MVC   SVPMNT,TASGPYMT                                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              DISPLAY CYCLE ELEMENTS                                           
         USING TASLD,R4                                                         
DRCYC    NTR1                                                                   
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         BNE   DRCYC10                                                          
         CLI   PFAID,14            AND USER WANTS ROOM FOR MORE                 
         BE    DRCYC20             CLEAR SAVED KEYS & LEAVE SCRN EMPTY          
DRCYC10  L     R4,AIO              R4=A(RECORD)                                 
         MVI   ELCODE,TASLELQ      GET SOAP GUARANTEE CYCLE                     
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
DRCYC20  XC    CLS,CLS             NONE FOUND- CLEAR SAVED ELEMENT KEYS         
         B     DRCYCX                                                           
*                                                                               
         OC    CLS,CLS             IF WE HAVEN'T DISPLAYED ANYTHING YET         
         BZ    DRCYC50             THEN START WITH FIRST                        
         XR    R0,R0               INITIALIZE START SWITCH                      
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         BE    *+12                                                             
         CLI   MODE,XRECADD        OR JUST ADDED IT                             
         BNE   DRCYC30                                                          
         GOTO1 FLDVAL,DMCB,(X'40',SSGLL1H),SSGLSTH AND SCREEN CHANGED           
         BE    DRCYC30                                                          
         LA    R0,1                SET START SWITCH TO RE-DISPLAY               
*                                                                               
DRCYC30  LTR   R0,R0               IF NEED TO DISPLAY CURRENT PAGE              
         BZ    DRCYC40                                                          
         CLC   TASLSPD,CLSPER      SCAN FOR FIRST ELEMENT DISPLAYED             
         BNL   DRCYC50                                                          
         B     *+14                                                             
DRCYC40  CLC   TASLSPD,CLEPER      ELSE SCAN FOR 1ST ELE AFTR LAST DISP         
         BH    DRCYC50                                                          
         BAS   RE,NEXTEL                                                        
         BE    DRCYC30                                                          
         XC    CLS,CLS             NONE LEFT- START FROM BEGINNING              
         B     DRCYC10                                                          
*                                                                               
DRCYC50  LA    R1,CYCLINES         NUMBER OF LINES                              
         LA    R3,SSGLL1H          R3=A(FIRST LINE)                             
         USING LINED,R3                                                         
         MVC   CLSPER,TASLSPD      SAVE KEY OF FIRST ELE DISPLAYED              
DRCYC60  MVC   CLEPER,TASLSPD      SAVE KEY OF LAST EL DISPLAYED                
         BAS   RE,DRCYCLIN         DISPLAY CYCLE LINE                           
         BAS   RE,NEXTEL                                                        
         BNE   DRCYCX                                                           
         LA    R3,LINELNQ(R3)      BUMP TO NEXT DISPLAY LINE                    
         BCT   R1,DRCYC60          LOOP                                         
*                                                                               
DRCYCX   XR    R2,R2               SET TO COUNT TOTAL N'PAGES                   
         BAS   RE,PGIT                                                          
         MVC   NPAGES,BYTE                                                      
         LA    R2,CLEPER           SET TO CALC THIS PAGE NUMBER BASED           
         BAS   RE,PGIT             ON LAST CYC ELEMENT DISPLAYED                
         MVC   THISPG,BYTE                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS ONE LINE                                        
DRCYCLIN NTR1                                                                   
         GOTO1 DATCON,DMCB,(1,TASLSSTR),(8,LAIR)  AIR CYCLE PERIOD              
         MVI   LAIR+8,C'-'                                                      
         GOTO1 DATCON,DMCB,(1,TASLSEND),(8,LAIR+9)                              
*                                                                               
         GOTO1 DATCON,DMCB,(1,TASLNTCD),(5,LNOT)  NOTICE BY DATE                
*                                                                               
         EDIT  TASLPERF,(9,LPERF),2               PERFORMANCE RATE              
*                                                                               
         LA    R2,LGRT                                                          
         EDIT  TASLGRT,(6,0(R2)),2,ALIGN=LEFT     EPISODES GUARANTEED           
         AR    R2,R0               + LENGTH OF EDIT                             
         SH    R2,=H'3'            - 3 (FOR DEC PT AND TWO PLACES)              
         CLC   =C'.00',0(R2)       DON'T DISPLAY DECIMAL ZERO                   
         BNE   *+14                                                             
         MVC   0(3,R2),SPACES                                                   
         B     DRCLIN5                                                          
         CLI   2(R2),C'0'                                                       
         BNE   *+8                                                              
         MVI   2(R2),C' '                                                       
*                                                                               
DRCLIN5  EDIT  TASLWKS,(3,LWEEK),ALIGN=LEFT       WEEKS GUARANTEED              
*                                                                               
         OC    TASLAVG,TASLAVG     AVERAGE                                      
         BZ    DRCLINX                                                          
         EDIT  TASLAVG,(9,LAVG),2                                               
*                                                                               
DRCLINX  B     XIT                                                              
         DROP  R3,R4,R8                                                         
         EJECT                                                                  
*              ROUTINE PRINTS THE REPORT                                        
PREP     NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         LA    R0,MYSPECS                                                       
         ST    R0,SPECS                                                         
         LA    R0,HDHOOK                                                        
         ST    R0,HEADHOOK                                                      
         BAS   RE,DRHDR            DISPLAY HEADER PART OF RECORD                
*                                                                               
PREP10   BAS   RE,DRCYC            DISPLAY CYCLE LINES                          
         LA    R2,SSGLL1H          R2=A(FIRST CYCLE LINE)                       
         LR    R3,R2               R3=A(LAST LINE TO PRINT)                     
         LA    R0,3                PRINT IN THREE CHUNKS                        
*                                                                               
PREP20   LR    R2,R3                                                            
         LA    R3,4*LINELNQ(R3)    PRINTING 4 LINES AT A TIME                   
         MVI   BYTE,C'P'                                                        
         GOTO1 PRTSCRN,DMCB,(R2),(R3),P                                         
         GOTO1 SPOOL,DMCB,ASPOOLD  SKIP A LINE                                  
         BCT   R0,PREP20                                                        
*                                                                               
         GOTO1 FLDVAL,DMCB,(X'20',AFRSTREC),999 MAKE ALL FIELDS VALID           
*                                                                               
         CLC   THISPG,NPAGES       TEST MORE TO DISPLAY                         
         BE    PREPX                                                            
         BAS   RE,CLRCYC           CLEAR CYCLE LINES                            
         B     PREP10                                                           
*                                                                               
PREPX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE CALCULATES CURRENT PAGE NUMBER                           
*                                  R2=0 (COUNT ALL) OR A(LAST DSPLY'D)          
PGIT     NTR1                                                                   
         XR    R3,R3               R3=COUNT                                     
         L     R4,AIO                                                           
         MVI   ELCODE,TASLELQ      LOOK FOR SOAP CYCLE ELEMENTS                 
         USING TASLD,R4                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PG10     BAS   RE,NEXTEL                                                        
         BNE   PG20                                                             
         AH    R3,=H'1'            ADD 1 TO COUNT                               
         LTR   R2,R2               IF SCANNING FOR LAST EL.                     
         BZ    PG10                                                             
         CLC   CLEPER,TASLSPD      TEST IF WE'VE REACHED IT                     
         BNE   PG10                                                             
         SPACE 1                                                                
PG20     LA    R1,CYCLINES-1                                                    
         AR    R1,R3               ADD N'LINES-1 FOR DIVIDE                     
         XR    R0,R0                                                            
         LA    R3,CYCLINES                                                      
         DR    R0,R3               DIVIDE BY N'LINES/SCREEN                     
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                MUST HAVE AT LEAST 1 PAGE                    
         STC   R1,BYTE             RETURN BYTE=PAGE NUMBER                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              HEADLINE HOOK FOR REPORT PRINTING                                
HDHOOK   NTR1                                                                   
         L     R5,ASPOOLD          R5=SPOOL DSECT                               
         USING SPOOLD,R5                                                        
         MVI   BYTE,C'H'           MOVE UPPER PORTION OF SCRN TO HEADS          
         GOTO1 PRTSCRN,DMCB,CONTAGH,SSGNCDN,H4                                  
         LA    R3,H7               HEADLINE FOR NEXT SET OF FIELDS              
         MVC   AIO,AIO2            SET IO AREA                                  
         OC    SSGNCDN,SSGNCDN     IF AGENT NAME                                
         BZ    HDHOOK10                                                         
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'8C',SSGNCDE),0 GET REC FOR ADDR           
         BNE   HDHOOK10                                                         
         XC    BLOCK(8),BLOCK      FAKE SCREEN HEADER                           
         MVI   BLOCK,128           L'ADDRESS+8                                  
         GOTO1 CHAROUT,DMCB,TAADELQ,BLOCK                                       
         OC    BLOCK+8(120),SPACES                                              
         LA    RE,4                PRINT INTO FOUR LINES                        
         LA    R1,BLOCK+8          THE AGENT ADDRESS                            
HDHOOK5  CLC   0(30,R1),SPACES     IF ADDRESS FOR THIS LINE                     
         BE    HDHOOK10                                                         
         MVC   51(30,R3),0(R1)     PRINT IT                                     
         LA    R1,30(R1)                                                        
         LA    R3,L'H1(R3)         BUMP TO NEXT HEADLINE                        
         BCT   RE,HDHOOK5          LOOP                                         
*                                                                               
HDHOOK10 MVC   AIO,AIO1            RESET IOAREA                                 
         GOTO1 PRTSCRN,DMCB,SSGAFLDH,SSGLL1H,(R3)                               
         MVI   BYTE,C'P'           RESET FOR LINE PRINTING                      
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
AMTINV   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     THEEND                                                           
         SPACE 1                                                                
ERRCRP   MVI   ERROR,ERGT1CRP      GT 1 CORP - NEED EXACT CODE                  
         B     THEEND                                                           
         SPACE 1                                                                
PGDSPMS2 CLI   PFAID,14            TEST DISPLAYED NEW PAGE                      
         BE    PLSENTER                                                         
PGDSPMSG MVI   MYMSGNO1,54         PAGE  X OF Y DISPLAYED                       
         L     R2,AFRSTKEY                                                      
         CLI   ACTNUM,ACTDIS                                                    
         BE    PGDSP2                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   *+12                                                             
         CLI   THISLSEL,C'S'                                                    
         BE    PGDSP2                                                           
         MVI   MYMSGNO1,55          ... - ENTER CHANGES AS DESIRED              
         L     R2,AFRSTREC                                                      
PGDSP2   MVI   BLOCK,2                                                          
         EDIT  THISPG,(1,BLOCK+1)                                               
         MVI   BLOCK+2,2                                                        
         EDIT  NPAGES,(1,BLOCK+3)                                               
         MVI   BLOCK+4,0                                                        
         B     INFEND                                                           
         SPACE 1                                                                
PLSENTER MVI   MYMSGNO1,2          PLEASE ENTER REQUIRED FIELDS                 
         MVI   MYMSYS,X'FF'                                                     
         LA    RE,SSGLL1H          CURSOR TO FIRST CYCLE LINE                   
         USING LINED,RE                                                         
         LA    R2,LAIRH                                                         
         B     INFEND                                                           
         DROP  RE                                                               
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
INFEND   OI    GENSTAT2,USGETTXT   SET INFO MESSAGE                             
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, EQUATES, ETC.                                         
         SPACE 2                                                                
PYMTBL   DS    0CL12                                                            
         DC    CL11'WEEK',AL1(TASGPWK)                                          
         DC    CL11'PERFORMANCE',AL1(TASGPPER)                                  
         DC    X'FF'                                                            
         SPACE 3                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'SGRT    ',CL8'LIST    '                               
PF13X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF14X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF24X-*,24,0,0,PFTSETPN)                                     
         DC    CL3' ',CL8'        ',CL8'REPORT  '                               
PF24X    EQU   *                                                                
         SPACE 1                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
CYCLINES EQU   12                  NUMBER OF CYCLE LINES                        
         SPACE 1                                                                
HEXFFS   DC    (L'TLSGGUA)X'FF'                                                 
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*        SPECS FOR REPORT                                                       
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,32,C'SOAP GUARANTEE'                                          
         SSPEC H2,32,C'--------------'                                          
         DC    X'00'                                                            
         EJECT                                                                  
*        DSECT TO COVER LOCAL SAVED STORAGE                                     
SOAPD    DSECT                                                                  
CLS      DS    0CL12               SOAP CYCLE KEYS                              
CLSPER   DS    XL6                 KEY OF FIRST ELEMENT DISPLAYED               
CLEPER   DS    XL6                 KEY OF LAST ELEMENT DISPLAYED                
*                                                                               
THISPG   DS    XL1                 CURRENT PAGE NUMBER                          
NPAGES   DS    XL1                 TOTAL N'PAGES                                
SVPMNT   DS    XL1                                                              
GUACODE  DS    CL(L'TLSGGUA)       DISPLAYABLE GUARANTEE CODE                   
SVKEY    DS    CL(L'KEY)                                                        
*                                                                               
W4IO     DS    CL4000              SAVED W4 RECORD                              
         EJECT                                                                  
*        DSECT TO COVER PAYMENT METHODS TABLE                                   
*                                                                               
PYMTBLD  DSECT                                                                  
PYMTTYP  DS    CL11                PAYMENT TYPE                                 
PYMTST   DS    XL1                 PAYMENT EQUATE FOR THIS TYPE                 
PYMTLNQ  EQU   (*-PYMTBLD)                                                      
         SPACE 2                                                                
*        DSECT TO COVER CYCLE LINE                                              
*                                                                               
LINED    DSECT                                                                  
         DS    CL8                                                              
         DS    CL1                 >                                            
LAIRH    DS    CL8                                                              
LAIR     DS    CL17                AIR CYCLE                                    
LNOTH    DS    CL8                                                              
LNOT     DS    CL8                 NOTICE DATE                                  
         DS    CL1                                                              
LPERFH   DS    CL8                                                              
LPERF    DS    CL9                 PERFORMANCE RATE                             
LGRTH    DS    CL8                                                              
LGRT     DS    CL6                 NUMBER OF EPISODES GUARANTEED                
LWEEKH   DS    CL8                                                              
LWEEK    DS    CL3                 NUMBER OF WEEKS                              
LAVGH    DS    CL8                                                              
LAVG     DS    CL9                 AVERAGE PAY                                  
LINELNQ  EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRCBD                                                       
         SPACE 3                                                                
         EJECT                                                                  
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
* DDPERVALD                                                                     
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022TAGENCB   10/24/12'                                      
         END                                                                    
