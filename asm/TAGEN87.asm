*          DATA SET TAGEN87    AT LEVEL 017 AS OF 01/24/14                      
*PHASE T70287A                                                                  
         TITLE 'T70287 - GUARANTEE TRACKING MANUAL ADJUSTMENTS'                 
T70287   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70287,R7                                                      
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
         CLI   TGCTSTTY,TASTTYPP   IF THIS IS NOT A PROGRAMMER                  
         BE    GT10                OR LEVEL 2, HIDE COMMENT PFKEY               
         CLI   TGCTSTTY,TASTTYP2                                                
         BE    GT10                                                             
         OI    SGTPFCMH+1,X'0C'    TURN COMMENT PFKEY TO LOW INTENSITY          
         OI    SGTPFCMH+6,X'80'                                                 
*                                                                               
GT10     BAS   RE,PFCMNT           IF PF16 HIT, SET UP FOR CMNT REC             
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
         MVC   SGTEPBD(13),=CL13'Prior Balance'                                 
         NI    SGTENBDH+1,X'FF'-X'0C'    REMOVE LOW INTENSITY                   
         NI    SGTENBLH+1,X'FF'-X'0C'    REMOVE LOW INTENSITY                   
         CLI   ACTNUM,ACTADD                                                    
         BNE   GT13                                                             
         MVC   SGTEPBD(13),=CL13'Balance      '                                 
         CLI   MODE,XRECADD        AFTER ADDING RECORD                          
         BE    GT13                HAVE TO SHOW IT                              
         OI    SGTENBDH+1,X'0C'    LOW INTENSITY                                
         OI    SGTENBLH+1,X'0C'    LOW INTENSITY                                
*                                                                               
GT13     OI    SGTEPBDH+6,X'80'                                                 
         OI    SGTENBDH+6,X'80'                                                 
         OI    SGTENBLH+6,X'80'                                                 
*                                                                               
GT15     CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,VALREC         BUILD RECORD                                 
         BNE   GT17                                                             
*                                                                               
         LA    R2,SGTEADAH                                                      
         TM    SGTEADAH+4,X'80'    NEW AMOUNT IN ONE OF THREE?                  
         BO    GT16                                                             
         TM    SGTESBAH+4,X'80'                                                 
         BO    GT16                                                             
         TM    SGTNAMTH+4,X'80'                                                 
         BO    GT16                                                             
         TM    SGTEADAH+4,X'20'    CAN'T BE PREVIOUSLY VALIDATED?               
         BZ    GT16                                                             
         TM    SGTESBAH+4,X'20'                                                 
         BZ    GT16                                                             
         TM    SGTNAMTH+4,X'20'                                                 
         BO    FLDMISS                                                          
*                                                                               
GT16     BAS   RE,BLDREC                                                        
         B     XIT                                                              
*                                                                               
GT17     CLI   MODE,RECDEL         DELETE RECORD                                
         BNE   GT18                                                             
         BAS   RE,DELETE                                                        
         B     XIT                                                              
*                                                                               
GT18     CLI   MODE,XRECADD        AFTER ADDING RECORD                          
         BNE   GT19                                                             
*                                                                               
         TM    STATUS,STPERCYC     ONLY FOR PER CYCLE AND                       
         BZ    GT18C                                                            
         CLI   SGTNPDH+5,0         MUST HAVE NEW PERIOD TO                      
         BE    GT18C                                                            
         BAS   RE,CASTPRIM         ADD $0 FTRACK                                
         BE    GT18C                                                            
         LA    R2,SGTGUAH                                                       
         B     MISSGRT                                                          
*                                                                               
GT18C    MVC   CONACT(7),=C'DISPLAY' SWITCH ACTION TO CHANGE                    
         OI    CONACTH+6,X'80'     FOR NEXT TIME IN                             
         MVC   SGTEPBD(13),=CL13'Prior Balance'                                 
         NI    SGTENBDH+1,X'FF'-X'0C'    REMOVE LOW INTENSITY                   
         NI    SGTENBLH+1,X'FF'-X'0C'    REMOVE LOW INTENSITY                   
         OI    SGTEPBDH+6,X'80'                                                 
         OI    SGTENBDH+6,X'80'                                                 
         OI    SGTENBLH+6,X'80'                                                 
         TM    STATUS,STPERCYC                                                  
         BZ    GT18K                                                            
         BRAS  RE,SETPCY                                                        
         B     GT18X                                                            
*                                                                               
GT18K    BRAS  RE,SETLOV                                                        
*                                                                               
GT18X    L     R3,AIO2                                                          
         MVI   ELCODE,TAGUELQ      SET TO GET GUAR. DETAILS EL.                 
         BAS   RE,GETEL                                                         
         BE    GT20                                                             
         DC    H'0'                                                             
*                                                                               
GT19     CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    GT20                                                             
         CLI   MODE,XRECPUT        AFTER CHANGING RECORD                        
         BNE   XIT                                                              
GT20     BAS   RE,DISPLAY          (RE-)DISPLAY IT                              
*                                                                               
         CLI   MODE,DISPREC        IF NOT JUST DISPLAYING                       
         BE    XIT                                                              
         BAS   RE,UPDATE           UPDATE GUARANTEE RECORD                      
         CLI   MODE,XRECADD        AFTER ADDING RECORD                          
         BNE   XIT                                                              
         MVI   SGTNPDH+5,0         CLEAR NEW PERIOD AND AMOUNT                  
         MVI   SGTNAMTH+5,0                                                     
         MVC   SGTNPD,SPACES                                                    
         MVC   SGTNAMT,SPACES                                                   
         OI    SGTNPHDH+6,X'80'                                                 
         OI    SGTNAMTH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         BAS   RE,INITSCRN         INITIALIZE SCREEN                            
*                                                                               
         XC    PREVCRD,PREVCRD                                                  
         TM    SGTPIDH+4,X'20'     TEST FIELD HAS CHANGED                       
         BO    *+8                                                              
         NI    SGTGUAH+4,X'DF'                                                  
         LA    R2,SGTPIDH                                                       
         TM    TGSYSTAT,TASYSPID   USING PID#?                                  
         BZ    VK1                                                              
         CLI   SGTPIDH+5,0                                                      
         BE    VK1A                                                             
         CLI   SGTPIDH+5,9         SSN ENTERED?                                 
         BE    VK1                                                              
         CLI   SGTPIDH+5,6         PID ENTERED?                                 
         BNE   FLDINV                                                           
         MVC   TGPID,SGTPID                                                     
VK1A     OC    TGPID,TGPID                                                      
         BZ    FLDMISS                                                          
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK1                                                              
         MVC   SGTPID,TGSSN                                                     
         MVI   SGTPIDH+5,9                                                      
*                                                                               
VK1      GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SGTPIDH),SGTPIDNH  S/S NUMBER         
         TM    TGSYSTAT,TASYSPID   USING PID#?                                  
         BZ    VK2                                                              
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SGTPID,SPACES                                                    
         MVC   SGTPID(L'TGPID),TGPID                                            
         MVI   SGTPIDH+5,L'TGPID                                                
         OI    SGTPIDH+6,X'80'                                                  
*                                                                               
VK2      LA    R2,SGTGUAH          VALIDATE GUARANTEE CODE                      
         TM    SGTGUAH+4,X'20'                                                  
         BO    VK2A                                                             
         TM    SGTEPDH+4,X'40'     WAS PERIOD ENTERED?                          
         BZ    VK2A                YES                                          
         MVC   SGTEPD,SPACES                                                    
         OI    SGTEPDH+6,X'80'                                                  
         MVI   SGTEPDH+5,0                                                      
         NI    SGTEPDH+4,X'DF'                                                  
*                                                                               
VK2A     CLI   5(R2),0             TEST SOMETHING INPUT                         
         BNE   VK4                                                              
         OC    TGGUA,TGGUA         NO - SOMETHING IN GLOBAL                     
         BZ    VK3                                                              
         MVC   8(4,R2),TGGUA       YES - MOVE TO FIELD                          
         XC    8(4,R2),HEXFFS      AND UNCOMPLEMENT IT                          
         OI    6(R2),X'80'                                                      
         B     VK5                                                              
*                                                                               
VK3      GOTO1 ANY                 REQUIRE INPUT                                
*                                                                               
VK4      MVC   TGGUA,8(R2)         CODE INPUT - MOVE TO GLOBAL AND              
         XC    TGGUA,HEXFFS        COMPLEMENT IT                                
*                                                                               
VK5      MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'30',0)  VALIDATE IT  (LOCK NOW)           
         BNE   THEEND                                                           
         OI    4(R2),X'20'                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R3,AIO2                                                          
         MVI   ELCODE,TAGUELQ      SET TO GET GUAR. DETAILS EL.                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGUD,R3            R3=A(GUARANTEE DETAILS EL.)                  
*                                                                               
         MVC   PRICOMM,TAGUCOM     SAVE FOR LATER                               
         NI    STATUS,X'FF'-STPERCYC                                            
         OC    TAGUCOM,TAGUCOM     IF PRIMARY COMMERCIAL DEFINED                
         BZ    VK8                                                              
         OI    STATUS,STPERCYC                                                  
         BRAS  RE,SETPCY           SET PER CYCLE GUARANTEE SCREEN               
         BRAS  RE,VALPD            RECOGNIZE INPUT TO PERIOD FIELD              
         BNE   VK6                                                              
         L     R1,TGELEM           R1=A(CORRESPONDING GUAR CYCLE EL.)           
         MVC   TAGUPD,TAGCPD-TAGCD(R1)  SET PERIOD IN GUAR DETAILS EL.          
         MVC   TAGUAMT,TAGCAMT-TAGCD(R1)    AMOUNT                              
         MVC   TAGUBAL,TAGCBAL-TAGCD(R1)    BALANCE                             
         BAS   RE,GUDTLS           DISPLAY GUARANTEE DETAILS                    
*                                                                               
VK6      BAS   RE,VALNPD           VALIDATE NEW PERIOD                          
         B     VK10                                                             
*                                                                               
VK8      BRAS  RE,SETLOV           SET LARGE OVERSCALE GUARANTEE SCREEN         
*                                                                               
         CLI   ACTNUM,ACTDEL       NOT FIXED CYCLE - DELETE NOT ALLOWED         
         BE    ACTINV                                                           
         LA    R2,SGTNPDH          INPUT NOT ALLOWED IN NEW PERIOD              
         CLI   5(R2),0                                                          
         BNE   NOINPUT                                                          
         BAS   RE,GUDTLS           DISPLAY GUARANTEE DETAILS                    
*                                                                               
VK10     LA    R4,KEY              BUILD KEY                                    
         USING TLGTD,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TLGTCD,TLGTCDQ      RECORD CODE                                  
         MVC   TLGTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLGTGUA,TGGUA       GUARANTEE CODE                               
         XC    TLGTGUA,HEXFFS      (NOT COMPLEMENTED)                           
         OC    TAGUCOM,TAGUCOM     IF COMMERCIAL IS DEFINED                     
         BZ    *+16                                                             
         MVC   TLGTSTRT(6),GUARPD  GUARANTEE PERIOD                             
         XC    TLGTSTRT(6),HEXFFS  (COMPLEMENTED)                               
         MVC   TLGTTRK,NEXTTRK     NEXT TRACKING NUMBER (COMPLEMENTED)          
*                                                                               
         CLI   ACTNUM,ACTADD       FINISHED IF ADDING                           
         BE    VKX                                                              
         MVC   TLGTTRK,LASTTRK     ELSE SET LAST TRACKING NUMBER (COMP)         
*                                                                               
         CLI   ACTNUM,ACTDEL       FINISHED IF DELETING                         
         BE    VKX                                                              
         GOTO1 HIGH                                                             
         CLC   TLGTKEY,KEYSAVE     GET LAST TRACKING RECORD                     
         BNE   NOTADJ                                                           
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,TAGTELQ      GET GUAR. TRACKING EL.                       
         BAS   RE,GETEL                                                         
         BNE   VKX                 IF NONE, OK TO CHANGE                        
         USING TAGTD,R3                                                         
         OC    TAGTINV,TAGTINV     IF THERE'S AN INVOICE NUMBER                 
         BNZ   NOTADJ              NOT VALID TO CHANGE ACTUAL PAYMENTS          
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE INITIALIZES SCREEN                                       
         SPACE 1                                                                
INITSCRN NTR1                                                                   
         XC    SGTHEAD,SGTHEAD     CLEAR HEADER FIELD                           
         GOTO1 FLDVAL,DMCB,(2,SGTHEADH),SGTPFCMH                                
         GOTO1 FLDVAL,DMCB,(8,SGTNPHDH),(8,SGTNCMTH)                            
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE VALIDATES NEW PERIOD FIELD                               
*---------------------------------------------------------------------          
VALNPD   NTR1                                                                   
         LA    R2,SGTNPDH          R2=A(NEW PERIOD FIELD)                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         CLI   ACTNUM,ACTADD       MUST BE ACTION ADD                           
         BNE   NOINPUT                                                          
                                                                                
         USING TLCAPD,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVI   TLCAPCD,TLCAGCDQ     X'81' (EMPLOYEE'S COMMLS)                   
         MVC   TLCAGSSN,TGSSN       GRT SS#                                     
         MVC   TLCAGGUA,TGGUA                                                   
         XC    TLCAGGUA,=X'FFFFFFFF'                                            
         MVC   TLCAGCOM,PRICOMM                                                 
         GOTO1 HIGH                                                             
         CLC   TLCAPKEY(TLCAGCAT-TLCAPD),KEYSAVE                                
         BE    VNPD1                                                            
         LA    R2,SGTGUAH                                                       
         B     MISSGRT                                                          
*                                                                               
VNPD1    LA    R3,ELEMENT                                                       
         USING TAGTD,R3            R3=A(GUARANTEE TRACKING ELEMENT)             
*                                                                               
         LA    R4,BLOCK            R4=A(PERVAL BLOCK)                           
         GOTO1 PDVAL,DMCB,(R4)     VALIDATE PERIOD                              
         USING PERVALD,R4                                                       
         MVC   TAGTSTRT(6),PVALPSTA  SAVE PWOS CYCLE DATES                      
         MVC   GUARPD,PVALPSTA                                                  
         MVC   TAGTAGY,SGTNAGY                                                  
*                                                                               
         L     R3,AIO2                                                          
         MVI   ELCODE,TAGCELQ      INSURE THIS CYCLE DOESN'T OVERLAP            
         BRAS  RE,GETEL            ANY EXISTING CYCLES                          
         BNE   VNPD8                                                            
         USING TAGCD,R3            R3=A(GUARANTEE DETAILS EL.)                  
*                                                                               
VNPD2    CLC   GUARSTRT,TAGCSTRT   COMPARE NEW START TO EXISTING                
         BE    OVERLAP             ERROR IF SAME                                
         BH    VNPD4               SKIP IF LATER                                
         CLC   GUAREND,TAGCSTRT    IF EARLIER, THEN SO MUST END DATE            
         BNL   OVERLAP                                                          
         B     VNPD6               OK - LOOK FOR ANOTHER CYCLE                  
*                                                                               
VNPD4    CLC   GUARSTRT,TAGCEND    START BEGINS AFTER EXISTING START            
         BNH   OVERLAP             SO IT MUST START AFTER EXISTING END          
*                                                                               
VNPD6    BAS   RE,NEXTEL           LOOK FOR ANOTHER CYCLE ELEMENT               
         BE    VNPD2                                                            
VNPD8    DS    0H                                                               
*NPD8    LA    R2,SGTVERH          INSURE INPUT IS VERIFIED                     
*        GOTO1 ANY                                                              
*        CLI   WORK,C'Y'                                                        
*        BNE   FLDINV                                                           
         MVC   NEXTTRK,=X'FFFF'    SET NEXT AVAILABLE TRACKING NO.              
         XC    GUARBAL,GUARBAL     SET NO CURRENT BALANCE                       
         MVC   AIO,AIO1            RESTORE AIO                                  
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO DISPLAY GUARANTEE DETAILS                             
*---------------------------------------------------------------------          
         USING TAGUD,R3            R3=A(GUARANTEE DETAILS EL.)                  
GUDTLS   NTR1                                                                   
         CLC   SGTEAGY,SPACES                                                   
         BH    *+10                                                             
         MVC   SGTEAGY,TAGUAGY     AGENCY                                       
         OI    SGTEAGYH+6,X'80'                                                 
         XC    SGTEAGN,SGTEAGN     PRE-CLEAR NAME                               
         OI    SGTEAGNH+6,X'80'                                                 
         CLI   SGTEAGY,0           IT MAY NOT BE AROUND                         
         BE    GUD9                                                             
*                                                                               
         OC    SGTEAGY,SPACES                                                   
         CLC   SGTEAGY,TAGUAGY     AGENCY HAVE TO BE IN GRT RECORD              
         BE    GUD5                                                             
*                                                                               
         LA    R2,SGTEAGYH                                                      
         LR    R5,R3               SAVE ADDRESS OF TAGU ELEMENT                 
         L     R3,AIO2             POINT TO GRT RECORD                          
         USING TAVAD,R3                                                         
         MVI   ELCODE,TAVAELQ                                                   
         BAS   RE,GETEL                                                         
GUD3     BNE   FLDINV                                                           
         CLC   SGTEAGY,TAVAAGY                                                  
         BNE   GUD4                                                             
         LR    R3,R5               RESTORE ADDRESS OF TAGU ELEMENT              
         B     GUD5                                                             
*                                                                               
GUD4     BAS   RE,NEXTEL                                                        
         B     GUD3                                                             
*                                                                               
         USING TAGUD,R3                                                         
GUD5     GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',SGTEAGY),SGTEAGNH                     
         MVC   SGTNAGY,SGTEAGY                                                  
         MVC   SGTNAGN,SGTEAGN                                                  
*                                                                               
GUD9     GOTO1 DATCON,DMCB,(X'11',TAGUPD),(8,SGTEPD) PERIOD                     
         OI    SGTEPDH+6,X'80'                                                  
         MVC   GUARPD,TAGUPD       SAVE PERIOD                                  
*                                                                               
         EDIT  (4,TAGUAMT),(12,SGTEAMT),2,FLOAT=-               AMOUNT          
         OI    SGTEAMTH+6,X'80'                                                 
*                                                                               
         MVC   GUARBAL,TAGUBAL     SAVE BALANCE                                 
*                                                                               
         BAS   RE,DISPBAL          DISPLAY IT                                   
*                                                                               
         BAS   RE,GETTRK           GET LAST/NEXT TRACKING NUMBER                
         B     XIT                                                              
*                                                                               
DISPBAL  DS    0H                  DISPLAY BALANCE                              
         L     RF,GUARBAL          NEW BALANCE                                  
         S     RF,PREVCRD          MINUS CREDIT                                 
         ST    RF,PRIRBAL          PRIOR BALANCE                                
         EDIT  (4,PRIRBAL),(12,SGTEPBL),2,FLOAT=-         PRIOR BALANCE         
         OI    SGTEPBLH+6,X'80'                                                 
*                                                                               
         EDIT  (4,GUARBAL),(12,SGTENBL),2,FLOAT=-           NEW BALANCE         
         OI    SGTENBLH+6,X'80'                                                 
*                                                                               
         CLC   CONACT(3),=C'ADD'   SKIP IF ACTION ADD                           
         BER   RE                                                               
         NI    SGTENBLH+1,X'F3'    SET TO NORMAL INTENSITY                      
         TM    GUARBAL,X'80'       IF BALANCE IS NEGATIVE                       
         BZ    *+8                                                              
         OI    SGTENBLH+1,X'08'    SET TO HIGH INTENSITY                        
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE DETERMINES NEXT TRACKING NUMBER                          
         SPACE 1                                                                
         USING TAGUD,R3            R3=A(GUARANTEE DETAILS EL.)                  
GETTRK   NTR1                                                                   
         LA    R4,KEY              R4=A(KEY)                                    
         USING TLGTD,R4                                                         
         XC    KEY,KEY             BUILD INITIAL KEY                            
         MVI   TLGTCD,TLGTCDQ      RECORD CODE                                  
         MVC   TLGTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLGTGUA,TGGUA       GUARANTEE CODE                               
         XC    TLGTGUA,HEXFFS      (UN-COMPLEMENTED)                            
         OC    TAGUCOM,TAGUCOM     IF COMMERCIAL IS DEFINED                     
         BZ    *+16                                                             
         MVC   TLGTSTRT(6),TAGUPD  GUARANTEE PERIOD                             
         XC    TLGTSTRT(6),HEXFFS  (COMPLEMENTED)                               
         GOTO1 HIGH                GET LAST LOGICAL TRACKING RECORD             
         SPACE 1                                                                
         CLC   TLGTKEY(TLGTTRK-TLGTD),KEYSAVE  IF WE DIDN'T FIND IT             
         BE    *+10                                                             
         MVC   TLGTTRK,HEXFFS                  SET DUMMY LAST TRK NUMB.         
         SPACE 1                                                                
         LH    RF,TLGTTRK          LAST TRACKING NUMBER                         
         STH   RF,LASTTRK          SAVE IT                                      
         LCR   RF,RF               UN-COMPLEMENTED                              
         LA    RF,1(RF)            ADD 1                                        
         LCR   RF,RF               COMPLEMENT IT AGAIN                          
         STH   RF,NEXTTRK          SAVE NEXT NUMBER                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUILDS RECORD                                            
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         MVI   ELCODE,TAGTELQ      REMOVE EXISTING ELEMENT (IF AROUND)          
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     BUILD GUARANTEE TRACKING ELEMENT             
         LA    R3,ELEMENT                                                       
         USING TAGTD,R3            R3=A(GUARANTEE TRACKING ELEMENT)             
         SPACE 1                                                                
         LA    R2,SGTECYCH         CYCLE DATES                                  
         CLI   5(R2),0                                                          
         BE    BLDR3                                                            
         CLI   SGTNPDH+5,0         INVALID IF NEW PERIOD POPULATED              
         BNE   FLDINV                                                           
*                                                                               
         LA    R4,BLOCK            R4=A(RETURN BLOCK FROM PERVAL)               
         GOTO1 PDVAL,DMCB,(R4)     VALIDATE PERIOD                              
         USING PERVALD,R4                                                       
*                                                                               
         CLC   PVALPSTA,GUARSTRT   MAKE SURE PERIOD INSIDE OF RANGE             
         BL    BLDR1                                                            
         CLC   PVALPSTA,GUAREND                                                 
         BH    BLDR1                                                            
         B     BLDR2                                                            
*                                                                               
BLDR1    CLC   PVALPEND,GUARSTRT                                                
         BL    STDINV                                                           
         CLC   PVALPEND,GUAREND                                                 
         BH    STDINV                                                           
*                                                                               
BLDR2    MVC   TAGTSTRT(6),PVALPSTA  SAVE PWOS CYCLE DATES                      
         B     BLDR40                                                           
*                                                                               
BLDR3    MVC   TAGTSTRT(6),GUARPD    SAVE PWOS CYCLE DATES FROM NPD             
*                                                                               
BLDR40   LA    R2,SGTEAGYH                                                      
         CLI   SGTEAGYH+5,0                                                     
         BE    BLDR43                                                           
         CLI   SGTNPDH+5,0         INVALID IF NEW PERIOD POPULATED              
         BNE   FLDINV                                                           
BLDR43   MVC   TAGTAGY,SGTEAGY                                                  
         LA    R2,SGTEADAH         GUARANTEE CREDITS                            
         CLI   SGTEADAH+5,0                                                     
         BNE   BLDR45                                                           
         CLI   SGTESBAH+5,0        HAS TO BE AN AMOUNT IN ONE OF THREE          
         BNE   BLDR50                                                           
         CLI   SGTNAMTH+5,0                                                     
         BNE   BLDR50                                                           
         TM    STATUS,STPERCYC     PER CYCLE GRT,                               
         BZ    FLDMISS                                                          
         CLI   SGTNPDH+5,0         NEW PERIOD EMPTY                             
         BE    FLDMISS                                                          
         LA    R2,SGTNAMTH                                                      
         B     FLDMISS             OTHERWISE, ERROR                             
*                                                                               
BLDR45   TM    STATUS,STPERCYC     PER CYCLE GRT,                               
         BZ    BLDR48                                                           
         CLI   SGTNPDH+5,0         INVALID IF NEW PERIOD POPULATED              
         BNE   FLDINV                                                           
BLDR48   CLI   SGTESBAH+5,0        ONLY ONE AMOUNT ALLOWED                      
         BNE   AMTINAL             INPUT NOT ALLOWED                            
         ZIC   R4,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R4)                                          
         BAS   RE,BLDVAMT                                                       
         OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
BLDR50   LA    R2,SGTESBAH         SUBTRACT AMOUNT                              
         CLI   5(R2),0                                                          
         BE    BLDR60                                                           
         TM    STATUS,STPERCYC     PER CYCLE GRT,                               
         BZ    BLDR50A                                                          
         CLI   SGTNPDH+5,0         INVALID IF NEW PERIOD POPULATED              
         BNE   FLDINV                                                           
BLDR50A  CLI   SGTEADAH+5,0        ONLY ONE AMOUNT ALLOWED                      
         BNE   AMTINAL             INPUT NOT ALLOWED                            
         ZIC   R4,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R4)                                          
         BAS   RE,BLDVAMT                                                       
*                                                                               
         L     RF,TAGTCRD                                                       
         LCR   RF,RF               NEGATE IT                                    
         ST    RF,TAGTCRD                                                       
*                                                                               
BLDR60   OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SGTNAMTH         NEW AMOUNT                                   
         CLI   5(R2),0                                                          
         BE    BLDR70                                                           
         ZIC   R4,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R4)                                          
         BAS   RE,BLDVAMT                                                       
*                                                                               
BLDR70   OI    4(R2),X'20'         VALIDATED                                    
         L     R1,GUARBAL          PREVIOUS BALANCE                             
         A     R1,TAGTCRD          + NEW APPLIED CREDITS                        
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    *+8                                                              
         S     R1,PREVCRD          SUBTRACT PREV APPLIED CREDITS                
         ST    R1,GUARBAL          SAVE NEW BALANCE                             
         ST    R1,TAGTBAL                                                       
*                                                                               
         OC    ELEMENT,ELEMENT     ANYTHING IN ELEMENT                          
         BZ    BLDR80                                                           
         MVI   TAGTEL,TAGTELQ      YES - FINISH IT UP                           
         MVI   TAGTLEN,TAGTLNQ                                                  
         OC    TAGTSTRT(6),TAGTSTRT                                             
         BNZ   BLDR70X                                                          
         MVC   TAGTSTRT,GUARSTRT                                                
         MVC   TAGTEND,GUAREND                                                  
BLDR70X  GOTO1 ADDELEM             AND ADD IT                                   
*                                                                               
BLDR80   TM    STATUS,STPERCYC                                                  
         BZ    BLDR80Z                                                          
         LA    R2,SGTECMTH                                                      
         CLI   SGTECMTH+5,0                                                     
         BZ    BLDR80Z                                                          
         CLI   SGTNPDH+5,0         INVALID IF NEW PERIOD POPULATED              
         BNE   FLDINV                                                           
BLDR80Z  GOTO1 NAMIN,DMCB,(2,TACMELQ),(X'80',SGTECMTH),TACMTYPH COMMENT         
*                                                                               
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY INFO                            
         B     XIT                                                              
*                                                                               
*--------------------------------------------------------                       
BLDVAMT  CLI   0(R1),X'FF'                                                      
         BE    AMTINV                                                           
         OC    DMCB+4(4),DMCB+4    AMOUNT CANNOT BE $0                          
         BZ    FLDINV                                                           
         L     RF,DMCB+4                                                        
         LTR   RF,RF               OR NEGATIVE                                  
         BM    AMTINV                                                           
         CLC   DMCB+4(4),=F'99999999'                                           
         BH    AMTINV                                                           
BLDVAMT5 MVC   TAGTCRD,4(R1)                                                    
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE DISPLAYS RECORD                                          
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SGTECYCH            CLEAR THE SCREEN                             
*                                                                               
         XC    PREVCRD,PREVCRD                                                  
         L     R3,AIO                                                           
         MVI   ELCODE,TAGTELQ      GET GUAR. TRACKING EL.                       
         BAS   RE,GETEL                                                         
         BNE   DISP6               OK TO NOT BE AROUND                          
         USING TAGTD,R3            R3=A(GUARANTEE TRACKING EL.)                 
         SPACE 1                                                                
         OC    TAGTSTRT,TAGTSTRT   CYCLE DATES                                  
         BZ    DISP4                                                            
         GOTO1 DATCON,DMCB,(X'11',TAGTSTRT),(8,SGTECYC)                         
*                                                                               
DISP4    DS    0H                                                               
         MVC   SGTEAGY,TAGTAGY                                                  
         CLC   SGTEAGY,SPACES                                                   
         BNH   DISP5                                                            
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',SGTEAGY),SGTEAGNH                     
         MVC   AIO,AIO1                                                         
*                                                                               
DISP5    MVC   PREVCRD,TAGTCRD     SAVE CREDIT AMOUNT                           
*                                                                               
         L     RF,GUARBAL          NEW BALANCE                                  
         S     RF,TAGTCRD          MINUS CREDIT                                 
         ST    RF,PRIRBAL          PRIOR BALANCE                                
         EDIT  (4,PRIRBAL),(12,SGTEPBL),2,FLOAT=-         PRIOR BALANCE         
         OI    SGTEPBLH+6,X'80'                                                 
*                                                                               
         L     RF,TAGTCRD                                                       
         LA    R2,SGTEADA                                                       
         TM    TAGTCRD,X'80'                                                    
         BZ    DISP5A                                                           
         LCR   RF,RF                                                            
         LA    R2,SGTESBA                                                       
DISP5A   EDIT  (RF),(12,(R2)),2,FLOAT=-,ZERO=BLANK                              
         OI    SGTEADAH+4,X'20'                                                 
         OI    SGTESBAH+4,X'20'                                                 
         OI    SGTNAMTH+4,X'20'                                                 
*                                                                               
DISP6    GOTO1 CHAROUT,DMCB,TACMELQ,SGTECMTH,TACMTYPH  COMMENT                  
         GOTO1 ACTVOUT,DMCB,SGTLCHGH                                            
*                                                                               
         CLI   MODE,XRECADD        AFTER ADDING RECORD                          
         BNE   XIT                                                              
*        L     R3,AIO2                                                          
*        MVI   ELCODE,TAGUELQ      SET TO GET GUAR. DETAILS EL.                 
*        BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        BAS   RE,GUDTLS                                                        
         OC    TAGTSTRT,TAGTSTRT   CYCLE DATES                                  
         BZ    DISP10                                                           
         GOTO1 DATCON,DMCB,(X'11',TAGTSTRT),(8,SGTEPD)                          
         OI    SGTEPDH+6,X'80'                                                  
*                                                                               
DISP10   L     RF,GUARBAL          NEW BALANCE                                  
         S     RF,TAGTCRD          MINUS CREDIT                                 
         ST    RF,PRIRBAL          PRIOR BALANCE                                
         EDIT  (4,PRIRBAL),(12,SGTEPBL),2,FLOAT=-         PRIOR BALANCE         
         OI    SGTEPBLH+6,X'80'                                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE UPDATES GUARANTEE RECORD BASED ON MANUAL ADJ.            
         SPACE 1                                                                
UPDATE   NTR1                                                                   
         L     R3,AIO2             READ GUAR RECORD FOR UPDATE                  
         ST    R3,AIO                                                           
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'30',0)                                    
         SPACE 1                                                                
         MVI   ELCODE,TAGUELQ      GET GUAR. DETAILS EL.                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGUD,R3            R3=A(GUARANTEE DETAILS EL.)                  
         SPACE 1                                                                
         OC    TAGUCOM,TAGUCOM     IF WE DON'T HAVE COMMERCIAL                  
         BNZ   *+14                                                             
         MVC   TAGUBAL,GUARBAL     UPDATE BALANCE HERE                          
         B     UPD8                                                             
         SPACE 1                                                                
         CLI   SGTNPDH+5,0         IF ADDING NEW PERIOD                         
         BE    UPD6                                                             
         XC    ELEMENT,ELEMENT     BUILD NEW GUAR. CYCLE EL.                    
         LA    R3,ELEMENT                                                       
         USING TAGCD,R3                                                         
         MVI   TAGCEL,TAGCELQ                                                   
         MVI   TAGCLEN,TAGCLNQ                                                  
         MVC   TAGCPD,GUARPD       SET NEW PERIOD DATES                         
         MVC   TAGCAMT,GUARBAL     SET AMOUNT                                   
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
UPD6     MVI   ELCODE,TAGCELQ      ELSE LOOK UP GUAR CYCLE EL.                  
         GOTO1 GETL,DMCB,(3,GUARSTRT)                                           
         BE    *+6                                                              
         DC    H'0'                VANISHED                                     
         L     R3,TGELEM                                                        
         USING TAGCD,R3            R3=A(GUAR CYCLE EL.)                         
         MVC   TAGCBAL,GUARBAL     UPDATE BALANCE HERE                          
         SPACE 1                                                                
UPD8     BAS   RE,DISPBAL          RE-DISPLAY IT                                
         SPACE 1                                                                
         GOTO1 PUTREC              WRITE IT BACK FROM I/O 2                     
         MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DELETE A FIXED CYCLE GUARANTEE                        
         SPACE 1                                                                
DELETE   NTR1                                                                   
         TM    STATUS,DELPEND      IS DELETE PENDING                            
         BZ    PFTODEL             NO, SO SET NOW                               
         SPACE 1                                                                
         CLI   PFAID,20            TEST IT WAS PRESSED                          
         BNE   PFTODEL                                                          
         SPACE 1                                                                
         MVC   AIO,AIO2            SET I/O=A(GUARANTEE RECORD)                  
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'30',0)  GET THE RECORD                    
         SPACE 1                                                                
         MVI   ELCODE,TAGCELQ                                                   
         GOTO1 GETL,DMCB,(6,GUARPD)  INSURE GUAR. CYCLE EL. AROUND              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DELL,DMCB,(6,GUARPD)  DELETE IT                                  
         SPACE 1                                                                
         GOTO1 PUTREC              WRITE BACK THE RECORD                        
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         LA    R4,KEY              BUILD KEY FOR TRACKING RECORDS               
         USING TLGTD,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TLGTCD,TLGTCDQ      RECORD CODE                                  
         MVC   TLGTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLGTGUA,TGGUA       GUARANTEE CODE                               
         XC    TLGTGUA,HEXFFS      (NOT COMPLEMENTED)                           
         MVC   TLGTSTRT(6),GUARPD  GUARANTEE PERIOD                             
         XC    TLGTSTRT(6),HEXFFS  (COMPLEMENTED)                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                GET FIRST RECORD                             
         SPACE 1                                                                
DEL4     CLC   TLGTKEY(TLGTTRK-TLGTD),KEYSAVE  TEST CORRECT GUARANTEE           
         BNE   DELETED                                                          
         SPACE 1                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              GET THE RECORD                               
         L     R4,AIO                                                           
         OI    TLGTSTAT,X'80'      DELETE FILE RECORD                           
         GOTO1 PUTREC                                                           
         SPACE 1                                                                
         LA    R4,KEY                                                           
         OI    TLDRSTAT-TLDRD(R4),X'80'  DELETE DIRECTORY RECORD                
         GOTO1 WRITE                                                            
         SPACE 1                                                                
         OI    DMINBTS,X'08'       RE-READ RECORD I JUST DELETED                
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         SPACE 1                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                 LOOK FOR ANOTHER                             
         B     DEL4                                                             
         EJECT                                                                  
*              ROUTINE TO SET UP COMMENT SCREEN IF PF16 HIT                     
PFCMNT   NTR1                                                                   
         CLI   PFAID,16                                                         
         BNE   PFCMNTX                                                          
         MVI   COMTYPG,TLCMTGUA    COMMENT TYPE G (PF16)                        
         MVI   SVRECUSE,GT         SAVE WHERE WE CAME FROM                      
PFCMNTX  B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 1                                                                
ACTINV   LA    R2,CONACTH          INVALID ACTION                               
         MVI   ERROR,INVACT                                                     
         B     THEEND                                                           
         SPACE 1                                                                
DELETED  MVI   MYMSGNO1,7          RECORD HAS BEEN DELETED                      
         MVI   MYMSYS,X'FF'                                                     
         B     INFEND                                                           
         SPACE 1                                                                
PFTODEL  OI    STATUS,DELPEND      SET DELETE PENDING                           
         LA    R2,CONACTH                                                       
         MVI   MYMSGNO1,56         PRESS PF20 TO CONFIRM DELETE                 
         B     INFEND                                                           
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
NOTADJ   MVI   ERROR,ERNOTADJ      ONLY MANUAL ADJS. ALLOWED                    
         B     THEEND                                                           
         SPACE 1                                                                
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED IN THIS FIELD              
         B     THEEND                                                           
         SPACE 1                                                                
AMTINV   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     THEEND                                                           
*                                                                               
STDINV   MVI   ERROR,ERINVSDT      INVALID START DATE                           
         B     THEEND                                                           
*                                                                               
ENDINV   MVI   ERROR,ERINVEDT      INVALID END DATE                             
         B     THEEND                                                           
*                                                                               
MISSGRT  MVI   ERROR,ERMISGRT      MISSING GRT CODE                             
         B     THEEND                                                           
*                                                                               
AMTINAL  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
*&&DO                                                                           
ERRAGY   MVC   MYMSGNO,=Y(ERAGYNFD)   AGENCY NOT FOUND                          
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         NI    STEOPTH+4,X'DF'     SET TO RE-START W/NEXT HIT OF ENTER          
         LA    R2,STEKEYH                                                       
         B     THEEND                                                           
*&&                                                                             
OVERLAP  MVI   ERROR,ERPDOVLP      NEW PERIOD OVERLAPS WITH EXISTING            
         B     THEEND                                                           
         SPACE 1                                                                
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
*                                                                               
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'GRT     ',CL8'LIST    '                               
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'GRT     ',CL8'DISPLAY '                               
PF14X    EQU   *                                                                
*                                                                               
         DC    AL1(PF15X-*,15,0,0,0)                                            
         DC    CL3' ',CL8'GTRACK  ',CL8'REPORT  '                               
PF15X    EQU   *                                                                
*                                                                               
         DC    AL1(PF16X-*,16,0,0,0)                                            
         DC    CL3' ',CL8'GRTCMT  ',CL8'DISPLAY '                               
PF16X    EQU   *                                                                
*                                                                               
         DC    AL1(PF20X-*,20,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF20X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
HEXFFS   DC    6X'FF'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
* FIND GRT'S CAST ON PRIMARY COMML                                              
*---------------------------------------------------------------------          
CASTPRIM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TLCAPD,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVI   TLCAPCD,TLCAGCDQ     X'81' (EMPLOYEE'S COMMLS)                   
         MVC   TLCAGSSN,TGSSN       GRT SS#                                     
         MVC   TLCAGGUA,TGGUA                                                   
         XC    TLCAGGUA,=X'FFFFFFFF'                                            
         MVC   TLCAGCOM,PRICOMM                                                 
         GOTO1 HIGH                                                             
         CLC   TLCAPKEY(TLCAGCAT-TLCAPD),KEYSAVE                                
         BNE   CPRIMNO                                                          
*                                                                               
         MVC   CASTSQ,TLCAGSEQ      SAVE CAST SEQUENCE                          
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         OI    WHENOK,X'01'         CHANGE SO SAVPTR WILL WORK                  
         GOTO1 SAVPTRS,DMCB,PTRBLK  SAVE PASSIVE POINTERS                       
         BAS   RE,ADD0TACR          ADD $0 HLD TACR INTO CAST RECORD            
         GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 ADDPTRS,DMCB,PTRBLK  UPDATE PASSIVE POINTERS                     
*                                                                               
         BAS   RE,ADD0FTRK          ADD $0 FTRACK                               
         MVC   AIO,AIO1             RESTORE AIO                                 
*                                                                               
CPRIMYES XR    RE,RE                                                            
CPRIMNO  LTR   RE,RE                                                            
CPRIMX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ADD $0 HLD TACR                                                  
*---------------------------------------------------------------------          
         USING TACRD,R4                                                         
ADD0TACR NTR1                      ADD $0 HLD TACR                              
*                                                                               
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TACREL,TACRELQ                                                   
         MVI   TACRLEN,TACRLNQ                                                  
         MVC   TACRSTRT(6),GUARPD  SAVE NEW PERIOD                              
         OI    TACRSTAT,TACRSTRK   TRACKING ENABLED                             
         MVC   TACRUSE,=C'HLD'                                                  
         GOTO1 ADDELEM                                                          
         B     CPRIMX                                                           
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ADD $0 FTRACK                                                    
*---------------------------------------------------------------------          
ADD0FTRK NTR1                      ADD $0 FTRACK                                
*                                                                               
         USING TLFTD,R4            BUILD THE FTRACK KEY                         
         LA    R4,KEY                                                           
         XC    TLFTKEY,TLFTKEY                                                  
         MVI   TLFTCD,TLFTCDQ                                                   
         MVC   TLFTSSN,TGSSN                                                    
         MVC   TLFTCOM,PRICOMM                                                  
         MVC   TLFTCAST,CASTSQ     CAST INPUT SEQUENCE NUMBER                   
         MVC   TLFTSTRT(6),GUARPD                                               
         XC    TLFTSTRT(6),=X'FFFFFFFFFFFF'                                     
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                GET LAST LOGICAL TRACKING RECORD             
*                                                                               
         CLC   TLFTKEY(TLFTTRK-TLFTD),KEYSAVE  IF KEYS MATCH                    
         BNE   A0FT010                                                          
         LH    RF,TLFTTRK          LAST TRACKING NUMBER                         
         AHI   RF,-1               COMPLEMENTED, ADD 1                          
         STH   RF,TLFTTRK                                                       
         XC    TLFTINV,TLFTINV     CLEAR INVOICE NUMBER                         
         B     A0FT020                                                          
*                                                                               
A0FT010  MVC   KEY,KEYSAVE                                                      
         MVC   TLFTTRK,=X'FFFE'    TRACKING NUMBER 1, COMPLEMENTED              
*                                                                               
A0FT020  L     RE,AIO              CLEAR AIO3                                   
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         L     R4,AIO                                                           
         MVC   0(TLFTINV-TLFTD,R4),KEY                                          
*                                                                               
         USING TAGTD,R4            ADD GUARANTEE TRACKING ELEMENT               
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         MVI   TAGTEL,TAGTELQ                                                   
         MVI   TAGTLEN,TAGTLNQ                                                  
         MVC   TAGTUSE,=C'HLD'                                                  
         MVC   TAGTSTRT(6),GUARPD                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY INFO                            
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         B     CPRIMX                                                           
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE VALIDATES PERIOD FIELD                                   
*---------------------------------------------------------------------          
VALPD    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SGTEPDH          R2=A(PERIOD FIELD)                           
         TM    4(R2),X'20'                                                      
         JO    *+8                                                              
         NI    STATUS,X'FF'-DELPEND                                             
*                                                                               
         CLI   5(R2),0             IF NOTHING INPUT                             
         JNE   VP1                                                              
         CLI   SGTNPDH+5,0         AND NO NEW PERIOD INPUT                      
         JE    VP2                 LOOK FOR LATEST CYCLE                        
VP0      OI    4(R2),X'20'                                                      
         J     NO                  ELSE RETURN CC NE                            
*                                                                               
VP1      TM    STATUS,STPERCYC     PER CYCLE?                                   
         JZ    VP1A                                                             
         CLI   SGTNPDH+5,0         NEW PERIOD HAS INPUT?                        
         JNE   VP0                 YES, IGNORE EXISTING PERIOD                  
*                                                                               
VP1A     LA    R4,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R4)     VALIDATE PERIOD                              
         USING PERVALD,R4          R4=A(PERVAL BLOCK)                           
*                                                                               
VP2      L     R3,AIO2             R3=A(RECORD)                                 
         XR    R0,R0                                                            
         MVI   ELCODE,TAGCELQ      SET TO LOOK UP GUAR CYCLE EL.                
         BRAS  RE,GETEL                                                         
         BNE   NOTRK                                                            
         USING TAGCD,R3                                                         
VP4      LR    R0,R3               SAVE A(LAST ONE FOUND)                       
         CLI   5(R2),0             IF NO INPUT SCAN UNTIL END                   
         JE    VP6                                                              
         CLC   PVALPSTA,TAGCSTRT   ELSE MATCH ON START DATE                     
         JE    VP8                                                              
VP6      BRAS  RE,NEXTEL           TRY NEXT ELEMENT                             
         JE    VP4                                                              
         CLI   5(R2),0             NO MORE - OK IF NO PD INPUT                  
         JNE   NOTRK                                                            
         LTR   R3,R0               USE LAST ELEMENT FOUND                       
         JZ    NOTRK                                                            
*                                                                               
VP8      GOTO1 DATCON,DMCB,(X'11',TAGCSTRT),(8,8(R2))  DISPLAY DATES            
         OI    6(R2),X'80'                                                      
*                                                                               
         ST    R3,TGELEM           RETURN A(ELEMENT)                            
         OI    4(R2),X'20'                                                      
         J     YES                                                              
*                                                                               
NOTRK    MVI   ERROR,ERNOTRK       NO TRACKING RECORDS FOUND                    
         J     THEEND                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE SETS UP SCREEN FOR PER CYCLE GUARANTEES                  
*---------------------------------------------------------------------          
SETPCY   NTR1  BASE=*,LABEL=*                                                   
         MVC   SGTHEAD,PCYHEAD     SET PER CYCLE HEADER                         
         GOTO1 FLDVAL,DMCB,SGTNPHDH,(X'10',SGTNCMTH)                            
         NI    SGTNPDH+1,X'DF'                                                  
         NI    SGTNAMTH+1,X'DF'                                                 
         NI    SGTNCMTH+1,X'DF'                                                 
         GOTO1 FLDVAL,DMCB,(X'80',SGTEPDH),(1,SGTNCMTH)                         
         NI    SGTEPDH+1,X'FF'-X'20'   UNPROTECTED                              
         OI    SGTEPDH+6,X'80'     TRANSMIT                                     
*                                                                               
         CLC   CONACT(3),=C'ADD'         MUST BE ACTION ADD                     
         BNE   SETCPY1                                                          
         NI    SGTNPHDH+1,X'FF'-X'0C'    REMOVE LOW INTENSITY                   
         NI    SGTNPDH+1,X'FF'-X'2C'                                            
         NI    SGTNAMHH+1,X'FF'-X'0C'                                           
         NI    SGTNAMTH+1,X'FF'-X'2C'                                           
         NI    SGTNAGHH+1,X'FF'-X'0C'                                           
         NI    SGTNAGYH+1,X'FF'-X'0C'                                           
         NI    SGTNAGNH+1,X'FF'-X'0C'                                           
         NI    SGTNCMHH+1,X'FF'-X'0C'                                           
         NI    SGTNCMTH+1,X'FF'-X'2C'                                           
         B     SETCPY2                                                          
*                                                                               
SETCPY1  OI    SGTNPHDH+1,X'0C'     LOW INTENSITY                               
         OI    SGTNPDH+1,X'2C'                                                  
         OI    SGTNAMHH+1,X'0C'                                                 
         OI    SGTNAMTH+1,X'2C'                                                 
         OI    SGTNAGHH+1,X'0C'                                                 
         OI    SGTNAGYH+1,X'0C'                                                 
         OI    SGTNAGNH+1,X'0C'                                                 
         OI    SGTNCMHH+1,X'0C'                                                 
         OI    SGTNCMTH+1,X'2C'                                                 
*                                                                               
SETCPY2  OI    SGTNPHDH+6,X'80'     TRANSMIT                                    
         OI    SGTNPDH+6,X'80'                                                  
         OI    SGTNAMHH+6,X'80'                                                 
         OI    SGTNAMTH+6,X'80'                                                 
         OI    SGTNAGHH+6,X'80'                                                 
         OI    SGTNAGYH+6,X'80'                                                 
         OI    SGTNAGNH+6,X'80'                                                 
         OI    SGTNCMHH+6,X'80'                                                 
         OI    SGTNCMTH+6,X'80'                                                 
*                                                                               
         XIT1                                                                   
*                                                                               
PCYHEAD  DC    CL(L'SGTHEAD)'Per Cycle Guarantee'                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE SETS UP SCREEN FOR LARGE OVERSCALE GUARANTEES            
*---------------------------------------------------------------------          
SETLOV   NTR1  BASE=*,LABEL=*                                                   
         MVC   SGTHEAD,LOVHEAD     SET LARGE OVERSCALE HEADER                   
         MVI   SGTNPDH+5,0                                                      
         XC    SGTNPD,SGTNPD                                                    
         MVI   SGTNAMTH+5,0                                                     
         XC    SGTNAMT,SGTNAMT                                                  
         MVI   SGTNAGYH+5,0                                                     
         XC    SGTNAGY,SGTNAGY                                                  
         MVI   SGTNCMTH+5,0                                                     
         XC    SGTNCMT,SGTNCMT                                                  
         GOTO1 FLDVAL,DMCB,(X'80',SGTEPDH),(1,SGTNCMTH)                         
         OI    SGTEPDH+1,X'20'     PROTECTED                                    
         OI    SGTEPDH+6,X'80'     TRANSMIT                                     
*                                                                               
         OI    SGTNPHDH+1,X'0C'     LOW INTENSITY                               
         OI    SGTNPDH+1,X'0C'                                                  
         OI    SGTNAMHH+1,X'0C'                                                 
         OI    SGTNAMTH+1,X'0C'                                                 
         OI    SGTNAGHH+1,X'0C'                                                 
         OI    SGTNAGYH+1,X'0C'                                                 
         OI    SGTNAGNH+1,X'0C'                                                 
         OI    SGTNCMHH+1,X'0C'                                                 
         OI    SGTNCMTH+1,X'0C'                                                 
*                                                                               
         OI    SGTNPHDH+6,X'80'     TRANSMIT                                    
         OI    SGTNPDH+6,X'80'                                                  
         OI    SGTNAMHH+6,X'80'                                                 
         OI    SGTNAMTH+6,X'80'                                                 
         OI    SGTNAGHH+6,X'80'                                                 
         OI    SGTNAGYH+6,X'80'                                                 
         OI    SGTNAGNH+6,X'80'                                                 
         OI    SGTNCMHH+6,X'80'                                                 
         OI    SGTNCMTH+6,X'80'                                                 
*                                                                               
         XIT1                                                                   
*                                                                               
LOVHEAD  DC    CL(L'SGTHEAD)'Large Overscale Guarantee'                         
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR87D                                                       
*                                                                               
PRIRBAL  DS    F                   PRIOR BALANCE                                
GUARBAL  DS    F                   GUARANTEE BALANCE                            
PREVCRD  DS    F                   PREV CREDITS TAKEN THIS TRACKING REC         
LASTTRK  DS    H                   LAST TRACKING NUMBER (COMPLEMENTED)          
NEXTTRK  DS    H                   NEXT TRACKING NUMBER (COMPLEMENTED)          
GUARPD   DS    0PL6                GUARANTEE PERIOD                             
GUARSTRT DS    PL3                                                              
GUAREND  DS    PL3                                                              
STATUS   DS    XL1                                                              
DELPEND  EQU   X'80'               DELETE PENDING                               
STPERCYC EQU   X'40'               PER CYCLE GRT, LARGE SCALE IF OFF            
COMTYPG  DS    CL1                 COMMENT RECORD TYPE G                        
PRICOMM  DS    XL4                 PRIMARY COMMERCIAL                           
CASTSQ   DS    XL2                 CAST SEQUENCE                                
PTRBLK   DS    CL(5*L'TLDRREC+1)                                                
         EJECT                                                                  
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
* PERVALD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
         ORG   TWAHOLE                                                          
SVRECUSE DS    CL1                 SAVED REC/USE                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017TAGEN87   01/24/14'                                      
         END                                                                    
