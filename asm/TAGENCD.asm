*          DATA SET TAGENCD    AT LEVEL 013 AS OF 02/16/07                      
*PHASE T702CDA,*                                                                
         TITLE 'T702CD - EPISODE COMMENT ON CHECK DISPLAY'                      
T702CD   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702CD                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
         TM    TGSYSTAT,TASYSPID                                                
         BZ    *+14                                                             
         MVC   SCHSHED(7),=C'Pid Num'                                           
         OI    SCHSHEDH+6,X'80'                                                 
         SPACE 3                                                                
         CLI   MODE,SETFILE        SET ALTERNATE FILE                           
         BNE   *+12                                                             
         BAS   RE,SETCHK           SET FILE OVERRIDES                           
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   XIT                                                              
         BAS   RE,DISPLAY                                                       
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,SCHCHKH          VALIDATE CHECK NUMBER                        
         SPACE 1                                                                
         CLI   5(R2),0             IF NOTHING INPUT                             
         BNE   VK10                                                             
         OC    TGCHK,TGCHK         AND THERE'S A GLOBAL NUMBER                  
         BZ    VK10                                                             
         MVC   8(L'TGCHK,R2),TGCHK  USE IT                                      
         OI    6(R2),X'80'                                                      
         MVI   5(R2),L'TGCHK                                                    
         SPACE 1                                                                
VK10     GOTO1 ANY                 INPUT REQUIRED                               
         CLI   5(R2),L'TGCHK       INSURE FULL CHECK NUMBER INPUT               
         BNE   FLDINV                                                           
         MVC   TGCHK,WORK          SAVE UNCOMPLEMENTED IN GLOBAL                
         SPACE 1                                                                
         LA    R3,KEY              R3 = A(CHECK KEY)                            
         USING TLCKPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ    RECORD CODE                                  
         MVC   TLCKCCHK,TGCHK      CHECK NUMBER                                 
         XC    TLCKCCHK,HEXFFS     COMPLEMENTED                                 
         SPACE 1                                                                
         BAS   RE,SETCHK           SET FILE OVERRIDES                           
         GOTO1 HIGH                READ FOR KEY                                 
         SPACE 1                                                                
         CLC   TLCKPKEY(TLCKCBNK-TLCKPD),KEYSAVE                                
         BNE   NTFOUND                                                          
         SPACE 1                                                                
         GOTO1 GETREC              READ RECORD                                  
         SPACE 1                                                                
         BAS   RE,LIMITCHK         CHECK LIMIT ACCESS                           
         BNE   NTFOUND                                                          
         SPACE 1                                                                
         L     R4,AIO              GET PAYMENT DETAILS ELEMENT                  
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         TM    TAPDADJS,TAPDADVD   IF CHECK IS A VOID CHECK                     
         BZ    VKX                                                              
         GOTO1 SEQ                 THEN BUMP TO NORMAL CHECK                    
         CLC   TLCKPKEY(TLCKCBNK-TLCKPD),KEYSAVE                                
         BE    VKX                                                              
         MVC   KEY,KEYSAVE         IF CAN'T FIND (I.E. ORIGINAL PURGED)         
         GOTO1 HIGH                SHOW VOID                                    
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK AGENCY AND CLIENT LIMIT ACCESS                  
         SPACE 1                                                                
LIMITCHK NTR1                                                                   
         LHI   R2,1                                                             
*                                                                               
         USING TLCKD,R4                                                         
         L     R4,AIO                                                           
         MVC   CHKAGY,TLCKAGY      INITIALIZE CHECK'S AGENCY                    
         XC    CHKCLI,CHKCLI       AND CLIENT                                   
         DROP  R4                                                               
*                                                                               
         USING TAOID,R4                                                         
         MVI   ELCODE,TAOIELQ      SAVE CHECK'S AGENCY                          
         BRAS  RE,GETEL                                                         
         BNE   LIMCHK10                                                         
         MVC   CHKAGY,TAOIAGY                                                   
         DROP  R4                                                               
*                                                                               
         USING TAPDD,R4                                                         
LIMCHK10 L     R4,AIO              CHECK CLIENT LIMIT ACCESS                    
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LIMCHK20                                                         
         MVC   CHKCLI,TAPDCLI                                                   
         DROP  R4                                                               
*                                                                               
         USING FAWSSVRD,R1                                                      
LIMCHK20 LA    R1,LIMBLK                                                        
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R2,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0           IF NOT FOUND, STAFF HAS NO ACCESS            
         BNE   NO                                                               
         DROP  R1                                                               
*                                                                               
         AHI   R2,1                                                             
*                                                                               
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         BZ    YES                 STAFF HAS ACCESS TO ALL RECORDS              
*                                                                               
LIMCHK30 CLI   0(R1),0             RECALL NEXT RECORD FROM WSSVR                
         BE    LIMCHK20                                                         
*                                                                               
         CLC   CHKAGY,TAVAAGY      IF AGENCY IS FOUND IN STAFF LIMITS           
         BNE   LIMCHK50                                                         
*                                                                               
         CLI   TAVALEN,TAVALNQ     IF NO CLIENT LIMITS ARE DEFINED              
         BE    YES                 ACCESS IS GRANTED                            
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
LIMCHK40 CLC   CHKCLI,0(RF)        IF CLIENT IS FOUND IN STAFF LIMITS           
         BE    YES                 ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         BNZ   LIMCHK40                                                         
*                                                                               
LIMCHK50 ZIC   RE,TAVALEN          BUMP TO NEXT VALID AGENCY/CLIENT             
         AR    R1,RE               ELEMENT                                      
         B     LIMCHK30                                                         
         DROP  R1                                                               
         EJECT                                                                  
*              DISPLAY THE KEY                                                  
         SPACE 1                                                                
DKEY     NTR1                                                                   
         L     R4,AIO              GET CHECK DETAILS ELEMENT                    
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DKX                                                              
         USING TACDD,R4                                                         
         MVC   SCHCHK,TACDCHK      DISPLAY CHECK NUMBER                         
         OI    SCHCHKH+6,X'80'                                                  
         SPACE 1                                                                
DKX      B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SCHSSNH             CLEAR THE SCREEN                             
         SPACE 1                                                                
         MVC   SYSDIR,SVSYSDIR     RESET FILE OVERRIDES                         
         MVC   SYSFIL,SVSYSFIL                                                  
         SPACE 1                                                                
         L     R3,AIO              R3=A(CHECK RECORD)                           
         USING TLCKD,R3                                                         
         MVC   SCHSSN,TLCKSSN      SOCIAL SECURITY NUMBER                       
         MVC   SCHCAT,TLCKCAT      CATEGORY                                     
         MVC   TGINV,TLCKINV                                                    
         MVC   TGAGY,TLCKAGY       SET AGENCY AND                               
         XC    TGINV,HEXFFS        TGINV FOR INVOICE REC LATER                  
         SPACE 1                                                                
         MVC   AIO,AIO2            DISPLAY PERF NAME                            
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',SCHSSN),SCHSSNNH                      
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    DISP05                                                           
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SCHSSN,SPACES                                                    
         MVC   SCHSSN(L'TGPID),TGPID                                            
         MVI   SCHSSNH+5,6                                                      
         OI    SCHSSNH+6,X'80'                                                  
DISP05   MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         MVC   SCHAGY,TLCKAGY      AGENCY                                       
         SPACE 1                                                                
         MVC   AIO,AIO2            DISPLAY AGENCY NAME                          
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'8C',SCHAGY),SCHAGYNH                      
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         GOTO1 TINVCON,DMCB,TLCKINV,SCHINV,DATCON  INVOICE NUMBER               
         SPACE 1                                                                
         LR    R4,R3               GET PAYMENT DETAILS ELEMENT                  
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         MVC   TGCOM,TAPDCOM       SAVE INTERNAL COMM'L #                       
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         MVC   SCHUSEN,TGUSNAME    DISPLAY USE NAME                             
         OI    SCHUSENH+6,X'80'                                                 
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(X'11',TAPDCYCS),(8,SCHCYC)  CYCLE DATES             
         OI    SCHCYCH+6,X'80'                                                  
         SPACE 1                                                                
         LR    R4,R3               GET CHECK DETAILS ELEMENT                    
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACDD,R4                                                         
         XC    CID2,CID2                                                        
         TM    TACDSTAT,TACDS2ND   IF CHECK FROM 2ND SOAP CABLE COMM'L          
         BZ    DISP10                                                           
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',0) GET INVOICE REC                    
         MVC   AIO,AIO1                                                         
         BNE   DISP10                                                           
         L     R4,AIO2             GET SOAP CABLE ELEMENT                       
         MVI   ELCODE,TASBELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISP10                                                           
         USING TASBD,R4                                                         
         MVC   CID2,TASBCID2       SAVE CID2 TO DISPLAY LATER                   
         SPACE 1                                                                
DISP10   MVC   AIO,AIO2            DISPLAY COMMERCIAL NAME                      
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'88',0),SCHCIDNH                          
         MVC   AIO,AIO1                                                         
         BNE   DISP15                                                           
         SPACE 1                                                                
         L     R4,AIO2             GET COMMERCIAL DETAILS ELEMENT               
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   SCHCID,TACOCID      COMMERCIAL ID                                
         MVC   COMSTAT,TACOSTAT    SAVE COMMERCIAL STATUS BYTE                  
         SPACE 1                                                                
DISP15   OC    CID2,CID2           IF HAVE CID2                                 
         BZ    *+10                                                             
         MVC   SCHCID,CID2         DISPLAY IT INSTEAD                           
         SPACE 1                                                                
         BAS   RE,EPIDTLS          DISPLAY EPISODE DETAILS                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS EPISODE DETAILS                                 
         SPACE 1                                                                
EPIDTLS  NTR1                                                                   
         LA    R3,SCHDATAH         R3 = A(SCREEN FIELD)                         
         USING LINED,R3                                                         
*                                                                               
         L     R4,AIO              GET SOAP DETAILS ELEMENT                     
         MVI   ELCODE,TASOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   EDTLX                                                            
         USING TASOD,R4            R4 = A(SOAP DETAILS ELEMENT)                 
*                                                                               
EDTL05   ZIC   R5,TASONUM          R5 = N'EPISODE SUB-ELEMENTS                  
         LTR   R5,R5                                                            
         BZ    EDTL60                                                           
*                                                                               
         LA    R2,TASOSEPI         R2 = A(EPISODE SUB-ELEMENT)                  
         USING TASOSEPI,R2                                                      
*                                                                               
EDTL10   EDIT  (2,TASOEPI),(5,LINEPI),FILL=0  EPISODE NUMBER                    
         MVC   TGEPI,LINEPI                                                     
*                                                                               
         LA    RF,LINROLE          ROLE                                         
         TM    TASOSTAT,TASOSH+TASOSB+TASOSS  IF DID ALL ROLES                  
         BNO   *+12                                                             
         MVI   0(RF),C'A'                     DISPLAY 'A'LL                     
         B     EDTL20                                                           
         TM    TASOSTAT,TASOSH     TEST HEAD WRITER                             
         BZ    *+12                                                             
         MVI   0(RF),C'H'                                                       
         LA    RF,1(RF)                                                         
         TM    TASOSTAT,TASOSB     TEST BREAKDOWN WRITER                        
         BZ    *+12                                                             
         MVI   0(RF),C'B'                                                       
         LA    RF,1(RF)                                                         
         TM    TASOSTAT,TASOSS     TEST SCRIPT WRITER                           
         BZ    *+8                                                              
         MVI   0(RF),C'S'                                                       
*                                                                               
EDTL20   CLI   TGUSEQU,USOP        NO AMOUNTS IF SOP                            
         BE    EDTL34                                                           
         EDIT  (3,TASOPAY),(7,LINPAY),2,FLOAT=-    PAYMENT AMOUNT               
         OC    TASOAPPL,TASOAPPL                                                
         BZ    EDTL30                                                           
         EDIT  (3,TASOAPPL),(7,LINAPPL),2,FLOAT=-  APPLIED AMOUNT               
EDTL30   EDIT  (2,TASOPNH),(6,LINPNH),2,FLOAT=-    PENSION AND HEALTH           
*                                                                               
EDTL34   BAS   RE,GETDATE          GET AIR/WORK DATE                            
*                                                                               
         OC    TGTHREE,TGTHREE     IF DATE DEFINED                              
         BZ    EDTL50                                                           
         GOTO1 DATCON,DMCB,(1,TGTHREE),(8,LINDATE)  DISPLAY IT                  
*                                                                               
EDTL50   LA    R2,L'TASOSEPI(R2)   BUMP TO NEXT EPISODE SUB-ELEMENT             
         LA    R3,LINNEXT          BUMP TO NEXT ENTRY ON SCREEN                 
         BCT   R5,EDTL10           LOOP                                         
*                                                                               
EDTL60   MVI   ELCODE,TASOELQ      LOOK FOR ANOTHER SOAP ELEMENT                
         BAS   RE,NEXTEL                                                        
         BE    EDTL05              GO PROCESS                                   
*                                                                               
EDTLX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET  AIR/WORK DATE                                    
         SPACE 1                                                                
GETDATE  NTR1                                                                   
         XC    TGTHREE,TGTHREE     RETURN HERE                                  
*                                                                               
         MVC   AIO,AIO2            READ EPISODE RECORD                          
         GOTO1 RECVAL,DMCB,TLEPCDQ,(X'20',0)                                    
         MVC   AIO,AIO1                                                         
         BNE   GETDX                                                            
*                                                                               
         L     R4,AIO2             R4 = A(EPISODE INFORMATION ELEMENT)          
         MVI   ELCODE,TAEIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   GETDX                                                            
         USING TAEID,R4                                                         
         MVC   TGTHREE,TAEIADT     SET TO PRINT AIR DATE                        
         TM    COMSTAT,TACOSWDT    IF THIS COMML REQUIRES WORK DATE             
         BZ    *+10                                                             
         MVC   TGTHREE,TAEIWDT     SET TO PRINT WORK DATE                       
*                                                                               
GETDX    B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 3                                                                
SETCHK   DS    0H                                                               
         MVC   SYSDIR,=CL8'CHKDIR' SET FILE OVERRIDES                           
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         BR    RE                                                               
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 3                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
NTFOUND  MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'ECHECK  ',CL8'LIST'                                 
PF13     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'CHECK   ',CL8'DISPLAY'                              
PF14     DC    AL1(KEYTYTWA,L'SCHCHK-1),AL2(SCHCHK-T702FFD)                     
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'HISTORY ',CL8'DISPLAY'                              
PF15     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'SCHINV-1),AL2(SCHINV-T702FFD)                     
PF15X    EQU   *                                                                
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'CEPISODE',CL8'LIST'                                 
PF16     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
PF16X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
HEXFFS   DC    8X'FF'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER SUB-ELEMENT DISPLAY ENTRY                         
         SPACE 1                                                                
LINED    DSECT                                                                  
         DS    CL8                 HEADER FOR '>'                               
         DS    CL1                 '>'                                          
         DS    CL8                 HEADER FOR DATA                              
LINEPI   DS    CL5                 EPISODE                                      
         DS    CL1                                                              
LINDATE  DS    CL8                 DATE                                         
         DS    CL1                                                              
LINROLE  DS    CL2                 ROLE(S)                                      
LINAPPL  DS    CL7                 APPLIED AMOUNT                               
LINPAY   DS    CL7                 PAYMENT AMOUNT                               
LINPNH   DS    CL6                 P&H                                          
LINNEXT  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRCDD                                                       
         SPACE 3                                                                
COMSTAT  DS    XL1                 COMMERCIAL STATUS BYTE                       
CDSTAT   DS    XL1                 CHECK DETAILS STATUS BYTE                    
CID2     DS    CL12                2ND CID                                      
CHKAGY   DS    CL6                 CHECK'S AGENCY                               
CHKCLI   DS    CL6                 CHECK'S CLIENT                               
LIMBLK   DS    XL100               AGENCY/CLIENT LIMIT WORK BLOCK               
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013TAGENCD   02/16/07'                                      
         END                                                                    
