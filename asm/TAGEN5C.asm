*          DATA SET TAGEN5C    AT LEVEL 005 AS OF 12/30/14                      
*PHASE T7025CE,*                                                                
         TITLE 'T70218 - CHECK2 MAINTENANCE'                                    
T7025C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7025C,R7,R6                                                   
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
                                                                                
         STCM  RB,15,MYBASERB                                                   
         BRAS  RE,INIT             INITIALIZE PROGRAM                           
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+8                                                              
         BRAS  RE,DREC                                                          
         J     XIT                                                              
                                                                                
         EJECT                                                                  
***********************************************************************         
*        ERROR MESSAGES AND EXITS                                     *         
***********************************************************************         
                                                                                
ERINV    MVI   ERROR,INVALID       INVALID INPUT                                
         J     END                                                              
                                                                                
ERMIS    MVI   ERROR,MISSING       MISSING INPUT                                
         J     END                                                              
                                                                                
ERNFD    MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         J     END                                                              
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
INFEND   MVI   MYMTYP,GTMINF       INFORMATION MESSAGE EXIT                     
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
ERRMORE  ZICM  RB,MYBASERB,(15)      MORE TO DISPLAY                            
         MVC   MYMSGNO,=H'77'                                                   
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
ERREOL   ZICM  RB,MYBASERB,(15)      END OF DISPLAY                             
         MVC   MYMSGNO,=H'262'                                                  
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
END      GOTO1 EXIT,DMCB,0                                                      
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE PROGRAM                                *         
***********************************************************************         
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 INITIAL,DMCB,PFTAB                                               
         J     XIT                                                              
                                                                                
PFTAB    DS    0C                  PF TABLE                                     
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'HISTORY',CL8'DISPLAY'                                 
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'RCHECK ',CL8'DISPLAY'                                 
PF14X    EQU   *                                                                
*                                                                               
         DC    AL1(PF15X-*,15,0,0,0)                                            
         DC    CL3' ',CL8'CHECK  ',CL8'STOP   '                                 
PF15X    EQU   *                                                                
*                                                                               
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CHECK  ',CL8'PULL   '                                 
PF16     DC    AL1(KEYTYGLB,L'TGCHK-1),AL2(TGCHK-TGD)                           
PF16X    EQU   *                                                                
*                                                                               
         DC    AL1(PF17X-*,17,0,0,0)                                            
         DC    CL3' ',CL8'CHECK  ',CL8'DISPL  '                                 
PF17X    EQU   *                                                                
*                                                                               
         DC    AL1(PF19X-*,19,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF19X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR INIT ROUTINE                      *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         TM    CONRECH+4,X'20'  IF RECCORD IS NEW THEN ALWAYS VALIDATE          
         JNO   VK0                                                              
         TM    CK2CHKH+4,X'80'   IT WILL SEEM LIKE WE ARE VALIDATED             
         JNO   VK01                                                             
*                                                                               
*  1ST TIME IN - INITIALIZATION                                                 
VK0      XC    STATEIDX,STATEIDX                                                
*        MVI   PROCFEDF,C'N'                                                    
         MVI   PROCFEDF,0                                                       
         XC    SVCITY,SVCITY                                                    
         XC    RESUME_F,RESUME_F   NEW                                          
                                                                                
VK01     LA    R2,CK2CHKH                                                       
         CLI   ACTNUM,ACTDIS       IF ACTION IS DISPLAY                         
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA       OR CHANGE                                    
         BNE   VK10                                                             
         CLI   5(R2),0             AND NOTHING INPUT                            
         BNE   VK10                                                             
         OC    TGCHK,TGCHK         AND THERE'S A GLOBAL NUMBER                  
         BZ    VK10                                                             
         MVC   8(L'TLCKCCHK,R2),TGCHK  USE IT                                   
         OI    6(R2),X'80'                                                      
         MVI   5(R2),L'TLCKCCHK                                                 
                                                                                
VK10     GOTO1 ANY                                                              
         CLI   5(R2),L'TLCKCCHK    INSURE FULL CHECK NUMBER INPUT               
         JNE   ERINV                                                            
         MVC   TGCHK,WORK          SAVE UNCOMPLEMENTED IN GLOBAL                
                                                                                
         LA    R2,CK2CHKH                                                       
         GOTO1 ANY                                                              
         CLI   5(R2),L'TLCKCCHK    INSURE FULL CHECK NUMBER INPUT               
         JNE   ERINV                                                            
         MVC   TGCHK,WORK          SAVE UNCOMPLEMENTED IN GLOBAL                
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY              R3 = A(CHECK KEY)                            
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ    RECORD CODE                                  
         MVC   TLCKCCHK,TGCHK      CHECK NUMBER                                 
         XC    TLCKCCHK,=8X'FF'    COMPLEMENTED                                 
         MVC   SYSDIR,=C'CHKDIR'                                                
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         JNE   ERNFD                                                            
         DROP  R3                                                               
                                                                                
         MVC   SYSFIL,=C'CHKFIL'                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   =C'P+',TAPDEMP                                                   
         JNE   ERINV                                                            
                                                                                
* WE JUST GOT VOID CHECK- LETS SEE IF WE ASKED FOR THE VOID CHECK               
         USING TLCKPD,R3                                                        
         CLI   TWALREC,CH                                                       
         JE    *+8                                                              
         CLI   TWALREC,K2                                                       
         JNE   *+14                                                             
         CLC   TLCKCSEQ,TGSEQ                                                   
         JE    XIT                                                              
         DROP  R3                                                               
                                                                                
         BRAS  RE,LIMITCHK         CHECK LIMIT ACCESS                           
         JNE   ERNFD                                                            
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVW4TYP,TAPDW4TY                                                 
         TM    TAPDADJS,TAPDADVD   IF CHECK IS A VOID CHECK                     
         JZ    XIT                                                              
         DROP  R4                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         JE    XIT                                                              
         MVC   KEY,KEYSAVE         IF CAN'T FIND (I.E. ORIGINAL PURGED)         
         GOTO1 HIGH                SHOW VOID                                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR VALIDATE KEY ROUTINES                           *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY CANADIAN TAXES                            *         
*        ON ENTRY ... AIO = A(CHECK RECORD)                           *         
***********************************************************************         
DISCAN   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         USING TAATD,R4                                                         
*                                                                               
         MVI   ELCODE,TAATELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DISCAN10 BRAS  RE,NEXTEL                                                        
         JNE   DISCANX                                                          
         CLC   TAATUNIT,=C'CN '                                                 
         JE    DISCAN20                                                         
         CLC   TAATUNIT,=C'FD '                                                 
         JE    DISCAN10                                                         
         MVC   TD1PPRO,TAATUNIT    DISPLAY PROVINCE                             
         J     DISCAN30                                                         
                                                                                
DISCAN20 DS    0C                                                               
         EDIT  (B4,TAATTAX),TD1FD,2,ZERO=BLANK,ALIGN=RIGHT                      
         J     DISCAN10                                                         
                                                                                
DISCAN30 DS    0C                                                               
         EDIT  (B4,TAATTAX),TD1PR,2,ZERO=BLANK,ALIGN=RIGHT                      
                                                                                
         CLC   =C'QC',TD1PPRO                                                   
         BE    DISCAN40                                                         
         EDIT  (B4,TAATPP),TD1CPP,2,ZERO=BLANK,ALIGN=RIGHT                      
         EDIT  (B4,TAATEI),TD1EI,2,ZERO=BLANK,ALIGN=RIGHT                       
         EDIT  (B4,TAATPIP),TD1QPIP,2,ZERO=BLANK,ALIGN=RIGHT                    
         J     DISCAN10                                                         
                                                                                
DISCAN40 EDIT  (B4,TAATPP),TD1CPP2,2,ZERO=BLANK,ALIGN=RIGHT                     
         EDIT  (B4,TAATEI),TD1EI2,2,ZERO=BLANK,ALIGN=RIGHT                      
         EDIT  (B4,TAATPIP),TD1QPI2,2,ZERO=BLANK,ALIGN=RIGHT                    
         J     DISCAN10                                                         
                                                                                
DISCANX  J     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
*        LITERALS FOR VALIDATE KEY ROUTINES                           *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE RECORD                                *         
*        ON ENTRY ... AIO = A(PRIMARY COMMERCIAL RECORD)              *         
***********************************************************************         
                                                                                
DREC     NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,SETSCR                                                        
         GOTO1 FLDVAL,DMCB,(2,CK2CHKH),999                                      
         BAS   RE,DISPFLDS         DISPLAY SCREEN FIELDS                        
                                                                                
         CLI   TWASCR,SCR66                                                     
         JNE   DR10                                                             
         GOTO1 FLDVAL,DMCB,(X'20',CK2UNITH),(X'80',999)                         
         GOTO1 (RF),(R1),(1,CK2UNITH),CK2LSTH                                   
         BAS   RE,ADDTACWS         ADD TEMPORARY CHK WITHHOLDING ELEMS          
         GOTO1 DISPTACW            DISPLAY ALL TACW ELEMENTS                    
         OC    RESUME_F,RESUME_F   EXIT IF END OF SCREEN ERROR                  
         JNZ   ERRMORE                                                          
*        MVI   PROCFEDF,C'N'                                                    
         MVI   PROCFEDF,0                                                       
         XC    RESUME_F,RESUME_F                                                
         XC    STATEIDX,STATEIDX                                                
         J     ERREOL                                                           
                                                                                
DR10     BRAS  RE,DISTD1                                                        
         BRAS  RE,DISCAN           DISPLAY P+ CANADIAN TAXES                    
         J     XIT                                                              
*                                                                               
*********************************************************************           
* DISPLAY ALL WITHHOLDING ELEMENTS IN CHECK RECORD - SCREEN SCROLLS             
*********************************************************************           
DISPTACW NTR1                                                                   
DISTACW8 L     R4,AIO                                                           
         ZICM  R0,STATEIDX,(15)                                                 
         MVI   ELCODE,TACWELQ      GET CHECK WITHHOLDING ELEMENT                
         BRAS  RE,GETEL                                                         
         B     *+12                                                             
DISTAC10 MVI   ELCODE,TACWELQ      GET CHECK WITHHOLDING ELEMENT                
         BRAS  RE,NEXTEL                                                        
         BE    DISTAC11                                                         
*        CLI   PROCFEDF,C'Y'       IF THERE ARE NO FD UNITS ON REC              
*        BE    DISTACWX            THEN SET FLAG TO INDICATE WE                 
         TM    PROCFEDF,PROCFD     IF THERE ARE NO FD UNITS ON REC              
         BO    DISTACWX            THEN SET FLAG TO INDICATE WE                 
*        MVI   PROCFEDF,C'Y'       ALREADY PROCESSED FD AND PROCEED             
*        B     DISTACW8            WITH REST OF THE UNITS                       
         OI    PROCFEDF,PROCFD     ALREADY PROCESSED FD AND PROCEED             
         B     DISTACW8            WITH REST OF THE UNITS                       
                                                                                
DISTAC11 OC    RESUME_F,RESUME_F                                                
         BZ    DISTAC12                                                         
         BCT   R0,DISTAC10                                                      
         B     DISTAC13                                                         
*                                                                               
DISTAC12 L     RE,STATEIDX                                                      
         AHI   RE,1                                                             
         ST    RE,STATEIDX                                                      
                                                                                
         USING TACWD,R4                                                         
DISTAC13 CLC   TACWUNIT(2),=C'FD'  IF UNIT CODE IS FOR FEDERAL                  
         BNE   DISTAC14                                                         
         OC    RESUME_F,RESUME_F   DONT PROCESS FD FOR RESUMES                  
         BNZ   DISTAC10                                                         
*        CLI   PROCFEDF,C'Y'        IF FED UNIT PROCESSED ALREADY               
*        BE    DISTAC10            THEN GO TO NEXT ELEMENT                      
         TM    PROCFEDF,PROCFD      IF FED UNIT PROCESSED ALREADY               
         BO    DISTAC10            THEN GO TO NEXT ELEMENT                      
****     CLI   TATUFLG,C'N'        IF NO TATU ELEMENT - DONT SHOW FD            
****     BE    DISTAC10                                                         
         CLI   DISFDFLG,C'N'       DONT DISPLAY FD ?                            
         BE    DISTAC10                                                         
         BAS   RE,PROCFED          BRANCH TO PROCFED                            
*                                                                               
         XC    STATEIDX,STATEIDX                                                
         B     DISTACW8                                                         
                                                                                
*DISTAC14 CLI   PROCFEDF,C'Y'       ONLY PROCESS STATE AFTER FED                
*         BNE   DISTAC10            HAS BEEN PROCESSED                          
DISTAC14 TM    PROCFEDF,PROCFD     ONLY PROCESS STATE AFTER FED                 
         BZ    DISTAC10            HAS BEEN PROCESSED                           
*                                                                               
         CLC   TACWUNIT(2),=C'CN'  IF UNIT CODE IS FOR FEDERAL                  
         BNE   *+12                                                             
         BAS   RE,PROCFED          BRANCH TO PROCFED                            
         B     DISTAC20                                                         
*                                                                               
         TM    PROCFEDF,PROCCN     CAN PROVINCES DON'T SET TACWSTAT             
         BO    DISTAC16                                                         
*                                                                               
         TM    TACWSTAT,TACWSRES+TACWSWRK                                       
         BNZ   DISTAC16            IF NEITHER RESIDENCE NOR WORK                
         B     DISTAC20                                                         
*  --------------STATE -----------                                              
DISTAC16 ST    R4,STATEPTR         SAVE OFF CURRENT STATE ELEM ADDRESS          
         CLI   TACWUNIT+2,X'40'    IF UNIT CODE IS FOR A STATE                  
         BH    DISTAC20                                                         
         TM    TACWSTAT,TACWSWRK   IF WORK BIT ON                               
         BZ    DISTAC18                                                         
         BAS   RE,PROCSOW          PROCESS STATE OF WORK/RESIDENT               
         OC    RESUME_F,RESUME_F   EXIT IF END OF SCREEN ERROR                  
         JNZ   XIT                                                              
         B     DISTAC20                                                         
DISTAC18 DS    0C                                                               
         BAS   RE,PROCSOR          ELSE PROCESS STATE OF RESIDENCE              
         OC    RESUME_F,RESUME_F   EXIT IF END OF SCREEN ERROR                  
         JNZ   XIT                                                              
         B     DISTAC20                                                         
*                                                                               
DISTAC20 DS    0C                                                               
         B     DISTAC10            KEEP LOOPING TILL NO MORE ELEMENTS           
DISTACWX DS    0H                                                               
         J     XIT                                                              
                                                                                
*********************************************************************           
* THIS ROUTINE ADDS TEMPORARY CHECK WITHHOLDING ELEMENTS TO THE CHK *           
* FOR TAX UNITS THAT RECEIVED PAYMENT BUT HAD NO TAXES WITHHELD     *           
*********************************************************************           
                                                                                
ADDTACWS NTR1                                                                   
         USING TATUD,R4                                                         
****     MVI   TATUFLG,C'N'                                                     
         L     R4,AIO                                                           
         MVI   ELCODE,TATUELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
ACWS10   BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
* we have tatu element                                                          
****     MVI   TATUFLG,C'Y'                                                     
* LOGIC TO FIGURE OUT IF THIS IS A FOREIGN CORP WORKING IN CANADA               
* SET FLAG NO TO DISPLAY FD UNIT IS SO                                          
         MVI   DISFDFLG,C'Y'                                                    
         CLI   SVW4TYP,C'I'                                                     
         BE    ACWS20                                                           
         CLC   =C'FD',TATUUNIT                                                  
         BE    ACWS20                                                           
         CLC   =C'CN',TATUUNIT                                                  
         BE    ACWS20                                                           
* FOR NON FD AND CN TATU - CHECK TATUSCAN                                       
         TM    TATUSTAT,TATUSCAN                                                
         BNO   *+8                                                              
         MVI   DISFDFLG,C'N'                                                    
*                                                                               
ACWS20   GOTO1 HASTACW,DMCB,TATUUNIT                                            
         JE    ACWS10                                                           
*        CLC   =C'NYC',TATUUNIT                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 ADDTACW,DMCB,TATUUNIT                                            
                                                                                
         XC    TGTHREE,TGTHREE                                                  
         CLC   =C'DET',TATUUNIT                                                 
         JNE   *+10                                                             
         MVC   TGTHREE,=C'MI '                                                  
         CLC   =C'NYC',TATUUNIT                                                 
         JNE   *+10                                                             
         MVC   TGTHREE,=C'NY '                                                  
         CLC   =C'CIN',TATUUNIT                                                 
         JNE   *+10                                                             
         MVC   TGTHREE,=C'OH '                                                  
         CLC   =C'CLV',TATUUNIT                                                 
         JNE   *+10                                                             
         MVC   TGTHREE,=C'OH '                                                  
         CLC   =C'PHL',TATUUNIT                                                 
         JNE   *+10                                                             
         MVC   TGTHREE,=C'PA '                                                  
         OC    TGTHREE,TGTHREE                                                  
         JZ    ACWS10                                                           
         GOTO1 HASTACW,DMCB,TGTHREE                                             
         JE    ACWS10                                                           
         GOTO1 ADDTACW,DMCB,TGTHREE                                             
         J     ACWS10                                                           
         DROP  R4                                                               
                                                                                
*********************************************************************           
* THIS ROUTINE DETERMINES IF CHECK WITHHOLDING ELEMENT EXISTS FOR   *           
* THE PROVIDED TAX UNIT                                             *           
* ON ENTRY ... R1 = A(TAX UNIT)                                     *           
*********************************************************************           
                                                                                
HASTACW  NTR1                                                                   
         LHI   R0,1                                                             
                                                                                
         L     R2,0(R1)                                                         
         MVI   ELCODE,TACWELQ                                                   
**       CLC   =C'NYC',0(R2)                                                    
**       BNE   *+6                                                              
**       DC    H'0'                                                             
         GOTO1 GETL,DMCB,(3,0(R2))                                              
         JNE   HCW10                                                            
         XR    R0,R0                                                            
                                                                                
HCW10    MVI   ELCODE,TATUELQ                                                   
         LTR   R0,R0                                                            
         J     XIT                                                              
                                                                                
*********************************************************************           
* THIS ROUTINE ADDS CHECK WITHHOLDING ELEMENT FOR PROVIDED TAX      *           
* UNIT                                                              *           
* ON ENTRY ... R1 = A(TAX UNIT)                                     *           
*********************************************************************           
                                                                                
ADDTACW  NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         USING TACWD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TACWEL,TACWELQ                                                   
         MVI   TACWLEN,TACWLN2Q                                                 
         MVC   TACWUNIT,0(R2)                                                   
         OI    TACWSTAT,TACWSWRK                                                
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
*********************************************************************           
* THIS ROUTINE DISPLAYS  ALL THE STATIC SCREEN FIELDS                           
*********************************************************************           
DISPFLDS NTR1                                                                   
         XC    CK2MSG,CK2MSG       CLEAR RETURNED CHECK MESSAGE                 
         OI    CK2MSGH+6,X'80'                                                  
*  PID                                                                          
         L     R4,AIO                                                           
         USING TLCKD,R4                                                         
         MVC   TGSSN,TLCKSSN                                                    
         GOTO1 SSNPACK,DMCB,TGSSN,CK2PID                                        
         MVI   CK2PIDH+5,6                                                      
                                                                                
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8A',TGSSN),CK2PIDNH                       
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   AIO,AIO1                                                         
                                                                                
         MVC   TGAGY,TLCKAGY       SAVE AGENCY IN GLOBAL                        
         MVC   TGINV,TLCKINV            INVOICE                                 
         MVC   TGSSN,TLCKSSN                                                    
                                                                                
         L     R4,AIO              IF ORIGINAL AGY/INV ELEMENT EXISTS           
         MVI   ELCODE,TAOIELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DISP2                                                            
         USING TAOID,R4                                                         
         MVC   TGAGY,TAOIAGY       SAVE ORIGINAL AGY/INV IN GLOBAL              
         MVC   TGINV,TAOIINV                                                    
                                                                                
*                                                                               
DISP2    DS    0C                                                               
         XC    TGINV,=6X'FF'                                                    
         DROP  R4                                                               
         L     R4,AIO              R4 = A(PAYMENT DETAILS ELEMENT)              
         LA    RE,CK2UNITH                                                      
         SHI   RE,TLNQ                                                          
*                                                                               
         STCM  RE,15,CK2SCNPTR        SCREEN POINTER                            
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
                                                                                
         MVC   CK2TYPE,TAPDW4TY    W4 TYPE                                      
         MVI   CK2TYPEH+5,1                                                     
         MVC   CK2EMP,TAPDEMP      EMPLOYER                                     
                                                                                
         USING TACDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   CK2FREQ,TACDFREQ    FREQUENCY                                    
         MVI   CK2FREQH+5,1                                                     
                                                                                
DISPX    J     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*  ROUTINE TO DISPLAY CHECK INFORMATION                                         
*********************************************************************           
DISPRTN  NTR1                                                                   
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TARNELQ      GET CHECK RETURNED DETAILS ELEMENT           
         BRAS  RE,GETEL                                                         
         BNE   DISPRTNX                                                         
*                                                                               
         USING TARND,R4                                                         
         LA    R3,CK2MSG               DISPLAY                                  
         MVC   0(L'LTRCHK,R3),LTRCHK   'THIS CHECK WAS'                         
         LA    R3,L'LTRCHK+1(R3)                                                
         TM    TARNSTAT,TARNMAIL                                                
         BZ    DISPRTN4                                                         
         MVC   0(6,R3),=C'MAILED'                                               
         LA    R3,7(R3)                                                         
         B     DISPRTN5                                                         
DISPRTN4 TM    TARNSTAT,TARNFILE                                                
         BZ    DISPRTN7                                                         
         MVC   0(5,R3),=C'FILED'                                                
         LA    R3,6(R3)                                                         
DISPRTN5 MVC   0(2,R3),=C'ON'                                                   
         GOTO1 DATCON,DMCB,(1,TARNDDTE),(8,3(R3))                               
         B     DISPRTNX                                                         
*                                                                               
DISPRTN7 MVC   0(11,R3),=CL11'RETURNED ON'                                      
         GOTO1 DATCON,DMCB,(1,TARNRDTE),(8,12(R3))                              
*                                                                               
DISPRTNX J     XIT                                                              
LTRCHK   DC    C'This check was'                                                
         DROP  R4                                                               
**********************************************************************          
* THIS ROUTINE DISPLAYS THE CONTENTS OF A FEDERAL WITHHOLDING ELEMENT           
* TO THE SCREEN.                                                                
**********************************************************************          
                                                                                
         USING TACWD,R4                                                         
PROCFED  NTR1                                                                   
         ZICM  R2,CK2SCNPTR,(15)                                                
         AHI   R2,TLNQ                                                          
         STCM  R2,15,CK2SCNPTR        BUMP TO UNIT SCREEN SECTION               
         ZICM  R2,CK2SCNPTR,(15)                                                
         USING TAXD,R2                                                          
         MVC   TUNIT(2),=C'FD'                                                  
         MVI   TUNITH+5,2                                                       
                                                                                
         L     R3,TACWTAX          DISPLAY AMOUNTS IN SCREEN                    
                                                                                
         CLC   =C'CN',TACWUNIT                                                  
         BNE   *+10                                                             
         MVC   TUNIT(2),=C'CN'                                                  
****     L     R3,TACWGST          DISPLAY AMOUNTS IN SCREEN                    
         OI    PROCFEDF,PROCCN                                                  
                                                                                
         LA    R5,TTAX                                                          
         MVI   TTAXH+5,L'TTAX                                                   
         BAS   RE,EDITAM10                                                      
                                                                                
*        MVI   PROCFEDF,C'Y'        SET FEDERAL WITHHOLD FLAG - Y               
         OI    PROCFEDF,PROCFD      SET FEDERAL WITHHOLD FLAG - Y               
                                                                                
         XC    DMCB2(24),DMCB2      ONLY DISPLAY UNITS WITH TATU                
         ST    R4,DMCB2             ELEMENT OR IF TACWTAX IS WITHHELD           
         GOTO1 GETTATU,DMCB2                                                    
                                                                                
         BE    PROCFED2                                                         
*                                                                               
*  NO TATU FOR CN GET TAX AND WAGES ELSEWHERE                                   
         CLC   TUNIT(2),=C'CN'                                                  
         BNE   PROCFED1                                                         
         CLI   DISFDFLG,C'Y'         IF NO TATU FOR CN THEN                     
         BNE   *+10                  GET THE WAGES FROMT HE FD LINE             
         MVC   DMCB2+4(4),SVGWAGE     PREVIOUSLY PROCESSED                      
         OC    TACWGST,TACWGST                                                  
         BNZ   PROCFED2                                                         
*                                                                               
PROCFED1 OC    TACWTAX,TACWTAX                                                  
         BNZ   PROCFED2                                                         
         BAS   RE,PREVLINE                                                      
         B     PROCFEDX                                                         
                                                                                
                                                                                
PROCFED2 L     R3,DMCB2+4                                                       
         LA    R5,TWAGE                                                         
         MVC   SVGWAGE,DMCB2+4                                                  
                                                                                
                                                                                
         MVI   TWAGEH+5,L'TWAGE                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
         L     R3,DMCB2+8                                                       
         LA    R5,TNTRM                                                         
         MVI   TNTRMH+5,L'TNTRM                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
         L     R3,DMCB2+12                                                      
         LA    R5,TTXRM                                                         
         MVI   TTXRMH+5,L'TTXRM                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         MVC   TUNEMP,=C'    ---- '                                             
         MVI   TUNEMPH+5,L'TUNEMP                                               
         MVC   TDIS,=C'    ---- '                                               
         MVI   TDISH+5,L'TDIS                                                   
         MVC   TFLI,=C'    ---- '                                               
         MVI   TFLIH+5,L'TFLI                                                   
                                                                                
         USING TACDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   PROCFEDX                                                         
                                                                                
         OC    TACDRUN,TACDRUN     IF CHECK RUN ALREADY                         
         BNZ   *+14                                                             
         OC    TACDNET,TACDNET     OR CHECK ALREADY HAS NET AMOUNT              
         BZ    PROCFED5                                                         
                                                                                
         L     R3,TACDEARN         DISPLAY GROSS TAXABLE EARNINGS               
         OC    TWAGE,TWAGE         IF ITS ALREADY FILLED IN FROM                
         BNZ   PROCFED4            TATUWAGE ABOVE                               
         MVC   SVGWAGE,TACDEARN                                                 
         LA    R5,TWAGE                                                         
         BAS   RE,EDITAM10         ALSO PRINT IF 0                              
                                                                                
*                                                                               
* NEWEST ADDTION DEC/13/12                                                      
         USING TAODD,R4                                                         
PROCFED4 MVI   ELCODE,TAODELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPF))  GET OTHER DEDUCTION ELEM           
         BNE   PROCFED5                      FOR FEDERAL TAX TYPE               
         L     R4,TGELEM                                                        
         L     R3,TAODAMT          DISPLAY AMOUNT ON SCREEN                     
         LA    R5,TTAX                                                          
         BAS   RE,EDITAM10                                                      
*                                                                               
                                                                                
         USING TAPDD,R4                                                         
PROCFED5 L     R4,AIO              CHECK NOT WRITTEN YET                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   TAPDW4TY,TAW4TYIN   IF CHECK IS FOR INDIVIDUAL                   
         BNE   PROCFEDX                                                         
         L     R3,TAPDPAYI         SHOW INDIV PAYMENT                           
         MVC   SVGWAGE,TAPDPAYI                                                 
         LA    R5,TWAGE             DISPLAY IN GROSS                            
         BAS   RE,EDITAM10         ALSO PRINT IF 0                              
         L     R3,TAPDPAYI         SHOW REIMBURSED EXPENSES                     
         LA    R5,TWAGE             DISPLAY IN NON-TAXABLE                      
         BAS   RE,EDITAM10         DON'T PRINT IF 0                             
                                                                                
PROCFEDX J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
                                                                                
*********************************************************************           
* THIS ROUTINE DISPLAYS THE CONTENTS OF A WITHHOLDING ELEMENT FOR THE           
* THE STATE OF WORK TO THE SCREEN                                               
*********************************************************************           
                                                                                
         USING TACWD,R4                                                         
PROCSOW  NTR1                                                                   
                                                                                
* CHECK IF WE NEED TO RESUME BECAUSE OF END OF SCREEN                           
                                                                                
         OC    RESUME_F,RESUME_F                                                
         BZ    PROCSOW3                                                         
                                                                                
         LA    R2,CK2UNITH                                                      
         SHI   R2,TLNQ                                                          
         STCM  R2,15,CK2SCNPTR        SCREEN POINTER                            
                                                                                
* WHILE IN THE MIDDLE OF PROCESSING A WORK STATE                                
* TEST IF EOS CAUSED BY CITY OF WORK PROCESSING                                 
                                                                                
         TM    RESUME_F,RESUME_GETCITY                                          
         BNO   PROCSOW3                                                         
         TM    RESUME_F,RESUME_PROCCOW+RESUME_CHKEOS1                           
         BNO   PROCSOW2                                                         
         CLI   RESUME_F,X'19'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DMCB(3),=C'Wrk'                                                  
         B     PROCSOW7                                                         
                                                                                
* WHILE IN THE MIDDLE OF PROCESSING A RES STATE                                 
* TEST IF EOS CAUSED BY CITY OF RESIDENT PROCESSING                             
                                                                                
PROCSOW2 TM    RESUME_F,RESUME_PROCCOR+RESUME_CHKEOS1                           
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   DMCB(3),=C'Res'                                                  
         B     PROCSOW7                                                         
                                                                                
PROCSOW3 ZICM  R2,CK2SCNPTR,(15)                                                
         USING TAXD,R2                                                          
         AHI   R2,TLNQ                                                          
         STCM  R2,15,CK2SCNPTR        BUMP TO UNIT SCREEN SECTION               
                                                                                
* CHECK END OF SCREEN - IF EOS SET APPROPRIATE FLAGS                            
                                                                                
         XC    RESUME_F,RESUME_F                                                
         BAS   RE,CHKEOS                                                        
         JE    *+16                                                             
         OI    RESUME_F,RESUME_PROCSOW                                          
         OI    RESUME_F,RESUME_CHKEOS1                                          
         J     XIT                                                              
                                                                                
* PROCESS THE UNPROTECTED FIELDS                                                
         MVC   TUNIT,TACWUNIT     DISPLAY UNIT IN STATE OF RESIDENCE            
         MVI   TUNITH+5,L'TUNIT+L'TWRO                                          
                                                                                
                                                                                
         L     R3,TACWTAX          DISPLAY AMOUNTS IN SCREEN                    
         LA    R5,TTAX                                                          
         BAS   RE,EDITAM10                                                      
         MVI   TTAXH+5,L'TTAX                                                   
                                                                                
         L     R3,TACWSUI                                                       
         LA    R5,TUNEMP                                                        
         MVI   TUNEMPH+5,L'TUNEMP                                               
         BAS   RE,EDITAM09                                                      
                                                                                
         L     R3,TACWSDI                                                       
                                                                                
         LA    R5,TDIS                                                          
         MVI   TDISH+5,L'TDIS                                                   
         BAS   RE,EDITAM09                                                      
                                                                                
         CLI   TACWLEN,TACWLN2Q                                                 
         JL    XIT                                                              
         L     R3,TACWSFLI                                                      
         LA    R5,TFLI                                                          
         MVI   TFLIH+5,L'TFLI                                                   
         BAS   RE,EDITAM09                                                      
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
                                                                                
         BE    PROCSOW4              ONLY DISPLAY UNITS WITH TATU               
         OC    TACWTAX,TACWTAX       ELEMENT OR IF TACWTAX IS WITHHELD          
         BNZ   PROCSOW4                                                         
         BAS   RE,PREVLINE                                                      
         B     PROCSOWX                                                         
                                                                                
PROCSOW4 LA    R5,TWAGE                                                         
         L     R3,DMCB2+4                                                       
         MVI   TWAGEH+5,L'TWAGE                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
         L     R3,DMCB2+8                                                       
         LA    R5,TNTRM                                                         
         MVI   TNTRMH+5,L'TNTRM                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
         L     R3,DMCB2+12                                                      
         LA    R5,TTXRM                                                         
         MVI   TTXRMH+5,L'TTXRM                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
* PROCESS THE PROTECTED FIELDS                                                  
PROCSOW5 MVI   TWRO,C' '                                                        
         XC    DMCB,DMCB                                                        
         MVC   DMCB(3),=C'Wrk'                                                  
         TM    TACWSTAT,TACWSRES   IF RESIDENCE BIT IS ON                       
         JNO   *+12                RESIDENT SCREEN UNIT ALSO                    
         MVI   TWRO,C'*'                                                        
                                                                                
PROCSOW7 GOTO1 GETCITY,DMCB                                                     
         ZICM  R2,CK2SCNPTR,(15)   GET NEW R2 IF BUMPED BY GETCITY              
                                                                                
         OC    RESUME_F,RESUME_F   EXIT IF END OF SCREEN ERROR                  
         JNZ   XIT                                                              
                                                                                
                                                                                
PROCSOWX J     XIT                                                              
         DROP  R2                                                               
         SPACE 3                                                                
                                                                                
*********************************************************************           
* THIS ROUTINE DISPLAYS THE CONTENTS OF A WITHHOLDING ELEMENT FOR THE           
* THE STATE OF RESIDENCE TO THE SCREEN.                                         
*********************************************************************           
                                                                                
         USING TACWD,R4                                                         
PROCSOR  NTR1                                                                   
                                                                                
* CHECK IF WE NEED TO RESUME BECAUSE OF END OF SCREEN                           
                                                                                
         OC    RESUME_F,RESUME_F                                                
         BZ    PROCSOR3                                                         
         LA    R2,CK2UNITH                                                      
         SHI   R2,TLNQ                                                          
         STCM  R2,15,CK2SCNPTR        SCREEN POINTER                            
* WHILE IN THE MIDDLE OF PROCESSING A RES STATE                                 
* TEST IF EOS CAUSED BY RESIDENT OF STATE PROCESSING                            
         TM    RESUME_F,RESUME_PROCSOR+RESUME_CHKEOS1                           
         BO    PROCSOR8                                                         
* WHILE IN THE MIDDLE OF PROCESSING A RES STATE                                 
* TEST IF EOS CAUSED BY CITY OF RES PROCESSING                                  
                                                                                
         TM    RESUME_F,RESUME_GETCITY                                          
         BNO   PROCSOR3                                                         
         TM    RESUME_F,RESUME_PROCCOR+RESUME_CHKEOS1                           
         BNO   PROCSOR3                                                         
         MVC   DMCB(3),=C'Wrk'                                                  
         B     PROCSOR8                                                         
                                                                                
PROCSOR3 ZICM  R2,CK2SCNPTR,(15)                                                
         AHI   R2,TLNQ                                                          
         STCM  R2,15,CK2SCNPTR        BUMP TO UNIT SCREEN SECTION               
         XC    RESUME_F,RESUME_F                                                
         BAS   RE,CHKEOS                                                        
         JE    *+16                                                             
         OI    RESUME_F,RESUME_PROCSOR                                          
         OI    RESUME_F,RESUME_CHKEOS1                                          
         J     XIT                                                              
         USING TAXD,R2                                                          
                                                                                
* PROCESS THE UNPROTECTED FIELDS                                                
         MVC   TUNIT,TACWUNIT     DISPLAY UNIT IN STATE OF RESIDENCE            
         OC    TUNIT,=X'404040'                                                 
         MVI   TUNITH+5,L'TUNIT+L'TWRO                                          
                                                                                
         L     R3,TACWTAX          DISPLAY AMOUNTS IN SCREEN                    
         LA    R5,TTAX                                                          
         BAS   RE,EDITAM10                                                      
         MVI   TTAXH+5,L'TTAX                                                   
                                                                                
         L     R3,TACWSUI                                                       
         LA    R5,TUNEMP                                                        
         MVI   TUNEMPH+5,L'TUNEMP                                               
         BAS   RE,EDITAM09                                                      
                                                                                
         L     R3,TACWSDI                                                       
         LA    R5,TDIS                                                          
         MVI   TDISH+5,L'TDIS                                                   
         BAS   RE,EDITAM09                                                      
                                                                                
         CLI   TACWLEN,TACWLN2Q                                                 
         JL    XIT                                                              
         L     R3,TACWSFLI                                                      
         LA    R5,TFLI                                                          
         MVI   TFLIH+5,L'TFLI                                                   
         BAS   RE,EDITAM09                                                      
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
                                                                                
         BE    PROCSOR4                ONLY DISPLAY UNIT IF TATU                
         OC    TACWTAX,TACWTAX         ELEMENT EXISTS OR TACWTAX                
         BNZ   PROCSOR4                IS WITHHELD                              
         BAS   RE,PREVLINE                                                      
*  IF UNIT=PA - PROCEED TO GET CITIES UNDER IT EVEN WITHOUT TATU ELEM           
*  PHL RESIDENTS GETS TAXED IF WORKED IN OTHER CITIES                           
         CLC   =C'PA',TACWUNIT                                                  
         BE    PROCSOR5                                                         
         CLC   =C'NY',TACWUNIT                                                  
         BE    PROCSOR5                                                         
         B     PROCSORX                                                         
                                                                                
PROCSOR4 L     R3,DMCB2+4                                                       
         LA    R5,TWAGE                                                         
         MVI   TWAGEH+5,L'TWAGE                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
         L     R3,DMCB2+8                                                       
         LA    R5,TNTRM                                                         
         MVI   TNTRMH+5,L'TNTRM                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
         L     R3,DMCB2+12                                                      
         LA    R5,TTXRM                                                         
         MVI   TTXRMH+5,L'TTXRM                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
* PROCESS THE PROTECTED FIELDS                                                  
PROCSOR5 MVI   TWRO,C'*'                                                        
         XC    DMCB,DMCB                                                        
         MVC   DMCB+3(3),=C'Res'                                                
                                                                                
PROCSOR8 BAS   RE,GETCITY                                                       
         ZICM  R2,CK2SCNPTR,(15)   GET NEW R2 IF BUMPED BY GETCITY              
         OC    RESUME_F,RESUME_F   EXIT IF END OF SCREEN ERROR                  
         JNZ   XIT                                                              
                                                                                
PROCSORX J     XIT                                                              
                                                                                
****************************************************************                
* GET A CITY FOR A STATE - AT THIS POINT TUNIT SHOULD HAVE STATE                
* --- THIS ROUTINE LOOKS UP THE CITIES FOR A STATE FROM TASYSUNITS              
* --- FOR EVERY VALID CITY DEFINED IN TASYSUNITS WE WILL LOOK FOR               
* --- CITY WITHOLDING ELEMENT IN THE RECORD AMD DISPLAY                         
* --- NOTE WE ASSUME THERE A CITY FOR A STATE COMBO IS ONLY                     
* --- SLOTED ONCE IN THE RECORD ELSE WE WILL HAVE A DISPLAY ISSUE               
*                                                                               
* INPUT - DMCB(3) = Res OR Wrk                                                  
*       - SVCITY= LAST SAVED CITY PROCESSED                                     
****************************************************************                
*                                                                               
GETCITY  NTR1                                                                   
         L     R1,TGAUNITS         R1=A(UNITS TABLE)                            
         USING TALUNITD,R1                                                      
GETC04   CLI   TALUCODE,X'FF'                                                   
         BE    GETCITYX                                                         
                                                                                
         CLC   TALUCODE,TACWUNIT   FIND THE STATE IN UNITS TABLE                
         BE    GETC05                                                           
         AHI   R1,TALUNEXT-TALUNITD                                             
         B     GETC04                                                           
* FOUND STATE                      IF THERE ARE NO CITY FOR THE STATE           
GETC05   XC    DUB,DUB             DEFINED IN THE UNITS TABLE THEN              
         AHI   R1,TALUNEXT-TALUNITD    THERE CAN NOT BE ANY CITIES              
         CLI   TALUCODE+2,X'40'        TO LOOK UP FOR THE STATE IN              
         BNH   GETCITYX                THE RECORD                               
                                                                                
         MVC   DUB(3),TALUCODE     DUB= CITY FOR THE STATE                      
* DO NOT SAVE CITY IF RESUMING FROM A CITY UNIT                                 
         OC    SVCITY,SVCITY                                                    
         BZ    GETC07                                                           
         OC    RESUME_F,RESUME_F                                                
         BZ    GETC07                                                           
         TM    RESUME_F,RESUME_PROCCOW   RESUME FROM PREVIOUS CITY              
         BNO   GETC07                                                           
         CLI   TALUCODE,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TALUCODE(3),SVCITY  GO BACK TILL WE FIND SVCITY                  
         BNE   GETC05                                                           
                                                                                
GETC07   MVC   SVCITY,TALUCODE     SAVE OFF THE CITY WE ARE PROCESSING          
                                                                                
* LOOK FOR THE CITY STORED IN DUB - UNLESS RESUME_F=RESUME_GETCITY              
* WHICH MEANS WE HIT END SCREEN BEFORE WHILE PROCESSING CITY                    
* THEN WE SHOULD RESUME FROM SVCITY                                             
                                                                                
GETC09   L     R4,AIO              RESTART ELEM LOOKUP                          
         MVI   ELCODE,TACWELQ      GET CHECK WITHHOLDING ELEMENT                
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
GETC10   BRAS  RE,NEXTEL                                                        
         BNE   GETC05               NEXT CITY IN UNITS TABLE                    
                                                                                
         USING TACWD,R4                                                         
         CLC   TACWUNIT(2),=C'FD'   SKIP FEDERAL WITHHOLDING ELEM               
         BE    GETC10                                                           
         CLI   TACWUNIT+2,X'40'     3RD CHAR FILLED  = CITY                     
         BNH   GETC10                                                           
                                                                                
* IF WRK STATE LOOK FOR WORK CITIES IF RES STATE LOOK FOR RES CITY              
* -------------CITY ------------                                                
         TM    RESUME_F,RESUME_GETCITY                                          
         BNO   GETC14                                                           
         CLC   TACWUNIT,SVCITY     MATCH ON CITY FOR THE STATE                  
         BNE   GETC20              FROM SVCITY                                  
         B     GETC15                                                           
                                                                                
GETC14   CLC   TACWUNIT,DUB        MATCH ON CITY FOR THE STATE                  
         BNE   GETC20                                                           
                                                                                
GETC15   CLC   DMCB(3),=C'Wrk'                                                  
         BNE   GETC18                                                           
                                                                                
         TM    TACWSTAT,TACWSWRK   IS THIS A WORK CITY WH ELEMENT?              
         BNO   GETC20              CHECK WORK BIT                               
         BAS   RE,PROCCOW          IF ON, PROCESS CITY OF WORK                  
         OC    RESUME_F,RESUME_F   IF END OF SCREEN ERROR - EXIT                
         BZ    GETC20              PROCESS NEXT ELEMENT                         
         OI    RESUME_F,RESUME_GETCITY    SET FLAG AND EXIT                     
         B     GETCITYX                                                         
GETC18   DS    0C                                                               
         BAS   RE,PROCCOR          IF ON, PROCESS CITY OF RESIDENT              
         OC    RESUME_F,RESUME_F   IF END OF SCREEN ERROR - EXIT                
         BZ    GETCITYX                                                         
         OI    RESUME_F,RESUME_GETCITY  SET FLAG AND EXIT                       
         B     GETCITYX                                                         
*                                                                               
GETC20   B     GETC10              KEEP LOOPING TILL NO MORE ELEMENTS           
GETC22   DS    0H                                                               
GETCITYX L     R4,STATEPTR                                                      
         J     XIT                                                              
*********************************************************************           
* THIS ROUTINE DISPLAYS THE CONTENTS OF A WITHHOLDING ELEMENT FOR THE           
* THE CITY OF RESIDENCE TO THE SCREEN.                                          
* THIS ROUTINE IS ONLY CALLED BY GETCITY ROUTINE                                
*********************************************************************           
                                                                                
         USING TACWD,R4                                                         
PROCCOR  NTR1                                                                   
         ZICM  R2,CK2SCNPTR,(15)                                                
         USING TAXD,R2                                                          
         AHI   R2,TLNQ                                                          
         STCM  R2,15,CK2SCNPTR        BUMP TO UNIT SCREEN SECTION               
         XC    RESUME_F,RESUME_F                                                
         BAS   RE,CHKEOS                                                        
         JE    *+16                                                             
         OI    RESUME_F,RESUME_PROCCOR                                          
         OI    RESUME_F,RESUME_CHKEOS1                                          
         J     XIT                                                              
                                                                                
* PROCESS THE PROTECTED FIELDS                                                  
         MVI   TWRO,C'*'                                                        
                                                                                
*                                                                               
* PROCESS THE UNPROTECTED FIELDS                                                
         MVC   TUNIT,TACWUNIT     DISPLAY UNIT IN STATE OF RESIDENCE            
         OC    TUNIT,=X'404040'                                                 
         MVI   TUNITH+5,L'TUNIT+L'TWRO                                          
*  DONT DISPLAY MARITAL STATUS AND EXCEMPTIONS IF WORKING                       
* BIT IS ON WHICH MEANS WE ALREADY DISPLAYED IT FOR WORKING UNIT                
                                                                                
         TM    TACWSTAT,TACWSRES+TACWSWRK                                       
         BO    PROCCORX                                                         
                                                                                
         L     R3,TACWTAX          DISPLAY AMOUNTS IN SCREEN                    
         LA    R5,TTAX                                                          
         BAS   RE,EDITAM10                                                      
         MVI   TTAXH+5,L'TTAX                                                   
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
                                                                                
         BE    PROCCOR4                ONLY DISPLAY UNIT IF TATU                
         OC    TACWTAX,TACWTAX         ELEMENT EXISTS OR TACWTAX                
         BNZ   PROCCOR4                IS WITHHELD                              
         BAS   RE,PREVLINE                                                      
         B     PROCCORX                                                         
                                                                                
PROCCOR4 L     R3,DMCB2+4                                                       
         LA    R5,TWAGE                                                         
         MVI   TWAGEH+5,L'TWAGE                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
         L     R3,DMCB2+8                                                       
         LA    R5,TNTRM                                                         
         MVI   TNTRMH+5,L'TNTRM                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
         L     R3,DMCB2+12                                                      
         LA    R5,TTXRM                                                         
         MVI   TTXRMH+5,L'TTXRM                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         MVC   TUNEMP,=C'    ---- '                                             
         MVI   TUNEMPH+5,L'TUNEMP                                               
         MVC   TDIS,=C'    ---- '                                               
         MVI   TDISH+5,L'TDIS                                                   
         MVC   TFLI,=C'    ---- '                                               
         MVI   TFLIH+5,L'TFLI                                                   
                                                                                
PROCCORX J     XIT                                                              
         SPACE 3                                                                
*********************************************************************           
* THIS ROUTINE DISPLAYS THE CONTENTS OF A WITHHOLDING ELEMENT FOR THE           
* THE CITY OF WORK TO THE SCREEN.                                               
* THIS ROUTINE IS ONLY CALLED BY GETCITY ROUTINE                                
*********************************************************************           
*                                                                               
         USING TACWD,R4                                                         
PROCCOW  NTR1                                                                   
         ZICM  R2,CK2SCNPTR,(15)                                                
         USING TAXD,R2                                                          
         AHI   R2,TLNQ                                                          
         STCM  R2,15,CK2SCNPTR        BUMP TO UNIT SCREEN SECTION               
         XC    RESUME_F,RESUME_F                                                
         BAS   RE,CHKEOS                                                        
         JE    *+16                                                             
         OI    RESUME_F,RESUME_PROCCOW                                          
         OI    RESUME_F,RESUME_CHKEOS1                                          
         J     XIT                                                              
                                                                                
* PROCESS THE UNPROTECTED FIELDS                                                
         MVC   TUNIT,TACWUNIT     DISPLAY UNIT IN STATE OF RESIDENCE            
         MVI   TUNITH+5,L'TUNIT+L'TWRO                                          
                                                                                
         L     R3,TACWTAX          DISPLAY AMOUNTS IN SCREEN                    
         LA    R5,TTAX                                                          
         BAS   RE,EDITAM10                                                      
         MVI   TTAXH+5,L'TTAX                                                   
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
                                                                                
         BE    PROCCOW4                ONLY DISPLAY UNIT IF TATU                
         OC    TACWTAX,TACWTAX         ELEMENT EXISTS OR TACWTAX                
         BNZ   PROCCOW4                IS WITHHELD                              
         BAS   RE,PREVLINE                                                      
         B     PROCCOWX                                                         
                                                                                
PROCCOW4 L     R3,DMCB2+4                                                       
         LA    R5,TWAGE                                                         
         MVI   TWAGEH+5,L'TWAGE                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
         L     R3,DMCB2+8                                                       
         LA    R5,TNTRM                                                         
         MVI   TNTRMH+5,L'TNTRM                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         XC    DMCB2(24),DMCB2                                                  
         ST    R4,DMCB2                                                         
         GOTO1 GETTATU,DMCB2                                                    
         L     R3,DMCB2+12                                                      
         LA    R5,TTXRM                                                         
         MVI   TTXRMH+5,L'TTXRM                                                 
         BAS   RE,EDITAM10                                                      
                                                                                
         MVC   TUNEMP,=C'    ---- '                                             
         MVI   TUNEMPH+5,L'TUNEMP                                               
         MVC   TDIS,=C'    ---- '                                               
         MVI   TDISH+5,L'TDIS                                                   
         MVC   TFLI,=C'    ---- '                                               
         MVI   TFLIH+5,L'TFLI                                                   
                                                                                
* PROCESS THE PROTECTED FIELDS                                                  
PROCCOW5 MVI   TWRO,C' '                                                        
         TM    TACWSTAT,TACWSRES   IF RESIDENCE BIT IS ON                       
         JNO   *+8                 RESIDENT SCREEN UNIT ALSO                    
         MVI   TWRO,C'*'                                                        
                                                                                
PROCCOWX J     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
* BUMP TO PREVIOUS DISPLAY LINE                                                 
***********************************************************************         
PREVLINE NTR1                                                                   
         ZICM  R2,CK2SCNPTR,(15)                                                
         USING TAXD,R2                                                          
         XC    TWRO,TWRO                                                        
         XC    TUNIT,TUNIT                                                      
         XC    TWAGE,TWAGE                                                      
         XC    TNTRM,TNTRM                                                      
         XC    TTXRM,TTXRM                                                      
         XC    TTAX,TTAX                                                        
         XC    TUNEMP,TUNEMP                                                    
         XC    TDIS,TDIS                                                        
         XC    TFLI,TFLI                                                        
         SHI   R2,TLNQ                                                          
         STCM  R2,15,CK2SCNPTR        BUMP TO UNIT SCREEN SECTION               
         J     XIT                                                              
***********************************************************************         
* THIS ROUTINE EDITS THE AMOUNT AT R3 TO THE ADDRESS AT R5.                     
***********************************************************************         
EDITAMT  DS    0H                                                               
         LTR   R3,R3               RETURN IF AMOUNT IS ZERO                     
         BZR   RE                                                               
*                                  EDIT AMOUNT                                  
EDITAM09 EDIT  (R3),(9,0(R5)),2,MINUS=Y,FLOAT=-,ZERO=BLANK,ALIGN=RIGHT          
         BR    RE                                                               
*                                                                               
EDITAM10 EDIT  (R3),(10,0(R5)),2,MINUS=Y,FLOAT=-,ZERO=BLANK,ALIGN=RIGHT         
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS FOR THE CORRESPONDING TATUELQ ELEMENT                      
* FOR STATE AND CITY                                                            
* DMCB= A(CURRENT TACW ELEMENT)                                                 
* OUTPUT:                                                                       
* DMCB+4 =WAGE                                                                  
* DMCB+8=NON TAXABLE NON-WAGES                                                  
* DMCB+12=TAXABLE NON WAGES                                                     
* CC : EQUALS = HAVE TATU, NO EQUAL = DONT HAVE TATU ELEMENT                    
***********************************************************************         
GETTATU  NTR1                                                                   
         L     R3,DMCB2            CURRENT TACW ELEMENT                         
         L     R4,AIO                                                           
         USING TACWD,R3                                                         
         MVI   ELCODE,TATUELQ      GET TAX UNIT ELEMENT                         
*                                                                               
         USING TATUD,R4                                                         
         BRAS  RE,GETEL                                                         
         BE    GETTATU3                                                         
         BNE   GETTATU5                                                         
*     HAVE TATU ELEMENT                                                         
                                                                                
GETTATU2 BRAS  RE,NEXTEL                                                        
         BNE   GETTATUX                                                         
GETTATU3 CLC   TACWUNIT,TATUUNIT                                                
         BNE   GETTATU2                                                         
GETATU4  MVC   DMCB2+4(4),TATUWAGE                                              
         MVC   DMCB2+12(4),TATUTNWA                                             
         MVC   DMCB2+8(4),TATUNNWA                                              
         J     GETTATUY                                                         
*     DONT HAVE TATU ELEMENT                                                    
GETTATU5 L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         USING TAPDD,R4                                                         
         BRAS  RE,GETEL                                                         
         JNE   GETTATUX                                                         
         MVC   DMCB2+4(4),=X'00000000'                                          
         MVC   DMCB2+12(4),=X'00000000'                                         
         MVC   DMCB2+8(4),TAPDREXP                                              
         CLI   TAPDLEN,TAPDLNQ                                                  
         BL    GETTATUN                                                         
         MVC   DMCB2+12(4),TAPDTXNW                                             
         MVC   DMCB2+8(4),TAPDNTNW                                              
         B     GETTATUN                                                         
                                                                                
GETTATUY CR    RB,RB                                                            
         J     GETTATUX                                                         
GETTATUN CR    RB,RE                                                            
         J     GETTATUX                                                         
GETTATUX XIT1                                                                   
***********************************************************************         
* CHECK END OF SCREEN                                                           
***********************************************************************         
CHKEOS   NTR1                                                                   
         ZICM  R2,CK2SCNPTR,(15)   BUMP TO UNIT SCREEN SECTION                  
         LA    RE,CK2LST                                                        
         CR    R2,RE                                                            
         BL    CHKEOSYS                                                         
         B     CHKEOSNO                                                         
CHKEOSNO DS    0C                                                               
         LA    R2,CK2UNITH                                                      
         STCM  R2,15,CK2SCNPTR        SCREEN POINTER                            
         CR    RE,RB                                                            
         J     XIT                                                              
CHKEOSYS CR    RE,RE                                                            
         J     XIT                                                              
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        ROUTINE SETS SCREEN                                                    
***********************************************************************         
                                                                                
SETSCR   NTR1                                                                   
         MVI   OVERLAY,SCR66                                                    
         L     R4,AIO                                                           
         MVI   ELCODE,TAATELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   SSCR10                                                           
         MVI   OVERLAY,SCR78                                                    
                                                                                
SSCR10   CLC   OVERLAY,TWASCR                                                   
         JE    XIT                                                              
         MVC   SV2CHKH,CK2CHKH                                                  
                                                                                
         LA    R3,CONTAGH                                                       
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVC   CK2CHKH(L'SV2CHKH),SV2CHKH                                       
         MVC   TWASCR,OVERLAY                                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DISPLAY RECORD ROUTINES                         *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TALIMCHK                                                       
       ++INCLUDE TADISTD1                                                       
**********************************************************************          
*        SAVED STORAGE                                               *          
**********************************************************************          
                                                                                
       ++INCLUDE TAGENFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR66D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR78D                                                       
         ORG   CK2WORK                                                          
CK2SCNPTR DS   A                                                                
PROCFEDF  DS   C                                                                
PROCFD   EQU   X'80'                                                            
PROCCN   EQU   X'40'                                                            
REXP      DS   F                                                                
STATEPTR  DS   A                                                                
STATEIDX  DS   A                                                                
MYBASERB  DS   A                                                                
SVCITY    DS   CL(L'TALUCODE)                                                   
DMCB2     DS   6F                                                               
SVGWAGE   DS   F                                                                
TATUFLG   DS   C                                                                
SVW4TYP   DS   C                                                                
DISFDFLG  DS   C                                                                
SV2CHKH   DS   XL(L'CK2CHKH+L'CK2CHK)                                           
                                                                                
*                                                                               
RESUME_F  DS   X              BITS TO IDICATE END OF SCREEN SCENARIO            
RESUME_PROCSOW EQU X'80'      IN MIDST OF PROCESSING PROCSOW ROUTINE            
RESUME_PROCSOR EQU X'40'      IN MIDST OF PROCESSING PROCSOR ROUTINE            
RESUME_PROCCOW EQU X'20'      IN MIDST OF PROCESSING PROCCOW ROUTINE            
RESUME_PROCCOR EQU X'10'      IN MIDST OF PROCESSING PROCCOR ROUTINE            
RESUME_GETCITY EQU X'08'      IN MIDST OF PROCESSING GETCITY ROUTINE            
RESUME_CHKEOS2 EQU X'02'      END OF SCREEN TRIGGER BY 2ND CHKEOS ROU           
RESUME_CHKEOS1 EQU X'01'      END OF SCREEN TRIGGER BY 1ST CHKEOS ROU           
         DS    D                                                                
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* DDPERVALD                                                                     
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
**********************************************************************          
*        DSECT TO COVER EACH TAX ENTRY                               *          
**********************************************************************          
                                                                                
TAXD     DSECT                                                                  
*TWROH    DS    XL8                                                             
*TWRO     DS    CL1                WORK/RESIDENT - * for RES                    
TUNITH   DS    XL8                                                              
TWRO     DS    CL1                UNIT                                          
TUNIT    DS    CL3                UNIT                                          
TWAGEH   DS    XL8                                                              
TWAGE    DS    CL10               WAGE                                          
TNTRMH   DS    XL8                                                              
TNTRM    DS    CL10               NON TAXABLE REIMB                             
TTXRMH   DS    XL8                                                              
TTXRM    DS    CL10               TAXABLE REIMB                                 
TTAXH    DS    XL8                                                              
TTAX     DS    CL10               TAX                                           
TUNEMPH  DS    CL8                UNEMPLOYMENT                                  
TUNEMP   DS    XL9                                                              
TDISH    DS    CL8                DISABILITY                                    
TDIS     DS    XL9                                                              
TFLIH    DS    CL8                FLI                                           
TFLI     DS    XL9                                                              
TLNQ     EQU   *-TAXD                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005TAGEN5C   12/30/14'                                      
         END                                                                    
