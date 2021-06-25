*          DATA SET TAGENB8    AT LEVEL 050 AS OF 04/15/04                      
*PHASE T702B8A                                                                  
         TITLE 'T702B8 - CERROR DISPLAY'                                        
T702B8   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702B8                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTAB                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    *+14                                                             
         MVC   SCESHED(7),=C'Pid Num'                                           
         OI    SCESHEDH+6,X'80'                                                 
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   CERR10                                                           
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
CERR10   CLI   MODE,DISPREC                                                     
         BNE   XIT                                                              
         BAS   RE,DISPLAY          DISPLAY THE RECORD                           
         B     XIT                                                              
         EJECT                                                                  
VKEY     NTR1                                                                   
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BNE   *+12                                                             
         TM    SCESSNH+4,X'20'     OR SSN HAS CHANGED                           
         BO    VK4                                                              
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK2                                                              
         CLI   SCESSNH+5,6                                                      
         BH    VK2                                                              
         MVC   TGPID,SCESSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK2                                                              
         MVC   SCESSN,TGSSN                                                     
         MVI   SCESSNH+5,9                                                      
VK2      GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SCESSNH),SCESSNNH                     
         NI    SCEAGYH+4,X'DF'     FORCE AGY RE-VALIDATION                      
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK4                                                              
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SCESSN,SSNSPACE                                                  
         MVC   SCESSN(L'TGPID),TGPID                                            
         MVI   SCESSNH+5,6                                                      
         OI    SCESSNH+6,X'80'                                                  
         SPACE 1                                                                
VK4      TM    SCEAGYH+4,X'20'     TEST AGENCY CHANGED                          
         BO    VK6                                                              
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SCEAGYH),SCEAGYNH                     
         NI    SCECIDH+4,X'DF'     FORCE CID RE-VALIDATION                      
         SPACE 1                                                                
VK6      TM    SCECIDH+4,X'20'     TEST COMMERCIAL ID CHANGED                   
         BO    VK8                                                              
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',SCECIDH),SCECIDNH                    
         L     R3,AIO                                                           
         SPACE 1                                                                
         USING TLCOD,R3                                                         
         MVC   TGCOM,TLCOCOM       SET GLOBAL INTERNAL COMM #                   
         NI    SCECATH+4,X'DF'     FORCE CATEGORY RE-VALIDATION                 
         SPACE 1                                                                
VK8      BAS   RE,VALCAT           VALIDATE CATEGORY                            
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CATEGORY FIELD                               
         SPACE 1                                                                
VALCAT   NTR1                                                                   
         LA    R2,SCECATH                                                       
         TM    4(R2),X'20'         IF CATEGORY CHANGED                          
         BO    VCX                                                              
         XC    TGCAT,TGCAT                                                      
         CLI   5(R2),0             OK TO LEAVE BLANK - I'LL LOOK UP             
         BE    VC10                                                             
         OC    8(3,R2),=C'   '                                                  
         GOTO1 CATVAL,DMCB,8(R2)   VALIDATE CATEGORY CODE                       
         BE    VC10                                                             
         SPACE 1                                                                
         MVC   FULL(3),=3C'0'      IF NOT VALID TEST IF VALID HEX               
         ZIC   R1,5(R2)                                                         
         LA    RF,L'FULL                                                        
         SR    RF,R1                                                            
         LA    RF,FULL(RF)         SET TO RIGHT-ALIGN IN FULL                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
         SPACE 1                                                                
         GOTO1 HEXIN,DMCB,FULL,TGFULL,4                                         
         OC    12(4,R1),12(R1)                                                  
         BZ    FLDINV                                                           
         SPACE 1                                                                
VC10     XC    KEY,KEY             BUILD CAST KEY                               
         LA    R3,KEY                                                           
         USING TLCAPD,R3                                                        
         MVI   TLCAPCD,TLCACCDQ                                                 
         MVC   TLCACSSN,TGSSN      SOCIAL SECURITY NUMBER                       
         MVC   TLCACCOM,TGCOM      INTERNAL COMMERCIAL NUMBER                   
         MVC   TLCACCAT,TGCAT      CATEGORY                                     
         GOTO1 HIGH                                                             
         SPACE 1                                                                
VC20     CLC   TLCAPKEY(TLCACCAT-TLCAPD),KEYSAVE  STILL SAME COMML              
         BNE   FLDINV                                                           
         OC    TGCAT,TGCAT         IF CATEGORY INPUT                            
         BZ    VC30                                                             
         CLC   TLCACCAT,TGCAT      TAKE FIRST MATCH                             
         BE    VC50                                                             
         B     VC40                                                             
         SPACE 1                                                                
VC30     CLI   5(R2),0             IF NO INPUT TAKE FIRST ONE FOUND             
         BE    VC50                                                             
         CLC   TLCACSEQ,TGFULL     ELSE SCAN FOR MATCHING SEQUENCE NO.          
         BE    VC50                                                             
         SPACE 1                                                                
VC40     GOTO1 SEQ                 GET NEXT CAST RECORD                         
         B     VC20                AND KEEP ON TRYING                           
         SPACE 1                                                                
VC50     GOTO1 HEXOUT,DMCB,TLCACSEQ,FULL,2,0  CVT SEQ TO CHAR.                  
         MVC   CASTSEQ,FULL+1                 SAVE 3 LOBS                       
         MVC   8(3,R2),TLCACCAT    DISPLAY ACTUAL CATEGORY                      
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'         SET VALIDATED                                
         NI    SCEPDH+4,X'DF'      FORCE PERIOD RE-VALIDATION                   
VCX      B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SCEFSTH,SCELSTH,PROT=Y                                           
         LA    R2,SCEFSTH                                                       
         ST    R2,ATHISERR         SET A(FIRST DISPLAY LINE)                    
         LA    R2,SCEPDH                                                        
         TM    4(R2),X'20'         TEST PERIOD CHANGED                          
         BO    DIS20                                                            
         XC    PDSTAT,PDSTAT                                                    
         ZIC   R1,5(R2)                                                         
         LTR   R0,R1               R0=R1=LENGTH OF INPUT                        
         BZ    DIS20               TEST SOMETHING INPUT                         
         SPACE                                                                  
         USING PERVALD,R3                                                       
         LA    R3,BLOCK            R3=A(BLOCK FOR PERVAL)                       
         BAS   RE,TESTONE          TEST ONLY ONE DATE INPUT                     
         BNE   DIS10                                                            
         AR    R1,R2                                                            
         AH    R1,=H'7'            R1=A(LAST CHAR INPUT)                        
         CLI   0(R1),C'*'          IF FIELD ENDS WITH AN ASTERISK               
         BNE   DIS10                                                            
         OI    PDSTAT,START        SET INPUT IS START AT DATE                   
         MVI   0(R1),C' '          CLEAR ASTERISK                               
         BCTR  R0,0                                                             
         STC   R0,5(R2)            SET LENGTH TO 1 LESS                         
         SPACE                                                                  
         GOTO1 DATVAL,DMCB,8(R2),DUB      VALIDATE DATE W/YEAR                  
         OC    0(4,R1),0(R1)                                                    
         BZ    DIS3                                                             
         CLC   5(1,R2),3(R1)                                                    
         BE    DIS5                                                             
         SPACE                                                                  
DIS3     GOTO1 DATVAL,DMCB,(1,8(R2)),DUB  ELSE VALIDATE DATE W/O YEAR           
         OC    0(4,R1),0(R1)                                                    
         BZ    DATEINV                                                          
         CLC   5(1,R2),3(R1)                                                    
         BNE   DATEINV                                                          
         MVC   DUB(2),TGTODAY0            SET TODAY'S YEAR                      
         SPACE                                                                  
DIS5     GOTO1 DATCON,DMCB,DUB,(8,8(R2))     RE-DISPLAY DATE                    
         GOTO1 DATCON,DMCB,DUB,(1,PDSTART)   SAVE PWOS                          
         MVI   16(R2),C'*'                   MOVE "*" BACK                      
         B     DIS20                                                            
         SPACE 1                                                                
DIS10    GOTO1 PDVAL,DMCB,(R3)     VALIDATE PERIOD                              
         MVC   PDSTART,PVALPSTA    SAVE DATES GENERATED BY PERVAL               
         MVC   PDEND,PVALPEND                                                   
         CLC   PDSTART,PDEND       IF END DATE NOT INPUT (END=START)            
         BNE   DIS20                                                            
         XC    PDEND,PDEND         CLEAR END DATE SET BY PERVAL                 
         XC    16(9,R2),16(R2)     ALSO CLEAR FROM SCREEN                       
         SPACE 1                                                                
         USING TACED,R4                                                         
DIS20    L     R4,AIO              R4=A(RECORD)                                 
         XR    R1,R1                                                            
         MVI   ELCODE,TACEELQ      SET TO LOOK UP CAST ERROR EL.                
         BAS   RE,GETEL                                                         
         BNE   DISX                                                             
         SPACE 1                                                                
         USING SCRND,R2                                                         
DIS30    L     R2,ATHISERR         R2=A(THIS DISPLAY LINE)                      
         SPACE                                                                  
         CLI   SCEPDH+5,0          IF PERIOD INPUT                              
         BE    DIS50                                                            
         CLC   PDSTART,TACECYCS    CAN'T BE GOOD IF INPUT > START               
         BH    DIS40                                                            
         TM    PDSTAT,START        IF INPUT ISN'T START AT DATE                 
         BO    DIS50                                                            
         CLC   PDSTART,TACECYCS    MATCH ON START DATE                          
         BNE   DIS40                                                            
         OC    PDEND,PDEND                                                      
         BZ    DIS50                                                            
         CLC   PDEND,TACECYCE      IF END DATE INPUT, MUST MATCH                
         BE    DIS50                                                            
         SPACE                                                                  
DIS40    BAS   RE,NEXTEL           TRY NEXT ELEMENT                             
         BE    DIS30                                                            
         B     DISX                                                             
         SPACE 1                                                                
DIS50    OC    TACEINV,TACEINV     TEST ERROR ADDED BY REOPEN                   
         BZ    DIS55                                                            
         GOTO1 DATCON,DMCB,(X'11',TACECYCS),(8,SCRCYC)                          
         GOTO1 TINVCON,DMCB,TACEINV,SCRINV,DATCON                               
         GOTO1 USEVAL,DMCB,(X'C0',TACEUSEQ) GET USE CODE                        
         BNE   *+10                                                             
         MVC   SCRUSE,TGUSCDE                                                   
DIS55    BAS   RE,DISERR           DISPLAY ERRORS AND SET NEXT LINE             
         ST    R2,ATHISERR         SAVE A(NEXT LINE)                            
         B     DIS40                                                            
         SPACE 1                                                                
DISX     OI    SCEPDH+4,X'20'      SET PERIOD VALIDATED                         
         OI    SCEPDH+6,X'80'                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TESTS IF ONLY ONE DATE WAS INPUT BY LOOKING              
*              FOR A "-".  RETURNS CC EQUAL IF ONE DATE, ELSE CC NOT EQ         
*              R2=A(FIELD), R1=LENGTH OF INPUT                                  
         SPACE                                                                  
TESTONE  NTR1                                                                   
         LA    R2,8(R2)                                                         
TSTONE5  CLI   0(R2),C'-'                                                       
         BE    NO                  IF FIND "-", RETURN CC NOT EQ                
         LA    R2,1(R2)                                                         
         BCT   R1,TSTONE5                                                       
         B     YES                 ELSE RETURN CC EQ                            
         SPACE                                                                  
*              ROUTINE DISPLAYS THE ERRORS IN TACEEL AT R4                      
*              R2=A(DISPLAY LINE)                                               
         SPACE                                                                  
         USING TACED,R4                                                         
DISERR   NTR1                                                                   
         ZIC   R3,TACENUM          R3=N'SUB ELEMENTS                            
         LA    R5,TACEERR          R5=A(ERROR SUB-EL)                           
         SPACE                                                                  
DISE5    EDIT  (1,0(R5)),(2,SCRERNUM),ALIGN=LEFT                                
         MVI   SCRDASH,C'-'                                                     
         GOTO1 CERROUT,DMCB,(0(R5),SCRERDES)                                    
         LA    R2,SCRNXT                                                        
         LA    R1,SCELSTH                                                       
         CR    R2,R1               IF PAST LAST DISPLAY LINE                    
         BH    NOROOM              GET OUT AND DISPLAY MESSAGE                  
         LA    R5,1(R5)            BUMP TO NEXT SUB-EL                          
         BCT   R3,DISE5                                                         
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              LOCAL ERROR/EXIT ROUTINES                                        
         SPACE                                                                  
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
DATEINV  MVI   ERROR,INVDATE       INVALID DATE                                 
         B     THEEND                                                           
         SPACE                                                                  
NOINPER  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
         SPACE                                                                  
NOTRK    MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
         SPACE                                                                  
NOROOM   MVI   MYMSGNO1,76         NO MORE ROOM ON SCREEN FOR ERRORS            
         B     INFEND                                                           
         SPACE 1                                                                
INFEND   OI    GENSTAT2,USGETTXT                                                
RECEND   LA    R2,CONRECH                                                       
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
SSNSPACE DS    CL9' '              SPACES FIELD FOR SS#                         
*                                                                               
         SPACE 2                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         SPACE                                                                  
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'FTRACK  ',CL8'REP     '                               
PF13     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYTWA,L'CASTSEQ-1),AL2(CASTSEQ-T702FFD)                   
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER SCREEN LINE                                       
SCRND    DSECT                                                                  
         DS    CL8                                                              
SCRINV   DS    CL6                                                              
         DS    CL4                                                              
SCRUSE   DS    CL3                                                              
         DS    CL5                                                              
SCRCYC   DS    CL17                                                             
         DS    CL4                                                              
SCRERNUM DS    CL2                                                              
SCRDASH  DS    CL1                                                              
         DS    CL1                                                              
SCRERDES DS    CL36                                                             
         SPACE                                                                  
SCRNXT   EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRB8D                                                       
         EJECT                                                                  
         ORG   SCEWORK                                                          
         SPACE                                                                  
*              DSECT TO COVER PROGRAM STORAGE AT END OF TWA0                    
         SPACE                                                                  
PDSTAT   DS    XL1                 PERIOD STATUS                                
START    EQU   X'80'               PERIOD INPUT IS START AT DATE                
         SPACE                                                                  
PDSTART  DS    XL3                 PERIOD START DATE                            
PDEND    DS    XL3                        END DATE                              
ATHISERR DS    A                   A(HEADER OF NEXT FIELD TO DISPLAY)           
CASTSEQ  DS    CL3                 CAST INPUT SEQ NUMBER (EBCDIC)               
         EJECT                                                                  
* DDGENTWA    (MUST FOLLOW LAST SCREEN)                                         
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050TAGENB8   04/15/04'                                      
         END                                                                    
