*          DATA SET TAGEN6F    AT LEVEL 001 AS OF 04/28/14                      
*PHASE T7026FA                                                                  
         TITLE 'T7026F - TD1 MAINTENANCE'                                       
T7026F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7026F,R7,RR=R2                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    R2,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JNE   *+8                                                              
         BRAS  RE,DK                                                            
         CLI   MODE,DISPREC        DISPLAY REC                                  
         JNE   *+8                                                              
         BRAS  RE,DREC                                                          
         CLI   MODE,VALREC         VALIDATE REC                                 
         JNE   *+8                                                              
         BRAS  RE,VREC                                                          
*                                                                               
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         JNE   *+8                                                              
         BRAS  RE,DREC                                                          
         J     XIT                                                              
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT                                
         J     ERRXIT                                                           
MISSERR  MVI   ERROR,MISSING       MISSING INPUT                                
         J     ERRXIT                                                           
ERRXIT   GOTO1 EXIT,DMCB,0                                                      
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
******************************************************************              
VK       NTR1  BASE=*,LABEL=*                                                   
         USING TLW4D,R3                                                         
         LA    R2,TD1PIDH                                                       
         CLI   5(R2),0                                                          
         JNE   VK02                                                             
         OC    TGSSN,TGSSN                                                      
         JZ    VK02                                                             
         MVC   TD1PID,TGSSN                                                     
         MVI   TD1PIDH+5,9                                                      
*                                                                               
VK02     CLI   TD1PIDH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BNE   VK06                RECVAL CALL DOES NOT CHECK LENGTH            
         MVC   WORK(9),=9X'F0'     CHECK FOR VALID NUMERIC                      
         MVZ   WORK(9),8(R2)                                                    
         CLC   WORK(9),=9X'F0'                                                  
         JNE   INVERR                                                           
         B     VK20                                                             
*                                                                               
VK06     CLI   TD1PIDH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         JNE   INVERR                                                           
         MVC   TGPID,TD1PID                                                     
         LA    RE,6                ENSURE ALL PID CHARACTERS                    
         LA    RF,TD1PID                                                        
VK10     CLI   0(RF),C'A'          ARE VALID ALPHANUMERIC                       
         BL    INVPID                                                           
         CLI   0(RF),C'9'                                                       
         BH    INVPID                                                           
         CLI   0(RF),C'Z'                                                       
         BNH   VK16                                                             
         CLI   0(RF),C'0'                                                       
         BL    INVPID                                                           
VK16     LA    RF,1(RF)                                                         
         BCT   RE,VK10                                                          
         GOTO1 SSNUNPK,DMCB,TD1PID,TGSSN                                        
         BNE   VK20                                                             
         MVC   TD1PID,TGSSN                                                     
         MVI   TD1PIDH+5,9                                                      
                                                                                
VK20     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'20',(R2))                                 
                                                                                
         L     R4,AIO              GET W4 ELEMENT                               
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,KEY              R3 = A(CHECK KEY)                            
         XC    KEY,KEY                                                          
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,TD1PID                                                   
         MVC   TLW4STA2,TAW4TYPE                                                
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING, DON'T CONVERT SSN                 
         JE    XIT                 TO PID ON SCREEN                             
         MVC   TGSSN,TLW4SSN      SOCIAL SECURITY NUMBER                        
         BRAS  RE,DISPID                                                        
*                                                                               
         L     R4,AIO              GET W4 ELEMENT                               
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TAW4TYPE,TAW4TYCA                                                
         JE    XIT                CANDADIAN                                     
         L     R4,AIO             OR IF W4 ALREADY HAVE TAD1ELQ ELEMENT         
         MVI   ELCODE,TAD1ELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   INVERR                                                           
*                                                                               
         J     XIT                                                              
*                                                                               
         XIT1                                                                   
INVPID   LA    R2,TD1PIDH          INVALID PID                                  
         J     INVERR                                                           
         DROP  R3                                                               
******************************************************************              
DK       NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R4,AIO              ADDRESS OF THE RECORD                        
         USING TLW4D,R4                                                         
         MVC   TD1PID,TLW4SSN      SOCIAL SECURITY NUMBER                       
         MVC   TGSSN,TLW4SSN      SOCIAL SECURITY NUMBER                        
         BRAS  RE,DISPID                                                        
                                                                                
         L     R4,AIO                                                           
         USING TLW4D,R4                                                         
         MVC   TD1TYPE,TLW4STA2                                                 
         MVI   TD1TYPEH+5,1                                                     
         OI    TD1TYPEH+6,X'80'                                                 
         J     XIT                                                              
         DROP  R4                                                               
*                                                                               
*********************************************************************           
*              ROUTINE TO DISPLAY PID INSTEAD OF SS#                            
DISPID   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   TD1PID,SPACES                                                    
         MVC   TD1PID(L'TGPID),TGPID                                            
         MVI   TD1PIDH+5,6                                                      
         OI    TD1PIDH+6,X'80'                                                  
DPIDX    XIT1                                                                   
******************************************************************              
*                                                                               
DREC     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    DR00                                                             
         GOTO1 FLDVAL,DMCB,(1,TD1TYPEH),(X'80',TD1NOT1H)                        
*                                                                               
DR00     GOTO1 FLDVAL,DMCB,(X'22',TD1TYPEH),TD1NOT1H                            
*                                                                               
         L     R4,AIO                                                           
         USING TLW4D,R4                                                         
         MVC   TD1TYPE,TLW4STA2                                                 
         MVI   TD1TYPEH+5,1                                                     
         OI    TD1TYPEH+6,X'80'                                                 
*                                                                               
         L     R4,AIO              GET W4 ELEMENT                               
         USING TAA2D,R4                                                         
         MVI   ELCODE,TAA2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DREC10                                                           
         MVC   TD1PPRO,TAA2ST                                                   
*                                                                               
DREC10   L     R4,AIO              GET W4 ELEMENT                               
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TD1NAM1,TAW4NAM1    DISPLAY FIRST NAME                           
         MVC   TD1NAM2,TAW4NAM2            LAST NAME                            
         CLI   TAW4LEN,TAW4LN2Q    NEW W4 NAME ELEMENT?                         
         BNE   *+16                NO - DON'T DISPLAY MID NAME/SUFFIX           
         MVC   TD1NAM3,TAW4MIDN    DISPLAY MIDDLE NAME                          
         MVC   TD1NAM4,TAW4SUFF            SUFFIX                               
*                                                                               
         XC    TD1AKA1,TD1AKA1                                                  
         XC    TD1AKA2,TD1AKA2                                                  
         L     R4,AIO              GET AKA ELEMENT                              
         USING TAAKD,R4                                                         
         MVI   ELCODE,TAAKELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR60                                                             
         MVC   TD1AKA1,TAAKNAM1            AKA FIRST NAME                       
         MVC   TD1AKA2,TAAKNAM2            AKA LAST NAME                        
         DROP  R4                                                               
                                                                                
DR60     NI    TD1ADDRH+1,X'FF'-X'20'                                           
         NI    TD1ADD2H+1,X'FF'-X'20'                                           
         NI    TD1ADD3H+1,X'FF'-X'20'                                           
         NI    TD1ADDCH+1,X'FF'-X'20'                                           
         NI    TD1ADDSH+1,X'FF'-X'20'                                           
         NI    TD1ADDZH+1,X'FF'-X'20'                                           
         NI    TD1CTRYH+1,X'FF'-X'20'                                           
                                                                                
         GOTO1 CHAROUT,DMCB,TAA2ELQ,(6,TD1ADDRH)   ADDRESS                      
                                                                                
         OI    TD1ADDRH+1,X'20'                                                 
         OI    TD1ADD2H+1,X'20'                                                 
         OI    TD1ADD3H+1,X'20'                                                 
         OI    TD1ADDCH+1,X'20'                                                 
         OI    TD1ADDSH+1,X'20'                                                 
         OI    TD1ADDZH+1,X'20'                                                 
         OI    TD1CTRYH+1,X'20'                                                 
                                                                                
         USING TAA2D,R4                                                         
         L     R4,AIO              GET ADDRESS ELEMENT                          
         MVI   ELCODE,TAA2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR80                                                             
         CLI   TAA2LEN,TAA2LNQ                                                  
         BL    DR80                                                             
         MVC   TD1CTRY,TAA2CTRY    COUNTRY                                      
         DROP  R4                                                               
                                                                                
DR80     BRAS  RE,DISTD1                                                        
                                                                                
DR90     DS    0C                                                               
         GOTO1 ACTVOUT,DMCB,(X'80',TD1LCHGH)                                    
                                                                                
         XC    TD1NOT1,TD1NOT1                                                  
         MVI   TD1NOT1H+5,L'NOTE1                                               
         OI    TD1NOT1H+6,X'80'                                                 
                                                                                
         L     R4,AIO              GET W4 ELEMENT                               
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
* NOT CANADIAN AND HAS TD1 ELEMENTS                                             
                                                                                
         CLI   TAW4TYPE,TAW4TYCA                                                
         JE    XIT                                                              
         MVC   TD1NOT1(L'NOTE1),NOTE1                                           
         J     XIT                                                              
         LTORG                                                                  
NOTE1    DC  C'TD1 was set up when performer was designed as Canadian'          
       ++INCLUDE TADISTD1                                                       
******************************************************************              
VREC     NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAD1ELQ      REMOVE TD1 ELEMENTS                          
         GOTO1 REMELEM                                                          
                                                                                
******************************************************************              
*        FEDERAL                                                 *              
******************************************************************              
                                                                                
         MVI   INPSTAT,0                                                        
                                                                                
         USING TAD1D,R4                                                         
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TAD1EL,TAD1ELQ      INITIALIZE TD1 FEDERAL ELEMENT               
         MVI   TAD1LEN,TAD1LNQ                                                  
         OI    TAD1STAT,TAD1SCAN                                                
                                                                                
         CLI   TD1FNCLH+5,0        IF FEDERAL NET CLAIM AMOUNT IS               
         JE    VR10                PROVIDED, VALIDATE IT                        
         LA    R2,TD1FNCLH                                                      
         ZIC   R0,TD1FNCLH+5                                                    
         GOTO1 CASHVAL,DMCB,(2,TD1FNCL),(R0)                                    
         CLI   0(R1),0                                                          
         JNE   INVERR                                                           
         CLC   =F'10000000',DMCB+4   < 100,000.00?                              
         JNH   INVERR                                                           
         MVC   TAD1NCL1,DMCB+4                                                  
         OI    INPSTAT,ISCCAENT                                                 
                                                                                
VR10     CLI   TD1FDC1H+5,0        IF FEDERAL DEFAULT TO CLAIM CODE             
         JNE   VR20                1 IS EMPTY, DEFAULT IT TO N                  
         MVI   TD1FDC1,C'N'                                                     
         MVI   TD1FDC1H+5,1                                                     
         J     VR30                                                             
                                                                                
VR20     CLI   TD1FDC1,C'N'        IF FEDERAL DEFAULT TO CLAIM CODE             
         JE    VR30                1 IS PROVIDED, VALIDATE IT                   
         LA    R2,TD1FDC1H                                                      
         CLI   TD1FDC1,C'Y'                                                     
         JNE   INVERR                                                           
         TM    INPSTAT,ISCCAENT    NOT ALLOWED IF NET CLAIM AMOUNT              
         JO    INVERR              IS PROVIDED                                  
         OI    TAD1STAT,TAD1SCC1                                                
         OI    INPSTAT,ISDC1ENT                                                 
                                                                                
VR30     CLI   TD1FEXMH+5,0        IF FEDERAL EXEMPT IS EMPTY,                  
         JNE   VR40                DEFAULT IT TO N                              
         MVI   TD1FEXM,C'N'                                                     
         MVI   TD1FEXMH+5,1                                                     
         J     VR50                                                             
                                                                                
VR40     CLI   TD1FEXM,C'N'        IF FEDERAL EXEMPT IS PROVIDED,               
         JE    VR50                VALIDATE IT                                  
         LA    R2,TD1FEXMH                                                      
         CLI   TD1FEXM,C'Y'        NOT ALLOWED IF NET CLAIM AMOUNT              
         JNE   INVERR              OR DEFAULT TO CC1 IS PROVIDED                
         TM    INPSTAT,ISCCAENT+ISDC1ENT                                        
         JNZ   INVERR                                                           
         OI    TAD1STAT,TAD1SEXM                                                
         OI    INPSTAT,ISEXEMPT                                                 
                                                                                
VR50     TM    INPSTAT,ISCCAENT+ISDC1ENT+ISEXEMPT                               
         JNZ   VR60                                                             
         LA    R2,TD1FNCLH        ENSURE ONLY 1 OF NET CLAIM AMOUNT,            
         J     MISSERR            DEFAULT TO CC1 AND EXEMPT IS PROVIDED         
                                                                                
VR60     CLI   TD1FPREH+5,0                                                     
         JE    VR70                                                             
         LA    R2,TD1FPREH                                                      
         CLI   TD1FEXM,C'Y'                                                     
         JE    INVERR                                                           
         ZIC   R0,TD1FPREH+5                                                    
         GOTO1 CASHVAL,DMCB,(2,TD1FPRE),(R0)                                    
         CLI   0(R1),0                                                          
         JNE   INVERR                                                           
         MVC   TAD1PZON,DMCB+4                                                  
                                                                                
VR70     CLI   TD1FCPPH+5,0        EXCLUDE FROM CPP                             
         JE    VR80                                                             
         LA    R2,TD1FCPPH                                                      
         CLI   TD1FCPP,C'N'                                                     
         JE    VR80                                                             
         CLI   TD1FCPP,C'Y'                                                     
         JNE   INVERR                                                           
         OI    TAD1STAT,TAD1SECP                                                
                                                                                
VR80     GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
******************************************************************              
*        PROVINCIAL                                              *              
******************************************************************              
                                                                                
         MVI   INPSTAT,0                                                        
                                                                                
         USING TAD1D,R4                                                         
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TAD1EL,TAD1ELQ      INITIALIZE TD1 PROVINCE ELEMENT              
         MVI   TAD1LEN,TAD1LNQ                                                  
         OI    TAD1STAT,TAD1SPRO                                                
                                                                                
         CLI   TD1PNCLH+5,0        IF PROVINCE NET CLAIM AMOUNT IS              
         JE    VR90                PROVIDED, VALIDATE IT                        
         LA    R2,TD1PNCLH                                                      
         ZIC   R0,TD1PNCLH+5                                                    
         GOTO1 CASHVAL,DMCB,(2,TD1PNCL),(R0)                                    
         CLI   0(R1),0                                                          
         JNE   INVERR                                                           
         CLC   =F'10000000',DMCB+4   < 100,000.00?                              
         JNH   INVERR                                                           
         MVC   TAD1NCL1,DMCB+4                                                  
         OI    INPSTAT,ISCCAENT                                                 
                                                                                
VR90     CLI   TD1PDC1H+5,0        IF PROVINCE DEFAULT TO CLAIM CODE            
         JNE   VR100               1 IS EMPTY, DEFAULT IT TO N                  
         MVI   TD1PDC1,C'N'                                                     
         MVI   TD1PDC1H+5,1                                                     
         J     VR110                                                            
                                                                                
VR100    CLI   TD1PDC1,C'N'        IF PROVINCE DEFAULT TO CLAIM CODE            
         JE    VR110               1 IS PROVIDED, VALIDATE IT                   
         LA    R2,TD1PDC1H                                                      
         CLI   TD1PDC1,C'Y'                                                     
         JNE   INVERR                                                           
         TM    INPSTAT,ISCCAENT    NOT ALLOWED IF NET CLAIM AMOUNT              
         JO    INVERR              IS PROVIDED                                  
         OI    TAD1STAT,TAD1SCC1                                                
         OI    INPSTAT,ISDC1ENT                                                 
                                                                                
VR110    CLI   TD1PEXMH+5,0        IF PROVINCE EXEMPT IS EMPTY,                 
         JNE   VR120               DEFAULT IT TO N                              
         MVI   TD1PEXM,C'N'                                                     
         MVI   TD1PEXMH+5,1                                                     
         J     VR130                                                            
                                                                                
VR120    CLI   TD1PEXM,C'N'        IF PROVINCE EXEMPT IS PROVIDED,              
         JE    VR130               VALIDATE IT                                  
         LA    R2,TD1PEXMH                                                      
         CLI   TD1PEXM,C'Y'        NOT ALLOWED IF NET CLAIM AMOUNT              
         JNE   INVERR              OR DEFAULT TO CC1 IS PROVIDED                
         TM    INPSTAT,ISCCAENT+ISDC1ENT                                        
         JNZ   INVERR                                                           
         OI    TAD1STAT,TAD1SEXM                                                
         OI    INPSTAT,ISEXEMPT                                                 
                                                                                
VR130    TM    INPSTAT,ISCCAENT+ISDC1ENT+ISEXEMPT                               
         JNZ   VR140                                                            
         LA    R2,TD1PNCLH        ENSURE ONLY 1 OF NET CLAIM AMOUNT,            
         J     MISSERR            DEFAULT TO CC1 AND EXEMPT IS PROVIDED         
                                                                                
VR140    CLI   TD1PPREH+5,0                                                     
         JE    VR150                                                            
         LA    R2,TD1PPREH                                                      
         CLI   TD1PEXM,C'Y'                                                     
         JE    INVERR                                                           
         ZIC   R0,TD1PPREH+5                                                    
         GOTO1 CASHVAL,DMCB,(2,TD1PPRE),(R0)                                    
         CLI   0(R1),0                                                          
         JNE   INVERR                                                           
         MVC   TAD1PZON,DMCB+4                                                  
                                                                                
VR150    CLI   TD1PHDEH+5,0                                                     
         JE    VR170                                                            
         LA    R2,TD1PHDEH                                                      
         CLC   TD1PPRO,=C'QC'                                                   
         JNE   INVERR                                                           
         CLI   TD1PEXM,C'Y'                                                     
         JE    INVERR                                                           
         ZIC   R0,TD1PHDEH+5                                                    
         GOTO1 CASHVAL,DMCB,(2,TD1PHDE),(R0)                                    
         CLI   0(R1),0                                                          
         JNE   INVERR                                                           
         MVC   TAD1HOUS,DMCB+4                                                  
                                                                                
VR170    CLI   TD1PSPYH+5,0                                                     
         JE    VR180                                                            
         LA    R2,TD1PSPYH                                                      
         CLC   TD1PPRO,=C'QC'                                                   
         JNE   INVERR                                                           
         CLI   TD1PEXM,C'Y'                                                     
         JE    INVERR                                                           
         ZIC   R0,TD1PSPYH+5                                                    
         GOTO1 CASHVAL,DMCB,(2,TD1PSPY),(R0)                                    
         CLI   0(R1),0                                                          
         JNE   INVERR                                                           
         MVC   TAD1SPRT,DMCB+4                                                  
                                                                                
VR180    CLI   TD1PEHCH+5,0                                                     
         JE    VR190                                                            
         LA    R2,TD1PEHCH                                                      
         CLC   TD1PPRO,=C'QC'                                                   
         JNE   INVERR                                                           
         CLI   TD1PEXM,C'Y'                                                     
         JE    INVERR                                                           
         CLI   TD1PEHC,C'N'                                                     
         JE    VR190                                                            
         CLI   TD1PEHC,C'Y'                                                     
         JNE   INVERR                                                           
         CLI   TD1PEXM,C'Y'                                                     
         JE    INVERR                                                           
         OI    TAD1STAT,TAD1SEHC                                                
                                                                                
VR190    GOTO1 ADDELEM                                                          
                                                                                
******************************************************************              
                                                                                
         GOTO1 ACTVIN,DMCB,(X'80',0)                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR76D                                                       
         EJECT                                                                  
*                                                                               
         ORG   TD1WORK                                                          
*                                                                               
RELO     DS    F                                                                
INPSTAT  DS    X                                                                
ISCCAENT EQU   X'80'               CLAIM CODE AMOUNT ENTERED                    
ISDC1ENT EQU   X'40'               DEFAULT TO CLAIM CODE 1 ENTERED              
ISEXEMPT EQU   X'20'               EXEMPT ENTERED                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         SPACE 5                                                                
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENEQUS                                                                     
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TAGEN6F   04/28/14'                                      
         END                                                                    
