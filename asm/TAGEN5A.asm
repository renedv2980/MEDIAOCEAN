*          DATA SET TAGEN5A    AT LEVEL 002 AS OF 06/22/15                      
*PHASE T7025AB                                                                  
*INCLUDE TAPOST                                                                 
         TITLE 'T7025A - BRATE/BOVER/HIST2'                                     
T7025A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLNQ,T7025A,RR=R2,R7                                          
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    RE,APTRBLK                                                       
         AHI   RE,L'PTRBLK                                                      
         ST    RE,ATPSTBLK                                                      
         ST    R2,RELO                                                          
                                                                                
         BRAS  RE,INIT             INITIALIZE PROGRAM                           
                                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JNE   *+8                                                              
         BRAS  RE,DK                                                            
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+8                                                              
         BRAS  RE,DREC                                                          
                                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         JNE   *+8                                                              
         BRAS  RE,VREC                                                          
                                                                                
         CLI   MODE,RECDEL         DELETE RECORD                                
         JNE   *+8                                                              
         BRAS  RE,DE                                                            
                                                                                
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         JNE   MAIN10                                                           
         MVC   LISTAR,SPACES                                                    
         LA    R2,LISTAR                                                        
         BRAS  RE,LREC                                                          
                                                                                
MAIN10   CLI   MODE,PRINTREP       PRINT RECORD                                 
         JNE   XIT                                                              
         ZAP   COUNTER,=P'0'                                                    
         XC    KEY,KEY                                                          
         LARL  R2,MYSPECS                                                       
         CLI   RECNUM,BR                                                        
         BNE   *+10                                                             
         LARL  R2,MYSPEC2                                                       
         CLI   RECNUM,BV                                                        
         BNE   *+10                                                             
         LARL  R2,MYSPEC3                                                       
         ST    R2,SPECS                                                         
         LA    R2,P+1                                                           
         BRAS  RE,LREC                                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR MESSSAGES AND EXITS                                    *         
***********************************************************************         
                                                                                
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         J     ERREXIT                                                          
                                                                                
ERRINVA  LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT        INVALID ACTION                               
         J     ERREXIT                                                          
                                                                                
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT                                
         J     ERREXIT                                                          
                                                                                
ERRDEL   MVI   ERROR,ERINVDEL      CANNOT DELETE RECORD                         
         J     ERREXIT                                                          
                                                                                
ERRNFND  MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         NI    4(R2),X'FF'-X'20'                                                
         J     ERREXIT                                                          
                                                                                
ERRZERO  LHI   RE,ERMUS532         ZERO AMOUNTS FOUND, HIT PF18                 
         STH   RE,MYMSGNO                                                       
         J     ERREXIT2                                                         
                                                                                
ERRREX   LA    R2,BRAAGYH                                                       
         MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         NI    4(R2),X'FF'-X'20'                                                
         J     ERREXIT                                                          
                                                                                
ERRCHG   LA    R2,BOVINVH                                                       
         MVI   ERROR,ERINVPD       CANT CHANGE, INVOICE PAID/BILLED             
         J     ERREXIT                                                          
                                                                                
ERRBRADD LHI   RE,ERRADDBR         MUST ADD BRATE FIRST                         
         STH   RE,MYMSGNO                                                       
         J     ERREXIT2                                                         
                                                                                
ERRAGYBT LHI   RE,ERRBPF16         AGENCY BILL TYPE WILL APPLY                  
         STH   RE,MYMSGNO                                                       
         J     ERREXIT2                                                         
                                                                                
ERRBILL  LA    R2,CONACTH                                                       
         LHI   RE,ERMUS525         INVOICE BILLED                               
         STH   RE,MYMSGNO                                                       
         J     ERREXIT2                                                         
                                                                                
INFADD   MVI   MYMTYP,GTMINF                                                    
         LHI   RE,282                                                           
         STH   RE,MYMSGNO                                                       
         MVI   BLOCK,0                                                          
                                                                                
ERREXIT2 MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
INFEND   OI    GENSTAT2,USGETTXT                                                
         J     ERREXIT                                                          
                                                                                
ERREXIT  GOTO1 EXIT,DMCB,0                                                      
                                                                                
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
*        ROUTINE INITIALIZES PROGRAM                                  *         
***********************************************************************         
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         CLI   TWASCR,SCR5A        IF ON BRATE SCREEN                           
         JNE   INIT10                                                           
         CLI   BRACLIH+5,0         AND CLIENT IS EMPTY                          
         JNE   INIT10                                                           
         TM    PROSTAT,PSLRBOV     AND WE'RE COMING FROM BOVER                  
         JZ    INIT10                                                           
         OC    TGCLI,TGCLI         AND GLOBAL CLIENT IS DEFINED                 
         JZ    INIT10                                                           
         MVC   BRACLI,TGCLI        FILL IT IN AUTOMATICALLY                     
         NI    BRACLIH+4,X'FF'-X'20'                                            
         MVI   BRACLIH+5,L'BRACLI                                               
         OI    BRACLIH+6,X'80'                                                  
                                                                                
INIT10   NI    PROSTAT,X'FF'-PSLRBOV                                            
         CLI   TWASCR,SCR60                                                     
         JNE   INIT20                                                           
         OI    PROSTAT,PSLRBOV                                                  
                                                                                
INIT20   CLI   TWASCR,SCR62                                                     
         JNE   INIT30                                                           
         GOTO1 FLDVAL,DMCB,(X'80',BRLSELH),(X'80',999)                          
         JE    INIT30                                                           
         MVC   TIQSKEY,FRSTKEY                                                  
                                                                                
INIT30   OI    GENSTAT4,NODELLST                                                
                                                                                
         LA    R2,PFTAB                                                         
         CLI   RECNUM,BR                                                        
         JE    INIT40                                                           
         LA    R2,H2PFTAB                                                       
         CLI   RECNUM,HT                                                        
         JE    INIT40                                                           
         LA    R2,HRPFTAB                                                       
         CLI   RECNUM,HR                                                        
         JE    INIT40                                                           
         LA    R2,BOLPFTAB                                                      
         CLI   ACTNUM,ACTLIST                                                   
         JE    INIT40                                                           
         LA    R2,BOPFTAB                                                       
INIT40   GOTO1 INITIAL,DMCB,(R2)                                                
                                                                                
         LA    RE,STATTAB            SAVE A(STATTUS TABLE)                      
         ST    RE,ASTATTAB                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        PF KEY TABLE FOR BRATE                                       *         
***********************************************************************         
                                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,PFTNOSEL)                   
         DC    CL3'   ',CL8'BRATE',CL8'LIST'                                    
PF13     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'AGY',CL8'DIS'                                       
PF14     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'CLI',CL8'DIS'                                       
PF15     DC    AL1(KEYTYGLB,L'TGCLI-1),AL2(TGCLI-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
PF15X    EQU   *                                                                
         DC    AL1(PF16X-*,16,0,0,PFTRETRN)                                     
PF16     DC    CL3'   ',CL8'        ',CL8'        '                             
PF16X    EQU   *                                                                
         DC    AL1(PF18X-*,18,0,(PF18X-PF18)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'BOVER',CL8'DIS'                                     
PF18     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'SVINV-1),AL2(SVINV-T702FFD)                       
PF18X    EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        PF KEY TABLE FOR HIST2                                       *         
***********************************************************************         
                                                                                
H2PFTAB  DS    0C                                                               
         DC    AL1(H2PF13X-*,13,0,(H2PF13X-H2PF13)/KEYLNQ,PFTNOSEL)             
         DC    CL3'   ',CL8'BRATE',CL8'LIST'                                    
H2PF13   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
H2PF13X  EQU   *                                                                
         DC    AL1(H2PF14X-*,14,0,(H2PF14X-H2PF14)/KEYLNQ,0)                    
         DC    CL3'   ',CL8'AGY',CL8'DIS'                                       
H2PF14   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
H2PF14X  EQU   *                                                                
         DC    AL1(H2PF15X-*,15,0,(H2PF15X-H2PF15)/KEYLNQ,0)                    
         DC    CL3'   ',CL8'CLI',CL8'DIS'                                       
H2PF15   DC    AL1(KEYTYGLB,L'TGCLI-1),AL2(TGCLI-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
H2PF15X  EQU   *                                                                
         DC    AL1(H2PF16X-*,16,0,(H2PF16X-H2PF16)/KEYLNQ,0)                    
         DC    CL3'   ',CL8'HRATE',CL8'DIS'                                     
H2PF16   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'SVINV-1),AL2(SVINV-T702FFD)                       
H2PF16X  EQU   *                                                                
         DC    AL1(H2PF17X-*,17,0,(H2PF17X-H2PF17)/KEYLNQ,0)                    
         DC    CL3'   ',CL8'BOVER',CL8'DIS'                                     
H2PF17   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'SVINV-1),AL2(SVINV-T702FFD)                       
H2PF17X  EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        PF KEY TABLE FOR HRATE                                       *         
***********************************************************************         
                                                                                
HRPFTAB  DS    0C                                                               
         DC    AL1(HRPF13X-*,13,0,(HRPF13X-HRPF13)/KEYLNQ,PFTNOSEL)             
         DC    CL3'   ',CL8'HIST',CL8'DIS'                                      
HRPF13   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'SVINV-1),AL2(SVINV-T702FFD)                       
HRPF13X  EQU   *                                                                
         DC    AL1(HRPF14X-*,14,0,(HRPF14X-HRPF14)/KEYLNQ,0)                    
         DC    CL3'   ',CL8'HIST2',CL8'DIS'                                     
HRPF14   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'SVINV-1),AL2(SVINV-T702FFD)                       
HRPF14X  EQU   *                                                                
         DC    AL1(HRPF15X-*,15,0,(HRPF15X-HRPF15)/KEYLNQ,0)                    
         DC    CL3'   ',CL8'BRATE',CL8'DIS'                                     
HRPF15   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCLI-1),AL2(TGCLI-TGD)                           
HRPF15X  EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        PF KEY TABLE FOR BOVER MAINTENANCE                           *         
***********************************************************************         
                                                                                
BOPFTAB  DS    0C                                                               
         DC    AL1(BOPF13X-*,13,0,(BOPF13X-BOPF13)/KEYLNQ,PFTNOSEL)             
         DC    CL3'   ',CL8'BOVER',CL8'LIST'                                    
BOPF13   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
BOPF13X  EQU   *                                                                
         DC    AL1(BOPF14X-*,14,0,(BOPF14X-BOPF14)/KEYLNQ,0)                    
         DC    CL3'   ',CL8'AGY',CL8'DIS'                                       
BOPF14   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
BOPF14X  EQU   *                                                                
         DC    AL1(BOPF15X-*,15,0,(BOPF15X-BOPF15)/KEYLNQ,0)                    
         DC    CL3'   ',CL8'CLI',CL8'DIS'                                       
BOPF15   DC    AL1(KEYTYGLB,L'TGCLI-1),AL2(TGCLI-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
BOPF15X  EQU   *                                                                
         DC    AL1(BOPF16X-*,16,0,(BOPF16X-BOPF16)/KEYLNQ,0)                    
         DC    CL3'   ',CL8'HIST2',CL8'DIS'                                     
BOPF16   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'SVINV-1),AL2(SVINV-T702FFD)                       
BOPF16X  EQU   *                                                                
         DC    AL1(BOPF17X-*,17,0,(BOPF17X-BOPF17)/KEYLNQ,0)                    
         DC    CL3'   ',CL8'COM',CL8'DIS'                                       
BOPF17   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
BOPF17X  EQU   *                                                                
         DC    AL1(BOPF18X-*,18,0,0,PFTRETRN)                                   
         DC    CL3'   ',CL8'        ',CL8'        '                             
BOPF18X  EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        PF KEY TABLE FOR BOVER LIST                                  *         
***********************************************************************         
                                                                                
BOLPFTAB DS    0C                                                               
         DC    AL1(BOLPF13X-*,13,0,(BOLPF13X-BOLPF13)/KEYLNQ,0)                 
         DC    CL3'   ',CL8'BOVER',CL8'DIS'                                     
BOLPF13  DC    AL1(KEYTYCUR,L'OLSTAGY-1),AL2(OLSTAGY-OLISTD)                    
         DC    AL1(KEYTYCUR,L'OLSTINV-1),AL2(OLSTINV-OLISTD)                    
BOLPF13X EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        BOVER STATUS TABLE                                           *         
***********************************************************************         
                                                                                
STATTAB  DS    0CL11                                                            
         DC    AL1(TARASB10),CL10'TYPE10'        TYPE 10                        
         DC    AL1(TARASB13),CL10'TYPE13'        TYPE 13                        
         DC    AL1(TARASB16),CL10'TYPE16'        TYPE 16                        
         DC    X'FF'                                                            
***********************************************************************         
*        CONSTANTS AND LITERALS FOR INIT ROUTINES                     *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY KEY                                       *         
***********************************************************************         
                                                                                
DK       NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         BAS   RE,DKBR            DISPLAY BRATE                                 
         BAS   RE,DKBO            OR BOVER KEY FIELDS                           
         BRAS  RE,VK              THEN GO VALIDATE KEY                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY KEY FOR BRATE MAINTENANCE                 *         
*        ON ENTRY ... R4=A(SELECTED RECORD)                           *         
***********************************************************************         
                                                                                
DKBR     NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,BR          CLEAR ALL KEY FIELDS AND MARK                 
         JNE   XIT                THEM UNVALIDATED                              
                                                                                
         GOTO1 FLDVAL,DMCB,(X'11',BRAAGYH),(X'80',BRACLIH)                      
                                                                                
         USING TLAYD,R4                                                         
         CLI   0(R4),TLAYCDQ      IF WE HAVE AN AGENCY RECORD                   
         JNE   DKBR10             FILL IN THE AGENCY FIELD                      
         MVC   BRAAGY,TLAYAGY                                                   
         MVI   BRAAGYH+5,L'BRAAGY                                               
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         USING TLCLD,R4                                                         
DKBR10   MVC   BRAAGY,TLCLAGY      IF WE HAVE A CLIENT RECORD                   
         MVI   BRAAGYH+5,L'BRAAGY  FILL IN THE AGENCY AND CLIENT                
         MVC   BRACLI,TLCLCLI      FIELDS                                       
         MVI   BRACLIH+5,L'BRACLI                                               
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY KEY FOR BRATE MAINTENANCE                 *         
*        ON ENTRY ... R4=A(SELECTED RECORD)                           *         
***********************************************************************         
                                                                                
DKBO     NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,BV           CLEAR ALL KEY FIELDS AND MARK                
         JNE   XIT                 THEM UNVALIDATED                             
         GOTO1 FLDVAL,DMCB,(X'11',BOVAGYH),(X'80',BOVINVH)                      
                                                                                
         USING TLIND,R4                                                         
         MVC   BOVAGY,TLINAGY      FILL IN THE AGENCY AND INVOICES              
         MVI   BOVAGYH+5,L'BOVAGY  FIELDS                                       
         XC    TLININV,=6X'FF'                                                  
         GOTO1 TINVCON,DMCB,TLININV,BOVINV,DATCON                               
         MVI   BOVINVH+5,L'BOVINV                                               
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
**********************************************************************          
*        CONSTANTS AND LITERALS FOR DK ROUTINES                                 
**********************************************************************          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,STKYFLDS                                                      
         GOTO1 FLDVAL,DMCB,(X'40',(R2)),(X'80',(R4))                            
         JNE   VK10                                                             
         CLC   RECNUM,TWALREC                                                   
         JNE   VK10                                                             
         CLC   ACTNUM,TWALACT                                                   
         JE    XIT                                                              
         CLI   ACTNUM,ACTADD                                                    
         JE    VK10                                                             
         CLI   ACTNUM,ACTDIS                                                    
         JE    VK10                                                             
         CLI   TWALACT,ACTADD                                                   
         JE    VK10                                                             
         CLI   TWALACT,ACTSEL                                                   
         JNE   XIT                                                              
                                                                                
VK10     XC    VARS(VARSLNQ),VARS  CLEAR ALL VARIABLES                          
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
                                                                                
         LA    R3,KEY              VALIDATE KEY                                 
         BAS   RE,VKBRBO           FOR BRATE OR BOVER MAINTENANCE               
         BAS   RE,VKBRL            OR BRATE LIST                                
         BAS   RE,VKBOL            OR BOVER LIST                                
         BAS   RE,VKH2             OR HIST2 MAINTENANCE                         
         BAS   RE,VKHR             OR HRATE MAINTENANCE                         
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         JNE   *+8                                                              
         MVI   KEY,X'11'                                                        
                                                                                
         GOTO1 FLDVAL,DMCB,(X'20',(R2)),(X'80',(R4))                            
         J     XIT                 MARK ALL FIELDS VALIDATED                    
                                                                                
***********************************************************************         
*        ROUTINE SETS R2 WITH THE FIRST KEY FIELD AND R4 WITH THE     *         
*        HEADER OF THE LAST KEY FIELD                                 *         
***********************************************************************         
                                                                                
STKYFLDS NTR1                                                                   
         LA    R2,BRAAGYH          SET FIRST AND LAST FIELD                     
         LA    R4,BRACLIH          FOR BRATE MAINTENANCE                        
         CLI   TWASCR,SCR5A                                                     
         JE    SKFX                                                             
         LA    R2,BRLAGYH          OR BRATE LIST                                
         LA    R4,BRLFMTH                                                       
         CLI   TWASCR,SCR62                                                     
         JE    SKFX                                                             
         LA    R2,BOVAGYH          OR BOVER MAINTENANCE                         
         LA    R4,BOVINVH                                                       
         CLI   TWASCR,SCR60                                                     
         JE    SKFX                                                             
         LA    R2,BOLAGYH          OR BOVER LIST                                
         LA    R4,BOLINVH                                                       
         CLI   TWASCR,SCR63                                                     
         JE    SKFX                                                             
         CLI   TWASCR,SCR44        OR INVOICE APPROVE                           
         JE    SKFX                                                             
         LA    R2,HI2AGYH          OR HIST2 MAINTENANCE                         
         LA    R4,HI2INVH                                                       
         CLI   TWASCR,SCR61                                                     
         JE    SKFX                                                             
         LA    R2,HRTAGYH          OR HRATE MAINTENANCE                         
         LA    R4,HRTINVH                                                       
         CLI   TWASCR,SCR77                                                     
         JE    SKFX                                                             
         DC    H'00'                                                            
SKFX     XIT1  REGS=(R2,R4)                                                     
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY FOR BRATE AND BOVER MAINTENANCE  *         
***********************************************************************         
                                                                                
VKBRBO   NTR1                                                                   
         BAS   RE,VKBR             VALIDATE KEY FIELDS FOR BRATE                
         JE    VKBRBO10            MAINTENANCE                                  
         BAS   RE,VKBO             OR BOVER MAINTENANCE                         
         JNE   XIT                                                              
                                                                                
VKBRBO10 L     R4,AIO              IF AGENCY/CLIENT/INVOICE                     
         MVI   ELCODE,TARAELQ      RECORD DOES NOT HAVE A TARAELQ               
         BRAS  RE,GETEL            ELEMENT YET, ACTION MUST BE ADD              
         JE    VKBRBO20                                                         
         CLI   ACTNUM,ACTADD                                                    
         JNE   ERRNFND                                                          
         J     XIT                                                              
                                                                                
VKBRBO20 CLI   ACTNUM,ACTADD       IF AGENCY/CLIENT/INVOICE RECORD              
         JE    ERRREX              DOES HAVE A TARAELQ ELEMENT ALREADY          
         J     XIT                 ACTION CANNOT BE ADD                         
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY FOR BRATE MAINTENANCE            *         
***********************************************************************         
                                                                                
VKBR     NTR1                                                                   
         CLI   RECNUM,BR           IF RECORD IS BRATE                           
         JNE   NO                                                               
         CLI   ACTNUM,ACTLIST      AND ACTION IS NOT LIST                       
         JE    NO                                                               
                                                                                
         GOTO1 FLDVAL,DMCB,(1,BRAAGYNH),1  CLEAR NAME FIELDS                    
         GOTO1 (RF),(R1),(1,BRACLINH),1                                         
                                                                                
***********************************************************************         
                                                                                
         LA    R2,BRAAGYH          VALIDATE AGENCY (AIO1)                       
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',(R2)),BRAAGYNH                        
         BRAS  RE,SVAGY            AND SET AGENCY VARIABLES                     
                                                                                
***********************************************************************         
                                                                                
         CLI   BRACLIH+5,0         IF CLIENT IS ENTERED                         
         JE    VKBR10                                                           
         LA    R2,BRACLIH                                                       
         TM    PROSTAT,PSAGYBR     AGENCY MUST HAVE A BRATE                     
         JZ    ERRBRADD                                                         
         CLC   TGAGY,=CL6'8800'    AGENCY CANNOT BE 8800                        
         JE    ERRINV                                                           
         MVC   AIO,AIO2            VALIDATE CLIENT (AIO2)                       
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'38',(R2)),BRACLINH                        
                                                                                
         USING TLCLD,R4                                                         
         L     R4,AIO2                                                          
         OC    TLCLAGY,TLCLAGY     CLIENT CANNOT BE GLOBAL                      
         JZ    ERRINV                                                           
         DROP  R4                                                               
                                                                                
         USING TACID,R4                                                         
         L     R4,AIO2             SET CLIENT VARIABLES                         
         MVI   ELCODE,TACIELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VKBR10                                                           
         TM    TACISTAT,TACISLCK                                                
         JZ    VKBR10                                                           
         OI    PROSTAT,PSACLCK                                                  
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
VKBR10   CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JNE   YES                 DISPLAY AGENCY VALUES                        
         GOTO1 FLDVAL,DMCB,(2,BRABTYPH),999                                     
         GOTO1 (RF),(R1),(1,BRAAGCH),1                                          
         GOTO1 (RF),(R1),(1,BRAACH),(X'80',BRASRH)                              
         GOTO1 (RF),(R1),(1,BRASIGH),1                                          
         BRAS  RE,DRAGYFLD                                                      
         MVC   AIO,AIO2                                                         
                                                                                
         USING TABRD,R4                                                         
         CLI   BRACLIH+5,0         IF CLIENT IS ENTERED                         
         JE    YES                                                              
         L     R4,AIO2             DISPLAY BILLING TYPE                         
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
VKBR20   BRAS  RE,NEXTEL                                                        
         JNE   YES                                                              
         TM    TABRSTAT,TABRSACP   SKIP IF ADDITIONAL RATE                      
         JO    VKBR20                                                           
         EDIT  (1,TABRTYPE),(2,BRABTYP),ALIGN=LEFT                              
         STC   R0,BRABTYPH+5                                                    
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY FOR BOVER MAINTENANCE            *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
***********************************************************************         
                                                                                
VKBO     NTR1                                                                   
         CLI   ACTNUM,ACTAPP                                                    
         JE    VKBO10                                                           
         CLI   RECNUM,BV           IF RECORD IS BOVER                           
         JNE   NO                                                               
         CLI   ACTNUM,ACTLIST      AND ACTION IS NOT LIST                       
         JE    NO                                                               
                                                                                
VKBO10   LA    R2,BOVAGYH          ENSURE AGENCY EXISTS (AIO1)                  
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',(R2))                                 
         L     R4,AIO                                                           
         MVI   ELCODE,TARAELQ      ENSURE AGENCY HAS BRATE                      
         BRAS  RE,GETEL                                                         
         JNE   ERRBRADD                                                         
         BRAS  RE,SVAGY            AND SET AGENCY VARIABLES                     
                                                                                
         GOTOR VKINV,DMCB,BOVINVH  ENSURE INVOICE EXISTS (AIO2)                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE KEY FOR BRATE LIST                       *         
***********************************************************************         
                                                                                
VKBRL    NTR1                                                                   
         CLI   RECNUM,BR           IF RECORD IS BRATE                           
         JNE   XIT                                                              
         CLI   ACTNUM,ACTLIST      AND ACTION IS LIST                           
         JNE   XIT                                                              
                                                                                
         CLI   BRLAGYH+5,0         IF AGENCY IS SUPPLIED                        
         JE    VKBRL10             ENSURE IT EXISTS                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'30',BRLAGYH)                              
         MVC   TIFAGY,TGAGY                                                     
         OC    TIFAGY,SPACES       SET FILTER                                   
         BRAS  RE,SVAGY            AND SET AGENCY VARIABLES                     
                                                                                
VKBRL10  CLI   BRLCLIH+5,0         IF CLIENT IS SUPPLIED                        
         JE    VKBRL20                                                          
         LA    R2,BRLAGYH          ENSURE THAT AGENCY IS SUPPLIED               
         CLI   5(R2),0                                                          
         JE    ERRINV              ENSURE CLIENT EXISTS                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'30',BRLCLIH)                              
         MVC   TIFCLI,TGCLI        AND SET FILTER                               
         OC    TIFCLI,SPACES                                                    
                                                                                
VKBRL20  CLI   BRLBTYPH+5,0        IF BILLING TYPE IS SUPPLIED                  
         JE    VKBRL30                                                          
         LA    R2,BRLBTYPH                                                      
         GOTO1 VALINUM             ENSURE IT EXISTS                             
         MVC   TIFBTYP,ACTUAL                                                   
         GOTO1 BTYPVAL,DMCB,TIFBTYP                                             
         JNE   ERRINV                                                           
         TM    TGBTSTAT,BTYPSDFT   AND ENSURE ITS NOT DEFUNCT                   
         JO    ERRINV              AND SET FILTER                               
                                                                                
VKBRL30  CLI   BRLOFCH+5,0         IF OFFICE IS SUPPLIED                        
         JE    VKBRL40                                                          
         MVC   TIFOFF,BRLOFC       SET FILTER                                   
                                                                                
VKBRL40  CLI   BRLSVCH+5,0         IF SERVICE IS PROVIDED                       
         JE    VKBRL60                                                          
         MVC   TIFSVC,BRLSVC                                                    
         CLC   BRLSVC,=C'PRE'      VALIDATE IT                                  
         JE    VKBRL60             AND SET FILTER                               
         CLC   BRLSVC,=C'BAS'                                                   
         JNE   ERRINV                                                           
                                                                                
VKBRL60  CLI   BRLFMTH+5,0         IF FORMAT IS PROVIDED                        
         JE    XIT                                                              
         CLI   BRLFMT,C'A'         VALIDATE FORMAT IS A OR C                    
         JE    XIT                                                              
         CLI   BRLFMT,C'C'                                                      
         JE    XIT                                                              
         LA    R2,BRLFMTH                                                       
         J     ERRINV                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE KEY FOR BOVER LIST                       *         
***********************************************************************         
                                                                                
VKBOL    NTR1                                                                   
         CLI   RECNUM,BV           IF RECORD IS BOVER                           
         JNE   XIT                                                              
         CLI   ACTNUM,ACTLIST      AND ACTION IS LIST                           
         JNE   XIT                                                              
                                                                                
         CLI   BOLAGYH+5,0         IF AGENCY IS PROVIDED                        
         JE    VKBOL10             ENSURE IT EXISTS                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'30',BOLAGYH)                              
         MVC   TIFAGY,TGAGY        AND SET FILTER                               
         OC    TIFAGY,SPACES                                                    
                                                                                
VKBOL10  CLI   BOLINVH+5,0         IF INVOICE IS PROVIDED                       
         JE    VKBOL20             ENSURE IT EXISTS                             
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'04',BOVINVH)                              
         GOTO1 TINVCON,DMCB,BOVINV,TIFINV,DATCON                                
         CLI   0(R1),X'FF'                                                      
         JNE   VKBOL20             AND SET FILTER                               
         LA    R2,BOLINVH                                                       
         J     ERRINV                                                           
                                                                                
VKBOL20  XC    TIQSTART,TIQSTART   SET UP SYSIO VARIABLES                       
         MVC   TIUSERID,TWAORIG                                                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLINOCDQ                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY FOR HIST2 MAINTANENCE            *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
***********************************************************************         
                                                                                
VKH2     NTR1                                                                   
         CLI   RECNUM,HT           IF RECORD IS HIST2                           
         JNE   XIT                                                              
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,HI2AGYH VALIDATE AGENCY                      
         GOTOR VKINV,DMCB,BOVINVH          AND INVOICE                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY FOR HRATE MAINTANENCE            *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
***********************************************************************         
                                                                                
VKHR     NTR1                                                                   
         CLI   RECNUM,HR           IF RECORD IS HRATE                           
         JNE   XIT                                                              
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,HRTAGYH VALIDATE AGENCY                      
         GOTOR VKINV,DMCB,BOVINVH          AND INVOICE                          
         J     XIT                                                              
                                                                                
**********************************************************************          
*        CONSTANTS AND LITERALS FOR VKEY ROUTINES                               
**********************************************************************          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE RECORD                                *         
***********************************************************************         
                                                                                
DREC     NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'24',0)                                    
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         BAS   RE,DRBR             DISPLAY BRATE                                
         BAS   RE,DRBO             OR BOVER                                     
         BAS   RE,DRH2             OR HIST2                                     
         BRAS  RE,DRHR             OR HRATE                                     
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY BRATE MAINTENANCE                         *         
*        ON ENTRY ... AIO1 = A(AGENCY RECORD)                         *         
*                     AIO2 = A(CLIENT RECORD)                         *         
***********************************************************************         
                                                                                
DRBR     NTR1                                                                   
         CLI   RECNUM,BR                                                        
         JNE   XIT                                                              
                                                                                
         BRAS  RE,BRSETIO                                                       
                                                                                
         GOTO1 FLDVAL,DMCB,(1,BRABTYPH),(X'80',999)                             
         GOTO1 (RF),(R1),(1,BRAOFCH),1                                          
         GOTO1 (RF),(R1),(1,BRALCHGH),1                                         
         GOTO1 (RF),(R1),(1,BRASIGH),1                                          
                                                                                
         CLI   ACTNUM,ACTDEL                                                    
         JE    XIT                                                              
                                                                                
         BRAS  RE,DRAGYFLD         DISPLAY AGENCY VALUES                        
                                                                                
         MVC   AIO,AIO1            SET AIO TO BE AGENCY OR CLIENT               
         CLI   BRACLIH+5,0                                                      
         JE    DRBR10                                                           
         MVC   AIO,AIO2                                                         
                                                                                
         USING TABRD,R4                                                         
DRBR10   L     R4,AIO              DISPLAY BILLING TYPE                         
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DRBR15   BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TABRSTAT,TABRSACP   SKIP IF ADDITIONAL RATE                      
         JO    DRBR15                                                           
         EDIT  (1,TABRTYPE),(2,BRABTYP),ALIGN=LEFT                              
         STC   R0,BRABTYPH+5                                                    
         DROP  R4                                                               
                                                                                
         USING TARAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TARAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         BRAS  RE,DISSTAT                                                       
         GOTO1 DISRATE,DMCB,BRAHFBH,TARAHFB                                     
         GOTO1 (RF),(R1),BRAHFBCH,TARAHFBC                                      
         GOTO1 (RF),(R1),BRAHFPH,TARAHFP                                        
                                                                                
         BRAS  RE,CKBLTYP          CHECK FOR REUSE BILL TYPE                    
         BNE   DRBR18                                                           
         GOTO1 DISRATE,DMCB,BRARFBH,TARARFB                                     
         GOTO1 (RF),(R1),BRARFBCH,TARARFBC                                      
         GOTO1 (RF),(R1),BRARFPH,TARARFP                                        
                                                                                
DRBR18   MVI   BRAAGR,C'N'                                                      
         CLI   TARAGRUL,0          DISPLAY APP GRT RULE                         
         JE    DRBR20                                                           
         EDIT  (1,TARAGRUL),(1,BRAAGR),ALIGN=LEFT,ZERO=BLANK                    
         STC   R0,BRAAGRH+5                                                     
                                                                                
DRBR20   MVC   BRAWB,=C'NOLIM  '   DISPLAY WAGE BREAK                           
         TM    TARASTA1,TARASBNL                                                
         JO    DRBR30                                                           
         MVC   BRAWB,=C'TIER   '                                                
         TM    TARASTA1,TARASBTI                                                
         JO    DRBR30                                                           
         MVC   BRAWB,=C'FICA   '                                                
         TM    TARASTA1,TARASBFI                                                
         JO    DRBR30                                                           
         MVC   BRAWB,=C'DEFAULT'                                                
                                                                                
DRBR30   GOTO1 DISRATE,DMCB,BRATFUH,TARATFU                                     
         GOTO1 (RF),(R1),BRATSUH,TARATSU                                        
         GOTO1 (RF),(R1),BRATFIH,TARATFI                                        
         GOTO1 (RF),(R1),BRATMH,TARATM                                          
         GOTO1 (RF),(R1),BRATWCH,TARATWC                                        
                                                                                
         GOTO1 CHAROUT,DMCB,(X'80',TACMELQ),BRACMNTH,TACMTBRA                   
                                                                                
         GOTO1 ACTVOUT,DMCB,(X'80',BRALCHGH)                                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY BOVER MAINTENANCE                         *         
*        ON ENTRY ... AIO1 = A(AGENCY RECORD)                         *         
*                     AIO2 = A(INVOICE RECORD)                        *         
***********************************************************************         
                                                                                
DRBO     NTR1                                                                   
         CLI   ACTNUM,ACTAPP                                                    
         JE    DRBO00                                                           
         CLI   RECNUM,BV                                                        
         JNE   XIT                                                              
                                                                                
DRBO00   LA    R3,KEY                                                           
         GOTOR VKINV,DMCB,BOVINVH                                               
                                                                                
         GOTO1 FLDVAL,DMCB,(1,BOVCOMH),BOVSVCH                                  
         GOTO1 (RF),(R1),(1,BOVBTYPH),1                                         
         GOTO1 (RF),(R1),(1,BOVCLIH),1                                          
         GOTO1 (RF),(R1),(1,BOVHFBH),(X'80',999)                                
         GOTO1 (RF),(R1),(1,BOVLCHGH),1                                         
                                                                                
         CLI   ACTNUM,ACTAPP       IF COMING FROM APPROVE, PROTECT              
         JNE   DRBO10              ALL FIELDS                                   
         GOTO1 (RF),(R1),(8,BOVAGYH),999                                        
                                                                                
         USING TACOD,R4                                                         
DRBO10   L     R4,AIO2                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   DRBO30                                                           
         MVC   BOVCOM,TACOCID                                                   
         MVI   BOVCOMH+5,L'BOVCOM                                               
         DROP  R4                                                               
                                                                                
         USING TARAD,R4                                                         
         L     R4,AIO2          IF INVOICE IS BILLED OR PUR PRINTED,            
         MVI   ELCODE,TARAELQ   INVOICE WILL HAVE BILLING TYPE                  
         BRAS  RE,GETEL                                                         
         JNE   DRBO20                                                           
         EDIT  (1,TARATYPE),(2,BOVBTYP),ALIGN=LEFT                              
         STC   R0,BOVBTYPH+5                                                    
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
DRBO20   L     R4,AIO2                                                          
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   BOVCLI,TAPDCLI                                                   
         MVI   BOVCLIH+5,L'BOVCLI                                               
         MVC   TGCLI,TAPDCLI                                                    
         DROP  R4                                                               
                                                                                
DRBO30   GOTOR DRSVC,DMCB,BOVSVCH                                               
                                                                                
         USING TARAD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TARAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         TM    TARASTA2,TARASSHD                                                
         JZ    DRBO40                                                           
         GOTO1 DISRATE,DMCB,BOVHFBH,TARAHFB                                     
                                                                                
DRBO40   TM    TARASTA2,TARASSPD                                                
         JZ    DRBO50                                                           
         GOTO1 DISRATE,DMCB,BOVHFPH,TARAHFP                                     
                                                                                
DRBO50   TM    TARASTA2,TARASSCD                                                
         JZ    DRBO60                                                           
         GOTO1 DISRATE,DMCB,BOVHFBCH,TARAHFBC                                   
                                                                                
DRBO60   TM    TARASTA2,TARASFUD                                                
         JZ    DRBO70                                                           
         GOTO1 DISRATE,DMCB,BOVTFUH,TARATFU                                     
                                                                                
DRBO70   TM    TARASTA2,TARASSUD                                                
         JZ    DRBO80                                                           
         GOTO1 DISRATE,DMCB,BOVTSUH,TARATSU                                     
                                                                                
DRBO80   TM    TARASTA2,TARASFID                                                
         JZ    DRBO90                                                           
         GOTO1 DISRATE,DMCB,BOVTFIH,TARATFI                                     
                                                                                
DRBO90   TM    TARASTA2,TARASMED                                                
         JZ    DRBO100                                                          
         GOTO1 DISRATE,DMCB,BOVTMH,TARATM                                       
                                                                                
DRBO100  TM    TARASTA2,TARASWCD                                                
         JZ    DRBO110                                                          
         GOTO1 DISRATE,DMCB,BOVTWCH,TARATWC                                     
                                                                                
DRBO110  GOTO1 CHAROUT,DMCB,TACMELQ,(2,BOVCMNTH),TACMTBRA                       
                                                                                
         MVC   TGBYTE3,TWASCR                                                   
         MVI   TWASCR,SCR60                                                     
         GOTO1 ACTVOUT,DMCB,(X'80',BOVLCHGH)                                    
         MVC   TWASCR,TGBYTE3                                                   
                                                                                
         CLI   ACTNUM,ACTAPP                                                    
         JNE   XIT                                                              
         XC    TGINV,=6X'FF'                                                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY HIST2 MAINTENANCE                         *         
*        ON ENTRY ... AIO1 = A(AGENCY RECORD)                         *         
*                     AIO2 = A(INVOICE RECORD)                        *         
***********************************************************************         
                                                                                
DRH2     NTR1                                                                   
         CLI   RECNUM,HT                                                        
         JNE   XIT                                                              
                                                                                
         GOTO1 FLDVAL,DMCB,(2,HI2AGYH),(0,999)                                  
         GOTO1 (RF),(R1),(1,HI2DESH),(X'80',999)                                
                                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',CMPINV)                               
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                    IF NO COMML, INV NOT PAID YET             
         MVC   HI2COM,TACOCID                                                   
         DROP  R4                                                               
                                                                                
         USING TPD,R2                                                           
         L     R2,ATPSTBLK                                                      
         LHI   RF,TPLNQ                                                         
         XCEFL 0(R2),(RF)                                                       
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   HI2CLI,TAPDCLI                                                   
         CLC   TAPDEMP,=C'PP '        IF EMPLOYER IS PP                         
         BNE   *+8                                                              
         OI    TPOPTS1,TPOPRNT        SET PRINT OPTION                          
         CLC   TAPDEMP,=C'P+ '        IF EMPLOYER IS P+                         
         BNE   *+8                                                              
         OI    TPOPTS1,TPOPLUS        SET PARYROLL PLUS OPTION                  
         DROP  R4                                                               
                                                                                
         MVC   TPINVCON,TINVCON                                                 
         MVC   TPRCVAL,RECVAL                                                   
         MVC   TPHEX,TGACCHX                                                    
         MVC   TPUSER,TGUSERID                                                  
                                                                                
         OI    TPOPTS1,TPOHIST2                                                 
         ST    RC,TPRC                A(GENCON)                                 
         MVI   TPTYPE,TPTYPEB         BILLING                                   
         MVC   TPFAGY,TGAGY           FILTER AGENCY                             
         MVC   TPFINV,CMPINV          FILTER INVOICE                            
         XC    TPFINV,=6X'FF'                                                   
         LA    RF,HI2DESH                                                       
         ST    RF,TPHISTLN            A(FIRST DESCRIPTION FIELD)                
         LA    RF,HI2DES2H                                                      
         ST    RF,TPHISLN2            A(FIRST DESCRIPTION 2ND COL)              
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TPSDATE,TAINBDTE                                                 
         OC    TPSDATE,TPSDATE       MUST BE BILLED                             
         JZ    XIT                                                              
         DROP  R4                                                               
                                                                                
         GOTO1 =V(TAPOST),DMCB,TPD,RR=RELO                                      
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY PERCENTAGE/AMOUNT                         *         
*        ON ENTRY ... P1 = A(SCREEN FIELD)                            *         
*                     P2 = A(ELEMENT FIELD)                           *         
***********************************************************************         
                                                                                
DISRATE  NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         CLI   0(R3),X'FF'                                                      
         JNE   DRATE10                                                          
         MVC   8(4,R2),=C'0.00 '                                                
         J     XIT                                                              
DRATE10  CLI   RECNUM,BR                                                        
         JE    DRATE20                                                          
         EDIT  (4,0(R3)),(9,8(R2)),2,ALIGN=LEFT,ZERO=BLANK                      
         STC   R0,5(R2)                                                         
         J     XIT                                                              
DRATE20  EDIT  (4,0(R3)),(5,8(R2)),2,ALIGN=LEFT,ZERO=NOBLANK                    
         STC   R0,5(R2)                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR DREC ROUTINES                     *         
***********************************************************************         
BOVER    DC    CL5'BOVER'                                                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY HRATE MAINTENANCE                         *         
*        ON ENTRY ... AIO1 = A(AGENCY RECORD)                         *         
*                     AIO2 = A(INVOICE RECORD)                        *         
***********************************************************************         
                                                                                
DRHR     NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,HR                                                        
         JNE   XIT                                                              
                                                                                
         XC    TGBTYPE,TGBTYPE                                                  
         XC    TGCLI,TGCLI                                                      
                                                                                
         GOTO1 FLDVAL,DMCB,(2,HRTAGYH),(0,999)                                  
         GOTO1 FLDVAL,DMCB,(3,HRTCOMH),(X'80',999)                              
                                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',CMPINV)                               
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   DRHR10                                                           
         MVC   HRTCOM,TACOCID                                                   
         MVI   HRTCOMH+5,L'HRTCOM                                               
                                                                                
         USING TAPDD,R4                                                         
DRHR10   L     R4,AIO2                                                          
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   HRTCLI,TAPDCLI                                                   
         MVI   HRTCLIH+5,L'HRTCLI                                               
         MVC   TGCLI,TAPDCLI                                                    
         MVC   HRTOFC,TAPDOFF                                                   
         MVI   HRTOFCH+5,L'TAPDOFF                                              
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         MVC   BYTE,TAPDSTAT       SAVE STATUS FOR CAN$                         
         DROP  R4                                                               
                                                                                
         USING TABDD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         EDIT  TABDTYPE,HRTBTYP,ALIGN=LEFT                                      
         STC   R0,HRTBTYPH+5                                                    
         MVC   TGBTYPE,TABDTYPE                                                 
         DROP  R4                                                               
                                                                                
         USING TARAD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TARAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
                                                                                
         MVC   HRTSVC,=C'PRE'                                                   
         TM    TARASTA1,TARASPRE   DISPLAY SERVICE                              
         JO    DRHR20                                                           
         MVC   HRTSVC,=C'BAS'                                                   
DRHR20   MVI   HRTSVCH+5,L'HRTSVC                                               
                                                                                
         TM    TGUSSTAT,SESSION    SESSION PAYMENT?                             
         JZ    DRHR50                                                           
                                                                                
         TM    BYTE,TAPDSCAN       CANADIAN?                                    
         JO    DRHR30                                                           
         MVC   HRTHFB,=C'0.00 '                                                 
         LA    R0,5                                                             
         CLC   TARAHFB,=4X'FF'       DISPLAY SESSION                            
         JE    DRHR25                                                           
         EDIT  TARAHFB,HRTHFB,2,ALIGN=LEFT,ZERO=NOBLANK                         
DRHR25   STC   R0,HRTHFBH+5                                                     
         J     DRHR40                                                           
                                                                                
DRHR30   MVC   HRTHFBC,=C'0.00 '                                                
         LA    R0,5                                                             
         CLC   TARAHFBC,=4X'FF'                                                 
         JE    DRHR35                                                           
         EDIT  TARAHFBC,HRTHFBC,2,ALIGN=LEFT,ZERO=NOBLANK                       
DRHR35   STC   R0,HRTHFBCH+5                                                    
                                                                                
DRHR40   MVC   HRTHFP,=C'0.00 '                                                 
         LA    R0,5                                                             
         CLC   TARAHFP,=4X'FF'                                                  
         JE    DRHR45                                                           
         TM    TARASTA1,TARASPRE   IF AGENCY IS PREMIUM SERVICE,                
         JZ    DRHR43              DISPLAY ZEROS                                
         EDIT  TARAHFP,HRTHFP,2,ALIGN=LEFT,ZERO=NOBLANK                         
         J     DRHR45                                                           
DRHR43   EDIT  TARAHFP,HRTHFP,2,ALIGN=LEFT,ZERO=BLANK                           
DRHR45   STC   R0,HRTHFPH+5                                                     
         J     DRHR78                                                           
*                                  REUSE                                        
DRHR50   TM    BYTE,TAPDSCAN       CANADIAN?                                    
         JO    DRHR60                                                           
         L     R3,TARAHFB                                                       
         BRAS  RE,CKBLTYP          CHECK BILL TYPE                              
         BNE   *+8                                                              
         L     R3,TARARFB                                                       
         MVC   HRTRFB,=C'0.00 '                                                 
         LA    R0,5                                                             
         C     R3,=4X'FF'         DISPLAY REUSE                                 
         JE    DRHR55                                                           
         EDIT  (R3),HRTRFB,2,ALIGN=LEFT,ZERO=NOBLANK                            
DRHR55   STC   R0,HRTRFBH+5                                                     
         J     DRHR70                                                           
                                                                                
DRHR60   L     R3,TARAHFBC                                                      
         BRAS  RE,CKBLTYP          CHECK BILL TYPE                              
         BNE   *+8                                                              
         L     R3,TARARFBC                                                      
         MVC   HRTRFBC,=C'0.00 '                                                
         LA    R0,5                                                             
         C     R3,=4X'FF'                                                       
         JE    DRHR65                                                           
         EDIT  (R3),HRTRFBC,2,ALIGN=LEFT,ZERO=NOBLANK                           
DRHR65   STC   R0,HRTRFBCH+5                                                    
                                                                                
DRHR70   L     R3,TARAHFP                                                       
         BRAS  RE,CKBLTYP          CHECK BILL TYPE                              
         BNE   *+8                                                              
         L     R3,TARARFP                                                       
         MVC   HRTRFP,=C'0.00 '                                                 
         LA    R0,5                                                             
         C     R3,=4X'FF'                                                       
         JE    DRHR75                                                           
         TM    TARASTA1,TARASPRE   IS AGENCY PREMIUM SERVICE?                   
         JZ    DRHR73                                                           
         EDIT  (R3),HRTRFP,2,ALIGN=LEFT,ZERO=NOBLANK                            
         J     DRHR75                                                           
DRHR73   EDIT  (R3),HRTRFP,2,ALIGN=LEFT,ZERO=BLANK                              
DRHR75   STC   R0,HRTRFPH+5                                                     
                                                                                
DRHR78   MVI   HRTAGR,C'N'                                                      
         CLI   TARAGRUL,0                                                       
         JE    DRHR80                                                           
         EDIT  TARAGRUL,HRTAGR,ALIGN=LEFT,ZERO=BLANK                            
         STC   R0,HRTAGRH+5                                                     
                                                                                
DRHR80   EDIT  TARAGCAP,HRTAGC,ALIGN=LEFT,ZERO=BLANK                            
         STC   R0,HRTAGCH+5                                                     
                                                                                
         MVC   HRTWB,=C'DEFAULT'                                                
         TM    TARASTA1,TARASBNL                                                
         JZ    *+14                                                             
         MVC   HRTWB,=C'NOLIM  '                                                
         J     DRHR90                                                           
         TM    TARASTA1,TARASBTI                                                
         JZ    *+14                                                             
         MVC   HRTWB,=C'TIER   '                                                
         J     DRHR90                                                           
         TM    TARASTA1,TARASBFI                                                
         JZ    *+10                                                             
         MVC   HRTWB,=C'FICA   '                                                
                                                                                
                                                                                
DRHR90   MVC   HRTTFU,=C'0.00 '                                                 
         LA    R0,5                                                             
         CLI   TARATFU,X'FF'                                                    
         JE    DRHR91                                                           
         EDIT  TARATFU,HRTTFU,2,ALIGN=LEFT,ZERO=NOBLANK                         
DRHR91   STC   R0,HRTTFUH+5                                                     
                                                                                
         MVC   HRTTSU,=C'0.00 '                                                 
         LA    R0,5                                                             
         CLI   TARATSU,X'FF'                                                    
         JE    DRHR92                                                           
         EDIT  TARATSU,HRTTSU,2,ALIGN=LEFT,ZERO=NOBLANK                         
DRHR92   STC   R0,HRTTSUH+5                                                     
                                                                                
         MVC   HRTTFI,=C'0.00 '                                                 
         LA    R0,5                                                             
         CLI   TARATFI,X'FF'                                                    
         JE    DRHR93                                                           
         EDIT  TARATFI,HRTTFI,2,ALIGN=LEFT,ZERO=NOBLANK                         
DRHR93   STC   R0,HRTTFIH+5                                                     
                                                                                
         MVC   HRTTM,=C'0.00 '                                                  
         LA    R0,5                                                             
         CLI   TARATM,X'FF'                                                     
         JE    DRHR94                                                           
         EDIT  TARATM,HRTTM,2,ALIGN=LEFT,ZERO=NOBLANK                           
DRHR94   STC   R0,HRTTMH+5                                                      
                                                                                
         MVC   HRTTWC,=C'0.00 '                                                 
         LA    R0,5                                                             
         CLI   TARATWC,X'FF'                                                    
         JE    DRHR95                                                           
         EDIT  TARATWC,HRTTWC,2,ALIGN=LEFT,ZERO=NOBLANK                         
DRHR95   STC   R0,HRTTWCH+5                                                     
                                                                                
         TM    BYTE,TAPDSCAN       CANADIAN?                                    
         JO    DRHR100                                                          
         LA    R2,HRTHFBH          BASIC SESSION                                
         TM    TGUSSTAT,SESSION    SESSION PAYMENT?                             
         JNZ   *+8                                                              
         LA    R2,HRTRFBH          BASIC REUSE                                  
         TM    TARASTA2,TARASSHD                                                
         JZ    DRHR100                                                          
         MVC   8(L'BOVER,R2),BOVER                                              
         MVI   5(R2),L'HRTHFB                                                   
         B     DRHR110                                                          
                                                                                
DRHR100  LA    R2,HRTHFBCH         CANADIAN SESSION                             
         TM    TGUSSTAT,SESSION    SESSION PAYMENT?                             
         JNZ   *+8                                                              
         LA    R2,HRTRFBCH         CANADIAN REUSE                               
         TM    TARASTA2,TARASSCD                                                
         JZ    DRHR110                                                          
         MVC   8(L'BOVER,R2),BOVER                                              
         MVI   5(R2),L'HRTHFBC                                                  
                                                                                
DRHR110  LA    R2,HRTHFPH          PREMIUM SESSION                              
         TM    TGUSSTAT,SESSION    SESSION PAYMENT?                             
         JNZ   *+8                                                              
         LA    R2,HRTRFPH          PREMIUM REUSE                                
         TM    TARASTA2,TARASSPD                                                
         JZ    DRHR120                                                          
         MVC   8(L'BOVER,R2),BOVER                                              
         MVI   5(R2),L'HRTHFP                                                   
                                                                                
DRHR120  TM    TARASTA2,TARASFUD                                                
         JZ    DRHR130                                                          
         LA    R2,HRTTFUH          FUTA                                         
         MVC   8(L'BOVER,R2),BOVER                                              
         MVI   5(R2),L'HRTTFU                                                   
                                                                                
DRHR130  TM    TARASTA2,TARASSUD                                                
         JZ    DRHR140                                                          
         LA    R2,HRTTSUH          SUTA                                         
         MVC   8(L'BOVER,R2),BOVER                                              
         MVI   5(R2),L'HRTTSU                                                   
                                                                                
DRHR140  TM    TARASTA2,TARASFID                                                
         JZ    DRHR150                                                          
         LA    R2,HRTTFIH          FICA                                         
         MVC   8(L'BOVER,R2),BOVER                                              
         MVI   5(R2),L'HRTTFI                                                   
                                                                                
DRHR150  TM    TARASTA2,TARASMED                                                
         JZ    DRHR160             MEDICARE                                     
         LA    R2,HRTTMH                                                        
         MVC   8(L'BOVER,R2),BOVER                                              
         MVI   5(R2),L'HRTTM                                                    
                                                                                
DRHR160  TM    TARASTA2,TARASWCD                                                
         JZ    DRHR170                                                          
         LA    R2,HRTTWCH         WC                                            
         MVC   8(L'BOVER,R2),BOVER                                              
         MVI   5(R2),L'HRTTWC                                                   
                                                                                
DRHR170  CLI   TARAAACO,X'FF'                                                   
         JNE   *+14                                                             
         MVC   HRTAC,=C'0.00 '   AGENCY COMM PERCENTAGE                         
         J     DRHR180                                                          
         EDIT  TARAAACO,HRTAC,2,ALIGN=LEFT,ZERO=BLANK                           
         STC   R0,HRTACH+5                                                      
DRHR180  CLI   TARAASFE,X'FF'                                                   
         JNE   *+14                                                             
         MVC   HRTSF,=C'0.00 '   SIG FEE PERCENTAGE                             
         J     DRHR190                                                          
         EDIT  TARAASFE,HRTSF,2,ALIGN=LEFT,ZERO=BLANK                           
         STC   R0,HRTSFH+5                                                      
DRHR190  CLI   TARAASCA,X'FF'                                                   
         JNE   *+14                                                             
         MVC   HRTSC,=C'0.00 '   SIG CAP AMOUNT                                 
         J     DRHR200                                                          
         EDIT  TARAASCA,HRTSC,2,ALIGN=LEFT,ZERO=BLANK                           
         STC   R0,HRTSCH+5                                                      
                                                                                
                                                                                
DRHR200  CLC   HRTSF,SPACES        IF NOTHING IN SIG FEE % THEN DON'T           
         JE    DRHR210             DISPLAY SIG RULE                             
                                                                                
         CLI   TARASGNS,0                                                       
         JNE   *+14                                                             
         MVC   HRTSR,=C'SIG   '    WAGES + TAXES + PNH + HANDLING               
         J     DRHR205                                                          
         CLI   TARASGNS,1          WAGES ONLY                                   
         JNE   *+14                                                             
         MVC   HRTSR,=C'SIGW  '                                                 
         J     DRHR205                                                          
         CLI   TARASGNS,2          WAGES + TAXES                                
         JNE   *+14                                                             
         MVC   HRTSR,=C'SIGWT '                                                 
         J     DRHR205                                                          
         CLI   TARASGNS,3          WAGES + PNH                                  
         JNE   *+14                                                             
         MVC   HRTSR,=C'SIGWP '                                                 
         J     DRHR205                                                          
         CLI   TARASGNS,4          WAGES + HANDLING                             
         JNE   *+14                                                             
         MVC   HRTSR,=C'SIGWH '                                                 
         J     DRHR205                                                          
         CLI   TARASGNS,5          WAGES + TAXES + PNH                          
         JNE   *+14                                                             
         MVC   HRTSR,=C'SIGWTP'                                                 
         J     DRHR205                                                          
         CLI   TARASGNS,6          WAGES + PNH + HANDLING                       
         JNE   *+14                                                             
         MVC   HRTSR,=C'SIGWPH'                                                 
         J     DRHR205                                                          
         CLI   TARASGNS,7          WAGES + TAXES + HANDLING                     
         JNE   *+10                                                             
         MVC   HRTSR,=C'SIGWTH'                                                 
DRHR205  MVI   HRTSRH+5,L'HRTSR                                                 
                                                                                
DRHR210  MVC   HRTACC,TARACOMM     AGY COM CALC                                 
         MVI   HRTACCH+5,L'HRTACC                                               
         DROP  R4                                                               
                                                                                
         GOTO1 CHAROUT,DMCB,TANUELQ,HRTSIGH,TANUTSIG                            
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        ROUTINE TO CHECK FOR BILL TYPES 21, 23, OR 24                          
*        THESE BILLING TYPES HAVE SEPARATE REUSE AND SESSION RATES    *         
*        RETURN CC EQUAL IF ONE OF THESE TYPES                        *         
***********************************************************************         
                                                                                
CKBLTYP  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGBTYPE,TABRTY21    BASIC HANDLING REUSE                         
         JE    YES                                                              
         CLI   TGBTYPE,TABRTY23                                                 
         JE    YES                                                              
         CLI   TGBTYPE,TABRTY24                                                 
         JE    YES                                                              
         J     NO                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY AGENCY-LEVEL BRATE INFORMATION            *         
*        ON ENTRY ... AIO1 = A(AGENCY RECORD)                         *         
***********************************************************************         
                                                                                
DRAGYFLD NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO1                                                         
                                                                                
         USING TABRD,R4                                                         
         L     R4,AIO1             DISPLAY BILLING TYPE                         
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DRAF05   BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TABRSTAT,TABRSACP   SKIP IF ADDITIONAL RATE                      
         JO    DRAF05                                                           
         EDIT  (1,TABRTYPE),(2,BRABTYP),ALIGN=LEFT                              
         STC   R0,BRABTYPH+5                                                    
         EDIT  (1,TABRHRLS),(1,BRAAGR),ALIGN=LEFT                               
         STC   R0,BRAAGRH+5                                                     
         DROP  R4                                                               
                                                                                
         GOTOR DRSVC,DMCB,BRASVCH                                               
                                                                                
         USING TAGHD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAGHELQ      DISPLAY GRT CAP                             
         BRAS  RE,GETEL                                                         
         JNE   DRAF10                                                           
         OC    TAGHLIM,TAGHLIM                                                  
         JZ    DRAF10                                                           
         EDIT  (3,TAGHLIM),(6,BRAAGC),ALIGN=LEFT                                
         STC   R0,BRAAGCH+5                                                     
         DROP  R4                                                               
                                                                                
         USING TARAD,R4                                                         
DRAF10   L     R4,AIO1                                                          
         MVI   ELCODE,TARAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   DRAF20                                                           
         MVI   BRAAGR,C'N'                                                      
         CLI   TARAGRUL,0          DISPLAY APP GRT RULE                         
         JE    DRAF12                                                           
         EDIT  (1,TARAGRUL),(1,BRAAGR),ALIGN=LEFT,ZERO=BLANK                    
         STC   R0,BRAAGRH+5                                                     
DRAF12   OC    TARACOMM,TARACOMM                                                
         JNZ   DRAF15                                                           
         EDIT  (2,TARAAACO),(5,BRAAC),2,ALIGN=LEFT,ZERO=BLANK                   
         STC   R0,BRAACH+5         DISPLAY AGY COMM%                            
         J     DRAF30                                                           
                                                                                
DRAF15   EDIT  (2,TARAAACO),(5,BRAAC),2,ALIGN=LEFT,ZERO=NOBLANK                 
         STC   R0,BRAACH+5         DISPLAY AGY COMM%                            
         MVC   BRAACC,TARACOMM     DISPLAY AGY COM RULE                         
         MVI   BRAACCH+5,L'BRAACC                                               
         J     DRAF30                                                           
         DROP  R4                                                               
                                                                                
DRAF20   MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLCTCDQ,(X'24',0)                                    
         JNE   DRAF30                                                           
                                                                                
         USING TAEPD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAEPELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   DRAF30                                                           
                                                                                
         EDIT  (2,TAEPRATE),(5,BRAAC),2,ALIGN=LEFT,ZERO=NOBLANK                 
         STC   R0,BRAACH+5         DISPLAY AGY COMM%                            
                                                                                
         MVC   BRAACC,TAEPCOMM     DISPLAY AGY COM RULE                         
         MVI   BRAACCH+5,L'BRAACC                                               
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO1                                                         
*                                  DISPLAY SIG FEE%                             
DRAF30   GOTO1 CHAROUT,DMCB,TANUELQ,BRASFH,TANUTSGN                             
                                                                                
*                                  DISPLAY SIG CAP                             
         GOTO1 CHAROUT,DMCB,TANUELQ,BRASCH,TANUTSCP                             
                                                                                
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSGN))                                     
         JNE   DRAF40                                                           
         L     R4,TGELEM           DISPLAY SIG FEE%                             
         ZIC   RF,TANULEN                                                       
         SHI   RF,TANULNQ+1                                                     
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   BRASF(0),TANUMBER                                                
         MVI   BRASFH+5,L'BRASF                                                 
         DROP  R4                                                               
                                                                                
         USING TAAYD,R4                                                         
DRAF40   L     R4,AIO1                                                          
         MVI   ELCODE,TAAYELQ      DISPLAY OFFICE                               
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   BRAOFC,TAAYTPOF                                                  
         MVI   BRAOFCH+5,L'TAAYTPOF                                             
                                                                                
         CLI   BRASFH+5,0                                                       
         JE    DRAF50                                                           
         MVC   BRASR,=C'SIG   '    DISPLAY SIG RULE                             
         CLI   TAAYSGNS,TAAYSGN0                                                
         JE    DRAF50                                                           
         MVC   BRASR,=C'SIGW  '                                                 
         CLI   TAAYSGNS,TAAYSGN1                                                
         JE    DRAF50                                                           
         MVC   BRASR,=C'SIGWT '                                                 
         CLI   TAAYSGNS,TAAYSGN2                                                
         JE    DRAF50                                                           
         MVC   BRASR,=C'SIGWP '                                                 
         CLI   TAAYSGNS,TAAYSGN3                                                
         JE    DRAF50                                                           
         MVC   BRASR,=C'SIGWH '                                                 
         CLI   TAAYSGNS,TAAYSGN4                                                
         JE    DRAF50                                                           
         MVC   BRASR,=C'SIGWTP'                                                 
         CLI   TAAYSGNS,TAAYSGN5                                                
         JE    DRAF50                                                           
         MVC   BRASR,=C'SIGWPH'                                                 
         CLI   TAAYSGNS,TAAYSGN6                                                
         JE    DRAF50                                                           
         MVC   BRASR,=C'SIGWTH'                                                 
         CLI   TAAYSGNS,TAAYSGN7                                                
         JE    DRAF50                                                           
         DC    H'00'                                                            
         DROP  R4                                                               
*                                  DISPLAY SIGNATORY                            
DRAF50   GOTO1 CHAROUT,DMCB,TANUELQ,BRASIGH,TANUTSIG                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR DRAGYFLD ROUTINES                 *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY AGENCY-LEVEL SERVICE INFORMATION          *         
*        ON ENTRY ... P1 = A(SERVICE LEVEL FIELD)                     *         
*                     AIO1 = A(AGENCY RECORD)                         *         
***********************************************************************         
                                                                                
DRSVC    NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
                                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   8(3,R2),=C'PRE'                                                  
         TM    TAAYMISC,TAAYPREM   DISPLAY SERVICE                              
         JO    DRSVC10                                                          
         MVC   8(3,R2),=C'BAS'                                                  
DRSVC10  MVI   5(R2),L'BRASVC                                                   
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR DRSVC ROUTINES                    *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DELETE THE RECORD                                 *         
***********************************************************************         
                                                                                
DE       NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,DEBR                                                          
         JE    DE10                                                             
         BAS   RE,DEBO                                                          
         JNE   NO                                                               
                                                                                
DE10     MVI   ELCODE,TARAELQ                                                   
         GOTO1 REMELEM                                                          
                                                                                
         BRAS  RE,MYPUTREC                                                      
         MVI   IOOPT,C'Y'                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DELETE THE BRATE RECORD                           *         
***********************************************************************         
                                                                                
DEBR     NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,BR                                                        
         JNE   NO                                                               
                                                                                
         BRAS  RE,BRSETIO                                                       
                                                                                
         MVC   AIO,AIO2                                                         
         CLI   BRACLIH+5,0                                                      
         JNE   YES                                                              
         MVC   AIO,AIO1                                                         
                                                                                
         USING TLBRPD,R3                                                        
         LA    R3,KEY              CAN'T DELETE IF CLIENT LEVEL EXISTS          
         XC    KEY,KEY                                                          
         MVI   TLBRPCD,TLBRCCDQ                                                 
         MVC   TLBRCAGY,TGAGY                                                   
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(TLBRCCLI-TLBRPCD),KEYSAVE                                    
         JE    ERRDEL                                                           
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DELETE THE BOVER RECORD                           *         
***********************************************************************         
                                                                                
DEBO     NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,BV                                                        
         JNE   NO                                                               
                                                                                
         LA    R3,KEY                                                           
         GOTOR VKINV,DMCB,BOVINVH                                               
                                                                                
         L     R4,AIO2                                                          
         MVI   ELCODE,TAPDELQ      CAN'T DELETE IF PAID                         
         BRAS  RE,GETEL                                                         
         JE    ERRDEL                                                           
                                                                                
         MVC   AIO,AIO2                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR DE ROUTINES                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE RECORD                               *         
***********************************************************************         
                                                                                
VREC     NTR1  BASE=*,LABEL=*                                                   
         TM    PROSTAT,PSACLCK                                                  
         JO    ERRINVA                                                          
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'24',0)                                    
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         BAS   RE,VRBR                                                          
         BAS   RE,VRBO                                                          
         BRAS  RE,MYPUTREC                                                      
         BRAS  RE,DREC                                                          
         MVI   IOOPT,C'Y'                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE THE BRATE RECORD                         *         
***********************************************************************         
                                                                                
VRBR     NTR1                                                                   
         CLI   RECNUM,BR                                                        
         JNE   XIT                                                              
                                                                                
         BRAS  RE,BRSETIO                                                       
                                                                                
         MVC   AIO,AIO1                                                         
         CLI   BRACLIH+5,0                                                      
         JE    VRBR10                                                           
         MVC   AIO,AIO2                                                         
                                                                                
         USING TARAD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TARAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVRASTA1,TARASTA1                                                
         DROP  R4                                                               
                                                                                
         USING TARAD,R4                                                         
VRBR10   LA    R2,BRABTYPH         BILLING TYPE                                 
         CLI   5(R2),0                                                          
         JNE   VRBR20                                                           
         CLI   BRACLIH+5,0         REQUIRED AT AGENY LEVEL                      
         JE    ERRMISS                                                          
         CLI   ACTNUM,ACTCHA       IF NOT PROVIDED AT CLIENT LEVEL              
         JNE   VRBR20              AND ACTION IS CHANGE                         
         CLI   PFAID,16            AND MESSAGE IS ACKNOWLEDGED                  
         JNE   ERRAGYBT            DEFAULT TO AGENCY BILLING TYPE               
         L     R4,AIO1                                                          
         MVI   ELCODE,TARAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         EDIT  (1,TARATYPE),(2,BRABTYP),ALIGN=LEFT                              
         STC   R0,BRABTYPH+5                                                    
         OI    6(R2),X'80'                                                      
         DROP  R4                                                               
                                                                                
VRBR20   MVI   ELCODE,TARAELQ      DELETE BILLING RATE ELEMENT                  
         GOTO1 REMELEM                                                          
                                                                                
         USING TARAD,R4                                                         
         LA    R4,ELEMENT          INITIALIZE ELEMENT                           
         XC    ELEMENT,ELEMENT                                                  
         MVI   TARAEL,TARAELQ                                                   
         MVI   TARALEN,TARALNQ                                                  
                                                                                
         GOTO1 VALINUM                                                          
         MVC   TARATYPE,ACTUAL                                                  
         GOTO1 BTYPVAL,DMCB,TARATYPE                                            
         JNE   ERRINV                                                           
         TM    TGBTSTAT,BTYPSDFT                                                
         JO    ERRINV                                                           
         CLI   TARATYPE,TABRTY10   GETTING RID OF 10, 13, AND 16                
         JE    ERRINV                                                           
         CLI   TARATYPE,TABRTY13                                                
         JE    ERRINV                                                           
         CLI   TARATYPE,TABRTY16                                                
         JE    ERRINV                                                           
                                                                                
         CLI   BRACLIH+5,0                                                      
         JNE   VRBR30                                                           
         LA    R2,BRASVCH          SERVICE                                      
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         CLC   BRASVC,=C'BAS'                                                   
         JE    VRBR25                                                           
         CLC   BRASVC,=C'PRE'                                                   
         JNE   ERRINV                                                           
         OI    TARASTA1,TARASPRE                                                
VRBR25   MVC   SVRASTA1,TARASTA1                                                
                                                                                
VRBR30   BRAS  RE,VALSTAT                                                       
                                                                                
VRBR35   GOTO1 VALRATE,DMCB,BRAHFBH,TARAHFB,1    BASIC HANDLING                 
                                                                                
         CLI   TGBTYPE,TABRTY21    BASIC HANDLING REUSE                         
         JE    VRBR40                                                           
         CLI   TGBTYPE,TABRTY23                                                 
         JE    VRBR40                                                           
         CLI   TGBTYPE,TABRTY24                                                 
         JE    VRBR40                                                           
         CLI   BRARFBH+5,0                                                      
         JE    VRBR50                                                           
         LA    R2,BRARFBH                                                       
         J     ERRINV                                                           
VRBR40   GOTO1 VALRATE,DMCB,BRARFBH,TARARFB,1                                   
                                                                                
VRBR50   GOTO1 VALRATE,DMCB,BRAHFBCH,TARAHFBC,1 BASIC CAN HANDLING              
                                                                                
         CLI   TGBTYPE,TABRTY21    BASIC CAN HANDLING REUSE                     
         JE    VRBR60                                                           
         CLI   TGBTYPE,TABRTY23                                                 
         JE    VRBR60                                                           
         CLI   TGBTYPE,TABRTY24                                                 
         JE    VRBR60                                                           
         CLI   BRARFBCH+5,0                                                     
         JE    VRBR70                                                           
         LA    R2,BRARFBCH                                                      
         J     ERRINV                                                           
VRBR60   GOTO1 VALRATE,DMCB,BRARFBCH,TARARFBC,1                                 
                                                                                
VRBR70   TM    SVRASTA1,TARASPRE                                                
         JO    VBR80                                                            
         LA    R2,BRAHFPH                                                       
         CLI   5(R2),0                                                          
         JNE   ERRINV                                                           
         LA    R2,BRARFPH                                                       
         CLI   5(R2),0                                                          
         JNE   ERRINV                                                           
         J     VRBR100                                                          
VBR80    GOTO1 VALRATE,DMCB,BRAHFPH,TARAHFP,1                                   
                                                                                
         CLI   TGBTYPE,TABRTY21    REUSE VALID FOR THESE BILL TYPES             
         JE    VRBR90                                                           
         CLI   TGBTYPE,TABRTY23                                                 
         JE    VRBR90                                                           
         CLI   TGBTYPE,TABRTY24                                                 
         JE    VRBR90                                                           
         CLI   BRARFPH+5,0                                                      
         JE    VRBR100                                                          
         LA    R2,BRARFPH                                                       
         J     ERRINV                                                           
VRBR90   GOTO1 VALRATE,DMCB,BRARFPH,TARARFP,1    PREMIUM REUSE                  
                                                                                
VRBR100  CLI   BRAAGRH+5,0         APP GRT RULE                                 
         JE    VRBR120                                                          
         CLI   BRAAGR,C'N'                                                      
         JE    VRBR120                                                          
         LA    R2,BRAAGRH                                                       
         CLI   8(R2),C'3'                                                       
         JE    VRBR110                                                          
         CLI   8(R2),C'4'                                                       
         JNE   ERRINV                                                           
VRBR110  GOTO1 VALINUM                                                          
         MVC   TARAGRUL,ACTUAL                                                  
         MVC   SVBRHRLS,ACTUAL                                                  
                                                                                
VRBR120  LA    R2,BRAAGCH          GRT CAP $                                    
         CLI   5(R2),0                                                          
         JE    VRBR130                                                          
         TM    4(R2),X'08'         VALID NUMERIC                                
         JZ    ERRINV                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,7,TARAGCAP                                                    
                                                                                
VRBR130  CLI   BRAWBH+5,0          WAGE BREAK                                   
         JE    VRBR170                                                          
         CLC   =C'DEFAULT',BRAWB                                                
         JE    VRBR170                                                          
                                                                                
         CLC   =C'NOLIM',BRAWB                                                  
         JNE   VRBR150                                                          
         OI    TARASTA1,TARASBNL                                                
         J     VRBR170                                                          
                                                                                
VRBR150  CLI   TGBTYPE,TABRTY9                                                  
         JE    VRBR160                                                          
         CLI   TGBTYPE,TABRTY10                                                 
         JE    VRBR160                                                          
         CLI   TGBTYPE,TABRTY12                                                 
         JE    VRBR160                                                          
         CLI   TGBTYPE,TABRTY13                                                 
         JE    VRBR160                                                          
         CLI   TGBTYPE,TABRTY14                                                 
         JE    VRBR160                                                          
         CLI   TGBTYPE,TABRTY15                                                 
         JE    VRBR160                                                          
         CLI   TGBTYPE,TABRTY22                                                 
         JE    VRBR160                                                          
         CLI   TGBTYPE,TABRTY24                                                 
         JE    VRBR160                                                          
         LA    R2,BRAWBH                                                        
         J     ERRINV                                                           
                                                                                
         CLC   =C'TIER',BRAWBH                                                  
         JNE   VRBR160                                                          
         OI    TARASTA1,TARASBTI                                                
         J     VRBR170                                                          
                                                                                
VRBR160  OI    TARASTA1,TARASBFI                                                
         CLC   =C'FICA',BRAWB                                                   
         JE    VRBR170                                                          
         LA    R2,BRAWBH                                                        
         J     ERRINV                                                           
                                                                                
VRBR170  GOTO1 VALRATE,DMCB,BRATFUH,TARATFU,1                                   
         GOTO1 VALRATE,DMCB,BRATSUH,TARATSU,1                                   
         GOTO1 VALRATE,DMCB,BRATFIH,TARATFI,1                                   
         GOTO1 VALRATE,DMCB,BRATMH,TARATM,1                                     
         GOTO1 VALRATE,DMCB,BRATWCH,TARATWC,1                                   
                                                                                
         CLI   BRACLIH+5,0                                                      
         JNE   VRBR190                                                          
                                                                                
         GOTO1 FLDVAL,DMCB,(X'80',BRAACH),(X'80',BRAACCH)                       
         JE    VRBR180                                                          
         LA    R2,BRAACH                                                        
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         GOTO1 VALRATE,DMCB,BRAACH,FULL,1                                       
         MVC   TARAAACO,FULL+2                                                  
         LA    R2,BRAACH                                                        
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         MVC   TARACOMM,BRAACC                                                  
         CLI   TARACOMM,TARACYES                                                
         JE    VRBR180                                                          
         CLI   TARACOMM,TARACPAY                                                
         JE    VRBR180                                                          
         CLI   TARACOMM,TARACTNH                                                
         JE    VRBR180                                                          
         CLI   TARACOMM,TARACHND                                                
         JE    VRBR180                                                          
         CLI   TARACOMM,TARACNO                                                 
         JE    VRBR180                                                          
         CLI   TARACOMM,TARACCLI                                                
         JNE   ERRINV                                                           
                                                                                
VRBR180  GOTO1 FLDVAL,DMCB,(X'80',BRASFH),(X'80',BRASRH)                        
         JE    VRBR190                                                          
         LA    R2,BRASFH                                                        
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         GOTO1 VALRATE,DMCB,BRASFH,TARAASFE,1                                   
         LA    R2,BRASCH                                                        
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         GOTO1 VALRATE,DMCB,BRASCH,TARAASCA,3                                   
         LA    R2,BRASRH                                                        
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         OC    BRASR,SPACES                                                     
         CLC   =C'SIG   ',BRASR    WAGES + TAXES + PNH + HANDLING               
         JE    VRBR190                                                          
         MVI   TARASGNS,TAAYSGN1   WAGES ONLY                                   
         CLC   =C'SIGW  ',BRASR                                                 
         JE    VRBR190                                                          
         MVI   TARASGNS,TAAYSGN2   WAGES + TAXES                                
         CLC   =C'SIGWT ',BRASR                                                 
         JE    VRBR190                                                          
         MVI   TARASGNS,TAAYSGN3   WAGES + PNH                                  
         CLC   =C'SIGWP ',BRASR                                                 
         JE    VRBR190                                                          
         MVI   TARASGNS,TAAYSGN4   WAGES + HANDLING                             
         CLC   =C'SIGWH ',BRASR                                                 
         JE    VRBR190                                                          
         MVI   TARASGNS,TAAYSGN5   WAGES + TAXES + PNH                          
         CLC   =C'SIGWTP',BRASR                                                 
         JE    VRBR190                                                          
         MVI   TARASGNS,TAAYSGN6   WAGES + PNH + HANDLING                       
         CLC   =C'SIGWPH',BRASR                                                 
         JE    VRBR190                                                          
         MVI   TARASGNS,TAAYSGN7   WAGES + TAXES + HANDLING                     
         CLC   =C'SIGWTH',BRASR                                                 
         JNE   ERRINV                                                           
                                                                                
VRBR190  MVC   TGBYTE,TARATYPE                                                  
         MVC   SVRASTA1,TARASTA1                                                
         MVC   SVAYSGNS,TARASGNS                                                
         MVC   SVGHLIM,TARAGCAP                                                 
                                                                                
         GOTO1 ADDELEM                                                          
                                                                                
         GOTO1 NAMIN,DMCB,TANUELQ,(X'80',BRASFH),TANUTSGN                       
         GOTO1 (RF),(R1),TANUELQ,(X'80',BRASCH),TANUTSCP                        
         GOTO1 (RF),(R1),TACMELQ,(X'80',BRACMNTH),TACMTBRA                      
                                                                                
         USING TABRD,R4                                                         
         L     R4,AIO              UPDATE BILLING TYPE                          
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
VRBR195  BRAS  RE,NEXTEL                                                        
         JNE   VRBR200                                                          
         TM    TABRSTAT,TABRSACP   SKIP IF ADDITIONAL RATE                      
         JO    VRBR195                                                          
         MVC   TABRTYPE,TGBTYPE    BILLING TYPE                                 
         MVC   TABRHRLS,SVBRHRLS                                                
         J     VRBR210                                                          
         DROP  R4                                                               
                                                                                
         USING TABRD,R4                                                         
VRBR200  LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TABREL,TABRELQ                                                   
         MVI   TABRLEN,TABRLNQ                                                  
         MVC   TABRTYPE,TGBTYPE                                                 
         MVC   TABRHRLS,SVBRHRLS                                                
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
VRBR210  GOTO1 ACTVIN,DMCB,(X'80',0)                                            
                                                                                
         CLI   BRACLIH+5,0                                                      
         JNE   XIT                                                              
                                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TAAYSGNS,SVAYSGNS                                                
         OI    TAAYMISC,TAAYPREM                                                
         TM    SVRASTA1,TARASPRE                                                
         JO    *+8                                                              
         NI    TAAYMISC,X'FF'-TAAYPREM                                          
         DROP  R4                                                               
                                                                                
         USING TAGHD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAGHELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VRBR220                                                          
         MVC   TAGHLIM,SVGHLIM                                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         USING TAGHD,R4                                                         
VRBR220  LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAGHEL,TAGHELQ                                                   
         MVI   TAGHLEN,TAGHLNQ                                                  
         MVC   TAGHLIM,SVGHLIM                                                  
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE THE BOVER RECORD                         *         
***********************************************************************         
                                                                                
VRBO     NTR1                                                                   
         CLI   RECNUM,BV                                                        
         JNE   XIT                                                              
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'24',BOVAGYH)                              
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TARAD,R4                                                         
         L     R4,AIO1             IF AGENCY HAS BRATE                          
         MVI   ELCODE,TARAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   ERRBRADD                                                         
         MVC   SVRASTA1,TARASTA1                                                
         DROP  R4                                                               
                                                                                
         LA    R3,KEY                                                           
         GOTOR VKINV,DMCB,BOVINVH                                               
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         LA    R2,BOVINVH                                                       
         TM    TAINSTAT,TAINSBIL   CAN'T CHANGE IF INVOICE IS BILLED            
         JO    ERRBILL             OR (COD) INVOICE PRINTED                     
         TM    TAINSTAT,TAINSAPR   CAN'T CHANGE IF INVOICE IS APPROVED          
         JO    ERRINV              OR (COD) INVOICE PRINTED                     
         TM    TAINSTA2,TAINSHLP                                                
         JO    ERRINV                                                           
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   ELCODE,TARAELQ                                                   
         GOTO1 REMELEM                                                          
                                                                                
         USING TARAD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TARAEL,TARAELQ                                                   
         MVI   TARALEN,TARALNQ                                                  
         TM    SVRASTA1,TARASPRE                                                
         JZ    VRBO10                                                           
         OI    TARASTA1,TARASPRE                                                
                                                                                
VRBO10   NI    PROSTAT,X'FF'-PSBOV0A                                            
                                                                                
         GOTO1 VALRATE,DMCB,BOVHFBH,TARAHFB,3                                   
         MVC   TARARFB,TARAHFB                                                  
         CLI   BOVHFBH+5,0                                                      
         JE    *+8                                                              
         OI    TARASTA2,TARASSHD                                                
                                                                                
         GOTO1 VALRATE,DMCB,BOVHFBCH,TARAHFBC,3                                 
         MVC   TARARFBC,TARAHFBC                                                
         CLI   BOVHFBCH+5,0                                                     
         JE    *+8                                                              
         OI    TARASTA2,TARASSCD                                                
                                                                                
         TM    TARASTA1,TARASPRE                                                
         JO    VRBO20                                                           
         CLI   BOVHFPH+5,0                                                      
         JE    VRBO30                                                           
         LA    R2,BOVHFPH                                                       
         J     ERRINV                                                           
                                                                                
VRBO20   GOTO1 VALRATE,DMCB,BOVHFPH,TARAHFP,3                                   
         MVC   TARARFP,TARAHFP                                                  
         CLI   BOVHFPH+5,0                                                      
         JE    VRBO30                                                           
         OI    TARASTA2,TARASSPD                                                
                                                                                
VRBO30   GOTO1 VALRATE,DMCB,BOVTFUH,TARATFU,3                                   
         CLI   BOVTFUH+5,0                                                      
         JE    *+8                                                              
         OI    TARASTA2,TARASFUD                                                
                                                                                
         GOTO1 VALRATE,DMCB,BOVTSUH,TARATSU,3                                   
         CLI   BOVTSUH+5,0                                                      
         JE    *+8                                                              
         OI    TARASTA2,TARASSUD                                                
                                                                                
         GOTO1 VALRATE,DMCB,BOVTFIH,TARATFI,3                                   
         CLI   BOVTFIH+5,0                                                      
         JE    *+8                                                              
         OI    TARASTA2,TARASFID                                                
                                                                                
         GOTO1 VALRATE,DMCB,BOVTMH,TARATM,3                                     
         CLI   BOVTMH+5,0                                                       
         JE    *+8                                                              
         OI    TARASTA2,TARASMED                                                
                                                                                
         GOTO1 VALRATE,DMCB,BOVTWCH,TARATWC,3                                   
         CLI   BOVTWCH+5,0                                                      
         JE    *+8                                                              
         OI    TARASTA2,TARASWCD                                                
                                                                                
         TM    PROSTAT,PSBOV0A                                                  
         JZ    VRBO40                                                           
         CLI   PFAID,18                                                         
         JNE   ERRZERO                                                          
                                                                                
VRBO40   GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         GOTO1 NAMIN,DMCB,(2,TACMELQ),(X'80',BOVCMNTH),TACMTBRA                 
         GOTO1 ACTVIN,DMCB,(X'80',0)                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE RATES                                    *         
*        ON ENTRY PARAM 1 = A(FIELD)                                  *         
*                 PARAM 2 = OUTPUT                                    *         
***********************************************************************         
                                                                                
VALRATE  NTR1                                                                   
         ICM   R2,15,0(R1)         A(FIELD)                                     
         ICM   R3,15,4(R1)         A(OUTPUT)                                    
         MVC   BYTE,11(R1)         MIN/MAX RANGE TABLE ENTRY                    
                                                                                
         XC    DUB,DUB                                                          
         LA    RF,MINMAXTB         FIND MIN/MAX RANGE                           
VRAT02   CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   BYTE,0(RF)                                                       
         JE    *+12                                                             
         AHI   RF,MINMAXL                                                       
         J     VRAT02                                                           
                                                                                
         MVC   DUB(4),1(RF)        SET MIN/MAX RANGE                            
         MVC   DUB+4(4),5(RF)                                                   
         J     VRAT04                                                           
                                                                                
VRAT04   CLI   RECNUM,BV           BOVER RECORD?                                
         JE    VRAT06                                                           
         CLI   5(R2),0             ANY INPUT?                                   
         JE    ERRMISS             REQUIRED                                     
         J     VRAT08                                                           
                                                                                
VRAT06   CLI   5(R2),0             ANY INPUT?                                   
         JE    XIT                 NOT REQUIRED                                 
                                                                                
VRAT08   ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF) VALIDATE RATE/DOLLAR                     
         CLI   0(R1),0                                                          
         JNE   ERRINV                                                           
         CLC   4(4,R1),DUB         TEST WITHIN MIN/MAX RANGE                    
         JL    ERRINV                                                           
         CLC   4(4,R1),DUB+4                                                    
         JH    ERRINV                                                           
         CLI   RECNUM,BV           BOVER RECORD?                                
         JNE   VRAT10                                                           
         MVC   0(4,R3),=X'FFFFFFFF'   DEFAULT TO NO ENTRY                       
         CLC   4(4,R1),=F'0'                                                    
         JNE   VRAT10                                                           
         OI    PROSTAT,PSBOV0A     $0 OVERRIDE ON BOVER                         
         J     XIT                                                              
VRAT10   MVC   0(4,R3),4(R1)                                                    
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS FOR INIT ROUTINES                     *         
***********************************************************************         
                                                                                
MINMAXTB DC    X'01',XL4'00000000',X'0000270F'   0-99.99                        
MINMAXL  EQU   *-MINMAXTB                                                       
         DC    X'02',XL4'00000000',X'7FFFFFFF'   0-21474836.47                  
         DC    X'03',XL4'00000000',X'05F5E0FF'   0-999999.99                    
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO LIST RECORD                                       *         
***********************************************************************         
                                                                                
LREC     NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,LRBR                                                          
         BAS   RE,LRBO                                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO LIST BRATE RECORDS                                *         
***********************************************************************         
                                                                                
         USING LISTD,R2                                                         
LRBR     NTR1                                                                   
         CLI   RECNUM,BR           IF LISTING BRATE RECORDS                     
         JNE   XIT                                                              
         MVI   NLISTS,16           SET TO LIST 16 PER PAGE                      
                                                                                
         USING TLBRPD,R3                                                        
         LA    R3,KEY              R3=A(KEY)                                    
         XC    KEY,KEY                                                          
                                                                                
         OC    TIQSKEY,TIQSKEY     IF THIS IS NOT THE FIRST PAGE                
         JZ    LRBR10              RESET ALL AGENCY VALUES                      
         LA    R4,TIQSKEY+TLBRCAGY-TLBRPD                                       
         CLI   TIQSKEY,TLBRCCDQ                                                 
         JE    LRBR05                                                           
         LA    R4,TIQSKEY+TLBRNAGY-TLBRPD                                       
         OC    0(L'TLBRNAGY,R4),0(R4)                                           
         JNZ   LRBR05                                                           
         LA    R4,TIQSKEY+TLBRNCOD-TLBRPD                                       
LRBR05   GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A4',(R4))                                 
         JE    *+6                                                              
         DC    H'00'                                                            
         BRAS  RE,SVAGY                                                         
                                                                                
         MVC   KEY(L'TIQSKEY),TIQSKEY                                           
         GOTO1 HIGH                                                             
         XR    R0,R0                                                            
         CLC   TIQSKEY,FRSTKEY                                                  
         JE    *+8                                                              
         LHI   R0,1                                                             
         XC    FRSTKEY,FRSTKEY                                                  
         LTR   R0,R0                                                            
         JZ    LRBR60                                                           
                                                                                
         CLI   BRLFMT,C'C'         IF LISTING BY NAME                           
         JE    LRBR50                                                           
         OC    TLBRNAGY,TLBRNAGY   AND WE HAVE AN AGENCY KEY                    
         JNZ   LRBR130                                                          
         MVC   SVBRPKEY,KEY                                                     
         XC    KEY,KEY                                                          
         MVI   TLBRPCD,TLBRNCDQ    NOW LOOK FOR ANY CLIENT RECORDS              
         MVC   TLBRNAGY,TGAGY      BELOW IT                                     
         GOTO1 HIGH                                                             
         CLC   TLBRNAGY,SVBRPKEY+TLBRNCOD-TLBRPD                                
         JNE   LRBR140                                                          
         J     LRBR70                                                           
                                                                                
LRBR10   CLI   BRLFMT,C'C'         IF LISTING BY CODE                           
         JNE   LRBR30                                                           
         MVI   TLBRPCD,TLBRCCDQ                                                 
         MVC   TLBRCAGY,TIFAGY     SET AGENCY FILTER/START IN KEY               
         OC    TIFAGY,TIFAGY                                                    
         JNZ   LRBR20                                                           
         OC    BRLSTR,BRLSTR                                                    
         JZ    LRBR40                                                           
         MVC   TLBRCAGY,BRLSTR                                                  
         OC    TLBRCAGY,SPACES                                                  
         J     LRBR40                                                           
LRBR20   MVC   TLBRCCLI,TIFCLI     AND SET CLIENT FILTER/START                  
         OC    TIFCLI,TIFCLI       IN KEY                                       
         JNZ   LRBR40                                                           
         OC    BRLSTR,BRLSTR                                                    
         JZ    LRBR40                                                           
         MVC   TLBRCCLI,BRLSTR                                                  
         OC    TLBRCCLI,SPACES                                                  
         J     LRBR40                                                           
                                                                                
LRBR30   MVI   TLBRPCD,TLBRNCDQ    IF LISTING BY NAME                           
         MVC   TLBRNAGY,TIFAGY                                                  
         OC    BRLSTR,BRLSTR                                                    
         JZ    LRBR40                                                           
         MVC   TLBRNNAM,BRLSTR     SET NAME START IN KEY                        
         OC    TLBRNNAM,SPACES                                                  
                                                                                
LRBR40   GOTO1 HIGH                READ KEY                                     
         J     LRBR60                                                           
LRBR50   GOTO1 SEQ                                                              
LRBR60   CLC   KEY(1),KEYSAVE                                                   
         JNE   LRBR150                                                          
                                                                                
         CLI   BRLFMT,C'C'         IF LISTING BY CODE                           
         JNE   LRBR70                                                           
         OC    TIFAGY,TIFAGY       AND FILTERING ON AGENCY                      
         JZ    LRBR70                                                           
         CLC   TIFAGY,TLBRCAGY     DONE IF IT DOESN'T MATCH                     
         JNE   LRBR150                                                          
         DROP  R3                                                               
                                                                                
LRBR70   GOTO1 GETREC              GET RECORD                                   
                                                                                
         MVC   LISTD(LISTLNQ),SPACES                                            
                                                                                
***********************************************************************         
                                                                                
         L     R4,AIO                                                           
         CLI   0(R4),TLAYCDQ       IF THIS IS AGENCY RECORD                     
         JNE   LRBR80                                                           
         BRAS  RE,SVAGY            SAVE AGENCY VALUES                           
                                                                                
***********************************************************************         
                                                                                
LRBR80   CLI   TIFOFF,0            ENFORCE OFFICE FILTER                        
         JE    *+14                                                             
         CLC   TIFOFF,TGOFF                                                     
         JNE   LRBR130                                                          
                                                                                
         OC    TIFSVC,TIFSVC       ENFORCE SERVICE FILTER                       
         JZ    *+14                                                             
         CLC   TIFSVC,SERV                                                      
         JNE   LRBR130                                                          
                                                                                
         USING TARAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TARAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLI   TIFBTYP,0           ENFORCE BILLING TYPE FILTER                  
         JE    *+14                                                             
         CLC   TARATYPE,TIFBTYP    MOVE BILLING TYPE TO PRINT LINE              
         JNE   LRBR130                                                          
         EDIT  (1,TARATYPE),(2,LSTBTYP),ALIGN=LEFT                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TLCLD,R4                                                         
         L     R4,AIO              IF THIS IS CLIENT RECORD                     
         CLI   TLCLCD,TLCLCDQ                                                   
         JNE   LRBR90                                                           
         DROP  R4                                                               
                                                                                
         USING TLCLD,R4                                                         
         L     R4,AIO                                                           
         MVC   LSTCLI,TLCLCLI      MOVE CLIENT CODE TO PRINT LINE               
         DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
         MVC   WORK,SPACES                                                      
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,TANALEN                                                       
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   WORK(0),TANANAME                                                 
         MVC   LSTCLIN,WORK        MOVE CLIENT NAME TO PRINT LINE               
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
LRBR90   OC    TIFAGY,TIFAGY       ENFORCE AGENCY FILTER                        
         JZ    LRBR95                                                           
         CLC   TGAGY,TIFAGY                                                     
         JNE   LRBR130                                                          
                                                                                
LRBR95   OC    TIFCLI,TIFCLI       ENFORCE CLIENT FILTER                        
         JZ    LRBR100                                                          
         CLC   LSTCLI,TIFCLI                                                    
         JNE   LRBR130                                                          
                                                                                
***********************************************************************         
                                                                                
LRBR100  MVC   LSTAGY,TGAGY        MOVE AGENCY CODE                             
         MVC   LSTAGYN,TGNAME      AGENCY NAME                                  
         MVC   LSTOFC,TGOFF        OFFICE                                       
         MVC   LSTSVC,SERV         AND SERVICE TO PRINT LINE                    
         DROP  R2                                                               
                                                                                
         OC    FRSTKEY,FRSTKEY                                                  
         JNZ   *+10                                                             
         MVC   FRSTKEY,KEY                                                      
                                                                                
         MVC   TIQSKEY,KEY         SAVE LAST KEY                                
         CLI   MODE,PRINTREP       MOVE PRINT LINE TO REPORT                    
         JNE   LRBR110                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    COUNTER,=P'1'                                                    
         J     LRBR120                                                          
LRBR110  GOTO1 LISTMON             OR SCREEN                                    
                                                                                
***********************************************************************         
                                                                                
         USING TLBRPD,R3                                                        
LRBR120  CLI   BRLFMT,C'C'         IF LISTING BY NAME                           
         JE    LRBR50                                                           
         OC    TLBRNAGY,TLBRNAGY   AND WE HAVE AN AGENCY KEY                    
         JNZ   LRBR130                                                          
         MVC   SVBRPKEY,KEY                                                     
         XC    KEY,KEY                                                          
         MVI   TLBRPCD,TLBRNCDQ    NOW LOOK FOR ANY CLIENT RECORDS              
         MVC   TLBRNAGY,TGAGY      BELOW IT                                     
         GOTO1 HIGH                                                             
         CLC   TLBRNAGY,SVBRPKEY+TLBRNCOD-TLBRPD                                
         JNE   LRBR140                                                          
         J     LRBR70                                                           
                                                                                
***********************************************************************         
                                                                                
LRBR130  OC    TIFAGY,TIFAGY                                                    
         JNZ   LRBR135                                                          
         OC    SVBRPKEY,SVBRPKEY   IF WE'RE SUBREADING CLIENTS                  
         JZ    LRBR50                                                           
LRBR135  GOTO1 SEQ                 IF LISTING BY NAME AND HAVE CLIENT           
         CLI   KEY,TLBRNCDQ        READ THE NEXT KEY TO SEE IF IT               
         JNE   LRBR140             IS ANOTHER CLIENT FOR SAME AGENCY            
         CLC   TLBRNAGY,TGAGY                                                   
         JE    LRBR60                                                           
LRBR140  OC    TIFAGY,TIFAGY                                                    
         JNZ   LRBR150                                                          
         MVC   KEY,SVBRPKEY        IF NOT RESTORE AGENCY READ SEQUENCE          
         XC    SVBRPKEY,SVBRPKEY                                                
         GOTO1 HIGH                                                             
         J     LRBR50                                                           
                                                                                
***********************************************************************         
                                                                                
LRBR150  XC    TIQSKEY,TIQSKEY     END OF RECORDS - EXIT                        
                                                                                
         CLI   MODE,PRINTREP                                                    
         JNE   XIT                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         JE    XIT                                                              
         EDIT  COUNTER,(3,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(13,R1),=C'BRATE RECORDS'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         JZ    XIT                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO LIST BOVER RECORDS                                *         
***********************************************************************         
                                                                                
LRBO     NTR1                                                                   
         CLI   RECNUM,BV                                                        
         JNE   XIT                                                              
                                                                                
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
                                                                                
         MVI   NLISTS,17           IN ORDER TO GET CONTROL                      
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,16           BACK AFTER 1 FULL PAGE                       
                                                                                
         CLI   MODE,PRINTREP                                                    
         JNE   XIT                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         JE    XIT                                                              
         EDIT  COUNTER,(3,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(13,R1),=C'BRATE RECORDS'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         JZ    XIT                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        HOOK FOR BOVER LIST                                          *         
***********************************************************************         
                                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         JNE   XIT                                                              
                                                                                
         USING OLISTD,R2           R2=A(OUTPUT AREA)                            
         MVC   OLISTD(OLISTLNQ),SPACES                                          
                                                                                
         USING TLIND,R4                                                         
         L     R4,TIAREC                                                        
         CLI   0(R4),TLINCDQ       INVOICE RECORD?                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    TLINSTAT,TLINSDEL                                                
         JO    XIT                                                              
                                                                                
         BRAS  RE,LIMITCHK                                                      
         JNE   XIT                                                              
                                                                                
         OC    TIFAGY,TIFAGY       AGENCY START FILTER?                         
         JZ    *+14                                                             
         CLC   TLINAGY,TIFAGY                                                   
         JL    XIT                                                              
                                                                                
         MVC   OLSTAGY,TLINAGY                                                  
                                                                                
         XC    TLININV,=6X'FF'                                                  
         GOTO1 TINVCON,DMCB,TLININV,OLSTINV,DATCON                              
                                                                                
         OC    TIFINV,TIFINV       INVOICE FILTER?                              
         JZ    *+14                                                             
         CLC   TLININV,TIFINV                                                   
         JNE   XIT                                                              
                                                                                
         USING TARAD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TARAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    RF,OLSTOVER                                                      
         TM    TARASTA2,TARASSHD                                                
         JZ    LRHBO02                                                          
         MVC   0(2,RF),=C'BH'                                                   
         AHI   RF,2                                                             
                                                                                
LRHBO02  TM    TARASTA2,TARASSCD                                                
         JZ    LRHBO04                                                          
         LA    R0,OLSTOVER                                                      
         CR    RF,R0                                                            
         JE    *+12                                                             
         MVI   0(RF),C','                                                       
         AHI   RF,1                                                             
         MVC   0(3,RF),=C'BCH'                                                  
         AHI   RF,3                                                             
                                                                                
LRHBO04  TM    TARASTA2,TARASSPD                                                
         JZ    LRHBO06                                                          
         LA    R0,OLSTOVER                                                      
         CR    RF,R0                                                            
         JE    *+12                                                             
         MVI   0(RF),C','                                                       
         AHI   RF,1                                                             
         MVC   0(3,RF),=C'PRE'                                                  
         AHI   RF,3                                                             
                                                                                
LRHBO06  TM    TARASTA2,TARASFUD                                                
         JZ    LRHBO08                                                          
         LA    R0,OLSTOVER                                                      
         CR    RF,R0                                                            
         JE    *+12                                                             
         MVI   0(RF),C','                                                       
         AHI   RF,1                                                             
         MVC   0(4,RF),=C'FUTA'                                                 
         AHI   RF,4                                                             
                                                                                
LRHBO08  TM    TARASTA2,TARASSUD                                                
         JZ    LRHBO10                                                          
         LA    R0,OLSTOVER                                                      
         CR    RF,R0                                                            
         JE    *+12                                                             
         MVI   0(RF),C','                                                       
         AHI   RF,1                                                             
         MVC   0(4,RF),=C'SUTA'                                                 
         AHI   RF,4                                                             
                                                                                
LRHBO10  TM    TARASTA2,TARASFID                                                
         JZ    LRHBO12                                                          
         LA    R0,OLSTOVER                                                      
         CR    RF,R0                                                            
         JE    *+12                                                             
         MVI   0(RF),C','                                                       
         AHI   RF,1                                                             
         MVC   0(4,RF),=C'FICA'                                                 
         AHI   RF,4                                                             
                                                                                
LRHBO12  TM    TARASTA2,TARASMED                                                
         JZ    LRHBO14                                                          
         LA    R0,OLSTOVER                                                      
         CR    RF,R0                                                            
         JE    *+12                                                             
         MVI   0(RF),C','                                                       
         AHI   RF,1                                                             
         MVC   0(3,RF),=C'MED'                                                  
         AHI   RF,3                                                             
                                                                                
LRHBO14  TM    TARASTA2,TARASWCD                                                
         JZ    LRHBO16                                                          
         LA    R0,OLSTOVER                                                      
         CR    RF,R0                                                            
         JE    *+12                                                             
         MVI   0(RF),C','                                                       
         AHI   RF,1                                                             
         MVC   0(2,RF),=C'WC'                                                   
                                                                                
LRHBO16  CLI   MODE,PRINTREP                                                    
         JNE   LRHBO18                                                          
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         J     XIT                                                              
                                                                                
LRHBO18  CLI   LISTNUM,16          END OF 1 PAGE                                
         JNE   LRHBO20                                                          
         MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,BOLSELH                                                       
         GOTO1 EXIT,DMCB,0         AND DON'T COME BACK NO MORE, NO MORE         
                                                                                
LRHBO20  MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         J     XIT                                                              
*                                                                               
* FILTER AGENCY RECORD                                                          
*                                                                               
*     ON EXIT  FULL(3) = SERVICE                                                
*              FULL+3(1) = OFFICE                                               
*              WORK = AGENCY NAME                                               
*              BYTE = BILLING TYPE                                              
*                                                                               
FILTAGY  NTR1                                                                   
         USING TLAYD,R4                                                         
         L     R4,AIO                                                           
                                                                                
         BRAS  RE,LIMITCHK                                                      
         JNE   NO                                                               
                                                                                
         OC    TIFAGY,TIFAGY       AGENCY START FILTER?                         
         JZ    *+14                                                             
         CLC   TLAYAGY,TIFAGY                                                   
         JNE   NO                                                               
                                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   YES                                                              
                                                                                
         MVC   SERV,=C'BAS'                                                     
         TM    TAAYMISC,TAAYPREM   GET SERVICE                                  
         JZ    *+10                                                             
         MVC   SERV,=C'PRE'                                                     
         OC    TIFSVC,TIFSVC       SERVICE FILTER?                              
         JZ    *+14                                                             
         CLC   TIFSVC,SERV                                                      
         JNE   NO                                                               
                                                                                
         OC    TIFOFF,TIFOFF       OFFICE FILTER?                               
         JZ    *+14                                                             
         CLC   TIFOFF,TAAYTPOF                                                  
         JNE   NO                                                               
         MVC   OFFC,TAAYTPOF                                                    
                                                                                
         USING TARAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TARAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   BTYPE,TARATYPE                                                   
                                                                                
         OC    TIFBTYP,TIFBTYP     BILLING TYPE FILTER?                         
         JZ    *+14                                                             
         CLC   TARATYPE,TIFBTYP                                                 
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
* FILTER CLIENT RECORD                                                          
*                                                                               
*     ON EXIT  FULL(3) = SERVICE                                                
*              FULL+3(1) = OFFICE                                               
*              WORK = AGENCY NAME                                               
*              BYTE = BILLING TYPE                                              
*                                                                               
FILTCLI  NTR1                                                                   
         USING TLCLD,R4                                                         
         L     R4,AIO                                                           
                                                                                
         BRAS  RE,LIMITCHK                                                      
         JNE   NO                                                               
                                                                                
         OC    TLCLAGY,TLCLAGY                                                  
         JZ    NO                                                               
                                                                                
         OC    TIFAGY,TIFAGY       AGENCY FILTER?                               
         JZ    *+14                                                             
         CLC   TLCLAGY,TIFAGY                                                   
         JNE   NO                                                               
                                                                                
         OC    TIFCLI,TIFCLI       CLIENT FILTER?                               
         JZ    *+14                                                             
         CLC   TLCLCLI,TIFCLI                                                   
         JNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',TLCLAGY)  GET AGENCY RECORD           
                                                                                
         MVC   WORK,SPACES                                                      
         USING TANAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,TANALEN                                                       
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   WORK(0),TANANAME    GET NAME                                     
                                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FILTCL02                                                         
                                                                                
         MVC   FULL(3),=C'BAS'                                                  
         TM    TAAYMISC,TAAYPREM   GET SERVICE                                  
         JZ    *+10                                                             
         MVC   FULL(3),=C'PRE'                                                  
         MVC   FULL+3(1),TAAYTPOF                                               
                                                                                
FILTCL02 MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         L     RF,AIO                                                           
         MVC   KEY(L'TLCLKEY),0(RF)    RESTORE CLIENT RECORD                    
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
                                                                                
         OC    TIFSVC,TIFSVC       SERVICE FILTER?                              
         JZ    *+14                                                             
         CLC   TIFSVC,FULL                                                      
         JNE   NO                                                               
                                                                                
         OC    TIFOFF,TIFOFF       OFFICE FILTER?                               
         JZ    *+14                                                             
         CLC   TIFOFF,FULL+3                                                    
         JNE   NO                                                               
                                                                                
         USING TARAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TARAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,TARATYPE                                                    
                                                                                
         OC    TIFBTYP,TIFBTYP     BILLING TYPE FILTER?                         
         JZ    *+14                                                             
         CLC   TARATYPE,TIFBTYP                                                 
         JNE   NO                                                               
         J     YES                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
                                                                                
         SSPEC H1,33,C'BRATE'                                                   
         SSPEC H2,33,C'-----'                                                   
                                                                                
         SSPEC H4,2,C'AGENCY  CLIENT NAME      MUSIC'                           
         SSPEC H5,2,C'------  -----------      -----'                           
         SSPEC H4,37,C'COMPOSITION NAME'                                        
         SSPEC H5,37,C'----------------'                                        
                                                                                
         DC    X'00'                                                            
         EJECT                                                                  
MYSPEC2  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
                                                                                
         SSPEC H1,33,C'BRATE'                                                   
         SSPEC H2,33,C'-----'                                                   
                                                                                
         SSPEC H4,2,C'AGENCY  NAME                  '                           
         SSPEC H5,2,C'------  ----                  '                           
         SSPEC H4,32,C'CLIENT  CLIENT NAME           '                          
         SSPEC H5,32,C'------  -----------           '                          
         SSPEC H4,61,C'BTYP OFF SRV'                                            
         SSPEC H5,61,C'---- --- ---'                                            
                                                                                
         DC    X'00'                                                            
MYSPEC3  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
                                                                                
         SSPEC H1,33,C'BOVER'                                                   
         SSPEC H2,33,C'-----'                                                   
                                                                                
         SSPEC H4,2,C'AGENCY  CLIENT  INVOICE  '                                
         SSPEC H5,2,C'------  ------  -------  '                                
         SSPEC H4,27,C'OVERRIDES'                                               
         SSPEC H5,27,C'---------'                                               
                                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
* VALIDATE STATUS FIELD                                                         
*                                                                               
         USING TARAD,R4                                                         
VALSTAT  NTR1  BASE=*,LABEL=*                                                   
         MVI   SVBTSTA,0           BILL TYPE STATUS                             
*                                                                               
         LA    R2,BRASTATH         R2=A(STATUS FIELD)                           
         CLI   5(R2),0             IF INPUT                                     
         JE    XIT                                                              
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R5,BLOCK            R5=A(SCAN BLOCK)                             
         USING SCAND,R5                                                         
         GOTO1 SCANNER,DMCB,(R2),(10,(R5))                                      
         CLI   4(R1),0                                                          
         JE    ERRINV                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
*                                                                               
VALST30  L     RF,ASTATTAB         LOOP THROUGH STATUS TABLE                    
VALST40  CLI   0(RF),X'FF'         ERROR IF END OF TABLE FOUND                  
         JE    ERRINV                                                           
         CLI   SCLEN2,0            INSURE NO INPUT ON RHS                       
         JNE   ERRINV                                                           
*                                                                               
         CLC   SCDATA1,1(RF)                                                    
         BE    VALST50                                                          
         LA    RF,L'STATTAB(RF)                                                 
         B     VALST40                                                          
*                                                                               
VALST50  CLI   SVBTSTA,C'Y'        CAN ONLY HAVE ONE BILL TYPE STATUS           
         JE    ERRINV                                                           
         OC    TARASTA1,0(RF)                                                   
         MVI   SVBTSTA,C'Y'        BILL TYPE STATUS SET                         
         LA    R5,SCANNEXT         BUMP R5 TO NEXT SCANNER ENTRY                
         BCT   R0,VALST30          AND CONTINUE                                 
*                                                                               
         TM    TARASTA1,TARASB10+TARASB13                                       
         BZ    VALST60                                                          
         CLI   TARATYPE,TABRTY12   MUST BE BILLING TYPE 12                      
         JNE   ERRINV                                                           
VALST60  TM    TARASTA1,TARASB16                                                
         BZ    VALSTX                                                           
         CLI   TARATYPE,TABRTY20   MUST BE BILLING TYPE 20                      
         JNE   ERRINV                                                           
VALSTX   J     XIT                                                              
         DROP  R4,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DISPLAY STATUS FIELD                                                          
*                                                                               
         USING TARAD,R4                                                         
DISSTAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,BRASTAT          R2=A(STATUS FIELD)                           
*                                                                               
         L     RF,ASTATTAB         RF=A(STATUS CODE TABLE)                      
DSTAT10  CLI   0(RF),X'FF'         END OF TABLE?                                
         JE    XIT                                                              
*                                                                               
         MVC   FULL(3),0(RF)       IF BIT IS ON IN STATUS BYTE                  
         NC    FULL(3),TARASTA1                                                 
         BZ    DSTAT30                                                          
*                                                                               
         CLI   0(R2),C' '          THEN IF NOT FIRST CODE                       
         BNH   *+12                                                             
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
         MVC   0(10,R2),1(RF)      DISPLAY LITERAL                              
*                                                                               
         LA    R2,9(R2)            POINT R2 TO LAST NON-SPACE                   
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
DSTAT30  LA    RF,L'STATTAB(RF)    BUMP TO NEXT STATUS CODE TABLE ENTRY         
         B     DSTAT10             LOOP BACK                                    
*                                                                               
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS LIMIT ACCESS RESTRICTIONS FOR AGENCY          *         
*        AND RETURNS CC EQUAL IF OK                                   *         
*        R4=A(RECORD)                                                 *         
***********************************************************************         
                                                                                
LIMITCHK NTR1  BASE=*,LABEL=*                                                   
         XC    LIMAGY,LIMAGY                                                    
         XC    LIMCLI,LIMCLI                                                    
                                                                                
         USING TLAYD,R4                                                         
         CLI   0(R4),TLAYCDQ                                                    
         JNE   *+10                                                             
         MVC   LIMAGY,TLAYAGY                                                   
                                                                                
         USING TLCLD,R4                                                         
         CLI   0(R4),TLCLCDQ                                                    
         JNE   *+16                                                             
         MVC   LIMAGY,TLCLAGY                                                   
         MVC   LIMCLI,TLCLCLI                                                   
                                                                                
         USING TLIND,R4                                                         
         CLI   0(R4),TLINCDQ                                                    
         JNE   *+10                                                             
         MVC   LIMAGY,TLINAGY                                                   
                                                                                
         LHI   R2,1                                                             
                                                                                
         USING FAWSSVRD,R1                                                      
LIMC02   LA    R1,LIMBLK                                                        
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R2,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0           IF NOT FOUND, STAFF HAS NO ACCESS            
         JNE   NO                                                               
         DROP  R1                                                               
                                                                                
         AHI   R2,1                                                             
                                                                                
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         JZ    YES                 STAFF HAS ACCESS TO ALL RECORDS              
                                                                                
LIMC04   CLI   0(R1),0             RECALL NEXT RECORD FROM WSSVR                
         JE    LIMC02                                                           
                                                                                
         CLI   0(R4),TLCLCDQ       CLIENT?                                      
         JE    LIMC06                                                           
         CLC   LIMAGY,TAVAAGY      IF AGENCY IS FOUND IN STAFF LIMITS           
         JNE   LIMC10                                                           
         J     YES                 ACCESS IS GRANTED                            
                                                                                
LIMC06   CLC   LIMAGY,TAVAAGY                                                   
         JNE   LIMC10                                                           
         CLI   TAVALEN,TAVALNQ     ANY CLIENT ACCESS?                           
         JE    YES                                                              
                                                                                
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
LIMC08   CLC   LIMCLI,0(RF)        IF CLIENT IS FOUND                           
         JE    YES                 ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         JNZ   LIMC08                                                           
                                                                                
LIMC10   ZIC   RE,TAVALEN          BUMP TO NEXT VALID AGENCY/CLIENT             
         AR    R1,RE               ELEMENT                                      
         J     LIMC04                                                           
         DROP  R1,R4                                                            
                                                                                
LIMAGY   DS    CL(L'TLAYAGY)                                                    
LIMCLI   DS    CL(L'TLCLCLI)                                                    
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PUT THE RECORD IN AIO TO FILE                     *         
***********************************************************************         
                                                                                
MYPUTREC NTR1  BASE=*,LABEL=*                                                   
         MVC   TGFULL,AIO          SAVE AIO INTO TGFULL                         
                                                                                
         L     RE,TGFULL           READ RECORD FOR UPDATE INTO AIO3             
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLDRKEY),0(RE)                                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLDRKEY),KEYSAVE                                           
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         OI    WHENOK,X'01'                                                     
         GOTO1 SAVPTRS,DMCB,APTRBLK                                             
                                                                                
         MVC   AIO,TGFULL          RESTORE AIO                                  
         GOTO1 PUTREC              PUT THE UPDATED RECORD                       
         GOTO1 ADDPTRS,DMCB,APTRBLK AND UPDATE PASSIVE POINTERS                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR MYPUTREC ROUTINES                 *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SET AIO1 AS AGENCY AND AIO2 AS CLIENT FOR         *         
*        BRATE ACTIONS                                                *         
***********************************************************************         
                                                                                
BRSETIO  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'24',BRAAGYH)                              
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   BRACLIH+5,0                                                      
         JE    XIT                                                              
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'24',BRACLIH)                              
         JE    XIT                                                              
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR BRSETIO ROUTINES                  *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SAVES OFF AGENCY VALUES                              *         
*        ON ENTRY ... AIO1 = A(AGENCY RECORD)                         *         
***********************************************************************         
                                                                                
SVAGY    NTR1  BASE=*,LABEL=*                                                   
         USING TLAYD,R4                                                         
         L     R4,AIO                                                           
         MVC   TGAGY,TLAYAGY                                                    
         DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
         MVC   TGNAME,SPACES                                                    
         L     R4,AIO                                                           
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         ZIC   R1,TANALEN                                                       
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   TGNAME(0),TANANAME  SAVE AGENCY NAME                             
         DROP  R4                                                               
                                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGOFF,TAAYTPOF      SAVE AGENCY OFFICE                           
         MVC   SERV,=C'BAS'                                                     
         TM    TAAYMISC,TAAYPREM   AND SERVICE                                  
         JZ    *+10                                                             
         MVC   SERV,=C'PRE'                                                     
         TM    TAAYSTA3,TAAYSLCK   IF AGENCY IS LOCKED                          
         JZ    SA10                                                             
         OI    PROSTAT,PSACLCK     SET STATUS                                   
         DROP  R4                                                               
                                                                                
         USING TARAD,R4                                                         
SA10     L     R4,AIO1             IF AGENCY HAS BRATE                          
         MVI   ELCODE,TARAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         OI    PROSTAT,PSAGYBR     SET STATUS                                   
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR SVAGY ROUTINES                    *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE PROVIDED INVOICE KEY FIELD           *         
*        ON ENTRY ... R2 = A(INVOICE KEY FIELD)                       *         
*                     R3 = A(KEY)                                     *         
***********************************************************************         
                                                                                
VKINV    NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(INVOICE FIELD)                          
         MVC   AIO,AIO2                                                         
                                                                                
         CLI   5(R2),0             IF INVOICE IS NOT PROVIDED                   
         JNE   VKI10               ATTEMPT TO USE GLOBAL INVOICE                
         GOTO1 RECVAL,DMCB,TLINCDQ,(4,(R2))                                     
         CLI   ERROR,MISSING                                                    
         JE    ERRMISS                                                          
         MVC   TGINV,8(R2)                                                      
         XC    TGINV,=X'FFFFFFFFFFFF'                                           
         GOTO1 TINVCON,DMCB,TGINV,DUB,DATCON                                    
         MVC   8(L'TGINV,R2),DUB                                                
         CLI   0(R1),X'FF'                                                      
         JNE   VKI10                                                            
         XC    8(L'TGINV,R2),8(R2)                                              
         J     ERRMISS                                                          
                                                                                
         USING TLIND,R3                                                         
VKI10    XC    KEY,KEY             ENSURE INVOICE EXISTS                        
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,TGAGY                                                    
         GOTO1 TINVCON,DMCB,8(R2),TLININV,DATCON                                
         CLI   DMCB,X'FF'                                                       
         JE    ERRINV                                                           
         XC    TLININV,=X'FFFFFFFFFFFF'                                         
         MVC   CMPINV,TLININV                                                   
         MVC   SVINV,8(R2)                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLINKEY),KEYSAVE                                           
         JNE   ERRNFND                                                          
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         CLI   RECNUM,BV           IF BOVER,                                    
         JNE   XIT                                                              
         L     R4,AIO                                                           
         USING TLIND,R4                                                         
         TM    TLINSTAT,TLINSDEL   IF INVOICE IS DELETED                        
         JO    ERRNFND             RETURN RECORD NOT FOUND ERROR                
         DROP  R4                                                               
                                                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VKINV ROUTINES                    *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE                                                *         
***********************************************************************         
                                                                                
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR5AD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR60D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR61D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR63D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR62D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR77D                                                       
         ORG   HI2WORK                                                          
*                                                                               
APTRBLK  DS    A                   A(POINTER BLOCK)                             
ATPSTBLK DS    A                   A(TAPOST BLOCK)                              
RELO     DS    A                   RELOCATION FACTOR                            
ASTATTAB DS    A                   A(STATTAB)                                   
                                                                                
VARS     DS    0X                                                               
PROSTAT  DS    X                   PROGRAM STATUS                               
PSAGYBR  EQU   X'80'               AGENCY HAS A BRATE                           
PSACLCK  EQU   X'40'               AGENCY OR CLIENT IS LOCKED                   
PSBOV0A  EQU   X'20'               0 OVERRIDE AMOUNT ENTERED                   
PSCLISR  EQU   X'10'               SUBREAD OF CLIENTS IN PROGRESS               
PSLRBOV  EQU   X'08'               LAST RECORD/ACTION WAS BOVER/DISPLAY         
                                                                                
SVINV    DS    CL6                                                              
CMPINV   DS    CL6                                                              
SVRASTA1 DS    XL(L'TARASTA1)                                                   
SVAYSGNS DS    XL(L'TARASGNS)                                                   
SVGHLIM  DS    XL(L'TARAGCAP)                                                   
SVBRPKEY DS    XL(L'KEY)                                                        
SVBRHRLS DS    XL(L'TABRHRLS)                                                   
*                                                                               
TIFBTYP  DS    CL1                 BILLING TYPE                                 
TIFSVC   DS    CL3                 SERVICE                                      
TIFSTRT  DS    CL8                 START                                        
*                                                                               
COUNTER  DS    PL4                 RECORD COUNTER                               
*                                                                               
AGYKEY   DS    XL(L'TLBRPKEY)                                                   
CURAGY   DS    XL(L'TLBRNAGY)                                                   
FRSTKEY  DS    XL(L'KEY)                                                        
                                                                                
SERV     DS    CL3                                                              
OFFC     DS    XL1                                                              
BTYPE    DS    XL1                                                              
SVBTSTA  DS    XL1                                                              
VARSLNQ  EQU   *-VARS                                                           
*                                                                               
LIMBLK   DS    XL100                                                            
*                                                                               
*                                                                               
* TASYSIOD                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAPOSTD                                                                       
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDDLCB                                                                        
* FAGETTXTD                                                                     
         PRINT ON                                                               
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAPOSTD                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
LISTD    DSECT                                                                  
LSTAGY   DS    CL6                 AGENCY                                       
         DS    CL2                                                              
LSTAGYN  DS    CL20                                                             
         DS    CL2                                                              
LSTCLI   DS    CL6                                                              
         DS    CL2                                                              
LSTCLIN  DS    CL20                                                             
         DS    CL2                                                              
LSTBTYP  DS    CL2                                                              
         DS    CL3                                                              
LSTOFC   DS    CL1                                                              
         DS    CL2                                                              
LSTSVC   DS    CL3                                                              
LISTLNQ  EQU   *-LISTD                                                          
                                                                                
OLISTD   DSECT                                                                  
OLSTAGY  DS    CL6                 AGENCY                                       
         DS    CL2                                                              
OLSTCLI  DS    CL6                                                              
         DS    CL2                                                              
OLSTINV  DS    CL6                                                              
         DS    CL4                                                              
OLSTOVER DS    CL40                                                             
OLISTLNQ EQU   *-OLISTD                                                         
         EJECT                                                                  
PTRBLK   DS    CL(7*L'TLDRREC+1)                                                
TAPSTBLK DS    XL(TPLNQ)                                                        
WORKLNQ  EQU   *-PTRBLK                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAGEN5A   06/22/15'                                      
         END                                                                    
