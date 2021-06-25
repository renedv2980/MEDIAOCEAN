*          DATA SET TAGEN5F    AT LEVEL 008 AS OF 10/31/14                      
*PHASE T7025FD,*                                                                
         TITLE 'T7025F - EVTIME MAINTENANCE/LIST'                               
T7025F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T7025F,R7,R6                                              
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=LOCAL WORKING STORAGE                     
         USING MYD,R7                                                           
                                                                                
         ST    RE,ATATDTAB                                                      
         AHI   RE,L'TATDTAB                                                     
         ST    RE,AUPDTSKS                                                      
         AHI   RE,L'UPDTSKS                                                     
         ST    RE,AUPDPTYS                                                      
*                                                                               
                                                                                
         BRAS  RE,INIT             INITIALIZE PROGRAM                           
         JE    XIT                                                              
                                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JNE   *+8                                                              
         BRAS  RE,DK                                                            
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         JNE   *+8                                                              
         BRAS  RE,LR                                                            
                                                                                
         CLI   MODE,RECDEL         DELETE RECORD                                
         JNE   *+8                                                              
         BRAS  RE,DE                                                            
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         JNE   XIT                                                              
         BRAS  RE,VR                                                            
         NI    PROSTAT,X'FF'-PSCNFHND                                           
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ERROR MESSAGES AND EXITS                                     *         
***********************************************************************         
                                                                                
ERIRA    LA    R2,CONACTH                                                       
         MVI   ERROR,INVRCACT      INVALID RECORD/ACTION                        
         J     END                                                              
                                                                                
ERINV    MVI   ERROR,INVALID       INVALID INPUT                                
         J     END                                                              
                                                                                
ERNDE    MVI   ERROR,ERINVDEL      NOT AVAILABLE FOR DELETION                   
         J     END                                                              
                                                                                
ERMIS    MVI   ERROR,MISSING       MISSING INPUT                                
         J     END                                                              
                                                                                
ERIST    MVI   ERROR,ERRECCTY      INVALID SCREEN FOR COMML TYPE                
         J     END                                                              
                                                                                
ERNFD    MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         J     END                                                              
                                                                                
ERIPD    MVI   ERROR,ERINVPD       INVOICE IS ALREADY PAID                      
         J     END                                                              
                                                                                
EREMYNF  LA    R2,ETLGTEH                                                       
EREMYNF2 LHI   RE,ERREMYNF         GO TO EMPLOYEE NOT FOUND                     
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERACINV  LHI   RE,ERRINVAC         AGENCY COMMISSION NOT ALLOWED FOR            
         STH   RE,MYMSGNO          THIS CLIENT                                  
         J     ERREND                                                           
                                                                                
EROSMIN  LHI   RE,ERROSMIN         ONLY 1 STATE ALLOWED ON A MINOR'S            
         STH   RE,MYMSGNO          EVTIME                                       
         J     ERREND                                                           
ERAC1    LHI   RE,ERRAC1AL         ONLY ONE AGENCY COMMISSION ENTRY             
         STH   RE,MYMSGNO          ALLOWED                                      
         J     ERREND                                                           
                                                                                
EREVTAM  LHI   RE,ERREVTAM         AMOUNT CANNOT EXCEED 999,999.99             
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
EREVTTO  LHI   RE,ERREVTTO         TOTAL CANNOT EXCEED 999,999.99              
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
EREVTRA  LHI   RE,ERREVTRA         RATE CANNOT EXCEED 999,999.99               
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERNEG    L     R2,ALSTPOAR         TOTAL CANNOT BE NEGATIVE                     
         LHI   RE,EREVTNEG                                                      
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERSTACT  LHI   RE,ERRSTACT         MUST SET EMPLOYEE ACTOR STATUS               
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
EROSTHI  LHI   RE,ERROSTHI         ONLY 1 STATE ALLOWED FOR HAWAII              
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
EROSTWA  LHI   RE,ERROSTWA         ONLY 1 STATE ALLOWED FOR WASHINGTON          
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
CONFIRM  LA    R2,ETIFRSTH                                                      
         LHI   RE,276              CONFIRM ALL CHANGES COMPLETE                 
         STH   RE,MYMSGNO                                                       
         J     INFEND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
INFEND   MVI   MYMTYP,GTMINF       INFORMATION MESSAGE EXIT                     
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
         CLI   TWASCR,SCR6B                                                     
         JNE   INIT10                                                           
         CLI   THISLSEL,0                                                       
         JE    ERIRA                                                            
         J     INIT40                                                           
**INIT10   LA    R2,ETLSELH                                                     
**         LA    R3,ETLLSTH                                                     
INIT10   LA    R2,ETLL1H                                                        
*                                                                               
         LA    R3,ETLL8H                                                        
INIT20   CLI   5(R2),0                                                          
         JE    INIT30                                                           
         OC    8(L'ETLL1,R2),SPACES                                             
         CLC   =C'S  ',8(R2)                                                    
         JE    INIT30                                                           
         CLC   =C'C  ',8(R2)                                                    
         JE    INIT30                                                           
         CLC   =C'DE ',8(R2)                                                    
         JE    *+8                                                              
         J     ERINV                                                            
                                                                                
         USING LISTD,RE                                                         
         ZIC   RE,0(R2)          BUMP SELECT  FIELD  TO PID                     
         AR    RE,R2                                                            
**       ZIC   RE,0(R2)          BUMP PASS PID FIELD                            
**       AR    RE,R2                                                            
         OC    LAMT,LAMT                                                        
         JZ    ERNDE                                                            
         DROP  RE                                                               
                                                                                
INIT30   ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
**       ZIC   RE,0(R2)          BUMP PASS 2ND LINE                             
**       AR    R2,RE                                                            
**       ZIC   RE,0(R2)          BUMP PASS 2ND LINE                             
**       AR    R2,RE                                                            
*                                                                               
         CR    R2,R3                                                            
         JL    INIT20                                                           
*                                                                               
INIT40   GOTO1 INITIAL,DMCB,PFTAB                                               
                                                                                
         CLI   TWASCR,SCR6B                                                     
         JNE   INIT50                                                           
         MVC   CONACT,=CL8'SELECT'                                              
                                                                                
INIT50   TM    TRNSTAT,RACHANG+USERCHA                                          
         JNZ   INIT60                                                           
         MVC   SVACTION,CONACT                                                  
                                                                                
INIT60   BAS   RE,CHKADC                                                        
         BAS   RE,HNDCNF                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE DETERMINES IF ADD/CHANGE/DELETE IS ALLOWED           *         
***********************************************************************         
                                                                                
CHKADC   NTR1                                                                   
         CLI   TWASCR,SCR6C                                                     
         JNE   XIT                                                              
         TM    PROSTAT,PSINVPAD                                                 
         JZ    XIT                                                              
         GOTO1 FLDVAL,DMCB,(X'40',ETLAGYH),(X'80',ETLGTEH)                      
         JNE   XIT                                                              
**       LA    R2,ETLSELH                                                       
**       LA    R3,ETLLSTH                                                       
         LA    R2,ETLL1H                                                        
         LA    R3,ETLL8H                                                        
         LA    RE,ETLL2H                                                        
         SR    RE,R2           RE=LENGTH OF LINE                                
CADC10   TM    1(R2),X'20'                                                      
         JO    CADC20                                                           
         CLI   8(R2),C'C'                                                       
         JE    ERIPD                                                            
         CLI   8(R2),C'D'                                                       
         JE    ERIPD                                                            
*CADC20   ZIC   RE,0(R2)                                                        
CADC20   AR    R2,RE                                                            
         CR    R2,R3                                                            
         JNH   CADC10                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO HANDLE CONFIRMATION OF CHANGES                    *         
***********************************************************************         
                                                                                
HNDCNF   NTR1                                                                   
         TM    PROSTAT,PSCNFPND                                                 
         JZ    NO                                                               
                                                                                
         CLI   TWASCR,SCR6B                                                     
         JNE   HC10                                                             
         BRAS  RE,VR                                                            
         NI    PROSTAT,X'FF'-PSCNFPND                                           
         OI    PROSTAT,PSCNFHND                                                 
         J     YES                                                              
                                                                                
HC10     NI    PROSTAT,X'FF'-PSCNFPND                                           
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR INIT ROUTINES                     *         
***********************************************************************         
                                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF24X-*,24,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF24X    DC    X'FF'                                                            
                                                                                
SCR6B    EQU   X'6B'                                                            
SCR6C    EQU   X'6C'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,VKLIST                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE KEY FOR LIST SCREEN                      *         
***********************************************************************         
                                                                                
VKLIST   NTR1                                                                   
         CLI   ACTNUM,ACTLIST                                                   
         JNE   XIT                                                              
*  CLEAR ALL SELECT FIELDS                                                      
         BRAS  RE,CLRSEL                                                        
*                                                                               
         GOTO1 FLDVAL,DMCB,(X'40',ETLAGYH),(X'80',ETLTMSTH)                     
         JE    XIT                                                              
                                                                                
         LHI   RF,TIEND-TASYSIOD                                                
         XCEFL TASYSIOD,(RF)                                                    
                                                                                
         L     R4,AIO                                                           
         GOTO1 VKAGY,DMCB,ETLAGYH                                               
         MVC   TIFAGY,TGAGY                                                     
                                                                                
         GOTO1 VKINV,DMCB,ETLINVH,ETLEVTH,ETLPERH                               
         MVC   TIFINV,TGINV                                                     
         MVC   TIFCOM,TGCOM                                                     
                                                                                
         GOTO1 VKPIDS,DMCB,ETLPIDSH                                             
*                                                                               
         LA    R2,ETLTMSTH                                                      
         CLI   ETLTMSTH+5,0                                                     
         JE    VKL20                                                            
         CLI   ETLTMST,C'A'                                                     
         JE    *+12                                                             
         CLI   ETLTMST,C'T'                                                     
         JNE   ERINV                                                            
         LA    R2,ETLTMSTH                                                      
         GOTOR CHKPAID,DMCB,0                                                   
         TM    PROSTAT,PSINPAID                                                 
         JO    ERIPD                                                            
         MVC   SVTMSTF,ETLTMST                                                  
*                                                                               
VKL20    DS    0C                                                               
                                                                                
         MVC   TIUSERID,TWAORIG                                                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLCANCDQ                                                  
         TM    TGSYSTA2,TASYSEEN                                                
         JO    VKL10                                                            
         MVI   TIREAD,TLCACDQ                                                   
                                                                                
VKL10    GOTO1 FLDVAL,DMCB,(X'22',ETLAGYH),ETLTMSTH                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE AGENCY                                   *         
*        ON ENTRY ... P1=A(AGENCY FIELD)                                        
*                     R4=A(I/O AREA 1)                                *         
***********************************************************************         
                                                                                
VKAGY    NTR1                                                                   
         L     R2,0(R1)                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'22',(R2))                                 
                                                                                
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL            ENSURE AGENCY IS PRODUCTIONS                 
         JE    *+6                 PLUS AGENCY                                  
         DC    H'00'                                                            
         TM    TAAYSTA7,TAAYSPPL                                                
         JZ    ERINV                                                            
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE INVOICE                                  *         
*        ON ENTRY ... P1=A(INVOICE FIELD)                             *         
*                     P2=A(EVENT FIELD)                               *         
*                     P3=A(PAY PERIOD FIELD)                          *         
*                     R4=A(I/O AREA 1)                                *         
***********************************************************************         
                                                                                
VKINV    NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R5,8(R1)                                                         
                                                                                
         CLI   5(R2),0                                                          
         JNE   VK10                                                             
         OC    TGINV,TGINV                                                      
         JZ    ERMIS                                                            
         CLI   TGINV+5,X'FF'                                                    
         JNE   *+10                                                             
         XC    TGINV,VKHEXFFS                                                   
         GOTO1 TINVCON,DMCB,TGINV,8(R2),DATCON                                  
         MVI   5(R2),6                                                          
                                                                                
VK10     GOTO1 TINVCON,DMCB,8(R2),TGINV,DATCON                                  
         CLI   0(R1),X'FF'                                                      
         JE    ERINV                                                            
         XC    TGINV,=6X'FF'                                                    
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',0)                                    
         JNE   ERNFD                                                            
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAINTMCO,TAINTMCO                                                
         JZ    ERINV                                                            
         GOTO1 DATCON,DMCB,(X'11',TAINPTPD),(8,8(R5))                           
         MVC   TGPCYC,TAINPTPD                                                  
         NI    PROSTAT,X'FF'-PSINVPAD                                           
         OC    TAINPINF,TAINPINF                                                
         JZ    VK20                                                             
         OI    PROSTAT,PSINVPAD                                                 
                                                                                
VK20     GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',TAINTMCO)                            
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R4                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         MVC   TGCLI,TLCOCLI                                                    
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   8(L'TACOCID,R3),TACOCID                                          
         MVC   TGCID,TACOCID                                                    
         DROP  R4                                                               
                                                                                
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A4',0)                                    
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVI   CLISTAT,0                                                        
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTPAC))                                     
         JNE   XIT                                                              
         OI    CLISTAT,CSAC                                                     
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE PID SEARCH                               *         
*        ON ENTRY ... P1=A(AGENCY FIELD)                                        
***********************************************************************         
                                                                                
VKPIDS   NTR1                                                                   
         L     R2,0(R1)                                                         
         TM    4(R2),X'20'         VALIDATED?                                   
         JO    XIT                                                              
                                                                                
         XC    SVSSNFLT,SVSSNFLT                                                
         CLI   5(R2),0                                                          
         JE    VKPIDS90                                                         
         CLI   5(R2),6             PID ENTERED                                  
         JL    ERINV               LESS THAN 6 CHARS, INVALID                   
         JE    VKPIDS30                                                         
         MVC   TGSSN,8(R2)                                                      
         OC    TGSSN,SPACES                                                     
         J     VKPIDS50                                                         
VKPIDS30 MVC   TGPID,8(R2)                                                      
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         JNE   ERINV                                                            
                                                                                
VKPIDS50 GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TGSSN)                                
         JNE   EREMYNF2                                                         
         BRAS  RE,INEVENT          SEE IF IN EVENT                              
         JNE   EREMYNF2                                                         
                                                                                
         MVC   SVSSNFLT,TGSSN                                                   
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   8(L'ETLPIDS,R2),SPACES                                           
         MVC   8(6,R2),TGPID                                                    
         OI    6(R2),X'80'                                                      
VKPIDS90 OI    4(R2),X'20'         MARK VALIDATED                               
                                                                                
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LITERALS FOR VALIDATE KEY ROUTINES                           *         
***********************************************************************         
                                                                                
VKHEXFFS DC    10X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CLEAR LIST SELECTION FIELDS                                                   
***********************************************************************         
CLRSEL   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   ETLTMST,C'A'        IF TMSHEET FIELD=A THEN PROCESS              
**       JNE   YES                 MYLISTAB TABLE TO SEE WHAT LIST              
         JE    CLRSEL02            LINES NEEDS TO CLEAR OUT                     
*                                                                               
         LA    RE,MYLISTAB         IF TMSHEET FIELD != A THEN                   
         ST    RE,AMLISTAB         SIMPLY CLEAR OUT MYLISTAB SO WE              
         L     RE,AMLISTAB         DONT ADD ANY BLANK TMSHEET RECORDS           
         LHI   RF,MYLISTBX                                                      
         XCEFL 0(RE)                                                            
         J     YES                                                              
*                                                                               
                                                                                
CLRSEL02 LA    RE,ETLL1H                                                        
         LA    RF,ETLL8NH                                                       
         LA    R3,MYLISTAB                                                      
*                                                                               
CLRSEL06 CLI   8(RE),C'S'                                                       
         BE    *+8                                                              
         CLI   8(RE),C'C'                                                       
         BNE   CLRSEL10                                                         
         XC    8(L'ETLL1,RE),0(RE)    SELECTED LINE                             
         MVI   5(RE),0                                                          
         OI    6(RE),X'80'                                                      
         B     CLRSEL30                                                         
CLRSEL10 DS    0C                     NOT SELCTED LINE                          
         XC    0(L'MYLISTAB,R3),0(R3) CLEAR OUT ENTRY IN MYLISTAB               
CLRSEL30 AHI   R3,L'MYLISTAB                                                    
         CR    RE,RF                                                            
         BNL   CLRSEL60                                                         
         AHI   RE,ETLL2H-ETLL1H                                                 
         B     CLRSEL06                                                         
CLRSEL60 DS    0C                                                               
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SEE IF TGSSN IS IN EVENT                                                      
***********************************************************************         
         USING TLCAPD,R3                                                        
INEVENT  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         MVI   TLCAPCD,TLCACCDQ                                                 
         MVC   TLCACSSN,TGSSN                                                   
         MVC   TLCACCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLCACCAT-TLCAPD),KEYSAVE                                     
         JNE   NO                                                               
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* READ CAST RECORD                                                              
***********************************************************************         
         USING TLCAPD,R3                                                        
READCAST NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         MVI   TLCAPCD,TLCACCDQ                                                 
         MVC   TLCACSSN,TGSSN                                                   
         MVC   TLCACCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLCACCAT-TLCAPD),KEYSAVE                                     
         JNE   NO                                                               
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY EMPLOYEE/EVTIME KEY FIELDS                *         
*        ON ENTRY ... AIO = A(EMPLOYEE RECORD)                        *         
***********************************************************************         
                                                                                
DK       NTR1  BASE=*,LABEL=*                                                   
         NI    PROSTAT,X'FF'-PSCNFPND-PSCNFHND                                  
                                                                                
         MVC   ETIAGY,TGAGY                                                     
         MVI   ETIAGYH+5,L'ETIAGY                                               
                                                                                
         MVC   FULL(L'TGINV),TGINV                                              
         XC    FULL(L'TGINV),=6X'FF'                                            
         GOTO1 TINVCON,DMCB,FULL,ETIINV,DATCON                                  
         MVI   ETIINVH+5,L'ETIINV                                               
                                                                                
         MVC   ETIEVT,TGCID                                                     
         MVI   ETIEVTH+5,L'ETIEVT                                               
                                                                                
         GOTO1 DATCON,DMCB,(X'11',TGPCYC),(8,ETIPER)                            
         MVI   ETIPERH+5,L'ETIPER                                               
*                                                                               
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         USING TACAD,R4                                                         
         BRAS  RE,GETEL            IF NO CAST DETAILS ELEMENT                   
         BNE   *+10                                                             
         MVC   SVCORP,TACACORP     CORP NUM ON CAST RECORD                      
*                                                                               
         XC    SVCORPN,SVCORPN                                                  
         MVC   AIO,AIO3                                                         
         BRAS  RE,GETCPNAM                                                      
         MVC   ETICRPC,SVCORP                                                   
         OI    ETICRPNH+6,X'80'                                                 
         OI    ETICRPCH+6,X'80'                                                 
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   SVCORP,C' '                                                      
         BNH   DK04                                                             
         MVC   ETICRPN,TGNAME                                                   
         J     DK06                                                             
DK04     TM    PROSTAT2,PSHASCRP                                                
         BNO   DK06                                                             
         XC    ETICRPN,ETICRPN                                                  
         MVC   ETICRPN(23),=C'*EMPLOYEE HAS CORP*    '                          
*                                                                               
                                                                                
         USING TLCAD,R4                                                         
DK06     L     R4,AIO                                                           
         GOTO1 SSNPACK,DMCB,TLCASSN,ETIPID                                      
         MVC   TGSSN,TLCASSN                                                    
         MVC   TGPID,ETIPID                                                     
         MVC   TGCSORT,TLCASORT                                                 
         MVC   SVCAKEY,0(R4)                                                    
         DROP  R4                                                               
*                                                                               
DK08     MVC   AIO,AIO2                                                         
         MVI   FLDWHDR,L'FLDWHDR                                                
         MVC   FLDWHDR+8(L'FLDWHDR-8),SPACES                                    
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'0C',0),FLDWHDR                            
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   ETINAM,FLDWHDR+8                                                 
         MVC   AIO,AIO1                                                         
                                                                                
         GOTO1 FLDVAL,DMCB,(2,ETIAGYH),ETINAMH                                  
                                                                                
         USING MAINTD,R2                                                        
         LA    R2,ETIFRSTH                                                      
         LA    R3,ETILSTH                                                       
DK10     GOTO1 FLDVAL,DMCB,(8,MUNITH),MAMTH                                     
         AHI   R2,MLNQ                                                          
         CR    R2,R3                                                            
         JNH   DK10                                                             
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        LITERALS FOR DISPLAY RECORD ROUTINES                         *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY EVTIME RECORD                             *         
*        ON ENTRY ... AIO = A(EMPLOYEE RECORD)                        *         
***********************************************************************         
                                                                                
DR       NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,DRINIT                                                        
                                                                                
         GOTOR RESTEVT,DMCB,0                                                   
         JNE   DR10                                                             
                                                                                
         BRAS  RE,DRFILL                                                        
                                                                                
DR10     GOTOR DISTOT,DMCB,ETITOTH                                              
                                                                                
         BRAS  RE,PROURA                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DISPLAY RECORD ROUTINES                         *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE INITIALIZES SCREEN FOR DISPLAY                       *         
***********************************************************************         
                                                                                
DRINIT   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(1,ETILCHGH),ETILCHGH                                
         GOTO1 (RF),(R1),(X'21',ETIFRSTH),ETILSTH                               
         XC    ETITOT,ETITOT                                                    
         OI    ETITOTH+6,X'80'                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DRINIT ROUTINE                                  *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE FILLS IN SCREEN FOR DISPLAY                          *         
***********************************************************************         
                                                                                
DRFILL   NTR1  BASE=*,LABEL=*                                                   
         USING MAINTD,R2                                                        
         LA    R2,ETIFRSTH                                                      
         LA    R3,ETILSTH                                                       
                                                                                
         USING TATDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DRF10    BRAS  RE,NEXTEL                                                        
         JNE   DRF40                                                            
                                                                                
         MVC   MTAX,TATDUNIT                                                    
         MVI   MTAXH+5,L'MTAX                                                   
                                                                                
         MVC   MTASK,TATDTASK                                                   
         MVI   MTASKH+5,L'MTASK                                                 
                                                                                
         MVC   MPTYP,TATDPMTY                                                   
         MVI   MPTYPH+5,L'MPTYP                                                 
                                                                                
         GOTO1 FLDVAL,DMCB,(8,MUNITH),MAMTH                                     
                                                                                
         OC    TATDUNTS,TATDUNTS                                                
         JZ    DRF20                                                            
         EDIT  TATDUNTS,MUNIT,2                                                 
         STC   R0,MUNITH+5                                                      
         NI    MUNITH+1,X'DF'                                                   
         EDIT  TATDRATE,MRATE,2                                                 
         STC   R0,MRATEH+5                                                      
         NI    MRATEH+1,X'DF'                                                   
                                                                                
DRF20    EDIT  TATDAMNT,MAMT,2                                                  
         STC   R0,MAMTH+5                                                       
         OC    TATDUNTS,TATDUNTS                                                
         JNZ   DRF30                                                            
         NI    MAMTH+1,X'DF'                                                    
                                                                                
DRF30    TM    TATDSTAT,TATDSOVR                                                
         JZ    *+8                                                              
         MVI   MOVER,C'*'                                                       
                                                                                
         AHI   R2,MLNQ                                                          
         CR    R2,R3                                                            
         JL    DRF10                                                            
         DROP  R2                                                               
                                                                                
DRF40    GOTO1 CHAROUT,DMCB,TACMELQ,ETICMTH,TACMTYPG                            
                                                                                
         GOTO1 ACTVOUT,DMCB,ETILCHGH                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DRFILL ROUTINE                                  *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE RECORD                               *         
***********************************************************************         
                                                                                
VR       NTR1  BASE=*,LABEL=*                                                   
         TM    PROSTAT,PSCNFHND                                                 
         JO    XIT                                                              
                                                                                
         GOTO1 FLDVAL,DMCB,(1,ETITOTH),ETITOTH                                  
         NI    PROSTAT,X'FF'-PSUSTAX-PSACETRD                                   
         NI    PROSTAT2,X'FF'-PSWATAX-PSHITAX                                   
         OI    PROSTAT,PSCNFPND                                                 
         XC    LASTATE,LASTATE                                                  
         XC    CANTAX,CANTAX                                                    
         XC    TOTAL,TOTAL                                                      
         MVI   LSTDTSQ,1                                                        
                                                                                
         L     R4,AIO                                                           
                                                                                
         GOTOR CHKPAID,DMCB,(X'80',0)                                           
                                                                                
         BAS   RE,SAVW4                                                         
         BAS   RE,SAVEMP                                                        
                                                                                
         MVI   MYACTNUM,ACTCHA                                                  
         GOTOR RESTEVT,DMCB,(X'80',0)                                           
         JE    VR10                                                             
         MVI   MYACTNUM,ACTADD                                                  
                                                                                
         USING TLTMD,R4                                                         
         XC    0(255,R4),0(R4)                                                  
         MVC   TLTMKEY,KEYSAVE                                                  
         MVI   TLTMLEN+1,41                                                     
         DROP  R4                                                               
                                                                                
VR10     BAS   RE,CLRURA                                                        
         BRAS  RE,PROURA                                                        
                                                                                
         MVI   ELCODE,TATDELQ                                                   
         GOTO1 REMELEM                                                          
                                                                                
         USING MAINTD,R3                                                        
         LA    R3,ETIFRSTH                                                      
         BAS   RE,VRTTP                                                         
         BAS   RE,POPURA                                                        
         BAS   RE,VRURA                                                         
         DROP  R3                                                               
                                                                                
         L     RE,TOTAL                                                         
         C     RE,=F'0'                                                         
         JL    ERNEG                                                            
                                                                                
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',ETICMTH),TACMTYPG                      
                                                                                
         GOTO1 ACTVIN,DMCB,ETILCHGH                                             
                                                                                
         BRAS  RE,DRINIT                                                        
         BRAS  RE,DRFILL                                                        
         GOTOR DISTOT,DMCB,ETITOTH                                              
         BRAS  RE,PROURA                                                        
                                                                                
         CLI   PFAID,24                                                         
         JNE   CONFIRM                                                          
                                                                                
         MVI   IOOPT,C'Y'                                                       
                                                                                
         GOTOR UPDTPS,DMCB,('TLTKSCDQ',AUPDTSKS)                                
         GOTOR UPDTPS,DMCB,('TLPMSCDQ',AUPDPTYS)                                
                                                                                
         CLI   MYACTNUM,ACTADD                                                  
         JNE   VR20                                                             
         BAS   RE,HASTATD                                                       
         JNE   XIT                                                              
         BRAS  RE,MYADDREC                                                      
         GOTO1 ADDPTRS,DMCB,(X'02',PTRBLK)                                      
         J     XIT                                                              
                                                                                
VR20     CLI   MYACTNUM,ACTCHA                                                  
         JNE   XIT                                                              
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLTMKEY),SVTMKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLTMKEY),KEYSAVE                                           
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
                                                                                
         MVC   AIO,AIO1                                                         
                                                                                
         BAS   RE,HASTATD                                                       
         JNE   VR30                                                             
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'02',PTRBLK)                                      
         J     XIT                                                              
                                                                                
         USING TLRCD,R4                                                         
VR30     L     R4,AIO                                                           
         OI    TLRCSTAT,X'80'                                                   
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
                                                                                
         USING TLDRD,R3                                                         
         LA    R3,KEY                                                           
         OI    TLDRSTAT,X'80'                                                   
         GOTO1 WRITE                                                            
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE SAVES OFF W4 FIELDS                                  *         
***********************************************************************         
                                                                                
SAVW4    NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A4',SVCAKEY+TLCASSN-TLCAD)                
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         NI    PROSTAT,X'FF'-PSMINOR                                            
                                                                                
         USING TAWXD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAWXELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
                                                                                
         OC    TAWXDOB,TAWXDOB                                                  
         JZ    SW410                                                            
         GOTO1 DATCON,DMCB,(1,TAWXDOB),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+8,18                                 
         CLC   TGTODAY0,WORK+8                                                  
         JNL   XIT                                                              
         OI    PROSTAT,PSMINOR                                                  
         J     XIT                                                              
                                                                                
SW410    OC    TAWXTSSN,TAWXTSSN                                                
         JZ    XIT                                                              
         OI    PROSTAT,PSMINOR                                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE SAVES OFF EMPLOYEE RECORD FIELDS                     *         
***********************************************************************         
                                                                                
SAVEMP   NTR1                                                                   
         USING TLCAD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   TLCAKEY,SVCAKEY                                                  
         GOTO1 HIGH                                                             
         CLC   TLCAKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         L     R2,ATATDTAB                                                      
                                                                                
         USING TATDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
SE10     BRAS  RE,NEXTEL                                                        
         JNE   SE20                                                             
         MVC   0(TATDLNQ,R2),TATDD                                              
         LA    R2,TATDLNQ(R2)                                                   
         J     SE10                                                             
         DROP  R4                                                               
                                                                                
SE20     MVI   0(R2),X'FF'                                                      
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVCASTA3,TACASTA3                                                
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE CLEARS UNIT, RATE AND AMOUNT FIELDS IF TAX UNIT,     *         
*        TASK OR PAY TYPE IS CHANGED                                  *         
***********************************************************************         
                                                                                
CLRURA   NTR1                                                                   
         USING MAINTD,R2                                                        
         LA    R2,ETIFRSTH                                                      
         LA    R3,ETILSTH                                                       
                                                                                
CURA10   TM    MTASKH+4,X'80'                                                   
         JO    CURA20                                                           
         TM    MPTYPH+4,X'80'                                                   
         JZ    CURA30                                                           
CURA20   GOTO1 FLDVAL,DMCB,(9,MUNITH),MOVERH                                    
                                                                                
CURA30   AHI   R2,MLNQ                                                          
         CR    R2,R3                                                            
         JL    CURA10                                                           
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES TAX UNIT, TASK AND PAY TYPE FIELDS         *         
*        ON ENTRY ... R3=A(FIRST LINE)                                *         
***********************************************************************         
                                                                                
         USING MAINTD,R3                                                        
VRTTP    NTR1                                                                   
         L     RE,AUPDTSKS                                                      
         MVI   0(RE),X'FF'                                                      
                                                                                
         L     RE,AUPDPTYS                                                      
         MVI   0(RE),X'FF'                                                      
                                                                                
VRTTP10  LR    R2,R3                                                            
                                                                                
         CLI   5(R2),0                                                          
         JNE   VRTTP20                                                          
         GOTO1 FLDVAL,DMCB,(X'80',MTASKH),MAMTH                                 
         JE    VRTTP100                                                         
         J     ERMIS                                                            
                                                                                
VRTTP20  CLI   5(R2),2                                                          
         JL    ERINV                                                            
         OC    8(L'MTAX,R2),SPACES                                              
         CLC   =C'OT ',8(R2)                                                    
         JE    ERINV                                                            
         ZIC   RF,5(R2)                                                         
         GOTO1 TAXVAL,DMCB,((RF),MTAX)                                          
         JNE   VRTTP30                                                          
                                                                                
         OC    CANTAX,CANTAX                                                    
         JNZ   ERINV                                                            
                                                                                
         TM    TGSYSTA2,TASYSVHW                                                
         JO    VRTTP23                                                          
         CLC   =C'WA ',8(R2)                                                    
         JNE   VRTTP21                                                          
         TM    PROSTAT2,PSWATAX                                                 
         JO    VRTTP23                                                          
         TM    PROSTAT,PSUSTAX                                                  
         JO    EROSTWA                                                          
         OI    PROSTAT2,PSWATAX                                                 
         J     VRTTP23                                                          
                                                                                
VRTTP21  CLC   =C'HI ',8(R2)                                                    
         JNE   VRTTP22                                                          
         TM    PROSTAT2,PSHITAX                                                 
         JO    VRTTP23                                                          
         TM    PROSTAT,PSUSTAX                                                  
         JO    EROSTHI                                                          
         OI    PROSTAT2,PSHITAX                                                 
         J     VRTTP23                                                          
                                                                                
VRTTP22  TM    PROSTAT2,PSWATAX                                                 
         JO    EROSTWA                                                          
         TM    PROSTAT2,PSHITAX                                                 
         JO    EROSTHI                                                          
                                                                                
VRTTP23  OI    PROSTAT,PSUSTAX                                                  
         J     VRTTP50                                                          
                                                                                
VRTTP30  TM    PROSTAT,PSUSTAX                                                  
         JO    ERINV                                                            
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',MTAX)                                         
         JNE   ERINV                                                            
         TM    SVCASTA3,TACASACT+TACASNAC                                       
         JZ    ERSTACT                                                          
         OC    CANTAX,CANTAX                                                    
         JZ    VRTTP40                                                          
         CLC   CANTAX,MTAX                                                      
         JNE   ERINV                                                            
VRTTP40  MVC   CANTAX,MTAX                                                      
                                                                                
VRTTP50  TM    PROSTAT,PSMINOR                                                  
         JZ    VRTTP70                                                          
         OC    LASTATE,LASTATE                                                  
         JNZ   VRTTP60                                                          
         GOTO1 SETSTATE,DMCB,LASTATE                                            
         J     VRTTP70                                                          
VRTTP60  CLC   LASTATE,MTAX                                                     
         JE    VRTTP70                                                          
         GOTO1 SETSTATE,DMCB,THISTATE                                           
         CLC   THISTATE,LASTATE                                                 
         JNE   EROSMIN                                                          
                                                                                
VRTTP70  TM    TGTASTAT,TASUINGF                                                
         JO    ERINV                                                            
         CLC   =C'FD',MTAX                                                      
         JE    ERINV                                                            
                                                                                
         LA    R2,MTASKH                                                        
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLTKCDQ,(R2),('TLTKSCDQ',0)                          
         GOTOR ADDTPS,DMCB,AUPDTSKS                                             
                                                                                
         LA    R2,MPTYPH                                                        
         GOTO1 ANY                                                              
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLPMCDQ,(X'20',(R2)),('TLPMSCDQ',0)                  
         GOTOR ADDTPS,DMCB,AUPDPTYS                                             
         MVC   AIO,AIO1                                                         
                                                                                
         USING TAYDD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TAYDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 FLDVAL,DMCB,(8,MUNITH),MAMTH                                     
         TM    TAYDSTAT,TAYDSACO                                                
         JZ    VRTTP80                                                          
         TM    CLISTAT,CSAC                                                     
         JZ    ERACINV                                                          
         TM    PROSTAT,PSACETRD                                                 
         JO    ERAC1                                                            
         OI    PROSTAT,PSACETRD                                                 
VRTTP80  NI    MPTYPS,0                                                         
         TM    TAYDSTAT,TAYDSACO+TAYDSDED                                       
         JZ    *+8                                                              
         OI    MPTYPS,MPTYPSN                                                   
         TM    TAYDSTAT,TAYDSACO                                                
         JZ    *+8                                                              
         OI    MPTYPS,MPTYPAC                                                   
         TM    TAYDSTAT,TAYDSUNT                                                
         JZ    VRTTP90                                                          
         NI    MUNITH+1,X'DF'                                                   
         NI    MRATEH+1,X'DF'                                                   
         J     VRTTP100                                                         
VRTTP90  NI    MAMTH+1,X'DF'                                                    
         DROP  R4                                                               
                                                                                
VRTTP100 AHI   R3,MLNQ                                                          
         LA    RF,ETILSTH                                                       
         CR    R3,RF                                                            
         JL    VRTTP10                                                          
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE SETS PROVIDED STATE VARIABLE BASED ON MTAX VALUE     *         
*        ON ENTRY ... R3=A(FIRST LINE)                                *         
*                     P1=A(STATE TO SET)                              *         
***********************************************************************         
                                                                                
         USING MAINTD,R3                                                        
SETSTATE NTR1                                                                   
         L     R2,0(R1)                                                         
         MVC   0(3,R2),MTAX                                                     
                                                                                
         CLC   =C'DET',MTAX                                                     
         JNE   *+10                                                             
         MVC   0(3,R2),=C'MI '                                                  
                                                                                
         CLC   =C'NYC',MTAX                                                     
         JNE   *+10                                                             
         MVC   0(3,R2),=C'NY '                                                  
                                                                                
         CLC   =C'CIN',MTAX                                                     
         JNE   *+10                                                             
         MVC   0(3,R2),=C'OH '                                                  
                                                                                
         CLC   =C'CLV',MTAX                                                     
         JNE   *+10                                                             
         MVC   0(3,R2),=C'OH '                                                  
                                                                                
         CLC   =C'PHL',MTAX                                                     
         JNE   *+10                                                             
         MVC   0(3,R2),=C'PA '                                                  
         DROP  R3                                                               
                                                                                
         OC    0(3,R2),SPACES                                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE POPULATE RATE AND AMOUNT FIELDS                      *         
*        ON ENTRY ... R3=A(FIRST LINE)                                *         
***********************************************************************         
                                                                                
         USING MAINTD,R3                                                        
POPURA   NTR1                                                                   
POPURA10 TM    MUNITH+1,X'20'                                                   
         JO    POPURA20                                                         
         LA    R2,MRATEH                                                        
         J     POPURA30                                                         
                                                                                
POPURA20 TM    MAMTH+1,X'20'                                                    
         JO    POPURA40                                                         
         LA    R2,MAMTH                                                         
                                                                                
POPURA30 BAS   RE,POPTATD                                                       
         BAS   RE,POPTPRO                                                       
                                                                                
POPURA40 AHI   R3,MLNQ                                                          
         LA    RF,ETILSTH                                                       
         CR    R3,RF                                                            
         JL    POPURA10                                                         
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE POPS NEGOTIATED RATE FROM EMPLOYEE RECORD INTO       *         
*        EMPTY RATE OR AMOUNT FIELD                                   *         
*        ON ENTRY ... R2 = A(RATE OR AMOUNT FIELD)                    *         
*                     R3 = A(LINE BEING VALIDATED)                    *         
***********************************************************************         
                                                                                
         USING MAINTD,R3                                                        
POPTATD  NTR1                                                                   
         XC    OVERAMT,OVERAMT                                                  
         OC    MTASK,SPACES                                                     
         OC    MPTYP,SPACES                                                     
                                                                                
         USING TATDD,R5                                                         
         L     R5,ATATDTAB                                                      
PTATD10  CLI   0(R5),X'FF'                                                      
         JE    XIT                                                              
         CLC   MTASK,TATDTASK                                                   
         JNE   PTATD20                                                          
         CLC   MPTYP,TATDPMTY                                                   
         JNE   PTATD20                                                          
         MVC   OVERAMT,TATDRATE                                                 
         CLI   5(R2),0                                                          
         JNE   XIT                                                              
         EDIT  TATDRATE,(L'MRATE,8(R2)),2,ALIGN=LEFT                            
         STC   R0,5(R2)                                                         
         OI    6(R2),X'80'                                                      
         J     XIT                                                              
PTATD20  LA    R5,TATDLNQ(R5)                                                   
         J     PTATD10                                                          
         DROP  R3,R5                                                            
                                                                                
***********************************************************************         
*        ROUTINE POPS TPROFILE VALUE INTO EMPTY RATE OR AMOUNT FIELD  *         
*        ON ENTRY ... P2 = A(RATE OR AMOUNT FIELD)                    *         
*                     R3 = A(LINE BEING VALIDATED)                    *         
***********************************************************************         
                                                                                
         USING MAINTD,R3                                                        
POPTPRO  NTR1                                                                   
         OC    OVERAMT,OVERAMT                                                  
         JNZ   XIT                                                              
                                                                                
         MVC   AIO,AIO3                                                         
         MVC   TGTASK,MTASK                                                     
         OC    TGTASK,SPACES                                                    
         MVC   TGPTYP,MPTYP                                                     
         OC    TGPTYP,SPACES                                                    
         GOTO1 RECVAL,DMCB,TLTRCDQ,(X'24',0),('TLTRSCDQ',0)                     
         JNE   PTPRO10                                                          
                                                                                
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTDRT))                                     
         JNE   PTPRO10                                                          
                                                                                
         USING TANUD,R4                                                         
         L     R4,TGELEM                                                        
         MVC   OVERAMT,TANUOVAM                                                 
         CLI   5(R2),0                                                          
         JNE   PTPRO10                                                          
         EDIT  TANUOVAM,(L'MRATE,8(R2)),2,ALIGN=LEFT                            
         STC   R0,5(R2)                                                         
         OI    6(R2),X'80'                                                      
         DROP  R3,R4                                                            
                                                                                
PTPRO10  MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES UNITS, RATE, AMOUNT FIELDS                 *         
*        ON ENTRY ... R3=A(FIRST LINE)                                *         
***********************************************************************         
                                                                                
         USING MAINTD,R3                                                        
VRURA    NTR1                                                                   
         USING TATDD,R4                                                         
         LA    R4,ELEMENT                                                       
VRURA10  XC    ELEMENT,ELEMENT                                                  
                                                                                
         TM    MUNITH+1,X'20'      IF UNITS FIELD IS NOT PROTECTED              
         JO    VRURA20                                                          
                                                                                
         LA    R2,MRATEH                                                        
         BAS   RE,POPTATD                                                       
         BAS   RE,POPTPRO                                                       
                                                                                
         LA    R2,MUNITH           VALIDATE UNITS                               
         GOTO1 ANY                                                              
         ZIC   R5,MUNITH+5                                                      
         GOTO1 CASHVAL,DMCB,(2,MUNIT),(R5)                                      
         OC    DMCB+4(4),DMCB+4                                                 
         JZ    ERINV                                                            
         CLC   DMCB+4(4),=F'99999'                                              
         JH    ERINV                                                            
         CLI   DMCB,X'FF'                                                       
         JE    ERINV                                                            
         MVC   TATDUNTS,DMCB+4                                                  
                                                                                
         LA    R2,MRATEH           AND RATE                                     
         GOTO1 ANY                                                              
         ZIC   R5,MRATEH+5                                                      
         GOTO1 CASHVAL,DMCB,(2,MRATE),(R5)                                      
         OC    DMCB+4(4),DMCB+4                                                 
         JZ    ERINV                                                            
         CLC   DMCB+4(4),=F'999999999'                                          
         JH    EREVTRA                                                          
         CLI   DMCB,X'FF'                                                       
         JE    ERINV                                                            
         MVC   TATDRATE,DMCB+4                                                  
         GOTO1 SETSOVR,DMCB,TATDRATE                                            
                                                                                
         ZAP   WORK(16),=PL1'0'                                                 
         ICM   RF,15,TATDRATE                                                   
         CVD   RF,WORK+8                                                        
         ICM   RF,15,TATDUNTS                                                   
         CVD   RF,DUB                                                           
         MP    WORK(16),DUB+4(4)                                                
         AP    WORK(16),=PL4'50'                                                
         DP    WORK(16),=PL4'100'                                               
         CP    WORK(12),=PL12'999999999'                                        
         JH    EREVTAM                                                          
         CVB   RF,WORK+4                                                        
         TM    MPTYPS,MPTYPSN                                                   
         JZ    *+6                                                              
         LNR   RF,RF                                                            
         STCM  RF,15,TATDAMNT                                                   
         ST    R2,ALSTPOAR                                                      
                                                                                
         TM    MPTYPS,MPTYPSN                                                   
         JZ    VRURA40                                                          
         ICM   RF,15,TATDRATE                                                   
         LNR   RF,RF                                                            
         STCM  RF,15,TATDRATE                                                   
         J     VRURA40                                                          
                                                                                
VRURA20  TM    MAMTH+1,X'20'       IF AMOUNT FIELD IS NOT PROTECTED             
         JO    VRURA50                                                          
                                                                                
         LA    R2,MAMTH            VALIDATE AMOUNT                              
         BAS   RE,POPTATD                                                       
         BAS   RE,POPTPRO                                                       
         GOTO1 ANY                                                              
         ZIC   R5,MAMTH+5                                                       
         GOTO1 CASHVAL,DMCB,(2,MAMT),(R5)                                       
         OC    DMCB+4(4),DMCB+4                                                 
         JZ    ERINV                                                            
         CLC   DMCB+4(4),=F'999999999'                                          
         JH    EREVTAM                                                          
         CLI   DMCB,X'FF'                                                       
         JE    ERINV                                                            
         ICM   RF,15,DMCB+4                                                     
                                                                                
         TM    MPTYPS,MPTYPSN                                                   
         JZ    VRURA30                                                          
         L     RE,OVERAMT                                                       
         LNR   RE,RE                                                            
         ST    RE,OVERAMT                                                       
         LNR   RF,RF                                                            
                                                                                
VRURA30  STCM  RF,15,TATDAMNT                                                   
         GOTO1 SETSOVR,DMCB,TATDAMNT                                            
         ST    R2,ALSTPOAR                                                      
                                                                                
VRURA40  ZICM  RE,TATDAMNT,4                                                    
         A     RE,TOTAL                                                         
         C     RE,=F'999999999'                                                 
         JH    EREVTTO                                                          
         ST    RE,TOTAL                                                         
                                                                                
         MVI   TATDEL,TATDELQ                                                   
         MVI   TATDLEN,TATDLNQ                                                  
         ZICM  RE,LSTDTSQ,1                                                     
         AHI   RE,1                                                             
         STCM  RE,1,LSTDTSQ                                                     
         TM    TGSYSTA2,TASYSROI                                                
         JZ    *+8                                                              
         STCM  RE,1,TATDTSQ                                                     
         MVC   TATDUNIT,MTAX                                                    
         OC    TATDUNIT,SPACES                                                  
         MVC   TATDTASK,MTASK                                                   
         OC    TATDTASK,SPACES                                                  
         MVC   TATDPMTY,MPTYP                                                   
         OC    TATDPMTY,SPACES                                                  
         TM    MPTYPS,MPTYPAC                                                   
         JZ    *+8                                                              
         OI    TATDSTAT,TATDSACO                                                
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         GOTOR DISTOT,DMCB,ETITOTH                                              
                                                                                
VRURA50  AHI   R3,MLNQ                                                          
         LA    RF,ETILSTH                                                       
         CR    R3,RF                                                            
         JL    VRURA10                                                          
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE SETS RATE/AMOUNT OVERRIDE STATUS                     *         
*        ON ENTRY ... P1 = A(TATDRATE OR TATDAMNT)                    *         
*                     R4 = A(TATD ELEMENT)                                      
***********************************************************************         
                                                                                
         USING TATDD,R4                                                         
SETSOVR  NTR1                                                                   
         L     R1,0(R1)                                                         
         OC    OVERAMT,OVERAMT                                                  
         JZ    XIT                                                              
         CLC   OVERAMT,0(R1)                                                    
         JE    XIT                                                              
         OI    TATDSTAT,TATDSOVR                                                
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS IF TIMESHEET HAS ANY EVENT TIME TASK-PAY TYPE *         
*        DETAILS ELEMENTS                                             *         
***********************************************************************         
                                                                                
HASTATD  NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        ROUTINE TO ADD TIMESHEET RECORD                                        
*        AIO = CAST RECORD                                                      
***********************************************************************         
ADDTMST  NTR1  BASE=*,LABEL*                                                    
         USING TLTMD,RF                                                         
*                                                                               
                                                                                
         LA    R3,MYLISTAB                                                      
*                                                                               
***      OC    MYLISTAB,MYLISTAB                                                
***      JZ    XIT                                                              
         CLI   SVTMSTF,C'A'       ADD TIMESHEET FILTER ON?                      
         JNE   XIT                                                              
ADDTM10  OC    0(L'MYLISTAB,R3),0(R3)                  `                        
         BZ    ADDTM30                                                          
         LA    RF,KEY                                `                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLTMKEY),0(R3)                                             
         DROP  RF                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLTMKEY),KEYSAVE                                           
         JNE   *+6                                                              
         DC    H'0'                                                             
***      JE    ADDTM30                                                          
                                                                                
         L     R4,AIO                                                           
                                                                                
         USING TLTMD,R4                                                         
         XC    0(255,R4),0(R4)                                                  
         MVC   TLTMKEY,KEYSAVE                                                  
         MVI   TLTMLEN+1,41                                                     
         DROP  R4                                                               
                                                                                
         MVI   ELCODE,TATDELQ                                                   
         GOTO1 REMELEM                                                          
                                                                                
         BRAS  RE,MYADDREC       ADD BLANK TIMESHEET RECORD                     
ADDTM30  AHI   R3,L'MYLISTAB                                                    
         LA    RF,MYLISTBX                                                      
         LA    RE,MYLISTAB                                                      
         AR    RE,RF                                                            
         CR    R3,RE                                                            
         BL    ADDTM10                                                          
                                                                                
         LA    RE,MYLISTAB                                                      
         ST    RE,AMLISTAB                                                      
         L     RE,AMLISTAB                                                      
         LHI   RF,MYLISTBX                                                      
         XCEFL 0(RE)                                                            
*                                                                               
ADDMST   J     XIT                                                              
***********************************************************************         
*        LITERALS FOR VALIDATE RECORD ROUTINES                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
MYADDREC NTR1  BASE=*,LABEL=*                                                   
         USING TLRCD,R4                                                         
         L     R4,AIO                                                           
                                                                                
         USING TLDRD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   TLDRKEY,TLRCKEY                                                  
         MVC   TLDRSTAT,TLRCSTAT                                                
         OI    DMINBTS,X'08'                                                    
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   TLDRKEY,KEYSAVE                                                  
         JNE   MAR10                                                            
         TM    TLDRSTAT,X'80'                                                   
         JO    *+6                                                              
         DC    H'00'                                                            
         MVC   TLDRKEY,KEYSAVE                                                  
         MVC   TLDRSTAT,KEYSAVE+TLDRSTAT-TLDRD                                  
         GOTO1 WRITE                                                            
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         ST    R4,AIO                                                           
         NI    TLRCSTAT,X'7F'                                                   
         GOTO1 PUTREC                                                           
         J     MAR20                                                            
         DROP  R3,R4                                                            
                                                                                
MAR10    MVC   KEY,KEYSAVE                                                      
         GOTO1 ADDREC                                                           
                                                                                
MAR20    NI    DMINBTS,X'F7'                                                    
         J     XIT                                                              
***********************************************************************         
*        GET W4 NAME                                                            
* INPUT- TGSSN HAS TO BE SET TO THE PERFORMER SSN                               
* OUTPUT- TGNAME = CORP NAME                                                    
***********************************************************************         
GETCPNAM NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    FLDWHDR,FLDWHDR                                                  
         MVC   TGNAME,SPACES                                                    
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A4',TGSSN)                                
         JE    *+6                                                              
         DC    H'00'                                                            
         NI    PROSTAT2,X'FF'-PSHASCRP                                          
         MVI   ELCODE,TATIELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TATITYCO))                                     
         JNE   GETCPNMX                                                         
         OI    PROSTAT2,PSHASCRP                                                
                                                                                
         L     R4,TGELEM                                                        
         USING TATID,R4                                                         
         MVI   FLDWHDR,L'FLDWHDR                                                
         MVC   FLDWHDR+8(L'FLDWHDR-8),SPACES                                    
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'AC',TATIID),FLDWHDR                       
         MVC   TGNAME,SPACES                                                    
         MVC   TGNAME(L'FLDWHDR),FLDWHDR+8                                      
                                                                                
GETCPNMX J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
         DROP  R4                                                               
***********************************************************************         
*        ROUTINE TO LIST RECORDS                                      *         
***********************************************************************         
                                                                                
LR       NTR1  BASE=*,LABEL=*                                                   
         MVC   SVATHISL,ATHISLST                                                
         GOTOR CHKPAID,DMCB,0                                                   
         BRAS  RE,ADDTMST                                                       
                                                                                
         LA    RE,MYLISTAB                                                      
         ST    RE,AMLISTAB                                                      
         L     RE,AMLISTAB                                                      
         LHI   RF,MYLISTBX                                                      
         XCEFL 0(RE)                                                            
                                                                                
* CLEAR SCREEN                                                                  
          GOTO1 FLDVAL,DMCB,(1,ETLL1H),ETLL9H                                   
*                                                                               
         LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
                                                                                
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVI   NLISTS,9                                                         
                                                                                
         OC    TIQSKEY,TIQSKEY                                                  
         JNZ   LR10                                                             
         OI    PROSTAT,PSGOTORE                                                 
         CLI   ETLGTEH+5,0                                                      
         JE    *+8                                                              
         NI    PROSTAT,X'FF'-PSGOTORE                                           
                                                                                
LR10     GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         XC    TIQSKEY,TIQSKEY                                                  
                                                                                
         CLI   ETLGTEH+5,0                                                      
         JE    XIT                                                              
         TM    PROSTAT,PSGOTORE                                                 
         JZ    EREMYNF                                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO PROCESS RECORDS FROM SYSIO                        *         
***********************************************************************         
                                                                                
LRHOOK   NTR1                                                                   
         USING LISTD,R2                                                         
         CLI   TIMODE,PROCREC                                                   
         JNE   XIT                                                              
                                                                                
         OC    SVSSNFLT,SVSSNFLT                                                
         JZ    LRH05                                                            
         CLC   SVSSNFLT,TISSN                                                   
         JNE   YES                                                              
                                                                                
LRH05    MVC   LISTD(LISTLNQ),SPACES                                            
                                                                                
LRH06    XC    SVCORP,SVCORP                                                    
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ                                                   
         USING TACAD,R4                                                         
         BRAS  RE,GETEL            IF NO CAST DETAILS ELEMENT                   
         BNE   *+16                                                             
         MVC   LCORP,TACACORP      DISPLAY CORPORATION NUMBER                   
         MVC   SVCORP,TACACORP     CORP NUM ON CAST RECORD                      
                                                                                
LRH08    GOTO1 SSNPACK,DMCB,TISSN,LPID                                          
         MVC   SVLPID,LPID                                                      
                                                                                
         MVI   FLDWHDR,L'FLDWHDR                                                
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'AC',TISSN),FLDWHDR                        
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
LRH12    TM    PROSTAT,PSGOTORE                                                 
         JO    LRH14                                                            
         ZIC   RE,ETLGTEH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   ETLGTE(0),FLDWHDR+8                                              
         JE    LRH14                                                            
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
         J     NO                                                               
                                                                                
LRH14    OI    PROSTAT,PSGOTORE                                                 
         MVC   SVPFNAM,FLDWHDR+8                                                
         USING TLTMD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLTMCD,TLTMCDQ                                                   
         MVI   TLTMSTA,TLTMSPPL                                                 
         MVC   TLTMCOM,TGCOM                                                    
         MVC   TLTMINV,TGINV                                                    
         MVC   TLTMSSN,TGSSN                                                    
         MVC   TLTMSORT+4(L'TLCASEQ),TICASEQ                                    
         GOTO1 HIGH                                                             
         CLC   TLTMKEY,KEYSAVE                                                  
         JE    LRH20                                                            
         DROP  R3                                                               
* DONT HAVE TIMESHEET                                                           
         CLI   ETLTMST,C'T'       HAVE TIMESHEET FILTER                         
         JE    LRH16                                                            
                                                                                
***      TM    PROSTAT,PSINPAID                                                 
***      JZ    LRH30                                                            
         TM    PROSTAT,PSINPAID                                                 
         JNZ   LRH16                                                            
         BRAS  RE,FILLTMTB                                                      
*                                                                               
         J     LRH30                                                            
*                                                                               
LRH16    MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
         J     NO                                                               
                                                                                
LRH20    DS    0C                 HAVE TIMESHEET                                
         CLI   ETLTMST,C'A'       ADDING TIMESHEET FILTER?                      
         JE    LRH16                                                            
         GOTO1 GETREC                                                           
         MVC   FLDWHDR+8(L'FLDWHDR-8),SPACES                                    
         GOTOR DISTOT,DMCB,FLDWHDR                                              
         MVC   LAMT,FLDWHDR+8                                                   
                                                                                
LRH30    XC    SVCORPN,SVCORPN                                                  
         BRAS  RE,GETCPNAM                                                      
         MVC   SVCORPN,TGNAME                                                   
                                                                                
         MVC   TIQSKEY,TIKEY                                                    
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
                                                                                
         MVC   DMDSKADD,TIDSKADD                                                
         GOTO1 LISTMON                                                          
* DISPLAY SECOND LIST LINE PER PERFORMER MANUALLY                               
         L     RE,ATHISLST                                                      
*                                                                               
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         MVC   8(L'LPNAME,RE),SVPFNAM                                           
         OI    6(RE),X'80'                                                      
         ST    RE,ATHISLST                                                      
                                                                                
         L     R2,ATHISLST                                                      
         USING LIST2D,R2                                                        
         CLI   SVCORP,C' '                                                      
         BNH   LRH40                                                            
         MVC   LCORPN(L'SVCORPN),SVCORPN                                        
         J     LRH50                                                            
LRH40    TM    PROSTAT2,PSHASCRP                                                
         BNO   LRH50                                                            
         XC    LCORPN,LCORPN                                                    
         MVC   LCORPN(23),=C'*EMPLOYEE HAS CORP*    '                           
                                                                                
* BUMP  TO THE NEXT LINE MANUALLY                                               
LRH50    L     RE,ATHISLST                                                      
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         ST    RE,ATHISLST                                                      
         L     R2,ATHISLST                                                      
                                                                                
         J     YES                                                              
         DROP  R2,R4,RB                                                         
                                                                                
***********************************************************************         
*        LITERALS FOR LIST RECORD ROUTINES                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
FILLTMTB NTR1  BASE=*,LABEL=*                                                   
         CLI   ETLTMST,C'A'                                                     
         JNE   XIT                                                              
         ZICM  RE,AMLISTAB,(15)                                                 
         USING TLTMD,RE                                                         
         MVI   TLTMCD,TLTMCDQ                                                   
         MVI   TLTMSTA,TLTMSPPL                                                 
         MVC   TLTMCOM,TGCOM                                                    
         MVC   TLTMINV,TGINV                                                    
         MVC   TLTMSSN,TGSSN                                                    
         MVC   TLTMSORT+4(L'TLCASEQ),TICASEQ                                    
         AHI   RE,L'MYLISTAB                                                    
         STCM  RE,15,AMLISTAB                                                   
         J     XIT                                                              
         DROP  RE                                                               
***********************************************************************         
*        ROUTINE TO DELETE RECORD                                     *         
***********************************************************************         
                                                                                
DE       NTR1  BASE=*,LABEL=*                                                   
         GOTOR CHKPAID,DMCB,(X'80',0)                                           
                                                                                
         GOTOR RESTEVT,DMCB,(X'80',0)                                           
         J     XIT                                                              
         DROP  RB                                                               
                                                                                
***********************************************************************         
*        LITERALS FOR DELETE RECORD ROUTINES                          *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RESTORES EVTIME RECORD IN AIO                        *         
*        ON ENTRY ... P1 BYTE 0 = X'80' READ FOR UPDATE               *         
***********************************************************************         
                                                                                
RESTEVT  NTR1  BASE=*,LABEL=*                                                   
         MVC   TGBYTE3,0(R1)                                                    
                                                                                
         USING TLTMD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLTMCD,TLTMCDQ                                                   
         MVI   TLTMSTA,TLTMSPPL                                                 
         MVC   TLTMCOM,TGCOM                                                    
         MVC   TLTMINV,TGINV                                                    
         MVC   TLTMSSN,TGSSN                                                    
         MVC   TLTMSORT+4(L'TLCASEQ),TGCSORT+4                                  
         GOTO1 HIGH                                                             
         CLC   TLTMKEY,KEYSAVE                                                  
         JNE   NO                                                               
         MVC   SVTMKEY,KEY                                                      
         DROP  R3                                                               
                                                                                
         TM    TGBYTE3,X'80'                                                    
         JZ    RE10                                                             
         MVI   RDUPDATE,C'Y'                                                    
RE10     GOTO1 GETREC                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR RESTEVT ROUTINES                                *         
***********************************************************************         
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PROTECTS UNIT, RATE AND AMOUNT FIELDS IF TAX UNIT,   *         
*        TASK OR PAY TYPE IS EMPTY                                    *         
***********************************************************************         
                                                                                
PROURA   NTR1  BASE=*,LABEL=*                                                   
         USING MAINTD,R2                                                        
         LA    R2,ETIFRSTH                                                      
         LA    R3,ETILSTH                                                       
                                                                                
PURA10   GOTO1 FLDVAL,DMCB,(X'80',MTAXH),(X'80',MPTYPH)                         
         JNE   PURA20                                                           
         GOTO1 (RF),(R1),(8,MUNITH),MOVERH                                      
                                                                                
PURA20   AHI   R2,MLNQ                                                          
         CR    R2,R3                                                            
         JL    PURA10                                                           
         J     XIT                                                              
         DROP  RB                                                               
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        LITERALS FOR PROURA ROUTINES                                 *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CALCULATES AND DISPLAYS TIMESHEET'S TOTAL DOLLAR     *         
*        AMOUNT                                                       *         
*        ON ENTRY ... P1=A(TOTAL FIELD)                               *         
*                     AIO1=A(TIMESHEET RECORD)                        *         
***********************************************************************         
                                                                                
DISTOT   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
                                                                                
         XR    R3,R3                                                            
                                                                                
         USING TATDD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DT10     BRAS  RE,NEXTEL                                                        
         JNE   DT20                                                             
         ZICM  RF,TATDAMNT,4                                                    
         AR    R3,RF                                                            
         J     DT10                                                             
         DROP  R4                                                               
                                                                                
DT20     EDIT  (R3),(10,8(R2)),2,ZERO=NOBLANK,FLOAT=-                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DISTOT ROUTINES                                 *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS IF INVOICE IS PAID                            *         
*        ON ENTRY ... P1 BYTE 0 = X'80' RETURN ERROR                  *         
***********************************************************************         
                                                                                
CHKPAID  NTR1  BASE=*,LABEL=*                                                   
         NI    PROSTAT,X'FF'-PSINPAID                                           
         XR    R0,R0                                                            
         TM    0(R1),X'80'                                                      
         JZ    CP10                                                             
         LHI   R0,1                                                             
                                                                                
CP10     CLI   TGINV+5,X'FF'                                                    
         JE    *+10                                                             
         XC    TGINV,CPHEXFFS                                                   
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',0)                                    
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAINPINF,TAINPINF                                                
         JZ    XIT                                                              
         OI    PROSTAT,PSINPAID                                                 
         LTR   R0,R0                                                            
         JZ    XIT                                                              
         LA    R2,ETIFRSTH                                                      
         J     ERIPD                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        LITERALS FOR CHKPAID ROUTINE                                 *         
***********************************************************************         
                                                                                
CPHEXFFS DC    10X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,R6                                                            
       ++INCLUDE TAUSEPT                                                        
**********************************************************************          
*        SCREENS                                                     *          
**********************************************************************          
                                                                                
       ++INCLUDE TAGENFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR6CD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR6BD                                                       
*                                                                               
PTRBLK   DS    CL((40*L'TLDRREC)+1)                                             
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
         PRINT ON                                                               
**********************************************************************          
*        DSECT TO COVER WORKING STORAGE                              *          
**********************************************************************          
                                                                                
MYD      DSECT                                                                  
MYACTNUM DS    XL(L'ACTNUM)                                                     
PROSTAT  DS    X                                                                
PSCNFPND EQU   X'80'               CONFIRMATION PENDING                         
PSGOTORE EQU   X'40'               GO TO EMPLOYEE REACHED                       
PSINVPAD EQU   X'20'               INVOICE IS PAID                              
PSUSTAX  EQU   X'10'               US TAX AREA ENTERED                          
PSCNFHND EQU   X'08'               CONFIRMATION HANDLED                         
PSACETRD EQU   X'04'               AGENCY COMMISSION ENTERED                    
PSINPAID EQU   X'02'               INVOICE IS PAID                              
PSMINOR  EQU   X'01'               W4 IS FOR A MINOR                            
PROSTAT2 DS    X                                                                
PSWATAX  EQU   X'80'               WA TAX AREA ENTERED                          
PSHITAX  EQU   X'40'               HI TAX AREA ENTERED                          
PSHASCRP EQU   X'20'               PERFORMER HAS CORP ON W4                     
CLISTAT  DS    X                                                                
CSAC     EQU   X'80'               AGENCY COMMISION VALID FOR CLIENT            
CANTAX   DS    CL(L'TATDUNIT)                                                   
LSTDTSQ  DS    XL(L'TATDTSQ)       LAST ASSIGNED TAX UNIT SEQUENCE NUM          
                                                                                
SVKEY    DS    XL(L'KEY)                                                        
SVCAKEY  DS    XL(L'TLCAKEY)                                                    
SVTMKEY  DS    XL(L'TLTMKEY)                                                    
SVLPID   DS    CL(L'LPID)                                                       
                                                                                
LASTATE  DS    CL3                                                              
THISTATE DS    CL3                                                              
                                                                                
**FLDWHDR  DS    XL(8+L'LNAM)                                                   
FLDWHDR  DS    XL(8+L'LCORPN)                                                   
FLDWHDR2 DS    XL(8+L'LCORPN)                                                   
                                                                                
TOTAL    DS    F                                                                
OVERAMT  DS    F                                                                
                                                                                
SVCASTA3 DS    XL(L'TACASTA3)                                                   
SVSSNFLT DS    CL(L'TGSSN)                                                      
SVCORP   DS    XL(L'TACACORP)                                                   
SVCORPN  DS    CL(L'LCORPN)                                                     
SVPFNAM  DS    CL(L'LCORPN)                                                     
SVTMSTF  DS    C                                                                
SVCASEQ  DS    CL(L'TLCASEQ)                                                    
                                                                                
ALSTPOAR DS    A                                                                
ATATDTAB DS    A                                                                
AUPDTSKS DS    A                                                                
AUPDPTYS DS    A                                                                
SVATHISL DS    A                                                                
AMLISTAB DS    A                                                                
MYLISTAB DS    10CL(L'TLTMKEY)                                                  
MYLISTBX EQU   *-MYLISTAB                                                       
                                                                                
                                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
**********************************************************************          
*        DSECT TO COVER MAINTENANCE SCREEN LINE                      *          
**********************************************************************          
                                                                                
MAINTD   DSECT                                                                  
MTAXH    DS    XL8                                                              
MTAX     DS    XL3                 TAX UNIT                                     
MTASKH   DS    XL8                                                              
MTASK    DS    CL6                 TASK                                         
MPTYPH   DS    XL8                                                              
MPTYP    DS    CL6                 PAY TYPE                                     
         DS    XL4                                                              
MPTYPS   DS    XL1                 PAY TYPE STATUS                              
MPTYPSN  EQU   X'80'               AMOUNT WILL BE NEGATIVE                      
MPTYPAC  EQU   X'40'               AGENCY COMMISSION                            
         DS    XL3                                                              
MUNITH   DS    XL8                                                              
MUNIT    DS    CL6                 UNITS                                        
MRATEH   DS    XL8                                                              
MRATE    DS    CL10                RATE                                         
MAMTH    DS    XL8                                                              
MAMT     DS    CL10                AMOUNT                                       
MOVERH   DS    XL8                                                              
MOVER    DS    CL2                 OVERRIDE                                     
MLNQ     EQU   *-MAINTD                                                         
         EJECT                                                                  
**********************************************************************          
*        DSECT TO COVER LIST LINE                                    *          
**********************************************************************          
                                                                                
LISTD    DSECT                                                                  
LPID     DS    CL6                 PID                                          
**       DS    CL3                                                              
         DS    CL3                                                              
LCORP    DS    CL1                 CORP                                         
         DS    CL39                                                             
LAMT     DS    CL10                AMOUNT                                       
LISTLNQ  EQU   *-LISTD                                                          
                                                                                
LIST2D    DSECT                                                                 
         DS    CL8                 SCREEN LINE HEADER 8 BYTES                   
LPNAME   DS    CL16                PNAME                                        
         DS    CL6                                                              
LCORPN   DS    CL23                CORPNAME                                     
         DS    CL2                                                              
LEXPDT   DS    CL23                EXPIRATION DATE                              
LST2LNQ  EQU   *-LIST2D                                                         
                                                                                
**********************************************************************          
*        DSECT FOR NMOD STORAGE GRAB AREA                            *          
**********************************************************************          
                                                                                
TMPD     DSECT                                                                  
TATDTAB  DS    XL(5*TATDLNQ+1)                                                  
UPDTSKS  DS    XL(16*L'TLTKTASK+1)                                              
UPDPTYS  DS    XL(16*L'TLPMPTYP+1)                                              
TMPLNQ   EQU   *-TMPD                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008TAGEN5F   10/31/14'                                      
         END                                                                    
