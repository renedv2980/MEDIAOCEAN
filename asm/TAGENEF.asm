*          DATA SET TAGENEF    AT LEVEL 005 AS OF 07/20/12                      
*PHASE T702EFC                                                                  
         TITLE 'T702EF - CONTRACT LIST'                                         
T702EF   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702EF                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE                                                       
         USING CNLISTD,R7                                                       
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
         CLI   MODE,LISTRECS       IF MODE LISTRECS                             
         BNE   XIT                                                              
         MVC   LISTAR,SPACES                                                    
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS                                   
         SPACE 2                                                                
VK       GOTO1 FLDVAL,DMCB,(X'40',CNLAGYH),(X'80',CNLCONIH)                     
         BE    VKX                                                              
         SPACE                                                                  
         XC    FILTERS,FILTERS                                                  
         XC    LASTKEY,LASTKEY                                                  
         XC    TIQSTART,TIQSTART                                                
         XC    TIFCOM,TIFCOM                                                    
         XC    TIFVERS,TIFVERS                                                  
         XC    TIFCLI,TIFCLI                                                    
         XC    TIFPRD,TIFPRD                                                    
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',CNLAGYH) VALIDATE AGENCY              
         MVC   TIFAGY,TGAGY                                                     
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
         CLI   CNLCOMIH+5,0        IF COMMERCIAL ID FILTER ENTERED              
         BE    VK20                                                             
         LA    R2,CNLCOMIH         MUST BE A VALID COMMERCIAL ID                
         GOTO1 RECVAL,DMCB,(X'20',TLCOICDQ),(X'24',(R2))                        
         BNE   ERRINV                                                           
         SPACE                                                                  
         USING TLCOPD,R4                                                        
         LA    R4,KEY                                                           
         MVC   TIFCOM,TLCOICOM      SAVE COMMERCIAL ID FILTER                   
         SPACE                                                                  
         CLI   TLCOIVER,0           IF VERSION ID WAS ENTERED                   
         BE    VK10                                                             
         LA    R2,CNLVERSH          ANY INPUT IN VERSION FIELD                  
         CLI   5(R2),0              IS INVALID                                  
         BNE   ERRINV                                                           
         MVC   CNLVERS,TLCOIVER     MOVE VERSION LETTER ONTO SCREEN             
         MVI   CNLVERSH+5,1                                                     
         OI    CNLVERSH+6,X'80'                                                 
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    VK05                                                             
         EDIT  TLCOIVER,CNLVERS,ALIGN=LEFT                                      
         STC   R0,CNLVERSH+5                                                    
         DROP  R4                                                               
         SPACE                                                                  
VK05     L     R4,AIO                                                           
         SPACE                                                                  
         USING TLCOD,R4                                                         
         TM    TGSYSTAT,TASYS3VR   IF SYSTEM SET TO HANDLE 3-CHARACTER          
         BZ    VK07                VERSIONS                                     
         CLI   TLCOVER,TLCOV026    AND AIO DOES NOT CONTAIN MAIN                
         BE    VK07                COMMERCIAL, GO GET IT                        
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',TIFCOM)                              
         BE    VK07                                                             
         DC    H'00'                                                            
         DROP  R4                                                               
         SPACE                                                                  
         USING TACOD,R4                                                         
VK07     MVI   ELCODE,TACOELQ      AND REPLACE VERSION ID WITH                  
         BAS   RE,GETEL            COMMERCIAL ID                                
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   CNLCOMI,TACOCID                                                  
         OI    CNLCOMIH+6,X'80'                                                 
         B     VK20                                                             
         DROP  R4                                                               
         SPACE                                                                  
VK10     CLI   CNLVERSH+5,0                                                     
         BNE   VK20                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ      IF VERSION LETTER NOT IN KEY                 
         BAS   RE,GETEL            BUT VERSION ELEMENTS EXIST                   
         BNE   VK20                                                             
         MVI   CNLVERS,C'A'        MUST BE A VERSION                            
         MVI   CNLVERSH+5,1                                                     
         OI    CNLVERSH+6,X'80'                                                 
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    VK20                                                             
         MVI   CNLVERS,C'1'                                                     
         SPACE                                                                  
VK20     CLI   CNLVERSH+5,0        IN VERSION FIELD INPUT                       
         BE    VK40                                                             
         LA    R2,CNLVERSH                                                      
         OC    TIFCOM,TIFCOM       COMMERCIAL FILTER MUST BE ENTERED            
         BZ    ERRINV                                                           
         SPACE                                                                  
         MVC   TIFVERS,CNLVERS     SAVE VERSION ID FILTER                       
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    VK25                                                             
         GOTO1 VALINUM                                                          
         MVC   TIFVERS,ACTUAL                                                   
         SPACE                                                                  
         CLI   TIFVERS,26          IF VERSION CODE IS GREATER THAN              
         BNH   VK25                26                                           
         SPACE                                                                  
         USING VINDEXD,RE                                                       
         LA    RE,VERINDEX         FIND RECORD EQUATE FOR THIS                  
VK21     CLI   0(RE),X'FF'         VERSION NUMBER                               
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TIFVERS,VINDUPLM                                                 
         BNH   VK22                                                             
         LA    RE,VINDLNQ(RE)                                                   
         B     VK21                                                             
         SPACE 1                                                                
VK22     L     R4,AIO              GET COMMERCIAL RECORD FOR                    
         XC    KEY,KEY             THAT VERSION                                 
         MVC   KEY(L'TLCOKEY),0(R4)                                             
         MVC   KEY+TLCOVER-TLCOD(L'TLCOVER),VINDEQUT                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   ERRINV                                                           
         GOTO1 GETREC                                                           
         DROP  RE                                                               
         SPACE 1                                                                
         USING TAVRD,R4                                                         
VK25     L     R4,AIO              AND VERSION MUST EXIST                       
         MVI   ELCODE,TAVRELQ      ON COMMERCIAL                                
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VK30     BAS   RE,NEXTEL                                                        
         BNE   ERRINV                                                           
         CLI   TIFVERS,C'*'                                                     
         BE    VK40                                                             
         CLC   TIFVERS,TAVRVERS                                                 
         BNE   VK30                                                             
         DROP  R4                                                               
         SPACE                                                                  
VK40     GOTO1 RECVAL,DMCB,TLCLCDQ,CNLCLIH                                      
         MVC   TIFCLI,TGCLI        VALIDATE CLIENT                              
         SPACE                                                                  
VK41     CLI   CNLPRDH+5,0         VALIDATE PRODUCT                             
         BE    VK49                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,CNLPRDH                                      
         MVC   TIFPRD,TGPRD                                                     
         SPACE                                                                  
         USING TYPETABD,RE                                                      
VK49     LA    RE,TYPETAB          RE=A(TYPE TABLE)                             
VK50     CLI   0(RE),X'FF'         EXIT WHEN END OF TABLE REACHED               
         BE    VK70                                                             
         ZICM  R2,TYPEDISP,2                                                    
         AR    R2,RA               R2=A(TYPE FIELD)                             
         CLI   5(R2),0             IF TYPE FIELD CHECKED OFF                    
         BE    VK60                                                             
         CLI   FLTTYPE1,0          ERROR IF PREVIOUS TYPE FIELD                 
         BNE   ERRINV              CHECKED                                      
         CLI   FLTTYPE2,0                                                       
         BNE   ERRINV                                                           
         MVC   FLTTYPE1,TYPEEQU1   MOVE IN TYPE EQU                             
         MVC   FLTTYPE2,TYPEEQU2                                                
VK60     LA    RE,TYPELNQ(RE)                                                   
         B     VK50                                                             
         DROP  RE                                                               
         SPACE                                                                  
         USING STATTABD,RE                                                      
VK70     LA    RE,STATTAB          RE=A(STATUS TABLE)                           
VK80     CLI   0(RE),X'FF'         EXIT WHEN END OF TABLE REACHED               
         BE    VK100                                                            
         ZICM  R2,STATDISP,2                                                    
         AR    R2,RA               R2=A(TYPE FIELD)                             
         CLI   5(R2),0             IF TYPE FIELD CHECKED OFF                    
         BE    VK90                                                             
         CLI   FLTSTAT,0           ERROR IF PREVIOUS STATUS FIELD               
         BNE   ERRINV              CHECKED                                      
         MVC   FLTSTAT,STATEQU     MOVE IN STATUS EQU                           
VK90     LA    RE,STATLNQ(RE)                                                   
         B     VK80                                                             
         DROP  RE                                                               
         SPACE                                                                  
VK100    CLI   CNLSTARH+5,0        IF STARTING POINT ENTERED                    
         BE    VK110                                                            
         MVC   FLTCONS,CNLSTAR     SAVE START POINT FOR CONTRACT ID             
         OC    FLTCONS,SPACES                                                   
         MVC   TIQSTART,FLTCONS                                                 
         SPACE                                                                  
VK110    CLI   CNLCONIH+5,0        IF CONTRACT ID FILTER ENTERED                
         BE    VK120                                                            
         LA    R2,CNLCONIH         CONTRACT ID START POINT CANNOT               
         OC    FLTCONS,FLTCONS     BE ENTERED                                   
         BNZ   ERRINV                                                           
         MVC   FLTCONI,CNLCONI     SAVE CONTRACT ID FILTER                      
         OC    FLTCONI,SPACES                                                   
         MVC   TIQSTART,FLTCONI                                                 
         SPACE                                                                  
VK120    GOTO1 FLDVAL,DMCB,(X'20',CNLAGYH),(X'80',CNLCONIH)                     
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   TIUSERID,TWAORIG    INITIALIZE SYSIO FIELDS                      
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIQFLAGS,TIQFDIR                                                 
         SPACE                                                                  
         MVI   TIREAD,TLCNCDQ      SET TO READ CONTRACT KEYS                    
         OC    TIFCOM,TIFCOM                                                    
         BZ    VKX                                                              
         MVI   TIREAD,TLCNPCDQ     OR CONTRACT/COMM ID KEYS                     
         SPACE                                                                  
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO LIST CONTRACT RECORDS                                 
         SPACE 2                                                                
LR       TWAXC CNLSELH,CNLLSTH,PROT=Y                                           
         SPACE                                                                  
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         SPACE 1                                                                
         MVI   NLISTS,11           IN ORDER TO GET CONTROL                      
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,10           BACK AFTER 1 FULL PAGE                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              PROCESS SYSIO RECORDS                                            
         SPACE 1                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR                                                   
         BNE   LRH10                                                            
         BAS   RE,FILTKEY                                                       
         BE    YES                                                              
         B     NO                                                               
         SPACE                                                                  
LRH10    CLI   TIMODE,PROCREC                                                   
         BNE   XIT                                                              
         BAS   RE,FILTREC                                                       
         BNE   NO                                                               
         BAS   RE,DISPLAY                                                       
         B     XIT                                                              
         EJECT                                                                  
*              FILTER THE KEY                                                   
         SPACE 2                                                                
FILTKEY  NTR1                                                                   
         LA    R3,TIKEY                                                         
         OC    FLTCONS,FLTCONS     IF CONTRACT START FILTER                     
         BZ    FK10                ENTERED                                      
         SPACE                                                                  
         USING TLCNPD,R3                                                        
         CLI   TIREAD,TLCNPCDQ     AND READING PASSIVE KEY                      
         BNE   FK10                                                             
         CLC   TLCNPCND,FLTCONS    CONTRACT ID CAN'T BE LESS THAN               
         BL    NO                  START POINT                                  
         DROP  R3                                                               
         SPACE                                                                  
FK10     OC    FLTCONI,FLTCONI     IF CONTRACT ID FILTER                        
         BZ    FK30                ENTERED                                      
         SPACE                                                                  
         USING TLCND,R3                                                         
         CLI   TIREAD,TLCNCDQ      AND READING PRIMARY KEY                      
         BNE   FK20                                                             
         CLC   TLCNCNID,FLTCONI    CONTRACT ID MUST MATCH                       
         BE    FK30                                                             
         B     NO                                                               
         DROP  R3                                                               
         SPACE                                                                  
         USING TLCNPD,R3                                                        
FK20     CLC   TLCNPCND,FLTCONI    OR READING PASSIVE KEY                       
         BE    FK30                CONTRACT ID MUST MATCH                       
         B     NO                                                               
         DROP  R3                                                               
         SPACE                                                                  
         USING TLCNPD,R3                                                        
FK30     CLI   TIREAD,TLCNPCDQ     IF READING PASSIVE KEY                       
         BNE   YES                                                              
         CLC   TLCNPCOM,TIFCOM     COMMERCIAL OF CONTRACT                       
         BNE   NO                  MUST MATCH FILTER                            
         CLI   TIFVERS,C'*'        IF LISTING ALL VERSIONS                      
         BNE   FK40                                                             
         CLC   TLCNPKEY(31),FLTLKEY                                             
         BE    NO                  ONLY LIST THE FIRST INSTANCE                 
         MVC   FLTLKEY,TLCNPKEY    OF THIS CONTRACT                             
         B     FK60                                                             
FK40     CLI   TIFVERS,0           IF VERSION FILTER NOT DEFINED                
         BE    YES                 THEN FINE                                    
         MVI   TGBYTE,C'A'                                                      
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    *+8                                                              
         MVI   TGBYTE,1                                                         
         CLC   TIFVERS,TGBYTE      IF VERSION FILTER DEFINED AS                 
         BNE   FK50                A VERSION                                    
         CLI   TLCNPVER,0          AND CONTRACT HAS NO VERSION                  
         BE    YES                 THEN FINE                                    
FK50     CLC   TLCNPVER,TIFVERS    OTHERWISE, IF VERSION FILTER                 
         BE    YES                 DEFINED AS SPECIFIC VERSION                  
         B     NO                  GET ONLY THAT VERSION                        
         SPACE                                                                  
FK60     CLC   TIKEY(TLCNPVER-TLCNPD),LASTKEY                                   
         BE    NO                                                               
         MVC   LASTKEY,TIKEY                                                    
         B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              FILTER THE RECORD                                                
         SPACE 2                                                                
FILTREC  NTR1                                                                   
         L     R4,TIAREC           R4=A(CONTRACT RECORD)                        
         SPACE                                                                  
         USING TLCND,R4                                                         
         CLI   FLTSTAT,STATACT     IF STATUS FILTER IS ACTIVE                   
         BNE   FR10                                                             
         CLC   TGTODAY1,TLCNTRMS   TODAY MUST BE >= TERM START DATE             
         BL    NO                                                               
         CLC   TGTODAY1,TLCNTRME   AND <= TERM END DATE                         
         BH    NO                                                               
         SPACE                                                                  
FR10     CLI   FLTSTAT,STATEXP     IF STATUS FILTER IS EXPIRED                  
         BNE   FR20                                                             
         CLC   TGTODAY1,TLCNTRME   TODAY MUST BE > TERM START DATE              
         BNH   NO                                                               
         SPACE                                                                  
FR20     CLI   FLTSTAT,STATEXE     IF STATUS FILTER IS ALL EXCEPT EXP           
         BNE   FR30                                                             
         CLC   TGTODAY1,TLCNTRME   TODAY MUST BE <= END DATE                    
         BH    NO                                                               
         DROP  R4                                                               
         SPACE                                                                  
         USING TARDD,R4                                                         
FR30     MVI   ELCODE,TARDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'               R4=A(CONTRACT DETAILS ELEMENT)               
         SPACE                                                                  
         OC    FLTTYPE1(2),FLTTYPE1 IF TYPE FILTER INPUTTED                     
         BZ    FR40                                                             
         CLC   TARDTYP1(2),FLTTYPE1 GET NEXT RECORD IF TYPE OF                  
         BNE   NO                   CONTRACT RECORD DOES NOT MATCH              
         SPACE                                                                  
FR40     OC    TIFCLI,TIFCLI        IF CLIENT FILTER INPUTTED                   
         BZ    FR50                                                             
         CLC   TARDCLI,TIFCLI       ONLY GET THAT CLIENT                        
         BNE   NO                                                               
         SPACE                                                                  
FR50     OC    TIFPRD,TIFPRD        IF PRODUCT FILTER INPUTTED                  
         BZ    YES                                                              
         CLC   TARDPRD,TIFPRD       ONLY GET THAT PRODUCT                       
         BNE   NO                                                               
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         USING LISTD,R2                                                         
DISPLAY  NTR1                                                                   
         USING TLCND,R4                                                         
         L     R4,TIAREC                                                        
         GOTO1 DATCON,DMCB,(1,TLCNTRMS),(8,LTERMS)                              
         MVI   LTDASH,C'-'                                                      
         GOTO1 DATCON,DMCB,(1,TLCNTRME),(8,LTERME)                              
         MVC   LCONTID,TLCNCNID                                                 
         DROP  R4                                                               
         SPACE                                                                  
         USING TARDD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TARDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'               R4=A(CONTRACT DETAILS ELEMENT)               
         SPACE                                                                  
         USING TYPETABD,RE                                                      
         LA    RE,TYPETAB          RE=A(TYPE TABLE)                             
DISP10   CLI   0(RE),X'FF'         EXIT IF END OF TABLE REACHED                 
         BE    DISP30                                                           
         CLC   TARDTYP1(2),TYPEEQU1                                             
         BNE   DISP20                                                           
         MVC   LTYPE,TYPEDESC                                                   
         B     DISP30                                                           
DISP20   LA    RE,TYPELNQ(RE)                                                   
         B     DISP10                                                           
         DROP  RE                                                               
         SPACE                                                                  
DISP30   GOTO1 DATCON,DMCB,(1,TARDDATE),(8,LCONDATE)                            
         MVC   LPRODUCT,TARDPRD                                                 
         DROP  R4                                                               
         SPACE                                                                  
         MVC   AIO,TIAREC                                                       
         MVI   FAKEFLD,L'FAKEFLD                                                
         GOTO1 CHAROUT,DMCB,TAFNELQ,(1,FAKEFLD),TAFNTCON                        
         MVC   LDESCRIP,FAKEFLD+8                                               
         SPACE                                                                  
DISP40   MVC   DMDSKADD,TIDSKADD                                                
         GOTO1 LISTMON                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS AND MESSAGES                                              
         SPACE 2                                                                
ERRINV   MVI   ERROR,INVALID                                                    
         B     MESSXIT                                                          
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     MESSXIT                                                          
                                                                                
MESSXIT  GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              TABLES                                                           
         SPACE 2                                                                
*              TABLE OF TYPE FIELDS AND CORRESPONDING EQUATES                   
         SPACE 1                                                                
TYPETAB  DS    0X                                                               
         DC    AL2(CNLMUSLH-T702FFD)   MUSIC LICENSE                            
         DC    AL1(TARDTYML,0)                                                  
         DC    CL8'MUS LIC'                                                     
         DC    AL2(CNLNEDMH-T702FFD)   NEEDLEDROP MUSIC                         
         DC    AL1(TARDTYNM,0)                                                  
         DC    CL8'NEED MUS'                                                    
         DC    AL2(CNLSTFAH-T702FFD)   STOCK FOOTAGE AGREEMENT                  
         DC    AL1(TARDTYSF,0)                                                  
         DC    CL8'STK FTG'                                                     
         DC    AL2(CNLNONUH-T702FFD)   NON-UNION AGREEMENT                      
         DC    AL1(TARDTYNU,0)                                                  
         DC    CL8'NON UNI'                                                     
         DC    AL2(CNLPRORH-T702FFD)   PROPERTY RELEASE                         
         DC    AL1(TARDTYPR,0)                                                  
         DC    CL8'PROP REL'                                                    
         DC    AL2(CNLCNLKH-T702FFD)   CHARACTER/NAME/LIKENESS                  
         DC    AL1(TARDTYCL,0)                                                  
         DC    CL8'CH/NM/LI'                                                    
         DC    AL2(CNLMSTRH-T702FFD)   MASTER RECORDING                         
         DC    AL1(TARDTYMR,0)                                                  
         DC    CL8'MAST REC'                                                    
         DC    AL2(CNLOTHRH-T702FFD)   OTHER                                    
         DC    AL1(TARDTYOT,0)                                                  
         DC    CL8'OTHER'                                                       
         DC    AL2(CNLCOVGH-T702FFD)   CELEBRITY/OVERSCALE GUARANTEE            
         DC    AL1(TARDTYCG,0)                                                  
         DC    CL8'CELEB/OV'                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
*              TABLE OF STATUS FIELDS AND CORRESPONDING EQUATES                 
         SPACE 1                                                                
STATTAB  DS    0X                                                               
         DC    AL2(CNLSTAAH-T702FFD)   ACTIVE                                   
         DC    AL1(STATEXE)                                                     
         DC    AL2(CNLSTAEH-T702FFD)   EXPIRED                                  
         DC    AL1(STATEXP)                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
*              TABLE OF PF KEYS                                                 
         SPACE 2                                                                
PFTABLE  DS    0C                  PF KEYS TABLE                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE                   
*              VERSION CODE IS ON                                               
         SPACE 1                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              EQUATES                                                          
         SPACE 2                                                                
STATACT  EQU   X'80'                                                            
STATEXP  EQU   X'40'                                                            
STATEXE  EQU   X'20'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL W/S (TWAHOLE)                               
         SPACE 2                                                                
CNLISTD  DSECT                                                                  
FILTERS  DS    0XL58                                                            
FLTTYPE1 DS    XL1                 FILTER FOR CONTRACT TYPES                    
FLTTYPE2 DS    XL1                                                              
FLTSTAT  DS    XL1                 FILTER FOR CONTRACT STATUS                   
FLTCONI  DS    XL12                FILTER FOR CONTRACT ID                       
FLTCONS  DS    XL12                START POINT FOR CONTRACTS                    
FLTLKEY  DS    XL31                LAST KEY LISTED                              
         SPACE                                                                  
FAKEFLD  DS    XL68                FAKE CONTRACT DESCRIPTION FIELD              
         SPACE                                                                  
LASTKEY  DS    XL32                                                             
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 2                                                                
LISTD    DSECT                                                                  
LDESCRIP DS    CL17                CONTRACT DESCRIPTION                         
         DS    XL1                                                              
LTERMS   DS    CL8                 TERM START                                   
LTDASH   DS    XL1                 SLASH                                        
LTERME   DS    CL8                 TERM END                                     
         DS    XL1                                                              
LCONTID  DS    CL12                CONTRACT ID                                  
         DS    XL1                                                              
LCONDATE DS    CL8                 CONTRACT DATE                                
         DS    XL1                                                              
LPRODUCT DS    CL6                 PRODUCT                                      
         DS    XL1                                                              
LTYPE    DS    CL8                 CONTRACT TYPE                                
         EJECT                                                                  
*        DSECT TO COVER TABLE OF TYPE FIELDS AND EQUATES                        
         SPACE 2                                                                
TYPETABD DSECT                                                                  
TYPEDISP DS    AL2                     DISPLACEMENT OF SCREEN FIELD             
TYPEEQUS DS    0XL2                                                             
TYPEEQU1 DS    XL1                     TYPE EQUATE FOR FLTTYPE1                 
TYPEEQU2 DS    XL1                     TYPE EQUATE FOR FLTTYPE2                 
TYPEDESC DS    CL8                     TYPE DESCRIPTION FOR LIST                
TYPELNQ  EQU   *-TYPETABD                                                       
         SPACE 2                                                                
STATTABD DSECT                                                                  
STATDISP DS    AL2                     DISPLACEMENT OF SCREEN FIELD             
STATEQU  DS    XL1                     STATUS EQUATE FOR FLTSTAT                
STATLNQ  EQU   *-STATTABD                                                       
         EJECT                                                                  
*              DSECT TO COVER TABLE THAT DETERMINES WHICH COMMERCIAL            
*              RECORD THE VERSION CODE IS ON                                    
         SPACE 1                                                                
VINDEXD  DSECT                                                                  
VINDUPLM DS    X                 RECORD'S UPPER LIMIT                           
VINDEQUT DS    X                 RECORD'S EQUATE                                
VINDLNQ  EQU   *-VINDEXD                                                        
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR0DD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCREFD                                                       
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005TAGENEF   07/20/12'                                      
         END                                                                    
