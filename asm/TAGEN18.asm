*          DATA SET TAGEN18    AT LEVEL 013 AS OF 12/01/16                      
*PHASE T70218B,*                                                                
         TITLE 'T70218 - COMMERCIAL MAINTENANCE'                                
T70218   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T70218,R7,R6                                              
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    RE,ASVPTRS          SAVE A(SAVED POINTER BLOCK)                  
         AHI   RE,L'SVPTRBLK                                                    
         ST    RE,AUPPTRS          SAVE A(UPDATED POINTER BLOCK)                
                                                                                
         BRAS  RE,INIT             INITIALIZE PROGRAM                           
                                                                                
         CLI   ACTNUM,ACTCOPY      COPY COM2 INFORMATION                        
         JNE   *+8                                                              
         BRAS  RE,COPY                                                          
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JNE   *+8                                                              
         BRAS  RE,DK                                                            
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         JNE   *+8                                                              
         BRAS  RE,VR                                                            
                                                                                
         CLI   MODE,RECDEL         DELETE RECORD                                
         JNE   *+8                                                              
         BRAS  RE,DE                                                            
                                                                                
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ERROR MESSAGES AND EXITS                                     *         
***********************************************************************         
                                                                                
ERPFK    LA    R2,SC2CIDH                                                       
         MVI   ERROR,ERINVPFK      INVALID PF KEY                               
         J     END                                                              
                                                                                
ERRAY    MVI   ERROR,ERINVRAG      INVALID RECORD FOR THIS AGENCY               
         J     END                                                              
                                                                                
ERIRC    MVI   ERROR,ERINVRCL      INVALID RECORD FOR THIS CLIENT               
         J     END                                                              
                                                                                
ERIST    MVI   ERROR,ERRECCTY      INVALID SCREEN FOR COMML TYPE                
         J     END                                                              
                                                                                
ERINV    MVI   ERROR,INVALID       INVALID INPUT                                
         J     END                                                              
                                                                                
ERMIS    MVI   ERROR,MISSING       MISSING INPUT                                
         J     END                                                              
                                                                                
ERNFD    MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         J     END                                                              
                                                                                
EREXI    MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         J     END                                                              
                                                                                
ERNDE    BRAS  RE,DR                                                            
         MVI   ERROR,ERINVDEL      RECORD CANNOT BE DELETED                     
         J     END                                                              
                                                                                
ERIRA    LA    R2,CONACTH                                                       
         MVI   ERROR,INVRCACT      INVALID RECORD/ACTION                        
         J     END                                                              
                                                                                
ERALK    LA    R2,SCOAGYH                                                       
         MVI   ERROR,ERAGYLCK      AGENCY IS LOCKED                             
         J     END                                                              
                                                                                
ERCLK    MVI   ERROR,ERCLILCK      CLIENT IS LOCKED                             
         J     END                                                              
                                                                                
ERPLK    LHI   RE,ERPRDLCK         PRODUCT IS LOCKED                            
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERGLC    MVI   ERROR,ERNOGLOB      GLOBAL CLIENT NOT ALLOWED                    
         J     END                                                              
                                                                                
ERINA    MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         J     END                                                              
                                                                                
ERV2T    LHI   RE,ERVTNODL         CANNOT DELETE VERSION 2+ IF                  
         STH   RE,MYMSGNO          IT HAS TRACKS                                
         J     ERREND                                                           
                                                                                
ERUWB    LHI   RE,ERUSEWEB         RECORD MUST BE UPDATED FROM                  
         STH   RE,MYMSGNO          VITA SESSION                                 
         J     ERREND                                                           
                                                                                
ERCRF    LHI   RE,ERRCOREF         RECORD MUST BE REFRESHED                     
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERICS    LHI   RE,ERSEACST         INVALID CAST FOR SEASONAL                    
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
INFOR    LHI   RE,268              FIRST FIXED CYCLE OUT OF RANGE               
         STH   RE,MYMSGNO                                                       
         J     INFEND                                                           
                                                                                
ERMFA    LHI   RE,ERMSSFAD         MISSING FIRST AIR DATE                       
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERLFA    LHI   RE,ERAFTFAD         MUST BE LATER THAN FIRST AIR DATE            
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERRPC    LHI   RE,ERRRLPRI         CANNOT RELEASE - PRIMARY COMM'L              
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERM476   LHI   RE,ERMUS476         TRACK NUMBER ALREADY EXISTS                  
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERM477   LHI   RE,ERMUS477         CANNOT CHANGE-TRACK USED ON COMM'L           
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERM478   LHI   RE,ERMUS478         CANNOT DELETE-TRACK USED ON COMM'L           
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERM479   LHI   RE,ERMUS479         CANNOT DELETE-MUST BE REMOVED FROM           
         STH   RE,MYMSGNO          ASSOCIATED MUSICIANS                         
         J     ERREND                                                           
                                                                                
ERM480   LHI   RE,ERMUS480         TRACKS EXIST FOR COMMERCIAL                  
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERM482   LHI   RE,ERMUS482         AGENCY CODE DOES NOT EXIST                   
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERM483   LHI   RE,ERMUS483         CONTRACT ID DOES NOT EXIST                   
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERM484   LHI   RE,ERMUS484         TRACK NUMBER DOES NOT EXIST                  
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERM485   LHI   RE,ERMUS485         TRACKS EXIST ON COMMERCIAL                   
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERM486   LHI   RE,ERMUS486         MUSICIANS EXIST ON COMMERCIAL                
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERM488   LHI   RE,ERMUS488         MUSICIANS EXIST FOR COMMERCIAL               
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERM497   LHI   RE,ERMUS497         TRACK DOES NOT HAVE ANY ASSOCIATED           
         STH   RE,MYMSGNO          MUSICIANS                                    
         J     ERREND                                                           
                                                                                
ERM498   LHI   RE,ERMUS498         CANNOT CHANGE-MUST BE REMOVED FROM           
         STH   RE,MYMSGNO          ASSOCIATED MUSICIANS                         
         J     ERREND                                                           
                                                                                
ERM499   LHI   RE,ERMUS499         VERSIONS NOT ALLOWED FOR TYPE M              
         STH   RE,MYMSGNO          COMMERCIAL                                   
         J     ERREND                                                           
                                                                                
ERM502   LHI   RE,ERMUS502         AFM CONTRACT NEQ COMMERCIAL ID               
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERPPLSI  LHI   RE,ERRIAPPA         RECORD / ACTION INVALID FOR P+               
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
INJAC    MVI   PARAS,C'N'                                                       
         LHI   RE,116              RECORD ADD - NEED TO ADD PAGE                
         STH   RE,MYMSGNO          2 DATA?                                      
         J     INFEND                                                           
                                                                                
INC2D    LA    R2,CONACTH                                                       
         LHI   RE,ERDISCO2         COM2 MUST BE DISPLAYED BEFORE                
         STH   RE,MYMSGNO          COPYING                                      
         J     ERREND                                                           
                                                                                
ERWCAN   LHI   RE,ERRWCAN          2404 INVALID WITH CAN$/CRATE                 
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
INC2C    LHI   RE,274              COM2 INFORMATION COPIED                      
         STH   RE,MYMSGNO                                                       
         J     INFEND                                                           
                                                                                
INROV    NI    PROSTAT,X'FF'-PSROVAP                                            
         LHI   RE,275              MEDIA/TYPE CHANGE REMOVED                    
         STH   RE,MYMSGNO          OVERSCALE AMOUNT/PERCENTAGE                  
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
                                                                                
CO       EQU   41                                                               
                                                                                
ACTVER   EQU   18                                                               
ACTCOPY  EQU   27                                                               
                                                                                
SCR18    EQU   X'18'                                                            
SCRF8    EQU   X'F8'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE PROGRAM                                *         
***********************************************************************         
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTREST      ACTION RESTORE IS NOT ALLOWED                
         JE    ERIRA                                                            
                                                                                
         CLI   PFAID,24            IF COMING FROM REPORT                        
         JNE   *+8                                                              
         MVI   PFAID,0             CLEAR PF KEY NOW                             
                                                                                
         CLI   CALLSP,0            IF STACK IS POPULATED                        
         JE    INIT10                                                           
         CLI   PFAID,0             AND PFKEY IS NOT HIT                         
         JNE   INIT10                                                           
         GOTO1 FLDVAL,DMCB,(X'40',SCOAGYH),(X'80',999)                          
         JNE   INIT10                                                           
         OI    TRNSTAT,OKINTPFK    AND NO FIELDS HAVE BEEN CHANGED              
         MVI   PFAID,24            SET TO GO TO NEXT ENTRY IN STACK             
                                                                                
INIT10   CLI   TWASCR,SCRF8        IF NOT ON COM2 SCREEN                        
         JE    *+8                 SET COM2 NOT DISPLAYED YET                   
         NI    PROSTAT,X'FF'-PSCO2DIS                                           
                                                                                
         MVC   PF19REC,IRECCO2                                                  
         CLI   TWASCR,SCR18        SET PF18 RECORD TO COMMERCIAL OR             
         JE    *+10                COM2 BASED ON CURRENT RECORD                 
         MVC   PF19REC,IRECCOM                                                  
                                                                                
         MVC   PF19ACT,IACTDIS                                                  
         CLI   ACTNUM,ACTDIS       SET PF18 ACTION TO DISPLAY OR                
         JE    *+10                CHANGE BASED ON CURRENT ACTION               
         MVC   PF19ACT,IACTCHA                                                  
                                                                                
         MVI   TGBYTE3,TLCMTCOM                                                 
         MVI   TWAHOLE,CO                                                       
                                                                                
         CLI   MODE,XRECADD        IF COMMERCIAL WAS JUST ADDED                 
         JNE   INIT20              FORCE FLIP TO PAGE 2                         
         CLI   TGVER,1                                                          
         JNE   INIT20                                                           
         MVI   PARAS,C'Y'                                                       
         MVI   PFAID,19                                                         
                                                                                
INIT20   GOTO1 INITIAL,DMCB,PFTABLE                                             
                                                                                
         LA    RE,STATAB                                                        
         ST    RE,ASTATAB          SAVE A(COMMERCIAL STATUS TABLE)              
                                                                                
         LA    RE,MTYTAB                                                        
         ST    RE,AMTYTAB          SAVE A(MUSIC TYPE TABLE)                     
                                                                                
         LA    RE,STYTAB                                                        
         ST    RE,ASTYTAB          SAVE A(SESSION TYPE TABLE)                   
                                                                                
         LA    RE,INDTAB                                                        
         ST    RE,AINDTAB          SAVE A(INDEXED COMMERCIAL TABLE)             
                                                                                
         LA    RE,UVCTAB                                                        
         ST    RE,AUVCTAB          SAVE A(UNVERIFY FIELDS TABLE)                
                                                                                
         LA    RE,RHFTAB                                                        
         ST    RE,ARHFTAB          SAVE A(HF REISSUE FIELDS TABLE)              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        PF KEY TABLE                                                 *         
***********************************************************************         
                                                                                
PFTABLE  DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'COMMERCL',CL8'LIST    '                               
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'CID     ',CL8'NEW     '                               
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,0,0,0)                                            
         DC    CL3' ',CL8'CAST    ',CL8'VERIFY  '                               
PF15X    EQU   *                                                                
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3'FT ',CL8'FTRACK  ',CL8'LIST'                                 
PF16     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF16X    EQU   *                                                                
         DC    AL1(PF17X-*,17,0,0,0)                                            
         DC    CL3' ',CL8'VERSIONS',CL8'DISPLAY'                                
PF17X    EQU   *                                                                
         DC    AL1(PF18X-*,18,0,(PF18X-PF18)/KEYLNQ,0)                          
         DC    CL3' ',CL8'COMMENT ',CL8'DISPLAY '                               
PF18     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGBYTE3-1),AL2(TGBYTE3-TGD)                       
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF18X    EQU   *                                                                
         DC    AL1(PF19X-*,19,0,(PF19X-PF19)/KEYLNQ,0)                          
         DC    CL3' '                                                           
PF19REC  DC    CL8'        '                                                    
PF19ACT  DC    CL8'        '                                                    
PF19     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF19X    EQU   *                                                                
         DC    AL1(PF20X-*,20,0,(PF20X-PF20)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CONTRACT',CL8'LIST    '                               
PF20     DC    AL1(KEYTYTWA,L'SCOAGY-1),AL2(SCOAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'SCOCID-1),AL2(SCOCID-T702FFD)                     
         DC    AL1(KEYTYTWA,L'SCOVRL-1),AL2(SCOVRL-T702FFD)                     
PF20X    EQU   *                                                                
         DC    AL1(PF21X-*,21,0,0,PFTSETPN)                                     
         DC    CL3' ',CL8'COMMERCL',CL8'REPORT  '                               
PF21X    EQU   *                                                                
         DC    AL1(PF22X-*,22,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF22X    EQU   *                                                                
         DC    AL1(PF23X-*,23,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF23X    EQU   *                                                                
         DC    AL1(PF24X-*,24,PFTRPROG+PFTINT,0,0)                              
         DC    CL3' ',CL8' ',CL8' '                                             
PF24X    DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        COMMERCIAL STATUS TABLE                                      *         
***********************************************************************         
                                                                                
STATAB   DS    0CL11                                                            
         DC    AL1(TACOSTLO),CL10'LOCK'          LOCKED                         
         DC    AL1(TACOSTRL),CL10'REL'           RELEASED                       
         DC    AL1(TACOSCAN),CL10'CAN$'          CANADIAN DOLLARS               
         DC    AL1(TACOSCRT),CL10'CRATE'         CANADIAN RATES                 
         DC    AL1(TACOSWDT),CL10'WORKDATE'      WORKDATE ON CHECKS             
         DC    AL1(TACOSRES),CL10'RESIDUALS'     SOAP RESIDUALS                 
         DC    AL1(TACOSPRT),CL10'PRT'           DISPLAY AS PRINT               
         DC    AL1(0),CL10'PERCYC'               PER CYCLE DUMMY                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        MUSIC TYPE TABLE                                             *         
***********************************************************************         
                                                                                
MTYTAB   DS    0CL1                                                             
         DC    C'A'                ARRANGEMENT OF LICENSED                      
         DC    C'C'                ARRANGEMENT OF CLIENT OWNED                  
         DC    C'L'                LICENSED                                     
         DC    C'O'                ORIGINAL                                     
         DC    C'P'                PUBLIC DOMAIN                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        SESSION TYPE TABLE                                           *         
***********************************************************************         
                                                                                
STYTAB   DS    0CL1                                                             
         DC    C'D'                DEMO                                         
         DC    C'F'                FINAL                                        
         DC    C'M'                MASTER RECORDING                             
         DC    C'T'                TEST MARKET                                  
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        INDEXED COMMERCIAL TABLE                                     *         
***********************************************************************         
                                                                                
INDTAB   DS    0XL2                                                             
         DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        TABLE OF FIELDS THAT, WHEN CHANGED, UNVERIFY COMMERCIAL      *         
***********************************************************************         
                                                                                
UVCTAB   DS    0XL3                                                             
         DC    AL1(SCR18),AL2(SCOCLIH-T702FFD)                                  
         DC    AL1(SCR18),AL2(SCOTYPEH-T702FFD)                                 
         DC    AL1(SCR18),AL2(SCOFFCH-T702FFD)                                  
         DC    AL1(SCR18),AL2(SCOMEDH-T702FFD)                                  
         DC    AL1(SCR18),AL2(SCOLENH-T702FFD)                                  
         DC    AL1(SCR18),AL2(SCOLLENH-T702FFD)                                 
         DC    AL1(SCR18),AL2(SCOSTATH-T702FFD)                                 
         DC    AL1(SCRF8),AL2(SC2AFMH-T702FFD)                                  
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        TABLE OF FIELDS THAT, WHEN CHANGED, NECCESIATE REISSUE       *         
*        OF PENDING HOLDING FEE NOTICE                                *         
***********************************************************************         
                                                                                
RHFTAB   DS    0XL3                                                             
         DC    AL1(SCR18),AL2(SCOCLIH-T702FFD)                                  
         DC    AL1(SCR18),AL2(SCOPRDH-T702FFD)                                  
         DC    AL1(SCR18),AL2(SCOPRDNH-T702FFD)                                 
         DC    AL1(SCR18),AL2(SCOTITNH-T702FFD)                                 
         DC    AL1(SCR18),AL2(SCOTYPEH-T702FFD)                                 
         DC    AL1(SCR18),AL2(SCOFFCH-T702FFD)                                  
         DC    AL1(SCR18),AL2(SCOFAIRH-T702FFD)                                 
         DC    AL1(SCR18),AL2(SCOSAIRH-T702FFD)                                 
         DC    AL1(SCR18),AL2(SCOFDATH-T702FFD)                                 
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR INIT ROUTINE                      *         
***********************************************************************         
                                                                                
IRECCOM  DC    C'COMMERCL'                                                      
IRECCO2  DC    C'COM2    '                                                      
                                                                                
IACTDIS  DC    C'DISPLAY '                                                      
IACTCHA  DC    C'CHANGE  '                                                      
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO COPY COM2 DATA TO ANOTHER COMMERCIAL/VERSION      *         
***********************************************************************         
                                                                                
COPY     NTR1  BASE=*,LABEL=*                                                   
         TM    PROSTAT,PSCO2DIS    ENSURE COM2 HAS ALREADY BEEN                 
         JZ    INC2D               DISPLAYED                                    
                                                                                
         LA    R2,SC2CIDH                                                       
         TM    SC2TAGYH+1,X'20'    ENSURE THAT COPY FROM COMMERCIAL             
         JO    ERINV               IS NOT TYPE MUSIC                            
                                                                                
         BRAS  RE,VK               VALIDATE KEY                                 
                                                                                
         USING TACOD,R4                                                         
         CLI   TGVER,0             IF KEY CONTAINS A VERSION                    
         JE    COPY10                                                           
         GOTO1 GETREC              GET VERSION RECORD                           
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SCOVRI,TACOCID      DISPLAY VERSION ID                           
         MVI   SCOVRIH+5,L'SCOVRI                                               
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   COPY10              ENSURE COPY TO VERSION IS NOT                
         L     R4,TGELEM           STAMPED WITH VITA COMPLETIONS ID             
         CLC   =C'VC',TAFNNAME                                                  
         JE    ERUWB               IF COPYING TO VERSION 1                      
         CLC   =C'TC',TAFNNAME                                                  
         JE    ERUWB                                                            
         CLC   =C'RC',TAFNNAME     ENSURE COM'L IS NOT STAMPED                  
         JE    ERUWB               WITH VITA SESSIONS ID                        
         CLI   TGVER,1                                                          
         JE    ERUWB                                                            
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
COPY10   L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      ENSURE THAT COPY TO COMMERCIAL               
         BRAS  RE,GETEL            IS NOT TYPE MUSIC                            
         JE    *+6                                                              
         DC    H'00'                                                            
         CLI   TACOTYPE,CTYMUS                                                  
         JE    ERINV                                                            
         DROP  R4                                                               
                                                                                
         BRAS  RE,VR               VALIDATE RECORD                              
         BRAS  RE,DR               DISPLAY RECORD                               
         DC    H'00'               (SHOULD NEVER RETURN)                        
                                                                                
***********************************************************************         
*        LITERALS FOR COPY ROUTINE                                    *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         CLC   ACTNUM,TWALACT                                                   
         JE    VK10                                                             
         NI    SCOAGYH+4,X'DF'                                                  
VK10     GOTO1 FLDVAL,DMCB,(X'40',SCOAGYH),(X'80',SCOVRLH)                      
         JE    XIT                                                              
                                                                                
         LA    R3,KEY                                                           
         L     R4,AIO                                                           
         MVI   TGVER,0                                                          
                                                                                
         BAS   RE,VKINTCID          INTERNAL COMMERCIAL ID?                     
         BAS   RE,VKAGY                                                         
         BAS   RE,VKCID                                                         
         BAS   RE,VKVER                                                         
                                                                                
         GOTO1 FLDVAL,DMCB,(2,SCOAGYH),SCOVRIH                                  
         GOTO1 FLDVAL,DMCB,(X'20',SCOAGYH),(X'80',SCOVRLH)                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE IF WE ARE PROCESSING AN INTERNAL CID     *         
*        ON ENTRY ... R4=A(I/O AREA 1)                                *         
***********************************************************************         
VKINTCID NTR1                                                                   
         CLI   SCOAGYH+5,0                                                      
         BNE   VKINTCDX                                                         
         CLI   SCOCIDH+5,0                                                      
         BE    VKINTCDX                                                         
         CLI   SCOCID,C'('                                                      
         BNE   VKINTCDX                                                         
         ZIC   RE,SCOCIDH+5                                                     
         LA    RF,SCOCID                                                        
         AR    RF,RE                                                            
         SHI   RF,1                                                             
         CLI   0(RF),C')'                                                       
         BNE   VKINTCDX                                                         
* COMMERCIAL ID = ENCLOSED IN "( )"  - HENCE MUST BE INTERNAL CID               
         MVI   0(RF),0                                                          
         ZIC   RE,SCOCIDH+5                                                     
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SCOCID(0),SCOCID+1                                               
         SHI   RF,1                                                             
         MVI   0(RF),0                                                          
         ZIC   RE,SCOCIDH+5                                                     
         SHI   RE,2                                                             
         STC   RE,SCOCIDH+5                                                     
         GOTO1 HEXIN,DMCB,SCOCID,THISCOM,8                                      
*                                                                               
         XC    KEY,KEY             READ COMMERCIAL PASSIVE                      
         LA    R3,KEY                                                           
         USING TLCOPD,R3                                                        
         MVI   TLCOPCD,TLCOCCDQ                                                 
                                                                                
         MVC   TLCOCCOM,THISCOM                                                 
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         BNE   VKINTCDX                                                         
         DROP  R3                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         USING TLCOD,R4                                                         
         XC    SCOCID,SCOCID                                                    
         MVC   SCOCID,TLCOCID                                                   
         MVI   SCOCIDH+5,L'TLCOCID                                              
         XC    SCOAGY,SCOAGY                                                    
         MVC   SCOAGY,TLCOAGY                                                   
         MVI   SCOAGYH+5,L'TLCOAGY                                              
*                                                                               
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      GET AGENCY ELEMENT                           
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         XC    SCOCID,SCOCID                                                    
         MVC   SCOCID,TACOCID                                                   
         MVI   SCOCIDH+5,L'TACOCID                                              
VKINTCDX J     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
*        ROUTINE TO VALIDATE AGENCY                                   *         
*        ON ENTRY ... R4=A(I/O AREA 1)                                *         
***********************************************************************         
                                                                                
VKAGY    NTR1                                                                   
         MVI   BYTE,0                                                           
         GOTO1 CHKCLG,DMCB,SCOAGYH,SCOCIDH                                      
         JE    *+8                                                              
         MVI   BYTE,X'80'                                                       
         GOTO1 RECVAL,DMCB,(BYTE,TLAYCDQ),(X'22',SCOAGYH)                       
                                                                                
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   TAAYTPOF,C'O'       OK FOR CHLOE OFFICE                          
         JE    VKAGY5                                                           
         CLI   TAAYTPOF,C'F'       OK FOR OFFICE F                              
         JE    VKAGY5                                                           
         CLI   TAAYTPOF,C'0'       ENSURE OFFICE IS A NUMBER                    
         JL    ERRAY                                                            
VKAGY5   MVC   AYSTAT,TAAYSTAT     AND SAVE AGENCY STATUS BYTES                 
         MVC   AYSTAT3,TAAYSTA3                                                 
         MVC   AYSTAT5,TAAYSTA5                                                 
         MVC   AYSTAT6,TAAYSTA6                                                 
         OC    TAAYAGG,TAAYAGG     IS THERE AN AGENCY GROUP                     
         JZ    *+10                                                             
         MVC   TGAGG,TAAYAGG       SAVE INTO GLOBAL STORAGE                     
         DROP  R4                                                               
                                                                                
         USING TABRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ      GET BILLING RULES ELEMENT                    
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   AYBRSTAT,TABRSTAT   SAVE AGENCY STATUS                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE COMMERCIAL ID                            *         
*        ON ENTRY ... R3=A(KEY)                                       *         
*                     R4=A(I/O AREA 1)                                *         
***********************************************************************         
                                                                                
VKCID    NTR1                                                                   
         LA    R2,SCOCIDH          R2=A(COMMERCIAL ID FIELD)                    
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JNE   VKC10                                                            
         GOTO1 ANY                 ENSURE COMMERCIAL ID PROVIDED                
                                                                                
VKC10    GOTO1 RECVAL,DMCB,(X'30',TLCOICDQ),(X'24',SCOCIDH)                     
         JE    VKC20                                                            
                                                                                
         CLI   ACTNUM,ACTADD       IF COMMERCIAL DOES NOT EXIST                 
         JNE   ERNFD               ENSURE THAT ACTION IS ADD                    
         MVC   SCOVRL,=C'1  '      AND SET TO ADD VERSION 1                     
         MVI   SCOVRLH+5,1                                                      
         MVI   SCOVRLH+4,X'0A'                                                  
         MVC   SCOVRI,TGCID                                                     
         MVI   SCOVRIH+5,L'SCOVRI                                               
         MVC   KEY,KEYSAVE                                                      
         J     XIT                                                              
                                                                                
         USING TLCOPD,R3                                                        
VKC20    MVC   TGCOM,TLCOICOM      IF COMMERCIAL DOES EXIST, SAVE               
         MVC   TGVER,TLCOIVER      INTERNAL NUMBER AND VERSION                  
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         CLI   TLCOVER,TLCOV026    IF COMMERCIAL ID WAS FOR A VERSION           
         JE    VKC30               GREATER THAN 26, READ PRIMARY COM            
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',0)                                   
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R4                                                               
                                                                                
         USING TAVRD,R4                                                         
VKC30    CLI   SCOVRLH+5,0         IF VERSION NUMBER FIELD DOES                 
         JNE   VKC40               NOT CONTAIN INPUT                            
         CLI   TGVER,0             AND KEY DOES NOT INDICATE THAT               
         JNE   VKC40               COMMERCIAL IS A VERSION, IT                  
         MVI   ELCODE,TAVRELQ      STILL MAY BE VERSION 1                       
         BRAS  RE,GETEL                                                         
         JNE   VKC50                                                            
         MVC   TGVER,TAVRVERS                                                   
         DROP  R4                                                               
                                                                                
VKC40    CLI   TGVER,0             IF COMMERCIAL IS A VERSION                   
         JE    VKC50                                                            
         EDIT  TGVER,SCOVRL,ALIGN=LEFT                                          
         STC   R0,SCOVRLH+5        COPY VERSION NUMBER TO                       
*        MVI   SCOVRLH+4,X'0A'     VERSION NUMBER FIELD                         
         MVI   SCOVRLH+4,X'8A'                                                  
                                                                                
         USING TACOD,R4                                                         
VKC50    L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLI   TACOMED,TACOMEDP    ENSURE MEDIA IS NOT PRINT                    
         JE    ERIST                                                            
         CLI   TACOMED,TACOMEDE    OR EVENT                                     
         JE    ERPPLSI             AND COPY MASTER COMMERCIAL ID                
         MVC   SCOCID,TACOCID      INTO COMMERCIAL ID FIELD                     
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE VERSION CODE AND ID                      *         
*        ON ENTRY ... R3=A(KEY)                                       *         
*                     R4=A(PRIMARY COMMERCIAL RECORD)                 *         
***********************************************************************         
                                                                                
VKVER    NTR1                                                                   
         CLI   ACTNUM,ACTVER     EXIT IF ACTION IS VERIFY                       
         JE    XIT                                                              
                                                                                
         CLI   SCOVRLH+5,0       IF VERSION IS NOT PROVIDED                     
         JNE   VKV10                                                            
         MVC   SCOVRI,SPACES     CLEAR VERSION ID FIELD                         
         MVI   SCOVRIH+5,0                                                      
         J     XIT                                                              
                                                                                
VKV10    LA    R2,SCOVRLH        IF VERSION CODE IS PROVIDED                    
         GOTO1 VALINUM           MUST BE DIGIT BETWEEN 1 AND 250                
         CLI   ACTUAL,1                                                         
         JL    ERINV                                                            
         CLI   ACTUAL,250                                                       
         JH    ERINV                                                            
         MVC   TGVER,ACTUAL                                                     
                                                                                
         CLI   ACTNUM,ACTADD     IF ADDING A COMMERCIAL                         
         JNE   VKV40                                                            
         CLI   TGVER,1           COPY COMMERCIAL ID TO VERSION ID               
         JNE   VKV20                                                            
         MVC   SCOVRI,TGCID                                                     
         MVI   SCOVRIH+5,L'SCOVRI                                               
         J     XIT                                                              
                                                                                
         USING TAFND,R4                                                         
VKV20    CLI   TGVER,2           IF ADDING VERSION 2                            
         JNE   VKV30             ENSURE COMMERCIAL IS NOT STAMPED               
         MVI   ELCODE,TAFNELQ    WITH A VITA SESSION ID                         
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   VKV30                                                            
         L     R4,TGELEM                                                        
         CLC   =C'VS',TAFNNAME                                                  
         JE    ERUWB                                                            
         CLC   =C'TS',TAFNNAME                                                  
         JE    ERUWB                                                            
         CLC   =C'RS',TAFNNAME                                                  
         JE    ERUWB                                                            
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
VKV30    L     R4,AIO            IF ADDING VERSION 2 OR HIGHER                  
         MVI   ELCODE,TACOELQ    ENSURE COMMERCIAL TYPE IS NOT MUSIC            
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLI   TACOTYPE,CTYMUS                                                  
         JE    ERM499                                                           
         DROP  R4                                                               
                                                                                
VKV40    MVI   ELCODE,TALFELQ    IF COMMERCIAL ALREADY EXISTS                   
         BRAS  RE,GETEL          IT CANNOT HAVE A LIFT                          
         JE    ERINV                                                            
                                                                                
         CLI   ACTNUM,ACTCHA     IF CHANGING AN EXISTING COMMERCIAL             
         JNE   VKV50             AND THIS IS COMMERCIAL'S FIRST                 
         L     R4,AIO            VERSION IT MUST BE NUMBER 1                    
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    VKV60                                                            
         CLI   TGVER,1                                                          
         JNE   ERINV                                                            
                                                                                
VKV50    CLI   ACTNUM,ACTDIS     IF DISPLAYING AN EXISTING COMMERCIAL           
         JNE   VKV60             VERSION 1, ENSURE COMMERCIAL HAS               
         CLI   TGVER,1           VERSIONS                                       
         JNE   VKV60                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   ERINV                                                            
         J     XIT                                                              
                                                                                
VKV60    CLI   TGVER,2           IF ADDING/DISPLAYING/CHANGING VERSION          
         JL    XIT               HIGHER THAN 1, READ FOR VERSION KEY            
         GOTO1 RECVAL,DMCB,TLVRCDQ,(X'40',0)                                    
         GOTO1 HIGH                                                             
                                                                                
         CLI   ACTNUM,ACTADD     IF ACTION IS ADD ENSURE VERSION                
         JNE   VKV70             DOES NOT ALREADY EXIST                         
         CLC   KEY(L'TLVRKEY),KEYSAVE                                           
         JE    ERINV                                                            
         MVC   KEY,KEYSAVE                                                      
         J     XIT                                                              
                                                                                
VKV70    CLC   KEY(L'TLVRKEY),KEYSAVE                                           
         JNE   ERINV             ELSE, ENSURE IT DOES EXIST                     
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR VALIDATE KEY ROUTINES                           *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE KEY                                   *         
*        (ONLY CALLED WHEN VERIFYING CAST)                            *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
DK       NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVC   TGAGY,TLCOAGY                                                    
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   TGCID,TACOCID                                                    
         DROP  R4                                                               
                                                                                
         BRAS  RE,VK                                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DISPLAY KEY ROUTINES                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE RECORD                                *         
*        ON ENTRY ... AIO = A(PRIMARY COMMERCIAL RECORD)              *         
***********************************************************************         
                                                                                
DR       NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         BAS   RE,DRINIT           INITIALIZE SCREEN/VARIABLES                  
                                                                                
         BAS   RE,DISPID           DISPLAY PREVIOUS ID                          
         BAS   RE,DISCLI           DISPLAY CLIENT FIELDS                        
         BAS   RE,DISPRD           DISPLAY PRODUCT FIELDS                       
                                                                                
***********************************************************************         
                                                                                
         CLI   TWASCR,SCR18        IF ON COMMERCIAL SCREEN                      
         JNE   DR10                                                             
         BAS   RE,DISADS           DISPLAY ADDENDUM STATE                       
         BAS   RE,DISFFC           DISPLAY FIRST FIXED CYCLE                    
         BAS   RE,DISEXP           DISPLAY EXPIRATION DATE                      
         BAS   RE,DISMED           DISPLAY MEDIA                                
         BAS   RE,DISLFT           DISPLAY LIFT FIELDS                          
         BAS   RE,DISSAR           DISPLAY 2ND SEASON 1ST AIR DATE              
         BAS   RE,DISACT           DISPLAY ACTRA TYPE                           
         BAS   RE,DISSTA           DISPLAY STATUS FIELD                         
         BAS   RE,DISAED           DISPLAY ACTRA EXPIRATION DATE                
         BAS   RE,DISINM           DISPLAY INTERNET/NEW MEDIA CODES             
         BAS   RE,DISPOL           DISPLAY COMMERCIAL POOL                      
         BAS   RE,DISCSF           DISPLAY CONTRACT SERVICE FEE                 
         BAS   RE,DISSCY           DISPLAY SPLIT CYCLE INDICATOR                
         BAS   RE,DISBTC           DISPLAY BILL TO CODE                         
         BAS   RE,DISCON           DISPLAY CONTRACT TYPE                        
         BAS   RE,DISCOM           DISPLAY INTERNAL COMMERICAL NUMBER           
         BAS   RE,DISRIP           DISPLAY REISSUE PENDING INDICATOR            
         BAS   RE,DISVER           DISPLAY VERIFICATION INFORMATION             
                                                                                
         BAS   RE,GETVER           (POSSIBLY) GET VERSION RECORD                
                                                                                
         BAS   RE,DISVID           DISPLAY VERSION ID                           
         GOTO1 CHAROUT,DMCB,TANAELQ,SCOTITNH                                    
         BAS   RE,DISCTY           DISPLAY COMMERCIAL TYPE                      
         GOTO1 CHAROUT,DMCB,TAFNELQ,SCOTAFMH,TAFNTMUS                           
         BAS   RE,DISFAR           DISPLAY FIRST AIR DATE                       
         BAS   RE,DISLEN           DISPLAY LENGTH                               
         BAS   RE,DISSTAV          DISPLAY STATUS FIELD FOR VERSION             
         GOTO1 DISFRM,DMCB,('TACSTYPF',SCOFDATH)                                
         GOTO1 DISFRM,DMCB,('TACSTYPR',SCORDATH)                                
         GOTO1 DISFRM,DMCB,('TACSTYPM',SCOMDATH)                                
         BAS   RE,DISETY           DISPAY EDIT YEAR/TYPE                        
         BAS   RE,DISAID           DISPLAY ACTIVE/INACTIVE DATES                
         GOTO1 CHAROUT,DMCB,TACMELQ,SCOCOMMH,TACMTYPG                           
         BAS   RE,DISACM           DISPLAY ATTACHED COMMENT INDICATOR           
         BAS   RE,DISWID           DISPLAY WEB APPLICATION ID                   
         GOTO1 ACTVOUT,DMCB,SCOLCHGH                                            
                                                                                
         TM    PROSTAT,PSROVAP     IF MEDIA/TYPE CHANGE RESULTED IN             
         JO    INROV               REMOVAL OF OVERSCALE AMT/PCT FROM            
         J     DRX                 CAST RECORD, GIVE MESSAGE                    
                                                                                
***********************************************************************         
                                                                                
DR10     BAS   RE,DISAFM           DISPLAY AFM RATE                             
         BAS   RE,DISSTY           DISPLAY SESSION TYPE                         
         BAS   RE,DISFLT           DISPLAY FILTERS                              
                                                                                
         BAS   RE,GETVER           (POSSIBLY) GET VERSION RECORD                
                                                                                
         BAS   RE,DISVID           DISPLAY VERSION ID                           
         GOTO1 CHAROUT,DMCB,TANAELQ,SC2TITNH                                    
         BAS   RE,DISTRK           DISPLAY MUSIC CONTRACT TRACKS                
         BAS   RE,DISMCO           DISPLAY MUSIC CODES                          
         BAS   RE,DISARU           DISPLAY ALLOWABLE/REMAING USES               
         BAS   RE,DISDUB           DISPLAY DUB DATE                             
                                                                                
         OI    PROSTAT,PSCO2DIS    SET COM2 DISPLAYED                           
                                                                                
         CLI   PARAS,C'Y'          IF JUST ADDED RECORD                         
         JE    INJAC               GIVE SPECIAL MESSAGE                         
                                                                                
         CLI   ACTNUM,ACTCOPY      IF COPYING                                   
         JE    INC2C               GIVE SPECIAL MESSAGE                         
                                                                                
DRX      BAS   RE,SVLCHG           SAVE LAST CHANGED DATE/TIME                  
                                                                                
         CLI   ACTNUM,ACTCHA       UNLESS ACTION IS CHANGE                      
         JNE   *+12                                                             
         CLI   MODE,XRECPUT        AND MODE IS NOT XRECPUT                      
         JNE   XIT                 SET ALL FIELDS VALID                         
         GOTO1 FLDVAL,DMCB,(X'20',SCOVRIH),(X'80',999)                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO INITIALIZE SCREEN AND VARIABLES                   *         
***********************************************************************         
                                                                                
DRINIT   NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'20',0)                                   
         JE    *+6                                                              
         DC    H'00'               READ PRIMARY COMMERCIAL RECORD               
                                                                                
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL            SAVE A(COMMERCIAL DETAILS ELEMENT)           
         JE    *+6                                                              
         DC    H'00'                                                            
         ST    R4,ATACOEL                                                       
                                                                                
         XC    ATALFEL,ATALFEL                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TALFELQ      SAVE A(LIFT DETAILS ELEMENT)                 
         BRAS  RE,GETEL                                                         
         JNE   *+8                                                              
         ST    R4,ATALFEL                                                       
                                                                                
         GOTO1 FLDVAL,DMCB,(2,SCOVRIH),999                                      
         GOTO1 FLDVAL,DMCB,(1,SCOVRIH),(X'80',999)                              
                                                                                
         MVC   SCOPID,SPACES       CLEAR PREVIOUS ID                            
         MVC   SCOCLIN,SPACES      CLIENT NAME                                  
         MVC   SCOCGRP,SPACES      CLIENT GROUP NAME                            
                                                                                
***********************************************************************         
                                                                                
         CLI   TWASCR,SCR18        IF ON COMMERCIAL SCREEN                      
         JNE   IS20                                                             
         MVC   SCOPTYN,SPACES      CLEAR PRODUCT TYPE NAME                      
         MVC   SCOTNAM,SPACES      COMMERCIAL TYPE NAME                         
         MVC   SCOADSN,SPACES      ADDENDUM STATE NAME                          
         MVC   SCOMEDN,SPACES      MEDIA NAME                                   
         MVC   SCOBTCN,SPACES      BILL TO CODE NAME                            
         MVC   SCOCOGN,SPACES      COMMERCIAL POOL                              
         MVC   SCOCSF,SPACES       CSF AGENCY/INVOICE                           
         MVC   SCOINTN,SPACES      INTERNAL COMMERCIAL NUMBER                   
         MVC   SCOWID,SPACES       WEB APPLICATION ID                           
         MVC   SCOCVER,SPACES      CAST VERIFICATION DATE                       
         MVC   SCOCVEN,SPACES      CAST VERIFICATION NAME                       
                                                                                
         CLI   ACTNUM,ACTVER       IF ACTION IS VERIFY                          
         JNE   IS10                HIDE ALL PF KEYS                             
         GOTO1 FLDVAL,DMCB,SCOPF1H,(8,SCOPF2H)                                  
         J     XIT                                                              
                                                                                
IS10     GOTO1 FLDVAL,DMCB,SCOPF1H,(X'20',SCOPF2H)                              
         OI    SCOXCOMH+1,X'0C'                                                 
         OI    SCOCHHFH+1,X'0C'                                                 
         OI    SCOWIDH+1,X'20'                                                  
                                                                                
         OC    ATALFEL,ATALFEL     IF COMMERCIAL HAS LIFT                       
         JZ    *+12                HIDE VERSION PFKEY                           
         OI    SCOPFVH+1,X'0C'                                                  
         J     XIT                                                              
                                                                                
         CLI   TGVER,0             IF COMMERCIAL DOES NOT HAVE                  
         JNE   XIT                 VERSIONS, MAKE VERSION PFKEY                 
         NI    SCOPFVH+1,X'F3'     NORMAL INTENSITY                             
         J     XIT                                                              
                                                                                
***********************************************************************         
                                                                                
IS20     GOTO1 FLDVAL,DMCB,SC2ANNOH,(8,SC2CLFTH)                                
         GOTO1 FLDVAL,DMCB,(8,SC2ALLOH),(8,SC2RUSEH)                            
         GOTO1 FLDVAL,DMCB,(1,SC2TAGYH),SC2TTIXH                                
                                                                                
         MVC   SC2MNM1,SPACES      CLEAR MUSIC CODE 1                           
         MVC   SC2MNM2,SPACES      MUSIC CODE 2                                 
         MVC   SC2MNM3,SPACES      MUSIC CODE 3                                 
         MVC   SC2MNM4,SPACES      MUSIC CODE 4                                 
                                                                                
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         TM    TACOSTA2,TACOSANO   IF ANNOUNCER IS ONLY NON-MUSICIAN            
         JZ    IS30                                                             
         CLI   TACOMED,TACOMEDR    AND MEDIA IS NOT RADIO                       
         JNE   IS30                                                             
         NI    SC2ANNOH+1,X'FB'    MAKE "ANNOUNCER ONLY NON-MUSICIAN"           
         OI    SC2ANNOH+6,X'80'    FIELD HIGH INTENSITY                         
                                                                                
IS30     TM    TACOSTA2,TACOSLFT   IF CAST MEMBERS ON LIFT                      
         JZ    IS40                                                             
         NI    SC2CLFTH+1,X'FB'    MAKE "CAST MEMBERS ON LIFT" FIELD            
         OI    SC2CLFTH+6,X'80'    HIGH INTENSITY                               
                                                                                
IS40     CLI   TACOTYPE,CTYMUS     IF COMMERCIAL TYPE IS MUSIC                  
         JNE   IS50                                                             
         NI    SC2ALLOH+1,X'F3'    MAKE ALLOWABLE USES                          
         NI    SC2AUSEH+1,X'D3'    AND REMAINING FREE USES FIELDS               
         NI    SC2REMAH+1,X'F3'    NORMAL INTENSITY                             
         NI    SC2RUSEH+1,X'D3'                                                 
                                                                                
         USING TRACKD,RE                                                        
IS50     LA    RE,SC2TAGYH         RE=A(1ST MUSIC TRACK FIELD)                  
         LA    RF,SC2TTIXH         RF=A(LAST MUSIC TRACK FIELD)                 
                                                                                
IS60     CLI   TACOTYPE,CTYMUS     IF COMMERCIAL TYPE IS MUSIC                  
         JNE   IS70                                                             
         OI    TAGYH+1,X'20'       PROTECT AGENCY                               
         OI    TCNTH+1,X'20'       AND CONTRACT ID FIELDS                       
         NI    TLFTH+1,X'DF'       UNPROTECT ALL OTHER FIELDS                   
         NI    TLENH+1,X'DF'                                                    
         NI    TTYPH+1,X'DF'                                                    
         NI    TTITH+1,X'DF'                                                    
         J     IS80                                                             
                                                                                
IS70     NI    TAGYH+1,X'DF'       IF COMMERCIAL TYPE IS NOT MUSIC              
         NI    TCNTH+1,X'DF'       UNPROTECT AGENCY AND CONTRACT ID             
         OI    TLFTH+1,X'20'       PROTECT ALL OTHER FIELDS                     
         OI    TLENH+1,X'20'                                                    
         OI    TTYPH+1,X'20'                                                    
         OI    TTITH+1,X'20'                                                    
                                                                                
IS80     LA    RE,TLNQ(RE)         BUMP TO NEXT MUSIC ENTRY                     
         CR    RE,RF                                                            
         JL    IS60                                                             
         DROP  R4,RE                                                            
                                                                                
         CLI   ACTNUM,ACTVER       IF ACTION IS VERIFY                          
         JNE   IS90                HIDE ALL PF KEYS                             
         GOTO1 FLDVAL,DMCB,SC2PF1H,(8,SC2PF2H)                                  
         J     XIT                                                              
                                                                                
IS90     GOTO1 FLDVAL,DMCB,SC2PF1H,(X'20',SC2PF2H)                              
                                                                                
         OC    ATALFEL,ATALFEL     IF COMMERCIAL HAS LIFT                       
         JZ    *+12                HIDE VERSION PFKEY                           
         OI    SC2PFVH+1,X'0C'                                                  
         J     XIT                                                              
                                                                                
         CLI   TGVER,0             IF COMMERCIAL DOES NOT HAVE                  
         JNE   XIT                 VERSIONS, MAKE VERSION PFKEY                 
         NI    SC2PFVH+1,X'F3'     NORMAL INTENSITY                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO READ VERSION RECORD                               *         
*        ON ENTRY ... R4=A(I/O AREA)                                  *         
***********************************************************************         
                                                                                
GETVER   NTR1                                                                   
         CLI   TGVER,2             IF DISPLAYING VERSION 2 OR HIGHER            
         JL    XIT                 READ VERSION RECORD                          
         GOTO1 RECVAL,DMCB,TLVRCDQ,(X'24',0)                                    
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL            SAVE A(VERSION DETAILS ELEMENT)              
         JE    *+6                                                              
         DC    H'00'                                                            
         ST    R4,ATACOEL                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY PREVIOUS ID                               *         
*        ON ENTRY ... R4 = A(PRIMARY COMMERCIAL RECORD)               *         
***********************************************************************         
                                                                                
DISPID   NTR1                                                                   
         USING TAOCD,R4                                                         
         MVI   ELCODE,TAOCELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         CLC   TAOCCID,SPACES                                                   
         JNH   XIT                                                              
         MVC   SCOPID(L'TAOCAGY),TAOCAGY                                        
         MVC   SCOPID+7(L'TAOCCID),TAOCCID                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY VERSION ID                                *         
***********************************************************************         
                                                                                
DISVID   NTR1                                                                   
         CLI   TGVER,0                                                          
         JE    XIT                                                              
                                                                                
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         MVC   SCOVRI,TACOCID                                                   
         MVI   SCOVRIH+5,L'SCOVRI                                               
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY CLIENT VALUES                             *         
*        ON ENTRY ... R4 = A(PRIMARY COMMERCIAL RECORD)               *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
DISCLI   NTR1                                                                   
         MVC   SCOCLI,TLCOCLI                                                   
         MVI   SCOCLIH+5,L'SCOCLI                                               
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'0C',SCOCLIH),SCOCLINH                     
         MVC   AIO,AIO1                                                         
                                                                                
         USING TACOD,R4                                                         
         L     R4,ATACOEL         IF CLIENT BELONGS TO CLIENT GROUP             
         MVC   SCOCGRP,TACOCLG    DISPLAY CLIENT GROUP CODE                     
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY PRODUCT VALUES                            *         
*        ON ENTRY ... R4 = A(PRIMARY COMMERCIAL RECORD)               *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
DISPRD   NTR1                                                                   
         OC    TLCOPRD,TLCOPRD     IF PRODUCT CODE PRESENT                      
         JZ    DPRD20              DISPLAY PRODUCT CODE AND NAME                
         MVC   SCOPRD,TLCOPRD                                                   
         MVI   SCOPRDH+5,L'SCOPRD                                               
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'0C',SCOPRDH),SCOPRDNH                     
         DROP  R4                                                               
                                                                                
         USING TAPID,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAPIELQ      IF PRODUCT HAS A TYPE                        
         BRAS  RE,GETEL            DISPLAY PRODUCT TYPE CODE AND NAME           
         JNE   DPRD10                                                           
         CLI   TAPILEN,TAPILNQ2                                                 
         JNE   DPRD10                                                           
         OC    TAPIPTYP,TAPIPTYP                                                
         JZ    DPRD10                                                           
         MVC   SCOPTYP,TAPIPTYP                                                 
         MVI   SCOPTYPH+5,L'SCOPTYP                                             
         GOTO1 RECVAL,DMCB,TLPTCDQ,(X'0C',SCOPTYPH),SCOPTYNH                    
         JE    DPRD10                                                           
         MVC   SCOPTYP,SPACES                                                   
         MVI   SCOPTYPH+5,0                                                     
DPRD10   MVC   AIO,AIO1                                                         
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
DPRD20   GOTO1 CHAROUT,DMCB,TAFNELQ,SCOPRDNH,TAFNTPRD                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY ADDENDUM STATE VALUES                     *         
***********************************************************************         
                                                                                
DISADS   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         OC    TACOADST,TACOADST                                                
         JZ    XIT                                                              
         MVC   WORK(2),TACOADST                                                 
         MVI   WORK+2,X'40'                                                     
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SCOADST,TACOADST                                                 
         MVI   SCOADSTH+5,L'SCOADST                                             
         MVC   SCOADSN,TGTANAME                                                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY FIRST FIXED CYCLE DATE                    *         
***********************************************************************         
                                                                                
DISFFC   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         GOTO1 DISDAT,DMCB,TACOFCYC,SCOFFCH                                     
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY EXPIRATION DATE                           *         
***********************************************************************         
                                                                                
DISEXP   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         GOTO1 DISDAT,DMCB,TACOEXP,SCOEXPH                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY MEDIA VALUES                              *         
***********************************************************************         
                                                                                
DISMED   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         GOTO1 MEDVAL,DMCB,TACOMED                                              
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SCOMED,TACOMED                                                   
         MVI   SCOMEDH+5,L'SCOMED                                               
         MVC   SCOMEDN,TGMENAME                                                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY LIFT VALUES                               *         
***********************************************************************         
                                                                                
DISLFT   NTR1                                                                   
         OC    ATALFEL,ATALFEL                                                  
         JZ    DLFT10                                                           
                                                                                
         USING TALFD,R4                                                         
         L     R4,ATALFEL                                                       
         MVC   SCOLCID,TALFLID                                                  
         MVI   SCOLCIDH+5,L'SCOLCID                                             
         EDIT  (B1,TALFSEC),(3,SCOLLEN),ALIGN=LEFT                              
         STC   R0,SCOLLENH+5                                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         USING TAVRD,R4                                                         
DLFT10   CLI   TGVER,1                                                          
         JNE   XIT                                                              
         L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DLFT20   BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         CLI   TAVRVERS,2                                                       
         JNE   DLFT20                                                           
         MVC   SCOLCID,TAVRCID                                                  
         EDIT  (B1,TAVRSEC),(3,SCOLLEN),ALIGN=LEFT                              
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY SECOND SEASON FIRST AIR DATE              *         
***********************************************************************         
                                                                                
DISSAR   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         GOTO1 DISDAT,DMCB,TACOSAIR,SCOSAIRH                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY ACTRA TYPE                                *         
***********************************************************************         
                                                                                
DISACT   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         CLI   TACOCTYP,0                                                       
         JE    XIT                                                              
         GOTO1 CCTYPVAL,DMCB,(X'80',TACOCTYP)                                   
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SCOCTYP,TGCCTCDE                                                 
         MVI   SCOCTYPH+5,L'SCOCTYP                                             
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY STATUS                                    *         
***********************************************************************         
                                                                                
DISSTA   NTR1                                                                   
         XR    R0,R0               R0=L'STATUS FIELD                            
         LA    R2,SCOSTAT          R2=A(STATUS FIELD)                           
         L     R3,ASTATAB          R3=A(STATUS TABLE)                           
                                                                                
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
                                                                                
         TM    TACOSTA2,TACOPCYC                                                
         JZ    DSTA10                                                           
         GOTO1 DISKYW,DMCB,STAPCYC                                              
                                                                                
DSTA10   TM    TACOSTA2,TACOSNCS   DON'T CHARGE CSF ON THIS COMMERCIAL?         
         JZ    DSTA20                                                           
         GOTO1 DISKYW,DMCB,STANCSF                                              
                                                                                
DSTA20   CLI   TACOLEN,TACOLNQ2                                                 
         BL    DSTA30                                                           
                                                                                
         TM    TACOSTA3,TACOSNHF   DO NOT GENERATE HOLDING FEE NOTICES          
         JZ    DSTA25                                                           
         GOTO1 DISKYW,DMCB,STANHFN                                              
                                                                                
DSTA25   TM    TACOSTA3,TACOSSMW   SOCIAL MEDIA WAVER                           
         JZ    DSTA30                                                           
         GOTO1 DISKYW,DMCB,STASSMW                                              
                                                                                
DSTA30   CLI   TACOSTAT,0                                                       
         JE    DSTA60                                                           
                                                                                
DSTA40   CLI   0(R3),X'FF'                                                      
         JE    DSTA60                                                           
         MVC   BYTE,0(R3)                                                       
         NC    BYTE,TACOSTAT                                                    
         JZ    DSTA50                                                           
         GOTO1 DISKYW,DMCB,1(R3)                                                
                                                                                
DSTA50   LA    R3,L'STATAB(R3)                                                  
         J     DSTA40                                                           
         DROP  R4                                                               
                                                                                
DSTA60   LTR   R0,R0                                                            
         JZ    XIT                                                              
         STC   R0,SCOSTATH+5                                                    
         LA    R2,SCOSTAT                                                       
         AR    R2,R0                                                            
         MVI   0(R2),C' '                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY STATUS KEYWORD                            *         
*        ON ENTRY ... P1 = A(STATUS KEYWORD LITERAL)                  *         
*                     R2 = A(STATUS FIELD)                            *         
***********************************************************************         
                                                                                
DISKYW   NTR1                                                                   
         L     R1,0(R1)                                                         
         AR    R2,R0                                                            
                                                                                
         LTR   R0,R0                                                            
         JZ    DKYW10                                                           
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         AHI   R0,1                                                             
                                                                                
DKYW10   MVC   0(10,R2),0(R1)                                                   
                                                                                
DKYW20   CLI   0(R2),C' '                                                       
         JE    DKYWX                                                            
         LA    R2,1(R2)                                                         
         AHI   R0,1                                                             
         J     DKYW20                                                           
                                                                                
DKYWX    XIT1  REGS=(R0)                                                        
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY ACTRA EXPIRATION DATE                     *         
***********************************************************************         
                                                                                
DISAED   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         CLI   TACOCTYP,0                                                       
         JE    XIT                                                              
         OC    TACOAEXP,TACOAEXP                                                
         JZ    XIT                                                              
         GOTO1 DISDAT,DMCB,TACOAEXP,SCOAEXPH                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY COMMERCIAL POOL VALUES                    *         
***********************************************************************         
                                                                                
DISPOL   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         OC    TACOCGRP,TACOCGRP                                                
         JZ    XIT                                                              
         MVC   SCOCOGR,TACOCGRP                                                 
         MVI   SCOCOGRH+5,L'SCOCOGR                                             
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLOGCDQ,(X'0C',SCOCOGRH),SCOCOGNH                    
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY SPLIT CYCLE INDICATOR                     *         
***********************************************************************         
                                                                                
DISSCY   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         MVC   SCOSPCY,TACOSPCY                                                 
         MVI   SCOSPCYH+5,L'SCOSPCY                                             
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY BILL TO VALUES                            *         
***********************************************************************         
                                                                                
DISBTC   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         MVC   SCOBTC,TACOATT                                                   
         MVI   SCOBTCH+5,L'TACOATT                                              
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLATCDQ,(X'0C',SCOBTCH),SCOBTCNH                     
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY CONTRACT TYPE                             *         
***********************************************************************         
                                                                                
DISCON   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         MVC   SCOCONT,TACOCONT                                                 
         MVI   SCOCONTH+5,L'TACOCONT                                            
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY CONTRACT SERVICE FEE VALUES               *         
***********************************************************************         
                                                                                
DISCSF   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         TM    TACOSTA2,TACOSJPC   IF JPC CONTRACT SERVICE FEE HAS              
         JZ    XIT                 BEEN PAID ...                                
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO3                                                         
                                                                                
         USING TLINPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY             READ ALL INVOICES FOR THIS                   
         MVI   TLINPCD,TLINHCDQ    COMMERCIAL                                   
         MVC   TLINHCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         J     DCSF20                                                           
DCSF10   GOTO1 SEQ                                                              
DCSF20   CLC   KEY(TLINHINV-TLINPD),KEYSAVE                                     
         JNE   DCSF50                                                           
         CLI   TLINHSEQ,128        IF NO INVOICE, CSF STATUS IS WRONG           
         BNH   DCSF10              SKIP HISTORY COMMENTS                        
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TABDD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   DCSF10                                                           
         OC    TABDCSF,TABDCSF     SKIP IF CONTRACT SERVICE FEE                 
         JZ    DCSF10              WAS NOT CHARGED                              
         DROP  R4                                                               
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL           SKIP IF CANCELLED OR CANCELLER                
         JE    *+6                INVOICE                                       
         DC    H'00'                                                            
         TM    TAINSTAT,TAINSCIN+TAINSCAN                                       
         JNZ   DCSF10                                                           
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAPDSTA2,TAPDSSUB  SKIP SUBSIDIARY INVOICES                      
         JO    DCSF10                                                           
         DROP  R4                                                               
                                                                                
         USING TLIND,R4                                                         
         L     R4,AIO3                                                          
         MVC   SCOCSF(4),=C'CSF-'                                               
         MVC   SCOCSF+4(L'TLINAGY),TLINAGY                                      
                                                                                
         LA    R2,SCOCSF+10                                                     
DCSF30   CLI   0(R2),C' '                                                       
         JH    DCSF40                                                           
         AHI   R2,-1                                                            
         J     DCSF30                                                           
DCSF40   LA    R2,1(R2)                                                         
         MVI   0(R2),C'/'                                                       
                                                                                
         XC    TLININV,=6X'FF'                                                  
         GOTO1 TINVCON,DMCB,TLININV,1(R2),DATCON                                
         DROP  R4                                                               
                                                                                
DCSF50   MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY INTERNAL COMMERICAL NUMBER                *         
***********************************************************************         
                                                                                
DISCOM   NTR1                                                                   
         CLI   TGCTSTTY,TASTTYPP                                                
         JNE   XIT                                                              
         MVI   SCOINTN,C'('                                                     
         GOTO1 HEXOUT,DMCB,TGCOM,SCOINTN+1,L'TGCOM,0                            
         MVI   SCOINTN+9,C')'                                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY REISSUE PENDING INDICATOR                 *         
***********************************************************************         
                                                                                
DISRIP   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         TM    TACOSTA2,TACOCHHF                                                
         JZ    XIT                                                              
         OI    SCOCHHFH+1,X'08'                                                 
         NI    SCOCHHFH+1,X'FB'                                                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY VERIFICATION VALUES                       *         
***********************************************************************         
                                                                                
DISVER   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         OC    TACOVDTE,TACOVDTE                                                
         JZ    XIT                                                              
         GOTO1 DISDAT,DMCB,TACOVDTE,SCOCVERH                                    
         MVC   SCOCVEN,TACOVST                                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY COMMERCIAL TYPE VALUES                    *         
***********************************************************************         
                                                                                
DISCTY   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         CLI   TACOTYPE,0                                                       
         JE    XIT                                                              
         CLI   TACOTYPE,C' '                                                    
         JE    XIT                                                              
         GOTO1 CTYPVAL,DMCB,TACOTYPE                                            
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SCOTYPE,TACOTYPE                                                 
         MVI   SCOTYPEH+5,L'SCOTYPE                                             
         MVC   SCOTNAM,TGCTNAME                                                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY FIRST AIR DATE                            *         
***********************************************************************         
                                                                                
DISFAR   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         GOTO1 DISDAT,DMCB,TACOAIR,SCOFAIRH                                     
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY LENGTH                                    *         
***********************************************************************         
                                                                                
DISLEN   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         EDIT  (B1,TACOSEC),(3,SCOLEN),ALIGN=LEFT                               
         STC   R0,SCOLENH+5                                                     
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY STATUS FOR VERSION                        *         
***********************************************************************         
                                                                                
DISSTAV  NTR1                                                                   
         LA    R2,SCOSTATH                                                      
         ZIC   R0,5(R2)            R0=L'STATUS FIELD                            
         LA    R2,SCOSTAT          R2=A(STATUS FIELD)                           
                                                                                
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
                                                                                
         TM    TACOSTA3,TACOS26K                                                
         JZ    XIT                                                              
         GOTO1 DISKYW,DMCB,STA26K                                               
         DROP  R4                                                               
                                                                                
         LTR   R0,R0                                                            
         JZ    XIT                                                              
         STC   R0,SCOSTATH+5                                                    
         LA    R2,SCOSTAT                                                       
         AR    R2,R0                                                            
         MVI   0(R2),C' '                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY FILM, RECORDING OR MUSIC VALUES           *         
*        ON ENTRY ... P1 BYTE 0 = FILM,RECORD OR MUSIC EQUATE         *         
*                     P1        = A(DATE FIELD)                       *         
***********************************************************************         
                                                                                
DISFRM   NTR1                                                                   
         MVC   BYTE,0(R1)                                                       
         ZICM  R2,1(R1),3                                                       
                                                                                
         MVI   ELCODE,TACSELQ                                                   
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         JNE   XIT                                                              
                                                                                
         USING TACSD,R4                                                         
         L     R4,TGELEM                                                        
         GOTO1 DISDAT,DMCB,TACSDATE,0(R2)                                       
                                                                                
         GOTOR BUMP,DMCB,2                                                      
         MVC   8(L'TACSSTUD,R2),TACSSTUD                                        
         MVI   5(R2),L'TACSSTUD                                                 
                                                                                
         GOTOR BUMP,DMCB,2                                                      
         MVC   8(L'TACSCITY,R2),TACSCITY                                        
         MVI   5(R2),L'TACSCITY                                                 
                                                                                
         CLI   TACSLEN,TACSLNQ                                                  
         JL    XIT                                                              
         GOTOR BUMP,DMCB,2                                                      
         MVC   8(L'TACSSTAT,R2),TACSSTAT                                        
         MVI   5(R2),L'TACSSTAT                                                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY INTERNET/NEW MEDIA VALUES                 *         
*        ON ENTRY ... R4 = A(COMMERCIAL OR VERSION RECORD)            *         
***********************************************************************         
                                                                                
DISINM   NTR1                                                                   
         LA    R2,SCOMED1H         R2=A(FIRST INTERNET/NEW MEDIA FIELD)         
                                                                                
         USING TAMDD,R4                                                         
         MVI   ELCODE,TAMDELQ      READ ALL MEDIA (INTERNET/                    
         BRAS  RE,GETEL            NEW MEDIA) ELEMENTS                          
         J     *+8                                                              
DINM10   BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         MVC   8(L'SCOMED1,R2),TAMDCODE                                         
         MVI   5(R2),L'SCOMED1                                                  
         DROP  R4                                                               
                                                                                
         GOTOR BUMP,DMCB,1         BUMP TO NEXT MEDIA FIELD                     
         J     DINM10                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY EDIT YEAR AND TYPE                        *         
***********************************************************************         
                                                                                
DISETY   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         OC    TACOEDT,TACOEDT                                                  
         JZ    XIT                                                              
         MVC   SCOETYP,TACOEDT                                                  
         MVI   SCOETYPH+5,L'SCOETYP                                             
                                                                                
         XC    FULL,FULL                                                        
         MVC   FULL(1),TACOEDYR                                                 
         XI    FULL,X'FF'                                                       
         MVC   FULL+1(2),=X'0101'                                               
         GOTO1 DATCON,DMCB,(1,FULL),(20,SCOEDYR)                                
         MVI   SCOEDYRH+5,L'SCOEDYR                                             
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY ACTIVE AND INACTIVE DATES                 *         
***********************************************************************         
                                                                                
DISAID   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         OC    TACOACT,TACOACT                                                  
         JZ    DISAID10                                                         
         GOTO1 DISDAT,DMCB,TACOACT,SCOACTH                                      
DISAID10 OC    TACOINAC,TACOINAC                                                
         JZ    XIT                                                              
         GOTO1 (RF),(R1),TACOINAC,SCOINACH                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY ATTACHED COMMENT INDICATOR                *         
***********************************************************************         
                                                                                
DISACM   NTR1                                                                   
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLCMCDQ,(X'A4',TLCMTCOM)                             
         JNE   *+12                                                             
         OI    SCOXCOMH+1,X'08'                                                 
         NI    SCOXCOMH+1,X'FB'                                                 
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY WEB APPLICATION ID                        *         
***********************************************************************         
                                                                                
DISWID   NTR1                                                                   
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTOWB))                                     
         JNE   DWID10                                                           
         L     R4,TGELEM                                                        
         MVC   SCOWID,TAFNNAME                                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
DWID10   MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   XIT                                                              
         L     R4,TGELEM                                                        
         MVC   SCOWID,TAFNNAME                                                  
         NI    SCOWIDH+1,X'DF'                                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY AFM RATE                                  *         
***********************************************************************         
                                                                                
DISAFM   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         CLI   TACOAFM,0                                                        
         JE    XIT                                                              
         MVC   SC2AFM,TACOAFM                                                   
         MVI   SC2AFMH+5,L'SC2AFM                                               
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY SESSION TYPE                              *         
***********************************************************************         
                                                                                
DISSTY   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         CLI   TACOSES,0                                                        
         JE    XIT                                                              
         MVC   SC2STYP,TACOSES                                                  
         MVI   SC2STYPH+5,L'SC2STYP                                             
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY FILTERS                                   *         
*        ON ENTRY ... R4 = A(COMMERCIAL OR VERSION RECORD)            *         
***********************************************************************         
                                                                                
DISFLT   NTR1                                                                   
         USING TAFLD,R4                                                         
         MVI   ELCODE,TAFLELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   SC2FILT,TAFLFLT1                                                 
         MVI   SC2FILTH+5,L'SC2FILT                                             
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY MUSIC CONTRACT TRACK VALUES               *         
*        ON ENTRY ... R4 = A(COMMERCIAL OR VERSION RECORD)            *         
***********************************************************************         
                                                                                
DISTRK   NTR1                                                                   
         USING TACOD,R3                                                         
         L     R3,ATACOEL                                                       
                                                                                
         USING TAMCD,R4                                                         
         MVI   ELCODE,TAMCELQ      GET MUSIC CONTRACT DETAILS ELEMENT           
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DTRK10   BRAS  RE,NEXTEL                                                        
         JNE   DTRK50                                                           
                                                                                
         USING TRACKD,R2                                                        
         LA    R2,SC2TAGYH                                                      
         ZIC   RE,TAMCSEQ          BUMP TO TRACK FIELDS FOR ELEMENT'S           
         MHI   RE,TLNQ             SEQUENCE NUMBER                              
         AR    R2,RE                                                            
                                                                                
         CLI   TACOTYPE,CTYMUS     IF COMMERCIAL TYPE IS MUSIC                  
         JNE   DTRK30              AGENCY MATCHES COMMERCIAL AGENCY             
         MVC   TAGY,SC2AGY                                                      
         MVC   TAGYH+5(1),SC2AGYH+5                                             
         DROP  R3                                                               
                                                                                
         TM    TAMCSTAT,TAMCSNEW   IF CONFORMING TO NEW RULES                   
         JZ    DTRK20              CONTRACT ID MATCHES COMMERCIAL ID            
         MVC   TCNT,SC2CID                                                      
         MVC   TCNTH+5(1),SC2CIDH+5                                             
         J     DTRK40                                                           
DTRK20   MVC   TCNT,TAMCCON        OTHERWISE, DISPLAY ENTERED VALUE             
         NI    TCNTH+1,X'DF'       AND UNPROTECT FIELD                          
         MVI   TCNTH+5,L'TAMCCON                                                
         J     DTRK40                                                           
                                                                                
DTRK30   MVC   TAGY,SPACES         IF COMMERCIAL IS NOT MUSIC                   
         MVI   TAGYH+5,0           DISPLAY BLANKS FOR AGENCY                    
         MVC   TCNT,TAMCCON        DISPLAY ENTERED CONTRACT ID                  
         MVI   TCNTH+5,L'TAMCCON   UNPROTECT REST OF FIELDS                     
         GOTO1 FLDVAL,DMCB,(4,TLFTH),TTITH                                      
                                                                                
DTRK40   MVC   TTRK,TAMCTRK                                                     
         MVI   TTRKH+5,L'TAMCTRK                                                
         BAS   RE,DISTAMC                                                       
         J     DTRK10                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
DTRK50   MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
                                                                                
         USING TATRD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TATRELQ      GET MUSIC CONTRACT DETAILS ELEMENT           
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DTRK60   BRAS  RE,NEXTEL                                                        
         JNE   DTRK80                                                           
         LR    R0,R4                                                            
                                                                                
         USING TRACKD,R2                                                        
         LA    R2,SC2TAGYH                                                      
         ZIC   RE,TATRSEQ          BUMP TO TRACK FIELDS FOR ELEMENT'S           
         MHI   RE,TLNQ             SEQUENCE NUMBER                              
         AR    R2,RE                                                            
                                                                                
         MVC   TTRK,TATRTRK        DISPLAY TRACK LETTER                         
         MVI   TTRKH+5,L'TATRTRK                                                
                                                                                
         USING TLCOPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,TATRCOM    READ AFM CONTRACT RECORD                     
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         MVC   TAGY,TLCOAGY        DISPLAY AFM CONTRACT AGENCY                  
         MVI   TAGYH+5,L'TLCOAGY                                                
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TCNT,TACOCID        DISPLAY AFM CONTRACT ID                      
         MVI   TCNTH+5,L'TACOCID                                                
         DROP  R4                                                               
                                                                                
         USING TAMCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAMCELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DTRK70   BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAMCTRK,TTRK                                                     
         JNE   DTRK70                                                           
         BAS   RE,DISTAMC          DISPLAY TRACK INFORMATION                    
         DROP  R2,R4                                                            
                                                                                
         MVI   ELCODE,TATRELQ                                                   
         LR    R4,R0                                                            
         J     DTRK60                                                           
                                                                                
DTRK80   MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY MUSIC CONTRACT TRACK'S LIFT, LENGTH,      *         
*        TYPE AND TITLE                                                         
*        ON ENTRY ... R2 = A(TRACK SCREEN LINE)                       *         
*                     R4 = A(COMMERCIAL OR VERSION RECORD)            *         
***********************************************************************         
                                                                                
         USING TRACKD,R2                                                        
         USING TAMCD,R4                                                         
DISTAMC  NTR1                                                                   
         MVC   TLFT,TAMCLFT        DISPLAY TRACK'S LIFT SETTING                 
         MVI   TLFTH+5,L'TAMCLFT                                                
                                                                                
         EDIT  TAMCLLEN,TLEN,ALIGN=LEFT                                         
         STC   R0,TLENH+5          DISPLAY TRACK LENGTH                         
                                                                                
         MVC   TTYP,TAMCTYP        DISPLAY TRACK MUSIC TYPE                     
         MVI   TTYPH+5,L'TAMCTYP                                                
                                                                                
         CLI   TAMCLEN,TAMCLNQ                                                  
         JE    XIT                                                              
         ZIC   RE,TAMCLEN          DISPLAY TRACK TITLE                          
         SHI   RE,TAMCLNQ                                                       
         STC   RE,TTITH+5                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   TTIT(0),TAMCTRKT                                                 
         J     XIT                                                              
         DROP  R2,R4                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY MUSIC VALUES                              *         
*        ON ENTRY ... R4 = A(COMMERCIAL OR VERSION RECORD)            *         
***********************************************************************         
                                                                                
DISMCO   NTR1                                                                   
         LA    R2,SC2MUS1H                                                      
                                                                                
         USING TACPD,R4                                                         
         MVI   ELCODE,TACPELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DMCO10   BRAS  RE,NEXTEL                                                        
         JNE   DMCO20                                                           
         MVC   8(L'TACPMUS,R2),TACPMUS                                          
         MVI   5(R2),L'TACPMUS                                                  
         GOTOR BUMP,DMCB,3                                                      
         J     DMCO10                                                           
         DROP  R4                                                               
                                                                                
DMCO20   CLI   SC2MUS1H+5,0                                                     
         JE    XIT                                                              
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLMUCDQ,(X'0C',SC2MUS1H),SC2MNM1H                    
                                                                                
         CLI   SC2MUS2H+5,0                                                     
         JE    DMCOX                                                            
         GOTO1 (RF),(R1),TLMUCDQ,(X'0C',SC2MUS2H),SC2MNM2H                      
                                                                                
         CLI   SC2MUS3H+5,0                                                     
         JE    DMCOX                                                            
         GOTO1 (RF),(R1),TLMUCDQ,(X'0C',SC2MUS3H),SC2MNM3H                      
                                                                                
         CLI   SC2MUS4H+5,0                                                     
         JE    DMCOX                                                            
         GOTO1 (RF),(R1),TLMUCDQ,(X'0C',SC2MUS4H),SC2MNM4H                      
                                                                                
DMCOX    MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY ALLOWABLE/REMAINING FREE USES             *         
***********************************************************************         
                                                                                
DISARU   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         OC    TACOAUSE,TACOAUSE                                                
         JZ    DISARU10                                                         
         EDIT  (B1,TACOAUSE),(2,SC2AUSE),ALIGN=LEFT,ZERO=NOBLANK                
         STC   R0,SC2AUSEH+5                                                    
DISARU10 OC    TACORUSE,TACORUSE                                                
         JZ    XIT                                                              
         EDIT  (B1,TACORUSE),(2,SC2RUSE),ALIGN=LEFT,ZERO=NOBLANK                
         STC   R0,SC2RUSEH+5                                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY DUB DATE                                  *         
***********************************************************************         
                                                                                
DISDUB   NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,ATACOEL                                                       
         GOTO1 DISDAT,DMCB,TACODUB,SC2DBDTH                                     
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY DATE                                      *         
*        ON ENTRY ... P1 = A(DATE FIELD IN ELEMENT)                   *         
*                     P2 = A(SCREEN FIELD)                            *         
***********************************************************************         
                                                                                
DISDAT   NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         OC    0(3,R2),0(R2)                                                    
         JZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,0(R2)),(8,8(R3))                                  
         MVI   5(R3),8                                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO SAVE LAST CHANGED DATE AND TIME                   *         
*        ON ENTRY ... R4 = A(COMMERCIAL/VERSION RECORD)               *         
***********************************************************************         
                                                                                
         USING TAACD,R4                                                         
SVLCHG   NTR1                                                                   
         MVI   ELCODE,TAACELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   SVACCDTE,TAACCDTE                                                
         MVC   SVACCTIM,TAACCTIM                                                
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        LITERALS FOR DISPLAY RECORD ROUTINES                         *         
***********************************************************************         
                                                                                
STAPCYC  DC    CL10'PERCYC'                                                     
STANCSF  DC    CL10'NOCSF'                                                      
STANHFN  DC    CL10'NOHFN'                                                      
STA26K   DC    CL10'26K'                                                        
STASSMW  DC    CL10'SMW'                                                        
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE RECORD                               *         
***********************************************************************         
                                                                                
VR       NTR1  BASE=*,LABEL=*                                                   
         TM    AYSTAT3,TAAYSLCK    ENSURE AGENCY IS NOT LOCKED                  
         JO    ERALK                                                            
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
         LA    R3,KEY                                                           
         L     R4,AIO                                                           
         LA    R5,TACOELEM                                                      
                                                                                
         BAS   RE,VRINIT           INITIALIZE RECORD AND VARIABLES              
                                                                                
         BAS   RE,VRWID            VALIDATE WEB APPLICATION ID                  
                                                                                
         BRAS  RE,VRCOMN           VALIDATE COMMON FIELDS                       
         BRAS  RE,VRCOM1           VALIDATE COMMERCIAL SCREEN FIELDS            
         BRAS  RE,VRCOM2           VALIDATE COMMERCIAL2 SCREEN FIELDS           
                                                                                
         BAS   RE,VRUVC            MAY NEED TO UNVERIFY CAST                    
         BAS   RE,VRRHF            MAY NEED TO REISSUE HOLDING FEE              
                                                                                
         BRAS  RE,VRFINSH          ADD/UPDATE COMML/VERSION RECORDS             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO INITIALIZE RECORD AND VARIABLES                   *         
*        ON ENTRY ... R4 = A(I/O AREA)                                *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
VRINIT   NTR1                                                                   
         XC    TACOELEM,TACOELEM                                                
         XC    TRKALIST,TRKALIST   INITIALIZE TRACKS TO ADD LIST                
         XC    TRKDLIST,TRKDLIST   AND TRACKS TO DELETE LIST                    
         NI    PROSTAT,X'FF'-PSCSTADD-PSWEBREM                                  
                                                                                
         BAS   RE,VRINCOA          INITIALIZE FOR COMMERCIAL ADD                
         BAS   RE,VRINCOC          INITIALIZE FOR COMMERCIAL CHANGE             
                                                                                
         BAS   RE,VRINVRA          INITIALIZE FOR VERSION ADD                   
         BAS   RE,VRINVRC          INITIALIZE FOR VERSION CHANGE                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO INITIALIZE COMMERCIAL FOR ADD                     *         
*        ON ENTRY ... R4 = A(I/O AREA)                                *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
         USING TACOD,R5                                                         
VRINCOA  NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF ADDING COMMERCIAL                         
         JNE   XIT                                                              
         CLI   TGVER,1                                                          
         JNE   XIT                                                              
         XC    SVCOKEY,SVCOKEY                                                  
                                                                                
         XC    0(255,R4),0(R4)     INITIALIZE COMMERCIAL RECORD                 
         MVI   TLCOCD,TLCOCDQ      WITH RECORD CODE                             
         MVC   TLCOAGY,TGAGY       AGENCY CODE                                  
         MVC   TLCOCID,TGCID       AND COMMERCIAL ID                            
         MVI   TLCOLEN+1,41                                                     
         DROP  R4                                                               
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         MVI   TACOEL,TACOELQ      INITIALIZE COMMERCIAL DETAILS                
         MVI   TACOLEN,TACOLNQ3    ELEMENT                                      
         MVC   TACOCID,TGCID       WITH COMMERCIAL ID                           
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO INITIALIZE COMMERCIAL FOR CHANGE                  *         
*        ON ENTRY ... R4 = A(I/O AREA)                                *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VRINCOC  NTR1                                                                   
         CLI   TGVER,1             IF VERSION 0 OR 1                            
         JH    XIT                                                              
         CLI   ACTNUM,ACTCHA       AND CHANGING                                 
         JE    *+12                                                             
         CLI   ACTNUM,ACTCOPY      OR COPYING COMMERCIAL                        
         JNE   XIT                                                              
                                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'30',0)                                   
         JE    *+6                                                              
         DC    H'00'               READ PRIMARY COMMERCIAL RECORD               
                                                                                
         MVC   SVCOKEY,0(R4)                                                    
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         MVI   ELCODE,TACOELQ      INITIALIZE COMMERCIAL DETAILS                
         BRAS  RE,GETEL            ELEMENT WITH PRIMARY COMMERCIAL              
         JE    *+6                 EXISTING VALUES                              
         DC    H'00'                                                            
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   TACOELEM(0),0(R4)                                                
         MVC   SVMED,TACOMED                                                    
         MVC   SVPTYPE,TACOTYPE                                                 
         MVI   TACOLEN,TACOLNQ3                                                 
         GOTO1 REMELEM             AND DELETE EXISTING ELEMENT                  
         DROP  R5                                                               
                                                                                
         BAS   RE,SVNXTSEQ         SAVE NEXT CAST SEQUENCE NUMBER               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO INITIALIZE VERSION FOR ADD                        *         
*        ON ENTRY ... R4 = A(I/O AREA)                                *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VRINVRA  NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF ADDING VERSION                            
         JNE   XIT                                                              
         CLI   TGVER,2                                                          
         JL    XIT                                                              
                                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'20',0)                                   
         JE    *+6                                                              
         DC    H'00'               READ PRIMARY COMMERCIAL RECORD               
         MVC   SVCOKEY,0(R4)                                                    
                                                                                
         USING TLCOD,R4                                                         
         MVC   TGCLI,TLCOCLI       SAVE CLIENT                                  
         MVC   TGPRD,TLCOPRD       AND PRODUCT CODES                            
         DROP  R4                                                               
                                                                                
         USING TLVRD,R4                                                         
         XC    TLVRKEY,TLVRKEY     INSERT VERSION KEY OVER PRIMARY              
         MVI   TLVRCD,TLVRCDQ      COMMERCIAL KEY                               
         MVC   TLVRCOM,TGCOM                                                    
         MVC   TLVRVER,TGVER                                                    
         DROP  R4                                                               
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         MVI   ELCODE,TACOELQ      INITIALIZE COMMERCIAL DETAILS                
         BRAS  RE,GETEL            ELEMENT WITH PRIMARY COMMERCIAL              
         JE    *+6                 EXISTING VALUES                              
         DC    H'00'                                                            
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   TACOELEM(0),0(R4)                                                
         MVI   TACOLEN,TACOLNQ3                                                 
         MVC   SVPTYPE,TACOTYPE                                                 
         GOTO1 REMELEM             AND DELETE EXISTING ELEMENT                  
         DROP  R5                                                               
                                                                                
         MVI   ELCODE,TAOCELQ      DELETE OLD AGENCY/CID ELEMENT                
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAVRELQ      DELETE VERSION ELEMENTS                      
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAFNELQ      DELETE FREE FORM NAME ELEMENTS               
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAMCELQ      DELETE MUSIC CONTRACT ELEMENTS               
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TATRELQ      DELETE MUSIC TRACK ELEMENTS                  
         GOTO1 REMELEM                                                          
                                                                                
         USING TAFND,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     ADD AGENCY CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TGAGY                                          
         MVI   TAFNTYPE,TAFNTAGY                                                
         MVC   TAFNNAME(L'TGAGY),TGAGY                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         XC    ELEMENT,ELEMENT     ADD CLIENT CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TGCLI                                          
         MVI   TAFNTYPE,TAFNTCLI                                                
         MVC   TAFNNAME(L'TGCLI),TGCLI                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         OC    TGPRD,TGPRD                                                      
         JZ    XIT                                                              
         XC    ELEMENT,ELEMENT     ADD PRODUCT CODE ELEMENT                     
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TGPRD                                          
         MVI   TAFNTYPE,TAFNTPRD                                                
         MVC   TAFNNAME(L'TGPRD),TGPRD                                          
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO INITIALIZE VERSION FOR CHANGE                     *         
*        ON ENTRY ... R4 = A(I/O AREA)                                *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
VRINVRC  NTR1                                                                   
         CLI   TGVER,2             IF VERSION 2 OR HIGHER                       
         JL    XIT                                                              
         CLI   ACTNUM,ACTCHA       AND CHANGING                                 
         JE    *+12                                                             
         CLI   ACTNUM,ACTCOPY      OR COPYING COMMERCIAL                        
         JNE   XIT                                                              
                                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'20',0)                                   
         JE    *+6                                                              
         DC    H'00'               READ PRIMARY COMMERCIAL RECORD               
         MVC   SVCOKEY,0(R4)                                                    
                                                                                
         USING TLCOD,R4                                                         
         MVC   TGCLI,TLCOCLI       SAVE CLIENT                                  
         MVC   TGPRD,TLCOPRD       AND PRODUCT CODES                            
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVMED,TACOMED       SAVE PRIMARY COMMERCIAL'S MEDIA              
         MVC   SVSTAT,TACOSTAT     STATUS                                       
         MVC   SVPTYPE,TACOTYPE    AND TYPE                                     
         DROP  R4                                                               
                                                                                
         BAS   RE,SVNXTSEQ         SAVE NEXT CAST SEQUENCE NUMBER               
                                                                                
         GOTO1 RECVAL,DMCB,TLVRCDQ,(X'30',0)                                    
         JE    *+6                                                              
         DC    H'00'               READ VERSION RECORD                          
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         USING TACOD,R5                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      INITIALIZE VERSION DETAILS                   
         BRAS  RE,GETEL            ELEMENT WITH VERSION                         
         JE    *+6                 EXISTING VALUES                              
         DC    H'00'                                                            
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   TACOELEM(0),0(R4)                                                
         MVI   TACOLEN,TACOLNQ3                                                 
         MVC   TACOMED,SVMED       INSERT PRIMARY MEDIA                         
         MVC   TACOSTAT,SVSTAT     AND STATUS INTO VERSION                      
         GOTO1 REMELEM             AND DELETE EXISTING ELEMENT                  
         DROP  R5                                                               
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTAGY))                                     
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTCLI))                                     
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTPRD))                                     
                                                                                
         USING TAFND,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     ADD AGENCY CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TGAGY                                          
         MVI   TAFNTYPE,TAFNTAGY                                                
         MVC   TAFNNAME(L'TGAGY),TGAGY                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         XC    ELEMENT,ELEMENT     ADD CLIENT CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TGCLI                                          
         MVI   TAFNTYPE,TAFNTCLI                                                
         MVC   TAFNNAME(L'TGCLI),TGCLI                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         OC    TGPRD,TGPRD                                                      
         JZ    XIT                                                              
         XC    ELEMENT,ELEMENT     ADD PRODUCT CODE ELEMENT                     
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TGPRD                                          
         MVI   TAFNTYPE,TAFNTPRD                                                
         MVC   TAFNNAME(L'TGPRD),TGPRD                                          
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE SAVES COMMERCIAL'S NEXT CAST SEQUENCE NUMBER         *         
*        ON ENTRY ... AIO = A(COMMERCIAL RECORD)                      *         
***********************************************************************         
                                                                                
SVNXTSEQ NTR1                                                                   
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSEQ))                                     
         JE    *+6                                                              
         DC    H'00'                                                            
         L     R4,TGELEM                                                        
         MVC   SVNUNXTC,TANUNXTC   SAVE NEXT CAST SEQ NUMBER                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE FOR SUPER USER PASSWORD IN WEB           *         
*        APPLICATION ID FIELD. IF PRESENT, REMOVE WEB APPLICATION     *         
*        ID FROM CAST RECORD                                          *         
*        ON ENTRY ... R4 = A(COMMERCIAL RECORD)                       *         
***********************************************************************         
                                                                                
VRWID    NTR1                                                                   
         XC    SVWID,SVWID         INITIALIZE WEB APPLICATION ID                
         XC    SVOWID,SVOWID       AND ORIGINAL WEB APPLICATION ID              
                                                                                
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTOWB))                                     
         JNE   VRWID10                                                          
         L     R4,TGELEM                                                        
         MVC   SVOWID,TAFNNAME     SAVE ORIGINAL WEB APPLICATION ID             
         DROP  R4                                                               
                                                                                
VRWID10  GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   XIT                                                              
                                                                                
         USING TAFND,R3                                                         
         L     R3,TGELEM                                                        
                                                                                
         CLI   ACTNUM,ACTCOPY                                                   
         JE    VRWID40                                                          
                                                                                
         CLI   TGVER,2                                                          
         JNE   VRWID20                                                          
         CLC   =C'VS',TAFNNAME                                                  
         JE    VRWID40                                                          
         CLC   =C'TS',TAFNNAME                                                  
         JE    VRWID40                                                          
         CLC   =C'RS',TAFNNAME                                                  
         JE    VRWID40                                                          
                                                                                
VRWID20  CLI   TWASCR,SCR18        IF ON COMMERCIAL SCREEN                      
         JNE   VRWID30                                                          
         OC    SCOWID,SPACES                                                    
         CLC   SCOWID,=CL10'VIDA'  AND PASSWORD IS IN WEB APP FLD               
         JE    VRWID25                                                          
         TM    PRGSTAT,TESTSYS     TST STYSTEM                                  
         JO    VRWID24                                                          
         CLC   TGUSER,=H'7538'     AND USER-IDS TALFQA                          
         JE    VRWID24                                                          
         CLC   TGUSER,=H'7697'     AND CLIFQA                                   
         JNE   VRWID30                                                          
VRWID24  CLC   SCOWID,=CL10'ATIV'  STILL RECOGNIZE OLD PASSWORD                 
         JNE   VRWID30                                                          
VRWID25  OI    PROSTAT,PSWEBREM    SET WEB APPLICATION BEING REMOVED            
                                                                                
         USING TAACD,R4                                                         
VRWID30  MVI   ELCODE,TAACELQ      IF RECORD HAS BEEN UPDATED FROM              
         BRAS  RE,GETEL            WEB APPLICATION SINCE INITIAL                
         JNE   VRWID40                                                          
         CLC   TAACCDTE,SVACCDTE                                                
         JNE   ERCRF                                                            
         CLC   TAACCTIM,SVACCTIM                                                
         JNE   ERCRF                                                            
         DROP  R4                                                               
                                                                                
VRWID40  TM    PROSTAT,PSWEBREM    IF WEB APPLICATION ID IS BEING               
         JZ    VRWID50             REMOVED                                      
         MVI   TAFNTYPE,TAFNTOWB   CHANGE WEB APPLICATION ELEMENT TYPE          
         MVC   SVOWID,TAFNNAME                                                  
         J     XIT                                                              
                                                                                
VRWID50  MVC   SVWID,TAFNNAME      OTHERWISE, SAVE WEB APPLICATION ID           
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO TURN OFF CAST VERIFICATION STATUS                 *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VRUVC    NTR1                                                                   
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         JNE   XIT                                                              
         CLI   TGVER,1             AND VERSION IS 1 OR LOWER                    
         JH    XIT                                                              
         OC    TACOVDTE,TACOVDTE   AND CAST IS VERIFIED                         
         JZ    XIT                                                              
         TM    TACOSTAT,TACOSTRL   AND COMMERCIAL IS NOT RELEASED               
         JO    XIT                                                              
                                                                                
         L     RE,AUVCTAB                                                       
VRUVC10  CLI   0(RE),X'FF'         EXIT IF NONE OF THE FIELDS THAT              
         JE    XIT                 UNVERIFY A COMMERCIAL HAVE CHANGED           
         CLC   TWASCR,0(RE)        CHANGED                                      
         JNE   VRUVC20                                                          
         ZICM  RF,1(RE),2                                                       
         AR    RF,RA                                                            
         TM    4(RF),X'20'                                                      
         JZ    VRUVC30                                                          
VRUVC20  LA    RE,L'UVCTAB(RE)                                                  
         J     VRUVC10                                                          
                                                                                
VRUVC30  XC    TACOVDTE,TACOVDTE                                                
         XC    TACOVTIM,TACOVTIM                                                
         XC    TACOVSTU,TACOVSTU                                                
         XC    TACOVST,TACOVST                                                  
         OI    TACOUVST,TACOUVCO                                                
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO SET HOLDING FEE REISSUE REQUIRED STATUS           *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VRRHF    NTR1                                                                   
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         JNE   XIT                                                              
         CLI   TGVER,1             AND VERSION IS 1 OR LOWER                    
         JH    XIT                                                              
         TM    TACOSTA2,TACOCHHF   AND REISSUE REQUIRED STATUS                  
         JO    XIT                 IS NOT ALREADY ON                            
                                                                                
         L     RE,ARHFTAB                                                       
VRRHF10  CLI   0(RE),X'FF'         EXIT IF NONE OF THE FIELDS THAT              
         JE    XIT                 TRIGGER A NEW HOLDING FEE HAVE               
         CLC   TWASCR,0(RE)        CHANGED                                      
         JNE   VRRHF20                                                          
         ZICM  RF,1(RE),2                                                       
         AR    RF,RA                                                            
         TM    4(RF),X'20'                                                      
         JZ    VRRHF30                                                          
VRRHF20  LA    RE,L'RHFTAB(RE)                                                  
         J     VRRHF10                                                          
                                                                                
         USING TLCAPD,R3                                                        
VRRHF30  XC    KEY,KEY                                                          
         MVI   TLCAPCD,TLCAHCDQ                                                 
         MVC   TLCAHCOM,TGCOM                                                   
         GOTO1 HIGH                (SKIP COMMERCIAL POINTER)                    
VRRHF40  GOTO1 SEQ                                                              
         CLC   KEY(TLCAHSRT-TLCAPD),KEYSAVE                                     
         JNE   XIT                                                              
         OC    TLCAHNXT,TLCAHNXT   IF NO CAST HAS RECEIVED HOLDING              
         JZ    VRRHF40             FEE YET                                      
         CLC   TLCAHDTE,TLCAHNXT   OR LATEST NOTICE WAS PAID                    
         JH    VRRHF40             DO NOT TURN ON STATUS                        
         DROP  R3                                                               
                                                                                
         OI    TACOSTA2,TACOCHHF                                                
                                                                                
         GOTOR SNDMQHFR,DMCB,(PRGSTAT,TGCOM),(TGSYSTA2,HEXOUT),MQIO             
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        LITERALS FOR VALIDATE RECORD ROUTINES                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE FIELDS COMMON TO COMMERCIAL AND          *         
*        COMMERCIAL2 SCREENS                                          *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R4 = A(PRIMARY COMMERCIAL RECORD)               *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
VRCOMN   NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,VALVRI           VALIDATE VERSION ID                          
         BAS   RE,VALCLI           VALIDATE CLIENT FIELD                        
         BAS   RE,VALPRD           VALIDATE PRODUCT FIELD                       
         BAS   RE,VALTIT           VALIDATE TITLE                               
         GOTO1 ACTVIN,DMCB,0                                                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE VERSION ID                               *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALVRI   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,2             OR MAINTAINING VERSION 0 OR 1                
         JL    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOVRIH          R2=A(VERSION ID FIELD)                       
         GOTOR VALWEB,DMCB,(X'60',0)                                            
                                                                                
         GOTO1 ANY                 VERSION ID IS REQUIRED                       
         MVC   TACOCID,WORK                                                     
         MVC   TGCID,WORK                                                       
                                                                                
         USING TLCOPD,R3                                                        
         NI    4(R2),X'DF'                                                      
         GOTO1 RECVAL,DMCB,(X'20',TLCOICDQ),(X'04',(R2))                        
         JNE   XIT                                                              
         CLC   TLCOICOM,TGCOM                                                   
         JNE   EREXI                                                            
         CLC   TLCOIVER,TGVER                                                   
         JNE   EREXI                                                            
         J     XIT                                                              
         DROP  R3,R5                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE CLIENT FIELD                             *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R4 = A(PRIMARY COMMERCIAL RECORD)               *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
         USING TACOD,R5                                                         
VALCLI   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOCLIH          R2=A(CLIENT FIELD                            
         GOTO1 ANY                 ENSURE CLIENT IS PROVIDED                    
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'22',(R2))                                 
         MVC   TLCOCLI,TGCLI                                                    
         MVC   AIO,AIO1            ELSE, ENSURE CLIENT EXISTS                   
         DROP  R4                                                               
                                                                                
         XC    TACOCLG,TACOCLG     INITIALIZE CLIENT GROUP                      
                                                                                
         USING TACID,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACIELQ      GET CLIENT INFORMATION ELEMENT               
         BRAS  RE,GETEL                                                         
         JNE   VCLI10                                                           
         MVC   TACOCLG,TACICLG     SAVE CLIENT GROUP                            
                                                                                
         CLI   ACTNUM,ACTADD       IF ATTEMPTING TO ADD COMMERCIAL              
         JNE   VCLI10                                                           
         CLI   TGVER,1                                                          
         JNE   VCLI10                                                           
         TM    TACISTAT,TACISLCK   ENSURE CLIENT IS NOT LOCKED                  
         JO    ERCLK                                                            
                                                                                
         USING TABRD,R4                                                         
VCLI10   L     R4,AIO3                                                          
         MVI   ELCODE,TABRELQ      GET BILLING RULES ELEMENT                    
         BRAS  RE,GETEL                                                         
         JNE   VCLI20                                                           
         CLC   =C'PP ',TABROEOR    ENSURE EMPLOYER IS NOT PRINT                 
         JE    ERIRC                                                            
                                                                                
         USING TLCLD,R3                                                         
VCLI20   TM    AYBRSTAT,TABRSINT   IF AGY ON INTERFACE                          
         JO    VCLI30                                                           
         TM    AYSTAT6,TAAYST10    OR TYPE 10 JOB VALIDATION                    
         JO    VCLI30                                                           
         TM    AYSTAT5,TAAYNOGL    OR NO GLOBAL ALLOWED                         
         JZ    XIT                                                              
VCLI30   OC    TLCLAGY,TLCLAGY     ENSURE CLIENT IS NOT GLOBAL                  
         JZ    ERGLC                                                            
         J     XIT                                                              
         DROP  R3,R5                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE PRODUCT FIELD                            *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R4 = A(PRIMARY COMMERCIAL RECORD)               *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
VALPRD   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOPRDH          R2=A(PRODUCT FIELD)                          
         XC    TLCOPRD,TLCOPRD     INITIALIZE PRODUCT FIELD                     
         MVI   ELCODE,TAFNELQ      AND DELETE PRODUCT NAME ELEMENT              
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTPRD))                                     
                                                                                
         CLI   5(R2),0             IF PRODUCT IS NOT PROVIDED                   
         JNE   VPRD10                                                           
         TM    AYBRSTAT,TABRSINT   ENSURE AGENCY IS NOT ON INTERFACE            
         JO    ERMIS                                                            
         TM    AYSTAT6,TAAYST10    OR TYPE 10 JOB VALIDATION                    
         JO    ERMIS                                                            
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SCOPRDNH),TAFNTPRD                     
         J     XIT                                                              
                                                                                
VPRD10   MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'22',(R2))                                 
         MVC   TLCOPRD,TGPRD                                                    
         MVC   AIO,AIO1            ELSE, ENSURE PRODUCT EXISTS                  
         DROP  R4                                                               
                                                                                
         USING TAPID,R4                                                         
         CLI   ACTNUM,ACTADD       IF ATTEMPTING TO ADD COMMERCIAL              
         JNE   VPRD20                                                           
         CLI   TGVER,1                                                          
         JNE   VPRD20                                                           
         L     R4,AIO3                                                          
         MVI   ELCODE,TAPIELQ      GET PRODUCT INFORMATION ELEMENT              
         BRAS  RE,GETEL                                                         
         JNE   VPRD20                                                           
         TM    TAPISTAT,TAPISLCK   AND ENSURE IT IS NOT LOCKED                  
         JO    ERPLK                                                            
         DROP  R4                                                               
                                                                                
         USING TLPRD,R3                                                         
VPRD20   TM    AYBRSTAT,TABRSINT   IF AGENCY ON INTERFACE                       
         JO    VPRD30                                                           
         TM    AYSTAT6,TAAYST10    OR TYPE 10 JOB VALIDATION                    
         JO    VPRD30                                                           
         TM    AYSTAT5,TAAYNOGL    OR NO GLOBAL ALLOWED                         
         JZ    XIT                                                              
VPRD30   OC    TLPRAGY,TLPRAGY     ENSURE PRODUCT IS NOT GLOBAL                 
         JZ    ERGLC                                                            
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE TITLE                                    *         
***********************************************************************         
                                                                                
VALTIT   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   ACTNUM,ACTCOPY      OR ACTION IS COPY                            
         JE    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOTITNH         R2=A(COMMERCIAL TITLE FIELD)                 
         GOTOR VALWEB,DMCB,(X'40',0)                                            
                                                                                
         GOTO1 NAMIN,DMCB,TANAELQ,(R2)                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR VRCOMN ROUTINES                                 *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE COMMERCIAL SCREEN FIELDS                 *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R4 = A(PRIMARY COMMERCIAL RECORD)               *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
VRCOM1   NTR1  BASE=*,LABEL=*                                                   
         CLI   TWASCR,SCR18        IF ON COMMERCIAL SCREEN                      
         JNE   XIT                                                              
         BAS   RE,VALCTY           VALIDATE COMMERCIAL TYPE                     
         BAS   RE,VALTIA           VALIDATE TITLE FOR AFM                       
         BAS   RE,VALADS           VALIDATE ADDENDUM STATE                      
         BAS   RE,VALFFC           VALIDATE FIRST FIXED CYCLE                   
         BAS   RE,VALFAR           VALIDATE FIRST AIR DATE                      
         BAS   RE,VALMED           VALIDATE MEDIA                               
         BAS   RE,VALLEN           VALIDATE LENGTH                              
         BAS   RE,VALLFT           VALIDATE LIFT FIELDS                         
         BRAS  RE,VALSTA           VALIDATE STATUS FIELD                        
         BRAS  RE,VALACT           VALIDATE ACTRA TYPE                          
         BAS   RE,VALFIN           VALIDATE FILM DATE/STUDIO/CITY               
         BAS   RE,VALRIN           VALIDATE RECORD DATE/STUDIO/CITY             
         BAS   RE,VALMIN           VALIDATE MUSIC DATE/STUDIO/CITY              
         BAS   RE,VALINM           VALIDATE INTERNET/NEW MEDIA CODES            
         BAS   RE,VALETY           VALIDATE EDIT YEAR/TYPE                      
         BAS   RE,VALAID           VALIDATE ACTIVE/INACTIVE DATES               
         BAS   RE,VALPOL           VALIDATE COMMERCIAL POOL                     
         BAS   RE,VALSCY           VALIDATE SPLIT CYCLE INDICATOR               
         BAS   RE,VALBTC           VALIDATE BILL TO CODE                        
         BRAS  RE,VALCON           VALIDATE CONTRACT TYPE                       
         BAS   RE,VALCMT           VALIDATE COMMENT                             
                                                                                
         BAS   RE,VALEXP           VALIDATE EXPIRATION DATE                     
         BAS   RE,VALSAR           VALIDATE 2ND SEASON 1ST AIR DATE             
         BAS   RE,VALAEX           VALIDATE ACTRA EXPIRATION DATE               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE COMMERCIAL TYPE FIELD                    *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALCTY   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOTYPEH         R2=A(COMMERCIAL TYPE FIELD)                  
         GOTOR VALWEB,DMCB,(X'40',0)                                            
                                                                                
         CLI   TGVER,2             IF MAINTAINING VERSION 2 OR                  
         JL    VCTY20              HIGHER                                       
         CLC   SVPTYPE,8(R2)       ENSURE VERSION TYPE IS COMPATABLE            
         JE    VCTY20              WITH VERSION 1 TYPE                          
         CLI   SVPTYPE,0                                                        
         JE    VCTY10                                                           
         CLI   SVPTYPE,CTYSPAN                                                  
         JNE   ERINV                                                            
VCTY10   CLI   5(R2),0                                                          
         JE    VCTY20                                                           
         CLI   8(R2),CTYSPAN                                                    
         JNE   ERINV                                                            
                                                                                
VCTY20   MVI   TGCTEQU,0                                                        
         CLI   SCOTYPEH+5,0        IF COMMERCIAL TYPE IS PROVIDED               
         JE    VCTY30              VALIDATE COMMERCIAL TYPE                     
         GOTO1 CTYPVAL,DMCB,SCOTYPE                                             
         JNE   ERINV                                                            
                                                                                
         CLC   TACOTYPE,TGCTEQU    IF ADDING COMMERCIAL                         
         JE    VCTY60              OR CHANGING TYPE                             
         CLI   TGCTEQU,CTYSEAS     ENSURE TYPE IS NOT OLD SEASONAL              
         JE    ERINV                                                            
                                                                                
         USING TLCAD,R3                                                         
VCTY30   CLI   ACTNUM,ACTADD       IF ACTION IS NOT ADD                         
         JE    VCTY60                                                           
         CLI   TACOTYPE,CTYMUS     AND CHANGING TYPE FROM MUSIC                 
         JE    VCTY40                                                           
         CLI   TGCTEQU,CTYMUS      OR CHANGING TYPE TO MUSIC                    
         JNE   VCTY60                                                           
VCTY40   L     R4,AIO                                                           
         MVI   ELCODE,TAMCELQ      ENSURE THERE ARE NO ATTACHED                 
         BRAS  RE,GETEL            TRACKS                                       
         JNE   VCTY50                                                           
         CLI   TACOTYPE,CTYMUS                                                  
         JE    ERM480                                                           
         J     ERM485                                                           
VCTY50   L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    ERM485                                                           
         XC    KEY,KEY             ENSURE NO CAST MEMBERS ARE                   
         MVI   TLCACD,TLCACDQ      ATTACHED                                     
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   VCTY60                                                           
         CLI   TACOTYPE,CTYMUS                                                  
         JE    ERM488                                                           
         J     ERM486                                                           
         DROP  R3                                                               
                                                                                
VCTY60   MVC   TACOTYPE,TGCTEQU    SET COMMERCIAL TYPE                          
         DROP  R5                                                               
                                                                                
         CLI   SCOTYPEH+5,0        EXIT IF COMMERCIAL TYPE IS NOT               
         JE    XIT                 PROVIDED                                     
                                                                                
         USING TLCAD,R3                                                         
         CLI   TGVER,1             IF MAINTAINING VERSION 0 OR 1                
         JH    XIT                                                              
         CLI   TGCTEQU,CTYSEAS2    AND TYPE IS NEW SEASONAL                     
         JNE   XIT                                                              
         XC    KEY,KEY             ENSURE NO CAST MEMBERS HAVE AN               
         MVI   TLCACD,TLCACDQ      FFC OR EXPIRATION DATE                       
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         J     VCTY80                                                           
VCTY70   GOTO1 SEQ                                                              
VCTY80   CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   XIT                                                              
         DROP  R3                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VCTY70                                                           
         OC    TACAFCYC,TACAFCYC                                                
         JNZ   ERICS                                                            
         OC    TACAEXP,TACAEXP                                                  
         JNZ   ERICS                                                            
         J     VCTY70                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE TITLE FOR AFM                            *         
***********************************************************************         
                                                                                
VALTIA   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SCOTAFMH),TAFNTMUS                     
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE ADDENDUM STATE FIELD                     *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALADS   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOADSTH         R2=A(ADDENDUM STATE FIELD)                   
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         XC    TACOADST,TACOADST   INITIALIZE ADDENDUM STATE                    
                                                                                
         CLI   SCOADSTH+5,0        EXIT IF ADDENDUM STATE IS NOT                
         JE    XIT                 PROVIDED                                     
                                                                                
         CLI   TACOTYPE,CTYADD     ONLY ALLOW INPUT IF COMMERCIAL TYPE          
         JNE   ERINA               IS ADDENDUM                                  
                                                                                
         MVC   WORK(2),8(R2)       ENSURE ADDENDUM STATE IS VALID               
         MVI   WORK+2,C' '                                                      
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         JNE   ERINV                                                            
         TM    TGTASTAT,TALUOKAD                                                
         JNO   ERINV                                                            
         MVC   TACOADST,8(R2)                                                   
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE FIRST FIXED CYCLE DATE FIELD             *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALFFC   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         TM    TACOSTA2,TACOPCYC   IF PER CYCLE COMMERCIAL                      
         JO    XIT                 CANNOT CHANGE FIRST FIXED CYCLE              
                                                                                
         LA    R2,SCOFFCH          R2=A(FIRST FIXED CYCLE FIELD)                
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         GOTO1 DTVAL,DMCB,TACOFCYC                                              
                                                                                
         CLI   ACTNUM,ACTADD       IF ATTEMPTING TO ADD COMMERCIAL              
         JNE   XIT                                                              
         CLI   PFAID,22            ENSURE FIRST FIXED CYCLE IS NOT              
         JNE   VFFC10              MORE THAN 13 WEEKS FROM TODAY                
         MVI   PFAID,0                                                          
         J     XIT                                                              
                                                                                
         USING PERVALD,R3                                                       
VFFC10   LA    R3,BLOCK                                                         
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(L'TGTODAY8),TGTODAY8                                        
         MVC   WORK+8(6),=C'(-13W)'                                             
         GOTO1 PERVAL,DMCB,(14,WORK),('PVIN1DYL',BLOCK)                         
         CLC   TACOFCYC,PVALPEND                                                
         JNH   INFOR                                                            
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(L'TGTODAY8),TGTODAY8                                        
         MVC   WORK+8(5),=C'(13W)'                                              
         GOTO1 (RF),(R1),(14,WORK),('PVIN1DYL',BLOCK)                           
         CLC   TACOFCYC,PVALPEND                                                
         JNL   INFOR                                                            
         J     XIT                                                              
         DROP  R3,R5                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE FIRST AIR DATE FIELD                     *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALFAR   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
                                                                                
         XC    TACOAIR,TACOAIR     INITIALIZE FIRST AIR DATE                    
                                                                                
         CLI   SCOFAIRH+5,0        EXIT IF FIRST AIR DATE IS NOT                
         JE    XIT                 PROVIDED                                     
                                                                                
         LA    R2,SCOFAIRH         ENSURE FIRST AIR DATE IS VALID               
         GOTO1 DTVAL,DMCB,TACOAIR                                               
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE MEDIA FIELD                              *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALMED   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOMEDH          R2=A(MEDIA FIELD)                            
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         GOTO1 ANY                 INPUT IS REQUIRED                            
                                                                                
         GOTO1 MEDVAL,DMCB,8(R2)   ENSURE MEDIA IS VALID                        
         JNE   ERINV                                                            
         CLI   SCOMED,TACOMEDP     (PRINT IS NOT VALID)                         
         JE    ERINV                                                            
         CLI   SCOMED,TACOMEDE     (EVENT IS NOT VALID)                         
         JE    ERINV                                                            
         MVC   TACOMED,SCOMED                                                   
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE LENGTH FIELD                             *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALLEN   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOLENH          R2=A(LENGTH FIELD)                           
         GOTOR VALWEB,DMCB,(X'60',0)                                            
                                                                                
         MVI   TACOSEC,0           INITIALIZE LENGTH                            
                                                                                
         CLI   5(R2),0             LENGTH IS REQUIRED                           
         JNE   VLEN10                                                           
         CLI   TACOMED,TACOMEDT    IF MEDIA IS TELEVISION                       
         JE    VLEN10                                                           
         CLI   TACOMED,TACOMEDI    INTERNET                                     
         JE    VLEN10                                                           
         CLI   TACOMED,TACOMEDN    OR NEW MEDIA                                 
         JNE   XIT                                                              
                                                                                
VLEN10   GOTO1 VALINUM             ENSURE LENGTH IS VALID                       
         MVC   TACOSEC,ACTUAL                                                   
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE LIFT FIELDS                              *         
***********************************************************************         
                                                                                
VALLFT   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
                                                                                
         MVI   ELCODE,TALFELQ      DELETE EXISTING LIFT ELEMENT                 
         GOTO1 REMELEM                                                          
                                                                                
         CLI   TGVER,0             EXIT IF COMMERCIAL HAS VERSIONS              
         JNE   XIT                                                              
         CLI   SCOLCIDH+5,0        OR LIFT ID IS NOT PROVIDED                   
         JE    XIT                                                              
                                                                                
         LA    R2,SCOLLENH         ENSURE LIFT LENGTH IS PRESENT                
         GOTO1 ANY                 AND VALID                                    
         GOTO1 VALINUM                                                          
                                                                                
         USING TALFD,R4                                                         
         LA    R4,ELEMENT          ADD LIFT ELEMENT TO COMMERCIAL               
         XC    ELEMENT,ELEMENT     RECORD                                       
         MVI   TALFEL,TALFELQ                                                   
         MVI   TALFLEN,TALFLNQ                                                  
         MVC   TALFLID,SCOLCID                                                  
         OC    TALFLID,SPACES                                                   
         MVC   TALFSEC,ACTUAL                                                   
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE FILM DATE/STUDIO/CITY                    *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALFIN   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOFDATH         R2=A(FILM DATE)                              
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         MVI   ELCODE,TACSELQ      DELETE EXISTING FILM STUDIO ELEMENT          
         GOTO1 DELL,DMCB,(1,=AL1(TACSTYPF))                                     
                                                                                
         CLI   TACOMED,TACOMEDR    IF MEDIA IS RADIO                            
         JNE   VFIN10                                                           
         CLI   5(R2),0             ENSURE FILM DATE IS NOT PROVIDED             
         JNE   ERINV                                                            
         J     XIT                                                              
                                                                                
VFIN10   CLI   5(R2),0             IF MEDIA IS NOT RADIO                        
         JNE   VFIN20                                                           
         CLI   SCORDATH+5,0        AND RECORD DATE IS NOT PROVIDED              
         JNE   XIT                                                              
         CLI   TACOTYPE,CTYMUS     AND TYPE IS NOT MUSIC                        
         JNE   ERMIS               FILM DATE IS REQUIRED                        
         J     XIT                                                              
                                                                                
VFIN20   GOTOR VALFRM,DMCB,('TACSTYPF',SCOFDATH)                                
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE RECORD DATE/STUDIO/CITY                  *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALRIN   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCORDATH         R2=A(RECORD DATE)                            
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         MVI   ELCODE,TACSELQ      DELETE EXISTING RECORD STUDIO ELEM           
         GOTO1 DELL,DMCB,(1,=AL1(TACSTYPR))                                     
                                                                                
         CLI   5(R2),0             NEVER REQUIRED IF COMMERCIAL TYPE            
         JNE   VRIN30              IS MUSIC                                     
         CLI   TACOTYPE,CTYMUS                                                  
         JE    XIT                                                              
                                                                                
         CLI   TACOMED,TACOMEDR    ALWAYS REQUIRED IF MEDIA IS RADIO            
         JE    ERMIS                                                            
                                                                                
         CLI   TACOMED,TACOMEDT    IF MEDIA IS TELEVISION                       
         JE    VRIN10                                                           
         CLI   TACOMED,TACOMEDI    INTERNET                                     
         JE    VRIN10                                                           
         CLI   TACOMED,TACOMEDN    OR NED MEDIA                                 
         JNE   VRIN20                                                           
VRIN10   LA    R2,SCOFDATH         FILM DATE OR RECORD DATE MUST                
         GOTO1 ANY                 BE PROVIDED                                  
         DROP  R5                                                               
                                                                                
VRIN20   LA    R2,SCORDATH         RECORD DATE IS REQUIRED                      
         CLI   SCORSTUH+5,0        IF RECORD STUDIO                             
         JNE   ERMIS                                                            
         CLI   SCORCITH+5,0        OR RECORD CITY                               
         JNE   ERMIS                                                            
         CLI   SCORSTAH+5,0        OR RECORD STATE IS PROVIDED                  
         JNE   ERMIS                                                            
         J     XIT                                                              
                                                                                
VRIN30   GOTOR VALFRM,DMCB,('TACSTYPR',SCORDATH)                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE MUSIC DATE/STUDIO/CITY                   *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALMIN   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOMDATH         R2=A(MUSIC DATE)                             
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         MVI   ELCODE,TACSELQ      DELETE EXISTING MUSIC STUDIO ELEM            
         GOTO1 DELL,DMCB,(1,=AL1(TACSTYPM))                                     
                                                                                
         CLI   5(R2),0             ALWAYS REQUIRED IF COMMERCIAL TYPE           
         JNE   VMIN10              IS MUSIC                                     
         CLI   TACOTYPE,CTYMUS                                                  
         JE    ERMIS                                                            
         DROP  R5                                                               
                                                                                
         CLI   SCOMSTUH+5,0        MUSIC DATE IS REQUIRED                       
         JNE   ERMIS               IF MUSIC STUDIO                              
         CLI   SCOMCITH+5,0        OR MUSIC CITY                                
         JNE   ERMIS                                                            
         CLI   SCOMSTAH+5,0        OR MUSIC STATE IS PROVIDED                   
         JNE   ERMIS                                                            
         J     XIT                                                              
                                                                                
VMIN10   GOTOR VALFRM,DMCB,('TACSTYPM',SCOMDATH)                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE INTERNET/NEW MEDIA FIELDS                *         
***********************************************************************         
                                                                                
VALINM   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         MVI   ELCODE,TAMDELQ      DELETE EXISTING INTERNET/NEWMEDIA            
         GOTO1 REMELEM             ELEMENTS                                     
                                                                                
         LA    R2,SCOMED1H         R2=A(FIRST INTERNET/NEW MEDIA FIELD)         
         LA    R3,SCOMED6H         R3=A(LAST INTERNET/NEW MEDIA FIELD)          
                                                                                
         USING TAMDD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAMDEL,TAMDELQ                                                   
         MVI   TAMDLEN,TAMDLNQ                                                  
         MVC   TAMDTYPE,TGMEEQU                                                 
                                                                                
VINM10   CR    R2,R3               IF ALL FIELDS PROCESSED                      
         JH    XIT                 DONE                                         
                                                                                
         GOTOR VALWEB,DMCB,(X'80',0)                                            
                                                                                
         CLI   5(R2),0             IF PROVIDED, ENSURE INTERNET/                
         JE    VINM20              NEWMEDIA CODE IS VALID                       
         TM    TGMEEQU,INTERNET+NEWMEDIA                                        
         JZ    ERINV                                                            
         GOTO1 RECVAL,DMCB,TLMDCDQ,(R2)                                         
         MVC   TAMDCODE,TGMDCODE                                                
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
VINM20   ZIC   RE,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RE                                                            
         J     VINM10                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE EDIT YEAR/TYPE FIELDS                    *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALETY   NTR1                                                                   
         CLI   TGCTSTTY,TASTTYPF   IF CONNECT STAFF IS F                        
         JE    XIT                 IGNORE ANY INPUT                             
                                                                                
         MVI   TACOEDYR,0          INITIALIZE EDIT TYPE YEAR                    
         XC    TACOEDT,TACOEDT     AND EDIT TYPE                                
                                                                                
         LA    R2,SCOEDYRH         R2=A(EDIT YEAR FIELD)                        
                                                                                
         CLI   5(R2),0             EDIT YEAR REQUIRED                           
         JNE   VETY10                                                           
         CLI   SCOETYPH+5,0        IF EDIT TYPE IS PROVIDED                     
         JNE   ERMIS                                                            
         J     XIT                                                              
                                                                                
VETY10   TM    4(R2),X'08'         ELSE, ENSURE YEAR IS VALID NUMERIC           
         JZ    ERINV                                                            
         CLC   8(4,R2),=C'1900'    AND BETWEEN 1900                             
         JL    ERINV                                                            
         CLC   8(4,R2),=C'2027'    AND 2027                                     
         JH    ERINV                                                            
                                                                                
         MVC   BLOCK(L'SCOEDYR),SCOEDYR                                         
         MVC   BLOCK+L'SCOEDYR(4),=C'0101'                                      
         GOTO1 DATCON,DMCB,(9,BLOCK),(1,FULL)                                   
         MVC   TACOEDYR,FULL                                                    
         XI    TACOEDYR,X'FF'                                                   
                                                                                
         USING EDTD,RE                                                          
         L     RE,TGAEDTYP                                                      
VETY30   CLI   0(RE),X'FF'         ENSURE EDIT YEAR IS VALID                    
         JE    ERINV                                                            
         CLC   TACOEDYR,EDTYR                                                   
         JE    VETY40                                                           
         LA    RE,EDTLNQ(RE)                                                    
         J     VETY30                                                           
VETY40   ZICM  R3,EDTYTAB,2                                                     
         DROP  RE                                                               
                                                                                
         LA    R2,SCOETYPH         EDIT TYPE IS REQUIRED                        
         GOTO1 ANY                                                              
                                                                                
         L     RE,TGAEDTYP                                                      
         AR    RE,R3                                                            
VETY50   CLI   0(RE),X'FF'         ENSURE EDIT TYPE IS VALID FOR                
         JE    ERINV               EDIT YEAR                                    
         CLC   0(L'SCOETYP,RE),WORK                                             
         JE    VETY60                                                           
         LA    RE,L'SCOETYP(RE)                                                 
         J     VETY50                                                           
                                                                                
VETY60   MVC   TACOEDT,WORK                                                     
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE ACTIVE/INACTIVE DATE FIELDS              *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALAID   NTR1                                                                   
         CLI   TGCTSTTY,TASTTYPF   IF CONNECT STAFF IS F                        
         JE    XIT                 IGNORE ANY INPUT                             
                                                                                
         XC    TACOACT,TACOACT     INITIALIZE ACTIVE DATE                       
         XC    TACOINAC,TACOINAC   AND INACTIVE DATE                            
                                                                                
         CLI   SCOACTH+5,0         IF ACTIVE DATE IS PROVIDED                   
         JE    VAID10              ENSURE IT IS VALID                           
         LA    R2,SCOACTH                                                       
         GOTO1 DTVAL,DMCB,TACOACT                                               
                                                                                
VAID10   CLI   SCOINACH+5,0        IF INACTIVE DATE IS PROVIDED                 
         JE    XIT                 ENSURE IT IS VALID                           
         LA    R2,SCOINACH                                                      
         GOTO1 DTVAL,DMCB,TACOINAC                                              
                                                                                
         CLC   TACOINAC,TACOACT    ENSURE INACTIVE DATE IS LATER                
         JL    ERINV               THAN ACTIVE DATE                             
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE COMMERCIAL POOL FIELD                    *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R4 = A(PRIMARY COMMERCIAL RECORD)               *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
         USING TACOD,R5                                                         
VALPOL   NTR1                                                                   
         CLI   TGCTSTTY,TASTTYPF   IF CONNECT STAFF IS F                        
         JE    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         XC    TACOCGRP,TACOCGRP   INITIALIZE COMMERCIAL POOL                   
                                                                                
         CLI   SCOCOGRH+5,0        EXIT IF COMMERCIAL POOL IS NOT               
         JE    XIT                 PROVIDED                                     
                                                                                
         GOTO1 RECVAL,DMCB,TLOGCDQ,(4,SCOCOGRH)                                 
         JE    VPOL10                                                           
                                                                                
         XC    TGPRD,TGPRD                                                      
         GOTO1 RECVAL,DMCB,TLOGCDQ,SCOCOGRH                                     
         MVC   TGPRD,TLCOPRD                                                    
         DROP  R4                                                               
                                                                                
VPOL10   MVC   TACOCGRP,TGCOG                                                   
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE SPLIT CYCLES FIELD                       *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALSCY   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         MVI   TACOSPCY,0          INITIALIZE SPLIT CYCLE INDICATOR             
                                                                                
         LA    R2,SCOSPCYH         R2=A(SPLIT CYCLE FIELD)                      
         GOTOR VALWEB,DMCB,(X'80',0)                                            
         CLI   5(R2),0             INPUT IS NOT REQUIRED                        
         JNE   VSCY20                                                           
         CLI   TACOTYPE,0          IF COMMERCIAL TYPE IS SET                    
         JE    VSCY10                                                           
         CLI   TACOTYPE,CTYASIAN   AND IS NOT ASIAN                             
         JE    VSCY10                                                           
         CLI   TACOTYPE,CTYADD     ADDENDUM                                     
         JE    VSCY10                                                           
         CLI   TACOTYPE,CTYSEAS    SEASONAL                                     
         JE    VSCY10                                                           
         CLI   TACOTYPE,CTYSEAS2   NEW SEASONAL                                 
         JE    VSCY10                                                           
         CLI   TACOTYPE,CTYSPAN    OR SPANISH                                   
         JNE   XIT                                                              
                                                                                
VSCY10   CLI   TACOMED,TACOMEDT    ELSE, INPUT IS REQUIRED                      
         JE    ERMIS               IF MEDIA IS TELEVISION                       
         CLI   TACOMED,TACOMEDC    CABLE                                        
         JE    ERMIS                                                            
         CLI   TACOMED,TACOMEDI    INTERNET                                     
         JE    ERMIS                                                            
         CLI   TACOMED,TACOMEDN    OR NEW MEDIA                                 
         JE    ERMIS                                                            
         J     XIT                                                              
                                                                                
VSCY20   MVC   TACOSPCY,8(R2)                                                   
         CLI   8(R2),C'N'         INPUT MUST BE NO                              
         JE    XIT                                                              
         CLI   8(R2),C'Y'         OR YES                                        
         JNE   ERINV                                                            
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE BILL-TO-CODE FIELD                       *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALBTC   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOBTCH          R2=A(BILL TO CODE FIELD)                     
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         XC    TACOATT,TACOATT     INITIALIZE BILL-TO-CODE                      
                                                                                
         CLI   5(R2),0             IF PROVIDED                                  
         JE    XIT                 ENSURE BILL-TO-CODE IS VALID                 
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLATCDQ,(R2)                                         
         MVC   AIO,AIO1                                                         
         MVC   TACOATT,TGATT                                                    
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE COMMENT FIELD                            *         
***********************************************************************         
                                                                                
VALCMT   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOCOMMH         R2=A(COMMENT FIELD)                          
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',(R2)),TACMTYPG                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE EXPIRATION DATE FIELD                    *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALEXP   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOEXPH          R2=A(EXPIRATION DATE FIELD)                  
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         XC    TACOEXP,TACOEXP     INITIALIZE EXPIRATION DATE                   
                                                                                
         CLI   TACOTYPE,CTYIND     IF COMMERCIAL TYPE IS INDUSTRIAL             
         JE    XIT                                                              
         CLI   TACOTYPE,CTYDEM     DEMO                                         
         JE    XIT                                                              
         CLI   TACOTYPE,CTYSDEM    SPANISH DEMO                                 
         JE    XIT                                                              
         CLI   TACOTYPE,CTYMUS     MUSIC                                        
         JE    XIT                                                              
         CLI   TACOTYPE,CTYPROMO   PROMO                                        
         JE    XIT                                                              
         CLI   TACOTYPE,CTYICAT1   OR INDUSTRIAL CATEGORY 1                     
         JE    XIT                 DO NOT SET EXPIRATION DATE                   
                                                                                
         CLI   5(R2),0             IF EXPIRATION DATE IS PROVIDED               
         JE    VEXP10                                                           
         CLI   TACOTYPE,CTYSEAS2   IGNORE IS COMMERCIAL TYPE IS                 
         JE    VEXP10              NEW SEASONAL                                 
         GOTO1 DTVAL,DMCB,TACOEXP  ELSE ENSURE EXPIRATION DATE IS VALID         
         J     XIT                                                              
                                                                                
VEXP10   CLI   TACOMED,TACOMEDR    ELSE, IF MEDIA IS RADIO                      
         JNE   VEXP20              CALCULATE EXPIRATION DATE BASED              
         LA    R2,SCORDATH         ON RECORD DATE                               
         GOTO1 DTVAL,DMCB,TGDATE                                                
         GOTO1 EXPIRE,DMCB,(R5),TGDATE,TACOEXP                                  
         J     XIT                                                              
                                                                                
VEXP20   GOTO1 EXPIRE,DMCB,(R5),0,TACOEXP                                       
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE SECOND SEASON FIRST AIR DATE             *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALSAR   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         XC    TACOSAIR,TACOSAIR   INITIALIZE 2ND SEASON 1ST AIR DATE           
                                                                                
         CLI   SCOSAIRH+5,0        EXIT IF 2ND SEASON 1ST AIR DATE              
         JE    XIT                 IS NOT PROVIDED                              
                                                                                
         LA    R2,SCOSAIRH                                                      
         CLI   TACOTYPE,CTYSEAS2   ONLY ALLOWED FOR NEW SEASONAL                
         JNE   ERINV               COMMERCIALS                                  
         OC    TACOAIR,TACOAIR     WITH A SET FIRST AIR DATE                    
         JZ    ERMFA                                                            
         GOTO1 DTVAL,DMCB,TACOSAIR MUST BE VALID DATE                           
                                                                                
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,TACOAIR),(8,WORK)                                 
         MVI   TGBYTE,13                                                        
         MVC   WORK+8(5),=C'-(3M)'                                              
         TM    AYSTAT,TAAYS13W     DATE MUST BE MORE THAN 13 WEEKS/             
         JZ    VSAR10              3 MONTHS AFTER FIRST AIR DATE                
         MVI   TGBYTE,14                                                        
         MVC   WORK+8(6),=C'-(13W)'                                             
                                                                                
VSAR10   GOTO1 PERVAL,DMCB,(TGBYTE,WORK),('PVINSGLS+PVIN1DYL',BLOCK)            
         CLC   TACOSAIR,BLOCK+PVALPEND-PERVALD                                  
         JNH   ERLFA                                                            
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE ACTRA EXPIRATION DATE                    *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALAEX   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         XC    TACOAEXP,TACOAEXP   INITIALIZE ACTRA EXPIRATION DATE             
                                                                                
         CLI   SCOAEXPH+5,0                                                     
         JE    VAEX20                                                           
         LA    R2,SCOAEXPH         CAN ONLY BE PROVIDED IF ACTRA TYPE           
         CLI   TACOCTYP,CCTY04A    IS 2404A                                     
         JE    VAEX10                                                           
         CLI   TACOCTYP,CCTY2404   OR 2404                                      
         JNE   ERINV                                                            
VAEX10   GOTO1 DTVAL,DMCB,TACOAEXP                                              
         J     XIT                                                              
                                                                                
         USING PERVALD,R3                                                       
VAEX20   CLI   TACOCTYP,CCTY04A    IF ACTRA TYPE 2404A                          
         JE    VAEX30                                                           
         CLI   TACOCTYP,CCTY2404   OR ACTRA TYPE 2404 AND ACTRA                 
         JNE   XIT                 EXPIRATION DATE NOT PROVIDED                 
VAEX30   CLI   SCOAEXPH+5,0        CALCULATE IT NOW                             
         JNE   XIT                                                              
                                                                                
         USING TACSD,R4                                                         
         MVI   ELCODE,TACSELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACSTYPF))                                     
         JNE   XIT                                                              
         L     R4,TGELEM                                                        
         GOTO1 DATCON,DMCB,(1,TACSDATE),(8,WORK)                                
         MVC   WORK+8(6),=C'-(24M)'                                             
         LA    R3,BLOCK                                                         
         GOTO1 PERVAL,DMCB,(14,WORK),('PVIN1DYL',BLOCK)                         
         MVC   TACOAEXP,PVALPEND                                                
         OC    TACOAIR,TACOAIR                                                  
         JZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,TACOAIR),(8,WORK)                                 
         MVC   WORK+8(6),=C'-(18M)'                                             
         GOTO1 PERVAL,DMCB,(14,WORK),('PVIN1DYL',BLOCK)                         
         CLC   TACOAEXP,PVALPEND                                                
         JL    XIT                                                              
         MVC   TACOAEXP,PVALPEND                                                
         J     XIT                                                              
         DROP  R3,R4,R5                                                         
                                                                                
***********************************************************************         
*        LITERALS FOR VRCOM1 ROUTINES                                 *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE STATUS FIELD                             *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALSTA   NTR1  BASE=*,LABEL=*                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
         MVC   SVCOSTAT,TACOSTAT   SAVE INITIAL STATUS BYTES                    
         MVC   SVCOSTA2,TACOSTA2                                                
                                                                                
         MVI   TACOSTAT,0          INITIALIZE STATUSES                          
         NI    TACOSTA2,X'FF'-TACOSNCS                                          
         NI    TACOSTA3,X'FF'-TACOSNHF-TACOS26K-TACOSSMW                        
                                                                                
         CLI   SCOSTATH+5,0        EXIT IF STATUS IS NOT PROVIDED               
         JE    VSTA75                                                           
                                                                                
         LA    R2,SCOSTATH         R2=A(STATUS FIELD)                           
         XC    HALF,HALF                                                        
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   4(R1),0                                                          
         JE    ERINV                                                            
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
                                                                                
         USING SCAND,R1                                                         
         LA    R1,BLOCK            R1=A(SCAN BLOCK)                             
                                                                                
VSTA10   L     RF,ASTATAB                                                       
VSTA20   CLI   0(RF),X'FF'                                                      
         JNE   VSTA40                                                           
                                                                                
         CLI   TGVER,2             IF MAINTAINING VERSION 4 OR HIGHER           
         JL    VSTA25                                                           
         CLI   TGVER,4                                                          
         JL    VSTA60                                                           
         CLC   SCDATA1,VSTA26K     26K STATUS IS VALID                          
         JNE   VSTA60                                                           
         OI    TACOSTA3,TACOS26K                                                
         J     XIT                                                              
                                                                                
VSTA25   CLC   SCDATA1,VSTANCSF                                                 
         JNE   VSTA30                                                           
         OI    TACOSTA2,TACOSNCS   DON'T CHARGE CSF ON THIS COMMERCL            
         J     VSTA60                                                           
                                                                                
VSTA30   CLC   SCDATA1,VSTANHFN                                                 
         JNE   VSTA35                                                           
         OI    TACOSTA3,TACOSNHF   DO NOT GENERATE HOLDING FEE NOTICES          
         J     VSTA60                                                           
                                                                                
VSTA35   CLC   SCDATA1,VSTASSMW                                                 
         JNE   ERINV                                                            
         CLI   TACOMED,TACOMEDI    MEDIA MUST BE INTERNET                       
         JE    VSTA38                                                           
         CLI   TACOMED,TACOMEDN    OR NEW MEDIA ONLY                            
         JNE   ERINV                                                            
VSTA38   OI    TACOSTA3,TACOSSMW   SOCIAL MEDIA WAVER                           
         J     VSTA60                                                           
                                                                                
VSTA40   CLC   SCDATA1,1(RF)                                                    
         JE    VSTA50                                                           
         LA    RF,L'STATAB(RF)                                                  
         J     VSTA20                                                           
                                                                                
VSTA50   OC    TACOSTAT,0(RF)                                                   
                                                                                
VSTA60   ZIC   RF,SCLEN1                                                        
         ZIC   RE,SCLEN2                                                        
         LTR   RE,RE                                                            
         JZ    *+8                                                              
         AHI   RE,1                                                             
         LA    RF,1(RF,RE)                                                      
         AH    RF,HALF                                                          
         STH   RF,HALF                                                          
                                                                                
         LA    R1,SCANNEXT                                                      
         BCT   R0,VSTA10                                                        
         DROP  R1                                                               
                                                                                
VSTA75   CLC   =C'VC',SVWID        IF WEB APPLICATION ID CONTAINS               
         JE    VSTA76              A VITA COMPLETION ID                         
         CLC   =C'TC',SVWID                                                     
         JE    VSTA76                                                           
         CLC   =C'RC',SVWID                                                     
         JNE   VSTA79                                                           
VSTA76   MVC   TGBYTE,TACOSTA2                                                  
         NI    TGBYTE,TACOSNCS     ENSURE CHARGE CSF STATUS HAS NOT             
         NI    SVCOSTA2,TACOSNCS   BEEN CHANGED                                 
         CLC   TGBYTE,SVCOSTA2                                                  
         JNE   ERUWB                                                            
                                                                                
         MVC   TGBYTE,TACOSTAT                                                  
         NI    TGBYTE,TACOSCAN+TACOSCRT                                         
         NI    SVCOSTAT,TACOSCAN+TACOSCRT                                       
         CLC   TGBYTE,SVCOSTAT     ENSURE CAN$ + CRATE STATUS HAS NOT           
         JNE   ERUWB               BEEN CHANGED                                 
                                                                                
VSTA79   TM    TACOSTAT,TACOSTRL   IF  RELEASED STATUS IS ON                    
         JZ    XIT                                                              
         DROP  R5                                                               
                                                                                
         MVC   AIO,AIO3                                                         
                                                                                
         USING TLCAD,R3                                                         
         XC    KEY,KEY             READ ALL CAST KEYS FOR COMMERCIAL            
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         J     VSTA80                                                           
VSTA70   GOTO1 SEQ                                                              
VSTA80   CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   VSTAX                                                            
         TM    TLCASORT,X'02'      REJECT THOSE NOT ON GUARANTEES               
         JO    VSTA70                                                           
         MVC   SVPCKEY,KEY         SAVE PRIMARY COMMERCIAL CAST KEY             
         MVC   TGSSN,TLCASSN       AND PERFORMER'S SS#                          
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC              GET CAST RECORD                              
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO              R4=A(CAST RECORD)                            
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGGUA,TACAGUA       SAVE GUARANTEE CODE                          
         XC    TGGUA,=4X'FF'                                                    
         DROP  R4                                                               
                                                                                
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'24',0)                                    
         JNE   VSTA110                                                          
         XC    TGGUA,=4X'FF'       READ GUARANTEE RECORD                        
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO              R4=A(GUARANTEE RECORD)                       
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAGUCOM,TAGUCOM     REJECT IF OVERSCALE GUARANTEE                
         JZ    VSTA110                                                          
         CLC   TAGUCOM,TGCOM       OR IF THIS IS NOT GUARANTEE'S                
         JNE   VSTA110             PRIMARY COMMERCIAL                           
         DROP  R4                                                               
                                                                                
         USING TLCAPD,R3                                                        
         XC    KEY,KEY             READ CAST RECORDS FOR ALL OF                 
         MVI   TLCAPCD,TLCAGCDQ    THE GUARANTEE'S SUBSIDIARY                   
         MVC   TLCAGSSN,TGSSN      COMMERCIALS                                  
         MVC   TLCAGGUA,TGGUA                                                   
         GOTO1 HIGH                                                             
         J     VSTA100                                                          
VSTA90   GOTO1 SEQ                                                              
VSTA100  CLC   KEY(TLCAGCOM-TLCAPD),KEYSAVE                                     
         JNE   VSTA110                                                          
         CLC   TLCACCOM,TGCOM      SKIP THE PRIMARY COMMERCIAL                  
         JE    VSTA90                                                           
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO              R4=A(SUBSIDIARY COMMERCIAL'S CAST            
         MVI   ELCODE,TACAELQ           RECORD)                                 
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACALAST,TACALAST   SKIP IF CAST HAS BEEN LAST                   
         JNZ   VSTA90              SERVICED                                     
         DROP  R4                                                               
                                                                                
         MVC   SVGCKEY,KEY         SAVE GUARANTEE CAST KEY                      
                                                                                
         USING TLCOPD,R3                                                        
         XC    KEY,KEY             READ SUBSIDIARY COMMERCIAL RECORD            
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVGCKEY+TLCAGCOM-TLCAPD                                 
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO              R4=A(SUBSIDIARY COMMERCIAL RECORD)           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TACOSTAT,TACOSTRL   IF COMMERCIAL IS NOT RELEASED,               
         JZ    ERRPC               RETURN ERROR                                 
         DROP  R4                                                               
                                                                                
         MVC   KEY,SVGCKEY         RESTORE GUARANTEE COMMERCIAL'S               
         GOTO1 HIGH                READ SEQUENCE                                
         J     VSTA90                                                           
                                                                                
VSTA110  MVC   KEY,SVPCKEY         RESTORE PRIMARY COMMERCIAL'S                 
         GOTO1 HIGH                CAST READ SEQUENCE                           
         J     VSTA70                                                           
                                                                                
VSTAX    MVC   AIO,AIO1            RESTORE I/O AREA                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR VRSTA ROUTINE                                   *         
***********************************************************************         
                                                                                
VSTANCSF DC    CL10'NOCSF'                                                      
VSTANHFN DC    CL10'NOHFN'                                                      
VSTA26K  DC    CL10'26K'                                                        
VSTASSMW DC    CL10'SMW'           SOCIAL MEDIA WAVER                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE FILM, RECORDING OR MUSIC INFORMATION     *         
*        ON ENTRY ... P1 BYTE 0 = FILM,RECORD OR MUSIC EQUATE         *         
*                     P1        = A(DATE FIELD)                       *         
***********************************************************************         
                                                                                
VALFRM   NTR1  BASE=*,LABEL=*                                                   
         MVC   BYTE,0(R1)          BYTE=FILM,RECORD OR MUSIC EQUATE             
         ZICM  R2,1(R1),3          R2=A(DATE FIELD)                             
                                                                                
         GOTO1 DTVAL,DMCB,TGDATE   ENSURE DATE IS VALID                         
         CLC   TGDATE,TGTODAY1     (AND NOT MORE THAN 31 DAYS IN                
         JNH   VFRM10              THE FUTURE)                                  
         GOTO1 DATCON,DMCB,(1,TGDATE),(0,DUB)                                   
         GOTO1 PERVERT,DMCB,TGTODAY0,DUB                                        
         CLC   8(2,R1),=H'31'                                                   
         JH    ERINV                                                            
                                                                                
VFRM10   GOTOR BUMP,DMCB,2         R2=A(STUDIO FIELD)                           
         GOTOR VALWEB,DMCB,(0,0)                                                
         GOTO1 ANY                                                              
                                                                                
         USING TACSD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     INITIALIZE COMMERCIAL STUDIO                 
         MVI   TACSEL,TACSELQ      ELEMENT                                      
         MVI   TACSLEN,TACSLNQ                                                  
         MVC   TACSTYPE,BYTE                                                    
         MVC   TACSDATE,TGDATE                                                  
         MVC   TACSSTUD,WORK                                                    
                                                                                
         GOTOR BUMP,DMCB,2         R2=A(CITY FIELD)                             
         GOTOR VALWEB,DMCB,(0,0)                                                
         GOTO1 ANY                                                              
         MVC   TACSCITY,WORK                                                    
                                                                                
         GOTOR BUMP,DMCB,2         R2=A(STATE FIELD)                            
         GOTOR VALWEB,DMCB,(0,0)                                                
         CLC   =C'FD',8(R2)                                                     
         JE    ERINV                                                            
         MVC   TACSSTAT,8(R2)                                                   
         OC    TACSSTAT,SPACES                                                  
         CLC   TACSSTAT,SPACES                                                  
         JE    VFRM20                                                           
         MVC   WORK(2),8(R2)                                                    
         MVI   WORK+2,X'40'                                                     
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         JE    VFRM20                                                           
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',WORK)                                         
         JNE   ERINV                                                            
VFRM20   GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        LITERALS FOR VALFRM ROUTINE                                  *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE CONTRACT TYPE FIELD                      *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALCON   NTR1  BASE=*,LABEL=*                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOCONTH         R2=A(CONTRACT TYPE)                          
         CLI   5(R2),0             ENSURE CONTRACT TYPE IS PROVIDED             
         JNE   VCON10                                                           
         MVI   TACOCONT,0                                                       
         CLI   TACOTYPE,CTYMUS     UNLESS COMMERCIAL TYPE IS MUSIC              
         JE    XIT                                                              
         CLI   TACOTYPE,CTYPRNT    OR PRINT                                     
         JNE   ERMIS                                                            
         J     XIT                                                              
                                                                                
******** GOTOR VALWEB,DMCB,(0,0)                                                
VCON10   MVC   TACOCONT,8(R2)                                                   
                                                                                
         USING CONTD,R1                                                         
         L     R1,TGACONTS                                                      
VCON20   CLC   TACOCONT,CONTEQU    ENSURE CONTRACT TYPE IS VALID                
         JE    XIT                                                              
         CLI   CONNEXT,X'FF'                                                    
         JE    ERINV                                                            
         LA    R1,CONNEXT                                                       
         J     VCON20                                                           
         DROP  R1,R5                                                            
                                                                                
***********************************************************************         
*        LITERALS FOR VALCON ROUTINE                                  *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE COMMERCIAL 2 SCREEN FIELDS               *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R4 = A(PRIMARY COMMERCIAL RECORD)               *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
VRCOM2   NTR1  BASE=*,LABEL=*                                                   
         CLI   TWASCR,SCRF8        IF ON COMMERCIAL2 SCREEN                     
         JNE   XIT                                                              
         BRAS  RE,VALAC            VALIDATE AFM CONTRACTS                       
         BAS   RE,VALMCO           VALIDATE MUSIC CODES                         
         BAS   RE,VALARU           VALIDATE ALLOWABLE/REMAING USES              
         BAS   RE,VALAFM           VALIDATE AFM RATE                            
         BAS   RE,VALDUB           VALIDATE DUB DATE                            
         BAS   RE,VALSTY           VALIDATE SESSION TYPE                        
         BAS   RE,VALFLT           VALIDATE FILTERS                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE MUSIC CODE FIELDS                        *         
***********************************************************************         
                                                                                
VALMCO   NTR1                                                                   
         MVI   ELCODE,TACPELQ      DELETE PUBLISHED MUSIC ELEMENTS              
         GOTO1 REMELEM                                                          
                                                                                
         LA    R2,SC2MUS1H                                                      
         LHI   R0,4                                                             
                                                                                
         USING TACPD,R3                                                         
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TACPEL,TACPELQ                                                   
         MVI   TACPLEN,TACPLNQ                                                  
         DROP  R3                                                               
                                                                                
VMCO10   CLI   5(R2),0                                                          
         JE    VMCO40                                                           
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLMUCDQ,(R2)                                         
         MVC   AIO,AIO1                                                         
                                                                                
         USING TACPD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TACPELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
VMCO20   BRAS  RE,NEXTEL                                                        
         JNE   VMCO30                                                           
         CLC   TACPMUS,TGMUS                                                    
         JE    ERINV                                                            
         J     VMCO20                                                           
         DROP  R4                                                               
                                                                                
         USING TACPD,R3                                                         
VMCO30   MVC   TACPMUS,TGMUS                                                    
         GOTO1 ADDELEM                                                          
         DROP  R3                                                               
                                                                                
VMCO40   GOTOR BUMP,DMCB,3                                                      
         BCT   R0,VMCO10                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE ALLOWABLE/REMAINING FREE USES FIEDLS     *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALARU   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
                                                                                
         MVI   TACOAUSE,0          INITIALIZE ALLOWABLE USES                    
         MVI   TACORUSE,0          AND REMAINING FREE USES                      
                                                                                
         CLI   TACOTYPE,CTYMUS     UNLESS COMMERCIAL TYPE IS                    
         JNE   XIT                 MUSIC IGNORE ANY INPUT                       
                                                                                
         CLI   SC2AUSEH+5,0                                                     
         JE    VARU10                                                           
         CLI   SC2AUSE,C'0'                                                     
         JE    VARU10                                                           
         LA    R2,SC2AUSEH                                                      
         GOTO1 VALINUM                                                          
         MVC   TACOAUSE,ACTUAL                                                  
                                                                                
VARU10   CLI   SC2RUSEH+5,0                                                     
         JE    XIT                                                              
         CLI   SC2RUSE,C'0'                                                     
         JE    XIT                                                              
         LA    R2,SC2RUSEH                                                      
         GOTO1 VALINUM                                                          
         MVC   TACORUSE,ACTUAL                                                  
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE AFM RATE FIELD                           *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALAFM   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         MVI   TACOAFM,0           INITIALIZE AFM RATE                          
                                                                                
         LA    R2,SC2AFMH                                                       
         CLI   5(R2),0                                                          
         JNE   VAFM10                                                           
         MVI   ELCODE,TACSELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACSTYPM))                                     
         JE    ERMIS                                                            
         J     XIT                                                              
                                                                                
VAFM10   MVC   TACOAFM,SC2AFM                                                   
         CLI   SC2AFM,C'1'                                                      
         JE    XIT                                                              
         CLI   SC2AFM,C'2'                                                      
         JE    XIT                                                              
         CLI   SC2AFM,C'5'                                                      
         JNE   ERINV                                                            
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE DUB DATE FIELD                           *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALDUB   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
                                                                                
         XC    TACODUB,TACODUB     INITIALIZE DUB DATE                          
                                                                                
         CLI   SC2DBDTH+5,0        IF PROVIDED, ENSURE DUB DATE                 
         JE    XIT                 IS VALID                                     
         LA    R2,SC2DBDTH                                                      
         GOTO1 DTVAL,DMCB,TACODUB                                               
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE ACTRA TYPE FIELD                         *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALACT   NTR1  BASE=*,LABEL=*                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         LA    R2,SCOCTYPH         R2=A(ACTRA TYPE)                             
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         MVI   TACOCTYP,0          INITIALIZE ACTRA TYPE                        
                                                                                
         CLI   5(R2),0             EXIT IF ACTRA TYPE IS NOT                    
         JE    VACT50              PROVIDED                                     
                                                                                
         OC    SCOCTYP,SPACES                                                   
         GOTO1 CCTYPVAL,DMCB,SCOCTYP                                            
         JNE   ERINV                                                            
         MVC   TACOCTYP,TGCCTEQU   ENSURE ACTRA TYPE IS VALID                   
                                                                                
         TM    TACOSTAT,TACOSCAN+TACOSCRT                                       
         JZ    VACT10                                                           
         CLI   TACOCTYP,CCTY04A    IF STATUS SET AS CAN$ OR CANRATE             
         JE    ERWCAN                                                           
         CLI   TACOCTYP,CCTY04B    ENSURE ACTRA TYPE IS NOT 2404A               
         JE    ERWCAN              OR 2404B                                     
         CLI   TACOCTYP,CCTY2404   OR 2404                                      
         JE    ERWCAN                                                           
                                                                                
VACT10   CLI   TACOMED,TACOMEDN    IF MEDIA IS NEW MEDIA                        
         JNE   VACT20                                                           
         CLI   TACOCTYP,CCTYVDO    ACTRA TYPE CAN ONLY BE VIDEO                 
         JE    VACT50                                                           
         CLI   TACOCTYP,CCTYADO    OR AUDIO                                     
         JNE   ERINV                                                            
         J     VACT50                                                           
                                                                                
VACT20   CLI   TACOCTYP,CCTYVDO    IF MEDIA IS NOT NEW MEDIA                    
         JE    ERINV                                                            
         CLI   TACOCTYP,CCTYADO    ACTRA TYPE CANNOT BE VIDEO                   
         JE    ERINV               OR AUDIO                                     
         J     VACT50                                                           
                                                                                
VACT50   CLI   TACOCTYP,CCTY2404   IF ACTRA TYPE IS NOT 2404,                   
         JE    XIT                 MAKE SURE CAST IS NOT ACT 16                 
         DROP  R5                                                               
                                                                                
         USING TLCAD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         B     VACT70                                                           
VACT60   GOTO1 SEQ                                                              
VACT70   CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   XIT                                                              
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   VACT60                                                           
         CLC   TACAUN,=C'ACT'                                                   
         BNE   VACT60                                                           
         CLC   TACAYEAR,=C'16 '    ACTRA 16 ONLY ALLLOWED FOR 2404              
         JE    ERINV                                                            
         B     VACT60                                                           
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE SESSION TYPE FIELD                       *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALSTY   NTR1                                                                   
         CLI   TGCTSTTY,TASTTYPF   IF CONNECT STAFF IS F                        
         JE    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         MVI   TACOSES,0           INITIALIZE SESSION TYPE                      
                                                                                
         CLI   SC2STYPH+5,0        EXIT IF SESSION TYPE IS NOT                  
         JE    XIT                 PROVIDED                                     
                                                                                
         LA    R2,SC2STYPH         R2=A(SESSION TYPE FIELD)                     
                                                                                
         L     R1,ASTYTAB                                                       
VSTY10   CLI   0(R1),X'FF'                                                      
         JE    ERINV                                                            
         CLC   SC2STYP,0(R1)                                                    
         JE    VSTY20                                                           
         LA    R1,L'STYTAB(R1)                                                  
         J     VSTY10                                                           
VSTY20   MVC   TACOSES,8(R2)                                                    
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE FILTERS FIELD                            *         
***********************************************************************         
                                                                                
VALFLT   NTR1                                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                                                              
         CLI   TGVER,1             OR MAINTAINING VERSION 2 OR HIGHER           
         JH    XIT                 IGNORE ANY INPUT                             
                                                                                
         MVI   ELCODE,TAFLELQ      DELETE ORIGINAL FILTERS ELEMENTS             
         GOTO1 REMELEM                                                          
                                                                                
         CLI   SC2FILTH+5,0        EXIT IF FILTERS ARE NOT PROVIDED             
         JE    XIT                                                              
         OC    SC2FILT,SPACES                                                   
                                                                                
         USING TAFLD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAFLEL,TAFLELQ                                                   
         MVI   TAFLLEN,TAFLLNQ                                                  
         OC    SC2FILT,SPACES                                                   
         MVC   TAFLFLT1(4),SC2FILT                                              
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        LITERALS FOR VRCOM2 ROUTINES                                 *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURES THAT VERSION 1'S VITA SESSION LOCKED FIELDS  *         
*        ARE NOT UPDATED FROM THIS MAINFRAME APPLICATION              *         
*        ON ENTRY ... P1 BYTE = X'80' ONLY CHECK FOR VITA COMPLETIONS *         
*                                     (COMMERCIAL AND VERSIONS)       *         
*                     P1 BYTE = X'40' WHEN CHECKING FOR VITA          *         
*                                     COMPLETIONS, CHECK FOR BOTH     *         
*                                     COMM'L AND VERSIONS             *         
*                     P1 BYTE = X'20' WHEN CHECKING FOR VITA          *         
*                                     SESSION, CHECK FOR BOTH         *         
*                                     COMM'L AND VERSION 2            *         
*                     R2 = A(LOCKED FIELD)                            *         
***********************************************************************         
                                                                                
VALWEB   NTR1  BASE=*,LABEL=*                                                   
         TM    4(R2),X'20'         IF FIELD HAS CHANGED ...                     
         JO    XIT                                                              
                                                                                
         CLC   =C'VC',SVWID        ... AND RECORD IS STAMPED WITH VITA          
         JE    VW00                COMPLETION ID                                
         CLC   =C'TC',SVWID                                                     
         JE    VW00                                                             
         CLC   =C'RC',SVWID                                                     
         JNE   VW10                                                             
VW00     CLI   TGVER,1             ALWAYS RETURN ERROR IF CHECKING              
         JE    ERUWB               COMM'L                                       
         TM    0(R1),X'40'         RETURN ERROR IF THIS IS A VERSION            
         JO    ERUWB               AND CHECKING VERSIONS                        
         J     XIT                                                              
                                                                                
VW10     TM    0(R1),X'80'         ... AND CHECKING FOR VITA SESSIONS           
         JO    XIT                                                              
         CLC   =C'VS',SVWID        AND RECORD IS STAMPED WITH                   
         JE    VW20                A VITA SESSION ID                            
         CLC   =C'TS',SVWID                                                     
         JE    VW20                                                             
         CLC   =C'RS',SVWID        OR VITA RADIO SESSION ID                     
         JNE   XIT                                                              
VW20     CLI   TGVER,1             RETURN ERROR IF THIS IS VERSION 1            
         JE    ERUWB                                                            
         CLI   TGVER,2             RETURN ERROR IF THIS IS VERSION 2            
         JNE   XIT                 AND FIELD CANNOT BE CHANGED ON               
         TM    0(R1),X'20'         VERSION 2                                    
         JO    ERUWB                                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ADD/UPDATE COMMERCIAL/VERSION RECORDS             *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R4 = A(IN PROGRESS COMMERCIAL/VERSION RECORD)   *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VRFINSH  NTR1  BASE=*,LABEL=*                                                   
         MVC   ELEMENT,TACOELEM                                                 
         GOTO1 ADDELEM                                                          
                                                                                
         BAS   RE,VRFNCOA          FINISH COMMERCIAL ADD                        
         BAS   RE,VRFNCOC          FINISH COMMERCIAL CHANGE                     
                                                                                
         BAS   RE,VRFNVRA          FINISH VERSION ADD                           
         BAS   RE,VRFNVRC          FINISH VERSION CHANGE                        
                                                                                
         L     RF,ASVPTRS          IF PASSIVE POINTER HAVE NOT                  
         CLI   0(RF),X'FF'         ALREADY BEEN UPDATED                         
         JE    VRF10               UPDATE THEM NOW                              
         GOTO1 ADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                 
                                                                                
VRF10    BRAS  RE,UPDCAST          UPDATE CAST RECORDS                          
                                                                                
         BAS   RE,UPDNXTC          UPDATE NEXT CAST SEQUENCE NUMBER             
         MVI   IOOPT,C'Y'                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO ADD COMMERCIAL RECORD                             *         
*        ON ENTRY ... R4 = A(IN PROGRESS COMMERCIAL RECORD)           *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VRFNCOA  NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF ADDING COMMERCIAL RECORD                  
         JNE   XIT                 READ SYSTEM RECORD                           
         CLI   TGVER,1                                                          
         JNE   XIT                                                              
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'30',0)                                    
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TASYD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TASYELQ      GET NEXT INTERNAL COMMERCIAL                 
         BRAS  RE,GETEL            NUMBER                                       
         JE    *+6                                                              
         DC    H'00'                                                            
         ZICM  R1,TASYLCOM,4                                                    
         AHI   R1,1                                                             
         STCM  R1,15,TASYLCOM                                                   
         STCM  R1,15,TGCOM                                                      
         GOTO1 PUTREC              UPDATE SYSTEM RECORD                         
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO1                                                         
                                                                                
         USING TAVRD,R4                                                         
         LA    R4,ELEMENT          ADD VERSION 1 ELEMENT TO                     
         XC    ELEMENT,ELEMENT     COMMERCIAL RECORD                            
         MVI   TAVREL,TAVRELQ                                                   
         MVI   TAVRLEN,TAVRLNQ                                                  
         MVI   TAVRVERS,1                                                       
         MVC   TAVRCID,TGCID                                                    
         MVC   TAVRSEC,TACOSEC                                                  
         GOTO1 ADDELEM                                                          
         DROP  R4,R5                                                            
                                                                                
         USING TANUD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     ADD NEXT CAST SEQUENCE NUMBER                
         MVI   TANUEL,TANUELQ      ELEMENT TO COMMERCIAL RECORD                 
         MVI   TANULEN,TANULNQ1                                                 
         MVI   TANUTYPE,TANUTSEQ                                                
         MVC   TANUNXTC,=X'0001'                                                
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO1             INSERT INTERNAL COMMERCIAL NUMBER            
         MVC   TLCOCOM,TGCOM       INTO RECORD KEY                              
         BRAS  RE,MYADDREC         AND ADD COMMERCIAL RECORD                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO UPDATE COMMERCIAL RECORD                          *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R4 = A(IN PROGRESS COMMERCIAL RECORD)           *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VRFNCOC  NTR1                                                                   
         CLI   TGVER,1             IF CHANGING OR COPYING COMMERCIAL            
         JH    XIT                                                              
         CLI   ACTNUM,ACTCHA                                                    
         JE    *+12                                                             
         CLI   ACTNUM,ACTCOPY                                                   
         JNE   XIT                                                              
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'20',0)                                   
         JE    *+6                                                              
         DC    H'00'               REREAD COMMERCIAL TO AVOID                   
         MVC   AIO,AIO1            PUTREC DRAMA                                 
                                                                                
***********************************************************************         
                                                                                
         USING TAVRD,R4                                                         
         CLI   TGVER,1             IF THIS IS VERSION 1                         
         JNE   VRFCC20                                                          
         L     R4,AIO1                                                          
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VRFCC10                                                          
         MVI   TAVREL,X'FF'        DELETE EXISTING VERSION 1 ELEMENT            
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
                                                                                
VRFCC10  LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     ADD UPDATED VERSION 1 ELEMENT                
         MVI   TAVREL,TAVRELQ                                                   
         MVI   TAVRLEN,TAVRLNQ                                                  
         MVC   TAVRVERS,TGVER                                                   
         MVC   TAVRCID,TGCID                                                    
         MVC   TAVRSEC,TACOSEC                                                  
         GOTO1 ADDELEM                                                          
         DROP  R4,R5                                                            
                                                                                
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
VRFCC20  L     R4,AIO1                                                          
         CLC   TLCOKEY,SVCOKEY     IF KEY IS NOT CHANGING                       
         JNE   VRFCC40                                                          
         GOTO1 PUTREC              PUT COMMERCIAL RECORD                        
                                                                                
         CLI   TGVER,1             EXIT IF CHANGING A COMMERCIAL                
         JNE   XIT                 WITHOUT VERSIONS                             
                                                                                
         CLC   SVPTYPE,TACOELEM+TACOTYPE-TACOD                                  
         JNE   VRFCC30             IF TYPE IS CHANGED                           
         CLC   SVMED,TACOELEM+TACOMED-TACOD                                     
         JNE   VRFCC30             OR MEDIA IS CHANGED                          
         TM    PROSTAT,PSWEBREM    OR WEB APPLICATION ID IS BEING               
         JZ    XIT                 REMOVED, CONTINUE ON                         
                                                                                
VRFCC30  GOTO1 ADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                 
                                                                                
***********************************************************************         
                                                                                
VRFCC40  MVC   AIO,AIO3                                                         
                                                                                
         CLC   TLCOKEY,SVCOKEY     IF KEY IS CHANGING                           
         JE    VRFCC70                                                          
         DROP  R4                                                               
                                                                                
         USING TLCOD,R2                                                         
         L     R2,AIO3                                                          
         OI    TLCOSTAT,X'80'      DELETE INITIAL RECORD AND KEYS               
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'E8',ASVPTRS),AUPPTRS                             
                                                                                
         MVC   AIO,AIO1                                                         
         BRAS  RE,MYADDREC         ADD PRIMARY COMMERCIAL RECORD                
         L     RE,ASVPTRS                                                       
         XC    0(255,RE),0(RE)                                                  
         GOTO1 ADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                 
         MVC   AIO,AIO3                                                         
                                                                                
         BAS   RE,UPDGRTS          UPDATE GUARANTEES                            
                                                                                
***********************************************************************         
                                                                                
         CLI   TGVER,0             IF COMMERCIAL HAS VERSIONS ...               
         JE    VRFCC140                                                         
                                                                                
         LHI   R0,TLCOV120                                                      
VRFCC50  MVC   KEY(L'TLCOKEY),SVCOKEY                                           
         STC   R0,KEY+TLCOVER-TLCOD                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                READ ALL INDEXED COMMERCIAL RECORDS          
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         JNE   VRFCC60                                                          
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
         OI    TLCOSTAT,X'80'      DELETE INDEXED COMMERCIAL RECORD             
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'C8',ASVPTRS),AUPPTRS                             
                                                                                
         L     RE,ASVPTRS                                                       
         XC    0(255,RE),0(RE)                                                  
         MVC   TLCOKEY,0(R4)       ADD NEW INDEXED COMMERCIAL RECORDS           
         STC   R0,TLCOVER                                                       
         NI    TLCOSTAT,X'FF'-X'80'                                             
         BRAS  RE,MYADDREC                                                      
         GOTO1 ADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                 
         DROP  R2                                                               
                                                                                
VRFCC60  AHI   R0,1                                                             
         CHI   R0,TLCOV250                                                      
         JNH   VRFCC50                                                          
                                                                                
         USING TLVRD,R3                                                         
VRFCC70  XC    KEY,KEY             READ ALL VERSION RECORDS                     
         MVI   TLVRCD,TLVRCDQ      ATTACHED TO THIS COMMERCIAL                  
         MVC   TLVRCOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         J     VRFCC90                                                          
VRFCC80  GOTO1 SEQ                                                              
VRFCC90  CLC   KEY(TLVRVER-TLVRD),KEYSAVE                                       
         JNE   VRFCC140                                                         
         MVC   SVKEY,KEY                                                        
         DROP  R3                                                               
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTAGY))                                     
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTCLI))                                     
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTPRD))                                     
                                                                                
         USING TAFND,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     ADD AGENCY CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TGAGY                                          
         MVI   TAFNTYPE,TAFNTAGY                                                
         MVC   TAFNNAME(L'TGAGY),TGAGY                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         XC    ELEMENT,ELEMENT     ADD CLIENT CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TGCLI                                          
         MVI   TAFNTYPE,TAFNTCLI                                                
         MVC   TAFNNAME(L'TGCLI),TGCLI                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         OC    TGPRD,TGPRD                                                      
         JZ    VRFCC100                                                         
         XC    ELEMENT,ELEMENT     ADD PRODUCT CODE ELEMENT                     
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TGPRD                                          
         MVI   TAFNTYPE,TAFNTPRD                                                
         MVC   TAFNNAME(L'TGPRD),TGPRD                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
VRFCC100 CLC   SVPTYPE,TACOELEM+TACOTYPE-TACOD                                  
         JE    VRFCC110            IF TYPE IS CHANGING                          
         L     R4,AIO              UPDATE VERSION TYPE                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TACOTYPE,TACOELEM+TACOTYPE-TACOD                                 
         J     VRFCC120                                                         
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
VRFCC110 CLC   SVMED,TACOELEM+TACOMED-TACOD                                     
         JE    VRFCC130            IF TYPE OR MEDIA IS CHANGING                 
VRFCC120 CLC   =C'VC',SVOWID       AND OLD WEB APPLICATION ID                   
         JE    VRFCC125            IS VITA COMPLETIONS                          
         CLC   =C'TC',SVOWID                                                    
         JE    VRFCC125                                                         
         CLC   =C'RC',SVOWID                                                    
         JNE   VRFCC130                                                         
VRFCC125 MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   VRFCC130                                                         
         L     R4,TGELEM           CHANGE WEB APPLICATION ID ELEMENT            
         MVI   TAFNTYPE,TAFNTOWB   TO OLD WEB APPLICATION ID ELEMENT            
         DROP  R4                                                               
                                                                                
VRFCC130 GOTO1 PUTREC              AND WRITE BACK VERSION                       
         GOTO1 ADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                 
                                                                                
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         J     VRFCC80                                                          
                                                                                
***********************************************************************         
                                                                                
VRFCC140 TM    PROSTAT,PSWEBREM    IF WEB APPLICATION ID IS BEING               
         JZ    VRFCC150            REMOVED                                      
                                                                                
         USING TLVRD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLVRCD,TLVRCDQ      READ VERSION 2 RECORD                        
         MVC   TLVRCOM,TGCOM                                                    
         MVI   TLVRVER,2                                                        
         GOTO1 HIGH                                                             
         CLC   TLVRKEY,KEYSAVE                                                  
         JNE   VRFCC150                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   VRFCC150                                                         
         L     R4,TGELEM                                                        
         CLC   =C'VS',TAFNNAME     IF STAMPED WITH VITA SESSION ID              
         JE    VRFCC145                                                         
         CLC   =C'TS',TAFNNAME                                                  
         JE    VRFCC145                                                         
         CLC   =C'RS',TAFNNAME     OR VITA RADIO SESSION ID                     
         JNE   VRFCC150            CHANGE WEB APPLICATION ID ELEMENT            
VRFCC145 MVI   TAFNTYPE,TAFNTOWB   TO OLD WEB APPLICATION ID ELEMENT            
         GOTO1 PUTREC              AND WRITE BACK THE RECORD                    
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
VRFCC150 MVC   AIO,AIO1                                                         
                                                                                
         L     RE,ASVPTRS          COMMERCIAL POINTERS ALREADY UPDATED          
         MVI   0(RE),X'FF'                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO UPDATE GUARANTEE RECORDS WITH NEW CLIENT          *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
***********************************************************************         
                                                                                
UPDGRTS  NTR1                                                                   
         CLC   TGCLI,SVCOKEY+TLCOCLI-TLCOD                                      
         JE    XIT                                                              
                                                                                
         USING TLCAD,R3                                                         
         XC    KEY,KEY             READ ALL CAST RECORDS FOR THIS               
         MVI   TLCACD,TLCACDQ      COMMERCIAL                                   
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         J     UGRTS20                                                          
UGRTS10  GOTO1 SEQ                                                              
UGRTS20  CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   XIT                                                              
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO              R4=A(CAST RECORD)                            
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         OC    TACAGUA,TACAGUA     IF GUARANTEE CODE IS PRESENT                 
         JZ    UGRTS10                                                          
         MVC   SVKEY,KEY           SAVE CAST KEY                                
                                                                                
         USING TLGUD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY             READ FOR GUARANTEE RECORD                    
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,SVKEY+TLCASSN-TLCAD                                      
         MVC   TLGUGUA,TACAGUA                                                  
         XC    TLGUGUA,=6X'FF'                                                  
         GOTO1 HIGH                                                             
         CLC   TLGUKEY,KEYSAVE                                                  
         JNE   UGRTS50                                                          
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R3,R4                                                            
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO              R4=A(GUARANTEE RECORD)                       
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JNE   UGRTS30                                                          
         OC    TAGUAGY(L'TAGUAGY+L'TAGUCLI),TAGUAGY                             
         JZ    UGRTS30                                                          
         CLC   TAGUAGY,TGAGY       IF AGENCY IS GUARANTEE'S PRIMARY             
         JNE   UGRTS30                                                          
         OC    TAGUCLI,TAGUCLI     AND THERE ARE NO CLIENT LIMITS               
         JZ    UGRTS50                                                          
         CLC   TAGUCLI,TGCLI       OR CLIENT MATCHES GRT'S PRIMARY              
         JE    UGRTS50             NO NEED TO UPDATE GUARANTEE                  
                                                                                
         USING GACTABD,R3                                                       
UGRTS30  LA    R3,ADDGAC                                                        
         MVC   GACAGY,TGAGY        PREPARE TO ADD NEW AGENCY/CLIENT             
         MVC   GACCLI,TGCLI        TO GUARANTEE                                 
         MVI   GACLNQ(R3),X'FF'                                                 
                                                                                
         CLC   TAGUCOM,TGCOM       IF THIS IS THE GUARANTEE'S PRIMARY           
         JNE   UGRTS40             COMMERCIAL                                   
         MVC   GACCLI,TAGUCLI      STRIP OLD PRIMARY OF PRIMARY STATUS          
         MVC   TAGUCLI,TGCLI       ADD NEW AGENCY/CLIENT AS PRIMARY             
         DROP  R4                                                               
                                                                                
UGRTS40  L     R3,AGACTAB          R2=A(GUARANTEE AGENCY/CLIENT TABLE)          
         BAS   RE,CLRGTBL          CLEAR TABLE                                  
         BAS   RE,BLDGTBL          AND BUILD IT                                 
                                                                                
         MVI   ELCODE,TAVAELQ      REMOVE OLD AGENCY/CLIENT LIMITS              
         GOTO1 REMELEM                                                          
                                                                                
         BAS   RE,BLDGACEL         BUILD AGENCY/CLIENT LIMITS                   
         GOTO1 PUTREC              AND WRITE BACK GUARANTEE                     
         DROP  R3                                                               
                                                                                
UGRTS50  MVC   KEY,SVKEY           RESTORE CAST READ SEQUENCE                   
         GOTO1 HIGH                                                             
         J     UGRTS10                                                          
                                                                                
***********************************************************************         
*        ROUTINE TO CLEAR GUARANTEE'S AGENCY/CLIENT TABLE             *         
*        ON ENTRY ... R3=A(GUARANTEE'S AGENCY/CLIENT TABLE)           *         
***********************************************************************         
                                                                                
CLRGTBL  NTR1                                                                   
         LR    RE,R3               RE=(GUARANTEE AGENCY/CLIENT TABLE)           
         LR    RF,R3                                                            
         AHI   RF,GACTLNQ          RF=(END OF GRT AGENCY/CLIENT TABLE)          
CGTBL10  XC    0(250,RE),0(RE)                                                  
         LA    RE,250(RE)                                                       
         CR    RE,RF                                                            
         JL    CGTBL10                                                          
         MVI   0(R3),X'FF'                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO BUILD GUARANTEE'S AGENCY/CLIENT TABLE             *         
*        ON ENTRY ... R3=A(AGENCY/CLIENT TABLE AREA)                            
***********************************************************************         
                                                                                
         USING GACTABD,R3                                                       
BLDGTBL  NTR1                                                                   
         USING TAVAD,R4                                                         
         L     R4,AIO              READ ALL SUBSIDIARY AGENCY/CLIENT            
         MVI   ELCODE,TAVAELQ      ELEMENTS                                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
BGTBL10  BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
                                                                                
         XR    RE,RE                                                            
         ZIC   RF,TAVALEN          CALCULATE NUMBER OF CLIENTS                  
         SHI   RF,TAVALNQ          IN ELEMENT                                   
         LTR   RF,RF                                                            
         JNZ   BGTBL20                                                          
         LHI   RF,TAVALNQ                                                       
BGTBL20  D     RE,=A(L'TAVACLI)                                                 
                                                                                
         LR    R2,RF               R2=(NUMBER OF CLIENTS IN ELEMENT)            
         LA    R5,TAVACLI          RE=A(CURRENT CLIENT IN ELEMENT)              
                                                                                
BGTBL30  CLI   TAVALEN,TAVALNQ                                                  
         JE    BGTBL40                                                          
                                                                                
         MVC   GACCLI-GACTABD(L'GACCLI,R3),0(R5)                                
                                                                                
BGTBL40  MVC   0(GACCLI-GACTABD,R3),TAVAAGY                                     
         LA    R3,GACLNQ(R3)                                                    
         MVI   0(R3),X'FF'         ADD CURRENT CLIENT TO TABLE                  
                                                                                
         LA    R5,L'TAVACLI(R5)                                                 
         BCT   R2,BGTBL30          BUMP TO NEXT CLIENT IN ELEMENT               
         J     BGTBL10                                                          
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
*        BLDGACELTO BUILD GUARANTEE'S AGENCY/CLIENT LIMIT ELEMENTS    *         
***********************************************************************         
                                                                                
BLDGACEL NTR1                                                                   
         USING GACTABD,R2                                                       
         L     R2,AGACTAB        R2=A(INITIAL TABLE)                            
         LA    R3,ADDGAC         R3=A(NEW AGENCY/CLIENT TABLE)                  
                                                                                
         USING TAVAD,R4                                                         
BGACE10  LA    R4,ELEMENT        R4=A(TAVA ELEMENT TO BUILD)                    
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAVAEL,TAVAELQ    INITIALIZE AGENCY/CLIENT LIMIT ELEMENT         
         MVI   TAVALEN,TAVALNQ                                                  
                                                                                
         LA    R5,TAVACLI        R5=A(WHERE TO SAVE NXT CLI IN ELEMENT)         
                                                                                
BGACE20  OC    GACTABD(GACLNQ),GACTABD                                          
         JNZ   BGACE30                                                          
         LA    R2,GACLNQ(R2)     IF AT DELETED ENTRY FROM INITIAL TABLE         
         J     BGACE20           BUMP TO NEXT INITIAL ENTRY                     
                                                                                
BGACE30  CLI   0(R2),X'FF'       IF AT THE END OF BOTH TABLES                   
         JNE   BGACE40           GO ADD THE FINAL AGY/CLI ELEMENT               
         CLI   0(R3),X'FF'                                                      
         JE    BGACE100                                                         
                                                                                
BGACE40  CLI   0(R2),X'FF'       IF AT END OF INITIAL TABLE                     
         JE    BGACE60           GO ADD FROM ADDITIONS TABLE                    
                                                                                
         CLI   0(R3),X'FF'       IF AT END OF ADDITIONS TABLE                   
         JE    BGACE50           GO ADD ENTRY FROM INITAL TABLE                 
                                                                                
         CLC   0(GACLNQ,R2),0(R3) COMPARE ENTRIES FROM INITIAL AND              
         JH    BGACE60           ADDITIONS - ADD THE ALPHA LOWER                
                                                                                
BGACE50  LR    R1,R2             SET TO ADD FROM INITIAL TABLE                  
         LA    R2,GACLNQ(R2)     AND BUMP TO NEXT INITIAL ENTRY                 
         J     BGACE70                                                          
                                                                                
BGACE60  LR    R1,R3             SET TO ADD FROM ADDITIONS TABLE                
         LA    R3,GACLNQ(R3)     AND BUMP TO NEXT ADDITIONS ENTRY               
                                                                                
BGACE70  OC    TAVAAGY,TAVAAGY   IF ELEMENT BUILDING IN PROGRESS                
         JZ    BGACE80                                                          
         CLC   TAVAAGY,GACAGY-GACTABD(R1)                                       
         JE    BGACE80           BUT CURRENT AGENCY DOES NOT MATCH              
         MVC   ELEMENT,ELEMENT   THE ELEMENT, GO ADD THE ELEMENT                
         GOTO1 ADDELEM                                                          
         XC    ELEMENT,ELEMENT   AND INITIALIZE THE NEW ELEMENT                 
         MVI   TAVAEL,TAVAELQ                                                   
         MVI   TAVALEN,TAVALNQ                                                  
         LA    R5,TAVACLI                                                       
                                                                                
BGACE80  MVC   TAVAAGY,GACAGY-GACTABD(R1)                                       
                                                                                
         OC    GACCLI-GACTABD(L'GACCLI,R1),GACCLI-GACTABD(R1)                   
         JZ    BGACE20                                                          
         MVC   0(L'TAVACLI,R5),GACCLI-GACTABD(R1)                               
         ZIC   RE,TAVALEN                                                       
         AHI   RE,L'TAVACLI     IF NEW CLIENT IS BEING ADDED                    
         STC   RE,TAVALEN       ADD CLIENT AND BUMP UP ELEMENT LENGTH           
                                                                                
         CLI   TAVALEN,255      IF ELEMENT IS NOW AT MAXIMUM LENGTH             
         JL    BGACE90                                                          
         MVC   ELEMENT,ELEMENT  ADD ELEMENT TO RECORD                           
         GOTO1 ADDELEM                                                          
         J     BGACE10          AND REINITIALIZE THE ELEMENT                    
                                                                                
BGACE90  LA    R5,L'GACCLI(R5)  BUMP TO SPOT IN ELEMENT FOR THE                 
         J     BGACE20          NEXT CLIENT                                     
         DROP  R2                                                               
                                                                                
BGACE100 OC    TAVAAGY,TAVAAGY  WHEN END OF BOTH TABLES IS REACHED              
         JZ    XIT                                                              
         MVC   ELEMENT,ELEMENT  ADD THE FINAL ELEMENT                           
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO ADD VERSION RECORD                                *         
*        ON ENTRY ... R4 = A(IN PROGRESS VERSION RECORD)              *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VRFNVRA  NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF ADDING VERSION RECORD                     
         JNE   XIT                 ADD IT AND ADD POINTERS                      
         CLI   TGVER,2                                                          
         JL    XIT                                                              
         BRAS  RE,MYADDREC                                                      
         GOTO1 ADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                 
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLCOKEY),SVCOKEY                                           
                                                                                
         USING INDTABD,RE                                                       
         L     RE,AINDTAB          SET COMMERCIAL KEY                           
VRFVA10  CLC   TGVER,ITUPLM                                                     
         JNH   VRFVA20                                                          
         LA    RE,ITLNQ(RE)                                                     
         J     VRFVA10                                                          
VRFVA20  MVC   KEY+TLCOVER-TLCOD(L'TLCOVER),ITEQUT                              
         DROP  RE                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         JE    VRFVA30                                                          
                                                                                
         CLI   KEYSAVE+TLCOVER-TLCOD,TLCOV026                                   
         JNE   *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TLCOD,R4                                                         
         XC    0(255,R4),0(R4)     IF COMMERCIAL RECORD DOES NOT                
         MVC   TLCOKEY,KEYSAVE     EXIST, INITIALIZE IT                         
         MVI   TLCOLEN+1,41                                                     
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
         OI    PROSTAT,PSADDIND    AND SET TO ADD INDEXED COMMERCIAL            
         J     VRFVA40             RECORD                                       
         DROP  R4                                                               
                                                                                
VRFVA30  MVI   RDUPDATE,C'Y'       IF COMMERCIAL RECORD DOES EXIST              
         GOTO1 GETREC              READ COMMERCIAL RECORD                       
         MVI   WHENOK,X'01'                                                     
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
         NI    WHENOK,X'FE'                                                     
                                                                                
         USING TAVRD,R4                                                         
VRFVA40  LA    R4,ELEMENT          ADD VERSION ELEMENT                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAVREL,TAVRELQ                                                   
         MVI   TAVRLEN,TAVRLNQ                                                  
         MVC   TAVRVERS,TGVER                                                   
         MVC   TAVRCID,TGCID                                                    
         MVC   TAVRSEC,TACOSEC                                                  
         GOTO1 ADDELEM                                                          
         DROP  R4,R5                                                            
                                                                                
         TM    PROSTAT,PSADDIND    IF ADDED INDEXED COMMERCIAL                  
         JZ    VRFVA50             RECORD, ADD IT NOW                           
         BRAS  RE,MYADDREC                                                      
         NI    PROSTAT,X'FF'-PSADDIND                                           
         J     XIT                                                              
VRFVA50  GOTO1 PUTREC              ELSE, PUT EXISTING COMMERCIAL                
         J     XIT                 RECORD                                       
                                                                                
***********************************************************************         
*        ROUTINE TO UPDATE VERSION RECORDS                            *         
*        ON ENTRY ... R4 = A(IN PROGRESS VERSION RECORD)              *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VRFNVRC  NTR1                                                                   
         CLI   TGVER,2             IF CHANGING OR COPYING                       
         JL    XIT                 VERSION RECORD                               
         CLI   ACTNUM,ACTCHA                                                    
         JE    *+12                                                             
         CLI   ACTNUM,ACTCOPY                                                   
         JNE   XIT                                                              
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLVRCDQ,(X'20',0)                                    
         JE    *+6                                                              
         DC    H'00'               REREAD VERSION TO AVOID                      
         MVC   AIO,AIO1            PUTREC DRAMA                                 
                                                                                
         GOTO1 PUTREC              PUT VERSION RECORD                           
         GOTO1 ADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                 
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLCOKEY),SVCOKEY                                           
                                                                                
         USING INDTABD,RE                                                       
         L     RE,AINDTAB          SET COMMERCIAL KEY                           
VRFVC10  CLC   TGVER,ITUPLM                                                     
         JNH   VRFVC20                                                          
         LA    RE,ITLNQ(RE)                                                     
         J     VRFVC10                                                          
VRFVC20  MVC   KEY+TLCOVER-TLCOD(L'TLCOVER),ITEQUT                              
         DROP  RE                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         JE    VRFVC30                                                          
                                                                                
         CLI   KEYSAVE+TLCOVER-TLCOD,TLCOV026                                   
         JNE   *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TLCOD,R4                                                         
         XC    0(255,R4),0(R4)     IF COMMERCIAL RECORD DOES NOT                
         MVC   TLCOKEY,KEYSAVE     EXIST, INITIALIZE IT                         
         MVI   TLCOLEN+1,41                                                     
         L     RE,ASVPTRS                                                       
         XC    0(255,RE),0(RE)                                                  
         OI    PROSTAT,PSADDIND    AND SET TO ADD INDEXED COMMERCIAL            
         J     VRFVC50             RECORD                                       
         DROP  R4                                                               
                                                                                
VRFVC30  MVI   RDUPDATE,C'Y'      IF COMMERCIAL RECORD DOES EXIST               
         GOTO1 GETREC             READ COMMERCIAL RECORD                        
         MVI   WHENOK,X'01'                                                     
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
         NI    WHENOK,X'FE'                                                     
                                                                                
         USING TAVRD,R4                                                         
         L     R4,AIO1            DELETE EXISTING VERSION ELEMENT               
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
VRFVC40  BRAS  RE,NEXTEL                                                        
         JNE   VRFVC50                                                          
         CLC   TAVRVERS,TGVER                                                   
         JNE   VRFVC40                                                          
         MVI   TAVREL,X'FF'                                                     
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
                                                                                
VRFVC50  LA    R4,ELEMENT          ADD VERSION ELEMENT                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAVREL,TAVRELQ                                                   
         MVI   TAVRLEN,TAVRLNQ                                                  
         MVC   TAVRVERS,TGVER                                                   
         MVC   TAVRCID,TGCID                                                    
         MVC   TAVRSEC,TACOSEC                                                  
         GOTO1 ADDELEM                                                          
         DROP  R4,R5                                                            
                                                                                
         TM    PROSTAT,PSADDIND    IF ADDED INDEXED COMMERCIAL                  
         JZ    VRFVC60             RECORD, ADD IT NOW                           
         BRAS  RE,MYADDREC                                                      
         NI    PROSTAT,X'FF'-PSADDIND                                           
         J     XIT                                                              
VRFVC60  GOTO1 PUTREC              ELSE, PUT EXISTING COMMERCIAL                
         J     XIT                 RECORD                                       
                                                                                
***********************************************************************         
*        ROUTINE TO UPDATE COMMERCIAL'S NEXT CAST SEQUENCE NUMBER     *         
***********************************************************************         
                                                                                
UPDNXTC  NTR1                                                                   
         TM    PROSTAT,PSCSTADD    IF ANY CAST RECORDS WERE                     
         JZ    XIT                 ADDED ...                                    
                                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'30',0)                                   
         JE    *+6                                                              
         DC    H'00'               REREAD COMMERCIAL FOR UPDATE                 
                                                                                
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSEQ))                                     
         JE    *+6                                                              
         DC    H'00'                                                            
         L     R4,TGELEM                                                        
         MVC   TANUNXTC,SVNUNXTC   SAVE NEXT CAST SEQ NUMBER                    
         GOTO1 PUTREC                                                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO UPDATE CAST RECORDS                               *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
***********************************************************************         
                                                                                
UPDCAST  NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,UCCOM            UPDATE CAST BASED ON PAGE 1 CHANGES          
         BAS   RE,UCCOM2           UPDATE CAST BASED ON PAGE 2 CHANGES          
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO UPDATE CAST RECORDS BASED ON PAGE 1 CHANGES       *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
***********************************************************************         
                                                                                
UCCOM    NTR1                                                                   
         CLI   TWASCR,SCR18        IF ON COMMERCIAL SCREEN                      
         JNE   XIT                                                              
         CLI   TGVER,1             AND VERSION IS 0 OR 1                        
         JH    XIT                                                              
         CLI   ACTNUM,ACTCHA       AND ACTION IS CHANGE                         
         JNE   XIT                                                              
                                                                                
         CLC   =C'VS',SVOWID       IF ORIGINAL WEB APPLICATION ID               
         JE    UCC05               IS VITA SESSIONS                             
         CLC   =C'TS',SVOWID                                                    
         JE    UCC05                                                            
         CLC   =C'RS',SVOWID       OR VITA RADIO SESSIONS                       
         JNE   UCC10                                                            
UCC05    TM    PROSTAT,PSWEBREM    AND WEB APPLICATION ID IS BEING              
         JO    UCC20               REMOVED                                      
UCC10    CLC   SVMED,TACOELEM+TACOMED-TACOD                                     
         JNE   UCC20               OR MEDIA IS CHANGING                         
         CLC   SVPTYPE,TACOELEM+TACOTYPE-TACOD                                  
         JE    XIT                 OR TYPE IS CHANGING                          
                                                                                
UCC20    GOTO1 MEDVAL,DMCB,TACOELEM+TACOMED-TACOD                               
         JE    *+6                                                              
         DC    H'00'               ENSURE MEDIA VARIABLES ARE SET               
                                                                                
         USING TLCAD,R3                                                         
         XC    KEY,KEY             READ ALL CAST RECORDS FOR THIS               
         MVI   TLCACD,TLCACDQ      COMMERCIAL                                   
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         J     UCC40                                                            
UCC30    GOTO1 SEQ                                                              
UCC40    CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   XIT                                                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         XR    R0,R0               DEFAULT TO "UPDATE NOT NEEDED"               
                                                                                
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ      IF WEB APPLICATION ID IS ON RECORD           
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   UCC50                                                            
         L     R4,TGELEM           CHANGE WEB APPLICATION ID ELEMENT            
         MVI   TAFNTYPE,TAFNTOWB   TO OLD WEB APPLICATION ID ELEMENT            
         AHI   R0,1                AND SET "UPDATE NEEDED"                      
         DROP  R4                                                               
                                                                                
UCC50    CLC   SVMED,TACOELEM+TACOMED-TACOD                                     
         JNE   UCC60                        IF MEDIA                            
         CLC   SVPTYPE,TACOELEM+TACOTYPE-TACOD                                  
         JE    UCC70                        OR TYPE IS CHANGING                 
UCC60    MVC   TGCTEQU,TACOELEM+TACOTYPE-TACOD                                  
         GOTO1 UPDOAP,DMCB,('TAOAELQ',AIO)  ENSURE THAT OVERSCALE AMT           
         GOTO1 (RF),(R1),('TAOPELQ',AIO)    AND PERCENTAGES REMAIN IN           
         GOTO1 (RF),(R1),('TAO2ELQ',AIO)    SYNC WITH TYPE AND MEDIA            
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         CLI   ELEMENT,0                 IF ANY ADJUSTMENTS WERE                
         JE    UCC70                     NECESSARY, SET "UPDATE NEEDED"         
         OI    PROSTAT,PSROVAP                                                  
         AHI   R0,1                                                             
                                                                                
UCC70    LTR   R0,R0                     IF "UPDATE NEEDED"                     
         JZ    UCC30                                                            
         GOTO1 PUTREC                    PUT THE CAST RECORD                    
         J     UCC30                                                            
       ++INCLUDE TAUPDOAP                                                       
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO UPDATE CAST RECORDS BASED ON PAGE 2 CHANGES       *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
***********************************************************************         
                                                                                
UCCOM2   NTR1                                                                   
         CLI   TWASCR,SCRF8        IF ON COM2 SCREEN                            
         JNE   XIT                                                              
         BAS   RE,UCADDTRK         PROCESS ADDED TRACKS                         
         BAS   RE,UCREMTRK         PROCESS REMOVED TRACKS                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO UPDATE CAST LIST BASED ON TRACKS BEING ADDED      *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
***********************************************************************         
                                                                                
UCADDTRK NTR1                                                                   
         OC    TRKALIST(L'TLCOM),TRKALIST                                       
         JZ    XIT                                                              
                                                                                
         USING TLISTD,R2                                                        
         LA    R2,TRKALIST         R2=A(LIST OF TRACKS TO ADD)                  
                                                                                
         USING TLCAD,R3                                                         
UCAT10   XC    KEY,KEY                                                          
         MVI   TLCACD,TLCACDQ      READ ALL MUSICIANS FOR THE                   
         MVC   TLCACOM,TLCOM       CURRENT ADDED TRACK'S CONTRACT               
         MVI   TLCASORT,X'80'                                                   
         GOTO1 HIGH                                                             
         J     UCAT30                                                           
UCAT20   GOTO1 SEQ                                                              
UCAT30   CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   UCAT40                                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         GOTOR ONTRK,DMCB,TLTRK    IF MUSICIAN IS ON TRACK BEING                
         JNE   UCAT20              ADDED                                        
         BAS   RE,UCBCTAL          BUILD CAST-SPECIFIC TRACK ADD LIST           
                                                                                
         MVC   SVKEY,KEY                                                        
         BAS   RE,UCPROTRK         ADD/UPDATE CAST RECORD FOR COMM'L            
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         J     UCAT20                                                           
                                                                                
UCAT40   LA    R2,TLLNQ(R2)        IF ADDING ANOTHER TRACK TO THE               
         OC    TLCOM,TLCOM         COMMERCIAL, GO PROCESS IT NOW                
         JNZ   UCAT10                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINES BUILDS CAST-SPECIFIC TRACKS TO ADD LIST IN CTRKALST *         
*        ON ENTRY ... AIO=A(CAST RECORD)                              *         
*                     R2=A(CURRENT ENTRY IN TRACKS TO ADD LIST)       *         
***********************************************************************         
                                                                                
         USING TLISTD,R2                                                        
UCBCTAL  NTR1                                                                   
         MVC   TGFULL,TLCOM        R2=A(LIST OF TRACKS TO ADD)                  
                                                                                
         LA    R3,CTRKALST         R3=A(CAST SPECIFIC TRKS TO ADD)              
         XC    CTRKALST,CTRKALST                                                
                                                                                
UCBCTAL1 GOTOR ONTRK,DMCB,TLTRK    IF MUSICIAN IS ON TRACK BEING                
         JNE   UCBCTAL2            ADDED                                        
         MVC   0(TLLNQ,R3),TLCOM   ADD TO CAST-SPECIFIC TRACK ADD LIST          
         LA    R3,TLLNQ(R3)                                                     
                                                                                
UCBCTAL2 LA    R2,TLLNQ(R2)                                                     
         OC    TLCOM,TLCOM                                                      
         JZ    XIT                                                              
         CLC   TGFULL,TLCOM                                                     
         JE    UCBCTAL1                                                         
         J     UCBCTAL2                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS IF MUSICIAN ALREADY EXISTS ON THE COMMERCIAL  *         
*        IF IT DOES NOT, UPDATES IT WITH TRACKS TO ADD AND REMOVES    *         
*        TRACKS TO DELETE                                             *         
*        IF NOT, ADDS IT WITH TRACKS TO ADD                           *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
UCPROTRK NTR1                                                                   
***********************************************************************         
*        IF MUSICIAN ALREADY EXISTS ON THIS COMMERCIAL, REMOVE ANY    *         
*        TRACKS THAT ARE BEING DELETED                                *         
***********************************************************************         
                                                                                
         USING TLISTD,R2                                                        
         LA    R2,CTRKALST                                                      
                                                                                
         USING TLCAPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCAPCD,TLCATCDQ                                                 
         MVC   TLCATMCO,TLCOM                                                   
         MVC   TLCATMCS,SVKEY+TLCASEQ-TLCAD                                     
         MVC   TLCATCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLCATSEQ-TLCAPD),KEYSAVE                                     
         JNE   UCPT30                                                           
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         USING TATRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
UCPT10   BRAS  RE,NEXTEL                                                        
         JNE   UCPT20                                                           
         CLC   TATRTRK,TLTRK                                                    
         JNE   UCPT10                                                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
UCPT20   GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         BAS   RE,UCDELTRK                                                      
         J     UCPT40                                                           
                                                                                
***********************************************************************         
*        IF MUSICIAN DOES NOT ALREADY EXIST ON COMMERCIAL,            *         
*        INITIALIZE CAST WITH MUSIC CONTRACT CAST'S DETAILS           *         
***********************************************************************         
                                                                                
UCPT30   L     RE,ASVPTRS                                                       
         XC    0(255,RE),0(RE)                                                  
                                                                                
         BAS   RE,UCCPYCST                                                      
                                                                                
***********************************************************************         
*        ADD THE TRACK ASSOCIATION                                    *         
***********************************************************************         
                                                                                
         USING TATRD,R4                                                         
UCPT40   L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
UCPT50   BRAS  RE,NEXTEL                                                        
         JNE   UCPT60                                                           
         CLC   TATRTRK,TLTRK                                                    
         JNE   UCPT50                                                           
         J     UCPT70                                                           
         DROP  R4                                                               
                                                                                
         USING TATRD,R4                                                         
UCPT60   LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TATREL,TATRELQ                                                   
         MVI   TATRLEN,TATRLNQ                                                  
         MVC   TATRCOM,TLCOM                                                    
         MVC   TATRCSQ,SVKEY+TLCASEQ-TLCAD                                      
         MVC   TATRTRK,TLTRK                                                    
         GOTO1 ADDELEM                                                          
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
*        IF ANY ADDITIONAL TRACKS ARE BEING ADDED, ADD THOSE          *         
*        TO THE CAST RECORD AS WELL                                   *         
***********************************************************************         
                                                                                
UCPT70   LA    R2,TLLNQ(R2)                                                     
         OC    TLCOM,TLCOM                                                      
         JNZ   UCPT40                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        NOW THAT ALL TRACK ASSOCIATIONS ARE PROCESSED,               *         
*        ADD OR PUT COMMERCIAL'S CAST RECORD                          *         
***********************************************************************         
                                                                                
         BAS   RE,UCUPDCST                                                      
         J     XIT                                                              
***********************************************************************         
*        ROUTINE DELETES TRACKS FOR NON-TYPE M CAST RECORD            *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
UCDELTRK NTR1                                                                   
         OC    TRKDLIST(L'TLCOM),TRKDLIST                                       
         JZ    NO                  IF TRACKS ARE BEING DELETED ...              
                                                                                
         XR    R0,R0                                                            
                                                                                
         USING TATRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ      READ THROUGH ALL TRACK ELEMENTS              
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
UCDT10   BRAS  RE,NEXTEL                                                        
         JNE   UCDT70                                                           
                                                                                
         USING TLISTD,R2                                                        
         LA    R2,TRKDLIST                                                      
UCDT20   CLC   TATRCOM,TLCOM       IF TRACK IS BEING DELETED ...                
         JNE   UCDT60                                                           
         CLC   TATRTRK,TLTRK                                                    
         JNE   UCDT60                                                           
         DROP  R2                                                               
                                                                                
         USING TLCOPD,R3                                                        
         XC    KEY,KEY             ... AND IS NOT ON ANY ADDITIONAL             
         MVI   TLCOPCD,TLCOTCDQ    VERSIONS ...                                 
         MVC   TLCOTMCO,TATRCOM                                                 
         MVC   TLCOTTRK,TATRTRK                                                 
         MVC   TLCOTCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         J     UCDT40                                                           
UCDT30   GOTO1 SEQ                                                              
UCDT40   CLC   KEY(TLCOTVER-TLCOPD),KEYSAVE                                     
         JNE   UCDT50                                                           
         CLC   TLCOTVER,TGVER                                                   
         JE    UCDT30                                                           
         J     UCDT60                                                           
         DROP  R3                                                               
                                                                                
UCDT50   MVI   0(R4),X'FF'         ... MARK ELEMENT DELETED                     
         AHI   R0,1                                                             
         J     UCDT10                                                           
         DROP  R4                                                               
                                                                                
         USING TLISTD,R2                                                        
UCDT60   LA    R2,TLLNQ(R2)                                                     
         OC    TLCOM,TLCOM                                                      
         JZ    UCDT10                                                           
         J     UCDT20                                                           
         DROP  R2                                                               
                                                                                
UCDT70   LTR   R0,R0                                                            
         JZ    NO                                                               
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE INITIALIZE CAST WITH MUSIC CONTRACT CAST'S DETAILS   *         
***********************************************************************         
                                                                                
UCCPYCST NTR1                                                                   
         L     RE,ASVPTRS                                                       
         XC    0(255,RE),0(RE)                                                  
                                                                                
         USING TLCAD,R4                                                         
         L     R4,AIO                                                           
         MVC   TLCACOM,TGCOM                                                    
         MVC   TLCASEQ,SVNUNXTC                                                 
         OI    PROSTAT,PSCSTADD                                                 
         DROP  R4                                                               
                                                                                
         ZICM  RE,SVNUNXTC,2                                                    
         AHI   RE,1                                                             
         STCM  RE,3,SVNUNXTC                                                    
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
UCCC10   BRAS  RE,NEXTEL                                                        
         JNE   UCCC30                                                           
         CLI   0(R4),TAOPELQ                                                    
         JE    UCCC10                                                           
         CLI   0(R4),TAOAELQ                                                    
         JE    UCCC10                                                           
         CLI   0(R4),TAO2ELQ                                                    
         JE    UCCC10                                                           
         CLI   0(R4),TACAELQ                                                    
         JE    UCCC10                                                           
         CLI   0(R4),TACMELQ                                                    
         JE    UCCC10                                                           
                                                                                
         USING TAFND,R4                                                         
         CLI   0(R4),TAFNELQ                                                    
         JNE   UCCC20                                                           
         CLI   TAFNTYPE,TAFNTTRK                                                
         JE    UCCC20                                                           
         CLI   TAFNTYPE,TAFNTVER                                                
         JNE   UCCC10                                                           
UCCC20   MVI   0(R4),X'FF'                                                      
         J     UCCC10                                                           
         DROP  R4                                                               
                                                                                
UCCC30   MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ADDS OR PUTS COMMERCIAL'S CAST RECORD                *         
***********************************************************************         
                                                                                
UCUPDCST NTR1                                                                   
         L     RE,ASVPTRS                                                       
         CLI   0(RE),0                                                          
         JNE   UCUC10                                                           
         BRAS  RE,MYADDREC                                                      
         J     UCUC20                                                           
UCUC10   GOTO1 PUTREC                                                           
UCUC20   GOTO1 ADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                 
         J     XIT                                                              
                                                                                
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO UPDATE CAST LIST BASED ON TRACKS BEING REMOVED    *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
***********************************************************************         
                                                                                
UCREMTRK NTR1                                                                   
         OC    TRKDLIST(L'TLCOM),TRKDLIST                                       
         JZ    XIT                                                              
                                                                                
         USING TLCAD,R3                                                         
         XC    KEY,KEY             READ THROUGH ALL CAST KEYS                   
         MVI   TLCACD,TLCACDQ      FOR THE COMMERCIAL                           
         MVC   TLCACOM,TGCOM                                                    
         MVI   TLCASORT,X'80'                                                   
         GOTO1 HIGH                                                             
         J     UCRT20                                                           
UCRT10   GOTO1 SEQ                                                              
UCRT20   CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   XIT                                                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         MVC   SVKEY,KEY                                                        
         MVI   TGBYTE3,0                                                        
                                                                                
         BAS   RE,UCDELTRK         REMOVE ANY TRACKS THAT ARE                   
         JNE   UCRT40              BEING REMOVED                                
                                                                                
         USING TLCAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ      IF NO TRACK ASSOCIATIONS ARE LEFT            
         BRAS  RE,GETEL            ...                                          
         JE    UCRT30                                                           
         DROP  R4                                                               
                                                                                
         USING TLDRD,R3                                                         
         USING TLCAD,R4                                                         
         MVC   KEY,SVKEY                                                        
         OI    TLDRSTAT,TLCASDCG                                                
         GOTO1 WRITE               ... SET TO DELETE CAST RECORD                
         L     R4,AIO              AND ITS POINTERS                             
         OI    TLCASTAT,TLCASDCG                                                
         MVI   TGBYTE3,X'40'                                                    
         DROP  R3,R4                                                            
                                                                                
UCRT30   GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(TGBYTE3,ASVPTRS)                                   
                                                                                
UCRT40   MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         TM    TGBYTE3,X'40'                                                    
         JO    UCRT20                                                           
         J     UCRT10                                                           
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ADD RECORD                                        *         
*        ON ENTRY ... AIO1 = A(RECORD TO ADD)                         *         
***********************************************************************         
                                                                                
MYADDREC NTR1  BASE=*,LABEL=*                                                   
         USING TLRCD,R4                                                         
         L     R4,AIO1                                                          
                                                                                
         USING TLDRD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   TLDRKEY,TLRCKEY     SET KEY WITH RECORD KEY                      
         MVC   TLDRSTAT,TLRCSTAT                                                
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                                                             
                                                                                
         CLC   TLDRKEY,KEYSAVE     IF RECORD IS FOUND                           
         JNE   MYAR10                                                           
         TM    TLDRSTAT,X'80'      IT MUST BE DELETED                           
         JO    *+6                                                              
         DC    H'0'                                                             
         MVC   TLDRKEY,KEYSAVE     WRITE BACK KEY IN UNDELETED STATUS           
         MVC   TLDRSTAT,KEYSAVE+TLDRSTAT-TLDRD                                  
         GOTO1 WRITE                                                            
         DROP  R3                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'       READ RECORD FOR UPDATE                       
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         NI    TLRCSTAT,X'7F'      PUT BACK IN UNDELETED STATUS                 
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
         J     MYARX                                                            
         DROP  R4                                                               
                                                                                
MYAR10   MVC   KEY,KEYSAVE         IF RECORD WAS NOT FOUND                      
         GOTO1 ADDREC              ADD IT                                       
                                                                                
MYARX    NI    DMINBTS,X'F7'       TURN OFF READ FOR DELETED                    
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DELETES COMMERCIAL/VERSION                           *         
***********************************************************************         
                                                                                
DE       NTR1  BASE=*,LABEL=*                                                   
         LA    R3,KEY              R3=A(KEY)                                    
         L     R4,AIO              R4=A(I/O AREA)                               
         NI    DMINBTS,X'F7'                                                    
                                                                                
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
         USING TLCAD,R3                                                         
         CLI   TGVER,1             IF DELETING VERSION 0 OR 1                   
         JH    DE10                                                             
         XC    KEY,KEY             ENSURE NO CAST ATTACHED TO                   
         MVI   TLCACD,TLCACDQ      COMMERCIAL                                   
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLCACOM+L'TLCACOM-TLCAKEY),KEYSAVE                           
         JE    ERNDE                                                            
         DROP  R3                                                               
                                                                                
         USING TLINPD,R3                                                        
         XC    KEY,KEY             ENSURE COMMERCIAL DOES NOT HAVE              
         MVI   TLINPCD,TLINHCDQ    ANY ATTACHED INVOICES                        
         MVC   TLINHCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLINHINV-TLINPD),KEYSAVE                                     
         JE    ERNDE                                                            
         DROP  R3                                                               
                                                                                
         USING TLTMD,R3                                                         
         XC    KEY,KEY             ENSURE COMMERCIAL DOES NOT HAVE              
         MVI   TLTMCD,TLTMCDQ      ANY ATTACHED TIMESHEETS                      
         MVC   TLTMCOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLTMINV-TLTMD),KEYSAVE                                       
         JE    ERNDE                                                            
         J     DE30                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TLVRD,R3                                                         
DE10     XC    KEY,KEY                                                          
         MVI   TLVRCD,TLVRCDQ      IF DELETING VERSION 2 OR HIGHER              
         MVC   TLVRCOM,TGCOM       READ VERSION KEY/RECORD                      
         MVC   TLVRVER,TGVER                                                    
         GOTO1 HIGH                                                             
         CLC   TLVRKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ      ENSURE IT DOES NOT HAVE ANY                  
         BRAS  RE,GETEL            TRACKS ATTACHED                              
         JE    ERV2T                                                            
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JE    ERUWB                                                            
                                                                                
         L     R4,AIO              RESTORE R4                                   
                                                                                
***********************************************************************         
                                                                                
DE20     GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'B4',0)                                   
         JE    *+6                                                              
         DC    H'00'               READ PRIMARY COMMERCIAL RECORD               
                                                                                
         USING TLCOD,R4                                                         
         MVC   SVCOKEY,TLCOKEY     SAVE COMMERCIAL KEY                          
                                                                                
         USING TAFND,R1                                                         
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   DE30                                                             
         L     R1,TGELEM           ENSURE COMMERCIAL/VERSION IS NOT             
         CLC   =C'VC',TAFNNAME     STAMPED WITH VITA COMPLETIONS ID             
         JE    ERUWB                                                            
         CLC   =C'TC',TAFNNAME                                                  
         JE    ERUWB                                                            
         CLC   =C'RC',TAFNNAME                                                  
         JE    ERUWB                                                            
         CLI   TGVER,2             ENSURE COMMERCIAL/VERSION 2 IS NOT           
         JNH   ERUWB               STAMPED WITH VITA SESSIONS ID                
         DROP  R1                                                               
                                                                                
***********************************************************************         
                                                                                
DE30     CLI   TGVER,1             IF DELETING VERSION 0 OR 1                   
         JH    DE80                                                             
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JE    ERUWB                                                            
                                                                                
DE40     GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
         OI    TLCOSTAT,X'80'      DELETE PRIMARY COMMERCIAL                    
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'E8',ASVPTRS),AUPPTRS                             
         DROP  R4                                                               
                                                                                
         USING TLCOD,R3                                                         
DE50     MVC   TLCOKEY,SVCOKEY      READ AND DELETE ALL INDEXED                 
         ZIC   RE,TLCOVER           COMMERCIAL RECORDS                          
         CHI   RE,TLCOV250                                                      
         JE    DE60                                                             
         AHI   RE,1                                                             
         STC   RE,TLCOVER                                                       
         MVC   SVCOKEY,TLCOKEY                                                  
         GOTO1 HIGH                                                             
         CLC   TLCOKEY,KEYSAVE                                                  
         JNE   DE50                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         J     DE40                                                             
         DROP  R3                                                               
                                                                                
         USING TLVRD,R3                                                         
DE60     CLI   TGVER,1              IF DELETING VERSION 1                       
         JNE   DEX                  READ AND DELETE ALL VERSION                 
         XC    KEY,KEY              RECORDS                                     
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,TGCOM                                                    
DE70     GOTO1 HIGH                                                             
         CLC   TLVRKEY(TLVRVER-TLVRD),KEYSAVE                                   
         JNE   DEX                                                              
         MVC   SVKEY,TLVRKEY                                                    
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         USING TLVRD,R4                                                         
         OI    TLVRSTAT,X'80'                                                   
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'E8',ASVPTRS),AUPPTRS                             
         DROP  R4                                                               
                                                                                
         MVC   KEY,SVKEY                                                        
         J     DE70                                                             
                                                                                
***********************************************************************         
                                                                                
         USING TLCOD,R3                                                         
DE80     MVC   KEY,SVCOKEY         IF DELETING VERSION 2 OR HIGHER              
                                                                                
         USING INDTABD,RE                                                       
         L     RE,AINDTAB                                                       
DE90     CLC   TGVER,ITUPLM        READ INDEXED COMMERCIAL RECORD               
         JNH   DE100                                                            
         LA    RE,ITLNQ(RE)                                                     
         J     DE90                                                             
DE100    MVC   TLCOVER,ITEQUT                                                   
         GOTO1 HIGH                                                             
         CLC   TLCOKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R3,RE                                                            
                                                                                
         USING TAVRD,R4                                                         
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL            DELETE VERSION ELEMENT FROM                  
         J     *+8                 INDEXED COMMERCIAL RECORD                    
DE110    BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAVRVERS,TGVER                                                   
         JNE   DE110                                                            
         MVI   TAVREL,X'FF'                                                     
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                 
         DROP  R4                                                               
                                                                                
         USING TLVRD,R4                                                         
         GOTO1 RECVAL,DMCB,TLVRCDQ,(X'34',0)                                    
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
         L     R4,AIO                                                           
         OI    TLVRSTAT,X'80'      DELETE VERSION RECORD                        
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'E8',ASVPTRS),AUPPTRS                             
         DROP  R4                                                               
                                                                                
DEX      MVI   IOOPT,C'Y'                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DE ROUTINE                                      *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE BUMPS PROVIDED NUMBER OF SCREEN FIELDS AHEAD         *         
*        ON ENTRY ... P1 = # OF FIELDS TO BUMP                        *         
*                     R2 = A(CURRENT FIELD)                           *         
***********************************************************************         
                                                                                
BUMP     NTR1  BASE=*,LABEL=*                                                   
         L     R0,0(R1)                                                         
BUMP10   ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R0,BUMP10                                                        
         XIT1  REGS=(R2)                                                        
                                                                                
***********************************************************************         
*        LITERALS FOR BUMP ROUTINE                                    *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE AFM CONTRACT FIELDS                      *         
*        ON ENTRY ... R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
VALAC    NTR1  BASE=*,LABEL=*                                                   
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF IS CLIENT                   
         JO    XIT                 IGNORE ANY INPUT                             
                                                                                
         USING TRACKD,R2                                                        
         LA    R2,SC2TAGYH         R2=A(1ST FLD OF 1ST TRACK LINE)              
         LA    R3,SC2TTIXH         R3=A(LST FLD OF LST TRACK LINE)              
         XR    R0,R0                                                            
                                                                                
         BAS   RE,SVINTRKS         SAVE INPUTTED TRACKS                         
         BAS   RE,SVORTRKS         SAVE ORIGINAL TRACKS                         
                                                                                
VALAC10  BAS   RE,VACM             VALIDATE TRACKS FOR AFM CONTRACTS            
         BAS   RE,VAC              OR COMMERCIALS                               
                                                                                
         AHI   R0,1                                                             
         LA    R2,TLNQ(R2)         BUMP TO NEXT MUSIC TRACK LINE                
         CR    R2,R3                                                            
         JL    VALAC10                                                          
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SAVE INPUTTED MUSIC TRACKS                        *         
*        ON ENTRY ... R2 = A(FIRST FLD OF 1ST TRACK LINE)             *         
*                     R3 = A(LAST FLD OF LST TRACK LINE)              *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TRACKD,R2                                                        
         USING TACOD,R5                                                         
SVINTRKS NTR1                                                                   
         CLI   TACOTYPE,CTYMUS     IF UPDATING COMMERCIAL                       
         JE    XIT                                                              
         DROP  R5                                                               
                                                                                
         MVC   AIO,AIO3                                                         
                                                                                
         USING TLISTD,R5                                                        
         LA    R5,ITTAB            R5=A(INPUTTED TRACK TABLE)                   
                                                                                
SIT10    OC    TAGY,SPACES         INITIALIZE INPUT FIELDS                      
         OC    TCNT,SPACES                                                      
         OC    TTRK,SPACES                                                      
                                                                                
         CLI   TAGYH+5,0           IF AGENCY                                    
         JE    SIT30                                                            
         CLI   TCNTH+5,0           CONTRACT                                     
         JE    SIT30                                                            
         CLI   TTRKH+5,0           AND TRACK FIELDS ARE POPULATED ...           
         JE    SIT30                                                            
                                                                                
         USING TLCOPD,R1                                                        
         LA    R1,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOICDQ    READ FOR AFM CONTRACT KEY/RECORD             
         MVC   TLCOIAGY,TAGY                                                    
         MVC   TLCOICID,TCNT                                                    
         GOTO1 HIGH                                                             
         JNE   SIT30                                                            
         GOTO1 GETREC                                                           
         DROP  R1                                                               
                                                                                
         USING TAMCD,R4                                                         
         L     R4,AIO3             IF FOUND, SEE IF TRACK EXISTS                
         MVI   ELCODE,TAMCELQ      ON THE CONTRACT                              
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
SIT20    BRAS  RE,NEXTEL                                                        
         JNE   SIT30                                                            
         CLC   TAMCTRK,TTRK                                                     
         JNE   SIT20                                                            
         DROP  R4                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO3             IF SO, ADD TRACK TO INPUTTED                 
         MVC   TLCOM,TLCOCOM       TRACKS TABLE                                 
         MVC   TLTRK,TTRK                                                       
         LA    R5,TLLNQ(R5)                                                     
         DROP  R5                                                               
                                                                                
SIT30    LA    R2,TLNQ(R2)                                                      
         CR    R2,R3               IF AT END OF TRACK LINES, MARK               
         JL    SIT10               END OF TABLE AND EXIT                        
         MVI   0(R5),X'FF'                                                      
         DROP  R2                                                               
                                                                                
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO SAVE ORIGINAL MUSIC TRACKS                        *         
*        ON ENTRY ... AIO1 =A (COMMERCIAL/VERSION RECORD)             *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TACOD,R5                                                         
SVORTRKS NTR1                                                                   
         CLI   TACOTYPE,CTYMUS     IF UPDATING COMMERCIAL                       
         JE    XIT                                                              
         DROP  R5                                                               
                                                                                
         USING TLISTD,R5                                                        
         LA    R5,OTTAB            R5=A(ORIGINAL TRACK TABLE)                   
                                                                                
         USING TATRD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TATRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
SOT10    BRAS  RE,NEXTEL                                                        
         JNE   SOT20                                                            
         MVC   TLCOM,TATRCOM                                                    
         MVC   TLTRK,TATRTRK       INITIALIZE INPUT FIELDS                      
         LA    R5,TLLNQ(R5)        AND BUMP TO NEXT TRACK TABLE ENTRY           
         J     SOT10                                                            
         DROP  R5                                                               
                                                                                
SOT20    MVI   0(R5),X'FF'                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE TRACK LINE FIELD FOR AFM CONTRACTS       *         
*        ON ENTRY ... R0 = MUSIC TRACK LINE SEQUENCE NUMBER           *         
*                     R2 = A(1ST FIELD OF MUSIC TRACK LINE)           *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TRACKD,R2                                                        
         USING TACOD,R5                                                         
VACM     NTR1                                                                   
         CLI   TACOTYPE,CTYMUS     IF UPDATING AFM CONTRACT                     
         JNE   XIT                                                              
         DROP  R5                                                               
                                                                                
         USING TAMCD,R4                                                         
         BAS   RE,SVATAMC          IF LINE WAS POPULATED PREVIOUSLY             
         L     R4,ATAMCEL          SAVE A(ELEMENT)                              
                                                                                
***********************************************************************         
*        IF ANY FIELD ON THE LINE IS POPULATED AND THE LINE WAS       *         
*        POPULATED PREVIOUSLY AND THE PREVIOUS INFO CONFORMED TO      *         
*        THE NEW MUSIC RULES AND THE TRACK FIELD IS POPULATED ...     *         
***********************************************************************         
                                                                                
         GOTO1 FLDVAL,DMCB,(X'80',TAGYH),TTITH                                  
         JE    XIT                                                              
         OC    ATAMCEL,ATAMCEL                                                  
         JZ    VACM70                                                           
         CLI   TAMCTRK,0                                                        
         JE    VACM60                                                           
         CLI   TTRKH+5,0                                                        
         JE    VACM30                                                           
                                                                                
         GOTO1 TRKUNIQM,DMCB,TTRKH  ENSURE TRACK IS UNIQUE                      
                                                                                
         CLC   TTRK,TAMCTRK        IF TRACK HAS CHANGED                         
         JE    VACM20                                                           
         BAS   RE,TRKMOVED         AND NOT JUST MOVED                           
         JE    VACM20              ENSURE PREVIOUS TRACK DOES NOT HAVE          
         GOTO1 TRKACOMS,DMCB,TAMCTRK          ANY ATTACHED COMMERCIALS          
         JNE   VACM10                                                           
         LA    R2,TTRKH                                                         
         J     ERM477                                                           
VACM10   GOTO1 TRKACAST,DMCB,TTRKH,TAMCTRK            OR ATTACHED CAST          
                                                                                
VACM20   BAS   RE,DELTRK           REMOVE OLD LINE                              
         BAS   RE,VALTLINE         VALIDATE ALL TRACK FIELDS                    
         J     XIT                 AND SAVE ELEMENT                             
                                                                                
***********************************************************************         
*        IF ANY FIELD ON THE LINE IS POPULATED AND THE LINE WAS       *         
*        POPULATED PREVIOUSLY AND THE PREVIOUS INFO CONFORMED TO      *         
*        THE NEW MUSIC RULES AND THE TRACK FIELD HAS BEEN CLEARED ... *         
***********************************************************************         
                                                                                
VACM30   BAS   RE,TRKMOVED              IF TRACK HAS NOT JUST MOVED             
         JE    VACM50                                                           
         GOTO1 TRKACOMS,DMCB,TAMCTRK    ENSURE PREVIOUS TRACK DOES NOT          
         JNE   VACM40                    HAVE ANY ATTACHED COMMERCIALS          
         LA    R2,TTRKH                                                         
         J     ERM478                                                           
VACM40   GOTO1 TRKACAST,DMCB,TTRKH,TAMCTRK            OR ATTACHED CAST          
                                                                                
VACM50   BAS   RE,DELTRK           REMOVE OLD LINE                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        IF ANY FIELD ON THE LINE IS POPULATED AND THE LINE WAS       *         
*        POPULATED PREVIOUSLY AND THE PREVIOUS INFO DID NOT CONFORM   *         
*        TO THE NEW MUSIC RULES ...                                   *         
***********************************************************************         
                                                                                
VACM60   BAS   RE,DELTRK           REMOVE OLD LINE                              
         BAS   RE,VALTLINE         VALIDATE ALL TRACK FIELDS                    
         J     XIT                 AND SAVE ELEMENT                             
                                                                                
***********************************************************************         
*        IF ANY FIELD ON THE LINE IS POPULATED AND THE LINE WAS       *         
*        NOT POPULATED PREVIOUSLY ...                                 *         
***********************************************************************         
                                                                                
VACM70   CLI   TTRKH+5,0           ENSURE TRACK FIELD IS POPULATED              
         JNE   VACM80                                                           
         LA    R2,TTRKH                                                         
         J     ERMIS                                                            
                                                                                
VACM80   GOTO1 TRKUNIQM,DMCB,TTRKH  ENSURE TRACK IS UNIQUE                      
         BAS   RE,VALTLINE         VALIDATE ALL TRACK FIELDS                    
         J     XIT                 AND SAVE ELEMENT                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURE THAT TRACK LETTER IS UNIQUE                   *         
*        ON ENTRY ... P1 = A(TRACK FIELD)                             *         
***********************************************************************         
                                                                                
TRKUNIQM NTR1                                                                   
         USING TRACKD,RE                                                        
         LA    RE,SC2TAGYH         RE=A(1ST MUSIC TRACK FIELD)                  
         L     R2,0(R1)            R2=A(CURRENT TRACK FIELD)                    
                                                                                
TUM10    LA    RF,TTRKH                                                         
         CR    RF,R2                                                            
         JNL   XIT                                                              
         CLC   TTRK,8(R2)          ENSURE TRACK INPUT IS UNIQUE                 
         JE    ERM476                                                           
         LA    RE,TLNQ(RE)                                                      
         J     TUM10                                                            
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS IF TRACK HAS MOVED TO A DIFFERENT LINE        *         
*        AND RETURNS CONDITION CODE                                   *         
*        ON ENTRY ... P4 = A(SAVED MUSIC TRACK ELEMENT)               *         
***********************************************************************         
                                                                                
         USING TAMCD,R4                                                         
TRKMOVED NTR1                                                                   
         USING TRACKD,R2                                                        
         LA    R2,SC2TAGYH         R2=A(1ST FLD OF 1ST TRACK LINE)              
         LA    R3,SC2TTIXH         R3=A(LST FLD OF LST TRACK LINE)              
TM10     CLC   TTRK,TAMCTRK        IF TRACK FIELD MATCHES THE SAVED             
         JE    YES                 TRACK ELEMENT, RETURN POSTIVE                
         LA    R2,TLNQ(R2)         CONDITION CODE                               
         CR    R2,R3                                                            
         JL    TM10                                                             
         J     NO                                                               
         DROP  R2,R4                                                            
                                                                                
***********************************************************************         
*        ROUTINE CHECKS IF PROVIDED TRACK IS ASSOCIATED TO ANY        *         
*        COMMERCIALS AND RETURNS CONDITION CODE                       *         
*        ON ENTRY ... P1 = A(TRACK LETTER)                            *         
***********************************************************************         
                                                                                
TRKACOMS NTR1                                                                   
         L     R5,0(R1)                                                         
                                                                                
         USING TLCOPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOTCDQ                                                 
         MVC   TLCOTMCO,TGCOM                                                   
         MVC   TLCOTTRK,0(R5)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLCOTCOM-TLCOPD),KEYSAVE                                     
         JE    YES                                                              
         J     NO                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REMOVED TRACK DOES NOT HAVE ATTACHED    *         
*        CAST                                                         *         
*        ON ENTRY ... P1 = A(TRACK FIELD)                             *         
*                     P2 = A(ORIGINAL TRACK VALUE)                    *         
***********************************************************************         
                                                                                
TRKACAST NTR1                                                                   
         L     R2,0(R1)            R2=A(TRACK FIELD)                            
         L     R5,4(R1)            R5=A(ORIGINAL TRACK VALUE)                   
                                                                                
         MVC   AIO,AIO3            PREPARE TO READ CAST INTO AIO3               
                                                                                
         USING TLCAD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY             READ ALL MUSICIANS ATTACHED                  
         MVI   TLCACD,TLCACDQ      TO AFM CONTRACT                              
         MVC   TLCACOM,TGCOM                                                    
         MVI   TLCASORT,X'80'                                                   
         GOTO1 HIGH                                                             
         J     TAC20                                                            
TAC10    GOTO1 SEQ                                                              
TAC20    CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   TACX                                                             
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTTRK))                                     
         JNE   TAC10                                                            
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         CLI   TAFNNAME,C'*'                                                    
         JE    ERMTAC                                                           
                                                                                
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,3                                                             
         LA    R4,TAFNNAME                                                      
         DROP  R4                                                               
                                                                                
TAC30    CLC   0(1,R4),0(R5)       IF TRACK EXISTS ON ANY                       
         JE    ERMTAC              CAST RECORDS, PROHIBIT DELETE                
         LA    R4,1(R4)                                                         
         BCT   RE,TAC30                                                         
         J     TAC10                                                            
                                                                                
TACX     MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
ERMTAC   CLI   5(R2),0                                                          
         JE    ERM479                                                           
         J     ERM498                                                           
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DELETES TRACKS FROM COMMERCIAL/VERSION               *         
*        ON ENTRY ... R2 = A(1ST FIELD OF MUSIC TRACK LINE)           *         
*                     ATATREL OR ATAMCEL = A(TRACK TO DELETE)         *         
***********************************************************************         
                                                                                
         USING TRACKD,R2                                                        
DELTRK   NTR1                                                                   
         OC    ATAMCEL,ATAMCEL    IF LINE WAS POPULATED PREVIOUSLY              
         JZ    DT10               WITH MUSIC CONTRACT DETAILS ELEMENT           
         L     R4,ATAMCEL         SET TO DELETE IT                              
         J     DT50                                                             
                                                                                
***********************************************************************         
                                                                                
         USING TATRD,R5                                                         
DT10     L     R5,ATATREL                                                       
                                                                                
         USING TLISTD,RE                                                        
         LA    RE,ITTAB                                                         
DT20     CLC   TLCOM,TATRCOM      IF TRACK IS NOT REALLY BEING                  
         JNE   DT30               DELETED, JUST MOVED, GO AHEAD                 
         CLC   TLTRK,TATRTRK      AND DELETE ELEMENT WITHOUT                    
         JE    DT50               ADDING TO DELETE LIST                         
DT30     CLI   TLLNQ(RE),X'FF'                                                  
         JE    DT40                                                             
         LA    RE,TLLNQ(RE)                                                     
         J     DT20                                                             
         DROP  R2,R5,RE                                                         
                                                                                
         USING TATRD,R5                                                         
DT40     GOTO1 ADDTLIST,DMCB,TRKDLIST,TATRCOM,TATRTRK                           
         L     R4,ATATREL         OTHERWISE, ADD TO DELETE LIST                 
         DROP  R5                                                               
                                                                                
***********************************************************************         
                                                                                
DT50     MVI   0(R4),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES TRACK LINE FIELDS AND ADDS ELEMENT         *         
*        ON ENTRY ... R0 = MUSIC TRACK LINE SEQUENCE NUMBER           *         
*                     R2 = A(1ST FIELD OF MUSIC TRACK LINE)           *         
***********************************************************************         
                                                                                
         USING TRACKD,R2                                                        
VALTLINE NTR1                                                                   
         MVC   TGBYTE3,0(R1)       TGBYTE3=COMMERCIAL TYPE                      
                                                                                
         USING TAMCD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     INITIALIZE MUSIC CONTRACT ELEMENT            
         MVI   TAMCEL,TAMCELQ                                                   
         MVI   TAMCLEN,TAMCLNQ                                                  
         STC   R0,TAMCSEQ                                                       
                                                                                
         GOTO1 VALTCN,DMCB,TCNTH   VALIDATE CONTRACT                            
         GOTO1 VALTTK,DMCB,TTRKH   VALIDATE TRACK LETTER                        
         GOTO1 VALTLF,DMCB,TLFTH   VALIDATE LIFT                                
         GOTO1 VALTLN,DMCB,TLENH   VALIDATE LENGTH                              
         GOTO1 VALTTY,DMCB,TTYPH   VALIDATE TYPE                                
         GOTO1 VALTTI,DMCB,TTITH   VALIDATE TRACK TITLE                         
                                                                                
         TM    TAGYH+1,X'20'       IF UPDATING AFM CONTRACT                     
         JZ    VTL10                                                            
         OC    TAMCCON,TAMCCON     AND CONTRACT ID MATCHES COMM'L ID            
         JNZ   VTL10                                                            
         CLI   TAMCTRK,0           AND TRACK IS POPULATED                       
         JE    VTL10                                                            
         OI    TAMCSTAT,TAMCSNEW   TURN ON CONFORMS TO NEW RULES STATUS         
                                                                                
VTL10    GOTO1 ADDELEM             AND ADD ELEMENT                              
         J     XIT                                                              
         DROP  R2,R4                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE AFM# FIELD                               *         
*        AND SAVE IN MUSIC CONTRACT DETAILS ELEMENT                   *         
*        ON ENTRY ... P1 = A(AFM# FIELD)                              *         
*                     R4 = A(MUSIC CONTRACT DETAILS ELEMENT)          *         
***********************************************************************         
                                                                                
         USING TAMCD,R4                                                         
VALTCN   NTR1                                                                   
         L     R2,0(R1)                                                         
         TM    1(R2),X'20'         EXIT IF AFM# IS PROTECTED                    
         JO    XIT                                                              
                                                                                
         OC    8(L'TCNT,R2),SPACES                                              
                                                                                
         USING TACOD,R5                                                         
         LA    R5,TACOELEM                                                      
         CLI   TACOTYPE,CTYMUS     IF UPDATING AFM CONTRACT                     
         JNE   VTCN10                                                           
         CLI   5(R2),0             SAVE AFM# IF IT IS PROVIDED                  
         JE    XIT                 AND DOES NOT MATCH COMMERCIAL ID             
         CLC   TACOCID,8(R2)                                                    
         JNE   VTCN20                                                           
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
VTCN10   CLI   5(R2),0             IF UPDATING COMMERCIAL                       
         JE    ERMIS               AFM# IS REQUIRED                             
                                                                                
VTCN20   MVC   TAMCCON,8(R2)                                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE TRACK LETTER FIELD                       *         
*        AND SAVE IN MUSIC CONTRACT DETAILS ELEMENT                   *         
*        ON ENTRY ... P1 = A(TRACK LETTER FIELD)                      *         
*                     R4 = A(MUSIC CONTRACT DETAILS ELEMENT)          *         
***********************************************************************         
                                                                                
         USING TAMCD,R4                                                         
VALTTK   NTR1                                                                   
         L     R2,0(R1)            R2=A(TRACK FIELD)                            
         MVC   TAMCTRK,8(R2)                                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE TRACK LIFT FIELD                         *         
*        AND SAVE IN MUSIC CONTRACT DETAILS ELEMENT                   *         
*        ON ENTRY ... P1 = A(TRACK LIFT FIELD)                        *         
*                     R4 = A(MUSIC CONTRACT DETAILS ELEMENT)          *         
***********************************************************************         
                                                                                
         USING TAMCD,R4                                                         
VALTLF   NTR1                                                                   
         L     R2,0(R1)            R2=A(TRACK LIFT FIELD)                       
         MVC   TAMCLFT,8(R2)                                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE TRACK LENGTH FIELD                       *         
*        AND SAVE IN MUSIC CONTRACT DETAILS ELEMENT                   *         
*        ON ENTRY ... P1 = A(TRACK LENGTH FIELD)                      *         
*                     R4 = A(MUSIC CONTRACT DETAILS ELEMENT)          *         
***********************************************************************         
                                                                                
         USING TAMCD,R4                                                         
VALTLN   NTR1                                                                   
         L     R2,0(R1)            R2=A(TRACK LENGTH FIELD)                     
         CLI   5(R2),0                                                          
         JE    XIT                                                              
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(X'C0',(RF))                                  
         CLI   0(R1),X'FF'                                                      
         JE    ERINV                                                            
         TM    4(R1),X'80'                                                      
         JO    ERINV                                                            
         OC    4(4,R1),4(R1)                                                    
         JZ    ERINV                                                            
         MVC   TAMCLLEN,6(R1)                                                   
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE MUSIC TYPE FIELD                         *         
*        AND SAVE IN MUSIC CONTRACT DETAILS ELEMENT                   *         
*        ON ENTRY ... P1 = A(MUSIC TYPE FIELD)                        *         
*                     R4 = A(MUSIC CONTRACT DETAILS ELEMENT)          *         
***********************************************************************         
                                                                                
         USING TAMCD,R4                                                         
VALTTY   NTR1                                                                   
         L     R2,0(R1)            R2=A(MUSIC CONTRACT TYPE FIELD)              
         CLI   5(R2),0                                                          
         JE    XIT                                                              
         MVC   TAMCTYP,8(R2)                                                    
                                                                                
         L     R1,AMTYTAB                                                       
VTTY10   CLI   0(R1),X'FF'                                                      
         JE    ERINV                                                            
         CLC   TAMCTYP,0(R1)                                                    
         JE    XIT                                                              
         LA    R1,L'MTYTAB(R1)                                                  
         J     VTTY10                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE TRACK TITLE FIELD                        *         
*        AND SAVE IN MUSIC CONTRACT DETAILS ELEMENT                   *         
*        ON ENTRY ... P1 = A(TRACK TITLE FIELD)                       *         
*                     R4 = A(MUSIC CONTRACT DETAILS ELEMENT)          *         
***********************************************************************         
                                                                                
         USING TAMCD,R4                                                         
VALTTI   NTR1                                                                   
         L     R2,0(R1)            R2=A(MUSIC TRACK TITLE FIELD)                
                                                                                
         CLI   5(R2),0             IF FIELD CONTAINS INPUT                      
         JE    XIT                 PAD WITH SPACES AND SAVE LENGTH              
         OC    8(L'TTIT,R2),SPACES                                              
         ZIC   R0,5(R2)                                                         
                                                                                
         LR    RE,R0               ADD TRACK TITLE TO ELEMENT                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   TAMCTRKT(0),8(R2)                                                
                                                                                
         AHI   R0,TAMCLNQ          CALCULATE NEW ELEMENT LENGTH                 
         STC   R0,TAMCLEN          AND RE-ADD ELEMENT                           
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE TRACK LINE FIELD FOR COMMERCIALS         *         
*        ON ENTRY ... R0 = MUSIC TRACK LINE SEQUENCE NUMBER           *         
*                     R2 = A(1ST FIELD OF MUSIC TRACK LINE)           *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
         USING TRACKD,R2                                                        
         USING TACOD,R5                                                         
VAC      NTR1                                                                   
         CLI   TACOTYPE,CTYMUS     IF UPDATING COMMERCIAL                       
         JE    XIT                                                              
         DROP  R5                                                               
                                                                                
         LA    R3,KEY              INITIALIZE KEY                               
*                                  IF LINE WAS POPULATED PREVIOUSLY             
*                                  BY AN UNVALIDATED ELEMENT                    
         BAS   RE,SVATAMC          SAVE A(ELEMENT)                              
                                                                                
         USING TATRD,R4                                                         
         XC    ATATREL,ATATREL                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
VAC10    BRAS  RE,NEXTEL                                                        
         JNE   VAC20                                                            
         ZIC   RE,TATRSEQ                                                       
         CR    R0,RE               IF LINE WAS POPULATED PREVIOUSLY             
         JNE   VAC10               BY A VALIDATED ELEMENT                       
         ST    R4,ATATREL          SAVE A(ELEMENT)                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF ANY FIELD ON THE LINE IS POPULATED AND LINE WAS POPULATED *         
*        PREVIOUSLY BY A VALIDATED TRACK AND THE AGENCY-CONTRACT-TRK  *         
*        FIELDS ARE ALL POPULATED AND NONE OF THEM HAS CHANGED,       *         
*        DO NOTHING!                                                  *         
***********************************************************************         
                                                                                
VAC20    BAS   RE,VALTRKW                                                       
                                                                                
         GOTO1 FLDVAL,DMCB,(X'80',TAGYH),TTITH                                  
         JE    VAC90                                                            
         OC    ATATREL,ATATREL                                                  
         JZ    VAC60                                                            
         CLI   TAGYH+5,0                                                        
         JE    VAC40                                                            
         CLI   TCNTH+5,0                                                        
         JE    VAC40                                                            
         CLI   TTRKH+5,0                                                        
         JE    VAC40                                                            
                                                                                
         BAS   RE,TRKUNIQ         ENSURE TRACK IS UNIQUE                        
                                                                                
         USING TATRD,R4                                                         
         L     R4,ATATREL                                                       
                                                                                
         USING TLCOPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOCCDQ   READ AFM CONTRACT FOR THE ORIGINAL            
         MVC   TLCOCCOM,TATRCOM   TRACK INTO AIO3                               
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         DROP  R3                                                               
                                                                                
         CLC   TATRTRK,TTRK       HAS TRACK ...                                 
         JNE   VAC30                                                            
         DROP  R4                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO3                                                          
         CLC   TLCOAGY,TAGY       ... AGENCY ...                                
         JNE   VAC30                                                            
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACOCID,TCNT       ... OR CONTRACT BEEN CHANGED?                 
         JE    XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF ANY FIELD ON THE LINE IS POPULATED AND LINE WAS POPULATED *         
*        PREVIOUSLY BY A VALIDATED TRACK AND THE AGENCY-CONTRACT-TRK  *         
*        FIELDS ARE ALL POPULATED BUT ONE OF THEM HAS CHANGED ...     *         
***********************************************************************         
                                                                                
VAC30    BAS   RE,DELTRK          REMOVE OLD LINE                               
         BAS   RE,VALTRK          VALIDATE ALL TRACK FIELDS                     
         J     XIT                AND SAVE ELEMENT                              
                                                                                
***********************************************************************         
*        IF ANY FIELD ON THE LINE IS POPULATED AND THE LINE WAS       *         
*        POPULATED PREVIOUSLY BY A VALIDATED TRACK AND THE AGENCY-    *         
*        CONTRACT-TRACK FIELDS ARE NOW ALL EMPTY ...                  *         
***********************************************************************         
                                                                                
VAC40    GOTO1 FLDVAL,DMCB,(X'80',TAGYH),TTRKH                                  
         JNE   VAC50                                                            
         BAS   RE,DELTRK          REMOVE OLD TRACK LINE                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        IF ANY FIELD ON THE LINE IS POPULATED AND THE LINE WAS       *         
*        POPULATED PREVIOUSLY BY A VALIDATED TRACK AND SOME BUT NOT   *         
*        ALL OF THE AGENCY-CONTRACT-TRACKS FIELDS ARE NOW EMPTY ...   *         
***********************************************************************         
                                                                                
VAC50    CLI   TAGYH+5,0          GIVE MISSING INPUT FIELD MESSAGE              
         JE    ERMIS                                                            
         CLI   TCNTH+5,0                                                        
         JE    VACCMIS                                                          
         J     VACTMIS                                                          
                                                                                
***********************************************************************         
*        IF ANY FIELD ON THE LINE IS POPULATED AND THE LINE WAS       *         
*        POPULATED PREVIOUSLY BY AN UNVALIDATED TRACK AND THE AGENCY  *         
*        FIELD IS POPULATED ...                                       *         
***********************************************************************         
                                                                                
VAC60    OC    ATAMCEL,ATAMCEL                                                  
         JZ    VAC80                                                            
         CLI   TAGYH+5,0                                                        
         JE    VAC70                                                            
         CLI   TCNTH+5,0          ENSURE THAT CONTRACT                          
         JE    VACCMIS                                                          
         CLI   TTRKH+5,0          AND TRACK FIELD ARE POPULATED                 
         JE    VACTMIS                                                          
         BAS   RE,TRKUNIQ         ENSURE TRACK IS UNIQUE                        
         BAS   RE,DELTRK          REMOVE OLD TRACK LINE                         
         BAS   RE,VALTRK          VALIDATE ALL TRACK FIELDS                     
         J     XIT                AND SAVE ELEMENT                              
                                                                                
***********************************************************************         
*        IF ANY FIELD ON THE LINE IS POPULATED AND THE LINE WAS       *         
*        POPULATED PREVIOUSLY BY AN UNVALIDATED TRACK AND THE AGENCY  *         
*        FIELD IS NOT POPULATED ....                                  *         
***********************************************************************         
                                                                                
VAC70    BAS   RE,DELTRK          REMOVE OLD TRACK LINE                         
         BAS   RE,VALTLINE        VALIDATE ALL TRACK FIELDS                     
         J     XIT                AND SAVE ELEMENT                              
                                                                                
***********************************************************************         
*        IF ANY FIELD ON THE LINE IS POPULATED AND THE LINE WAS NOT   *         
*        POPULATED PREVIOUSLY ...                                               
***********************************************************************         
                                                                                
VAC80    CLI   TAGYH+5,0          ENSURE THAT AGENCY                            
         JE    ERMIS                                                            
         CLI   TCNTH+5,0          CONTRACT                                      
         JE    VACCMIS                                                          
         CLI   TTRKH+5,0          AND TRACK FIELDS ARE ALL POPULATED            
         JE    VACTMIS                                                          
         BAS   RE,TRKUNIQ         ENSURE TRACK IS UNIQUE                        
         BAS   RE,VALTRK          VALIDATE ALL TRACK FIELDS                     
         J     XIT                AND SAVE ELEMENT                              
                                                                                
***********************************************************************         
*        IF NO FIELDS ON THE LINE ARE POPULATED AND THE LINE WAS      *         
*        POPULATED PREVIOUSLY BY AN UNVALIDATED TRACK ...             *         
***********************************************************************         
                                                                                
VAC90    OC    ATAMCEL,ATAMCEL                                                  
         JZ    XIT                                                              
         BAS   RE,DELTRK          REMOVE OLD TRACK LINE                         
         J     XIT                                                              
                                                                                
***********************************************************************         
                                                                                
VACCINV  LA    R2,TCNTH                                                         
         J     ERINV                                                            
                                                                                
VACCMIS  LA    R2,TCNTH                                                         
         J     ERMIS                                                            
                                                                                
VACT497  LA    R2,TTRKH                                                         
         J     ERM497                                                           
                                                                                
VACTMIS  LA    R2,TTRKH                                                         
         J     ERMIS                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT TRACK AGENCY, CONTRACT AND LETTER       *         
*        ARE UNIQUE                                                   *         
*        ON ENTRY ... R2 = A(TRACK SCREEN LINE)                       *         
***********************************************************************         
                                                                                
         USING TRACKD,R2                                                        
TRKUNIQ  NTR1                                                                   
         LA    R3,TAGYH                                                         
         LA    R4,TCNTH                                                         
         LA    R2,TTRKH                                                         
         DROP  R2                                                               
                                                                                
         USING TRACKD,RE                                                        
         LA    RE,SC2TAGYH         RE=A(1ST MUSIC TRACK FIELD)                  
                                                                                
TU10     CR    RE,R3                                                            
         JNL   XIT                                                              
         CLC   TAGY,8(R3)                                                       
         JNE   TU20                                                             
         CLC   TCNT,8(R4)                                                       
         JNE   TU20                                                             
         CLC   TTRK,8(R2)          ENSURE TRACK IS UNIQUE                       
         JE    ERM476                                                           
TU20     LA    RE,TLNQ(RE)                                                      
         J     TU10                                                             
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES ALL TRACK FIELDS AND ADD IT TO             *         
*        COMMERCIAL OR VERSION IN AIO1                                *         
*        ON ENTRY ... R2 = A(1ST FIELD OF MUSIC TRACK LINE)           *         
*                     R3 = A(KEY)                                     *         
***********************************************************************         
                                                                                
         USING TRACKD,R2                                                        
VALTRK   NTR1                                                                   
         USING TLAYD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLAYCD,TLAYCDQ     ENSURE AGENCY EXISTS                          
         MVC   TLAYAGY,TAGY       EXISTS                                        
         GOTO1 HIGH                                                             
         CLC   TLAYKEY,KEYSAVE                                                  
         JNE   ERM482                                                           
         DROP  R3                                                               
                                                                                
         USING TLCOPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOICDQ                                                 
         MVC   TLCOIAGY,TAGY      ENSURE AFM CONTRACT EXISTS                    
         MVC   TLCOICID,TCNT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLCOICOM-TLCOPD),KEYSAVE                                     
         JNE   VT10                                                             
         CLI   TLCOIVER,0                                                       
         JE    VT20                                                             
VT10     LA    R2,TCNTH                                                         
         J     ERM483                                                           
                                                                                
         USING TLISTD,RE                                                        
VT20     LA    RE,OTTAB          IF TRACK IS NOT REALLY BEING                   
VT30     CLI   0(RE),X'FF'       ADDED, JUST MOVED, GO AHEAD                    
         JE    VT50              AND ADD ELEMENT WITHOUT                        
         CLC   TLCOM,TLCOICOM    ANY FURTHER VALIDATION                         
         JNE   VT40                                                             
         CLC   TLTRK,TTRK                                                       
         JE    VT110                                                            
VT40     LA    RE,TLLNQ(RE)                                                     
         J     VT30                                                             
         DROP  R3,RE                                                            
                                                                                
VT50     MVC   AIO,AIO3                                                         
         GOTO1 GETREC             READ AFM CONTRACT INTO AIO3                   
         MVC   AIO,AIO1                                                         
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACOELQ     ENSURE THAT COMMERCIAL TYPE                   
         BRAS  RE,GETEL           IS MUSIC                                      
         JE    *+6                                                              
         DC    H'00'                                                            
         CLI   TACOTYPE,CTYMUS                                                  
         JNE   VACCINV                                                          
         DROP  R4                                                               
                                                                                
         USING TAMCD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAMCELQ                                                   
         BRAS  RE,GETEL           ENSURE THAT TRACK EXISTS                      
         J     *+8                ON THE AFM CONTRACT                           
VT60     BRAS  RE,NEXTEL                                                        
         JE    VT70                                                             
         LA    R2,TTRKH                                                         
         J     ERM484                                                           
VT70     CLC   TAMCTRK,TTRK                                                     
         JNE   VT60                                                             
                                                                                
         TM    TAMCSTAT,TAMCSNEW  ENSURE THAT TRACK CONFORMS                    
         JO    VT80               TO NEW MUSIC RULES                            
         LA    R2,TTRKH                                                         
         J     ERM502                                                           
         DROP  R4                                                               
                                                                                
         USING TLCOPD,R4                                                        
VT80     LA    R4,SVKEY           SAVE KEY FOR ITS INTERNAL                     
         MVC   SVKEY,KEY          COMMERCIAL NUMBER                             
                                                                                
         USING TLCAD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLCACD,TLCACDQ     READ ALL MUSICIANS FOR THE                    
         MVC   TLCACOM,TLCOICOM   AFM CONTRACT                                  
         MVI   TLCASORT,X'80'                                                   
         GOTO1 HIGH                                                             
         J     VT100                                                            
VT90     GOTO1 SEQ                                                              
VT100    CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   VACT497                                                          
         DROP  R3,R4                                                            
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC             ENSURE AT LEAST ONE IS ON THE                 
         GOTOR ONTRK,DMCB,TTRK    TRACK                                         
         MVC   AIO,AIO1                                                         
         JNE   VT90                                                             
                                                                                
         USING TLCOPD,R3                                                        
         MVC   KEY,SVKEY          RESTORE COMMERCIAL KEY                        
         GOTO1 ADDTLIST,DMCB,TRKALIST,TLCOICOM,TTRK                             
VT110    BAS   RE,ADDTRK          ADD TRACK TO COMMERCIAL/VERSION               
         J     XIT                RECORD                                        
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
*        ROUTINE ADDS TRACK ELEMENT TO COMMERCIAL                     *         
*        ON ENTRY ... R2 = A(1ST FIELD OF MUSIC TRACK LINE)           *         
*                     R3 = A(VALIDATED TYPE M COMMERCIAL KEY)         *         
***********************************************************************         
                                                                                
         USING TRACKD,R2                                                        
         USING TLCOPD,R3                                                        
ADDTRK   NTR1                                                                   
         USING TATRD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TATREL,TATRELQ                                                   
         MVI   TATRLEN,TATRLNQ                                                  
         STC   R0,TATRSEQ                                                       
         MVC   TATRCOM,TLCOICOM                                                 
         MVC   TATRTRK,TTRK                                                     
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R2,R3,R4                                                         
                                                                                
***********************************************************************         
*        ROUTINE ADDS INTERNAL COMMERCIAL NUMBER/TRACK ENTRY TO LIST  *         
*        ON ENTRY ... P1 = A(LIST TO APPEND TO)                       *         
*                     P2 = A(INTERNAL COMMERCIAL NUMBER)              *         
*                     P3 = A(TRACK)                                   *         
***********************************************************************         
                                                                                
ADDTLIST NTR1                                                                   
         USING TLISTD,RE                                                        
         L     RE,0(R1)                                                         
ATL10    OC    TLCOM,TLCOM                                                      
         JZ    ATL20                                                            
         LA    RE,TLLNQ(RE)                                                     
         J     ATL10                                                            
ATL20    L     RF,4(R1)                                                         
         MVC   TLCOM,0(RF)                                                      
         L     RF,8(R1)                                                         
         MVC   TLTRK,0(RF)                                                      
         J     XIT                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        ROUTINE SAVES ADDRESS OF MUSIC CONTRACT DETAILS ELEMENT      *         
*        ON ENTRY ... R0 = MUSIC TRACK LINE SEQUENCE NUMBER           *         
***********************************************************************         
                                                                                
SVATAMC  NTR1                                                                   
         USING TAMCD,R4                                                         
         XC    ATAMCEL,ATAMCEL                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TAMCELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
SAMC10   BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         ZIC   RE,TAMCSEQ                                                       
         CR    R0,RE                                                            
         JNE   SAMC10                                                           
         ST    R4,ATAMCEL                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE ENSURES AGENCY-CONTRACT-TRACK FIELDS HAVE NOT BEEN   *         
*        CHANGED FOR COMMERCIAL/VERSION WITH WEB APPLICATION STAMP    *         
*        ON ENTRY ... R2 = A(1ST FIELD OF MUSIC TRACK LINE)           *         
***********************************************************************         
                                                                                
VALTRKW  NTR1                                                                   
         USING TRACKD,R3                                                        
         LR    R3,R2                                                            
         GOTOR VALWEB,DMCB,(X'C0',0)                                            
                                                                                
         LA    R2,TCNTH                                                         
         GOTOR VALWEB,DMCB,(X'C0',0)                                            
                                                                                
         LA    R2,TTRKH                                                         
         GOTOR VALWEB,DMCB,(X'C0',0)                                            
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE DETERMINES IF MUSICIAN IS ON THE PROVIDED TRACK      *         
*        ON ENTRY ... P1=A(TRACK LETTER)                              *         
*                     AIO=A(CAST RECORD)                              *         
***********************************************************************         
                                                                                
ONTRK    NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTTRK))                                     
         JNE   NO                                                               
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         CLI   TAFNNAME,C'*'                                                    
         JE    YES                                                              
         LA    R1,TAFNNAME                                                      
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,3                                                             
OT10     CLC   0(1,R2),0(R1)                                                    
         JE    YES                                                              
         LA    R1,1(R1)                                                         
         BCT   RE,OT10                                                          
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        LITERALS FOR ONTRK ROUTINE                                   *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAMQHFR                                                        
**********************************************************************          
*        SAVED STORAGE                                               *          
**********************************************************************          
                                                                                
       ++INCLUDE TAGENFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRF8D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR18D                                                       
                                                                                
         DS    2D                                                               
                                                                                
ASVPTRS  DS    A                   A(SAVED POINTER BLOCK)                       
AUPPTRS  DS    A                   A(UPDATED POINTER BLOCK)                     
         ORG   AUPPTRS                                                          
AGACTAB  DS    A                   A(GUARANTEE AGENCY/CLIENT TABLE)             
                                                                                
ASTATAB  DS    A                   A(STATUS TABLE)                              
AMTYTAB  DS    A                   A(MUSIC TYPE TABLE)                          
ASTYTAB  DS    A                   A(SESSION TYPE TABLE)                        
AINDTAB  DS    A                   A(INDEXED COMMERCIAL TABLE)                  
AUVCTAB  DS    A                   A(UNVERIFY CAST FIELD)                       
ARHFTAB  DS    A                   A(REISSUE HOLDING FEE FIELD TABLE)           
                                                                                
PROSTAT  DS    XL1                 PROGRAM STATUS                               
PSADDIND EQU   X'80'               ADDING INDEXED COMMERCIAL                    
PSCO2DIS EQU   X'40'               COM2 INFO PREVIOUSLY DISPLAYED               
PSROVAP  EQU   X'20'               MEDIA/TYPE CHG REMOVED OV AMT/PCT            
PSCSTADD EQU   X'10'               CAST ADDED DUE TO CHANGE                     
PSWEBREM EQU   X'08'               WEB APPLICATION ID BEING REMOVED             
                                                                                
TACOELEM DS    XL(TACOLNQ3)        COMMERCIAL DETAILS ELEMENT                   
SVNUNXTC DS    XL(L'TANUNXTC)      NEXT CAST SEQ NUMBER                         
                                                                                
AYSTAT   DS    XL(L'TAAYSTAT)      SAVED AGENCY STATUS BYTES                    
AYSTAT3  DS    XL(L'TAAYSTA3)                                                   
AYSTAT5  DS    XL(L'TAAYSTA5)                                                   
AYSTAT6  DS    XL(L'TAAYSTA6)                                                   
AYBRSTAT DS    XL(L'TABRSTAT)                                                   
                                                                                
SVMED    DS    CL(L'TACOMED)       SAVED MEDIA                                  
SVSTAT   DS    CL(L'TACOSTAT)      SAVED STATUS                                 
SVPTYPE  DS    CL(L'TACOTYPE)      SAVED PRIMARY COMMERCIAL TYPE                
SVCOSTAT DS    XL(L'TACOSTAT)      SAVED COMMERCIAL STATUS                      
SVCOSTA2 DS    XL(L'TACOSTA2)      SAVED SECOND COMMERCIAL STATUS               
SVWID    DS    CL18                SAVED WEB APPLICATION ID                     
SVOWID   DS    CL18                SAVED ORIGINAL WEB APPLICATION ID            
SVACCDTE DS    XL(L'TAACCDTE)      SAVED LAST CHANGED DATE                      
SVACCTIM DS    XL(L'TAACCTIM)      SAVED LAST CHANGED TIME                      
                                                                                
SVCOKEY  DS    CL(L'TLCOKEY)       SAVED COMMERCIAL KEY                         
SVKEY    DS    CL(L'KEY)           SAVED KEY                                    
SVPCKEY  DS    CL(L'KEY)           SAVED PRIMARY CAST KEY                       
SVGCKEY  DS    CL(L'KEY)           SAVED GUARANTEE CAST KEY                     
                                                                                
ATACOEL  DS    A                   A(COMMERCIAL DETAILS ELEMENT)                
ATALFEL  DS    A                   A(LIFT DETAILS ELEMENT)                      
ATAMCEL  DS    A                   A(MUSIC CONTRACT DETAILS ELEMENT)            
ATATREL  DS    A                   A(MUSIC CONTRACT/TRACK ELEMENT)              
                                                                                
ACASTREC DS    A                   A(CAST RECORD)                               
OLENGTH  DS    X                   ORIGINAL ELEMENT LENGTH                      
                                                                                
ADDGAC   DS    XL(GACLNQ+1)        NEW AGENCY/CLIENT LIMITATION                 
                                                                                
ITTAB    DS    XL(TLLNQ*7+1)       INPUT TRACK TABLE                            
OTTAB    DS    XL(TLLNQ*7+1)       ORIGINAL TRACK TABLE                         
TRKALIST DS    XL(TLLNQ*7+1)       ADDED TRACKS LIST                            
TRKDLIST DS    XL(TLLNQ*7+1)       DELETED TRACKS LIST                          
CTRKALST DS    XL(TLLNQ*7+1)       CAST SPECIFIC ADDED TRACKS LIST              
THISCOM  DS    XL4                                                              
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
*        DSECT FOR INDEXED VERSION TABLE                             *          
**********************************************************************          
                                                                                
INDTABD  DSECT                                                                  
ITUPLM   DS    X                 RECORD'S UPPER LIMIT                           
ITEQUT   DS    X                 RECORD'S EQUATE                                
ITLNQ    EQU   *-INDTABD                                                        
                                                                                
**********************************************************************          
*        DSECT FOR GUARANTEE'S AGENCY/CLIENT TABLE                   *          
**********************************************************************          
                                                                                
GACTABD  DSECT                                                                  
GACAGY   DS    CL6                                                              
GACCLI   DS    CL6                                                              
GACLNQ   EQU   *-GACTABD                                                        
GACTLNQ  EQU   (GACLNQ*100)+1                                                   
                                                                                
**********************************************************************          
*        DSECT FOR MUSIC TRACK SCREEN FIELDS                         *          
**********************************************************************          
                                                                                
TRACKD   DSECT                                                                  
TAGYH    DS    XL8                                                              
TAGY     DS    CL(L'SC2TAGY)                                                    
TCNTH    DS    XL8                                                              
TCNT     DS    CL(L'SC2TCNT)                                                    
TTRKH    DS    XL8                                                              
TTRK     DS    CL(L'SC2TTRK)                                                    
TLFTH    DS    XL8                                                              
TLFT     DS    CL(L'SC2TLFT)                                                    
TLENH    DS    XL8                                                              
TLEN     DS    CL(L'SC2TLEN)                                                    
TTYPH    DS    XL8                                                              
TTYP     DS    CL(L'SC2TTYP)                                                    
TTITH    DS    XL8                                                              
TTIT     DS    CL(L'SC2TTIT)                                                    
TLNQ     EQU   *-TRACKD                                                         
                                                                                
**********************************************************************          
*        DSECT FOR TRACK ADD/DELETE LISTS                            *          
**********************************************************************          
                                                                                
TLISTD   DSECT                                                                  
TLCOM    DS    XL4                                                              
TLTRK    DS    XL1                                                              
TLLNQ    EQU   *-TLISTD                                                         
                                                                                
**********************************************************************          
*        DSECT FOR NMOD STORAGE GRAB AREA                            *          
**********************************************************************          
                                                                                
TMPD     DSECT                                                                  
SVPTRBLK DS    CL((520*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS                      
UPPTRBLK DS    CL((520*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS FOR ADDPTRS          
TMPLNQ   EQU   *-TMPD                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013TAGEN18   12/01/16'                                      
         END                                                                    
