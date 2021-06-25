*          DATA SET TAREP72    AT LEVEL 003 AS OF 08/14/13                      
*PHASE T70372E,*                                                                
*INCLUDE DLFLD                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'T70372 - CITYRES REPORT'                                        
T70302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70302,R6                                                      
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         BRAS  RE,VOPT             VALIDATION OPTIONS                           
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   YES                                                              
         BRAS  RE,PREP             PRINT REPORT                                 
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OPTIONS                                             *         
***********************************************************************         
                                                                                
VOPT     NTR1  BASE=*,LABEL=*                                                   
         MVI   PROSTAT,0                                                        
         MVI   ADHOCFLG,0                                                       
                                                                                
         LA    R2,SPLOPTH                                                       
         CLI   SPLOPTH+5,0                                                      
         JE    MISSERR                                                          
                                                                                
         USING SCAND,R3                                                         
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3),0                                         
         CLI   4(R1),0                                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
         ZIC   R0,4(R1)                                                         
                                                                                
VOPT10   CLC   =C'TRACE',SCDATA1                                                
         JNE   VOPT15                                                           
         CLI   SCDATA2,C'N'                                                     
         JE    VOPT100                                                          
         CLI   SCDATA2,C'Y'                                                     
         JNE   INVERR                                                           
         OI    PROSTAT,PSTRACE                                                  
                                                                                
VOPT15   CLC   =C'NYC',SCDATA1                                                  
         JNE   *+12                                                             
         OI    ADHOCFLG,AHNYC                                                   
         J     VOPT100                                                          
                                                                                
         CLC   =C'PHL',SCDATA1                                                  
         JNE   INVERR                                                           
         OI    ADHOCFLG,AHPHL                                                   
         J     VOPT100                                                          
                                                                                
VOPT100  LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT10                                                        
                                                                                
VOPTX    J     XIT                                                              
         DROP  R3                                                               
                                                                                
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
                                                                                
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
                                                                                
PREP     NTR1  BASE=*,LABEL=*                                                   
         TM    ADHOCFLG,AHNYC                                                   
         BZ    *+12                                                             
         BRAS  RE,W4                                                            
         NI    ADHOCFLG,X'FF'-AHNYC                                             
                                                                                
         TM    ADHOCFLG,AHPHL                                                   
         BZ    *+12                                                             
         BRAS  RE,W4                                                            
         NI    ADHOCFLG,X'FF'-AHPHL                                             
                                                                                
         J     XIT                                                              
         LTORG                                                                  
***********************************************************************         
*        NYC/PHL W4 ADHOC                                             *         
***********************************************************************         
                                                                                
W4       NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BRAS  RE,INITDOWN         INITIALIZE DOWNLOAD                          
*                                                                               
         BAS   RE,W4HDR                                                         
*                                                                               
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
*                                                                               
         MVC   TGTHREE,=C'NYC'                                                  
         TM    ADHOCFLG,AHNYC                                                   
         BO    *+10                                                             
         MVC   TGTHREE,=C'PHL'                                                  
*                                                                               
         LA    R3,KEY                                                           
         USING TLW4D,R3                                                         
         XC    TLW4KEY,TLW4KEY                                                  
         MVI   TLW4CD,TLW4CDQ                                                   
*                                                                               
         GOTO1 HIGH                                                             
         J     W410                                                             
W4SEQ    GOTO1 SEQ                                                              
W410     LA    R3,KEY                                                           
         CLI   TLW4CD,TLW4CDQ                                                   
         JNE   XIT                                                              
         OC    TLW4CD+1(20),TLW4CD+1                                            
         JNZ   W4SEQ                                                            
         DROP  R3                                                               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         USING TLW4D,R4                                                         
         CLI   TLW4STA2,TAW4TYCO   SKIP CORPORATIONS                            
         JE    W4SEQ                                                            
*                                                                               
         MVC   SVSSN,TLW4SSN       SAVE SSN                                     
         MVC   W4KEY,KEY           SAVE W4 KEY                                  
*                                                                               
         L     R4,AIO                                                           
         USING TAA2D,R4                                                         
         MVI   ELCODE,TAA2ELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   W4SEQ                                                            
*                                                                               
         TM    ADHOCFLG,AHNYC                                                   
         BO    W415                                                             
*                                                                               
         CLC   =C'191',TAA2ZIP     PHL                                          
         JE    W420                                                             
         CLC   =C'19019',TAA2ZIP                                                
         JE    W420                                                             
         CLC   =C'19092',TAA2ZIP                                                
         JE    W420                                                             
         CLC   =C'19093',TAA2ZIP                                                
         JE    W420                                                             
         CLC   =C'19099',TAA2ZIP                                                
         JE    W420                                                             
         CLC   =C'19244',TAA2ZIP                                                
         JE    W420                                                             
         CLC   =C'19255',TAA2ZIP                                                
         JNE   W4SEQ                                                            
         J     W420                                                             
*                                                                               
W415     CLC   =C'100',TAA2ZIP     NYC                                          
         JE    W420                                                             
         CLC   =C'101',TAA2ZIP                                                  
         JE    W420                                                             
         CLC   =C'102',TAA2ZIP                                                  
         JE    W420                                                             
         CLC   =C'103',TAA2ZIP                                                  
         JE    W420                                                             
         CLC   =C'104',TAA2ZIP                                                  
         JE    W420                                                             
         CLC   =C'110',TAA2ZIP                                                  
         JE    W420                                                             
         CLC   =C'111',TAA2ZIP                                                  
         JE    W420                                                             
         CLC   =C'112',TAA2ZIP                                                  
         JE    W420                                                             
         CLC   =C'113',TAA2ZIP                                                  
         JE    W420                                                             
         CLC   =C'114',TAA2ZIP                                                  
         JE    W420                                                             
         CLC   =C'116',TAA2ZIP                                                  
         JNE   W4SEQ                                                            
*                                                                               
W420     MVC   SVZIP,TAA2ZIP                                                    
         MVC   SVCITY,=C'   '                                                   
         MVC   SVFEDS,=C'  '                                                    
         MVC   SVRESS,=C'  '                                                    
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAWHELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   W4SEQ                                                            
         J     *+8                                                              
W425     BRAS  RE,NEXTEL                                                        
         JNE   W450                                                             
         USING TAWHD,R4                                                         
*                                                                               
         CLC   =C'FD',TAWHUNIT                                                  
         JE    W425                                                             
*                                                                               
         CLI   TAWHUNIT+2,C' '                                                  
         JNE   *+14                                                             
         MVC   SVRESS,TAWHUNIT                                                  
         J     W425                                                             
*                                                                               
         MVC   SVCITY,TAWHUNIT                                                  
*                                                                               
         CLC   TAWHUNIT,TGTHREE                                                 
         JNE   W425                                                             
         J     W4SEQ                                                            
*                                                                               
W450     MVC   SVCSSN,=C'         '                                             
         L     R4,AIO                                                           
         USING TATID,R4                                                         
         MVI   ELCODE,TATIELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   W460                                                             
         J     *+8                                                              
W455     BRAS  RE,NEXTEL                                                        
         JNE   W460                                                             
*                                                                               
         CLI   TATITYPE,TATITYCO                                                
         JNE   W455                                                             
         MVC   SVCSSN,TATIID                                                    
*                                                                               
W460     L     R4,AIO                                                           
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 OUTPDOWN,DMCB,(C'T',SVSSN),L'SVSSN                               
         GOTO1 SSNPACK,DMCB,SVSSN,TGPID                                         
         GOTO1 OUTPDOWN,DMCB,(C'T',TGPID),L'TGPID                               
         GOTO1 OUTPDOWN,DMCB,(C'T',TAW4NAM2),L'TAW4NAM2                         
         GOTO1 OUTPDOWN,DMCB,(C'T',TAW4NAM1),L'TAW4NAM1                         
         GOTO1 OUTPDOWN,DMCB,(C'T',TAW4TYPE),L'TAW4TYPE                         
         GOTO1 OUTPDOWN,DMCB,(C'T',SVZIP),L'SVZIP                               
         GOTO1 OUTPDOWN,DMCB,(C'T',SVCITY),L'SVCITY                             
         GOTO1 OUTPDOWN,DMCB,(C'T',SVRESS),L'SVRESS                             
*                                                                               
         CLI   SVCSSN,C' '         ATTACHED TO CORP?                            
         JE    W4100                                                            
         MVC   SVCST,=C'  '                                                     
*                                                                               
         LA    R3,KEY              GET CORP W4 INFO                             
         USING TLW4D,R3                                                         
         XC    TLW4KEY,TLW4KEY                                                  
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,SVCSSN                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLW4KEY),KEYSAVE                                           
         JE    W470                                                             
*                                                                               
         GOTO1 OUTPDOWN,DMCB,(C'T',SVCSSN),L'SVCSSN                             
         GOTO1 SSNPACK,DMCB,SVCSSN,TGPID                                        
         GOTO1 OUTPDOWN,DMCB,(C'T',TGPID),L'TGPID                               
         MVC   FULL,=C'DNE '                                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',FULL),L'FULL                                 
         J     W4100                                                            
*                                                                               
W470     L     R4,AIO                                                           
         USING TAA2D,R4                                                         
         MVI   ELCODE,TAA2ELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   W490                                                             
*                                                                               
         MVC   SVCST,TAA2ST        CORP ADDRESS STATE                           
*                                                                               
W490     GOTO1 OUTPDOWN,DMCB,(C'T',SVCSSN),L'SVCSSN                             
         GOTO1 SSNPACK,DMCB,SVCSSN,TGPID                                        
         GOTO1 OUTPDOWN,DMCB,(C'T',TGPID),L'TGPID                               
         GOTO1 OUTPDOWN,DMCB,(C'T',SVCST),L'SVCST                               
*&&DO                                                                           
         L     R4,AIO                                                           
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                CORP NAME                                    
*                                                                               
         GOTO1 OUTPDOWN,DMCB,(C'T',TAW4NAM2),L'TAW4NAM2                         
         GOTO1 OUTPDOWN,DMCB,(C'T',TAW4NAM1),L'TAW4NAM1                         
*&&                                                                             
W4100    BRAS  RE,EOLDOWN                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'W4KEY),W4KEY                                               
         GOTO1 HIGH                                                             
         J     W4SEQ                                                            
                                                                                
W4HDR    NTR1                                                                   
         GOTO1 OUTPDOWN,DMCB,(C'T',=C'SSN'),3                                   
         GOTO1 OUTPDOWN,DMCB,(C'T',=C'PID'),3                                   
         GOTO1 OUTPDOWN,DMCB,(C'T',=C'FIRST'),5                                 
         GOTO1 OUTPDOWN,DMCB,(C'T',=C'LAST'),4                                  
         GOTO1 OUTPDOWN,DMCB,(C'T',=C'W4 TYPE'),7                               
         GOTO1 OUTPDOWN,DMCB,(C'T',=C'ZIP'),3                                   
         GOTO1 OUTPDOWN,DMCB,(C'T',=C'CITY'),4                                  
         GOTO1 OUTPDOWN,DMCB,(C'T',=C'TAX STATE'),9                             
         GOTO1 OUTPDOWN,DMCB,(C'T',=C'CORP SSN'),8                              
         GOTO1 OUTPDOWN,DMCB,(C'T',=C'CORP PID'),8                              
         GOTO1 OUTPDOWN,DMCB,(C'T',=C'CORP STATE'),10                           
         BRAS  RE,EOLDOWN                                                       
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TRACES RECORD GETS/PUTS/ADDS                         *         
*        ON ENTRY ... P1 = A(TRACE LABEL)                             *         
***********************************************************************         
                                                                                
PTRACE   NTR1  BASE=*,LABEL=*                                                   
         TM    PROSTAT,PSTRACE                                                  
         JZ    XIT                                                              
                                                                                
         L     R2,0(R1)                                                         
                                                                                
         MVI   FORCEHED,C'N'                                                    
         GOTO1 TRACE,DMCB,AIO,0,0(R2),(0,6)                                     
         MVI   FORCEHED,C'Y'                                                    
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
INITDOWN NTR1  BASE=*,LABEL=*                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         LTORG                                                                  
                                                                                
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1  BASE=*,LABEL=*                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1  BASE=*,LABEL=*                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         LTORG                                                                  
                                                                                
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1  BASE=*,LABEL=*                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
         LTORG                                                                  
                                                                                
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1  BASE=*,LABEL=*                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
AGYHEAD  DC    C'AGY'                                                           
CODHEAD  DC    C'CODE'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
       ++INCLUDE TASYSCATS                                                      
                                                                                
MYD      DSECT                                                                  
PROSTAT  DS    X                                                                
PSTRACE  EQU   X'80'                                                            
*                                                                               
ADHOCFLG DS    X                   ADHOC REPORTS                                
AHNYC    EQU   X'80'               NYC W4                                       
AHPHL    EQU   X'40'               PHL W4                                       
*                                                                               
W4KEY    DS    XL32                                                             
YTDWAMT  DS    F                   WGA                                          
YTDAAMT  DS    F                   AFT                                          
SVSSN    DS    CL9                                                              
SVCSSN   DS    CL9                                                              
SVCST    DS    CL2                                                              
CURSSN   DS    CL9                                                              
SVZIP    DS    CL10                                                             
SVCITY   DS    CL3                                                              
SVRESS   DS    CL2                                                              
SVFEDS   DS    CL2                                                              
*                                                                               
TMPINV   DS    CL6                                                              
TMPFEE   DS    CL10                                                             
ECVTRATE DS    F                                                                
*                                                                               
CHKSSN   DS    CL(6*L'TLW4SSN+1)   TABLE OF CORPORATION SSN'S                   
*                                                                               
MYDLNQ   EQU   *-MYD                                                            
                                                                                
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAREP72   08/14/13'                                      
         END                                                                    
