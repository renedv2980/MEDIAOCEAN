*          DATA SET ACPRO22    AT LEVEL 017 AS OF 06/25/04                      
*PHASE T60B22A                                                                  
         TITLE 'T60B22 - PRICE RECORD REPORT'                                   
T60B22   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B22,RA,RR=R2                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VKEY     NTR1                                                                   
         XC    REQOG(REQLNG),REQOG                                              
         MVC   PROOGRN,SPACES                                                   
         OI    PROOGRNH+6,X'80'                                                 
         MVC   PROOFFN,SPACES                                                   
         OI    PROOFFNH+6,X'80'                                                 
         MVC   PROCLIN,SPACES                                                   
         OI    PROCLINH+6,X'80'                                                 
         MVC   PROPRON,SPACES                                                   
         OI    PROPRONH+6,X'80'                                                 
         MVC   PROJOBN,SPACES                                                   
         OI    PROJOBNH+6,X'80'                                                 
         MVC   PROMGRN,SPACES                                                   
         OI    PROMGRNH+6,X'80'                                                 
         MVC   PROMEDN,SPACES                                                   
         OI    PROMEDNH+6,X'80'                                                 
         MVC   PROWGRN,SPACES                                                   
         OI    PROWGRNH+6,X'80'                                                 
         MVC   PROWRKN,SPACES                                                   
         OI    PROWRKNH+6,X'80'                                                 
*                                                                               
         MVI   OPTION,C'Y'                                                      
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR             NEED KEY LENGTHS                             
*                                                                               
         LA    R2,PROOGRH          OFFICE GROUP                                 
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         GOTO1 VALOG                                                            
         MVC   REQOG,EFFOFG                                                     
*                                                                               
VKEY2    LA    R2,PROOFFH          OFFICE                                       
         CLI   5(R2),0                                                          
         BE    VKEY4                                                            
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALOFF                                                           
         MVC   REQOFF,EFFOFFC                                                   
*                                                                               
VKEY4    LA    R2,PROCLIH          CLIENT OPTIONAL                              
         CLI   5(R2),0                                                          
         BE    VKEY6                                                            
         MVI   ERROR,NOTCLNOG                                                   
         CLI   PROOGRH+5,0         NOT COMPATIBLE WITH OFFICE GROUP             
         BNE   ERREND                                                           
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   PROOFFH+5,0                                                      
         BNE   ERREND                                                           
         ZIC   R3,LCLI                                                          
         GOTO1 VALCLI                                                           
         MVC   REQCLI,CLICODE                                                   
*                                                                               
VKEY6    LA    R2,PROPROH          PRODUCT OPTIONAL, UNLESS 'SOON'              
         CLI   5(R2),0                                                          
         BE    VKEY8                                                            
         LA    R2,PROCLIH                                                       
         MVI   ERROR,NEEDCLI       NEED CLIENT IF INPUT                         
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         LA    R2,PROPROH                                                       
         ZIC   R3,LCLIPRO                                                       
         GOTO1 VALPROD                                                          
         MVC   REQPRO,PRODCODE                                                  
*                                                                               
VKEY8    LA    R2,PROJOBH          JOB OPTIONAL                                 
         CLI   5(R2),0                                                          
         BE    VKEY12                                                           
         LA    R2,PROPROH                                                       
         MVI   ERROR,NEEDPRO       NEED PRODUCT IF INPUT                        
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         LA    R2,PROJOBH                                                       
         GOTO1 VALJOB                                                           
         MVC   REQJOB,JOBNUM                                                    
*                                                                               
VKEY12   LA    R2,PROMGRH          MEDIA GROUP                                  
         CLI   5(R2),0                                                          
         BE    VKEY14                                                           
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   PROJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMG                                                            
         MVC   REQMEDG,MGROUP                                                   
*                                                                               
VKEY14   LA    R2,PROMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    VKEY16                                                           
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   PROMGRH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   PROJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMED                                                           
         MVC   REQMED,MEDIA                                                     
*                                                                               
VKEY16   LA    R2,PROWGRH          WORK GROUP                                   
         CLI   5(R2),0                                                          
         BE    VKEY18                                                           
         GOTO1 VALWG                                                            
         MVC   REQWRKG,WGROUP                                                   
*                                                                               
VKEY18   LA    R2,PROWRKH          WORK CODE                                    
         CLI   5(R2),0                                                          
         BE    VKEY20                                                           
         MVI   ERROR,NOTWKNWG      NOT COMPATIBLE WITH WORK GROUP               
         CLI   PROWGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALWORK                                                          
         MVC   REQWRK,WORKCODE                                                  
*                                                                               
VKEY20   LA    R2,PROSDTH          START DATE                                   
         CLI   5(R2),0                                                          
         BE    VKEY30                                                           
         GOTO1 VALIDATE,DMCB,WORK                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,REQSDT)                                  
         GOTO1 DATCON,DMCB,(0,WORK),(8,PROSDT)                                  
         XC    REQSDT,EFFS         STORE COMPLEMENT, AS IN KEY                  
         OI    PROSDTH+6,X'80'     TRANSMIT                                     
*                                                                               
VKEY30   LA    R2,PROEDTH          END DATE                                     
         CLI   5(R2),0                                                          
         BE    VKEYX                                                            
         GOTO1 VALIDATE,DMCB,WORK                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,REQEDT)  CONVERT TO X'YMD'               
         GOTO1 DATCON,DMCB,(0,WORK),(8,PROEDT)                                  
         XC    REQEDT,EFFS         STORE COMPLEMENT, AS IN KEY                  
         OI    PROEDTH+6,X'80'     TRANSMIT                                     
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              PRINT REPORT                                                     
*                                                                               
PREP     NTR1                                                                   
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         BAS   RE,DORAW                                                         
         B     XIT                                                              
         EJECT                                                                  
*              READ AGENCY LEVEL RECORDS                                        
*                                                                               
         USING PRCRECD,R4                                                       
DORAW    NTR1                                                                   
*                                                                               
         GOTO1 VSETNEW                                                          
DR10     BAS   RE,SETKEY                                                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
DR11     BAS   RE,FILTER           FILTER KEY VS. REQUEST                       
         BNE   DR12                                                             
*                                                                               
         BAS   RE,FILTDATE                                                      
         BNE   DR12                                                             
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
         BAS   RE,PROCRAW          PRINT THE PRICE RECORD                       
*                                                                               
DR12     GOTO1 SEQ                                                              
         CLC   PRCKEY(PRCKLDG-PRCKEY+L'PRCKLDG),KEYSAVE SAME CUL                
         BE    DR11                                     YES                     
*                                                                               
DRX      GOTO1 VSETEMU                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILTER A KEY VS. REQUEST VALUES                       
*              ASSUMES R4 IS A(KEY)                                             
         USING PRCRECD,R4                                                       
FILTER   NTR1                                                                   
         LA    R2,PRCKOFG          START WITH OFFICE GROUP                      
         LA    R3,REQOG                                                         
         LA    R5,FLDLENS          LIST OF FIELD LENGTHS TO COMPARE             
         XR    R1,R1                                                            
*                                                                               
FLT10    CLI   0(R5),0             ANY MORE FIELDS TO COMPARE                   
         BE    FLTOK               NO                                           
*                                                                               
         IC    R1,0(R5) LENGTH OF FIELD TO COMPARE                              
*                                                                               
         CLI   0(R3),0             IS THIS REQUEST FIELD DEFINED                
         BE    FLT30               NO                                           
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(R3)       COMPARE KEY FIELD VS REQUEST                 
         BNE   FLTNO              REJECT IF REQUEST DEFINED AND KEY NEQ         
*                                                                               
         LA    R1,1(R1)            RESTORE FIELD LENGTH                         
*                                                                               
FLT30    LA    R2,0(R1,R2)         BUMP TO NEXT KEY FIELD                       
         LA    R3,0(R1,R3)         BUMP TO NEXT REQUEST FIELD                   
         LA    R5,1(R5)            BUMP TO NEXT FIELD LENGTH                    
         B     FLT10                                                            
*                                                                               
FLTOK    CR    RE,RE                                                            
         B     FLTX                                                             
FLTNO    LTR   RE,RE                                                            
FLTX     B     XIT                                                              
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FILTER A KEY FROM THE REPORT                          
*              ASSUMES R4 IS A(KEY)                                             
         USING PRCRECD,R4                                                       
FILTDATE EQU   *                                                                
         CLI   REQSDT,0            IS THERE A START DATE                        
         BE    FDT10                                                            
         CLC   REQSDT,PRCKEFF      COMPARE COMPLEMENTED DATES                   
         BL    FDTNO                                                            
*                                                                               
FDT10    CLI   REQEDT,0                                                         
         BE    FDTOK                                                            
         CLC   REQEDT,PRCKEFF      COMPARE COMPLEMENTED DATES                   
         BH    FDTNO                                                            
FDTOK    CR    RE,RE                                                            
         B     FDTX                                                             
FDTNO    LTR   RE,RE                                                            
FDTX     BR    RE                                                               
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
*                                                                               
PROCRAW  NTR1                                                                   
         USING PRCKEY,R4                                                        
         MVC   P,SPACES            CLEAR THE PRINT LINE                         
         MVC   P+02(1),PRCKOFG                                                  
         MVC   P+06(2),PRCKOFC                                                  
         MVC   P+09(6),PRCKCLI                                                  
         MVC   P+16(6),PRCKPRO                                                  
         MVC   P+24(6),PRCKJOB                                                  
         CLI   PRCKMGR,X'FF'                                                    
         BE    *+10                                                             
         MVC   P+32(1),PRCKMGR                                                  
         MVC   P+36(1),PRCKMED                                                  
         CLI   PRCKWGR,X'FF'                                                    
         BE    *+10                                                             
         MVC   P+40(1),PRCKWGR                                                  
         MVC   P+44(2),PRCKWRK                                                  
         MVC   EFFDATE,PRCKEFF                                                  
         XC    EFFDATE,EFFS                                                     
         GOTO1 DATCON,DMCB,(1,EFFDATE),(8,P+47)                                 
*                                                                               
         MVI   ANYOPT,C'N'                                                      
         MVI   ELCODE,PRCELQ                                                    
         BAS   RE,GETELIO                                                       
         B     PRAW4                                                            
*                                                                               
PRAW2    BAS   RE,NEXTEL                                                        
*                                                                               
PRAW4    BE    PRAW6                                                            
         MVI   ELCODE,PACELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   PRAW5                                                            
         USING PACELD,R6                                                        
         LA    R5,P+108                                                         
         MVC   0(L'PACPERS,R5),PACPERS                                          
         GOTO1 DATCON,DMCB,(1,PACDATE),(8,8(R5))                                
*                                                                               
PRAW5    CLI   ANYOPT,C'Y'                                                      
         BNE   XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         USING PRCELD,R6                                                        
PRAW6    MVI   ANYOPT,C'Y'                                                      
         LA    R5,P+55                                                          
*                                                                               
         USING LND,R5                                                           
PRAW8    MVI   LNDBAS,C'U'         SET DEFAULT BASIS                            
         CURED PRCSUNT,(6,LNDSUNT),0,ALIGN=LEFT,ZERO=NOBLANK                    
         ORG   *-2                                                              
         TM    PRCKSTAT,PRCSQTRH   QUARTER HOURS?                               
         BZ    PRAW10              NO                                           
         MVI   11(R1),2            YES, SET DECIMALS                            
         MVI   LNDBAS,C'H'         CHANGE BASIS                                 
*                                                                               
PRAW10   BASR  RE,RF                                                            
*                                                                               
         CURED PRCEUNT,(6,LNDEUNT),0,ALIGN=LEFT,ZERO=NOBLANK                    
         ORG   *-2                                                              
         TM    PRCKSTAT,PRCSQTRH   QUARTER HOURS?                               
         BZ    *+8                 NO                                           
         MVI   11(R1),2            YES, SET DECIMALS                            
         BASR  RE,RF                                                            
*                                                                               
         CURED PRCPRC1,(8,LNDPRC1),2,ALIGN=RIGHT                                
         CURED PRCPRC2,(8,LNDPRC2),2,ALIGN=RIGHT                                
         CURED PRCPRC3,(8,LNDPRC3),2,ALIGN=RIGHT                                
         CURED PRCPRC4,(8,LNDPRC4),2,ALIGN=RIGHT                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRAW2                                                            
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
*              HEAD HOOK FOR RAW OPTIONS                                        
*                                                                               
HOOK     NTR1                                                                   
         LA    R3,H1+48            DEAL WITH HEADING                            
         MVC   0(34,R3),=CL34'PRICE LIST REPORT'                                
         GOTO1 CENTER,DMCB,(R3),34                                              
         GOTO1 UNDERLIN,DMCB,(34,(R3)),(X'BF',132(R3))                          
*                                                                               
         MVC   H8+1(3),=C'OFG'                                                  
         MVC   H8+5(3),=C'OFF'                                                  
         MVC   H8+9(6),=C'CLIENT'                                               
         MVC   H8+16(7),=C'PRODUCT'                                             
         MVC   H8+24(6),=C' JOB  '                                              
         MVC   H9+24(6),=C'NUMBER'                                              
         MVC   H8+31(3),=C'MGR'                                                 
         MVC   H8+35(3),=C'MED'                                                 
         MVC   H8+39(3),=C'WGR'                                                 
         MVC   H8+43(3),=C'WRK'                                                 
         MVC   H8+47(8),=C'EFFECTVE'                                            
         MVC   H9+48(8),=C'  DATE  '                                            
         MVC   H8+56(1),=C'B'                                                   
         MVC   H8+58(6),=C'START '                                              
         MVC   H9+58(6),=C'UNITS '                                              
         MVC   H8+65(6),=C' END  '                                              
         MVC   H9+65(6),=C'UNITS '                                              
         MVC   H8+72(8),=C'  PRICE '                                            
         MVC   H9+72(8),=C'    A   '                                            
         MVC   H8+81(8),=C'  PRICE '                                            
         MVC   H9+81(8),=C'    B   '                                            
         MVC   H8+90(8),=C'  PRICE '                                            
         MVC   H9+90(8),=C'    C   '                                            
         MVC   H8+99(8),=C'  PRICE '                                            
         MVC   H9+99(8),=C'    D   '                                            
         MVC   H8+108(13),=C'LAST ACTIVITY'                                     
         MVC   H9+108(13),=C' BY       ON '                                     
*                                                                               
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
*                                                                               
         CLI   BOXOPT,C'N'                                                      
         BE    XIT                                                              
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+4,C'C'                                                   
         MVI   BOXCOLS+8,C'C'                                                   
         MVI   BOXCOLS+15,C'C'                                                  
         MVI   BOXCOLS+23,C'C'                                                  
         MVI   BOXCOLS+30,C'C'                                                  
         MVI   BOXCOLS+34,C'C'                                                  
         MVI   BOXCOLS+38,C'C'                                                  
         MVI   BOXCOLS+42,C'C'                                                  
         MVI   BOXCOLS+46,C'C'                                                  
         MVI   BOXCOLS+55,C'C'                                                  
         MVI   BOXCOLS+57,C'C'                                                  
         MVI   BOXCOLS+64,C'C'                                                  
         MVI   BOXCOLS+71,C'C'                                                  
         MVI   BOXCOLS+80,C'C'                                                  
         MVI   BOXCOLS+89,C'C'                                                  
         MVI   BOXCOLS+98,C'C'                                                  
         MVI   BOXCOLS+107,C'C'                                                 
         MVI   BOXCOLS+124,C'R'                                                 
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         B     XIT                                                              
         EJECT                                                                  
         USING PRCRECD,R4                                                       
SETKEY   LA    R4,BIGKEY                                                        
         XC    PRCKEY,PRCKEY                                                    
         MVI   PRCKTYP,PRCKTYPQ                                                 
         MVI   PRCKSUB,PRCKSUBQ                                                 
         MVC   PRCKCPY(3),CUL                                                   
         BR    RE                                                               
         DROP  R4                                                               
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
EFFS     DC    8X'FF'                                                           
*---------------------------------------------------------------------          
*        THE FOLLOWING TABLE IS USED BY THE FILTER ROUTINE TO TEST              
*        KEYS VS. REQUEST VALUES. THE FILTER ROUTINE DEPENDS ON THE             
*        REQUEST FIELDS, THE KEY FIELDS AND THE FIELD LENGTHS DEFINED           
*        BELOW ALL BEING IN THE SAME ORDER. THE LENGTHS ARE USED FOR            
*        AN EXECUTED COMPARE AND TO BUMP TO THE NEXT KEY/REQUEST FIELD          
*---------------------------------------------------------------------          
*                                                                               
FLDLENS  DS    0X                                                               
         DC    AL1(L'PRCKOFG)                                                   
         DC    AL1(L'PRCKOFC)                                                   
         DC    AL1(L'PRCKCLI)                                                   
         DC    AL1(L'PRCKPRO)                                                   
         DC    AL1(L'PRCKJOB)                                                   
         DC    AL1(L'PRCKMGR)                                                   
         DC    AL1(L'PRCKMED)                                                   
         DC    AL1(L'PRCKWGR)                                                   
         DC    AL1(L'PRCKWRK)                                                   
         DC    AL1(0)                                                           
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              SPECS FOR HEADINGS ETC                                           
*                                                                               
MYSPECS  DS    0F                                                               
         SSPEC H1,2,CREATED        SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
LND      DSECT                                                                  
         DS    CL1                                                              
LNDBAS   DS    CL1                                                              
         DS    CL1                                                              
LNDSUNT  DS    CL6                                                              
         DS    CL1                                                              
LNDEUNT  DS    CL6                                                              
         DS    CL1                                                              
LNDPRC1  DS    CL8                                                              
         DS    CL1                                                              
LNDPRC2  DS    CL8                                                              
         DS    CL1                                                              
LNDPRC3  DS    CL8                                                              
         DS    CL1                                                              
LNDPRC4  DS    CL8                                                              
         EJECT                                                                  
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROD2D                                                       
         SPACE 2                                                                
         ORG   T60BFFD+2504         SAVED STORAGE                               
REQOG    DS    X                                                                
REQOFF   DS    XL2                                                              
REQCLI   DS    CL6                                                              
REQPRO   DS    CL6                                                              
REQJOB   DS    CL6                                                              
REQMEDG  DS    CL1                                                              
REQMED   DS    CL1                                                              
REQWRKG  DS    CL1                                                              
REQWRK   DS    CL2                                                              
REQSDT   DS    XL3                 START DATE                                   
REQEDT   DS    XL3                 END DATE                                     
REQLNG   EQU   *-REQOG                                                          
         EJECT                                                                  
SUBSYSD  DSECT                                                                  
         ORG   DRGEN                                                            
LOCAL    DS    0X                                                               
STRKEY   DS    CL(L'KEY)                                                        
ENDKEY   DS    CL(L'KEY)                                                        
ANYOPT   DS    CL1                                                              
LEVEL    DS    C                                                                
EFFDATE  DS    PL3                                                              
*                                                                               
LOCALLN  EQU   *-LOCAL                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACPRO22   06/25/04'                                      
         END                                                                    
