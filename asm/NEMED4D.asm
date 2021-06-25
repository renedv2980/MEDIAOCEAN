*          DATA SET NEMED4D    AT LEVEL 105 AS OF 03/16/10                      
*PHASE T31E4DA,+0                                                               
*                                                                               
         TITLE 'T31E4D - EDIT FOR EQUALIZED COST REPORT'                        
**********************************************************************          
* NEMEDED(T31E4D) - THIS EDITS REQ SCREEN FOR EQUALIZED COST REPORT  *          
*                                                                               
* INPUTS - PARAMETER 1 - LOCATION OF THE GEND SPOOL DSECT. CONTAINS  *          
*                           MANY USEFUL ADDRESSES, DATA              *          
*                                                                               
* OUTPUTS - NETBLOCK - THE BLOCK USED TO READ THE NETWORK FILE.      *          
*                      MANY FIELDS FILLED IN BY NETIO.               *          
*                                                                               
*                                                                               
*  CALLS TO -                                                        *          
*   NVVALID - VALIDATION ROUTINES.                                   *          
**********************************************************************          
*                                                                               
         PRINT NOGEN                                                            
T31E4D   CSECT                                                                  
         NMOD1 0,**NE4D**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING MYWORKD,R7                                                       
         ST    R2,RELO                                                          
         L     R6,ANETWS4                                                       
         ST    R6,NBADEM                                                        
         USING NDDEMBLK,R6                                                      
****     LR    R1,R6                                                            
****     A     R1,=F'1000'                                                      
         CLI   OFFLINE,C'Y'                                                     
         BNE   SKIPBUFF                                                         
         L     R1,=A(NETBUFF)                                                   
         ST    R1,NBANBUFF                                                      
         OI    NBINDS7,NBI7NTIO    SET NETBUFF FLAG                             
SKIPBUFF CLC   =C'DU',NBSELAGY        IF AGY DU                                 
         BE    EDT00                  GO                                        
         BNE   EDT00              ***    ALL AGENCIES ARE SPECIAL ***           
*                                                                               
***      CLC   =C'TC',NBSELAGY        IF AGY RTC  (SJR TEST AGY)                
***      BE    EDT00                  GO                                        
***      CLC   =C'S5',NBSELAGY        IF AGY STW  (SJR TEST AGY)                
***      BE    EDT00                  GO                                        
***      CLI   OFFLINE,C'Y'           IF OFFLINE                                
***      BNE   EDT0                                                             
***      CLI   SPLTST,C'G'            AND G                                     
***      BE    EDT00                  GO                                        
**EDT0     CLC   TWAORIG,=X'250E'       OR SIGN ON BRSA                         
**       BE    EDT00                                                            
**       CLC   TWAORIG,=X'2599'       OR SIGN ON MVBROM                         
**       BE    EDT00                                                            
**       B     *+8                                                              
EDT00    MVI   SPECAGY,C'Y'           TURN ON SPEC AGY                          
                                                                                
         EJECT                                                                  
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
*                                    NEEDED IF ALREADY VALIDATED BITS           
*                                    NOT USED                                   
*                                    TO PASS TO PRINT MODULE                    
*                                                                               
         LA    R2,SPLCLIH          CLIENT                                       
         NETGO NVCLI,DMCB,SPLCLIN      FILL IN NAME                             
         OI    SPLCLINH+6,X'80'           TRANSMIT NAME                         
         L     R3,NBAIO                                                         
         USING CLTHDR,R3                                                        
         MOVE  (CLISTSV,880),CLIST      SAVE CLIST                              
         MVC   CLTNMSV,CNAME                                                    
         MVC   CLTCORP,CPRPRD                                                   
*                                                                               
         MVC   CLTTIS,COPT3        SAVE COP3T OR COP3TI                         
         NI    CLTTIS,X'FF'-X'F9'  CLEAR ALL BUT X'06'(COP3T/COP3TI)            
         TM    COPT4,COP4TIS       T+I+S                                        
         BZ    *+8                                                              
         MVI   CLTTIS,COP4TIS      SET T+I+S                                    
         DROP  R3                                                               
*                                                                               
         MVI   FTERMFLG,1                 SET OPTIONAL FLAG                     
         LA    R2,SPLPROH          PRODUCT                                      
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         CLI   NBSELPGR,X'40'      PROD GROUPS NOT ALLOWED                      
         BH    EDTINV                                                           
         CLC   =C'ALL',SPLCLI      IF CLI=ALL SKIP PRDNM FOUT                   
         BE    *+8                 SINCE YOU GET GARBAGE                        
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
         MVI   FTERMFLG,0                                                       
         LA    R2,SPLESTH          ESTIMATE (REQUIRED)                          
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         CLI   NDDEMOS,0                                                        
         BNE   EDT5                                                             
         L     R3,NBAIO                                                         
         USING ESTHDR,R3                                                        
         MVC   NDDEMOS(60),EDEMLST                                              
         MVC   NDDEMOS+60(3),EDEM21                                             
EDT5     OI    SPLESTNH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
         MVI   FTERMFLG,1                 SET OPTIONAL FLAG                     
         LA    R2,SPLNETH          NETWORK                                      
         NETGO NVNETALL,DMCB                                                    
         CLI   NBSELNET,X'40'                                                   
         BNH   EDT8                                                             
         LA    R1,IO                                                            
         USING STAREC,R1                                                        
         MVC   STATYPE,STYPE                                                    
         DROP  R1                                                               
*                                                                               
EDT8     LA    R2,SPLPAKH          PACKAGE                                      
         CLC   =C'ALL',SPLPAK                                                   
         BE    EDT10                                                            
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT10                                                            
         NETGO NVPAKLOK,DMCB,SPLPAKN     ELSE VALIDATE/FILL IN SPLPAKN          
         OI    SPLPAKNH+6,X'80'       TRANSMIT PAKN                             
         SPACE                                                                  
EDT10    DS    0H                                                               
*                                                                               
         LA    R2,SPLDPTH              DAYPART                                  
         NETGO NVDPTALL,DMCB,SPLDPTN                                            
         OI    SPLDPTNH+6,X'80'                                                 
*                                                                               
         MVI   SOONRUN,0                                                        
         CLC   CONWHEN(3),=C'SOON'                                              
         BNE   *+12                                                             
         MVI   FTERMFLG,0          SOON DATES REQUIRED                          
         MVI   SOONRUN,C'Y'                                                     
*                                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB       START DATE                                   
*                                                                               
         LA    R2,SPLENDH          END DATE                                     
         CLC   CONWHEN(3),=C'SOON'                                              
         BNE   EDT12                                                            
         NETGO NVENDDAT,DMCB,126    (ON SOON JOB 18 WEEK LIMIT)                 
         B     EDT15                                                            
EDT12    NETGO NVENDDAT,DMCB                                                    
         EJECT                                                                  
         SPACE                                                                  
*                                  TEST RUN CHECK                               
EDT15    MVI   FTERMFLG,0                                                       
         LA    R2,SPLTSTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         CLI   SPLTST,C'Y'                                                      
         BE    EDT30                                                            
         CLI   SPLTST,C'N'                                                      
         BE    EDT25                                                            
         B     EDTINV                                                           
*                                                                               
*****    CLI   SPLTST,C'T'      SPECIAL MEDIA VEST INPUT                        
*****    BNE   EDT17                                                            
*****    CLI   SPECAGY,C'Y'                                                     
*****    BE    EDT30                                                            
*****    B     EDTINV                                                           
***EDT17    CLI   OFFLINE,C'Y'            ,,,IF OFFLINE                         
***         BNE   EDTINV                                                        
***         CLI   SPLTST,C'G'             ,,,SPECIAL FOR MEDIAVEST              
***         BE    EDT30                   ,,,'BU' GENERATED                     
***         DC    H'0'                                                          
*                                                                               
EDT25    CLC   CONWHEN(3),=C'SOON'                                              
         BNE   EDT30                                                            
         CLI   SPECAGY,C'Y'           DU/BRSA                                   
         BE    EDT25B                                                           
****     CLC   =C'SJ',NBSELAGY        SJR FOR TESTING                           
****     BE    EDT25B                                                           
****     CLC   =C'DR',NBSELAGY        SMGTEST (DR)                              
****     BE    EDT25B                                                           
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'*** SOON ONLY FOR TEST RUN=Y   '                  
         GOTO1 ERREX2                                                           
EDT25B   EQU   *                                                                
         CLI   NBSELCLI,X'40'                                                   
         BE    *+14                                                             
         CLC   =C'ALL',NBSELCLI                                                 
         BNE   LOCK0                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'*** UPDATIVE SOON FOR ONE CLIENT'                 
         GOTO1 ERREX2                                                           
* - GET SE NUMBER                                                               
LOCK0    GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           MUST BE NET                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,FASYS          GET SE NUMBER                                
         DROP  R1                                                               
* - LOCK / UNLOCK                                                               
         MVI   LOCKSW,C'L'         LOCKING                                      
         MVI   TWAWHEN,5           SET FOR UPDATIVE SOON                        
         LA    R3,MYKEY                                                         
         USING LKKEYD,R3                                                        
         XC    MYKEY(L'LOCKEY),MYKEY                                            
         MVC   LOCKSE,BYTE         SET SE NUMBER                                
         MVC   LOCKAGY,NBSELAGY    AGENCY                                       
         MVC   LOCKRTY,=C'UN'      UNIT RECORDS                                 
         MVC   LOCKKEY(3),NBSELCLI    3 BYTE CLIENT CODE                        
         MVC   LOCKKEY+3(4),NBSELNET  4 BYTE NETWORK                            
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,X'7E'                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         L     R4,ACOMFACS                                                      
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),(LOCKSW,MYKEY),(R4)                                    
         CLI   DMCB+4,0            ANY ERRORS                                   
         BE    LOCKX                                                            
         XC    CONHEAD,CONHEAD    YES/ERRORS                                    
         MVC   CONHEAD(33),=C'*** CLIENT LOCKED - TRY LATER ***'                
         GOTO1 ERREX2                                                           
         DROP  R3                                                               
LOCKX    EQU   *                                                                
                                                                                
*                                                                               
EDT30    DS    0H                                                               
         MVI   FTERMFLG,1                                                       
*                                                                               
         MVI   INTEGFLG,C'N'       INTEGFLG                                     
         LA    R2,SPLOPTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT40                                                            
         MVI   INTEGFLG,C'Y'                                                    
         CLI   8(R2),C'Y'          Y=INTEG+ACTUAL DOLLARS                       
         BE    EDT40                                                            
         MVI   INTEGFLG,C'S'                                                    
         CLI   8(R2),C'S'          S=INTEG+ACTUAL+SPECIAL                       
         BE    EDT40                                                            
         MVI   INTEGFLG,C'B'       B=BBDO FEATURE                               
         CLI   8(R2),C'B'                                                       
         BE    EDT40                                                            
         MVI   INTEGFLG,C'N'                                                    
         CLI   8(R2),C'N'                                                       
         BNE   EDTINV                                                           
         SPACE                                                                  
* INTEGFLG OPTION MUST MATCH COPTION ON CLIENT RECORD                           
EDT40    TM    CLTTIS,X'07'         CLIENT COPT3/4 SET?                         
         BZ    EDT41               NO/THEY DO WHAT THEY WANT                    
         CLI   INTEGFLG,C'N'      TIME ONLY?                                    
         BNE   *+16                                                             
         TM    CLTTIS,X'04'                                                     
         BNO   EDTINV1                                                          
         B     EDT41                                                            
         CLI   INTEGFLG,C'Y'      TIME+INT ?                                    
         BNE   *+16                                                             
         TM    CLTTIS,X'02'                                                     
         BNO   EDTINV1                                                          
         B     EDT41                                                            
         CLI   INTEGFLG,C'S'      TIME+INT+SPECIAL                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CLTTIS,X'01'                                                     
         BNO   EDTINV1                                                          
         B     EDT41                                                            
EDT41    LA    R2,SPLFACH               FACTOR                                  
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT50                                                            
         LR    R3,R1                      GETFLD RETURNS L' IN R1               
         GOTO1 CASHVAL,DMCB,(3,SPLFAC),(R3)                                     
         CLI   0(R1),X'FF'                                                      
         BNE   EDT42                                                            
         B     EDTINV                                                           
EDT42    MVI   FCTRFLG,C'Y'                                                     
         MVC   FACTOR,4(R1)                                                     
         SPACE                                                                  
EDT50    LA    R2,SPLRNDH          ROUNDING                                     
         NETGO NVGETFLD,DMCB,=F'10000'                                          
         BZ    EDT60                                                            
         LTR   R0,R0                                                            
         BNZ   EDT55               R0=0 MEANS NON-NUMERIC                       
         B     EDTINV                                                           
EDT55    CLC   =C'1',SPLRND                                                     
         BE    EDT57                                                            
         CLC   =C'10',SPLRND                                                    
         BE    EDT57                                                            
         CLC   =C'100',SPLRND                                                   
         BE    EDT57                                                            
         CLC   =C'1000',SPLRND                                                  
         BE    EDT57                                                            
         CLC   =C'10000',SPLRND                                                 
         BNE   EDTINV                                                           
EDT57    LR    R3,R1               GETFLD RETURNS L' IN R1                      
         GOTO1 CASHVAL,DMCB,(0,SPLRND),(R3)   CASHVAL WILL RETURN               
         CLI   0(R1),X'FF'                    FULLWORD BINARY CENTS             
         BE    EDTINV                                                           
         L     R3,4(R1)                                                         
         CVD   R3,DUB                                                           
         ZAP   ROUND,DUB                                                        
         MVI   ROUNDFLG,C'Y'                                                    
EDT60    LA    R2,SPLPGKH          CHK PKG COST OPTION                          
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT70                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   EDTINV                                                           
         MVI   PKGOPT,C'Y'                                                      
         SPACE 3                                                                
EDT70    LA    R2,SPLASSH                                                       
         MVI   ASGNDOVR,C'N'   CHECK IF REALLOCATING ASSIGNED                   
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT72                                                            
         MVC   ASGNDOVR,FLD                                                     
         SPACE 3                                                                
EDT72    LA    R2,SPLDEMH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT74                                                            
         CLI   FLD,C'E'                                                         
         BE    EDT74                                                            
         CLI   FLD,C'A'                                                         
         BNE   EDTINV                                                           
         MVI   DEMOPT,C'A'                                                      
         SPACE 3                                                                
EDT74    LA    R2,SPLEQVH          EQUIVALENCE OVERRIDE                         
         MVI   EQIVAL,X'FF'        0/30/60                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT76                                                            
         TM    4(R2),X'08'         IS IT NUMERIC                                
         BZ    EDTINV                                                           
         CH    R0,=H'0'                                                         
         BE    EDT74X                                                           
         CH    R0,=H'30'                                                        
         BE    EDT74X                                                           
         CH    R0,=H'60'                                                        
         BNE   EDTINV                                                           
EDT74X   STC   R0,EQIVAL                                                        
         SPACE 3                                                                
EDT76    LA    R2,SPLOPSH          OPTIONS                                      
         BAS   RE,EDITOPT                                                       
         SPACE 3                                                                
         LA    R2,SPLDMOH          DEMO OVERRIDE                                
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
         SPACE 3                                                                
EDITXX   DS    0H                                                               
* - NEED TO HAVE COPIES WRITTEN TO RECOVERY FILE                                
         ICM   R1,15,TWAMASTC                                                   
         BZ    EDITXXX                                                          
         USING MCBLOCK,R1                                                       
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE COPIES                       
         DROP  R1                                                               
*                                                                               
EDITXXX  LA    R2,SPLCLIH          NORMAL END OF EDIT                           
         B     XIT                                                              
         SPACE                                                                  
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
*                                                                               
EDTINV   MVI   ERROR,INVALID                                                    
         GOTO1 TRAPERR                                                          
         SPACE 2                                                                
EDTINV1  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'*OPTION MUST MATCH CLIENT REC OPTION*'            
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
*                                                                               
TRAPERR  EQU   ERREX                                                            
         EJECT                                                                  
*                                                                               
*          DATA SET NEPUP24    AT LEVEL 035 AS OF 05/31/88                      
         SPACE 3                                                                
EDITOPT  NTR1                                                                   
         MVI   CPMS,0                                                           
         MVI   UNASIGND,0                                                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    EDTINV                                                           
         SPACE 1                                                                
OPT2     CLC   12(3,R4),=C'CPM'    CPM OPTION                                   
         BNE   OPT4                                                             
         MVC   CPMS,22(R4)                                                      
         CLI   CPMS,C'Y'                                                        
         BNE   OPT2A                                                            
         CLI   EQIVAL,X'FF'        IF REQUESTING CPMS                           
         BNE   EDTINV              EQUIVALENCE FIELD MUST BE BLANK              
         BE    OPTEND                                                           
OPT2A    CLI   CPMS,C'N'                                                        
         BE    OPTEND                                                           
         B     EDTINV                                                           
         SPACE 1                                                                
OPT4     CLC   12(3,R4),=C'NOASS'  NO ASSIGNED COST UNITS ONLY                  
         BNE   OPT6                                                             
         MVC   UNASIGND,22(R4)                                                  
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         B     EDTINV                                                           
         SPACE 1                                                                
OPT6     DS    0H                                                               
         CLC   12(3,R4),=C'PFB'    PFB=YES                                      
         BNE   OPT8                                                             
         MVC   PFBOPT,22(R4)                                                    
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         B     EDTINV                                                           
*                                                                               
OPT8     DS    0H                                                               
         CLC   12(3,R4),=C'PRE'    PRE=CAB                                      
         BNE   OPT10                                                            
         CLC   =C'CAB',22(R4)                                                   
         BNE   EDTINV                                                           
         B     OPT8D               GIVE THEM PLENTY OF ROOM                     
         CLI   NBSELMFL,C'C'       ALL,C OK                                     
         BE    OPT8D                                                            
         CLI   NBSELNET,X'40'                                                   
         BNH   EDTCABX                                                          
         CLI   STATYPE,C'C'        IS IT CABLE STATION?                         
         BNE   EDTCABX                                                          
OPT8D    MVI   CABOPT,C'Y'                                                      
         B     OPTEND                                                           
EDTCABX  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'OPTION NOT VALID WITH NON-CABLE NETWORK'          
         GOTO1 ERREX2                                                           
         B     XIT                                                              
EDTCPMX  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'*** PRE=CAB AND CPM=Y INVALID MIX ***'            
         GOTO1 ERREX2                                                           
         B     XIT                                                              
*                                                                               
OPT10    CLC   =C'NOZERO',12(R4)   NO ZERO DOLLAR UNITS                         
         BNE   OPT12                                                            
         OI    ECOPTS,ECNOZERO                                                  
         B     OPTEND                                                           
OPT12    DS    0H                                                               
         B     EDTINV                                                           
*                                                                               
OPTEND   LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,OPT2                                                          
***      CLI   CPMS,C'Y'                IF CPM=Y                                
***      BNE   OPTENDX                                                          
***      CLI   CABOPT,C'Y'             AND CABOPT=Y                             
***      BE    EDTCPMX                 ERROR                                    
OPTENDX  B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
NETBUFF  DS    CL6000                                                           
         EJECT                                                                  
         SPACE                                                                  
MYWORKD  DSECT                     *** MY WORK DSECT USING ANETWS1 ***          
CLISTSV  DS    CL880                                                            
CLTNMSV  DS    CL20                                                             
CLTCORP  DS    CL3                 CORPORATE PRODUCT                            
INTEGFLG DS    CL1                                                              
FCTRFLG  DS    CL1                                                              
ROUNDFLG DS    CL1                                                              
ROUND    DS    PL3                                                              
PKGOPT   DS    CL1                                                              
DEMOPT   DS    CL1                                                              
ASGNDOVR DS    CL1                                                              
FACTOR   DS    F                                                                
RELO     DS    F                                                                
EQIVAL   DS    CL1                                                              
CPMS     DS    CL1                                                              
UNASIGND DS    CL1                                                              
PFBOPT   DS    CL1                                                              
SOONRUN  DS    CL1                                                              
CABOPT   DS    CL1                                                              
SPECAGY  DS    CL1              C'Y'=DU/RBSA                                    
ECOPTS   DS    XL1                 ECOST OPTIONS                                
ECNOZERO EQU   X'01'               NO ZERO DOLLAR UNITS                         
**                                                                              
STATYPE  DS    CL1                                                              
MYKEY    DS    CL30                                                             
LOCKSW   DS    CL1                                                              
CLTTIS   DS    CL1                 TIME+INT+SPECIAL PROFILE FLAG                
*                                                                               
         SPACE 3                                                                
       ++INCLUDE NETINCLN                                                       
NDBLK  DSECT                                                                    
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDEDD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALOCKETD                                                      
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'105NEMED4D   03/16/10'                                      
         END                                                                    
