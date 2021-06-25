*          DATA SET NEWRI41X   AT LEVEL 018 AS OF 05/01/02                      
*PHASE T32041A,+0                                                               
*                                                                               
         TITLE 'T32041 - EDIT FOR BRM EQUALIZED COST'                           
**********************************************************************          
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
T32041   CSECT                                                                  
         NMOD1 0,**BRME**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING MYWORKD,R7                                                       
         ST    R2,RELO                                                          
**       L     R6,ANETWS4      ANETWS4 = NETWORK BUFFER FOR NET FILTERS         
**       ST    R6,NBANBUFF                                                      
**       A     R6,=F'1000'         ANETWS4+1000 = DEMOBLOCKS                    
**       ST    R6,NBADEM                                                        
         L     R6,ANETWS4       ANETWS4 FOR DEMOBLOCK                           
         ST    R6,NBADEM                                                        
SKIPNTBF DS    0H                                                               
         USING NDDEMBLK,R6                                                      
*                                                                               
* - NEED TO HAVE COPIES WRITTEN TO RECOVERY FILE                                
         ICM   R1,15,TWAMASTC                                                   
         BZ    ENDMST                                                           
         USING MCBLOCK,R1                                                       
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE COPIES                       
         DROP  R1                                                               
ENDMST   DS    0H                                                               
*                                                                               
         CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
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
         DROP  R3                                                               
*                                                                               
         MVI   FTERMFLG,1                 SET OPTIONAL FLAG                     
         LA    R2,SPLPROH          PRODUCT                                      
         NETGO NVPRDALL,DMCB,SPLPRON                                            
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
         MVC   NDDEMOS,EDEMLST                                                  
EDT5     OI    SPLESTNH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
         MVI   FTERMFLG,1                 SET OPTIONAL FLAG                     
         LA    R2,SPLNETH          NETWORK                                      
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLPAKH          PACKAGE                                      
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
         CLC   CONWHEN(3),=C'SOON'                                              
         BNE   *+8                                                              
         MVI   FTERMFLG,0          SOON DATES REQUIRED                          
*                                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB       START DATE                                   
*                                                                               
         LA    R2,SPLENDH          END DATE                                     
         CLC   CONWHEN(3),=C'SOON'                                              
         BNE   EDT12                                                            
         NETGO NVENDDAT,DMCB,112    (ON SOON JOB QUARTER LIMIT)                 
         B     EDT30                                                            
EDT12    NETGO NVENDDAT,DMCB                                                    
         EJECT                                                                  
         SPACE                                                                  
*                                                                               
EDT30    DS    0H                                                               
         MVI   FTERMFLG,1                                                       
         SPACE                                                                  
EDT50    DS    0H                  ROUNDING                                     
         ZAP   ROUND,=P'100'       ROUND TO ONE DOLLAR                          
         MVI   ROUNDFLG,C'Y'                                                    
*                                                                               
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
         MVI   DEMOPT,C'A'                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT74                                                            
         CLI   FLD,C'A'                                                         
         BE    EDT74                                                            
         CLI   FLD,C'E'                                                         
         BNE   EDTINV                                                           
         MVI   DEMOPT,C'E'                                                      
         SPACE 3                                                                
EDT74    LA    R2,SPLEQVH          EQUIVALENCE OVERRIDE                         
         MVI   EQIVAL,30           DEFAULT = 30                                 
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
EDT76    LA    R2,SPLPFBH          PFB                                          
         MVI   PFBOPT,C'Y'         DEFAULT = DEMOS FOR PFBS                     
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT78                                                            
         CLI   FLD,C'Y'                                                         
         BE    EDT78                                                            
         CLI   FLD,C'N'                                                         
         BNE   EDTINV                                                           
         MVI   PFBOPT,C'N'                                                      
*                                                                               
EDT78    LA    R2,SPLOPSH          OPTIONS                                      
         BAS   RE,EDITOPT                                                       
         SPACE 3                                                                
         LA    R2,SPLDMOH          DEMO OVERRIDE                                
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
         SPACE 3                                                                
EDT100   MVI   FTERMFLG,0          TEST RUN                                     
         LA    R2,SPLTSTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         CLI   SPLTST,C'Y'                                                      
         BE    EDITXX                                                           
         CLI   SPLTST,C'N'                                                      
         BNE   EDTINV                                                           
* LOCK FOR UPDATIVE SOON                                                        
         MVI   HALF,C'L'                                                        
         CLI   OFFLINE,C'Y'          LOCK IT ONLINE                             
         BE    EDITXX                                                           
         BAS   RE,LOCKEM                                                        
*                                                                               
EDITXX   LA    R2,SPLCLIH          NORMAL END OF EDIT                           
         B     XIT                                                              
         SPACE                                                                  
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
*                                                                               
EDTINV   MVI   ERROR,INVALID                                                    
         GOTO1 TRAPERR                                                          
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
         BE    OPTEND                                                           
         CLI   CPMS,C'N'                                                        
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
OPT6     CLC   12(2,R4),=C'SP'     INCLUDE SPECIAL CHARGES                      
         BNE   OPT8                                                             
         MVC   SPCLOPT,22(R4)                                                   
         CLI   SPCLOPT,C'Y'                                                     
         BE    OPTEND                                                           
         CLI   SPCLOPT,C'N'                                                     
         BE    OPTEND                                                           
         B     EDTINV                                                           
*                                                                               
OPT8     DS    0H                                                               
         B     EDTINV                                                           
*                                                                               
OPTEND   LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         EJECT                                                                  
* LOCKER FOR UPDATIVE SOON                                                      
         SPACE                                                                  
LOCKEM   NTR1                                                                   
         CLC   =C'SOON',CONWHEN    IS IT SOON                                   
         BNE   LOCKX               NO/FORGET IT                                 
                                                                                
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
         LA    R3,MYKEY                                                         
         USING LKKEYD,R3                                                        
         XC    MYKEY(L'LOCKEY),MYKEY                                            
         MVC   LOCKSE,BYTE         SET SE NUMBER                                
         MVC   LOCKAGY,NBSELAGY    AGENCY                                       
         MVC   LOCKRTY,=C'UN'      UNIT RECORDS                                 
         MVC   LOCKKEY(3),NBSELCLI    3 BYTE CLIENT CODE                        
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,X'7E'                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R2,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         L     R6,ACOMFACS                                                      
         LTR   R6,R6                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (R2),(R1),(HALF,MYKEY),(R6)                                      
         CLI   DMCB+4,0            ANY ERRORS                                   
         BE    LOCKX                                                            
         XC    CONHEAD,CONHEAD    YES/ERRORS                                    
         MVC   CONHEAD(33),=C'*** CLIENT LOCKED - TRY LATER ***'                
         GOTO1 ERREX2                                                           
         DROP  R3                                                               
LOCKX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
MYWORKD  DSECT                     *** MY WORK DSECT USING ANETWS1 ***          
CLISTSV  DS    CL880                                                            
CLTNMSV  DS    CL20                                                             
ROUNDFLG DS    CL1                                                              
ROUND    DS    PL3                                                              
PKGOPT   DS    CL1                                                              
DEMOPT   DS    CL1                                                              
ASGNDOVR DS    CL1                                                              
RELO     DS    F                                                                
EQIVAL   DS    CL1                                                              
CPMS     DS    CL1                                                              
UNASIGND DS    CL1                                                              
PFBOPT   DS    CL1                                                              
SPCLOPT  DS    CL1                                                              
MYKEY    DS    CL40                                                             
*                                                                               
         PRINT OFF                                                              
         SPACE 3                                                                
       ++INCLUDE NETINCLS                                                       
NDBLK  DSECT                                                                    
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
*                                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIEBD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
*                                                                               
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDMASTC                                                        
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018NEWRI41X  05/01/02'                                      
         END                                                                    
