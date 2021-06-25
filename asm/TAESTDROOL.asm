*          DATA SET TAESTDROOL AT LEVEL 051 AS OF 05/11/13                      
*PHASE T70207A,*                                                                
         TITLE 'T70207 - SYSTEM DRIVER FOR ONLINE TALENT ESTIMATING'            
ESTDROOL CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ESTDROOL,R7,RR=R2                                              
         L     RA,0(R1)                                                         
         USING GLOBALD,RA          RA=A(DRIVER W/S)                             
         L     RC,GLAWORKD                                                      
         USING GEND,RC             RC=A(GENCON W/S)                             
         L     R9,ASYSD                                                         
         USING SYSD,R9             R9=A(TALENT SYSTEM W/S)                      
         LA    R8,TWAHOLE                                                       
         USING ESTD,R8             R8=A(ESTIMATING W/S)                         
         ST    R2,RELO             SAVE RELOCATION FACTOR                       
         EJECT                                                                  
*              HOOK CONTROLLED ROUTINES                                         
         SPACE 3                                                                
         CLI   GLHOOK,GLRESOLV     RESOLVING ADDRESSES                          
         BNE   TA2                                                              
         BAS   RE,SYSRES                                                        
         B     XIT                                                              
         SPACE 1                                                                
TA2      CLI   GLHOOK,GLROUT       EXECUTING ROUTINES                           
         BNE   XIT                                                              
         BAS   RE,SYSEXEC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              RESOLVING ROUTINE ADDRESSES                                      
         SPACE 3                                                                
SYSRES   NTR1                                                                   
         LA    R1,ROUTLIST                                                      
         SPACE 1                                                                
SYSRES2  CLC   GLLABEL,0(R1)                                                    
         BE    SYSRES4                                                          
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         B     SYSRES2                                                          
         SPACE 1                                                                
SYSRES4  MVC   GLAROUT,8(R1)       RETURN ADDRESS                               
         B     XIT                                                              
         SPACE 1                                                                
ROUTLIST DS    0F                  ** INPUT ROUTINES **                         
*                                                                               
         DC    C'INAY    ',A(INAY)      HIGH-LEVEL RECORDS                      
         DC    C'INCO    ',A(INCO)                                              
         DC    C'INCODET ',A(INCODET)                                           
         DC    C'INES    ',A(INES)                                              
         DC    C'INREV   ',A(INREV)                                             
*                                                                               
         DC    C'INPAY   ',A(INPAY)     ACCUMULATORS                            
         DC    C'INPNH   ',A(INPNH)                                             
         DC    C'ININR   ',A(ININR)                                             
         DC    C'INCSF   ',A(INCSF)                                             
         DC    C'INHNW   ',A(INHNW)                                             
         DC    C'INTNH   ',A(INTNH)                                             
         DC    C'INCOMM  ',A(INCOMM)                                            
         DC    C'INGROSS ',A(INGROSS)                                           
*                                                                               
*                                  ** OUTPUT ROUTINES **                        
*                                                                               
         DC    C'OUTAY   ',A(OUTAY)     HIGH-LEVEL RECORDS                      
         DC    C'OUTES   ',A(OUTES)                                             
         DC    C'OUTREV  ',A(OUTREV)                                            
         DC    C'OUTCODET',A(OUTCODET)                                          
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*              EXECUTING ROUTINES (ROWS)                                        
         SPACE 3                                                                
SYSEXEC  NTR1                                                                   
         MVC   WORK,SPACES         PRESET WORK AREAS                            
         MVC   AREAS,SPACES                                                     
         ZAP   DUB,=P'0'                                                        
         L     RF,GLAROUT          RF=A(ROUTINE)                                
         A     RF,RELO                                                          
         L     R2,GLAIFLD          R2=A(INPUT AREA)                             
         L     R3,GLAOFLD          R3=A(OUTPUT AREA)                            
         ST    R3,MYOFLD                                                        
         SPACE 1                                                                
         CLI   GLMODE,GLOUTPUT     IF THIS IS NOT OUTPUT PHASE                  
         BE    SYSOUT                                                           
         L     R1,GLADTENT                                                      
         USING DLIND,R1            R1=A(DROOL INPUT RECORD)                     
         ZIC   R3,DLINLEN          R3=L'EXPECTED INPUT - 1                      
         BCTR  R3,0                                                             
         BR    RF                                                               
         SPACE 1                                                                
SYSOUT   L     R1,GLADTENT                                                      
         USING DLOD,R1             R1=A(DROOL OUTPUT RECORD)                    
         CLI   DLOLTYP,C'N'        NO PRINT - NOT INTERESTED                    
         BE    XIT                                                              
         MVC   MYPOSO,DLOPOS       SAVE OUTPUT POSITION                         
         MVC   MYOLEN,DLOLEN       AND LENGTH LOCALLY                           
         BR    RF                                                               
         EJECT                                                                  
*              AGENCY                                                           
         SPACE 1                                                                
INAY     DS    0H                                                               
         MVC   CODEAREA(L'TGAGY),TGAGY                                          
         MVC   NAMEAREA(L'AGYNAME),AGYNAME                                      
         LA    RF,L'TGAGY-1                                                     
         B     RECIN                                                            
         SPACE 1                                                                
OUTAY    DS    0H                                                               
         CLI   GLARGS,0            IF USER WANTS CODE AND NAME                  
         BNE   OAYX                                                             
         MVC   TGNAME,AGYNAME      SET CURRENT SAVED NAME                       
         CLC   TGAGY,0(R2)         IF CODE CHANGED                              
         BE    OAYX                                                             
         MVC   TGAGY,0(R2)         SET NEW CODE                                 
         GOTO1 GETNAME,DMCB,TLAYCDQ,AGYNAME  GET REC. AND EXTRACT NAME          
         MVI   TGEST,X'FD'         SET LOWER LEVELS CHANGED                     
         MVI   TGCLI,X'FD'                                                      
         SPACE 1                                                                
OAYX     MVC   LABLAREA(6),=C'AGENCY'                                           
         LA    RF,L'TGAGY-1                                                     
         BAS   RE,RECOUT                                                        
         B     XIT                                                              
         SPACE 3                                                                
*              ESTIMATE                                                         
         SPACE 1                                                                
INES     DS    0H                                                               
         MVC   CODEAREA(L'TGEST),TGEST                                          
         MVC   NAMEAREA(L'ESTNAME),ESTNAME                                      
         LA    RF,L'TGEST-1                                                     
         B     RECIN                                                            
         SPACE 1                                                                
OUTES    DS    0H                                                               
         CLI   GLARGS,0            IF USER WANTS CODE AND NAME                  
         BNE   OESX                                                             
         MVC   TGNAME,ESTNAME      SET CURRENT SAVED NAME                       
         CLC   TGEST,0(R2)         IF CODE CHANGED                              
         BE    OESX                                                             
         MVC   TGEST,0(R2)         SET NEW CODE                                 
         GOTO1 GETNAME,DMCB,TLESCDQ,ESTNAME  GET REC. AND EXTRACT NAME          
         SPACE 1                                                                
OESX     MVC   LABLAREA(8),=C'ESTIMATE'                                         
         LA    RF,L'TGEST-1                                                     
         BAS   RE,RECOUT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              COMMERCIAL                                                       
         SPACE 1                                                                
INCO     DS    0H                                                               
         MVC   CODEAREA(L'TGCID),TGCID                                          
         LA    RF,L'TGCID-1                                                     
**NO-OP**MVC   CODEAREA+L'TGCID(L'TGCOM),TGCOM                                  
**NO-OP**MVC   NAMEAREA(L'COMNAME),COMNAME                                      
**NO-OP**LA    RF,L'TGCID+L'TGCOM-1                                             
         B     RECIN                                                            
         SPACE 2                                                                
*              COMMERCIAL MEDIA CODE                                            
         SPACE 1                                                                
INCODET  DS    0H                                                               
         CLI   SUMINDEX,NALL       IF MISC ADDITIONS                            
         BNH   INCDT2                                                           
         MVC   0(L'SUMINDEX,R2),SUMINDEX                                        
         LA    R2,L'SUMINDEX(R2)                                                
         MVC   0(L'SUMLIT,R2),SUMLIT                                            
         LA    R2,L'SUMLIT(R2)                                                  
         MVC   0(L'SUMAMT,R2),SUMAMT                                            
         B     ACCX                ADD TO GROSS                                 
*                                                                               
INCDT2   MVI   0(R2),0                                                          
         LA    R2,L'SUMINDEX(R2)                                                
         TM    GLARGS,X'80'                                                     
         BZ    *+14                                                             
         MVC   0(L'TGCID,R2),TGCID                                              
         LA    R2,L'TGCID(R2)                                                   
*                                                                               
         TM    GLARGS,X'20'                                                     
         BZ    *+14                                                             
         MVC   0(L'TGMENAME,R2),TGMENAME                                        
         LA    R2,L'TGMENAME(R2)                                                
*                                                                               
         TM    GLARGS,X'10'                                                     
         BZ    *+10                                                             
         MVC   0(L'COMNAME,R2),COMNAME                                          
         B     XIT                                                              
         SPACE 2                                                                
OUTCODET DS    0H                                                               
         CLI   0(R2),NALL                                                       
         BNH   OUTCDT1                                                          
         MVC   0(L'SUMLIT,R3),L'SUMINDEX(R2)                                    
         B     XIT                                                              
*                                                                               
OUTCDT1  LA    R2,1(R2)            BUMP PAST INDEX                              
         TM    GLARGS,X'80'                                                     
         BZ    OUTCDT2                                                          
         MVC   0(L'TGCID,R3),0(R2)                                              
         LA    R2,L'TGCID(R2)                                                   
*                                                                               
OUTCDT2  TM    GLARGS,X'20'                                                     
         BZ    OUTCDT4                                                          
         MVC   L'TGCID+1(L'TGMENAME,R3),0(R2)                                   
         LA    R2,L'TGMENAME(R2)                                                
*                                                                               
OUTCDT4  TM    GLARGS,X'10'                                                     
         BZ    XIT                                                              
         MVC   LINELNQ(L'COMNAME,R3),0(R2)                                      
         B     XIT                                                              
         EJECT                                                                  
*              GENERAL RECORD INPUT ROUTINE                                     
         SPACE 1                                                                
*                                  CODEAREA=RECORD CODE                         
*                                  NAMEAREA=RECORD NAME                         
*                                  RF=L'CODE-1                                  
*                                                                               
*                                  GLARGS C'C' = CODE ONLY                      
*                                         C'N' = NAME ONLY                      
RECIN    DS    0H                                                               
         CLI   GLARGS,C'N'         IF USER WANTS NAME ONLY                      
         BNE   *+14                                                             
         MVC   0(L'NAMEAREA,R2),NAMEAREA  SAVE NAME                             
         B     RECIX                                                            
         SPACE 1                                                                
         EX    RF,INCODE           ELSE SAVE CODE                               
RECIX    B     XIT                                                              
         SPACE 3                                                                
INCODE   MVC   0(0,R2),CODEAREA    MOVE CODE TO INPUT AREA                      
         EJECT                                                                  
*              ROUTINE TO GET A RECORD AND EXTRACT ITS NAME                     
         SPACE 1                                                                
*                                  P1=RECORD CODE, P2=A(NAME IN W/S)            
GETNAME  NTR1                                                                   
         CLI   0(R2),X'FF'         GET OUT IF DATA NOT REAL                     
         BE    XIT                                                              
         CLI   0(R2),X'FE'         IGNORE DUMMY RECORDS                         
         BE    XIT                                                              
         LM    R3,R4,0(R1)                                                      
         MVC   TGNAME,SPACES       PRE-CLEAR NAME                               
         GOTO1 RECVAL,DMCB,(R3),(X'20',0)  GET THE RECORD                       
         BNE   GETNAME5                                                         
         GOTO1 CHAROUT,DMCB,TANAELQ,0      EXTRACT THE NAME                     
GETNAME5 MVC   0(36,R4),TGNAME             SAVE THE NAME                        
         B     XIT                                                              
         EJECT                                                                  
*              GENERAL RECORD OUTPUT ROUTINE  (ASSUMES DATA FROM RECIN)         
         SPACE 1                                                                
*                                  LABLAREA=LABEL FOR HEADLINES                 
*                                  RF=L'CODE-1                                  
*                                                                               
*                                  GLARGS   C'C' = CODE ONLY                    
*                                           C'N' = NAME ONLY                    
*                                  GLARGS+1 C'X' = SUPPRESS LABEL               
*                                  GLARGS+2 C'S' = SQUASH OUTPUT                
*                                           C'2' = NAME ON 2ND LINE             
RECOUT   NTR1                                                                   
         CLI   0(R2),X'FF'         GET OUT IF DATA NOT REAL                     
         BE    XIT                                                              
         CLI   0(R2),X'FE'         IGNORE DUMMY RECORDS                         
         BE    XIT                                                              
         CLI   MYLTYP,C'H'                IF WE'RE IN HEADS                     
         BNE   RECO2                                                            
         CLI   GLARGS+1,C'X'              AS LONG AS NOT SUPPRESSED             
         BE    RECO2                                                            
         MVC   0(L'LABLAREA,R3),LABLAREA  DISPLAY TAG                           
         LA    R3,L'LABLAREA(R3)                                                
         SPACE 1                                                                
RECO2    CLI   MYLTYP,C'M'                         IF WE'RE IN MIDS             
         BNE   *+10                                                             
         MVC   0(L'CODEAREA+L'NAMEAREA,R3),SPACES  PRE-CLEAR O/P AREA           
         SPACE 1                                                                
         CLI   GLARGS,C'N'             IF USER WANTS NAME ONLY                  
         BNE   *+14                                                             
         MVC   0(L'NAMEAREA,R3),0(R2)  IT'S IN DRIVER RECORD                    
         B     RECO6                                                            
         SPACE 1                                                                
         EX    RF,OUTCODE          ELSE MOVE CODE TO O/P AREA                   
         SPACE 1                                                                
         CLI   GLARGS,0            IF USER WANTS CODE AND NAME                  
         BNE   RECO6                                                            
         CLI   GLARGS+2,C'2'       TEST WHETHER NAME GOES ON NEXT LINE          
         BNE   *+14                                                             
         MVC   LINELNQ(L'NAMEAREA,R3),TGNAME NAME IS IN GLOBAL                  
         B     *+10                                                             
         MVC   L'CODEAREA(L'NAMEAREA,R3),TGNAME                                 
         SPACE 1                                                                
RECO6    LA    R4,L'CODEAREA+L'NAMEAREA                                         
         CLI   GLARGS+2,C'S'              FORCE SQUASH OUTPUT                   
         BE    RECO8                                                            
         CLI   MYLTYP,C'M'                IF THIS IS A MIDLINE                  
         BE    RECO8                                                            
         CLI   MYLTYP,C'H'                OR IF THIS IS A HEADLINE              
         BNE   RECOX                                                            
**NO-OP* CLI   MYCOL,50                   AND DATA IS ON RHS                    
**NO-OP* BL    RECOX                                                            
         LA    R4,L'AREAS                                                       
RECO8    GOTO1 SQUASHER,DMCB,GLAOFLD,(R4) SQUASH IT ALL TOGETHER                
RECOX    B     XIT                                                              
         SPACE                                                                  
OUTCODE  MVC   0(0,R3),0(R2)              MOVE CODE TO OUTPUT AREA              
         EJECT                                                                  
*              REVISION NUMBER                                                  
         SPACE 2                                                                
INREV    DS    0H                                                               
         MVC   0(3,R2),REVISION                                                 
         B     XIT                                                              
         SPACE 3                                                                
OUTREV   DS    0H                                                               
         CLC   0(3,R2),SPACES                                                   
         BNH   XIT                                                              
         CLI   MYLTYP,C'P'                                                      
         BE    *+14                                                             
         MVC   0(8,R3),=C'REVISION'                                             
         LA    R3,9(R3)                                                         
         MVC   0(3,R3),REVISION                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ACCUMULATOR INPUT ROUTINES                                       
         SPACE 2                                                                
*                                  PAY  GLARGS   M=MUSIC ONLY                   
*                                                T=TALENT ONLY                  
INPAY    DS    0H                                                               
         BAS   RE,SUMCHECK         IF PROCESSING MISC ADDITIONS -XIT            
         BAS   RE,CKMUSTAL         CHECK IF OK BASED ON GLARGS                  
         L     R1,TCPAYI           IF RETURNED THEN OK                          
         A     R1,TCPAYC                                                        
         A     R1,TCEXP                                                         
         ST    R1,0(R2)                                                         
         B     ACCX                                                             
         SPACE 2                                                                
*                                  P&H  GLARGS  M=MUSIC ONLY                    
*                                               T=TALENT ONLY                   
INPNH    DS    0H                                                               
         BAS   RE,SUMCHECK         IF PROCESSING MISC ADDITIONS -XIT            
         BAS   RE,CKMUSTAL         CHECK IF OK BASED ON GLARGS                  
         MVC   0(4,R2),TCPNH       IF RETURNED THEN OK                          
         B     ACCX                                                             
         EJECT                                                                  
CKMUSTAL DS    0H                                                               
         ST    RE,TGFULL                                                        
         CLI   GLARGS,C'M'         MUSIC ONLY                                   
         BNE   CKMT2                                                            
         TM    ESTMODE,PROCESTP+PROCAUTO  IS THIS IS AN EST. PAYMENT            
         BZ    CKMT1                                                            
*        TM    TGCAUNI,AFM         MUSIC IF AFM ALLOWABLE FOR CATEGORY          
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         L     RE,TGFULL                                                        
         BZ    XIT                                                              
         B     CKMT4                                                            
*        TM    TGUSXUNI,AFM        FOR ACTUALS, MUST GO BY USE TYPE             
CKMT1    GOTO1 UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         L     RE,TGFULL                                                        
         BO    XIT                                                              
         CLI   TGUSEQU,UGRT        TREAT GUARANTEES AS NON-MUSIC                
         BE    XIT                                                              
*                                                                               
CKMT2    CLI   GLARGS,C'T'                                                      
         BNE   CKMT4                                                            
         TM    ESTMODE,PROCESTP+PROCAUTO  IS THIS IS AN EST. PAYMENT            
         BZ    CKMT3                                                            
*        TM    TGCAUNI,AFM         MUSIC IF AFM ALLOWABLE FOR CATEGORY          
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         L     RE,TGFULL                                                        
         BO    XIT                                                              
         B     CKMT4                                                            
CKMT3    CLI   TGUSEQU,UGRT        TREAT GUARANTEES AS NON-MUSIC                
         BE    CKMT4                                                            
*        TM    TGUSXUNI,AFM        FOR ACTUALS, MUST GO BY USE TYPE             
         GOTO1 UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         L     RE,TGFULL                                                        
         BZ    XIT                                                              
CKMT4    BR    RE                                                               
         EJECT                                                                  
*              ACCUMULATOR INPUT ROUTINES                                       
         SPACE 2                                                                
*                                  T&H  GLARGS  T=PAYROLL TAXES ONLY            
*                                               H=HANDLING FEES ONLY            
INTNH    DS    0H                                                               
         BAS   RE,SUMCHECK         IF PROCESSING MISC ADDITIONS -XIT            
         L     R1,TAXAMT           PAYROLL TAXES                                
         ST    R1,0(R2)                                                         
         CLI   GLARGS,C'T'                                                      
         BE    INTNHX                                                           
         L     RF,HNDAMT           HANDLING                                     
         ST    RF,0(R2)                                                         
         CLI   GLARGS,C'H'                                                      
         BE    INTNHX                                                           
         AR    R1,RF               TAX & HANDLING                               
         ST    R1,0(R2)                                                         
INTNHX   B     ACCX                                                             
         EJECT                                                                  
*              ACCUMULATOR INPUT ROUTINES                                       
         SPACE 2                                                                
INAPPLCR DS    0H                  APPLIED CREDITS                              
         BAS   RE,SUMCHECK                                                      
         MVC   0(4,R2),TCAPPLCR                                                 
         B     XIT                                                              
         SPACE 2                                                                
SUMCHECK DS    0H                  REJECT REGULAR COLS IF PROC. SUMMARY         
         CLI   SUMINDEX,0                                                       
         BER   RE                                                               
         CLI   SUMINDEX,NALL                                                    
         BER   RE                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ACCUMULATOR INPUT ROUTINES                                       
         SPACE 2                                                                
ININR    DS    0H                  INSURANCE & RETIREMENT                       
         BAS   RE,SUMCHECK         IF PROCESSING MISC ADDITIONS -XIT            
         MVC   0(4,R2),TCINR                                                    
         B     ACCX                                                             
         SPACE 2                                                                
INCSF    DS    0H                  CONTRACT SERVICE FEE                         
         BAS   RE,SUMCHECK         IF PROCESSING MISC ADDITIONS -XIT            
         MVC   0(4,R2),CSFAMT                                                   
         B     ACCX                                                             
         SPACE 2                                                                
INHNW    DS    0H                  HEALTH & WELFARE                             
         BAS   RE,SUMCHECK         IF PROCESSING MISC ADDITIONS -XIT            
         MVC   0(4,R2),TCHNW                                                    
         B     ACCX                                                             
         SPACE 2                                                                
INCOMM   DS    0H                  AGENCY COMMISSION                            
         BAS   RE,SUMCHECK         IF PROCESSING MISC ADDITIONS -XIT            
         MVC   0(4,R2),COMMAMT                                                  
         B     ACCX                                                             
         EJECT                                                                  
ACCX     DS    0H                  GENERAL ACCUMULATOR EXIT ROUTINE             
         L     RE,GROSS                                                         
         A     RE,0(R2)                                                         
         ST    RE,GROSS                                                         
         B     XIT                                                              
         SPACE 2                                                                
INGROSS  DS    0H                  GROSS                                        
         MVC   0(4,R2),GROSS                                                    
         XC    GROSS,GROSS                                                      
         B     XIT                                                              
         EJECT                                                                  
*              EXIT ROUTINES                                                    
         SPACE 2                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
LINELNQ  EQU   132                                                              
         SPACE 1                                                                
SPACES   DC    CL132' '                                                         
MYOLEN   DC    X'00'                                                            
MYPOSO   DS    0CL3                                                             
MYLTYP   DC    X'00'                                                            
MYLINE   DC    X'00'                                                            
MYCOL    DC    X'00'                                                            
MYOFLD   DC    A(0)                                                             
RELO     DS    F                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAESTDSECT                                                     
         EJECT                                                                  
       ++INCLUDE TAGENESTD                                                      
         EJECT                                                                  
* TAGENFFD                                                                      
* TASCR40D                                                                      
* TASYSIOD                                                                      
* DRGLOBAL                                                                      
* DROOLTABLE                                                                    
* DDSPLWORKD                                                                    
* TAGENFILE                                                                     
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR40D                                                       
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DROOLTABLE                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051TAESTDROOL05/11/13'                                      
         END                                                                    
