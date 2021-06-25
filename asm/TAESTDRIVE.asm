*          DATA SET TAESTDRIVE AT LEVEL 073 AS OF 01/16/14                      
*PHASE T7029EC,*                                                                
         TITLE 'T7029E - SYSTEM DRIVER FOR TALENT ESTIMATING'                   
ESTDRIVE CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ESTDRIVE,R7,R6                                                 
         L     RA,0(R1)                                                         
         USING GLOBALD,RA          RA=A(DRIVER W/S)                             
         L     RC,GLAWORKD                                                      
         USING GEND,RC             RC=A(GENCON W/S)                             
         L     R9,ASYSD                                                         
         USING SYSD,R9             R9=A(TALENT SYSTEM W/S)                      
         LA    R8,TWAHOLE                                                       
         USING ESTD,R8             R8=A(ESTIMATING W/S)                         
*                                                                               
*        SPECIALS                                                               
*        --------                                                               
*        GLARGS+9 = X'FF' MEANS DO OUTPUT ROUTINE EVEN IF P=NO                  
*        GLARGS+10 IS COLUMN FILTER PERIOD TYPE                                 
*        GLARGS+11 IS COLUMN FILTER PERIOD VALUE                                
*        GLARGS+12 IS COLUMN FILTER ACTUAL/ESTIMATE                             
         EJECT                                                                  
*              HOOK CONTROLLED ROUTINES                                         
         SPACE 3                                                                
         CLI   GLHOOK,GLRESOLV     RESOLVING ADDRESSES                          
         BNE   TA2                                                              
         BAS   RE,SYSRES                                                        
         B     XIT                                                              
         SPACE 1                                                                
TA2      CLI   GLHOOK,GLROUT       EXECUTING ROUTINES                           
         BNE   TA4                                                              
         BAS   RE,SYSEXEC                                                       
         B     XIT                                                              
         SPACE 1                                                                
TA4      CLI   GLHOOK,GLRESLIT     RESOLVING LITERALS                           
         BNE   TA8                                                              
         L     R2,GLAIFLD          R2=A(SOFT EXPRESSION)                        
         L     R3,GLAOFLD          R3=A(OUTPUT AREA)                            
         CLC   =C'COMLSUMM',0(R2)                                               
         BNE   TA6                                                              
         MVC   0(32,R3),=C'COMMERCIALS INCLUDED IN ESTIMATE'                    
         MVI   GLARGS+1,32                                                      
         SPACE 1                                                                
TA6      CLC   =C'RECAP',0(R2)                                                  
         BNE   TA8                                                              
         MVC   0(14,R3),=C'ESTIMATE RECAP'                                      
         MVI   GLARGS+1,14                                                      
         SPACE 1                                                                
TA8      CLC   =C'ALLCALLP',0(R2)                                               
         BNE   TA10                                                             
         MVC   0(31,R3),=C'ALL COMMERCIALS - ENTIRE PERIOD'                     
         MVI   GLARGS+1,31                                                      
         SPACE 1                                                                
TA10     B     XIT                                                              
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
         DC    C'INCL    ',A(INCL)                                              
         DC    C'INES    ',A(INES)                                              
         DC    C'INPR    ',A(INPR)                                              
         DC    C'INCO    ',A(INCO)                                              
*                                                                               
         DC    C'INCODET ',A(INCODET)   COMMERCIAL DETAILS                      
         DC    C'INCOCONT',A(INCOCONT)                                          
         DC    C'INLIDET ',A(INLIDET)                                           
         DC    C'INLIID  ',A(INLIID)                                            
         DC    C'INLILEN ',A(INLILEN)                                           
*                                                                               
         DC    C'INCADET ',A(INCADET)   CAST DETAILS                            
*                                                                               
         DC    C'INCYC   ',A(INCYC)     USE DETAILS                             
         DC    C'INUSENM ',A(INUSENM)                                           
         DC    C'INUSNMD ',A(INUSNMD)                                           
         DC    C'INUSNMXU',A(INUSNMXU)                                          
         DC    C'INUSCDE ',A(INUSCDE)                                           
         DC    C'INUSDET ',A(INUSDET)                                           
         DC    C'INAPCD  ',A(INAPCD)                                            
         DC    C'INAPDATE',A(INAPDATE)                                          
         DC    C'INPAYALL',A(INPAYALL)  ACCUMULATORS                            
         DC    C'INPAY   ',A(INPAY)                                             
         DC    C'INSCALE ',A(INSCALE)                                           
         DC    C'INPNH   ',A(INPNH)                                             
         DC    C'ININR   ',A(ININR)                                             
         DC    C'INCSF   ',A(INCSF)                                             
         DC    C'INHNW   ',A(INHNW)                                             
         DC    C'INTNH   ',A(INTNH)                                             
         DC    C'INNETT  ',A(INNETT)                                            
         DC    C'INCOMM  ',A(INCOMM)                                            
         DC    C'INAPPLCR',A(INAPPLCR)                                          
         DC    C'INGUARCR',A(INGUARCR)                                          
         DC    C'INGROSS ',A(INGROSS)                                           
*                                                                               
         DC    C'INTANU  ',A(INTANU)    ACTUAL USE DETAILS                      
         DC    C'INTAPD  ',A(INTAPD)                                            
*                                                                               
         DC    C'INPD    ',A(INPD)      GENERAL                                 
         DC    C'INREV   ',A(INREV)                                             
         DC    C'INMIDHD ',A(INMIDHD)                                           
         DC    C'INTOTRQ ',A(INTOTRQ)                                           
         DC    C'INCOUNT ',A(INCOUNT)                                           
         DC    C'INPAYLIT',A(INPAYLIT)                                          
         DC    C'INWC    ',A(INWC)                                              
*                                                                               
         DC    C'ICLAMRGE',A(ICLAMRGE)  INPUT DPG ROUTINE                       
         DC    C'IWSPMAJS',A(IWSPMAJS)                                          
         DC    C'IWSPUNIS',A(IWSPUNIS)                                          
         DC    C'IPYEGRSS',A(IPYEGRSS)                                          
*                                                                               
*                                  ** OUTPUT ROUTINES **                        
*                                                                               
         DC    C'OUTAY   ',A(OUTAY)     HIGH-LEVEL RECORDS                      
         DC    C'OUTCL   ',A(OUTCL)                                             
         DC    C'OUTES   ',A(OUTES)                                             
         DC    C'OUTPR   ',A(OUTPR)                                             
         DC    C'OUTCO   ',A(OUTCO)                                             
*                                                                               
         DC    C'OUTCODET',A(OUTCODET)  COMMERCIAL DETAILS                      
         DC    C'OUTCOCNT',A(OUTCOCNT)                                          
         DC    C'OUTLIDET',A(OUTLIDET)                                          
*                                                                               
         DC    C'OUTCADET',A(OUTCADET)  CAST DETAILS                            
*                                                                               
         DC    C'OUTCYC  ',A(OUTCYC)    USE DETAILS                             
         DC    C'OUTUSENM',A(OUTUSENM)                                          
         DC    C'OUTUSNMD',A(OUTUSNMD)                                          
         DC    C'OUTUSNXU',A(OUTUSNXU)                                          
         DC    C'OUTUSDET',A(OUTUSDET)                                          
         DC    C'OUTPYALL',A(OUTPYALL)  ACCUMULATORS                            
         DC    C'OUTSIZE ',A(OUTSIZE)                                           
*                                                                               
         DC    C'OUTPD   ',A(OUTPD)     GENERAL                                 
         DC    C'OUTREV  ',A(OUTREV)                                            
         DC    C'OUTMIDHD',A(OUTMIDHD)                                          
         DC    C'OUTTOTRQ',A(OUTTOTRQ)                                          
         DC    C'OUTCOUNT',A(OUTCOUNT)                                          
         DC    C'OUTPYLIT',A(OUTPYLIT)                                          
         DC    C'OUTLTALL',A(OUTLTALL)                                          
         DC    C'OUTNOTOT',A(OUTNOTOT)                                          
         DC    C'OUTTULIT',A(OUTTULIT)                                          
         DC    C'OUTPYONE',A(OUTPYONE)                                          
*                                                                               
         DC    C'OPYGROSS',A(OPYGROSS)  OUTPUT DPG ROUTINE                      
         DC    X'FF'                                                            
         EJECT                                                                  
*              EXECUTING ROUTINES (ROWS)                                        
         SPACE 3                                                                
SYSEXEC  NTR1                                                                   
         MVC   WORK,SPACES         PRESET WORK AREAS                            
         MVC   AREAS,SPACES                                                     
         ZAP   DUB,=P'0'                                                        
         L     RF,GLAROUT          RF=A(ROUTINE)                                
         L     R2,GLAIFLD          R2=A(INPUT AREA)                             
         L     R3,GLAOFLD          R3=A(OUTPUT AREA)                            
         ST    R3,MYOFLD                                                        
         SPACE 1                                                                
         CLI   GLMODE,GLOUTPUT     IF THIS IS NOT OUTPUT PHASE                  
         BE    SYSOUT                                                           
         BAS   RE,COLFILT          TEST COLUMN FILTERS                          
         BNE   XIT                                                              
         L     R1,GLADTENT                                                      
         USING DRIND,R1                                                         
         ZIC   R3,DRINLEN          R3=L'EXPECTED INPUT - 1                      
         BCTR  R3,0                                                             
         BR    RF                                                               
         SPACE 1                                                                
SYSOUT   L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         CLI   GLARGS+9,X'FF'      FORCE OUTPUT                                 
         BE    *+12                                                             
         CLI   DROLTYP,C'N'        NO PRINT - NOT INTERESTED                    
         BE    XIT                                                              
         TM    GLINDS,GLTOTLIN     IF THIS IS TOTAL LINE                        
         BZ    *+16                                                             
         MVC   HOLDA,SPACES        CLEAR HOLD AREAS                             
         MVC   HOLDE,SPACES                                                     
         SPACE 1                                                                
         MVC   MYPOSO,DROPOS       SAVE OUTPUT POSITION                         
         MVC   MYOLEN,DROLEN        AND LENGTH LOCALLY                          
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
         CLI   GLARGS,C'D'                                                      
         BNE   INES10                                                           
         MVC   0(8,R2),TGTODAY8                                                 
         B     XIT                                                              
*                                                                               
INES10   MVC   CODEAREA(L'TGEST),TGEST                                          
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
*              CLIENT                                                           
         SPACE 1                                                                
INCL     DS    0H                                                               
         MVC   CODEAREA(L'TGCLI),TGCLI                                          
         MVC   NAMEAREA(L'CLINAME),CLINAME                                      
         LA    RF,L'TGCLI-1                                                     
         B     RECIN                                                            
         SPACE 1                                                                
OUTCL    DS    0H                                                               
         CLI   GLARGS,0            IF USER WANTS CODE AND NAME                  
         BNE   OCLX                                                             
         MVC   TGNAME,CLINAME      SET CURRENT SAVED NAME                       
         CLC   TGCLI,0(R2)         IF CODE CHANGED                              
         BE    OCLX                                                             
         MVC   TGCLI,0(R2)         SET NEW CODE                                 
         GOTO1 GETNAME,DMCB,TLCLCDQ,CLINAME  GET REC. AND EXTRACT NAME          
         MVI   TGPRD,X'FD'         SET LOWER LEVEL CHANGED                      
         SPACE 1                                                                
OCLX     MVC   LABLAREA(6),=C'CLIENT'                                           
         LA    RF,L'TGCLI-1                                                     
         BAS   RE,RECOUT                                                        
         B     XIT                                                              
         SPACE 3                                                                
*              PRODUCT                                                          
         SPACE 1                                                                
INPR     DS    0H                                                               
         MVC   CODEAREA(L'TGPRD),TGPRD                                          
         OC    TGPRD,TGPRD         IF NOT PRODUCT DEFINED                       
         BNZ   *+8                                                              
         MVI   CODEAREA,X'FE'      SET DUMMY CODE                               
         MVC   NAMEAREA(L'PRDNAME),PRDNAME                                      
         LA    RF,L'TGPRD-1                                                     
         B     RECIN                                                            
         SPACE 1                                                                
OUTPR    DS    0H                                                               
         CLI   GLARGS,0            IF USER WANTS CODE AND NAME                  
         BNE   OPRX                                                             
         MVC   TGNAME,PRDNAME      SET CURRENT SAVED NAME                       
         CLC   TGPRD,0(R2)         IF CODE CHANGED                              
         BE    OPRX                                                             
         MVC   TGPRD,0(R2)         SET NEW CODE                                 
         GOTO1 GETNAME,DMCB,TLPRCDQ,PRDNAME  GET REC. AND EXTRACT NAME          
         MVI   TGCOM,X'FD'         SET LOWER LEVEL CHANGED                      
         SPACE 1                                                                
OPRX     MVC   LABLAREA(7),=C'PRODUCT'                                          
         LA    RF,L'TGPRD-1                                                     
         BAS   RE,RECOUT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              COMMERCIAL                                                       
         SPACE 1                                                                
INCO     DS    0H                                                               
         MVC   CODEAREA(L'TGCID),TGCID                                          
         MVC   CODEAREA+L'TGCID(L'TGCOM),TGCOM                                  
         MVC   NAMEAREA(L'COMNAME),COMNAME                                      
         LA    RF,L'TGCID+L'TGCOM-1                                             
         B     RECIN                                                            
         SPACE 1                                                                
OUTCO    DS    0H                                                               
         CLI   GLARGS,0            IF USER WANTS CODE AND NAME                  
         BNE   OCOX                                                             
         MVC   TGNAME,COMNAME      SET CURRENT SAVED NAME                       
         CLC   TGCOM,L'TGCID(R2)   TEST IF CODE CHANGED                         
         BE    OCOX                                                             
*                                                                               
         L     R1,HCOMTAB          R1=A(HYPOTHETICAL COMMLS TABLE)              
         L     R0,MAXHCOM          R0=MAXIMUM NUMBER OF TABLE ENTRIES           
         USING HCOMD,R1                                                         
OUTCO10  OC    0(HCOMLNQ,R1),0(R1) TEST LOGICAL END OF TABLE                    
         BZ    OUTCO20                                                          
         CLC   HCOMCID,0(R2)       LOOK FOR MATCH ON COMMERCIAL ID              
         BE    OUTCO15                                                          
         LA    R1,HCOMNEXT         BUMP TO NEXT SPOT                            
         BCT   R0,OUTCO10          AND TRY AGAIN                                
         B     OUTCO20             NOT IN TABLE - TRY FILE                      
*                                                                               
OUTCO15  MVC   COMNAME,HCOMNAME    EXTRACT COMMERCIAL NAME                      
         MVC   TGNAME,HCOMNAME     SET GLOBAL NAME                              
         MVC   TGCOM,L'TGCID(R2)   AND FAKE INTERNAL COMML NUMBER               
         B     OCOX                                                             
         DROP  R1                                                               
*                                                                               
OUTCO20  MVC   TGCOM,L'TGCID(R2)                                                
         GOTO1 GETNAME,DMCB,TLCOCCDQ,COMNAME  GET REC. AND EXTRACT NAME         
         SPACE 1                                                                
OCOX     MVC   LABLAREA(10),=C'COMMERCIAL'                                      
         LA    RF,L'TGCID-1                                                     
         BAS   RE,RECOUT                                                        
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
*                                           C'C' = CODE & NAME CHOPPED          
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
         CLI   GLARGS+2,C'C'       IF CODE AND NAME CHOPPED                     
         BNE   RECO4                                                            
         MVI   MEDFLG,C'N'         SET CODE & CHOP NAME ONLY                    
         BAS   RE,SETNCHOP                                                      
         B     RECOX                                                            
         SPACE 1                                                                
RECO4    EX    RF,OUTCODE          ELSE MOVE CODE TO O/P AREA                   
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
         CLI   MYCOL,90                   AND DATA IS ON RHS                    
         BL    RECOX                                                            
         LA    R4,L'AREAS                                                       
RECO8    GOTO1 SQUASHER,DMCB,GLAOFLD,(R4) SQUASH IT ALL TOGETHER                
RECOX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS CODE IN OUTPUT AREA, POSSIBLE MEDIA,                
*              THEN NAME CHOPPED                                                
         SPACE 1                                                                
SETNCHOP NTR1                                                                   
         LA    RF,12               RF=LENGTH OF TGCID                           
         LR    R1,R2                                                            
         LA    R1,11(R1)           R1=A(END OF TGCID)                           
SETNCHP5 CLI   0(R1),C' '          FIGURE OUT REAL LENGTH OF TGCID              
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,SETNCHP5                                                      
*                                                                               
         BCTR  RF,0                AND MOVE IT TO OUTPUT AREA                   
         EX    RF,OUTCODE                                                       
         LA    RF,2(RF)                                                         
         AR    R3,RF                                                            
*                                                                               
         CLI   MEDFLG,C'Y'         IF DISPLAYING MEDIA TOO                      
         BNE   SETNCHP8                                                         
         LA    R2,12(R2)           BUMP PAST CID                                
         LR    R0,RF               SAVE LENGTH USED SO FAR                      
*                                                                               
         LR    R1,R2                                                            
         LA    R1,4(R1)            R1=A(END OF MEDIA)                           
         LA    RF,5                                                             
SETNCHP6 CLI   0(R1),C' '          FIGURE OUT REAL LENGTH OF TGCID              
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,SETNCHP6                                                      
*                                                                               
         BCTR  RF,0                AND MOVE IT TO OUTPUT AREA                   
         EX    RF,OUTCODE                                                       
         LA    RF,2(RF)                                                         
         AR    R3,RF                                                            
         AR    RF,R0               COMBINED LENGTH USED SO FAR                  
*                                                                               
SETNCHP8 ZIC   R5,MYOLEN           THEN CHOP NAME WITH AVAILABLE SPACE          
         SR    R5,RF                                                            
         BNP   XIT                                                              
         GOTO1 MYCHOP,DMCB,(36,TGNAME),((R5),(R3)),4,0                          
         B     XIT                                                              
         SPACE 2                                                                
MYCHOP   NTR1                                                                   
         TM    GLDOWNLD,X'80'      IF DOWNLOADING                               
         BNO   *+12                                                             
         LA    R1,1                ONLY PRINT ONE LINE                          
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198                                                       
         GOTO1 CHOPPER,DMCB                                                     
         B     XIT                                                              
         SPACE 2                                                                
OUTCODE  MVC   0(0,R3),0(R2)       MOVE CODE TO OUTPUT AREA                     
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
*              COMMERCIAL DETAILS                                               
         SPACE 2                                                                
*                                  ARGS X'80' = CID                             
*                                       X'40' = LENGTH                          
*                                       X'20' = MEDIA                           
*                                       X'10' = NAME                            
*                                       X'08' = EXPIRY                          
INCODET  DS    0H                                                               
         TM    GLARGS,X'80'                                                     
         BZ    *+14                                                             
         MVC   0(L'TGCID,R2),TGCID                                              
         LA    R2,L'TGCID(R2)                                                   
         SPACE 1                                                                
         TM    GLARGS,X'40'                                                     
         BZ    *+14                                                             
         MVC   0(1,R2),ELTACO+TACOSEC-TACOD                                     
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
         TM    GLARGS,X'20'                                                     
         BZ    *+14                                                             
         MVC   0(L'TGMENAME,R2),TGMENAME                                        
         LA    R2,L'TGMENAME(R2)                                                
*                                                                               
         TM    GLARGS,X'10'                                                     
         BZ    *+14                                                             
         MVC   0(L'COMNAME,R2),COMNAME                                          
         LA    R2,L'COMNAME(R2)                                                 
*                                                                               
         TM    GLARGS,X'08'                                                     
         BZ    *+10                                                             
         MVC   0(L'ACTLEXP,R2),ACTLEXP                                          
*                                                                               
         TM    GLARGS,X'04'                                                     
         BZ    *+10                                                             
         MVC   0(L'FLMDATE,R2),FLMDATE                                          
*                                                                               
         TM    GLARGS,X'02'                                                     
         BZ    *+10                                                             
         MVC   0(L'RECDATE,R2),RECDATE                                          
*                                                                               
         TM    GLARGS,X'01'                                                     
         BZ    *+10                                                             
         MVC   0(L'MUSDATE,R2),MUSDATE                                          
         B     XIT                                                              
         EJECT                                                                  
*              COMMERCIAL DETAILS                                               
*                                  ARGS X'80' = CID                             
*                                       X'40' = LENGTH                          
*                                       X'20' = MEDIA                           
*                                       X'10' = NAME                            
OUTCODET DS    0H                                                               
         CLI   GLARGS,X'B0'        IF PRINTING CID,MEDIA, & NAME                
         BNE   OUTCDT1                                                          
         MVI   MEDFLG,C'Y'                                                      
         BAS   RE,SETNCHOP         SET CID, MEDIA, & CHOP NAME                  
         B     XIT                                                              
*                                                                               
OUTCDT1  TM    GLARGS,X'80'                                                     
         BZ    OUTCDT2                                                          
         MVC   0(L'TGCID,R3),0(R2)                                              
         LA    R2,L'TGCID(R2)                                                   
         LA    R3,L'TGCID+1(R3)                                                 
         SPACE 1                                                                
OUTCDT2  TM    GLARGS,X'40'                                                     
         BZ    *+8                                                              
         BAS   RE,OUTLEN                                                        
         SPACE 1                                                                
         TM    GLARGS,X'20'        MEDIA                                        
         BZ    OUTCDT6                                                          
         CLI   MYLTYP,C'H'         IF THIS IS A HEADLINE                        
         BNE   OUTCDT4                                                          
         MVC   LABLAREA(5),=C'MEDIA'  SET UP FOR GENERAL HANDLING               
         MVC   CODEAREA(L'TGMENAME),TGMENAME                                    
         LA    RF,L'TGMENAME-1                                                  
         MVI   GLARGS,C'C'         SIMULATE CODE ONLY                           
         BAS   RE,RECOUT                                                        
         B     XIT                                                              
         SPACE 1                                                                
OUTCDT4  MVC   0(L'TGMENAME,R3),0(R2)                                           
         LA    R2,L'TGMENAME(R2)                                                
         LA    R3,L'TGMENAME+1(R3)                                              
         SPACE 1                                                                
OUTCDT6  TM    GLARGS,X'10'                                                     
         BZ    *+10                                                             
         MVC   0(L'COMNAME,R3),0(R2)                                            
         SPACE 1                                                                
         CLI   MYLTYP,C'M'                IF THIS IS A MIDLINE                  
         BNE   XIT                                                              
         GOTO1 SQUASHER,DMCB,GLAOFLD,64   SQUASH THE DATA                       
         B     XIT                                                              
         SPACE                                                                  
*              GENERAL ROUTINE TO EDIT COMML/LIFT LENGTH                        
         SPACE 1                                                                
OUTLEN   DS    0H                                                               
         CLI   MYLTYP,C'H'         IF WE'RE IN HEADS                            
         BE    *+12                                                             
         CLI   MYLTYP,C'M'         OR MIDS                                      
         BNE   *+12                                                             
         MVI   0(R3),C':'          PRECEED WITH COLON                           
         LA    R3,1(R3)                                                         
         EDIT  (1,0(R2)),(3,(R3)),ALIGN=LEFT                                    
         LA    R2,1(R2)                                                         
         LR    R1,R0                                                            
         LA    R3,1(R1,R3)                                                      
         BR    RE                                                               
         EJECT                                                                  
*              COMMERCIAL CONTRACT NUMBERS                                      
         SPACE 2                                                                
INCOCONT DS    0H                                                               
         MVC   0(48,R2),CONTRCTS                                                
         B     XIT                                                              
         SPACE 3                                                                
OUTCOCNT DS    0H                                                               
         LA    R0,4                                                             
OUTCONT2 CLC   0(12,R2),SPACES                                                  
         BNH   OUTCONT3                                                         
         MVC   0(12,R3),0(R2)                                                   
         LA    R3,LINELNQ(R3)                                                   
OUTCONT3 LA    R2,12(R2)                                                        
         BCT   R0,OUTCONT2                                                      
         B     XIT                                                              
         EJECT                                                                  
*              LIFT DETAILS                                                     
*                                  ARGS X'80' = CID                             
*                                       X'40' = LENGTH                          
INLIDET  DS    0H                                                               
         CLI   ELTALF,TALFELQ      TEST WE HAVE LIFT DETAILS                    
         BNE   XIT                                                              
         TM    GLARGS,X'80'                                                     
         BZ    *+14                                                             
         MVC   0(L'TALFLID,R2),ELTALF+TALFLID-TALFD                             
         LA    R2,L'TALFLID(R2)                                                 
         SPACE 1                                                                
         TM    GLARGS,X'40'                                                     
         BZ    *+10                                                             
         MVC   0(1,R2),ELTALF+TALFSEC-TALFD                                     
         B     XIT                                                              
         SPACE 3                                                                
*                                  ARGS X'80' = CID                             
*                                       X'40' = LENGTH                          
OUTLIDET DS    0H                                                               
         CLI   0(R2),0             TEST WE HAVE LIFT DETAILS                    
         BE    XIT                                                              
         TM    GLARGS,X'80'                                                     
         BZ    OUTLDT2                                                          
         MVC   0(L'TALFLID,R3),0(R2)                                            
         LA    R2,L'TALFLID(R2)                                                 
         LA    R3,L'TALFLID+1(R3)                                               
         SPACE 1                                                                
OUTLDT2  TM    GLARGS,X'40'                                                     
         BZ    *+8                                                              
         BAS   RE,OUTLEN                                                        
         SPACE 1                                                                
         CLI   MYLTYP,C'M'                IF THIS IS A MIDLINE                  
         BNE   XIT                                                              
         GOTO1 SQUASHER,DMCB,GLAOFLD,24   SQUASH THE DATA                       
         B     XIT                                                              
         EJECT                                                                  
*              LIFT DETAILS                                                     
INLIID   DS    0H                                                               
         CLI   ELTALF,TALFELQ      TEST WE HAVE LIFT DETAILS                    
         BNE   XIT                                                              
         MVC   0(L'TALFLID+1,R2),SPACES                                         
         MVC   0(L'TALFLID,R2),ELTALF+TALFLID-TALFD                             
         LA    R2,L'TALFLID+1(R2)                                               
         MVC   0(L'TALFLID+1,R2),SPACES                                         
         CLI   ELTALF2,TAL2ELQ     TEST WE HAVE LIFT DETAILS                    
         BNE   XIT                                                              
         MVC   0(L'TALFLID,R2),ELTALF2+TALFLID-TALFD                            
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
INLILEN  DS    0H                                                               
         CLI   ELTALF,TALFELQ      TEST WE HAVE LIFT DETAILS                    
         BNE   INLILEN2                                                         
         EDIT  (1,ELTALF+TALFSEC-TALFD),(3,(R2)),ALIGN=LEFT                     
         AHI   R2,3                                                             
         MVC   0(3,R2),SPACES                                                   
INLILEN2 CLI   ELTALF2,TAL2ELQ     TEST WE HAVE LIFT DETAILS                    
         BNE   XIT                                                              
         EDIT  (1,ELTALF2+TALFSEC-TALFD),(3,(R2)),ALIGN=LEFT                    
         B     XIT                                                              
*              CAST DETAILS                                                     
         SPACE 1                                                                
*                                  GLARGS   C=CATEGORY                          
*                                           D=DOUBLES                           
*                                           G=GUARANTEE?                        
*                                           L=LIFT?                             
*                                           M=CAMERA                            
*                                           P=CORP?                             
*                                           U=UNION                             
*                                           X=EXCHANGE RATE                     
*                                           Y=YEAR                              
*                                           1=OV1                               
*                                           2=OV2                               
*                                  GLARGS+1 S=INCLUDE SORT BYTES                
*                                           M=INCLUDE SORT BYTES (MINI)         
*                                  GLARGS+2 C=FOR OV1/2 FROM CASTLIST           
INCADET  DS    0H                                                               
         CLI   GLARGS+1,C'S'                                                    
         BE    *+12                                                             
         CLI   GLARGS+1,C'M'                                                    
         BNE   INCDT2                                                           
         MVC   0(1,R2),TGCSORT     SET CAST SORT SEQ. (BIT FLAGS)               
         MVC   1(1,R2),TGCSORT+2                      (CAT. SORT CODE)          
         CLI   GLARGS+1,C'M'       IF MINI SORT                                 
         BNE   *+8                                                              
         NI    0(R2),X'FD'         TURN OFF GUAR STATUS                         
         LA    R2,2(R2)                                                         
         SPACE 1                                                                
INCDT2   CLI   GLARGS,C'C'         CATEGORY                                     
         BNE   *+10                                                             
         MVC   0(3,R2),TGCACDE                                                  
         SPACE 1                                                                
         CLI   GLARGS,C'D'         DOUBLES                                      
         BNE   *+10                                                             
         MVC   0(1,R2),TCCADBL                                                  
         SPACE 1                                                                
         CLI   GLARGS,C'G'         GUAR?                                        
         BNE   INCDT3                                                           
         OC    TGGUA,TGGUA                                                      
         BZ    *+8                                                              
         MVI   0(R2),C'Y'                                                       
         SPACE 1                                                                
INCDT3   CLI   GLARGS,C'L'         LIFT?                                        
         BNE   INCDT4                                                           
         TM    TCCASTAT,TACASTLF   PERFORMER ON LIFT                            
         BZ    *+8                                                              
         MVI   0(R2),C'Y'                                                       
         TM    TCCASTAT,TACASTLO   ONLY ON LIFT                                 
         BZ    *+8                                                              
         MVI   0(R2),C'O'                                                       
         TM    TCCASTA4,TACAST2O   ONLY ON 2ND LIFT                             
         BZ    *+8                                                              
         MVI   0(R2),C'S'                                                       
         TM    TCCASTA4,TACAS2LF   ON MAIN + 2ND LIFT ONLY                      
         BZ    *+8                                                              
         MVI   0(R2),C'X'                                                       
         TM    TCCASTA4,TACASALL   ON MAIN + 1ST + 2ND LIFT                     
         BZ    *+8                                                              
         MVI   0(R2),C'A'                                                       
         TM    TCCASTA4,TACASL2O   ON 1ST + 2ND LIFT ONLY                       
         BZ    *+8                                                              
         MVI   0(R2),C'E'                                                       
         TM    CASTSTA2,TACAST2O   ONLY ON 2ND LIFT                             
         BZ    *+8                                                              
         MVI   0(R2),C'S'                                                       
         TM    CASTSTA2,TACAS2LF   ON MAIN + 2ND LIFT ONLY                      
         BZ    *+8                                                              
         MVI   0(R2),C'X'                                                       
         TM    CASTSTA2,TACASALL   ON MAIN + 1ST + 2ND LIFT                     
         BZ    *+8                                                              
         MVI   0(R2),C'A'                                                       
         TM    CASTSTA2,TACASL2O   ON 1ST + 2ND LIFT ONLY                       
         BZ    *+8                                                              
         MVI   0(R2),C'E'                                                       
         SPACE 1                                                                
INCDT4   CLI   GLARGS,C'M'         ON/OFF CAMERA                                
         BNE   *+10                                                             
         MVC   0(3,R2),TCCAONOF                                                 
         SPACE 1                                                                
         CLI   GLARGS,C'P'         CORP OR TRUSTEE?                             
         BNE   INCDT6                                                           
         CLI   TCW4TYPE,TAW4TYCO                                                
         BE    INCDT5                                                           
         CLI   TCW4TYPE,TAW4TYCA                                                
         BE    INCDT5                                                           
         CLI   TCW4TYPE,TAW4TYTR                                                
         BNE   INCDT6                                                           
INCDT5   MVI   0(R2),C'Y'                                                       
         SPACE 1                                                                
INCDT6   CLI   GLARGS,C'U'         UNION                                        
         BNE   *+10                                                             
         MVC   0(3,R2),TGUNCDE                                                  
         SPACE 1                                                                
         CLI   GLARGS,C'Y'         YEAR                                         
         BNE   *+10                                                             
         MVC   0(3,R2),TGYRCDE                                                  
         SPACE 1                                                                
         CLI   GLARGS,C'1'         OVERSCALE RATE 1                             
         BNE   INCDT8                                                           
         MVC   0(4,R2),TCOV1       MAY HAVE OVERRIDE                            
         OC    0(4,R2),0(R2)                                                    
         BNZ   INCDT8                                                           
         TM    OTHOPT,OTHOV1       IF OVERRIDE COULD BE ZERO                    
         BO    INCDT8                                                           
         L     R0,AIO                                                           
         MVC   AIO,TCACAST         GET ALL= FROM CAST REC.                      
         GOTO1 GETOV1,DMCB,SPACES,(R2)                                          
         ST    R0,AIO                                                           
         SPACE 1                                                                
INCDT8   CLI   GLARGS,C'2'         OVERSCALE RATE 2                             
         BNE   INCDT10                                                          
         MVC   0(4,R2),TCOV2                                                    
         OC    0(4,R2),0(R2)                                                    
         BNZ   INCDT10                                                          
         TM    OTHOPT,OTHOV1       IF OVERRIDE COULD BE ZERO                    
         BO    INCDT10                                                          
         L     R4,TCACAST                                                       
         MVI   ELCODE,TACAELQ      GET OV2 FROM CAST REC.                       
         BAS   RE,GETEL                                                         
         BNE   INCDT10                                                          
         USING TACAD,R4                                                         
         GOTO1 SETOV2,DMCB,(R4),TCACAST,SPACES                                  
         MVC   0(4,R2),TACAOV2                                                  
         SPACE 1                                                                
INCDT10  CLI   GLARGS,C'X'         EXCHANGE RATE                                
         BNE   INCDT15                                                          
         CLI   CANDOLS,C'Y'        ONLY DISPLAY IF COMMERCIAL                   
         BE    INCDT12             IS CANADIAN DOLLARS                          
         TM    OTHOPT,OTHEXCH      OR IF EXCHANGE RATE IS OVERRIDDEN            
         BZ    INCDT15                                                          
INCDT12  MVC   0(4,R2),DEFEXCH                                                  
*        OC    EXCHANGE,EXCHANGE                                                
*        BNZ   INCDT15                                                          
*        MVC   0(4,R2),DEFEXCH                                                  
*                                                                               
INCDT15  CLI   GLARGS,C'N'         NUMBER OF PERFORMERS                         
         BNE   INCDT20                                                          
         MVC   0(1,R2),TGNOPERF                                                 
*                                                                               
INCDT20  CLI   GLARGS,C'3'         CATEGORY NAME                                
         BNE   INCDT90                                                          
         SR    R1,R1                                                            
         L     RF,TGACATS                                                       
         USING CATTABD,RF                                                       
*                                                                               
INCDT23  CLI   0(RF),X'FF'         EOT?                                         
         BE    INCDT20X                                                         
         CLC   CATEQU,TGCAEQU                                                   
         BE    INCDT25                                                          
         ZIC   R1,CATLEN                                                        
         AR    RF,R1                                                            
         B     INCDT23                                                          
*                                                                               
INCDT25  IC    R1,CATLEN                                                        
         LA    R0,CATNAME-CATTABD                                               
         SR    R1,R0                                                            
         CHI   R1,32                                                            
         BNH   *+8                                                              
         LA    R1,32                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),CATNAME                                                  
                                                                                
INCDT20X DS    0H                                                               
*                                                                               
INCDT90  B     XIT                                                              
         SPACE 2                                                                
*                                  GLARGS   C=CATEGORY                          
*                                  GLARGS+1 S=INCLUDE SORT BYTES                
OUTCADET DS    0H                           M=INCLUDE SORT BYTE                 
         CLI   GLARGS+1,C' '                                                    
         BNH   *+8                                                              
         LA    R2,2(R2)                                                         
         MVC   0(3,R3),0(R2)                                                    
         B     XIT                                                              
         EJECT                                                                  
*              CYCLE DATES                                                      
         SPACE 1                                                                
INCYC    DS    0H                                                               
         MVC   0(6,R2),TCPCYC                                                   
         OC    0(6,R2),0(R2)       IF NO CYCLE DATES PRESENT                    
         BNZ   *+10                                                             
         MVC   0(3,R2),APPLYDTE    THEN MAKE CYCLE START APPLY DATE             
         SPACE 1                                                                
         CLI   GLARGS+1,C'U'       IF USE DETAILS ALSO REQUIRED                 
         BNE   XIT                                                              
         LA    R2,6(R2)            BUMP PAST DATES                              
         B     INUSDET             AND GO GET THEM                              
         SPACE 2                                                                
OUTCYC   DS    0H                                                               
         TM    GLINDS,GLTOTLIN     DON'T PRINT ON TOTAL LINES                   
         BO    XIT                                                              
         CLI   GLARGS,C'R'         IF THIS IS PLACE HOLDER FOR ROW              
         BNE   OCYC2                                                            
         CLI   GLARGS+1,C'E'       SAVE A(OUTPUT LOC.) BASED ON ARG2            
         BNE   *+12                                                             
         ST    R3,ACYCLEE          SAVE A(ESTIMATED CYCLE DATES)                
         B     XIT                                                              
         ST    R3,ACYCLEA          SAVE A(ACTUAL CYCLE DATES)                   
         B     XIT                                                              
         SPACE 1                                                                
OCYC2    LA    RE,ACYCLEE          WE MAY HAVE A(OVERRIDE OUTPUT AREA)          
         CLI   GLARGS,C'E'                                                      
         BE    *+8                                                              
         LA    RE,ACYCLEA                                                       
         ICM   R1,15,0(RE)         IF WE HAVE A(OVERRIDE OUTPUT AREA)           
         BZ    *+12                                                             
         LR    R3,R1               USE IT                                       
         XC    0(4,RE),0(RE)       AND CLEAR IT FOR NEXT TIME                   
         SPACE 1                                                                
         LTR   R3,R3               GET OUT IF NO OUTPUT AREA SET                
         BZ    XIT                                                              
         LA    RF,X'01'            SET HOB OF P1 BASED ON N'DATES               
         OC    3(3,R2),3(R2)                                                    
         BZ    *+8                                                              
         LA    RF,X'11'                                                         
         GOTO1 DATCON,DMCB,((RF),(R2)),(8,(R3))                                 
         SPACE 1                                                                
OCYCX    CLI   GLARGS+1,C'U'       IF USE DETAILS ALSO REQUIRED                 
         BNE   XIT                                                              
         LA    RE,HOLDE            WE MAY HAVE OVERRIDE OUTPUT AREA             
         CLI   GLARGS,C'E'                                                      
         BE    *+8                                                              
         LA    RE,HOLDA                                                         
         CLC   0(50,RE),SPACES     IF SOMETHING IN HOLD AREA                    
         BE    *+14                                                             
         MVC   0(23,R3),0(RE)      DISPLAY IT INSTEAD OF DATES                  
         B     XIT                                                              
         CLC   0(17,R3),SPACES     IF DATES HERE                                
         BNH   *+6                                                              
         LR    R3,RE               SET TO MOVE DETAILS TO HOLD AREA             
         ST    R3,MYOFLD                                                        
         LA    R2,6(R2)            BUMP PAST DATES                              
         B     OUTUSDET            AND GO HANDLE                                
HOLDE    DC    CL50' '                                                          
HOLDA    DC    CL50' '                                                          
         EJECT                                                                  
*              ROUTINE TO SET USE CODE                                          
         SPACE 1                                                                
INUSCDE  DS    0H                                                               
         MVC   0(3,R2),TGUSCDE     SET 3 LETTER USE CODE                        
         B     XIT                                                              
         SPACE 2                                                                
*              USE NAME AND DETAILS                                             
         SPACE 1                                                                
*                                  GLARGS S = SORT                              
INUSNMD  DS    0H                                                               
         LA    RF,INUSENM          FIRST GET USE NAME                           
         BAS   RE,GETIT                                                         
         LA    R2,16(R2)                                                        
         CLI   GLARGS,C'S'                                                      
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
         B     INUSDET             NOW GET USE DETAILS                          
         SPACE 2                                                                
*                                  GLARGS S = SORT                              
OUTUSNMD DS    0H                                                               
         TM    GLINDS,GLTOTLIN     DON'T PRINT ON TOTAL LINES                   
         BO    XIT                                                              
         CLI   GLARGS,C'S'                                                      
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
         MVC   0(16,R3),0(R2)                                                   
         LA    R3,17(R3)           BUMP TO NEXT SLOT IN O/P AREA                
         LA    R2,16(R2)           AND BUMP TO USE DETAILS                      
         B     OUTUSDET            DISPLAY USE DETAILS - WILL SQUASH            
         EJECT                                                                  
*              USE NAME, IGNORING UPGRADES                                      
*                                  GLARGS S = SORT                              
*                                  GLARGS+2=M - COMBINE MUSIC USES TO           
*                                               JUST HAVE TOTAL LINE            
INUSNMXU DS    0H                                                               
         XC    HALF,HALF                                                        
         TM    TGUSTYST,UPGRADE    IF USE TYPE IS AN UPGRADE                    
         BZ    INUSNX8                                                          
         MVC   HALF(1),TGUSEQU                SAVE USE CODE EQUATE              
         MVC   HALF+1(1),TGUSTYP                   USE TYPE EQUATE              
         GOTO1 USEVAL,DMCB,TGUPTOCD,TGUPTOTY  SET GLOBAL FOR DEST USE           
         SPACE 1                                                                
INUSNX8  LA    RF,INUSENM          GET USE NAME                                 
         BAS   RE,GETIT                                                         
         SPACE 1                                                                
         OC    HALF,HALF           IF HAVE SAVED USE INFO                       
         BZ    INUSNXX                                                          
         GOTO1 USEVAL,DMCB,(X'80',HALF),HALF+1  RESTORE TO GLOBALS              
INUSNXX  B     XIT                                                              
         SPACE                                                                  
*                                  GLARGS S = SORT                              
*                                  GLARGS+2=M - COMBINE MUSIC USES TO           
*                                               JUST HAVE TOTAL LINE            
*                                  GLARGS+3=I - IGNORE USE DETAILS              
OUTUSNXU DS    0H                                                               
         TM    GLINDS,GLTOTLIN     TEST THIS IS TOTALS                          
         BO    XIT                                                              
         LR    R1,R2                                                            
         CLI   GLARGS,C'S'         IF SPECIAL SORT                              
         BNE   OUTUSNX2                                                         
         CLI   GLARGS+2,C'M'       AND WANT ALL MUSIC USES COMBINED             
         BNE   OUTUSNX1                                                         
         CLI   0(R1),SRTEQMUS      AND MUSIC USE                                
         BNE   OUTUSNX1                                                         
         OI    STATUS2,NOPRINT     SET DON'T PRINT LINE                         
OUTUSNX1 LA    R1,1(R1)                                                         
         SPACE 1                                                                
OUTUSNX2 MVC   0(16,R3),0(R1)                                                   
         MVC   SVUSNAME,0(R1)      SAVE USENAME FOR OUTTULIT ROUTINE            
         SPACE 1                                                                
         CLI   GLARGS+3,C'I'       IF NOT IGNORING USE DETAILS                  
         BE    XIT                                                              
         OC    DISPUSED,DISPUSED   IF HAVE DISP. TO USE DETAILS                 
         BZ    XIT                                                              
         LR    R4,R2               POINT TO THEM                                
         AH    R4,DISPUSED                                                      
         GOTO1 USEVAL,DMCB,(X'80',11(R4)),12(R4)  LOOK UP ACTUAL NAME           
         MVC   0(16,R3),TGUSNAME   DISPLAY THAT NAME                            
         SPACE 1                                                                
         CLI   GLARGS+1,C'D'       IF DETAILS COMES NEXT                        
         BNE   XIT                                                              
         LA    R3,17(R3)           BUMP TO NEXT SLOT IN O/P AREA                
         AH    R2,DISPUSED         BUMP TO USE DETAILS                          
         B     OUTUSDET            DISPLAY USE DETAILS - WILL SQUASH            
         EJECT                                                                  
*              USE NAME                                                         
*                                  GLARGS S = SORT                              
*                                  GLARGS+2=M - COMBINE MUSIC USES TO           
*                                               JUST HAVE TOTAL LINE            
INUSENM  DS    0H                                                               
         ST    R2,AUSENAME                                                      
         XR    R1,R1               CLEAR IND FOR SPECIAL MUSIC TITLE            
         CLI   GLARGS,C'S'         SPECIAL SORT REQUESTED                       
         BNE   INUSENX                                                          
         CLI   TGUSEQU,UGRT        GUARANTEES FIRST                             
         BE    INUSEN2                                                          
         MVI   0(R2),SRTEQSES                                                   
         TM    TGUSSTAT,SESSION    THEN SESSIONS                                
         BO    INUSEN2                                                          
         MVI   0(R2),SRTEQHLD                                                   
         CLI   TGUSEQU,UHLD        THEN HOLDING FEES                            
         BE    INUSEN2                                                          
         MVI   0(R2),SRTEQNMU                                                   
*        TM    TGUSXUNI,AFM        THEN NON-MUSIC USAGE                         
         GOTO1 UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         LHI   R1,0                                                             
         BO    INUSEN1                                                          
         MVI   0(R2),SRTEQMUS      THEN MUSIC USAGE                             
         CLI   GLARGS+2,C'M'       IF REQUEST TO COMBINE MUSIC                  
         BNE   INUSEN1                                                          
         LA    R1,1                SET INDICATOR TO TITLE 'MUSIC'               
INUSEN1  TM    TGUSTYST,UPGRADE                                                 
         BZ    INUSEN2                                                          
         MVI   0(R2),SRTEQUPG      AND LAST IS UPGRADES                         
INUSEN2  LA    R2,1(R2)                                                         
INUSENX  MVC   0(16,R2),TGUSNAME                                                
         LTR   R1,R1               IF TITLING MUSIC USAGE 'MUSIC'               
         BZ    *+10                                                             
         MVC   0(16,R2),=CL16'MUSIC'                                            
         B     XIT                                                              
         SPACE 2                                                                
*                                  GLARGS S = SORT                              
*                                  GLARGS+1 D = PROC. USE DETAILS ALSO          
OUTUSENM DS    0H                                                               
         TM    GLINDS,GLTOTLIN     TEST THIS IS TOTALS                          
         BO    XIT                                                              
         LR    R1,R2                                                            
         CLI   GLARGS,C'S'                                                      
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         MVC   0(16,R3),0(R1)                                                   
         CLI   GLARGS+1,C'D'       IF DETAILS COMES NEXT                        
         BNE   XIT                                                              
         LA    R3,17(R3)           BUMP TO NEXT SLOT IN O/P AREA                
         OC    DISPUSED,DISPUSED   IF HAVE DISP. TO USE DETAILS                 
         BZ    XIT                                                              
         AH    R2,DISPUSED         BUMP TO USE DETAILS                          
         B     OUTUSDET            DISPLAY USE DETAILS - WILL SQUASH            
         EJECT                                                                  
*              ACCUMULATOR LITERAL ROUTINES                                     
         SPACE 2                                                                
*                                  R3=L'INPUT - 1                               
INPAYLIT DS    0H                                                               
         CLI   SUMINDEX,NALL       IF INDEX GT N'LITERALS                       
         BNH   INPYLIT2                                                         
         MVC   0(1,R2),SUMINDEX    SET HIGH INDEX                               
         MVC   1(L'SUMLIT,R2),SUMLIT  ASSUME LITERAL IS BEING PASSED            
         B     XIT                                                              
INPYLIT2 BAS   RE,SETALLTB         SET R4=A(ACCUM TABLE)                        
         ZIC   R1,SUMINDEX         INDEX BEING PASSED                           
         BCTR  R1,0                                                             
         MH    R1,=AL2(ALLTLNQ)                                                 
         AR    R4,R1               R4=A(TABLE ENTRY FOR THIS ACCUM)             
         USING ALLTD,R4                                                         
         OC    ALLTRTN,ALLTRTN     IF NO ROUTINE, THEN GET OUT                  
         BZ    XIT                                                              
         ZIC   R1,SUMINDEX         INDEX INTO ACCLITS                           
         STC   R1,0(R2)            SET INDEX FOR SORT                           
         BCTR  R1,0                                                             
         MH    R1,=AL2(LITLNQ)                                                  
         LA    R4,ACCLITS(R1)                                                   
         USING LITD,R4                                                          
         LA    R1,LITLIT           ASSUME WE WILL USE REGULAR LITERAL           
         BCTR  R3,0                SUBTRACT FROM L'INPUT FOR SORT BYTE          
         CLM   R3,1,=AL1(L'LITLIT-1)                                            
         BNL   *+8                                                              
         LA    R1,LITSHORT         NEED SMALLER LITERAL                         
         EX    R3,*+8                                                           
         B     XIT                                                              
         MVC   1(0,R2),0(R1)       SET LITERAL                                  
*                                                                               
         SPACE 3                                                                
OUTPYLIT DS    0H                                                               
         CLC   =C'MISCREC',GLRECLAB   IF MISC. ADDITIONS SUMMARY                
         BNE   OUTPYLT1                                                         
         CLI   0(R2),NALL          ONLY PRINT MISC. AMT & TOTAL                 
         BH    OUTPYLT2                                                         
         OI    STATUS2,NOPRINT                                                  
         B     OUTPYLT2                                                         
*                                                                               
OUTPYLT1 CLI   0(R2),NALL          IF THIS IS TOTALS LIT                        
         BNE   *+12                                                             
         OI    STATUS2,NOPRINT     SET DON'T PRINT THIS LINE                    
         B     XIT                                                              
*                                                                               
OUTPYLT2 ZIC   R1,MYOLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),1(R2)       ELSE MOVE OUT LITERAL                        
*                                                                               
         BAS   RE,SUBTFLAG         SET SUBTOTAL LINE FLAG                       
*                                                                               
         CLC   ACOLIT+1(10),0(R3)  IF CURRENT LITERAL IS                        
         BE    OUTPYLT3            AGENCY COMMISSION                            
         CLC   ACOLIT+11(20),0(R3)                                              
         BNE   OUTPYLT8                                                         
*                                                                               
OUTPYLT3 LA    R2,0(R3)                                                         
         TM    SFCMSTAT,SFCMSFEE   IF SIGNATORY FEE HAS BEEN                    
         BZ    OUTPYLT5            CALCULATED                                   
         TM    SFCMSTAT,SFCMCOMM   AND ACCOUNTS FOR ENTIRE                      
         BO    OUTPYLT4            AMOUNT OF AGY COMMISSION                     
         LA    R2,=CL20'SIGNATORY FEE'                                          
         CLM   R1,1,=AL1(L'LITLIT-1)                                            
         BNL   *+8                                                              
         LA    R2,=CL10'SIGN. FEE' CHANGE LITERAL TO SIGNATORY FEE              
         B     OUTPYLT7                                                         
*                                                                               
OUTPYLT4 LA    R2,=CL20'AGY COM.+SIGN. FEE'   IF SIGNATORY FEE DOES             
         CLM   R1,1,=AL1(L'LITLIT-1)          NOT ACCOUNT FOR ENTIRE            
         BNL   *+8                            COMMISSION, COMBINE               
         LA    R2,=CL10'COMM+SIGN'            LITERALS                          
*                                                                               
OUTPYLT5 CLI   PROFBTYP,TABRTY20              IF BILLING TYPE 20,               
         BNE   OUTPYLT6                       CHANGE LITERAL TO                 
         LA    R2,=CL20'TALENT MASTERS FEE'   TALENT MASTERS FEE                
         CLC   TGAGY,=CL6'0585'               GRUPO GALLEGOS?                   
         BNE   *+8                                                              
         LA    R2,=CL20'LUNA PROD && TALENT'  LUNA PRODUCTIONS                  
         CLM   R1,1,=AL1(L'LITLIT-1)                                            
         BNL   OUTPYT5A                                                         
         LA    R2,=CL10'TAL MASTER'                                             
         CLC   TGAGY,=CL6'0585'               GRUPO GALLEGOS?                   
         BNE   *+8                                                              
         LA    R2,=CL10'LUNA P && T'                                            
*                                                                               
OUTPYT5A TM    SFCMSTAT,SFCMSFEE              IF SIGNATORY FEE WAS              
         BZ    OUTPYLT7                       CALCULATED TOO, COMBINE           
         LA    R2,=CL20'TAL MASTERS+SIGN.FEE' LITERALS                          
         CLC   TGAGY,=CL6'0585'               GRUPO GALLEGOS?                   
         BNE   *+8                                                              
         LA    R2,=CL20'LUNA P&&T+SIGN.FEE'   LITERALS                          
         CLM   R1,1,=AL1(L'LITLIT-1)                                            
         BNL   OUTPYLT7                                                         
         LA    R2,=CL10'MAST+SIGN'                                              
         CLC   TGAGY,=CL6'0585'               GRUPO GALLEGOS?                   
         BNE   *+8                                                              
         LA    R2,=CL10'LUNA+SIGN'                                              
         B     OUTPYLT7                                                         
*                                                                               
OUTPYLT6 CLI   PROFBTYP,TABRTYE               IF BILLING TYPE E                 
         BNE   OUTPYLT7                       CHANGE LITERAL TO                 
         LA    R2,=CL20'EMS FEE'              EMS FEE                           
         CLM   R1,1,=AL1(L'LITLIT-1)                                            
         BNL   *+8                                                              
         LA    R2,=CL10'EMS FEE'                                                
*                                                                               
         TM    SFCMSTAT,SFCMSFEE              IF SIGNATORY FEE WAS              
         BZ    OUTPYLT7                       CALCULATED TOO, COMBINE           
         LA    R2,=CL20'EMS FEE+SIGN.FEE'     LITERALS                          
         CLM   R1,1,=AL1(L'LITLIT-1)                                            
         BNL   OUTPYLT7                                                         
         LA    R2,=CL10'EMS+SIGN'                                               
*                                                                               
OUTPYLT7 EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R3),0(R2)       SET LITERAL                                  
         B     XIT                                                              
*                                                                               
OUTPYLT8 CLC   CSFLIT+1(10),0(R3)  IF CURRENT LITERAL IS                        
         BE    OUTPYLT9            CSF                                          
         CLC   CSFLIT+11(20),0(R3)                                              
         BNE   XIT                                                              
OUTPYLT9 LA    R2,0(R3)                                                         
         TM    SFCMSTAT,SFCMCSF    IF COMM SERVICE FEE HAS BEEN                 
         BZ    XIT                 CALCULATED                                   
         LA    R2,=CL20'COMMERCL SERVICE FEE'                                   
         CLM   R1,1,=AL1(L'LITLIT-1)                                            
         BNL   *+8                                                              
         LA    R2,=CL10'CSF'       CHANGE LITERAL TO COMM SERVICE FEE           
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R3),0(R2)       SET LITERAL                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET FLAG IF SUBTOTAL LINE                             
*                                                                               
SUBTFLAG NTR1                                                                   
         MVI   SUBTLINE,C'N'       SET NOT SUBTOTAL LINE                        
         EX    R1,SUBT1            IF LITERAL IS ONE OF THESE                   
         BE    SUBTFLGY                                                         
         EX    R1,SUBT2                                                         
         BE    SUBTFLGY                                                         
         EX    R1,SUBT3                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
SUBTFLGY MVI   SUBTLINE,C'Y'       SET THIS IS SUBTOTAL LINE                    
         B     XIT                                                              
         SPACE 1                                                                
SUBT1    CLC   0(0,R3),SUBTLIT                                                  
SUBT2    CLC   0(0,R3),SUBTLITS                                                 
SUBT3    CLC   0(0,R3),NETTLIT                                                  
         SPACE 2                                                                
*              SET A(MULTIPLE ACCUM TABLE)                                      
         SPACE 1                                                                
SETALLTB DS    0H                                                               
         L     R4,=A(ALLTAB1)      R4=A(MULTIPLE ACCUM TABLE)                   
         CLI   GLOPTS,C'Y'         SEPARATE MUSIC                               
         BE    SETTB2                                                           
         CLI   GLOPTS+2,C' '       T&H COMBINED                                 
         BH    *+8                                                              
         L     R4,=A(ALLTAB3)                                                   
         B     SETTBX                                                           
         SPACE 1                                                                
SETTB2   L     R4,=A(ALLTAB2)                                                   
         CLI   GLOPTS+2,C' '       T&H COMBINED                                 
         BH    *+8                                                              
         L     R4,=A(ALLTAB4)                                                   
SETTBX   BR    RE                                                               
         EJECT                                                                  
*              ACCUMULATOR ROUTINES                                             
         SPACE 2                                                                
INPAYALL DS    0H                                                               
         LR    R3,R2               R3=A(ACCUMS)                                 
         USING ALLD,R3                                                          
         SPACE 1                                                                
         CLI   SUMINDEX,0          IF PROCESSING ESTIMATE SUMMARY               
         BE    INPYAL1                                                          
         CLI   SUMINDEX,NALL       AND INDEX GT N'LITERALS                      
         BL    INPYAL2                                                          
         BE    INPYALX             (DON'T PROCESS NALL - FOR TOTALS)            
         CLI   GLARGS+10,C'Q'      AND COLUMN FILTER NOT PRESENT                
         BE    INPYALX                                                          
         MVC   0(4,R2),SUMAMT      ASSUME AMOUNT IS BEING PASSED                
         MVI   7(R2),1             FORCE ROW TO PRINT                           
         B     ACCX                                                             
         SPACE 1                                                                
INPYAL1  LA    RF,INPAY            FIRST GET PAYMENT AMOUNT                     
         LA    R2,ALLPAY           (FOR DETAIL PRINTING)                        
         BAS   RE,GETIT                                                         
         SPACE 1                                                                
INPYAL2  XC    GROSS,GROSS         PRE-CLEAR TOTAL                              
         SPACE 1                                                                
         BAS   RE,SETALLTB         SET R4=A(ACCUM TABLE)                        
         SPACE 1                                                                
         CLI   SUMINDEX,0          IF PROCESSING ESTIMATE SUMMARY               
         BE    INPYAL4                                                          
         ZIC   R1,SUMINDEX         INDEX BEING PASSED                           
         BCTR  R1,0                                                             
         MH    R1,=AL2(ALLTLNQ)                                                 
         AR    R4,R1               R4=A(TABLE ENTRY FOR THIS ACCUM)             
         SPACE 1                                                                
         USING ALLTD,R4                                                         
INPYAL4  ICM   RF,15,ALLTRTN       SET A(INPUT ROUTINE)                         
         BZ    INPYAL8                                                          
         MVC   GLARGS(1),ALLTARG   SET ARGUMENT FROM TABLE ENTRY                
         CLI   SUMINDEX,0          IF PROCESSING ESTIMATE SUMMARY, SKIP         
         BNE   INPYAL6                                                          
         ZIC   R2,ALLTDSP          GET DISP. TO ACCUM                           
         AR    R2,R3               R2=A(ACCUM)                                  
         SPACE 1                                                                
INPYAL6  BAS   RE,GETIT            GO GET IT                                    
         SPACE 1                                                                
INPYAL8  CLI   SUMINDEX,0          IF PROCESSING ESTIMATE SUMMARY               
         BNE   INPYALX             THEN DONE                                    
         LA    R4,ALLTNEXT         ELSE BUMP TO NEXT TABLE ENTRY                
         CLI   0(R4),X'FF'         AND IF HAVEN'T REACHED END, CONTINUE         
         BNE   INPYAL4                                                          
INPYALX  B     XIT                                                              
         SPACE 3                                                                
GETIT    NTR1                                                                   
         BR    RF                                                               
         EJECT                                                                  
*              MULTIPLE ACCUMULATOR ENTRY OUTPUT ROUTINE                        
*                                  GLARGS+3=LVL # PRINT ALL AMOUNTS             
*                                  GLARGS+4=T - PRINT 'NET+P&H' TOTAL           
         SPACE                                                                  
OUTPYALL DS    0H                                                               
         TM    GLINDS,GLTOTLIN     IF THIS ISN'T TOTALS                         
         BO    OUTP4                                                            
         CLI   GLARGS,C'F'         OR FORCING TOTALS                            
         BE    OUTP4                                                            
         CLI   GLARGS,C'T'         GET OUT IF WANT TOTALS ONLY                  
         BE    XIT                                                              
         LTR   R3,R3               GET OUT IF NO OUTPUT AREA                    
         BZ    XIT                                                              
         EDIT  (4,0(R2)),(12,(R3)),2,MINUS=YES,ZERO=NOBLANK  PRINT ONLY         
         B     XIT                                           PAY AMOUNT         
         SPACE 1                                                                
OUTP4    DS    0H                  ELSE PRINT ENTIRE STACK                      
         CLI   GLARGS,C'D'         GET OUT IF WANT DETAILS ONLY                 
         BE    XIT                                                              
         LA    R4,ACCLITS                                                       
         USING LITD,R4                                                          
         LA    R2,ALLFRST-ALLD(R2)                                              
         LA    RE,NALL                                                          
         CLI   GLARGS+3,0          IF LEVEL NUMBER SPECIFIED                    
         BE    OUTP6                                                            
         CLC   GLLEVEL,GLARGS+3    AND MATCH ON IT                              
         BE    OUTP6                                                            
         LA    RE,1                ELSE, JUST PRINT FIRST PAY AMOUNT            
         OC    0(4,R2),0(R2)       IF NO TALENT NET                             
         BNZ   OUTP6                                                            
         LA    R2,ALLPAYM-ALLPAYT(R2) SET TO SHOW MUSIC NET                     
         LA    R4,MUSICLIT                                                      
         SPACE 1                                                                
OUTP6    LA    R1,COMBLIT          IF UP TO TOTAL NET AND P&H                   
         CR    R1,R4                                                            
         BNE   *+12                                                             
         CLI   GLARGS+4,C'T'       AND NOT ASKING TO PRINT THIS LINE            
         BNE   OUTP16              SKIP IT                                      
         SPACE 1                                                                
         LA    R1,NETLIT           IF NET SUBTOTAL                              
         CR    R1,R4                                                            
         BNE   *+12                                                             
         CLI   GLOPTS+6,C'S'       AND NOT ASKING TO PRINT THIS LINE            
         BNE   OUTP16              SKIP IT                                      
         SPACE 1                                                                
         CLI   GLARGS+1,C'L'       TEST SET LITERALS ONLY                       
         BNE   *+12                                                             
         MVI   LITYORN,C'N'        INITIALIZE TO NOT PRINT                      
         B     OUTP8                                                            
         SPACE 1                                                                
         CLI   GLARGS+1,C'X'       IF SUPPRESSING LITERAL                       
         BE    *+12                                                             
         CLI   GLARGS+1,0          OR FORCING PRINTING LITERAL                  
         BE    OUTP8                                                            
         CLI   LITYORN,C'Y'        TEST WHETHER WE NEED TO PRINT ROW            
         BE    OUTP10                                                           
         B     OUTP16              ELSE SKIP TO NEXT                            
         SPACE 1                                                                
OUTP8    OC    0(4,R2),0(R2)       SKIP THIS ENTRY IF ZERO                      
         BZ    OUTP16                                                           
         MVI   LITYORN,C'Y'        SET TO PRINT THIS COLUMN                     
         CLI   GLARGS+1,C'L'       GET OUT IF SET LITS ONLY                     
         BE    OUTP16                                                           
         SPACE 1                                                                
OUTP10   CLI   GLARGS+1,C'X'       IF NOT SUPPRESSING LITERAL                   
         BE    OUTP12                                                           
         ZIC   RF,GLARGS+1         DIFF IN WIDTH BETWEEN L'LITLIT AND           
         LCR   RF,RF               PREVIOUS COLUMN                              
         BM    *+8                                                              
         LH    RF,=H'-3'                                                        
         AR    RF,R3                                                            
         SH    RF,=AL2(L'LITLIT+1)                                              
         MVC   0(L'LITLIT,RF),LITLIT  DISPLAY LITERAL                           
         CLI   PROFBTYP,TABRTY9    IF THIS IS BILLING TYPE 9                    
         BNE   OUTP11                                                           
         LA    R1,TNHLIT           AND THIS IS T&H LITERAL                      
         CR    R1,R4                                                            
         BNE   OUTP12                                                           
         MVC   0(L'LITLIT,RF),PTAXLIT+LITLIT-LITD  DISP 'PAYROLL TAXES'         
*                                                                               
OUTP11   LA    R1,ACOLIT                      IF CURRENT LITERAL IS             
         CR    R1,R4                          AGENCY COMMISSION                 
         BNE   OUTP11D                                                          
*                                                                               
         TM    SFCMSTAT,SFCMSFEE              IF SIGNATORY FEE HAS              
         BZ    OUTP11B                        BEEN CALCULATED, ADJUST           
         TM    SFCMSTAT,SFCMCOMM              LITERAL                           
         BO    OUTP11A                                                          
         MVC   0(20,RF),=CL20'SIGNATORY FEE'                                    
         B     OUTP12                                                           
OUTP11A  MVC   0(20,RF),=CL20'AGY COM.+SIGN. FEE'                               
*                                                                               
OUTP11B  CLI   PROFBTYP,TABRTY20              IF BILLING TYPE 20,               
         BNE   OUTP11C                        RESET TO MASTERS FEE              
         MVC   0(20,RF),=CL20'TALENT MASTERS FEE'                               
         CLC   TGAGY,=CL6'0585'               GRUPO GALLEGOS?                   
         BNE   *+10                                                             
         MVC   0(20,RF),=CL20'LUNA PROD && TALENT'                              
         TM    SFCMSTAT,SFCMSFEE                                                
         BZ    OUTP12                                                           
         MVC   0(20,RF),=CL20'TAL MASTERS+SIGN.FEE'                             
         CLC   TGAGY,=CL6'0585'               GRUPO GALLEGOS?                   
         BNE   *+10                                                             
         MVC   0(20,RF),=CL20'LUNA P&&T+SIGN.FEE'                               
         B     OUTP12                                                           
*                                                                               
OUTP11C  CLI   PROFBTYP,TABRTYE               IF BILLING TYPE E                 
         BNE   OUTP12                         RESET TO EMS FEE                  
         MVC   0(20,RF),=CL20'EMS FEE'                                          
         TM    SFCMSTAT,SFCMSFEE                                                
         BZ    OUTP12                                                           
         MVC   0(20,RF),=CL20'EMS FEE+SIGN.FEE'                                 
         B     OUTP12                                                           
*                                                                               
OUTP11D  LA    R1,CSFLIT                      IF CURRENT LITERAL IS             
         CR    R1,R4                          CSF                               
         BNE   OUTP12                                                           
         TM    SFCMSTAT,SFCMCSF               IF COMM SERVICE FEE HAS           
         BZ    OUTP12                         BEEN CALCULATED, ADJUST           
         MVC   0(20,RF),=CL20'COMMERCL SERVICE FEE'  LITERAL                    
*                                                                               
OUTP12   EDIT  (4,0(R2)),(12,(R3)),2,MINUS=YES  DISPLAY AMOUNT                  
         LA    R3,LINELNQ(R3)                                                   
         SPACE 1                                                                
OUTP16   LA    R2,4(R2)                                                         
         LA    R4,LITNEXT                                                       
         BCT   RE,OUTP6                                                         
         CLI   GLARGS+2,C'S'       SKIP A LINE                                  
         BNE   *+8                                                              
         MVI   0(R3),0                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE EDITS ACCUMULATORS WHOSE SIZE MAY REQUIRE P2             
         SPACE 2                                                                
*                                  GLARGS = V  -  COMPUTE VARIANCE              
OUTSIZE  DS    0H                                                               
         CLI   GLARGS,C'V'         IF THIS IS VARIANCE                          
         BNE   OSIZ10                                                           
         SH    R2,=H'8'            ASSUME PREV 2 FWDS ARE EST/ACT               
         L     R1,4(R2)            COMPUTE ACTUAL                               
         S     R1,0(R2)            LESS ESTIMATED                               
         ST    R1,8(R2)            IS VARIANCE                                  
         LA    R2,8(R2)                                                         
         SPACE 1                                                                
OSIZ10   BCTR  R3,0                SLIDE BACK (OVER LHS COLUMN)                 
         EDIT  (4,0(R2)),(10,(R3)),2,FLOAT=-  EDIT TO 1 MORE THAN L'COL         
         SPACE 1                                                                
         CLI   0(R3),X'40'         IS THERE SIGNIFICANT DATA IN 1ST POS         
         BNH   XIT                 NO, SO DONE                                  
         SPACE 1                                                                
         MVC   0(10,R3),SPACES     YES, SO CLEAR IT                             
         SH    R3,=H'2'            SLIDE BACK TWO MORE INTO PREV COL            
         SPACE 1                                                                
         CLI   0(R3),X'40'         IF THERE IS SIGNIFICANT DATA THERE           
         BNH   *+8                                                              
         LA    R3,LINELNQ(R3)      SET TO PRINT ON NEXT LINE                    
         SPACE 1                                                                
         EDIT  (4,0(R2)),(12,(R3)),2,FLOAT=-  AND EDIT TO NEXT LINE             
         B     XIT                                                              
         EJECT                                                                  
*              ACCUMULATOR INPUT ROUTINES                                       
         SPACE 2                                                                
*                                  PAY  GLARGS   M=MUSIC ONLY                   
*                                                T=TALENT ONLY                  
INPAY    DS    0H                                                               
         BAS   RE,CKMUSTAL         CHECK IF OK BASED ON GLARGS                  
         L     R1,TCPAYI           IF RETURNED THEN OK                          
         A     R1,TCPAYC                                                        
         A     R1,TCEXP                                                         
         CLI   PROFHNW,C'Y'        IF WANT HNW IN NET                           
         BNE   INPAY5                                                           
         CLI   GLARGS,C'T'         AND NOT TALENT NET                           
         BE    INPAY5                                                           
         CLI   GLARGS+1,C'2'       AND RECAP 2                                  
         BNE   INPAY5                                                           
         A     R1,TCHNW            ADD IT IN                                    
INPAY5   ST    R1,0(R2)                                                         
         B     ACCX                                                             
         SPACE 2                                                                
*                                  P&H  GLARGS  M=MUSIC ONLY                    
*                                               T=TALENT ONLY                   
INPNH    DS    0H                                                               
         BAS   RE,CKMUSTAL         CHECK IF OK BASED ON GLARGS                  
         MVC   0(4,R2),TCPNH       IF RETURNED THEN OK                          
         B     ACCX                                                             
*                                  NET+PNH+INR+TCSF GLARGS T=TAL ONLY           
INSUBT   DS    0H                                                               
         BAS   RE,CKMUSTAL         CHECK IF OK BASED ON GLARGS                  
         L     R1,TCPAYI           IF RETURNED THEN OK                          
         A     R1,TCPAYC                                                        
         A     R1,TCEXP                                                         
         A     R1,TCPNH                                                         
         A     R1,TCINR                                                         
         A     R1,CSFAMT                                                        
         ST    R1,0(R2)                                                         
         B     XIT                 DON'T ADD TO GROSS                           
         SPACE 2                                                                
*                                  EVERYTHING BUT COMMISSION AND GROSS          
INNETT   DS    0H                                                               
         BAS   RE,CKMUSTAL         CHECK IF OK BASED ON GLARGS                  
         L     R1,TCPAYI           IF RETURNED THEN OK                          
         A     R1,TCPAYC                                                        
         A     R1,TCEXP                                                         
         A     R1,TCPNH                                                         
         A     R1,TCINR                                                         
         A     R1,TCHNW                                                         
         A     R1,CSFAMT                                                        
         A     R1,TAXAMT                                                        
         A     R1,HNDAMT                                                        
         ST    R1,0(R2)                                                         
         B     XIT                 DON'T ADD TO GROSS                           
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
         SPACE 2                                                                
INSCALE  DS    0H                  SCALE                                        
         MVC   0(4,R2),TCGROSS                                                  
         B     ACCX                                                             
         EJECT                                                                  
*              ACCUMULATOR INPUT ROUTINES                                       
         SPACE 2                                                                
*                                  T&H  GLARGS  T=PAYROLL TAXES ONLY            
*                                               H=HANDLING FEES ONLY            
INTNH    DS    0H                                                               
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
INGUARCR DS    0H                  GUARANTEE CREDITS                            
         BAS   RE,SUMCHECK                                                      
         MVC   0(4,R2),TCGUAR                                                   
         B     XIT                                                              
         SPACE 2                                                                
INAPPLCR DS    0H                  APPLIED CREDITS                              
         BAS   RE,SUMCHECK                                                      
         MVC   0(4,R2),TCAPPLCR                                                 
         B     XIT                                                              
         SPACE 2                                                                
INAPCD   DS    0H                  APPLIED CODE                                 
         OC    TCAPPLCR,TCAPPLCR                                                
         BZ    *+10                                                             
         MVC   0(1,R2),TCAPPLCD                                                 
         B     XIT                                                              
         SPACE 2                                                                
INAPDATE DS    0H                  APPLY DATE                                   
         MVC   0(3,R2),APPLYDTE                                                 
         B     XIT                                                              
         SPACE 3                                                                
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
         MVC   0(4,R2),TCINR                                                    
         B     ACCX                                                             
         SPACE 2                                                                
INCSF    DS    0H                  CONTRACT SERVICE FEE                         
         MVC   0(4,R2),CSFAMT                                                   
         B     ACCX                                                             
         SPACE 2                                                                
INHNW    DS    0H                  HEALTH & WELFARE                             
         CLI   PROFHNW,C'Y'        IF REQUESTED IN NET                          
         BNE   INHNW5                                                           
         CLI   GLARGS,C'M'         AND THIS IS REALLY FROM INPAYALL             
         BNE   INHNW5                                                           
         CLI   GLARGS+1,C'2'       AND RECAP 2                                  
         BE    ACCX                DON'T SHOW SEPERATELY                        
INHNW5   MVC   0(4,R2),TCHNW                                                    
         B     ACCX                                                             
         SPACE 2                                                                
INCOMM   DS    0H                  AGENCY COMMISSION                            
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
         XC    AGROSS(8),AGROSS                                                 
         CLI   FORMAT,TAEPFW       IF FORMAT W                                  
         BNE   INGROSSX                                                         
         TM    ESTMODE,PROCPAY     SAVE GROSS IN APPROPRIATE FIELD              
         BZ    *+14                                                             
         MVC   AGROSS,GROSS                                                     
         B     *+10                                                             
         MVC   EGROSS,GROSS                                                     
INGROSSX XC    GROSS,GROSS                                                      
         B     XIT                                                              
         SPACE 2                                                                
IPYEGRSS DS    0H                  SUM OF ACTUAL + ESTIMATED GORSS              
         L     R1,AGROSS                                                        
         A     R1,EGROSS                                                        
         ST    R1,0(R2)                                                         
         XC    AGROSS(8),AGROSS                                                 
         B     XIT                                                              
         EJECT                                                                  
*              USE DETAILS                                                      
*              NOTE - ANY DPG USING THIS ENTRY MUST ALSO HAVE AN                
*              IN STATMENT IN THERE DATA SECTION (IN R=CLAMERGE)                
*              ELSE CLAOPT=Y WILL PRODUCE INCORRECT RESULTS                     
         SPACE 1                                                                
INUSDET  DS    0H                                                               
         ST    R2,AUSDET                                                        
         OC    DISPUSED,DISPUSED   IF DSP FROM USE NAME TO HERE NOT SET         
         BNZ   *+14                                                             
         LR    R1,R2                                                            
         S     R1,AUSENAME                                                      
         STH   R1,DISPUSED         SET IT NOW                                   
         SPACE 1                                                                
* DON'T CHANGE DISP. TO FIELDS W/O CHECKING ALL REFERENCES TO DISPUSED          
         SPACE 1                                                                
         CLI   FORMAT,TAEPFW       IF FORMAT W                                  
         BNE   INUSDET1                                                         
         BAS   RE,CHKWSP           AND WILDSPOT USE                             
         BNE   INUSDET1                                                         
         CLI   ESTMODE,PROCPAY     FOR ACTUAL PAYMENT ONLY                      
         BNE   INUSDET1                                                         
         MVC   07(3,R2),=3X'FF'    N'UNITS AND MAJORS                           
         MVC   10(1,R2),TCUSETAB+3 PAYMENT TO LIFT?                             
         MVC   11(1,R2),TGUSEQU                                                 
         MVI   12(R2),1            SET GENERIC TYPE                             
         MVC   13(1,R2),ESTMODE                                                 
         B     XIT                                                              
         SPACE 1                                                                
INUSDET1 MVC   00(3,R2),MYUSDATE    USE DATE                                    
         CLI   CLAOPT,C'Y'          IF MERGING CLASS A                          
         BNE   INUSDET2                                                         
         CLI   ESTMODE,PROCPAY      AND ACTUAL PAYMENT                          
         BNE   INUSDET2                                                         
         L     R3,ACLAENT           R3=A(CURRENT CLATAB ENTRY)                  
         USING CLATABD,R3                                                       
         MVC   03(2,R2),CLAEND      SAVE END USE NUMBER ONLY                    
         B     INUSDET5                                                         
         SPACE 1                                                                
INUSDET2 MVC   03(2,R2),TCNUSES     N'USES PAID PREVIOUSLY                      
         MVC   05(2,R2),TCTUSES     N'USES PAID NOW                             
         MVC   10(1,R2),TCUSETAB+3  PAYMENT TO LIFT?                            
         MVC   16(1,R2),TCPAYST2   PAY STATUS                                   
*                                                                               
INUSDET5 MVC   07(2,R2),TCUNITS     N'UNITS                                     
         MVC   09(1,R2),TCMAJORS    MAJORS                                      
         MVC   11(1,R2),TGUSEQU     USE EQUATE                                  
         MVC   12(1,R2),TGUSTYP     USE TYPE EQUATE                             
         MVC   13(1,R2),ESTMODE     CURRENT MODE                                
         TM    TGUSTYST,INSERTS                                                 
         BZ    *+14                                                             
         MVC   14(2,R2),TCINSRTS    N'INSERTS                                   
         B     XIT                                                              
         CLI   TGUSEQU,UTAG                                                     
         BNE   *+14                                                             
         MVC   14(1,R2),TCTAGS     N'TAGS                                       
         B     XIT                                                              
         CLI   TGUSEQU,UDEM                                                     
         BE    *+12                                                             
         CLI   TGUSEQU,UCDM                                                     
         BNE   XIT                                                              
         MVC   14(1,R2),TCDEMO     N'DEMOS                                      
         B     XIT                                                              
         EJECT                                                                  
CHKWSP   DS    0H                  XIT CC EQ IF WSP USE                         
         CLI   TGUSEQU,UWSP        IF WILDSPOT USE                              
         BER   RE                                                               
         CLI   TGUSEQU,UADW        OR ADDENDUM WILDSPOT                         
         BER   RE                                                               
         CLI   TGUSEQU,UWSC        OR CANADIAN WILDSPOT                         
         BER   RE                                                               
         CLI   TGUSEQU,UWSM        OR CANADIAN NETWORK/WILSPOT COMBINED         
         BR    RE                                                               
         EJECT                                                                  
IWSPUNIS DS    0H                                                               
         OC    DISPWSPM,DISPWSPM   IF DSP FROM USDET TO HERE NOT SET            
         BNZ   *+14                                                             
         LR    R1,R2                                                            
         S     R1,AUSDET                                                        
         STH   R1,DISPWSPM         SET IT NOW                                   
*                                                                               
         BAS   RE,CHKWSP           IF WILDSPOT USE                              
         BNE   XIT                                                              
         MVC   0(2,R2),TCUNITS     UNITS PAID NOW                               
         B     XIT                                                              
         SPACE 2                                                                
IWSPMAJS DS    0H                                                               
         BAS   RE,CHKWSP           IF WILDSPOT USE                              
         BNE   XIT                                                              
         MVC   0(1,R2),TCMAJORS    SET MAJORS                                   
         B     XIT                                                              
         EJECT                                                                  
OUTUSDET DS    0H                                                               
         LR    R4,R3               SAVE A(OUTPUT AREA)                          
         MVI   BLOCK,C' '          PRE-CLEAR TEMP OUTPUT AREA                   
         MVC   BLOCK+1(49),BLOCK                                                
         LA    R3,BLOCK            MOVE OUTPUT TO TEMP AREA FIRST               
         BAS   RE,SETWSP           GET REAL MAJORS & UNITS IF NECESSARY         
         SPACE 1                                                                
         CLI   13(R2),PROCPAY                                                   
         BNE   *+12                                                             
         MVI   0(R3),C'*'          INDICATE ACTUAL PAYMENT                      
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
         TM    10(R2),TCLFTPR2     IF THIS IS FOR LIFT                          
         BZ    *+12                                                             
         MVI   0(R3),C'S'          FLAG WITH AN 'S'                             
         B     UDTL00                                                           
         TM    10(R2),TCLFTPRO     IF THIS IS FOR LIFT                          
         BZ    *+12                                                             
         MVI   0(R3),C'L'          FLAG WITH AN 'L'                             
UDTL00   LA    R3,2(R3)                                                         
         SPACE 1                                                                
         XC    TGUSDATA,TGUSDATA                                                
         GOTO1 USEVAL,DMCB,(X'80',11(R2)),12(R2)  LOOK UP USE IN TABLE          
         BNE   UDTLO8                                                           
         TM    TGUSTYST,USES       USE NUMBERS PRESENT                          
         BZ    UDTLO4                                                           
         CLI   TGUSEQU,UCLA        CLASS A'S ARE SPECIAL                        
         BE    UDTLO1                                                           
         CLI   TGUSEQU,UPAX        PAX'S ARE SPECIAL TOO                        
         BE    UDTLO1                                                           
         CLI   TGUSEQU,UITN        ITN'S ARE SPECIAL TOO                        
         BE    UDTLO1                                                           
         LH    RF,5(R2)            N'USES PAID                                  
         BAS   RE,EDITUSEN         JUST PRINT ONE NUMBER                        
         MVC   0(3,R3),=C'USE'                                                  
         CLC   =H'1',5(R2)         IS THERE GT 1 USE                            
         BE    *+12                                                             
         MVI   3(R3),C'S'          ADD AN 'S'                                   
         LA    R3,1(R3)                                                         
         LA    R3,4(R3)                                                         
         B     UDTLO4                                                           
         SPACE 1                                                                
UDTLO1   CLI   CLAOPT,C'Y'         IF MERGING CLASS A                           
         BNE   UDTLO1D                                                          
         CLI   13(R2),PROCPAY      AND ACTUAL PAYMENT                           
         BNE   UDTLO1D                                                          
         OC    DISPCLAM,DISPCLAM   AND IF HAVE DISP. TO CLA MERGE DATA          
         BZ    UDTLO1D                                                          
         LR    R1,R2               POINT TO THEM                                
         AH    R1,DISPCLAM                                                      
         MVC   05(2,R2),0(R1)      SET NUMBER OF USES PAID                      
         LH    RE,03(R2)                                                        
         SH    RE,05(R2)                                                        
         STH   RE,03(R2)           SET NUMBER OF USES PAID PREVIOUSLY           
*                                                                               
UDTLO1D  CLI   FORMAT,TAEPFQTR     IF FORMAT Q                                  
         BE    UDTLO1X             SKIP 'USES' BECAUSE TIGHT ON SPACE           
         MVC   0(3,R3),=C'USE'     START WITH LITERAL IF MULT. USES             
         CLC   =H'1',5(R2)         IS THERE GT 1 USE                            
         BE    *+12                                                             
         MVI   3(R3),C'S'          ADD AN 'S'                                   
         LA    R3,1(R3)                                                         
         LA    R3,4(R3)                                                         
         SPACE 1                                                                
UDTLO1X  DS    0H                                                               
         CLI   TGUSEQU,UITN        ITN'S ARE DIFFERENT                          
         BE    UDTLO1Y                                                          
         LH    RF,3(R2)            N'USES PAID PREVIOUSLY                       
         LA    RF,1(RF)            +1 = STARTING USE NUMBER                     
         BAS   RE,EDITUSEN                                                      
         CLC   =H'1',5(R2)         FINISHED IF ONE USE                          
         BE    UDTLO2                                                           
         BCTR  R3,0                                                             
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LH    RF,3(R2)            N'USES PAID PREVIOUSLY                       
         AH    RF,5(R2)            + N'USES PAID = ENDING USE NUMBER            
         B     UDTLO1Z                                                          
*                                                                               
UDTLO1Y  LH    RF,7(R2)            LAST CLA USE                                 
         CLC   =H'1',5(R2)         ONE USE ONLY?                                
         BE    UDTLO1Z                                                          
         SH    RF,5(R2)                                                         
         LA    RF,1(RF)                                                         
         BAS   RE,EDITUSEN                                                      
         BCTR  R3,0                                                             
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LH    RF,7(R2)                                                         
*                                                                               
UDTLO1Z  BAS   RE,EDITUSEN                                                      
*                                                                               
UDTLO2   OC    0(3,R2),0(R2)                  IF THERE'S A USE DATE             
         BZ    UDTLO4                                                           
         GOTO1 DATCON,DMCB,(1,(R2)),(8,(R3))  DISPLAY IT                        
         LA    R3,9(R3)                                                         
         B     UDTLO4                                                           
         SPACE 1                                                                
EDITUSEN DS    0H                                                               
         EDIT  (RF),(4,0(R3)),ALIGN=LEFT                                        
         LR    R1,R0                                                            
         LA    R3,1(R1,R3)                                                      
         BR    RE                                                               
         SPACE 1                                                                
UDTLO4   TM    TGUSTYST,MAJORS                                                  
         BZ    UDTLO6                                                           
         GOTO1 MAJVAL,DMCB,(X'80',9(R2))                                        
         MVC   0(L'TGMACHAR,R3),TGMACHAR                                        
         LA    R3,L'TGMACHAR+1(R3)                                              
         SPACE 1                                                                
UDTLO6   TM    TGUSTYST,UNITS                                                   
         BZ    UDTLO7                                                           
         CLI   TGUSEQU,UITN        ITN USES THIS FOR LAST CLA USE               
         BE    UDTLO7                                                           
         EDIT  (2,7(R2)),(4,(R3)),ZERO=NOBLANK                                  
         MVC   5(5,R3),=C'UNITS'                                                
         LA    R3,11(R3)                                                        
         SPACE 1                                                                
UDTLO7   TM    TGUSTYST,INSERTS                                                 
         BZ    UDTLO7D                                                          
         EDIT  (2,14(R2)),(4,(R3)),ZERO=NOBLANK                                 
         MVC   5(7,R3),=C'INSERTS'                                              
         LA    R3,13(R3)                                                        
         B     UDTLO8                                                           
UDTLO7D  CLI   TGUSEQU,UDEM                                                     
         BE    *+12                                                             
         CLI   TGUSEQU,UCDM                                                     
         BNE   UDTLO7T                                                          
         EDIT  (1,14(R2)),(3,(R3)),ZERO=NOBLANK                                 
         MVC   4(5,R3),=C'DEMOS'                                                
         LA    R3,10(R3)                                                        
         B     UDTLO8                                                           
UDTLO7T  CLI   TGUSEQU,UTAG                                                     
         BNE   UDTLO8                                                           
         EDIT  (1,14(R2)),(3,(R3)),ZERO=NOBLANK                                 
         MVC   4(4,R3),=C'TAGS'                                                 
         LA    R3,9(R3)                                                         
         SPACE 1                                                                
UDTLO8   CLI   13(R2),PROCAUTO                                                  
         BNE   *+10                                                             
         MVC   0(4,R3),=C'AUTO'    INDICATE AUTO PAYMENT                        
         SPACE 1                                                                
         CLI   TGUSEQU,UVRE                                                     
         BNE   UDTLO9                                                           
         MVC   0(4,R3),=C'VARS'                                                 
         AHI   R3,5                                                             
         LH    RF,5(R2)            N'VARIATIONS                                 
         BAS   RE,EDITUSEN                                                      
         SPACE 1                                                                
UDTLO9   CLI   TGUSEQU,UVNR                                                     
         BNE   UDTLO10                                                          
         MVC   0(4,R3),=C'VARS'                                                 
         AHI   R3,5                                                             
         TM    16(R2),TCVNR1U                                                   
         BZ    UDTLO9A                                                          
         MVC   0(3,R3),=C'1ST'                                                  
         AHI   R3,4                                                             
         OC    5(2,R2),5(R2)       ANY VARIATIONS?                              
         BZ    UDTLO9A                                                          
         MVC   0(2,R3),=C'+ '                                                   
         AHI   R3,2                                                             
UDTLO9A  LH    RF,5(R2)            N'VARIATIONS                                 
         BAS   RE,EDITUSEN                                                      
         SPACE 1                                                                
UDTLO10  GOTO1 SQUASHER,DMCB,BLOCK,50  SQUASH EVERYTHING TOGETHER               
         ZIC   R1,MYOLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),BLOCK       MOVE TO OUPUT AREA                           
         B     XIT                                                              
         EJECT                                                                  
*              GET WILDSPOT MAJORS & UNITS IF NECESSARY                         
*                                                                               
SETWSP   DS    0H                                                               
         CLC   7(3,R2),=3X'FF'     IF WILDSPOT MERGED                           
         BNE   SETWSPX                                                          
         OC    DISPWSPM,DISPWSPM   PT TO MERGED DATA INFORMATION                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LR    R1,R2                                                            
         AH    R1,DISPWSPM                                                      
         MVC   7(3,R2),0(R1)       AND PUT THEM INTO THEIR POSITION             
SETWSPX  BR    RE                                                               
         SPACE 2                                                                
*              CLA MERGE - USED IN CONJUCTION WITH USEDET                       
         SPACE 1                                                                
ICLAMRGE DS    0H                                                               
         CLI   CLAOPT,C'Y'         IF MERGING CLASS A'S                         
         BNE   XIT                                                              
         CLI   ESTMODE,PROCPAY     AND IF ACTUAL PAYMENT                        
         BNE   XIT                                                              
*                                                                               
         OC    DISPCLAM,DISPCLAM   IF DSP FROM USDET TO HERE NOT SET            
         BNZ   *+14                                                             
         LR    R1,R2                                                            
         S     R1,AUSDET                                                        
         STH   R1,DISPCLAM         SET IT NOW                                   
         SPACE 1                                                                
         MVC   0(2,R2),TCTUSES     NUMBER OF USES PAID                          
         B     XIT                                                              
         EJECT                                                                  
*              PERIOD                                                           
         SPACE 1                                                                
*                                  GLARGS   M=MONTH                             
*                                           Q=QUARTER                           
*                                  GLARGS+1 H=HOLDING FEES ONLY                 
INPD     DS    0H                                                               
         CLI   GLARGS+1,C'H'       ONLY WANT PERIOD FOR HOLDING FEES            
         BNE   *+12                                                             
         TM    TGUSSTA2,HLDTYPE                                                 
         BZ    XIT                                                              
         SPACE 1                                                                
         LA    R1,APPLYDTE         SET TO USE R1=APPLY DATE                     
         OC    APPLYMTH,APPLYMTH   IF MONTH IS ALREADY SET                      
         BZ    *+8                                                              
         LA    R1,APPLYMTH         SET TO USE R1=APPLY MONTH                    
         SPACE 1                                                                
         CLI   GLARGS,C'M'                                                      
         BNE   *+14                                                             
         MVC   0(2,R2),0(R1)       PWOS YEAR/MONTH                              
         B     XIT                                                              
         SPACE 1                                                                
         CLI   GLARGS,C'Q'                                                      
         BNE   XIT                                                              
         BAS   RE,QTRIT            DETERMINE QUARTER - PASS R1=YEAR/MTH         
         MVC   0(1,R2),HALF+1      SAVE BINARY YY                               
         MVC   1(1,R2),HALF        SAVE BINARY QQ                               
         B     XIT                                                              
         SPACE 2                                                                
*                                  GLARGS  M=MONTH                              
*                                          Q=QUARTER                            
OUTPD    DS    0H                                                               
         CLI   GLARGS,C'M'                                                      
         BNE   OUTPD2                                                           
         MVC   WORK(2),0(R2)       MOVE YEAR/MONTH TO W/S                       
         MVI   WORK+2,1            NOW HAVE PWOS DATE                           
         GOTO1 DATCON,DMCB,(1,WORK),(6,(R3))                                    
         B     XIT                                                              
         SPACE 1                                                                
OUTPD2   CLI   GLARGS,C'Q'                                                      
         BNE   XIT                                                              
         MVC   WORK(2),0(R2)       BINARY YYQQ                                  
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(X'20',WORK+3)                              
         MVC   0(1,R3),WORK+6      SET SINGLE QUARTER                           
         MVC   1(2,R3),=C'Q/'                                                   
         MVC   3(2,R3),WORK+3      SET YY                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DETERMINE CALENDAR QUARTER                            
*                                  NTRY - R1=A(PWOS DATE)                       
*                                  XIT -  RF=HALF=BINARY QQYY                   
QTRIT    NTR1                                                                   
         MVC   WORK(3),0(R1)                                                    
         CLI   WORK+2,0                                                         
         BNE   *+8                                                              
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+12)                                 
         GOTO1 DATCON,DMCB,(1,WORK),(3,WORK+3)                                  
         ZIC   RF,WORK+3           RF=BINARY YEAR                               
         ZIC   R1,WORK+4           R1=BINARY MONTH                              
*                                                                               
         CLI   ESTMODE,PROCPAY     IF THIS IS AN ACTUAL PAYMENT                 
         BNE   QTRIT5                                                           
         CLI   PROFFILT,TAEPFCES   AND IF GROUPING THEM BY ESTIMATE PD          
         BE    *+12                                                             
         CLI   PROFFILT,TAEPFBES                                                
         BNE   QTRIT5                                                           
         TM    BRSTAT,TABRSQTR     AND IF ESTIMATE PD IS BY QUARTER             
         BO    QTRIT10             THEN SKIP FISCAL YEAR ADJUSTMENT -           
*                                  PAYMENTS ARE MADE WITH ADJUSTED QTR!         
*                                                                               
QTRIT5   ZIC   R0,PROFMTH          R0=START MONTH OF FISCAL YEAR                
         SH    R0,=H'1'               LESS ONE                                  
         BM    QTRIT10                (N/D, SO SKIP)                            
*                                                                               
         SR    R1,R0               SUBTRACT FROM MONTH                          
         BP    QTRIT10                                                          
*                                                                               
         AH    R1,=H'12'           IF RESULT NEGATIVE OR 0, ADD 12              
         BAS   RE,SUBYEAR          AND SUBTRACT 1 FROM YEAR                     
         ZIC   RF,BYTE                                                          
*                                                                               
QTRIT10  AH    R1,=H'2'            PLUS TWO                                     
         XR    R0,R0                                                            
         D     R0,=F'3'            DIVIDED BY THREE                             
*                                                                               
         CLI   PROFMTH,1           IF START OF FISCAL YEAR IS NOT JAN           
         BNH   QTRITX                                                           
         BAS   RE,ADDYEAR          THEN FISCAL YEAR IS NEXT                     
         ZIC   RF,BYTE                                                          
*                                                                               
QTRITX   STC   R1,HALF             RETURN BINARY QUARTER (1-4)                  
         STC   RF,HALF+1           RETURN BINARY YEAR                           
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE ADDS OR SUBTRACTS A YEAR                                 
*                                  NTRY WORK+12=YYMMDD                          
*                                  XIT  WORK+12=YYMMDD ADJUSTED                 
*                                        & BYTE=NEW BINARY YEAR                 
         SPACE 2                                                                
ADDYEAR  NTR1                                                                   
         GOTO1 ADDAY,DMCB,(C'Y',WORK+12),WORK,1                                 
         B     AOSYEAR5                                                         
*                                                                               
SUBYEAR  NTR1                                                                   
         GOTO1 ADDAY,DMCB,(C'Y',WORK+12),WORK,-1                                
*                                                                               
AOSYEAR5 MVC   WORK+12(6),WORK     SAVE NEW EBCDIC DATE                         
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         MVC   BYTE,WORK+6         RETURN BYTE=NEW BINARY YEAR                  
         B     XIT                                                              
         EJECT                                                                  
*              ACTUAL USE DETAILS - PAYMENT DETAILS ELEMENT                     
         SPACE 1                                                                
*                                  GLARGS I=INVOICE NUMBER                      
*                                         P=ESTIMATE PERIOD                     
INTAPD   DS    0H                                                               
         L     R3,ATWA             ESTABLISH ADDRESSABILITY TO SYSIO            
         USING T702FFD,R3                                                       
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         SPACE 1                                                                
         CLI   GLARGS,C'I'                                                      
         BNE   TAPD4                                                            
         GOTO1 TINVCON,DMCB,TAPDINV,(R2),DATCON                                 
         B     TAPDX                                                            
         SPACE 1                                                                
TAPD4    CLI   GLARGS,C'P'                                                      
         BNE   *+10                                                             
         MVC   0(1,R2),TAPDESPD                                                 
         SPACE 1                                                                
TAPDX    B     XIT                                                              
         EJECT                                                                  
*              ACTUAL USE DETAILS - FREE FORM NUMBER ELEMENT                    
         SPACE 1                                                                
*                                  GLARGS = NUMBER TYPE                         
INTANU   DS    0H                                                               
         L     R3,ATWA             ESTABLISH ADDRESSABILITY TO SYSIO            
         USING T702FFD,R3                                                       
         L     R0,AIO                                                           
         MVC   AIO,TIAREC          SET A(RECORD)                                
         ZIC   R4,GLARGS           NUMBER TYPE                                  
         GOTO1 CHAROUT,DMCB,TANUELQ,0,(R4)                                      
         MVC   0(L'TGNAME,R2),TGNAME                                            
         ST    R0,AIO                                                           
         B     XIT                                                              
         EJECT                                                                  
*              MIDHEAD CONTROL ROUTINES                                         
         SPACE 1                                                                
INMIDHD  DS    0H                                                               
         OI    GLINDS2,GLMIDHED    TURN THEM ON                                 
         B     XIT                                                              
         SPACE 1                                                                
OUTMIDHD DS    0H                                                               
         NI    GLINDS2,ALL-GLMIDHED TURN THEM OFF                               
         B     XIT                                                              
         SPACE 3                                                                
*              ALL TOTALS REQUIRED/PRINT WHOLE DETAIL LINE ROUTINES             
         SPACE 1                                                                
INTOTRQ  DS    0H                  TURN THEM ON                                 
         OI    GLINDS,GLPALTOT                                                  
         CLI   FORMAT,TAEPFQTR     IF NOT QUARTERLY REPORT                      
         BE    INTOTRQ5                                                         
         CLI   FORMAT,TAEPFV1      AND NOT VARIANCE REPORTS                     
         BE    INTOTRQ5                                                         
         CLI   FORMAT,TAEPFV2                                                   
         BE    INTOTRQ5                                                         
         OI    GLINDS,GLPALDET     TURN ON DETAILS TOO                          
INTOTRQ5 CLI   GLARGS,C'X'                                                      
         BE    *+8                                                              
         OI    GLINDS2,GLPWHOLE                                                 
         B     XIT                                                              
         SPACE 1                                                                
OUTTOTRQ DS    0H                  TURN THEM OFF                                
         CLI   FORMAT,TAEPFCOM     IF FORMAT "C" BY COML/CYC/USE                
         BNE   OUTTOTR2                                                         
         CLI   HORZOPT,C'N'        HORIZONTAL TOTALS                            
         BE    OUTTOTR2                                                         
         BNE   *+8                                                              
OUTTOTR2 NI    GLINDS,ALL-GLPALDET           TURN OFF ALL DTLS ALSO             
OUTTOTR3 NI    GLINDS,ALL-GLPALTOT                                              
         NI    GLINDS2,ALL-GLPWHOLE                                             
         B     XIT                                                              
         SPACE 3                                                                
*              COUNT ROUTINES                                                   
         SPACE 1                                                                
INCOUNT  DS    0H                                                               
         MVI   3(R2),1              ADD 1                                       
         B     XIT                                                              
         SPACE 1                                                                
OUTCOUNT DS    0H                                                               
         TM    GLINDS,GLTOTLIN     DON'T BOTHER FOR TOTALS                      
         BO    XIT                                                              
         CLI   GLARGS,C'R'         IF THIS IS ROW                               
         BNE   *+12                                                             
         ST    R3,ACOUNTOP         SAVE A(COUNT OUTPUT AREA)                    
         B     XIT                                                              
         ICM   R1,15,ACOUNTOP      IF WE HAVE A(COUNT OUTPUT AREA)              
         BZ    *+6                                                              
         LR    R3,R1               USE IT                                       
         EDIT  (4,0(R2)),(2,(R3))                                               
         XC    ACOUNTOP,ACOUNTOP                                                
         B     XIT                                                              
         EJECT                                                                  
*              WORK-CODE DETAILS                                                
         SPACE 2                                                                
*                                  ARGS C'C' = CODE                             
*                                       C'A' = AMOUNT                           
INWC     DS    0H                                                               
         L     R4,ATHSWC           R4=A(CURRENT WORK-CODE TABLE ENTRY)          
         USING WCD,R4                                                           
         CLI   GLARGS,C'C'                                                      
         BNE   *+10                                                             
         MVC   0(2,R2),WCCODE                                                   
         SPACE 1                                                                
         CLI   GLARGS,C'A'                                                      
         BNE   *+10                                                             
         MVC   0(4,R2),WCAMT                                                    
         B     XIT                                                              
         EJECT                                                                  
*              SPECIAL HEADING SETTING ROUTINES                                 
         SPACE 1                                                                
OUTLTALL DS    0H                                                               
         MVC   MYHEAD5,SPACES                                                   
         MVC   MYHEAD5+9(15),=C'ALL COMMERCIALS'                                
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SUPPRESS OUTPUT ON TOTAL LINE                         
         SPACE 1                                                                
OUTNOTOT DS    0H                                                               
         TM    GLINDS,GLTOTLIN     IF THIS IS TOTAL LINE                        
         BO    XIT                 THEN GET OUT                                 
         MVI   GLHOOK,GLEDIT       ELSE TELL DRIVER TO GO AHEAD                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DETAIL AMOUNT AND ENSURE NET+P&H NOT IN TOT           
         SPACE 1                                                                
OUTPYONE DS    0H                                                               
         TM    GLINDS,GLTOTLIN     IF THIS IS TOTAL LINE                        
         BZ    OUTPYO5                                                          
         L     R1,0(R2)                                                         
         S     R1,SUBTACC          SUBTRACT SUBTOTAL                            
         ST    R1,0(R2)            FROM TOTAL GROSS                             
         XC    SUBTACC,SUBTACC                                                  
*                                                                               
OUTPYO5  OC    0(4,R2),0(R2)       IF NOT ZERO                                  
         BZ    XIT                                                              
         EDIT  (4,0(R2)),(12,(R3)),2,MINUS=YES                                  
         CLI   SUBTLINE,C'Y'       IF A SUBTOTAL LINE                           
         BNE   XIT                                                              
         L     RE,SUBTACC          ADD TO SUBTOTAL                              
         A     RE,0(R2)                                                         
         ST    RE,SUBTACC                                                       
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DISPLAY USE NAME AT TOTAL LINE                        
         SPACE                                                                  
OUTTULIT DS    0H                                                               
         MVC   0(L'SVUSNAME,R3),SVUSNAME                                        
         LA    R3,L'SVUSNAME-1(R3) SCAN BACKWARDS LOOKING FOR                   
         LA    R1,L'SVUSNAME-1        NEXT AVAILABLE SPOT                       
OUTTUL5  CLI   0(R3),C' '                                                       
         BH    *+14                                                             
         BCTR  R3,0                                                             
         BCT   R1,OUTTUL5                                                       
         B     *+8                                                              
         LA    R3,2(R3)                                                         
         MVC   0(6,R3),=CL6'TOTALS'                                             
         B     XIT                                                              
         EJECT                                                                  
*              DPG OUTPUT ROUTINES                                              
         SPACE 1                                                                
OPYGROSS DS    0H                                                               
         LA    R2,ALLTOT-ALLD(R2)  OUTPUT GROSS ONLY                            
         EDIT  (4,0(R2)),(12,(R3)),2,MINUS=YES                                  
         B     XIT                                                              
         EJECT                                                                  
*              COLUMN FILTERING ROUTINES                                        
         SPACE 1                                                                
COLFILT  NTR1                                                                   
         CLI   GLARGS+10,C'Q'      TEST QUARTER FILTER REQUESTED                
         BNE   COLF10                                                           
         LA    R1,APPLYDTE         SET TO USE R1=APPLY DATE                     
         OC    APPLYMTH,APPLYMTH   IF MONTH IS ALREADY SET                      
         BZ    *+8                                                              
         LA    R1,APPLYMTH         SET TO USE R1=APPLY MONTH                    
         SPACE 1                                                                
         BAS   RE,QTRIT            RETURNS QUARTER (1-4) IN HALF                
         ZIC   R1,HALF                                                          
         CLM   R1,1,GLARGS+11                                                   
         BNE   NO                                                               
         SPACE 1                                                                
COLF10   CLI   GLARGS+12,C'A'      ACTUALS PAYMENTS ONLY                        
         BNE   *+12                                                             
         TM    ESTMODE,PROCPAY                                                  
         BZ    NO                                                               
         SPACE 1                                                                
         CLI   GLARGS+12,C'E'      ESTIMATED PAYMENTS ONLY                      
         BNE   *+12                                                             
         TM    ESTMODE,PROCPAY                                                  
         BO    NO                                                               
         SPACE 1                                                                
COLFX    B     YES                                                              
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
LINELNQ  EQU   198                                                              
SRTEQSES EQU   1                   SESSIONS                                     
SRTEQHLD EQU   2                   HOLDING FEES                                 
SRTEQNMU EQU   3                   NON MUSIC USAGE                              
SRTEQMUS EQU   4                   MUSIC USAGE                                  
SRTEQUPG EQU   5                   UPDATES                                      
         SPACE 1                                                                
SPACES   DC    CL198' '                                                         
SVUSNAME DC    CL16' '                                                          
MYOLEN   DC    X'00'                                                            
MYPOSO   DS    0CL3                                                             
MYLTYP   DC    X'00'                                                            
MYLINE   DC    X'00'                                                            
MYCOL    DC    X'00'                                                            
MYOFLD   DC    A(0)                                                             
ACOUNTOP DC    A(0)                                                             
ACYCLEE  DC    A(0)                                                             
ACYCLEA  DC    A(0)                                                             
AUSENAME DC    A(0)                                                             
AUSDET   DC    A(0)                                                             
AGROSS   DC    F'0'                DON'T SEPERATE AGROSS & EGROSS               
EGROSS   DC    F'0'                                                             
DISPUSED DC    H'0'                                                             
DISPCLAM DC    H'0'                                                             
DISPWSPM DC    H'0'                                                             
SUBTLINE DC    CL1'N'              SUBTOTAL LINE                                
MEDFLG   DC    CL1'N'              MEDIA FLAG                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS, ETC.  (CONT'D)                                        
         SPACE 2                                                                
CONVLIT  DC    C'(CANADIAN MONIES CONVERTED AT X.XXXX)'                         
MULTLIT  DC    C'(MONIES INCREASED AT RATE OF XXX.XX%)'                         
SUBTLIT  DC    CL20'NET+P&&H+I&&R+CSF'                                          
SUBTLITS DC    CL20'NET+PH+I+C'                                                 
NETTLIT  DC    CL20'NET'                                                        
         SPACE 2                                                                
ACCLITS  DS    0C                  TOTAL LITERALS                               
         DC    C'N',CL10'TALENT NET',CL20'TALENT NET'                           
         DC    C'N',CL10'TALENT P&&H',CL20'TALENT P&&H'                         
MUSICLIT DC    C'N',CL10'MUSIC NET ',CL20'MUSIC NET'                            
         DC    C'N',CL10'MUSIC P&&H',CL20'MUSIC P&&H'                           
         DC    C'N',CL10'I && R    ',CL20'INSUR && RETIREMENT'                  
CSFLIT   DC    C'N',CL10'CSF       ',CL20'CONTRACT SERVICE FEE'                 
COMBLIT  DC    C'N',CL10'NET+PH+I+C',CL20'NET+P&&H+I&&R+CSF'                    
         DC    C'N',CL10'H && W    ',CL20'HEALTH && WELFARE'                    
PTAXLIT  DC    C'N',CL10'PAYRLL TAX',CL20'PAYROLL TAXES'                        
         DC    C'N',CL10'HANDLING  ',CL20'HANDLING'                             
TNHLIT   DC    C'N',CL10'TAX && HAND',CL20'TAXES && HANDLING'                   
NETLIT   DC    C'N',CL10'NET        ',CL20'NET'                                 
ACOLIT   DC    C'N',CL10'AGY COMM  ',CL20'AGENCY COMMISSION'                    
         DC    C'N',CL10'TOTAL     ',CL20'TOTAL GROSS'                          
         EJECT                                                                  
*              TABLES TO CONTROL PAYALL ENTRIES                                 
         SPACE 2                                                                
ALLTAB1  DS    0C                                                               
         DC    AL4(INPAY),C' ',AL1(ALLPAYT-ALLD)                                
         DC    AL4(INPNH),C' ',AL1(ALLPNHT-ALLD)                                
         DC    AL4(0),C' ',AL1(0)                                               
         DC    AL4(0),C' ',AL1(0)                                               
         DC    AL4(ININR),C' ',AL1(ALLINR-ALLD)                                 
         DC    AL4(INCSF),C' ',AL1(ALLCSF-ALLD)                                 
         DC    AL4(INSUBT),C' ',AL1(ALLSUBT-ALLD)                               
         DC    AL4(INHNW),C'M',AL1(ALLHNW-ALLD)                                 
         DC    AL4(0),C' ',AL1(0)                                               
         DC    AL4(0),C' ',AL1(0)                                               
         DC    AL4(INTNH),C' ',AL1(ALLTNH-ALLD)                                 
         DC    AL4(INNETT),C' ',AL1(ALLNET-ALLD)                                
         DC    AL4(INCOMM),C' ',AL1(ALLCOMM-ALLD)                               
         DC    AL4(INGROSS),C' ',AL1(ALLTOT-ALLD)                               
         DC    X'FF'                                                            
         SPACE 2                                                                
ALLTAB2  DS    0C                                                               
         DC    AL4(INPAY),C'T',AL1(ALLPAYT-ALLD)                                
         DC    AL4(INPNH),C'T',AL1(ALLPNHT-ALLD)                                
         DC    AL4(INPAY),C'M',AL1(ALLPAYM-ALLD)                                
         DC    AL4(INPNH),C'M',AL1(ALLPNHM-ALLD)                                
         DC    AL4(ININR),C' ',AL1(ALLINR-ALLD)                                 
         DC    AL4(INCSF),C' ',AL1(ALLCSF-ALLD)                                 
         DC    AL4(INSUBT),C' ',AL1(ALLSUBT-ALLD)                               
         DC    AL4(INHNW),C'M',AL1(ALLHNW-ALLD)                                 
         DC    AL4(0),C' ',AL1(0)                                               
         DC    AL4(0),C' ',AL1(0)                                               
         DC    AL4(INTNH),C' ',AL1(ALLTNH-ALLD)                                 
         DC    AL4(INNETT),C' ',AL1(ALLNET-ALLD)                                
         DC    AL4(INCOMM),C' ',AL1(ALLCOMM-ALLD)                               
         DC    AL4(INGROSS),C' ',AL1(ALLTOT-ALLD)                               
         DC    X'FF'                                                            
         SPACE 2                                                                
ALLTAB3  DS    0C                                                               
         DC    AL4(INPAY),C' ',AL1(ALLPAYT-ALLD)                                
         DC    AL4(INPNH),C' ',AL1(ALLPNHT-ALLD)                                
         DC    AL4(0),C' ',AL1(0)                                               
         DC    AL4(0),C' ',AL1(0)                                               
         DC    AL4(ININR),C' ',AL1(ALLINR-ALLD)                                 
         DC    AL4(INCSF),C' ',AL1(ALLCSF-ALLD)                                 
         DC    AL4(INSUBT),C' ',AL1(ALLSUBT-ALLD)                               
         DC    AL4(INHNW),C'M',AL1(ALLHNW-ALLD)                                 
         DC    AL4(INTNH),C'T',AL1(ALLTAX-ALLD)                                 
         DC    AL4(INTNH),C'H',AL1(ALLHND-ALLD)                                 
         DC    AL4(0),C' ',AL1(0)                                               
         DC    AL4(INNETT),C' ',AL1(ALLNET-ALLD)                                
         DC    AL4(INCOMM),C' ',AL1(ALLCOMM-ALLD)                               
         DC    AL4(INGROSS),C' ',AL1(ALLTOT-ALLD)                               
         DC    X'FF'                                                            
         SPACE 2                                                                
ALLTAB4  DS    0C                                                               
         DC    AL4(INPAY),C'T',AL1(ALLPAYT-ALLD)                                
         DC    AL4(INPNH),C'T',AL1(ALLPNHT-ALLD)                                
         DC    AL4(INPAY),C'M',AL1(ALLPAYM-ALLD)                                
         DC    AL4(INPNH),C'M',AL1(ALLPNHM-ALLD)                                
         DC    AL4(ININR),C' ',AL1(ALLINR-ALLD)                                 
         DC    AL4(INCSF),C' ',AL1(ALLCSF-ALLD)                                 
         DC    AL4(INSUBT),C' ',AL1(ALLSUBT-ALLD)                               
         DC    AL4(INHNW),C'M',AL1(ALLHNW-ALLD)                                 
         DC    AL4(INTNH),C'T',AL1(ALLTAX-ALLD)                                 
         DC    AL4(INTNH),C'H',AL1(ALLHND-ALLD)                                 
         DC    AL4(0),C' ',AL1(0)                                               
         DC    AL4(INNETT),C' ',AL1(ALLNET-ALLD)                                
         DC    AL4(INCOMM),C' ',AL1(ALLCOMM-ALLD)                               
         DC    AL4(INGROSS),C' ',AL1(ALLTOT-ALLD)                               
         DC    X'FF'                                                            
         EJECT                                                                  
*              DSECT TO COVER PAYALL TABLES                                     
         SPACE 2                                                                
ALLTD    DSECT                                                                  
ALLTRTN  DS    A                   A(INPUT ROUTINE)                             
ALLTARG  DS    CL1                 ARGUMENT (OPT)                               
ALLTDSP  DS    XL1                 DISP. TO ACCUMULATOR IN ENTRY                
ALLTLNQ  EQU   *-ALLTD                                                          
ALLTNEXT EQU   *                                                                
         SPACE 3                                                                
*              DSECT TO COVER ACCLIT TABLE                                      
         SPACE 2                                                                
LITD     DSECT                                                                  
LITYORN  DS    CL1                 PRINT SWITCH                                 
LITSHORT DS    CL10                SHORT LITERAL                                
LITLIT   DS    CL20                LITERAL                                      
LITLNQ   EQU   *-LITD                                                           
LITNEXT  EQU   *                                                                
         SPACE 3                                                                
         EJECT                                                                  
       ++INCLUDE TAESTDSECT                                                     
         EJECT                                                                  
       ++INCLUDE TAGENESTD                                                      
         EJECT                                                                  
* TAGENFFD                                                                      
* TASCR40D                                                                      
* TASYSIOD                                                                      
* DRGLOBAL                                                                      
* DRIVETABLE                                                                    
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
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073TAESTDRIVE01/16/14'                                      
         END                                                                    
