*          DATA SET NEMED6CN   AT LEVEL 005 AS OF 03/07/06                      
*          DATA SET NEMED6C    AT LEVEL 148 AS OF 09/02/99                      
*PHASE T31E6CA,+0                                                               
*INCLUDE PRINT                                                                  
T31E6C   TITLE '-  NETWORK SCHEDULE'                                            
         SPACE 1                                                                
         MACRO                                                                  
&NAME    CPP   &P1,&P2,&P3                                                      
         NETGO NVDEMTYP,DMCB,(&P1,NDDEMBLK),BYTE                                
         CLI   BYTE,C'R'                                                        
         BE    RCP&SYSNDX                                                       
         SPACE 1                                                                
         L     R1,ACCOST                                                        
         LA    R0,200                                                           
         CLI   NBHUNOPT,C'Y'                                                    
         BNE   *+8                                                              
         LA    R0,2000             ADJUST IF IN HUNDRED MODE                    
         MR    R0,R0                                                            
         OC    &P2,&P2                                                          
         BZ    XCP&SYSNDX                                                       
         D     R0,&P2                                                           
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(9,&P3),2,FLOAT=$                                           
         B     XCP&SYSNDX                                                       
         SPACE 1                                                                
RCP&SYSNDX L     R1,ACCOST         RTG                                          
         M     R0,=F'20'                                                        
         OC    &P2,&P2                                                          
         BZ    XCP&SYSNDX                                                       
         D     R0,&P2                                                           
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(9,&P3),FLOAT=$                                             
XCP&SYSNDX DS  0H                                                               
         MEND                                                                   
         EJECT                                                                  
*******************************************************************             
         SPACE 3                                                                
T31E6C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NTSC**,RA,RR=R2                                              
         USING T31E6C+4096,RA                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          USE W/S AREA 2                               
         USING SCHEDD,R7                                                        
         ST    R2,RELO                                                          
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         MVI   NBHUNOPT,C'Y'       PROGRAM MAY DEAL IN HUNDREDS                 
         OI    NBVARIND,X'40'      USE NDDEMOS FOR NAD                          
         LA    RE,NDUNBLOK                                                      
         ST    RE,NDAUBLOK         SET OPTIONAL UNIVERSE AREA                   
         LA    R1,STALIST                                                       
         ST    R1,NBCNVNTI                                                      
         SPACE 1                                                                
         XC    PERTYPE,PERTYPE                                                  
         MVI   PERTYPE,C'M'        SET UP FOR MONTHS OR WEEKS                   
         CLI   DAYOPT,C'W'                                                      
         BNE   *+8                                                              
         MVI   PERTYPE,C'W'                                                     
         MVI   PERTYPE+1,1            USE MONTHS IF TOO MANY WEEKS              
         MVI   PERTYPE+2,0            NEVER USE QUARTERS                        
         MVC   MENU,FORMAT            IF FORMAT SPECIFIED, USE IT.              
         CLI   MENU,0                 ELSE, USE PROFILE.                        
         BNE   CKMENU                                                           
         MVC   MENU,NBUSER+9                                                    
         CLI   MENU,0                                                           
         BNE   *+8                                                              
         MVI   MENU,1                                                           
         SPACE 1                                                                
CKMENU   CLI   MENU,12                                                          
         BNH   *+8                                                              
         MVI   MENU,1                                                           
         EJECT                                                                  
*              PROCESS DATES                                                    
         SPACE 3                                                                
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT     PROCESS DATES                                
         BE    IN6                                                              
         B     PROCDAT             OTHER MODES ARE IGNORED                      
         SPACE 1                                                                
*                                                                               
IN6      DS    0H                                                               
*****    MVI   NBTRCOPT,C'Y'                                                    
*****    MVC   NBPRINT,=V(PRINT)                                                
*                                                                               
         MVI   NBDATA,C'U'         SELECT UNIT RECORDS                          
         MVI   NBSEQ,C'D'          READ IN DATE ORDER                           
         MVI   NBSPLOPT,X'80'      OPTION TO SPLIT PIGGIES                      
         CLI   FLAVOR,C'P'                                                      
         BE    FLAVP                                                            
         CLI   FLAVOR,C'V'                                                      
         BE    FLAVV                                                            
FLAVE    MVI   RCSUBPRG,2                                                       
         MVI   NBUSER+13,C'N'      DONT FILTER PREEMPTS                         
         MVI   NBUSER+8,C'Y'       ALWAYS USE ASS COST FOR EST FLAV             
         MVI   NREPTYP,C'A'        USE ACCTG DATES                              
         B     IN20                                                             
         SPACE 1                                                                
FLAVP    MVI   RCSUBPRG,1                                                       
         MVI   NBSELUOP,C'A'       USE ACTUAL SCHEDULE                          
         MVI   NBACTOPT,C'Y'       TO GET ACTUAL DEMOS                          
         NI    NBINDS,X'FF'-X'40'  TURN OFF PRIMP                               
         B     IN20                                                             
         SPACE 1                                                                
FLAVV    MVI   RCSUBPRG,1                                                       
         MVI   NBSELUOP,C'A'       USE ACTUAL SCHEDULE                          
         MVI   NBESTOPT,C'A'       ESTIMATED DEMOS ON ACTUAL SCHEDULE           
         CLI   PFBFLG,C'Y'         OPTION TO GET EST ON PFB                     
         BNE   IN20                                                             
         MVI   NBESTOPT,C'P'                                                    
         B     IN20                                                             
         SPACE 1                                                                
IN20     LA    R1,MAXMONTS         SET UP MAX SIZE OF MONTH (WEEK) LIST         
         ST    R1,NUMMONS          GET LIST INTO MONLIST. NUMMONS IS            
*                                    NEW SIZE OF LIST                           
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
         SPACE 1                                                                
         LA    R6,MONLIST          USE BEGINNING OF MONTH LIST                  
         MVI   ESTFLAG,0           NO ESTIMATES FOUND YET                       
         XC    MONUNIT,MONUNIT     NUM UNITS IN MONTH                           
         XC    TOTUNIT,TOTUNIT     NUM UNITS IN REPORT                          
         SPACE 1                                                                
GETNEXT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         BAS   RE,CHKMAXIO            CHECK IO COUNT                            
*                                                                               
         CLI   NBMODE,NBPROCUN     IF FIRST UNIT RECORD                         
         BE    CKFLAV                                                           
         CLI   NBMODE,NBREQLST     IF NO UNITS                                  
         BE    TOTALS                                                           
         B     GETNEXT             OTHER MODES ARE IGNORED                      
         SPACE 1                                                                
CKFLAV   CLI   FLAVOR,C'P'         PRINT ACT TOTALS FOR POST                    
         BNE   CKDATE                                                           
         CLI   ESTFLAG,0           IF ACT DEMS FROM EST FOR 1ST TIME            
         BNE   CKDATE                                                           
         CLI   NBRESULT,C'E'                                                    
         BNE   CKDATE                                                           
         CLI   NBSURVEY,C'N'       AND ITS MAIN NETWORK                         
         BNE   CKDATE                                                           
         BAS   RE,ACTTOTS          PRINT ACTUAL TOTAL LINE                      
         MVI   ESTFLAG,1           SET FLAG                                     
CKDATE   CLC   NBACTDAT,2(R6)      IF IN CURRENT MONTH SET                      
         BH    NEWMONTH                                                         
         BAS   RE,SPOT             FILL IN UNIT LINE.                           
         B     GETNEXT                                                          
         SPACE 1                                                                
NEWMONTH BAS   RE,MONTOTS          FILL IN MONTH TOTAL                          
         LA    R6,4(R6)            POINT TO NEXT DATE-SET                       
         XC    MONUNIT,MONUNIT     RESTART COUNT                                
         B     CKDATE                                                           
         SPACE 1                                                                
TOTALS   BAS   RE,MONTOTS          FILL LAST MONTH                              
         MVC   P+6(15),=C'SCHEDULE TOTALS'                                      
         LA    R2,SCHEDAC                                                       
         TM    NBINDS,X'40'        INCREASED PRECISION                          
         BNO   *+8                                                              
         BAS   RE,IMPDIV                                                        
         MVI   OPTION,C'Y'                                                      
         BAS   RE,FORMATL                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         CLI   UTOTOPT,C'Y'                                                     
         BNE   TOTXIT                                                           
         EDIT  (2,TOTUNIT),(4,P+6),ALIGN=L     TOTAL UNITS                      
         MVC   P+11(5),=C'UNITS'                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
TOTXIT   XMOD1                                                                  
         SPACE 1                                                                
PROCERR  DC    F'0'                                                             
         EJECT                                                                  
*              PRINT ACTUAL TOTAL LINE                                          
         SPACE 3                                                                
ACTTOTS NTR1                                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         OC    SCHEDAC,SCHEDAC     DONT PRINT IF NO ACTUAL TOTALS               
         BZ    XITAT                                                            
         MVC   P+6(13),=C'ACTUAL TOTALS'                                        
         MVI   SPACING,2                                                        
         LA    R2,SCHEDAC                                                       
         TM    NBINDS,X'40'                                                     
         BNO   *+8                                                              
         BAS   RE,IMPDIVA                                                       
         MVI   OPTION,C'Y'                                                      
         BAS   RE,FORMATL                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
XITAT    XIT1                                                                   
         EJECT                                                                  
*              TOTALS AT NEW MONTH                                              
         SPACE 3                                                                
*        INPUTS: R6- CURRENT DATE SET                                           
         SPACE 1                                                                
MONTOTS  NTR1                                                                   
         OC    MONUNIT,MONUNIT                                                  
         BZ    XITMONT                                                          
         GOTO1 SPOOL,DMCB,(R8)     SPACE                                        
         GOTO1 DATCON,DMCB,(2,0(R6)),(4,P+12)                                   
         MVI   P+18,C'-'                                                        
         GOTO1 DATCON,DMCB,(2,2(R6)),(4,P+20)                                   
         MVC   P+6(5),=C'MONTH'                                                 
         CLI   PERTYPE,C'M'                                                     
         BE    *+16                                                             
         MVC   P+6(5),=C'WEEK '                                                 
         MVC   P+11(14),P+12                                                    
         LA    R2,PERAC                                                         
         TM    NBINDS,X'40'                                                     
         BNO   *+8                                                              
         BAS   RE,IMPDIV                                                        
         BAS   RE,FORMATL                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         CLI   UTOTOPT,C'Y'                                                     
         BNE   MTOT2                                                            
         EDIT  (2,MONUNIT),(3,P+6),ALIGN=L     MONTHLY UNITS                    
         MVC   P+10(5),=C'UNITS'                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
MTOT2    GOTO1 SPOOL,DMCB,(R8)      BLANK LINE                                  
         SPACE 1                                                                
         XC    PERAC,PERAC                                                      
XITMONT  XIT1                                                                   
         EJECT                                                                  
*              ROUTINES TO HANDLE A SPOT - FORMAT LEFT                          
         SPACE 3                                                                
SPOT     NTR1                                                                   
         MVI   ZEROACT,C' '        SET ACTUAL FLAG                              
         TM    NBUNITST,X'20'                                                   
         BNO   *+8                                                              
         MVI   ZEROACT,C'0'                                                     
         MVI   ZEROASS,C' '        SET ASSIGNED FLAG                            
         TM    NBUNITST,X'08'                                                   
         BNO   *+8                                                              
         MVI   ZEROASS,C'0'                                                     
         XC    SPOTAC,SPOTAC                                                    
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(4,P)                                   
         XC    P+6(16),P+6                                                      
         MVC   P+6(6),NBACTPRG                                                  
         CLI   PRGCDFLG,C'Y'                                                    
         BE    SPT1                                                             
         MVC   P+6(16),NBPROGNM                                                 
SPT1     MVC   P+23(4),NBACTNET                                                 
         MVC   P+30(3),NBDAYNAM                                                 
         GOTO1 UNTIME,DMCB,NBTIME,P+34                                          
         CLC   NBSELPRD,=C'POL'    IF ITS POOL                                  
         BNE   SPOT2                                                            
         BAS   RE,PRLUP                                                         
         MVC   P+46(3),WORK                                                     
         MVC   P+46+132(3),WORK+3                                               
**       CLI   NBPRDNO,0                                                        
**       BE    SPOT2                                                            
**       MVC   P+38+132(20),WORK+4     MOVE EM ALL IN                           
**       GOTO1 CENTER,DMCB,P+170,20                                             
         SPACE 1                                                                
SPOT2    LA    R2,SPOTAC                                                        
         USING ACD,R2                                                           
         CLI   FLAVOR,C'E'                                                      
         BE    FLAVEST                                                          
         EJECT                                                                  
*              POSTING FOR FLAVOR V,P                                           
         SPACE 3                                                                
         ZIC   R3,NBLEN            LENGTH                                       
         EDIT  (R3),(3,P+51)                                                    
         CLI   P+51,X'40'          ,,IF THERE'S ROOM                            
         BH    SKIPC                                                            
         TM    NBUNST3,X'40'       ,,IF COPYSPLIT                               
         BZ    SKIPC                                                            
         MVI   P+51,C'C'           ,,SET IN C                                   
SKIPC    L     R1,NBINTEG                                                       
         L     R0,NBACTUAL                                                      
         CLI   NBUSER+15,C'Y'                (OPTIONALLY + INTEG)               
         BNE   *+6                                                              
         AR    R0,R1                                                            
*        SR    R1,R1                                                            
*        A     R1,NBACTUAL                                                      
*        M     R0,=F'1'            ACCOST IS DOLLARS                            
*        D     R0,=F'100'                                                       
*        ST    R1,ACCOST                                                        
         BAS   RE,ROUND                                                         
         ST    R1,ACCOST                                                        
         MVC   ZEROTHIS,ZEROACT                                                 
         MVC   ACUNITS+2(2),NBACTUN                                             
         CLI   NBUSER+8,C'Y'         OPTION TO USE ASSIGNED COSTS               
         BNE   ESTORACT                                                         
         L     R0,NBASSIGN                                                      
         BAS   RE,ROUND                                                         
         ST    R1,ACCOST                                                        
*        L     R1,NBASSIGN                                                      
*        M     R0,=F'1'            GET INTO DOLLARS                             
*        D     R0,=F'100'                                                       
*        ST    R1,ACCOST                                                        
         MVC   ZEROTHIS,ZEROACT                                                 
         ST    R1,SPOTAC                                                        
         B     ESTORACT                                                         
*                                                                               
ROUND    SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
*                                                                               
ESTORACT CLI   FLAVOR,C'V'                                                      
         BE    USEESTS                                                          
*                                  *** USE ACTUAL DEMOS                         
USEACTS  TM    NBUNITST,X'02'      MISSED SPOT. NO DEMOS TO RETURN              
         BO    SPOT20                                                           
         MVC   ACGRPS+2(2),NBACTHOM+2                                           
         MVC   ACHOMES,NBACTHOM+4                                               
         XC    ACDEMA(12),ACDEMA                                                
         SPACE 1                                                                
**** GET DEMOS IN ACDEMA,ACDEMB,ACDEMC                                          
         NETGO NVGETDEM,DMCB,(0,NDDEMBLK),(C'A',ACDEMA)  1ST ACT DEMO           
         NETGO NVGETDEM,DMCB,(1,NDDEMBLK),(C'A',ACDEMB)  2ND ACT DEMO           
         NETGO NVGETDEM,DMCB,(2,NDDEMBLK),(C'A',ACDEMC)  3RD ACT DEMO           
         B     SPOT20                                                           
*                                  USE ESTIMATED DEMOS                          
USEESTS  DS    0H                                                               
         SPACE 1                                                                
******** CLI   PFBFLG,C'Y'         PRINT BONUS                                  
******** BE    USEST2              IF YES/SKIP TEST                             
******** TM    NBUNITST,X'04'      ELSE TEST PFB                                
******** BO    SPOT20              IF PFB/SKIP DEMOS                            
         SPACE                                                                  
USEST2   LA    R2,SPOTAC                                                        
         USING ACD,R2                                                           
         MVC   ACGRPS+2(2),NBESTHOM+2                                           
         MVC   ACHOMES,NBESTHOM+4                                               
         XC    ACDEMA(12),ACDEMA                                                
         SPACE 1                                                                
**** GET DEMOS IN ACDEMA,ACDEMB,ACDEMC                                          
         NETGO NVGETDEM,DMCB,(0,NDDEMBLK),(C'E',ACDEMA)  1ST EST DEMO           
         NETGO NVGETDEM,DMCB,(1,NDDEMBLK),(C'E',ACDEMB)  2ND EST DEMO           
         NETGO NVGETDEM,DMCB,(2,NDDEMBLK),(C'E',ACDEMC)  3RD EST DEMO           
         TM    NBINDS,X'40'        INCREASED PRECISION                          
         BNO   SPOT20                                                           
         BAS   RE,IMPD100                                                       
         B     SPOT20                                                           
*                                                                               
IMPD100  NTR1                        DROP GREATER PRECISION                     
         CLI   NBPOSTYP,C'C'       IF CABLE                                     
         BE    IMPDX               LEAVE AS IS                                  
         LA    R2,ACHOMES                                                       
         LA    R3,4                                                             
         LA    RE,NDDEMOS                                                       
IMPDAA   C     R3,=F'4'            SKIP HOMES                                   
         BE    *+12                                                             
         CLI   1(RE),C'I'          MAKE SURE DEMOS ARE IMPS                     
         BNE   IMPDAB                                                           
         SR    R0,R0                                                            
         L     R1,0(R2)                                                         
         D     R0,=F'100'                                                       
         ST    R1,0(R2)                                                         
IMPDAB   LA    R2,4(R2)                                                         
         C     R3,=F'4'                                                         
         BE    *+8                                                              
         LA    RE,3(RE)                                                         
         BCT   R3,IMPDAA                                                        
IMPDX    XIT1                                                                   
         EJECT                                                                  
*              POSTING FOR FLAVOR= ESTIMATE                                     
         SPACE 3                                                                
FLAVEST  NETGO NVACFLT,DMCB        THIS MAY RESET NBACTUAL,NBASSIGN             
*                                    AND NBINTEG BASED ON NVFILT                
         BZ    FE2                 SETS STATWORD IF SHOULD BE FILTERED          
         XC    SPOTAC,SPOTAC       FILTER IT. RESET SPOTAC AND XIT.             
         MVC   P,SPACES            CLEAR PRINT LINE                             
         B     XITSPOT                                                          
         SPACE 1                                                                
*FE2      L     R1,NBASSIGN                                                     
*        M     R0,=F'1'            GET INTO DOLLARS                             
*        D     R0,=F'100'                                                       
*        ST    R1,SPOTAC+4                                                      
FE2      L     R0,NBASSIGN                                                      
         BAS   RE,ROUND                                                         
         ST    R1,SPOTAC+4                                                      
*        L     R1,NBACTUAL                                                      
*        M     R0,=F'1'            GET INTO DOLLARS                             
*        D     R0,=F'100'                                                       
*        ST    R1,SPOTAC+8                                                      
         L     R0,NBACTUAL                                                      
         BAS   RE,ROUND                                                         
         ST    R1,SPOTAC+8                                                      
         MVC   SPOTAC+12(4),NBINTEG                                             
         CLI   NBSELFLT,C'1'       ARE FILTERS OPERATIVE                        
         BL    SPOT20                                                           
         XC    SPOTAC+16(4),SPOTAC+16        NO CUT INS                         
         SPACE 1                                                                
SPOT16   OC    0(20,R2),0(R2)      IGNORE INACTIVE LINES                        
         BNZ   *+14                                                             
         MVC   P,SPACES                                                         
         B     XIT                                                              
         SPACE 1                                                                
SPOT20   DS    0H                                                               
         LA    R3,PERAC            ADD TO LOWER LEVELS                          
         BAS   RE,ADDLINS                                                       
         LA    R3,SCHEDAC                                                       
         BAS   RE,ADDLINS                                                       
         TM    NBINDS,X'40'        IF INCREASED PRECISION                       
         BNO   *+8                                                              
         BAS   RE,IMPDIV                                                        
*                                                                               
SPOT22   MVI   OPTION,C'N'         PRINT A LINE                                 
         MVI   EPLOPT,C'Y'                                                      
         BAS   RE,FORMATL                                                       
         MVI   EPLOPT,C'N'                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   ZEROACT,C' '        CLEAR SWITCHES                               
         MVI   ZEROASS,C' '                                                     
         MVI   ZEROTHIS,C' '                                                    
         SPACE 1                                                                
         LH    R1,MONUNIT                                                       
         AH    R1,=H'1'            ADD UNIT TO COUNT OF UNITS IN MONTH          
         STH   R1,MONUNIT                                                       
         LH    R1,TOTUNIT                                                       
         AH    R1,=H'1'            ADD UNIT TO COUNT OF TOTAL UNITS             
         STH   R1,TOTUNIT                                                       
XITSPOT  B     XIT                                                              
*                                                                               
IMPDIV   NTR1                                                                   
         USING ACD,R2                                                           
         LA    R2,ACHOMES          HOMES FOLLOWED BY 3 IMPS                     
         LA    R3,4                                                             
         SR    R0,R0                                                            
         LA    RE,NDDEMOS                                                       
IMPD00   C     R3,=F'4'            MAKE SURE DEMOS ARE IMPS BUT                 
         BE    *+12                SKIP HOMES                                   
         CLI   1(RE),C'I'                                                       
         BNE   IMPD05                                                           
         L     R1,0(R2)                                                         
         LTR   R1,R1                                                            
         BZ    IMPD05                                                           
         SR    R0,R0                                                            
         A     R1,=F'50'                                                        
         D     R0,=F'100'                                                       
         SR    R0,R0                                                            
         CLI   NBPOSTYP,C'C'                                                    
         BE    *+8                                                              
         M     R0,=F'100'                                                       
         ST    R1,0(R2)                                                         
IMPD05   LA    R2,4(R2)                                                         
         C     R3,=F'4'            SKIP HOMES                                   
         BE    *+8                                                              
         LA    RE,3(RE)                                                         
         BCT   R3,IMPD00                                                        
         XIT1                                                                   
         DROP  R2                                                               
IMPDIVA  NTR1                                                                   
         USING ACD,R2                                                           
         LA    R2,ACHOMES                                                       
         LA    R3,4                                                             
         SR    R0,R0                                                            
         LA    RE,NDDEMOS                                                       
IMPD00A  C     R3,=F'4'                                                         
         BE    *+12                                                             
         CLI   1(RE),C'I'                                                       
         BNE   IMPD05A                                                          
         L     R1,0(R2)                                                         
         LTR   R1,R1                                                            
         BZ    IMPD05A                                                          
         SR    R0,R0                                                            
         A     R1,=F'500'                                                       
         D     R0,=F'1000'                                                      
         SR    R0,R0                                                            
         CLI   NBPOSTYP,C'C'                                                    
         BE    *+8                                                              
         M     R0,=F'1000'                                                      
         ST    R1,0(R2)                                                         
IMPD05A  LA    R2,4(R2)                                                         
         C     R3,=F'4'                                                         
         BE    *+8                                                              
         LA    RE,3(RE)                                                         
         BCT   R3,IMPD00A                                                       
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*              PRODUCT LOOKUP                                                   
         SPACE 3                                                                
PRLUP    NTR1                                                                   
         MVC   WORK,SPACES                                                      
***      LA    R4,NBPRD                                                         
* NO LONGER MOVE IN ALL PRODS IF MULTI PROD + POL - JUST FIRST 2                
* CHANGE LIVE WITH 500 PROD OVERFLOW                                            
**       CLI   NBPRDNO,0           IF MULT PRODS                                
**       BE    *+8                                                              
**       B     PRLUPMLT            GO TO MULTIPLE PRODUCT LOOKUP                
***      LA    R5,WORK                                                          
***      BAS   RE,PRLUP2                                                        
         MVC   WORK(3),NBPR1CL3                                                 
***      LA    R4,NBPRD2                                                        
***      LA    R5,WORK+3                                                        
***      BAS   RE,PRLUP2                                                        
         CLI   NBPR2CL3,0                                                       
         BE    *+10                                                             
         MVC   WORK+3(3),NBPR2CL3                                               
         B     XIT                                                              
         SPACE 1                                                                
**PRLUPMLT DS    0H                                                             
**         ZIC   R1,NBPRDNO                                                     
**         LA    R4,NBPRDLST                                                    
**         LA    R5,WORK                                                        
**PRLM7    BAS   RE,PRLUP2                                                      
**         LA    R5,4(R5)                                                       
**         LA    R4,1(R4)                                                       
**         BCT   R1,PRLM7                                                       
**         B     XIT                                                            
         SPACE 1                                                                
**PRLUP2   L     R2,ANETWS1          CLIENT RECORD PASSED IN W/S AREA 1         
**         USING CLTHDR,R2                                                      
**         LA    R2,CLIST                                                       
**         DROP  R2                                                             
**         SPACE 1                                                              
**         LA    R3,220                                                         
**         CLI   0(R4),0                                                        
**         BER   RE                                                             
**         CLI   0(R4),X'FF'                                                    
**         BER   RE                                                             
**         SPACE 1                                                              
**PRLUP4   CLC   0(1,R4),3(R2)                                                  
**       BE    PRLUP6                                                           
**       LA    R2,4(R2)                                                         
**       BCT   R3,PRLUP4                                                        
**       BR    RE                                                               
         SPACE 1                                                                
**PRLUP6   MVC   0(3,R5),0(R2)                                                  
**       BR    RE                                                               
         SPACE 1                                                                
         EJECT                                                                  
ADDLINS  NTR1                                                                   
         LA    R4,8                                                             
         SPACE 1                                                                
ADD2     L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,ADD2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT NUMBERS - POSTS                                           
         SPACE 3                                                                
         USING ACD,R2                                                           
FORMATL  NTR1                                                                   
         CLI   FLAVOR,C'E'                                                      
         BE    FORMATLE                                                         
         EDIT  (4,ACCOST),(8,P+54),FLOAT=-                                      
         OC    P+54+7(1),ZEROTHIS  SHOW TRUE ZERO = 0                           
         TM    NBINDS3,NBI3A2DC                                                 
         BO    FP1AA                                                            
         EDIT  (4,ACGRPS),(9,P+62),1                      HOMES GRP             
         CLI   NBPREOPT,C'Y'                   CABLE PRECISION                  
         BNE   FP1A                                                             
         CLI   NBPOSTYP,C'N'       FOR NETWORK                                  
         BE    *+12                                                             
         CLI   NBPOSTYP,C'S'       AND SYNDICATION                              
         BNE   FP1AA                                                            
         ICM   R1,15,ACGRPS                                                     
         MH    R1,=H'10'           ADD A ZERO IN 2 DEC OUTPUT                   
         STCM  R1,15,ACGRPS                                                     
FP1AA    EDIT  (4,ACGRPS),(9,P+62),2                      HOMES GRP             
FP1A     NETGO NVPRDEM,DMCB,(C'I',0),ACHOMES,(9,P+71)          IMPS             
         CLI   NDNDEMOS,0          IF NO DEMOS                                  
         BZ    FP2                                                              
         NETGO NVPRDEM,DMCB,(0,NDDEMBLK),ACDEMA,(9,P+80)  DEMOS                 
         NETGO NVPRDEM,DMCB,(1,NDDEMBLK),ACDEMB,(9,P+89)                        
         NETGO NVPRDEM,DMCB,(2,NDDEMBLK),ACDEMC,(9,P+98)                        
         SPACE 1                                                                
FP2      CLI   OPTION,C'Y'                                                      
         BNE   XIT                                                              
         OC    ACCOST,ACCOST                                                    
         BZ    XIT                                                              
         LA    R4,P+132                                                         
         MVC   55(7,R4),=C'CPP/CPM'                                             
         L     R1,ACCOST           CPP                                          
         M     R0,=F'20'                                                        
         OC    ACGRPS,ACGRPS                                                    
         BZ    FP4                                                              
         D     R0,ACGRPS                                                        
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(9,62(R4)),FLOAT=$                                          
         SPACE 1                                                                
FP4      L     R1,ACCOST           CPM                                          
         LA    R0,200                                                           
         CLI   NBHUNOPT,C'Y'                                                    
         BNE   *+8                                                              
         LA    R0,2000             ADJUST IF IN HUNDRED MODE                    
         MR    R0,R0                                                            
         OC    ACHOMES,ACHOMES                                                  
         BZ    FP6                                                              
         D     R0,ACHOMES                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(9,71(R4)),2,FLOAT=$                                        
         SPACE 1                                                                
FP6      CLI   NDNDEMOS,0                                                       
         BZ    XIT                                                              
         SPACE 1                                                                
         CPP   0,ACDEMA,80(R4)                                                  
         CPP   1,ACDEMB,89(R4)                                                  
         CPP   2,ACDEMC,98(R4)                                                  
         B     XIT                                                              
         SPACE 3                                                                
*              FORMAT NUMBERS - ESTIMATES                                       
         SPACE 3                                                                
FORMATLE MVI   RCSUBPRG,2                                                       
         LA    R3,P+50                                                          
         BAS   RE,SOFTCOL                                                       
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
         DS    0H                                                               
HOOK     NTR1                                                                   
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         SPACE 1                                                                
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(3),SPLPRO                                                  
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+14(20),SPLCLIN                                                
         MVC   H5+14(20),SPLPRON                                                
         MVC   H6+14(24),SPLESTN                                                
         CLI   SPLPRO+3,X'40'                                                   
         BNH   HOOK1                                                            
         XC    H5+10(25),H5+10                                                  
         MVC   H5+10(8),SPLPRO                                                  
         MVC   H5+19(20),SPLPRON                                                
         GOTO1 SQUASHER,DMCB,H5+10,29                                           
HOOK1    CLI   NBSELESE,0                                                       
         BE    HOOK2                                                            
         MVC   H6+10(7),SPLEST                                                  
         MVI   H6+17,C' '                                                       
         MVC   H6+18(24),SPLESTN                                                
         OC    H6+10(32),SPACES                                                 
         GOTO1 SQUASHER,DMCB,H6+10,32                                           
         SPACE 1                                                                
HOOK2    MVC   H5+74(9),=C'NETWORK -'                                           
         MVC   H5+84(4),SPLNET                                                  
         OC    NBSELNET,NBSELNET                                                
         BNZ   *+10                                                             
         MVC   H5+74(14),=C'ALL NETWORKS  '                                     
         MVC   H5+90(8),SPLDPTN                                                 
         CLI   NBSELPAK,0                                                       
         BE    *+10                                                             
         MVC   H6+74(36),SPLPAKN                                                
         CLI   PRGCDFLG,C'Y'       IF USE PROG CODE                             
         BNE   *+10                                                             
         MVC   H10+6(12),=C'PROGRAM CODE'                                       
         CLC   SPLPRO(3),=C'POL'                                                
         BNE   HOOK3                                                            
         MVC   H10+45(5),=C'BRAND'                                              
         MVC   H11+45(5),=C'-----'                                              
         SPACE 1                                                                
HOOK3    CLI   FLAVOR,C'E'                                                      
         BNE   HOOKDEM                                                          
         LA    R3,H10+50                                                        
         BAS   RE,SOFTHEAD                                                      
         IC    R1,NBSELFLT                                                      
         SLL   R1,28                                                            
         SRL   R1,28               0-4                                          
         MH    R1,=H'9'                                                         
         LA    R1,FILTTITL(R1)                                                  
         MVC   H6+49(9),0(R1)                                                   
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINES TO EDIT COLUMNS                                         
         SPACE 3                                                                
SOFTCOL  NTR1                                                                   
         MVI   ZEROTHIS,C' '       PRESET ZERO FLAG                             
         ZIC   R1,MENU             (R2=ADDRESS OF ACCUMS)                       
         BCTR  R1,0                (R3=ADDRESS OF FIRST COL)                    
         MH    R1,=H'5'                                                         
         LA    R1,MENUTAB(R1)                                                   
         LA    R0,5                                                             
         SPACE 1                                                                
SFT2     CLI   0(R1),0                                                          
         BE    *+8                                                              
         BAS   RE,SFT4                                                          
         LA    R3,12(R3)                                                        
         LA    R1,1(R1)                                                         
         BCT   R0,SFT2                                                          
         B     XIT                                                              
         SPACE 1                                                                
SFT4     NTR1                                                                   
         ZAP   MYDUB,=P'0'                                                      
         ZIC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     BRANCH(RF)                                                       
         SPACE 1                                                                
BRANCH   B     SFT6                1    ASS                                     
         B     SFT8                2    ASS  +2NT +CUT                          
         B     SFT10               3    ACT                                     
         B     SFT12               4    ACT  +INT +CUT                          
         B     SFT10               5    GRSS                                    
         B     SFT16               6    DIFF                                    
         B     SFT18               7    INT                                     
         B     SFT20               8    CUT                                     
         B     SFT22               9    ACT  +INT                               
         B     SFT12               10   ACT  +INT +CUT                          
         B     SFT28               11   EST-PAK-LIN                             
         B     SFT24               12   COMM                                    
         B     SFT26               13   NET                                     
         B     SFT27                                                            
         SPACE 1                                                                
SFT6     L     R1,ACASS            ASS                                          
         BAS   RE,S100                                                          
         MVC   ZEROTHIS,ZEROASS                                                 
         B     SFTEND                                                           
         SPACE 1                                                                
SFT8     L     R1,ACASS            ASS  +CUT +INT                               
         MVC   ZEROTHIS,ZEROASS                                                 
         B     SFT14                                                            
         SPACE 1                                                                
SFT10    L     R1,ACACT            ACT                                          
         BAS   RE,S100                                                          
         MVC   ZEROTHIS,ZEROACT                                                 
         B     SFTEND                                                           
         SPACE 1                                                                
SFT12    L     R1,ACACT            ACT  +CUT +INT                               
         MVC   ZEROTHIS,ZEROACT                                                 
         SPACE 1                                                                
SFT14    BAS   RE,S100                                                          
         L     R1,ACCUT                                                         
         BAS   RE,SADD                                                          
         L     R1,ACINT                                                         
         BAS   RE,SADD                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT16    L     R1,ACASS            DIFFERENCE                                   
         S     R1,ACACT                                                         
         BAS   RE,S100                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT18    L     R1,ACINT            INTEGRATION                                  
         BAS   RE,SADD                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT20    L     R1,ACCUT            CUT-IN                                       
         BAS   RE,SADD                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT22    L     R1,ACINT            ACT  +INT                                    
         BAS   RE,SADD                                                          
         B     SFT10                                                            
         SPACE 1                                                                
SFT24    L     R1,ACACT            COMMISSION                                   
         MVC   ZEROTHIS,ZEROACT                                                 
         MH    R1,=H'100'          PUT IN CENTS                                 
         LR    R0,R1                                                            
         BAS   RE,FINDNET          GET NET IN R1                                
*****    CVD   R1,DUB              PXZ                                          
         AP    MYDUB,DUB                                                        
         MVC   WORK,MYDUB                                                       
         ZAP   MYDUB,=P'0'                                                      
****     BAS   RE,S100             PXZ                                          
         MP    DUB,=P'100'          PXZ                                         
         AP    DUB,MYDUB           PXZ                                          
         SP    MYDUB,WORK(8)                                                    
         B     SFTEND                                                           
         SPACE 1                                                                
SFT26    L     R1,ACACT            NET                                          
         MVC   ZEROTHIS,ZEROACT                                                 
         MH    R1,=H'100'          PUT IN CENTS                                 
         LR    R0,R1                                                            
         BAS   RE,FINDNET          GET NET IN R1                                
*****    CVD   R1,DUB                                                           
         AP    MYDUB,DUB                                                        
         B     SFTEND                                                           
         SPACE 1                                                                
SFT27    L     R1,ACACT                                                         
         MVC   ZEROTHIS,ZEROACT                                                 
         MH    R1,=H'100'          PUT IN CENTS                                 
         LR    R0,R1                                                            
         BAS   RE,FINDNET          GET NET IN R1                                
*****    CVD   R1,DUB                                                           
         AP    MYDUB,DUB                                                        
         L     R0,ACINT                                                         
         BAS   RE,FINDNET          NET COST IN R1                               
*****    CVD   R1,DUB                                                           
         AP    MYDUB,DUB                                                        
         SPACE 1                                                                
SFTEND   EDIT  (P8,MYDUB),(12,0(R3)),2,MINUS=YES,ZERO=BLANK                     
         CLI   ZEROTHIS,C' '                                                    
         BE    XIT                                                              
         MVI   ZEROTHIS,C' '                                                    
         CP    MYDUB,=P'0'                                                      
         BNZ   XIT                                                              
         MVC   8(3,R3),=C'.00'                                                  
         B     XIT                                                              
         SPACE 1                                                                
SFT28    CLI   EPLOPT,C'Y'                                                      
         BNE   XIT                                                              
         EDIT  (1,NBACTEST),(3,1(R3))         EST-PAK                           
         EDIT  (1,NBPACK),(3,5(R3))                                             
         OC    1(7,R3),=7X'F0'                                                  
         MVI   4(R3),C'-'                                                       
         B     XIT                                                              
         SPACE 1                                                                
S100     CVD   R1,DUB                                                           
         MP    DUB,=P'100'                                                      
         AP    MYDUB,DUB                                                        
         BR    RE                                                               
         SPACE 1                                                                
SADD     CVD   R1,DUB                                                           
         AP    MYDUB,DUB                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
         SPACE 3                                                                
FINDNET  SRDA  R0,32               PREPARE MULTIPLICAND                         
*        M     R0,=F'8500'                                                      
*        M     R0,=F'85'                                                        
*        SLDA  R0,1                DOUBLE FOR ROUNDING                          
*        D     R0,=F'10000'                                                     
*        D     R0,=F'100'                                                       
*        LTR   R1,R1                                                            
*        BM    *+8                                                              
*        AH    R1,=H'1'                                                         
*        SRA   R1,1                                                             
*        LR    R0,R1    PXZ        REPLACE COST WITH NET                        
         CVD   R1,DUB              PXZ                                          
         MP    DUB,=P'85'          PXZ                                          
         AP    DUB,=P'50'          PXZ                                          
         DP    DUB,=P'100'         PXZ                                          
         ZAP   DUB,DUB(6)          PXZ                                          
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINES TO FILL HEADLINES                                       
         SPACE 3                                                                
SOFTHEAD NTR1                                                                   
         ZIC   R2,MENU             (R3=ADDRESS OF HEADLINE)                     
         BCTR  R2,0                                                             
         MH    R2,=H'5'                                                         
         LA    R2,MENUTAB(R2)                                                   
         LA    R4,5                                                             
         SPACE 1                                                                
SH2      ZIC   R1,0(R2)            GET DATA NUMBER                              
         LTR   R1,R1                                                            
         BZ    SH6                                                              
         BCTR  R1,0                                                             
         MH    R1,=H'12'                                                        
         LA    R1,TITLTAB(R1)      LOCATE TITLE                                 
         MVC   0(11,R3),1(R1)      MOVE IT IN                                   
         LR    R1,R3                                                            
         LA    R0,12                                                            
         SPACE 1                                                                
SH4      CLI   0(R1),C' '          UNDERLINE                                    
         BE    *+8                                                              
         MVI   132(R1),C'-'                                                     
         LA    R1,1(R1)                                                         
         BCT   R0,SH4                                                           
         SPACE 1                                                                
SH6      LA    R2,1(R2)                                                         
         LA    R3,12(R3)                                                        
         BCT   R4,SH2                                                           
         B     XIT                                                              
         SPACE 1                                                                
MENUTAB  DS    0H                  COL  1    2    3    4    5                   
         DC    AL1(3,7,10,0,0)     1    ACT  INT  TOT                           
         DC    AL1(3,7,10,11,0)    2    ACT  INT  TOT  LIST                     
         DC    AL1(1,3,6,7,0)      3    ASS  ACT  DIFF INT                      
         DC    AL1(2,4,6,7,0)      4    ASS+ ACT+ DIFF INT                      
         DC    AL1(5,0,0,0,0)      5    GRSS                                    
         DC    AL1(5,13,0,0,0)     6    GRSS NET                                
         DC    AL1(5,7,09,0,0)     7    GRSS INT  TOT                           
         DC    AL1(5,7,09,14,0)    8    GRSS INT  TOT  NET                      
         DC    AL1(5,12,13,0,0)    9    GRSS COMM NET                           
         DC    AL1(5,7,9,14,11)    10   GRSS INT  TOT  NET  LIST                
         DC    AL1(7,0,0,0,0)      11   INT                                     
         DC    5X'00'              12   SPARE                                   
         SPACE 1                                                                
TITLTAB  DS    0H                                                               
         DC    CL12'    ASSIGNED'  1                                            
         DC    CL12'    ASSIGNED'  2 (+INT +CUT)                                
         DC    CL12'      ACTUAL'  3                                            
         DC    CL12'      ACTUAL'  4 (+INT +CUT)                                
         DC    CL12'       GROSS'  5                                            
         DC    CL12'  DIFFERENCE'  6                                            
         DC    CL12' INTEGRATION'  7                                            
         DC    CL12'      CUT-IN'  8                                            
         DC    CL12'       TOTAL'  9  (ACT + INT)                               
         DC    CL12'       TOTAL'  10 (ACT + INT + CUT)                         
         DC    CL12' EST-PAK    '  11                                           
         DC    CL12'  COMMISSION'  12                                           
         DC    CL12'         NET'  13                                           
         DC    CL12'     NET TOT'  14                                           
         EJECT                                                                  
*              ROUTINE TO PUT IN DEMO NAMES                                     
         SPACE 3                                                                
HOOKDEM  LA    R3,H10+83                                                        
         ZIC   R4,NDNDEMOS                                                      
         CLI   FLAVOR,C'V'                                                      
         BNE   *+10                                                             
         MVC   H6+45(17),=C'(ESTIMATED DEMOS)'                                  
         CLI   NDNDEMOS,0                                                       
         BE    XIT                                                              
         LTR   R4,R4               IF NO DEMOS                                  
         BZ    XIT                                                              
         CH    R4,=H'3'                                                         
         BL    *+8                                                              
         LA    R4,3                                                             
         SR    R2,R2               COUNTER FOR DEMO NUMBER                      
         SPACE 1                                                                
HOOKDEM2 NETGO NVDEMCON,DMCB,((R2),NDDEMBLK),DBLOCK,(7,WORK)                    
         MVC   0(7,R3),WORK                                                     
         MVC   133(5,R3),WORK+7                                                 
         LA    R5,NDDEMOS                                                       
         LR    R1,R2                                                            
         MH    R1,=H'3'                                                         
         AR    R5,R1                                                            
         CLI   0(R5),0                                                          
         BE    HKDEM5                                                           
         LA    R6,133(R3)                                                       
         LA    R6,133(R6)                                                       
         EDIT  (B1,0(R5)),(5,0(R6)),ALIGN=LEFT                                  
HKDEM5   LA    R2,1(R2)                                                         
         LA    R3,9(R3)                                                         
         BCT   R4,HOOKDEM2                                                      
         B     XIT                                                              
         SPACE 1                                                                
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
* SET MAXIOCTR TO 90% OF MAX IO'S SHOWN BY GETFACT                              
* CHECK IF I/O OVER                                                             
CHKMAXIO NTR1                                                                   
                                                                                
         L     R5,ACOMFACS         GET MAX IO                                   
         USING COMFACSD,R5                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
                                                                                
         CLI   MAXIOCTR,0          HAVE WE SET MAXIO ?                          
         BNE   MAX20                           YES                              
         MVC   MAXIOCTR,FATMAXIO-FACTSD(R1)    NO                               
         SR    R2,R2                                                            
         LH    R3,MAXIOCTR                                                      
         LA    R4,9                MULTIPLY MAX IO BY 9                         
         MR    R2,R4                                                            
         LA    R4,10               DIVIDE MAXIMUM BY 10                         
         DR    R2,R4                                                            
         STH   R3,MAXIOCTR                                                      
         B     MAXOK                                                            
MAX20    DS    0H                                                               
         CLC   MAXIOCTR,FATIOCNT-FACTSD(R1)   MAXED OUT ?                       
         BH    MAXOK                                                            
                                                                                
         DROP  R5                                                               
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         MVC   CONHEAD(38),=C'*-IO TIMEOUT REDUCE REQUEST PARAMETERS'           
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2,DMCB                                                      
         B     MAXOK                                                            
                                                                                
MAXOK    XIT1                                                                   
         DROP  R5                                                               
                                                                                
MAXIOCTR DS    H                                                                
                                                                                
         EJECT                                                                  
*              LTORG ETC.                                                       
         SPACE 3                                                                
FILTTITL DC    C'(ORDERED)'                                                     
         DC    C'(CLEARED)'                                                     
         DC    C'UNCLEARED'                                                     
         DC    C' (BILLED)'                                                     
         DC    C' BILLABLE'                                                     
         SPACE 1                                                                
RELO     DS    A                                                                
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
         SPACE 1                                                                
         EJECT                                                                  
*              DSECT TO COVER ACCUMULATORS                                      
         SPACE 3                                                                
ACD      DSECT                                                                  
ACCOST   DS    F              COST                                              
ACUNITS  DS    F              UNITS                                             
ACGRPS   DS    F              HOMES GRPS                                        
ACHOMES  DS    F              HOMES                                             
ACDEMA   DS    F              IMPRESSIONS DEMO 1                                
ACDEMB   DS    F                               2                                
ACDEMC   DS    F                               3                                
         DS    F              SPARE                                             
         SPACE 1                                                                
         ORG   ACCOST                                                           
ACUN     DS    F              UNITS                                             
ACASS    DS    F              ASSIGNED COST                                     
ACACT    DS    F              ACTUAL COST                                       
ACINT    DS    F              INTEGRATION                                       
ACCUT    DS    F              CUT-INS                                           
         SPACE 3                                                                
         EJECT                                                                  
SCHEDD   DSECT                                                                  
**** COMMON WITH EDIT                                                           
*                                                                               
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NETUNIVD                                                       
*                                                                               
         EJECT                                                                  
*** ARGS FROM EDIT                                                              
*                                                                               
FLAVOR   DS    CL1                                                              
DAYOPT   DS    CL1                                                              
FORMAT   DS    CL1                                                              
PRGCDFLG DS    CL1                 Y IF USE PROG CODE NOT PROG NAME             
PFBFLG   DS    CL1                 Y=PRINT PFB                                  
HUNOPT   DS    CL1                 Y=PROCESS IN HUNDREDS                        
UTOTOPT  DS    CL1                 Y=SHOW UNIT TOTALS                           
         DS    CL7                 SPARE                                        
*                                                                               
*                                                                               
*** LOCAL W/S                                                                   
*                                                                               
PERTYPE  DS    CL3                 1ST BYTE IS PERIOD TYPE                      
MAXMONTS EQU   53                  MAX MONS (WKS) IN LIST                       
MONLIST  DS    CL(4*MAXMONTS)                                                   
NUMMONS  DS    F                                                                
ESTFLAG  DS    F                   SET WHEN GET 1ST ESTIMATE DEMOS              
MONUNIT  DS    H                                                                
TOTUNIT  DS    H                                                                
*                                                                               
         DS    0D                                                               
SPOTAC   DS    CL32                                                             
PERAC    DS    CL32                                                             
SCHEDAC  DS    CL32                                                             
EPLOPT   DS    CL1                                                              
MYDUB    DS    D                                                                
MENU     DS    CL1                                                              
WORKDEMS DS    CL24                                                             
ZEROASS  DS    CL1                                                              
ZEROACT  DS    CL1                                                              
ZEROTHIS DS    CL1                                                              
*                                                                               
STALIST  DS    CL2000                                                           
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLN                                                     
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDECD                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEMED6CN  03/07/06'                                      
         END                                                                    
