*          DATA SET NEMED2F    AT LEVEL 013 AS OF 05/01/02                      
*PHASE T31E2FA                                                                  
         TITLE 'T31E2F - NETWORK EGS TEST REPORT'                               
         PRINT NOGEN                                                            
*******************************************************************             
*                                                                               
* GLOBALS:  R5 - A(CURRENT PRINT LINE)                                          
*           R7 - WORKING STORAGE                                                
*           R8 - A(DSECT FOR SPOOL PRINTING)                                    
*           R9 - NETWORK SYSTEM DSECT                                           
*           RA - A(TWA)                                                         
*           RC - GEND                                                           
*                                                                               
*  INPUTS: NETBLOCK SET UP BY EDIT.                                             
*          CLIENT RECORD WILL BE RETURNED IN W/S AREA 1.                        
************************************************************                    
*                                                                               
T31E2F   CSECT                                                                  
         NMOD1 0,**NTEG**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         USING COLINFO,R5                                                       
         L     R7,ANETWS2          LOCAL W/S                                    
         USING WORKD,R7                                                         
*                                                                               
         LA    R5,P1               FIRST PRINT LINE                             
*                                                                               
         MVI   NBDATA,C'U'         SELECT UNIT RECORDS                          
         MVI   NBSEQ,C'P'          READ IN PROGRAM ORDER                        
         MVI   NBESTOPT,C'Y'       TO GET DEMOS                                 
*                                                                               
         MVI   NBACTOPT,C'Y'       TO GET DEMOS                                 
         MVI   NBREVOPT,C'A'                                                    
*                                                                               
GETFIRST NETGO NSNETIO,DMCB,NETBLOCK    GET FIRST UNIT RECORD                   
         CLI   NBMODE,NBPROCUN     IF A UNIT RECORD                             
         BE    GOTONE                                                           
         CLI   NBMODE,NBREQLST     IF NO UNITS                                  
         BE    ENDIT                                                            
         B     GETFIRST            OTHER MODES ARE IGNORED                      
GOTONE   BAS   RE,ACTIVFLT         FILTER ON ACTIVITY DATE                      
         BNZ   GETFIRST                                                         
         BAS   RE,FILPRG1          FILL PROG FOR FIRST ONE                      
         BAS   RE,DOMAINLN                                                      
         BAS   RE,DOFREE                                                        
         B     GETUNIT             NOW DO OTHERS                                
*                                                                               
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN     IF A UNIT RECORD                             
         BE    FILLIT                                                           
         CLI   NBMODE,NBREQLST     IF NO MORE UNITS                             
         BE    ENDIT                                                            
         B     GETUNIT             OTHER MODES ARE IGNORED                      
*                                                                               
******   TM    NBSUBMSK,NBSBMPRG   IF NEW PROGRAM                               
******   BZ    NOPROG                                                           
FILLIT   BAS   RE,ACTIVFLT         ACTIVITY DATE FILTERING                      
         BNZ   GETUNIT                                                          
         BAS   RE,FILLPROG         FILL IN PROGRAM LINE                         
         BAS   RE,DOMAINLN         FILL IN MAIN LINE INFO                       
         BAS   RE,DOFREE           FILL IN THE FREE FORMATTED LINES             
         B     GETUNIT                                                          
*                                                                               
NOPROG   LA    R1,P4               CONTINUATION OF OLD PROG                     
         CR    R5,R1               IF USED LAST PRINT LINE                      
         BL    INCLINE                                                          
         GOTO1 SPOOL,DMCB,(R8)        PRINT WHATS THERE                         
         LA    R5,P1                  POINT R5 TO FIRST PRINT LINE              
         B     NP2                                                              
*                                    ELSE                                       
INCLINE  LA    R5,132(R5)               NEXT PRINTLINE                          
*                                    FI                                         
NP2      BAS   RE,DOMAINLN                                                      
         BAS   RE,DOFREE                                                        
         B     GETUNIT              GET NEXT UNIT                               
*                                                                               
ENDIT    GOTO1 SPOOL,DMCB,(R8)     FLUSH PRINT BUFFER                           
         XMOD1                                                                  
******                                                                          
PROCERR  DC    F'0'                                                             
***************************************************************8                
         EJECT                                                                  
****************************************************************                
*  FILLPROG - PROCESS A NEW PROGRAM. FLUSH THE PRINT BUFFERS,                   
*              MOVE IN PROGRAM INFORMATION TO NEW BUFFER.                       
* FILPRG1 (ENTRY) - FOR THE FIRST PROGRAM                                       
*                                                                               
*  OUTPUT - R5 - SET TO FIRST PRINT LINE                                        
****************************************************************                
FILLPROG NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)     DUMP WHATS THERE                             
         MVI   SPACING,2           WRITE 2 BLANK LINES                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1               USE TOP OF PRINT LINE                        
*                                                                               
         MVC   CLPRNM(L'NBPROGNM),NBPROGNM                                      
         MVC   CLPRCD(L'NBACTPRG),NBACTPRG                                      
         GOTO1 UNTIME,DMCB,NBTIME,CLPRTM                                        
*                                                                               
         XIT1  REGS=(R5)                                                        
*                                                                               
FILPRG1  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)     DUMP HEADERS                                 
         LA    R5,P1               USE TOP OF PRINT LINE                        
*                                                                               
         MVC   CLPRNM(L'NBPROGNM),NBPROGNM                                      
         MVC   CLPRCD(L'NBACTPRG),NBACTPRG                                      
         GOTO1 UNTIME,DMCB,NBTIME,CLPRTM                                        
*                                                                               
         XIT1  REGS=(R5)                                                        
****************************************************************                
         EJECT                                                                  
****************************************************************                
* ACTIVFLT - FILTER ON ACTIVITY DATE                                            
*    INPUT - ACTIVDAT - BLANK IF NO FILTERING                                   
*    OUTPUT - (R5) STATUS WORD - NONZERO IF TO BE REJECTED                      
****************************************************************                
ACTIVFLT NTR1                                                                   
         SR    R5,R5                                                            
         OC    ACTIVDAT,ACTIVDAT                                                
         BZ    XITACEL                                                          
         L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL                                                      
         MVI   SRCHEL,X'99'        GET ACTIVITY ELEMENT                         
         BAS   RE,NEXTEL                                                        
         BE    GOTACTEL                                                         
         DC    H'0'                                                             
         USING NUACTD,R2                                                        
GOTACTEL CLC   NUACTCDT,ACTIVDAT                                                
         BE    XITACEL                                                          
         LA    R5,1                                                             
XITACEL  LTR   R5,R5                                                            
         XIT1                                                                   
****************************************************************                
         EJECT                                                                  
****************************************************************                
*  DOMAINLN - FILL IN THE FIXED FIELD INFO. (ONE LINE PER UNIT)                 
****************************************************************                
DOMAINLN NTR1                                                                   
*                                                                               
         OC    NBPRFILT,NBPRFILT   PROG FILTER                                  
         BZ    DM2                                                              
         MVC   CLFILTH,=CL4'FLT='                                               
         MVC   CLFILT,NBPRFILT                                                  
DM2      MVC   CLNET,NBACTNET      NETWORK                                      
         MVC   CLDAYP,NBDPNAM      DAYPART                                      
*                                                                               
         MVC   CLDISKH,=C'D/A='                                                 
         LA    R3,NBKEY                                                         
         USING NURECD,R3                                                        
         EDIT  NUDA,(8,CLDISKA)       D/A                                       
         DROP  R3                                                               
*                                                                               
         EDIT  NBACTEST,(3,CLEST)      ESTIMATE                                 
         EDIT  NBPACK,(3,CLPAK)        PACKAGE                                  
         EDIT  NBPACKST,(3,CLPAKST)    PACKAGE STATUS                           
         EDIT  NBUNITST,(3,CLUNITST)   UNIT STATUS                              
*                                                                               
         MVC   CLDAY(L'NBDAYNAM),NBDAYNAM                                       
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(8,CLDATE)                              
         MVC   CLSUB,NBSUBOUT                                                   
         EDIT  NBLEN,(3,CLLEN)                                                  
*                                                                               
         L     R4,ANETWS1          ADDRESS OF CLIENT RECORD                     
         USING CLTHDR,R4                                                        
         LA    R2,CLIST            LOOK UP PRODUCT CODE                         
PRD1LOOP CLI   3(R2),0             IF END-OF-LIST THEN EXIT                     
         BE    DOPRD2                                                           
         CLC   3(1,R2),NBPRD                                                    
         BE    PRD1FND                                                          
         LA    R2,4(R2)            NEXT IN LIST                                 
         B     PRD1LOOP                                                         
PRD1FND  MVC   CLPRD1(3),0(R2)                                                  
*                                                                               
DOPRD2   CLI   NBPRD2,X'00'        IF NOT EXIST SKIP PRINTING                   
         BE    DOCOST                                                           
         MVI   CLSLASH,C'/'                                                     
         LA    R2,CLIST            LOOK-UP PRODUCT ABBR                         
PRD2LOOP CLI   3(R2),0             IF END-OF-LIST THEN EXIT                     
         BE    DOCOST                                                           
         CLC   3(1,R2),NBPRD2                                                   
         BE    PRD2FND                                                          
         LA    R2,4(R2)            NEXT IN LIST                                 
         B     PRD2LOOP                                                         
PRD2FND  MVC   CLPRD2(3),0(R2)                                                  
         DROP  R4                                                               
*                                                                               
DOCOST   OC    NBACTUAL,NBACTUAL   CK FOR ZERO COST                             
         BNZ   DOC2                                                             
         TM    NBUNITST,X'20'      TEST IF COST ALLOCATED                       
         BZ    DOC4                                                             
         MVC   CLACT+5(2),=C'$0'      IF SO, PRINT $0                           
         B     DOC4                                                             
DOC2     EDIT  NBACTUAL,(9,CLACT),DROP=2                                        
         MVC   CLACT+7(2),SPACES        DONT PRINT CENTS                        
DOC4     EDIT  NBASSIGN,(9,CLASS),DROP=2                                        
         MVC   CLASS+7(2),SPACES        DONT PRINT CENTS                        
         EDIT  NBINTEG,(6,CLINT),2                                              
*                                                                               
DOBILL   CLC   NBBILTGR,=F'0'      IF NOT BILLED SKIP PRINTING                  
         BE    DOPAY                                                            
         MVC   CLBP(1),=C'B'       BILLED                                       
DOPAY    CLC   NBPAYTGR,=F'0'      IF NOT PAID SKIP PRINTING                    
         BE    DONTI                                                            
         MVC   CLBP+1(1),=C'P'     PAID                                         
DONTI    EDIT  NBNTI,(4,CLNTI)                                                  
         MVC   CLRESUL,NBRESULT                                                 
         MVC   CLRES2,NBRES2                                                    
DOSHR    EDIT  NBACTSHR,(4,CLSHARE),1                                           
         EDIT  NBESTHUT,(4,CLHUT),1                                             
         MVC   HALF(2),NBESTHOM+2                                               
         EDIT  (2,HALF),(4,CLRTG),1                                             
*                                                                               
XITMNL   XIT1  REGS=(R5)                                                        
****************************************************************                
         EJECT                                                                  
****************************************************************                
* DOFREE - FILL IN THOSE FIELDS WITH NO FIXED LOCATION. THESE FIELDS            
*          ARE ONLY OUTPUT WHEN DATA EXIST FOR THE FIELD.                       
*                                                                               
* LOCALS : R6 - THE BEGINNING OF THE CURRENT FREE FIELD.                        
****************************************************************                
DOFREE   NTR1                                                                   
         LA    R6,CLOTH1           THE FIRST FREE FIELD                         
*                                                                               
DOMGF    CLC   NBMGFPCD,ZEROES                                                  
         BE    DOMGB                                                            
         BAS   RE,NEXTLIN          GET NEXT PRINT LINE                          
         MVC   0(7,R6),=C'M/G FOR '                                             
         MVC   8(16,R6),NBMGFPNM                                                
         GOTO1 DATCON,DMCB,(2,NBMGFDAT),(4,25(R6))                              
         MVC   30(3,R6),NBMGFSBP                                                
         GOTO1 SQUASHER,DMCB,0(R6),34    GET RID OF SPACES                      
*                                                                               
DOMGB    CLC   NBMGBPCD,ZEROES                                                  
         BE    DOFEED                                                           
         BAS   RE,NEXTLIN          GET NEXT PRINT LINE                          
         MVC   0(7,R6),=C'M/G BY '                                              
         MVC   8(16,R6),NBMGBPNM                                                
         GOTO1 DATCON,DMCB,(2,NBMGBDAT),(4,25(R6))                              
         MVC   30(3,R6),NBMGBSBP                                                
         GOTO1 SQUASHER,DMCB,0(R6),34    GET RID OF SPACES                      
*                                                                               
DOFEED   CLC   NBFEED,ZEROES                                                    
         BE    DOUNIV1                                                          
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(5,R6),=C'FEED='                                                
         EDIT  NBFEED,(5,5(R6)),2                                               
*                                                                               
DOUNIV1  CLC   NBUNIV,ZEROES                                                    
         BE    DOUNIV2                                                          
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(5,R6),=C'UNIV='                                                
         EDIT  NBUNIV,(5,5(R6)),2                                               
*                                                                               
DOUNIV2  CLC   NBUNCODE,ZEROES                                                  
         BE    DOIMP                                                            
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(5,R6),=C'UNIV='                                                
         MVO   FULL(3),NBUNCODE    EDIT OUT PWO                                 
         OI    FULL+2,X'0F'                                                     
         EDIT  (P3,FULL),(4,5(R6)),ALIGN=LEFT                                   
*                                                                               
DOIMP    CLC   NBIMPACT,ZEROES                                                  
         BE    DONSI                                                            
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(7,R6),=C'IMPACT='                                              
         EDIT  NBIMPACT,(5,8(R6)),2                                             
*                                                                               
DONSI    CLC   NBNSI,ZEROES                                                     
         BE    DOATIM                                                           
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(4,R6),=C'NSI='                                                 
         EDIT  NBNSI,(4,5(R6))                                                  
*                                                                               
DOATIM   CLC   NBAFFTIM,ZEROES                                                  
         BE    DOFMG                                                            
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(7,R6),=C'AFFTIM='                                              
         MVC   DUB(2),NBAFFTIM                                                  
         MVC   DUB+2(2),ZEROES     SET SECOND TIME TO 0                         
         GOTO1 UNTIME,DMCB,DUB,8(R6)                                            
*                                                                               
DOFMG    CLC   NBFEEDMG,ZEROES                                                  
         BE    DOCOMM                                                           
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(4,R6),=C'FMG='                                                 
         UNPK  5(4,R6),NBFEEDMG+1(3)                                            
         MVI   9(R6),C' '                                                       
*                                                                               
DOCOMM   MVI   SRCHEL,X'04'        LOOK FOR COMMENT ELEMENT                     
         USING NURECD,R3                                                        
         L     R3,NBAIO                                                         
         LA    R2,NUMAINEL         ADDRESS OF FIRST ELEMENT                     
         DROP  R3                                                               
*                                                                               
         BAS   RE,NEXTEL           IF ONE EXISTS                                
         BNE   XITFREE                                                          
*                                                                               
         BAS   RE,MOVECOMM           R2 POINTS TO COMMENT ELEMENT               
*                                                                               
XITFREE  XIT1  REGS=(R5)                                                        
****************************************************************                
         EJECT                                                                  
******************************************************                          
*  NEXTOTH - GETS NEXT AVAILABLE PRINT LOCATION. (CLOTH1 OR CLOTH2)             
*                                                                               
*  ENTRY NEXTLIN - GETS NEXT AVAILABLE PRINT LINE.                              
*                                                                               
*    CALL  SPOOL IF ALL ARE FULL.                                               
*                                                                               
*    INPUT:  R5 - CURRENT PRINTLINE                                             
*    OUTPUT: R5 - NEW PRINTLINE                                                 
*            R6 - PRINT LOCATION                                                
*****************************************************                           
NEXTOTH  NTR1                                                                   
NEXTBEG  CLC   0(L'CLOTH1,R6),SPACES    IF FIELD IS NOT USED                    
         BE    NTXIT                      DONT CHANGE ANYTHING                  
*                                                                               
         LA    R1,CLOTH1                                                        
         CR    R6,R1               IF CURRENT IS CLOTH1                         
         BNE   NXT2                                                             
         LA    R6,CLOTH2             THEN TRY CLOTH2                            
         B     NEXTBEG                    GO CHECK IT                           
*                                                                               
NEXTLIN  NTR1                      (ENTRY)                                      
         CLC   0(L'CLOTHER,R6),SPACES   IF FIELD IS NOT USED                    
         BE    NTXIT                       DONT CHANGE ANYTHING                 
*                                                                               
NXT2     LA    R1,P4                                                            
         CR    R5,R1                 IF USED LAST PRINT LINE                    
         BL    NXT4                                                             
         GOTO1 SPOOL,DMCB,(R8)        PRINT WHATS THERE                         
         LA    R5,P1                  POINT R5 TO FIRST PRINT LINE              
         LA    R6,CLOTH1                                                        
         B     NTXIT                                                            
*                                                                               
NXT4     LA    R5,132(R5)            ELSE NEXT PRINT LINE                       
         LA    R6,CLOTH1                                                        
*                                                                               
NTXIT    XIT1  REGS=(R5,R6)                                                     
*************************************************************                   
         EJECT                                                                  
************************************************************8                   
* MOVECOMM - MOVES COMMENTS INTO FREE FORMAT FIELD.                             
*                                                                               
* INPUT : R2 - LOCATION OF COMMENT ELEMENT                                      
*                                                                               
* LOCALS:  R3 - START POSITION OF PART OF COMMENT TO MOVE                       
*          R4 - LENGTH OF COMMENT REMAINING                                     
*          R9 - LENGTH-1 OF COMMENT TO MOVE                                     
************************************************************8                   
MOVECOMM NTR1                                                                   
*                                                                               
         USING NUCOMD,R2                                                        
         ZIC   R4,NUCOMLEN                                                      
         SH    R4,=H'4'                                                         
         LA    R3,NUCOMMNT         ADDRESS OF BEGINNING OF COMMENT              
         DROP  R2                                                               
*                                                                               
         BAS   RE,NEXTLIN          GET A FULL PRINT LINE                        
*                                                                               
COMLOOP  LA    R1,L'CLOTHER                                                     
         CR    R4,R1               CK IF REQUIRES MORE THAN ONE LINE            
         BNH   COMM2                                                            
*                                                                               
         LA    R9,L'CLOTHER-1                                                   
SPACLOOP LA    R1,0(R9,R3)         CHECK FOR SPACE                              
         CLI   0(R1),C' '          IF A SPACE THEN GOTO MOVE IT                 
         BE    SPAC2                                                            
         BCT   R9,SPACLOOP         ELSE TRY PREVIOUS CHARACTER                  
*                                                                               
         LA    R9,L'CLOTHER-1      NO SPACES IN COMMENT SO MOVE ALL             
*                                                                               
SPAC2    EXMVC R9,CLOTHER,0(R3)     MOVE FIRST PART                             
         LA    R3,1(R3,R9)          NEXT PART OF COMMENT                        
         SR    R4,R9                R4=LENGTH OF COMMENT REMAINING              
         LA    R4,1(R4)                                                         
         BAS   RE,NEXTLIN           NEXT PRINT LINE                             
         B     COMLOOP                                                          
*                                                                               
COMM2    BCTR  R4,0                MOVE REMAINING COMMENT                       
         EXMVC R4,CLOTHER,0(R3)                                                 
         XIT1  REGS=(R5,R6)                                                     
************************************************************8                   
         EJECT                                                                  
**************************************************************                  
* HOOK - BUILDS THE HEADER. SUPPLEMENTS THE SSPECS IN NEMED50                   
************************************************************                    
HOOK     NTR1                                                                   
*                                                                               
* MAIN HEADER INFO                                                              
         LA    R5,HEAD1            BASE ADDRESS FOR OFFSETS                     
         USING PHEAD,R5                                                         
         MVC   PHCLI,NBSELCLI                                                   
         MVC   PHCLNM,UNLCLIN                                                   
         MVC   PHPRD,UNLPRO                                                     
         MVC   PHPRNM,UNLPRON                                                   
         MVC   PHNET,NBACTNET                                                   
         MVC   PHPAK(4),UNLPAK                                                  
         MVC   PHEST(7),UNLEST                                                  
         MVC   PHESNM,UNLESTN                                                   
         MVC   PHPKNM(36),UNLPAKN                                               
*                                                                               
* COLUMN HEADERS                                                                
         LA    R5,H9               BASE ADDRESS FOR PROPER OFFSETS              
         USING COLINFO,R5                                                       
*                                                                               
*                                                                               
         MVC   CLEST(3),=C'EST'                                                 
         MVC   CLPAK(3),=C'PAK'                                                 
         MVC   CLDAY(3),=C'DAY'                                                 
         MVC   CLDATE(4),=C'DATE'                                               
         MVC   CLPAKST(3),=C'PKG'                                               
         MVC   CLUNITST(3),=C'UNT'                                              
         MVC   CLLEN(3),=C'LEN'                                                 
         MVC   CLPRD1(7),=C'PRODUCT'                                            
         MVC   CLACT(7),=C' ACTUAL'                                             
         MVC   CLASS(7),=C'ASSIGN '                                             
         MVC   CLINT(6),=C'INTEG.'                                              
         MVC   CLBP(2),=C'BP'                                                   
         MVC   CLNTI(4),=C'NTI '                                                
         MVC   CLRESUL(4),=C'RSLT'                                              
         MVC   CLSHARE(4),=C' SHR'                                              
         MVC   CLHUT(4),=C' HUT'                                                
         MVC   CLRTG(4),=C' RTG'                                                
*                                                                               
         LA    R5,132(R5)          SECOND LINE                                  
*                                                                               
         MVC   CLEST(3),=C'---'                                                 
         MVC   CLPAK(3),=C'---'                                                 
         MVC   CLDAY(3),=C'---'                                                 
         MVC   CLDATE(4),=C'----'                                               
         MVC   CLPAKST(3),=C'STA'                                               
         MVC   CLUNITST(3),=C'STA'                                              
         MVC   CLLEN(3),=C'---'                                                 
         MVC   CLPRD1(7),=C'-------'                                            
         MVC   CLACT(7),=C'  COST '                                             
         MVC   CLASS(7),=C'  COST '                                             
         MVC   CLINT(6),=C'CHARGE'                                              
         MVC   CLBP(2),=C'--'                                                   
         MVC   CLNTI(4),=C'CODE'                                                
         MVC   CLRESUL(3),=C'1 2'                                               
         MVC   CLOTHER(22),=C'--------OTHERS--------'                           
*                                                                               
         XIT1                                                                   
***********************************************************                     
         EJECT                                                                  
***********************************************************                     
* GETEL,NEXTEL MACRO DEFINITION                                                 
*                                                                               
         GETEL R2,NBDTADSP,SRCHEL                                               
***********************************************************                     
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
* * LOOKUP TABLES * *                                                           
EFFEFFS  DC    50XL1'FF'           HEX FF'S TO COMPARE VS. END OF TBL           
ZEROES   DC    50XL1'00'           HEX ZEROES                                   
*                                                                               
PKSTLKUP EQU   *                   LOOKUP TABLE FOR PACKAGE STATUS              
PKSTLEN1 EQU   1                   LENGTH OF KEY VALUE                          
PKSTLEN2 EQU   9                   LENGTH OF RETURNED VALUE                     
         DC    XL1'80'                                                          
         DC    CL9'FROZEN   '                                                   
         DC    XL1'20'                                                          
         DC    CL9'LOCKED   '                                                   
         DC    XL1'04'                                                          
         DC    CL9'NON-COM  '                                                   
         DC    XL1'FF'             END OF LOOKUP TABLE                          
         DC    CL9'VARIABLE'                                                    
*                                                                               
         EJECT                                                                  
*OFFSETS FOR PAGE HEADER INFORMATION                                            
         DSECT                                                                  
PHEAD    EQU   *                                                                
PHLENGTH EQU   132                                                              
*                                                                               
         ORG   PHEAD+3*PHLENGTH+11                                              
PHCLI    DS    CL3                 CLIENT ABBR                                  
         DS    CL1                                                              
PHCLNM   DS    CL25                CLIENT NAME                                  
         ORG   PHEAD+4*PHLENGTH+11                                              
PHPRD    DS    CL3                 PRODUCT ABBR                                 
         DS    CL1                                                              
PHPRNM   DS    CL25                PRODUCT NAME                                 
         ORG   PHEAD+4*PHLENGTH+48                                              
PHNET    DS    CL4                 NETWORK                                      
         ORG   PHEAD+4*PHLENGTH+65                                              
PHPAK    DS    CL2                 PACKAGE ABBR                                 
         ORG   PHEAD+5*PHLENGTH+11                                              
PHEST    DS    CL7                 ESTIMATE ABBR                                
         DS    CL1                                                              
PHESNM   DS    CL24                ESTIMATE NAME                                
         DS    CL1                                                              
PHPKNM   DS    CL17                PACKAGE NAME                                 
         DS    CL1                                                              
PHDOLL   DS    CL1                 DOLLAR SIGN                                  
PHPKVL   DS    CL9                 PACKAGE VALUE (DOLLARS)                      
         DS    CL6                                                              
         ORG   PHEAD+7*PHLENGTH                                                 
PHSEQN   DS    CL4                 SELSEQ                                       
PHSEQ    DS    CL1                                                              
         DS    CL3                                                              
PHUOPN   DS    CL4                 SEL UNIT OPTION                              
PHUOP    DS    CL1                                                              
         DS    CL3                                                              
PHESTON  DS    CL7                 SEL EST OPT                                  
PHESTO   DS    CL1                                                              
*                                                                               
*DSECT FOR PRINT LINES                                                          
         DSECT                                                                  
COLINFO  EQU   *                                                                
*                                                                               
CLPRNM   DS    CL16                PROGRAM NAME                                 
         DS    CL1                                                              
CLPRCD   DS    CL6                 PROGRAM CODE                                 
         DS    CL1                                                              
CLFILTH  DS    CL4                 C'FLT='                                      
CLFILT   DS    CL3                 PROGRAM FILTER                               
         DS    CL1                                                              
CLPRTM   DS    CL11                PROGRAM TIME                                 
         DS    CL1                                                              
CLNET    DS    CL4                 NETWORK                                      
         DS    CL1                                                              
CLDAYP   DS    CL8                 DAYPART                                      
         DS    CL1                                                              
CLDISKH  DS    CL4                 C'D/A='                                      
CLDISKA  DS    CL8                 DISK ADDRESS                                 
*                                                                               
         ORG   COLINFO+132                                                      
CLEST    DS    CL3                 ESTIMATE                                     
         DS    CL1                                                              
CLPAK    DS    CL3                 PACKAGE                                      
         DS    CL1                                                              
CLDAY    DS    CL3                 DAY NAME                                     
         DS    CL1                                                              
CLDATE   DS    CL8                 DATE                                         
CLSUB    DS    CL3                 SUB-LINE PRINT                               
         DS    CL1                                                              
CLPAKST  DS    CL3                 PACKAGE STATUS                               
         DS    CL1                                                              
CLUNITST DS    CL3                 UNIT STATUS                                  
         DS    CL1                                                              
CLLEN    DS    CL3                 LENGTH (SECONDS)                             
         DS    CL1                                                              
CLPRD1   DS    CL3                 PRODUCT1                                     
CLSLASH  DS    CL1                 SLASH (/) IF PRD2 EXISTS                     
CLPRD2   DS    CL3                 PRODUCT2                                     
CLACT    DS    CL7                 ACTUAL COST                                  
         DS    CL1                                                              
CLASS    DS    CL7                 ASSIGNED COST                                
         DS    CL1                                                              
CLINT    DS    CL6                 INTEGRATION CHARGE                           
         DS    CL1                                                              
CLBP     DS    CL2                 BOUGHT / PAID                                
         DS    CL1                                                              
CLNTI    DS    CL4                 NTI NUMBER                                   
         DS    CL1                                                              
CLRESUL  DS    CL1                 WHERE ACTUAL DEMOS CAME FORM                 
         DS    CL1                                                              
CLRES2   DS    CL1                 SAME OR X=NETVALUE ELEM                      
*                                                                               
         ORG   COLINFO+86                                                       
         DS    CL2                                                              
CLSHARE  DS    CL4                 SHARE                                        
         DS    CL1                                                              
CLHUT    DS    CL4                 HUT                                          
         DS    CL1                                                              
CLRTG    DS    CL4                 RATING                                       
         DS    CL1                                                              
*                                                                               
         ORG   COLINFO+86+132                                                   
CLOTHER  DS    0CL31                OTHER                                       
CLOTH1   DS    CL15                                                             
         DS    CL1                                                              
CLOTH2   DS    CL15                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
******                                                                          
*** ARGS FROM EDIT                                                              
ACTIVDAT DS    CL3                 ACTIVITY DATE                                
*                                                                               
         EJECT                                                                  
*                                                                               
**** WORKING STORAGE                                                            
WORKD    DS    0D                                                               
*                                                                               
SRCHEL   DS    CL1                                                              
*                                                                               
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMED4FD                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013NEMED2F   05/01/02'                                      
         END                                                                    
