*          DATA SET NEMED60    AT LEVEL 095 AS OF 05/01/02                      
*PHASE T31E60A                                                                  
         TITLE 'T31E60 - NETWORK UNIT LIST'                                     
*                NETWORK LIST REPORT                                            
*                                                                               
*                GLOBALS:  R5 - A(CURRENT PRINT LINE)                           
*                          R7 - WORKING STORAGE                                 
*                          R8 - A(DSECT FOR SPOOL PRINTING)                     
*                          R9 - NETWORK SYSTEM DSECT                            
*                          RA - A(TWA)                                          
*                          RC - GEND                                            
*                                                                               
*                INPUTS:   NETBLOCK SET UP BY EDIT.                             
*                          CLIENT RECORD IN W/S AREA 1.                         
         SPACE 1                                                                
T31E60   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NTUL**                                                       
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
         SPACE 1                                                                
         LA    R5,P1               FIRST PRINT LINE                             
         MVI   NBDATA,C'U'         SELECT UNIT RECORDS                          
         MVI   NBESTOPT,C'Y'       TO GET DEMOS                                 
         SPACE 1                                                                
GETFIRST NETGO NSNETIO,DMCB,NETBLOCK    GET FIRST UNIT RECORD                   
         CLI   NBMODE,NBPROCUN     IF A UNIT RECORD                             
         BE    GOTONE                                                           
         CLI   NBMODE,NBREQLST     IF NO UNITS                                  
         BE    ENDIT                                                            
         B     GETFIRST            OTHER MODES ARE IGNORED                      
         SPACE 1                                                                
GOTONE   BAS   RE,FILPRG1          FILL PROG FOR FIRST ONE                      
         BAS   RE,DOMAINLN                                                      
         BAS   RE,DOFREE                                                        
         CLI   UNLTRAF,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,UNLTRAFIC                                                     
         B     GETUNIT             NOW DO OTHERS                                
         SPACE 1                                                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN     IF A UNIT RECORD                             
         BE    FILLIT                                                           
         CLI   NBMODE,NBREQLST     IF NO MORE UNITS                             
         BE    ENDIT                                                            
         B     GETUNIT             OTHER MODES ARE IGNORED                      
         SPACE 1                                                                
FILLIT   TM    NBSUBMSK,NBSBMPRG   IF NEW PROGRAM                               
         BZ    NOPROG                                                           
         BAS   RE,FILLPROG         FILL IN PROGRAM LINE                         
         BAS   RE,DOMAINLN         FILL IN MAIN LINE INFO                       
         BAS   RE,DOFREE           FILL IN THE FREE FORMATTED LINES             
         CLI   UNLTRAF,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,UNLTRAFIC                                                     
         B     GETUNIT                                                          
         SPACE 1                                                                
NOPROG   LA    R1,P4               CONTINUATION OF OLD PROG                     
         CR    R5,R1               IF USED LAST PRINT LINE                      
         BL    INCLINE                                                          
         GOTO1 SPOOL,DMCB,(R8)        PRINT WHATS THERE                         
         LA    R5,P1                  POINT R5 TO FIRST PRINT LINE              
         B     NP2                                                              
         SPACE 1                     ELSE                                       
INCLINE  LA    R5,132(R5)               NEXT PRINTLINE                          
         SPACE 1                     FI                                         
NP2      BAS   RE,DOMAINLN                                                      
         BAS   RE,DOFREE                                                        
         CLI   UNLTRAF,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,UNLTRAFIC                                                     
         B     GETUNIT              GET NEXT UNIT                               
         SPACE 1                                                                
ENDIT    GOTO1 SPOOL,DMCB,(R8)     FLUSH PRINT BUFFER                           
         XMOD1                                                                  
         SPACE 1                                                                
PROCERR  DC    F'0'                                                             
         EJECT                                                                  
*              PROCESS A NEW PROGRAM                                            
         SPACE 3                                                                
*                                  FLUSH PRINT BUFFERS                          
*                                  MOVE IN PROGRAM TO NEW BUFFER                
*              FILPRG1 (ENTRY)     FOR THE FIRST PROGRAM                        
*              OUTPUT              (R5) SET TO FIRST PRINT LINE                 
         SPACE 1                                                                
FILLPROG NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)     DUMP WHATS THERE                             
         MVI   SPACING,2           WRITE 2 BLANK LINES                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1               USE TOP OF PRINT LINE                        
         SPACE 1                                                                
         MVC   CLPRNM(L'NBPROGNM),NBPROGNM                                      
         MVC   CLPRCD(L'NBACTPRG),NBACTPRG                                      
         GOTO1 UNTIME,DMCB,NBTIME,CLPRTM                                        
         SPACE 1                                                                
         XIT1  REGS=(R5)                                                        
         SPACE 1                                                                
FILPRG1  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)     DUMP HEADERS                                 
         LA    R5,P1               USE TOP OF PRINT LINE                        
         SPACE 1                                                                
         MVC   CLPRNM(L'NBPROGNM),NBPROGNM                                      
         MVC   CLPRCD(L'NBACTPRG),NBACTPRG                                      
         GOTO1 UNTIME,DMCB,NBTIME,CLPRTM                                        
         SPACE 1                                                                
         XIT1  REGS=(R5)                                                        
         EJECT                                                                  
*              FILL IN FIXED FIELD INFORMATION                                  
         SPACE 3                                                                
DOMAINLN NTR1                                                                   
         MVC   CLDAY(L'NBDAYNAM),NBDAYNAM                                       
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(7,CLDATE)                              
         MVC   CLSUB,NBSUBOUT                                                   
         EDIT  NBLEN,(3,CLLEN)                                                  
         CLI   NBLEN1,0                                                         
         BE    DO5                                                              
         MVI   CLLEN+3,C'*'                                                     
         EDIT  NBLEN1,(3,CLLEN+4),ALIGN=LEFT                                    
DO5      L     R4,ANETWS1          ADDRESS OF CLIENT RECORD                     
         USING CLTHDR,R4                                                        
         LA    R2,CLIST            LOOK UP PRODUCT CODE                         
         SPACE 1                                                                
PRD1LOOP CLI   3(R2),0             IF END-OF-LIST THEN EXIT                     
         BE    DOPRD2                                                           
         CLC   3(1,R2),NBPRD                                                    
         BE    PRD1FND                                                          
         LA    R2,4(R2)            NEXT IN LIST                                 
         B     PRD1LOOP                                                         
         SPACE 1                                                                
PRD1FND  MVC   CLPRD1(3),0(R2)                                                  
         SPACE 1                                                                
DOPRD2   CLI   NBPRD2,X'00'        IF NOT EXIST SKIP PRINTING                   
         BE    DOCOST                                                           
         MVI   CLSLASH,C'*'                                                     
         LA    R2,CLIST            LOOK-UP PRODUCT ABBR                         
         SPACE 1                                                                
PRD2LOOP CLI   3(R2),0             IF END-OF-LIST THEN EXIT                     
         BE    DOCOST                                                           
         CLC   3(1,R2),NBPRD2                                                   
         BE    PRD2FND                                                          
         LA    R2,4(R2)            NEXT IN LIST                                 
         B     PRD2LOOP                                                         
         SPACE 1                                                                
PRD2FND  MVC   CLPRD2(3),0(R2)                                                  
         DROP  R4                                                               
         SPACE 1                                                                
DOCOST   OC    NBACTUAL,NBACTUAL   CK FOR ZERO COST                             
         BNZ   DOC2                                                             
         TM    NBUNITST,X'20'      TEST IF COST ALLOCATED                       
         BZ    DOC4                                                             
         MVC   CLACT+5(2),=C'$0'      IF SO, PRINT $0                           
         B     DOC4                                                             
         SPACE 1                                                                
DOC2     EDIT  NBACTUAL,(9,CLACT),DROP=2                                        
         MVC   CLACT+7(2),SPACES        DONT PRINT CENTS                        
         SPACE 1                                                                
DOC4     EDIT  NBINTEG,(6,CLINT),2                                              
         SPACE 1                                                                
DOBILL   CLC   NBBILTGR,=F'0'      IF NOT BILLED SKIP PRINTING                  
         BE    DOPAY                                                            
         MVC   CLBP(1),=C'B'       BILLED                                       
         SPACE 1                                                                
DOPAY    CLC   NBPAYTGR,=F'0'      IF NOT PAID SKIP PRINTING                    
         BE    DONTI                                                            
         MVC   CLBP+1(1),=C'P'     PAID                                         
         SPACE 1                                                                
DONTI    EDIT  NBNTI,(5,CLNTI),ALIGN=LEFT                                       
         MVC   CLSTAT(3),=C'EST'      STAT= EST OR PFB                          
         TM    NBUNITST,X'04'                                                   
         BZ    DOSHR                                                            
         MVC   CLSTAT(3),=C'PFB'                                                
         SPACE 1                                                                
DOSHR    EDIT  NBESTSHR,(4,CLSHARE),1                                           
         EDIT  NBESTHUT,(4,CLHUT),1                                             
         MVC   HALF(2),NBESTHOM+2                                               
         EDIT  (2,HALF),(4,CLRTG),1                                             
         SPACE 1                                                                
XITMNL   XIT1  REGS=(R5)                                                        
         EJECT                                                                  
*              FILL IN FREE FORM FIELDS                                         
         SPACE 3                                                                
*              LOCALS              (R6) BEGINNING OF CURRENT FREE FIELD         
         SPACE 1                                                                
DOFREE   NTR1                                                                   
         LA    R6,CLOTH1           THE FIRST FREE FIELD                         
         SPACE 1                                                                
DOMGF    DS    0H                                                               
         L     R2,NBAIO                                                         
         USING NUMGD,R2                                                         
         MVI   SRCHEL,6                                                         
         BAS   RE,GETEL                                                         
         BNE   DOMGB                                                            
DOMGF5   BAS   RE,NEXTLIN          GET NEXT PRINT LINE                          
         MVC   0(7,R6),=C'M/G FOR '                                             
         MVC   8(16,R6),NUMGPNM                                                 
         GOTO1 DATCON,DMCB,(2,NUMGDATE),(4,25(R6))                              
         MVI   30(R6),C'-'                                                      
         EDIT  (B1,NUMGSUB),(2,31(R6)),ALIGN=LEFT                               
         GOTO1 SQUASHER,DMCB,0(R6),34    GET RID OF SPACES                      
         BAS   RE,NEXTEL           ARE THERE ANY MORE                           
         BE    DOMGF5                                                           
         SPACE 1                                                                
DOMGB    DS    0H                                                               
         L     R2,NBAIO                                                         
         USING NUMGD,R2                                                         
         MVI   SRCHEL,7                                                         
         BAS   RE,GETEL                                                         
         BNE   DOFEED                                                           
DOMGB5   BAS   RE,NEXTLIN          GET NEXT PRINT LINE                          
         MVC   0(7,R6),=C'M/G BY  '                                             
         MVC   8(16,R6),NUMGPNM                                                 
         GOTO1 DATCON,DMCB,(2,NUMGDATE),(4,25(R6))                              
         MVI   30(R6),C'-'                                                      
         EDIT  (B1,NUMGSUB),(2,31(R6)),ALIGN=LEFT                               
         GOTO1 SQUASHER,DMCB,0(R6),34    GET RID OF SPACES                      
         BAS   RE,NEXTEL           ARE THERE ANY MORE                           
         BE    DOMGB5                                                           
         SPACE 1                                                                
DOFEED   CLC   NBFEED,ZEROES                                                    
         BE    DOUNIV1                                                          
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(5,R6),=C'FEED='                                                
         EDIT  NBFEED,(5,5(R6)),2                                               
         SPACE 1                                                                
DOUNIV1  CLC   NBUNIV,ZEROES                                                    
         BE    DOUNIV2                                                          
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(5,R6),=C'UNIV='                                                
         EDIT  NBUNIV,(5,5(R6)),2                                               
         SPACE 1                                                                
DOUNIV2  CLC   NBUNCODE,ZEROES                                                  
         BE    DOIMP                                                            
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(5,R6),=C'UNIV='                                                
         MVO   FULL(3),NBUNCODE    EDIT OUT PWO                                 
         OI    FULL+2,X'0F'                                                     
         EDIT  (P3,FULL),(4,5(R6)),ALIGN=LEFT                                   
         SPACE 1                                                                
DOIMP    CLC   NBIMPACT,ZEROES                                                  
         BE    DONSI                                                            
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(7,R6),=C'IMPACT='                                              
         EDIT  NBIMPACT,(5,8(R6)),2                                             
         SPACE 1                                                                
DONSI    CLC   NBNSI,ZEROES                                                     
         BE    DOATIM                                                           
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(4,R6),=C'NSI='                                                 
         EDIT  NBNSI,(4,5(R6))                                                  
         SPACE 1                                                                
DOATIM   CLC   NBAFFTIM,ZEROES                                                  
         BE    DOFMG                                                            
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(7,R6),=C'AFFTIM='                                              
         MVC   DUB(2),NBAFFTIM                                                  
         MVC   DUB+2(2),ZEROES     SET SECOND TIME TO 0                         
         GOTO1 UNTIME,DMCB,DUB,8(R6)                                            
         SPACE 1                                                                
DOFMG    CLC   NBFEEDMG,ZEROES                                                  
         BE    DOCOMM                                                           
         BAS   RE,NEXTOTH          GET NEXT PRINT FIELD                         
         MVC   0(4,R6),=C'FMG='                                                 
         UNPK  5(4,R6),NBFEEDMG+1(3)                                            
         MVI   9(R6),C' '                                                       
         SPACE 1                                                                
DOCOMM   MVI   SRCHEL,X'04'        LOOK FOR COMMENT ELEMENT                     
         USING NURECD,R3                                                        
         L     R3,NBAIO                                                         
         LA    R2,NUMAINEL         ADDRESS OF FIRST ELEMENT                     
         DROP  R3                                                               
         SPACE 1                                                                
         BAS   RE,NEXTEL           IF ONE EXISTS                                
         BNE   DOFILT                                                           
         SPACE 1                                                                
         BAS   RE,MOVECOMM           R2 POINTS TO COMMENT ELEMENT               
DOFILT   MVI   SRCHEL,X'08'        LOOK FOR FILTER ELEMENT                      
         USING NURECD,R3                                                        
         L     R3,NBAIO                                                         
         LA    R2,NUMAINEL         ADDRESS OF FIRST ELEMENT                     
         DROP  R3                                                               
         SPACE 1                                                                
DOFILT2  BAS   RE,NEXTEL           IF ONE EXISTS                                
         BNE   DOPKGUA                                                          
         BAS   RE,NEXTOTH                                                       
         USING NUFILD,R2                                                        
         MVC   0(1,R6),NUFILSCM    SHOW SCHEME                                  
         MVI   1(R6),C'='                                                       
         MVC   2(6,R6),NUFILTER    =FILTER                                      
         B     DOFILT2                                                          
         SPACE 1                                                                
DOPKGUA  CLC   NBGUAFAC,=2X'00'    PACKAGE GUARANTEE                            
         BE    DOPOST                                                           
         BAS   RE,NEXTOTH                                                       
         MVC   0(6,R6),=C'PKGUA='                                               
         EDIT  (B2,NBGUAFAC),(6,6(R6)),2                                        
         SPACE 1                                                                
DOPOST   CLI   NBPOSTDT,0          POSTING DATA TYPE                            
         BE    XITFREE                                                          
         BAS   RE,NEXTOTH                                                       
         MVC   0(9,R6),=C'POSTDTYP='                                            
         MVC   9(1,R6),NBPOSTDT                                                 
         SPACE 1                                                                
XITFREE  XIT1  REGS=(R5)                                                        
         EJECT                                                                  
* TRAFFIC DATA                                                                  
*                                                                               
UNLTRAFIC NTR1                                                                  
         LA    R6,CLOTH1           THE FIRST FREE FIELD                         
         L     R2,NBAIO                                                         
         USING NUCMLEL,R2                                                       
         MVI   SRCHEL,X'21'        COMMERCIAL ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   DOT10                                                            
         BAS   RE,NEXTLIN          GET NEXT PRINT LINE                          
         CLI   NUCML1,0                                                         
         BE    DOT5A                                                            
         MVC   0(11,R6),=C'COMMERCIAL='                                         
         MVC   11(8,R6),NUCML1                                                  
         CLI   NUCML2,0                                                         
         BE    DOT5                                                             
         MVI   19(R6),C'/'                                                      
         MVC   20(8,R6),NUCML2                                                  
DOT5     BAS   RE,NEXTLIN                                                       
DOT5A    CLI   NUCMLPOS,0                                                       
         BE    DOT7                                                             
         MVC   0(9,R6),=C'POSITION='                                            
         MVC   9(4,R6),NUCMLPOS                                                 
         BAS   RE,NEXTLIN                                                       
DOT7     CLI   NUCMLBSL,0                                                       
         BE    DOT10                                                            
         MVC   0(9,R6),=C'BILL LEN='                                            
         EDIT  (B1,NUCMLBSL),(3,9(R6)),ALIGN=LEFT                               
         BAS   RE,NEXTOTH                                                       
         CLI   NUCMLBSN,0                                                       
         BE    DOT7A                                                            
         MVC   0(9,R6),=C'SLIDE NO='                                            
         MVC   9(8,R6),NUCMLBSN                                                 
DOT7A    CLI   NUCMLBCN,0                                                       
         BE    DOT8                                                             
         BAS   RE,NEXTLIN                                                       
         CLI   NUCMLBCN,0                                                       
         BE    DOT7B                                                            
         MVC   0(8,R6),=C'COPY NO='                                             
         MVC   8(8,R6),NUCMLBCN                                                 
DOT7B    BAS   RE,NEXTOTH                                                       
         B     *+8                                                              
DOT8     BAS   RE,NEXTLIN                                                       
         CLI   NUCMLBPS,0                                                       
         BE    DOT10                                                            
         MVC   0(14,R6),=C'BILLBOARD POS='                                      
         MVC   14(4,R6),NUCMLBPS                                                
         BAS   RE,NEXTLIN                                                       
DOT10    MVI   SRCHEL,X'22'                                                     
         L     R2,NBAIO                                                         
         BAS   RE,GETEL                                                         
         USING NUFEDEL,R2                                                       
         BNE   DOT20                                                            
         MVC   0(14,R6),=C'UNIT = FEED = '                                      
         MVC   14(4,R6),NUFEEDCD                                                
****     BAS   RE,NEXTLIN                                                       
DOT20    MVI   SRCHEL,X'23'                                                     
         L     R2,NBAIO                                                         
         BAS   RE,GETEL                                                         
         BNE   DOTXX                                                            
         BAS   RE,NEXTLIN                                                       
         USING NUFDCEL,R2                                                       
         MVC   0(12,R6),=C'ATTACH FEED='                                        
         MVC   12(4,R6),NUFDCFED                                                
         CLI   NUFDCML2,0                                                       
         BE    DOT22                                                            
         MVI   20(R6),C'/'                                                      
         MVC   21(8,R6),NUFDCML2                                                
DOT22    BAS   RE,NEXTLIN                                                       
         CLI   NUFDCPOS,0                                                       
         BE    DOT22A                                                           
         MVC   0(9,R6),=C'POSITION='                                            
         MVC   9(4,R6),NUFDCPOS                                                 
         BAS   RE,NEXTLIN                                                       
DOT22A   CLI   NUFDCBSL,0                                                       
         BE    DOT25                                                            
         MVC   0(9,R6),=C'BILL LEN='                                            
         EDIT  (B1,NUFDCBSL),(3,9(R6)),ALIGN=LEFT                               
         BAS   RE,NEXTOTH                                                       
         CLI   NUFDCBCN,0                                                       
         BE    DOT23                                                            
         MVC   0(9,R6),=C'SLIDE NO='                                            
         MVC   9(8,R6),NUFDCBSN                                                 
DOT23    CLI   NUFDCBCN,0                                                       
         BE    DOT25                                                            
         BAS   RE,NEXTLIN                                                       
         MVC   0(8,R6),=C'COPY NO='                                             
         MVC   8(8,R6),NUFDCBCN                                                 
DOT25    BAS   RE,NEXTLIN                                                       
         CLI   NUFDCBPS,0                                                       
         BE    DOT27                                                            
         MVC   0(14,R6),=C'BILLBOARD POS='                                      
         MVC   14(4,R6),NUFDCBPS                                                
         BAS   RE,NEXTLIN                                                       
DOT27    CLI   NUFDCFED,0                                                       
         BE    DOTXX                                                            
         MVC   0(11,R6),=C'FEED SOURCE='                                        
         MVC   12(4,R6),NUFDCFED                                                
         BAS   RE,NEXTLIN                                                       
DOTXX    XIT1  REGS=(R5)                                                        
         EJECT                                                                  
*              GETS NEXT AVAILABLE PRINT LOCATION                               
         SPACE 3                                                                
*                                  CLOTH1 OR CLOTH2                             
*              ENTRY               NEXTLIN=NEXT AVAILABLE PRINT LINE            
*                                  CALL SPOOL IF ALL ARE FULL                   
*              INPUT               (R5)=CURRENT PRINT LINE                      
*              OUTPUTS             (R5)=NEW PRINTLINE                           
*                                  (R6)=PRINT LOCATION                          
         SPACE 1                                                                
NEXTOTH  NTR1                                                                   
NEXTBEG  CLC   0(L'CLOTH1,R6),SPACES    IF FIELD IS NOT USED                    
         BE    NTXIT                      DONT CHANGE ANYTHING                  
         LA    R1,CLOTH1                                                        
         CR    R6,R1               IF CURRENT IS CLOTH1                         
         BNE   NXT2                                                             
         LA    R6,CLOTH2             THEN TRY CLOTH2                            
         B     NEXTBEG                    GO CHECK IT                           
         SPACE 1                                                                
NEXTLIN  NTR1                      (ENTRY)                                      
         CLC   0(L'CLOTHER,R6),SPACES   IF FIELD IS NOT USED                    
         BE    NTXIT                       DONT CHANGE ANYTHING                 
         SPACE 1                                                                
NXT2     LA    R1,P4                                                            
         CR    R5,R1                 IF USED LAST PRINT LINE                    
         BL    NXT4                                                             
         GOTO1 SPOOL,DMCB,(R8)       PRINT WHATS THERE                          
         LA    R5,P1                 POINT R5 TO FIRST PRINT LINE               
         LA    R6,CLOTH1                                                        
         B     NTXIT                                                            
         SPACE 1                                                                
NXT4     LA    R5,132(R5)            ELSE NEXT PRINT LINE                       
         LA    R6,CLOTH1                                                        
         SPACE 1                                                                
NTXIT    XIT1  REGS=(R5,R6)                                                     
         EJECT                                                                  
*              MOVES COMMENTS INTO FREE FORMAT FIELD                            
         SPACE 3                                                                
*              INPUT               (R2)=LACATION OF COMMENT ELEMENT             
*              LOCALS              (R3)=START OF COMMENT TO MOVE                
*                                  (R4)=LENGTH OF COMMENT LEFT                  
*                                  (R9)=L-1 OF COMMENT TO MOVE                  
         SPACE 1                                                                
MOVECOMM NTR1                                                                   
         USING NUCOMD,R2                                                        
         ZIC   R4,NUCOMLEN                                                      
         SH    R4,=H'4'                                                         
         LA    R3,NUCOMMNT         ADDRESS OF BEGINNING OF COMMENT              
         DROP  R2                                                               
         SPACE 1                                                                
         BAS   RE,NEXTLIN          GET A FULL PRINT LINE                        
         SPACE 1                                                                
COMLOOP  LA    R1,L'CLOTHER                                                     
         CR    R4,R1               CK IF REQUIRES MORE THAN ONE LINE            
         BNH   COMM2                                                            
         LA    R9,L'CLOTHER-1                                                   
         SPACE 1                                                                
SPACLOOP LA    R1,0(R9,R3)         CHECK FOR SPACE                              
         CLI   0(R1),C' '          IF A SPACE THEN GOTO MOVE IT                 
         BE    SPAC2                                                            
         BCT   R9,SPACLOOP         ELSE TRY PREVIOUS CHARACTER                  
         SPACE 1                                                                
         LA    R9,L'CLOTHER-1      NO SPACES IN COMMENT SO MOVE ALL             
         SPACE 1                                                                
SPAC2    EXMVC R9,CLOTHER,0(R3)     MOVE FIRST PART                             
         LA    R3,1(R3,R9)          NEXT PART OF COMMENT                        
         SR    R4,R9                R4=LENGTH OF COMMENT REMAINING              
         LA    R4,1(R4)                                                         
         BAS   RE,NEXTLIN           NEXT PRINT LINE                             
         B     COMLOOP                                                          
         SPACE 1                                                                
COMM2    BCTR  R4,0                MOVE REMAINING COMMENT                       
         EXMVC R4,CLOTHER,0(R3)                                                 
         XIT1  REGS=(R5,R6)                                                     
         EJECT                                                                  
*              HOOK BUILDS HEADER                                               
         SPACE 3                                                                
*                                  SUPPLEMENTS SSPECS IN NEMED50                
         SPACE 1                                                                
HOOK     NTR1                                                                   
         LA    R5,HEAD1            BASE ADDRESS FOR OFFSETS                     
         USING PHEAD,R5                                                         
         MVC   PHCLI,NBSELCLI                                                   
         MVC   PHCLNM,UNLCLIN                                                   
         MVC   PHPRD,UNLPRO                                                     
         MVC   PHPRNM,UNLPRON                                                   
         MVC   PHNET,NBACTNET                                                   
         EDIT  NBPACK,(4,PHPAK),ALIGN=LEFT                                      
         EDIT  NBSELEST,(4,PHEST),ALIGN=LEFT                                    
         MVC   PHESNM,UNLESTN                                                   
         MVC   PHPKNM(L'PHPKNM),NBPAKNAM                                        
         MVI   PHDOLL,C'$'                                                      
         EDIT  NBPAKCST,(12,PHPKVL),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK          
         LKUP  NBPACKST,PKST,PHSTAT                                             
         SPACE 1                                                                
*                                  COLUMN HEADERS                               
         LA    R5,H9               BASE ADDRESS FOR PROPER OFFSETS              
         USING COLINFO,R5                                                       
         MVC   CLPRNM(12),=C'PROGRAM NAME'                                      
         MVC   CLPRCD(4),=C'CODE'                                               
         MVC   CLPRTM(4),=C'TIME'                                               
         MVC   CLDAY(3),=C'DAY'                                                 
         MVC   CLDATE(4),=C'DATE'                                               
         MVC   CLLEN(3),=C'LEN'                                                 
         MVC   CLPRD1(7),=C'PRODUCT'                                            
         MVC   CLACT(7),=C' ACTUAL'                                             
         MVC   CLINT(6),=C'INTEG.'                                              
         MVC   CLBP(2),=C'BP'                                                   
         MVC   CLNTI(4),=C'NTI '                                                
         MVC   CLSTAT(4),=C'STAT'                                               
         MVC   CLSHARE(4),=C' SHR'                                              
         MVC   CLHUT(4),=C' HUT'                                                
         MVC   CLRTG(4),=C' RTG'                                                
         MVC   CLOTHER(22),=C'--------OTHERS--------'                           
         SPACE 1                                                                
         LA    R5,132(R5)          SECOND LINE                                  
         MVC   CLDAY(3),=C'---'                                                 
         MVC   CLDATE(4),=C'----'                                               
         MVC   CLLEN(7),=C'---'                                                 
         MVC   CLPRD1(7),=C'-------'                                            
         MVC   CLACT(7),=C'  COST '                                             
         MVC   CLINT(6),=C'CHARGE'                                              
         MVC   CLBP(2),=C'--'                                                   
         MVC   CLNTI(4),=C'CODE'                                                
         MVC   CLSTAT(4),=C'----'                                               
         MVC   CLSHARE(4),=C' ---'                                              
         MVC   CLHUT(4),=C' ---'                                                
         MVC   CLRTG(4),=C' ---'                                                
         SPACE 1                                                                
         XIT1                                                                   
         EJECT                                                                  
*              GETEL NEXTEL LTORG ETC                                           
         SPACE 3                                                                
         GETEL R2,NBDTADSP,SRCHEL                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              LOOKUP TABLES                                                    
         SPACE 3                                                                
EFFEFFS  DC    50XL1'FF'           HEX FF'S TO COMPARE VS. END OF TBL           
ZEROES   DC    50XL1'00'           HEX ZEROES                                   
         SPACE 1                                                                
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
         SPACE 1                                                                
         EJECT                                                                  
*              OFFSETS FOR PAGE HEADER INFO                                     
         SPACE 3                                                                
         DSECT                                                                  
PHEAD    EQU   *                                                                
PHLENGTH EQU   132                                                              
         SPACE 1                                                                
         ORG   PHEAD+3*PHLENGTH+11                                              
PHCLI    DS    CL3                 CLIENT ABBR                                  
         DS    CL1                                                              
PHCLNM   DS    CL20                CLIENT NAME                                  
         ORG   PHEAD+4*PHLENGTH+11                                              
PHPRD    DS    CL3                 PRODUCT ABBR                                 
         DS    CL1                                                              
PHPRNM   DS    CL20                PRODUCT NAME                                 
         ORG   PHEAD+4*PHLENGTH+48                                              
PHNET    DS    CL4                 NETWORK                                      
         ORG   PHEAD+4*PHLENGTH+65                                              
PHPAK    DS    CL2                 PACKAGE ABBR                                 
         ORG   PHEAD+5*PHLENGTH+11                                              
PHEST    DS    CL1                 ESTIMATE ABBR                                
         DS    CL3                                                              
PHESNM   DS    CL24                ESTIMATE NAME                                
         DS    CL1                                                              
PHPKNM   DS    CL17                PACKAGE NAME                                 
         DS    CL1                                                              
PHDOLL   DS    CL1                 DOLLAR SIGN                                  
PHPKVL   DS    CL9                 PACKAGE VALUE (DOLLARS)                      
         DS    CL6                                                              
         ORG   PHEAD+5*PHLENGTH+82                                              
PHSTAT   DS    CL6                 STATUS ALPHA                                 
         SPACE 3                                                                
*                                  DSECT FOR PRINT LINE                         
         DSECT                                                                  
COLINFO  EQU   *                                                                
         SPACE 1                                                                
CLPRNM   DS    CL16                PROGRAM NAME                                 
         SPACE 1                                                                
         ORG   COLINFO+132                                                      
CLPRCD   DS    CL6                 PROGRAM CODE                                 
         DS    CL1                                                              
CLPRTM   DS    CL11                PROGRAM TIME                                 
         SPACE 1                                                                
         ORG   COLINFO+19                                                       
CLDAY    DS    CL3                 DAY NAME                                     
         DS    CL1                                                              
CLDATE   DS    CL5                 DATE                                         
CLSUB    DS    CL3                 SUB-LINE PRINT                               
         DS    CL1                                                              
CLLEN    DS    CL7                 LENGTH (SECONDS)                             
         DS    CL1                                                              
CLPRD1   DS    CL3                 PRODUCT1                                     
CLSLASH  DS    CL1                 SLASH (/) IF PRD2 EXISTS                     
CLPRD2   DS    CL3                 PRODUCT2                                     
CLACT    DS    CL7                 ACTUAL COST                                  
         DS    CL1                                                              
CLINT    DS    CL6                 INTEGRATION CHARGE                           
         DS    CL1                                                              
CLBP     DS    CL2                 BOUGHT / PAID                                
         DS    CL1                                                              
CLNTI    DS    CL4                 NTI NUMBER                                   
         DS    CL1                                                              
CLSTAT   DS    CL4                 STATUS LOOKUP                                
         DS    CL1                                                              
CLSHARE  DS    CL4                 SHARE                                        
         DS    CL1                                                              
CLHUT    DS    CL4                 HUT                                          
         DS    CL1                                                              
CLRTG    DS    CL4                 RATING                                       
         DS    CL1                                                              
         SPACE 1                                                                
         ORG   COLINFO+90                                                       
CLOTHER  DS    0CL31                OTHER                                       
CLOTH1   DS    CL15                                                             
         DS    CL1                                                              
CLOTH2   DS    CL15                                                             
         SPACE 1                                                                
*              NETINCLS            INCLUDED HERE                                
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                  WORKING STORAGE                              
WORKD    DS    0D                                                               
         SPACE 1                                                                
SRCHEL   DS    CL1                                                              
         SPACE 1                                                                
WORKX    EQU   *                                                                
         SPACE 1                                                                
*              NEMEDFFD            INCLUDED HERE                                
*              NEMEDE0D                                                         
*              NEGENUNIT                                                        
*              SPGENCLT                                                         
         PRINT OFF                                                              
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE0D                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'095NEMED60   05/01/02'                                      
         END                                                                    
