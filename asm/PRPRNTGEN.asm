*          DATA SET PRPRNTGEN  AT LEVEL 010 AS OF 05/01/02                      
*PHASE T00PRNT                                                                  
*INCLUDE PERVERT                                                                
************************ MACROS  **********************************             
         MACRO                                                                  
&TAG     IFMVC &A,&B,&C                                                         
&TAG     LTR   RF,&A                                                            
         BZ    IFM&SYSNDX                                                       
         LA    R1,&B                                                            
         MOVE  ((RF),(R1)),&C                                                   
IFM&SYSNDX   EQU   *                                                            
         MEND                                                                   
*                                                                               
         TITLE 'T00A42 - PRINT GENERAL ROUTINES'                                
*******************************************************************             
* ORGANIZATION OF MEMORY: USED BY ENTIRE PRINT SYSTEM                           
*                                                                               
*      20000 BYTES                     TWA                                      
*      ---------------                ---------------                           
* R8-> ' SPOOLD      '           RA-> ' SYSTEM TWA  '                           
*      ' (3144 BYTES)'                '             '                           
*      '-------------'                '-------------'                           
* RC-> ' GEND        '                ' APPLICATION '                           
*      ' (2792 BYTES)'                '        TWA  '                           
*      '-------------'                '-------------'                           
*      ' IO AREA     '                                                          
*      ' (2000 BYTES)'                                                          
*      '-------------'                                                          
* R9-> ' PRNTSYSD     '                                                         
*      '  (424 BYTES)'                                                          
*      '-------------'                                                          
*      ' PRTBLOK     '                                                          
*      ' (1024 BYTES)'                                                          
*      '-------------'                                                          
*      ' DRIVEBLOCK  '                                                          
*      ' (1280 BYTES)'                                                          
*      '-------------'                                                          
*      ' DRONEBLOCK  '                                                          
*      ' (580 BYTES) '                                                          
*      '-------------'                                                          
*      ' APPLICATION '                                                          
*      '   COMMON    '                                                          
*      '  (W/S 1)    '                                                          
*      ' (2008 BYTES)                                                           
*      '-------------'                                                          
*      ' LOCAL W/S   '                                                          
*      '   (W/S 2)   '                                                          
*      '-------------'                                                          
************************************************************                    
         EJECT                                                                  
T00A42   CSECT                                                                  
         PRINT NOGEN                                                            
         REQUS                                                                  
         USING *,RF                                                             
NGEN     NTR1                                                                   
         LA    RB,NGEN                                                          
         DROP  RF                                                               
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    RA,2048(R7)                                                      
         LA    RA,2048(RA)                                                      
         USING T00A42,RB,R7,RA                                                  
         LA    R3,RELOC                                                         
         S     R3,RELOC                                                         
         ST    R3,RELO                                                          
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING PRNTSYSD,R9                                                      
         SRL   RF,24               GET ROUTINE NUMBER                           
         SLL   RF,2                MULTIPLY BY 4                                
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
RELOC    DC    A(*)                                                             
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BRANCH TABLE                                                     
         SPACE 3                                                                
*                                  ORDERED AS IN NENVEQUS                       
         SPACE 1                                                                
VBRANCH  B     VVERR               0                                            
         B     VVGETFLD            1                                            
         B     VVAGYOUT            2                                            
         B     VVERR               3  (VVAGYALL)                                
         B     VVAGY               4                                            
         B     VVMEDIA    *******                                               
         B     VVCLIOUT            5                                            
         B     VVCLIALL            6                                            
         B     VVCLI               7                                            
         B     VVPRDOUT            8                                            
         B     VVPRDALL            9                                            
         B     VVPRD               10                                           
         B     VVESTOUT            11                                           
         B     VVESTALL            12                                           
         B     VVEST               13                                           
         B     VVSTRDAT            22                                           
         B     VVENDDAT            23                                           
         B     VVESTRNG            25                                           
         B     VVDAY               29                                           
*        B     VVHEAD              47                                           
         B     VVDRINIT            48                                           
         B     VVMEDFIL            53                                           
***      B     VVTARBUF            54                                           
***      B     VVTITLE             55                                           
***      B     VVTITOUT            56                                           
***      B     VVACNEW             57                                           
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
VVERR    DC    H'0'                                                             
         EJECT                                                                  
*              GETFLD ROUTINE - EXTRACT DATA FROM SCREEN FIELD                  
         SPACE 3                                                                
*              INPUTS              R2=A(FIELD HEADER)                           
*                                  FTERMFLG=1 FIELD IS OPTIONAL                 
*              ARGUMENTS           1 (R5) MAXIMUM NUMERIC VALUE                 
*                                         DEFAULT IS 255                        
*              LOCAL               R4=MAX NUMERIC VALUE                         
*              OUTPUTS             FLDH  CONTAINS FIELD HEADER                  
*                                  FLD   FIELD DATA SPACE FILLED                
*                                  R0    BINARY VALUE IF FIELD NUMERIC          
*                                  R1    FIELD LENGTH                           
*                                  CONDITION CODE ZERO IF R1=0                  
         SPACE 1                                                                
VVGETFLD L     R5,0(R1)            GET A(FULLWORD)                              
         LA    R4,255              SET R4 TO DEFAULT IN CASE R5=0               
         LTR   R5,R5               IF ZERO THEN                                 
         BZ    *+8                                                              
         L     R4,0(R5)            GET ACTUAL VALUE IN R4                       
         MVC   FLDH,0(R2)                                                       
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL WITH SPACES                             
         SR    R0,R0               PRE-CLEAR REGISTER FOR NUMERIC VALUE         
         ZIC   R1,FLDH                                                          
         SH    R1,=H'9'                                                         
         TM    FLDH+1,X'02'        TEST FOR EXTENDED HEADER                     
         BNO   *+8                                                              
         SH    R1,=H'8'            (SUBTRACT 8 MORE)                            
         EX    R1,FLDMOVE                                                       
         LA    RE,FLD(R1)          POINT RE AT LAST EXTRACTED BYTE              
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
         SPACE 1                                                                
GETFLD1  CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLD2                                                          
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,GETFLD1                                                       
         SPACE 1                                                                
GETFLD2  STC   R1,FLDH+5           SET ACTUAL DATA LENGTH                       
         LTR   R1,R1               TEST FOR EMPTY FIELD                         
         BZ    GTFERRCK                                                         
         SPACE 1                                                                
GETFLD4  LR    R3,R1               GET FLD LENGTH-1 IN R3                       
         BCTR  R3,0                                                             
         MVC   WORK(6),=6X'F0'     TEST FOR NUMERIC FIELD                       
         EX    R3,MOVEZONE                                                      
         CLC   WORK(6),=6X'F0'                                                  
         BNE   GETFLDX                                                          
         EX    R3,FLDPACK                                                       
         CVB   R0,DUB                                                           
         LTR   R0,R0               CK IF INPUT=0                                
         BZ    GTNOTNUM                                                         
         CR    R0,R4               CK IF GT SPECIFIED MAXIMUM                   
         BH    GTNOTNUM                                                         
         B     GETFLDX                                                          
         SPACE 1                                                                
GTNOTNUM SR    R0,R0               NON-NUMERIC. SO SET R0 TO 0                  
         B     GETFLDX                                                          
         SPACE 1                                                                
GTFERRCK CLI   FTERMFLG,1          IS THIS OK?                                  
         BE    GETFLDX                                                          
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
GETFLDX  LTR   R1,R1                                                            
         XIT1  REGS=(R0,R1)                                                     
         SPACE 1                                                                
FLDMOVE  MVC   FLD(0),8(R2)                                                     
         SPACE 1                                                                
FLDPACK  PACK  DUB,FLD(0)                                                       
         SPACE 1                                                                
MOVEZONE MVZ   WORK(0),8(R2)                                                    
         EJECT                                                                  
*              AGENCY AND MEDIA ROUTINES                                        
         SPACE 3                                                                
VVAGYOUT DS    0H                                                               
         MVC   PBACOM,ACOMFACS     SET UP NETBLOCK                              
         MVC   PBAIO,AIO                                                        
         MVI   PBQINIT,0           RE-INITIALIZE                                
         L     R1,ATWA                                                          
         USING TWADSEC,R1                                                       
         MVC   PBNOWRIT,TWAWRITE   ALLOWS WRITE TO BE INHIBITED                 
         DROP  R1                                                               
         SPACE 1                                                                
         NETGO CALLOV,DMCB,0,X'D9000A39'   LOAD T00A39 (DRONE)                  
         L     R2,DMCB                                                          
         ST    R2,PDDRONE                                                       
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        ALSO INHIBIT WRITE IF OFFLINE                
         BNE   VVA2                                                             
         MVI   PBNOWRIT,C'N'                                                    
         CLI   OFFLINE,C'Y'        ALSO INHIBIT WRITE IF OFFLINE                
         BNE   VVA2                                                             
         MVI   PBNOWRIT,C'N'                                                    
         SPACE 1                                                                
VVA2     MVI   PBSELMOD,PBVALAGY                                                
         MVC   PBSELAGY,AGENCY                                                  
         NETGO PRNTIO,DMCB,PRNTBLOK                                             
         LA    R1,PRNTBLOK                                                      
         ST    R1,PDNETBLK                                                      
         MVC   PDAGYABR,AGENCY                                                  
         MVC   PDAGYKEY,PBACTAGY                                                
         MVI   PBSELMOD,0          RESET SELECT MODE                            
         MVC   ERROR,PBERROR       RETURNED                                     
         CLI   PBERROR,PBGOOD                                                   
         BNE   TRAPERR                                                          
         L     R4,PBAIO                                                         
         USING PAGYREC,R4                                                       
         MVC   USERNAME,PAGYNAME                                                
         MVC   USERADDR,PAGYADDR                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              STANDARD VALIDATE AGENCY                                         
         SPACE 3                                                                
*              ARGUMENTS           1 (R3) A(OUTPUT AGENCY NAME)                 
*                                  2 (R4) A(OUTPUT AGENCY ADDRESS)              
         SPACE 1                                                                
VVAGY    LM    R3,R4,0(R1)         SAVE ARG LIST                                
         MVI   PBQINIT,0           START AT BEGINNING                           
         MVI   PBSELMOD,PBVALAGY                                                
         NETGO PRNTIO,DMCB,PRNTBLOK   VALIDATE AGY                              
         MVI   PBSELMOD,0          RESET SELECT MODE                            
         MVC   ERROR,PBERROR       RETURNED                                     
         CLI   PBERROR,PBGOOD                                                   
         BNE   TRAPERR                                                          
         L     R6,PBAIO            AGY RECORD                                   
         USING PAGYREC,R6                                                       
         IFMVC R3,L'PAGYNAME,PAGYNAME     MOVE IF ARG 1 GIVEN                   
         IFMVC R4,L'PAGYADDR,PAGYADDR     MOVE IF ARG 2 GIVEN                   
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              VALIDATE MEDIA                                                   
         SPACE 3                                                                
*              INPUT               ASSUME AGY VALIDATED                         
*              OUTPUT                                                           
*                                                                               
*              LOCALS                                                           
*                                                                               
         SPACE 1                                                                
VVMEDIA  LM    R3,R4,0(R1)         SAVE ARG LIST                                
         MVI   PBSELMOD,PBVALMED                                                
         NETGO NVGETFLD,DMCB                                                    
         BZ    XITMEDIA                                                         
         CLI   FLD,C'P'                                                         
         BE    VMED3                                                            
         CLI   FLD,C'N'                                                         
         BE    VMED3                                                            
         CLI   FLD,C'M'                                                         
         BE    VMED3                                                            
         CLI   FLD,C'T'                                                         
         BE    VMED3                                                            
         CLI   FLD,C'O'                                                         
         BE    VMED3                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
VMED3    CLI   PBSELAGY,0          IF AGY =ALL                                  
         BE    XITMEDIA                                                         
         CLI   PBSELAGY,=C'ALL'                                                 
         BE    XITMEDIA                                                         
         NETGO PRNTIO,DMCB,PRNTBLOK   VALIDATE MEDIA                            
         MVI   PBSELMOD,0          RESET SELECT MODE                            
         MVC   ERROR,PBERROR       RETURNED                                     
         CLI   PBERROR,PBGOOD                                                   
         BNE   TRAPERR                                                          
         L     R6,PBAIO                                                         
         USING PAGYREC,R6                                                       
         IFMVC R3,L'PAGYMED,PAGYMED                                             
XITMEDIA B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT CLIENT                                         
         SPACE 3                                                                
*                                  ASSUMES AGY IN NETBLOCK                      
*              INPUT               R2=A(CLIENT FIELD)                           
*              LOCAL               R3=A(TWA)                                    
         SPACE 1                                                                
VVCLIOUT L     R3,ATWA                                                          
         USING TWADSEC,R3                                                       
         MVI   PBSELMOD,PBVALCLI                                                
         NETGO PRNTIO,DMCB,PRNTBLOK                                             
         MVI   PBSELMOD,0          RESET SELECT MODE                            
         MVC   ERROR,PBERROR       RETURNED                                     
         CLI   PBERROR,PBGOOD                                                   
         BNE   TRAPERR                                                          
         CLI   PBMODE,PBREQLST                                                  
         BNE   VVC2                                                             
         MVI   ERROR,INVOFF        NO CLIENTS                                   
         B     TRAPERR                                                          
         SPACE 1                                                                
VVC2     L     R4,PBAIO                                                         
         USING PCLTREC,R4                                                       
         OC    TWAACCS(2),TWAACCS  TWA TELLS IF LOCKOUT CHECKS                  
         BZ    VVCLIXIT                                                         
         CLI   TWAACCS,C'*'        IF * THEN SKIP CLI LOCKOUT                   
         BE    VVCLIOFF                                                         
         CLI   TWAACCS,C'-'        IF NOT * OR - THEN CLIENT LOCKOUT            
         BNE   VVCLICLI                                                         
         CLI   TWAACCS+1,C'*'      -* MEANS ALL BUT THIS OFFICE                 
         B     VVCLXOFF                                                         
         SPACE 1                                                                
VVCLXCLI CLC   TWAACCS+1(2),PBSELCLI   ALL BUT THIS CLIENT                      
         BE    VVCLIERR                                                         
         B     VVCLIXIT                                                         
         SPACE 1                                                                
VVCLICLI CLC   TWAACCS(2),PBSELCLI    CLIENT LOCKOUT                            
         BE    VVCLIXIT                                                         
         B     VVCLIERR                                                         
         SPACE 1                                                                
****** HOW DOES PRINT HANDLE OFFICE LOCKOUT                                     
*******************************************************                         
*   VVCLIOFF CLC   TWAACCS+1(1),COFFICE     OFFICE LOCKOUT                      
*         BE    VVCLIXIT                                                        
*         B     VVCLIERR                                                        
         SPACE 1                                                                
*   VCLXOFF CLC   TWAACCS+2(1),COFFICE      ALL BUT THIS OFFICE                 
*          BE    VVCLIERR                                                       
*           B     VVCLIXIT                                                      
         SPACE 1                                                                
VVCLIERR MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VVCLIXIT B     XIT                                                              
         DROP  R4                                                               
         DROP  R3                                                               
         EJECT                                                                  
*              STANDARD VALIDATE CLIENT                                         
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT CLIENT NAME)                     
*                                  2  A(OUTPUT PRODLIST)                        
         SPACE 1                                                                
VVCLI    LM    R3,R4,0(R1)         ARG LIST                                     
         XC    PBSELCLI,PBSELCLI   CLEAR TO DEFAULTS                            
         ST    R2,PDCLISCR         SAVE ADDRESS OF SCREEN HEADER                
         MVI   PDCLIRMD,PDTOG                                                   
         NETGO NVGETFLD,DMCB                                                    
         BZ    XITVCLI             IF NO INPUT                                  
         MVC   PBSELCLI,FLD                                                     
         CLC   PBSELCLI,=C'ALL'                                                 
         BNE   LVC2                                                             
         MVI   ERROR,INVCLI        DONT ALLOW ALL                               
         B     TRAPERR                                                          
         SPACE 1                                                                
LVC2     NETGO NVCLIOUT,DMCB       FILL STANDARD STUFF                          
         MVI   PDCLIRMD,PDONE                                                   
         L     R6,PBAIO            CLIENT RECORD                                
         USING PCLTREC,R6                                                       
         IFMVC R3,L'PCLTNAME,PCLTNAME   MOVE IF FIRST ARG GIVEN                 
         SPACE 1                                                                
XITVCLI  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              VALIDATE CLIENT - SPECIAL CASE=ALL                               
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT CLIENT NAME)                     
*                                  2  A(OUTPUT PRODLIST)                        
*                                  SPACES RETURNED IF CLI=ALL                   
*              OUTPUT              PBSELCLI                                     
         SPACE 1                                                                
VVCLIALL LM    R3,R4,0(R1)         SAVE ARG LIST                                
         MVI   PDCLIRMD,PDTOG                                                   
         ST    R2,PDCLISCR         SAVE ADDRESS OF SCREEN HEADER                
         MVC   PBSELCLI,=C'ALL'    ASSUME ITS ALL FOR NETIO                     
         NETGO NVGETFLD,DMCB                                                    
         BZ    LVCALL              TAKE NO INPUT AS TOG                         
         CLC   FLD(3),=C'NO,'                                                   
         BNE   VVCLIA1             NO,*O IS TO WITH OFFICE FILTER               
         LA    R5,FLD+3                                                         
         BAS   RE,DOOFF            SET OFFICE FILTERS                           
         B     LVCALL                                                           
         SPACE 1                                                                
VVCLIA1  MVI   PDCLIRMD,PDSEP                                                   
         CLI   FLD,C'*'            IF REQUEST BY OFFICE                         
         BNE   VVCLA2                                                           
         LA    R5,FLD                                                           
         BAS   RE,DOOFF            SET OFFICE FILTER                            
         B     LVCALL                                                           
         SPACE 1                                                                
VVCLA2   MVC   PBSELCLI,FLD                                                     
         NETGO NVCLIOUT,DMCB       FILL STANDARD STUFF                          
         CLC   FLD(3),=C'ALL'      IF ALL GIVEN                                 
         BE    LVCALL                THEN DO FOR ALL                            
         MVI   PDCLIRMD,PDONE                                                   
         L     R6,PBAIO            CLIENT RECORD                                
         USING PCLTREC,R6                                                       
         IFMVC R3,L'PCLTNAME,PCLTNAME   MOVE IF FIRST ARG GIVEN                 
         B     LVCAXIT                                                          
         SPACE 1                                                                
LVCALL   IFMVC R3,L'PCLTNAM,SPACES  MOVE SPACES IF FIRST ARG GIVEN              
         LTR   R4,R4               ARG GIVEN?                                   
         BZ    LVCAXIT             YES. THEN PROGRAM LOGIC ERROR.               
         MVI   ERROR,PBSYSERR                                                   
         B     TRAPERR                                                          
         SPACE 1                                                                
DOOFF    NTR1                      SET OFFICE FILTER                            
*                                  R5=A(FILTER EXPRESSION)                      
         CLI   FLD,C'*'            GET PAST * UNLESS FORGOTTEN                  
         BNE   *+8                                                              
         LA    R5,1(R5)                                                         
         MVC   PBSELOFF,0(R5)      FILL IN OFFICE FILTER                        
         CLI   0(R5),C'-'          CHECK FOR NEGATIVE OFFICE FILT               
         BNE   XIT                 (*-O)                                        
         MVC   PBSELOFF,1(R5)                                                   
         NI    PBSELOFF,X'BF'      TURN OFF X'40' BIT                           
         B     XIT                                                              
         SPACE                                                                  
LVCAXIT  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO OUTPUT PRODUCT                                        
         SPACE 3                                                                
*                                  ASSUMES AGY/CLI IN NETBLOCK                  
         SPACE 1                                                                
VVPRDOUT DS    0H                                                               
         MVI   PBSELMOD,PBVALPRD                                                
         NETGO PRNTIO,DMCB,PRNTBLOK                                             
         MVI   PBSELMOD,0          RESET SELECT MODE                            
         MVC   ERROR,PBERROR       RETURNED                                     
         CLI   PBERROR,PBGOOD                                                   
         BNE   TRAPERR                                                          
         B     XIT                                                              
         EJECT                                                                  
*              STANDARD VALIDATE PRODUCT                                        
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT PRODUCT NAME)                    
*              OUTPUT              PBSLEPRD                                     
         SPACE 1                                                                
VVPRD    L     R3,0(R1)            SAVE ARG LIST                                
         MVI   PDPRDRMD,PDTOG                                                   
         ST    R2,PDPRDSCR         SAVE ADDRESS OF SCREEN HEADER                
         XC    PBSELPRD,PBSELPRD   RESET                                        
         IFMVC R3,L'PPRDNAME,SPACES   INITIALIZE FIRST ARG TO SPACES            
         NETGO NVGETFLD,DMCB                                                    
         BZ    XITVPRD             IF NO INPUT                                  
         MVC   PBSELPRD,FLD                                                     
         CLC   PBSELPRD,=C'ALL'                                                 
         BNE   LVP2                                                             
         MVI   ERROR,INVPROD       DONT ALLOW ALL                               
         B     TRAPERR                                                          
         SPACE 1                                                                
LVP2     NETGO NVPRDOUT,DMCB       FILL STANDARD STUFF                          
         MVI   PDPRDRMD,PDONE                                                   
         L     R6,PBAIO            CLIENT RECORD                                
         USING PPRDREC,R6                                                       
         IFMVC R3,L'PPRDNAME,PPRDNAME      MOVE IF FIRST ARG GIVEN              
         SPACE 1                                                                
XITVPRD  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              VALIDATE PRODUCT - SPECIAL CASE=ALL                              
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT PRODUCT NAME)                    
*                                  SPACES RETURNED IF PRD=ALL                   
*              OUTPUT              PBSELPRD                                     
         SPACE 1                                                                
VVPRDALL L     R3,0(R1)            SAVE ARG LIST                                
         MVI   PDPRDRMD,PDTOG                                                   
         ST    R2,PDPRDSCR         SAVE ADDRESS OF SCREEN HEADER                
         MVC   PBSELPRD,=C'POL'                                                 
         IFMVC R3,L'PPRDNAME,SPACES   INITIALIZE FIRST ARG TO SPACES            
         NETGO NVGETFLD,DMCB                                                    
         BZ    LVPAXIT             NO INPUT=POL                                 
         MVI   PDPRDRMD,PDSEP                                                   
         CLI   FLD+1,C'='          CHECK FOR GROUP                              
         BE    VVPRDGRP                                                         
         MVC   PBSELPRD,FLD                                                     
         NETGO NVPRDOUT,DMCB       FILL STANDARD STUFF                          
         CLC   PBSELPRD,=C'ALL'    IF ALL GIVEN                                 
         BE    LVPAXIT               THEN EXIT                                  
         MVI   PDPRDRMD,PDONE                                                   
         L     R6,PBAIO            PRODUCT RECORD                               
         USING PPRDREC,R6                                                       
         IFMVC R3,L'PPRDNAME,PPRDNAME   MOVE IF FIRST ARG GIVEN                 
         B     LVPAXIT                                                          
         SPACE 1                                                                
VVPRDGRP MVC   PBSELPGR(1),FLD    GROUP PASS X0000 FOR X=ALL                    
         MVC   PBSELPRD,=C'ALL'                                                 
         CLC   FLD+2(3),=C'ALL'                                                 
         BE    *+10                                                             
         MVC   PBSELPGR+1(4),FLD+2        OR X1234 FOR X=1234                   
         NETGO NVPRDOUT,DMCB                                                    
         MVI   PDPRGRMD,1                                                       
         CLI   FLD+5,C'*'                                                       
         BNE   LVPAXIT                                                          
         MVI   PDPRGRMD,2                                                       
         SPACE 1                                                                
LVPAXIT  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO OUTPUT ESTIMATE                                       
         SPACE 3                                                                
*                                  ASSUMES AGY/CLI/PRD IN NETBLOCK              
         SPACE 1                                                                
VVESTOUT MVI   PBSELMOD,PBVALEST                                                
         NETGO PRNTIO,DMCB,PRNTBLOK                                             
         MVI   PBSELMOD,0          RESET SELECT MODE                            
         MVC   ERROR,PBERROR       RETURNED                                     
         CLI   PBERROR,PBGOOD                                                   
         BNE   TRAPERR                                                          
         CLI   PDESTRMD,PDONE      IF ONE ESTIMATE WAS SELECTED                 
         BNE   XIT                                                              
         MVI   PBDONTFD,C'Y'       TELL NETIO NOT TO FILTER DATES               
         B     XIT                                                              
         EJECT                                                                  
*              STANDARD VALIDATE ESTIMATE                                       
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT ESTIMATE NAME)                   
*                                  2  A(NET DEMO BLOCK)                         
*              OUTPUT              NBSELEST + THOSE IN PROCEST                  
*                                  IF ARG 2 THEN NDDEMOS NDNDEMOS               
         SPACE 1                                                                
VVEST    L     R3,0(R1)            SAVE ARG LIST                                
         MVI   PDESTRMD,PDTOG                                                   
         ST    R2,PDESTSCR         SAVE ADDRESS OF SCREEN HEADER                
         L     R4,4(R1)                                                         
         IFMVC R3,24,SPACES        INITIALIZE FIRST ARG TO SPACES               
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVE2                IF NO INPUT                                  
         MVI   PDESTRMD,PDONE                                                   
         LTR   R0,R0               CHECK NUMERIC                                
         BNZ   VVE2                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VVE2     STC   R0,PBSELEST                                                      
         NETGO NVESTOUT,DMCB       FILL STANDARD STUFF                          
         CLI   PBSELEST,0          DONT PROCEST IF NONE GIVEN                   
         BE    VVEXIT                                                           
         BAS   RE,PROCEST                                                       
         SPACE 1                                                                
VVEXIT   B     XIT                                                              
         EJECT                                                                  
*              VALIDATE ESTIMATE - SPECIAL CASE=ALL                             
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT ESTIMATE NAME)                   
*                                  SPACES RETURNED IF EST=ALL                   
*                                  2  A(NET DEMO BLOCK)                         
*              OUTPUT              NBSELEST + THOSE IN PROCEST                  
*                                  IF ARG 2 THEN NDDEMOS NDNDEMOS               
         SPACE 1                                                                
VVESTALL L     R3,0(R1)            ARG LIST                                     
         L     R4,4(R1)                                                         
         MVI   PDESTRMD,PDTOG                                                   
         ST    R2,PDESTSCR         SAVE ADDRESS OF SCREEN HEADER                
         IFMVC R3,24,SPACES        INITIALIZE FIRST ARG TO SPACES               
         NETGO NVGETFLD,DMCB                                                    
         BZ    VEA2                NO INPUT = ALL                               
         MVI   PDESTRMD,PDSEP                                                   
         CLC   FLD(3),=C'ALL'         IF ALL, SELEST IS 0 SO GO ON.             
         BE    VEA2                                                             
         MVI   PDESTRMD,PDONE                                                   
         LTR   R0,R0               MAKE SURE ITS NUMERIC                        
         BNZ   VEA2                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VEA2     STC   R0,PBSELEST         NUMERIC VALUE OF INPUT                       
         NETGO NVESTOUT,DMCB       FILL STANDARD STUFF                          
         CLI   NBSELEST,0          DONT PROCEST IF NONE GIVEN                   
         BE    VEAXIT                                                           
         BAS   RE,PROCEST          FILL FIELDS                                  
         SPACE 1                                                                
VEAXIT   B     XIT                                                              
         EJECT                                                                  
*              VALIDATE ESTIMATE RANGE                                          
         SPACE 3                                                                
*              SPECIAL CASE        ALL, NO, RANGE OF ESTS                       
*                                                                               
*              ARGUMENTS           1  A(OUTPUT ESTIMATE NAME)                   
*                                  SPACES RETURNED IF EST=ALL                   
*                                  2  A(NET DEMO BLOCK)                         
*              OUTPUT              NBSELEST + THOSE IN PROCEST                  
*                                  IF ARG 2 THEN NDDEMOS NDNDEMOS               
*              LOCAL               R5 FOR ESTIMATE FILTERS                      
         SPACE 1                                                                
VVESTRNG LM    R3,R4,0(R1)         ARG LIST                                     
         MVI   PDESTRMD,PDTOG                                                   
         ST    R2,PDESTSCR         SAVE ADDRESS OF SCREEN HEADER                
         IFMVC R3,24,SPACES        INITIALIZE FIRST ARG TO SPACES               
         NETGO SCANNER,DMCB,(R2),(3,BLOCK)   LOOK FOR RANGE                     
         CLI   DMCB+4,2            MUST BE 2 PARTS FOR RANGE                    
         BH    ESTBADRG                                                         
         BL    VER1                                                             
         CLC   BLOCK+12(2),=C'NO'  CK FOR NO,ESTFILT                            
         BE    VER1                                                             
         CLC   BLOCK+12(3),=C'ALL' OR FOR ALL,ESTFILT                           
         BE    VER1                                                             
         MVC   PBSELEST,BLOCK+7      FIRST RANGE VALUE                          
         CLI   PBSELEST,0          ZERO MEANS NON NUMERIC OR ZERO               
         BE    ESTBADRG                                                         
         MVC   PBSELESE,BLOCK+39   SECOND RANGE VALUE                           
         CLC   PBSELESE,PBSELEST   CK FOR 0 OR LESS THAN 1ST VALUE              
         BNH   ESTBADRG                                                         
         NETGO NVESTOUT,DMCB                                                    
         B     VERXIT                                                           
         SPACE 1                                                                
VER1     NETGO NVGETFLD,DMCB       ONLY ONE OR NONE GIVEN OR NO,ESTFILT         
         BZ    VER4                NO INPUT = NO                                
         LA    R5,BLOCK+44         R5=A(POSSIBLE FILTER EXPRESSION)             
         CLC   FLD(2),=C'NO'                                                    
         BE    VER2                                                             
         LA    R5,BLOCK+22                                                      
         CLC   FLD(2),=C'F='                                                    
         BE    VER2                                                             
         MVI   PDESTRMD,PDSEP                                                   
         LA    R5,BLOCK+44                                                      
         CLC   FLD(3),=C'ALL'         IF ALL, SELEST IS 0 SO GO ON.             
         BE    VER2                                                             
         MVI   PDESTRMD,PDONE                                                   
         LTR   R0,R0               MAKE SURE ITS NUMERIC                        
         BZ    VER1A                                                            
         STC   R0,PBSELEST         NUMERIC VALUE OF INPUT                       
         B     VER4                                                             
         SPACE 1                                                                
VER1A    MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VER2     CLI   0(R5),X'41'         IF ANYTHING IS IN BLOCK+NN                   
         BL    VER4                   IT MUST BE FILTER EXPRESSION              
         NETGO NVEFOUT,DMCB           SO DEAL WITH FILTERS NOW                  
         SPACE 1                                                                
VER4     NETGO NVESTOUT,DMCB       FILL STANDARD STUFF                          
         CLI   PBSELEST,0          DONT PROCEST IF NONE GIVEN                   
         BE    VERXIT                                                           
         BAS   RE,PROCEST          FILL FIELDS                                  
         SPACE 1                                                                
VERXIT   B     XIT                                                              
         SPACE 1                                                                
ESTBADRG MVI   ERROR,PBINVEST                                                   
         B     TRAPERR                                                          
         EJECT                                                                  
*              FILL FIELDS FOR ESTIMATE RECORD                                  
         SPACE 3                                                                
*                                  INTERNAL - NO FROM NETGO                     
*              INPUTS              NBAIO=A(ESTIMATE RECORD)                     
*                                  R3=A(ESTNAME)                                
*                                  R4=A(DEMO BLOCK)                             
*              OUTPUTS             USERQSTR USERQEND NBSELSTR NBSELEND          
*                                  NDWGTLST NDDEMOS NDUSRNMS                    
         SPACE 1                                                                
PROCEST  NTR1                                                                   
         L     R6,PBAIO            ESTIMATE RECORD                              
         USING PESSTREC,R6                                                      
         IFMVC R3,L'PESTNAME,PESTNAME  MOVE IF FIRST ARG GIVEN                  
         MVC   USERQSTR,PESTST     SET UP DATES FOR SPOO(F/L)                   
         MVC   USERQEND,PESTEND                                                 
         MVC   NBSELSTR,PESTST     SET UP DATES FOR NETIO                       
         MVC   PBSELEND,PESTEND                                                 
         SPACE 1                                                                
PESTXIT  B     XIT                                                              
         DROP  R6                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE ESTIMATE FILTER                              
         SPACE 3                                                                
*              OUTPUT              PBSELEFL                                     
         SPACE 1                                                                
VVEFILT  DS    0H                                                               
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVEFLXIT                                                         
         LA    R5,FLD              SET ARG                                      
         NETGO NVEFOUT,DMCB                                                     
         SPACE 1                                                                
VVEFLXIT B     XIT                                                              
         EJECT                                                                  
*              ESTIMATE FILTER OUT                                              
         SPACE 3                                                                
*              INPUT               R5=A(BEGINNING OF FIELD)                     
*                                  MUST BE SPACE TERMINATED                     
         SPACE 1                                                                
VVEFOUT  DS    0H                                                               
         XC    PBSELEFL,PBSELEFL   RESET FILTER FIELD                           
         LA    R4,PBSELEFL                                                      
         LA    R3,L'PBSELEFL                                                    
         SPACE 1                                                                
VVEFLOOP CLI   0(R5),X'40'         END OF LIST                                  
         BE    VVEFXIT                                                          
         CLI   0(R5),C'*'          WILD CARD. ACCEPTABLE                        
         BE    VVEFNXT                                                          
         CLI   0(R5),C'-'          MINUS FILT.                                  
         BNE   VVEF2                                                            
         LA    R5,1(R5)                                                         
         NI    0(R5),X'FF'-X'40'   TURN OFF X'40' BIT                           
         B     VVEFNXT                                                          
         SPACE 1                                                                
VVEF2    CLI   0(R5),X'C0'         MUST BE A LETTER OR NUMBER                   
         BL    VVEFERR                                                          
         SPACE 1                                                                
VVEFNXT  MVC   0(1,R4),0(R5)                                                    
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R3,VVEFLOOP                                                      
         CLI   0(R5),X'40'         MUST BE LAST ONE                             
         BNE   VVEFERR                                                          
         SPACE 1                                                                
VVEFXIT  B     XIT                                                              
         SPACE 1                                                                
VVEFERR  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*              VALIDATE PUBLICATION                                             
         SPACE 3                                                                
         EJECT                                                                  
*              VALIDATE A START DATE                                            
         SPACE 3                                                                
*              OUTPUT              PBSELSTR USERQSTR                            
         SPACE 1                                                                
VVSTRDAT NETGO NVGETFLD,DMCB                                                    
         BNZ   VVSTDINP            CK FOR NO INPUT                              
         CLI   PBSELESE,0          IF EST RANGE, DATE REQUIRED                  
         BNE   VVSTDREQ                                                         
         CLI   PBSELEST,0          IF NO INPUT, EST IS REQUIRED                 
         BNZ   VVSTDXIT                                                         
         SPACE 1                                                                
VVSTDREQ MVI   ERROR,DATREQ                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VVSTDINP GOTO1 DATVAL,DMCB,(0,FLD),PBSELSTR  VALIDATE IT                        
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    VVSTDERR                                                         
         MVC   USERQSTR,PBSELSTR                                                
         MVI   PBDONTFD,C'N'       DATES SPECIFICALLY SELECTED                  
*                                  SO NOW NETIO SHOULD FILTER                   
         SPACE 1                                                                
VVSTDXIT B     XIT                                                              
         SPACE 1                                                                
VVSTDERR MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*              VALIDATE AN END DATE                                             
         SPACE 3                                                                
*              PARAMETERS          1  MAX N'DAYS END-START                      
*                                                                               
*              OUTPUT              NBSELEND USERQEND                            
         SPACE 1                                                                
VVENDDAT L     R5,0(R1)        *** SAVE ARGS OF MAX DATE SPREAD                 
         NETGO NVGETFLD,DMCB                                                    
         BNZ   VVENDINP            CK FOR NO INPUT                              
         CLI   PBSELEST,0          IF NO INPUT, EST IS REQUIRED                 
         BNZ   VVEB4SCK                                                         
         MVI   ERROR,DATREQ                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VVENDINP GOTO1 DATVAL,DMCB,(0,FLD),PBSELEND  VALIDATE IT                        
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    VVENDERR                                                         
         MVC   USERQEND,PBSELEND                                                
         MVI   PBDONTFD,C'N'       DATES SPECIFICALLY SELECTED                  
*                                  SO NOW NETIO SHOULD FILTER                   
         SPACE 1                                                                
VVEB4SCK CLC   PBSELSTR,PBSELEND   CK END IS NOT BEFORE START                   
         BNH   VVENDXIT                                                         
         MVI   ERROR,INVEBFRS                                                   
         B     TRAPERR                                                          
         SPACE 1                                                                
VVENDXIT DS    0H                                                               
         LTR   R5,R5               IS THERE A MAX DATE SPREAD                   
         BZ    VVENDXX                                                          
         GOTO1 =V(PERVERT),DMCB,PBSELSTR,PBSELEND,RR=RELO                       
         MVC   HALF,8(R1)                                                       
         LH    R1,HALF                                                          
         CR    R1,R5                                                            
         BNH   VVENDXX                                                          
         MVI   ERROR,INVDTSP     EXCEEDS DATE SPREAD LIMIT                      
         B     TRAPERR                                                          
         SPACE 1                                                                
VVENDXX  B     XIT                                                              
         SPACE 1                                                                
VVENDERR MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*              VALIDATE BILLING START AND END DATES                             
         SPACE 3                                                                
*              OUTPUT              NBBILSTR AND/OR NBBILEND                     
         SPACE 1                                                                
VVSTRBIL NETGO NVGETFLD,DMCB       START DATE                                   
         BZ    XIT                 OPTIONAL FIELD                               
         GOTO1 DATVAL,DMCB,(0,FLD),WORK      VALIDATE IT                        
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    VVSTDERR                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,PBBILSTR)                                
         B     XIT                                                              
         SPACE 3                                                                
VVENDBIL NETGO NVGETFLD,DMCB       END DATE                                     
         BZ    XIT                 OPTIONAL FIELD                               
         GOTO1 DATVAL,DMCB,(0,FLD),WORK      VALIDATE IT                        
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    VVENDERR                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,PBBILEND)                                
         CLI   PBBILSTR,0          IF START DATE WAS REQUESTED                  
         BE    XIT                                                              
         CLC   PBBILSTR,PBBILEND   CHECK END IS NOT BEFORE START                
         BNH   XIT                                                              
         MVI   ERROR,INVEBFRS                                                   
         B     TRAPERR                                                          
         EJECT                                                                  
*              VALIDATE PAYING START AND END DATES                              
         SPACE 3                                                                
*              OUTPUT              NBPAYSTR AND/OR NBPAYEND                     
         SPACE 1                                                                
VVSTRPAY NETGO NVGETFLD,DMCB       START DATE                                   
         BZ    XIT                 OPTIONAL FIELD                               
         GOTO1 DATVAL,DMCB,(0,FLD),WORK      VALIDATE IT                        
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    VVSTDERR                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,PBPAYSTR)                                
         B     XIT                                                              
         SPACE 3                                                                
VVENDPAY NETGO NVGETFLD,DMCB       END DATE                                     
         BZ    XIT                 OPTIONAL FIELD                               
         GOTO1 DATVAL,DMCB,(0,FLD),WORK      VALIDATE IT                        
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    VVENDERR                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,PBPAYEND)                                
         CLI   PBPAYSTR,0          IF START DATE WAS REQUESTED                  
         BE    XT                                                               
         CLC   PBPAYSTR,PBPAYEND   CHECK END IS NOT BEFORE START                
         BNH   XIT                                                              
         MVI   ERROR,INVEBFRS                                                   
         B     TRAPERR                                                          
         EJECT                                                                  
*              VALIDATE A DAY                                                   
         SPACE 3                                                                
*              INPUT               R2=A(FIELD)                                  
*              PARAMETER           1  A(1BYTE FIELD)                            
*                                     X'FF'=ALL OR DAY NUMBER                   
         SPACE 1                                                                
VVDAY    L     R3,0(R1)            SAVE ARG                                     
         IFMVC R3,1,=X'FF'         DEFAULT TO X'FF'                             
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVDYXIT             CK FOR NO INPUT                              
         LKUP  FLD,DAY,WORK        PUT OUTPUT OF LOOKUP IN WORK                 
         LTR   R3,R3               IF OPTIONAL ARG GIVEN                        
         BZ    VVDY2                                                            
         MVC   0(1,R3),WORK        THEN MOVE VALUE OF LOOKUP                    
         SPACE 1                                                                
VVDY2    CLI   WORK,C' '           IF NOT FOUND                                 
         BE    VVDYERR                                                          
         SPACE 1                                                                
VVDYXIT  B     XIT                                                              
         SPACE 1                                                                
VVDYERR  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
DAYLKUP  EQU   *                   DAY LOOKUP TABLE.                            
DAYLEN1  EQU   3                   LENGTH OF KEY VALUE                          
DAYLEN2  EQU   1                   LENGTH OF RETURNED VALUE                     
         DC    CL3'ALL',XL1'FF'                                                 
         DC    CL3'M-F',XL1'00'                                                 
         DC    CL3'MON',XL1'01'                                                 
         DC    CL3'TUE',XL1'02'                                                 
         DC    CL3'WED',XL1'03'                                                 
         DC    CL3'THU',XL1'04'                                                 
         DC    CL3'FRI',XL1'05'                                                 
         DC    CL3'SAT',XL1'06'                                                 
         DC    CL3'SUN',XL1'07'                                                 
         DC    CL3'M-S',XL1'08'                                                 
         DC    CL3'VAR',XL1'09'                                                 
         DC    XL3'FFFFFF',CL1' '      END OF TABLE                             
*                                                                               
         DS    0H                                                               
         EJECT                                                                  
*              ROUTINES TO SET VALUES IN GENERAL CONTROLLER BLOCK               
*              THIS IS TO DO I/O ON SPECIFIED FILES                             
         SPACE 3                                                                
*              INPUT               FILENAME                                     
*                                                                               
*              OUTPUT              LSTATUS  LENGTH OF KEY STATUS FIELD          
*                                  DATADISP DISPLACEMENT TO FIRST ELEM.         
*                                  LKEY     LENGTH OF KEY                       
         SPACE 1                                                                
VVSETUNT MVC   LKEY,=H'20'         FOR UNIT RECORDS                             
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'                                                  
         B     XIT                                                              
         SPACE 1                                                                
VVSETSPT MVC   LKEY,=H'13'         FOR SPOT RECORDS                             
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         B     XIT                                                              
         SPACE 1                                                                
VVSETSTA MVC   LKEY,=H'17'         FOR STATION RECORDS                          
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'23'                                                  
         B     XIT                                                              
         SPACE 1                                                                
VVSETCT  MVC   LKEY,=H'25'         FOR CONTROL RECORDS                          
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'28'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET INPUT AND NAME FROM TWA                           
         SPACE 3                                                                
*              INPUT               R5=A(SCREEN HEADER)                          
*              OUTPUT              INPUT AT WORK                                
*                                  NAME AT WORK+10                              
         SPACE 3                                                                
FROMTWA  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         ZIC   R1,0(R5)            R5=A(FIELD HEADER)                           
         SH    R1,=H'9'                                                         
         BM    XIT                                                              
         TM    1(R5),X'02'                                                      
         BNO   FROMTWA2                                                         
         SH    R1,=H'8'                                                         
         BM    XIT                                                              
         SPACE 1                                                                
FROMTWA2 CH    R1,=H'40'                                                        
         BH    XIT                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R5)                                                    
         SPACE 1                                                                
         ZIC   R0,0(R5)            BUMP TO NEXT FIELD ON SCREEN                 
         AR    R5,R0                                                            
         ZIC   R1,0(R5)            R5=A(FIELD HEADER)                           
         SH    R1,=H'9'                                                         
         BM    XIT                                                              
         TM    1(R5),X'02'                                                      
         BNO   FROMTWA4                                                         
         SH    R1,=H'8'                                                         
         BM    XIT                                                              
         SPACE 1                                                                
FROMTWA4 CH    R1,=H'40'                                                        
         BH    XIT                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+10(0),8(R5)                                                 
         OC    WORK,SPACES                                                      
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZE TO RUN DRIVER                                         
         SPACE 3                                                                
*                                  LOADS PHASES                                 
*                                  SETS GLOBAL ADDRESSES                        
         SPACE 1                                                                
VVDRINIT GOTO1 CALLOV,DMCB,X'B1000000',0,0  LOAD T320B1(GLOBAL STORAGE)         
         L     R4,DMCB                     FOR DRIVER                           
         ST    R4,PDGLOBAL                                                      
         USING GLOBALD,R4                                                       
****     LR    R1,R4                                                            
****     AH    R1,DMCB+10           NEED TO UPDATE AOVERLAY                     
****     LA    R1,8(R1)             TO POINT TO NEXT FREE LOCATION              
****     SRL   R1,3                (MAKE SURE ITS ON DOUBLE WORD BOUND)         
****     SLL   R1,3                                                             
****     ST    R1,AOVERLAY                                                      
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A40'   LOAD T00A40 (DRIVER)                 
         L     R2,DMCB                                                          
         ST    R2,PDDRIVER                                                      
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A41'   LOAD T00A41 (PRINT DRIVER)           
         L     R2,DMCB                                                          
         ST    R2,GLASYSDR                                                      
         ST    RC,GLAWORKD                                                      
         MVI   GLTWORKD,GLTSPOOL                                                
         CLI   PDDOWNL,C'Y'         OPTION TO DOWNLOAD                          
         BNE   DRI2                                                             
         MVI   GLDOWNLD,X'80'                                                   
         SPACE 1                                                                
DRI2     B     XIT                                                              
         DROP  R4                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS & LTORG                                                
         SPACE 3                                                                
RELO     DS    A                                                                
EFFEFFS  DC    X'FFFFFFFF'         USED BY LKUP MACRO                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              NEGENINCLS                                                       
*              PAGYREC                                                          
*              PCLTREC                                                          
*              PDIVREC                                                          
*              PREGREC                                                          
*              PDSTREC                                                          
*              PPRDREC                                                          
*              PESTREC                                                          
*              PUBREC                                                           
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE PRNTINCLS                                                      
       ++INCLUDE PAGYREC                                                        
       ++INCLUDE PCLTREC                                                        
       ++INCLUDE PDIVREC                                                        
       ++INCLUDE PREGREC                                                        
       ++INCLUDE PDSTREC                                                        
       ++INCLUDE PPRDREC                                                        
       ++INCLUDE PESTREC                                                        
       ++INCLUDE PUBREC                                                         
TWADSEC  DSECT                                                                  
       ++INCLUDE DDGENCTWAD                                                     
       ++INCLUDE NEGENNBUFF                                                     
       ++INCLUDE NEGENTBUFF                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PRPRNTGEN 05/01/02'                                      
         END                                                                    
