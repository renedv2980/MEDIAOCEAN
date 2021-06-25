*          DATA SET ACPRO24    AT LEVEL 016 AS OF 10/07/15                      
*PHASE T60B24A                                                                  
         TITLE 'T60B24 - PRODUCTION OPTION REPORT'                              
T60B24   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B24,RA,RR=R2                                                
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
         MVI   LEVEL,C'A'          DEFAULT IS AGENCY                            
         MVI   REQOG,X'00'                                                      
         XC    REQOFF,REQOFF                                                    
         MVC   REQCLI,SPACES                                                    
         MVC   REQPRO,SPACES                                                    
         MVC   REQJOB,SPACES                                                    
         MVI   OPTION,C'Y'                                                      
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR             NEED KEY LENGTHS                             
*                                                                               
         LA    R2,PROOGRH         OFFICE GROUP                                  
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         GOTO1 VALOG                                                            
         MVC   REQOG,EFFOFG                                                     
         MVI   LEVEL,C'G'          NOTE OFFICE GROUP                            
*                                                                               
VKEY2    LA    R2,PROOFFH          OFFICE                                       
         CLI   5(R2),0                                                          
         BE    VKEY4                                                            
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALOFF                                                           
         MVC   REQOFF,EFFOFFC                                                   
         MVI   LEVEL,C'O'          NOTE OFFICE LEVEL                            
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
         MVI   LEVEL,C'C'          NOTE CLIENT LEVEL                            
*                                                                               
VKEY6    LA    R2,PROPROH          PRODUCT OPTIONAL, UNLESS 'SOON'              
         CLI   5(R2),0                                                          
         BE    VKEY8                                                            
         LA    R2,PROCLIH                                                       
         MVI   ERROR,NEEDCLI       NEED CLIENT IF INPUT                         
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         LA    R2,PROPROH                                                       
         MVI   LEVEL,C'P'                                                       
         ZIC   R3,LCLIPRO                                                       
         GOTO1 VALPROD                                                          
         MVC   REQPRO,PRODCODE                                                  
*                                                                               
VKEY8    TM    WHEN,X'20'          ARE WE 'SOONING' ?                           
         BZ    VKEY10              NO                                           
         LA    R2,PROCLIH          YES, MAKE SURE WE HAVE CLIENT/PROD           
         CLI   5(R2),0                                                          
         BE    SOONER                                                           
         LA    R2,PROPROH                                                       
         CLI   5(R2),0                                                          
         BE    SOONER                                                           
*                                                                               
VKEY10   LA    R2,PROJOBH          JOB OPTIONAL                                 
         CLI   5(R2),0                                                          
         BE    VKEY12                                                           
         LA    R2,PROPROH                                                       
         MVI   ERROR,NEEDPRO       NEED PRODUCT IF INPUT                        
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         LA    R2,PROJOBH                                                       
         MVI   LEVEL,C'J'                                                       
         GOTO1 VALJOB                                                           
         MVC   REQJOB,JOBNUM                                                    
         MVI   ERROR,JOBISDFT      CAN'T REQUEST A DRAFT JOB                    
         L     R6,AIO                                                           
         TM    ACTRSTAT-ACTRECD(R6),ACTSDRFT                                    
         BO    ERREND                                                           
*                                                                               
VKEY12   MVI   RULEOPT,C'N'        NO TO COMMISSION RULES ONLY                  
         LA    R2,PRORULEH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY14                                                           
         MVC   RULEOPT,8(R2)       EXTRACT OPTION                               
         MVI   ERROR,INVALID                                                    
         CLI   RULEOPT,C'Y'                                                     
         BE    VKEY14                                                           
         CLI   RULEOPT,C'N'                                                     
         BNE   ERREND                                                           
*                                                                               
VKEY14   B     XIT                                                              
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         BAS   RE,DORAW                                                         
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL RAW OPTION REPORT                                        
         SPACE 3                                                                
DORAW    NTR1                                                                   
         LA    R4,KEY                                                           
         USING ACOPKEY,R4                                                       
DR10     XC    ACOPKEY,ACOPKEY                                                  
         MVC   ACOPRTYP(2),=X'2C20'                                             
         MVC   ACOPCUL,CUL                                                      
         MVC   SAVEH4,=CL20'AGENCY RECORDS'                                     
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     DR14                                                             
*                                                                               
DR12     GOTO1 SEQ                                                              
*                                                                               
DR14     CLC   ACOPKEY(ACOPMG-ACOPKEY),KEYSAVE                                  
         BNE   DR20                                                             
         BAS   RE,PROCRAW                                                       
         B     DR12                                                             
*                                                                               
DR20     XC    ACOPKEY,ACOPKEY                                                  
         MVC   ACOPRTYP(2),=X'2C20'                                             
         MVC   ACOPCUL,CUL                                                      
         MVC   ACOPOG,EFFOFG                                                    
         CLI   REQOG,X'00'         TEST OF OFFICE GROUP INPUT                   
         BE    *+10                                                             
         MVC   ACOPOG,REQOG        YES-SET IT IN KEY                            
         CLI   ACOPOG,0                                                         
         BNE   *+8                                                              
         MVI   ACOPOG,C'A'                                                      
         MVC   SAVEH4,=CL20'OFFICE GROUP RECORDS'                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     DR24                                                             
*                                                                               
DR22     GOTO1 SEQ                                                              
*                                                                               
DR24     CLC   ACOPKEY(ACOPOG-ACOPKEY),KEYSAVE                                  
         BNE   DR30                                                             
         CLI   REQOG,X'00'         TEST OFFICE GROUP REQUESTED                  
         BE    DR25                NO                                           
         CLC   ACOPKEY(ACOPOFC-ACOPKEY),KEYSAVE  TEST FOR MATCH ON OG           
         BE    DR26                                                             
         B     DR30                NO-GO ON TO OFFICES                          
*                                                                               
DR25     CLI   EFFOFG,0                                                         
         BE    DR26                                                             
         CLC   EFFOFG,ACOPOG                                                    
         BNE   DR30                                                             
*                                                                               
DR26     BAS   RE,PROCRAW                                                       
         B     DR22                                                             
*                                                                               
DR30     XC    ACOPKEY,ACOPKEY                                                  
         MVC   ACOPRTYP(2),=X'2C20'                                             
         MVC   ACOPCUL,CUL                                                      
         MVC   ACOPOFC,EFFOFFC                                                  
         OC    REQOFF,REQOFF       TEST OFFICE REQUESTED                        
         BZ    *+10                                                             
         MVC   ACOPOFC,REQOFF      YES-SET IT IN KEY                            
         CLC   ACOPOFC,SPACES                                                   
         BH    *+8                                                              
         MVI   ACOPOFC,C'A'                                                     
         MVC   SAVEH4,=CL20'OFFICE RECORDS'                                     
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     DR34                                                             
*                                                                               
DR32     GOTO1 SEQ                                                              
*                                                                               
DR34     CLC   ACOPKEY(ACOPOFC-ACOPKEY),KEYSAVE  TEST MATCH UP TO OFC           
         BNE   DR40                                                             
         OC    REQOFF,REQOFF       TEST OFFICE REQUESTED                        
         BZ    DR35                NO                                           
         CLC   ACOPKEY(ACOPCLI-ACOPKEY),KEYSAVE  TEST MATCH ON OFFICE           
         BE    DR36                                                             
         B     DR40                                                             
*                                                                               
DR35     CLI   LEVEL,C'G'                                                       
         BNE   DR35A                                                            
         MVC   EFFOFFC,ACOPOFC                                                  
         MVC   SAVELACC,ACOPKEY    SAVE THE KEY                                 
         GOTO1 SETOG                                                            
         MVC   ACOPKEY,SAVELACC    RESTORE KEY AND RE-READ                      
         GOTO1 READ                                                             
         CLC   EFFOFG,REQOG                                                     
         BNE   DR32                                                             
         B     DR36                                                             
*                                                                               
DR35A    CLC   EFFOFFC,SPACES                                                   
         BNH   DR36                                                             
         CLC   EFFOFFC,ACOPOFC                                                  
         BNE   DR40                                                             
*                                                                               
DR36     BAS   RE,PROCRAW                                                       
         B     DR32                                                             
*                                                                               
DR40     XC    ACOPKEY,ACOPKEY                                                  
         MVC   ACOPRTYP(2),=X'2C20'                                             
         MVC   ACOPCUL,CUL                                                      
         MVC   SAVEH4,=CL20'CLIENT RECORDS'                                     
         MVC   ACOPCLI,REQCLI                                                   
         CLI   REQPRO,C' '         TEST PRODUCT INPUT                           
         BE    *+10                                                             
         MVC   ACOPPRO,REQPRO                                                   
         CLI   REQJOB,C' '                                                      
         BE    *+10                                                             
         MVC   ACOPJOB,REQJOB                                                   
         CLI   REQCLI,C' '                                                      
         BNE   *+8                                                              
         MVI   ACOPCLI,C'A'                                                     
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     DR44                                                             
*                                                                               
DR42     GOTO1 SEQ                                                              
*                                                                               
DR44     CLC   ACOPKEY(ACOPCLI-ACOPKEY),KEYSAVE                                 
         BNE   XIT                                                              
         CLI   REQCLI,C' '                                                      
         BE    DR45                                                             
         CLC   ACOPKEY(ACOPPRO-ACOPKEY),KEYSAVE                                 
         BNE   XIT                                                              
         CLI   REQPRO,C' '                                                      
         BE    DR45                                                             
         CLC   ACOPKEY(ACOPJOB-ACOPKEY),KEYSAVE                                 
         BNE   XIT                                                              
         CLI   REQJOB,C' '                                                      
         BE    DR45                                                             
         CLC   ACOPKEY(ACOPMG-ACOPKEY),KEYSAVE                                  
         BNE   XIT                                                              
*                                                                               
DR45     MVC   SAVELACC,ACOPKEY    SAVE KEY OF OPTION RECORD                    
         LA    R4,SAVELACC                                                      
         BAS   RE,READCLI          SET UP CLIENT                                
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE      DOES CLIENT EXIST ?                          
         BNE   DR48                NO, SKIP IT                                  
         GOTO1 SETCLI                                                           
         GOTO1 SETOG                                                            
*                                                                               
         CLI   ACOPPRO,X'41'       ANY PRODUCT?                                 
         BL    DR47                NO                                           
         BAS   RE,READCLI          YES                                          
         MVC   KEY+L'ACOPCLI(L'ACOPPRO),ACOPPRO                                 
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE      DOES PRODUCT EXIST?                          
         BNE   DR48                NO, SKIP IT                                  
         GOTO1 SETPROD                                                          
         GOTO1 SETOG                                                            
*                                                                               
         CLI   ACOPJOB,X'41'       ANY JOB?                                     
         BL    DR47                NO                                           
         BAS   RE,READCLI          YES                                          
         MVC   KEY+L'ACOPCLI(L'ACOPPRO),ACOPPRO                                 
         MVC   KEY+9(L'ACOPJOB),ACOPJOB                                         
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE     DOES JOB EXIST?                              
         BNE   DR48                NO, SKIP IT                                  
*                                                                               
         MVI   ERROR,JOBISDFT      CAN'T REQUEST A DRAFT JOB                    
         L     R4,AIO                                                           
         TM    ACTRSTAT-ACTRECD(R4),ACTSDRFT                                    
         BO    DR48                                                             
*                                                                               
DR47     LA    R4,KEY                                                           
         MVC   ACOPKEY,SAVELACC                                                 
         GOTO1 READ                                                             
         BAS   RE,CHKOGR           VALIDATE OFFICE GROUP/OFFICE                 
         BNE   DR42                                                             
         BAS   RE,CHKOFF                                                        
         BNE   *+8                                                              
         BAS   RE,PROCRAW                                                       
         B     DR42                                                             
         SPACE 3                                                                
DR48     LA    R4,KEY              IF ERROR, COME HERE AND RESET KEY            
         MVC   ACOPKEY,SAVELACC                                                 
         GOTO1 READ                                                             
         B     DR42                                                             
         SPACE 3                                                                
READCLI  MVC   KEY,SPACES                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(L'ACOPCLI),ACOPCLI                                         
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO COMPARE OFFICE GROUP OR OFFICE                        
CHKOFF   OC    REQOFF,REQOFF                                                    
         BZ    CHKOFFY                                                          
         CLC   REQOFF,EFFOFFC                                                   
         BE    CHKOFFY                                                          
         TM    REQOFF,X'40'                                                     
         BO    CHKOFFN                                                          
         MVC   HALF,REQOFF                                                      
         OI    HALF,X'40'                                                       
         CLC   HALF,EFFOFFC                                                     
         BNE   CHKOFFY                                                          
*                                                                               
CHKOFFN  LTR   RB,RB                                                            
         B     CHKOFFX                                                          
*                                                                               
CHKOFFY  CR    RB,RB                                                            
*                                                                               
CHKOFFX  BR    RE                                                               
         SPACE 3                                                                
CHKOGR   OC    REQOG,REQOG                                                      
         BZ    CHKOFFY                                                          
         CLC   REQOG,EFFOFG                                                     
         BE    CHKOFFY                                                          
         B     CHKOFFN                                                          
         EJECT                                                                  
*              ROUTINE TO PROCESS A RAW OPTION RECORD                           
         SPACE 3                                                                
PROCRAW  NTR1                                                                   
         USING ACOPKEY,R4                                                       
         MVC   P,SPACES            CLEAR THE PRINT LINE                         
         MVC   P+03(1),ACOPOG                                                   
         MVC   P+09(2),ACOPOFC                                                  
         MVC   P+13(6),ACOPCLI                                                  
         MVC   P+20(6),ACOPPRO                                                  
         MVC   P+28(6),ACOPJOB                                                  
         CLI   ACOPMG,X'FF'                                                     
         BE    *+10                                                             
         MVC   P+37(1),ACOPMG                                                   
         MVC   P+43(1),ACOPMED                                                  
         CLI   ACOPWG,X'FF'                                                     
         BE    *+10                                                             
         MVC   P+48(1),ACOPWG                                                   
         MVC   P+53(2),ACOPWORK                                                 
         MVI   ANYOPT,C'N'                                                      
         MVI   ELCODE,X'A4'                                                     
         BAS   RE,GETELIO                                                       
         B     PRAW4                                                            
*                                                                               
PRAW2    BAS   RE,NEXTEL                                                        
*                                                                               
PRAW4    BE    PRAW6                                                            
         CLI   ANYOPT,C'Y'                                                      
         BNE   XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         USING ACOPD,R6                                                         
PRAW6    CLI   RULEOPT,C'Y'        TEST RULES ONLY                              
         BNE   *+12                                                             
         CLI   ACOPNUM,OPNCOM      TEST FOR COMMISSION RULE                     
         BNE   PRAW2               SKIP ALL THE OTHER RULES                     
*                                                                               
PRAW8    MVI   ANYOPT,C'Y'                                                      
         LA    R2,P+89-8           SETTING                                      
         MVC   P+89(24),SPACES                                                  
         GOTO1 VDISOPT,DMCB,(0,(R6)),(R2)                                       
         L     RE,AOPTENT          R5=A(OPTION ENTRY)                           
         USING OPTBD,RE                                                         
         MVC   P+57(3),OPTBSHRT                                                 
         MVI   P+60,C'-'                                                        
         MVC   P+61(L'OPTBDESC),OPTBDESC                                        
         DROP  RE                                                               
         MVC   P+114(8),ACOPPERS   ACTIVITY                                     
         GOTO1 DATCON,DMCB,(1,ACOPLAST),(8,P+123)                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRAW2                                                            
         EJECT                                                                  
*              HEAD HOOK FOR RAW OPTIONS                                        
         SPACE 3                                                                
HOOK     NTR1                                                                   
         LA    R3,H1+48            DEAL WITH HEADING                            
         MVC   0(34,R3),=CL34'OPTION DETAIL REPORT'                             
         CLI   PROTITH+5,0                                                      
         BE    HOOK2                                                            
         LA    R2,PROTITH                                                       
         GOTO1 ANY                                                              
         MVC   0(34,R3),SPACES                                                  
         MVC   0(32,R3),WORK                                                    
*                                                                               
HOOK2    GOTO1 CENTER,DMCB,(R3),34                                              
         GOTO1 UNDERLIN,DMCB,(34,(R3)),(X'BF',132(R3))                          
*                                                                               
         MVC   H4+1(20),SAVEH4                                                  
         MVC   H8+1(5),=C'OFFCE'                                                
         MVC   H9+1(5),=C'GROUP'                                                
         MVC   H8+7(5),=C'OFFCE'                                                
         MVC   H8+13(6),=C'CLIENT'                                              
         MVC   H8+20(7),=C'PRODUCT'                                             
         MVC   H8+28(6),=C' JOB  '                                              
         MVC   H9+28(6),=C'NUMBER'                                              
         MVC   H8+35(5),=C'MEDIA'                                               
         MVC   H9+35(5),=C'GROUP'                                               
         MVC   H8+41(5),=C'MEDIA'                                               
         MVC   H8+47(4),=C'WORK'                                                
         MVC   H9+47(4),=C'GRP.'                                                
         MVC   H8+52(4),=C'WORK'                                                
         MVC   H9+52(4),=C'CODE'                                                
         MVC   H8+57(6),=C'OPTION'                                              
         MVC   H8+89(7),=C'SETTING'                                             
         MVC   H8+114(14),=C' LAST ACTIVITY'                                    
         MVC   H9+114(14),=C'  BY       ON '                                    
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         CLI   BOXOPT,C'N'                                                      
         BE    XIT                                                              
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+6,C'C'                                                   
         MVI   BOXCOLS+12,C'C'                                                  
         MVI   BOXCOLS+19,C'C'                                                  
         MVI   BOXCOLS+27,C'C'                                                  
         MVI   BOXCOLS+34,C'C'                                                  
         MVI   BOXCOLS+40,C'C'                                                  
         MVI   BOXCOLS+46,C'C'                                                  
         MVI   BOXCOLS+51,C'C'                                                  
         MVI   BOXCOLS+56,C'C'                                                  
         MVI   BOXCOLS+88,C'C'                                                  
         MVI   BOXCOLS+113,C'C'                                                 
         MVI   BOXCOLS+131,C'R'                                                 
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         B     XIT                                                              
         EJECT                                                                  
SOONER   MVI   ERROR,NEEDSOON                                                   
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              SPECS FOR HEADINGS ETC                                           
         SPACE 3                                                                
MYSPECS  DS    0F                                                               
         SSPEC H1,2,CREATED        SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
SAVELACC DS    CL42                                                             
ANYOPT   DS    CL1                                                              
SAVEH4   DS    CL20                                                             
LEVEL    DS    C                                                                
RULEOPT  DS    C                                                                
REQOG    DS    X                                                                
REQOFF   DS    XL2                                                              
REQCLI   DS    CL6                                                              
REQPRO   DS    CL6                                                              
REQJOB   DS    CL6                                                              
         EJECT                                                                  
         LTORG                                                                  
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
       ++INCLUDE ACPROD4D                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACPRO24   10/07/15'                                      
         END                                                                    
