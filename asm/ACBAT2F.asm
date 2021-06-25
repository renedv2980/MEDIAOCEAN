*          DATA SET ACBAT2F    AT LEVEL 008 AS OF 06/14/18                      
*PHASE T61B2FA                                                                  
*INCLUDE RIGHT                                                                  
         TITLE 'T61B2F - ESTIMATED PRODUCTION'                                  
T61B2F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**BAT2F,R7,CLEAR=YES,RR=R2                          
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         L     R1,=V(RIGHT)                                                     
         AR    R1,R2                                                            
         ST    R1,RIGHT                                                         
*                                                                               
         CLI   MODE,0              MODE=0 - GO TO ORDER OVERLAY                 
         BE    PIC002                                                           
         CLC   ORDNO,PROORD        SAME ORDER - CARRY ON                        
         BE    PIC004                                                           
         MVI   MODE,0              OTHERWISE RE-READ - GOTO 07 OVERLAY          
PIC002   LA    R3,7                                                             
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,,(R9),WRKC1                                            
         LA    R2,PRODOCH                                                       
         CLI   MODE,1              EXIT WITH ORDER DISPLAYED                    
         BNE   *+12                                                             
         NI    PROORDH+6,X'BF'     SWITCH OFF CURSOR                            
         B     EXIT                                                             
         CLI   ERRNUM,X'FF'                                                     
         BE    PIC004              CARRY ON IF NO ORDER AND NO ERROR            
         B     CURSIT                                                           
         EJECT                                                                  
*------------------------------------------------------------                   
*        SCAN THE INPUT                                                         
*------------------------------------------------------------                   
*                                                                               
PIC004   DS    0H                                                               
         LA    R2,PRODOCH                                                       
         MVC   REFSAVE,SPACES                                                   
         MVC   DOCSAVE,SPACES                                                   
         GOTO1 SCANNER,DMCB,(R2),(2,WORK)                                       
         MVI   ERRNUM,2                                                         
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   WORK,6                                                           
         BH    ERROR                                                            
         MVC   DOCSAVE,WORK+12                                                  
         CLC   DOCSAVE,SPACES      INPUT STARTED WITH A COMMA                   
         BE    ERROR                                                            
         MVC   DOCLEN,WORK                                                      
         CLI   4(R1),1                                                          
         BE    PIC01               ONE ENTRY                                    
         CLI   WORK+32,6                                                        
         BH    ERROR                                                            
         MVC   REFSAVE,WORK+32+12                                               
         CLC   REFSAVE,SPACES      INPUT ENDED WITH A COMMA                     
         BE    ERROR                                                            
PIC01    DS    0H                                                               
         LA    R2,PRODATH                                                       
         MVI   ERRNUM,13                                                        
         CLI   PRODATH+5,0                                                      
         BNE   PIC02                                                            
         BAS   RE,GETODAY                                                       
         B     PIC04                                                            
PIC02    DS    0H                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
PIC04    DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,SAVEDATE)                                
         GOTO1 DATECHK,DMCB,SAVEDATE                                            
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         XC    SAVNUM,SAVNUM                                                    
         EJECT                                                                  
*------------------------------------------------------------                   
*        VALIDATE ALL INPUT ACCOUNTS & SAVE THEIR NAMES/NUMBERS                 
*------------------------------------------------------------                   
*                                                                               
         MVC   PROTOT,SPACES                                                    
         OI    PROTOTH+6,X'80'                                                  
         SR    R6,R6               NO PROFILES                                  
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         LA    R2,PROSBPH          SUPPLIER/ BANK A/C / PETTY CASH              
         BAS   RE,ANY                                                           
         MVI   ERRNUM,17                                                        
         SR    R3,R3                                                            
         IC    R3,PROSBPH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),PROSBP                                                  
         LA    R5,COMPEL                                                        
         USING ACCOMPD,R5                                                       
         MVC   KEY+1(2),ACMPBANK                                                
         CLI   KEY+3,C'*'          DEAL WITH *ULACCT INPUT                      
         BNE   PIC3                                                             
         MVC   KEY+1(20),SPACES                                                 
         MVC   KEY+1(14),PROSBP+1                                               
         OC    KEY+1(14),SPACES                                                 
         CLC   ACMPPETY,KEY+1                                                   
         BE    PIC3                                                             
         CLC   =C'SA',KEY+1        ALLOW FOR BILLABLE ADVANCES                  
         BE    PIC3                                                             
         B     ERROR                                                            
*                                                                               
PIC3     BAS   RE,GETACC                                                        
         MVC   SVSTAT,ACCTSTAT                                                  
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'10'      ACCOUNT IS LOCKED                            
         BO    ERROR                                                            
         MVC   SBPNUM,ACCTNUM                                                   
         MVC   SBPNAME,ACCTNAME                                                 
         CLC   SBPNUM+1(2),=C'SA'  DEAL WITH OPEN ITEM ADVANCES                 
         BNE   PIC3A                                                            
         LA    R2,PRODOCH                                                       
         MVI   ERRNUM,0                                                         
         BAS   RE,CHECKSA                                                       
         CLI   ERRNUM,0                                                         
         BNE   ERROR                                                            
PIC3A    DS    0H                                                               
         TM    PROSBPH+4,X'20'                                                  
         BO    PIC4                                                             
         OI    PROSBPH+4,X'20'                                                  
         MVC   PROSBPN,ACCTNAME                                                 
         OI    PROSBPNH+6,X'80'                                                 
         EJECT                                                                  
PIC4     LA    R2,PROCLIH          CLIENT/PRODUCT/JOB                           
         BAS   RE,ANY                                                           
         MVI   ERRNUM,14                                                        
         MVC   KEY+1(41),SPACES                                                 
         MVC   KEY+1(2),ACMPJOB    U/L FOR CLI/PRO/JOBS                         
         TM    PROCLIH+4,X'20'                                                  
         BO    PIC6                                                             
         NI    PROPROH+4,X'DF'                                                  
         NI    PROJOBH+4,X'DF'                                                  
         MVC   PROCLIN,SPACES                                                   
         OI    PROCLINH+6,X'80'                                                 
         MVC   PROPRON,SPACES                                                   
         OI    PROPRONH+6,X'80'                                                 
         MVC   PROJOBN,SPACES                                                   
         OI    PROJOBNH+6,X'80'                                                 
PIC6     IC    R3,PROCLIH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),PROCLI                                                  
*                                                                               
         XC    CLIPROF,CLIPROF                                                  
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,28                                                        
         TM    ACCTSTAT,X'10'      ACCOUNT IS LOCKED                            
         BO    ERROR                                                            
         MVC   PROCLIN,ACCTNAME                                                 
         OI    PROCLINH+6,X'80'                                                 
         OI    PROCLIH+4,X'20'                                                  
*                                                                               
PIC7     DS    0H                                                               
         LA    R6,CLIPROF                                                       
         OC    TWAACCS,TWAACCS     TEST LIMIT ACCESS                            
         BZ    PIC8                                                             
         CLC   TWAACCS,SPACES                                                   
         BE    PIC8                                                             
         USING ACPROFD,R6                                                       
         MVI   ERRNUM,55                                                        
         CLI   TWAACCS,C'*'        OFFICE -CODE                                 
         BE    PIC8                                                             
         CLI   TWAACCS,C'$'        AND LIST WILL BE CHECK BY BASE               
         BE    PIC8                                                             
         CLC   TWAACCS(2),KEY+3    2 CHARACTER CLIENT MATCH                     
         BNE   ERROR                                                            
         DROP  R6                                                               
*                                                                               
PIC8     MVC   CLINUM,KEY                                                       
         MVC   CLINAME,PROCLIN                                                  
         LA    R4,KEY+3                                                         
         IC    R3,CLILNGTH         LEVA LENGTH                                  
         AR    R4,R3               POINT TO PRODUCT POSITION                    
         LA    R2,PROPROH                                                       
         MVI   ERRNUM,15                                                        
         BAS   RE,ANY                                                           
         TM    PROPROH+4,X'20'                                                  
         BO    PIC10                                                            
         NI    PROJOBH+4,X'DF'                                                  
         MVC   PROPRON,SPACES                                                   
         OI    PROPRONH+6,X'80'                                                 
         MVC   PROJOBN,SPACES                                                   
         OI    PROJOBNH+6,X'80'                                                 
*                                                                               
PIC10    IC    R3,PROPROH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PROPRO                                                   
         LA    R4,KEY+3                                                         
         IC    R3,PRDLNGTH         LEVB LENGTH                                  
         AR    R4,R3               READY FOR JOB                                
         XC    PRODPROF,PRODPROF                                                
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,28                                                        
         TM    ACCTSTAT,X'10'      ACCOUNT IS LOCKED                            
         BO    ERROR                                                            
         MVC   PROPRON,ACCTNAME                                                 
         OI    PROPRONH+6,X'80'                                                 
         OI    PROPROH+4,X'20'                                                  
*                                                                               
PIC12    MVC   PRODNAM,PROPRON                                                  
         OC    PRODNAM,SPACES                                                   
         MVC   PRODNUM,KEY                                                      
         MVI   ERRNUM,16                                                        
         LA    R2,PROJOBH                                                       
         BAS   RE,ANY                                                           
         IC    R3,PROJOBH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PROJOB                                                   
         XC    JOBPROF,JOBPROF                                                  
         LA    R6,JOBPROF                                                       
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    ERROR                                                            
         MVI   ERRNUM,28                                                        
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    ERROR                                                            
         MVI   ERRNUM,23                                                        
         TM    ACCTSTAT,X'20'      CLOSED JOB?                                  
         BO    ERROR                                                            
         MVC   PROJOBN,ACCTNAME                                                 
         TM    PROJOBH+4,X'20'                                                  
         BO    PIC13                                                            
         OI    PROJOBNH+6,X'80'                                                 
         OI    PROJOBH+4,X'20'                                                  
*                                                                               
PIC13    MVC   JOBEXNUM,KEY                                                     
         MVC   JOBEXNAM,PROJOBN    NAME FROM SCREEN                             
         BAS   RE,PROFMERG         PICK UP ANALYSIS FILTER                      
         LA    R4,PROFILE          FROM A PROFILE                               
         USING ACPROFD,R4                                                       
         MVC   FILT,ACPROFFC                                                    
         LA    R4,JOBPROF                                                       
         CLC   ACPROFFC,SPACES                                                  
         BNH   PIC14                                                            
         MVC   FILT,ACPROFFC                                                    
         DROP  R4                                                               
*                                                                               
PIC14    DS    0H                                                               
         LA    R4,JOBEXNUM                                                      
         GOTO1 ASETJOB,DMCB,(R4)                                                
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    PIC15               NO                                           
         LA    R2,PROJOBH          YES, STOP THEM                               
         MVI   ERRNUM,45                                                        
         B     ERROR                                                            
*                                                                               
PIC15    SR    R6,R6               NO MORE PROFILES REQUIRED                    
         MVI   ERRNUM,1                                                         
         LA    R2,PROWRKH          ONE W/C FIELD MUST BE INPUT                  
         CLI   PRONCWKH+5,0                                                     
         BNE   *+12                                                             
         CLI   PROWRKH+5,0                                                      
         BE    ERROR                                                            
         ZAP   CSHTOT,=P'0'                                                     
*                                  VALIDATE COMMISSIONABLE FIELDS               
         LA    R3,PROWRKH                                                       
         LA    R4,WRKC1                                                         
         BAS   RE,VALWAM                                                        
         BNE   ERROR                                                            
*                                  VALIDATE NON-COMMISSIONABLE FIELDS           
         LA    R3,PRONCWKH                                                      
         LA    R4,WRKNC1                                                        
         BAS   RE,VALWAM                                                        
         BNE   ERROR                                                            
         EJECT                                                                  
         XC    VENDNUM,VENDNUM     CONSULTANT POSTING                           
         MVC   PROSUPN,SPACES                                                   
         OI    PROSUPNH+6,X'80'                                                 
         LA    R2,PROSUPH                                                       
*                                                                               
         TM    COMPSTA2,X'04'                                                   
         BZ    PIC20                                                            
         BAS   RE,ANY                                                           
         B     PIC22                                                            
*                                                                               
PIC20    CLI   5(R2),0                                                          
         BE    PIC42                                                            
*                                                                               
PIC22    MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+3(12),SPACES                                                 
         MVC   KEY+1(2),ACMPSUPP                                                
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),PROSUP                                                  
*                                                                               
         CLC   KEY+3(3),=C'*SW'                                                 
         BE    PIC24                                                            
         CLC   KEY+3(3),=C'*SX'                                                 
         BE    PIC24                                                            
         CLC   KEY+3(3),=C'*SV'                                                 
         BE    PIC24                                                            
         CLC   KEY+3(3),=C'*SY'                                                 
         BE    PIC24                                                            
         B     PIC26                                                            
*                                                                               
PIC24    MVC   KEY+1(20),SPACES                                                 
         MVC   KEY+1(14),PROSUP+1                                               
         OC    KEY+1(14),SPACES                                                 
*                                                                               
PIC26    BAS   RE,GETACC                                                        
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'10'                                                   
         BO    ERROR                                                            
         MVC   SVSTAT,ACCTSTAT                                                  
         MVC   VENDNUM,ACCTNUM                                                  
         MVC   VENDNAME,ACCTNAME                                                
         MVC   PROSUPN,ACCTNAME                                                 
         OI    PROSUPNH+6,X'80'                                                 
*                                                                               
         TM    SVSTAT,X'04'                                                     
         BZ    PIC42                                                            
         LA    R2,PROSUPH                                                       
         MVC   KEY+1(2),=C'2C'                                                  
         BAS   RE,GETACC           MUST HAVE SAME CODE IN '2C'                  
         MVC   V2CNUM,ACCTNUM                                                   
         MVC   V2CNAM,ACCTNAME                                                  
         MVC   KEY+1(14),=CL14'27999'                                           
         BAS   RE,GETACC                                                        
         MVC   CONTROL,ACCTNUM                                                  
         MVC   CONTROLN,ACCTNAME                                                
         EJECT                                                                  
*-------------------------------------------------------------                  
*        BUILD 64 ELEMENT                                                       
*-------------------------------------------------------------                  
*                                                                               
PIC42    LA    R8,IOAREA+2                                                      
         SR    R3,R3                                                            
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSREF,DOCSAVE                                                  
         MVC   DLDSDATE,SAVEDATE                                                
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
*                                                                               
PIC42C   DS    0H                                                               
         XC    DLDSSTAT+1(6),DLDSSTAT+1                                         
         LA    R2,PRONARH                                                       
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         MVC   0(L'EPCOMMNT,R3),EPCOMMNT    STANDARD COMMENT                    
         LA    R3,L'EPCOMMNT(R3)                                                
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         BAS   RE,NARRSCAN                                                      
         LA    R5,DLDSNARR                                                      
         SR    R5,R8               R5 = ELEMENT - NARRATIVE                     
         AR    R5,R6               R6 = L'NARRATIVE                             
         LA    R3,L'EPCOMMNT                                                    
         LA    R5,1(R3,R5)         ADD LENGTH OF STANDARD COMMENT               
         STH   R5,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         OC    CLINAME,SPACES                                                   
         OC    JOBEXNAM,SPACES                                                  
         B     PIC42F                                                           
*                                                                               
EPCOMMNT DC    C'**ESTIMATED PRODUCTION'                                        
         EJECT                                                                  
*-------------------------------------------------------------                  
*        BUILD 69 & 6A MAIN ACCOUNTING ELEMENTS                                 
*-------------------------------------------------------------                  
*                                                                               
PIC42F   AR    R8,R5                                                            
         XC    ORDNOEL,ORDNOEL                                                  
         XC    OTHEREL,OTHEREL                                                  
         XC    WORK69,WORK69                                                    
*                                                                               
         CLC   REFSAVE,SPACES                                                   
         BE    PIC43                                                            
         USING ACOTHERD,R1                                                      
         LA    R1,OTHEREL                                                       
         MVI   ACOTEL,X'23'                                                     
         MVI   ACOTLEN,ACOTLNQ1                                                 
         MVC   ACOTNUM(13),SPACES                                               
         MVC   ACOTNUM(L'REFSAVE),REFSAVE                                       
*                                                                               
PIC43    CLI   PROORDH+5,0                                                      
         BE    PIC44                                                            
         USING ACNOD,R1            EXTRA NO. ELEMENT FOR ORDER NUMBER           
         LA    R1,ORDNOEL                                                       
         MVC   ACNOEL(2),=X'250A'                                               
         MVC   ACNO(6),ORDNO                                                    
         CLI   PROORD+7,C'P'       PARTIAL ORDER?                               
         BNE   *+8                                                              
         MVI   ACNOSTAT,C'P'       YES, SET PARTIAL FLAG                        
         DROP  R1                                                               
*                                                                               
         USING DLPOSTD,R1                                                       
PIC44    LA    R1,WORK69                                                        
         MVI   DLPSEL,DLPSEDRQ     DEBIT JOB                                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,JOBEXNUM                                                
         MVC   DLPSDBNM,JOBEXNAM                                                
         MVC   DLPSCRAC,SBPNUM                                                  
         MVC   DLPSCRNM,SBPNAME                                                 
         MVC   DLPSANAL,SPACES                                                  
         MVI   DLPSTYPE,X'00'                                                   
         OC    VENDNUM,VENDNUM                                                  
         BZ    PIC45                                                            
         TM    COMPSTA2,X'02'                                                   
         BO    PIC45                                                            
         MVC   DLPSCRNM,VENDNAME                                                
         MVC   DLPSCRAC,VENDNUM                                                 
*                                                                               
PIC45    LA    R2,4                LOOP COUNTER                                 
         LA    R3,WRKC1            COMMISSIONABLE WORKCODES                     
         LA    R4,WRKNC1           NON-COMMISSIONABLE WORKCODES                 
*                                                                               
PIC45A   CLC   0(2,R3),SPACES      COMMISSIONABLE WORKCODE?                     
         BE    PIC45B              NO, TRY NON-COMMISSIONABLE                   
*                                                                               
         CLI   ORDNOEL,X'00'       YES, DO WE HAVE AN ORDER#?                   
         BE    *+14                NO                                           
         MVC   0(L'ORDNOEL,R8),ORDNOEL                                          
         LA    R8,L'ORDNOEL(R8)                                                 
*                                                                               
         CLI   OTHEREL,X'00'       YES, DO WE HAVE AN OTHER #?                  
         BE    *+14                NO                                           
         MVC   0(L'OTHEREL,R8),OTHEREL                                          
         LA    R8,L'OTHEREL(R8)                                                 
*                                                                               
         BAS   RE,BUILD4C          USE 4C FOR CASH ACCOUNT(IF NEEDED)           
*                                                                               
         MVC   DLPSANAL,0(R3)                                                   
         ZAP   DLPSAMNT,2(6,R3)                                                 
         MVI   DLPSTYPE,X'00'                                                   
         MVC   0(L'WORK69,R8),WORK69                                            
         LA    R8,L'WORK69(R8)                                                  
*                                                                               
PIC45B   CLC   0(2,R4),SPACES      NON-COMMISSIONABLE WORKCODE?                 
         BE    PIC45E              NO, LOOP THROUGH                             
*                                                                               
         CLI   ORDNOEL,X'00'                                                    
         BZ    *+14                                                             
         MVC   0(L'ORDNOEL,R8),ORDNOEL                                          
         LA    R8,L'ORDNOEL(R8)                                                 
*                                                                               
         CLI   OTHEREL,X'00'                                                    
         BZ    *+14                                                             
         MVC   0(L'OTHEREL,R8),OTHEREL                                          
         LA    R8,L'OTHEREL(R8)                                                 
*                                                                               
         BAS   RE,BUILD4C                                                       
*                                                                               
         MVC   DLPSANAL,0(R4)      AND CHANGE VARIABLES                         
         ZAP   DLPSAMNT,2(6,R4)                                                 
         OI    DLPSTYPE,X'40'                                                   
         MVC   0(L'WORK69,R8),WORK69                                            
         LA    R8,L'WORK69(R8)                                                  
*                                                                               
PIC45E   LA    R3,8(R3)            NEXT COMMISSIONABLE WC & AMT                 
         LA    R4,8(R4)            NEXT NON-COMMISSIONABLE WC & AMT             
         BCT   R2,PIC45A                                                        
         SR    R3,R3               CLEAR R3 AGAIN                               
*                                                                               
         USING ACOTHERD,R8                                                      
PIC46B   MVC   ACOTEL(2),=X'230F'  BUILD 'OTHERS' ELEMENT FOR                   
         MVC   ACOTNUM(13),SPACES  PRODUCT AND JOB                              
         IC    R3,PROPROH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ACOTNUM(0),PROPRO                                                
         IC    R3,PROJOBH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ACOTNUM+6(0),PROJOB                                              
*                                                                               
         IC    R3,ACOTLEN                                                       
         AR    R8,R3                                                            
         DROP  R1                                                               
*                                                                               
         USING DLPOSTD,R8                                                       
PIC47    MVC   DLPSEL(2),=X'6A71'  CREDIT SUPPLIER                              
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSDBAC,PRODNUM    CLI/PROD FOR CONTRA                          
         MVC   DLPSDBNM,PRODNAM                                                 
PIC47A   MVC   DLPSCRAC,SBPNUM                                                  
         MVC   DLPSCRNM,SBPNAME                                                 
         CLC   SBPNUM+1(2),=C'SX'                                               
         BNE   *+16                                                             
         MVC   DLPSDBAC,JOBEXNUM   OR CLI/PRD/JOB FOR EXPENSE VENDORS           
         MVC   DLPSDBNM,JOBEXNAM                                                
         CLC   SBPNUM+1(2),=C'SA'  OR SOMETHING ELSE FOR ADVANCES               
         BNE   PIC47C                                                           
         OC    SAVNUM,SAVNUM                                                    
         BZ    PIC47C                                                           
         MVC   DLPSDBAC,SAVNUM                                                  
         MVC   DLPSDBNM,SAVNAM                                                  
PIC47C   DS    0H                                                               
         MVC   DLPSANAL,SPACES                                                  
         MVC   DLPSANAL,FILT                                                    
         ZAP   DLPSAMNT,CSHTOT                                                  
         IC    R3,DLPSLEN                                                       
         EJECT                                                                  
*-------------------------------------------------------------                  
*        BUILD SUBSIDIARY ELEMENTS - IF NECESSARY                               
*-------------------------------------------------------------                  
*                                                                               
PIC100   DS    0H                                                               
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         MVI   0(R8),0                                                          
         LA    R8,1(R8)                                                         
         LA    R3,IOAREA                                                        
         SR    R8,R3                                                            
         STH   R8,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY                                                        
         EJECT                                                                  
*-------------------------------------------------------------                  
*        ADD ENTRY TO TWA1                                                      
*-------------------------------------------------------------                  
*                                                                               
         XC    WORK,WORK                                                        
         IC    R3,DOCLEN                                                        
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),PRODOC      REF                                          
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         ZAP   TRANSAMT,CSHTOT                                                  
         BAS   RE,ADTWA1                                                        
         MVC   PROTOT(10),=C'ITEM TOTAL'                                        
         LA    R2,PROTOT+11                                                     
         EDIT  (P6,CSHTOT),(13,(R2)),2,MINUS=YES                                
         GOTO1 SQUASHER,DMCB,PROTOT,25                                          
         OI    PROTOTH+6,X'80'                                                  
         CLI   MODE,1                                                           
         BNE   PIC102                                                           
         ZAP   TOTCASH,CSHTOT                                                   
         LA    R3,7                                                             
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,,(R9),WRKC1      ADDRESS OF WRKCODE TABLE              
         CLI   ERRNUM,X'FE'                                                     
         BL    EXIT                                                             
         BE    *+8                                                              
*                                                                               
PIC102   LA    R2,PROORDH                                                       
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------                  
*        ROUTINE TO VALIDATE UPTO 2 WORK CODES AND 2 AMOUNTS                    
*-------------------------------------------------------------                  
*                                                                               
*        R3=A(WORK CODE FIELD HDR),R4=A(OUTPUT VALUES)                          
*                                                                               
VALWAM   NTR1                                                                   
         USING VALD,R3                                                          
         NI    MULTWCSW,X'00'                                                   
         MVC   VALWNM,SPACES       CLEAR & TRANSMIT NAME                        
         OI    VALWNMH+6,X'80'                                                  
         MVC   VALWNM4,SPACES                                                   
         OI    VALWNM4H+6,X'80'                                                 
         MVC   0(2,R4),SPACES      CLEAR OUTPUT VALUES                          
         ZAP   2(6,R4),=P'0'                                                    
         MVC   8(2,R4),SPACES                                                   
         ZAP   10(6,R4),=P'0'                                                   
         MVC   16(2,R4),SPACES                                                  
         ZAP   18(6,R4),=P'0'                                                   
         MVC   24(2,R4),SPACES                                                  
         ZAP   26(6,R4),=P'0'                                                   
*                                                                               
         MVI   ERRNUM,1                                                         
         LA    R2,VALWRKH                                                       
         CLI   VALWRKH+5,0         CHECK FOR INPUT IN BOTH FIELDS               
         BNE   VALW02                                                           
         CLI   VALAMTH+5,0                                                      
         BNE   VALWERR                                                          
         B     VALWOK              NO INPUT IN BOTH IS OK                       
*                                                                               
VALW02   LA    R2,VALAMTH                                                       
         CLI   VALAMTH+5,0                                                      
         BE    VALWERR                                                          
*                                  VALIDATE WORK CODES                          
         LA    R2,VALWRKH                                                       
         GOTO1 SCANNER,DMCB,(R2),(4,WORK)                                       
         MVI   ERRNUM,19                                                        
         CLI   4(R1),0                                                          
         BE    VALWERR                                                          
*                                                                               
         MVC   SAVWLINE,4(R1)      SAVE NUMBER OF SCAN LINES                    
         MVI   THISLINE,1                                                       
         LA    R5,WORK             R5=A(SCAN BLOCK ENTRY)                       
         MVC   SVWCNMS,SPACES                                                   
         LA    R6,SVWCNMS                                                       
         LR    R8,R4                                                            
*                                                                               
         USING GOBLOCKD,R1                                                      
VALW04   L     R1,AGOBLOCK                                                      
         CLC   THISLINE,SAVWLINE                                                
         BH    VALW16                                                           
         MVI   ERRNUM,2                                                         
         CLI   0(R5),1             CHECK L'INPUT                                
         BL    VALWERR                                                          
         CLI   0(R5),2                                                          
         BH    VALWERR                                                          
         CLI   1(R5),0                                                          
         BNE   VALWERR                                                          
         MVI   ERRNUM,19                                                        
         MVC   0(2,R8),12(R5)                                                   
         CLC   0(2,R8),=C'99'                                                   
         BE    VALWERR                                                          
         LA    R1,GOUWLIST         CHECK IT'S A BILLABLE W/C                    
         BAS   RE,VALW06                                                        
         B     VALW10                                                           
         DROP  R1                                                               
*                                                                               
VALW06   LA    R0,6                                                             
VALW08   CLC   0(2,R8),0(R1)                                                    
         BE    VALWERR                                                          
         LA    R1,2(R1)                                                         
         BCT   R0,VALW08                                                        
         BR    RE                                                               
*                                                                               
VALW10   MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),JOBEXNUM                                                
         MVC   KEY+4(2),0(R8)                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'ACCOUNT',KEY,KEY                  
         MVI   ERRNUM,0                                                         
         CLI   8(R1),0             CHECK FOR ERRORS                             
         BNE   VALWERR                                                          
         LA    R1,IOAREA                                                        
         SR    RE,RE                                                            
*                                  FIND NAME ELEMENT                            
VALW12   CLI   0(R1),0                                                          
         BE    VALW14                                                           
         CLI   0(R1),ACANELQ                                                    
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VALW12                                                           
*                                                                               
         USING ACANALD,R1                                                       
         TM    MULTWCSW,FSTWC                                                   
         BZ    *+14                                                             
         MVC   0(2,R6),=C', '                                                   
         LA    R6,2(R6)                                                         
         MVC   0(L'ACANDESC,R6),ACANDESC                                        
         GOTO1 RIGHT,DMCB,(R6),15                                               
*                                                                               
VALW14   LA    R5,32(R5)           BUMP TO NEXT                                 
         LA    R6,L'ACANDESC(R6)                                                
         OI    MULTWCSW,FSTWC                                                   
         LA    R8,8(R8)                                                         
         ZIC   R1,THISLINE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,THISLINE                                                      
         B     VALW04                                                           
*                                                                               
VALW16   ST    R6,SAVER6                                                        
         NI    MULTWCSW,X'FF'-FSTWC                                             
         ZIC   R6,SAVWLINE                                                      
         LTR   R6,R6                                                            
         BNP   VALW18                                                           
         MVC   TSTWCNMS,SVWCNMS                                                 
         GOTO1 SQUASHER,DMCB,TSTWCNMS,66 ELIMINATE MULT. SPACES                 
         CLI   DMCB+7,49                                                        
         BNH   VALW18                                                           
         MVC   VALWNM(49),SVWCNMS                                               
         GOTO1 SQUASHER,DMCB,VALWNM,49   THREE WC DESC                          
         MVC   VALWNM4(15),SVWCNMS+51                                           
         GOTO1 SQUASHER,DMCB,VALWNM4,15  FOURTH WC DESC.                        
         GOTO1 RIGHT,DMCB,VALWNM4,15                                            
         OI    VALWNM4H+6,X'80'                                                 
         B     *+10                                                             
*                                                                               
VALW18   MVC   VALWNM(49),TSTWCNMS       VALIDATE AMOUNTS                       
         L     R6,SAVER6                                                        
         LA    R2,VALAMTH                                                       
         GOTO1 SCANNER,DMCB,(R2),(4,WORK)                                       
         MVI   ERRNUM,2                                                         
         CLI   4(R1),0                                                          
         BE    VALWERR                                                          
         MVC   SAVALINE,4(R1)      SAVE NUMBER OF SCAN LINES                    
         MVI   ERRNUM,1                                                         
         CLC   SAVALINE,SAVWLINE   CHECK SAME AS NUMBER OF W/C'S                
         BNE   VALWERR                                                          
         MVI   THISLINE,1                                                       
         LA    R5,WORK             R5=A(SCAN BLOCK ENTRY)                       
         LR    R8,R4                                                            
         MVI   FVINDX,1                                                         
*                                                                               
VALW28   CLC   THISLINE,SAVALINE                                                
         BH    VALWOK                                                           
         MVI   ERRNUM,2                                                         
         CLI   1(R5),0                                                          
         BNE   VALWERR                                                          
         ZIC   R0,0(R5)            CHECK FOR VALID CASH FILED                   
         GOTO1 AMTVAL,DMCB,12(R5),(R0)                                          
         MVI   ERRNUM,25                                                        
         CLI   0(R1),0                                                          
         BNE   VALWERR                                                          
         L     R1,4(R1)                                                         
         LA    R1,0(R1)                                                         
         ZAP   2(6,R8),0(8,R1)                                                  
         AP    CSHTOT,0(8,R1)                                                   
*                                                                               
         LA    R4,JOBEXNUM         RE-READ THE JOB W/WORKCODE                   
         GOTO1 ASETJOB,DMCB,(X'80',(R4)),(R8)                                   
         GOTO1 AOPTVAL                                                          
         BNE   VALWERR                                                          
*                                                                               
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    VALW30              NO                                           
*                                                                               
         MVI   ERRNUM,30                                                        
         GOTO1 AWRKVAL,DMCB,(R8)                                                
         BH    VALWERR                                                          
*                                                                               
VALW30   LA    R5,32(R5)                                                        
         LA    R8,8(R8)                                                         
         ZIC   R1,THISLINE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,THISLINE                                                      
         ZIC   R1,FVINDX                                                        
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         B     VALW28                                                           
*                                                                               
VALWOK   MVI   ERRNUM,X'FF'        SET CC=EQ IF ALL OK                          
VALWERR  CLI   ERRNUM,X'FF'                                                     
         B     XITR2                                                            
         DROP  R1,R3                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        LOOK AT 'SA' OR 'SB' LEDGER AND POSTINGS                               
*-------------------------------------------------------------                  
*                                                                               
CHECKSA  NTR1                                                                   
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),SBPNUM+1                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CHECKNO                                                          
*                                                                               
         LA    RF,IOAREA                                                        
         SR    R1,R1                                                            
CHECK02  CLI   0(RF),0             SEE IF LEDGER IS OPEN ITEM                   
         BE    CHECKNO                                                          
         CLI   0(RF),X'14'                                                      
         BE    CHECK04                                                          
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     CHECK02                                                          
*                                                                               
         USING ACLEDGD,RF                                                       
CHECK04  CLI   ACLTLIKE,C'R'                                                    
         BNE   CHECKYS                                                          
*                                                                               
         MVC   KEY(15),SBPNUM                                                   
         BAS   RE,HIGH                                                          
         B     *+8                                                              
CHECK10  BAS   RE,SEQ                                                           
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CHECKNO                                                          
         LA    RF,IOAREA                                                        
         CLI   0(RF),X'43'                                                      
         BNE   CHECK12                                                          
         USING TRSUBHD,RF          SAVE CONTRA-ACCOUNT DETAILS                  
         SR    R1,R1                                                            
         MVC   SAVNAM,SPACES                                                    
         MVC   SAVNUM,TRSBACNT                                                  
         IC    R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     CHECK10                                                          
         MVC   SAVNAM(0),TRSBNAME                                               
*                                                                               
CHECK12  DS    0H                                                               
         CLI   0(RF),X'44'                                                      
         BNE   CHECK10                                                          
         USING TRANSD,RF                                                        
         TM    TRNSSTAT,X'80'      MUST BE DEBIT                                
         BZ    CHECK10                                                          
         CLC   SAVEDATE,TRNSDATE   AND MATCH ON DATE                            
         BNE   CHECK10                                                          
         SR    R1,R1                                                            
         IC    R1,DOCLEN                                                        
         BCTR  R1,0                AND REF                                      
         EX    R1,CHECKDOC                                                      
         BE    CHECKYS                                                          
         B     CHECK10                                                          
*                                                                               
CHECKDOC CLC   TRNSREF(0),PRODOC   EXECUTED                                     
CHECKNO  MVI   ERRNUM,53                                                        
CHECKYS  B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------                  
*        BUILD 4C POSTING ELEMENT                                               
*-------------------------------------------------------------                  
*                                                                               
         USING TRSDESCD,R8                                                      
BUILD4C  NTR1                                                                   
         OC    VENDNUM,VENDNUM                                                  
         BZ    XIT                                                              
         TM    COMPSTA2,X'02'                                                   
         BO    XIT                                                              
*                                  IF VENDOR IS CONTRA NEED SC ACCOUNT          
*                                  FOR PROPER CREDIT WHEN REVERSING EP          
         MVI   TRSDEL,X'4C'                                                     
         MVC   TRSDACCS(80),SPACES                                              
         MVC   TRSDACCS(14),SBPNUM+1                                            
         GOTO1 SQUASHER,DMCB,TRSDACCS,80                                        
         ZIC   R5,DMCB+7                                                        
         LA    R5,2(R5)                                                         
         STC   R5,TRSDLEN                                                       
         AR    R8,R5                                                            
         XIT1  REGS=(R8)                                                        
         EJECT                                                                  
XIT      XIT1                                                                   
XITR2    XIT1  REGS=(R2)                                                        
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
       ++INCLUDE ACBATCDD                                                       
*                                                                               
         ORG   TWAHOLE                                                          
         EJECT                                                                  
*-------------------------------------------------------------                  
*        LOCAL WORKING STORAGE                                                  
*-------------------------------------------------------------                  
*                                                                               
PROGD    DSECT                                                                  
SAVER6   DS    A                                                                
RIGHT    DS    V                                                                
*                                                                               
SBPNUM   DS    CL15                                                             
SBPNAME  DS    CL36                                                             
JOBEXNUM DS    CL15                                                             
JOBEXNAM DS    CL36                                                             
SAVEDATE DS    CL3                                                              
SAVNUM   DS    CL15                                                             
SAVNAM   DS    CL36                                                             
SECNDSW  DS    CL1                                                              
FILT     DS    CL2                                                              
CLINUM   DS    CL15                                                             
CLINAME  DS    CL36                                                             
PRODNUM  DS    CL15                                                             
PRODNAM  DS    CL36                                                             
*                                                                               
WRKC1    DS    CL2                                                              
COMAMNT1 DS    PL6                                                              
WRKC2    DS    CL2                                                              
COMAMNT2 DS    PL6                                                              
WRKC3    DS    CL2                                                              
COMAMNT3 DS    PL6                                                              
WRKC4    DS    CL2                                                              
COMAMNT4 DS    PL6                                                              
WRKNC1   DS    CL2                                                              
AMNTNC1  DS    PL6                                                              
WRKNC2   DS    CL2                                                              
AMNTNC2  DS    PL6                                                              
WRKNC3   DS    CL2                                                              
AMNTNC3  DS    PL6                                                              
WRKNC4   DS    CL2                                                              
AMNTNC4  DS    PL6                                                              
*                                                                               
CSHTOT   DS    PL6                                                              
SAVWLINE DS    X                                                                
SAVALINE DS    X                                                                
THISLINE DS    X                                                                
DOCLEN   DS    CL1                                                              
DOCSAVE  DS    CL6                                                              
REFSAVE  DS    CL6                                                              
SVSTAT   DS    CL1                                                              
VENDNUM  DS    CL15                                                             
VENDNAME DS    CL36                                                             
V2CNUM   DS    CL15                                                             
V2CNAM   DS    CL36                                                             
CONTROL  DS    CL15                                                             
CONTROLN DS    CL36                                                             
*                                                                               
SVWCNMS  DS    CL((4*(L'ACANDESC+2))-2)                                         
TSTWCNMS DS    CL((4*(L'ACANDESC+2))-2)                                         
MULTWCSW DS    X                                                                
FSTWC    EQU   X'80'                                                            
*                                                                               
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
*                                                                               
ORDNOEL  DS    XL10                ORDER NUMBER ELEMENT                         
OTHEREL  DS    XL(ACOTLNQ1)        OTHER NUMBER ELEMENT                         
WORK69   DS    CL(DLPSLNQ)                                                      
PROGDX   DS    0C                                                               
         EJECT                                                                  
VALD     DSECT                                                                  
VALWRKH  DS    CL8                                                              
VALWRK   DS    CL11                                                             
VALWRKX  DS    CL8                                                              
VALWNMH  DS    CL8                                                              
VALWNM   DS    CL49                                                             
VALWNMX  DS    CL8                                                              
         DS    CL17                                                             
VALAMTH  DS    CL8                                                              
VALAMT   DS    CL40                                                             
VALAMTX  DS    CL8                                                              
VALWNM4H DS    CL8                                                              
VALWNM4  DS    CL15                                                             
         EJECT                                                                  
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENDAY                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACBAT2F   06/14/18'                                      
         END                                                                    
