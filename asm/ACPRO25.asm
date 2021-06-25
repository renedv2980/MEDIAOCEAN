*          DATA SET ACPRO25    AT LEVEL 078 AS OF 09/12/14                      
*PHASE T60B25A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B25 - LNKLST/REPORT'                                         
T60B25   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B25**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         MVI   IOOPT,C'Y'          THIS OVERLAY DOES THE IO                     
*                                                                               
         CLI   EMULATE,C'Y'                                                     
         BE    LLK                                                              
*                                                                               
         MVC   CONHEAD(L'OLDACC),OLDACC                                         
         B     MYERROR                                                          
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------------         
* MODE DRIVEN LOGIC                                                             
*----------------------------------------------------------------------         
*                                                                               
LLK      LA    RE,LOCAL                                                         
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ST    R2,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   LLK1                                                             
         CLI   ACTNUM,ACTNREP                                                   
         BNE   XIT                                                              
         BAS   RE,VALREP                                                        
         B     XIT                                                              
*                                                                               
LLK1     CLI   MODE,PRINTREP                                                    
         BNE   LLK2                                                             
*                                                                               
         OI    GENSTAT3,DIEONERR                                                
*                                                                               
         BAS   RE,REPORT                                                        
         B     XIT                                                              
*                                                                               
LLK2     CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
*                                                                               
LLK3     BAS   RE,VALHED                                                        
         BAS   RE,VALOPT                                                        
*                                                                               
         MVI   INTMODE,DISLIST     INITIALIZE INTERNAL MODE                     
*                                                                               
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
*                                                                               
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES                                          
*                                                                               
*                                                                               
         BAS   RE,SETLIM                                                        
*                                                                               
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    LLK6                                                             
*                                                                               
         BAS   RE,PROCSEL          PROCESS ANY SELECTS                          
*                                                                               
LLK6     GOTO1 VCLEARF,DMCB,PROSEL1H,PRODATX                                    
         GOTO1 (RF),(R1),(1,PROSEL1H),PRODATX                                   
         MVI   LNLISTS,0                                                        
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
         XC    LASTLINK,LASTLINK CLEAR OUT LAST POINTER LISTED                  
*                                                                               
         BAS   RE,LIST                                                          
*                                                                               
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
         LA    R2,PROSEL1H                                                      
         CLI   LNLISTS,NLINES      TEST IF SCREEN FILLED                        
         BE    LLK8                YES                                          
*                                                                               
         LA    R2,PROCLIH          PUT CURSOR AT FIRST KEY FIELD                
         MVC   CONHEAD(L'LIST2MSG),LIST2MSG                                     
         B     LLK9                                                             
*                                                                               
LLK8     MVC   CONHEAD(L'LISTMSG),LISTMSG                                       
LLK9     ST    R2,ACURFORC                                                      
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*       ACTION REPORT, WRITE THE LINK REPORT                                    
*----------------------------------------------------------------------         
*                                                                               
REPORT   NTR1  WORK=(R2,WCTABSZE)                                               
*                                                                               
*        OI    SPOOLIND,SPNSPACE                                                
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VSETEMU             NEED TO EMULATE FOR PROGEN ROUTINES          
         BAS   RE,VALREP           VALIDATE REPORT REQUEST HEADERS              
*                                                                               
         BAS   RE,INITREP                                                       
*                                                                               
         ST    R2,AWCTAB                                                        
         XC    NCODES,NCODES                                                    
         XC    LASTLINK,LASTLINK                                                
*                                                                               
         BAS   RE,SETRLIM          SET START AND END KEY FOR REPORT             
*                                                                               
         XC    JOBNUM,JOBNUM                                                    
         XC    PRODCODE,PRODCODE                                                
         XC    CLICODE,CLICODE                                                  
*                                                                               
         GOTO1 VSETNEW                                                          
         BAS   RE,LIST             PRODUCE REPORT                               
*                                                                               
         LA    R2,CONACTH          SET R2 FOR XIT                               
*                                                                               
REPX     B     XIT                                                              
         EJECT                                                                  
FIRSTS   NTR1                                                                   
*                                                                               
         BAS   RE,SETFLENS                                                      
         BAS   RE,SETOFFS                                                       
         LA    R6,BIGKEY                                                        
         LA    R5,LASTLINK                                                      
         XR    R1,R1                                                            
*                                                                               
         IC    R1,STLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,FRSTCMP                                                       
         BE    *+8                                                              
         BAS   RE,PRTHSTUD         FOR STUDIO HIGH REPORT                       
*                                                                               
         IC    R1,CLLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,FRSTCMP                                                       
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         IC    R1,PRLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,FRSTCMP                                                       
         BE    *+8                                                              
         BAS   RE,PRTPROD                                                       
*                                                                               
         IC    R1,JBLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,FRSTCMP                                                       
         BE    *+8                                                              
         BAS   RE,PRTJOB                                                        
*                                                                               
         IC    R1,STLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,FRSTCMP                                                       
         BE    *+8                                                              
         BAS   RE,PRTSTUD                                                       
*                                                                               
         IC    R1,SUBJLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,FRSTCMP                                                       
         BE    *+8                                                              
         BAS   RE,PRTSUBJ                                                       
         B     XIT                                                              
*                                                                               
FRSTCMP  CLC   0(0,R5),0(R6)                                                    
*                                                                               
LASTS    NTR1                                                                   
         CLC   LASTLINK(STCKAJB-STCKEY+L'STCKAJB),BIGKEY                        
         BE    L30                                                              
         TM    PSTATUS,PRTPEND                                                  
         BNO   L30                                                              
         BAS   RE,PRINTEM                                                       
*                                                                               
L30      OC    LASTLINK,LASTLINK   WAS THERE A PREVOIUS LINK                    
         BZ    LX                  NO                                           
         CLI   QAGSRT,C'Y'         AGENCY JOB REPORT                            
         BNE   LX                  NO                                           
*                                                                               
         CLI   FORCEHED,C'Y'       TOP-OF-FORM PENDING                          
         BE    LX                  YES                                          
         CLC   LASTLINK(SACKSTY-SACKEY+L'SACKSTY),BIGKEY NEW STUDIO             
         BE    XIT                 NO                                           
         BAS   RE,PRINTEM          SKIP LINE BEFORE NEW STUDIO                  
*                                                                               
LX       B     XIT                                                              
*                                                                               
PRTPROD  NTR1                                                                   
         BAS   RE,PRINTEM                                                       
*                                                                               
         LA    R6,BIGKEY                                                        
         ZIC   R1,CLOFF                                                         
         LA    R6,0(R1,R6)                                                      
         OC    0(6,R6),0(R6)      ANYTHING DEFINED                              
         BZ    XIT                 NO                                           
*                                                                               
         USING REPD,R5                                                          
         LA    R5,P                                                             
         MVC   REPROD,PRODCODE                                                  
*                                                                               
         MVC   BLOCK(45),SPACES                                                 
         MVC   BLOCK(36),PRODNAME                                               
         BAS   RE,PUTOFF                                                        
         LA    R2,REPRODN                                                       
         LA    R3,L'REPRODN                                                     
         GOTO1 SQUAPPER,DMCB,(R2),(R3)                                          
         CLI   DMCB+11,1           MORE THAN 1 LINE SQUASHED                    
         BNE   *+8                 YES, UNDERLINE MAX LEN                       
         IC    R3,DMCB+7           UNDERLINE FOR SQUASHED LEN                   
         LA    R3,5(R3)            PRODCODE LEN-2                               
*                                                                               
         BAS   RE,PRINTEM                                                       
*                                                                               
         MVI   REPROD,C'-'                                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   REPROD+1(0),REPROD                                               
         B     XIT                                                              
*                                                                               
PRTJOB   NTR1                                                                   
         BAS   RE,PRINTEM                                                       
*                                                                               
         USING REPD,R5                                                          
         LA    R5,P                                                             
*                                                                               
         LA    R6,BIGKEY                                                        
         ZIC   R1,CLOFF                                                         
         LA    R6,0(R1,R6)                                                      
         OC    0(12,R6),0(R6)      ANYTHING DEFINED                             
         BNZ   PJOB30                                                           
         MVC   REJOBN(12),=CL12'MISSING LINK'                                   
         B     XIT                                                              
*                                                                               
PJOB30   MVC   REJOB,JOBNUM                                                     
         MVC   BLOCK(45),SPACES                                                 
         MVC   BLOCK(36),JOBNAME                                                
         MVC   BYTE,JOBSTAT                                                     
         BAS   RE,PUTCLO                                                        
         LA    R2,REJOBN                                                        
         LA    R3,L'REJOBN                                                      
         GOTO1 SQUAPPER,DMCB,(R2),(R3)                                          
         OI    PSTATUS,PRTPEND                                                  
         B     XIT                                                              
PRTSUBJ  NTR1                                                                   
*                                                                               
         USING REPD,R5                                                          
         OI    PSTATUS,PRTPEND                                                  
         LA    R5,P                                                             
         LA    R6,BIGKEY                                                        
         ZIC   R1,SUBJOFF                                                       
         LA    R6,0(R1,R6)                                                      
         OC    0(12,R6),0(R6)      ANYTHING DEFINED                             
         BNZ   PSUB30                                                           
         MVC   REJOB2,=CL12'MISSING LINK'                                       
         B     XIT                                                              
PSUB30   MVC   BLOCK(45),SPACES                                                 
         MVC   BLOCK(12),0(R6)                                                  
         MVC   BYTE,SUBJSTAT                                                    
         BAS   RE,PUTCLO                                                        
         LA    R2,REJOB2                                                        
         LA    R3,L'REJOB2                                                      
         GOTO1 SQUAPPER,DMCB,(R2),(R3)                                          
         B     XIT                                                              
PRTHSTUD NTR1                                                                   
         CLI   QAGSRT,C'Y'         AGENCY JOB REPORT                            
         BE    XIT                 YES, STUDIO IN BODY                          
         MVI   FORCEHED,C'Y'       STUDIO IN HEADER                             
         B     XIT                                                              
*                                                                               
PRTSTUD  NTR1                                                                   
         CLI   QAGSRT,C'Y'         AGENCY JOB REPORT                            
         BNE   XIT                 NO                                           
*                                                                               
         USING REPD,R5             PRINT STUDIO IN BODY                         
         LA    R5,P                                                             
         MVC   RESTUD,STUDIO                                                    
         OI    PSTATUS,PRTPEND                                                  
         B     XIT                                                              
*--------------------------------------------------------------------           
*       SQUASH AND CHOP 45 BYTES OF BLOCK INTO THE ADDRESS IN P1, FOR           
*        RETURES SQUASH LEN IN DMCB+7 AND N'LINES IN DMCB+11                    
*--------------------------------------------------------------------           
*                                                                               
SQUAPPER NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         GOTO1 SQUASHER,DMCB,BLOCK,45                                           
         MVC   BYTE,DMCB+7                                                      
         GOTO1 CHOPPER,DMCB,(45,BLOCK),((R3),(R2)),(C'P',3),0                   
         MVC   DMCB+7(L'BYTE),BYTE                                              
         B     XIT                                                              
*                                                                               
PUTCLO   TM    BYTE,RSTSACIC                                                    
         BNO   *+10                                                             
         MVC   BLOCK+37(7),=C' (ST=C)'                                          
         BR    RE                                                               
*                                                                               
PUTOFF   EQU   *                                                                
         MVC   BLOCK+37(5),=C' (OF='                                            
         MVC   BLOCK+42(2),GOEFFOFC                                             
         MVI   BLOCK+44,C')'                                                    
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                                   
*----------------------------------------------------------------------         
*                                                                               
VALHED   NTR1                                                                   
         GOTO1 SETHEIR                                                          
         MVI   KEYCHG,C'N'         INITIALIZE KEY FIELD CHANGE SWITCH           
         MVI   OPTION,0            NAME FIELDS TO BE SHOWN                      
*                                                                               
         LA    R2,PROCLIH                                                       
         MVC   QCLI,SPACES                                                      
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED2                                                          
         GOTO1 VALCLI                                                           
         MVC   QCLI,CLICODE                                                     
*                                                                               
VALHED2  LA    R2,PROPROH                                                       
         MVC   QPROD,SPACES                                                     
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED4                                                          
         GOTO1 VALPROD                                                          
         MVC   QPROD,PRODCODE                                                   
*                                                                               
VALHED4  LA    R2,PROSTRH                                                       
         MVC   QJOB,SPACES                                                      
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED6                                                          
         ZIC   R1,5(R2)                                                         
         STC   R1,QJOBLN           SAVE LENGTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QJOB(0),8(R2)                                                    
*                                                                               
VALHED6  OI    PROCLIH+4,X'20'     SET ON PREV VALID BITS                       
         OI    PROPROH+4,X'20'                                                  
         OI    PROSTRH+4,X'20'                                                  
*                                                                               
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
*----------------------------------------------------------------------         
*        VALIDATE OPTION FIELD                                                  
*----------------------------------------------------------------------         
VALOPT   NTR1                                                                   
         LA    R2,PROOPTH                                                       
         BAS   RE,TSTKEY                                                        
         OI    4(R2),X'20'         FLAG FIELD AS VALIDATED                      
         CLI   5(R2),0                                                          
         BE    VOX                                                              
         CLI   8(R2),C'Y'                                                       
         BE    VO40                                                             
         CLI   8(R2),C'O'                                                       
         BE    VO40                                                             
         MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
*                                                                               
VO40     CLC   QCLI,SPACES                                                      
         BNE   VOER                                                             
         CLC   QPROD,SPACES                                                     
         BNE   VOER                                                             
         CLC   QJOB,SPACES                                                      
         BE    VOX                                                              
VOER     MVC   CONHEAD(L'LIST2MSG),LIST2MSG  AS PER LRES, "LIST DISP"           
         GOTO1 VCLEARF,DMCB,PROSEL1H,PRODATX                                    
         GOTO1 (RF),(R1),(1,PROSEL1H),PRODATX                                   
         B     MYERROR                                                          
VOX      B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        VALIDATE REPORT REQUEST OPTIONS                                        
*----------------------------------------------------------------------         
VALREP   NTR1                                                                   
         XC    REQU(REQULN),REQU                                                
         MVI   OPTION,C'Y'                                                      
         MVI   KEYCHG,C'N'                                                      
*                                                                               
         BAS   RE,CLRNAMES                                                      
*                                                                               
         LA    R2,RPTOFGH         OFFICE GROUP                                  
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VR2                                                              
         CLC   =C'ALL',8(R2)       TEST FOR 'ALL'                               
         BE    VR2                                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         GOTO1 VALOG                                                            
         MVC   QOFG,EFFOFG                                                      
*                                                                               
VR2      LA    R2,RPTOFFH          OFFICE                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VR4                                                              
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   RPTOFGH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALOFF                                                           
         MVC   QOFF,EFFOFFC                                                     
*                                                                               
VR4      LA    R2,RPTCLIH          CLIENT                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VR6                                                              
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   RPTOFGH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   RPTOFFH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALCLI                                                           
         MVC   QCLI,CLICODE                                                     
*                                                                               
VR6      LA    R2,RPTPROH          PRODUCT                                      
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VR8                                                              
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   RPTCLIH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALPROD                                                          
         MVC   QPROD,PRODCODE                                                   
*                                                                               
VR8      LA    R2,RPTJOBH          JOB                                          
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VR9                                                              
         MVI   ERROR,NEEDPRO       IF INPUT, NEED PRODUCT AS WELL               
         CLI   RPTPROH+5,0                                                      
         BE    ERREND                                                           
*                                                                               
         GOTO1 VALJOB                                                           
         MVC   QJOB,JOBNUM                                                      
         ZIC   R1,5(R2)                                                         
         STC   R1,QJOBLN           SAVE LENGTH                                  
*                                                                               
         MVI   QAGSRT,C'N'         WHEN JOB IS I/P, FORCE REPORT TYPE           
         TM    JOBJSTAT,JOBSTUD                                                 
         BO    *+8                                                              
         MVI   QAGSRT,C'Y'                                                      
*                                                                               
         B     VR10                                                             
*                                                                               
VR9      MVI   ERROR,MISSING                                                    
         TM    WHEN,X'80'+X'40'    CAN'T BE NOW, OR IMMED W/NO JOB              
         BNZ   ERREND              WITH NO JOB                                  
*                                                                               
*        TM    WHEN,X'20'          SOON?                                        
*        BNO   VR10                NO                                           
*                                                                               
         LA    R2,RPTSRTH          IF THIS A STUDIO REPORT ...                  
         CLI   5(R2),0                                                          
         BE    VR9A                DEFAULT IS STUDIO                            
*                                                                               
         CLI   8(R2),C'S'          UNLESS 'S' ENTERED, ITS AN AGY REP           
         BNE   VR10                                                             
*                                                                               
VR9A     LA    R2,RPTSTUH          MAKE SURE THEY ENTERED A STUDIO TYPE         
         CLI   5(R2),0                                                          
         BE    ERREND              FORCE STUDIO TYPE WHEN SOON & NO JOB         
*                                                                               
VR10     LA    R2,RPTMGPH          MEDIA GROUP                                  
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VR12                                                             
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   RPTJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMG                                                            
         MVC   QMGR,MGROUP                                                      
*                                                                               
VR12     LA    R2,RPTMEDH          MEDIA                                        
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VR14                                                             
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   RPTMGPH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   RPTJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMED                                                           
         MVC   QMED,MEDIA                                                       
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(12),MEDNAME+3   SEE PRO08 FOR REASON WHY                  
         LA    R2,RPTMEDNH                                                      
         BAS   RE,MOVEFLD                                                       
*                                                                               
VR14     LA    R2,RPTSTUH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VR14H                                                            
         MVC   STUDIO,SPACES                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STUDIO(0),8(R2)                                                  
         GOTO1 VSETNEW                                                          
         BAS   RE,READSTUD                                                      
         GOTO1 VSETEMU                                                          
         MVC   QSTUD,STUDIO                                                     
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR,STUDNAME                                                  
         LA    R2,RPTSTUNH                                                      
         BAS   RE,MOVEFLD          MOVE OUT STUDIO NAME                         
*                                                                               
VR14H    LA    R2,RPTSRTH                                                       
         BAS   RE,TSTKEY                                                        
         OC    QAGSRT,QAGSRT       HAS SORT OPTION BEEN SET                     
         BNZ   *+8                 YES, USE AS DEFAULT                          
*                                                                               
         MVI   QAGSRT,NO                                                        
         CLI   5(R2),0                                                          
         BE    VR15                                                             
         CLI   8(R2),C'S'          STUDIO SORT                                  
         BE    VR15                                                             
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'A'          AGENCY SORT                                  
         BNE   ERREND                                                           
         MVI   QAGSRT,YES          YES                                          
*       - - - - - - - - - -                                                     
VR15     LA    R2,RPTFIL1H                                                      
         LA    R3,QFILTS                                                        
         LA    R1,NQFILTS                                                       
VR16     CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   0(1,R3),8(R2)                                                    
         BAS   RE,TSTKEY                                                        
         BAS   RE,BUMPTOUN                                                      
         LA    R3,1(R3)                                                         
         BCT   R1,VR16                                                          
*                                                                               
VR20     LA    R2,RPTJNLH                                                       
         BAS   RE,TSTKEY                                                        
         MVI   QJNL,NO             SET DEFAULT                                  
         CLI   5(R2),0                                                          
         BE    VR30                                                             
         BAS   RE,Y_OR_N           VALIDATE INPUT AS YES OR NO                  
         MVC   QJNL,8(R2)                                                       
*                                                                               
VR30     LA    R2,RPTNOCH                                                       
         BAS   RE,TSTKEY                                                        
         MVI   QNOC,NO             SET DEFAULT                                  
         CLI   5(R2),0                                                          
         BE    VR40                                                             
         BAS   RE,Y_OR_N           VALIDATE INPUT AS YES OR NO                  
         MVC   QNOC,8(R2)                                                       
*                                                                               
VR40     LA    R2,RPTNOCJH                                                      
         BAS   RE,TSTKEY                                                        
         MVI   QNOCJ,NO            SET DEFAULT                                  
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         BAS   RE,Y_OR_N           VALIDATE INPUT AS YES OR NO                  
         MVC   QNOCJ,8(R2)                                                      
*                                                                               
VR50     LA    R2,RPTSTUH          ENSURE STUDIO SET FOR STUDIO NOW REP         
         CLI   QAGSRT,NO           STUDIO REP                                   
         BNE   VR60                NO                                           
         TM    WHEN,X'80'+X'40'    NOW REPORT                                   
         BZ    VR60                NO                                           
*                                                                               
         MVI   ERROR,MISSING                                                    
         OC    QSTUD,QSTUD         IS STUDIO SET                                
         BZ    ERREND              NO, ERROR                                    
*                                                                               
VR60     B     XIT                                                              
*                                                                               
Y_OR_N   CLI   8(R2),C'Y'                                                       
         BER   RE                                                               
         CLI   8(R2),C'N'                                                       
         BER   RE                                                               
         MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         EJECT                                                                  
CLRNAMES NTR1                                                                   
         MVC   LISTAR,SPACES                                                    
         LA    R2,RPTOFGNH                                                      
         BAS   RE,MOVEFLD                                                       
         LA    R2,RPTOFFNH                                                      
         BAS   RE,MOVEFLD                                                       
         LA    R2,RPTCLINH                                                      
         BAS   RE,MOVEFLD                                                       
         LA    R2,RPTPRONH                                                      
         BAS   RE,MOVEFLD                                                       
         LA    R2,RPTJOBNH                                                      
         BAS   RE,MOVEFLD                                                       
         LA    R2,RPTMGPNH                                                      
         BAS   RE,MOVEFLD                                                       
         LA    R2,RPTMEDNH                                                      
         BAS   RE,MOVEFLD                                                       
         LA    R2,RPTSTUNH                                                      
         BAS   RE,MOVEFLD                                                       
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SET STRTLINK AND ENDLINK, KEYS TO LIMIT READING                        
*----------------------------------------------------------------------         
SETLIM   NTR1                                                                   
         MVC   STRTLINK,SPACES                                                  
         LA    R2,PROOPTH                                                       
         CLI   5(R2),0             NO UNLINKED                                  
         BE    SL30                                                             
*                                                                               
         XC    STRTLINK,STRTLINK   PRIME                                        
         CLI   8(R2),C'O'          ONLY                                         
         BNE   SL30                NO                                           
         MVC   ENDLINK,SPACES                                                   
         B     SL40                                                             
*                                                                               
SL30     MVI   ENDLINK,X'FF'                                                    
         MVC   ENDLINK+1(L'ENDLINK-1),ENDLINK                                   
*                                                                               
SL40     LA    R6,STRTLINK                                                      
         BAS   RE,SETAKEY                                                       
         LA    R6,ENDLINK                                                       
         BAS   RE,SETAKEY                                                       
         USING SACRECD,R6                                                       
         MVI   SACKAJB+12,X'FF'   DON'T LET START LIMIT THE BACK END            
         B     XIT                                                              
*                                                                               
*----------------------------------------------------------------------         
*        SET STRTLINK AND ENDLINK BASED ON REPORT SCREEN OPTIONS                
*----------------------------------------------------------------------         
SETRLIM  NTR1                                                                   
         LA    RF,SETRST           ASSUME STUDIO JOB REPORT                     
         CLI   QAGSRT,C'Y'         SORT BY AGENCY JOBS?                         
         BNE   *+8                                                              
         LA    RF,SETRAG                                                        
*                                                                               
         BASR  RE,RF                                                            
         B     XIT                                                              
*                                                                               
SETRAG   NTR1                                                                   
*                                                                               
         XC    STRTLINK,STRTLINK   PRIME                                        
*                                                                               
SRA30    MVI   ENDLINK,X'FF'                                                    
         MVC   ENDLINK+1(L'ENDLINK-1),ENDLINK                                   
*                                                                               
         LA    R6,STRTLINK                                                      
         BAS   RE,SETAKEY                                                       
         LA    R6,ENDLINK                                                       
         BAS   RE,SETAKEY                                                       
         USING SACRECD,R6                                                       
         MVI   SACKAJB+12,X'FF'   DON'T LET START LIMIT THE BACK END            
*                                                                               
SETRAX   B     XIT                                                              
*                                                                               
SETRST   NTR1                                                                   
         XC    STRTLINK,STRTLINK   PRIME                                        
*                                                                               
         MVI   ENDLINK,X'FF'                                                    
         MVC   ENDLINK+1(L'ENDLINK-1),ENDLINK                                   
*                                                                               
         LA    R6,STRTLINK                                                      
         BAS   RE,SETJKEY                                                       
         LA    R6,ENDLINK                                                       
         BAS   RE,SETJKEY                                                       
         USING STCRECD,R6                                                       
         MVI   STCKSJB+12,X'FF'   DON'T LET START LIMIT THE BACK END            
*                                                                               
SETRSX   B     XIT                                                              
*                                                                               
         USING SACRECD,R6                                                       
SETAKEY  EQU   *                                                                
         MVI   SACKTYP,SACKTYPQ                                                 
         MVI   SACKSUB,SACKSUBQ                                                 
         MVC   SACKCPY,CUL                                                      
         CLC   QCLI,SPACES                                                      
         BNHR  RE                                                               
         MVC   SACKAJB(3),QCLI                                                  
         CLC   QPROD,SPACES                                                     
         BNHR  RE                                                               
         MVC   SACKAJB+3(3),QPROD                                               
*                                                                               
         CLC   QJOB,SPACES                                                      
         BNHR  RE                                                               
         MVC   SACKAJB+6(L'QJOB),QJOB                                           
         BR    RE                                                               
*                                                                               
         USING STCRECD,R6                                                       
SETJKEY  NTR1                                                                   
         MVI   STCKTYP,STCKTYPQ                                                 
         MVI   STCKSUB,STCKSUBQ                                                 
         MVC   STCKCPY,CUL                                                      
*                                                                               
         CLC   QJOB,SPACES         DID THEY ENTER A JOB                         
         BH    SETJK30             YES, I CAN GET THE STUDIO TYPE               
*                                                                               
         CLC   QSTUD,SPACES        SPECIFIC STUDIO REQUESTED                    
         BNH   SETJKX              NO, CAN'T LIMIT READS                        
         MVC   STCKSTY,QSTUD                                                    
*                                                                               
SETJK30  CLC   QCLI,SPACES                                                      
         BNH   SETJKX                                                           
         MVC   STCKSJB(3),QCLI                                                  
         CLC   QPROD,SPACES                                                     
         BNH   SETJKX                                                           
         MVC   STCKSJB+3(3),QPROD                                               
*                                                                               
         CLC   QJOB,SPACES                                                      
         BNH   SETJKX                                                           
*                                                                               
         MVC   STCKSJB+6(6),QJOB                                                
*                                                                               
         CLI   ACTNUM,ACTNREP      REPORT                                       
         BNE   SETJKX              NO                                           
*                                                                               
         MVI   STCKSTY,X'FF'       IF UNLINKED, NO REPORT                       
         LA    R4,STCKSJB                                                       
         BAS   RE,READACC                                                       
         LR    R4,R6               SAVE R6                                      
         MVI   ELCODE,LNKELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SETJKX                                                           
         USING LNKELD,R6                                                        
         MVC   STCKSTY-STCKEY(L'STCKSTY,R4),LNKSTUD                             
*                                                                               
SETJKX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO LIST LINK POINTERS                                             
*                                                                               
* AT ENTRY, LNLISTS CONTAINS N'LIST LINES ALREADY ON SCREEN                     
*           AND LASTLINK CONTAINS LAST KEY READ OR BINARY ZERO                  
*----------------------------------------------------------------------         
*                                                                               
LIST     NTR1                                                                   
         GOTO1 VSETNEW                                                          
         LA    R2,PROSEL1H                                                      
         LA    R6,BIGKEY                                                        
         USING SACRECD,R6                                                       
*                                                                               
LIST2    ST    R2,ATHISLIN         INITIALIZE LIST LINE POINTER                 
*                                                                               
         XC    SACKEY,SACKEY                                                    
         MVC   SACKEY(L'STRTLINK),STRTLINK                                      
*                                                                               
LIST4    OC    LASTLINK,LASTLINK   TEST RESUMING READ                           
         BZ    LIST10              NO-STARTING FROM BEGINNING                   
*                                                                               
LIST5    BAS   RE,SETNEXT          SET NEXT KEY FOR READ HIGH                   
*                                                                               
LIST10   MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   ENDLINK,BIGKEY                                                   
         BL    LISTX                                                            
*                                                                               
         CLI   ACTNUM,ACTNREP                                                   
         BNE   LIST20                                                           
*                                                                               
         BAS   RE,SETOFFS          SET OFFSETS FOR KEY FIELDS                   
*                                                                               
         BAS   RE,SETACNTS         SET PROGEN ACCOUNT FIELDS                    
         BNE   LIST5               IF ACCOUNT NOT FOUND, REJECT                 
*                                                                               
         BAS   RE,SETSTUD          SET STUDIO NAME                              
*                                                                               
         BAS   RE,FILTER                                                        
         BNE   LIST5               DON'T WANT                                   
*                                                                               
         BAS   RE,LASTS                                                         
*                                                                               
         BAS   RE,FIRSTS           PROCESS FIRST OF ACCOUNTS                    
*                                                                               
         BAS   RE,PRTPTR                                                        
LIST15   MVC   LASTLINK,SACKEY                                                  
         B     LIST30                                                           
*                                                                               
LIST20   L     R2,ATHISLIN                                                      
*                                                                               
         BAS   RE,SETLIN                                                        
         BAS   RE,DISLNK                                                        
*                                                                               
         MVC   LASTLINK,SACKEY                                                  
         MVC   ATHISLIN,ANEXTSEL                                                
         ZIC   RE,LNLISTS                                                       
         LR    R1,RE                                                            
         LA    RE,1(RE)            INCREMENT LIST LINES COUNT                   
         STC   RE,LNLISTS                                                       
*                                                                               
         CLI   LNLISTS,NLINES      TEST SCREEN FILLED                           
         BE    LISTX                                                            
*                                                                               
LIST30   B     LIST4               NEXT POINTER                                 
*                                                                               
LISTX    EQU   *                                                                
         CLI   ACTNUM,ACTNREP                                                   
         BE    LISTXR                                                           
*                                                                               
         CLC   BIGKEY(L'ENDLINK),ENDLINK HAVE I READ PAST THE END               
         BNH   LISTXX                   NO                                      
         MVC   LASTLINK,STRTLINK        START OVER NEXT TIME                    
         MVI   INTMODE,FSTLIST                                                  
         B     LISTXX                                                           
*                                                                               
LISTXR   MVC   BIGKEY,LASTLINK     SO THAT PRTCONT WILL CONSIDER THE            
*                                  PRODUCT NOT TO HAVE CHANGED IN THE           
*                                  CASE WHERE THERE THE LAST LINE               
*                                  LEFT TO PRINT CAUSES A TOP OF FORM           
*                                  LASTLINK WIL HAVE THE LAST KEY               
*                                  PROCESSED FOR THIS REPORT                    
         TM    PSTATUS,PRTPEND                                                  
         BNO   LISTXX                                                           
         BAS   RE,SETACNTS         RESET ACCOUNT NAMES                          
         BAS   RE,SETSTUD          SET STUDIO NAME                              
         BAS   RE,PRINTEM          PRINT THE LINE                               
*                                                                               
LISTXX   GOTO1 VSETEMU                                                          
         B     XIT                                                              
         EJECT                                                                  
SETNEXT  LA    R6,BIGKEY                                                        
         USING SACRECD,R6                                                       
*                                                                               
         CLI   ACTNUM,ACTNREP                                                   
         BE    *+10                                                             
         MVC   SACKEY(L'LASTLINK),LASTLINK                                      
*                                                                               
         LA    RF,SACKPO                                                        
         CLI   ACTNUM,ACTNREP                                                   
         BNE   *+8                                                              
         LA    RF,SACKPO+L'SACKPO                                               
         MVI   0(RF),X'FF'       BUMP TO NEXT POINTER                           
         BR    RE                                                               
*                                                                               
*----------------------------------------------------------------------         
* SUB-ROUTINE TO FILTER ITEMS FROM THE REPORT                                   
*----------------------------------------------------------------------         
*                                                                               
FILTER   NTR1  ,                                                                
*                                                                               
         L     R1,=A(FLTTAB)                                                    
         A     R1,RELO                                                          
         LA    R0,NNFLTS                                                        
*                                                                               
         USING FLTTABD,R1                                                       
FLT10    MVC   LAINST,LAR3         SET R3 TO A(REQUEST FIELD)                   
         MVC   LAINST+2(2),FLTREQ                                               
         EX    0,LAINST                                                         
*                                                                               
         MVC   LAINST,LAR2                                                      
         MVC   LAINST+2(2),FLTACT  SET R2 TO A(ACTUAL DATA)                     
         EX    0,LAINST                                                         
*                                                                               
         ZIC   R4,FLTLEN                                                        
         BAS   RE,FLTFLD                                                        
         BNE   FLTX                                                             
         LA    R1,FLTTABLN(R1)                                                  
         BCT   R0,FLT10                                                         
*                                                                               
         CLI   ACISDEL,C'Y'        IS ACC OR SUB ACC DELETED                    
         BE    FLTNG               REJECT                                       
*                                                                               
         CLI   QJNL,C'Y'           UNLINKED ONLY?                               
         BNE   FLT80               NO, ACCEPT                                   
*                                                                               
         USING STCRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         CLI   STCKSUB,STCKSUBQ    IS THIS A STUDIO TYPE POINTER                
         BNE   FLT60               NO                                           
         LA    R1,STCKAJB                                                       
         B     FLT70                                                            
         DROP  R6                                                               
*                                                                               
         USING SACRECD,R6                                                       
FLT60    CLI   SACKSUB,SACKSUBQ    IS THIS AN AGENCY JOB POINTER                
         BNE   FLTNG               NO, REJECT??                                 
*                                                                               
         LA    R1,SACKAJB                                                       
*                                                                               
FLT70    OC    0(L'SACKAJB,R1),0(R1)  AGENCY JOB DEFINED FOR THIS KEY           
         BNZ   FLTNG               NO, ACCEPT                                   
*                                                                               
FLT80    CLI   QNOCJ,C'Y'          SUPPRESS CLOSED JOBS                         
         BNE   FLTOK                                                            
*                                                                               
         TM    JOBSTAT,RSTSACIC    IS THIS JOB CLOSED                           
         BNO   FLTOK               NO, COOL                                     
*                                                                               
         DROP  R6                                                               
FLTNG    CR    R1,RB               REJECT                                       
         B     FLTX                                                             
*                                                                               
FLTOK    CR    R1,R1               RETURN EQUAL                                 
FLTX     B     XIT                                                              
*                                                                               
LAR3     LA    R3,FULL                                                          
LAR2     LA    R2,FULL                                                          
*                                                                               
*        FILTER THE KEY FIELD IN 0(R2) WITH THE REQUEST FIELD IN 0(R3)          
*               FOR A LENGTH OF R4                                              
*                                                                               
FLTFLD   EQU   *                                                                
         BCTR  R4,0                DEC R4 FOR EXECUTED CLC'S                    
         EX    R4,FFCHK                                                         
         BNH   FFOK                                                             
         EX    R4,FFCMP                                                         
         BR    RE                                                               
FFCHK    CLC   0(0,R3),SPACES      ANYTHING IN REQUEST FIELD                    
FFCMP    CLC   0(0,R3),0(R2)       COMPARE REQUEST  WITH KEY VALUE              
FFOK     CR    R1,R1                            COOL                            
         BR    RE                                                               
         EJECT                                                                  
SETOFFS  EQU   *                   SET OFFSETS FROM POINTER                     
         LA    R1,STCOFFS                                                       
         CLI   BIGKEY+1,X'02'      STUDIO TYPE CODE POINTER                     
         BE    *+8                                                              
         LA    R1,SACOFFS                                                       
         MVC   OFFSETS,0(R1)                                                    
         BR    RE                                                               
*                                                                               
STCOFFS  EQU   *                                                                
         DC    AL1(STCKSJB-STCKEY)   CLIENT                                     
         DC    AL1(STCKSJB+3-STCKEY) PRODUCT                                    
         DC    AL1(STCKSJB+6-STCKEY) JOB                                        
         DC    AL1(STCKSTY-STCKEY) STUDIO                                       
         DC    AL1(STCKAJB-STCKEY) SUB JOB                                      
*                                                                               
SACOFFS  EQU   *                                                                
         DC    AL1(SACKAJB-SACKEY)   CLIENT                                     
         DC    AL1(SACKAJB+3-SACKEY) PRODUCT                                    
         DC    AL1(SACKAJB+6-SACKEY) JOB                                        
         DC    AL1(SACKSTY-SACKEY) STUDIO                                       
         DC    AL1(SACKSJB-SACKEY) SUB JOB                                      
*                                                                               
SETFLENS EQU   *                   SET LENS FOR FIRSTS                          
         LA    R1,STCFL                                                         
         CLI   BIGKEY+1,X'02'      STUDIO TYPE CODE POINTER                     
         BE    *+8                                                              
         LA    R1,SACFL                                                         
         MVC   FLENS,0(R1)                                                      
         BR    RE                                                               
*                                                                               
STCFL    EQU   *                                                                
         DC    AL1(STCKSJB+3-STCKEY)                                            
         DC    AL1(STCKSJB+6-STCKEY)                                            
         DC    AL1(STCKSJB+12-STCKEY)                                           
         DC    AL1(STCKSTY+4-STCKEY)                                            
         DC    AL1(STCKAJB+12-STCKEY)                                           
*                                                                               
SACFL    EQU   *                                                                
         DC    AL1(SACKAJB+3-SACKEY)   CLIENT                                   
         DC    AL1(SACKAJB+6-SACKEY) PRODUCT                                    
         DC    AL1(SACKAJB+12-SACKEY) JOB                                       
         DC    AL1(SACKSTY+4-SACKEY) STUDIO                                     
         DC    AL1(SACKSJB+12-SACKEY) SUB JOB                                   
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO DISPLAY LIST LINE DATA FOR AN PIUNTER                          
*----------------------------------------------------------------------         
*                                                                               
DISLNK   NTR1  ,                                                                
         USING LISTD,R5                                                         
         LA    R5,LISTAR                                                        
         USING SACRECD,R6                                                       
         LA    R6,BIGKEY                                                        
*                                                                               
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
         L     R2,ADATA                                                         
         MVC   LISTAR,SPACES                                                    
         MVC   LISAJOB,SACKAJB                                                  
         MVC   LISTYPE,SACKSTY                                                  
         MVC   LISSJOB,SACKSJB                                                  
*                                                                               
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISLNKX  B     XIT                                                              
         SPACE 4                                                                
*----------------------------------------------------------------------         
* SUB-ROUTINE TO EDIT THE LIST SCREEN                                           
*----------------------------------------------------------------------         
*                                                                               
PROCSEL  NTR1                                                                   
         LA    R2,PROSEL1H         R2=A(SELECT FIELD)                           
         XR    R3,R3                                                            
         ICM   R3,1,LNLISTS                                                     
         BZ    PSELX                                                            
*                                                                               
         BAS   RE,SETLIN           SET FIELD HEADER ADCONS                      
*                                                                               
         USING LISTD,R5                                                         
PSEL2    L     R2,ASEL             RESTORE R2=A(SELECT FIELD)                   
         L     R5,ADATA                                                         
         LA    R5,8(R5)            GET PAST THE HEADER                          
*                                                                               
         CLI   5(R2),0             TEST ANY SELECT INPUT                        
         BE    PSEL60                                                           
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'*'          TEST ALREADY EDITED                          
         BE    PSEL60              YES                                          
*                                                                               
         CLI   8(R2),C'S'                                                       
         BNE   ERREND              INVALID SELECT FIELD                         
*                                                                               
         MVI   PFKEY,0                                                          
         MVI   8(R2),C'*'                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 VCALL,WORK,RECNLNK,ACTNMNT,(3,LISSJOB),(3,LISSJOB+3),(6,X        
               LISSJOB+6),0                                                     
*                                                                               
PSEL60   L     R2,ANEXTSEL                                                      
         BAS   RE,SETLIN                                                        
         BCT   R3,PSEL2                                                         
*                                                                               
PSELX    B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO SET ADCONS FOR A LIST FIELD LINE                               
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                           
*----------------------------------------------------------------------         
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         ST    R2,ASEL                                                          
         BAS   RE,BUMP                                                          
         ST    R2,ADATA                                                         
         BAS   RE,BUMP                                                          
         ST    R2,ANEXTSEL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*----------------------------------------------------------------------         
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*        PRINT THE POINTER IN BIGKEY                                            
*---------------------------------------------------------------------          
*                                                                               
PRTPTR   NTR1                                                                   
*                                                                               
*        GOTO1 =A(GETPRO),DMCB,(RC),RR=RELO GET PROFILE VALUES                  
*                                                                               
         LA    R6,BIGKEY                                                        
         USING STCRECD,R6                                                       
         OC    STCKPO,STCKPO       IS THIS A POINTER TO AN ORDER                
         BZ    PRTPTRX             NO, NEXT POINTER                             
*                                                                               
         USING ORDRECD,R6                                                       
         GOTO1 GETREC              GET ORDER RECORD                             
*                                                                               
         L     R6,AIO                                                           
         USING ORDRECD,R6                                                       
         TM    ORDRSTAT,ORDSFMCH   FULLY MATCHED?                               
         BNO   PP10                NO                                           
         CLI   QNOC,YES            WANT CLOSED ?                                
         BNE   XIT                 GUESS NOT                                    
*                                                                               
         USING REPD,R5                                                          
PP10     LA    R5,P                                                             
         MVC   REONUM,ORDKORD                                                   
*                                                                               
         MVI   ELCODE,ORDELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ORDELD,R6                                                        
         GOTO1 DATCON,DMCB,(1,ORDDATE),(8,REODATE)                              
         GOTO1 DATCON,DMCB,(1,ORDDDTE),(8,REDDATE)                              
         MVC   REOAUTH,ORDAUTH                                                  
*                                                                               
         L     R6,AIO                                                           
         USING ORDRECD,R6                                                       
         TM    ORDRSTAT,ORDSFMCH   FULLY MATCHED?                               
         BNO   PP12                NO                                           
         MVC   REDDATE,=CL8'CLOSED'                                             
*                                                                               
PP12     ZAP   ORDACCUM,=P'0'                                                   
         MVI   ELCODE,OAMELQ                                                    
         XC    WCCNT,WCCNT                                                      
         USING OAMELD,R6                                                        
         BAS   RE,GETELIO                                                       
*                                                                               
PP15     BNE   PP20                                                             
         AP    ORDACCUM,OAMAMNT    ACCUMULATE WORKCODE AMOUNT                   
*                                                                               
         ZIC   RE,WCCNT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,WCCNT                                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         B     PP15                                                             
*                                                                               
PP20     EQU   *                                                                
         EDIT  ORDACCUM,(13,REOTOT),2,MINUS=YES                                 
*                                                                               
         CLI   WCCNT,1             ONLY 1 CODE ON THIS ORDER                    
         BE    *+8                 YES, HOLD PRINT FOR DETAIL                   
         BAS   RE,PRINTEM                                                       
*                                                                               
         BAS   RE,PRTWCDET         PRINT WORKCODE DETAILS                       
*                                                                               
PRTPTRX  B     XIT                 GET NEXT POINTER                             
         EJECT                                                                  
PRTWCDET NTR1                                                                   
         MVI   ELCODE,OAMELQ                                                    
         USING OAMELD,R6                                                        
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   PWCDX                                                            
*                                                                               
PWCD40   MVC   REOWC,OAMWORK                                                    
         MVC   WORKCODE,OAMWORK                                                 
         BAS   RE,GETWCNAM                                                      
         MVC   REOWCN,WORKNAME                                                  
         EDIT  OAMAMNT,(13,REOWCAMT),2,MINUS=YES                                
         BAS   RE,PRINTEM                                                       
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,NEXTEL                                                        
         BE    PWCD40                                                           
         BAS   RE,PRINTEM                                                       
*                                                                               
PWCDX    B     XIT                                                              
*                                                                               
         EJECT                                                                  
INITREP  NTR1                                                                   
*                                                                               
         MVC   REMUSER,TWAALIAS    SUPPLY REQUESTOR ID                          
*        GOTO1 OPENPQ                                                           
*                                                                               
         LA    R1,STUHOOK                                                       
         CLI   QAGSRT,C'Y'                                                      
         BNE   *+8                                                              
         LA    R1,AGYHOOK                                                       
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R1,STUSPECS                                                      
         CLI   QAGSRT,C'Y'                                                      
         BNE   *+8                                                              
         LA    R1,AGYSPECS                                                      
         ST    R1,SPECS                                                         
*                                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
*--------------------------------------------------------------------           
*        CALL SPOOL                                                             
*--------------------------------------------------------------------           
PRINTEM  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         NI    PSTATUS,X'FF'-PRTPEND                                            
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------           
*        CALL PROGEN VALIDATE ROUTINES FOR ANY NEW SJ ACCOUNTS                  
*        NOTE, PROGEN SETS EFFECTIVE VALUES FOR FILTERING                       
*        RETURNS CC EQ IF ALL ACCONTS ARE OK (OR NOT DEFINED)                   
*--------------------------------------------------------------------           
         USING SAWORKD,R4                                                       
SETACNTS NTR1  WORK=(R4,SAWORKLN)                                               
         XC    ACISDEL,ACISDEL     SET IF EITHER AC IS DELETED                  
         GOTO1 VSETEMU                                                          
         GOTO1 SETHEIR                                                          
         MVI   OPTION,C'N'         DONT GET NAMES                               
         MVC   SAACCT,SPACES                                                    
         ZIC   R1,CLOFF                                                         
         LA    R6,BIGKEY                                                        
         LA    R6,0(R1,R6)                                                      
         OC    0(12,R6),0(R6)      ANYTHING DEFINED                             
         BNZ   SETA10              YES                                          
         XC    CLICODE,CLICODE                                                  
         XC    PRODCODE,PRODCODE                                                
         XC    JOBNUM,JOBNUM                                                    
*                                                                               
SETA10   LA    R1,2                CHECK FOR NEW CLENT                          
         MVC   SACLI,CLICODE                                                    
         LA    R5,SACLI                                                         
         EX    R1,CMPKEY                                                        
         BE    SA30                                                             
*                                                                               
         XC    PRODCODE,PRODCODE   FORCE PRODUCT/JOB VALIDATE                   
         XC    JOBNUM,JOBNUM                                                    
*                                                                               
         MVC   SACLI,0(R6)                                                      
         BAS   RE,READACC                                                       
         BNE   SETANG                                                           
         GOTO1 SETNAME,DMCB,AIO,CLINAME                                         
*                                                                               
         GOTO1 SETCLI                                                           
*                                                                               
*                                                                               
SA30     LA    R1,2                                                             
         LA    R6,3(R6)            BUMP KEY POINTER TO PRODUCT                  
         MVC   SAPROD,PRODCODE                                                  
         LA    R5,SAPROD                                                        
         EX    R1,CMPKEY                                                        
         BE    SA40                                                             
*                                                                               
         XC    JOBNUM,JOBNUM       FORCE JOB VALIDATE                           
*                                                                               
         MVC   SAPROD,0(R6)                                                     
         BAS   RE,READACC                                                       
         BNE   SETANG                                                           
         GOTO1 SETNAME,DMCB,AIO,PRODNAME                                        
*                                                                               
         GOTO1 SETPROD                                                          
*                                                                               
*                                                                               
SA40     LA    R1,5                                                             
         LA    R6,3(R6)            BUMP KEY POINTER TO JOB                      
         MVC   SAJOB,JOBNUM                                                     
         LA    R5,SAJOB                                                         
         EX    R1,CMPKEY                                                        
         BE    SA50                                                             
*                                                                               
         MVC   SAJOB,0(R6)                                                      
         BAS   RE,READACC                                                       
         BNE   SETANG                                                           
         GOTO1 SETNAME,DMCB,AIO,JOBNAME                                         
*                                                                               
         GOTO1 SETJOB                                                           
*                                                                               
         BAS   RE,ISDELETE                                                      
*                                                                               
         BAS   RE,RDOPT            UPDATE GETOPT FIELDS                         
*                                                                               
SA50     LA    R6,BIGKEY           CHECK SUBJOB                                 
         ZIC   R1,SUBJOFF                                                       
         LA    R6,0(R1,R6)                                                      
         OC    0(12,R6),0(R6)      SUBJOB DEFINED                               
         BZ    SETAOK              NO                                           
         CLC   PREVSJOB,0(R6)      SAME AS PREV SUBJOB                          
         BE    SETAOK                                                           
         MVC   PREVSJOB,0(R6)                                                   
         MVC   SAACCT,0(R6)                                                     
         BAS   RE,READACC                                                       
         BNE   SETANG                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RSTELD,R6                                                        
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SUBJSTAT,RSTSTAT1   EXTRACT STATUS BYTE                          
*                                                                               
         BAS   RE,ISDELETE                                                      
*                                                                               
SETAOK   GOTO1 VSETNEW                                                          
         CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
SETANG   GOTO1 VSETNEW                                                          
         CR    R1,RB                                                            
         B     XIT                                                              
*                                                                               
CMPKEY   CLC   0(0,R6),0(R5)                                                    
*                                                                               
*        READ THE 12 BYTE PROD ACCOUNT AT 0(R4)                                 
*                                                                               
READACC  ST    RE,SAVERE                                                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(12),0(R4)                                                  
         OI    DMINBTS,X'08'       TURN ON READ DELETED                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' TURN OFF READ DELETED                        
         L     RE,SAVERE                                                        
         CLC   KEY(12),KEYSAVE     DID I GET ACCOUNT                            
         BR    RE                                                               
*                                                                               
ISDELETE NTR1                                                                   
         USING ACTRECD,R6                                                       
         L     R6,AIO                                                           
         TM    ACTRSTAT,ACTSDELT                                                
         BNO   *+8                                                              
         MVI   ACISDEL,C'Y'                                                     
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------           
*        SET STUDIO/STUDNAME IN WORKING STORAGE FROM BIGKEY                     
*--------------------------------------------------------------------           
SETSTUD  NTR1                                                                   
         BAS   RE,SETOFFS                                                       
         LA    R6,BIGKEY                                                        
         ZIC   R1,STOFF                                                         
         LA    R6,0(R1,R6)                                                      
         CLC   STUDIO,0(R6)        SAME STUDIO                                  
         BE    XIT                 YES, NAME IS SET                             
*                                                                               
         MVC   STUDIO,0(R6)                                                     
         MVC   AIO,AIO2                                                         
         BAS   RE,READSTUD                                                      
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
*                                                                               
READSTUD NTR1                                                                   
         MVC   BIGSAVE,BIGKEY                                                   
         USING STURECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    STUKEY,STUKEY                                                    
         MVI   STUKTYP,STUKTYPQ                                                 
         MVI   STUKSUB,STUKSUBQ                                                 
         MVC   STUKCPY,CUL                                                      
         MVC   STUKCODE,STUDIO                                                  
         GOTO1 READ                                                             
*                                                                               
         GOTO1 GETREC                                                           
         GOTO1 SETNAME,DMCB,AIO,STUDNAME                                        
         MVC   BIGKEY(L'BIGSAVE),BIGSAVE                                        
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------           
*        LOOK FOR EN VALIDATE ROUTINES FOR ``Y NEW SJ ACCOUNTS                  
*--------------------------------------------------------------------           
GETWCNAM NTR1                                                                   
         L     R3,AWCTAB           TABLE OF WORKCODES                           
         L     R0,NCODES           NUMBER OF CODES IN TABLE                     
         USING WCTABD,R3                                                        
GWC10    OC    WCCODE,WCCODE       ANY WC DEFINED                               
         BZ    GWC20               NO                                           
         CLC   WCCODE,WORKCODE                                                  
         BE    GWC50                                                            
         LA    R3,WCTABLN(R3)                                                   
         BCT   R0,GWC10                                                         
*                                                                               
         L     R3,AWCTAB           WHEN TABLE FULL AND NOT FOUND                
*                                  REPLACE FIRST CODE                           
*                                                                               
GWC20    LA    R1,2                BUILD DUMMY FIELD IN ELEMENT                 
         MVC   WORK(2),WORKCODE                                                 
         BAS   RE,MAKEFLD                                                       
         GOTO1 VSETEMU                                                          
         MVC   AIO,AIO2                                                         
         GOTO1 VALWORK             GET NAME                                     
         GOTO1 VSETNEW                                                          
         MVC   WCCODE,WORKCODE     SAVE IN TABLE                                
         MVC   WCNAME,WORKNAME                                                  
         L     R1,NCODES                                                        
         C     R1,=A(WCTABMX)                                                   
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         ST    R1,NCODES                                                        
         MVC   AIO,AIO1                                                         
         B     GWCX                                                             
*                                                                               
GWC50    MVC   WORKNAME,WCNAME     RETURN NAME                                  
*                                                                               
GWCX     B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*              SUPPORTING SUBROUTINES                                           
*----------------------------------------------------------------------         
*                                                                               
TSTKEY   TM    4(R2),X'80'         THIS TIME                                    
         BO    TSTKY                                                            
         TM    4(R2),X'20'                                                      
         BOR   RE                                                               
TSTKY    MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
MYERROR  MVI   ERROR,X'FE'                                                      
         GOTO1 VSETEMU                                                          
         OI    GENSTAT2,USMYOK                                                  
         ST    R2,ACURFORC                                                      
         BAS   RE,XMITSCRN                                                      
         GOTO1 ERREX2                                                           
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
YESXIT   CR    RB,RB                                                            
         B     XIT                                                              
NOXIT    LTR   RB,RB                                                            
*                                                                               
XMITSCRN LA    R2,CONHEADH                                                      
         SR    RF,RF                                                            
XS40     IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   XS40                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT WHOLE SCREEN           
         BR    RE                                                               
*                                                                               
*  TAKE THE LENGTH IN R1 AND THE DATA IN WORK AND CREATE A FIELD IN             
*  ELEMENT. SETS R2 TO A(DUMMY FIELD)                                           
*                                                                               
MAKEFLD  XC    ELEMENT,ELEMENT     BUILD DUMMY FIELD IN ELEMENT                 
         LA    R2,ELEMENT                                                       
         STC   R1,ELEMENT+5         SET LENGTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+6                                                           
         BR    RE                                                               
         MVC   ELEMENT+8(0),WORK                                                
*                                                                               
RDOPT    ST    RE,SAVERE                                                        
         MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,PRODCODE                                                
         MVC   GOSELJOB,JOBNUM                                                  
         LA    RE,GOBLOCKX                                                      
         ST    RE,GOAEXT                                                        
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
XIT      XIT1                                                                   
*--------------------------------------------------------------------           
* HOOK ROUTINE FOR REPORT PRINTING                                              
*--------------------------------------------------------------------           
*                                                                               
STUHOOK  NTR1  ,                                                                
         MVC   H4+10(L'STUDIO),STUDIO                                           
         MVC   H4+18(36),STUDNAME                                               
         MVC   H5+10(L'RPTCLI),CLICODE                                          
         MVC   H5+18(36),CLINAME   CLIENT NAME                                  
*                                                                               
         USING STCRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         LA    R5,LASTLINK                                                      
         CLC   STCKEY(STCKSJB+6-STCKEY),0(R5)                                   
         BNE   STUHX                                                            
*                                                                               
         LA    R5,H9                                                            
         BAS   RE,PRTCONT                                                       
*                                                                               
STUHX    B     XIT                                                              
*                                                                               
AGYHOOK  NTR1  ,                                                                
         MVC   H4+10(L'RPTCLI),CLICODE                                          
         MVC   H4+18(36),CLINAME   CLIENT NAME                                  
*                                                                               
         USING SACRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         LA    R5,LASTLINK                                                      
         CLC   SACKEY(SACKAJB+6-SACKEY),0(R5)                                   
         BNE   AGYHX                                                            
*                                                                               
         LA    R5,H8               PRINT PRODUCT NAME CONTINUED                 
         BAS   RE,PRTCONT                                                       
*                                                                               
AGYHX    B     XIT                                                              
*                                                                               
*----------------------------------------------------------------------         
*        PRINT PRODUCT/NAME CONTINUED                                           
*        IN THE HEADER LINE ADDRESSED BY R5                                     
*----------------------------------------------------------------------         
*                                                                               
         USING REPD,R5                                                          
PRTCONT  NTR1                                                                   
         OC    CLICODE,CLICODE     MISSING LINK?                                
         BZ    PCX                 YES                                          
*                                                                               
         MVC   REPROD,PRODCODE                                                  
*                                                                               
         MVC   BLOCK(52),SPACES                                                 
         MVC   BLOCK(36),PRODNAME                                               
*                                                                               
         MVC   BLOCK+37(5),=C' (OF='                                            
         MVC   BLOCK+42(2),GOEFFOFC                                             
         MVI   BLOCK+44,C')'                                                    
*                                                                               
         MVC   BLOCK+46(6),=C'CONT''D'                                          
*                                                                               
         LA    R2,REPRODN                                                       
         LA    R3,L'REPRODN                                                     
*                                                                               
         GOTO1 SQUASHER,DMCB,BLOCK,52                                           
         MVC   BYTE,DMCB+7               SAVE SQUASHED LEN                      
         GOTO1 CHOPPER,DMCB,(52,BLOCK),((R3),(R2)),(C'P',3),0                   
*                                                                               
         LA    R5,L'HEAD1(R5)      UNDERLINE                                    
         IC    R3,BYTE             ... FOR SQUASHED LEN                         
         CLI   DMCB+11,1           MORE THAN 1 LINE SQUASHED                    
         BE    PC40                NO                                           
         LA    R5,L'HEAD1(R5)      BUMP 1 MORE LINE                             
         LA    R3,L'REPRODN        UNDERLINE FOR MAX LEN                        
*                                                                               
PC40     LA    R3,5(R3)            PRODCODE LEN-2                               
*                                                                               
         MVI   REPROD,C'-'                                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   REPROD+1(0),REPROD                                               
PCX      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------           
*        CALL GETPROF                                                           
*--------------------------------------------------------------------           
         USING PROFKD,R2                                                        
GETPRO   NMOD1 0,*GETP*                                                         
         L     RC,0(R1)                                                         
         GOTO1 GETPROF,DMCB,PROFKEY,PROGPROF,DATAMGR                            
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
AGYSPECS DS    0D                                                               
*--------------------------------------------------------------------           
         SSPEC H1,2,CREATED                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,51,C'STUDIO LINK REPORT'                                      
         SSPEC H2,51,C'------------------'                                      
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H4,95,REPORT                                                     
         SSPEC H4,109,PAGE                                                      
*                                                                               
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H6,2,C'PRODUCT/JOB'                                              
         SSPEC H6,37,C'STUDIO'                                                  
         SSPEC H7,37,C' TYPE '                                                  
         SSPEC H6,44,C'STUDIO JOB'                                              
*                                                                               
         SSPEC H6,57,C'ORDER'                                                   
         SSPEC H7,57,C'NUMBER'                                                  
         SSPEC H6,64,C'ORDER'                                                   
         SSPEC H7,64,C'DATE'                                                    
         SSPEC H6,71,C'  DUE'                                                   
         SSPEC H7,71,C'  DATE'                                                  
         SSPEC H6,82,C'AUTHORIZER'                                              
         SSPEC H6,93,C'WORKCODE'                                                
         SSPEC H6,115,C'AMOUNT'                                                 
*                                                                               
*                                                                               
         DC    X'00'                                                            
*                                                                               
*--------------------------------------------------------------------           
STUSPECS DS    0D                                                               
*--------------------------------------------------------------------           
         SSPEC H1,2,CREATED                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,51,C'STUDIO LINK REPORT'                                      
         SSPEC H2,51,C'------------------'                                      
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H4,95,REPORT                                                     
         SSPEC H4,109,PAGE                                                      
*                                                                               
         SSPEC H4,2,C'STUDIO'                                                   
         SSPEC H5,2,C'CLIENT'                                                   
         SSPEC H7,2,C'PRODUCT/JOB'                                              
         SSPEC H7,44,C'AGENCY JOB'                                              
         SSPEC H7,57,C'ORDER'                                                   
         SSPEC H8,57,C'NUMBER'                                                  
         SSPEC H7,64,C'ORDER'                                                   
         SSPEC H8,64,C'DATE'                                                    
         SSPEC H7,71,C'  DUE'                                                   
         SSPEC H8,71,C'  DATE'                                                  
         SSPEC H7,82,C'AUTHORIZER'                                              
         SSPEC H7,93,C'WORKCODE'                                                
         SSPEC H7,115,C'AMOUNT'                                                 
*                                                                               
*                                                                               
         DC    X'00'                                                            
*                                                                               
*                                                                               
LISTMSG  DC    C'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                    
BADOPT   DC    C'ERROR - UNLINKED NOT ALLOWED WITH AGENCY ACCOUNT'              
OLDACC   DC    C'ERROR - FILE NOT CONVERTED'                                    
LIST2MSG DC    C'LIST DISPLAYED'                                                
*                                                                               
FLTTAB   DS    0C                                                               
         DC    SL2(QCLI,CLICODE),AL1(L'QCLI)                                    
         DC    SL2(QPROD,PRODCODE),AL1(L'QPROD)                                 
         DC    SL2(QJOB,JOBNUM),AL1(L'QJOB)                                     
         DC    SL2(QSTUD,STUDIO),AL1(L'QSTUD)                                   
         DC    SL2(QF1,EFF1),AL1(L'QF1)                                         
         DC    SL2(QF2,EFF2),AL1(L'QF2)                                         
         DC    SL2(QF3,EFF3),AL1(L'QF3)                                         
         DC    SL2(QF4,EFF4),AL1(L'QF4)                                         
         DC    SL2(QF5,EFF5),AL1(L'QF5)                                         
         DC    SL2(QOFG,GOEFFOG),AL1(L'QOFG)                                    
         DC    SL2(QOFF,GOEFFOFC),AL1(L'QOFF)                                   
         DC    SL2(QMGR,GOEFFMG),AL1(L'QMGR)                                    
         DC    SL2(QMED,GOEFFMED),AL1(L'QMED)                                   
NNFLTS   EQU   (*-FLTTAB)/FLTTABLN                                              
*                                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         PRINT ON                                                               
         SPACE 3                                                                
*ACJOBBERD                                                                      
*ACPROWORKD                                                                     
*DD POOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENFILE                                                                      
*ACGENBOTH                                                                      
*DDFLDHDR                                                                       
* DDSPOOK                                                                       
* FAFACTS                                                                       
* DDGETPROFD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDGETPROFD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0X                                                               
RELO     DS    A                                                                
SAVERE   DS    A                                                                
ACURSOR  DS    A                                                                
AWCTAB   DS    A                                                                
AFLTTAB  DS    A                                                                
NCODES   DS    F                                                                
*                                                                               
ATHISLIN DS    A                                                                
ASEL     DS    A                                                                
ADATA    DS    A                                                                
ANEXTSEL DS    A                                                                
*                                                                               
LAINST   DS    CL4                 LA INSTRUCTION                               
*                                                                               
INTMODE  DS    X                   INTERNAL MODE                                
KEYCHG   DS    C                                                                
CHASW    DS    C                   AUTHORIZE SWITCH                             
WCCNT    DS    C                   COUNT OF WORKCODES ON AN ORDER               
*                                                                               
REQU     DS    0C                                                               
QCLI     DS    CL6                                                              
QPROD    DS    CL6                                                              
QJOB     DS    CL6                                                              
QSTUD    DS    CL4                                                              
QINKEYLN EQU   *-REQU                                                           
QJOBLN   DS    CL1                                                              
*                                                                               
QFILTS   DS    0C                  FILTERS REQUIRE CLI/PRO/JOB IS READ          
QF1      DS    CL1                                                              
QF2      DS    CL1                                                              
QF3      DS    CL1                                                              
QF4      DS    CL1                                                              
QF5      DS    CL1                                                              
NQFILTS  EQU   *-QFILTS                                                         
QGETOPT  EQU   *                   IF THE FOLLOWING FIELDS ARE FILLED           
QOFG     DS    CL1                 GETOPT CALL IS NEEDED                        
QOFF     DS    CL2                                                              
QMGR     DS    CL1                                                              
QMED     DS    CL1                                                              
QGETOPTL EQU   *-QGETOPT                                                        
QAGSRT   DS    CL1                 Y, SORT BY AGECY JOB                         
QJNL     DS    CL1                 Y, INCLUDE UNLINKED JOBS                     
QNOC     DS    CL1                 Y, INCLUDE CLOSED ORDERS                     
QNOCJ    DS    CL1                 Y, SUPPRESS CLOSED JOBS                      
REQULN   EQU   *-REQU                                                           
ORDACCUM DS    PL6                                                              
YES      EQU   C'Y'                Z                                            
NO       EQU   C'N'                                                             
*                                                                               
OFFSETS  DS    0CL5                                                             
CLOFF    DS    CL1                 OFFSET TO CLIENT IN POINTER                  
PROFF    DS    CL1                 PRODUCT                                      
JBOFF    DS    CL1                 JOB                                          
STOFF    DS    CL1                 STUDIO                                       
SUBJOFF  DS    CL1                 SUB JOB OFFSET                               
*                                                                               
FLENS    DS    0CL5                                                             
CLLEN    DS    CL1                 LENGTH OF LEVELS WITHIN KEY                  
PRLEN    DS    CL1                 PRODUCT                                      
JBLEN    DS    CL1                 JOB                                          
STLEN    DS    CL1                 STUDIO                                       
SUBJLEN  DS    CL1                 SUB JOB OFFSET                               
*                                                                               
CLINAME  DS    CL36                                                             
PRODNAME DS    CL36                                                             
JOBNAME  DS    CL36                                                             
*                                                                               
STUDIO   DS    CL4                                                              
STUDNAME DS    CL36                                                             
*                                                                               
SUBJSTAT DS    CL1                 SAVED STATUS BYTE FRON SUBJOB                
PREVSJOB DS    CL12                PREVIOUS SUB JOB                             
PSTATUS  DS    CL1                                                              
PRTPEND  EQU   X'01'                                                            
ACISDEL  DS    CL1                 Y, ACCOUNT OR SUBACCOUNT IS DELETED          
*                                                                               
BIGSAVE  DS    CL(STCKDA-STCKEY+L'STCKDA)                                       
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACGOXBLOCK                                                     
         PRINT ON                                                               
         ORG   GOBLOCK             REUSE GOBLOCK FOR EDITOR BLOCK               
       ++INCLUDE DDEBLOCK                                                       
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
WCTABD   DSECT                                                                  
WCCODE   DS    CL2                                                              
WCNAME   DS    CL15                                                             
WCTABLN  EQU   *-WCTABD                                                         
WCTABMX  EQU   100                 MAXIMUM NUMBER OF WORKCODES TO STORE         
WCTABSZE EQU   (WCTABLN*WCTABMX)   NUMBER OF DOUBLES NEEDED                     
*                                                                               
SAWORKD  DSECT                     SETNAMES LOCAL STORAGE                       
SAACCT   DS    0CL12               ACCOUNT BUILT FROM PROGEN SAVE FLDS          
SACLI    DS    CL3                                                              
SAPROD   DS    CL3                                                              
SAJOB    DS    CL6                                                              
SAWORKLN EQU   *-SAWORKD                                                        
*                                                                               
FLTTABD  DSECT                                                                  
FLTREQ   DS    SL2                                                              
FLTACT   DS    SL2                                                              
FLTLEN   DS    AL1                                                              
FLTTABLN EQU   *-FLTTABD                                                        
*                                                                               
PRTTABD  DSECT                                                                  
PRTTEL   DS    CL1                 ELEMENT ID (0 IF KEY DATA)                   
PRTTFLOF DS    CL1                 OFFSET OF FIELD INTO ELEMENT                 
PRTTFLLN DS    CL1                 LENGTH OF FIELD                              
         ORG   PRTTFLLN                                                         
PRTTBIT  DS    CL1                 FOR STAT EDIT, BIT TO TEST                   
PRTTPROF DS    CL1                 OFFSET OF PRINT FIELD INTO P                 
PRTTPRLN DS    CL1                 MAX LENGTH OF PRINT FIELD                    
         ORG   PRTTPRLN                                                         
PRTTCHAR DS    CL1                 FOR STAT EDIT, CHAR TO PRINT                 
PRTTEDIT DS    CL1                 EDIT ROUTINE                                 
PRTTNEXT EQU   *                                                                
PRTTLN   EQU   *-PRTTABD                                                        
         EJECT                                                                  
* DSECT TO COVER REQUEST HEADER AND CARD                                        
*                                                                               
REQHDR   DSECT                                                                  
       ++INCLUDE DMREQHDR                                                       
REQUEST  DS    0CL80               REQUEST CARD LAYOUT                          
REQJCLID DS    CL2                 JCL ID                                       
REQAGYID DS    CL2                 AGENCY ID                                    
         DS    CL1                 N/D                                          
REQSIN   DS    CL6                 SYSTEM INPUT NUMBER                          
         ORG   REQUEST+L'REQUEST                                                
         EJECT                                                                  
REPD     DSECT                     DSECT TO COVER REPORT PRINT LINE             
REPROD   DS    CL6                                                              
         DS    CL1                                                              
REPRODN  DS    CL30                                                             
         DS    CL1                                                              
         ORG   REPD                                                             
REJOB    DS    CL6                                                              
         DS    CL1                                                              
REJOBN   DS    CL28                                                             
         DS    CL1                                                              
RESTUD   DS    CL4                                                              
         DS    CL3                                                              
REJOB2   DS    CL12                                                             
         DS    CL1                                                              
REONUM   DS    CL6                                                              
         DS    CL1                                                              
REODATE  DS    CL8                                                              
         DS    CL1                                                              
REDDATE  DS    CL8                                                              
         DS    CL1                                                              
REOAUTH  DS    CL8                                                              
         DS    CL3                                                              
REOWC    DS    CL2                                                              
         DS    CL1                                                              
REOWCN   DS    CL15                                                             
         DS    CL1                                                              
REOWCAMT DS    0CL13                                                            
REOTOT   DS    CL13                                                             
         EJECT                                                                  
LISTD    DSECT                     DSECT TO COVER LISTAR                        
LISAJOB  DS    CL12                                                             
         DS    CL3                                                              
LISTYPE  DS    CL4                                                              
         DS    CL10                                                             
LISSJOB  DS    CL12                                                             
LISLEN   EQU   *-LISTD                                                          
*                                                                               
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROB2D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROB3D                                                       
         SPACE 2                                                                
         ORG   T60BFFD+2504                                                     
LSAVES   DS    0D                                                               
LNLISTS  DS    X                   N'LISTS ON SCREEN                            
LASTLINK DS    CL(L'SACKEY)  LAST SACKEY                                        
STRTLINK DS    CL(L'SACKEY)  SACKEY TO START                                    
ENDLINK  DS    CL(L'SACKEY)  HIGHEST SACKEY  TO DISPLAY                         
         DS    CL((SAVAREA-LSAVES)-(*-LSAVES))  SPARE                           
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
NLINES   EQU   16                  N'LIST SCREEN LINES                          
LISTFLDS EQU   (ANEXTSEL-ASEL)/4   N'FIELDS ON LIST SCREEN LINE                 
FSTLIST  EQU   1                                                                
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078ACPRO25   09/12/14'                                      
         END                                                                    
