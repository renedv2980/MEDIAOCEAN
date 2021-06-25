*          DATA SET ACLFM00    AT LEVEL 085 AS OF 08/11/08                      
*PHASE T60300A,*                                                                
*INCLUDE ACDELEL                                                                
*INCLUDE ACPUTEL                                                                
*INCLUDE ACRAPPER                                                               
*INCLUDE ACSPLIT                                                                
*INCLUDE ACSRCHP                                                                
*INCLUDE SRCHPASS                                                               
         TITLE 'CONTROLLER FOR LFM'                                             
T60300   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LOGWORKX-LOGWORKD,**LFM0*,R9,RR=R2,CLEAR=YES                     
         USING LOGWORKD,RC                                                      
         ST    R2,RELO                                                          
         BAS   RE,SETWORK                                                       
         ST    R9,LOGR9                                                         
         ST    RD,BASERD                                                        
         L     RA,4(R1)                                                         
         L     R8,12(R1)           TIA ADDRESS                                  
         USING T603FFD,RA                                                       
         LR    R6,RA                                                            
         USING TWAD,R6                                                          
         MVC   LOGHEAD,SPACES                                                   
         OI    LOGHEADH+6,X'80'                                                 
         OI    LOGSERVH+1,X'01'                                                 
         OI    LOGSERVH+6,X'80'                                                 
         SPACE 1                                                                
*                                                                               
         CLI   TWAOFFC,C'*'        ARE WE ON A DDS TERM?                        
         BE    LOG00                                                            
*                                                                               
         LA    R2,LOGRECH                                                       
         MVC   LOGHEAD(38),=C'PROGRAM NO LONGER AVAILABLE - USE =AFM'           
         B     LOGEXIT        **AS OF OCT01/04                                  
*                                                                               
*        GOTO1 DATAMGR,DMCB,=C'DTFAD',=C'ACCOUNT'                               
*        L     RE,12(R1)           GET A(DCB)                                   
*        TM    ISFTYPE-ISDTF(RE),ISFTEMU                                        
*        BNO   *+8                                                              
LOG00    MVI   ACCEMU,C'Y'         FILE IS EMULATED                             
         MVI   NEWPRD,C'Y'         SET EVERYONE ON NEW PRODUCTION               
         MVI   RAPTR,C'N'          GENERATE RECORD ACTIVITY POINTERS            
         MVC   UPDADV,SPACES                                                    
         MVC   IO(50),SPACES                                                    
         MVC   IO(1),COMPANY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',IO,IO                        
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                CAN'T READ COMPANY RECORD                    
         LA    R3,IO                                                            
         USING ACKEYD,R3                                                        
         LA    R3,ACRECORD                                                      
LOG01    CLI   0(R3),ACMPELQ                                                    
         BE    LOG03                                                            
         CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                MISSING COMPANY ELEMENT                      
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     LOG01                                                            
         USING ACCOMPD,R3                                                       
LOG03    TM    ACMPSTA4,X'20'                                                   
         BZ    *+8                 NOT ON NEW PRODUCTION                        
         MVI   NEWPRD,C'Y'         SET SWITCH IF USING NEW PROD.                
         MVC   COMPSUP,ACMPSUPP    PRODUCTION SUPPLIERS                         
         MVC   COMPJOB,ACMPJOB     PRODUCTION UNIT/LEDGER                       
         MVC   COMPSTA1,ACMPSTAT   COMPANY STATUS BYTES                         
         MVC   COMPSTA2,ACMPSTA2                                                
         MVC   COMPSTA3,ACMPSTA3                                                
         MVC   COMPSTA4,ACMPSTA4                                                
         MVC   COMPSTAA,(CPYSTATA-CPYEL)(R3)                                    
         CLI   ACMPLEN,ACMPLNQ     TEST ELEMENT LONG ENOUGH                     
         BL    LOG05               NO                                           
         TM    ACMPSTA6,X'40'      TEST FOR RECORD ACTIVITY POINTERS            
         BZ    *+8                                                              
         MVI   RAPTR,C'Y'                                                       
         CLI   ACMPXSUP,0          TEST FOR EXTRA PROD SUPPLIER                 
         BE    LOG05               NO                                           
*                                                                               
         MVC   COMPXSUP(1),COMPSUP                                              
         MVC   COMPXSUP+1(1),ACMPXSUP                                           
         SPACE 1                                                                
LOG05    GOTO1 =A(GETJOB),DMCB,(RC),RR=RELO                                     
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFACOMF,COMFACS    INITIALIZE OFFAL BLOCK                       
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFACPY,COMPANY                                                  
         MVC   OFFACST1(OFFAOPOS-OFFACST1),COMPSTA1                             
         MVC   OFFALIMA,TWAACCS                                                 
         MVC   OFFAAUTH,TWAAUTH                                                 
         MVI   OFFAINDS,OFFAIOFF                                                
         MVI   OFFAACT,OFFAINI                                                  
         OC    OFFASAV(OFFASAVL),SAVEOFFA                                       
         BZ    *+8                 ITS NEVER BEEN SAVED BEFORE                  
         MVI   OFFAACT,OFFARES     RESTORE OFFAL BLOCK                          
         GOTO1 OFFAL                                                            
         BE    LOG07                                                            
         LA    R2,LOGRECH                                                       
         MVI   ERROR,193           SETUP OFFICE RECORDS                         
         B     LOGERRS                                                          
LOG07    MVC   SAVEOFFA,OFFASAV                                                 
         DROP  R1                                                               
         EJECT                                                                  
*              GET PF KEY VALUE                                                 
*                                                                               
GETPFK   L     RF,ASYSPARM                                                      
         USING TIOBD,RF                                                         
         XR    R1,R1                                                            
         ICM   R1,3,TIOBCURD       DISPLACEMENT TO CURSOR                       
         AR    R1,RA               FROM START OF TWA                            
         ST    R1,ACURSOR          ADDRESS OF CURSOR FIELD                      
         SR    R1,R1                                                            
         IC    R1,TIOBAID          GET THE PF KEY                               
         CLI   TIOBAID,12          IF MORE THAN 12                              
         BNH   *+8                                                              
         SH    R1,=H'12'           SUBTRACT 12                                  
         STC   R1,PFKEY            SAVE PF KEY NUMBER                           
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,TIOBLAST                                                    
         AR    R3,RA               R3 TO LAST MODIFIED FIELD                    
         LA    R4,LOGSERVH         R4 TO SERVICE                                
*                                                                               
         CLI   PFKEY,0             IF NO PF KEY                                 
         BE    LOG10                                                            
         CR    R3,R4               IF PF KEY USED AND FIELD MODIFIED            
         BNH   LOG10                                                            
         MVI   PFKEY,0             IGNORE PF KEY                                
         EJECT                                                                  
*              CHECK RECORD LETTER WITH LIST                                    
         SPACE 1                                                                
LOG10    LA    R2,LOGRECH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,NOTVLREC                                                   
         L     R7,RECLIST                                                       
         USING RECLSTD,R7                                                       
         SR    R3,R3                                                            
         CLI   LOGRECH+5,1         COMPARE FOR LENGTH OF ONE                    
         BE    LOG20                                                            
         LA    R3,1                NEVER MORE THAN TWO                          
         SPACE 1                                                                
LOG20    CLI   0(R7),X'FF'         END OF RECORD LIST                           
         BE    LOGERRS             INVALID RECORD TYPE                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   RECNAME(0),LOGREC   MATCH RECORD LIST TO INPUT RECORD            
         BE    LOG23                                                            
         LA    R7,RECLLEN(R7)                                                   
         B     LOG20                                                            
LOG23    MVC   LOGREC(9),RECNAME   PUT OUT FULL NAME                            
         OI    LOGRECH+6,X'80'                                                  
         TM    RECSTAT,AFM         CHECK IF REC/ACT MAY BE AFM ONLY             
         BZ    *+8                                                              
         OI    LFMINDS,LFMIAFM                                                  
         TM    RECSTAT,SPCL        CHECK IF AUTHORISED FOR SFM                  
         BZ    LOG25                                                            
         CLI   TWAOFFC,C'*'                                                     
         BE    LOG25                                                            
         B     LOGERRS             THIS IS SPECIAL AND NOT DDS TERMINAL         
         SPACE 1                                                                
LOG25    DC    0H'0'                                                            
         CLC   LOGREC(2),=C'PE'    PERSONNEL LEDGER                             
         BNE   LOG27                                                            
         TM    TWAAUTH,X'10'       PASSWORD MUST BE USED                        
         BNO   LOGERRS                                                          
         SPACE 1                                                                
LOG27    EQU   *                                                                
         TM    RECSTAT,PROD        IS THIS A PRODUCTION TYPE                    
         BZ    LOG29               BRANCH IF NOT                                
         TM    TWAAUTH,X'02'       IS TERMINAL AUTHORIZED FOR PRODPAK           
         BO    LOG29               BRANCH IF IT IS                              
         B     LOGERRS             HARD LUCK                                    
         SPACE 1                                                                
LOG29    CLC   LOGREC(2),=C'MO'    FOR MOS-LOCK,MUST HAVE HIGH SECURITY         
         BE    *+14                                                             
         CLC   =C'GS',LOGREC       ALSO FOR GST RULES                           
         BNE   LOG30                                                            
         CLI   TWAAUTH+1,100                                                    
         BL    LOGERRS                                                          
*                                                                               
LOG30    CLI   NEWPRD,C'Y'         ARE THEY ON NEWPROD?                         
         BNE   LOG35               NO, OK                                       
         TM    RECSTAT,NPRD        YES, IS RECORD SUPPORTED BY NEWPROD?         
         BZ    LOG35               NO, OK                                       
         MVC   LOGHEAD(50),=CL50'** ERROR ** MUST USE NEW PRODUCTION SYX        
               STEM'                                                            
         B     LOGEXIT                                                          
*                                                                               
LOG35    CLI   PFKEY,0             DID THEY USE A PF KEY                        
         BE    LOG40               NO, OK TO CONTINUE                           
         TM    RECSTAT,PFVAL       ARE PF KEYS VALID                            
         BO    LOG40               YES, OK TO CONTINUE                          
         L     R2,ACURSOR          SET CURSOR FIELD                             
         MVI   ERROR,251           SET INVALID PF KEY                           
         B     LOGERRS                                                          
         EJECT                                                                  
*              CHECK ACTION                                                     
         SPACE 1                                                                
LOG40    MVC   RECOREL,RECTYPE                                                  
         LA    R2,LOGACTH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,NOTVLACT                                                   
         LA    R4,ACTLIST                                                       
         CLI   LFMLANG,LANGGER                                                  
         BNE   *+8                                                              
         LA    R4,ACTLISTG                                                      
         SPACE 1                                                                
LOG41    CLI   0(R4),X'FF'         END OF ACTION LIST                           
         BE    LOGERRS             INVALID ACTION                               
         CLC   0(1,R4),LOGACT       MATCH ACTION LIST TO INPUT ACTION           
         BE    LOG42                                                            
         LA    R4,L'ACTLIST(R4)                                                 
         B     LOG41                                                            
         SPACE 1                                                                
*                                                                               
LOG42    MVC   LOGACT(9),0(R4)                                                  
         MVC   LFMACT,9(R4)                                                     
         OI    LOGACTH+6,X'80'                                                  
*                                                                               
         CLI   TWAOFFC,C'*'        ARE WE ON A DDS TERM?                        
         BE    LOG42A                                                           
         CLI   LOGACT,C'I'                                                      
         BE    LOG42A                                                           
         LA    R2,LOGRECH                                                       
         MVC   LOGHEAD(37),=C'ACTION NO LONGER AVAILABLE - USE =AFM'            
         B     LOGEXIT                                                          
*                                                                               
LOG42A   MVC   GERROR,=AL2(AFMONLY)                                             
         CLI   LOGACT,C'N'                                                      
         BE    *+12                                                             
         CLI   LOGACT,C'A'                                                      
         BNE   LOG45                                                            
         TM    LFMINDS,LFMIAFM                                                  
         BO    LOGERRX                                                          
*                                                                               
         USING XTRAINFD,R3                                                      
         L     R3,AXTRAINF                                                      
         TM    XIFLAG1,XIROMODE    CONNECTED IN READ ONLY MODE                  
         BNO   LOG43                                                            
         MVC   GERROR,=AL2(AE$NAUPD)                                            
         B     LOGERRX                                                          
*                                                                               
LOG43    TM    XIFLAG1,XIWRONGF    CONNECTED TO WRONG FACPAK                    
         BNO   LOG44                                                            
         MVC   GERROR,=AL2(AE$HMADV)                                            
         MVC   UPDADV,XIUPDFAC                                                  
         B     LOGERRX                                                          
*                                                                               
LOG44    TM    XIFLAG1,XIROSYS     CONNECTED TO READ ONLY SYSTEM                
         BNO   LOG45                                                            
         MVC   GERROR,=AL2(AE$UPDNO)                                            
         B     LOGERRX                                                          
*                                                                               
LOG45    MVI   ACTION,C'A'                                                      
         CLI   LOGACT,C'N'         NEW FOR AN ELEMENT ADDING                    
         BNE   LOG47               FACILITY IS REALLY AN AMEND                  
         TM    RECOREL,ELMT                                                     
         BO    LOG47                                                            
         MVI   ACTION,C'N'                                                      
         SPACE 1                                                                
LOG47    CLI   LOGACT,C'E'         IS ACTION INQUIRY OR ENQUIRY                 
         BE    LOG49                                                            
         CLI   LOGACT,C'I'                                                      
         BE    LOG49                                                            
         TM    TWAAUTH,X'01'       IF NOT MUST BE AUTHORIZED                    
         BZ    LOGERRS             FOR ADD OR CHANGE                            
         TM    RECSTAT,CLPR        IS SPECIAL AUTHORIZATION REQUIRED            
         BZ    LOG48               TO ADD/CHANGE CLIENT OR PRODUCT              
         TM    TWAAUTH,X'08'       YES                                          
         BZ    LOGERRS             BUT YOU AIN'T GOT IT                         
         SPACE 1                                                                
LOG48    CLI   LOGACT,C'C'         IS ACTION CLOSE                              
         BNE   LOG49                                                            
         CLC   RECNAME,=CL9'JOB'                                                
         BE    LOG49                                                            
         CLC   RECNAME,=CL9'MJOB'                                               
         BNE   LOGERRS             IF SO - RECORD MUST BE JOB/MJOB              
         SPACE 1                                                                
LOG49    DC    0H'0'                                                            
         CLC   RECNAME,=CL9'OFFICE'                                             
         BNE   LOG50                                                            
         CLI   LOGACT,C'E'         IS ACTION INQUIRY OR ENQUIRY                 
         BE    LOG50                                                            
         CLI   LOGACT,C'I'                                                      
         BE    LOG50                                                            
         TM    TWAAUTH,X'10'       PW NEEDED FOR ADD OR CHANGE                  
         BNO   LOGERRS                                                          
         EJECT                                                                  
*              NOW HANDLE PHASES                                                
         SPACE 1                                                                
LOG50    CLC   RECNAME,=CL9'ESTIMATE'                                           
         BNE   LOG51                                                            
         CLI   LOGACT,C'E'                                                      
         BE    LOG51                                                            
         CLI   LOGACT,C'I'                                                      
         BE    LOG51                                                            
         LA    R7,RECLLEN(R7)      BUMP TO NEXT ENTRY IN LIST                   
         CLI   LOGACT,C'A'         ACTION AMEND?                                
         BE    LOG51               OK                                           
         MVI   ERROR,NOTVLACT      NO, NEW NOT ALLOWED ANYMORE                  
         B     LOGERRS                                                          
         SPACE 1                                                                
LOG51    MVI   ERROR,X'FF'                                                      
         MVI   RETURN,0                                                         
         CLI   LOGTABH,0                                                        
         BNE   *+8                                                              
         MVI   LASTSEQ,0           NO SAVED SCREEN IN TWA                       
         CLC   RECSEQ,LASTSEQ                                                   
         BE    LOG75               ALREADY HAVE SCREEN                          
         LR    R6,RA                                                            
         USING TWAD,R6                                                          
         XC    TWAXTRA,TWAXTRA    CLEAR ANY SAVED SWITCHES                      
         GOTO1 SCANKEY,DMCB,(RC),(R7)   LOAD SCREEN - FILL IN KEY               
         CLI   ERROR,X'FF'                                                      
         BNE   LOGERRS                NICE TRY                                  
         CLI   RETURN,0                                                         
         BNE   LOG75    SCANKEY HAS FILLED IN SCREEN GO TO APPLICATION          
         LA    R2,LOGTABH          IF NO INPUT SET CURSOR TO                    
         SR    R4,R4               FIRST UNPROTECTED FIELD                      
         IC    R4,0(R2)                                                         
         AR    R2,R4                                                            
         TM    1(R2),X'20'                                                      
         BO    *-10                                                             
         SPACE 1                                                                
         TM    RECOREL,MAC                                                      
         BO    LOG75               SPECIAL PHASE HANDLE OWN MESSAGES.           
         TM    RECOREL,NOKEY                                                    
         BO    LOG75               SPECIAL PHASE HANDLE OWN MESSAGES.           
         SPACE 1                                                                
LOG70    DS    0H                                                               
         LA    R3,OKMESS1                                                       
         CLI   LOGACT,C'E'                                                      
         BE    OKEXIT                                                           
         CLI   LOGACT,C'I'                                                      
         BE    OKEXIT                                                           
         LA    R3,OKMESS2                                                       
         CLI   LOGACT,C'N'                                                      
         BE    OKEXIT                                                           
         LA    R3,OKMESS3                                                       
         B     OKEXIT                                                           
         SPACE 1                                                                
LOG75    MVC   PHASE,RECPHASE                                                   
         GOTO1 CALLOV,DMCB,(PHASE,0),(0,TWA)                                    
         MVI   ERROR,CANTOVLY                                                   
         CLI   DMCB+4,X'FF'                                                     
         BE    LOGERRS                                                          
         TM    RECOREL,MAC                                                      
         BZ    LOG80                                                            
         MVI   MODE,0                                                           
         MVC   APPLIC,DMCB                                                      
         CLI   RETURN,0                                                         
         BE    LOG77               NO DATA IN KEY FIELD                         
         BAS   RE,GO               IF DATA  MODE 0 THAN DISPLAY                 
LOG77    BAS   RE,GO               SPECIAL PHASEES HANDLE OWN I/O AND           
         CLI   MODE,DSPLYREC       MODE SETTINGS.                               
         BE    LOG90                                                            
         CLI   MODE,BUILDREC                                                    
         BE    LOG105                                                           
         CLI   MODE,CLOSEJOB                                                    
         BE    LOG96                                                            
         B     LOG70                                                            
         EJECT                                                                  
*              NOW CONTROL COMMUNICATION WITH APPLICATION                       
         SPACE 1                                                                
LOG80    MVC   APPLIC,DMCB                                                      
         MVI   ANYKEY,C'N'                                                      
         MVI   MODE,BUILDKEY                                                    
         BAS   RE,GO                                                            
*                                                                               
         CLI   NEWPRD,C'Y'         TEST NEW PRODUCTION                          
         BNE   LOG81               NO                                           
         CLI   RECPHASE,X'07'      TEST FOR WORK-CODE                           
         BNE   LOG81               NO                                           
         CLC   COMPJOB,KEY+2       TEST FOR PRODUCTION WORKCODE                 
         BNE   LOG81                                                            
         LA    R2,LOGRECH                                                       
         MVC   LOGHEAD(50),=CL50'** ERROR ** MUST USE NEW PRODUCTION SYX        
               STEM'                                                            
         B     LOGEXIT                                                          
*                                                                               
LOG81    DS    0H                                                               
         CLC   LOGREC(2),=C'LE'                                                 
         BE    LOG83                                                            
         CLC   LOGREC(2),=C'CH'                                                 
         BE    LOG83                                                            
         CLI   KEY,X'40'                                                        
         BL    LOG83                                                            
         CLC   KEY+1(2),=C'SM'                                                  
         BE    *+14                                                             
         CLC   KEY+1(2),=C'SN'                                                  
         BNE   LOG83                                                            
         LA    R2,LOGRECH                                                       
         MVI   ERROR,NOTVLREC                                                   
         B     LOGERRS                                                          
LOG83    EQU   *                                                                
         CLI   ACTION,C'N'         NEW RECORD                                   
         BNE   LOG85                                                            
         TM    RECOREL,NOIO                                                     
         BO    LOG110                                                           
         OI    DMINBTS,X'08'       PASS DELETES AS WELL                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(15),KEYSAVE     IS KEY ON FILE YET                           
         BNE   LOG107              NO - THEN ITS OK TO ADD                      
         TM    IO+(ACSTATUS-ACKEYD),X'80' OR IF IT'S MARKED DELETED             
         BO    LOG107                                                           
         MVI   ERROR,RECONFLE                                                   
         B     LOGERRS                                                          
         SPACE 1                                                                
LOG85    CLI   LOGACT,C'A'                                                      
         BNE   LOG85A                                                           
         BAS   RE,LEDGLOCK                                                      
         CLI   ERROR,X'FF'                                                      
         BNE   LOGERRS                                                          
*                                                                               
LOG85A   TM    RECOREL,NOIO                                                     
         BZ    LOG87                                                            
         CLI   ANYKEY,C'Y'                                                      
         BE    LOG89                                                            
         CLI   LOGACT,C'A'                                                      
         BNE   LOG89                                                            
         B     LOG110                                                           
         SPACE 1                                                                
LOG87    CLI   LOGACT,C'C'         IF CLOSE - READ FOR UPDATE                   
         BE    LOG87A                                                           
         CLI   LOGACT,C'N'         MUST BE NEW ELEMENT - LOCK RECORD            
         BE    LOG87A                                                           
         CLI   ANYKEY,C'Y'                                                      
         BE    LOG88               KEY CHANGE - CAN'T UPDATE                    
         CLI   LOGACT,C'A'         IF AMEND READ FOR UPDATE                     
         BNE   LOG88                                                            
LOG87A   MVI   UPDATE,C'Y'         READ FOR UPDATE                              
LOG88    GOTO1 READ                                                             
         MVC   DMWORK2(96),DMWORK                                               
         LA    R0,IO2                                                           
         LA    R1,IOLENQ                                                        
         LA    RE,IO                                                            
         LA    RF,IOLENQ                                                        
         MVCL  R0,RE                                                            
         TM    RECSTAT,NISA        TREAT NEW AS AMEND                           
         BO    *+12                                                             
         CLI   LOGACT,C'N'                                                      
         BE    LOG100                                                           
         CLI   LOGACT,C'E'                                                      
         BE    LOG89                                                            
         CLI   LOGACT,C'I'                                                      
         BE    LOG89                                                            
         CLI   LOGACT,C'C'                                                      
         BE    LOG95                                                            
         CLI   ANYKEY,C'Y'         DISPLAY RECORD FOR ENQUIRY OR CHANGE         
         BNE   LOG95                                                            
         SPACE 1                                                                
LOG89    DS    0H                                                               
         MVI   MODE,DSPLYREC                                                    
         BAS   RE,GO                                                            
LOG90    LA    R3,OKMESS5                                                       
         TM    RECOREL,NOKEY                                                    
         BO    LOG93                                                            
         B     LOG94                                                            
LOG93    LA    R3,OKMESS8                                                       
LOG94    CLI   LOGACT,C'E'                                                      
         BE    RESETEX                                                          
         CLI   LOGACT,C'I'                                                      
         BE    RESETEX                                                          
         LA    R3,OKMESS4                                                       
         B     OKEXIT                                                           
         SPACE 1                                                                
LOG95    CLI   LOGACT,C'C'         CLOSE A JOB                                  
         BNE   LOG97                                                            
         MVI   MODE,CLOSEJOB                                                    
         BAS   RE,GO                                                            
         MVI   MODE,ADDAREQ                                                     
         BAS   RE,GO                                                            
         GOTO1 PUTREC                                                           
LOG96    LA    R3,OKMESS6                                                       
         B     RESETEX                                                          
         SPACE 1                                                                
LOG97    TM    RECOREL,ELMT                                                     
         BO    LOG100                                                           
         BAS   RE,CLEARELS                                                      
         B     LOG109                                                           
         SPACE 1                                                                
LOG100   MVI   MODE,NEWELEM                                                     
         CLI   LOGACT,C'N'                                                      
         BE    LOG103                                                           
         MVI   MODE,CHNGELEM                                                    
         SPACE 1                                                                
LOG103   BAS   RE,LEDGLOCK         SEE IF LEDGER IS LOCKED                      
         CLI   ERROR,X'FF'                                                      
         BNE   LOGERRS                                                          
         BAS   RE,GO                                                            
         GOTO1 PUTREC                                                           
LOG105   LA    R3,OKMESS7                                                       
         CLI   LOGACT,C'N'                                                      
         BE    RESETEX                                                          
         LA    R3,OKMESS6                                                       
         B     RESETEX                                                          
         SPACE 1                                                                
LOG107   LA    R0,IO2                                                           
         LA    R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   KEY,KEYSAVE                                                      
         SPACE 1                                                                
LOG109   MVC   IO2(15),KEY                                                      
         SPACE 1                                                                
LOG110   MVI   MODE,BUILDREC                                                    
         BAS   RE,GO                                                            
         CLI   LOGREC,C'J'                                                      
         BNE   LOG113                                                           
         MVI   MODE,ADDAREQ                                                     
         BAS   RE,GO                                                            
         SPACE 1                                                                
LOG113   DS    0H                                                               
         CLI   ACTION,C'N'                                                      
         BE    LOG117                                                           
         TM    RECOREL,NOIO                                                     
         BO    LOG115                                                           
         GOTO1 REMANEL,DMCB,0                                                   
         GOTO1 UPREC,1             PUT RECORD                                   
*                                                                               
LOG115   LA    R3,OKMESS6                                                       
         B     RESETEX                                                          
                                                                                
LOG117   TM    RECOREL,NOIO                                                     
         BO    LOG120                                                           
         GOTO1 STATIN                                                           
         GOTO1 UPREC,0             ADD RECORD                                   
*                                                                               
LOG120   LA    R3,OKMESS7                                                       
         B     RESETEX                                                          
         EJECT                                                                  
*                                                                               
*              ROUTINE TO CALL SEARCH NAME PASSIVE MAINTENANCE MODULE           
*                                                                               
PUTSRC   TM    RECOREL,SRCH        TEST SEARCH AMINT REQUIRED                   
         BZR   RE                                                               
         MVI   DMCB,C'C'                                                        
         TM    LFMINDS,LFMINMCH    TEST NAME CHANGED                            
         BO    PUTSRC02                                                         
         MVI   DMCB,C'A'                                                        
         CLI   ACTION,C'N'         TEST RECORD ADDED/RESTORED                   
         BNE   PUTSRC01                                                         
         TM    LFMINDS,LFMIARES    TEST RESTORE, NOT ADD                        
         BZ    PUTSRC02                                                         
         MVI   DMCB,C'R'                                                        
         B     PUTSRC02                                                         
                                                                                
PUTSRC01 LA    RF,IO2                                                           
         TM    ACCOSTAT(RF),X'80'  TEST RECORD DELETED                          
         BZR   RE                                                               
         MVI   DMCB,C'D'                                                        
                                                                                
PUTSRC02 LR    R0,RE                                                            
         MVC   DMCB+1(3),=CL3'TF '                                              
         XR    RF,RF                                                            
         ICM   RF,3,DSAVNAM                                                     
                                                                                
         GOTO1 =V(ACSRCHP),DMCB,,IO2,,LOCALS(RF),COMFACS,ACCFACS,      *        
               RR=RELO                                                          
                                                                                
PUTSRCX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO HANDLE RECORD UPDATES                                              
* AT ENTRY, R1=0 FOR ADD, R1=1 FOR PUT                                          
*                                                                               
UPREC    NTR1  WORK=(R5,RAPPERX-RAPPERD)                                        
         USING RAPPERD,R5                                                       
         STC   R1,RAADDSW          SAVE ADD/PUT FLAG                            
         LA    RF,IO2                                                           
         MVI   RAACTIVE,C'N'       SET RECORD ACTIVITY FLAG                     
*                                                                               
         CLI   RAPTR,C'Y'          TEST FOR RECORD ACTIVITY POINTERS            
         BNE   UPREC20             NO                                           
*                                                                               
         MVI   RARTYP,RAPKRLED     RECORD TYPE = LEDGER                         
         CLI   RECPHASE,2          TEST LEDGER MAINTENANCE                      
         BE    UPREC10                                                          
*                                                                               
         CLI   RECPHASE,3          TEST ACCOUNT MAINTENANCE                     
         BNE   UPREC40                                                          
*                                                                               
         CLC   =C'1R',1(RF)        ARE WE UPDATING U/L 1R                       
         BNE   UPREC05                                                          
         GOTO1 =A(GETLEVS),DMCB,(RC),(RF),RR=RELO                               
         LA    RF,IO2                                                           
*                                                                               
         MVI   RARTYP,RAPKR1RA                                                  
         LA    RE,ACTKACT-ACTKEY(RF) POINT AT START OF ACCT CODE                
         LA    R3,LEVELS             POINT AT FIRST LEVEL LENGTH                
         ZIC   R1,0(R3)            GET LENGTH OF CLIENT CODE                    
         AR    RE,R1               RE=A(PRODUCT CODE)                           
         CLI   0(RE),C' '          TEST FOR A 1R OFFICE                         
         BE    UPREC10             YES                                          
*                                                                               
         MVI   RARTYP,RAPKR1RB                                                  
         LA    RE,ACTKACT-ACTKEY(RF) POINT AT START OF ACCT CODE                
         LA    R3,LEVELS             POINT AT FIRST LEVEL LENGTH                
         ZIC   R1,1(R3)            GET LENGTH OF SECOND LEVEL                   
         AR    RE,R1               RE=A(PRODUCT CODE)                           
         CLI   0(RE),C' '          TEST FOR A 1R OFFICE                         
         BE    UPREC10             YES                                          
*                                                                               
         MVI   RARTYP,RAPKR1RC                                                  
         LA    RE,ACTKACT-ACTKEY(RF) POINT AT START OF ACCT CODE                
         LA    R3,LEVELS             POINT AT FIRST LEVEL LENGTH                
         ZIC   R1,2(R3)            GET LENGTH OF 1ST+2ND+3RD LEVEL              
         AR    RE,R1               RE=A(PRODUCT CODE)                           
         CLI   0(RE),C' '          TEST FOR A 1R OFFICE                         
         BE    UPREC10             YES                                          
*                                                                               
UPREC05  MVI   RARTYP,RAPKRSUP     RECORD TYPE = SUPPLIER                       
         CLC   COMPSUP,1(RF)       TEST SUPPLIER LEDGER                         
         BE    UPREC10                                                          
*                                                                               
         OC    COMPXSUP,COMPXSUP                                                
         BZ    *+14                                                             
         CLC   COMPXSUP,1(RF)      TEST EXTRA PROD SUPPLIER                     
         BE    UPREC10                                                          
*                                                                               
         CLC   COMPJOB,1(RF)       TEST PRODUCTION LEDGER                       
         BNE   UPREC20                                                          
*                                                                               
         MVI   RARTYP,RAPKRCLI                                                  
         LA    RE,ACTKACT-ACTKEY(RF) POINT AT START OF ACCT CODE                
         ZIC   R1,COMPLCLI         GET LENGTH OF CLIENT CODE                    
         AR    RE,R1               RE=A(PRODUCT CODE)                           
         CLI   0(RE),C' '          TEST FOR A CLIENT                            
         BE    UPREC10             YES                                          
*                                                                               
         MVI   RARTYP,RAPKRPRO                                                  
         LA    RE,ACTKACT-ACTKEY(R1,RF)                                         
         ZIC   R1,COMPLPRD                                                      
         AR    RE,R1               POINT AT FIRST BYTE OF JOB CODE              
         CLI   0(RE),C' '          TEST FOR A PRODUCT                           
         BE    UPREC10                                                          
         MVI   RARTYP,RAPKRJOB                                                  
*                                                                               
UPREC10  MVI   RAACTIVE,C'Y'       NOTE GENERATING ACTIVITY POINTERS            
         OI    LFMINDS,LFMIRADD    YES ADD RA POINTER                           
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM                                                 
         MVC   RAPCPY,COMPANY                                                   
         MVC   RAPRTYP,RARTYP      RECORD TYPE                                  
         MVI   RAPEMU,C'Y'                                                      
         MVC   RAPACOM,COMFACS                                                  
         LA    RE,IO2                                                           
         ST    RE,RAPAREC                                                       
         GOTO1 RAPPER,RAPBLK                                                    
         BE    UPREC20                                                          
         DC    H'0'                                                             
*                                                                               
UPREC20  DS    0H                                                               
         LA    RF,IO2                                                           
         CLC   =C'1C',1(RF)       ADD ACCENT POINTER ONLY FOR 1C                
         BNE   UPREC40                                                          
*                                                                               
         TM    COMPSTAA,CPYSACCT   IS COMPANY USING ACCENT                      
         BNO   UPREC40                                                          
*                                                                               
         TM    LFMINDS,LFMIRADD    DID WE ALREADY ADD RAPTR                     
         BO    UPREC40                                                          
*                                                                               
         CLI   RAADDSW,0           R V ADDING A NEW RECORD                      
         BE    UPREC30             YES ADD RA POINTER                           
         TM    LFMINDS,LFMINMCH    TEST NAME CHANGED                            
         BO    UPREC30                                                          
         TM    LFMINDS,LFMIFLCH    HAS FILTER CHANGED OR ADDED                  
         BNO   UPREC40                                                          
UPREC30  DS    0H                                                               
         MVI   RARTYP,RAPKRPAL     ACCENT USER                                  
         B     UPREC10                                                          
*                                                                               
UPREC40  L     RF,ADDREC                                                        
         CLI   RAADDSW,0           TEST TO ADD RECORD                           
         BE    *+8                 YES                                          
         L     RF,PUTREC                                                        
*                                                                               
         BASR  RE,RF                                                            
*                                                                               
         BAS   RE,PUTSRC                                                        
         CLI   RAACTIVE,C'Y'                                                    
         BNE   UPRECX                                                           
*                                                                               
         MVI   RAPACTN,RAPAPTR                                                  
         GOTO1 RAPPER,RAPBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPRECX   B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*              TEST IF LEDGER CURRENTLY LOCKED                                  
*                                                                               
LEDGLOCK NTR1                                                                   
         MVI   ERROR,X'FF'                                                      
         TM    RECTYPE,TSTLCK      NECESSARY TO CHECK IF LEDGER LOCKED?         
         BZ    XIT                                                              
         GOTO1 CALLOV,DMCB,0,X'D9000A66',0         GET A(SETLOCK)               
         L     RF,0(R1)                                                         
         LA    R0,KEY                                                           
         CLI   KEY,CHAKTYPQ        X'10' CHECK RECORD                           
         BNE   *+8                                                              
         LA    R0,KEY+1                                                         
         GOTO1 (RF),DMCB,(C'T',(R0)),COMFACS                                    
         CLI   DMCB+4,0                                                         
         BE    XIT                                                              
         MVI   ERROR,42            LEDGER IS CURRENTLY LOCKED                   
         CLI   DMCB+4,X'80'        DMCB+4 = LEDGER STATUS BYTE                  
         BE    XIT                                                              
         MVI   ERROR,NORECFND                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO TRIM DOWN IO2 FOR REBUILD                             
         SPACE 1                                                                
CLEARELS NTR1                                                                   
         LA    R2,IO2                                                           
         AH    R2,DATADISP                                                      
         SR    R3,R3                                                            
         SPACE 1                                                                
         CLC   LOGREC(2),=C'CH'    NO TRIMMING FOR OFFICE-CHECK                 
         BE    CLEAREL4                                                         
         CLC   LOGREC(2),=C'AP'    OR APG- RULES                                
         BE    CLEAREL4                                                         
CLEAREL2 CLI   0(R2),0                                                          
         BE    CLEAREL4                                                         
         CLI   0(R2),X'12'         ELEMENTS X'12' AND                           
         BNE   *+8                                                              
         MVI   0(R2),X'FF'                                                      
         CLI   0(R2),X'14'         ELEMENTS X'14'                               
         BL    CLEAREL3                                                         
         CLI   0(R2),X'15'                                                      
         BE    CLEAREL3            SAVE POSTING RULES                           
         CLI   0(R2),X'18'                                                      
         BE    CLEAREL3            KEEP X'18' ELEMENT                           
         CLI   0(R2),X'22'                                                      
         BE    CLEARL2B                                                         
         CLI   0(R2),X'23'                                                      
         BE    CLEARL2B                                                         
         CLI   0(R2),X'24'         FOR PROFILES                                 
         BE    CLEARL2A                                                         
         CLI   0(R2),X'25'         CLEAR 25 ON ACCT ONLY                        
         BE    CLEARL2B                                                         
         CLI   0(R2),X'26'         KEEP 26 ELEMENT                              
         BE    CLEAREL3                                                         
         CLI   0(R2),X'27'         CLEAR 27 ON ACCT ONLY                        
         BE    CLEARL2B                                                         
         CLI   0(R2),X'2F'         TO X'2F' ARE REBUILT                         
         BH    CLEAREL3                                                         
         MVI   0(R2),X'FF'                                                      
         B     CLEAREL3                                                         
         SPACE 1                                                                
CLEARL2A CLI   LOGREC,C'A'         DONT REMOVE WHEN RECORD = ACCOUNT            
         BE    CLEAREL3                                                         
         MVI   0(R2),X'FF'                                                      
         B     CLEAREL3                                                         
         SPACE 1                                                                
CLEARL2B CLI   LOGREC,C'A'         DONT REMOVE UNLESS RECORD=ACCOUNT            
         BNE   CLEAREL3                                                         
         MVI   0(R2),X'FF'                                                      
         SPACE 1                                                                
CLEAREL3 DS    0H                                                               
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     CLEAREL2                                                         
         SPACE 1                                                                
CLEAREL4 GOTO1 REMANEL,DMCB,0                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET UP WORKING STORAGE                                
         SPACE 1                                                                
SETWORK  NTR1                                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         ST    R1,FAPARM           SAVE A(FACPAK PARAMETER LIST).               
         L     RA,4(R1)            INTERPRET SE PARAMETER LIST                  
         ST    RA,TWA                                                           
         MVC   COMPANY,0(R1)                                                    
         L     RE,0(R1)                                                         
         LA    RE,0(RE)                                                         
         ST    RE,ASYSPARM                                                      
         L     R2,8(R1)                                                         
         MVC   DATAMGR(24),0(R2)   FACILITY LIST                                
         ST    R2,ACCFACS          SAVE A(ACCPAK FACILITIES LIST).              
         MVC   COMFACS,16(R1)      SAVE A(COMMON FACILITIES LIST).              
         MVC   TERMINAL,0(RA)                                                   
         MVI   LEVEL,1                                                          
         ST    RB,LOGBASE                                                       
         BAS   RE,SETEXTN                                                       
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A62'                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   OFFAL,0(R1)                                                      
         LH    R1,=Y(OFFBLK-LOGWORKD)                                           
         LA    R1,LOGWORKD(R1)                                                  
         ST    R1,AOFFBLK          OFFAL BLOCK ADDRESS                          
*                                                                               
         LA    R2,AREAD            RELOCATE COMMON ADDRESS                      
         LA    R3,READ                                                          
         LA    R4,NUMROUTS                                                      
         SPACE 1                                                                
SETWORK2 L     R5,0(R2)                                                         
         A     R5,RELO                                                          
         ST    R5,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SETWORK2                                                      
         MVI   DMINBTS,X'00'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         MVC   DATADISP,=H'49'                                                  
         DATE  DUB,DATE=NO         GET COUNTRY                                  
         MVC   COUNTRY,DUB+6                                                    
         MVI   UPDATE,C'N'                                                      
         SPACE 1                                                                
         L     RF,COMFACS          AND LANGUAGE                                 
         MVC   AXTRAINF,CXTRAINF-COMFACSD(RF)                                   
         MVC   GETTXT,CGETTXT-COMFACSD(RF)                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)                                                         
         MVC   LFMLANG,FALANG-FACTSD(RF)                                        
         MVC   LFMCTRY,FACTRY-FACTSD(RF)                                        
         SPACE 1                                                                
         MVC   DMCB-8(8),=C'**DMCB**'                                           
         MVC   MODE-4(4),=C'MODE'                                               
         MVC   READ-8(8),=C'*COMMON*'                                           
         MVC   KEY-8(8),=C'**KEYS**'                                            
         MVC   ELEMENT-8(8),=C'**ELEM**'                                        
         MVC   IO-8(8),=C'**IO 1**'                                             
         MVC   IO2-8(8),=C'**IO 2**'                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET EXTERNALS                                         
SETEXTN  NTR1                                                                   
         L     R2,=A(CRECLIST)                                                  
         A     R2,RELO                                                          
         ST    R2,RECLIST                                                       
         L     R2,=A(CSCANKEY)                                                  
         A     R2,RELO                                                          
         ST    R2,SCANKEY                                                       
         L     R2,=V(ACRAPPER)                                                  
         A     R2,RELO                                                          
         ST    R2,RAPPER                                                        
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATION ROUTINES                                              
         SPACE 1                                                                
VVALIDT  NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         GOTO1 DATVAL,DMCB,8(R2),DUB                                            
         L     R1,DMCB                                                          
         LTR   R1,R1                                                            
         BNZ   XITR1                                                            
         MVI   ERROR,DATERR                                                     
         B     LOGERRS                                                          
         SPACE 1                                                                
VVALICSH NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   DMCB,0                                                           
         BE    VVALCSH2                                                         
         MVI   ERROR,CASHERR                                                    
         B     LOGERRS                                                          
         SPACE 1                                                                
VVALCSH2 L     R1,DMCB+4                                                        
         CVD   R1,DUB                                                           
         B     XITR1                                                            
         SPACE 1                                                                
VANY     NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    VANY2                                                            
         BCTR  R1,0                                                             
         EX    R1,VANYCLC          SPACES ONLY = MISSING                        
         BE    VANY2                                                            
         LA    R1,1(R1)                                                         
         B     XITR1                                                            
         SPACE 1                                                                
VANYCLC  CLC   8(0,R2),SPACES                                                   
         SPACE 1                                                                
VANY2    DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         B     LOGERRS                                                          
         SPACE 1                                                                
VNUMERIC NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         TM    4(R2),X'08'                                                      
         BO    VPACK2                                                           
         MVI   ERROR,NOTNUMRC                                                   
         B     LOGERRS                                                          
         EJECT                                                                  
*              OTHER DATA HANDLING ROUTINES                                     
         SPACE 1                                                                
VPACK    NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
VPACK2   SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         SR    R1,R1                                                            
         ZAP   DUB,=P'0'                                                        
         LTR   R3,R3                                                            
         BZ    XITR1                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         B     XITR1                                                            
         SPACE 1                                                                
VMOVE    NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         MVC   WORK,SPACES                                                      
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
*              ROUTINE TO GET NAME OUT OF A RECORD                              
         SPACE 1                                                                
VNAMOUT  NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         SR    R3,R3                                                            
         IC    R3,0(R2)                                                         
         AR    R3,R2                                                            
         MVC   8(36,R3),SPACES                                                  
         OI    6(R3),X'80'                                                      
         LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
         SR    R5,R5                                                            
         SPACE 1                                                                
VNAMOUT2 CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),X'20'                                                      
         BNE   VNAMOUT4                                                         
         USING ACNAMED,R4                                                       
         IC    R5,ACNMLEN                                                       
         LTR   R5,R5                                                            
         BNZ   *+8                                                              
         LA    R5,38                                                            
         CH    R5,=H'38'                                                        
         BL    *+8                                                              
         LA    R5,38                                                            
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     XIT                                                              
         MVC   8(0,R3),ACNMNAME                                                 
         SPACE 1                                                                
VNAMOUT4 IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     VNAMOUT2                                                         
         EJECT                                                                  
*              ROUTINE TO GET AN ADDRESS OUT OF A RECORD                        
         SPACE 1                                                                
VADDROUT NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         MVI   ELEMENT,C' '                                                     
         MVC   ELEMENT+1(L'ELEMENT-1),ELEMENT                                   
         LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
         SR    R5,R5                                                            
         SPACE 1                                                                
VADDR2   CLI   0(R4),0                                                          
         BE    VADDR6                                                           
         CLI   0(R4),X'22'                                                      
         BNE   VADDR4                                                           
         USING ACADDD,R4                                                        
         LA    R3,ACADADD                                                       
         IC    R5,ACADLNES                                                      
         LTR   R5,R5                                                            
         BNP   XIT                                                              
         LA    R6,ELEMENT                                                       
         MVC   0(26,R6),0(R3)                                                   
         LA    R3,26(R3)                                                        
         LA    R6,26(R6)                                                        
         BCT   R5,*-14                                                          
         B     VADDR6                                                           
         SPACE 1                                                                
VADDR4   IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     VADDR2                                                           
         SPACE 1                                                                
VADDR6   LA    R3,ELEMENT                                                       
         LR    R4,R2                                                            
         LA    R5,4                                                             
         SPACE 1                                                                
VADDR8   CLC   0(26,R3),8(R4)                                                   
         BE    VADDR10                                                          
         MVC   8(26,R4),0(R3)                                                   
         OI    6(R4),X'80'                                                      
         SPACE 1                                                                
VADDR10  LA    R3,26(R3)                                                        
         SR    R6,R6                                                            
         IC    R6,0(R4)            BUMP TWO HEADERS                             
         AR    R4,R6                                                            
         IC    R6,0(R4)                                                         
         AR    R4,R6                                                            
         BCT   R5,VADDR8                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD A NAME ELEMENT                                  
         SPACE 1                                                                
VNAMIN   NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         GOTO1 REMANEL,DMCB,(X'20',0)                                           
         LA    R4,ELEMENT                                                       
         USING ACNAMED,R4                                                       
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R3,2(R3)                                                         
         MVI   ACNMEL,X'20'                                                     
         STC   R3,ACNMLEN                                                       
         MVC   ACNMNAME(36),8(R2)                                               
         GOTO1 ADDANEL                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD AN ADDRESS ELEMENT                              
         SPACE 1                                                                
VADDRIN  NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         GOTO1 REMANEL,DMCB,(X'22',0)                                           
         LA    R4,ELEMENT                                                       
         USING ACADDD,R4                                                        
         LA    R5,ACADADD                                                       
         SR    R6,R6                                                            
         LA    R7,4                                                             
         SR    R3,R3                                                            
         SPACE 1                                                                
VADDRIN2 CLI   5(R2),0                                                          
         BZ    VADDRIN4                                                         
         GOTO1 MOVE                                                             
         MVC   0(26,R5),WORK                                                    
         LA    R5,26(R5)                                                        
         IC    R3,0(R2)            BUMP TWO HEADERS                             
         AR    R2,R3                                                            
         IC    R3,0(R2)                                                         
         AR    R2,R3                                                            
         LA    R6,1(R6)                                                         
         BCT   R7,VADDRIN2                                                      
         SPACE 1                                                                
VADDRIN4 LTR   R6,R6                                                            
         BZ    XIT                                                              
         MVI   ACADEL,X'22'                                                     
         STC   R6,ACADLNES                                                      
         MH    R6,=H'26'                                                        
         LA    R6,3(R6)                                                         
         STC   R6,ACADLEN                                                       
         GOTO1 ADDANEL                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
VADDANEL NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         GOTO1 =V(ACPUTEL),DMCB,IO2,ELEMENT,RR=RELO                             
         BAS   RE,GETLNGTH                                                      
         LA    RF,IO2                                                           
         CLC   42(2,RF),=Y(IOLENQ)                                              
         BNH   XIT                                                              
         LA    R2,LOGRECH                                                       
         MVI   ERROR,66                                                         
         B     LOGERRS                                                          
         EJECT                                                                  
*              ROUTINE TO REMOVE AN ELEMENT                                     
         SPACE 1                                                                
VREMANEL NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         SR    R5,R5                                                            
         SPACE 1                                                                
VREMEL2  CLI   0(R4),0                                                          
         BE    VREMEL4                                                          
         CLC   0(1,R4),0(R1)                                                    
         BNE   *+8                 REMOVE ELEMENTS THAT MATCH                   
         MVI   0(R4),X'FF'                                                      
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     VREMEL2                                                          
         SPACE 1                                                                
VREMEL4  GOTO1 =V(ACDELEL),DMCB,IO2,RR=RELO                                     
         LA    R6,IO2                                                           
         AH    R6,DATADISP                                                      
         SPACE 1                                                                
VREMEL5  CLI   0(R6),0             FIND END OF NEW RECORD                       
         BE    VREMEL6                                                          
         IC    R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     VREMEL5                                                          
         SPACE 1                                                                
VREMEL6  MVI   0(R6),0             CLEAR BALANCE                                
         CR    R6,R4                                                            
         BE    XIT                                                              
         LA    R6,1(R6)                                                         
         B     VREMEL6                                                          
         EJECT                                                                  
*              ROUTINE TO ADD A BALANCE ELEMENT                                 
         SPACE 1                                                                
VBALIN   NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         LA    R4,ELEMENT                                                       
         USING ACBALD,R4                                                        
         MVI   ACBLEL,X'32'                                                     
         MVI   ACBLLEN,ACBLLNQ                                                  
         ZAP   ACBLFRWD,=P'0'                                                   
         ZAP   ACBLDR,=P'0'                                                     
         ZAP   ACBLCR,=P'0'                                                     
         ZAP   ACBLURG,=P'0'                                                    
         GOTO1 ADDANEL                                                          
         SPACE 1                                                                
         USING ACPEELD,R4                                                       
         MVC   ACPEEL(2),=X'3314'                                               
         XC    ACPEPLDT(6),ACPEPLDT                                             
         ZAP   ACPEDR,=P'0'                                                     
         ZAP   ACPECR,=P'0'                                                     
         GOTO1 ADDANEL                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD A STATUS ELEMENT                                  
         SPACE 1                                                                
VSTATIN  NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         LA    R4,ELEMENT                                                       
         USING ACSTATD,R4                                                       
         LA    R2,IO2                                                           
         AH    R2,DATADISP                                                      
         SR    R3,R3                                                            
         SPACE 1                                                                
VSTATIN2 CLI   0(R2),X'30'                                                      
         BE    VSTATIN6                                                         
         CLI   0(R2),0                                                          
         BE    VSTATIN4                                                         
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     VSTATIN2                                                         
         SPACE 1                                                                
VSTATIN4 XC    ELEMENT,ELEMENT                                                  
         MVC   ACSTEL(2),=X'301D'                                               
         GOTO1 DATCON,DMCB,(5,0),(1,ACSTLAST)                                   
         MVC   ACSTBFDT,ACSTLAST                                                
         MVI   ACSTANAL,C' '                                                    
         MVC   ACSTFILT,SPACES                                                  
         MVI   ACSTCOST,C' '                                                    
         GOTO1 ADDANEL                                                          
         MVI   ACSTCOST,C' '                                                    
         B     XIT                                                              
         SPACE 1                                                                
VSTATIN6 XC    ELEMENT,ELEMENT                                                  
         IC    R3,1(R2)            CURRENT LENGTH                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R2)    NOW I HAVE THE EXISTING 30                   
         MVI   ELEMENT+1,ACSTLNQ3  PUT IN NEW LENGTH                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK IF NAME CHANGED AND ADD POINTER                 
VCHKNAM  NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         CLI   ACCEMU,C'Y'         TEST EMULATING OLD FILE                      
         BNE   VCHKNX                                                           
         L     RF,0(R1)            P1=A(RECORD)                                 
         L     RE,4(R1)            P2=A(SAVED NAME)                             
         CLI   0(R1),C'B'          P1 BYTE 0 B=BEFORE                           
         BE    VSAVNAM                                                          
         LR    R2,RF               R2=A(RECORD)                                 
         SR    R1,R1                                                            
         AH    RF,DATADISP                                                      
         USING NAMELD,RF                                                        
VCHKN2   CLI   NAMEL,0                                                          
         BE    VCHKNX                                                           
         CLI   NAMEL,NAMELQ                                                     
         BE    VCHKN4                                                           
         IC    R1,NAMLN                                                         
         AR    RF,R1                                                            
         B     VCHKN2                                                           
VCHKN4   CLC   NAMLN,NAMLN-NAMEL(RE)                                            
         BNE   VCHKN6              LENGTH CHANGE                                
         IC    R1,NAMLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NAMEL(0),0(RE)                                                   
         BE    VCHKNX              NO CHANGE TO NAME                            
         USING ACTRECD,R2                                                       
VCHKN6   OI    LFMINDS,LFMINMCH    SET NAME CHANGED                             
         LA    R3,BLOCK            SMALL AREA ONLY FOR DIRECTORY READS          
         USING ANCRECD,R3                                                       
         MVC   ANCKEY,SPACES                                                    
         MVI   ANCKTYP,ANCKTYPQ                                                 
         MVC   ANCKCULA,ACTKCULA                                                
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMREAD'),=C'ACCDIR',(R3),(R3),0           
         BE    VCHKNX              POINTER ALREADY THERE                        
         MVC   0(L'ACTKEY,R3),ACTKEY                                            
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMREAD'),=C'ACCDIR',(R3),(R3),0           
         BNE   VCHKNX                                                           
         MVC   FULL,ACTKDA-ACTRECD(R3)                                          
         MVC   ANCKEY,SPACES       ADD POINTER RECORD                           
         MVI   ANCKTYP,ANCKTYPQ                                                 
         MVC   ANCKCULA,ACTKCULA                                                
         XC    ANCKSTA,ANCKSTA                                                  
         OI    ANCKSTA,ACTSDELT                                                 
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'ACCDIR',(R3),(R3),0                    
         B     VCHKNX                                                           
*                                  SAVE THE OLD NAME                            
VSAVNAM  XC    0(NAMLN1Q+L'NAMEREC,RE),0(RE)                                    
         AH    RF,DATADISP                                                      
         SR    R1,R1                                                            
         USING NAMELD,RF                                                        
VSAVNM2  CLI   NAMEL,0                                                          
         BE    VCHKNX              NO NAME ELEMENT                              
         CLI   NAMEL,NAMELQ                                                     
         BE    VSAVNM4                                                          
         IC    R1,NAMLN                                                         
         AR    RF,R1                                                            
         B     VSAVNM2                                                          
VSAVNM4  IC    R1,NAMLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RE),NAMEL                                                    
*                                                                               
VCHKNX   B     XIT                                                              
         EJECT                                                                  
*              CONTROL OF BRANCHING TO APPLICATION                              
         SPACE 1                                                                
GO       ST    RE,SAVERE                                                        
         MVI   LEVEL,2                                                          
         ST    RB,LOGBASE                                                       
         MVI   ERROR,X'FF'                                                      
         GOTO1 APPLIC,DMCB,(RA),(RC),(R8)                                       
         MVI   LEVEL,1                                                          
         ST    RB,LOGBASE                                                       
         BAS   RE,SETEXTN       SET EXTERNALS-APPLICATION MAY DESTROY           
         CLI   ERROR,X'FE'         USER HAS FORMATTED MESSAGE                   
         BE    LOGEXIT             AND POSITIONED CURSOR                        
         CLI   ERROR,X'FF'                                                      
         BNE   LOGERRS                                                          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*              COMMUNICATIONS WITH DATA MANAGER                                 
         SPACE 1                                                                
VWRITE   DS    0H                                                               
VPUTREC  NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         BAS   RE,GETLNGTH                                                      
         LA    RF,IO2                                                           
         CLC   42(2,RF),=Y(IOLENQ)                                              
         BNH   *+12                                                             
         MVI   ERROR,66                                                         
         B     LOGERRS                                                          
VPUT2    MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 =A(SECHECK),DMCB,(RC),COMMAND,RR=RELO                            
         BNZ   LOGERRS                                                          
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',IO2,IO2               
         B     DMCHECK                                                          
         SPACE 1                                                                
VADD     DS    0H                                                               
VADDREC  NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         BAS   RE,GETLNGTH                                                      
         LA    RF,IO2                                                           
         CLC   42(2,RF),=Y(IOLENQ)                                              
         BNH   *+12                                                             
         MVI   ERROR,66                                                         
         B     LOGERRS                                                          
         SPACE 1                                                                
         TM    RECOREL,FULLKEY                                                  
         BO    VADDREC1                                                         
         LA    RF,IO2                                                           
         CLI   PHASE,X'20'         SPECIAL FOR 'FEES'                           
         BE    *+10                                                             
         MVC   15(27,RF),SPACES                                                 
         CLI   14(RF),0                                                         
         BNE   *+10                                                             
         XC    15(27,RF),15(RF)                                                 
VADDREC1 MVC   KEY,IO2                                                          
         OI    DMINBTS,X'88'       SEE IF DELETED RECORD ALREADY EXISTS         
*                                  AND READ FOR UPDATE IN CASE IT DOES.         
         MVC   IO(L'KEY),KEY                                                    
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',IO,IO                 
         TM    DMCB+8,X'10'        DID I FIND RECORD                            
         BO    *+14                NO                                           
         TM    DMCB+8,X'02'        IF SO, IS IT DELETED                         
         BO    VPUT2               YES, WRITE IT BACK FROM I02                  
         DC    H'0'           RECORD ALREADY EXISTS (SOMETHING SCREWY)          
*                                                                               
         MVC   COMMAND,=C'DMADD'                                                
         MVI   DMCB+8,0                                                         
         GOTO1 =A(SECHECK),DMCB,(RC),COMMAND,RR=RELO                            
         BNZ   LOGERRS                                                          
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',IO2,IO2               
         B     DMCHECK                                                          
         EJECT                                                                  
*              COMMUNICATION WITH DATA MANAGER                                  
         SPACE 1                                                                
VREAD    NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         MVC   COMMAND,=C'DMREAD'                                               
         CLI   UPDATE,C'Y'                                                      
         BNE   DIRLINK                                                          
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         B     DIRLINK                                                          
         SPACE 1                                                                
VSEQ     NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRLINK                                                          
         SPACE 1                                                                
VHIGH    NTR1  BASE=LOGBASE                                                     
         L     R9,LOGR9                                                         
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         SPACE 1                                                                
DIRLINK  TM    RECOREL,FULLKEY                                                  
         BO    DIRLINK1                                                         
         XC    IO(49),IO                                                        
         MVC   IO(42),SPACES                                                    
         MVC   IO(15),KEY                                                       
         CLI   PHASE,X'20'                                                      
         BNE   *+10                                                             
         MVC   IO(32),KEY                                                       
         CLI   KEY+14,0                                                         
         BNE   *+10                                                             
         XC    IO+15(27),IO+15                                                  
DIRLINK1 GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',IO,IO                 
         MVC   KEY,IO                                                           
         MVI   UPDATE,C'N'                                                      
         NI    DMINBTS,X'7F'       TURN OFF - READ FOR UPDATE                   
         B     DMCHECK                                                          
         EJECT                                                                  
*              ROUTINE TO GET LENGTH OF RECORD IN IO2                           
         SPACE 1                                                                
GETLNGTH NTR1                                                                   
         LA    R2,IO2                                                           
         AH    R2,DATADISP                                                      
         SR    R3,R3                                                            
         SPACE 1                                                                
GETLEN2  CLI   0(R2),0                                                          
         BE    GOTEND                                                           
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         LTR   R3,R3                                                            
         BNZ   GETLEN2                                                          
         DC    H'0'                                                             
         DC    C'ZERO LENGTH ELEMENT'                                           
         SPACE 1                                                                
GOTEND   LA    R2,1(R2)                                                         
         LA    R3,IO2                                                           
         SR    R2,R3                                                            
         STH   R2,DUB                                                           
         LA    RF,IO2                                                           
         MVC   42(2,RF),DUB                                                     
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT HEADLINE AND EXIT                                         
         SPACE 1                                                                
OKEXIT   SR    R4,R4                                                            
         IC    R4,0(R3)                                                         
         MVC   LOGHEAD,SPACES                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     LOGEXIT                                                          
         MVC   LOGHEAD(0),1(R3)                                                 
         SPACE 1                                                                
RESETEX  LA    R2,LOGTABH          POSITION R2 TO SECOND FIELD                  
         CLC   LOGREC(2),=C'AL'    UNLESS BUDGET ALLOCATION                     
         BNE   RESETX1                                                          
         CLI   LOGACT,C'N'         POSITION AT ACTION FIELD                     
         BNE   RESETX1                                                          
         LA    R2,LOGACTH                                                       
         B     OKEXIT                                                           
RESETX1  SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         AR    R2,R4                                                            
         TM    1(R2),X'20'         OR 1ST SUBSEQUENT UNPROT FIELD.              
         BO    *-10                                                             
         B     OKEXIT                                                           
         EJECT                                                                  
*              HANDLE ERROR CONDITIONS AND EXITS                                
         SPACE 1                                                                
DMCHECK  MVC   DUB,DMCB+8                                                       
         NC    DUB(1),DMOUTBTS                                                  
         MVI   ERROR,0                                                          
         BNZ   LOGERRS                                                          
         MVI   ERROR,X'FF'                                                      
         CLC   COMMAND(3),=C'DMR'                                               
         BE    DMCHECK3            CHECK SECURITY                               
         CR    RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
DMCHECK3 L     R4,DMCB                                                          
         GOTO1 =A(SECHECK),DMCB,(RC),(R4),RR=RELO                               
         BZ    XIT                 CC EQU 0 IS GOOD SECURITY                    
*                                                                               
LOGERRS  GOTO1 GETMSG,DMCB+12,(ERROR,LOGHEAD),(FNDX,DMCB),0                     
LOGEXIT  OI    6(R2),X'40'         INSERT CURSOR                                
         OI    6(R2),X'80'                                                      
         L     RD,BASERD           GET OUT                                      
         B     XIT                                                              
*                                                                               
         USING GETTXTD,R1          BUILD PARAM LIST FOR GETTXT                  
LOGERRX  LA    R1,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         MVI   GTMTYP,GTMERR                                                    
         MVC   GTMSGNO,GERROR      SETTING INVALID AT THIS LEVEL                
         CLC   UPDADV,SPACES                                                    
         BNH   *+16                                                             
         MVI   GTLTXT,4                                                         
         LA    RE,UPDADV                                                        
         STCM  RE,7,GTATXT                                                      
         GOTO1 GETTXT,DMCB                                                      
         OI    LOGHEADH+1,X'08'    HIGH INTENSITY                               
         OI    LOGHEADH+6,X'80'    TRANSMIT                                     
         OI    LOGHEADH+7,60       DEFAULT ERR MSG LENGTH                       
XIT      XIT1                                                                   
         DROP  R1                                                               
XITR1    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*              MESSAGES                                                         
OKMESS1  DC    AL1(21)                                                          
         DC    C'ENTER KEY FOR INQUIRY'                                         
         SPACE 1                                                                
OKMESS2  DC    AL1(25)                                                          
         DC    C'ENTER ALL REQUIRED FIELDS'                                     
         SPACE 1                                                                
OKMESS3  DC    AL1(19)                                                          
         DC    C'ENTER KEY FOR AMEND'                                           
         SPACE 1                                                                
OKMESS4  DC    AL1(17)                                                          
         DC    C'NOW ENTER CHANGES'                                             
         SPACE 1                                                                
OKMESS5  DC    AL1(26)                                                          
         DC    C'ENTER KEY FOR NEXT INQUIRY'                                    
         SPACE 1                                                                
OKMESS6  DC    AL1(27)                                                          
         DC    C'RECORD AMENDED - ENTER NEXT'                                   
         SPACE 1                                                                
OKMESS7  DC    AL1(29)                                                          
         DC    C'NEW RECORD ADDED - ENTER NEXT'                                 
         SPACE 1                                                                
OKMESS8  DC    AL1(15)                                                          
         DC    C'ACTION COMPLETE'                                               
         SPACE 1                                                                
*              CONSTANTS                                                        
         SPACE 1                                                                
ACTLIST  DS    0CL10                                                            
         DC    CL9'NEW      ',C'N'                                              
         DC    CL9'AMEND    ',C'A'                                              
         DC    CL9'ENQUIRY  ',C'E'                                              
         DC    CL9'CLOSE    ',C'C'                                              
         DC    CL9'INQUIRY  ',C'I'                                              
         DC    X'FF'                                                            
ACTLISTG DS    0CL10                                                            
         DC    CL9'NEU      ',C'N'                                              
         DC    CL9'AENDERN  ',C'A'                                              
         DC    CL9'ANZEIGEN ',C'I'                                              
         DC    CL9'SCHL.JOB ',C'C'                                              
         DC    X'FF'                                                            
         SPACE 1                                                                
RELO     DS    F                                                                
AREAD    DC    A(VREAD)                                                         
         DC    A(VHIGH)                                                         
         DC    A(VSEQ)                                                          
         DC    A(VPUTREC)                                                       
         DC    A(VADD)                                                          
         DC    A(VPUTREC)                                                       
         DC    A(VADDREC)                                                       
         DC    A(VVALIDT)                                                       
         DC    A(VVALICSH)                                                      
         DC    A(VANY)                                                          
         DC    A(VNUMERIC)                                                      
         DC    A(VPACK)                                                         
         DC    A(VMOVE)                                                         
         DC    A(VNAMOUT)                                                       
         DC    A(VADDROUT)                                                      
         DC    A(VNAMIN)                                                        
         DC    A(VADDRIN)                                                       
         DC    A(VADDANEL)                                                      
         DC    A(VREMANEL)                                                      
         DC    A(VBALIN)                                                        
         DC    A(VSTATIN)                                                       
         DC    A(VCHKNAM)                                                       
NUMROUTS EQU   (*-AREAD)/4                                                      
         SPACE 1                                                                
         EJECT                                                                  
*              LITERAL POOL                                                     
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO GET JOB LEDGER CODE LENGTHS                           
GETJOB   NMOD1 0,**GJOB**                                                       
         L     RC,0(R1)            RE-ESTABLISH A(WORKING STORAGE)              
         LA    R3,IO                                                            
         USING LDGRECD,R3                                                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT(2),COMPJOB  JOB UNIT/LEDGER                              
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',IO,IO                        
         CLI   8(R1),0             TEST FOR RECORD FOUND                        
         BNE   GETJOBX             NO                                           
*                                                                               
         LA    R3,ACRECORD-ACKEYD(R3) POINT AT FIRST ELEMENT                    
         SR    R0,R0                                                            
*                                                                               
GETJOB2  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),ACLELQ        TEST FOR HEIRARCHY ELEMENT                   
         BE    GETJOB4             YES                                          
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETJOB2                                                          
*                                                                               
         USING ACLELD,R3                                                        
GETJOB4  MVC   COMPLCLI,ACLVLEN    EXTRACT AND CALCULATE LENGTHS                
         ZIC   RF,ACLVLEN+L'ACLVALS      LEVEL B LENGTH                         
         ZIC   RE,COMPLCLI                                                      
         SR    RF,RE                                                            
         STC   RF,COMPLPRD                                                      
         ZIC   RF,ACLVLEN+(2*L'ACLVALS)  LEVEL C LENGTH                         
         ZIC   RE,ACLVLEN+L'ACLVALS      LEVEL B LENGTH                         
         SR    RF,RE                                                            
         STC   RF,COMPLJOB                                                      
*                                                                               
GETJOBX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GETLEVS SUBROUTINE MAKES LEVELS SOFT, RETURNS INDIVIDUAL LEVEL     *          
* LENGTHS AND GIVES NUMBER OF NESTED LEVELS IN LEDGER RECDS          *          
**********************************************************************          
GETLEVS  NMOD1 0,*GETLEV*                                                       
         L     RC,0(R1)            RE-ESTABLISH A(WORKING STORAGE)              
         L     RF,4(R1)                                                         
         LA    R5,IO                                                            
         USING LDGRECD,R5                                                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,0(RF)                                                    
         MVC   LDGKUNT(2),1(RF)        UNIT/LEDGER                              
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',IO,IO                        
         CLI   8(R1),0             TEST FOR RECORD FOUND                        
         BNE   GLEVX               NO                                           
*                                                                               
         LA    R5,ACRECORD-ACKEYD(R5) POINT AT FIRST ELEMENT                    
         SR    R0,R0                                                            
*                                                                               
GLEV10   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),ACLELQ        TEST FOR HEIRARCHY ELEMENT                   
         BE    GLEV20              YES                                          
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GLEV10                                                           
*                                                                               
         USING ACLELD,R5                                                        
GLEV20   DS    0H                                                               
         XC    LEVELS(LEVLNQ),LEVELS     CLEAR LEVELS LENGTH/DISC               
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LHI   R0,LEVELQ                 R0 = MAXIMUM NUMBER OF LEVELS          
         STC   R0,LEVNUM                 ASSUME 4 LEVEL STRUCTURE               
         LA    R1,ACLVALS                R1 = FIRST LEVEL LENGTH                
         LA    RE,LEVELS                 STORE ACCUMULATIVE LNTH HERE           
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R6,R6                                                            
*                                                                               
GLEV30   ICM   R6,1,0(R1)                CURRENT LEVEL LENGTH                   
         BZ    GLEV40                    NO MORE LEVELS - ADJUST LEVNUM         
         STC   R6,0(RE)                                                         
         SR    R6,R3                     SUBTRACT CURRENT FROM PREVIOUS         
*                                                                               
         STC   R6,0(R2)                  CURRENT INDIVIDUAL LENGTH              
         IC    R3,0(R1)                  UPDATE R3                              
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,GLEV30                                                        
         B     GLEVX                                                            
*                                                                               
GLEV40   LA    R1,LEVELQ                 R1 = MAXIMUM NUMBER OF LEVELS          
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    XIT1                                                                   
         DROP  R5                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        ROUTINE TO PERFORM SECURITY CHECKING                                   
SECHECK  DS    0D                                                               
         NMOD1 SECHEND-SECHK,**SECHK*,RR=R7,CLEAR=YES                           
         LR    R8,RC                                                            
         LR    R6,RA                                                            
         USING SECHK,R8                                                         
         L     RC,0(R1)                                                         
         L     R3,4(R1)            CHECK SECURITY FOR READ FUNCTIONS            
         TM    8(R1),0             IF DATAMGR ERROR ETC.                        
         BNE   NOSECK              SKIP SECURITY                                
         ST    R7,RELO2                                                         
*                                                                               
         MVC   IOL-8(8),=C'**IO L**'                                            
         MVC   LSTCMMD,0(R3)       SAVE LAST DATAMGR COMMAND                    
         CLC   LSTCMMD(6),=C'DMRDHI'                                            
         BNE   SEC04                                                            
         CLC   KEY,KEYSAVE         DONT CHECK SECURITY FOR READ HIGH IF         
         BNE   NOSECK              REQUESTED RECORD NOT FOUND                   
*                                                                               
SEC04    MVI   UPDATESW,C'Y'                                                    
         LA    R7,IO2                                                           
         CLC   LSTCMMD(5),=C'DMADD'                                             
         BE    SEC05                                                            
         CLC   LSTCMMD(5),=C'DMWRT'                                             
         BE    SEC05                                                            
         LA    R7,IO                                                            
         MVI   UPDATESW,C'N'                                                    
*                                                                               
SEC05    ST    R7,AIOAREA                                                       
         USING ACKEYD,R7                                                        
         CLI   ACKEYACC,X'40'                                                   
         BNH   NOSECK              NO SECURITY ON SPECIAL RECORDS               
         CLC   ACKEYCON,SPACES     TEST FOR CONTRA/TRANS/PO RECORDS             
         BH    NOSECK              YES                                          
         CLC   3(L'KEY-3,R7),SPACES                                             
         BNE   SEC07               NO HIERARCHY FOR LEDGERS/UNITS               
*                                                                               
         ST    R7,AREC                                                          
         BAS   RE,GETVAL                                                        
         CLC   TWAAUTH+1(1),SECURITY                                            
         BL    SECERR                                                           
         B     XMOD                                                             
*                                                                               
SEC07    BAS   RE,GETLEDG                                                       
         BNE   ERXMOD                                                           
         DROP  R7                                                               
*                                                                               
* EXPLODE THE LEVEL KEYS AND FIND USER RECORD'S LEVEL                           
*                                                                               
SEC08    GOTO1 =V(ACSPLIT),DMCB1,(4,(R7)),AHEIR,ACLKEYS,RR=RELO2                
         LA    R3,1                                                             
         LA    R2,ACLKEYS                                                       
         LA    R4,4                                                             
*                                                                               
SEC10    CLC   0(L'ACKEYACC,R7),0(R2)                                           
         BE    SEC12                                                            
         LA    R3,1(R3)            INCREMENT LEVEL                              
         LA    R2,L'ACKEYACC(R2)   NEXT KEY                                     
         BCT   R4,SEC10                                                         
         MVI   ERROR,LEDGNVAL                                                   
         B     ERXMOD              HIERARCHY NOT COMPATIBLE                     
*                                                                               
* READ THE RECORDS ABOVE USER'S LEVEL AND CHECK SECURITY                        
*                                                                               
SEC12    STC   R3,SAVLEV           LEVEL OF USER'S RECORD                       
         SH    R3,=H'1'            READ ANY RECORDS ABOVE USER'S                
         BZ    SEC20                                                            
         LA    R4,ACLKEYS          R4=A(KEY)                                    
         LA    R5,1                R5=LEVEL NUMBER                              
*                                                                               
SEC14    MVC   LEDGKEY(L'ACKEYACC),0(R4)                                        
         BAS   RE,READLEV          AND READ INTO IOL                            
         BNZ   ERXMOD                                                           
         LA    RE,IOL                                                           
         ST    RE,AREC                                                          
         STC   R5,THISLEV                                                       
         BAS   RE,GETVAL                                                        
         CLC   TWAAUTH+1(1),SECURITY                                            
         BL    SECERR              SECURITY VIOLATION                           
         LA    R4,L'ACKEYACC(R4)   NEXT KEY                                     
         LA    R5,1(R5)            INCREMENT LEVEL NUMBER                       
         BCT   R3,SEC14                                                         
*                                                                               
* NOW DEAL WITH THE USER'S RECORD                                               
*                                                                               
SEC20    MVC   AREC,AIOAREA                                                     
         MVC   THISLEV,SAVLEV                                                   
         BAS   RE,GETVAL                                                        
         CLC   TWAAUTH+1(1),SECURITY                                            
         BL    SECERR                                                           
         BAS   RE,TSTOFF                                                        
         BE    XMOD                                                             
*                                                                               
SECERR   OI    DMCB+8,X'04'                                                     
         MVI   ERROR,0                                                          
         B     ERXMOD                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE LEDGER RECORD                                          
* AT ENTRY, R7=A(USER KEY)                                                      
* ON EXIT, CC=EQ IF OK, CC=NEQ FOR ERROR                                        
*          SETS BOTLEV AND OFFPOS                                               
*                                                                               
GETLEDG  NTR1  ,                                                                
         MVC   LEDGKEY,SPACES                                                   
         MVC   LEDGKEY(3),0(R7)                                                 
         BAS   RE,READLEV                                                       
         BNE   ERSEXIT                                                          
*                                                                               
         LA    R2,IOL                                                           
         AH    R2,DATADISP                                                      
*                                                                               
GETLEDG2 CLI   0(R2),0             TEST FOR EOR                                 
         BE    GETLEDGX            YES                                          
         CLI   0(R2),ACLTELQ       TEST FOR LEDGER ELEMENT                      
         BE    GETLEDG4                                                         
         CLI   0(R2),ACHRELQ       TEST FOR HEIRARCHY ELEMENT                   
         BE    GETLEDG6                                                         
*                                                                               
GETLEDG3 ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GETLEDG2                                                         
*                                                                               
         USING ACLEDGD,R2                                                       
GETLEDG4 MVC   OFFPOS,ACLTOFF                                                   
         B     GETLEDG3                                                         
         DROP  R2                                                               
*                                                                               
         USING ACHEIRD,R2                                                       
GETLEDG6 ST    R2,AHEIR            SAVE A(HEIRARCHY ELEMENT)                    
         LA    R1,ACHRLEVD                                                      
         LA    RF,4                RF=NUMBER OF LEVELS                          
         CLI   0(R1),0             LOOK FOR LOWEST LEVEL                        
         BH    *+12                                                             
         SH    R1,=Y(L'ACHRLEVD+L'ACHRDESD)                                     
         BCT   RF,*-12                                                          
*                                                                               
         STC   RF,BOTLEV                                                        
         B     GETLEDG3                                                         
*                                                                               
GETLEDGX B     SEXIT                                                            
         DROP  R2                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE SECURITY AND OFFICE VALUES FROM A RECORD               
* ON ENTRY, AREC=A(RECORD) AND THISLEV=RECORD'S LEVEL                           
* ON EXIT, THISOFF=RECORD'S OFFICE AND OFFICE=COMPOSITE OFFICE                  
*                                                                               
GETVAL   NTR1  ,                                                                
         MVI   SECURITY,0                                                       
         MVC   OFFICE,SPACES                                                    
         MVC   THISOFF,SPACES                                                   
         L     R4,AREC                                                          
         CLI   OFFPOS,1            TEST FOR OFFICE IN KEY LEDGER                
         BL    GETVAL1             NO                                           
         CLI   OFFPOS,12                                                        
         BH    GETVAL1                                                          
         ZIC   R1,OFFPOS                                                        
         LA    R1,2(R4,R1)                                                      
         MVC   THISOFF(1),0(R1)    EXTRACT OFFICE CODE                          
*                                                                               
GETVAL1  AH    R4,DATADISP                                                      
*                                                                               
GETVAL2  CLI   0(R4),0             TEST FOR EOR                                 
         BE    GETVAL8             YES                                          
         CLI   0(R4),ACPRELQ                                                    
         BE    GETVAL4                                                          
         CLI   0(R4),ACSTELQ                                                    
         BE    GETVAL6                                                          
*                                                                               
GETVAL3  ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETVAL2                                                          
*                                                                               
         USING ACPROFD,R4                                                       
GETVAL4  MVC   THISOFF,ACPROFFC    EXTRACT OFFICE CODE                          
         B     GETVAL3                                                          
*                                                                               
         USING ACSTATD,R4                                                       
GETVAL6  MVC   SECURITY,ACSTSECY+1                                              
         TM    OFFPOS,X'F0'        TEST FOR OFFICE IN FILTER                    
         BNO   GETVAL3             NO                                           
         LA    RF,ACSTFILT                                                      
         CLI   OFFPOS,C'1'                                                      
         BE    GETVAL7                                                          
         LA    RF,ACSTFILT+1                                                    
         CLI   OFFPOS,C'2'                                                      
         BE    GETVAL7                                                          
         LA    RF,ACSTANAL                                                      
         CLI   OFFPOS,C'3'                                                      
         BE    GETVAL7                                                          
         LA    RF,ACSTSUB                                                       
*                                                                               
GETVAL7  MVC   THISOFF(1),0(RF)                                                 
         OI    THISOFF,C' '                                                     
         B     GETVAL3                                                          
*                                                                               
* SET COMPOSITE OFFICE                                                          
*                                                                               
GETVAL8  L     R4,AREC                                                          
         CLC   3(L'KEY-3,R4),SPACES TEST FOR UNIT/LEDGER                        
         BE    GETVALX                                                          
*                                                                               
         ZIC   R1,THISLEV                                                       
         LR    R0,R1                                                            
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,OFFICES(R1)      R1=A(OFFICE CODE IN TABLE)                   
         MVC   0(2,R1),THISOFF                                                  
         CLI   OFFPOS,1            TEST FOR OFFICE IN KEY                       
         BL    GETVAL9                                                          
         CLI   OFFPOS,12                                                        
         BNH   GETVAL10            YES-NO NEED FOR COMPOSITE                    
*                                                                               
GETVAL9  CLC   0(2,R1),SPACES                                                   
         BH    GETVAL10            FOUND COMPOSITE OFFICE                       
         SH    R1,=H'2'            BACK UP ONE ENTRY                            
         BCT   R0,GETVAL9                                                       
         B     GETVALX                                                          
*                                                                               
GETVAL10 MVC   OFFICE,0(R1)                                                     
*                                                                               
GETVALX  B     SEXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO TEST OFFICE ACCESS                                             
* ON ENTRY, AREC=A(RECORD TO BE TESTED)                                         
* ON EXIT, CC=EQ IF OK, CC=NEQ IF NOT OK                                        
*                                                                               
TSTOFF   NTR1  ,                                                                
         CLC   TWAACCS,SPACES      TEST ANY ACCESS CONTROL                      
         BNH   TSTOFFY             NO                                           
         TM    OFFPOS,X'F0'        TEST FOR OFFICE IN FILTER LEDGER             
         BNO   TSTOFF2             NO                                           
         CLC   THISLEV,BOTLEV      TEST FOR LOWEST LEVEL                        
         BL    TSTOFFY             NO-GRANT ACCESS                              
         B     TSTOFF6                                                          
*                                                                               
TSTOFF2  L     R4,AREC                                                          
         CLC   1(2,R4),COMPJOB     TEST FOR PRODUCTION                          
         BNE   TSTOFF6             NO                                           
*                                                                               
         CLI   SAVLEV,1            TEST FOR CLIENT                              
         BNE   TSTOFF4             NO                                           
         CLI   LASTSEQ,X'08'       TEST FOR CLIENT OVERLAY                      
         BE    TSTOFF4             YES-PERFORM CHECK                            
         CLI   LASTSEQ,X'03'       TEST FOR ACCOUNT OVERLAY                     
         BE    TSTOFF4             YES-PERFORM CHECK                            
         CLI   LASTSEQ,X'19'       TEST MCLI                                    
         BE    TSTOFF4             YES                                          
         B     TSTOFFY             GIVE ACCESS UNDER OTHER CONDITIONS           
*                                                                               
TSTOFF4  CLC   TWAACCS(2),SPACES   TEST FOR ANY OLD SECURITY                    
         BNH   TSTOFF6             NO                                           
         CLI   TWAACCS,C'*'        TEST SINGLE OFFICE SECURITY                  
         BE    TSTOFF6             YES                                          
         CLI   TWAACCS,C'$'                                                     
         BE    TSTOFF6                                                          
         CLC   TWAACCS(2),3(R4)    CLIENT CODE SECURITY                         
         BE    TSTOFFY                                                          
         B     TSTOFFN             BOUNCE THE ITEM                              
*                                                                               
TSTOFF6  L     R1,AOFFBLK          BUILD OFFAL BLOCK                            
         USING OFFALD,R1                                                        
         MVC   OFFAREC,AREC                                                     
         MVC   OFFAOPOS,OFFPOS                                                  
         MVC   OFFAOFFC,OFFICE                                                  
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 OFFAL                                                            
         BE    TSTOFFY                                                          
         B     TSTOFFN                                                          
         DROP  R1                                                               
*                                                                               
TSTOFFN  B     ERSEXIT                                                          
*                                                                               
TSTOFFY  B     SEXIT                                                            
         EJECT                                                                  
READLEV  NTR1                                                                   
         GOTO1 DATAMGR,DMCB1,=C'DMREAD',=C'ACCOUNT',LEDGKEY,IOL                 
         CLI   DMCB1+8,0                                                        
         BE    SEXIT                                                            
         MVI   ERROR,0             LEDGER OR HIGH LEVEL MISSING                 
         TM    DMCB1+8,X'10'                                                    
         BNO   ERSEXIT                                                          
         MVI   ERROR,LEDGNVAL                                                   
         B     ERSEXIT                                                          
ERSEXIT  LTR   R8,R8                                                            
         B     *+6                                                              
SEXIT    SR    R8,R8                                                            
         XIT1                                                                   
*                                                                               
*        RESET  SEQUENCE ON GOOD EXIT                                           
*                                                                               
XMOD     XC    IOL(49),IOL                                                      
         MVC   IOL(42),IO                                                       
         GOTO1 DATAMGR,DMCB1,=C'DMREAD',=C'ACCOUNT',IOL,IOL                     
NOSECK   SR    R8,R8                                                            
         B     *+6                                                              
ERXMOD   LTR   R8,R8               CC OF ^0 IS BAD SECURITY                     
         XMOD1                                                                  
*        EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SWAP KEYS FROM SCREEN TO SCREEN                       
         SPACE 1                                                                
CSCANKEY DS    0D                                                               
         NMOD1 SCNEND-SCND,**SCNK**,CLEAR=YES                                   
         LR    R8,RC                                                            
         USING SCND,R8                                                          
         L     RC,0(R1)                                                         
         MVC   THISPHS,4(R1)                                                    
         CLI   LASTSEQ,0                                                        
         BE    SCNK30              FIRST TIME SO LOAD SCREEN                    
         L     R7,RECLIST                                                       
         USING RECLSTD,R7                                                       
SCNK10   CLI   0(R7),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                BAD SCREEN NUMBER                            
         CLC   RECSEQ,LASTSEQ     GET KEY LIST FOR OLD SCREEN                   
         BE    SCNK13                                                           
         LA    R7,RECLLEN(R7)                                                   
         B     SCNK10                                                           
SCNK13   MVC   SCNLST,RECKFLD      SAVE KEY FIELD LIST                          
         OC    SCNLST,SCNLST                                                    
         BZ    SCNK30              NO FIELDS DEFINED                            
         LA    R9,SCNLST                                                        
         LA    R3,1                                                             
SCNK15   TM    0(R9),UNT+LED       FIRST LOOK FOR UNIT AND LEDGER               
         BZ    *+8                                                              
         BAS   RE,SETSK            AND GET THEM INTO FIXED FIELDS               
         LA    R9,2(R9)                                                         
         AH    R3,=H'1'            KEEP TRACK OF FIELD NUMBERS                  
         CH    R3,=H'7'                                                         
         BNH   SCNK15                                                           
         CLI   SCNUNT,C'A'                                                      
         BL    SCNK30                                                           
         CLI   SCNLED,C'A'                                                      
         BL    SCNK30              NO VALID UNIT AND LEDGER                     
         MVC   SCNKEY,SPACES                                                    
         MVC   SCNKEY(1),COMPANY                                                
         MVC   SCNKEY+1(1),SCNUNT                                               
         MVC   SCNKEY+2(1),SCNLED                                               
         GOTO1 DATAMGR,PARM,(DMINBTS,=C'DMREAD'),=C'ACCOUNT',SCNKEY,IO          
         CLI   PARM+8,0                                                         
         BNE   SCNK30              CAN'T FIND LEDGER                            
         LA    R2,IO                                                            
         AH    R2,DATADISP                                                      
SCNK17   CLI   0(R2),X'16'         FIND THE HIERARCHY ELEMENT                   
         BE    SCNK19                                                           
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                CAN'T FIND ELEMENT                           
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     SCNK17                                                           
SCNK19   ST    R2,SCNHEIR          I'LL NEED THIS LATER                         
         CLC   RECNAME(2),=C'PE'                                                
         BNE   SCNK20                                                           
         L     R6,SCNHEIR                                                       
         USING ACHEIRD,R6                                                       
         CLI   ACHRLEVD,0                                                       
         BNE   SCNK20                                                           
         MVC   SCNLST,PETHREE      FIX LIST IF THREE LEVEL PE                   
SCNK20   LA    R9,SCNLST                                                        
         LA    R3,1                                                             
         LA    R2,7                                                             
SCNK21   CLI   1(R9),0                                                          
         BNE   SCNK23              SKIP HARD UNIT AND LEGDER                    
         BAS   RE,SETSK            NOW SCAN ALL OTHER FIELDS                    
         AH    R3,=H'1'            KEEP TRACK OF FIELD NUMBER                   
SCNK23   LA    R9,2(R9)                                                         
         BCT   R2,SCNK21                                                        
         SPACE 1                                                                
SCNK30   L     R7,THISPHS                                                       
         L     R1,=A(2304-(LOGTABH-T603FFD))                                    
         LA    R0,LOGTABH                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR ALL TWA FROM LOGTABH.                  
         MVC   PARM+4(3),=X'D90603'                                             
         MVC   PARM+7(1),RECSCRN                                                
         GOTO1 CALLOV,PARM,(0,LOGTABH)                                          
         CLI   PARM+4,X'FF'                                                     
         BNE   SCNK37                                                           
         MVI   ERROR,CANTOVLY                                                   
         B     SCNXIT                                                           
         SPACE 1                                                                
         USING TWAD,R6                                                          
SCNK37   LR    R6,RA                                                            
         MVC   LASTSEQ,RECSEQ                                                   
         LA    R2,LOGHEADH                                                      
SCNK40   CLI   0(R2),0             AND TURN ON TRANSMIT BITS                    
         BE    SCNK43                                                           
         OI    6(R2),X'80'                                                      
         ZIC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         B     SCNK40                                                           
SCNK43   OC    RECKFLD,RECKFLD                                                  
         BZ    SCNXIT              NEW SCREEN FIELDS NOT DEFINED                
         CLC   RECNAME(2),=C'AC'                                                
         BNE   SCNK44                                                           
         CLI   SCNLVA,X'41'       IF ACCOUNT SCREEN MUST HAVE LEVEL A           
         BL    SCNXIT             OR IGNORE ALL OUTPUT                          
         USING TWAD,R6                                                          
SCNK44   LR    R6,RA                                                            
         EJECT                                                                  
*              BUILD AN OUTPUT STRING                                           
         SPACE 1                                                                
         MVC   SCNLST,RECKFLD      SAVE NEW SCREEN LIST                         
         CLC   RECNAME(2),=C'PE'                                                
         BNE   SCNK49                                                           
         B     SCNXIT              IGNORE THIS FOR NOW                          
         L     R6,SCNHEIR                                                       
         USING ACHEIRD,R6                                                       
         CLI   ACHRLEVD,0                                                       
         BNE   SCNK49                                                           
         MVC   SCNLST,PETHREE      FIX LIST IF THREE LEVEL PE                   
SCNK49   MVC   NEWUNT(2),SCNUNT    DEFAULT IS OLD UNIT/LEDGER                   
         LA    R0,7                                                             
         LA    R9,SCNLST                                                        
         LA    R4,SCNOUT                                                        
SCNK50   TM    0(R9),LVA           DO WE WANT LEVEL A HERE                      
         BZ    SCNK51                                                           
         LA    R2,SCNLVA           LEVEL A DATA                                 
         ZIC   R3,SCNLVAL          LENGTH OF DATA                               
         BAS   RE,SETNK                                                         
SCNK51   TM    0(R9),LVB           DO WE WANT LEVEL B                           
         BZ    SCNK53                                                           
         LA    R2,SCNLVB           LEVEL B DATA                                 
         ZIC   R3,SCNLVBL          LENGTH OF DATA                               
         BAS   RE,SETNK                                                         
SCNK53   TM    0(R9),LVC           DO WE WANT LEVEL C                           
         BZ    SCNK55                                                           
         LA    R2,SCNLVC           LEVEL C DATA                                 
         ZIC   R3,SCNLVCL          LENGTH OF DATA                               
         BAS   RE,SETNK                                                         
SCNK55   TM    0(R9),LVD           DO WE WANT LEVEL C                           
         BZ    SCNK57                                                           
         LA    R2,SCNLVD           LEVEL D DATA                                 
         ZIC   R3,SCNLVDL          LENGTH OF DATA                               
         BAS   RE,SETNK                                                         
SCNK57   TM    0(R9),WKC           DO WE WANT WORK-CODE                         
         BZ    SCNK59                                                           
         LA    R2,SCNWKC           WKC DATA                                     
         ZIC   R3,SCNWKCL          LENGTH OF DATA                               
         BAS   RE,SETNK                                                         
SCNK59   TM    0(R9),UNT           DO WE WANT UNIT                              
         BZ    SCNK61                                                           
         LA    R2,SCNUNT           UNIT CODE                                    
         LA    R3,1                                                             
         CLI   1(R9),0                                                          
         BE    *+8                                                              
         LA    R2,1(R9)            UNIT IS HARD CODE                            
         MVC   NEWUNT,0(R2)       SAVE NEW UNIT                                 
         BAS   RE,SETNK                                                         
SCNK61   TM    0(R9),LED           DO WE WANT LEDGER                            
         BZ    SCNK62                                                           
         LA    R2,SCNLED           LEDGER                                       
         LA    R3,1                                                             
         CLI   1(R9),0                                                          
         BE    *+8                                                              
         LA    R2,1(R9)            LEDGER IS HARD                               
         MVC   NEWLED,0(R2)       SAVE NEW UNIT                                 
         BAS   RE,SETNK                                                         
SCNK62   TM    0(R9),COM           DO WE WANT COMPANY                           
         BZ    SCNK63                                                           
         LR    R6,RA                                                            
         USING TWAD,R6                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINALS ONLY                           
         BNE   SCNK63                                                           
         L     R3,COMFACS                                                       
         USING COMFACSD,R3                                                      
         GOTO1 CHEXOUT,PARM,COMPANY,WORK,1,=C'TOG'                              
         LA    R2,WORK             COMPANY                                      
         LA    R3,2                                                             
         BAS   RE,SETNK                                                         
SCNK63   CLI   1(R9),0                                                          
         BNE   SCNK65              HARD CODE NOT OUTPUT HERE                    
         SH    R4,=H'1'                                                         
         ZIC   RF,TOTLEN                                                        
         LTR   RF,RF                                                            
         BZ    SCNK64                                                           
         CLI   0(R4),C' '                                                       
         BH    SCNK64                                                           
         BCTR  R4,0                                                             
         BCT   RF,*-10                                                          
SCNK64   MVI   1(R4),C'/'          END OF FIELD                                 
         LA    R4,2(R4)                                                         
         AH    RF,=H'1'                                                         
         STC   RF,TOTLEN                                                        
SCNK65   LA    R9,2(R9)                                                         
         BCT   R0,SCNK50           NEXT KEY FIELD                               
         SPACE 1                                                                
         CLC   SCNUNT(2),NEWUNT                                                 
         BNE   SCNXIT              MUST BE SAME UNIT AND LEDGER                 
         MVC   SCNHEAD+5(1),TOTLEN                                              
         ZIC   R3,TOTLEN                                                        
         AH    R3,=H'8'                                                         
         STC   R3,SCNHEAD          MAKE IT LOOK LIKE A HEADER                   
         OC    SCNOUT,SPACES                                                    
         L     R3,COMFACS                                                       
         USING COMFACSD,R3                                                      
         GOTO1 CSCUNKEY,PARM,SCNHEAD,LOGTABH                                    
         SPACE 1                                                                
SCNXIT   MVC   RETURN,SCNHEAD     RETURN OUTPUT LENGTH                          
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO MOVE DATA TO FIXED FIELDS                             
SETSK    NTR1                                                                   
         CLI   1(R9),0             NO HARD CODE                                 
         BE    SETSK3                                                           
         LA    R4,SCNUNT           UNIT AND LEDGER CAN BE HARD                  
         TM    0(R9),UNT                                                        
         BO    *+8                                                              
         LA    R4,SCNLED                                                        
         MVC   0(1,R4),1(R9)       MOVE UNIT/LEDGER TO SCAN TABLE               
         B     SCNXIT                                                           
SETSK3   LA    R2,LOGTABH                                                       
         XR    R0,R0                                                            
SETSK5   AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                INCORRECT SCREEN DEFINITION                  
         IC    R0,0(R2)                                                         
         TM    1(R2),X'20'                                                      
         BO    SETSK5              SKIP PROTECTED                               
         BCT   R3,SETSK5                                                        
         SPACE 1                                                                
         TM    0(R9),UNT                                                        
         BZ    SETSK7                                                           
         MVC   SCNUNT,8(R2)        SAVE UNIT                                    
         TM    0(R9),LED                                                        
         BZ    SCNXIT                                                           
         MVC   SCNLED,9(R2)        FIELD IS UNIT/LEDGER                         
         B     SCNXIT                                                           
SETSK7   TM    0(R9),LED                                                        
         BZ    SETSK9                                                           
         MVC   SCNLED,8(R2)        FIELD IS LEDGER ONLY                         
         B     SCNXIT                                                           
         USING ACHEIRD,R6                                                       
SETSK9   L     R6,SCNHEIR          ADDRESS OF HIERARCHY ELEMENT                 
         TM    0(R9),LVA                                                        
         BZ    SETSK11                                                          
         ZIC   R3,ACHRLEVA                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SCNLVA(0),8(R2)     GET LEVEL A DATA                             
         ZIC   RF,SCNLVAL                                                       
         LA    RF,1(R3,RF)                                                      
         STC   RF,SCNLVAL          GET NEW LENGTH FOR THIS FILED                
SETSK11  TM    0(R9),LVB                                                        
         BZ    SETSK13                                                          
         ZIC   R0,ACHRLEVA                                                      
         ZIC   R3,ACHRLEVB                                                      
         LTR   R3,R3                                                            
         BZ    SETSK13                                                          
         SR    R3,R0                                                            
         BCTR  R3,0                GET LENGTH FOR MOVE                          
         LA    R4,8(R2)                                                         
         TM    0(R9),LVA           DOES FIELD ALSO HAVE LEVEL A                 
         BZ    *+6                                                              
         AR    R4,R0               IF SO ADD LENGTH OF A TO GET TO B            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SCNLVB(0),0(R4)                                                  
         ZIC   RF,SCNLVBL                                                       
         LA    RF,1(R3,RF)                                                      
         STC   RF,SCNLVBL          GET NEW LENGTH FOR THIS FILED                
SETSK13  TM    0(R9),LVC                                                        
         BZ    SETSK15                                                          
         ZIC   R0,ACHRLEVB                                                      
         ZIC   R3,ACHRLEVC                                                      
         LTR   R3,R3                                                            
         BZ    SETSK15                                                          
         SR    R3,R0                                                            
         BCTR  R3,0                GET LENGTH FOR MOVE                          
         LA    R4,8(R2)                                                         
         TM    0(R9),LVB           DOES FIELD ALSO HAVE LEVEL B                 
         BZ    *+6                                                              
         AR    R4,R0               IF SO ADD LENGTH OF B TO GET TO C            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SCNLVC(0),0(R4)                                                  
         ZIC   RF,SCNLVCL                                                       
         LA    RF,1(R3,RF)                                                      
         STC   RF,SCNLVCL          GET NEW LENGTH FOR THIS FILED                
SETSK15  TM    0(R9),LVD                                                        
         BZ    SETSK17                                                          
         ZIC   R0,ACHRLEVC                                                      
         ZIC   R3,ACHRLEVD                                                      
         LTR   R3,R3                                                            
         BZ    SETSK17                                                          
         SR    R3,R0                                                            
         BCTR  R3,0                GET LENGTH FOR MOVE                          
         LA    R4,8(R2)                                                         
         TM    0(R9),LVC           DOES FIELD ALSO HAVE LEVEL C                 
         BZ    *+6                                                              
         AR    R4,R0               IF SO ADD LENGTH OF C TO GET TO D            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SCNLVD(0),0(R4)                                                  
         ZIC   RF,SCNLVDL                                                       
         LA    RF,1(R3,RF)                                                      
         STC   RF,SCNLVDL          GET NEW LENGTH FOR THIS FILED                
SETSK17  TM    0(R9),WKC           LOOK FOR WORK-CODES                          
         BZ    SCNXIT                                                           
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    SCNXIT                                                           
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SCNWKC(0),8(R2)                                                  
         ZIC   RF,SCNWKCL                                                       
         LA    RF,1(R3,RF)                                                      
         STC   RF,SCNWKCL          GET NEW LENGTH FOR THIS FILED                
         B     SCNXIT                                                           
         EJECT                                                                  
*              BUILD OUTPUT STRING                                              
         SPACE 1                                                                
SETNK    CLI   1(R9),0                                                          
         BNER  RE                  DON'T DEFINED FOR OUTPUT                     
         LTR   R3,R3                                                            
         BZR   RE                                                               
SETNK10  BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)       INPUT DATA TO OUTPUT STRING                  
         LA    R4,1(R3,R4)                                                      
         ZIC   RF,TOTLEN                                                        
         LA    RF,1(R3,RF)                                                      
         STC   RF,TOTLEN           KEEP TRACK OF TOTAL LENGTH                   
         BR    RE                                                               
         SPACE 2                                                                
PETHREE  DC    AL1(UNT,C'1')       FOR THREE LEVEL 1R                           
         DC    AL1(LED,C'R')                                                    
         DC    AL1(LVA,0)                                                       
         DC    AL1(LVB,0)                                                       
         DC    AL1(LVC,0)                                                       
         DC    2AL1(0,0)                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CREATE PASSIVE POINTER FOR NAME CHANGE                
*&&DO                                                                           
VNAMPTR  DS    0D                                                               
         NMOD1 NMWKX-NMWKD,**NMPT**,CLEAR=YES                                   
         LR    R8,RC                                                            
         USING NMWKD,R8                                                         
         L     RC,0(R1)            COMMON WORKING STORAGE                       
         CLI   ACCEMU,C'Y'         IS IT A NEW STYLE FILE                       
         BNE   NMXIT                                                            
         L     RF,4(R1)            A(KEY)                                       
         LA    R4,NMANC                                                         
         LA    R5,NMACT                                                         
         USING ANCRECD,R4                                                       
         MVC   ANCKEY,SPACES       BUILD NAME POINTER                           
         MVI   ANCKTYP,ANCKTYPQ    RECORD TYPE                                  
         MVC   ANCKCULA,0(RF)      C/U/L/ACCOUNT                                
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'ACCDIR',(R4),(R5)             
         CLC   0(L'ANCKEY,R4),0(R5)                                             
         BE    NMXIT               ALREADY ON FILE                              
         LA    R5,NMACT                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,SPACES       ACCOUNT POINTER                              
         MVC   ACTKCULA,ANCKCULA   C/U/L/ACCOUNT                                
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR',(R5),(R5)                     
         CLI   8(R1),0             DID WE FIND THE ACCOUNT                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    ANCKSTAT,ANCSDELT    DELETED                                     
         MVC   ANCKDA,ACTKDA       DISK ADDRESS                                 
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'ACCDIR',(R4),(R4)                      
         CLI   8(R1),0             ADD THE NEW PASSIVE POINTER                  
         BE    NMXIT                                                            
         DC    H'0'                                                             
NMXIT    XIT1                                                                   
*&&                                                                             
         LTORG                                                                  
         EJECT                                                                  
*              RECORD LIST                                                      
CRECLIST DS    0D                                                               
         DC    CL9'UNIT'                                                        
         DC    X'01FE01'                                                        
         DC    AL1(SPCL+AFM)                                                    
         DC    AL1(RECR)                                                        
         DC    AL1(COM,0)                                                       
         DC    AL1(UNT,0)                                                       
         DC    5AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'LEDGER'                                                      
         DC    X'02FD02'                                                        
         DC    AL1(SPCL+AFM)                                                    
         DC    AL1(RECR+TSTLCK)                                                 
         DC    AL1(COM,0)                                                       
         DC    AL1(UNT,0)                                                       
         DC    AL1(LED,0)                                                       
         DC    4AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'ACCOUNT'                                                     
         DC    X'03FC03'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(RECR+SRCH)                                                   
         DC    AL1(UNT,0)                                                       
         DC    AL1(LED,0)                                                       
         DC    AL1(LVA+LVB+LVC+LVD,0)                                           
         DC    4AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'BUDGET'                                                      
         DC    X'04FB04'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(ELMT)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'MEDIA'                                                       
         DC    X'06F906'                                                        
         DC    AL1(LGCL+NPRD)                                                   
         DC    AL1(RECR)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'WORK-CODE'                                                   
         DC    X'07F807'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(RECR)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'CLIENT'                                                      
         DC    X'08F708'                                                        
         DC    AL1(LGCL+PROD+CLPR+NPRD)                                         
         DC    AL1(RECR)                                                        
         DC    AL1(UNT,C'S')                                                    
         DC    AL1(LED,C'J')                                                    
         DC    AL1(LVA,0)                                                       
         DC    4AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'PRODUCT'                                                     
         DC    X'09F609'                                                        
         DC    AL1(LGCL+PROD+CLPR+NPRD)                                         
         DC    AL1(RECR)                                                        
         DC    AL1(UNT,C'S')                                                    
         DC    AL1(LED,C'J')                                                    
         DC    AL1(LVA,0)                                                       
         DC    AL1(LVB,0)                                                       
         DC    3AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'JOB'                                                         
         DC    X'0AF50A'                                                        
         DC    AL1(LGCL+PROD+NPRD)                                              
         DC    AL1(RECR)                                                        
         DC    AL1(UNT,C'S')                                                    
         DC    AL1(LED,C'J')                                                    
         DC    AL1(LVA,0)                                                       
         DC    AL1(LVB,0)                                                       
         DC    AL1(LVC,0)                                                       
         DC    2AL1(0,0)                                                        
         SPACE 1                                                                
*        DC    CL9'ESTIMATE'       INQUIRY                                      
*        DC    X'0BF40B'                                                        
*        DC    AL1(LGCL+PROD)                                                   
*        DC    AL1(ELMT)                                                        
*        DC    AL1(UNT,C'S')                                                    
*        DC    AL1(LED,C'J')                                                    
*        DC    AL1(LVA,0)                                                       
*        DC    AL1(LVB,0)                                                       
*        DC    AL1(LVC,0)                                                       
*        DC    AL1(WKC,0)                                                       
*        DC    1AL1(0,0)                                                        
*        SPACE 1                                                                
*        DC    CL9'ESTIMATE'       AMEND AND NEW                                
*        DC    X'10EF0C'                                                        
*        DC    AL1(LGCL+PROD+NISA)                                              
*        DC    AL1(ELMT)                                                        
*        DC    AL1(UNT,C'S')                                                    
*        DC    AL1(LED,C'J')                                                    
*        DC    AL1(LVA,0)                                                       
*        DC    AL1(LVB,0)                                                       
*        DC    AL1(LVC,0)                                                       
*        DC    AL1(WKC,0)                                                       
*        DC    1AL1(0,0)                                                        
*        SPACE 1                                                                
         DC    CL9'RULES'                                                       
         DC    X'0CF30D'                                                        
         DC    AL1(LGCL+NPRD)                                                   
         DC    AL1(ELMT)                                                        
         DC    AL1(UNT,C'S')                                                    
         DC    AL1(LED,C'J')                                                    
         DC    AL1(LVA,0)                                                       
         DC    AL1(LVB,0)                                                       
         DC    AL1(LVC,0)                                                       
         DC    2AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'AJRATE'                                                      
         DC    X'0DF20E'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(RECR+NOIO)                                                   
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'COMPANY'                                                     
         DC    X'0EF10F'                                                        
         DC    AL1(SPCL+PFVAL+AFM)                                              
         DC    AL1(RECR)                                                        
         DC    AL1(COM,0)                                                       
         DC    6AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'GROUP'                                                       
         DC    X'0FF010'                                                        
         DC    AL1(SPCL)                                                        
         DC    AL1(ELMT)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'POOL'                                                        
         DC    X'11EE11'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(ELMT)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'NARRATION'                                                   
         DC    X'12ED12'                                                        
         DC    AL1(LGCL+NPRD)                                                   
         DC    AL1(RECR)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'DISTRIBS.'                                                   
         DC    X'13EC13'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(NOIO)                                                        
         DC    AL1(UNT+LED,0)                                                   
         DC    AL1(WKC,0)                                                       
         DC    AL1(LVA+LVB+LVC+LVD,0)                                           
         DC    4AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'BATCH'                                                       
         DC    X'14EB14'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(NOIO)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'PERSONNEL'                                                   
         DC    X'15EA15'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(RECR)                                                        
         DC    AL1(UNT,C'1')                                                    
         DC    AL1(LED,C'R')                                                    
         DC    AL1(LVA+LVB,0)                                                   
         DC    AL1(LVC,0)                                                       
         DC    AL1(LVD,0)                                                       
         DC    2AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'CHECKS'                                                      
         DC    X'17E817'                                                        
         DC    AL1(LGCL+PFVAL+NISA+AFM)                                         
         DC    AL1(ELMT+TSTLCK)                                                 
         DC    AL1(COM,0)                                                       
         DC    AL1(LED,0)                                                       
         DC    5AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'LIST'                                                        
         DC    X'18E718'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(RECR)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'MCLIENT'                                                     
         DC    X'19E619'                                                        
         DC    AL1(LGCL+PROD+CLPR+NPRD)                                         
         DC    AL1(MAC)                                                         
         DC    AL1(UNT,C'S')                                                    
         DC    AL1(LED,C'J')                                                    
         DC    AL1(LVA,0)                                                       
         DC    4AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'MPRODUCT'                                                    
         DC    X'19E61A'                                                        
         DC    AL1(LGCL+PROD+CLPR+NPRD)                                         
         DC    AL1(MAC)                                                         
         DC    AL1(UNT,C'S')                                                    
         DC    AL1(LED,C'J')                                                    
         DC    AL1(LVA+LVB,0)                                                   
         DC    4AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'MJOB'                                                        
         DC    X'19E61B'                                                        
         DC    AL1(LGCL+PROD+NPRD)                                              
         DC    AL1(MAC)                                                         
         DC    AL1(UNT,C'S')                                                    
         DC    AL1(LED,C'J')                                                    
         DC    AL1(LVA+LVB+LVC,0)                                               
         DC    4AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'FEES'                                                        
         DC    X'20E31C'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(NOIO)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'HOURS'                                                       
         DC    X'21E21D'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(ELMT+NOKEY)                                                  
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'APG-RULES'                                                   
         DC    X'22E11E'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(RECR)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
*        DC    CL9'ASSETS'                                                      
*        DC    X'23E01F'                                                        
*        DC    AL1(LGCL)                                                        
*        DC    AL1(RECR)                                                        
*        DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'ADJUST'                                                      
         DC    X'24DF20'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(RECR+NOIO)                                                   
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'HISTORY'                                                     
         DC    X'25DE21'                                                        
         DC    AL1(SPCL)                                                        
         DC    AL1(RECR+NOIO)                                                   
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'RETAIL'                                                      
         DC    X'26DD22'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(NOIO+FULLKEY)                                                
         DC    AL1(UNT+LED,0)                                                   
         DC    AL1(WKC,0)                                                       
         DC    AL1(LVA+LVB+LVC+LVD,0)                                           
         DC    4AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'ALLOCATE'                                                    
         DC    X'27DC23'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(ELMT)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'RATES'                                                       
         DC    X'28DB24'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(RECR+NOIO)                                                   
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'TX'                                                          
         DC    X'29DA25'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(RECR)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'MI'                                                          
         DC    X'31D427'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(RECR)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'OFFICE'                                                      
         DC    X'2BD828'                                                        
         DC    AL1(LGCL+PFVAL)                                                  
         DC    AL1(NOIO)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'INTEREST'                                                    
         DC    X'32D229'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(RECR+NOIO)                                                   
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'GSTRULES'                                                    
         DC    X'33CC2A'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(NOIO)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'GLRULES'                                                     
         DC    X'34CB2B'                                                        
         DC    AL1(LGCL+NISA)                                                   
         DC    AL1(ELMT)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'MALLOC  '                                                    
         DC    X'35CD2C'                                                        
         DC    AL1(LGCL)                                                        
         DC    AL1(ELMT)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'CNVERT'                                                      
         DC    X'36CE2D'                                                        
         DC    AL1(PFVAL)                                                       
         DC    AL1(ELMT)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    CL9'WIGROUP'                                                     
         DC    X'37CF2E'                                                        
         DC    AL1(PFVAL)                                                       
         DC    AL1(ELMT)                                                        
         DC    7AL1(0,0)                                                        
         SPACE 1                                                                
         DC    X'FF'               END OF TABLE                                 
         EJECT                                                                  
*              DSECT FOR RECORD LIST                                            
         SPACE 1                                                                
RECLSTD  DSECT                                                                  
RECNAME  DS    CL9                                                              
RECPHASE DS    CL1                 PHASE NUMBER                                 
RECSCRN  DS    CL1                 SCREEN NUMBER                                
RECSEQ   DS    CL1                 SEQUENCE NUMBER- MUST BE UNIQUE              
RECSTAT  DS    CL1                                                              
SPCL     EQU   X'80'               SPECIAL - DDS ONLY                           
LGCL     EQU   X'40'               LOGICAL - ANYBODY                            
PROD     EQU   X'20'               PRODUCTION                                   
CLPR     EQU   X'10'               AUTHORIZATION REQUIRED FOR CLI/PRD           
NISA     EQU   X'08'               TREAT ACTION=NEW AS ACTION=AMEND             
NPRD     EQU   X'04'               EQUIVALENT RECORD ON NEW PRODUCTION          
PFVAL    EQU   X'02'               PF KEYS MAY BE USED                          
AFM      EQU   X'01'               CERTAIN ACTIONS AFM ONLY                     
ALL      EQU   X'FF'                                                            
RECTYPE  DS    CL1                                                              
RECR     EQU   X'80'               UPDATE RECORD                                
ELMT     EQU   X'40'               UPDATE ELEMENT                               
NOIO     EQU   X'20'               NO IO'S ON THIS TYPE                         
MAC      EQU   X'10'               SPECIAL FOR MAC                              
FULLKEY  EQU   X'08'               USE 32 BYTE KEY                              
NOKEY    EQU   X'04'               NO KEY FOR RECORD TYPE                       
TSTLCK   EQU   X'02'               TEST IF LEDGER IS LOCKED                     
SRCH     EQU   X'01'               UPDATE NAME SEARCH POINTERS                  
RECKFLD  DS    CL14                7 KEY FIELDS 2 BYTES EACH                    
RECFLDT  DS    0CL1                FIELD TYPE                                   
LVA      EQU   X'80'                                                            
LVB      EQU   X'40'                                                            
LVC      EQU   X'20'                                                            
LVD      EQU   X'10'                                                            
UNT      EQU   X'08'                                                            
LED      EQU   X'04'                                                            
WKC      EQU   X'02'                                                            
COM      EQU   X'01'                                                            
RECFLDC  DS    0CL1                HARD CODE FOR UNIT/LEDGER                    
RECLLEN  EQU   *-RECLSTD                                                        
         EJECT                                                                  
*        DSECT FOR SCAN KEY WORK                                                
         SPACE 1                                                                
SCND     DSECT                                                                  
THISPHS  DS    F                                                                
SCNLST   DS    CL14                SAVED LIST OF KEY FIELD DEFINITIONS          
SCNKEY   DS    CL42                KEY FOR LEDGER RECORD                        
PARM     DS    6F                  PARAMETER LIST                               
SCNHEIR  DS    F                   ADDRESS OF HIERARCHY                         
SCNUNT   DS    CL1                 INPUT UNIT                                   
SCNLED   DS    CL1                 INPUT LEDGER                                 
SCNLVA   DS    CL12                INPUT LEVEL A DATA                           
SCNLVAL  DS    CL1                 LENGTH OF DATA                               
SCNLVB   DS    CL12                INPUT LEVEL B DATA                           
SCNLVBL  DS    CL1                 LENGTH OF DATA                               
SCNLVC   DS    CL12                INPUT LEVEL C DATA                           
SCNLVCL  DS    CL1                 LENGTH OF DATA                               
SCNLVD   DS    CL12                INPUT LEVEL D DATA                           
SCNLVDL  DS    CL1                 LENGTH OF DATA                               
SCNWKC   DS    CL40                INPUT CODES                                  
SCNWKCL  DS    CL1                 LENGTH OF DATA                               
         SPACE 1                                                                
SCNHEAD  DS    CL8                 OUTPUT HEADER                                
SCNOUT   DS    CL60                OUTPUT DATA                                  
TOTLEN   DS    CL1                 TOTAL LENGTH OF OUTPUT                       
NEWUNT   DS    CL1                 OUTPUT UNIT                                  
NEWLED   DS    CL1                 OUTPUT LEDGER                                
SCNEND   EQU   *                                                                
         EJECT                                                                  
*              DSECT FOR THE NAME CHANGE ROUTINE                                
*                                                                               
NMWKD    DSECT                                                                  
NMANC    DS    CL100                                                            
NMACT    DS    CL100                                                            
NMWKX    DS    0C                                                               
*                                                                               
* DSECT FOR ACRAPPER BLOCK                                                      
*                                                                               
RAPPERD  DSECT                                                                  
RAACTIVE DS    CL1                                                              
RAADDSW  DS    XL1                 0=ADD, 1=PUT                                 
RARTYP   DS    XL1                 RAPPER RECORD TYPE                           
         DS    0F                                                               
       ++INCLUDE ACRAPPERD                                                      
RAPPERX  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
         SPACE 1                                                                
RECLIST  DS    F                                                                
SCANKEY  DS    F                                                                
RETURN   DS    CL1                                                              
NEWPRD   DS    CL1                                                              
*                                                                               
LEVELS   DS    0XL1                                                             
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
         EJECT                                                                  
*              DSECT FOR SECURITY ROUTINE                                       
SECHK    DSECT                                                                  
RELO2    DS    F                                                                
DMCB1    DS    6F                                                               
AIOAREA  DS    A                   A(IO AREA)                                   
AREC     DS    A                                                                
AHEIR    DS    A                   A(HEIRARCHY ELEMENT)                         
THISOFF  DS    CL2                 THIS RECORD'S OFFICE                         
OFFICE   DS    CL2                 OFFICE CODE (COMPOSITE)                      
OFFICES  DS    CL8                 OFFICES (4 LEVELS)                           
SECURITY DS    XL1                 SECURITY NUMBER                              
LEDGKEY  DS    CL42                                                             
LSTCMMD  DS    CL6                 SAVE LAST DATA MANAGER COMMAND               
SAVLEV   DS    CL1                 LEVEL OF USER'S RECORD                       
THISLEV  DS    CL1                 LEVEL BEING CHECKED                          
OFFPOS   DS    CL1                 OFFICE POSITION FROM ACLTOFF                 
ACLKEYS  DS    CL60                4 HIERARCHICAL KEYS                          
BOTLEV   DS    CL1                 # OF LOW. LEVEL                              
UPDATESW DS    CL1                 Y=CALL FOR UPDATIVE IO                       
         DS    D                   IN DUMP = **IO L**                           
IOL      DS    2000C                                                            
SECHEND  DS    0C                                                               
LEDGNVAL EQU   9                                                                
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         SPACE 1                                                                
         ORG   T603FFD                                                          
       ++INCLUDE FATWA                                                          
       ++INCLUDE DDLANGEQUS                                                     
         ORG   TWAUSER                                                          
LASTSEQ  DS    CL1                 PHASE/SCREEN SEQUENCE NUMBER                 
TWAXTRA  DS    CL31                FOR THE USER                                 
SAVEOFFA DS    CL(OFFASAVL)        OFFAL SAVE AREA                              
         DS    CL8                 RESERVED FOR BASE                            
         ORG   TWATASK                                                          
         DS    CL2304                                                           
         SPACE 1                                                                
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
*ACLFMEQU                                                                       
*DDCOMFACS                                                                      
*DMDTFIS                                                                        
*FAFACTS                                                                        
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDCOMFACS                                                      
*********INCLUDE DMDTFIS                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085ACLFM00   08/11/08'                                      
         END                                                                    
