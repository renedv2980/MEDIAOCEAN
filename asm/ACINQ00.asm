*          DATA SET ACINQ00    AT LEVEL 013 AS OF 05/01/02                      
*PHASE T60600A,*,NOAUTO                                                         
*INCLUDE ACSPLIT                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE CONVMOS                                                                
*INCLUDE PRORATA                                                                
         TITLE 'ACCOUNT ENQUIRY MK2 - ROOT'                                     
T60600   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (GWSX-GWS),**T60600,R8,RR=R7,CLEAR=YES                           
         USING GWS,RC              RC = GLOBAL W/S                              
         ST    R7,RELO                                                          
         ST    R1,AFACS            SAVE A(FACILITIES PARM LIST)                 
         MVC   ATIOB(FACSLEN),0(R1) SAVE PARMS                                  
         MVC   MYCO,0(R1)          EXTRACT COMPANY CODE                         
*                                                                               
INIT1    L     RA,4(R1)                                                         
         USING TWAD,RA                                                          
         MVC   TERMINAL,TWATRM                                                  
         MVC   TERMACCS,TWAACCS                                                 
         MVC   TERMAUTH,TWAAUTH                                                 
         MVC   TERMAGY,TWAAGY                                                   
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         USING T606TWA,RA          RA = TWA                                     
*&&US                                                                           
         CLC   CAFLDIND,INFCACH+4  CORRECT INPUT INDICATORS WHICH ARE           
         BNE   *+8                 MISLEADINGLY CLEARED IN USA IF FIELD         
         OI    INFCACH+4,X'20'     IS BEYOND CURSOR THIS TIME                   
         MVI   CAFLDIND,X'FF'                                                   
         CLC   FIFLDIND,INFFILTH+4                                              
         BNE   *+8                                                              
         OI    INFFILTH+4,X'20'                                                 
         MVI   FIFLDIND,X'FF'                                                   
*&&                                                                             
*                                                                               
INIT2    ST    RD,AREGSAVE                                                      
         ST    RB,ABASE                                                         
         ST    R8,A2NDBASE                                                      
*                                                                               
INIT4    L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         MVC   MODFRST,TIOBFRST    DISP TO 1ST MODIFIED FIELD                   
         MVC   MODLAST,TIOBLAST    DISP TO LAST MODIFIED FIELD                  
         MVC   CURDISP,TIOBCURD    DISP TO CURSOR FIELD                         
         ZIC   RF,TIOBAID                                                       
         LA    R0,PFK12                                                         
         CR    RF,R0               EQUIVALENCE PF13-PF24=PF1-PF12               
         BNH   *+6                                                              
         SR    RF,R0                                                            
         STC   RF,PFKEY                                                         
         DROP  R1                                                               
*                                                                               
INIT6    L     RF,AXTRAI           EXTRACT EXTRA INFO BLOCK DATA                
         MVC   AGYOPTS,0(R1)                                                    
         MVC   AGYCTRY,1(RF)                                                    
         MVC   AGYLANG,3(RF)                                                    
         MVC   AGYCURR,4(RF)                                                    
*                                                                               
         CLI   AGYCTRY,0           TEST FOR DEFAULT COUNTRY                     
         BNE   *+8                                                              
*&&US*&& MVI   AGYCTRY,CTRYUSA                                                  
*&&UK*&& MVI   AGYCTRY,CTRYGBR                                                  
         CLI   AGYLANG,0           TEST FOR DEFAULT LANGUAGE                    
         BNE   *+8                                                              
*&&US*&& MVI   AGYLANG,LANGEUS                                                  
*&&UK*&& MVI   AGYLANG,LANGEUK                                                  
*                                                                               
INIT8    L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   VADDAY,CADDAY                                                    
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VCUREDIT,CCUREDIT                                                
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDICTATE,CDICTATE                                                
         MVC   VGETFACT,CGETFACT                                                
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VGETPROF,CGETPROF                                                
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VSCANNER,CSCANNER                                                
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VUNSCAN,CUNSCAN                                                  
         DROP  RE                                                               
*                                                                               
INIT10   LA    R2,CORETAB          GET ADCONS FOR CORE-RESIDENT RTNS            
         LA    R3,NCORES                                                        
         LA    R4,COREFACS                                                      
         L     RF,VCALLOV                                                       
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
INIT11   MVC   DMCB+7(1),0(R2)     ROUTINE NUMBER                               
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R4),0(R1)       EXTRACT ADDRESS                              
         LA    R2,1(R2)            NEXT ROUTINE NUMBER                          
         LA    R4,4(R4)            NEXT ADCON                                   
         BCT   R3,INIT11                                                        
*                                                                               
INIT12   LA    R2,ACOMMON          R2=A(COMMON ROUTINE ADCONS)                  
         LA    R3,COMMON           R3=A(WORKING STORAGE ADCON)                  
         LA    R4,NCOMMON          R4=LOOP COUNTER                              
         L     RF,RELO                                                          
*                                                                               
INIT13   L     R5,0(R2)                                                         
         AR    R5,RF                                                            
         ST    R5,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,INIT13                                                        
*                                                                               
INIT14   LA    R1,EXTTAB           SET EXTENDED STORAGE ADCONS                  
         LA    R0,NEXTTAB                                                       
INIT15   LM    RE,RF,0(R1)         EXTENDED ADCONS                              
         LA    RE,GWS(RE)                                                       
         LA    RF,GWS(RF)                                                       
         ST    RE,0(RF)                                                         
         LA    R1,L'EXTTAB(R1)                                                  
         BCT   R0,INIT15                                                        
*                                                                               
INIT20   LA    R1,ACRECORD-ACKEYD                                               
         STH   R1,DATADISP                                                      
         LA    RE,OFFICE                                                        
         ST    RE,AOFFICE                                                       
         LA    RE,LEVEL                                                         
         ST    RE,ALEVEL                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   KEY(L'ACCKEY),SPACES                                             
         MVC   INFHEAD,SPACES                                                   
         OI    INFHEADH+6,X'80'                                                 
         EJECT                                                                  
*              CHECK TYPE AND LOAD OVERLAY                                      
         SPACE 1                                                                
T62      LA    R2,INFRECH                                                       
         CLC   8(2,R2),=C'HE'                                                   
         BE    HELPREC                                                          
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    INFERR                                                           
         MVI   ERROR,INVALID                                                    
         L     R3,=A(RECLIST)                                                   
         A     R3,RELO                                                          
         USING RECTABD,R3                                                       
         SPACE 1                                                                
T63      CLI   0(R3),0                                                          
         BE    INFERR                                                           
         CLC   RECTYPE,INFREC                                                   
         BNE   T64                                                              
         TM    RECAUTH,DDSONLY                                                  
         BNO   T65                                                              
         CLI   DDS,C'Y'                                                         
         BE    T65                                                              
         SPACE 1                                                                
T64      LA    R3,RECTBLEN(R3)                                                  
         B     T63                                                              
         SPACE 1                                                                
T65      MVC   AUTHTYPE,RECAUTH                                                 
         ZIC   R5,RECOLAY          IF OVERLAY DIFFERS FROM LAST,                
         CLC   RECOLAY,PHASE       UNSET LASTKMK                                
         BE    *+16                                                             
         MVI   LASTKMK,0                                                        
         MVI   FRSTSW,0                                                         
         STC   R5,PHASE                                                         
         GOTO1 VCALLOV,DMCB,((R5),0),(0,T606TWA)                                
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R9,DMCB                                                          
         ST    R9,APHASE                                                        
         LA    R1,RECADDS          STORE RELOCATED OVERLAY ADDRESSES            
         LA    R6,4                IN RECADDS                                   
         LA    R7,RECADDX-4                                                     
         LR    R4,R9                                                            
         SPACE 1                                                                
T651     L     R5,0(R4)                                                         
         AR    R5,R9                                                            
         ST    R5,0(R1)                                                         
         AR    R4,R6                                                            
         BXLE  R1,R6,T651                                                       
         SPACE 1                                                                
         MVI   LIMIT,X'FF'         SET DEFAULT RECORD LIMIT FOR LIST            
         TM    AUTHTYPE,ACLISTER                                                
         BNO   *+10                                                             
         MVC   LIMIT,USRLIMIT                                                   
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
*              RESET COMPANY CODE IN MYCO IF THIS IS A DDS TERMINAL AND         
*              A ONE CHARACTER FIELD HAS BEEN INPUT AFTER RECORD TYPE           
*                                                                               
         CLI   INFRECH+5,2                                                      
         BE    T66                NO INPUT AFTER RECORD TYPE                    
         CLI   DDS,C'Y'                                                         
         BNE   T653                                                             
         MVI   ERROR,INVALID                                                    
         CLI   INFREC+2,C','                                                    
         BNE   INFERR                                                           
         MVI   FNDX,2                                                           
         CLI   INFRECH+5,4                                                      
         BL    INFERR                                                           
         BE    T652                1 CHAR AFTER COMMA SO ITS EBCDIC             
         CLI   INFRECH+5,5         2 CHARS MEANS HEX                            
         BH    INFERR                                                           
         GOTO1 VHEXIN,DMCB,INFREC+3,MYCO,2                                      
         OC    DMCB+12(4),DMCB+12                                               
         BZ    INFERR                                                           
         B     T654                                                             
T652     MVC   MYCO,INFREC+3                                                    
         B     T654                                                             
T653     MVC   INFREC+2(L'INFREC-2),SPACES                                      
         OI    INFRECH+6,X'80'                                                  
T654     DS    0H                                                               
*&&UK                                                                           
         CLI   MYCO,X'E7'          PREVENT A SNEAKY SWITCH TO DDS LTD           
         BNE   *+6                 FOR THE UK                                   
         DC    H'0'                                                             
*&&                                                                             
         SPACE 1                                                                
T66      CLC   MYCO,PRODCO         READ COMPANY RECORD IF DIFFERENT             
         BE    T665                FROM LAST AND SAVE PRODUCTION UNIT           
         MVI   LASTKMK,0           AND LEDGER AND RECEIVABLE U/L                
         XC    LASTKEY,LASTKEY                                                  
         MVI   FRSTSW,0                                                         
         NI    INFCACH+4,X'DF'                                                  
*                                                                               
         LA    R4,KEYB                                                          
         USING CPYRECD,R4                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,MYCO        READ THE COMPANY RECORD                      
         MVI   ERROR,NOTVLCDE                                                   
         BAS   RE,READB                                                         
         BZ    INFERR                                                           
         L     R9,AIOB                                                          
         MVI   ELCODE,CPYELQ       GET COMPANY ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   INFERR                                                           
         USING CPYELD,R9                                                        
         MVC   SPROUNIT(2),CPYPROD EXTRACT COMPANY VALUES                       
         MVC   SRECUNIT(2),CPYRECV                                              
         MVC   SAVEALFA,CPYALPHA                                                
         MVC   SAVCSTA1,CPYSTAT1                                                
         MVC   SAVCSTA2,CPYSTAT2                                                
         MVC   SAVCSTA3,CPYSTAT3                                                
         MVC   SAVCSTA4,CPYSTAT4                                                
         MVC   SAVEVATR,CPYVATR                                                 
*&&UK                                                                           
         MVC   SCMPSTA2,CPYSTAT2   SEE ESTIMATES REC TYPE                       
*&&                                                                             
         XC    KEYB,KEYB           BUILD KEY FOR ACCPAK MASTER PROFILE          
         MVI   KEYB,C'A'                                                        
         MVC   KEYB+2(2),=C'00'                                                 
         MVC   KEYB+4(1),MYCO                                                   
         MVC   KEYB+12(2),TERMAGY  ALPHA ID                                     
         GOTO1 VGETPROF,DMCB,KEYB,WORK,VDATAMGR                                 
         MVC   INVREG,WORK+1       PULL OUT 2ND BYTE OF MASTER PROFILE          
*                                                                               
T665     L     R1,AOFFBLK          INITIALIZE OFFAL                             
         USING OFFALD,R1                                                        
         MVC   OFFACOMF,ACOMFACS                                                
         MVC   OFFATSAR,VTSAR                                                   
         MVC   OFFAALPH,TERMAGY                                                 
         MVC   OFFACPY,MYCO                                                     
         MVC   OFFACST1(OFFAOPOS-OFFACST1),SAVCSTA1                             
         MVC   OFFALIMA,TERMACCS                                                
         MVI   OFFAACT,OFFAINI                                                  
         MVI   OFFAINDS,OFFAISEC+OFFAIOFF                                       
         CLC   MYCO,PRODCO         TEST FIRST TIME FOR COMPANY                  
         BNE   *+14                                                             
         MVI   OFFAACT,OFFARES                                                  
         MVC   OFFASAV(OFFASAVL),SAVEOFFA   RESTORE OFFAL SAVE AREA             
         GOTO1 VOFFAL                                                           
         BE    *+6                                                              
         DC    H'0'                BLOW UP IF OFFAL HAD PROBLEM                 
         MVC   SAVEOFFA,OFFASAV                                                 
         MVC   PRODCO,MYCO                                                      
         B     T67                                                              
         DROP  R1,R4,R9                                                         
         EJECT                                                                  
*              CHECK INPUT KEYS AND SET UP KEY                                  
         SPACE 1                                                                
T67      MVI   FNDX,0                                                           
         MVC   PRODUNIT(4),SPROUNIT                                             
         MVC   COMPALFA,SAVEALFA                                                
         LA    R2,INFKEYH          R2 USED BY ERROR ROUTINES                    
         CLC   INFKEY(4),=C'HELP'  HELP                                         
         BNE   *+12                                                             
         MVI   HELPKMK,1                                                        
         B     HELPKEY                                                          
         SPACE 1                                                                
         OC    INFKEY,SPACES                                                    
         MVC   SCANBLCK(L'INFKEY),INFKEY                                        
         SPACE 1                                                                
         L     R4,RECKEY                                                        
         USING KEYTABD,R4                                                       
         MVI   ERROR,INVALID                                                    
         CLC   INFKEY(4),=C'NEXT'                                               
         BNE   T676                                                             
*                                                                               
         CLI   LASTKMK,0                                                        
         BE    T671                                                             
         TM    INFCACH+4,X'20'     C/ACCOUNT PREVIOUSLY VALIDATED               
         BZ    T671                                                             
         TM    INFFILTH+4,X'20'    FILTER PREVIOUSLY VALIDATED                  
         BNZ   T672                                                             
T671     MVC   INFKEY(14),INFKEY+9                                              
         MVC   INFKEY+14(9),SPACES                                              
         OI    INFKEYH+6,X'80'                                                  
         MVC   SCANBLCK(L'INFKEY),INFKEY                                        
         B     T676                                                             
T672     TM    LASTKMK,X'80'       IS THERE A CONTINUATION SCREEN               
T674     BNO   INFERR                                                           
         MVC   KEY(L'ACCKEY),LASTKEY                                            
         B     T6E                                                              
T676     CLC   INFKEY(4),=C'PREV'                                               
         BNE   T677                                                             
         TM    LASTKMK,X'40'       IS THERE A PREVIOUS SCREEN                   
         BO    T6E                                                              
         TM    AUTHTYPE,TWA1USER                                                
         BNO   INFERR                                                           
         TM    LASTKMK,X'80'       IF THERE ISNT BUT MIGHT HAVE BEEN            
         BNO   INFERR              PRESERVE SCREEN & CONTINUATION DATA          
         MVC   INFHEAD(37),NOPREV                                               
         B     INFEXIT                                                          
NOPREV   DC    CL37'ERROR 000 THERE IS NO PREVIOUS SCREEN'                      
         SPACE 1                                                                
T677     MVI   LASTKMK,0                                                        
         XC    LASTKEY,LASTKEY                                                  
         MVI   FRSTSW,0            KEY CHANGE=FIRST TIME FOR INQUIRY            
         SPACE 1                                                                
T68      CLI   0(R4),0                                                          
         BE    T6E                                                              
         ZIC   R5,KEYDISP                                                       
         LA    R5,KEY(R5)          R5 = START POSITION IN KEY                   
         MVC   HALF,KEYPOINT                                                    
         LH    R6,HALF                                                          
         LTR   R6,R6                                                            
         BNZ   T68A                                                             
         MVC   0(1,R5),KEYDFLT                                                  
         ZIC   R3,KEYLEN           DEFAULT VALUE MAY BE MULTI-BYTE              
         SH    R3,=H'2'            FILLER EG NULLS                              
         BM    T6D                                                              
         EX    R3,*+8                                                           
         B     T6D                                                              
         MVC   1(0,R5),0(R5)                                                    
         SPACE 1                                                                
T68A     AR    R6,RC               R6 = LOCATION OF KEY COMPONENT VALUE         
         CLI   KEYMAND,C'M'        MANDATORY KEY COMPONENT                      
         BNE   T69                                                              
         MVI   ERROR,MISSING                                                    
         CLI   0(R6),C' '                                                       
         BE    INFERR                                                           
         B     T6B                                                              
         SPACE 1                                                                
T69      CLI   KEYMAND,C'O'        OPTIONAL KEY COMPONENT                       
         BNE   T6B                                                              
         CLI   0(R6),C' '                                                       
         BNE   T6B                                                              
         MVC   0(1,R5),KEYDFLT                                                  
         B     T6D                                                              
         SPACE 1                                                                
T6B      MVI   ERROR,INVALID       SET UP KEY COMPONENT VALUE IN KEY            
         BAS   RE,KEYSET                                                        
         BZ    INFERR                                                           
T6D      LA    R4,KEYTBLEN(R4)     BUMP TO NEXT KEY COMPONENT                   
         B     T68                                                              
*                                                                               
         EJECT                                                                  
*              ROUTINE TO SET UP A KEY COMPONENT VALUE IN KEY                   
*              ON ENTRY R4       = A(KEY TABLE ENTRY)                           
*                       R5       = START POSITION IN KEY                        
*                       R6       = A(KEY COMPONENT VALUE - PRE-EDIT)            
         SPACE 3                                                                
KEYSET   NTR1                                                                   
         LR    R2,R6                                                            
         ZIC   R3,KEYLEN                                                        
         L     RF,RECKNTRY         ANY EDITING REQUIRED - IN OVERLAY            
         CLI   KEYEDIT,X'FF'                                                    
         BE    KS2                                                              
         MVC   HALF,KEYEDIT                                                     
         LH    RF,HALF                                                          
         LTR   RF,RF                                                            
         BZ    KS3                                                              
         AR    RF,RC                                                            
         L     RF,0(RF)                                 - OR IN ROOT            
         SPACE 1                                                                
KS2      GOTO1 (RF),DMCB,(RC)                                                   
         TM    DMCB,1                                                           
         BO    KSERROR                                                          
         SPACE 1                                                                
KS3      BCTR  R3,0                                                             
         EX    R3,KSMOVE           MOVE VALUE INTO  (R5)                        
         B     KSXIT                                                            
         SPACE 1                                                                
KSERROR  SR    RE,RE               CC=EQU IF ERROR                              
KSXIT    LTR   RE,RE                                                            
         B     EXIT                                                             
         SPACE 1                                                                
KSMOVE   MVC   0(0,R5),0(R2)                                                    
         EJECT                                                                  
*              CHECK INPUT CONTRA-ACCOUNT AND SET UP FILTER TABLE ENTRY         
*                                                                               
T6E      LA    R2,INFCACH                                                       
         SPACE 1                                                                
         CLC   INFCAC(4),=C'HELP'  HELP REQUESTED                               
         BNE   T6E01                                                            
         MVC   INFCAC,SPACES                                                    
         MVC   INFCAC(7),=C'=FILTER'                                            
         CLI   PHASE,7             RECEIVABLE                                   
         BNE   *+10                                                             
         MVC   INFCAC+1(13),=C'SOURCE FILTER'                                   
         OI    INFCACH+6,X'80'                                                  
         OI    HELPKMK,X'02'                                                    
         TM    AUTHTYPE,X'08'                                                   
         BO    T6F                                                              
         MVC   INFCAC+1(11),=C'START POINT'                                     
         TM    AUTHTYPE,X'10'                                                   
         BO    T6F                                                              
         MVC   INFCAC(12),=CL12'NOT ALLOWED'                                    
         TM    AUTHTYPE,X'04'      HANDLED BY OVERLAY (NOT PART OF KEY)         
         BZ    T6F                                                              
         MVC   INFCAC,SPACES                                                    
         MVC   INFCAC(7),=C'=FILTER'                                            
         B     T6F                                                              
         SPACE 1                                                                
T6E01    OC    INFCAC,SPACES                                                    
         MVI   FILTAB,X'FF'                                                     
         CLI   LASTKMK,0                                                        
         BE    T6E1                                                             
         MVI   ERROR,CHANGNVL      CHANGES NOT ALLOWED FOR CONTINUATION         
         TM    4(R2),X'20'         SCREENS                                      
         BNO   INFERR                                                           
         TM    AUTHTYPE,TWA1USER                                                
         BO    T6E4                                                             
         SPACE 1                                                                
T6E1     XC    STAFILT,STAFILT                                                  
         MVI   ENDFILT,X'FF'                                                    
         MVC   ENDFILT+1(L'ENDFILT-1),ENDFILT                                   
         SPACE 1                                                                
         TM    4(R2),X'20'                                                      
         BO    T6E3                                                             
         MVC   SAVECACN,SPACES                                                  
         CLI   5(R2),0                                                          
         BE    T6E4                                                             
         TM    AUTHTYPE,X'1C'      IS FIELD VALID FOR THIS TYPE                 
         MVI   ERROR,INVFTYPE                                                   
         BZ    INFERR                                                           
         SPACE 1                                                                
         SPACE 1                                                                
         MVC   KEYB(L'ACCKEY),SPACES TRY TO FIND ACCOUNT RECORD                 
         MVC   KEYB(1),MYCO           FOR CAC                                   
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEYB+1(0),INFCAC                                                 
         BAS   RE,HIGHB                                                         
         BZ    INFERR                                                           
         L     R9,AIOB                                                          
         CLC   KEYB(L'ACCKEY),0(R9)                                             
         BNE   T6E3                                                             
         L     R9,AIOB             SAVE ITS NAME                                
         MVI   ELCODE,ACNMELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   T6E3                                                             
         USING ACNAMED,R9                                                       
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         BM    T6E3                                                             
         EX    R3,*+8                                                           
         B     T6E3                                                             
         MVC   SAVECACN(0),ACNMNAME                                             
         DROP  R9                                                               
         SPACE 1                                                                
T6E3     CLI   5(R2),0             CREATE A FILTER TABLE ENTRY                  
         BE    T6E4                                                             
         LA    R6,FILTAB                                                        
         USING FTBD,R6                                                          
         XC    FTBELMNT(FTBTBLEN),FTBELMNT                                      
         LA    RF,AKEY                                                          
         ST    RF,FTBELMNT                                                      
         MVI   FTBDISP,17                                                       
         MVC   FTBLEN,5(R2)                                                     
         MVI   FTBMARK,C'C'                                                     
         MVI   FTBSIGN,C'P'                                                     
         MVC   FTBSR,EDITCONT                                                   
         TM    AUTHTYPE,X'04'                                                   
         BNO   T6E4                                                             
         MVC   FTBSR,RECFNTRY      START FILTERS ARE HANDLED BY OVERLAY         
         OI    FTBSTAT,X'04'                                                    
         SPACE 1                                                                
T6E4     OI    4(R2),X'20'                                                      
         CLI   5(R2),0                                                          
         BNE   T6F                                                              
*&&US                                                                           
         MVI   CAFLDIND,0          = NO INPUT THIS TIME                         
*&&                                                                             
         B     T6F                                                              
         EJECT                                                                  
*              CHECK INPUT OPTIONS AND SET UP FILTER TABLE IN GLOBAL WS         
*                                                                               
T6F      LA    R2,INFFILTH         R2 USED BY ERROR ROUTINES                    
         LA    R6,FILTAB                                                        
         USING FTBD,R6                                                          
         CLI   FTBELMNT,X'FF'                                                   
         BE    *+8                                                              
         LA    R6,FTBTBLEN(R6)                                                  
         CLC   INFFILT(4),=C'HELP'                                              
         BE    HELPFILT                                                         
         CLI   HELPKMK,0                                                        
         BNE   INFEXIT                                                          
         SPACE 1                                                                
         CLI   LASTKMK,0           NO CHANGES FOR CONTINUATION SCREENS          
         BE    T6F1                                                             
         MVI   ERROR,CHANGNVL                                                   
         TM    4(R2),X'20'                                                      
         BNO   INFERR                                                           
         TM    AUTHTYPE,TWA1USER                                                
         BO    T6M                                                              
         SPACE 1                                                                
T6F1     CLI   5(R2),0                                                          
         BE    T6M                                                              
         XC    SCANBLCK(250),SCANBLCK                                           
         XC    SCANBLCK+250(70),SCANBLCK+250                                    
         GOTO1 VSCANNER,DMCB,(13,INFFILTH),(9,SCANBLCK)                         
         CLI   DMCB+4,0                                                         
         MVI   ERROR,INVALID                                                    
         BE    INFERR                                                           
         LA    R3,SCANBLCK                                                      
         CLI   DMCB+4,1                                                         
         BE    *+8                                                              
         MVI   FNDX,1                                                           
         SPACE 1                                                                
T6F2     CLI   0(R3),0                                                          
         BE    T6M                                                              
         CLI   0(R3),2                                                          
         BNE   T6H                                                              
         L     R4,RECFILT                                                       
         USING FILTERSD,R4                                                      
         LA    R9,T6G                                                           
T6G      CLI   FILFULKW,X'00'                                                   
         BE    T6H                                                              
         CLC   FILSHTKW,12(R3)                                                  
         B     T6I2                                                             
T6H      L     R4,RECFILT          LOOK FOR FULL KEYWORD MATCH                  
         LA    R9,T6I                                                           
         ZIC   R5,0(R3)                                                         
         BCTR  R5,0                                                             
T6I      CLI   FILFULKW,X'00'                                                   
         BE    INFERR                                                           
T6I1     EX    R5,KEYWCOMP                                                      
T6I2     BNE   T6I3                                                             
         TM    FILSTAT,DDSONLY                                                  
         BNO   T6J                                                              
         CLI   DDS,C'Y'                                                         
         BE    T6J                                                              
T6I3     LA    R4,FILTBLEN(R4)                                                  
         BR    R9                                                               
OVERCOMP CLC   12(0,R3),=C'OVERRIDE'                                            
KEYWCOMP CLC   FILFULKW(0),12(R3)                                               
         SPACE 2                                                                
T6J      ZIC   R7,1(R3)            COME HERE WITH A MATCHING KEYWORD            
         BCTR  R7,0                                                             
         LTR   R7,R7               R7 = L OF FILTER VALUE MINUS 1               
         BM    INFERR                                                           
         MVI   FTBSTAT,0                                                        
         MVC   WORK(1),1(R3)       WORK   = L OF FILTER VALUE                   
         LA    R1,22(R3)           R1     = POINTER TO FILTER VALUE             
         MVI   FTBSIGN,C'P'        POSITIVE FILTER                              
         CLI   0(R1),C'*'                                                       
         BNE   T6J1                                                             
         STC   R7,WORK             ADJUST R1,R7 & WORK   IF NEGATIVE            
         BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         BM    INFERR                                                           
         LA    R1,1(R1)                                                         
         MVI   FTBSIGN,C'N'                                                     
         SPACE 1                                                                
T6J1     CLI   FILNOTNL,C' '       FILTER HAS NOTIONAL VALUE (EG YES)           
         BE    T6K                                                              
         TM    FILSTAT,X'60'       IS IT A DATE FILTER                          
         BZ    T6J1A                                                            
         BAS   RE,DATECHK                                                       
         BZ    INFERR                                                           
         B     T6K0                                                             
T6J1A    LA    RF,L'FILNOTNL-1                                                  
         CR    R7,RF                                                            
         BNH   *+6                                                              
         LR    R7,RF                                                            
         EX    R7,NOTLCOMP         DOES THE VALUE MATCH                         
         BE    T6J3                                                             
         CLC   FILNOTNL+3(3),=C'/NO'    IF NOT IS IT A YES/NO CASE              
         BNE   T6J2                                                             
         EX    R7,NOTLCOM2              AND DOES 'NO' MATCH                     
         BNE   T6J2                                                             
         XI    FTBSIGN,X'02'            IF SO SWITCH THE SIGN (N-P/P-N)         
         B     T6J3                                                             
T6J2     LA    R4,FILTBLEN(R4)     IF NOT TRY ANOTHER TAB ENTRY                 
         BR    R9                                                               
T6J3     CLC   FILFULKW(7),=C'DELETED'                                          
         BNE   T6J3A                                                            
         CLI   FTBSIGN,C'P'                                                     
         BNE   T6L1                                                             
         MVI   DELETED,C'Y'        DELETED FILTER REQUIRES SPECIAL ACTN         
         B     T6L1                                                             
T6J3A    OC    FILELMNT,FILELMNT   IF ITS AN OPTION NOT A FILTER                
         BNZ   T6J4                                                             
         CLI   FTBSIGN,C'P'        AND POSITIVE                                 
         BNE   T6L2                                                             
         OC    OPTIONS,FILVALUE    SET THE APPROPRIATE OPTIONS BIT              
         B     T6L2                                                             
T6J4     MVI   FTBMARK,C'M'        OTHERWISE SET UP MASK MARKER                 
         MVC   FTBVAL(1),FILVALUE  MASK VALUE                                   
         MVI   FTBLEN,1            LENGTH                                       
         B     T6K0                                                             
NOTLCOMP CLC   FILNOTNL(0),0(R1)                                                
NOTLCOM2 CLC   0(0,R1),=C'NO'                                                   
         SPACE 1                                                                
T6K      CLC   FILENGTH,WORK       FILTER DOESNT HAVE NOTIONAL VALUE            
         BL    T6J2                VALUE TOO LONG - WRONG TAB ENTRY             
         MVI   FTBMARK,C'C'        SET COMPARE MARKER (NOT TM )                 
         MVC   FTBD+FTBTBLEN(1),FTBSIGN SAVE SIGN FROM BEING OVERWRTTEN         
         EX    R7,VALMOVE          PUT REAL VALUE AND LENGTH IN TABLE           
         MVC   FTBLEN,WORK                                                      
         SPACE 1                                                                
T6K0     OC    FILPREED,FILPREED   ANY PRE-EDITING OF FILTER VALUE              
         BZ    T6K1                                                             
         L     RF,RECFNTRY                                                      
         CLI   FILPREED,X'FF'      IN OVERLAY                                   
         BE    T6K01                                                            
         MVC   HALF,FILPREED       OR IN THE ROOT                               
         LH    RF,HALF                                                          
         AR    RF,RC                                                            
         L     RF,0(RF)                                                         
T6K01    GOTO1 (RF),DMCB,(RC)                                                   
         TM    DMCB,1                                                           
         BO    INFERR                                                           
         SPACE 1                                                                
T6K1     XC    FTBSR,FTBSR         A(ROUTINE FOR USE AT FILTER TIME)            
         LH    R5,FILEDIT                                                       
         LTR   R5,R5                                                            
         BZ    T6L                                                              
         CLI   FILEDIT,X'FF'       S/R IS IN OVERLAY                            
         BNE   T6K2                                                             
         MVC   FTBSR,RECFNTRY                                                   
         B     T6L                                                              
T6K2     AR    R5,RC               S/R IS IN ROOT                               
         MVC   FTBSR,0(R5)                                                      
         B     T6L                                                              
VALMOVE  MVC   FTBVAL(0),0(R1)                                                  
         SPACE 1                                                                
T6L      MVC   HALF,FILELMNT                                                    
         LH    R5,HALF                                                          
         AR    R5,RC                                                            
         ST    R5,FTBELMNT         A(A(ELEMENT CONTAINING FILTER))              
         MVC   FTBDISP,FILDISP     DISPLACEMENT INTO ELEMENT                    
         OC    FTBSTAT,FILSTAT                                                  
         SPACE 1                                                                
         TM    FILSTAT,X'02'       REVERSE SIGN FILTER                          
         BNO   T6L0                                                             
         XI    FTBSIGN,X'02'            'P' TO 'N' AND 'N' TO 'P'               
         SPACE 1                                                                
T6L0     TM    FILSTAT,X'18'       POSITIVE KEY FILTERS ARE HANDLED             
         BZ    T6L1                DIFFERENTLY TO MINIMISE I/O                  
         TM    FILSTAT,1 ST(ATEMENT) DATE FILTERS ARE                           
         BO    T6L1                DISABLED FROM NORMAL FILTERING               
         CLI   FTBSIGN,C'P'                                                     
         BNE   T6L1                                                             
         MVI   KEYFILT,C'Y'                                                     
         ZIC   RF,FTBDISP                                                       
         IC    R7,FTBLEN                                                        
         BCTR  R7,0                                                             
         TM    FILSTAT,X'10'       START KEY                                    
         BZ    *+12                                                             
         LA    RE,STAFILT-15(RF)                                                
         EX    R7,FILTMOVE                                                      
         TM    FILSTAT,X'08'       END KEY                                      
         BZ    T6L2                                                             
         LA    RE,ENDFILT-15(RF)                                                
         EX    R7,FILTMOVE                                                      
         B     T6L2                                                             
FILTMOVE MVC   0(0,RE),FTBVAL                                                   
         SPACE 2                                                                
T6L1     LA    R6,FTBTBLEN(R6)     BUMP TO NEXT FILTER INPUT                    
T6L2     LA    R3,L'SCANBLCK(R3)                                                
         ZIC   RF,FNDX             INCREMENT SUBFIELD NUMBER                    
         LA    RF,1(RF)                                                         
         STC   RF,FNDX                                                          
         B     T6F2                                                             
T6M      MVI   FTBELMNT,X'FF'      PUT TERMINATOR ON GENERATED TABLE            
         OI    4(R2),X'20'                                                      
*&&US                                                                           
         CLI   5(R2),0                                                          
         BNE   *+8                                                              
         MVI   FIFLDIND,0          = NO INPUT THIS TIME                         
*&&                                                                             
         MVI   FNDX,0                                                           
         GOTO1 =A(CLEARTWA),DMCB,(RC),RR=RELO                                   
         B     T6N                                                              
         EJECT                                                                  
*              ADDITIONAL FILTER VALIDATION ROUTINES                            
*                                                                               
DATECHK  NTR1                      CHECK AND CONVERT A DATE FILTER              
         LR    R3,R1                                                            
         LA    R7,FTBVAL                                                        
DCHK1    SR    R2,R2                                                            
         TM    FILSTAT,X'40'       DMY                                          
         BO    *+8                                                              
         LA    R2,2                OTHERWISE MY                                 
         GOTO1 VDATVAL,DMCB,((R2),0(R3)),WORK                                   
         OC    DMCB,DMCB                                                        
         BZ    DCHKERR                                                          
DCHK2    GOTO1 VDATCON,DMCB,(0,WORK),(1,(R7))                                   
         MVI   FTBMARK,C'C'                                                     
         MVI   FTBLEN,3                                                         
         TM    FILSTAT,X'20'       MY                                           
         BNO   DCHKX                                                            
         MVI   FTBLEN,2                                                         
         CLI   FILDISP,(TRNSBTCH-TRANSD)     MOS IS SPECIAL FORMAT              
         BNE   DCHKX                                                            
*        OI    0(R7),X'F0'         YEAR                                         
*        TM    1(R7),X'10'         MONTH                                        
*        BO    *+12                                                             
*        OI    1(R7),X'F0'                                                      
*        B     DCHK3                                                            
*        ZIC   R5,1(R7)            10-A,11-B,12-C                               
*        LA    R5,X'B1'(R5)                                                     
*        STC   R5,1(R7)                                                         
DCHK3    TM    FTBSTAT,5           1ST TIME CHECK FOR MOS RANGE                 
         BO    DCHKX                                                            
         LA    R0,1                                                             
         LA    R1,6(R3)                                                         
         CLI   0(R3),C'-'                                                       
         BE    DCHK4                                                            
         BXLE  R3,R0,*-8                                                        
         OI    OPTIONS,2           NO END MOS SPECIFIED BY USER                 
         OI    FTBSTAT,5           SET END MOS FROM START MOS                   
         MVC   FTBVAL+2(2),FTBVAL                                               
         B     DCHKX                                                            
DCHK4    LA    R3,1(R3)                                                         
         LA    R7,FTBVAL+2                                                      
         OI    OPTIONS,2                                                        
         OI    FTBSTAT,5                                                        
         B     DCHK1                                                            
DCHKERR  MVI   ERROR,DATINVAL                                                   
         SR    RE,RE                                                            
DCHKX    LTR   RE,RE                                                            
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              CONTINUATION SCREENS FOR SOME TYPES REQUIRE TWA1 TO BE           
*              READ INTO TIA AND CONTROL TO BE PASSED TO OVERLAY NOW            
         SPACE 1                                                                
T6N      TM    AUTHTYPE,TWA1USER                                                
         BNO   T6N0                                                             
         CLI   LASTKMK,0                                                        
         BE    T6N0                                                             
         BAS   RE,READTIA                                                       
         BZ    INFERR                                                           
         B     TOVERLAY                                                         
         EJECT                                                                  
*              'OR' STAFILT FIELDS INTO INITIAL KEY                             
*                                                                               
*              SET UP KEYMASK  AS A KEY MASK WITH ZEROS IN VARIABLE KEY         
*              COMPONENTS AND ONES OTHERWISE.                                   
*                                                                               
*              SET UP KEYCHK AS A COPY OF KEY WITH VARIABLE COMPONENTS          
*              ZEROISED.                                                        
         SPACE 1                                                                
T6N0     CLI   LASTKMK,0                                                        
         BNE   *+10                                                             
         OC    KEY+15(L'ACCKEY-15),STAFILT                                      
         MVI   KEYMASK,X'FF'                                                    
         MVC   KEYMASK+1(L'ACCKEY-1),KEYMASK                                    
         L     R3,RECKEY                                                        
         USING KEYTABD,R3                                                       
         SPACE 1                                                                
T6N1     CLI   0(R3),0                                                          
         BE    T6P                                                              
         CLI   KEYVARY,C'V'                                                     
         BNE   T6O                                                              
         SR    R4,R4                                                            
         IC    R4,KEYDISP                                                       
         LA    R4,KEYMASK(R4)                                                   
         SR    R5,R5                                                            
         IC    R5,KEYLEN                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)                                                    
T6O      LA    R3,KEYTBLEN(R3)                                                  
         B     T6N1                                                             
         SPACE 1                                                                
T6P      MVC   KEYCHK(L'ACCKEY),KEY                                             
         NC    KEYCHK(L'ACCKEY),KEYMASK                                         
         L     RE,AIO                                                           
         CLC   KEY(L'ACCKEY),0(RE) HAVE WE ALREADY GOT THE FIRST RECORD         
         BE    T6Q0B                                                            
         B     T6Q0                                                             
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
*              READ A RECORD AND CHECK KEY                                      
         SPACE 1                                                                
T6Q      TM    DMARK,1             COME HERE FOR A READ SEQUENTIAL              
         BZ    T6Q0A               BUT IF SEQUENCE IS BROKEN READ HIGH          
         ZIC   R1,KEY+41                                                        
         LA    R1,1(R1)                                                         
         STC   R1,KEY+41                                                        
T6Q0     MVI   DMARK,0             COME HERE FOR A READ HIGH                    
         BAS   RE,HIGH                                                          
         B     *+8                                                              
T6Q0A    BAS   RE,SEQ                                                           
         LA    R2,INFRECH                                                       
         BZ    INFERR                                                           
         SPACE 1                                                                
T6Q0B    DS    0H                  GET I/O COUNT FROM GETFACT                   
         GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,0(R1)            R1=A(GETFACT BLOCK)                          
         MVC   RECOUNT+2(2),FATIOCNT-FACTSD(R1)                                 
*                                  CHECK KEY                                    
         SPACE 1                                                                
         L     RE,AIO                                                           
         MVC   KEY(L'ACCKEY),0(RE)                                              
         NC    KEY(L'ACCKEY),KEYMASK                                            
         XC    KEY(L'ACCKEY),KEYCHK                                             
         BZ    T6Q2                                                             
         MVI   0(RE),RUNLAST       NO MORE RELEVANT RECORDS SO PASS             
         B     T6Y                 CONTROL TO OVERLAY FOR EOT ACTION            
         SPACE 1                                                                
*                                  HAVE WE REACHED LIMIT ?                      
T6Q2     CLC   RECOUNT,LIMIT                                                    
         BNH   T6Q3                                                             
         TM    AUTHTYPE,TWA1USER   DON'T STOP A TWA1 USER                       
         BO    T6Q3                                                             
         CLI   PHASE,X'0E'         ORDER SUMMARY CANT STOP IN MID-ACCNT         
         BE    *+12                                                             
         CLI   PHASE,5             LIST WITH LEVEL FILTER CANT STOP             
         BNE   *+12                IN MID ACCOUNT                               
         TM    OPTIONS,X'20'       (LEVTOTS)                                    
         BO    T6Q3                                                             
         CLI   LINE+1,18           IS SCREEN FULL                               
         BL    T6Q22               BRANCH IF NOT                                
         CLI   PHASE,2             DETAIL                                       
         BE    *+12                                                             
         CLI   PHASE,6             JOB DETAIL                                   
         BNE   *+12                                                             
         TM    OPTIONS,X'84'       DETAILS OR XTRA DETAILS NEED                 
         BNZ   *+12                EXTRA LINE - SO SCREEN IS NOW FULL           
         CLI   LINE+1,19           IS SCREEN FULL                               
         BL    T6Q22               BRANCH IF NOT                                
         MVI   LINE+1,3            NEXT TIME START FROM TOP                     
         B     TFULL               PUT OUT 'RECORDS DISPLAYED' MESSAGE          
*                                                                               
*                             SCREEN NOT FULL - BUT READ XXX RECORDS            
*                                                                               
T6Q22    EDIT  LIMIT,(4,SOFAR)                                                  
         MVC   INFHEAD(L'SOFAR),SOFAR                                           
         LH    R3,LINE             CLEAR END OF SCREEN                          
         MH    R3,=H'86'                                                        
         LA    R3,INFDATAH(R3)                                                  
         TWAXC (R3),PROT=Y                                                      
         OI    LASTKMK,X'80'       NEXT IS CONTINUATION                         
         B     TFULL1                                                           
SOFAR    DC    CL45'     RECORDS READ SO FAR - HIT ENTER FOR NEXT'              
         SPACE 1                                                                
T6Q3     L     RE,AIO                                                           
         MVC   KEY(L'ACCKEY),0(RE) POSITIVE FILTERING OF KEY ELEMENTS           
         CLI   KEYFILT,C'Y'                                                     
         BNE   T6R                                                              
         LA    R1,KEYTABLE                                                      
         SR    R3,R3                                                            
         SR    R4,R4                                                            
T6Q4     CLI   0(R1),X'FF'                                                      
         BE    T6R                                                              
         IC    R3,0(R1)            DISPLACEMENT OF KEY INTO STA/ENDFILT         
         IC    R4,1(R1)            LENGTH MINUS 1 OF KEY                        
         LA    R5,KEY+15(R3)                                                    
         LA    R6,STAFILT(R3)                                                   
         EX    R4,COMPFILT                                                      
         BNL   T6Q5                                                             
         LA    R7,L'ACCKEY-16                                                   
         EX    R7,*+8                                                           
         B     T6Q0                                                             
         MVC   0(0,R5),0(R6)                                                    
T6Q5     LA    R6,ENDFILT(R3)                                                   
         EX    R4,COMPFILT                                                      
         BNH   T6Q6                                                             
         MVI   0(R5),X'FF'                                                      
         B     T6Q0                                                             
T6Q6     LA    R1,2(R1)                                                         
         B     T6Q4                                                             
COMPFILT CLC   0(0,R5),0(R6)                                                    
KEYTABLE DC    X'0001020E110214051A00FF'                                        
*                WC  CAC DTE REF SUB                                            
         EJECT                                                                  
*              CHECK SECURITY                                                   
         SPACE 1                                                                
T6R      TM    AUTHTYPE,X'20'      IS SECURITY CHECK REQUIRED                   
         BZ    T6T                                                              
         L     R9,AIO                                                           
         CLC   L'ACKEYACC(17,R9),SPACES  MUST BE AN A/C REC                     
         BNE   T6T                                                              
         BAS   RE,SECURCHK                                                      
         BZ    *+16                                                             
         TM    ACSTATUS-ACKEYD(R9),X'80'                                        
         BO    TNEXT                                                            
         B     T6V                                                              
         BAS   RE,NEXTKEY          BUMP TO NEXT KEY AT THIS LEVEL               
         B     TNEXT               IF WE DIDNT PASS SECURITY CHECK              
*                                                                               
SECURCHK NTR1                                                                   
         LA    R1,WORK             CONSTRUCT HIERARCHY EL IN WORK               
         USING ACHEIRD,R1                                                       
         MVC   ACHRLEVA,SAVEHIER                                                
         MVC   ACHRLEVB,SAVEHIER+2                                              
         MVC   ACHRLEVC,SAVEHIER+4                                              
         MVC   ACHRLEVD,SAVEHIER+6                                              
         DROP  R1                                                               
         GOTO1 VACSPLIT,DMCB,(4,AIO),WORK,ACLKEYS                               
         LA    R1,1                                                             
         LA    R2,ACLKEYS                                                       
         L     R9,AIO                                                           
T6R1     CLC   0(L'ACKEYACC,R9),0(R2)                                           
         BE    T6R2                                                             
         LA    R2,L'ACKEYACC(R2)                                                
         LA    R1,1(R1)                                                         
         B     T6R1                                                             
         SPACE 1                                                                
T6R2     STH   R1,LEVEL            STORE LEVEL AS BINARY HALFWORD               
         STC   R1,LEVEL+2                                                       
         OI    LEVEL+2,X'F0'                   AS DECIMAL CHARACTER             
         LA    R2,X'1F'                                                         
         SRL   R2,1                            AND AS BIT PATTERN               
         BCT   R1,*-4                          IE      X'08','04','02'          
         STC   R2,LEVEL+3                      LEVEL -   1   1-2  1-3           
         SPACE 1                                                                
         CLI   FRSTSW,0            IF THIS IS 1ST RECORD OF THIS                
         BNE   T6R6                ENQUIRY AND ISN'T LEVEL 1, CHECK             
         MVI   FRSTSW,1                                                         
         CLI   LEVEL+1,1           SECURITY AT HIGHER LEVELS                    
         BE    T6R6                                                             
         CLI   LASTKMK,0                                                        
         BNE   T6R6                                                             
         MVC   KEYB(L'ACCKEY),SPACES                                            
         MVC   SAVLEVEL,LEVEL                                                   
         SR    R3,R3                                                            
T6R3     LR    R4,R3                                                            
         LA    R1,1(R3)                                                         
         STH   R1,LEVEL                                                         
         MH    R4,=H'15'                                                        
         LA    R2,ACLKEYS(R4)                                                   
         MVC   KEYB(L'ACKEYACC),0(R2)                                           
         BAS   RE,READB                                                         
         BNZ   T6R4                                                             
         MVI   ERROR,LEDGNVAL      IF WE CANT FIND IT, THE LEDGER               
         L     RD,AREGSAVE         HIERARCHY ELEMENT IS NOT COMPATIBLE          
         LM    RE,RC,12(RD)        WITH THE ACCOUNTS IN IT                      
         LA    R2,INFKEYH                                                       
         B     INFERR                                                           
*                                                                               
T6R4     L     R9,AIOB                                                          
         ST    R9,DMCB                                                          
         BAS   RE,SETELAD                                                       
         LA    R3,1(R3)            INCREMENT LEVEL NUMBER                       
         ICM   R9,15,ASTA                                                       
         BZ    *+14                                                             
         USING ACSTATD,R9                                                       
         CLC   TERMAUTH+1(1),ACSTSECY+1                                         
         BL    T6R7                SECURITY VIOLATION                           
         DROP  R9                                                               
*                                                                               
         TM    SAVLTOFF,X'F0'      TEST OFFICE IN FILTER LEDGER                 
         BNO   *+14                NO                                           
         CLC   OFFICE,SPACES       TEST FOR AN OFFICE                           
         BNH   T6R4A               NO-SKIP OFFICE CHECK AT HIGHER LEV.          
         SPACE 1                                                                
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAREC,AIOB                                                     
         MVC   OFFAOPOS,SAVLTOFF                                                
         MVC   OFFAOFFC,OFFICE                                                  
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BNE   T6R7                                                             
         DROP  R1                                                               
         SPACE 1                                                                
T6R4A    LA    R3,1(R3)                                                         
         CH    R3,SAVLEVEL                                                      
         BE    T6R5                                                             
         BCT   R3,T6R3                                                          
T6R5     MVC   LEVEL,SAVLEVEL                                                   
*                                                                               
* NOW APPLY SECURITY CHECKS TO ACCOUNT PASSED TO ROUTINE                        
*                                                                               
T6R6     L     R9,AIO                                                           
         ST    R9,DMCB                                                          
         BAS   RE,SETELAD                                                       
         ICM   R9,15,ASTA                                                       
         BZ    *+14                                                             
         USING ACSTATD,R9                                                       
         CLC   TERMAUTH+1(1),ACSTSECY+1                                         
         BL    T6R6A               SECURITY VIOLATION                           
         DROP  R9                                                               
*                                                                               
         TM    SAVLTOFF,X'F0'      TEST OFFICE IN FILTER LEDGER                 
         BNO   T6R61                                                            
         OC    ABAL,ABAL           TEST FOR LOW LEVEL ACCOUNT                   
         BNZ   T6R61               YES-PERFORM TEST                             
         CLC   OFFICE,SPACES       TEST FOR AN OFFICE                           
         BNH   T6RXIT              NO-ALLOW ACCESS                              
         SPACE 1                                                                
T6R61    L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAREC,AIO                                                      
         MVC   OFFAOPOS,SAVLTOFF                                                
         MVC   OFFAOFFC,OFFICE                                                  
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BE    T6RXIT                                                           
         DROP  R1                                                               
         SPACE 1                                                                
T6R6A    LH    R3,LEVEL                                                         
T6R7     SR    RE,RE               CC=EQU IF SECURITY CHECK FAILED              
T6RXIT   LTR   RE,RE                                                            
         XIT1  REGS=(R3)           R3=LEVEL OF NEXT A/C RECORD TO READ          
         SPACE 3                                                                
NEXTKEY  NTR1                      SETS KEY IN IO TO READ NEXT AT LEVEL         
         BCTR  R3,0                IN R3                                        
         SLA   R3,1                                                             
         ZIC   R2,SAVEHIER(R3)                                                  
         LA    R2,3(R2)            ADD IN COMP/UNIT/LEDG                        
         A     R2,AIO                                                           
         MVI   0(R2),X'FF'                                                      
         OI    DMARK,1                                                          
         B     EXIT                                                             
         EJECT                                                                  
*              SET UP ELEMENT ADDRESS TABLE IN GLOBAL W/S                       
*              ON ENTRY DMCB = A(IO OR IOB)                                     
         SPACE 1                                                                
SETELAD  NTR1  BASE=ABASE                                                       
         L     R8,A2NDBASE                                                      
         XC    AELTAB(AELTBLEN),AELTAB                                          
         L     R3,DMCB                                                          
         ST    R3,AKEY                                                          
         USING ACKEYD,R3           ENSURE RECORD TERMINATOR                     
         LR    R4,R3                                                            
         MVC   HALF,ACLENGTH                                                    
         AH    R4,HALF                                                          
         MVI   0(R4),0                                                          
         LA    R3,49(R3)                                                        
SE1      CLI   0(R3),0                                                          
         BE    SE4                                                              
         SR    R5,R5                                                            
SE2      L     R4,=A(ELLIST)                                                    
         A     R4,RELO                                                          
         AR    R4,R5                                                            
         CLI   0(R4),X'FF'                                                      
         BE    SE3                                                              
         CLC   0(1,R4),0(R3)                                                    
         BE    *+12                                                             
         LA    R5,1(R5)                                                         
         B     SE2                                                              
         SLL   R5,2                                                             
         L     R7,AELTAB(R5)                                                    
         LTR   R7,R7               ONLY FIRST ELEMENT OF A TYPE                 
         BNZ   *+8                                                              
         ST    R3,AELTAB(R5)                                                    
         SPACE 1                                                                
         CLI   0(R4),X'50'         SOME HARD CODE FOR 50 EL.                    
         BNE   SE3                                                              
         SPACE 1                                                                
         CLI   2(R3),C'D'          IF IT'S A DISCOUNT                           
         BNE   SE2A                                                             
         ST    R3,ACSHD            THEN STORE IT SPECIALLY                      
         SPACE 1                                                                
SE2A     CLI   2(R3),C'T'          IF IT'S A TAX                                
         BNE   SE3                                                              
         ST    R3,ATAX             THEN STORE IT AS SUCH                        
         SPACE 1                                                                
SE3      IC    R5,1(R3)                                                         
         AR    R3,R5                                                            
         B     SE1                                                              
         SPACE 1                                                                
SE4      MVC   OFFICE,SPACES       SET UP OFFICE                                
         L     R3,AKEY                                                          
         CLC   1(2,R3),PRODUNIT                                                 
         BE    SE4A                                                             
         CLI   SAVLTOFF,12                                                      
         BH    SE6                                                              
         SR    R1,R1                                                            
         ICM   R1,1,SAVLTOFF                                                    
         BZ    EXIT                                                             
         LA    R1,2(R1,R3)                                                      
         OC    OLDOFF,0(R1)                                                     
         B     EXIT                                                             
         SPACE 1                                                                
SE4A     LA    R3,SOFFICE+2        COMPOSITE OFFICE FOR PRODUCTION              
         CLI   LEVEL+1,3                                                        
         BH    SE5                                                              
         LH    R4,LEVEL                                                         
         BCTR  R4,0                                                             
         SLL   R4,1                X 2                                          
         LA    R3,SOFFICE(R4)                                                   
         SH    R3,=Y(L'OFFICE)                                                  
         MVC   2(2,R3),SPACES      DEFAULT                                      
         ICM   R9,15,APRO                                                       
         BZ    *+10                                                             
         USING ACPROFD,R9                                                       
         MVC   2(2,R3),ACPROFFC                                                 
         DROP  R9                                                               
         LTR   R4,R4               LEVEL 1 - NO COMPOSITE                       
         BZ    SE5                                                              
         CLC   2(2,R3),SPACES      MISSING AT THIS LEVEL                        
         BH    *+10                                                             
         MVC   2(2,R3),0(R3)       SO MOVE IN HIGHER LEVEL                      
SE5      OC    OFFICE,2(R3)                                                     
         B     EXIT                                                             
         SPACE 1                                                                
SE6      TM    SAVLTOFF,X'F0'      OFFICE IN STATUS ELEMENT                     
         BNO   EXIT                                                             
         ICM   R9,15,ASTA                                                       
         BZ    EXIT                                                             
         USING ACSTATD,R9                                                       
         LA    RF,ACSTFILT                                                      
         CLI   SAVLTOFF,C'1'                                                    
         BE    SE7                                                              
         LA    RF,ACSTFILT+1                                                    
         CLI   SAVLTOFF,C'2'                                                    
         BE    SE7                                                              
         LA    RF,ACSTANAL                                                      
         CLI   SAVLTOFF,C'3'                                                    
         BE    SE7                                                              
         LA    RF,ACSTSUB                                                       
SE7      MVC   OLDOFF,0(RF)                                                     
         B     EXIT                                                             
         DROP  R3,R9                                                            
*                                                                               
         EJECT                                                                  
*              SAVE ELEMENT ADDRESSES                                           
         SPACE 1                                                                
T6T      L     R3,AIO                                                           
         ST    R3,DMCB                                                          
         BAS   RE,SETELAD                                                       
         B     T6V                                                              
         EJECT                                                                  
*              APPLY FILTERS VIA FILTER TABLE USING FILTER ROUTINE              
*              ON ENTRY DMCB(1)='R' IF WE ARE FILTERING RECORDS AS              
*                               OPPOSED TO ELEMENTS                             
*              ON EXIT       CC=EQU IF NOT REQUIRED                             
*                            CC=NEQ IF REQUIRED                                 
         SPACE 1                                                                
T6V      MVI   DMCB,C'R'                                                        
         BAS   RE,FILTER                                                        
         BZ    TNEXT                                                            
         B     T6Y                                                              
         SPACE 1                                                                
FILTER   NTR1  BASE=ABASE                                                       
         L     R8,A2NDBASE                                                      
         MVC   THISTMBR,FTMBR      SET EXECUTED BRANCH AFTER TEST UNDER         
         CLI   DMCB,C'R'           MASK TO BO OR BNO DEPENDING ON DMCB          
         BE    *+8                                                              
         XI    THISTMBR+1,X'F0'                                                 
         SR    R9,R9                                                            
         LA    R6,FILTAB                                                        
         USING FTBD,R6                                                          
         SPACE 1                                                                
T6V1     CLI   FTBELMNT,X'FF'      END OF FILTER TABLE                          
         BE    FILTKEEP                                                         
         TM    FTBSTAT,X'01'       DISABLED                                     
         BO    T6V5                                                             
         TM    FTBSTAT,X'04'       IS THIS A RECORD OR ELEMENT FILTER           
         EX    R9,THISTMBR                                                      
         MVC   FULL,FTBELMNT                                                    
         L     R2,FULL                                                          
         L     R2,0(R2)                                                         
         LTR   R2,R2                                                            
         BNZ   T6V2                IF ELEMENT CONTAINING FILTER DOES            
         CLI   FTBSIGN,C'N'        NOT EXIST                                    
         BE    T6V5                NEGATIVE FILTER IS SATISFIED                 
         B     FILTSKIP            POSITIVE ONE IS NOT                          
         SPACE 1                                                                
T6V2     ZIC   R5,FTBDISP                                                       
         AR    R2,R5               R2 = FIELD ADDRESS                           
         ZIC   R3,FTBLEN           R3 = FIELD LENGTH                            
         LA    R5,FTBVAL           R5 = A(MASK OR COMPARE VALUE)                
         MVC   THISBR,FBRANCH                                                   
         SPACE 1                                                                
         OC    FTBSR,FTBSR         CALL CONVERSION S/R IF ANY                   
         BZ    T6V3                                                             
         L     RF,FTBSR                                                         
         GOTO1 (RF),DMCB,(RC)                                                   
         SPACE 1                                                                
T6V3     CLI   FTBMARK,C'C'        COMPARE (NOT TEST UNDER MASK)                
         BNE   T6V4                                                             
         CLI   FTBSIGN,C'N'                                                     
         BE    *+8                                                              
         XI    THISBR+1,X'F0'      SWITCH EXECUTED BRANCH FROM 'BE' TO          
         BCTR  R3,0                'BNE' IF POSITIVE                            
         EX    R3,FCOMPARE                                                      
         EX    R9,THISBR                                                        
         B     T6V5                                                             
         SPACE 1                                                                
T6V4     CLI   FTBSIGN,C'P'        TEST UNDER MASK (NOT COMPARE)                
         BE    *+8                                                              
         XI    THISBR+1,X'F0'      SWITCH EXECUTED BRANCH FROM 'BZ' TO          
         ZIC   R7,0(R5)            'BNZ' IF NEGATIVE                            
         EX    R7,FTEST                                                         
         EX    R9,THISBR                                                        
         SPACE 1                                                                
T6V5     LA    R6,FTBTBLEN(R6)     BUMP TO NEXT FILTER                          
         B     T6V1                                                             
         SPACE 1                                                                
FILTSKIP SR    RE,RE               CC=EQU FOR SKIP                              
FILTKEEP LTR   RE,RE               CC=NEQ FOR KEEP                              
         B     EXIT                                                             
         SPACE 1                                                                
FBRANCH  BE    FILTSKIP            EXECUTED INSTRUCTIONS                        
FTMBR    BO    T6V5                                                             
FCOMPARE CLC   0(0,R2),0(R5)                                                    
FTEST    TM    0(R2),0                                                          
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
*              SET UP STANDARD SUBHEADS  AND PASS CONTROL TO OVERLAY            
         SPACE 1                                                                
T6Y      CLI   VIRGIN,0            SET UP SUBHEADS                              
         BNE   TOVERLAY                                                         
         MVI   VIRGIN,1                                                         
         LA    R6,INFDATAH                                                      
         USING LINED,R6                                                         
         MVC   WORK(87),SPACES                                                  
         MVC   WORK(5),=C'UNIT='                                                
         MVC   WORK+5(36),SAVEUNIT                                              
         MVC   WORK+42(9),=C'/ LEDGER='                                         
         MVC   WORK+51(36),SAVELEDG                                             
         GOTO1 VSQASHER,DMCB,WORK,87                                            
         MVC   LINEDATA,WORK                                                    
         OI    LINEHDR+6,X'80'                                                  
         SPACE 1                                                                
TOVERLAY L     RF,RECDNTRY         PASS CONTROL TO OVERLAY                      
         GOTO1 (RF),DMCB,(RC)                                                   
*                                  ON RETURN VIRGIN=C'H' IF HITS                
*                                            LASTKMK X'40'= THERE IS A          
*                                            PREVIOUS SCREEN                    
         BP    TNEXT                         CC=POS FOR READ NEXT REC           
         BZ    TEND                             EQU FOR END OF ENQUIRY          
         BM    TFULL                            NEG FOR SCREEN FULL             
         EJECT                                                                  
*              END OF ENQUIRY                                                   
         SPACE 1                                                                
TEND     LA    R2,INFRECH                                                       
         CLC   INFKEY(4),=C'NEXT'                                               
         BNE   TEND1                                                            
         MVC   WORK(14),INFKEY+9                                                
         MVC   INFKEY,SPACES                                                    
         MVC   INFKEY(14),WORK                                                  
TEND1    OI    INFKEYH+6,X'80'                                                  
         NI    LASTKMK,X'7F'       UNSET CONTINUATION BIT                       
         CLI   VIRGIN,C'H'                                                      
         BE    TEND2                                                            
         XC    LINE,LINE                                                        
         GOTO1 =A(CLEARTWA),DMCB,(RC),RR=RELO                                   
         MVC   INFHEAD(L'NOHITS),NOHITS                                         
         B     INFEXIT                                                          
TEND2    MVC   INFHEAD(L'ENDREC),ENDREC                                         
         B     INFEXIT                                                          
NOHITS   DC    CL21'NO RECORDS TO DISPLAY'                                      
ENDREC   DC    CL28'END OF RECORDS FOR THIS TYPE'                               
*                                                                               
         EJECT                                                                  
*              SCREEN FULL                                                      
         SPACE 1                                                                
TFULL    MVC   INFHEAD(L'HITNEXT),HITNEXT                                       
TFULL1   OI    LASTKMK,X'80'                                                    
         OI    INFKEYH+6,X'81'                                                  
         LA    R2,INFTABH                                                       
         L     RE,AIO                                                           
         MVC   LASTKEY,0(RE)                                                    
         CLC   INFKEY(4),=C'NEXT'                                               
         BE    INFEXIT                                                          
         CLC   INFKEY(4),=C'PREV'                                               
         BNE   *+14                                                             
         MVC   INFKEY(9),=CL9'NEXT FOR'                                         
         B     INFEXIT                                                          
         MVC   WORK(14),INFKEY                                                  
         MVC   INFKEY(9),=CL9'NEXT FOR'                                         
         MVC   INFKEY+9(14),WORK                                                
         B     INFEXIT                                                          
HITNEXT  DC    CL38'RECORDS DISPLAYED - HIT ENTER FOR NEXT'                     
         EJECT                                                                  
*              UPDATE KEY AND BRANCH TO READ NEXT RECORD                        
         SPACE 1                                                                
TNEXT    CLI   KEYMASK+L'ACCKEY-1,0    IF KEY IS VARIABLE TO FULL LNTH          
         BE    T6Q                 READ SEQUENTIAL                              
         LA    R3,L'ACCKEY         OTHERWISE INSERT X'FF' AFTER LAST            
         LA    R2,KEYMASK-1(R3)    VARIABLE KEY COMPONENT OF LAST REC           
         CLI   0(R2),X'FF'                                                      
         BNE   *+8                                                              
         BCT   R3,*-12                                                          
         L     RE,AIO                                                           
         MVC   KEY(L'ACCKEY),0(RE)                                              
         LTR   R3,R3                                                            
         BZ    T6Q                                                              
         LA    R2,KEY(R3)                                                       
         MVI   0(R2),X'FF'                                                      
         B     T6Q0                AND READ HIGH                                
         EJECT                                                                  
*              ERROR HANDLING AND EXIT                                          
         SPACE 1                                                                
INFERR   GOTO1 VGETMSG,DMCB+12,(ERROR,INFHEAD),(FNDX,DMCB),0                    
         CLI   VIRGIN,C'H'                                                      
         BE    INFEXIT                                                          
         MVI   LASTKMK,0                                                        
         LA    R3,INFKEYH                                                       
         CR    R2,R3                                                            
         BE    HELPKEY                                                          
         LA    R3,INFFILTH                                                      
         CR    R2,R3                                                            
         BE    HELPFILT                                                         
         LA    R3,INFRECH                                                       
         CR    R2,R3                                                            
         BE    HELPREC                                                          
         B     INFX1                                                            
INFEXIT  TM    HELPKMK,X'01'                                                    
         BNO   *+12                                                             
         LA    R2,INFKEYH                                                       
         B     INFX2                                                            
         TM    HELPKMK,X'02'                                                    
         BNO   *+8                                                              
         LA    R2,INFCACH                                                       
INFX1    LTR   R2,R2                                                            
         BNZ   *+8                                                              
         LA    R2,INFRECH                                                       
INFX2    OI    6(R2),X'40'                                                      
         CLI   VIRGIN,C'H'                                                      
         BNE   *+8                                                              
         MVI   VIRGIN,1                                                         
         XMOD1 1                                                                
         EJECT                                                                  
*              EDITING SUBROUTINES FOR KEY COMPONENTS & FILTER FIELDS           
*              ON ENTRY P1 = A(GWS) -(FOR CONSISTENCY WITH OVERLAYS)            
*                       R2 = A(FIELD TO BE EDITED)                              
*                       R3 = LENGTH OF FIELD TO BE EDITED - IF REQUIRED         
*              ON EXIT  R2 = A(EDITED FIELD)                                    
*                       R3 = LENGTH OF EDITED FIELD - IF REQUIRED               
*              ON ERROR P1 BYTE 0 IS SET TO X'01'                               
         SPACE 1                                                                
VDITACC  NTR1                      CHECKS ACCOUNT KEY AND SAVES NAME            
         MVC   KEY+3(12),0(R2)                                                  
         CLC   KEY(15),LASTKEY     SAME AS LAST                                 
         BE    VXIT                                                             
         MVI   ERROR,ACCTNVAL                                                   
         BAS   RE,READ                                                          
         BZ    VERRXIT                                                          
         L     RF,AIO                                                           
         ST    RF,DMCB                                                          
         BAS   RE,SETELAD                                                       
         CLI   PHASE,15            NEW BUDGETS CAN BE HIGH LEVEL                
         BE    VACC4                                                            
         ICM   RF,15,ABAL          CHECK LEVEL (MUST BE LOW)                    
         MVI   ERROR,WRNGLEVL                                                   
         BZ    VERRXIT                                                          
VACC4    BAS   RE,SECURCHK         CHECK SECURITY                               
         BNZ   VACC6                                                            
         MVI   ERROR,0                                                          
         OI    DMCB+8,4                                                         
         B     VERRXIT                                                          
VACC6    MVC   SAVEACCN,SPACES                                                  
         ICM   R9,15,ANAM          SAVE NAME                                    
         BNZ   *+14                                                             
         MVC   SAVEACCN(7),=CL7'NO NAME'                                        
         B     VXIT                                                             
         USING ACNAMED,R9                                                       
         ZIC   R4,ACNMLEN                                                       
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     VXIT                                                             
         MVC   SAVEACCN(0),ACNMNAME                                             
         B     VXIT                                                             
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITUNLE NTR1                      CHECKS UNIT OR LEDGER KEY AND SAVES          
         CLI   0(R4),C'U'          NAME AND (LEDGER) HIERARCHY ELEMENT          
         BNE   VUNLE2              IS IT UNIT OR LEDGER                         
         LA    R4,1                UNIT                                         
         LA    R7,SAVEUNIT                                                      
         MVI   ERROR,UNITNVAL                                                   
         B     VUNLE3                                                           
VUNLE2   DS    0H                  LEDGER                                       
         MVI   ERROR,LEDGNVAL                                                   
*&&US                                                                           
         LR    R4,R2                                                            
         BCTR  R4,0                                                             
         CLI   0(R4),C'T'          SPECIAL FOR UNIT T (TALENT COMM'LS)          
         BNE   VUNLE2D                                                          
         GOTO1 VHEXIN,DMCB,(R2),(R2),2 VALIDATE HEX LEDGER CODE                 
         OC    DMCB+12(4),DMCB+12                                               
         BZ    VERRXIT                                                          
         LA    R4,SCANBLCK+1                                                    
         CR    R4,R2               ARE WE EDITTING KEY FIELD                    
         BNE   VUNLE2D                                                          
         MVC   SCANBLCK+2(L'INFKEY-3),SCANBLCK+3 RE-SHUFFLE REST OF KEY         
         MVI   SCANBLCK+L'INFKEY-1,C' '                                         
*&&                                                                             
VUNLE2D  LA    R4,2                                                             
         LA    R7,SAVELEDG                                                      
         SPACE 1                                                                
VUNLE3   MVC   KEYB(L'ACCKEY),KEY  BOTH                                         
         LA    R5,KEYB(R4)                                                      
         MVC   0(1,R5),0(R2)                                                    
         TM    AUTHTYPE,2          IS PRODUCTION LEDGER ALLOWED                 
         BNO   *+14                                                             
         CLC   KEYB+1(2),SPROUNIT                                               
         BE    VERRXIT                                                          
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   KEYB(0),LASTKEY     SAME AS LAST                                 
         BE    VXIT                                                             
         BAS   RE,READB                                                         
         BZ    VERRXIT                                                          
         L     R9,AIOB                                                          
         ST    R9,DMCB                                                          
         BAS   RE,SETELAD                                                       
         ICM   R9,15,ASTA          CHECK SECURITY                               
         BZ    VUNLE4                                                           
         L     RE,AKEY                                                          
*&&US*&& CLC   1(2,RE),=C'1R'     LEDGER 1R FOR US                              
*&&UK*&& CLC   1(2,RE),=C'1P'     LEDGER 1P FOR UK                              
         BNE   *+12                                                             
         TM    TERMAUTH,X'10'                                                   
         BNO   VUNLE4                                                           
         USING ACSTATD,R9                                                       
         CLC   TERMAUTH+1(1),ACSTSECY+1                                         
         BNL   VUNLE5                                                           
VUNLE4   MVI   ERROR,0                                                          
         OI    DMCB+8,X'04'                                                     
         B     VERRXIT                                                          
VUNLE5   L     RE,AIOB                                                          
         MVC   LASTKEY,0(RE)       SAVE KEY                                     
         ICM   R9,15,ALED          SAVE ACLTOFF IF NUMERIC                      
         BZ    VUNLE5A                                                          
         USING ACLEDGD,R9                                                       
         MVC   SAVLTOFF,ACLTOFF                                                 
VUNLE5A  MVC   0(36,R7),SPACES     SAVE NAME                                    
         ICM   R9,15,ANAM                                                       
         BZ    VUNLE6                                                           
         USING ACNAMED,R9                                                       
         ZIC   R4,ACNMLEN                                                       
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),ACNMNAME                                                 
VUNLE6   ICM   R9,15,AHIE          SAVE HIERARCHY LENGTHS                       
         BZ    VXIT                                                             
         USING ACHEIRD,R9                                                       
         MVC   SAVEHIER(2),ACHRLEVA                                             
         MVC   SAVEHIER+2(2),ACHRLEVB                                           
         MVC   SAVEHIER+4(2),ACHRLEVC                                           
         MVC   SAVEHIER+6(2),ACHRLEVD                                           
         B     VXIT                                                             
         DROP  R9                                                               
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITCONT NTR1                      HANDLE CONTRA-ACCOUNT FILTER                 
         USING FTBD,R6                                                          
         LA    R2,WORK             SET UP DUMMY COMPARANDS                      
         BCTR  R3,0                R3 = LENGTH MINUS 1                          
         EX    R3,VCONTMV1         EQUATE COMPARANDS                            
         LA    R4,1                R4 = DISPLACEMENT INTO CA/C                  
         L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         EX    R3,VCONTCL1                                                      
         BE    VCONT4                                                           
*&&US                                                                           
         LA    R4,3                                                             
         EX    R3,VCONTCL3                                                      
         BE    VCONT4                                                           
*&&                                                                             
         SR    R4,R4               IF NOT AT CA/C +1 OR +3 LOOK AFTER           
         LA    R0,15               INITIAL SPACES                               
VCONT3   LA    R7,ACKEYCON(R4)                                                  
         CLI   0(R7),C' '                                                       
         BNE   *+12                                                             
         LA    R4,1(R4)                                                         
         BCT   R0,VCONT3                                                        
         EX    R3,VCONTCLC                                                      
         BE    VCONT4                                                           
         MVI   ACKEYDTE,X'FF'      NO MATCH SO READ FOR NEXT CONTRA             
         L     RD,AREGSAVE                                                      
         LM    RE,RC,12(RD)                                                     
         L     RE,AIO                                                           
         MVC   KEY(L'ACCKEY),0(RE)                                              
         B     T6Q0                                                             
         SPACE 1                                                                
VCONT4   CLI   ACKEYCON,C' '       LOOK FOR CONTRAS WITH BLANK IN               
         BNE   VCONT4A             PLACE OF CO CODE(EXPENSE ORDERS)             
         CLI   ACKEYCON+1,C' '     SO AS NOT TO STOP AT END OF THESE            
         BH    VCONT5              SLIGHTLY PECULIAR CONTRA RECORDS             
VCONT4A  LA    R9,0(R3,R4)         R9 = LENGTH + PREFIX CHARS - 1               
         EX    R9,VCONTMV2                                                      
         EX    R9,VCONTMV3                                                      
         MVI   KEYFILT,C'Y'        FROM NOW ON USE OPTIMAL KEY FILTER           
         OI    FTBSTAT,X'01'       DISABLE FILTAB ENTRY                         
         SPACE 1                                                                
VCONT5   CLC   SAVECACN,SPACES     RECORD REQUIRED                              
         BNE   VCONTX              WE ALREADY HAVE NAME                         
         LA    R9,L'ACKEYCON                                                    
         SR    R9,R4                                                            
         BCTR  R9,0                                                             
         LA    R7,ACKEYCON(R4)                                                  
         EX    R9,VCONTCLC         DOES THE CA/C READ MATCH THE FILTER          
         BNE   VCONTX              IN FULL                                      
         CLI   ACRECORD,X'43'      IF SO IS THIS A CA/C HEADER                  
         BNE   VCONTX                                                           
         LA    R7,ACRECORD         IF SO SAVE THE NAME                          
         USING TRSUBHD,R7                                                       
         IC    R4,TRSBLEN                                                       
         SH    R4,=H'18'                                                        
         BM    VCONTX              NO NAME IF BRANCH                            
         EX    R4,VCONTMV4                                                      
         SPACE 1                                                                
VCONTX   LA    R3,1(R3)            RESTORE R3                                   
         LA    R5,INFCAC                                                        
         XIT1  REGS=(R2,R5)                                                     
         SPACE 1                                                                
VCONTCLC CLC   0(0,R7),INFCAC      EXECUTED INSTRUCTIONS                        
VCONTCL1 CLC   ACKEYCON+1(0),INFCAC                                             
VCONTCL3 CLC   ACKEYCON+3(0),INFCAC                                             
VCONTMV1 MVC   WORK(0),INFCAC                                                   
VCONTMV2 MVC   STAFILT+2(0),ACKEYCON                                            
VCONTMV3 MVC   ENDFILT+2(0),ACKEYCON                                            
VCONTMV4 MVC   SAVECACN(0),TRSBNAME                                             
         DROP  R5,R6,R7                                                         
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITSTRT BCTR  R3,0                FIDDLE START DATE FILTER                     
         EX    R3,VDATCOMP                                                      
         BL    *+12                                                             
         LA    R2,WORK                                                          
         EX    R3,VDATMOVE                                                      
         LA    R3,1(R3)                                                         
         BR    RE                                                               
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITEND  BCTR  R3,0                FIDDLE END DATE FILTER                       
         EX    R3,VDATCOMP                                                      
         BH    *+12                                                             
         LA    R2,WORK                                                          
         EX    R3,VDATMOVE                                                      
         LA    R3,1(R3)                                                         
         BR    RE                                                               
VDATCOMP CLC   0(0,R2),0(R5)                                                    
VDATMOVE MVC   WORK(0),0(R5)                                                    
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITCASH NTR1                      CHECK CASH FILTER AND PACK IT                
         USING FTBD,R6                                                          
         ZAP   DUB,=P'0'                                                        
         CLC   FTBVAL(4),=C'ZERO'                                               
         BE    VCASH2                                                           
         ZIC   R5,FTBLEN                                                        
         GOTO1 VCASHVAL,DMCB,FTBVAL,(R5)                                        
         CLI   DMCB,X'FF'                                                       
         BE    VCASHERR                                                         
         L     R4,DMCB+4                                                        
         CVD   R4,DUB                                                           
VCASH2   MVC   FTBVAL(6),DUB+2                                                  
         MVI   FTBLEN,6                                                         
         MVC   FTBSIGN,FTBD+FTBTBLEN    RESTORE SAVED SIGN                      
         B     VXIT                                                             
VCASHERR MVI   ERROR,CASHERR                                                    
         B     VERRXIT                                                          
         DROP  R6                                                               
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITYPE  NTR1                      CHECK AND CVB TYPE FILTER                    
         USING FTBD,R6                                                          
         ZIC   R5,FTBLEN                                                        
         LA    R1,FTBVAL                                                        
VTYPE4   CLI   0(R1),C'0'                                                       
         BL    VTYPERR             NUMERIC TEST                                 
         CLI   0(R1),C'9'                                                       
         BH    VTYPERR                                                          
         LA    R1,1(R1)                                                         
         BCT   R5,VTYPE4                                                        
         SPACE 1                                                                
         ZIC   R5,FTBLEN                                                        
         LA    R1,FTBVAL                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         CVB   RF,DUB                                                           
         CH    RF,=H'255'                                                       
         BH    VTYPERR                                                          
         XC    FTBVAL,FTBVAL                                                    
         STC   RF,FTBVAL           SET TYPE FILTER VALUE                        
         MVI   FTBLEN,1                                                         
         B     VXIT                                                             
VTYPERR  MVI   ERROR,INVALID                                                    
         B     VERRXIT                                                          
         DROP  R6                                                               
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITACNM NTR1  BASE=ABASE          SET UP ACCOUNT AND CONTRA NAMES              
         L     R8,A2NDBASE         IN SCREEN LINE                               
         USING LINED,R6            R6 = LINE HEADER                             
         MVC   WORK(100),SPACES                                                 
         MVC   WORK(8),=C'ACCOUNT='                                             
         MVC   WORK+8(36),SAVEACCN                                              
         CLI   INFCACH+5,0         IS THERE A CONTRA                            
         BE    VACNMX                                                           
         MVC   WORK+45(15),=C'/ START CONTRA='                                  
         LA    R5,WORK+60                                                       
         TM    AUTHTYPE,X'08'                                                   
         BO    *+12                                                             
         TM    AUTHTYPE,X'10'                                                   
         BO    *+14                                                             
         MVC   WORK+47(7),WORK+53  FULL FILT IF STA+END KEY OR NEITHER          
         LA    R5,WORK+54                                                       
         MVC   0(36,R5),SAVECACN                                                
         CLI   SAVECACN,C' '       HAVE WE A CONTRA NAME                        
         BNE   *+10                                                             
         MVC   0(14,R5),INFCAC     IF NOT SHOW CODE                             
         GOTO1 VSQASHER,DMCB,WORK,96                                            
VACNMX   MVC   LINEDATA,WORK                                                    
         OI    LINEHDR+6,X'80'                                                  
         B     VXIT                                                             
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITOVER TM    3(R3),X'80'         HANDLE OVERRIDE OPTION                       
         BNO   VOVERR                                                           
         MVC   LIMIT,8(R3)                                                      
         B     T6L2                                                             
VOVERR   MVI   DMCB,1                                                           
         MVI   ERROR,INVALID                                                    
         BR    RE                                                               
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITOPT1 OI    OPTIONS,X'01'       SET OPTIONS X'01' BIT                        
         BR    RE                                                               
*                                  ************************************         
         SPACE 3                                                                
VERRXIT  MVI   DMCB,1                                                           
         B     EXIT                                                             
VXIT     MVI   DMCB,0                                                           
         XIT1  REGS=(R2,R3)                                                     
         SPACE 2                                                                
HELPREC  L     RF,=A(HLPREC)                                                    
         B     HELPALL                                                          
HELPKEY  L     RF,=A(HLPKEY)                                                    
         B     HELPALL                                                          
HELPFILT L     RF,=A(HLPFLT)                                                    
HELPALL  GOTO1 (RF),DMCB,(RC),RR=RELO                                           
         BE    T6E                                                              
         B     INFEXIT                                                          
         EJECT                                                                  
*              DATAMANAGER ROUTINES                                             
*              ON ENTRY KEY OR KEYB = THE KEY                                   
*              ON EXIT  IO  OR IOB  = ACCOUNT FILE RECORD                       
*                       CC = EQU IF ERROR                                       
*              ERROR MESSAGE FOR RECORD NOT FOUND IS ASSUMED TO BE SET          
         SPACE 1                                                                
READ     NTR1                                                                   
         LA    R3,=C'DMREAD'                                                    
         B     ALLDMA                                                           
         SPACE 1                                                                
HIGH     NTR1                                                                   
         LA    R3,=C'DMRDHI'                                                    
         B     ALLDMA                                                           
         SPACE 1                                                                
SEQ      NTR1                                                                   
         LA    R3,=C'DMRSEQ'                                                    
         B     ALLDMA                                                           
READB    NTR1  BASE=ABASE                                                       
         L     R8,A2NDBASE                                                      
         LA    R3,=C'DMREAD'                                                    
         B     ALLDMB                                                           
         SPACE 1                                                                
HIGHB    NTR1  BASE=ABASE                                                       
         L     R8,A2NDBASE                                                      
         LA    R3,=C'DMRDHI'                                                    
         B     ALLDMB                                                           
         SPACE 1                                                                
ALLDMA   LA    R5,KEY                                                           
         L     R6,AIO                                                           
         B     ALLDM                                                            
         SPACE 1                                                                
ALLDMB   LA    R5,KEYB                                                          
         L     R6,AIOB                                                          
         MVI   DMARK,1                                                          
         B     ALLDM                                                            
         SPACE 1                                                                
READTIA  NTR1                                                                   
         LA    R3,=C'DMREAD'                                                    
         B     WRITIA2                                                          
         SPACE 1                                                                
WRITIA   NTR1  BASE=ABASE                                                       
         L     R8,A2NDBASE                                                      
         LA    R3,=C'DMWRT'                                                     
         SPACE 1                                                                
WRITIA2  LA    R4,=C'TEMPSTR'                                                   
         LH    R5,TERMINAL                                                      
         ICM   R5,8,=X'01'                                                      
         L     R6,ATIA                                                          
         B     ALLDM2                                                           
*                                                                               
ALLDM    LA    R4,=C'ACCOUNT'                                                   
ALLDM2   STM   R3,R6,DMCB                                                       
         MVI   DMCB,X'08'          ALWAYS READ FOR DELETES                      
         GOTO1 VDATAMGR,DMCB                                                    
         TM    DMCB+8,X'02'        RECORD DELETED?                              
         BZ    ALLDM8                                                           
         CLI   DELETED,C'Y'        PASS BACK DELETED RECORDS?                   
         BE    DMXIT                                                            
         MVI   ERROR,INVALID                                                    
         CLC   0(6,R3),=C'DMREAD'  ERROR IF ACTION=READ AND REC                 
         BE    DMERR2              IS DELETED                                   
         CLC   0(6,R3),=C'DMRSEQ'  DO ANOTHER READ SEQ IF ACTION=SEQ            
         BE    ALLDM2              AND RECORD IS DELETED                        
         CLC   0(6,R3),=C'DMRDHI'                                               
         BNE   DMXIT                                                            
         LR    R5,R6                                                            
         USING ACKEYD,R5                                                        
         CLI   ACKEYACC,X'40'      IF A SPECIAL RECORD THEN JUST DO             
         BH    *+12                A SEQUENTIAL READ TO NEXT RECORD             
         LA    R3,=C'DMRSEQ'                                                    
         B     ALLDM2                                                           
*                                                                               
         CLC   ACKEYCON,SPACES     IS THIS A CONTRA ACCOUNT REC                 
         BE    ALLDM4                                                           
         ZIC   R1,ACKEYCON+14                                                   
         LA    R1,1(R1)                                                         
         STC   R1,ACKEYCON+14      BUMP TO NEXT SUBACCT REC                     
         B     ALLDM2                                                           
*                                                                               
ALLDM4   ZIC   R1,ACKEYACC+14      MUST BE AN ACCOUNT RECORD                    
         LA    R1,1(R1)                                                         
         STC   R1,ACKEYACC+14                                                   
         B     ALLDM2                                                           
         DROP  R5                                                               
*                                                                               
ALLDM8   TM    DMCB+8,X'FD'                                                     
         BZ    DMXIT                                                            
*                                                                               
DMERR    TM    DMCB+8,X'10'                                                     
         BO    *+8                                                              
         MVI   ERROR,0                                                          
DMERR2   SR    RE,RE                                                            
DMXIT    LTR   RE,RE                                                            
EXIT     XIT1                                                                   
         SPACE 3                                                                
         GETEL R9,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
USRLIMIT DS    0F                                                               
         DC    F'1000'                                                          
         SPACE 1                                                                
         DS    CL12                SPARE                                        
RECADDS  DS    0F                  RELOCATED OVERLAY PHASE ADDRESSES            
RECKEY   DS    A                    KEY TABLE                                   
RECFILT  DS    A                    FILTER TABLE                                
RECKNTRY DS    A                   OPTIONAL ENTRY POINT FOR KEY EDITING         
RECFNTRY DS    A                   DITTO                    FILTER              
RECDNTRY DS    A                   DITTO                    DATA                
RECANY1  DS    A                    SPARE                                       
RECANY2  DS    A                    SPARE                                       
RECADDX  DS    0F                                                               
         SPACE 3                                                                
ACOMMON  DS    0F                  COMMON EDIT S/R'S                            
         DC    V(ACSPLIT)                                                       
         DC    V(BINSRCH)                                                       
         DC    A(VDITYPE)                                                       
         DC    A(VDITACC)                                                       
         DC    A(VDITACNM)                                                      
         DC    A(VDITCASH)                                                      
         DC    A(VDITCONT)                                                      
         DC    A(VDITSTRT)                                                      
         DC    A(VDITEND)                                                       
         DC    A(VDITUNLE)                                                      
         DC    A(VDITOVER)                                                      
         DC    A(VDITOPT1)                                                      
         DC    A(SETELAD)                                                       
         DC    A(FILTER)                                                        
         DC    A(READB)                                                         
         DC    A(HIGHB)                                                         
         DC    A(WRITIA)                                                        
         DC    V(CONVMOS)                                                       
         DC    V(PRORATA)                                                       
NCOMMON  EQU   (*-ACOMMON)/L'ACOMMON                                            
         SPACE 3                                                                
* EXTENDED STORAGE ADCONS TABLE                                                 
*                                                                               
EXTTAB   DS    0D                                                               
         DC    AL4(OFFBLK-GWS),AL4(AOFFBLK-GWS)                                 
         DC    AL4(IO-GWS),AL4(AIO-GWS)                                         
         DC    AL4(IOB-GWS),AL4(AIOB-GWS)                                       
         DC    AL4(IOC-GWS),AL4(AIOC-GWS)                                       
         DC    AL4(LOCAL-GWS),AL4(ALOCAL-GWS)                                   
NEXTTAB  EQU   (*-EXTTAB)/L'EXTTAB                                              
         SPACE 3                                                                
* TABLE OF CORE-RESIDENT ROUTINE NUMBERS                                        
*                                                                               
CORETAB  DS    0X                                                               
         DC    AL1(QCENTER)                                                     
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QGETOPT)                                                     
         DC    AL1(QJOBBER)                                                     
         DC    AL1(QOFFAL)                                                      
         DC    AL1(QQSORT)                                                      
         DC    AL1(QSQUASH)                                                     
         DC    AL1(QTSAR)                                                       
NCORES   EQU   *-CORETAB                                                        
         SPACE 3                                                                
ELLIST   DS    0C                  LIST OF ELEMENT CODES - SEE AELTAB           
         DC    X'16'                                                            
         DC    X'20'                                                            
         DC    X'21'                                                            
         DC    X'23'                                                            
         DC    X'30'                                                            
         DC    X'32'                                                            
         DC    X'34'                                                            
         DC    X'43'                                                            
         DC    X'44'                                                            
         DC    X'45'                                                            
         DC    X'50'                                                            
         DC    X'60'                                                            
         DC    X'24'                                                            
         DC    X'4E'                                                            
         DC    X'14'                                                            
         DC    X'25'                                                            
         DC    X'67'                                                            
         DC    X'68'                                                            
         DC    X'61'                                                            
         DC    X'35'                                                            
         DC    X'00'               DO NOT USE - USED FOR DISC. 50 EL.           
         DC    X'26'               SPARE                                        
         DC    X'40'                                                            
         DC    X'00'               DO NOT USE - USED FOR TAX 50 EL.             
         DC    X'C0'               ANALYSIS/ATTRIBUTE POINTERS                  
         DC    X'1A'               MEDIA TRANSFER                               
         DC    X'51'               PROJETC CONTROL                              
         DC    X'7C'               UNIT/PRICE INFORMATION                       
         DC    X'FF'               END OF LIST                                  
         SPACE 3                                                                
RECLIST  DS    0CL18               LIST OF RECORD TYPES                         
*                                  COVERED BY DSECT RECTABD                     
         DC    C'BU',X'045E',CL14'OLD BUDGETS'                                  
         DC    C'CL',X'1016',CL14'CONTRA LIST'                                  
         DC    C'DE',X'0217',CL14'DETAIL'                                       
         DC    C'DS',X'01D4',CL14'DDS STATEMENT'                                
         DC    C'ES',X'0B21',CL14'JOB ESTIMATES'                                
*&&UK*&& DC    C'ET',X'1218',CL14'ESTIMATED TIME'                               
         DC    C'JB',X'1118',CL14'JOB BILLING'                                  
         DC    C'JD',X'0618',CL14'JOB DETAIL'                                   
         DC    C'JS',X'0854',CL14'JOB STATEMENT'                                
         DC    C'LI',X'0521',CL14'LIST'                                         
         DC    C'MO',X'0358',CL14'MONTHLY'                                      
         DC    C'NB',X'0F5E',CL14'NEW BUDGETS'                                  
         DC    C'OD',X'0C18',CL14'ORDER DETAIL'                                 
         DC    C'OL',X'0D01',CL14'ORDER LIST'                                   
         DC    C'OS',X'0E21',CL14'ORDER SUMMARY'                                
         DC    C'RE',X'0718',CL14'RECEIVABLE'                                   
         DC    C'ST',X'0916',CL14'STATEMENT'                                    
         DC    X'00'                                                            
         EJECT                                                                  
*              CLEAR AS MUCH OF THE TWA AS REQUIRED                             
*              ON ENTRY VIRGIN  = 0 IF NO DISPLAY                               
*                       LINE    = NEXT LINE TO BE USED (0=INFDATA)              
*                       LASTKMK = 0 IF NOT A CONTINUATION SCREEN                
*              ON EXIT  VIRGIN  = 0 IF NO DISPLAY NOW, ELSE 1                   
*                       LINE    = 0 IF LASTKMK WAS 0, ELSE UNCHANGED            
         SPACE 1                                                                
         DS    0D                                                               
CLEARTWA NMOD1 0,*CLRTWA*                                                       
         L     RC,0(R1)                                                         
         CLI   VIRGIN,0                                                         
         BE    CTXIT                                                            
         CLI   LASTKMK,0                                                        
         BNE   *+10                                                             
         XC    LINE,LINE                                                        
         MVI   VIRGIN,0                                                         
         OC    LINE,LINE                                                        
         BZ    *+8                                                              
         MVI   VIRGIN,1                                                         
         LH    R3,LINE             CLEAR FROM LINE N ONWARDS                    
         MH    R3,=H'86'                                                        
         LA    R3,INFDATAH(R3)                                                  
         ZIC   R4,0(R3)                                                         
         LA    R5,INFDATLH                                                      
         DROP  R6                                                               
         USING LINED,R3                                                         
CT1      OC    LINEDATA,LINEDATA                                                
         BZ    CT2                                                              
         XC    LINEDATA,LINEDATA                                                
         OI    LINEHDR+6,X'80'                                                  
CT2      BXLE  R3,R4,CT1                                                        
CTXIT    XIT1                                                                   
         DROP  R3                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              HELPREC - DISPLAYS DETAILS OF VALID RECORD TYPES WHEN            
*                        HELP IS REQUESTED OR AN ERROR OCCURS                   
         SPACE 1                                                                
         DS    0D                                                               
HLPREC   NMOD1 0,*HLPREC*                                                       
         L     RC,0(R1)                                                         
         XC    LINE,LINE                                                        
         GOTO1 =A(CLEARTWA),DMCB,(RC),RR=RELO                                   
*                                                                               
         CLI   ERROR,0             IS THIS AN ERROR OR A HELP REQUEST +         
         BNE   HR01                                                             
         GOTO1 VCALLOV,DMCB,(10,0),(0,T606TWA)                                  
         CLI   DMCB+4,X'FF'                                                     
         BE    HR01                                                             
*                                                                               
         L     R1,DMCB             MOVE 19 LINES OF EXPLANATORY TEXT            
         LA    R6,INFDATAH          FROM OVERLAY INTO TWA.                      
         USING LINED,R6                                                         
         LA    R4,19                                                            
HR0      OI    LINEHDR+6,X'80'                                                  
         MVC   LINEDATA+21(57),0(R1)                                            
         LA    R1,57(R1)                                                        
         LA    R6,LINELEN(R6)                                                   
         BCT   R4,HR0                                                           
*                                                                               
HR01     MVC   INFDATA(15),=CL15'TYPES AVAILABLE'                               
         OI    INFDATAH+6,X'80'                                                 
         LA    R6,INFDAT3H                                                      
         L     R4,=A(RECLIST)                                                   
         A     R4,RELO                                                          
         USING RECTABD,R4                                                       
         SPACE 1                                                                
HR1      CLI   0(R4),0                                                          
         BE    HR3                                                              
         TM    RECAUTH,DDSONLY     SKIP PRIVILEGED RECORD TYPES AT              
         BNO   *+12                 NON-DDS TERMINALS.                          
         CLI   DDS,C'Y'                                                         
         BNE   HR2                                                              
         MVC   LINEDATA(L'RECNAME),RECNAME                                      
         MVC   LINEDATA+15(4),=C'(  )'                                          
         MVC   LINEDATA+16(2),RECTYPE                                           
         OI    LINEHDR+6,X'80'                                                  
         LA    R6,LINELEN(R6)                                                   
HR2      LA    R4,RECTBLEN(R4)                                                  
         B     HR1                                                              
HR3      MVI   VIRGIN,1                                                         
         LTR   R4,R4               RETURN CC NE                                 
         XIT1                                                                   
         DROP  R4,R6                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              HELPKEY - DISPLAYS DETAILS OF VALID KEY EXPRESSIONS WHEN         
*                        HELP IS REQUESTED OR A KEY ERROR OCCURS                
         SPACE 1                                                                
         DS    0D                                                               
HLPKEY   NMOD1 0,*HLPKEY*                                                       
         L     RC,0(R1)                                                         
         XC    LINE,LINE                                                        
         GOTO1 =A(CLEARTWA),DMCB,(RC),RR=RELO                                   
         LA    R6,INFDAT5H                                                      
         USING LINED,R6                                                         
         SR    R3,R3               R3 = KEY NUMBER                              
         L     R4,RECKEY                                                        
         USING KEYTABD,R4                                                       
         SPACE 1                                                                
HK1      CLI   0(R4),0                                                          
         BE    HK3                                                              
         CLI   KEYMAND,C' '        NOT PROVIDED BY USER                         
         BE    HK2                                                              
         LA    R3,1(R3)                                                         
         OI    LINEHDR+6,X'80'                                                  
         EDIT  (R3),(1,39(R6))     KEY NUMBER                                   
         MVC   LINEDATA+34(L'KEYNAME),KEYNAME                                   
         ZIC   R5,KEYLEN                                                        
         EDIT  (R5),(2,53(R6))     MAX LENGTH                                   
         CLI   KEYMAND,C'O'                                                     
         BNE   *+12                                                             
         MVI   LINEDATA+33,C'('                                                 
         MVI   LINEDATA+47,C')'                                                 
         LA    R6,LINELEN(R6)                                                   
         SPACE 1                                                                
HK2      LA    R4,KEYTBLEN(R4)                                                  
         B     HK1                                                              
         SPACE 2                                                                
HK3      LTR   R3,R3               ANY KEYS ?                                   
         BZ    HK5                                                              
         MVC   INFDATA+31(10),=CL10'VALID KEYS'                                 
         OI    INFDAT3H+6,X'80'                                                 
         MVC   INFDAT3+33(16),=C'DESCRIPTION LGTH'                              
         MVC   LINEDATA+31(22),=C'OR, IN CONTEXT, ''NEXT'''                     
         OI    LINEHDR+6,X'80'                                                  
         B     HK6                                                              
HK5      MVC   INFDATA+31(7),=CL7'NO KEYS'                                      
HK6      OI    INFDATAH+6,X'80'                                                 
         MVC   INFDAT2+31(13),=CL13'FOR THIS TYPE'                              
         OI    INFDAT2H+6,X'80'                                                 
         MVI   VIRGIN,1                                                         
         CLI   HELPKMK,1           RETURN CC                                    
         XIT1                                                                   
         DROP  R4,R6                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              HELPFILT -DISPLAYS DETAILS OF VALID OPTION EXPRESSIONS           
*                        WHEN HELP IS REQUESTED OR AN ERROR OCCURS              
         SPACE 1                                                                
         DS    0D                                                               
HLPFLT   NMOD1 0,*HLPFLT*                                                       
         L     RC,0(R1)                                                         
         TM    HELPKMK,X'01'                                                    
         BO    HF00                                                             
         XC    LINE,LINE                                                        
         GOTO1 =A(CLEARTWA),DMCB,(RC),RR=RELO                                   
HF00     LA    R6,INFDAT5H                                                      
         USING LINED,R6                                                         
         SR    R3,R3               R3 = ANY OPTIONS ?                           
         L     R4,RECFILT                                                       
         USING FILTERSD,R4                                                      
         LA    R9,15                                                            
         SPACE 1                                                                
         CLC   INFFILT+4(4),=C'NEXT' CONTINUATION                               
         BNE   HF1                                                              
         LA    R1,14                                                            
         CLI   0(R4),0                                                          
         BE    HF0                                                              
         LA    R4,FILTBLEN(R4)                                                  
         BCT   R1,*-12                                                          
         CLI   0(R4),0                                                          
         BNE   HF1                                                              
HF0      MVC   INFDATA(15),=C'NO MORE OPTIONS'                                  
         B     HF6                                                              
         SPACE 1                                                                
HF1      CLI   0(R4),0                                                          
         BE    HF4                                                              
         TM    FILSTAT,DDSONLY                                                  
         BNO   *+12                                                             
         CLI   DDS,C'Y'                                                         
         BNE   HF3A                                                             
         LA    R3,1                                                             
         OI    LINEHDR+6,X'80'                                                  
         MVC   LINEDATA(L'FILFULKW),FILFULKW                                    
         LA    R5,L'FILFULKW-1                                                  
         LA    R7,FILFULKW(R5)                                                  
         CLI   0(R7),C' '                                                       
         BNE   *+8                 R5 = KEYWORD LENGTH MINUS 1                  
         BCT   R5,*-12                                                          
         LA    R5,LINEDATA+1(R5)                                                
         MVC   0(5,R5),=C'(  )='                                                
         MVC   1(2,R5),FILSHTKW    SHORT KEYWORD                                
         CLI   FILNOTNL,C' '       NOTIONAL VALUE                               
         BE    HF2                                                              
         MVC   5(L'FILNOTNL,R5),FILNOTNL                                        
         B     HF3                                                              
HF2      MVC   5(15,R5),=C'(   CHARACTERS)'                                     
         ZIC   R7,FILENGTH         LENGTH OF REAL VALUE IN PARENTHESES          
         EDIT  (R7),(2,6(R5))                                                   
         C     R7,=F'1'                                                         
         BNE   *+8                                                              
         MVI   18(R5),C' '                                                      
         SPACE 1                                                                
HF3      LA    R6,LINELEN(R6)                                                   
HF3A     LA    R4,FILTBLEN(R4)                                                  
         BCT   R9,HF1                                                           
         CLI   0(R4),0                                                          
         BE    HF4                                                              
         MVC   INFDATL(30),=C'ENTER ''HELPNEXT'' FOR REMAINDER'                 
         CLC   INFFILT(4),=C'HELP'                                              
         BNE   HF4                                                              
         MVC   INFFILT+4(4),=C'NEXT'                                            
         OI    INFFILTH+6,X'80'                                                 
         OI    INFKEYH+6,X'81'                                                  
         MVI   HELPKMK,0                                                        
         LA    R2,INFTABH          PRESET FOR HIT ENTER FOR HELPNEXT            
         SPACE 1                                                                
HF4      LTR   R3,R3               ANY OPTIONS ?                                
         BZ    HF5                                                              
         MVC   INFDATA(13),=C'VALID OPTIONS'                                    
         B     *+10                                                             
HF5      MVC   INFDATA(10),=C'NO OPTIONS'                                       
HF6      MVC   INFDAT2(13),=CL13'FOR THIS TYPE'                                 
         OI    INFDATAH+6,X'80'                                                 
         OI    INFDAT2H+6,X'80'                                                 
         MVI   VIRGIN,1                                                         
         LTR   R4,R4               RETURN CC NE                                 
         XIT1                                                                   
         DROP  R4,R6                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              NESTED INCLUDES FOR DDCOMFACS                                    
*                                  DDCOREQUS                                    
*                                  FAFACTS                                      
*                                  FATIOB                                       
*                                  FATWA                                        
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
       ++INCLUDE ACINQDSECT                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACINQ00   05/01/02'                                      
         END                                                                    
