*          DATA SET ACINF00    AT LEVEL 054 AS OF 09/30/04                      
*PHASE T60500A                                                                  
*INCLUDE ACSPLIT                                                                
         TITLE 'ACCOUNTING INFO PROGRAM (AIS)'                                  
T60500   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (GWSX-GWS),**INFO**,R8,RR=R5,CLEAR=YES                           
         USING GWS,RC              RC = GLOBAL W/S                              
         L     RA,4(R1)                                                         
         ST    RA,ATWA             SAVE RA                                      
         USING T605TWA,RA          RA = TWA                                     
         ST    R5,RELO                                                          
         MVC   MYCO,0(R1)                                                       
         MVC   ATIA,12(R1)         SAVE A(TIA) FOR TWA1 STORAGE                 
         ST    RB,ABASE                                                         
         ST    R8,A2NDBASE                                                      
         ST    RD,AREGSAVE                                                      
*                                                                               
         L     RE,16(R1)           SAVE COMFACS ADDRESSES                       
         ST    RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   VDATAMGR(12),CDATAMGR                                            
         MVC   VHELLO(20),CHELLO                                                
         MVC   VDATVAL(8),CDATVAL                                               
         MVC   VGETFACT,CGETFACT                                                
         DROP  RE                                                               
*                                                                               
         LR    RE,RA               EXTRACT TWA VALUES                           
         USING TWAD,RE                                                          
         MVC   TERMACCS,TWAACCS                                                 
         MVC   TERMAGY,TWAAGY                                                   
         MVC   TERMAUT,TWAAUTH                                                  
         DROP  RE                                                               
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'C1000A0D',0                                     
         MVC   VSQASHER,0(R1)                                                   
         GOTO1 VCALLOV,DMCB,0,X'C1000A62',0                                     
         MVC   VOFFAL,0(R1)                                                     
         GOTO1 VCALLOV,DMCB,0,X'C1000A02',0                                     
         MVC   VCHOPPER,0(R1)                                                   
*                                                                               
         LA    R2,ACOMMON          RELOCATE COMMON ADDRESSES                    
         LA    R3,COMMON                                                        
         LA    R4,COMMONX-COMMON                                                
         SRL   R4,2                                                             
T60      L     R5,0(R2)                                                         
         A     R5,RELO                                                          
         ST    R5,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,T60                                                           
*                                                                               
         LA    RE,SAVEUNIT                                                      
         ST    RE,AUNITNAM                                                      
         LA    RE,SAVELEDG                                                      
         ST    RE,ALEDGNAM                                                      
         LA    RE,LEVEL                                                         
         ST    RE,ALEVEL                                                        
         LA    RE,SAVEHIER                                                      
         ST    RE,ASAVEHIE                                                      
         LA    RE,COMFILT                                                       
         ST    RE,ACOMFILT                                                      
         LH    RE,=Y(OFFBLK-GWS)                                                
         LA    RE,GWS(RE)                                                       
         ST    RE,AOFFBLK                                                       
         LH    RE,=Y(IO-GWS)                                                    
         LA    RE,GWS(RE)                                                       
         ST    RE,ADRIO                                                         
         LH    RE,=Y(IOB-GWS)                                                   
         LA    RE,GWS(RE)                                                       
         ST    RE,ADRIOB                                                        
         MVC   LIMIT,USRLIMIT                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   SAVENDKY,SPACES                                                  
         MVC   KEY,SPACES                                                       
         MVC   INFHEAD,SPACES                                                   
         OI    INFHEADH+6,X'80'                                                 
         SPACE 1                                                                
         L     R3,ATIA             GET TWA1                                     
         MVI   DMCB+8,1                                                         
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),2(RA)                                                 
         GOTO1 VDATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',,(R3)                       
         EJECT                                                                  
*              CHECK RECORD TYPE AND LOAD OVERLAY                               
         SPACE 1                                                                
T62      MVC   INFHEAD(23),=C'AIS UNAVAILABLE-USE AFM'                          
         B     INFEXIT         **AS OF OCT01/04                                 
*                                                                               
         LA    R2,INFRECH                                                       
         CLC   8(2,R2),=C'HE'                                                   
         BE    HELPREC                                                          
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    INFERR                                                           
         MVI   ERROR,NOTVLREC                                                   
         L     R3,ARECLIST                                                      
         USING RECTABD,R3                                                       
         SPACE 1                                                                
T63      CLI   0(R3),0                                                          
         BE    INFERR                                                           
         CLC   RECTYPE,INFREC                                                   
         BNE   T64                                                              
         TM    RECAUTH,X'F0'       DDS-ONLY RECORD TYPE                         
         BNO   T65                                                              
         CLI   T605TWA+1,C'*'                                                   
         BE    T65                                                              
         SPACE 1                                                                
T64      LA    R3,RECTBLEN(R3)                                                  
         B     T63                                                              
         SPACE 1                                                                
T65      MVC   AUTHTYPE,RECAUTH                                                 
         IC    R2,RECOLAY          IF OVERLAY DIFFERS FROM LAST,                
         CLC   RECOLAY,PHASE       UNSET LASTKMK                                
         BE    *+12                                                             
         MVI   LASTKMK,0                                                        
         STC   R2,PHASE                                                         
         GOTO1 VCALLOV,DMCB,((R2),0),(0,T605TWA)                                
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R9,DMCB                                                          
         ST    R9,APHASE                                                        
         LA    R1,RECADDS          STORE RELOCATED OVERLAY ADDRESSES            
         LA    R2,4                IN RECADDS                                   
         LA    R3,RECADDX-4                                                     
         LR    R4,R9                                                            
         SPACE 1                                                                
T651     L     R5,0(R4)                                                         
         AR    R5,R9                                                            
         ST    R5,0(R1)                                                         
         AR    R4,R2                                                            
         BXLE  R1,R2,T651                                                       
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
*              RESET COMPANY CODE IN MYCO IF THIS IS A DDS TERMINAL AND         
*              A ONE CHARACTER FIELD HAS BEEN INPUT AFTER RECORD TYPE           
*                                                                               
         LA    R2,INFRECH                                                       
         MVC   TERMAUTH,T605TWA+13                                              
         CLI   T605TWA+1,C'*'      DDS TERMINAL                                 
         BNE   T66                                                              
         CLI   INFRECH+5,2                                                      
         BE    T66                NO INPUT AFTER RECORD TYPE                    
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
         B     *+10                                                             
T652     MVC   MYCO,INFREC+3                                                    
*&&UK                                                                           
         CLI   MYCO,X'E7'          CANNOT SWITCH TO DDS LTD                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         SPACE 1                                                                
T66      CLC   MYCO,PRODCO         READ COMPANY RECORD IF DIFFERENT             
         BE    T665                FROM LAST AND SAVE PRODUCTION UNIT           
         MVI   LASTKMK,0           AND LEDGER                                   
         MVC   KEYB,SPACES                                                      
         MVC   KEYB(1),MYCO                                                     
         MVI   ERROR,NOTVLCDE                                                   
         BAS   RE,READ                                                          
         BZ    INFERR                                                           
         L     R4,ADRIOB                                                        
         MVI   ELCODE,ACMPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   INFERR                                                           
         USING ACCOMPD,R4                                                       
         L     R5,ATIA                                                          
         USING TWA1D,R5                                                         
         MVC   COMPSTA3,ACMPSTA3                                                
         MVC   SPROUNIT(2),ACMPJOB                                              
         MVC   SAVCSTA1,ACMPSTAT                                                
         MVC   SAVCSTA2,ACMPSTA2                                                
         MVC   SAVCSTA3,ACMPSTA3                                                
         MVC   SAVCSTA4,ACMPSTA4                                                
         DROP  R4,R5                                                            
*                                                                               
T665     L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         L     R5,ATIA                                                          
         USING TWA1D,R5                                                         
         MVC   OFFACOMF,ACOMFACS                                                
         MVC   OFFAALPH,TERMAGY                                                 
         MVC   OFFACPY,MYCO                                                     
         MVC   OFFACST1(OFFAOPOS-OFFACST1),SAVCSTA1                             
         MVC   OFFALIMA,TERMACCS                                                
         MVC   OFFAAUTH,TERMAUT    AUTHORIZATION VALUES                         
         MVI   OFFAINDS,OFFAIOFF                                                
         MVI   OFFAACT,OFFAINI     INITIALIZE                                   
         CLC   MYCO,PRODCO         TEST IF FIRST TIME FOR COMPANY               
         BNE   *+14                YES                                          
         MVI   OFFAACT,OFFARES     NO-RESTORE                                   
         MVC   OFFASAV(OFFASAVL),SAVEOFFA                                       
         GOTO1 VOFFAL                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEOFFA,OFFASAV                                                 
         MVC   PRODCO,MYCO                                                      
         B     T67                                                              
         DROP  R1,R5                                                            
         EJECT                                                                  
*              CHECK INPUT KEYS AND SET UP KEY                                  
         SPACE 1                                                                
T67      MVI   FNDX,0                                                           
         MVC   PRODUNIT(2),SPROUNIT                                             
         LA    R2,INFKEYH          R2 USED BY ERROR ROUTINES                    
         CLC   INFKEY(4),=C'HELP'  HELP                                         
         BNE   *+12                                                             
         MVI   HELPKMK,1                                                        
         B     HELPKEY                                                          
         SPACE 1                                                                
         MVI   SCANBLCK+20,X'FF'                                                
         MVC   SCANBLCK+21(19),SCANBLCK+20                                      
         MVC   SCANBLCK(20),SPACES PERFORM SCANNER FUNCTION ON START &          
         OC    INFKEY,SPACES       END KEYS, USING SCANBLCK(20) & +20           
         LA    R1,L'INFKEY                                                      
         SR    R3,R3               R3 = NO OF SIGNIFICANT CHARS AFTER           
         MVI   WORK,0                   COMMA                                   
T671     LA    R4,INFKEY-1(R1)                                                  
         CLI   WORK,0                                                           
         BNE   *+12                                                             
         CLI   0(R4),C' '                                                       
         BNH   T672                                                             
         CLI   0(R4),C','                                                       
         BE    T673                                                             
         MVI   WORK,1              SPACES ARE NOW SIGNIFICANT                   
         LA    R3,1(R3)                                                         
T672     BCT   R1,T671                                                          
         SR    R3,R3                                                            
         LA    R1,L'INFKEY+1                                                    
T673     STC   R3,SCANBLCK+40      SCANBLCK+40 = SIGNIFICANT LENGTH OF          
         SH    R1,=H'2'                          INPUT END KEY                  
         BM    T674                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    SCANBLCK(0),INFKEY                                               
T674     BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BM    T675                                                             
         MVI   FNDX,1                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SCANBLCK+20(0),1(R4)                                             
         MVC   SAVENDKY,1(R4)                                                   
         SPACE 1                                                                
T675     L     R4,RECKEY                                                        
         USING KEYTABD,R4                                                       
         CLC   INFKEY(4),=C'NEXT'                                               
         BNE   T68-4                                                            
         MVI   ERROR,INVALID                                                    
         CLI   LASTKMK,0                                                        
         BE    INFERR                                                           
*                                                                               
* CHECK IF FILTERS HAVE CHANGED WHILE SCROLLING THROUGH RECORDS                 
*                                                                               
         LA    R1,INFFILTH         FILTER HEADER                                
         TM    4(R1),X'80'         INPUT THIS TIME?                             
         BZ    T67A                                                             
         MVC   KEY,FIRSTKEY        RESET KEY TO BEGINNING                       
         MVI   LASTKMK,0                                                        
         B     T6E                                                              
*                                                                               
T67A     MVC   KEY,LASTKEY                                                      
         B     T6E                                                              
         MVI   LASTKMK,0                                                        
         SPACE 1                                                                
T68      CLI   0(R4),0                                                          
         BNE   *+14                                                             
         MVC   FIRSTKEY,KEY        SAVE THE FIRST KEY FOUND                     
*        MVC   SVSCRKEY,INFKEY     SAVE WHAT'S ON THE SCREEN                    
         B     T6E                                                              
         ZIC   R5,KEYDISP                                                       
         LA    R5,KEY(R5)          R5 = START POSITION IN KEY                   
         MVC   HALF,KEYPOINT                                                    
         LH    R6,HALF                                                          
         LTR   R6,R6                                                            
         BNZ   *+14                                                             
         MVC   0(1,R5),KEYDFLT                                                  
         B     T6D                                                              
         AR    R6,RC               R6 = LOCATION OF KEY COMPONENT VALUE         
         SPACE 1                                                                
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
         CLI   KEYMAND,C'O'                                                     
         BE    KSXIT                                                            
         CLI   KEYNAME,C'U'                                                     
         BNE   KS4                                                              
         MVI   ERROR,UNITNVAL                                                   
         LA    R7,SAVEUNIT                                                      
         BAS   RE,GETUNLE                                                       
         B     KSXIT1                                                           
KS4      CLI   KEYNAME,C'L'                                                     
         BNE   KSXIT1                                                           
         MVI   ERROR,LEDGNVAL                                                   
         LA    R7,SAVELEDG                                                      
         BAS   RE,GETUNLE                                                       
         B     KSXIT1                                                           
         SPACE 1                                                                
KSERROR  SR    RB,RB               CC=EQU IF ERROR                              
KSXIT    LTR   RB,RB                                                            
KSXIT1   XIT1                                                                   
         SPACE 1                                                                
KSMOVE   MVC   0(0,R5),0(R2)                                                    
         SPACE 1                                                                
         EJECT                                                                  
*              CHECK UNIT OR LEDGER RECORD AND SAVE NAME AND HIERARCHY          
*              ELEMENT                                                          
*              ON ENTRY R7 = A(SAVE AREA IN TWA FOR NAME)                       
*                       KEY= KEY SO FAR BUILT                                   
*              ON EXIT  CC = EQU IF ERROR                                       
         SPACE 1                                                                
GETUNLE  NTR1  BASE=ABASE                                                       
         L     R8,A2NDBASE                                                      
         MVC   KEYB,SPACES                                                      
         MVC   KEYB(3),KEY                                                      
         CLI   KEY,X'0A'           ANALYSIS RECORD TYPE                         
         BNE   *+10                                                             
         MVC   KEYB(3),KEY+1                                                    
         BAS   RE,READ                                                          
         BZ    GETUNLEX                                                         
         GOTO1 =A(SETELAD),DMCB,(RC),ADRIOB,RR=RELO                             
         CLC   TERMAUTH,SECURITY                                                
         BNL   GU1                                                              
         MVI   ERROR,0             ERROR IF ACCESS NOT AUTHORISED               
         OI    DMCB+8,X'04'                                                     
         SR    R1,R1                                                            
         B     GETUNLEX                                                         
GU1      MVC   0(36,R7),SPACES                                                  
         ICM   R3,15,ANAM                                                       
         BZ    GU3                                                              
         ZIC   R5,1(R3)                                                         
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),2(R3)                                                    
GU3      ICM   R3,15,AHIE                                                       
         BZ    GU4                                                              
         MVC   SAVEHIER(L'SAVEHIER),0(R3)                                       
GU4      ICM   R3,15,ALED          SAVE LEDGER STATUS BYTE                      
         BZ    GU5                                                              
         USING ACLEDGD,R3                                                       
         MVC   LEDGSTAT,ACLTSTAT                                                
         L     R5,ATIA                                                          
         MVC   SOFFPOS-TWA1D(1,R5),ACLTOFF                                      
GU5      LTR   RB,RB                                                            
GETUNLEX XIT1                                                                   
         EJECT                                                                  
*              CLEAR TWA UNLESS IT IS ALREADY CLEAR OR WE ARE BUILDING          
*              UP A SCREEN INCREMENTALLY                                        
         SPACE 1                                                                
T6E      OC    LINE,LINE                                                        
         BZ    *+12                                                             
         CLI   LASTKMK,1                                                        
         BE    T6E0                                                             
         BAS   RE,CLEARTWA                                                      
         B     T6E0                                                             
         SPACE 3                                                                
CLEARTWA NTR1                                                                   
*        CLI   VIRGIN,0                                                         
*        BE    CTXIT                                                            
         MVI   VIRGIN,0                                                         
         XC    LINE(5),LINE                                                     
         LA    R3,INFDATAH                                                      
         ZIC   R4,0(R3)                                                         
         LA    R5,INFDATLH                                                      
         USING LINED,R3                                                         
CT1      OC    LINEDATA,LINEDATA                                                
         BZ    CT2                                                              
         XC    LINEDATA,LINEDATA                                                
         OI    LINEHDR+6,X'80'                                                  
CT2      BXLE  R3,R4,CT1                                                        
CTXIT    XIT1                                                                   
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
*              SET UP ENDKEY FROM SCANBLCK+20(20) OR SET DEFAULT ENDKEY         
*              (INPUT LENGTH IS IN SCANBLCK+40)                                 
         SPACE 1                                                                
T6E0     MVI   ENDKEY,X'FF'        DEFAULT ENDKEY IS X'FF'S                     
         MVC   ENDKEY+1(L'ENDKEY-1),ENDKEY                                      
         CLI   SCANBLCK+40,0                                                    
         BE    T6EE                                                             
         MVI   FNDX,2                                                           
         L     R4,RECKEY                                                        
         SPACE 1                                                                
T6E1     CLI   0(R4),0             FIND FIRST VARIABLE KEY                      
         BE    T6E4                                                             
         CLI   KEYVARY,C'V'                                                     
         BE    *+12                                                             
         LA    R4,KEYTBLEN(R4)                                                  
         B     T6E1                                                             
         ZIC   R5,KEYDISP                                                       
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BM    T6E2                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ENDKEY(0),KEY       ENDKEY = KEY UP TO THIS POINT                
T6E2     LA    R5,ENDKEY+1(R5)                                                  
         LA    R6,SCANBLCK+20                                                   
         BAS   RE,KEYSET                                                        
         BZ    INFERR                                                           
         ZIC   R7,KEYLEN                                                        
         AR    R6,R7                                                            
         CLI   0(R6),X'FF'         ANY MORE ENDKEY                              
         BE    T6E3                                                             
         AR    R5,R7                                                            
         MVC   0(3,R5),0(R6)       IF SO MOVE IN A FEW CHARACTERS               
T6E3     DS    0H                                                               
         SPACE 1                                                                
T6E4     MVI   FNDX,0              START > END ?                                
         MVI   ERROR,STAGTRND                                                   
         CLC   KEY,ENDKEY                                                       
         BH    INFERR                                                           
         B     T6EE                                                             
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
*              CHECK INPUT FILTERS AND SET UP FILTER TABLE IN GLOBAL WS         
*                                                                               
T6EE     LA    R2,INFFILTH         R2 USED BY ERROR ROUTINES                    
         LA    R6,FILTAB                                                        
         USING FTBD,R6                                                          
         CLI   5(R2),0                                                          
         BE    T6M                 NO FILTERS                                   
         CLC   INFFILT(4),=C'HELP'                                              
         BE    HELPFILT                                                         
         CLI   HELPKMK,1                                                        
         BE    INFEXIT                                                          
         XC    SCANBLCK(253),SCANBLCK                                           
         GOTO1 VSCANNER,DMCB,(14,INFFILTH),(7,SCANBLCK)                         
         CLI   DMCB+4,0                                                         
         MVI   ERROR,INVALID                                                    
         BE    INFERR                                                           
         LA    R3,SCANBLCK                                                      
         SPACE 1                                                                
T6F      CLI   0(R3),0                                                          
         BE    T6M                                                              
         MVI   WORK,0              WORK=0 INDICATES SHORT KEYWORD MATCH         
         CLI   0(R3),2                                                          
         BNE   T6H                                                              
         L     R4,RECFILT                                                       
         USING FILTERSD,R4                                                      
T6G      CLI   FILFULKW,X'00'                                                   
         BE    T6H                                                              
         CLC   FILSHTKW,12(R3)                                                  
         BE    T6J                                                              
T6G1     LA    R4,FILTBLEN(R4)                                                  
         B     T6G                                                              
T6H      L     R4,RECFILT          LOOK FOR FULL KEYWORD MATCH                  
         MVI   WORK,1                                                           
         ZIC   R5,0(R3)                                                         
         BCTR  R5,0                                                             
T6I      CLI   FILFULKW,X'00'                                                   
         BE    INFERR                                                           
         EX    R5,OVERCOMP         LOOK FOR OVERRIDE FILTER - DDS               
         BNE   T6I1                PRIVILEGE TO RAISE RECORD NO. LIMIT          
         BAS   RE,OVERRIDE                                                      
         BZ    INFERR                                                           
         B     T6L2                                                             
T6I1     EX    R5,KEYWCOMP                                                      
         BE    T6J                                                              
         LA    R4,FILTBLEN(R4)                                                  
         B     T6I                                                              
OVERCOMP CLC   12(0,R3),=C'OVERRIDE'                                            
KEYWCOMP CLC   FILFULKW(0),12(R3)                                               
         SPACE 2                                                                
T6J      CLC   FILSHTKW,=C'DE'     COME HERE WITH A MATCHING KEYWORD            
         BNE   *+8                                                              
         MVI   DELETES,1           IF DELETED FILTER SET DELETES MARKER         
         ZIC   R7,1(R3)            TO SUPPRESS SKIPPING OF DELETED RECS         
         BCTR  R7,0                                                             
         LTR   R7,R7               R7 = L OF FILTER VALUE MINUS 1               
         BM    INFERR                                                           
         MVC   WORK+1(1),1(R3)     WORK+1 = L OF FILTER VALUE                   
         LA    R1,22(R3)           R1     = POINTER TO FILTER VALUE             
         MVI   FTBSIGN,C'P'        POSITIVE FILTER                              
         CLI   0(R1),C'*'                                                       
         BNE   T6J1                                                             
         STC   R7,WORK+1           ADJUST R1,R7 & WORK+1 IF NEGATIVE            
         BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         BM    INFERR                                                           
         LA    R1,1(R1)                                                         
         MVI   FTBSIGN,C'N'                                                     
         SPACE 1                                                                
T6J1     CLI   FILNOTNL,C' '       FILTER HAS NOTIONAL VALUE (EG YES)           
         BE    T6K                                                              
         LA    R9,L'FILNOTNL-1                                                  
         CR    R7,R9                                                            
         BNH   *+6                                                              
         LR    R7,R9                                                            
         EX    R7,NOTLCOMP         DOES THE VALUE MATCH                         
         BE    T6J3                                                             
T6J2     LA    R4,FILTBLEN(R4)     IF NOT TRY ANOTHER TAB ENTRY                 
         TM    WORK,1                                                           
         BO    T6I                 FULL KEYWORD                                 
         B     T6G                 SHRT KEYWORD                                 
T6J3     OC    FILELMNT,FILELMNT   IF ITS AN OPTION NOT A FILTER                
         BNZ   T6J4                                                             
         CLI   FTBSIGN,C'P'        AND POSITIVE                                 
         BNE   T6L2                                                             
         OC    OPTIONS,FILVALUE    SET THE APPROPRIATE OPTIONS BIT              
         B     T6L2                                                             
T6J4     MVI   FTBMARK,C'M'        IF YES SET MASK MARKER                       
         MVC   FTBVAL(1),FILVALUE  MASK VALUE                                   
         MVI   FTBLEN,1            LENGTH                                       
         B     T6K0                                                             
NOTLCOMP CLC   FILNOTNL(0),0(R1)                                                
         SPACE 1                                                                
T6K      CLC   FILENGTH,WORK+1     FILTER DOESNT HAVE NOTIONAL VALUE            
         BL    T6J2                VALUE TOO LONG - WRONG TAB ENTRY             
         OC    FILELMNT,FILELMNT   IS FILTER HANDLED ENTIRELY BY                
         BNZ   T6K00               OVERLAY - IF SO PASS CONTROL TO IT           
         L     RF,RECFNTRY                                                      
         ST    R1,DMCB+4           P2 = A(FILTER VALUE)                         
         GOTO1 (RF),DMCB,(RC)                                                   
         TM    DMCB,1                                                           
         BO    INFERR                                                           
         B     T6L2                AND SKIP TO NEXT                             
T6K00    MVI   FTBMARK,C'C'        SET COMPARE MARKER (NOT TM )                 
         EX    R7,VALMOVE          PUT REAL VALUE AND LENGTH IN TABLE           
         MVC   FTBLEN,WORK+1                                                    
         CLC   FILFULKW(3),=C'SEC' SECOVER/SECUPTO (SECURITY FILTERS)           
         BNE   T6K0                                                             
         BAS   RE,SECFILT                                                       
         BZ    INFERR                                                           
         SPACE 1                                                                
T6K0     XC    FTBSR,FTBSR         AND A(CONVERSION S/R) IF ANY                 
         LH    R5,FILEDIT                                                       
         LTR   R5,R5                                                            
         BZ    T6L                                                              
         CLI   FILEDIT,X'FF'       S/R IS IN OVERLAY                            
         BNE   T6K1                                                             
         MVC   FTBSR,RECFNTRY                                                   
         B     T6L                                                              
T6K1     AR    R5,RC               S/R IS IN ROOT                               
         MVC   FTBSR,0(R5)                                                      
         B     T6L                                                              
VALMOVE  MVC   FTBVAL(0),0(R1)                                                  
         SPACE 1                                                                
T6L      MVC   HALF,FILELMNT                                                    
         LH    R5,HALF                                                          
         AR    R5,RC                                                            
         ST    R5,FTBELMNT         A(A(ELEMENT CONTAINING FILTER))              
         MVC   FTBDISP,FILDISP     DISPLACEMENT INTO ELEMENT                    
         SPACE 1                                                                
         CLC   FILFULKW(5),=C'LEVEL'    IF FILTER IS 'LEVEL'                    
         BNE   T6L1                                                             
         CLI   FTBSIGN,C'P'        AND POSITIVE                                 
         BNE   T6L1                                                             
         MVI   LEVFILT,1           SET UP LEVFILT TO ALLOW EFFICIENT            
         CLI   FTBVAL,C'1'         FILTERING OF LEVEL 1                         
         BE    T6L1                                                             
         MVI   LEVFILT,2                                                        
         CLI   FTBVAL,C'2'                      LEVEL2                          
         BE    T6L1                                                             
         CLI   FTBVAL,X'04'                     LEVEL 1-2                       
         BE    T6L1                                                             
         MVI   LEVFILT,0                                                        
         SPACE 2                                                                
T6L1     LA    R6,FTBTBLEN(R6)     BUMP TO NEXT FILTER INPUT                    
T6L2     LA    R3,L'SCANBLCK+4(R3)                                              
         ZIC   RF,FNDX             INCREMENT SUBFIELD NUMBER                    
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LA    RF,1                                                             
         LA    RF,1(RF)                                                         
         STC   RF,FNDX                                                          
         B     T6F                                                              
         SPACE 1                                                                
T6M      TM    AUTHTYPE,X'02'      DOES RECORD TYPE REQUIRE JOB FILTER          
         BNO   T6M1                IF SO BUILD A FILTAB ENTRY - LEVEL=3         
         XC    FTBELMNT(FTBTBLEN),FTBELMNT                                      
         LA    RF,ALEVEL                                                        
         ST    RF,FTBELMNT                                                      
         MVC   FTBDISP+1(4),=X'0201C3F3'                                        
         MVI   FTBSIGN,C'P'                                                     
         LA    R6,FTBTBLEN(R6)                                                  
T6M1     MVI   FTBELMNT,X'FF'      PUT TERMINATOR ON GENERATED TABLE            
         CLI   HELPKMK,1                                                        
         BE    INFEXIT                                                          
         MVI   FNDX,0                                                           
         B     T6N0                                                             
         SPACE 3                                                                
*                                                                               
SECFILT  NTR1                      SECURITY NUMBER FILTER                       
         TM    3(R3),X'80'         NUMERIC                                      
         BNO   OVERROR                                                          
         CLC   8(4,R3),=F'255'     0-255                                        
         BH    OVERROR                                                          
         MVC   FTBVAL(1),11(R3)    PUT BINARY VALUE IN FILTAB ENTRY             
         MVI   FTBLEN,1                                                         
         CLI   FILSHTKW+1,C'U'                                                  
         BNE   OVERXIT                                                          
         XI    FTBSIGN,2           SWITCH SIGN IF SECUPTO - P/N AND N/P         
         B     OVERXIT             SO P MEANS GREATER THAN                      
         SPACE 3                                                                
*                                                                               
OVERRIDE NTR1                      RESET RECORD LIMIT                           
         CLI   T605TWA+1,C'*'      MUST BE DDS                                  
         BNE   OVERROR                                                          
         CLI   1(R3),3                                                          
         BH    OVERROR                                                          
         TM    3(R3),X'80'         NUMERIC (UP TO 999)                          
         BNO   OVERROR                                                          
         MVC   LIMIT,8(R3)                                                      
         LTR   R3,R3                                                            
         B     OVERXIT                                                          
OVERROR  SR    R3,R3                                                            
OVERXIT  XIT1                                                                   
         SPACE 1                                                                
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              SET UP KEYMASK  AS A KEY MASK WITH ZEROS IN VARIABLE KEY         
*              COMPONENTS AND ONES OTHERWISE.                                   
*                                                                               
*              SET UP KEYCHK AS A COPY OF KEY WITH VARIABLE COMPONENTS          
*              ZEROISED.                                                        
         SPACE 1                                                                
T6N0     MVI   KEYMASK,X'FF'                                                    
         MVC   KEYMASK+1(L'KEYMASK-1),KEYMASK                                   
         L     R3,RECKEY                                                        
         USING KEYTABD,R3                                                       
         SPACE 1                                                                
T6N      CLI   0(R3),0                                                          
         BE    T6P                                                              
         CLI   KEYVARY,C'V'                                                     
         BNE   T6O                                                              
         CLI   LASTKMK,0           IF NO PREVIOUS RECORDS, TREAT                
         BNE   T6N1                 MANDATORY KEYS AS NON-VARIABLE              
         CLC   RECOUNT,=F'0'                                                    
         BNE   T6N1                                                             
         CLI   KEYMAND,C'M'                                                     
         BE    T6O                                                              
T6N1     SR    R4,R4                                                            
         IC    R4,KEYDISP                                                       
         LA    R4,KEYMASK(R4)                                                   
         SR    R5,R5                                                            
         IC    R5,KEYLEN                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)                                                    
T6O      LA    R3,KEYTBLEN(R3)                                                  
         B     T6N                                                              
         SPACE 1                                                                
T6P      L     RE,ADRIO                                                         
         MVC   KEYCHK,0(RE)                                                     
         MVI   KEYCHK+15,C' '      FIX LEVFILT=N BUG                            
         CLC   RECOUNT,=F'0'                                                    
         BNE   *+10                                                             
         MVC   KEYCHK,KEY                                                       
*                                                                               
         CLI   KEYCHK+1,C'3'       UNIT 3 RETAIL                                
         BNE   *+10                                                             
         OC    KEYCHK+1(L'KEYCHK-1),SPACES   SPACE PAD KEY                      
*                                                                               
         NC    KEYCHK,KEYMASK                                                   
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
*              READ A RECORD AND CHECK KEY                                      
         SPACE 1                                                                
T6Q      GOTO1 VDATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'ACCOUNT',KEY,ADRIO           
T6Q2     TM    DMCB+8,X'FC'                                                     
         BZ    *+16                                                             
         MVI   ERROR,0                                                          
         LA    R2,INFRECH                                                       
         B     INFERR                                                           
         L     R1,RECOUNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,RECOUNT                                                       
         SPACE 1                                                                
         L     RE,ADRIO                                                         
         MVC   KEY,0(RE)                                                        
*                                                                               
         CLI   KEY+1,C'3'          UNIT 3 RETAIL                                
         BNE   *+10                                                             
         OC    KEY+1(L'KEY-1),SPACES   SPACE PAD KEY                            
*                                                                               
         NC    KEY,KEYMASK                                                      
         XC    KEY,KEYCHK                                                       
         BNZ   T6Q3                                                             
         L     RE,ADRIO                                                         
         CLC   ENDKEY,0(RE)        ARE WE PAST END KEY?                         
         BNL   T6Q4                                                             
*                                  NO MORE RELEVANT RECORDS                     
T6Q3     LA    R2,INFRECH                                                       
         XC    LINE(5),LINE                                                     
         MVI   LASTKMK,0                                                        
         CLC   INFKEY(4),=C'NEXT'                                               
         BNE   *+14                                                             
         MVC   INFKEY,SPACES                                                    
         OI    INFKEYH+6,X'80'                                                  
         CLI   VIRGIN,0                                                         
         BNE   *+14                                                             
         MVC   INFHEAD(L'NOHITS),NOHITS                                         
         B     INFEXIT                                                          
         MVC   INFHEAD(L'ENDREC),ENDREC                                         
         B     INFEXIT                                                          
NOHITS   DC    CL21'NO RECORDS TO DISPLAY'                                      
ENDREC   DC    CL28'END OF RECORDS FOR THIS TYPE'                               
*                                                                               
*                                  HAVE WE READ MORE RECDS THAN LIMIT?          
T6Q4     GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,0(R1)            R1=A(FACTS BLOCK)                            
         USING FACTSD,R1                                                        
         SR    R0,R0                                                            
         ICM   R0,3,FATIOCNT       GET ACTUAL IO COUNT                          
         C     R0,LIMIT            TEST FOR EXCEEDING IO LIMIT                  
         BNH   T6Q7                NO                                           
         DROP  R1                                                               
*                                                                               
         MVC   INFKEY(L'INFKEY),SPACES                                          
         MVC   INFKEY(4),=C'NEXT'                                               
         CLC   SAVENDKY,SPACES                                                  
         BE    *+14                                                             
         MVI   INFKEY+4,C','                                                    
         MVC   INFKEY+5(L'SAVENDKY),SAVENDKY                                    
         OI    INFKEYH+6,X'81'                                                  
         LA    R2,INFTABH                                                       
         MVI   LASTKMK,1                                                        
         L     RE,ADRIO                                                         
         MVC   LASTKEY,0(RE)                                                    
         EDIT  LIMIT,(4,SOFAR)                                                  
         MVC   INFHEAD(L'SOFAR),SOFAR                                           
         CLC   LINE,=H'20'         IF ROOM FOR MORE ON THIS SCREEN,             
         BNE   INFEXIT              SAVE DATA TO INCREMENT SCREEN NEXT          
         L     R4,RECFIELD          TIME                                        
         CLI   8(R4),0             TWO UP REQUIRED ?                            
         BE    T6Q5                                                             
         CLI   TWOUP,0             TWO UP IN USE ?                              
         BNE   T6Q5                                                             
         MVI   TWOUP,1                                                          
         B     INFEXIT                                                          
T6Q5     XC    LINE(5),LINE                                                     
         B     INFEXIT                                                          
SOFAR    DC    CL45'     RECORDS READ SO FAR - HIT ENTER FOR NEXT'              
         SPACE 2                                                                
*                                  CHECK FOR DELETES                            
T6Q7     L     R2,ADRIO                                                         
         TM    ACSTATUS-ACKEYD(R2),X'80'                                        
         BNO   T6QR                           NOT DELETED                       
         CLI   DELETES,1           DO I WANT DELETES                            
         BE    T6QR                YES, KEEP GOING                              
         CLI   0(R2),X'40'         IS IT A SPECIAL RECORD                       
         BL    T6Q8                IF IT IS READ NEXT                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),0(R2)        IF NOT SPECAIL                              
         MVI   KEY+15,X'FF'          SKIP TO NEXT ACCOUNT                       
         B     T6Q                                                              
T6Q8     GOTO1 VDATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'ACCOUNT',KEY,ADRIO           
         B     T6Q2                                                             
         EJECT                                                                  
*                                                                               
*              CHECK SECURITY                                                   
*                                                                               
T6QR     TM    AUTHTYPE,X'01'      IS THIS AN ACCOUNT-RECORD-READING            
         BNO   T6R6                RECORD TYPE                                  
         L     RE,ADRIO                                                         
         CLI   1(RE),C' '          AND NOT RULES AT COMPANY LEVEL               
         BE    T6R6                                                             
         SPACE 1                                                                
         L     R1,ATIA             RE-SET OFFPOS TUCKED IN TWA1                 
         XC    OFFICES,OFFICES                                                  
         MVC   OFFPOS,SOFFPOS-TWA1D(R1)                                         
*                                  THEN GET THE ACCOUNT LEVEL                   
         GOTO1 =V(ACSPLIT),DMCB,(4,ADRIO),SAVEHIER,ACLKEYS,RR=RB                
         LA    R1,1                                                             
         LA    R2,ACLKEYS                                                       
T6R1     L     RE,ADRIO                                                         
         CLC   0(L'ACKEYACC,RE),0(R2)                                           
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
*        CLC   RECOUNT,=F'1'       IF THIS IS 1ST RECORD OF THIS                
*        BNE   T6R6                ENQUIRY AND ISN'T LEVEL 1, CHECK             
         CLI   LEVEL+1,1           SECURITY AT HIGHER LEVELS                    
         BE    T6R6                                                             
*        CLI   LASTKMK,1                                                        
*        BE    T6R6                                                             
         MVC   KEYB,SPACES                                                      
         MVC   SAVLEVEL,LEVEL                                                   
         SR    R3,R3                                                            
T6R3     LR    R4,R3                                                            
         LA    R6,1(R3)                                                         
         STH   R6,LEVEL                                                         
         MH    R4,=H'15'                                                        
         LA    R2,ACLKEYS(R4)                                                   
         MVC   KEYB(L'ACKEYACC),0(R2)                                           
         MVI   ERROR,LVLMSSNG                                                   
         BAS   RE,READD                                                         
         BNZ   *+12                                                             
         LA    R2,INFRECH                                                       
         B     INFERR                                                           
         GOTO1 =A(SETELAD),DMCB,(RC),ADRIOB,RR=RELO                             
         GOTO1 =A(SETFILT),DMCB,(RC),RR=RELO                                    
         CLC   TERMAUTH,SECURITY                                                
         BL    T6R4                                                             
         L     R1,DMCB                                                          
         BAS   RE,SAVEPROF         SAVE HIGHER LEVEL PROFILE ELEMENTS           
         CLI   PHASE,X'0A'         IF IT'S RULES, GO TO OVERLAY TO SAVE         
         BNE   T6R3A               HIGHER LEVEL RULES ELEMENTS                  
         L     RF,RECFNTRY                                                      
         GOTO1 (RF),DMCB,(RC)                                                   
T6R3A    LA    R3,2(R3)                                                         
         CH    R3,SAVLEVEL                                                      
         BE    T6R5                                                             
         BCT   R3,T6R3                                                          
         B     T6R5                                                             
         SPACE 1                                                                
T6R4     LA    R3,1(R3)            HIGHER LEVEL RECORD LOCKED SO SET            
         BAS   RE,NEXTKEY          KEY IN IO TO READ NEXT AT THIS LEVEL         
         B     TNEXT               AND SKIP TO NEXT                             
         SPACE 1                                                                
T6R5     MVC   LEVEL,SAVLEVEL      SET UP ELEMENT ADDRESSES AND CHECK           
T6R6     GOTO1 =A(SETELAD),DMCB,(RC),ADRIO,RR=RELO                              
         GOTO1 =A(SETFILT),DMCB,(RC),RR=RELO                                    
         CLC   TERMAUTH,SECURITY                                                
         BNL   T6R8                SECURITY ^OK                                 
         LH    R3,LEVEL            OTHERWISE IS IT AN ACCOUNT RECORD            
         LTR   R3,R3                                                            
         BZ    TNEXT                                                            
         BAS   RE,NEXTKEY          SET KEY TO NEXT AT THIS LEVEL                
         B     TNEXT               SKIP TO NEXT                                 
*                                                                               
T6R8     TM    AUTHTYPE,X'01'      TEST ACCOUNT READING RECORD                  
         BZ    T6T                 NO-SKIP OFFICE SECURITY TEST                 
         TM    AUTHTYPE,X'40'      SKIP IF RETAIL RECORD ALSO                   
         BO    T6T                                                              
         L     RE,ADRIO                                                         
         CLI   0(RE),C' '          TEST SPECIAL RECORD TYPE                     
         BNH   T6T                 YES-SKIP TEST ALSO                           
         GOTO1 =A(CHEKOFF),DMCB,(RC),RR=RELO                                    
         BZ    T6T                 OFFICE SECURITY OK                           
         B     TNEXT               NOT OK. GET NEXT ACCOUNT                     
NEXTKEY  NTR1                      SETS KEY IN IO TO READ NEXT AT LEVEL         
         BCTR  R3,0                IN R3                                        
         SLL   R3,4                                                             
         ZIC   R2,SAVEHIER+2(R3)                                                
         L     R3,ADRIO                                                         
         LA    R3,3(R3)                                                         
         AR    R3,R2                                                            
         MVI   0(R3),X'FF'                                                      
         XIT1                                                                   
         EJECT                                                                  
*              IF THIS IS A RECORD TYPE THAT USES THE PROFILE ELEMENT,          
*              SAVE HIGHER LEVEL ELEMENTS IN TWA AND CREATE A COMPOSITE         
*              PROFILE ELEMENT. IF RECORD TYPE IS XP,DO SAME FOR                
*              EXTRA-PROFILE ELEMENT                                            
         SPACE 1                                                                
T6T      TM    AUTHTYPE,X'01'      ACCOUNT-READING RECOD TYPE                   
         BNO   T6V                                                              
         L     RE,ADRIO                                                         
         CLI   1(RE),C' '                                                       
         BE    T6V                                                              
         SPACE 1                                                                
         BAS   RE,SAVEPROF         SAVE PROFILE ELEMENT IF HIGHER LEVEL         
         CLI   LEVEL+1,1                                                        
         BE    T6V                                                              
         L     R2,APRO                                                          
         SPACE 1                                                                
T6T1     LH    R3,LEVEL                                                         
         BCTR  R3,0                                                             
         LR    R4,R3                                                            
         LA    R5,PROFTAB          R5 = A(COMPMOVE TABLE FOR PROF EL)           
         XC    WORK,WORK           PROFILE ELEMENT DEFAULTS ARE NULLS           
         LA    RF,WORK                                                          
         USING ACPROFD,RF                                                       
         MVC   ACPROFFC,SPACES     EXCEPT FOR OFFICE                            
         MVC   ACPRUNBL,SPACES     AND UNBILLABLE WORKCODES                     
         DROP  RF                                                               
         SPACE 1                                                                
T6T2     LR    R1,R3                                                            
         BCTR  R1,0                                                             
         LA    R6,L'SAVEPRO                                                     
         MR    R0,R6                                                            
         LA    R6,SAVEPRO(R1)      R6 = A(SAVED EL AT HIGHER LEVEL)             
         BAS   RE,COMPMOVE                                                      
         BE    T6T3                ALL RELEVANT FIELDS HAVE VALUES              
         BCT   R3,T6T2                                                          
         CLI   0(R2),0             DO WE HAVE AN ELEMENT                        
         BNE   T6T3                                                             
         XC    APRO,APRO           IF NOT CLEAR ITS ADDRESS                     
         SPACE 1                                                                
T6T3     CLI   PHASE,8             REPEAT FOR EXTRA-PROFILE IF XP               
         BNE   T6V                                                              
         MVC   XPRFEL,XPRDEFLT                                                  
         ICM   R2,15,AXPR                                                       
         BZ    T6T3A                                                            
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   XPRFEL(0),0(R2)                                                  
T6T3A    LA    R2,XPRFEL                                                        
         ST    R2,AXPR                                                          
         SPACE 1                                                                
T6T4     LR    R3,R4               RESTORE LEVEL MINUS |                        
         LA    R5,XPRFTAB                                                       
         LA    RF,XPRDEFLT                                                      
         SPACE 1                                                                
T6T5     LR    R1,R3                                                            
         BCTR  R1,0                                                             
         LA    R6,L'SAVEXPR                                                     
         MR    R0,R6                                                            
         LA    R6,SAVEXPR(R1)                                                   
         BAS   RE,COMPMOVE                                                      
         BE    T6V                                                              
         BCT   R3,T6T5                                                          
         B     T6V                                                              
         SPACE 3                                                                
*              ROUTINE TO SAVE A LEVEL 1 OR 2 PROFILE ELEMENT                   
         SPACE 1                                                                
SAVEPROF NTR1                                                                   
         ICM   R2,15,APRO          ANY PROFILE ELEMENT AT THIS LEVEL            
         BNZ   SP1                                                              
         XC    PROFEL,PROFEL       IF NOT PREPARE SPACE FOR ONE                 
         LA    R2,PROFEL                                                        
         ST    R2,APRO                                                          
SP1      DS    0H                  FRIG ACPRCOST TO CONTAIN COSTING &           
         USING ACPROFD,R2          SALES ANALYSIS A/C'S WITHOUT C/U/L           
         MVC   ACPRCOST(12),ACPRCOST+3                                          
         ICM   R3,15,ASAN                                                       
         BZ    *+10                                                             
         USING ACSAND,R3                                                        
**** MOVING SALES ANALYSIS INTO APRO ELEMENT TO COORDINATE WITH                 
***  PROFTAB                                                                    
         MVC   ACPRUNBL(12),ACSACODE+3                                          
         OC    ACPROFFC,SPACES     SPACE PADDED DEFAULT FIELDS                  
         DROP  R2,R3                                                            
         SPACE 1                                                                
         CLI   LEVEL+1,3                                                        
         BNL   SPEXIT                                                           
         LH    R5,LEVEL                                                         
         BCTR  R5,0                                                             
         LA    R1,L'SAVEPRO                                                     
         MR    R0,R5                                                            
         LA    R3,SAVEPRO(R1)                                                   
         MVC   0(L'SAVEPRO,R3),0(R2)                                            
         CLI   PHASE,8             XP REQUIRES EXTRA-PROFILE EL TOO             
         BNE   SPEXIT                                                           
         LA    R1,L'SAVEXPR                                                     
         MR    R0,R5                                                            
         LA    R3,SAVEXPR(R1)                                                   
         MVC   0(L'SAVEXPR,R3),XPRDEFLT                                         
         ICM   R2,15,AXPR                                                       
         BZ    SP2                                                              
         ZIC   R1,1(R2)            (VARIABLE-LENGTH ELEMENT)                    
         LA    R0,L'SAVEXPR                                                     
         CR    R1,R0                                                            
         BNH   *+6                                                              
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,SPMVC                                                         
         SPACE 1                                                                
SP2      CLI   LEVEL+1,1           IF LEVEL 1 SET UP A(EXTRA-PROF)              
         BNE   SPEXIT                                                           
         LA    R2,SAVEXPR                                                       
         ST    R2,AXPR                                                          
         SPACE 1                                                                
SPEXIT   XIT1                                                                   
SPMVC    MVC   0(0,R3),0(R2)                                                    
         SPACE 3                                                                
*              ROUTINE TO GENERATE A COMPOSITE PROFILE ELEMENT BY               
*              MOVING IN FIELD VALUES FROM THE NEXT HIGHER LEVEL IF             
*              MISSING AT THIS LEVEL                                            
*              ON ENTRY R2 = A(COMPOSITE ELEMENT)                               
*                       R5 = A(PROFILE FIELD TABLE)                             
*                       R6 = A(HIGHER LEVEL ELEMENT)                            
*                       RF = A(DEFAULT LIST)                                    
*              ON EXIT  CC = EQU IF NO VALUES NOW MISSING                       
         SPACE 1                                                                
COMPMOVE NTR1                                                                   
         MVI   DMCB,0                                                           
         SR    R7,R7                                                            
         SR    R9,R9                                                            
         SPACE 1                                                                
CM1      CLI   0(R5),X'FF'                                                      
         BE    CMEXIT                                                           
         IC    R7,0(R5)            DISPLACEMENT                                 
         LA    R3,0(R2,R7)                                                      
         LA    R1,0(RF,R7)                                                      
         IC    R9,1(R5)            LENGTH                                       
         BCTR  R9,0                                                             
         EX    R9,ALLDEF1          IS VALUE MISSING IN COMPOSITE EL ?           
         BNE   CM3                                                              
         LA    R4,0(R6,R7)         IF SO                                        
         EX    R9,ALLDEF2          IS VALUE MISSING AT HIGHER LEVEL ?           
         BE    CM2                                                              
         EX    R9,MVFIELD          IF NOT MOVE IT IN                            
         B     CM3                                                              
CM2      MVI   DMCB,1              SET MARKER FOR MISSING VALUE                 
CM3      LA    R5,L'PROFTAB(R5)                                                 
         B     CM1                                                              
         SPACE 1                                                                
ALLDEF1  CLC   0(0,R3),0(R1)                                                    
ALLDEF2  CLC   0(0,R4),0(R1)                                                    
MVFIELD  MVC   0(0,R3),0(R4)                                                    
         SPACE 1                                                                
CMEXIT   CLI   DMCB,0                                                           
         XIT1                                                                   
         SPACE 1                                                                
*              TABLE FOR BUILDING COMPOSITE PROFILE ELEMENT                     
*                                                                               
*              CLI       X         DISPLACEMENT FROM START OF ELEMENT           
*              CLI       X         LENGTH OF FIELD                              
         SPACE 1                                                                
PROFTAB  DS    0CL2                                                             
         DC    AL1(ACPREL-ACPROFD),AL1(L'ACPREL)                                
         DC    AL1(ACPRLEN-ACPROFD),AL1(L'ACPRLEN)                              
         DC    AL1(ACPRGRUP-ACPROFD),AL1(L'ACPRGRUP)                            
         DC    AL1(ACPRRECV-ACPROFD),AL1(L'ACPRRECV)                            
         DC    AL1(ACPRCOST-ACPROFD),AL1(12)      COSTING A/C                   
         DC    AL1(ACPRUNBL-ACPROFD),AL1(12)    SALES ANAL A/C                  
         DC    AL1(ACPRBILL-ACPROFD),AL1(L'ACPRBILL)                            
         DC    AL1(ACPRBLAM-ACPROFD),AL1(L'ACPRBLAM)                            
         DC    AL1(ACPROFFC-ACPROFD),AL1(L'ACPROFFC)                            
*        DC    AL1(ACPRUNBL-ACPROFD),AL1(L'ACPRUNBL)                            
         DC    X'FF'                                                            
         SPACE 1                                                                
XPRFTAB  DS    0CL2                                                             
         DC    AL1(ACXPEL-ACXPROFD),AL1(L'ACXPEL)                               
         DC    AL1(ACXPLEN-ACXPROFD),AL1(L'ACXPLEN)                             
         DC    AL1(ACXPDUE-ACXPROFD),AL1(L'ACXPDUE)                             
         DC    AL1(ACXPOVER-ACXPROFD),AL1(L'ACXPOVER)                           
         DC    AL1(ACXPLOW-ACXPROFD),AL1(L'ACXPLOW)                             
         DC    AL1(ACXPSUM-ACXPROFD),AL1(L'ACXPSUM)                             
         DC    AL1(ACXPNET-ACXPROFD),AL1(L'ACXPNET)                             
         DC    AL1(ACXPDET-ACXPROFD),AL1(L'ACXPDET)                             
         DC    AL1(ACXPCD-ACXPROFD),AL1(L'ACXPCD)                               
         DC    AL1(ACXPEST-ACXPROFD),AL1(L'ACXPEST)                             
         DC    X'FF'                                                            
         SPACE 1                                                                
XPRDEFLT DC    X'3C12'             DEFAULT EXTRA PROFILE ELEMENT                
         DC    PL2'10'                                                          
         DC    PL3'10000'                                                       
         DC    PL4'5000'                                                        
         DC    C'YNYN  '                                                        
         DC    X'00'                                                            
         EJECT                                                                  
*              APPLY FILTERS VIA FILTER TABLE                                   
T6V      CLI   PHASE,X'0A'         RULES                                        
         BE    T6Y                                                              
         LA    R6,FILTAB                                                        
         USING FTBD,R6                                                          
         L     RE,ADRIO                                                         
         LA    RE,42(RE)           R0=A(1ST ELEMENT)                            
         LR    R0,RE                                                            
         SPACE 1                                                                
T6V1     CLI   FTBELMNT,X'FF'      END OF FILTER TABLE                          
         BE    T6Y                                                              
         MVC   FULL,FTBELMNT                                                    
         L     R2,FULL                                                          
         L     R2,0(R2)                                                         
         LTR   R2,R2                                                            
         BNZ   T6V2                IF ELEMENT CONTAINING FILTER DOES            
T6V15    CLI   FTBSIGN,C'N'        NOT EXIST                                    
         BE    T6V5                NEGATIVE FILTER IS SATISFIED                 
         B     TNEXT               POSITIVE ONE IS NOT                          
         SPACE 1                                                                
T6V2     CR    R2,R0               SIMILARLY IF THE FILTER FIELD IS             
         BL    T6V25               BEYOND THE END OF THE ELEMENT                
         CLC   1(1,R2),FTBDISP+1                                                
         BNH   T6V15                                                            
T6V25    MVC   HALF,FTBDISP                                                     
         AH    R2,HALF             R2 = FIELD ADDRESS                           
         ZIC   R3,FTBLEN           R3 = FIELD LENGTH                            
         LA    R5,FTBVAL           R5 = A(MASK OR COMPARE VALUE)                
         MVC   THISBR,FBRANCH                                                   
         SR    R9,R9                                                            
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
         MVI   THISBR+1,X'70'      SWITCH EXECUTED BRANCH FROM 'BE' TO          
         BCTR  R3,0                'BNE' IF POSITIVE                            
         EX    R3,FCOMPARE                                                      
         EX    R9,THISBR                                                        
         B     T6V5                                                             
         SPACE 1                                                                
T6V4     MVI   THISBR+1,X'C0'      TEST UNDER MASK (NOT COMPARE)                
         CLI   FTBSIGN,C'P'                                                     
         BE    *+8                                                              
         MVI   THISBR+1,X'10'      SWITCH EXECUTED BRANCH FROM 'BNO' TO         
         ZIC   R7,0(R5)            'BO' IF NEGATIVE                             
         EX    R7,FTEST                                                         
         EX    R9,THISBR                                                        
         SPACE 1                                                                
T6V5     LA    R6,FTBTBLEN(R6)     BUMP TO NEXT FILTER                          
         B     T6V1                                                             
         SPACE 1                                                                
FBRANCH  BE    TNEXT               EXECUTED INSTRUCTIONS                        
THISBR   DS    F                                                                
FCOMPARE CLC   0(0,R2),0(R5)                                                    
FTEST    TM    0(R2),0                                                          
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*              COME HERE WITH A RECORD TO DISPLAY                               
*                                                                               
T6Y      CLI   VIRGIN,0            SCREEN EMPTY                                 
         BNE   T6Z                                                              
         MVI   VIRGIN,1                                                         
         MVI   LASTKMK,1                                                        
         L     R5,RECPREDT                                                      
         LA    R6,INFDATAH                                                      
         USING LINED,R6                                                         
         CLI   0(R5),X'FF'         ANY PRE-HEADING DATA ?                       
         BE    T6Y2                                                             
         L     R1,RECPREHD                                                      
         MVC   LINEDATA,0(R1)      IF SO SET UP CONSTANT ELEMENTS (XX=)         
         OI    LINEHDR+6,X'80'      USE COMMON CODE TO SETUP VARIABLE           
         B     T6621                DATA AND SQUASH THE RESULT                  
*                                                                               
T6Y1     DS    0H                  I'LL FIND THIS FUCKING BUG                   
         LA    RF,INFDATAH                                                      
         CR    R6,RF                                                            
         BNL   *+6                                                              
         DC    H'0'                GOT IT!                                      
         LA    RF,INFLAST                                                       
         CR    R6,RF                                                            
         BL    *+6                                                              
         DC    H'0'                NO, THIS IS IT!                              
*                                                                               
         GOTO1 VSQASHER,DMCB,INFDATA,78                                         
         LA    R1,2                UPDATE LINE COUNT AND SCREEN POINTER         
         STH   R1,LINE              BY 2                                        
         LA    R6,LINELEN*2(R6)                                                 
*                                                                               
T6Y2     L     R1,RECHEAD          TWO HEADING LINES                            
         MVC   LFIRSTH,0(R1)                                                    
         MVC   LSECONDH,78(R1)                                                  
         OI    LINEHDR+6,X'80'                                                  
         LA    R6,LINELEN(R6)                                                   
         MVC   LFIRSTH,39(R1)                                                   
         MVC   LSECONDH,117(R1)                                                 
         OI    LINEHDR+6,X'80'                                                  
         LH    R1,LINE                                                          
         LA    R1,2(R1)                                                         
         STH   R1,LINE                                                          
         STH   R1,FRSTLINE         FOR USE IF TWO UP                            
         SPACE 3                                                                
*                                  SCREEN FULL                                  
T6Z      CLC   LINE,=H'20'                                                      
         BNE   T661                                                             
         L     R5,RECFIELD                                                      
         USING DATTABD,R5                                                       
         CLI   DATTWOUP,0          TWO UP ?                                     
         BE    T660                                                             
         CLI   TWOUP,0             2ND HALF ALREADY USED ?                      
         BNE   T660                                                             
         MVI   TWOUP,1                                                          
         MVC   LINE,FRSTLINE                                                    
         B     T661                                                             
         SPACE 1                                                                
T660     MVC   INFKEY(L'INFKEY),SPACES                                          
         MVC   INFKEY(4),=C'NEXT'                                               
         CLC   SAVENDKY,SPACES                                                  
         BE    *+14                                                             
         MVI   INFKEY+4,C','                                                    
         MVC   INFKEY+5(L'SAVENDKY),SAVENDKY                                    
         OI    INFKEYH+6,X'81'                                                  
         LA    R2,INFTABH                                                       
         XC    LINE(5),LINE                                                     
         L     RE,ADRIO                                                         
         MVC   LASTKEY,0(RE)                                                    
         MVC   INFHEAD(L'HITNEXT),HITNEXT                                       
         B     INFEXIT                                                          
HITNEXT  DC    CL38'RECORDS DISPLAYED - HIT ENTER FOR NEXT'                     
*                                                                               
*                                  SCREEN NOT FULL                              
T661     LA    R6,INFDATAH                                                      
         LH    R7,LINE                                                          
         LA    RF,1(R7)                                                         
         STH   RF,LINE                                                          
         MVC   POSSLINE,LINE                                                    
         MH    R7,=H'86'                                                        
         AR    R6,R7               R6 = FIELD HEADER FOR THIS DATA LINE         
         L     R5,RECFIELD         R5 = FIELD TABLE ENTRY                       
*                                                                               
T662     CLI   0(R5),X'FF'                                                      
         BNE   T6621                                                            
*                                                                               
         L     RF,ATWA             MAKE SURE THAT RA IS STILL POINTING          
         CR    RA,RF               TO A(TWA)                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,INFDAT2H                                                      
         CR    RF,R6               HAVE WE BEEN SETTING UP PRE-HEADINGS         
         BH    T6Y1                YES                                          
         MVC   LINE,POSSLINE                                                    
         B     TNEXT               NO                                           
*                                                                               
T6621    MVC   HALF,DATELMNT                                                    
         LH    R2,HALF                                                          
         AR    R2,RC                                                            
         L     R2,0(R2)                                                         
         LTR   R2,R2                                                            
         BZ    T6634                                                            
*                                                                               
         L     R3,AKEY                                                          
         CR    R3,R2                                                            
         BE    T6621A                                                           
*                                                                               
         MVC   HALF,DATDISP        CHECK IF ELEMENT IS TOO SHORT                
         ZIC   R0,1(R2)            TO ADDRESS FIELD                             
         CH    R0,HALF                                                          
         BH    T6621A                                                           
         ZIC   RF,DATSTART                                                      
         LA    RF,6(R6,RF)                                                      
         MVI   0(RF),C'.'                                                       
         B     T6634                                                            
*                                                                               
T6621A   MVC   HALF,DATDISP                                                     
         AH    R2,HALF             R2 = FIELD ADDRESS                           
         ZIC   R3,DATLEN           R3 = FIELD LENGTH                            
         SR    R4,R4               R4 = NUMBER OF ADDITIONAL LINES USED         
*                                                                               
         OC    DATEDIT,DATEDIT     ANY EDITING REQUIRED                         
         BZ    T663                NO                                           
         L     RF,RECDNTRY                                                      
         CLI   DATEDIT,X'FF'       YES - IN OVERLAY                             
         BE    T6622                                                            
         MVC   HALF,DATEDIT        YES - IN ROOT                                
         LH    RF,HALF                                                          
         AR    RF,RC                                                            
         L     RF,0(RF)                                                         
*                                                                               
T6622    GOTO1 (RF),DMCB,(RC)      R2/3/4 RETURNED BY EDIT ROUTINE              
         TM    DMCB,X'01'          ERROR                                        
         BNO   T663                                                             
         SR    R2,R2                                                            
         B     INFERR                                                           
*                                                                               
T663     LTR   R4,R4               R4 = NEGATIVE IF WE DONT WANT THIS           
         BNM   T6630                    RECORD AFTER ALL                        
         MVC   LINEDATA,SPACES                                                  
         LH    RF,LINE                                                          
         BCTR  RF,0                                                             
         STH   RF,LINE                                                          
         B     TNEXT                                                            
*                                                                               
T6630    BCTR  R3,0                IS THERE ROOM FOR THE NUMBER OF              
         LH    R1,LINE             EXTRA LINES REQUIRED FOR THE FIELD ?         
         AR    R1,R4                                                            
         CH    R1,POSSLINE                                                      
         BNH   T6632                                                            
         CH    R1,=H'20'                                                        
         BNH   T6631                                                            
*                                                                               
         LA    R2,LINELEN          NO THERE ISN'T SO CLEAR ANYTHING             
         LA    R3,INFDATLH         WEVE SET UP ON THE SCREEN FOR THIS           
         XC    LINEDATA,LINEDATA   RECORD                                       
         BXLE  R6,R2,*-6                                                        
         B     T660                AND EXIT                                     
*                                                                               
T6631    STH   R1,POSSLINE                                                      
T6632    LA    R4,1(R4)            R4 = COUNT OF LINES                          
         LR    R7,R6               R7 = LINE HEADER                             
         ZIC   RF,DATSTART         RF = DISPLACEMENT INTO LINE (COL NO)         
         CLI   TWOUP,0                                                          
         BE    *+8                                                              
         IC    RF,DATTWOUP                                                      
*                                                                               
T6633    OI    6(R7),X'80'                                                      
         LA    R9,6(R7,RF)         R9 = START POSITION IN DATA LINE             
         EX    R3,FLDTOTWA                                                      
         LA    R2,1(R2,R3)                                                      
         LA    R7,LINELEN(R7)                                                   
         BCT   R4,T6633                                                         
*                                                                               
T6634    LA    R5,DATTBLEN(R5)                                                  
         CLC   DISP2C,DATTABD      ARE WE UP TO THE '2C' TABLE ENTRY?           
         BE    T6635               YES                                          
         B     T662                                                             
*                                                                               
T6635    TM    FLAG,GETFRM30       HAVE WE ALREADY GOT THE ANALYSIS             
*                                  CODE FROM THE '30'?                          
         BZ    T662                NO - SO CONTINUE READING TABLE               
         LA    R5,DATTBLEN(R5)     OTHERWISE BUMP PAST '2C' ENTRY               
         B     T662                                                             
*                                                                               
FLDTOTWA MVC   0(0,R9),0(R2)                                                    
         SPACE 1                                                                
         DROP  R5                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              UPDATE KEY AND BRANCH TO READ NEXT RECORD                        
         SPACE 1                                                                
TNEXT    NI    FLAG,X'FF'-GETFRM30                                              
         NI    FLAG,X'FF'-NO2CELEM                                              
         NI    FLAG,X'FF'-NOTANALY                                              
         NI    FLAG,X'FF'-GETFRM2C                                              
         CLI   LEVFILT,0           IF WE ARE FILTERING ON LEVEL 1 AND/          
         BE    T6641               OR 2 ACCOUNTS SET NEXT KEY TO READ           
         CLC   LEVFILT,LEVEL+1     NEXT AT THIST LEVEL                          
         BH    T6641                                                            
         ZIC   R3,LEVFILT                                                       
         BAS   RE,NEXTKEY                                                       
         SPACE 1                                                                
T6641    LA    R3,L'KEY            OTHERWISE INSERT X'FF' AFTER LAST            
         LA    R2,KEYMASK-1(R3)    VARIABLE KEY COMPONENT OF LAST REC           
         CLI   0(R2),X'FF'                                                      
         BNE   *+8                                                              
         BCT   R3,*-12                                                          
         L     RE,ADRIO                                                         
         MVC   KEY,0(RE)                                                        
         LTR   R3,R3                                                            
         BZ    T6Q                                                              
         LA    R2,KEY(R3)                                                       
         MVI   0(R2),X'FF'                                                      
         CLC   RECOUNT,=F'1'       IF WE HAVE READ ONLY ONE RECORD              
         BE    T6N0                 BRANCH TO RECONSTITUTE KEY TREATING         
         B     T6Q                  MANDATORY,VARIABLE KEYS AS VARIABLE         
         EJECT                                                                  
*              ERROR HANDLING AND EXIT                                          
         SPACE 1                                                                
INFERR   GOTO1 VGETMSG,DMCB+12,(ERROR,INFHEAD),(FNDX,DMCB),0                    
         OC    LINE,LINE                                                        
         BNZ   INFEXIT                                                          
         LA    R3,INFKEYH                                                       
         CR    R2,R3                                                            
         BE    HELPKEY                                                          
         LA    R3,INFFILTH                                                      
         CR    R2,R3                                                            
         BE    HELPFILT                                                         
         LA    R3,INFRECH                                                       
         CR    R2,R3                                                            
         BE    HELPREC                                                          
INFEXIT  CLI   HELPKMK,1                                                        
         BNE   *+8                                                              
         LA    R2,INFKEYH                                                       
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         LA    R2,INFRECH                                                       
         OI    6(R2),X'40'                                                      
         L     R3,ATIA             A(TWA1 - EXTRA TWA)                          
         MVC   DMCB+10(2),2(RA)    SAVE IT                                      
         MVC   DMCB+8(2),=X'0100'                                               
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R3)                        
         XMOD1 1                                                                
         EJECT                                                                  
*              HELPREC - DISPLAYS DETAILS OF VALID RECORD TYPES WHEN            
*                        HELP IS REQUESTED OR AN ERROR OCCURS                   
         SPACE 1                                                                
HELPREC  BAS   RE,CLEARTWA                                                      
*                                                                               
         CLI   ERROR,0             IS THIS AN ERROR OR A HELP REQUEST +         
         BNE   HR04                                                             
         GOTO1 VCALLOV,DMCB,(9,0),(0,T605TWA)                                   
         CLI   DMCB+4,X'FF'                                                     
         BE    HR04                                                             
*                                                                               
         L     R1,DMCB             MOVE 19 LINES OF EXPLANATORY TEXT            
         LA    R6,INFDATAH          FROM OVERLAY INTO TWA.                      
         USING LINED,R6                                                         
         LA    R4,19                                                            
*                                                                               
HR02     OI    LINEHDR+6,X'80'                                                  
         MVC   LINEDATA+19(59),0(R1)                                            
         LA    R1,59(R1)                                                        
         LA    R6,LINELEN(R6)                                                   
         BCT   R4,HR02                                                          
*                                                                               
HR04     MVC   INFDATA(12),=C'RECORD TYPES'                                     
         OI    INFDATAH+6,X'80'                                                 
         LA    R6,INFDAT3H                                                      
         L     R4,ARECLIST                                                      
         USING RECTABD,R4                                                       
         LA    R9,16                                                            
         USING RECTABD,R4                                                       
*                                                                               
         CLC   INFREC(5),=C'NHELP'                                              
         BNE   HR08                                                             
         LA    R1,16                                                            
         CLI   0(R4),0                                                          
         BE    HR06                                                             
         LA    R4,RECTBLEN(R4)                                                  
         BCT   R1,*-12                                                          
         CLI   0(R4),0                                                          
         BNE   HR08                                                             
*                                                                               
HR06     MVC   LINEDATA(12),=C'NO MORE HELP'                                    
         B     HR12                                                             
*                                                                               
HR08     CLI   0(R4),0                                                          
         BE    HR14                                                             
         TM    RECAUTH,X'F0'       SKIP PRIVILEGED RECORD TYPES AT              
         BNO   HR10                 NON-DDS TERMINALS.                          
         CLI   T605TWA+1,C'*'                                                   
         BE    HR10                                                             
         LA    R4,RECTBLEN(R4)                                                  
         B     HR08                                                             
*                                                                               
HR10     MVC   LINEDATA(L'RECNAME),RECNAME                                      
         MVC   LINEDATA+12(4),=C'(  )'                                          
         MVC   LINEDATA+13(2),RECTYPE                                           
         OI    LINEHDR+6,X'80'                                                  
         LA    R6,LINELEN(R6)                                                   
         LA    R4,RECTBLEN(R4)                                                  
         BCT   R9,HR08                                                          
*                                                                               
         CLI   0(R4),0                                                          
         BE    HR14                                                             
         MVC   LINEDATA(13),=C'ENTER ''NHELP'''                                 
*                                                                               
HR12     OI    LINEHDR+6,X'80'                                                  
*                                                                               
HR14     MVI   VIRGIN,1                                                         
         B     INFEXIT                                                          
         SPACE 1                                                                
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              HELPKEY - DISPLAYS DETAILS OF VALID KEY EXPRESSIONS WHEN         
*                        HELP IS REQUESTED OR A KEY ERROR OCCURS                
         SPACE 1                                                                
HELPKEY  BAS   RE,CLEARTWA                                                      
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
         EDIT  (R3),(1,32(R6))     KEY NUMBER                                   
         MVC   LINEDATA+27(L'KEYNAME),KEYNAME                                   
         ZIC   R5,KEYLEN                                                        
         EDIT  (R5),(2,46(R6))     MAX LENGTH                                   
         CLI   KEYMAND,C'O'                                                     
         BNE   *+12                                                             
         MVI   LINEDATA+26,C'('                                                 
         MVI   LINEDATA+40,C')'                                                 
         LA    R6,LINELEN(R6)                                                   
         SPACE 1                                                                
HK2      LA    R4,KEYTBLEN(R4)                                                  
         B     HK1                                                              
         SPACE 2                                                                
HK3      LTR   R3,R3               ANY KEYS ?                                   
         BZ    HK5                                                              
         LA    R6,LINELEN(R6)                                                   
HK4      CLI   LASTKMK,0                                                        
         BE    *+14                                                             
         OI    LINEHDR+6,X'80'                                                  
         MVC   LINEDATA+24(7),=C'OR NEXT'                                       
         MVC   INFDATA+24(14),=C'VALID KEYS FOR'                                
         OI    INFDAT3H+6,X'80'                                                 
         MVC   INFDAT3+26(16),=C'DESCRIPTION LGTH'                              
         B     HK6                                                              
HK5      MVC   INFDATA+24(11),=C'NO KEYS FOR'                                   
HK6      OI    INFDATAH+6,X'80'                                                 
         MVC   INFDAT2+24(16),=C'THIS RECORD TYPE'                              
         OI    INFDAT2H+6,X'80'                                                 
         MVI   VIRGIN,1                                                         
         CLI   HELPKMK,1                                                        
         BE    T6EE                                                             
         B     INFEXIT                                                          
         SPACE 1                                                                
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              HELPFILT -DISPLAYS DETAILS OF VALID FILTER EXPRESSIONS           
*                        WHEN HELP IS REQUESTED OR AN ERROR OCCURS              
         SPACE 1                                                                
HELPFILT CLI   HELPKMK,1                                                        
         BE    *+8                                                              
         BAS   RE,CLEARTWA                                                      
         LA    R6,INFDAT5H                                                      
         USING LINED,R6                                                         
         SR    R3,R3               R3 = ANY FILTERS ?                           
         L     R4,RECFILT                                                       
         USING FILTERSD,R4                                                      
         LA    R9,16                                                            
         SPACE 1                                                                
         CLC   INFFILT+4(4),=C'NEXT' IF REQUEST IS FOR CONTINUATION             
         BNE   HF1                 BUMP TO THE 16TH FILTER                      
         LA    R1,15                                                            
         CLI   0(R4),0                                                          
         BE    HF0                                                              
         LA    R4,FILTBLEN(R4)                                                  
         BCT   R1,*-12                                                          
         CLI   0(R4),0                                                          
         BNE   HF1                                                              
HF0      MVC   INFDATA+44(15),=C'NO MORE FILTERS'                               
         B     HF6                                                              
         SPACE 1                                                                
HF1      CLI   0(R4),0                                                          
         BE    HF4                                                              
         LA    R3,1                                                             
         OI    LINEHDR+6,X'80'                                                  
         MVC   LINEDATA+44(L'FILFULKW),FILFULKW                                 
         LA    R5,L'FILFULKW-1                                                  
         LA    R7,FILFULKW(R5)                                                  
         CLI   0(R7),C' '                                                       
         BNE   *+8                 R5 = KEYWORD LENGTH MINUS 1                  
         BCT   R5,*-12                                                          
         LA    R5,LINEDATA+45(R5)                                               
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
         LA    R4,FILTBLEN(R4)                                                  
         BCT   R9,HF1                                                           
         SPACE 1                                                                
         CLI   0(R4),0             ANY MORE                                     
         BE    HF4                                                              
         MVC   INFDATL+44(30),=C'ENTER ''HELPNEXT'' FOR REMAINDER'              
         SPACE 2                                                                
HF4      LTR   R3,R3               ANY FILTERS ?                                
         BZ    HF5                                                              
         MVC   INFDATA+44(13),=C'VALID FILTERS'                                 
         B     *+10                                                             
HF5      MVC   INFDATA+44(10),=C'NO FILTERS'                                    
HF6      MVC   INFDAT2+44(20),=C'FOR THIS RECORD TYPE'                          
         OI    INFDATAH+6,X'80'                                                 
         OI    INFDAT2H+6,X'80'                                                 
         MVI   VIRGIN,1                                                         
         B     INFEXIT                                                          
         SPACE 1                                                                
         DROP  R4                                                               
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*              EDITING SUBROUTINES FOR KEY COMPONENTS, FILTER FIELDS            
*              AND DATA FIELDS                                                  
*              ON ENTRY P1 = A(GWS) -(FOR CONSISTENCY WITH OVERLAYS)            
*                       R2 = A(FIELD TO BE EDITED)                              
*                       R3 = LENGTH OF FIELD TO BE EDITED - IF REQUIRED         
*              ON EXIT  R2 = A(EDITED FIELD)                                    
*                       R3 = LENGTH OF EDITED FIELD - IF REQUIRED               
*                       R4 = ADDITIONAL LINES REQUIRED TO DISPLAY DATA          
*                            FIELD - IF REQUIRED                                
*              ON ERROR P1 BYTE 0 IS SET TO X'01'                               
         SPACE 1                                                                
VDITCHAR NTR1                      LEFT-ALIGNS CHARACTER FIELDS                 
         SR    RF,RF                                                            
VCHAR1   LA    R5,0(R2,RF)                                                      
         CLI   0(R5),C' '                                                       
         BNE   VCHAR2                                                           
         LA    RF,1(RF)                                                         
         B     VCHAR1                                                           
VCHAR2   SR    R3,RF                                                            
         AR    R2,RF                                                            
         B     VXIT                                                             
         SPACE 1                                                                
*                                  **************\*********************         
         SPACE 1                                                                
VDITDATE NTR1                      CONVERTS DATES IN PACKED FORMAT TO           
         BCTR  R3,0                (D)DMMMYY OR /YY                             
         MVC   WORK+1(8),SPACES                                                 
         EX    R3,*+8                                                           
         B     *+10                                                             
         OC    0(0,R2),0(R2)                                                    
         BE    VDATE2                                                           
         GOTO1 VDATCON,DMCB,(1,0(R2)),(8,WORK+1)                                
*                                                                               
VDATE2   LA    R2,WORK+1                                                        
         LA    R3,8                                                             
         MVI   DMCB,0              SET FOR NO ERROR                             
         B     VXIT                                                             
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
EDITMASK SR    R3,R3               EDITS ANY STATUS BYTE VALUES TO              
*                                  DISPLAY FORMAT                               
         LA    R5,WORK                                                          
         SR    R1,R1               INPUTS:                                      
EM1      CLI   0(RF),0                      R2 = A(STATUS BYTE)                 
         BE    EM3                          RF = A(TABLE OF MASKS AND           
         IC    R1,0(RF)                            DISPLAY CHARACTERS)          
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(R2),0                                                          
         BNO   EM2                                                              
         MVC   0(1,R5),1(RF)                                                    
         B     *+8                                                              
EM2      MVI   0(R5),C'.'                                                       
         LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
         LA    RF,2(RF)                                                         
         B     EM1                                                              
EM3      LA    R2,WORK                                                          
         B     VXIT                                                             
         SPACE 1                                                                
*                                  ************************************         
EDITMSK2 SR    R3,R3               EDITS COST FIELD TO DISPLAY FORMAT           
*                                                                               
         LA    R5,WORK                                                          
ED1      CLI   0(RF),0             INPUTS:                                      
         BE    ED3                     R2 = A(STATUS BYTE)                      
         ZIC   R1,0(RF)                RF = A(TABLE OF MASKS AND                
         EX    R1,*+8                         DISPLAY CHARACTERS)               
         B     *+8                                                              
         TM    0(R2),0                                                          
         BO    ED2                                                              
         LA    RF,2(RF)                                                         
         B     ED1                                                              
*                                                                               
ED2      MVC   0(1,R5),1(RF)                                                    
         B     *+8                                                              
*                                                                               
ED3      MVI   0(R5),C'.'                                                       
*                                                                               
EDX      LA    R3,1(R3)                                                         
         LA    R2,WORK                                                          
         B     VXIT                                                             
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITBIN2 NTR1                      EDITS BINARY FIELDS TO 2 OUTPUT CHS          
         LR    R4,R2                                                            
         LR    R5,R3                                                            
         LA    R2,WORK+22                                                       
         LA    R3,2                                                             
VDITBN2X B     EDITBIN                                                          
*                                                                               
EDITBIN  XC    FULL,FULL                                                        
         LA    R6,4                                                             
         SR    R6,R5                                                            
         LA    R6,FULL(R6)                                                      
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R4)                                                    
         CH    R5,=H'1'                                                         
         BH    EDBIN1                                                           
         EDIT  FULL,(4,WORK+20)                                                 
         B     EDITBINX                                                         
EDBIN1   EDIT  FULL,(4,WORK+20)                                                 
EDITBINX B     VXIT                                                             
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITBIN3 NTR1                      EDITS BINARY FIELDS TO 3 OUTPUT CHS          
         LR    R4,R2                                                            
         LR    R5,R3                                                            
         LA    R2,WORK+21                                                       
         LA    R3,3                                                             
VDITBN3X B     EDITBIN                                                          
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITBIN4 NTR1                      EDITS BINARY FIELDS TO 4 OUTPUT CHS          
         LR    R4,R2                                                            
         LR    R5,R3                                                            
         LA    R2,WORK+20                                                       
         LA    R3,4                                                             
VDITBN4X B     EDITBIN                                                          
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITSTOP CLI   0(R2),C' '          EDITS '.' INTO SPACE OR NULL FIELDS          
         BE    *+12                                                             
         CLI   0(R2),0                                                          
         BNE   *+8                                                              
         MVI   0(R2),C'.'                                                       
VSTOPX   BR    RE                                                               
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                   EDITS '.' INTO SPACE OR NULL                 
*                                  ANALYSIS CODE FIELD                          
VDITACDE DS    0H                                                               
         USING DATTABD,R5                                                       
         TM    FLAG,GETFRM30       HAVE WE ALREADY TAKEN IT FROM '30'           
         BO    VACDEX                                                           
         CLC   DISPALYS,DATELMNT   SEE IF THE ANALYSIS CODE COMES FROM          
         BNE   VDIT3               THE '30' ELEMENT OR THE '2C'                 
         TM    FLAG,GETFRM2C       IS IT COMING FROM THE '2C' ELEMENT?          
         BO    *+12                YES                                          
         OI    FLAG,GETFRM30                                                    
         B     VDIT2                                                            
         TM    FLAG,NO2CELEM       IS THERE A '2C'?                             
         BO    VDIT3               NO!                                          
         TM    FLAG,NOTANALY       IS THIS AN ANALYSIS TYPE REC?                
         BO    VDIT3               NO - SO DO NOT WANT IT TO PRINT              
VDIT2    CLI   0(R2),C' '                                                       
         BE    *+12                                                             
         CLI   0(R2),0                                                          
         BNE   *+8                                                              
VDIT3    MVI   0(R2),C'.'                                                       
VACDEX   BR    RE                                                               
         SPACE 1                                                                
VDITSTAT NTR1                                                                   
         LA    RF,STSTATAB         EDITS STATUS ELEMENT STATUS BYTE TO          
         B     EDITMASK            DISPLAY FORMAT USING COMMON EDITMASK         
STSTATAB DC    X'80',C'P'          ROUTINE                                      
         DC    X'40',C'C'                                                       
         DC    X'20',C'L'                                                       
         DC    X'10',C'D'                                                       
         DC    X'08',C'0'                                                       
         DC    X'04',C'E'                                                       
         DC    X'02',C'V'                                                       
         DC    X'00'                                                            
VDITSTAX DS    0H                                                               
         SPACE 1                                                                
*                                                                               
*                                  ***********************************          
VDITCOST NTR1                      PRINTS COST Y/N OR '.' FOR NULL              
*                                                                               
*                                  BOTH COST AND OUTFILE OPTIONS ('80'          
*                                  IN ACSTSTAT) USE THE SAME AREA.  IS          
         SR    R3,R3                                                            
         CLI   WORK+4,C'0'         OUTFILE OPTION IN USE?                       
         BNE   *+14                                                             
         MVC   WORK(1),WORK+4      SET UP TO REPRINT THE OUTFILE                
         B     VDITCOSX            YES - DO NOT OVERWRITE WITH COST             
         LA    RF,COSTTAB                                                       
         B     EDITMSK2                                                         
*                                                                               
COSTTAB  DC    X'02',C'Y'                                                       
         DC    X'01',C'N'                                                       
         DC    X'00'                                                            
VDITCOSX DS    0H                                                               
         LA    R3,1(R3)                                                         
         LA    R2,WORK                                                          
         B     VXIT                                                             
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITNAME L     RF,ANAM             GIVES CORRECTED NAME LENGTH                  
         ZIC   R3,1(RF)                                                         
         SH    R3,=H'2'                                                         
VDITNAMX BR    RE                                                               
         SPACE 1                                                                
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITCNME L     RF,ATRS             GIVES CORRECTED CONTRA NAME LENGTH           
         ZIC   R3,1(RF)                                                         
         SH    R3,=H'17'                                                        
VDITCNMX BR    RE                                                               
         SPACE 1                                                                
*                                  ************************************         
         SPACE 1                                                                
VDITCOMP NTR1                      EDIT COMPANY CODE TO HEX                     
         GOTO1 VHEXOUT,DMCB,(R2),WORK,1,=C'MIX'                                 
         LA    R2,WORK                                                          
         LA    R3,2                                                             
         B     VXIT                                                             
         SPACE 1                                                                
*                                  ************************************         
         SPACE 3                                                                
VXIT     XIT1  REGS=(R2,R3)                                                     
         EJECT                                                                  
*              SUBSIDIARY DATAMANAGER ROUTINES                                  
*              (NOT USED FOR MAINSTREAM READING)                                
*              ON ENTRY KEYB = KEY                                              
*              ON EXIT  IOB  = ACCOUNT FILE RECORD                              
*                       CC = EQU IF ERROR                                       
*              ERROR MESSAGE FOR RECORD NOT FOUND IS ASSUMED TO BE SET          
         SPACE 1                                                                
READ     NTR1  BASE=ABASE,LABEL=N                                               
         L     R8,A2NDBASE                                                      
         LA    R3,=C'DMREAD'                                                    
         B     ALLDM                                                            
         SPACE 1                                                                
READD    NTR1  BASE=ABASE,LABEL=N                                               
         L     R8,A2NDBASE                                                      
         LA    R3,=C'DMREAD'                                                    
         O     R3,=X'08000000'     READ DELETES                                 
         B     ALLDM                                                            
         SPACE 1                                                                
HIGH     NTR1  BASE=ABASE,LABEL=N                                               
         L     R8,A2NDBASE                                                      
         LA    R3,=C'DMRDHI'                                                    
         SPACE 1                                                                
ALLDM    ST    R3,DMCB                                                          
         GOTO1 VDATAMGR,DMCB,,=C'ACCOUNT',KEYB,ADRIOB                           
         TM    DMCB+8,X'FD'                                                     
         BZ    DMXIT                                                            
         SPACE 1                                                                
DMERR    TM    DMCB+8,X'10'                                                     
         BO    *+8                                                              
         MVI   ERROR,0                                                          
         SR    RB,RB                                                            
         SPACE 1                                                                
DMXIT    LTR   RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
*              GET HIGH LEVEL FILTERS - SET COMPOSITE FILTER                    
         SPACE 1                                                                
         EJECT                                                                  
         GETEL R4,49,ELCODE                                                     
ELCODE   DS    C                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
USRLIMIT DS    0F                                                               
*&&UK*&& DC    F'150'                                                           
*&&US*&& DC    F'999'                                                           
         SPACE 3                                                                
RECADDS  DS    0F                  RELOCATED OVERLAY PHASE ADDRESSES            
RECKEY   DS    A                   KEY TABLE                                    
RECFILT  DS    A                   FILTER TABLE                                 
RECPREHD DS    A                   PRE-HEADING CONSTANTS                        
RECPREDT DS    A                   PRE-HEADING TABLE                            
RECHEAD  DS    A                   HEADINGS                                     
RECFIELD DS    A                   DISPLAY DATA TABLE                           
RECKNTRY DS    A                   OPTIONAL NTRY POINT FOR KEY EDITING          
RECFNTRY DS    A                   DITTO                   FILTER               
RECDNTRY DS    A                   DITTO                   DATA                 
RECANY1  DS    A                   SPARE                                        
RECANY2  DS    A                   SPARE                                        
RECADDX  DS    0F                                                               
         SPACE 3                                                                
ACOMMON  DS    0F                  COMMON EDIT S/R'S                            
         DC    A(VDITCHAR)                                                      
         DC    A(VDITDATE)                                                      
         DC    A(VDITSTOP)                                                      
         DC    A(VDITSTAT)                                                      
         DC    A(VDITBIN2)                                                      
         DC    A(VDITNAME)                                                      
         DC    A(VDITBIN3)                                                      
         DC    A(VDITCOMP)                                                      
         DC    A(GETUNLE)                                                       
         DC    A(READ)                                                          
         DC    A(HIGH)                                                          
         DC    A(RECLIST)                                                       
         DC    A(ELLIST)                                                        
         DC    A(VDITCNME)                                                      
         DC    A(VDITACDE)                                                      
         DC    A(VDITCOST)                                                      
         DC    A(VDITBIN4)                                                      
         DS    5CL4                SPARE                                        
         EJECT                                                                  
RECLIST  DS    0CL14               LIST OF RECORD TYPES                         
*                                  COVERED BY DSECT RECTABD                     
         DC    C'AC',X'0601',CL10'ACCOUNT'                                      
         DC    C'AN',X'0200',CL10'ANALYSIS'                                     
         DC    C'CO',X'03F0',CL10'COMPANY'                                      
*&&US*&& DC    C'DI',X'0E01',CL10'DISTRIBUTN'                                   
         DC    C'GE',X'0B01',CL10'GEN LEDGER'                                   
         DC    C'LE',X'0500',CL10'LEDGER'                                       
         DC    C'LI',X'0F00',CL10'LIST'                                         
         DC    C'ME',X'0100',CL10'MEDIA'                                        
         DC    C'OF',X'1200',CL10'OFFICE'                                       
         DC    C'OL',X'1300',CL10'OFFICE LST'                                   
         DC    C'NA',X'0C03',CL10'NARRATION'                                    
         DC    C'PR',X'0701',CL10'PRODUCTION'                                   
*&&US*&& DC    C'RE',X'1141',CL10'RETAIL'                                       
         DC    C'RL',X'0D01',CL10'RULE LIST'                                    
         DC    C'RU',X'0A01',CL10'RULES'                                        
*&&US*&& DC    C'TX',X'1000',CL10'TAX RATES'                                    
         DC    C'UN',X'0400',CL10'UNIT'                                         
         DC    C'WO',X'0200',CL10'WORKCODE'                                     
         DC    C'XP',X'0801',CL10'EXTRA PROF'                                   
*        DC    C'RS',X'1400',CL10'RS LIST'         OBSOLETE                     
*&&US*&& DC    C'MI',X'1500',CL10'MEDIA INTR'                                   
         DC    X'00'                                                            
         SPACE 3                                                                
ELLIST   DS    0C                  LIST OF ELEMENT CODES - SEE AELTAB           
         DC    X'10'                                                            
         DC    X'11'                                                            
         DC    X'12'                                                            
         DC    X'14'                                                            
         DC    X'15'                                                            
         DC    X'16'                                                            
         DC    X'20'                                                            
         DC    X'21'                                                            
         DC    X'24'                                                            
         DC    X'30'                                                            
         DC    X'3C'                                                            
         DC    X'3E'                                                            
         DC    X'42'                                                            
         DC    X'25'                                                            
         DC    X'3D'                                                            
         DC    X'54'                                                            
         DC    X'62'                                                            
         DC    X'26'                                                            
         DC    X'1E'                                                            
         DC    X'1F'                                                            
         DC    X'5F'                                                            
         DC    X'43'                                                            
         DC    X'32'                                                            
         DC    X'19'                                                            
         DC    X'2C'                                                            
         DC    X'00'                                                            
         EJECT                                                                  
*              SET UP ELEMENT ADDRESS TABLE IN GLOBAL W/S                       
*              ON ENTRY P2 = A(IO OR IOB)                                       
         SPACE 1                                                                
SETELAD  NMOD1 0,**SETELA                                                       
         L     RC,0(R1)                                                         
         XC    AELTAB(AELTBLEN),AELTAB                                          
         L     R3,4(R1)                                                         
         ST    R3,AKEY                                                          
         LA    R3,ACRECORD-ACKEYD(R3)                                           
SE1      CLI   0(R3),0                                                          
         BE    SE4                                                              
         SR    R5,R5                                                            
SE2      L     R4,AELLIST                                                       
         AR    R4,R5                                                            
         CLI   0(R4),0                                                          
         BE    SE3                                                              
         CLC   0(1,R4),0(R3)                                                    
         BE    *+12                                                             
         LA    R5,1(R5)                                                         
         B     SE2                                                              
         SLL   R5,2                                                             
         L     R7,AELTAB(R5)                                                    
         LTR   R7,R7                                                            
         BNZ   *+8                                                              
         ST    R3,AELTAB(R5)                                                    
SE3      IC    R5,1(R3)                                                         
         AR    R3,R5                                                            
         B     SE1                                                              
SE4      MVI   SECURITY,0          DERIVE SECURITY NUMBER                       
         ICM   R1,15,ASTA                                                       
         BZ    SE6                                                              
         USING ACSTATD,R1                                                       
         LH    R3,LEVEL                                                         
         LA    R4,SAVESECY-1(R3)                                                
         MVC   1(1,R4),ACSTSECY+1  SAVE SECURITY AT THIS LEVEL                  
         LTR   R3,R3                                                            
         BZ    SE5                                                              
         CLC   0(1,R4),1(R4)       CHECK HIGHER LEVEL                           
         BNH   *+10                                                             
         MVC   1(1,R4),0(R4)       IF GREATER SAVE IT                           
SE5      MVC   SECURITY,1(R4)                                                   
SE6      MVC   OFFICE,SPACES                                                    
         MVC   THISOFF,SPACES                                                   
         L     R3,AKEY                                                          
         CLC   1(2,R3),PRODUNIT                                                 
         BE    SE8                                                              
*&&US                                                                           
         CLI   1(R3),C'T'                                                       
         BE    SE8                                                              
*&&                                                                             
         CLI   OFFPOS,12                                                        
         BH    SE7                                                              
         SR    R1,R1                                                            
         ICM   R1,1,OFFPOS                                                      
         BZ    SETELADX            NO OFFICE POSITION                           
         LA    R1,2(R1,R3)                                                      
         MVC   THISOFF(1),0(R1)                                                 
         B     SE10                                                             
*                                                                               
SE7      TM    OFFPOS,X'F0'        OFFICE CODE IN STATUS ELEMENT                
         BNO   SETELADX                                                         
         ICM   R1,15,ASTA                                                       
         LA    RF,ACSTFILT                                                      
         CLI   OFFPOS,C'1'                                                      
         BE    SE7A                                                             
         LA    RF,ACSTFILT+1                                                    
         CLI   OFFPOS,C'2'                                                      
         BE    SE7A                                                             
         LA    RF,ACSTANAL                                                      
         CLI   OFFPOS,C'3'                                                      
         BE    SE7A                                                             
         LA    RF,ACSTSUB                                                       
SE7A     MVC   THISOFF(1),0(RF)                                                 
         OC    THISOFF,SPACES                                                   
         B     SE10                                                             
SE8      ICM   R1,15,APRO                                                       
         BZ    SE10                                                             
         MVC   THISOFF,ACPROFFC-ACPROFD(R1)                                     
         DROP  R1                                                               
SE10     LH    R1,LEVEL                                                         
         LR    R0,R1               SAVE LEVEL IN R0                             
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,OFFICES(R1)                                                   
         MVC   0(2,R1),THISOFF     SAVE THIS LEVEL OFFICE CODE                  
         CLI   OFFPOS,1            TEST FOR OFFICE IN KEY                       
         BL    SE12                                                             
         CLI   OFFPOS,12                                                        
         BNH   SE14                YES-SKIP COMPOSITE OFFICE                    
*                                                                               
SE12     CLC   0(2,R1),SPACES      TEST FOR OFFICE VALUE                        
         BH    SE14                YES                                          
         SH    R1,=H'2'            NO-BACK UP ONE LEVEL                         
         BCT   R0,SE12                                                          
         B     SETELADX                                                         
*                                                                               
SE14     MVC   OFFICE,0(R1)        SET COMPOSITE OFFICE                         
*                                                                               
SETELADX XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SETFILT  NMOD1 0,**SETFLT                                                       
         L     RC,0(R1)                                                         
         NI    FLAG,X'FF'-NO2CELEM                                              
         NI    FLAG,X'FF'-NOTANALY                                              
         NI    FLAG,X'FF'-GETFRM2C                                              
         L     R5,ATIA                                                          
         USING TWA1D,R5                                                         
         MVC   COMFILT,SPACES                                                   
         CLI   LEVEL+1,1                                                        
         BNE   *+10                                                             
         MVC   LEV1FILT(20),SPACES     AT HIGH LEVEL SET ALL TO SPACES          
         ICM   R1,15,ASTA                                                       
         BZ    SETFILTX                                                         
         LH    R0,=Y(ASTA-GWS)     SAVE THE DISPL TO THE '30' ELEM              
         STH   R0,DISPALYS         BUT MAY OVERWRITE WITH '2C' DISPL            
         LH    R0,=Y(APOS-GWS)                                                  
         STH   R0,DISP2C           SAVE THE DISPL TO THE '2C' ELEM              
         LH    R3,LEVEL                                                         
         LTR   R3,R3                                                            
         BZ    SETFILTX                                                         
         BCTR  R3,0                                                             
         MH    R3,=Y(L'LEV1FILT)   GET TO FILTER FIELDS FOR THIS LEVEL          
         LA    R7,LEV1FILT(R3)                                                  
         USING ACSTATD,R1                                                       
         MVC   0(2,R7),ACSTFILT    FILTER 1 AND 2                               
         MVC   2(1,R7),ACSTANAL    FILTER 3                                     
         MVC   3(1,R7),ACSTSUB                                                  
         CLI   ACSTLEN,ACSTLNQ2                                                 
         BL    *+10                                                             
         MVC   4(1,R7),ACSTFLT5    FILTER 5                                     
*                                                                               
         CLI   ACSTCOST,C' '       DOES COSTING GROUP = SPACES?                 
         BE    SETFILT2            YES - SO MUST GET FROM '2C' ELEM             
         CLI   ACSTCOST,0          DOES COSTING GROUP = ZERO?                   
         BNE   SETFLT2B            NO - SO MUST GET FROM '30' ELEM              
*                                                                               
*  OVERWRITE THE DISPLACEMENT TO THE '30' ELEM WITH '2C' DISPL.                 
SETFILT2 OI    FLAG,GETFRM2C       GET FROM '2C' ELEMENT                        
         ICM   R1,15,APOS          DOES THE '2C' ELEM EXIST?                    
         BNZ   *+12                YES                                          
         OI    FLAG,NO2CELEM                                                    
         B     SETFLT2A                                                         
         USING ACSPECD,R1                                                       
         CLI   ACSPTYP,ACSPOAN     IS THIS AN ANLYSIS TYPE REC (X'03')          
         BE    *+8                 YES                                          
         OI    FLAG,NOTANALY                                                    
SETFLT2A LH    R0,=Y(APOS-GWS)                                                  
         STH   R0,DISPALYS                                                      
*                                                                               
SETFLT2B LH    R3,LEVEL                                                         
         LA    R1,LEV1FILT         NOW BUILD COMPOSITE FROM ALL LEVELS          
SETFILT3 LA    R0,5                PRESENT SO FAR                               
         LA    R4,COMFILT                                                       
SETFILT5 CLI   0(R1),X'41'                                                      
         BL    *+10                DON'T REPLACE BLANKS                         
         MVC   0(1,R4),0(R1)                                                    
         LA    R4,1(R4)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,SETFILT5         4 FILTERS FOR EACH LEVEL                     
         BCT   R3,SETFILT3         NUMBER OF LEVELS                             
SETFILTX XMOD1                                                                  
*                                                                               
         EJECT                                                                  
*        CHECK OFFICE SECURITY                                                  
CHEKOFF  NMOD1 0,**CHKOFF                                                       
         L     RC,0(R1)                                                         
         CLC   TERMACCS,SPACES                                                  
         BNH   OFFOKX              NO ACCESS CONTROL                            
         L     R4,AKEY                                                          
*&&US                                                                           
         TM    COMPSTA3,X'02'      DPS-TYPE COMPANY                             
         BZ    CHEK01                                                           
         CLI   TERMACCS,C'T'       IS THERE A TALENT LIMIT ACCESS               
         BNE   CHEK01                                                           
         CLI   1(R4),C'T'          IF NOT DPS TALENT COMMERCIAL UNIT-OK         
         BNE   OFFOKX                                                           
         CLI   2(R4),C' '          IF THERE'S A LEDGER PRESENT                  
         BE    OFFOKX                                                           
         CLC   TERMACCS+1(1),2(R4) INSURE IT'S THE CORRECT ONE                  
         BNE   OFFBADX                                                          
         B     OFFOKX                                                           
*&&                                                                             
*                                                                               
CHEK01   TM    OFFPOS,X'F0'        TEST OFFICE IN FILTER LEDGER                 
         BNO   CHEK02              NO                                           
         OC    ABAL,ABAL           TEST FOR LOWEST LEVEL                        
         BNZ   CHEK02              YES-ALWAYS DO CHECK                          
         CLC   OFFICE,SPACES       AT HIGHER LEVEL-TEST FOR OFFICE              
         BE    OFFOKX              NONE-GIVE ACCESS                             
*                                                                               
CHEK02   CLI   TERMACCS,C'*'       TEST SINGLE OFFICE FILTER                    
         BE    CHEK04                                                           
         CLI   TERMACCS,C'$'       TEST FOR OFFICE LIST FILTER                  
         BE    CHEK04                                                           
         CLC   TERMACCO,SPACES     TEST FOR ANY OLD FILTER                      
         BNH   CHEK04              NO                                           
         CLC   TERMACCO,3(R4)                                                   
         BE    OFFOKX                                                           
         B     OFFBADX                                                          
*                                                                               
CHEK04   L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAREC,AKEY                                                     
         MVC   OFFAOPOS,OFFPOS                                                  
         MVC   OFFAOFFC,OFFICE                                                  
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BE    OFFOKX                                                           
         B     OFFBADX                                                          
         DROP  R1                                                               
*                                                                               
OFFOKX   SR    R1,R1                                                            
         B     *+6                                                              
OFFBADX  LTR   R1,R1                                                            
         XMOD1                                                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
         SPACE 2                                                                
         SPACE 2                                                                
       ++INCLUDE FATWA                                                          
         SPACE 2                                                                
       ++INCLUDE FAFACTS                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054ACINF00   09/30/04'                                      
         END                                                                    
