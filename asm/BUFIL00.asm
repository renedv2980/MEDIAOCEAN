*          DATA SET BUFIL00    AT LEVEL 025 AS OF 05/01/02                      
*PHASE T50200A                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE MONVAL                                                                 
*INCLUDE BURNEM                                                                 
*INCLUDE BUPPER                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T50200 - BUDGET CONTROL LFM - BASE '                            
T50200   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**T50200,RA,RR=R2,CLEAR=YES                              
         USING GEND,RC                                                          
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
*                                                                               
FIL      ST    R2,RELO                                                          
         LR    R8,RC               R8=A(SPOOLD)                                 
         LA    RC,SPOOLEND         RC=A(GENCON STORAGE)                         
         ST    R8,ASPOOLD                                                       
         LA    R9,IO               START OF IO 1 + TOTAL IO AREA LEN            
         AH    R9,=Y(LENIOAS)      R9=A(BUDGET SYSTEM WORKNG STORAGE)           
         ST    R9,ASYSD                                                         
         ST    RD,AWORK            SAVE BASE'S RD                               
*                                                                               
         ST    R1,SYSPARMS                                                      
         L     R0,0(R1)                                                         
         ST    R0,ATIOB                                                         
         L     R7,4(R1)                                                         
         ST    R7,ATWA             R7=A(TWA)                                    
         USING CONHEADH-64,R7                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RA,SYSRA                                                         
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
*                                                                               
         BAS   RE,SETRD            SET RD SO GENCON WILL RETURN                 
*                                                                               
FIL1     BAS   RE,MOVE             CHECK FOR OUTLINE MOVES                      
         BNE   *+12                NO                                           
         MVI   CLSAVSW,NO          PRESERVE SAVE KEY TABLE                      
         B     FIL4                SKIP GENCON CALL                             
*                                                                               
         GOTO1 =A(LIST),DMCB,(RC),RR=RELO CHECK SWAP BETWEEN LISTS              
         GOTO1 =A(ENT),DMCB,(RC),RR=RELO CHECK SWAP TO/FROM ENTRY               
         GOTO1 =A(SUBL),DMCB,(RC),RR=RELO CHECK SWAP FROM SUB LISTS             
*                                                                               
FIL2     GOTO1 VGENCON,DMCB,(R8)    CALL GENCON                                 
*                                                                               
FIL4     CLI   CLMOVESW,YES        TEST FOR CLEARING MOVE AREA                  
         BNE   *+10                                                             
         XC    MOVEVALS,MOVEVALS                                                
         CLI   CLSAVSW,YES         TEST FOR CLEARING SAVED KEYS                 
         BNE   FIL6                                                             
         XC    SAVKEYL,SAVKEYL                                                  
         XC    SAVKEYS,SAVKEYS                                                  
*                                                                               
FIL6     CLI   TWASCR,X'F8'        TEST FOR TEXT MAINT SCREEN                   
         BNE   EXIT                                                             
         ICM   R2,15,AFRSTREC      YES-R2=A(RECORD FIELD HEADER)                
         BZ    EXIT                                                             
         NI    1(R2),X'FF'-X'01'   UNDO FUDGETWA SO MODIFIED BITS               
         BAS   RE,BUMPU            WILL BE CORRECT                              
         BNE   *-8                                                              
*                                                                               
EXIT     XIT1                                                                   
         SPACE 2                                                                
SETRD    NTR1                                                                   
         ST    RD,SYSRD            SET RD SO GENCON WILL RETURN CONTROL         
         B     EXIT                                                             
         EJECT                                                                  
* INITIALIZE SYSTEM DEPENDENT VALUES                                            
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
*                                  GET TERMINAL VALUES                          
         MVI   DDS,NO                                                           
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,YES                                                          
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAORIG                                                   
         MVC   AGENCY,TWAAGY                                                    
         MVC   XTRA,SPACES                                                      
         MVI   CLSAVSW,YES         SET SWITCH TO CLEAR SAVED KEYS               
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         MVC   MODFRST,TIOBFRST    EXTRACT TIOB VALUES                          
         MVC   MODLAST,TIOBLAST                                                 
         MVC   CURDISP,TIOBCURD                                                 
         ZIC   RE,TIOBAID          GET PF KEY INPUT THIS TIME                   
         LA    RF,12                                                            
         CR    RE,RF                                                            
         BNH   *+6                                                              
         SR    RE,RF               ADJUST FOR PF13-PF24                         
         STC   RE,THISPF                                                        
*                                                                               
SYS1     LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
*                                                                               
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
*                                                                               
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
*                                                                               
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
*                                                                               
SYS6     LA    R2,ADTAB            SET ADCONS FOR EXTENDED ADDRESSING           
         LA    R0,NADCONS          COUNTER                                      
         LA    R3,EXTADS           POINT TO FIRST EXTENDED ADCON                
*                                                                               
SYS7     ICM   R1,7,0(R2)          GET DISPLACEMENT                             
         LA    R1,SYSD(R1)         INDEX INTO STORAGE                           
         ST    R1,0(R3)                                                         
         LA    R2,L'ADTAB(R2)      NEXT TABLE ENTRY                             
         LA    R3,4(R3)            NEXT WORKING STORAGE FIELD                   
         BCT   R0,SYS7                                                          
*                                  SET AREA FOR NODIO                           
SYS8     L     RF,ANODBLK                                                       
         SH    RF,=H'8'                                                         
         MVC   0(8,RF),=C'*NODBLK*'                                             
         SPACE 1                                                                
* SET SYSTEM DEPENDENT VALUES                                                   
*                                                                               
SYS10    LA    R1,TWA1SAVE         USE TWA1 FOR SAVE STORAGE TO SUPPORT         
         ST    R1,ASTARTSV         MOVE                                         
         MVI   NTWA,X'81'                                                       
*                                                                               
         MVI   SYSTEM,C'B'         BUDGET                                       
         MVI   GETMSYS,22          USES GETMSG FOR SYSTEM 22                    
*                                                                               
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2000 BYTES                       
         OI    GENSTAT1,USKYMRG    TRANSFER KEY FIELDS TO LIST SCREENS          
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,VGETAGY     ROUTINE TO GET USER NAME AND ADDRESS         
*                                                                               
         MVC   LKEY,=Y(L'BUKEY)    DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=Y(L'BUKCTL)                                             
         MVC   DATADISP,=Y(BUFRSTEL-BUKEY)                                      
         MVC   SYSFIL,=C'BUDFIL  '                                              
         MVC   SYSDIR,=C'BUDDIR  '                                              
         MVC   REQFILE,=C'MPLREQ  '                                             
*                                                                               
         MVC   LWORK,=AL4(LENWORK)      SET WORK AREA LENGTH                    
         MVC   RCPROG(2),=C'MP'         PREFIX FOR REPORT NO.                   
         MVC   SYSPHASE,=X'D9050200'    PRESET FOR SYSTEM CALLOVS               
         L     R1,=A(RECACTS)           RECORD/ACTION DIRECTORY                 
         A     R1,RELO                                                          
         ST    R1,ARECACT                                                       
*                                                                               
         SPACE 1                                                                
* SET UP CERTAIN ROUTINE ADDRESSES-  (CANT WAIT FOR GENCON)                     
*                                                                               
SYS12    L     R1,SYSPARMS                                                      
         LM    R2,R4,8(R1)         A(SYSLIST) A(TIA) A(COMFACS)                 
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         L     RE,=V(RECUP)        RELOCATE RECUP                               
         A     RE,RELO                                                          
         ST    RE,VRECUP                                                        
*                                                                               
SYS14    L     RF,CALLOV           RF=V(CALLOV)                                 
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
         LA    R2,CORETAB          R2=A(CORE RESIDENT PHASE NUMBERS)            
         LA    R3,CORES            R3=LOOP COUNTER                              
         LA    R4,VGENCON          R4=A(ADCON LIST)                             
SYS15    MVC   DMCB+7(1),0(R2)                                                  
         CLI   0(R2),0             TEST FOR A HOLE IN THE LIST                  
         BE    SYS17               DON'T LOAD IT !                              
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R4),0(R1)       EXTRACT MODULE ADDRESS                       
SYS17    LA    R2,1(R2)            NEXT PHASE NUMBER                            
         LA    R4,4(R4)            NEXT ADCON                                   
         BCT   R3,SYS15                                                         
* FOR STAPACK CONVERSION, REPLACE MSPACK/MSUNPK ADDRESSES                       
         L     R0,=A(GOMSPACK)                                                  
         A     R0,RELO                                                          
         ST    R0,VMSPACK                                                       
         L     R0,=A(GOMSUNPK)                                                  
         A     R0,RELO                                                          
         ST    R0,VMSUNPK                                                       
         B     XIT                                                              
         DROP  R1,R4                                                            
         EJECT                                                                  
*=============================================================*                 
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
*=============================================================*                 
         SPACE 1                                                                
GOMSPACK NTR1  BASE=SYSRB,WORK=(R4,8),LABEL=*                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     RA,SYSRA            2ND BASE REG - R9 MUST BE SET FIRST          
         L     R7,ATWA                                                          
         L     R8,ASPOOLD                                                       
         ST    RD,COMMRD                                                        
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF7                                                
         MVC   STAPMED,SVQRMED                                                  
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R5)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,8(R5)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
GOMSUNPK NTR1  BASE=SYSRB,WORK=(R4,8),LABEL=*                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     RA,SYSRA            2ND BASE REG - R9 MUST BE SET FIRST          
         L     R7,ATWA                                                          
         L     R8,ASPOOLD                                                       
         ST    RD,COMMRD                                                        
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         CLC   =C'DF',STAPAGY                                                   
         BNE   *+10                                                             
         MVC   STAPAGY,=C'TH'                                                   
         MVC   STAPCTRY,SVAPROF7                                                
         MVC   STAPMED,SVQRMED                                                  
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
*                                                                               
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,4(R5)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R5)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R5),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                                
         SPACE 1                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB,LABEL=*                                               
         L     R7,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD            RESTORE POINTER TO SYSTEM STORAGE            
         L     RA,SYSRA            2ND BASE REG - R9 MUST BE SET FIRST          
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     GETAGY                                                           
         B     GETFLD                                                           
         B     CLEARF                                                           
         B     FVAL                                                             
         B     NODERR                                                           
         B     ACTIV               ADDACTV                                      
         B     ACTIV1              CHAACTV                                      
         B     SETADD                                                           
         B     GETVAL                                                           
         B     SETKEY                                                           
         B     CURSERR                                                          
         B     VALCLT                                                           
         B     VALPRD                                                           
         B     VALPLAN                                                          
         B     FINDOUT                                                          
         B     TRACE                                                            
         B     PEROUT                                                           
         B     GOUPKEY                                                          
         B     GETKEYF                                                          
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
* GETAGY - GET AGENCY DATA FROM CONTROL FILE USER ID RECORD                     
*                                                                               
GETAGY   CLI   SVUSRNAM,C' '       DO ONE TIME ONLY                             
         BNH   GETAGY1                                                          
         MVC   USERNAME,SVUSRNAM   GET SAVED AGENCY NAME/ADDRESS                
         MVC   USERADDR,SVUSRADD                                                
         LA    RE,SYSD                                                          
         AH    RE,=Y(SVSIGNON-SYSD)                                             
         MVC   SIGNON,0(RE)        AND SIGNON                                   
         B     GETAGYX             EXIT                                         
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
GETAGY1  MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),USERID                                               
         GOTO1 READ                                                             
*                                                                               
         MVC   SIGNON,SPACES                                                    
         GOTO1 HELLO,PARAS,(C'G',FILENAME),('CTDSCELQ',AIO),0                   
         CLI   12(R1),0                                                         
         BNE   GETAGY2                                                          
         L     R6,12(R1)                                                        
         USING CTDSCD,R6                                                        
         MVC   SIGNON,CTDSC        GET SIGNON STRING FROM DESC EL               
*                                                                               
GETAGY2  LA    RE,SYSD                                                          
         AH    RE,=Y(SVSIGNON-SYSD)                                             
         MVC   0(L'SVSIGNON,RE),SIGNON                                          
         MVI   SVUSRNAM,C'*'                                                    
         GOTO1 HELLO,PARAS,(C'G',FILENAME),(X'36',AIO),0                        
         CLI   12(R1),0                                                         
         BNE   GETAGY4                                                          
         L     R6,12(R1)                                                        
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSRNAM,USERNAME                                                
         MVC   SVUSRADD,USERADDR                                                
         SPACE 1                                                                
GETAGY4  XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         BAS   RE,INITNOD                                                       
*                                                                               
GETAGYX  B     XIT                                                              
         EJECT                                                                  
* GETFLD - EXTRACT DATA FROM SCREEN FIELD                                       
*                                                                               
* ON ENTRY                                                                      
*        P1 BYTE  0   = X'FF' ZERO FILL EXTRACTED DATA                          
*           BYTES 1-3 = A(FIELD HEADER)                                         
*                                                                               
* ON EXIT                                                                       
*        FADDR = A(FIELD HEADER)                                                
*        FLDH  CONTAINS FIELD HEADER                                            
*        FLD   CONTAINS EXTRACTED FIELD DATA SPACE FILLED                       
*        R0    CONTAINS BINARY VALUE OF DATA IF FIELD IS NUMERIC                
*                                                                               
GETFLD   L     R2,0(R1)            R2=A(FIELD HEADER)                           
         CLI   0(R1),X'FF'         TEST FOR ZERO FILLING FLD                    
         BNE   *+8                                                              
         MVI   FZERO,YES           YES                                          
         LA    R2,0(R2)                                                         
         ST    R2,FADDR                                                         
         LA    RF,8(R2)            RF=A(FIELD START)                            
         STCM  RF,7,FLAST                                                       
         MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
*                                                                               
         MVC   FLDH,0(R2)                                                       
         XC    FLDXH,FLDXH         CLEAR EXTENDED FIELD HEADER                  
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL WITH SPACES                             
         SR    R0,R0               PRE-CLEAR REGISTER FOR NUMERIC VALUE         
*                                                                               
         ZIC   R1,FLDH                                                          
         SH    R1,=H'9'                                                         
         TM    FLDH+1,X'02'        TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         SH    R1,=H'8'            SUBTRACT ANOTHER 8 FOR FIELD LENGTH          
         EX    R1,FLDMOVE                                                       
         LA    RE,FLD(R1)          POINT RE AT LAST EXTRACTED BYTE              
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
*                                                                               
         TM    FLDH+1,X'02'        TEST FOR EXTENDED HEADER                     
         BZ    GETFLD1             NO                                           
         LA    RF,8(R1,R2)         RF=A(EXTENDED FIELD HEADER)                  
         MVC   FLDXH,0(RF)         EXTRACT EXTENDED FIELD HEADER                
         SPACE 1                                                                
GETFLD1  CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLD2                                                          
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,GETFLD1                                                       
         SPACE 1                                                                
GETFLD2  STC   R1,FLDH+5           SET ACTUAL DATA LENGTH                       
         LTR   R1,R1               TEST FOR EMPTY FIELD                         
         BZ    GETFLDX                                                          
*                                                                               
         CLI   FZERO,YES           TEST FOR ZERO FILLED DATA                    
         BNE   GETFLD3                                                          
         LA    RF,L'FLD                                                         
         SR    RF,R1               RF=L'REST OF FIELD                           
         LA    RE,FLD(R1)          RE=POINTER TO REST OF FIELD                  
         BCTR  RF,0                                                             
         EX    RF,*+8              ZERO FILL REST OF FIELD                      
         B     GETFLD3                                                          
         XC    0(0,RE),0(RE)                                                    
         SPACE 1                                                                
GETFLD3  TM    FLDH+4,X'08'        TEST FOR NUMERIC FIELD                       
         BZ    GETFLDX                                                          
         CLI   FLDH+5,15           NO MORE THAN 15 DIGITS                       
         BH    GETFLD4                                                          
         BCTR  R1,0                                                             
         EX    R1,FLDPACK                                                       
         CP    DUB,=P'2147000000'  TEST FOR FULLWORD MAX                        
         BH    GETFLD4             TREAT IT AS NON-NUMERIC                      
         CVB   R0,DUB                                                           
         B     GETFLDX                                                          
         SPACE 1                                                                
GETFLD4  NI    FLDH+4,X'FF'-X'08'  TURN OFF NUMERIC BIT                         
         SPACE 1                                                                
GETFLDX  MVI   FZERO,NO            TURN OFF ZERO FILL SWITCH                    
         XIT1  REGS=(R0)                                                        
         SPACE 1                                                                
FLDMOVE  MVC   FLD(0),8(R2)                                                     
FLDPACK  PACK  DUB,FLD(0)                                                       
         EJECT                                                                  
* CLEARF - CLEAR AND FOUT FIELDS                                                
*                                                                               
* ON ENTRY                                                                      
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS                                 
*                        = 1 PROTECTED FIELDS                                   
*              BYTES 1-3 = A(START FIELD HEADER)                                
*        P2    BYTES 1-3 = A(END FIELD HEADER)                                  
*                                                                               
CLEARF   LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    R5,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    R5,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
         SPACE 1                                                                
CLEARF2  IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,TSTBRAN          BRANCH ACCORDINGLY                           
         LR    R1,RE                                                            
         SH    R1,=H'9'            SET EXECUTE LENGTH FOR FIELD DATA            
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         SH    R1,=H'8'            LESS 8 MORE FOR EXTENDED FIELD               
         EX    R1,0(R5)            CLEAR FIELD                                  
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         SPACE 1                                                                
CLEARF4  LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    CLEARF2             NO-CONTINUE                                  
         B     XIT                 YES-ALL DONE                                 
         SPACE 2                                                                
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
TSTBRAN  BC    0,CLEARF4                                                        
         EJECT                                                                  
*                                                                               
* FVAL - FIELD VALIDATION AND SCANNING ROUTINE                                  
*                                                                               
* ON ENTRY                                                                      
*        FADDR = A(FIELD HEADER)                                                
*        FMAX  = MAXIMUM SCAN LENGTH (OPTIONALLY SET BY USER)                   
*        FLAST = A(LAST STRING) SET BY FVAL                                     
*              = ZERO (FORCES EDIT TO START AT FADDR+8)                         
*        FLEN  = LENGTH OF LAST STRING - SET BY FVAL                            
*        FTERM = LIST OF UP TO 6 SCAN TERMINATORS ENDED BY X'00'                
*        FZERO = Y TO RETURN ZERO FILLED OUTPUT IN FLD                          
*        FREDIT= Y TO FORCE RE-EDIT OF FIELD                                    
*                                                                               
* ON EXIT                                                                       
*        FLDH  = FIELD HEADER BUILT BY FVAL - CONTAINS DATA LENGTH              
*                AND VALIDITY BITS                                              
*        FLD   = EXTRACTED DATA STRING IN SPACE FILLED FIELD                    
*        FSTOP = STOP CHARACTER FOUND BY FVAL OR X'FF' FOR NO MORE DATA         
*        DUB   = CONTAINS PACKED VALUE OF DATA STRING FOR NUMERIC FIELD         
*                                                                               
FVAL     XC    FLDH,FLDH           CLEAR OUTPUT FIELD HEADER                    
         XC    FLDXH,FLDXH         CLEAR EXTENDED FIELD HEADER                  
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL OUTPUT FIELD WITH SPACES                
         MVI   FLDH,L'FLDH+L'FLD   SET DUMMY HEADER LENGTH                      
         MVI   FSTOP,X'FF'         SET FSTOP TO NO DATA FOUND                   
         L     R2,FADDR                                                         
         ZIC   R3,0(R2)                                                         
         SH    R3,=H'8'            R3 CONTAINS FIELD DATA LENGTH                
         TM    1(R2),X'02'         TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         SH    R3,=H'8'                                                         
         LA    R2,8(R2)            POINT TO DATA START                          
         OC    FLAST,FLAST         TEST FOR LAST STRING                         
         BNZ   FVAL2                                                            
*                                                                               
* CODE BELOW TO FVAL8 ASSUMES -                                                 
* R1 POINTS TO SCAN INPUT  R2 POINTS TO SCREEN FIELD START                      
* R3 CONTAINS BYTE REMAINING TO BE SCANNED                                      
*                                                                               
FVAL1    STCM  R2,7,FLAST          SAVE SCAN START POINT                        
         LR    R1,R2               SET R1 AS SCAN INPUT POINTER                 
         MVI   FLEN,0              CLEAR LAST LENGTH TO BE SAFE                 
         MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         B     FVAL4                                                            
*                                                                               
FVAL2    SR    R1,R1                                                            
         ICM   R1,7,FLAST          SET R1 TO POINT TO SCAN START                
         ZIC   R0,FLEN             LENGTH OF LAST STRING                        
         AR    R1,R0               POINT TO LAST STOP CHARACTER                 
         CLI   FREDIT,YES          TEST IF RE-EDITING                           
         BE    *+8                 YES-NO NEED TO JUMP OVER STOP CHAR.          
         LA    R1,1(R1)            INCREMENT LENGTH FOR STOP CHARACTER          
         LR    R0,R1               START POINT FOR SCAN                         
         SR    R0,R2               BYTES ALREADY SCANNED                        
         SR    R3,R0               BYTES LEFT TO SCAN                           
         BNP   FVALX               NOTHING TO SCAN - EXIT                       
         CLI   FMAX,0              TEST FOR USER SCAN LIMIT                     
         BE    *+8                 NO                                           
         IC    R3,FMAX             YES-USE IT IN PLACE OF DERIVED LIMIT         
         STCM  R1,7,FLAST          SAVE SCAN START                              
         MVI   FLEN,0              CLEAR LAST LENGTH                            
*                                                                               
FVAL4    LA    R0,L'FTERM                                                       
         LA    RE,FTERM            POINT AT SCAN TERMINATORS                    
         SPACE 1                                                                
FVAL5    CLI   0(RE),X'00'         TEST FOR END-OF-LIST                         
         BE    FVAL6                                                            
         CLC   0(1,R1),0(RE)       TEST FOR TERMINATOR                          
         BE    FVAL7               FOUND ONE                                    
         LA    RE,1(RE)            NEXT LIST ENTRY                              
         BCT   R0,FVAL5                                                         
*                                                                               
FVAL6    LA    R1,1(R1)            NEXT DATA BYTE                               
         BCT   R3,FVAL4                                                         
         B     FVAL8               SEARCH WAS FRUITLESS                         
*                                                                               
FVAL7    MVC   FSTOP,0(R1)         SET STOP CHARACTER                           
*                                                                               
FVAL8    LR    R3,R1               COMPUTE DATA LENGTH                          
         SR    RE,RE                                                            
         ICM   RE,7,FLAST                                                       
         SR    R3,RE                                                            
         BZ    FVALX               ONLY FOUND A TERMINATOR                      
         STC   R3,FLEN                                                          
         STC   R3,FLDH+5                                                        
         BCTR  R3,0                SET TO EXECUTE                               
         EX    R3,*+8              EXTRACT DATA STRING                          
         B     FVAL10                                                           
         MVC   FLD(0),0(RE)                                                     
*                                                                               
FVAL10   LA    RE,FLD(R3)          ADJUST LENGTH FOR TRAILING BLANKS            
         LA    R3,1(R3)            COUNTER                                      
         LR    R0,R3                                                            
*                                                                               
FVAL11   CLI   0(RE),0             TEST FOR TRAILING ZERO                       
         BNE   *+8                                                              
         MVI   0(RE),C' '          CHANGE IT TO A BLANK                         
         CLI   0(RE),C' '          TEST FOR A BLANK                             
         BNE   *+10                NO                                           
         BCTR  RE,0                YES-BACK UP FIELD POINTER                    
         BCT   R0,FVAL11                                                        
         STC   R0,FLDH+5                                                        
         LTR   R0,R0               TEST FOR ZERO REAL LENGTH                    
         BZ    FVALX               EXIT FOR EMPTY FIELD                         
*                                                                               
         LR    R3,R0               SET REAL DATA LENGTH                         
         LA    RE,FLD              POINT TO START OF DATA                       
         MVI   FLDH+4,X'0C'        VALID NUMERIC AND ALPHA DATA                 
*                                                                               
FVAL12   CLI   0(RE),C'A'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'I'                                                       
         BNH   FVAL14              VALID ALPHA                                  
         CLI   0(RE),C'J'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'R'                                                       
         BNH   FVAL14              VALID ALPHA                                  
         CLI   0(RE),C'S'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'Z'                                                       
         BNH   FVAL14              VALID ALPHA                                  
*                                                                               
FVAL13   NI    FLDH+4,X'FF'-X'04'  NOT ALPHA                                    
*                                                                               
FVAL14   CLI   0(RE),C'0'          TEST IF NUMERIC                              
         BL    *+12                                                             
         CLI   0(RE),C'9'                                                       
         BNH   FVAL15                                                           
         NI    FLDH+4,X'FF'-X'08'  NOT NUMERIC                                  
*                                                                               
FVAL15   LA    RE,1(RE)            NEXT BYTE IN DATA STRING                     
         BCT   R3,FVAL12                                                        
*                                                                               
         CLI   FZERO,YES           TEST FOR ZERO FILLED DATA                    
         BNE   FVAL16                                                           
         LA    RE,FLD                                                           
         LA    RF,L'FLD                                                         
         SR    RF,R0                                                            
         BZ    FVAL16                                                           
         AR    RE,R0               RE POINTS IMMEDIATELY AFTER DATA             
         BCTR  RF,0                                                             
         EX    RF,*+8              ZERO REMAINDER OF FIELD                      
         B     FVAL16                                                           
         XC    0(0,RE),0(RE)                                                    
*                                                                               
FVAL16   TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BZ    FVALX                                                            
         CLI   FLDH+5,15                                                        
         BNH   *+12                                                             
         NI    FLDH+4,X'FF'-X'08'                                               
         B     FVALX                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     FVALX                                                            
         PACK  DUB,FLD(0)          EXECUTED                                     
*                                                                               
FVALX    MVI   FMAX,0              ALWAYS CLEARED BY FVAL                       
         MVI   FZERO,NO            TURN OFF ZERO FILL SWITCH                    
         MVI   FREDIT,NO                                                        
         B     XIT                                                              
         EJECT                                                                  
* NODERR - TRANSLATE NODIO ERROR MESSAGE TO SYSTEM ERROR MESSAGE                
*                                                                               
* AT ENTRY - R2 POINTS TO ERROR FIELD                                           
*                                                                               
NODERR   L     R5,ANODBLK                                                       
         USING NODBLKD,R5                                                       
         ZIC   RF,NDERR                                                         
         SH    RF,=Y(NDERRFST)     RF HAS INDEX INTO TABLE                      
         IC    R0,NODERRTB(RF)                                                  
         STC   R0,ERROR                                                         
         B     TRAPERR                                                          
         DROP  R5                                                               
         SPACE 2                                                                
NODERRTB DS    0X                                                               
         DC    AL1(LEVERR)         NDLEVERR                                     
         DC    AL1(0)              NDCDLERR                                     
         DC    AL1(NOTFOUND)       NDRNFERR                                     
         DC    AL1(DUPLICAT)       NDADDERR                                     
         DC    AL1(0)              NDRENERR                                     
         DC    AL1(POSERR)         NDPMDERR                                     
         DC    AL1(TOOLONG)        NDOVFERR                                     
         DC    AL1(RESERR)         NDRESERR                                     
         DC    AL1(0)                                                           
         DC    AL1(0)              NDLIBERR                                     
         DC    AL1(0)              NDANFERR                                     
         DC    AL1(0)              NDAMXERR                                     
         DS    0H                                                               
         EJECT                                                                  
* ACTIV - ROUTINE TO MAINTAIN ACTIVITY ELEMENTS                                 
*                                                                               
* ENTRY POINT FOR ACTION ADD                                                    
*                                                                               
ACTIV    LA    R6,ELEMENT                                                       
         USING ACTVD,R6                                                         
         MVI   ELCODE,X'F1'                                                     
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,BTODAY     SET DATE                                     
         MVC   ACTVCHDT,BTODAY                                                  
         LA    R3,ACTVADID                                                      
         B     ACTIV6                                                           
         SPACE 1                                                                
* ENTRY POINT FOR CHAACTIV - ACTION CHANGE                                      
*                                                                               
ACTIV1   LA    R6,ELEMENT                                                       
         USING ACTVD,R6                                                         
         MVI   ELCODE,X'F1'                                                     
         GOTO1 REMELEM                                                          
         CLI   ELEMENT,0                                                        
         BNE   ACTIV2                                                           
         MVI   ACTVEL,X'F1'        REPAIRING IF NO ELEMENT YET                  
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,BTODAY     SET DATE                                     
         MVC   ACTVADID,TWAORIG    AND ID                                       
         MVC   ACTVCHDT,BTODAY                                                  
*                                                                               
ACTIV2   LA    R3,ACTVCHID                                                      
         CLC   ACTVCHDT,BTODAY     WAS RECORD CHANGED TODAY                     
         BNE   ACTIV3                                                           
         CLI   ACTVCHNM,0          (ADDED TODAY)                                
         BNE   ACTIV4                                                           
         SPACE 1                                                                
ACTIV3   MVC   ACTVCHDT,BTODAY     NO SO SET TODAYS DATE                        
         AI    ACTVCHNM,1          UP THE CHANGE NUMBER                         
         MVC   ACTVCHRE,CHREASON   AND MOVE IN THE REASON                       
         B     ACTIV6                                                           
*                                                                               
ACTIV4   OC    ACTVCHRE,CHREASON                                                
*                                                                               
ACTIV6   MVC   0(2,R3),TWAORIG     MOVE IN ID                                   
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'40'       IS PASSWORD PROTECT ACTIVE                   
         BZ    ACTIV8                                                           
         MVC   0(2,R3),FAPASSWD    YES SO USE THIS ID                           
         OI    2(R3),X'80'                                                      
         DROP  R1                                                               
*                                                                               
ACTIV8   GOTO1 ADDELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
* SETADD - SET UP TASK DEPENDENT ADDRESSES EACH TIME                            
*                                                                               
SETADD   L     R5,ANODBLK                                                       
         USING NODBLKD,R5                                                       
         MVC   NDDMGR,DATAMGR                                                   
         MVC   NDRECUP,VRECUP                                                   
         MVC   NDIOA,AIO1          NORMALLY USE 1ST AND 2ND IOAREAS             
         MVC   NDIOA2,AIO2                                                      
         MVI   NDUPDTSW,0          CLEAR READ FOR UPDATE SWITCH                 
         MVI   NDDELRSW,0                                                       
         MVI   NDSUBRSW,0          CLEAR PROCESS SUB-RECORD SWITCH              
         XC    NDHOOK,NDHOOK       CLEAR HOOK ADDRESS                           
         B     XIT                                                              
         EJECT                                                                  
* GETVAL - EXTRACT COMMON VALUES FROM CLIENT, PRODUCT AND PLAN RECS             
*                                                                               
*          AT ENTRY, NDIOA POINTS TO RECORD                                     
*                                                                               
GETVAL   L     R5,ANODBLK                                                       
         USING NODBLKD,R5                                                       
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCCLT     TEST FOR CLIENT RECORD                       
         BO    GETVAL2                                                          
         TM    BURCTYP,BUKCPRO     TEST FOR PRODUCT RECORD                      
         BO    GETVAL4                                                          
         TM    BURCTYP,BUKCPLN     TEST FOR PLAN RECORD                         
         BO    GETVAL6                                                          
         TM    BURCTYP,BUKCOUT     TEST FOR OUTLINE RECORD                      
         BO    GETVAL8                                                          
         B     XIT                                                              
*                                                                               
GETVAL2  MVI   ELCODE,BUCLTELQ     CLIENT RECORD                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUCLTD,R6                                                        
         MVC   CLTCODE,BUKCODE                                                  
         MVC   CLTNAM,BUCLTNAM                                                  
         MVC   CLTFIS,BUCLTFIS                                                  
         B     XIT                                                              
*                                                                               
GETVAL4  MVI   ELCODE,BUPROELQ     PRODUCT RECORD                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPROD,R6                                                        
         MVC   PRDCODE,BUKCODE                                                  
         MVC   PRDNAM,BUPRONAM                                                  
         B     XIT                                                              
*                                                                               
GETVAL6  MVI   ELCODE,BUPLNELQ     PLAN RECORD                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPLND,R6                                                        
         MVC   PLANCODE,BUKCODE                                                 
         MVC   PLANNAM,BUPLNNAM                                                 
         MVC   PLANST,BUPLNST      PLAN START/END (YM)                          
         MVC   PLANEND,BUPLNEND                                                 
         MVC   PLANCNT,BUPLNCNT    OUTLINE RECORD COUNT                         
         MVC   PLANLOW,BUPLNLOW                                                 
         MVC   PLANIND,BUPLNIND    PLAN INDICATORS                              
         B     XIT                                                              
*                                                                               
GETVAL8  MVI   ELCODE,BUOUTELQ     OUTLINE RECORD                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUOUTD,R6                                                        
         MVC   OUTCODE,BUKCODE                                                  
         MVC   OUTNAME,BUOUTNAM                                                 
         ZIC   R1,NDLEV                                                         
         SH    R1,=H'3'            SUBTRACT CLIENT/PRODUCT/PLAN                 
         STC   R1,OUTLEV                                                        
         MVC   OUTBEF,BUOUTBEF                                                  
         MVC   OUTAFT,BUOUTAFT                                                  
         MVC   OUTIND,BUOUTIND                                                  
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* SETKEY - SET NODAL KEY FOR CLIENT/PRODUCT/PLAN                                
*                                                                               
* ON EXIT, NODKEY AND NODKEYSV CONTAIN NODAL KEY                                
*                                                                               
SETKEY   XC    NODKEY,NODKEY                                                    
         LA    R2,NODKEY           R2=STRING POINTER                            
         LR    R0,R2               R0=A(STRING START)                           
         MVC   0(L'CLTCODE,R2),CLTCODE                                          
         LA    R2,L'CLTCODE+1(R2)                                               
         MVC   0(L'PRDCODE,R2),PRDCODE                                          
         LA    R2,L'PRDCODE+1(R2)                                               
         MVC   0(L'PLANCODE,R2),PLANCODE                                        
         LA    R2,L'PLANCODE(R2)                                                
         SR    R2,R0               R2=L'STRING                                  
         OC    NODKEY,SPACES       SPACE FILL STRING AREA                       
         GOTO1 SQUASHER,DMCB,NODKEY,(NDDELIM,(R2))                              
*                                                                               
SETKEYX  MVC   NODKEYSV,NODKEY     SAVE COPY OF KEY JUST BUILT                  
         B     XIT                                                              
         EJECT                                                                  
* CURSERR - SET CURSOR TO ERROR POSITION AND EXIT TO ERROR ROUTINE              
*                                                                               
* AT ENTRY, P1 = 0  FADDR=A(ERROR FIELD HEADER), FLAST=A(CURSOR)                
*                1  SAME AS FOR 0 EXCEPT EXITS DIRECTLY TO USER                 
*           ERROR=SUPPLIED FOR USER MESSAGE, SPACE PADDED TEXT AT               
*           WORK.                                                               
*           FNDX=OPTIONAL FIELD INDEX NUMBER TO BE ATTACHED TO MSG              
*           XTRA=OPTIONAL EXTRA TEXT TO BE ATTACHED TO SYSTEM MSG               
*                                                                               
CURSERR  MVC   BYTE,3(R1)          SAVE P1                                      
         L     R2,FADDR                                                         
         OI    6(R2),X'C0'         XMIT ERROR FIELD HEADER AND CURSOR           
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BE    CURSERR2            YES-DO NOT HAVE TIOB                         
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         LR    RF,R2               COMPUTE DISPLACEMENT OF ERROR FLDH           
         S     RF,ATWA             FROM TWA START                               
         STCM  RF,3,TIOBCURD                                                    
         LR    RF,R2                                                            
         SR    RE,RE                                                            
         ICM   RE,7,FLAST          RE=A(CURSOR POSITION)                        
         LA    RF,8(RF)            RF=A(FIELD START)                            
         SR    RE,RF               COMPUTE INDEX INTO FIELD FOR CURSOR          
         STC   RE,TIOBCURI                                                      
         OI    TIOBINDS,TIOBSETC                                                
*                                                                               
CURSERR2 CLI   ERROR,SUPPLIED      TEST FOR USER SUPPLIED TEXT                  
         BNE   *+14                NO                                           
         MVC   CONHEAD,WORK        YES-GET THE TEXT FROM WORK                   
         B     CURSERR4                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,GETMSYS          ERROR MESSAGE SYSTEM-BUDGET                  
         CLI   ERROR,60            TEST FOR BUDGET SYSTEM ERROR                 
         BH    *+8                 YES                                          
         LA    R0,GENERSYS         NO-USE GENCON ERROR SYSTEM                   
         GOTO1 GETMSG,DMCB+12,(ERROR,CONHEAD),(X'FF',DMCB),((R0),0)             
*                                                                               
CURSERR4 LA    R4,CONHEAD+L'CONHEAD-1 R4 POINTS TO END OF MSG FLD               
         LA    R3,L'CONHEAD        CALCULATE MESSAGE LENGTH                     
         CLI   0(R4),C' '          TEST FOR BLANK                               
         BNE   *+10                                                             
         BCTR  R4,0                BACK UP POINTER                              
         BCT   R3,*-10                                                          
         LA    R4,1(R4)            POINT TO BLANK AFTER LAST CHAR               
*                                                                               
CURSERR6 CLI   FNDX,0              TEST FOR INDEX NUMBER                        
         BE    CURSERR8                                                         
         LA    R0,L'CONHEAD                                                     
         LA    R3,8(R3)            ADD ON LENGTH OF INDEX MSG +1                
         CR    R3,R0               TEST FOR FIT IN FIELD                        
         BH    CURSERRX            NO - EXIT                                    
         LA    R4,CONHEAD-7(R3)                                                 
         MVC   0(7,R4),=C'- FLD#N'                                              
         EDIT  (B1,FNDX),(1,6(R4))                                              
         LA    R4,7(R4)            POINT TO BLANK AFTER INDEX MSG               
*                                                                               
CURSERR8 CLC   XTRA,SPACES         TEST FOR ANY EXTRA MESSAGE                   
         BE    CURSERRX                                                         
         LA    RE,XTRA+L'XTRA-1                                                 
         LA    R1,L'XTRA           CALCULATE LENGTH OF EXTRA MESSAGE            
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
         LA    R0,L'CONHEAD                                                     
         LA    R3,1(R1,R3)         COMPUTE TOTAL MESSAGE LENGTH                 
         CR    R3,R0               TEST FOR FIT                                 
         BH    CURSERRX                                                         
         BCTR  R1,0                LESS ONE FOR EXECUTE                         
         EX    R1,*+8              MOVE XTRA TO MESSAGE FIELD                   
         B     CURSERRX                                                         
         MVC   1(0,R4),XTRA        EXECUTED                                     
*                                                                               
CURSERRX CLI   ACTNUM,ACTCHA       TEST IF ERROR OCCURRED                       
         BNE   *+16                ON A CHANGE AFTER A SELECT                   
         CLI   TWALACT,ACTSEL                                                   
         BNE   *+8                                                              
         MVI   ACTNUM,ACTSEL       KEEP SELECT AS LAST ACTION                   
*                                                                               
         CLI   BYTE,1              TEST FOR DIRECT EXIT                         
         BNE   *+12                YES                                          
         L     RD,AWORK            GET BASE'S RD                                
         B     EXIT                                                             
*                                                                               
         GOTO1 ERREX2                                                           
         CLI   GCMODE,C'S'         TEST IN SLAVED MODE                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    CONHEADH+6,X'80'    XMIT MESSAGE BACK                            
         GOTO1 SAVEUWKA                                                         
         L     RD,AWORK                                                         
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
* VALCLT - VALIDATE CLIENT AND READ THROUGH NODIO                               
*                                                                               
* AT ENTRY, P1 = A(FIELD HEADER)                                                
*           P2   BYTE 0 = 0 FOR REGULAR EDIT                                    
*                       = D TO SET A DEFAULT VALUE IF NO INPUT                  
*                       = M TO PERMIT BLANK FIELD                               
*             BYTES 1-3 = A(DEFAULT VALUE)                                      
*                                                                               
VALCLT   LM    R2,R3,0(R1)                                                      
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST         EDIT FIELD FROM START                        
         XC    FTERM,FTERM                                                      
         MVI   FZERO,YES           ZERO FILL OUTPUT                             
         GOTO1 VFVAL                                                            
*                                                                               
VALCLT2  CLI   FLDH+5,0                                                         
         BNE   VALCLT4                                                          
         MVI   ERROR,MISSING                                                    
         STCM  R3,8,BYTE           ISOLATE HOB                                  
         CLI   BYTE,C'M'           TEST IF MISSING INPUT ALLOWED                
         BE    VALCLTX                                                          
         CLI   BYTE,C'D'           TEST IF DEFAULT S/B SET                      
         BNE   TRAPERR                                                          
         MVC   FLD(L'CLTCODE),0(R3) SET DEFAULT VALUE IN FIELD                  
         B     VALCLT6                                                          
*                                                                               
VALCLT4  MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,2            TEST MINIMUM LENGTH                          
         BL    TRAPERR                                                          
         CLI   FLDH+5,3                                                         
         BH    TRAPERR                                                          
*                                                                               
VALCLT6  MVC   CLTCODE,FLD         SAVE INPUT CODE                              
         CLC   SIGNON,=CL8'AYJW'                                                
         BNE   *+14                                                             
         CLC   CLTCODE,=C'DBS'     TEST FOR DEBEERS                             
         BNE   TRAPERR             NO-STOP ACCESS FOR JWT                       
*                                                                               
         GOTO1 VSETKEY             SET NODIO KEY                                
         LA    R1,VALCLT8          IN-LINE NODIO HOOK                           
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    VALCLTX                                                          
         GOTO1 VNODERR                                                          
*                                                                               
VALCLTX  B     XIT                                                              
*                                                                               
* NODIO HOOK ROUTINE FOR CLIENT                                                 
*                                                                               
VALCLT8  ST    RE,FULL                                                          
         CLI   NDLEV,0             TEST FOR MASTER RECORD                       
         BER   RE                                                               
         CLI   NDMODE,NDPROC       TEST FOR RIGHT LEVEL                         
         BNER  RE                                                               
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCCLT     TEST FOR CLIENT RECORD                       
         BZR   RE                                                               
         CLI   NDLEV,1             TEST NODIO IS IN SYNC                        
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE                                  
*                                                                               
         GOTO1 VGETVAL             EXTRACT RECORD VALUES                        
*                                                                               
VALCLT8X L     RE,FULL                                                          
         BR    RE                  RETURN TO NODIO                              
         EJECT                                                                  
* VALPRD - VALIDATE PRODUCT CODE AND READ RECORD THROUGH NODIO                  
*                                                                               
* AT ENTRY, P1 = A(FIELD HEADER)                                                
*           P2   BYTE 0 = 0 FOR REGULAR EDIT                                    
*                       = D TO SET A DEFAULT VALUE IF NO INPUT                  
*                       = M TO PERMIT NO INPUT IN FIELD                         
*             BYTES 1-3 = A(DEFAULT VALUE)                                      
*                                                                               
VALPRD   LM    R2,R3,0(R1)                                                      
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST         EDIT FIELD FROM START                        
         XC    FTERM,FTERM                                                      
         MVI   FZERO,YES           ZERO FILL OUTPUT                             
         GOTO1 VFVAL                                                            
*                                                                               
VALPRD2  CLI   FLDH+5,0                                                         
         BNE   VALPRD4                                                          
         MVI   ERROR,MISSING                                                    
         STCM  R3,8,BYTE           ISOLATE HOB                                  
         CLI   BYTE,C'M'           TEST IF MISSING INPUT ALLOWED                
         BE    VALPRDX                                                          
         CLI   BYTE,C'D'           TEST IF DEFAULT S/B SET                      
         BNE   TRAPERR                                                          
         MVC   FLD(L'PRDCODE),0(R3) SET DEFAULT VALUE                           
         B     VALPRD6                                                          
*                                                                               
VALPRD4  MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,2            TEST MINIMUM LENGTH                          
         BL    TRAPERR                                                          
         CLI   FLDH+5,3                                                         
         BH    TRAPERR                                                          
*                                                                               
VALPRD6  MVC   PRDCODE,FLD         SAVE INPUT CODE                              
         GOTO1 VSETKEY                                                          
         LA    R1,VALPRD8          IN-LINE NODIO HOOK                           
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    VALPRDX                                                          
         GOTO1 VNODERR                                                          
*                                                                               
VALPRDX  B     XIT                                                              
*                                                                               
* NODIO HOOK ROUTINE FOR PRODUCT                                                
*                                                                               
VALPRD8  ST    RE,FULL                                                          
         CLI   NDLEV,0             TEST FOR MASTER RECORD                       
         BER   RE                                                               
         CLI   NDMODE,NDPROC       TEST FOR RIGHT LEVEL                         
         BNER  RE                                                               
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         CLI   NDLEV,2             TEST NODIO IS IN SYNC                        
         BNER  RE                                                               
*                                                                               
         GOTO1 VGETVAL             EXTRACT RECORD VALUES                        
*                                                                               
VALPRD8X L     RE,FULL                                                          
         BR    RE                  RETURN TO NODIO                              
         EJECT                                                                  
* VALPLAN - VALIDATE PLAN CODE AND READ RECORD THROUGH NODIO                    
*                                                                               
* AT ENTRY, P1 = A(FIELD HEADER)                                                
*           P2   BYTE 0 = 0 FOR REGULAR EDIT                                    
*                       = D TO SET A DEFAULT VALUE IF NO INPUT                  
*                       = M TO PERMIT NO INPUT IN FIELD                         
*             BYTES 1-3 = A(DEFAULT VALUE)                                      
*                                                                               
VALPLAN  LM    R2,R3,0(R1)                                                      
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FZERO,YES                                                        
         GOTO1 VFVAL                                                            
*                                                                               
VALPLAN2 CLI   FLDH+5,0                                                         
         BNE   VALPLAN4                                                         
         MVI   ERROR,MISSING                                                    
         STCM  R3,8,BYTE                                                        
         CLI   BYTE,C'M'                                                        
         BE    VALPLANX                                                         
         CLI   BYTE,C'D'                                                        
         BNE   TRAPERR                                                          
         MVC   FLD(L'PLANCODE),0(R3)                                            
         B     VALPLAN6                                                         
*                                                                               
VALPLAN4 MVC   PLANCODE,FLD                                                     
*                                                                               
VALPLAN6 GOTO1 VSETKEY                                                          
         LA    R1,VALPLAN8         IN-LINE HOOK                                 
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    VALPLANX                                                         
         GOTO1 VNODERR                                                          
*                                                                               
VALPLANX B     XIT                                                              
*                                                                               
* NODIO HOOK ROUTINE FOR PLAN                                                   
*                                                                               
VALPLAN8 ST    RE,FULL                                                          
         CLI   NDLEV,0             TEST FOR MASTER RECORD                       
         BER   RE                                                               
         CLI   NDMODE,NDPROC       TEST FOR RIGHT LEVEL                         
         BNER  RE                                                               
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCPLN     TEST FOR PLAN RECORD                         
         BZR   RE                                                               
         CLI   NDLEV,3             TEST NODIO IS IN SYNC                        
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE                                  
*                                                                               
         GOTO1 VGETVAL             EXTRACT RECORD VALUES                        
*                                                                               
         L     RE,FULL             RETURN TO NODIO                              
         BR    RE                                                               
         EJECT                                                                  
* FINDOUT - VALIDATE OUTLINE CODE AND READ THE OUTLINE RECORD                   
*                                                                               
* AT ENTRY, P1 BYTE  0   = X'08' TO READ FOR DELETED OUTLINES                   
*                        = X'00' TO IGNORE DELETED OUTLINES                     
*           P1 BYTES 1-3 = A(OUTLINE CODE)                                      
*           P2 = A(IO AREA)                                                     
*                A(0) TO SKIP READ OF RECORD                                    
* ON EXIT, CC=EQ FOR OK, CC=NEQ FOR ERROR AND ERROR SET                         
*                                                                               
FINDOUT  MVI   ERROR,0                                                          
         LM    R2,R3,0(R1)         R2=A(OUTLINE CODE), R3=A(IO AREA)            
         OC    DMINBTS,0(R1)                                                    
         LA    R4,KEY                                                           
         USING BUCRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   BUCSYS,C'B'                                                      
         MVC   BUCAGY,AGENCY                                                    
         MVI   BUCRTYP,BUCRTYPQ                                                 
         MVC   BUCCLT,CLTCODE                                                   
         MVC   BUCPRD,PRDCODE                                                   
         MVC   BUCPLAN,PLANCODE                                                 
         MVC   BUCCODE,0(R2)       EXTRACT OUTLINE CODE                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUCKEY),KEYSAVE TEST FOR HIT ON CODE                       
         BE    FINDOUT2                                                         
         MVI   ERROR,NOTFOUND                                                   
         B     FINDOUTX                                                         
*                                                                               
FINDOUT2 LTR   R3,R3                                                            
         BZ    FINDOUTX                                                         
         MVC   FULL,AIO                                                         
         ST    R3,AIO              SET USER I/O AREA                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING BURECD,R4                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),BUKEY  SET KEY = RECORD KEY                         
         MVC   AIO,FULL            RESTORE IO AREA                              
*                                                                               
FINDOUTX NI    DMINBTS,X'FF'-X'08'                                              
         CLI   ERROR,0             SET CC ON EXIT                               
         B     XIT                                                              
         EJECT                                                                  
* TRACE - DO A NODIO TRACE AND READ OF A RECORD POINTED                         
* TO BY NDIOA.  ON EXIT NODKEY AND NODKEYSV SET TO NODAL KEY                    
*                                                                               
TRACE    XC    NODKEY,NODKEY                                                    
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'TRACE',NODKEY                             
         OC    NODKEY,SPACES                                                    
         MVC   NODKEYSV,NODKEY     SET UP KEY FOR READ                          
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
* PEROUT - DISPLAY PERIOD EXPRESSIONS                                           
*                                                                               
* ON ENTRY, P1 BYTE 0    = INPUT TYPE                                           
*                          0 = YEAR/MONTH                                       
*                          1 = START YEAR/MONTH-END YEAR/MONTH                  
*              BYTES 1-3 = A(INPUT)                                             
*           P2 = A(OUTPUT)                                                      
*                          FOR INPUT TYPE 0, RETURNS 6 BYTES                    
*                          FOR INPUT TYPE 1, RETURNS 13 BYTES                   
*                                                                               
* ROUTINE DESTROYS DUB                                                          
*                                                                               
PEROUT   LM    R2,R3,0(R1)         R2=A(PERIOD), R3=A(OUTPUT)                   
         ZIC   R4,0(R1)            R4=INPUT TYPE                                
         CLI   CLTTYPE,10          TEST FOR 444 FISCAL YEAR                     
         BE    PEROUT4                                                          
*                                                                               
PEROUT1  LTR   R4,R4               TEST FOR YM INPUT                            
         BZ    *+12                                                             
         CLI   0(R2),0             TEST FOR YEAR MISSING                        
         BE    PEROUT2             YES-MONTH TO MONTH EXPRESSION                
*                                                                               
         MVC   DUB(2),0(R2)        GET YEAR/MONTH                               
         MVI   DUB+2,X'01'                                                      
         GOTO1 DATCON,PARAS,(3,DUB),(6,(R3))                                    
*&&UK*&& MVI   3(R3),SLASH                                                      
         LTR   R4,R4               TEST FOR Y/M INPUT                           
         BZ    PEROUTX                                                          
         MVC   6(7,R3),SPACES      CLEAR END MONTH POSITION                     
         CLC   0(2,R2),2(R2)       TEST START Y/M=END Y/M                       
         BE    PEROUTX                                                          
         MVI   6(R3),DASH                                                       
         MVC   DUB(2),2(R2)        GET END Y/M                                  
         GOTO1 DATCON,PARAS,(3,DUB),(6,7(R3))                                   
*&&UK*&& MVI   10(R3),SLASH        UK COMPATIBILITY                             
         B     PEROUTX                                                          
*                                                                               
PEROUT2  MVC   0(13,R3),SPACES     MMM-MMM OUTPUT                               
         ZIC   R1,1(R2)                                                         
         MH    R1,=Y(L'MONTAB)                                                  
         LA    R1,MONTAB-L'MONTAB(R1) INDEX INTO TABLE                          
         MVC   0(3,R3),0(R1)                                                    
         CLC   1(1,R2),3(R2)       TEST START MONTH=END MONTH                   
         BE    PEROUTX             YES-ALL DONE                                 
         MVI   3(R3),DASH                                                       
         ZIC   R1,3(R2)                                                         
         MH    R1,=Y(L'MONTAB)                                                  
         LA    R1,MONTAB-L'MONTAB(R1)                                           
         MVC   4(3,R3),0(R1)                                                    
         B     PEROUTX                                                          
*                                                                               
PEROUT4  ZIC   R1,1(R2)            GET MONTH                                    
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(R3),C'P'                                                       
         UNPK  1(2,R3),DUB+6(2)                                                 
         LTR   R4,R4               TEST FOR INPUT TYPE 0                        
         BZ    *+12                YES                                          
         CLI   0(R2),0             TEST YEAR MISSING                            
         BE    PEROUT8             YES-HAVE MONTH-MONTH EXPRESSION              
*                                                                               
         MVI   3(R3),SLASH                                                      
         ZIC   R1,0(R2)            GET YEAR                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(2,R3),DUB+6(2)                                                 
*                                                                               
PEROUT5  LTR   R4,R4               TEST FOR YEAR/MONTH INPUT                    
         BZ    PEROUTX                                                          
         MVC   6(7,R3),SPACES      CLEAR END MONTH POSITION                     
         CLC   0(2,R2),2(R2)       TEST START Y/M=END Y/M                       
         BE    PEROUTX             YES                                          
*                                                                               
PEROUT6  ZIC   R1,3(R2)            GET END MONTH                                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   6(R3),DASH                                                       
         MVI   7(R3),C'P'                                                       
         UNPK  8(2,R3),DUB+6(2)                                                 
         MVI   10(R3),SLASH                                                     
         ZIC   R1,2(R2)            GET END YEAR                                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  11(2,R3),DUB+6(2)                                                
         B     PEROUTX                                                          
*                                                                               
PEROUT8  MVC   3(10,R3),SPACES     PERIOD-PERIOD EXPRESSION                     
         CLC   1(1,R2),3(R2)                                                    
         BE    PEROUTX                                                          
         MVI   3(R3),DASH                                                       
         ZIC   R1,3(R2)            GET END PERIOD                               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   4(R3),C'P'                                                       
         UNPK  5(2,R3),DUB+6(2)                                                 
         B     PEROUTX                                                          
*                                                                               
PEROUTX  B     XIT                                                              
         EJECT                                                                  
* GOUPKEY - CALL UPKEY SUB-ROUTINE TO MANAGE UPDATE OF SAVED                    
*           KEY FIELD TABLE                                                     
*                                                                               
GOUPKEY  CLI   OFFLINE,YES         TEST OFFLINE                                 
         BE    XIT                 NO NEED FOR THIS                             
         GOTO1 =A(UPKEY),DMCB,(RC),RR=RELO                                      
         B     XIT                                                              
         EJECT                                                                  
* GETKEYF  SUB-ROUTINE TO GET DATA FOR A KEY FIELD AND PLACE IT INTO            
*          AN ONGOING STRING                                                    
*                                                                               
* AT ENTRY, ELCODE=KEY FIELD NUMBER AND R4=A(OUTPUT)                            
* ON EXIT, R4=CURRENT POSITION OF OUTPUT STRING                                 
*                                                                               
GETKEYF  GOTO1 HELLO,DMCB,(C'G',=C'CORETAB'),(ELCODE,SAVKEYL),0                 
         CLI   12(R1),0                                                         
         BNE   GETKEYFX                                                         
         L     RE,12(R1)                                                        
         ZIC   R1,1(RE)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),2(RE)                                                    
         LA    R4,1(R1,R4)                                                      
*                                                                               
GETKEYFX XIT1  REGS=(R4)                                                        
         EJECT                                                                  
* INITNOD - INITIALIZE NODIO BLOCK AND READ MASTER RECORD                       
*                                                                               
INITNOD  NTR1                                                                   
         L     R5,ANODBLK                                                       
         USING NODBLKD,R5                                                       
*                                                                               
         LR    RE,R5                                                            
         LA    RF,LENODBLK                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR THE NODIO BLOCK                        
*                                                                               
         MVC   NDKLEN,LKEY                                                      
         MVC   NDELSTRT,DATADISP                                                
         MVC   NDRMAX,SIZEIO+2                                                  
         MVC   NDCNTL,=Y(BUKCTL-BUKEY)                                          
         MVC   NDDISK,=Y(BUKDA-BUKEY)                                           
         MVC   NDRLEN,=Y(BURLEN-BUKEY)                                          
*                                                                               
         MVC   NDDIRNAM,SYSDIR                                                  
         MVC   NDFILNAM,SYSFIL                                                  
         MVI   NDDELIM,C'.'                                                     
         MVI   NDMXLEVS,MAXLEVEL                                                
*                                                                               
         MVC   NDDMGR,DATAMGR                                                   
         MVC   NDRECUP,VRECUP                                                   
*                                                                               
         LA    R4,NDKEY            SET UP MASTER RECORD KEY                     
         USING BURECD,R4                                                        
         MVI   BUKSYS,C'B'                                                      
         MVC   BUKAGY,AGENCY                                                    
         MVI   BUKRTYP,BUKRTYPQ                                                 
*                                                                               
         ZIC   R1,CONRECH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,AGYCOMP                                                       
         BNE   INITNOD2                                                         
         ZIC   R1,CONACTH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,ADDCOMP                                                       
         BE    INITNODX                                                         
*                                                                               
INITNOD2 MVC   NDIOA2,AIO1         READ MASTER RECORD INTO AIO2                 
         MVC   NDIOA,AIO2                                                       
         MVC   NODKEY,SPACES                                                    
         GOTO1 VNODIO,DMCB,ANODBLK,=C'READ',NODKEY                              
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   NDIOA,AIO1          RESET TO READ INTO AIO1                      
         MVC   NDIOA2,AIO2                                                      
*                                                                               
INITNODX B     XIT                                                              
*                                                                               
AGYCOMP  CLC   CONREC(0),=C'AGENCY'                                             
ADDCOMP  CLC   CONACT(0),=C'ADD'                                                
         EJECT                                                                  
* SUB-ROUTINE TO MANAGE MOVES ON OUTLINE LIST SCREEN                            
*                                                                               
* ON EXIT, CC=EQ TO SKIP GENCON CALL, CC=NEQ TO MAKE GENCON CALL                
*                                                                               
MOVE     NTR1                                                                   
         MVI   CLMOVESW,YES        SET TO CLEAR MOVEMENT DETAILS                
         CLI   TWASCR,X'E5'        TEST IF OUTLINE LIST SCR DISPLAYED           
         BNE   MOVEN                                                            
         CLC   CONREC(7),=C'OUTLINE'                                            
         BNE   MOVEN                                                            
         CLC   CONACT(4),=C'LIST'                                               
         BE    MOVE2                                                            
         CLC   CONACT(5),=C'RESET'                                              
         BNE   MOVEN                                                            
*                                  RESET PROCESSING                             
MOVE1    XC    MOVEVALS,MOVEVALS   CLEAR OUT MOVE DETAILS                       
         MVC   CONACT,SPACES                                                    
         MVC   CONACT(4),=C'LIST' CHANGE ACTION FIELD TO 'LIST'                 
         OI    CONACTH+6,X'C0'     XMIT AND SET CURSOR                          
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'RESETMSG),RESETMSG                                     
         OI    CONHEADH+6,X'80'                                                 
         B     MOVEE                                                            
*                                                                               
MOVE2    OI    CONACTH+6,X'81'     XMIT AND ACTION MODIFIED NEXT TIME           
         XC    CONHEAD,CONHEAD     CLEAR MESSAGE HEADER                         
         OI    CONHEADH+6,X'80'    XMIT MESSAGE HEADER                          
         L     R5,ANODBLK          ESTABLISH NODIO BLOCK ADDRESSABILITY         
         USING NODBLKD,R5                                                       
         BAS   RE,EDLIST           CHECK THE SCREEN                             
         MVI   CLMOVESW,NO         SET TO KEEP MOVEMENT DETAILS                 
         CLI   LSTOTH,YES          TEST FOR REGULAR SELECT INPUT                
         BE    MOVEN               YES                                          
         OC    FROMVALS,FROMVALS   TEST IF MOVE 'FROM' PROVIDED                 
         BNZ   MOVE4                                                            
         OC    TOVALS,TOVALS       TEST IF MOVE'TO' GIVEN                       
         BNZ   MOVE3               YES                                          
         MVI   CLMOVESW,YES        SET TO CLEAR MOVEMENT DETAILS                
         B     MOVEN               AND CALL GENCON                              
*                                  HAVE MOVE 'TO'                               
MOVE3    OC    FROMVALS,FROMVALS   TEST IF MOVE 'FROM' SET                      
         BNZ   MOVE6               YES-SO DO MOVE                               
         CLI   LSTTON,0            TEST IF 'TO' INPUT THIS TIME                 
         BE    MOVEN               NO-CONTINUE LIST WITH GENCON                 
         MVC   CONHEAD(L'FPENDMSG),FPENDMSG                                     
         LA    R2,OULSELH                                                       
         B     MOVE8                                                            
*                                  HAVE MOVE 'FROM' DETAILS                     
MOVE4    OC    TOVALS,TOVALS       TEST IF 'TO' VALUES PRESENT                  
         BNZ   MOVE6               YES-GO AHEAD WITH MOVE                       
         CLI   LSTFROMN,0          TEST IF 'FROM' INPUT THIS TIME               
         BE    MOVEN               NO-CONTINUE WITH LIST                        
         MVC   CONHEAD(L'TPENDMSG),TPENDMSG                                     
         LA    R2,OULSELH                                                       
         B     MOVE8                                                            
*                                                                               
MOVE6    BAS   RE,PUTSAVE          PUT SAVE AREA SO GENCON WILL RESTORE         
         MVI   GCMODE,C'S'         CALL GENCON IN SLAVED MODE                   
         OI    GENSTAT2,USMYOK     I'LL SET MY OWN MESSAGES                     
         GOTO1 VGENCON,DMCB,(R8)                                                
*                                                                               
         CLC   FROMKEY,TOPARENT    TEST TRYING TO MOVE A PARENT                 
         BNE   MOVE7               DOWN TO LEVEL OF ITS CHILDREN                
         XC    MOVEVALS,MOVEVALS   CLEAR OUT MOVE VALUES                        
         MVI   ERROR,POSERR                                                     
         ICM   R2,15,LSTATO        PUT CURSOR AT MOVE TO FLDH                   
         BNZ   *+8                 UNLESS I DO NOT HAVE IT                      
         ICM   R2,15,LSTAFROM      WHEN THE FROM FIELD WILL DO                  
         B     EDLISTR                                                          
*                                                                               
MOVE7    BAS   RE,REPO             RE-POSITION OUTLINE                          
         XC    MOVEVALS,MOVEVALS                                                
         MVC   CONHEAD(L'MOVEMSG),MOVEMSG                                       
*                                                                               
* CALL OUTLINE LIST OVERLAY TO REBUILD LIST SCREEN STARTING                     
* FROM MOVED OUTLINE'S NEW POSITION                                             
*                                                                               
         GOTO1 VCLEARF,DMCB,OULSELH,OULPFH                                      
         GOTO1 (RF),DMCB,(1,OULSELH),OULPFH                                     
         MVI   OVERLAY,X'15'       OUTLINE LIST OVERLAY                         
         MVC   DMCB,VDUMMY         LOAD IN LIST OVERLAY AFTER BASE              
         MVC   DMCB+4(4),SYSPHASE                                               
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             GET OVERLAY ADDRESS                          
         MVI   MODE,LISTRECS                                                    
         OI    WHEN,X'80'          FUDGE VALUES SO LISTMON WORKS                
         MVI   LISTNUM,0                                                        
         XC    LISTDIR(108),LISTDIR                                             
         LA    R2,OULSELH                                                       
         BAS   RE,BUMP                                                          
         ST    R2,ATHISLST                                                      
         GOTO1 (RF),(R1),(RC)                                                   
         BAS   RE,PUTSAVE          ATTEND TO SAVE STORAGE                       
         LA    R2,CONACTH          SET CURSOR POSITION                          
*                                                                               
MOVE8    OI    CONHEADH+6,X'80'                                                 
         OI    6(R2),X'40'         SET CURSOR POSITION                          
         CLI   GCMODE,C'S'         TEST MOVE JUST PERFORMED                     
         BE    *+8                 YES-SELECT FIELDS ALREADY DONE               
         BAS   RE,ADJFLD           ADJUST SELECT FIELDS                         
         TM    OULCODH+4,X'80'     TEST START OUTLINE INPUT THIS TIME           
         BZ    *+8                 NO                                           
         OI    OULCODH+6,X'81'     XMIT BACK MODIFIED TO MAKE                   
         B     MOVEE               GENCON PICK UP CHANGE                        
*                                                                               
MOVEE    CR    RB,RB               SET CC=EQ TO SKIP GENCON CALL                
         B     MOVEX                                                            
*                                                                               
MOVEN    XC    LSTVALS,LSTVALS     CLEAR OVERLAY STORAGE                        
         LTR   RB,RB               SET CC=NEQ TO FORCE GENCON CALL              
*                                                                               
MOVEX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PRE-EDIT LIST SCREEN BEFORE GENCON CALL                        
*                                                                               
EDLIST   NTR1                                                                   
         LA    R2,OULSELH          R2=SELECT FIELD POINTER                      
         LA    R3,1                R3=FIELD NUMBER                              
*                                                                               
EDLIST2  GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BE    EDLIST15                                                         
*                                                                               
         CLI   FLD,STAR            TEST IF FIELD STARTS WITH STAR               
         BNE   EDLIST4                                                          
         ZIC   R1,FLDH                                                          
         SH    R1,=H'9'                                                         
         TM    FLDH+1,X'02'        TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      CLEAR SELECT FIELD                           
         OI    6(R2),X'80'         XMIT                                         
         MVI   5(R2),0                                                          
         B     EDLIST15            NEXT FIELD                                   
*                                                                               
EDLIST4  CLI   FLDH+5,1            TEST FOR 1 BYTE INPUT                        
         BNE   EDLIST10            NO-OTHER TYPE OF INPUT                       
*                                                                               
         CLI   FLD,C'M'            TEST FOR MOVE IDENTIFIER                     
         BNE   EDLIST6             NO                                           
*                                                                               
         MVI   ERROR,DUPFRERR                                                   
         OC    FROMKEY,FROMKEY     TEST IF ALREADY HAVE MOVE SOURCE             
         BNZ   EDLISTR             YES                                          
         MVI   ERROR,SELERR                                                     
         CLI   LSTOTH,YES          TEST IF OTHER SELECT INPUT FOUND             
         BE    EDLISTR                                                          
*                                                                               
EDLIST5  STC   R3,LSTFROMN         SAVE FROM FIELD NUMBER                       
         ST    R2,LSTAFROM         AND ITS ADDRESS                              
         CLI   LSTREST,YES         TEST IF SAVE STORAGE PRESENT                 
         BE    *+12                YES                                          
         BAS   RE,GETSAVE          NO-SO FETCH IT                               
         MVI   LSTREST,YES                                                      
         GOTO1 VSETADD                                                          
*                                                                               
         LR    R4,R3                                                            
         BCTR  R4,0                                                             
         MH    R4,=Y(OLISTLNQ)     R4=INDEX INTO LIST TABLE                     
         AH    R4,DISPLTAB         DISP TO LIST TABLE                           
         LA    R4,SYSD(R4)         R4=A(LIST ENTRY)                             
         USING OLISTD,R4                                                        
         MVI   ERROR,INVALID                                                    
         OC    OLNODE,OLNODE       TEST DATA EXISTS FOR LINE                    
         BZ    EDLISTR                                                          
*                                                                               
         MVC   FROMKEY,OLNODKEY    EXTRACT FROM OUTLINE'S NODAL KEY,            
         MVC   FROMNODE,OLNODE     NODE, CODE, AND LEVEL                        
         MVC   FROMCODE,OLCODE                                                  
         MVC   FROMLEV,OLEVEL                                                   
         B     EDLIST15                                                         
*                                                                               
EDLIST6  CLI   FLD,C'A'            TEST FOR TO POSITION                         
         BE    EDLIST8                                                          
         CLI   FLD,C'B'                                                         
         BE    EDLIST8                                                          
         CLI   FLD,C'+'                                                         
         BE    EDLIST8                                                          
         B     EDLIST10                                                         
*                                                                               
EDLIST8  MVI   ERROR,DUPTOERR                                                   
         OC    TOPARENT,TOPARENT   TEST IF 'TO' OUTLINE SPECIFIED               
         BNZ   EDLISTR                                                          
         MVI   ERROR,SELERR                                                     
         CLI   LSTOTH,YES                                                       
         BE    EDLISTR                                                          
*                                                                               
         STC   R3,LSTTON           TO FIELD NUMBER                              
         ST    R2,LSTATO           AND ADDRESS                                  
         CLI   LSTREST,YES                                                      
         BE    *+12                                                             
         BAS   RE,GETSAVE          RETRIEVE SAVE STORAGE                        
         MVI   LSTREST,YES                                                      
         GOTO1 VSETADD                                                          
*                                                                               
EDLIST9  LR    R4,R3                                                            
         BCTR  R4,0                                                             
         MH    R4,=Y(OLISTLNQ)                                                  
         AH    R4,DISPLTAB                                                      
         LA    R4,SYSD(R4)                                                      
         MVI   ERROR,INVALID                                                    
         OC    OLNODE,OLNODE                                                    
         BZ    EDLISTR                                                          
*                                                                               
         GOTO1 GETPAR,PARAS,OLNODKEY,TOPARENT                                   
         MVC   POSCOMM,FLD         SAVE POSITIONAL COMMAND                      
         MVC   TOOUTCOD,OLCODE                                                  
         MVC   TONODE,OLNODE                                                    
         MVC   TOLEVEL,OLEVEL                                                   
         CLI   POSCOMM,C'+'        TEST IF ADDING TO NEXT LEVEL                 
         BNE   EDLIST15                                                         
         MVC   TOPARENT,OLNODKEY   PARENT IS CHOSEN RECORD                      
*                                                                               
         LA    RE,TOPARENT         CLEAN UP THE KEY                             
         LA    R1,L'TOPARENT                                                    
         CLI   0(RE),C' '          TEST FOR END-OF-KEY                          
         BNH   *+14                YES                                          
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         DC    H'0'                                                             
*                                                                               
         BCTR  R1,0                CLEAR REST OF KEY TO SPACES                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SPACES                                                   
*                                                                               
         MVC   TONODE,OLNODE2      'TO' NODE IS ONE ESTABLISHED BY REC          
         ZIC   R1,TOLEVEL                                                       
         LA    R1,1(R1)            INCREMENT PARENT'S LEVEL                     
         STC   R1,TOLEVEL                                                       
         MVI   ERROR,LEVERR                                                     
         CLI   TOLEVEL,MAXOUTS     TEST IF PAST OUTLINE LEVEL LIMIT             
         BH    EDLISTT             YES-SHOW 'TO' DETAILS ERROR                  
         B     EDLIST15                                                         
*                                                                               
EDLIST10 MVI   LSTOTH,YES          OTHER SELECT INPUT                           
         MVI   ERROR,SELERR                                                     
         CLI   LSTFROMN,0          TEST IF MOVE 'FROM' INPUT                    
         BE    *+10                NO                                           
         XC    FROMVALS(FROMVALN),FROMVALS                                      
         CLI   LSTTON,0            TEST IF MOVE 'TO' INPUT                      
         BE    *+10                                                             
         XC    TOVALS(TOVALN),TOVALS                                            
         OC    LSTFROMN(2),LSTFROMN                                             
         BNZ   EDLISTR                                                          
*                                                                               
EDLIST15 LA    R3,1(R3)            INCREMENT FIELD NUMBER                       
         BAS   RE,BUMPU            NEXT SELECT FIELD                            
         BNE   EDLIST2                                                          
         B     XIT                 ALL DONE WITH EDIT                           
*                                                                               
EDLISTT  L     R2,LSTATO           POSITION CURSOR TO 'TO' FIELD                
         XC    TOVALS(TOVALN),TOVALS                                            
         OC    LSTAFROM,LSTAFROM   TEST IF MOVE 'FROM' ON THIS                  
         BZ    *+10                SCREEN                                       
         XC    FROMVALS(FROMVALN),FROMVALS YES-CLEAR FROM VALUES                
         B     EDLISTR                                                          
*                                                                               
EDLISTF  L     R2,LSTAFROM                                                      
         XC    FROMVALS(FROMVALN),FROMVALS                                      
         OC    LSTATO,LSTATO       TEST IF MOVE 'TO' ON THIS                    
         BZ    *+10                SCREEN                                       
         XC    TOVALS(TOVALN),TOVALS YES-CLEAR 'TO' DETAILS                     
*                                                                               
EDLISTR  ST    R2,FADDR                                                         
         LA    R2,8(R2)                                                         
         STCM  R2,7,FLAST                                                       
         B     SPERR1                                                           
         EJECT                                                                  
* SUB-ROUTINE TO PERFORM OUTLINE RE-POSITIONING                                 
*                                                                               
REPO     NTR1                                                                   
         CLC   TONODE,FROMNODE     TEST IF OUTLINE IS MOVING TO                 
         BE    *+8                 NEW PARENT                                   
         MVI   NDSUBRSW,YES        YES-SO SUB-RECORDS MUST BE CHANGED           
         MVC   LSTSVRER,NDREREAD   SAVE RE-READ VALUE BEFORE MOVE               
         MVI   NDREREAD,YES        FORCE RE-READ TO SOLVE GRANTS'S BUG          
         MVC   NDIOA,AIO1                                                       
         MVC   NDIOA2,AIO2                                                      
*                                                                               
         CLC   TONODE,FROMNODE     TEST FOR CHANGE OF PARENT                    
         BE    REPO1                                                            
         CLC   TOLEVEL,FROMLEV     TEST IF OUTLINE MOVING DOWN                  
         BNH   *+8                                                              
         BAS   RE,LEVCK            YES-CHECK FOR LEVEL OVERFLOW                 
*                                                                               
REPO1    XC    NDHOOK,NDHOOK                                                    
         CLI   POSCOMM,C'+'                                                     
         BNE   *+10                                                             
         XC    TOOUTCOD,TOOUTCOD                                                
         GOTO1 VNODIO,DMCB,NODBLKD,(POSCOMM,=C'REPO'),FROMKEY,         X        
               (8,TOOUTCOD),TOPARENT                                            
         CLI   NDERR,0                                                          
         BE    REPO2                                                            
         ICM   R2,15,LSTAFROM      POSITION CURSOR                              
         BNZ   *+8                 TO FROM FIELD IF ON SCREEN                   
         ICM   R2,15,LSTATO        OR TO FIELD                                  
         XC    MOVEVALS,MOVEVALS                                                
         GOTO1 VNODERR                                                          
*                                                                               
REPO2    GOTO1 GETOUT,PARAS,TOPARENT,FROMCODE                                   
         XC    NDHOOK,NDHOOK       RE-READ MOVED OUTLINE                        
         MVI   NDSUBRSW,NO         MAKE SURE NO SUB-RECORDS RETURNED            
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0             TEST IF RECORD RE-READ                       
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NODIO ERROR HERE              
         MVC   SVNKEY,NODKEY       SAVE MOVED KEY FOR OVERLAY                   
*                                                                               
REPO4    L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         XC    WORK,WORK           BUILD PASSIVE POINTER AT WORK                
*                                                                               
         MVI   ELCODE,BUPTRELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPTRD,R6                                                        
         MVC   WORK(L'BUCKEY),BUPOINT                                           
         MVC   WORK+L'BUCKEY(L'BUCCTL),BURCTL                                   
         LA    R4,WORK                                                          
         USING BUCRECD,R4                                                       
         MVC   BUCDA,NDLVDA                                                     
*                                                                               
REPO6    XC    KEY,KEY             READ AND WRITE POINTER                       
         LA    R4,KEY                                                           
         MVC   BUCKEY,WORK                                                      
         OI    DMINBTS,X'88'       READ FOR UPDATE AND PASS DELETES             
         GOTO1 HIGH                                                             
         CLC   BUCKEY,KEYSAVE                                                   
         BE    REPO7                                                            
         MVC   KEY(BUKLNQ),WORK                                                 
         GOTO1 ADD                                                              
         B     REPO8                                                            
*                                                                               
REPO7    MVC   KEY(BUKLNQ),WORK                                                 
         GOTO1 WRITE                                                            
*                                                                               
REPO8    L     R4,NDIOA            TRAP TO LOOK FOR DIFFERENT                   
         L     R3,NDLEVPTR         D/A ON PASSIVE POINTER                       
         MVI   ELCODE,BUPTRELQ     GET PASSIVE POINTER ELEM                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPTRD,R6                                                        
         MVC   KEY(L'BUCKEY),BUPOINT                                            
         GOTO1 HIGH                                                             
         CLC   NDLVDA,KEY+(BUCDA-BUCKEY) TEST SAME D/A                          
         BE    *+6                                                              
         DC    H'0'                PROBLEMS-TAKE A HIT                          
*                                                                               
         MVC   NDREREAD,LSTSVRER   RESTORE REREAD VALUE                         
         BAS   RE,PUTSAVE                                                       
         CLC   TOLEVEL,FROMLEV     TEST IF OUTLINE MOVED DOWN                   
         BNH   *+8                                                              
         BAS   RE,UPPLAN           UPDATE PLAN IF NECESSARY                     
         B     REPOX                                                            
*                                                                               
REPOX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR OVERFLOWING LEVEL LIMIT ON A MOVE                    
*                                                                               
LEVCK    NTR1                                                                   
         ICM   R2,15,LSTAFROM      POSITION FOR CURSOR IF ERROR                 
         BNZ   *+8                                                              
         ICM   R2,15,LSTATO                                                     
         MVC   HALF(1),FROMLEV     HALF+0(1)=PARENT'S LEVEL                     
         MVC   HALF+1(1),FROMLEV   HALF+1(1)=LOWEST LEVEL OF CHILDREN           
         MVI   NDUPDTSW,NO         SUPPRESS READ FOR UPDATE                     
         MVC   NODKEY,FROMKEY                                                   
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0             TEST FOR NODIO ERROR                         
         BE    LEVCK1                                                           
         XC    MOVEVALS,MOVEVALS                                                
         GOTO1 VNODERR                                                          
*                                                                               
LEVCK1   LA    R1,LEVCK2                                                        
         ST    R1,NDHOOK                                                        
         GOTO1 (RF),DMCB,NODBLKD,=C'LSEQ',NODKEY,0                              
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   NDUPDTSW,0          CLEAR READ FOR UPDATE SWITCH                 
*                                                                               
         ZIC   R1,HALF+1           LOWEST LEV ATTACHED TO 'FROM' OUTL           
         ZIC   R0,HALF             LEVEL OF 'FROM' OUTLINE                      
         SR    R1,R0               RELATIVE LEVELS DOWN FROM PARENT             
         ZIC   R0,TOLEVEL                                                       
         AR    R1,R0               LOWEST LEVEL AFTER MOVE                      
         STC   R1,LOWLEVEL                                                      
         CLI   LOWLEVEL,MAXOUTS    TEST FOR LEVEL OVERFLOW                      
         BNH   LEVCKX                                                           
         XC    MOVEVALS,MOVEVALS                                                
         MVI   ERROR,LEVERR                                                     
         B     EDLISTR                                                          
*                                                                               
LEVCK2   ST    RE,SAVERE           HOOK ROUTINE                                 
         CLI   NDMODE,NDPROC                                                    
         BNER  RE                                                               
         GOTO1 VGETVAL                                                          
         CLC   HALF+1(1),OUTLEV    TEST FOR LOWEST SO FAR                       
         BH    *+10                                                             
         MVC   HALF+1(1),OUTLEV                                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
LEVCKX   B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE PLAN RECORD'S LOWEST OUTLINE LEVEL FIELD IF             
* NECESSARY.  CALLED ONLY IF OUTLINE HAS BEEN MOVED TO A LOWER LEVEL.           
*                                                                               
* AT ENTRY, MOVED OUTLINE HAS JUST BEEN RE-READ AND NODKEY CONTAINS             
* ITS NODAL KEY                                                                 
*                                                                               
UPPLAN   NTR1                                                                   
         MVC   NODKEYSV,NODKEY     MOVED OUTLINE'S KEY                          
         ZIC   R2,NDLEV            GET ITS NEW LEVEL                            
         SH    R2,=H'3'            R2=N'LEVELS TO PLAN                          
         BZ    UPPLAN4             HAVE JUST READ PLAN                          
*                                                                               
UPPLAN2  GOTO1 GETPAR,PARAS,NODKEYSV,NODKEY                                     
         MVC   NODKEYSV,NODKEY     WORK OUT THE PLAN'S NODAL KEY                
         BCT   R2,UPPLAN2                                                       
*                                                                               
UPPLAN4  GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0             TEST FOR ERROR                               
         BNE   UPPLANX             BAIL OUT AND SKIP UPDATE                     
         L     R4,NDIOA                                                         
         MVI   ELCODE,BUPLNELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   UPPLANX                                                          
         USING BUPLND,R6                                                        
         CLC   BUPLNLOW,LOWLEVEL   TEST IF NEW LOWEST LEVEL                     
         BNL   UPPLANX             NO-SO SKIP PLAN UPDATE                       
         MVC   BUPLNLOW,LOWLEVEL                                                
         GOTO1 VNODIO,DMCB,NODBLKD,=C'PUT',NODKEY,0                             
*                                                                               
UPPLANX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO CONSTRUCT THE PARENT'S NODAL KEY FOR AN OUTLINE                
*                                                                               
* AT ENTRY,  P1=A(OUTLINE'S NODAL KEY)                                          
*            P2=A(PARENT'S NODAL KEY)                                           
*                                                                               
GETPAR   STM   RE,R1,WORK                                                       
         LM    RE,RF,0(R1)         RE=A(OUTLINE KEY),RF=A(PARENT'S KEY)         
         MVC   0(L'NODKEY,RF),0(RE) INITIALIZE PARENT KEY                       
         LA    R1,L'NODKEY         R1=COUNTER                                   
         LA    RE,L'NODKEY-1(RF)   RE=POINTER TO END OF KEY                     
*                                                                               
GETPAR1  CLC   NDDELIM,0(RE)       LOOK FOR LAST DELIMITER                      
         BE    GETPAR2                                                          
         MVI   0(RE),C' '          CLEAR KEY UNTIL LAST DELIMITER FOUND         
         BCTR  RE,0                                                             
         BCT   R1,GETPAR1                                                       
         DC    H'0'                                                             
*                                                                               
GETPAR2  MVI   0(RE),C' '          CLEAR LAST DELIMITER                         
*                                                                               
GETPARX  LM    RE,R1,WORK                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO FORM OUTLINE NODAL KEY BY CONCATENTATING OUTLINE CODE          
* TO PARENT'S NODAL KEY                                                         
*                                                                               
* AT ENTRY, P1=A(PARENT'S KEY), P2=A(OUTLINE CODE)                              
*                                                                               
* AT EXIT, NODKEY AND NODKEYSV SET                                              
*                                                                               
GETOUT   STM   RE,RF,DUB                                                        
         L     RE,0(R1)            RE=A(PARENT'S KEY)                           
         MVC   NODKEY,0(RE)        INITIALIZE TO PARENT'S KEY                   
         LA    RF,L'NODKEY                                                      
         LA    RE,NODKEY                                                        
*                                                                               
         CLI   0(RE),C' '          TEST FOR FIRST BLANK                         
         BE    GETOUT2                                                          
         LA    RE,1(RE)            NEXT BYTE                                    
         BCT   RF,*-12                                                          
         DC    H'0'                                                             
*                                                                               
GETOUT2  MVC   0(1,RE),NDDELIM     SET DELIMITER                                
         L     RF,4(R1)            GET A(OUTLINE CODE)                          
         MVC   1(L'BUKCODE,RE),0(RF) AND OUTLINE CODE                           
         MVI   L'BUKCODE+1(RE),C' ' BLANK TERMINATES NODAL KEY                  
         MVC   NODKEYSV,NODKEY                                                  
*                                                                               
GETOUTX  LM    RE,RF,DUB                                                        
         BR    RE                                                               
         EJECT                                                                  
* ADJUST MOVE 'TO' AND 'FROM' FIELDS IF ON SCREEN                               
*                                                                               
ADJFLD   ST    RE,SAVERE                                                        
         ICM   R2,15,LSTAFROM                                                   
         BZ    *+8                                                              
         BAS   RE,ADJDAT                                                        
         ICM   R2,15,LSTATO                                                     
         BZ    *+8                                                              
         BAS   RE,ADJDAT                                                        
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
ADJDAT   ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),8(R2)        REMOVE FIELD DATA                            
         MVI   8(R2),STAR          PLUG IN A STAR AT FIELD START                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   9(0,R2),DUB         SHIFT DATA 1 TO LEFT                         
         OI    6(R2),X'80'         XMIT                                         
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINES TO GET AND PUT SAVE STORAGE                                      
*                                                                               
PUTSAVE  ST    RE,SAVERE                                                        
         MVC   COMMAND(6),=C'DMWRT '                                            
         B     IOSAVE                                                           
*                                                                               
GETSAVE  ST    RE,SAVERE                                                        
         MVC   COMMAND(6),=C'DMREAD'                                            
*                                                                               
IOSAVE   XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,1                                                         
         MVC   DMCB+10(2),TERM     TERMINAL NUMBER                              
         GOTO1 DATAMGR,DMCB,COMMAND,=C'TEMPSTR',,ASTARTSV,0                     
         L     RE,SAVERE                                                        
         CLI   8(R1),0             TEST FOR ERROR                               
         BER   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
* SUPPORTING SMALL SUB-ROUTINES                                                 
*                                                                               
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
BUMPU    ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             TEST FOR EOS                                 
         BER   RE                                                               
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    BUMPU                                                            
         LTR   RE,RE               SET CC=NEQ FOR ANOTHER FIELD                 
         BR    RE                                                               
         SPACE 2                                                                
* GETEL - ELEMENT SEARCH                                                        
*         AT ENTRY, R4 POINTS TO RECORD, ELCODE IS SET                          
*         AT EXIT, CC=EQ IF ELEMENT FOUND AND R6 POINTS TO IT                   
*                  CC=NEQ IF ELEMENT DOES NOT EXIST                             
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(ELCODE,(R4)),0                         
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         EJECT                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
SPERR1   GOTO1 VCURSERR,PARAS,1                                                 
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
* CONSTANTS,TABLES,ETC                                                          
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
*&&US                                                                           
CORETAB  DC    AL1(QGENCON,QNODIO,QMSPACK,QMSUNPK,QSTAPACK,QGETINS)             
CORES    EQU   *-CORETAB                                                        
*&&                                                                             
*&&UK                                                                           
*ORETAB  DC    X'3070'                                                          
CORETAB  DC    AL1(QGENCON,QNODIO)                                              
CORES    EQU   *-CORETAB                                                        
*&&                                                                             
         SPACE 1                                                                
MONTAB   DS    0CL3                                                             
         DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
         SPACE 1                                                                
MOVEMSG  DC    C'** MOVE COMPLETED **'                                          
RESETMSG DC    C'** RESET COMPLETED **'                                         
FPENDMSG DC    C'WAITING FOR MOVE ''FROM'' SELECTION'                           
TPENDMSG DC    C'WAITING FOR MOVE ''TO'' SELECTION'                             
         SPACE 1                                                                
GENERSYS EQU   5                   GENCON ERROR SYSTEM NUMBER                   
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
SYSVCON  DS    0F                                                               
         DC    V(MONVAL)                                                        
         DC    V(BURNEM)                                                        
         DC    V(BUPPER)                                                        
         DC    V(DUMMY)                                                         
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 1                                                                
ADTAB    DS    0AL3                                                             
         DC    AL3(NODBLK-SYSD)                                                 
NADCONS  EQU   (*-ADTAB)/L'ADTAB                                                
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* SUB-ROUTINE TO MANAGE SWAPPING FROM LIST SCREENS USING SELECTED               
* RECORDS OR SCREEN KEY FIELD INPUT                                             
*                                                                               
LIST     NMOD1 0,**LIST                                                         
         L     RC,0(R1)                                                         
         CLI   TWASCR,X'E2'        TEST FOR CLIENT LIST                         
         BE    LIST1                                                            
         CLI   TWASCR,X'E3'        TEST FOR PRODUCT LIST                        
         BE    LIST2                                                            
         CLI   TWASCR,X'E4'        TEST FOR PLAN LIST                           
         BE    LIST3                                                            
         CLI   TWASCR,X'E5'        TEST FOR OUTLINE LIST                        
         BE    LIST4                                                            
         CLI   TWASCR,X'E6'        TEST FOR DTYPE LIST                          
         BNE   LISTX                                                            
*                                                                               
         CLC   CONREC(5),=C'DTYPE'                                              
         BNE   LISTX                                                            
         CLI   THISPF,PF12         TEST PF12=PLAN LIST                          
         BE    LIST5                                                            
         B     LISTX                                                            
*                                                                               
LIST1    CLC   CONREC(6),=C'CLIENT'                                             
         BNE   LISTX                                                            
         CLI   THISPF,PF2          TEST PF2=PRODUCT                             
         BE    LIST5                                                            
         CLI   THISPF,PF4          TEST PF4=TEXT LIST                           
         BE    LIST5                                                            
         B     LISTX                                                            
*                                                                               
LIST2    CLC   CONREC(7),=C'PRODUCT'                                            
         BNE   LISTX                                                            
         CLI   THISPF,PF2          TEST PF2=PLAN                                
         BE    LIST5                                                            
         CLI   THISPF,PF4          TEST PF4=TEXT LIST                           
         BE    LIST5                                                            
         CLI   THISPF,PF12         TEST PF12=CLIENT                             
         BE    LIST5                                                            
         B     LISTX                                                            
*                                                                               
LIST3    CLC   CONREC(4),=C'PLAN'                                               
         BNE   LISTX                                                            
         CLI   THISPF,PF2          TEST FOR PF2=OUTLINE                         
         BE    LIST5                                                            
         CLI   THISPF,PF3          OR PF3=DTYPE                                 
         BE    LIST5                                                            
         CLI   THISPF,PF4          OR PF4=TEXT LIST                             
         BE    LIST5                                                            
         CLI   THISPF,PF5          OR PF5=TEXT ADD                              
         BE    LIST5                                                            
         CLI   THISPF,PF12         OR PF12=PRODUCT                              
         BE    LIST5                                                            
         B     LISTX                                                            
*                                                                               
LIST4    CLC   CONREC(7),=C'OUTLINE'                                            
         BNE   LISTX                                                            
         CLI   THISPF,PF4          TEST PF4=TEXT LIST                           
         BE    LIST5                                                            
         CLI   THISPF,PF5          TEST PF5=TEXT ADD                            
         BE    LIST5                                                            
         CLI   THISPF,PF12         TEST PF12=PLAN                               
         BNE   LISTX                                                            
*                                                                               
LIST5    CLC   CONACT(4),=C'LIST'  MAKE SURE ACTION IS LIST                     
         BNE   LISTX                                                            
         LR    R3,R7                                                            
         AH    R3,CURDISP          R3=A(CURSOR POSITION)                        
         LR    RE,R7                                                            
         AH    RE,MODLAST                                                       
         LA    R2,CONACT                                                        
         CR    RE,R2               TEST LAST MODIFIED VS ACTION                 
         BH    LISTX               SOMETHING CHANGED AFTER ACTION               
*                                                                               
* MANAGE SWAPPING TO LIST SCREEN ONE LEVEL DOWN OR TO TEXT LIST/ADD             
* USE KEY OF SELECTED RECORD TO CONSTRUCT LIST SCREEN KEY FIELD                 
*                                                                               
LIST6    LA    R2,CONTAGH                                                       
         CLI   THISPF,PF12         TEST FOR PF12=ONE LEVEL UP                   
         BE    LIST30              YES-HANDLE SEPARATELY                        
         BAS   RE,NXTFLD                                                        
         TM    1(R2),X'20'         LOOK FOR 1 BYTE PROTECTED FIELD              
         BZ    *-8                                                              
         CLI   0(R2),9                                                          
         BNE   *-16                                                             
*                                                                               
         BAS   RE,NXTUNP           R2=A(FIRST SELECT FIELD)                     
         SR    R0,R0                                                            
         ICM   R0,1,LISTNUM        R0=N'ENTRIES ON SCREEN                       
         BZ    LISTX                                                            
         LA    R4,LISTDIR          R4=A(LIST DIRECTORY)                         
*                                                                               
LIST7    CR    R2,R3               TEST IF PF KEY PRESSED                       
         BE    LIST8               IN A SELECT FIELD                            
         BAS   RE,NXTUNP           NO                                           
         LA    R4,6(R4)                                                         
         BCT   R0,LIST7                                                         
         B     LISTX               NOT PRESSED IN A SELECT FIELD                
*                                                                               
LIST8    OC    2(4,R4),2(R4)       TEST IF PRESSED IN EMPTY FIELD               
         BZ    LISTX               YES                                          
         MVI   GCMODE,C'S'         FIRST CALL GENCON IN SLAVED MODE             
         GOTO1 VGENCON,DMCB,(R8)                                                
         MVI   GCMODE,0                                                         
         L     R5,ANODBLK                                                       
         USING NODBLKD,R5                                                       
         GOTO1 VSETADD                                                          
         XC    KEY,KEY                                                          
         LA    RE,KEY+(BUKDA-BUKEY)                                             
         MVC   0(4,RE),2(R4)       SET DA IN KEY                                
         GOTO1 GETREC                                                           
         GOTO1 VTRACE                                                           
*                                                                               
LIST10   XC    WORK,WORK                                                        
         LA    R4,WORK             R4=A(STRING POSITION)                        
         LA    R3,NDLVTAB+NDLVTABL R3=A(CLIENT LEVEL)                           
         ZIC   R0,NDLEV                                                         
         LA    RF,3                                                             
         CR    R0,RF               TEST PAST PLAN LEVEL                         
         BNH   *+6                                                              
         LR    R0,RF               YES-ONLY GO TO PLAN                          
*                                                                               
LIST11   MVC   0(L'BUKCODE+1,R4),NDLVCOD GET CODE                               
         LA    R4,L'BUKCODE+1(R4)  NEXT CODE POSITION                           
         LA    R3,NDLVTABL(R3)                                                  
         BCT   R0,LIST11                                                        
*                                                                               
LIST12   CLI   NDLEV,3             TEST FOR OUTLINE                             
         BNH   *+14                                                             
         L     R3,NDLEVPTR         YES-ALSO NEED ITS CODE                       
         MVC   0(L'BUKCODE,R4),NDLVCOD                                          
         OC    WORK,SPACES                                                      
         LA    R0,L'WORK                                                        
         GOTO1 SQUASHER,DMCB,WORK,(C',',(R0))                                   
*                                                                               
         MVC   CONKEY,SPACES                                                    
         L     R2,4(R1)            GET LENGTH RETURNED BY SQUASHER              
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   CONKEY(0),WORK                                                   
         LA    R2,1(R2)                                                         
         STC   R2,CONKEYH+5        SET KEY FIELD LENGTH                         
         OI    CONKEYH+4,X'80'     SET INPUT THIS TIME                          
*                                                                               
LIST14   CLI   THISPF,PF4          TEST SWAP TO TEXT LIST                       
         BNE   LIST15              NO                                           
         MVC   CONREC,SPACES       YES-SET RECORD TO TEXT                       
         MVC   CONREC(4),=C'TEXT'                                               
         MVI   CONRECH+5,4                                                      
         B     LIST25                                                           
*                                                                               
LIST15   CLI   THISPF,PF5          TEST SWAP TO TEXT ADD                        
         BNE   LIST16                                                           
         MVC   CONREC,SPACES       PREPARE RECORD/ACTION                        
         MVC   CONREC(4),=C'TEXT'                                               
         MVI   CONRECH+5,4                                                      
         MVC   CONACT,SPACES                                                    
         MVC   CONACT(3),=C'ADD'                                                
         MVI   CONACTH+5,3                                                      
         OI    CONACTH+4,X'80'                                                  
*                                                                               
         ZIC   R4,CONKEYH+5                                                     
         LA    R4,CONKEY(R4)       R4=END OF KEY STRING                         
         LA    R1,4                COMPUTE N'LEVELS BEFORE 1ST OUTLINE          
         ZIC   RF,NDLEV                                                         
         SR    R1,RF                                                            
         BNM   *+6                                                              
         SR    R1,R1               FORCE NEGATIVE RESULT TO ZERO                
*&&US*&& LA    R1,3(R1)            ADD IN DTYPE/PERIOD                          
*&&UK*&& LA    R1,1(R1)            ADD COMMA AFTER OUTLINE                      
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         BCT   R1,*-8                                                           
*                                                                               
         MVI   ELCODE,KEYFTEXT                                                  
         GOTO1 VGETKEYF                                                         
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         LA    R0,CONKEY                                                        
         SR    R4,R0                                                            
         STC   R4,CONKEYH+5                                                     
         B     LIST25                                                           
*                                                                               
LIST16   CLI   TWASCR,X'E2'        TEST FOR CLIENT                              
         BNE   LIST18                                                           
         MVC   CONREC(7),=C'PRODUCT'                                            
         MVI   CONRECH+5,7                                                      
         B     LIST25                                                           
*                                                                               
LIST18   CLI   TWASCR,X'E3'        TEST FOR PRODUCT                             
         BNE   LIST20                                                           
         MVC   CONREC,SPACES                                                    
         MVC   CONREC(4),=C'PLAN'                                               
         MVI   CONRECH+5,4                                                      
         B     LIST25                                                           
*                                                                               
LIST20   MVC   CONREC,SPACES                                                    
         MVC   CONREC(5),=C'DTYPE'                                              
         MVI   CONRECH+5,5                                                      
         CLI   THISPF,PF3          TEST DTYPE SELECTED                          
         BE    *+14                                                             
         MVC   CONREC(7),=C'OUTLINE'                                            
         MVI   CONRECH+5,7                                                      
*                                                                               
LIST25   OI    CONRECH+4,X'80'                                                  
         GOTO1 SAVEUWKA                                                         
         B     LISTX                                                            
*                                                                               
* MANAGE SWAPPING TO LIST SCREEN ONE LEVEL UP  -  USE KEY FIELDS                
* ON LIST SCREENS SINCE KEY FIELDS ARE NOT SAVED ON MAIN LIST SCREENS           
*                                                                               
LIST30   MVI   GCMODE,C'S'                                                      
         GOTO1 VGENCON,DMCB,(R8)                                                
         MVI   GCMODE,0                                                         
         BAS   RE,NXTUNP           SET R2=A(FIRST KEY FIELD)                    
         MVC   CONREC,SPACES       CLEAR RECORD FIELD                           
*                                                                               
         CLI   TWASCR,X'E5'        TEST OUTLINE LIST NOW                        
         BE    *+12                YES                                          
         CLI   TWASCR,X'E6'        TEST DTYPE LIST NOW                          
         BNE   LIST31              NO                                           
         LA    R3,3                SET R3=COUNTER                               
         MVC   CONREC(4),=C'PLAN'  SET FOR PLAN LIST                            
         MVI   CONRECH+5,4                                                      
         B     LIST34                                                           
*                                                                               
LIST31   CLI   TWASCR,X'E4'        TEST PLAN LIST NOW                           
         BNE   LIST32              NO-MUST BE ON PRODUCT                        
         LA    R3,2                                                             
         MVC   CONREC(7),=C'PRODUCT'                                            
         MVI   CONRECH+5,7                                                      
         B     LIST34                                                           
*                                                                               
LIST32   LA    R3,1                                                             
         MVC   CONREC(6),=C'CLIENT'                                             
         MVI   CONRECH+5,6                                                      
*                                                                               
LIST34   OI    CONRECH+4,X'80'                                                  
         CLI   5(R2),0             TEST FIRST KEY FIELD EMPTY                   
         BE    LIST38                                                           
         MVC   WORK,SPACES         BUILD KEY FIELD IN WORK                      
         LA    R4,WORK                                                          
*                                                                               
LIST35   SR    R1,R1                                                            
         ICM   R1,1,5(R2)          EXTRACT KEY FIELDS                           
         BZ    LIST36              STOP ON EMPTY FIELD                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),8(R2)                                                    
         LA    R4,2(R1,R4)                                                      
         BAS   RE,NXTUNP                                                        
         BCT   R3,LIST35                                                        
*                                                                               
LIST36   LA    R0,WORK                                                          
         SR    R4,R0               COMPUTE L'INPUT STRING                       
         GOTO1 SQUASHER,DMCB,WORK,(C',',(R4))                                   
         L     R2,4(R1)                                                         
         MVC   CONKEY,SPACES                                                    
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   CONKEY(0),WORK                                                   
         LA    R2,1(R2)                                                         
         STC   R2,CONKEYH+5                                                     
         OI    CONKEYH+4,X'80'                                                  
*                                                                               
LIST38   GOTO1 SAVEUWKA                                                         
*                                                                               
LISTX    XIT1  ,                                                                
         DROP  R6                                                               
         SPACE 2                                                                
NXTFLD   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
NXTUNP   ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             TEST FOR EOS                                 
         BER   RE                                                               
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    NXTUNP                                                           
         LTR   RE,RE               SET CC=NEQ FOR ANOTHER FIELD                 
         BR    RE                                                               
         SPACE 2                                                                
* LITERAL POOL FOR LIST                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* SUB-ROUTINE TO MANAGE SWAPPING FROM OUTLINE, DATATYPE OR TEXT                 
* LIST TO ENTRY AND VICE-VERSA.  SAVED VALUES FOR THIS ARE HELD                 
* IN THE SAVKEY TABLE IN TWA0                                                   
*                                                                               
ENT      NMOD1 0,**ENT                                                          
         L     RC,0(R1)                                                         
         CLI   TWASCR,X'E5'        TEST FOR OUTLINE LIST                        
         BE    ENT1                                                             
         CLI   TWASCR,X'E6'        TEST FOR DTYPE LIST                          
         BE    ENT2                                                             
         CLI   TWASCR,X'E8'        TEST FOR TEXT LIST                           
         BE    ENT3                                                             
         B     ENTX                                                             
*                                                                               
ENT1     CLC   CONREC(7),=C'OUTLINE' TEST RECORD IS THE SAME                    
         BE    ENT4                                                             
         B     ENTX                                                             
*                                                                               
ENT2     CLC   CONREC(5),=C'DTYPE' TEST RECORD IS THE SAME                      
         BE    ENT4                YES                                          
         B     ENTX                                                             
*                                                                               
ENT3     CLC   CONREC(4),=C'TEXT'  TEST RECORD IS THE SAME                      
         BNE   ENTX                                                             
*                                                                               
ENT4     CLC   CONACT(4),=C'LIST'                                               
         BNE   ENTX                                                             
         CLI   THISPF,PF2          TEST PF2=SWAP TO ENTRY                       
         BNE   ENTX                                                             
*                                                                               
         LR    R3,R7                                                            
         AH    R3,CURDISP          R3=A(CURSOR POSITION)                        
         LR    RE,R7                                                            
         AH    RE,MODLAST          RE=A(LAST MODIFIED FIELD)                    
         LA    R2,CONACT                                                        
         CR    RE,R2               TEST FIELD MODIFIED AFTER ACTION             
         BH    ENTX                YES-IGNORE SWAP                              
*                                                                               
         CLI   TWASCR,X'E8'        TEST SWAPPING FROM TEXT LIST                 
         BE    ENT14               NO                                           
*                                                                               
         LA    R2,CONTAGH                                                       
         SR    R0,R0                                                            
*                                                                               
ENT5     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST PROTECTED FIELD                         
         BZ    ENT5                NO                                           
         CLI   0(R2),9             WITH LENGTH OF 1                             
         BNE   ENT5                                                             
*                                                                               
         BAS   RE,NEXTU            ADVANCE TO FIRST SELECT FIELD                
         SR    R0,R0                                                            
         ICM   R0,1,LISTNUM        R0=N'LIST ENTRIES                            
         BZ    ENTX                                                             
         LA    R4,LISTDIR                                                       
         LH    R6,DISPLTAB                                                      
         LA    R6,SYSD(R6)         R6=OUTLINE LIST TABLE                        
         USING OLISTD,R6                                                        
*                                                                               
ENT6     CR    R2,R3               TEST PF2 PRESSED IN SELECT FIELD             
         BE    ENT8                                                             
         BAS   RE,NEXTU                                                         
         LA    R4,6(R4)                                                         
         LA    R6,OLISTLNQ(R6)     NEXT LIST TABLE ENTRY                        
         BCT   R0,ENT6                                                          
         B     ENTX                                                             
*                                                                               
ENT8     OC    2(4,R4),2(R4)       TEST FOR EMPTY FIELD                         
         BZ    ENTX                YES-NOTHING TO SELECT                        
*                                                                               
         MVI   GCMODE,C'S'         LOAD GENCON IN SLAVED MODE                   
         GOTO1 VGENCON,DMCB,(R8)                                                
         XC    CONHEAD,CONHEAD                                                  
         OI    CONHEADH+6,X'80'                                                 
*                                                                               
ENT10    L     R5,ANODBLK                                                       
         USING NODBLKD,R5                                                       
         XC    KEY,KEY                                                          
         LA    RE,KEY+(BUKDA-BUKEY)                                             
         MVC   0(L'BUKDA,RE),2(R4) SLOT D/A INTO KEY                            
         GOTO1 GETREC                                                           
*                                                                               
ENT11    L     R4,AIO                                                           
         USING BURECD,R4                                                        
*                                                                               
         MVI   ELCODE,BUPOLELQ     LOOK FOR FORMULA                             
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(ELCODE,(R4)),0                         
         CLI   12(R1),0            TEST IF ELEM FOUND                           
         BNE   ENT12               NO                                           
         MVI   ERROR,ROWFERR                                                    
         CLI   TWASCR,X'E5'                                                     
         BE    *+8                                                              
         MVI   ERROR,COLFERR                                                    
         B     ENTERRX                                                          
*                                                                               
ENT12    CLI   TWASCR,X'E6'        TEST FOR DTYPE LIST                          
         BE    *+18                YES                                          
         MVI   ERROR,PARERR                                                     
         OC    OLNODE2,OLNODE2     TEST IF OUTLINE IS PARENT                    
         BNZ   ENTERRX             YES-REJECT IT                                
*                                                                               
         MVI   ELEM,KEYFOUT        SET KEY COMPONENT                            
         MVC   ELEM+2(8),BUKCODE                                                
         CLI   TWASCR,X'E5'        TEST FOR OUTLINE LIST                        
         BE    *+14                YES                                          
         MVI   ELEM,KEYFDTYP       MUST BE A DATATYPE                           
         MVC   ELEM+2(8),BUDTYP                                                 
         GOTO1 HELLO,DMCB,(C'D',=C'CORETAB'),(ELEM,SAVKEYL),0                   
*                                                                               
         LA    RE,ELEM+9                                                        
         LA    R1,8                                                             
         CLI   0(RE),C' '          FIND LENGTH OF CODE                          
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         LA    R1,2(R1)            ADD IN ELEM CODE/LENGTH                      
         STC   R1,ELEM+1           SET ELEMENT LENGTH                           
         GOTO1 HELLO,DMCB,(C'P',=C'CORETAB'),SAVKEYL,ELEM,0                     
*                                                                               
ENT14    CLI   TWASCR,X'E8'        TEST FOR TEXT LIST                           
         BNE   ENT15                                                            
         MVI   GCMODE,C'S'         YES-LOAD GENCON IN SLAVED MODE NOW           
         GOTO1 VGENCON,DMCB,(R8)                                                
         XC    CONHEAD,CONHEAD                                                  
         OI    CONHEADH+6,X'80'                                                 
*                                                                               
ENT15    MVI   PHEDIT,X'07'                                                     
         MVI   PHSCREEN,X'F7'                                                   
         MVI   OVERLAY,X'07'                                                    
         MVI   TWASCR,X'F7'                                                     
         MVI   RECNUM,RECENTRY                                                  
         MVI   ACTNUM,ACTOVER                                                   
         MVC   DMCB+4(3),SYSPHASE  LOAD ENTRY SCREEN                            
         MVC   DMCB+7(1),PHSCREEN                                               
         GOTO1 CALLOV,DMCB,CONTAGH                                              
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,CONHEADH         RE-TRANSMIT HEADER FIELDS (UK)               
         LA    RF,CONTAGH-1                                                     
         SR    RE,RE                                                            
         OI    6(R1),X'80'                                                      
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,*-8                                                        
*                                                                               
         MVC   CONREC,SPACES                                                    
         MVC   CONACT,SPACES                                                    
         MVC   CONREC(5),=C'ENTRY'                                              
         MVI   CONRECH+5,5                                                      
         OI    CONRECH+4,X'80'                                                  
*                                                                               
         MVC   CONACT(8),=C'OVERRIDE'                                           
         MVI   CONACTH+5,8                                                      
         OI    CONACTH+4,X'80'                                                  
*                                                                               
ENT16    LA    R2,CONTAGH                                                       
         BAS   RE,NEXTU            POINT R2 TO FIRST KEY FIELD                  
         LA    R3,ENTKEYTB         R3=A(KEY FIELD LIST)                         
         LA    R0,ENTKEYS          R0=COUNTER                                   
         XC    BLOCK(L'P+8),BLOCK                                               
         MVI   BLOCK,L'P+8                                                      
         MVC   BLOCK+8(L'P),SPACES                                              
         LA    R4,BLOCK+8          R4=A(KEY STRING)                             
*                                                                               
ENT18    MVC   ELCODE,0(R3)        SET KEY FIELD NUMBER                         
         GOTO1 VGETKEYF                                                         
         MVI   0(R4),C','          PUT A COMMA AFTER ITEM                       
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)            NEXT KEY FIELD                               
         BCT   R0,ENT18                                                         
*                                                                               
ENT20    LA    R0,BLOCK+8                                                       
         SR    R4,R0                                                            
         STC   R4,BLOCK+5                                                       
         GOTO1 SCUNKEY,DMCB,(C',',BLOCK),(R2)                                   
*                                                                               
ENT22    MVC   DMCB,VDUMMY         LOAD IN ENTRY OVERLAY AFTER BASE             
         MVC   DMCB+4(4),SYSPHASE                                               
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             GET A(ENTRY OVERLAY)                         
         ST    RF,AGO              SAVE OVERLAY ADDRESS                         
         MVI   MODE,VALKEY                                                      
         GOTO1 (RF),DMCB,(RC)                                                   
         MVI   MODE,VALREC                                                      
         GOTO1 (RF),DMCB,(RC)                                                   
         GOTO1 SAVEUWKA            SAVE STORAGE TO TWA1                         
         MVC   TWALREC,RECNUM                                                   
         MVC   TWALACT,ACTNUM                                                   
         L     R2,ACURFORC                                                      
         OI    6(R2),X'40'         TURN ON CURSOR BIT                           
         L     RD,AWORK                                                         
         B     ENTX                GO DIRECTLY OUT TO USER                      
*                                                                               
ENTX     XIT1                                                                   
         DROP  R4,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO ADVANCE TO NEXT UNPROTECTED FIELD                              
*                                                                               
NEXTU    ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             TEST FOR EOS                                 
         BER   RE                                                               
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    NEXTU                                                            
         LTR   RE,RE               SET CC=NEQ FOR ANOTHER FIELD                 
         BR    RE                                                               
         SPACE 2                                                                
ENTERRX  OI    CONACTH+6,X'81'     XMIT MODIFIED TO ALLOW RE-ENTRY              
         ST    R2,FADDR                                                         
         LA    R2,8(R2)                                                         
         STCM  R2,7,FLAST                                                       
         GOTO1 VCURSERR,PARAS,1                                                 
         SPACE 2                                                                
* TABLE OF KEY FIELDS ON THE ENTRY SCREEN                                       
*                                                                               
ENTKEYTB DC    AL1(KEYFCLT)                                                     
         DC    AL1(KEYFPRD)                                                     
         DC    AL1(KEYFPLAN)                                                    
         DC    AL1(KEYFOUT)                                                     
         DC    AL1(KEYFDTYP)                                                    
         DC    AL1(KEYFEOPT)                                                    
ENTKEYS  EQU   *-ENTKEYTB                                                       
         SPACE 2                                                                
* LITERAL POOL FOR ENT                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* SUB-ROUTINE TO MANAGE SWAPPING TO SUBORDINATE LIST SCREENS                    
* USING SAVED KEY FIELDS                                                        
*                                                                               
SUBL     NMOD1 0,**SUBL                                                         
         L     RC,0(R1)                                                         
         CLI   TWASCR,X'E6'        TEST FOR DTYPE LIST                          
         BE    SUBL2                                                            
         CLI   TWASCR,X'E8'        TEST FOR TEXT LIST SCREEN                    
         BE    SUBL4                                                            
         CLI   TWASCR,X'F7'        TEST FOR ENTRY                               
         BE    SUBL6                                                            
         CLI   TWASCR,X'F8'        TEST FOR TEXT MAINT                          
         BE    SUBL8                                                            
         B     SUBLX                                                            
*                                                                               
SUBL2    CLC   CONREC(5),=C'DTYPE' TEST RECORD STILL=DTYPE                      
         BNE   SUBLX                                                            
         CLC   CONACT(4),=C'LIST'  TEST ACTION STILL=LIST                       
         BNE   SUBLX                                                            
         CLI   THISPF,PF11         TEST FOR PF11=OUTLINE LIST                   
         BE    SUBL10              YES                                          
         B     SUBLX                                                            
*                                                                               
SUBL4    CLC   CONREC(4),=C'TEXT'  TEST RECORD=TEXT                             
         BNE   SUBLX                                                            
         CLC   CONACT(4),=C'LIST'  TEST ACTION STILL=LIST                       
         BNE   SUBLX                                                            
         CLI   THISPF,PF11         TEST PF11=OUTLINE LIST                       
         BE    SUBL10                                                           
         CLI   THISPF,PF12         TEST PF12=PLAN LIST                          
         BE    SUBL10                                                           
         B     SUBLX                                                            
*                                                                               
SUBL6    CLC   CONREC(5),=C'ENTRY' TEST RECORD STILL=ENTRY                      
         BNE   SUBLX                                                            
         CLC   CONACT(7),=C'DISPLAY' TEST ACTION UNCHANGED                      
         BE    *+14                                                             
         CLC   CONACT(8),=C'OVERRIDE'                                           
         BNE   SUBLX                                                            
         CLI   THISPF,PF4          TEST PF4=TEXT LIST                           
         BE    SUBL10                                                           
         CLI   THISPF,PF10         TEST PF10=DTYPE LIST                         
         BE    SUBL10                                                           
         CLI   THISPF,PF11         TEST PF11=OUTLINE LIST                       
         BE    SUBL10                                                           
         B     SUBLX                                                            
*                                                                               
SUBL8    CLC   CONREC(4),=C'TEXT'  TEST RECORD STILL=TEXT                       
         BNE   SUBLX                                                            
         CLI   THISPF,PF11         TEST PF11=OUTLINE LIST                       
         BE    SUBL10                                                           
         CLI   THISPF,PF12         TEST PF12=PLAN LIST                          
         BE    SUBL10                                                           
         B     SUBLX                                                            
*                                                                               
SUBL10   LR    RE,R7                                                            
         AH    RE,MODLAST                                                       
         LA    R2,CONACT                                                        
         CR    RE,R2               TEST SOMETHING MODIFIED ON SCREEN            
         BH    SUBLX               YES                                          
*                                                                               
SUBL12   MVI   GCMODE,C'S'                                                      
         GOTO1 VGENCON,DMCB,(R8)                                                
         MVI   GCMODE,0                                                         
*                                                                               
         MVC   CONREC,SPACES                                                    
         OI    CONRECH+4,X'80'                                                  
*                                                                               
         MVC   CONACT,SPACES                                                    
         MVC   CONACT(4),=C'LIST'                                               
         MVI   CONACTH+5,4                                                      
         OI    CONACTH+4,X'80'                                                  
*                                                                               
         XC    CONWHEN,CONWHEN     CATER FOR REPORT                             
         MVI   CONWHENH+5,0                                                     
         OI    CONWHENH+6,X'80'                                                 
*                                                                               
SUBL14   MVC   CONKEY,SPACES                                                    
         MVC   WORK,SPACES                                                      
         LA    R4,WORK             R4=A(KEY FIELD STRING)                       
*                                                                               
         MVC   CONREC(4),=C'TEXT'                                               
         MVI   CONRECH+5,4                                                      
         LA    R3,SUBLTXTB         R3=A(KEY FIELD TABLE)                        
         LA    R0,SUBLTXTS         R0=LOOP COUNTER                              
         CLI   THISPF,PF4                                                       
         BE    SUBL16                                                           
*                                                                               
         MVC   CONREC(4),=C'PLAN'                                               
         MVI   CONRECH+5,4                                                      
         LA    R3,SUBLPLTB                                                      
         LA    R0,SUBLPLNS                                                      
         CLI   THISPF,PF12         TEST PF12=PLAN LIST                          
         BE    SUBL16                                                           
*                                                                               
         MVC   CONREC(5),=C'DTYPE'                                              
         MVI   CONRECH+5,5                                                      
         LA    R3,SUBLDTTB                                                      
         LA    R0,SUBLDTYS                                                      
         CLI   THISPF,PF10         TEST PF10=DTYPE LIST                         
         BE    SUBL16                                                           
*                                                                               
         MVC   CONREC(7),=C'OUTLINE'                                            
         MVI   CONRECH+5,7                                                      
         LA    R3,SUBLOUTB         R3=A(KEY FIELD LIST)                         
         LA    R0,SUBLOUTS         R0=LOOP COUNTER                              
*                                                                               
SUBL16   MVC   ELCODE,0(R3)        GET KEY FIELD NUMBER                         
         GOTO1 VGETKEYF                                                         
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,SUBL16                                                        
*                                                                               
SUBL18   LA    R0,WORK                                                          
         SR    R4,R0               COMPUTE KEY STRING                           
         LA    R1,L'CONKEY                                                      
         CR    R4,R1               CANNOT BLOW KEY FIELD                        
         BH    *+6                                                              
         LR    R1,R4                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CONKEY(0),WORK                                                   
         LA    R1,1(R1)                                                         
         STC   R1,CONKEYH+5                                                     
         OI    CONKEYH+4,X'80'                                                  
         GOTO1 SAVEUWKA                                                         
*                                                                               
SUBLX    XIT1  ,                                                                
         SPACE 2                                                                
SUBLOUTB DC    AL1(KEYFCLT)                                                     
         DC    AL1(KEYFPRD)                                                     
         DC    AL1(KEYFPLAN)                                                    
         DC    AL1(KEYFOUT)                                                     
         DC    AL1(KEYFOOPT)                                                    
SUBLOUTS EQU   *-SUBLOUTB                                                       
*                                                                               
SUBLPLTB DC    AL1(KEYFCLT)                                                     
         DC    AL1(KEYFPRD)                                                     
         DC    AL1(KEYFPLAN)                                                    
SUBLPLNS EQU   *-SUBLPLTB                                                       
*                                                                               
SUBLDTTB DC    AL1(KEYFCLT)                                                     
         DC    AL1(KEYFPRD)                                                     
         DC    AL1(KEYFPLAN)                                                    
SUBLDTYS EQU   *-SUBLDTTB                                                       
*                                                                               
SUBLTXTB DC    AL1(KEYFCLT)                                                     
         DC    AL1(KEYFPRD)                                                     
         DC    AL1(KEYFPLAN)                                                    
         DC    AL1(KEYFOUT)                                                     
SUBLTXTS EQU   *-SUBLTXTB                                                       
         SPACE 2                                                                
* LITERAL POOL FOR SUBL                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE THE SAVED KEY FIELD TABLE.  TABLE STORES                
* SCREEN KEY FIELDS OF ALL OUTLINE AND DTYPE RELATED SCREENS                    
* TO FACILITATE SWAPPING BETWEEN THESE SCREENS.                                 
*                                                                               
UPKEY    NMOD1 0,**UKEY                                                         
         L     RC,0(R1)                                                         
         LA    R4,KEYTAB           R4=A(KEY TABLE HEADER)                       
         USING KEYHEADD,R4                                                      
         LA    RF,KEYTABX-1        RF=BXLE LIMIT                                
         SR    RE,RE               CLEAR RE=INCREMENT REGISTER                  
*                                                                               
UPKEY2   CLC   TWASCR,KEYSCR       TEST FOR MATCH ON SCREEN                     
         BE    UPKEY4              FOUND IT                                     
         ICM   RE,3,KEYDISP        GET DISP TO NEXT TABLE                       
         BXLE  R4,RE,UPKEY2                                                     
         B     UPKEYX              NOT IN TABLE                                 
*                                                                               
* BUILD KEY FIELD ELEMENTS IN BLOCK BASED ON TABLE ENTRIES                      
*                                                                               
UPKEY4   MVI   CLSAVSW,NO          DO NOT CLEAR OUT TABLE                       
         XC    BLOCK(256),BLOCK                                                 
         LA    R3,BLOCK            R3=A(KEY FIELD ELEMENT AREA)                 
         LA    R4,KEYHEADL(R4)     R4=A(KEY TABLE ENTRIES)                      
         USING KEYTABD,R4                                                       
         ST    R4,AKEYTAB          SAVE START OF TABLE                          
         XC    WORK,WORK           WORK=A(CLIENT/PRODUCT/PLAN)                  
*                                                                               
UPKEY6   CLI   KEYFLD,0            TEST FOR EOT                                 
         BE    UPKEY14             YES                                          
         TM    KEYIND,KEYCON       TEST FOR CONTROLLER HEADLINE FIELD           
         BO    UPKEY8              YES                                          
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,3,KEYNUM         R6=KEY FIELD NUMBER                          
         LA    R2,CONTAGH          POINT TO LAST FLDH BEFORE KEYS               
         BAS   RE,JUMPU            GET NEXT UNPROTECTED FIELD                   
         BCT   R6,*-4                                                           
         B     UPKEY10                                                          
*                                                                               
UPKEY8   LA    R2,CONRECH                                                       
         SR    R6,R6                                                            
         ICM   R6,3,KEYNUM         GET DISP FROM RECORD FIELD                   
         AR    R2,R6                                                            
*                                                                               
UPKEY10  GOTO1 VGETFLD,DMCB,(R2)                                                
         CLI   FLDH+5,0            TEST FOR EMPTY FIELD                         
         BE    UPKEY12             YES-TRY NEXT ENTRY                           
*                                                                               
         MVC   0(1,R3),KEYFLD      KEY FIELD NUMBER=ELEM CODE                   
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R3),FLD         GET ELEM DATA                                
         LA    R1,3(R1)                                                         
         STC   R1,1(R3)                                                         
         LA    R3,0(R1,R3)         NEXT ELEM POSITION                           
*                                                                               
         LA    RE,WORK             RE=A(CLT/PRD/PLAN POSITION)                  
         CLI   KEYFLD,KEYFCLT      TEST FOR CLIENT                              
         BE    UPKEY11             YES                                          
         LA    RE,3(RE)                                                         
         CLI   KEYFLD,KEYFPRD      TEST FOR PRODUCT                             
         BE    UPKEY11             YES                                          
         LA    RE,3(RE)                                                         
         CLI   KEYFLD,KEYFPLAN     TEST FOR PLAN                                
         BNE   UPKEY12                                                          
*                                                                               
UPKEY11  MVC   0(3,RE),FLD         SAVE CODE                                    
*                                                                               
UPKEY12  LA    R4,KEYTABL(R4)      BUMP TABLE ENTRY POINTER                     
         B     UPKEY6                                                           
*                                                                               
* UPDATE THE SAVED KEY TABLE USING THE NEW ELEMENTS AT BLOCK                    
*                                                                               
UPKEY14  OC    SAVCNTRL,SAVCNTRL   TEST FOR FIRST TIME                          
         BZ    *+14                YES                                          
         CLC   SAVCNTRL,WORK       TEST CHANGE IN CLT/PRD/PLAN                  
         BE    UPKEY15             NO                                           
*                                                                               
         MVC   SAVCNTRL,WORK       UPDATE PLAN CONTROLS                         
         XC    SAVKEYS,SAVKEYS     CLEAR OUT SAVED KEYS                         
         MVC   SAVKEYL,=H'3'       INITIALIZE TABLE LENGTH                      
         B     UPKEY20             ADD ENTRIES TO TABLE                         
*                                                                               
UPKEY15  L     R4,AKEYTAB          RESTORE TABLE POINTER                        
*                                                                               
UPKEY16  CLI   KEYFLD,0            TEST FOR EOT                                 
         BE    UPKEY20                                                          
         GOTO1 HELLO,DMCB,(C'D',=C'CORETAB'),(KEYFLD,SAVKEYL),0                 
         LA    R4,KEYTABL(R4)                                                   
         B     UPKEY16                                                          
*                                                                               
UPKEY20  LA    R3,BLOCK            R3=A(KEY FIELD ELEMENTS)                     
*                                                                               
UPKEY22  CLI   0(R3),0             TEST FOR EOT                                 
         BE    UPKEYX                                                           
         GOTO1 HELLO,DMCB,(C'P',=C'CORETAB'),SAVKEYL,(R3),0                     
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     UPKEY22                                                          
*                                                                               
UPKEYX   XIT1  ,                                                                
         SPACE 2                                                                
JUMPU    ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             TEST FOR EOS                                 
         BER   RE                                                               
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    JUMPU                                                            
         LTR   RE,RE               SET CC=NEQ FOR ANOTHER FIELD                 
         BR    RE                                                               
         SPACE 2                                                                
* LITERAL POOL FOR UPKEY                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF SCREEN KEY FIELDS                                                    
*                                                                               
KEYTAB   DS    0D                                                               
*                                                                               
KEYTABE5 DC    X'E5',AL2(KEYTABF5-KEYTABE5)                                     
*                                  OUTLINE LIST                                 
         DC    AL1(KEYFCLT,0),AL2(1)                                            
         DC    AL1(KEYFPRD,0),AL2(2)                                            
         DC    AL1(KEYFPLAN,0),AL2(3)                                           
         DC    AL1(KEYFOUT,0),AL2(4)                                            
         DC    AL1(KEYFOOPT,0),AL2(5)                                           
         DC    X'00'               EOT                                          
*                                                                               
KEYTABF5 DC    X'F5',AL2(KEYTABE6-KEYTABF5)                                     
*                                  OUTLINE MAINT                                
         DC    AL1(KEYFCLT,0),AL2(1)                                            
         DC    AL1(KEYFPRD,0),AL2(2)                                            
         DC    AL1(KEYFPLAN,0),AL2(3)                                           
         DC    AL1(KEYFOUT,0),AL2(4)                                            
         DC    X'00'               EOT                                          
*                                                                               
KEYTABE6 DC    X'E6',AL2(KEYTABF6-KEYTABE6)                                     
*                                  DTYPE LIST                                   
         DC    AL1(KEYFCLT,0),AL2(1)                                            
         DC    AL1(KEYFPRD,0),AL2(2)                                            
         DC    AL1(KEYFPLAN,0),AL2(3)                                           
         DC    X'00'               EOT                                          
*                                                                               
KEYTABF6 DC    X'F6',AL2(KEYTABF7-KEYTABF6)                                     
*                                  DTYPE MAINT                                  
         DC    AL1(KEYFCLT,0),AL2(1)                                            
         DC    AL1(KEYFPRD,0),AL2(2)                                            
         DC    AL1(KEYFPLAN,0),AL2(3)                                           
         DC    X'00'               EOT                                          
*                                                                               
KEYTABF7 DC    X'F7',AL2(KEYTABE8-KEYTABF7)                                     
*                                  DATA ENTRY                                   
         DC    AL1(KEYFCLT,0),AL2(1)                                            
         DC    AL1(KEYFPRD,0),AL2(2)                                            
         DC    AL1(KEYFPLAN,0),AL2(3)                                           
         DC    AL1(KEYFOUT,0),AL2(4)                                            
         DC    AL1(KEYFDTYP,0),AL2(5)                                           
         DC    AL1(KEYFEOPT,0),AL2(6)                                           
         DC    X'00'               EOT                                          
*                                                                               
KEYTABE8 DC    X'E8',AL2(KEYTABF8-KEYTABE8)                                     
*                                  TEXT LIST                                    
         DC    AL1(KEYFCLT,0),AL2(1)                                            
         DC    AL1(KEYFPRD,0),AL2(2)                                            
         DC    AL1(KEYFPLAN,0),AL2(3)                                           
         DC    AL1(KEYFOUT,0),AL2(4)                                            
         DC    X'00'               EOT                                          
*                                                                               
KEYTABF8 DC    X'F8',AL2(KEYTABB0-KEYTABF8)                                     
*                                  TEXT MAINT                                   
         DC    AL1(KEYFCLT,0),AL2(1)                                            
         DC    AL1(KEYFPRD,0),AL2(2)                                            
         DC    AL1(KEYFPLAN,0),AL2(3)                                           
         DC    AL1(KEYFOUT,0),AL2(4)                                            
*&&US*&& DC    AL1(KEYFTEXT,0),AL2(7)                                           
*&&UK*&& DC    AL1(KEYFTEXT,0),AL2(5)                                           
         DC    X'00'               EOT                                          
*                                                                               
KEYTABB0 DC    X'B0',AL2(KEYTABX-KEYTABB0)                                      
*                                  DATA EXTRACT                                 
         DC    AL1(KEYFWHEN,KEYCON),AL2(CONWHENH-CONRECH)                       
         DC    AL1(KEYFCLT,0),AL2(1)                                            
         DC    AL1(KEYFPRD,0),AL2(2)                                            
         DC    AL1(KEYFPLAN,0),AL2(3)                                           
         DC    AL1(KEYFOUT,0),AL2(4)                                            
         DC    AL1(KEYFYR,0),AL2(6)                                             
         DC    AL1(KEYFPER,0),AL2(7)                                            
         DC    X'00'               EOT                                          
*                                                                               
KEYTABX  DC    X'00'               EOT                                          
         EJECT                                                                  
*                                                                               
RECACTS  DS    0D                                                               
         SPACE 1                                                                
* X'01' ENTRIES ARE AVAILABLE RECORDS                                           
*                                                                               
* CL8 EXPANDED RECORD NAME                                                      
* CL1 RECORD NUMBER                                                             
* CL1 PHASE NUMBER FOR DATA DICTIONARY                                          
* CL1 PHASE NUMBER FOR HELP SCREEN                                              
         SPACE 1                                                                
         DC    X'01',C'AGENCY  ',AL1(01),X'00C2'                                
         DC    X'01',C'CLIENT  ',AL1(02),X'00C2'                                
         DC    X'01',C'PRODUCT ',AL1(03),X'00C2'                                
         DC    X'01',C'PLAN    ',AL1(04),X'00C2'                                
         DC    X'01',C'OUTLINE ',AL1(05),X'00C2'                                
         DC    X'01',C'DTYPE   ',AL1(06),X'00C2'                                
         DC    X'01',C'ENTRY   ',AL1(07),X'00C2'                                
         DC    X'01',C'TEXT    ',AL1(08),X'00C2'                                
         DC    X'01',C'DATA    ',AL1(09),X'00C2'                                
         DC    X'01',C'TEST    ',AL1(10),X'00C2'                                
         DC    X'01',C'JOB     ',AL1(11),X'00C2'                                
         SPACE 2                                                                
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'OVERRIDE',AL1(08,04,00)                                  
         DC    X'02',C'RENAME  ',AL1(09,05,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'LOOK    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'COPY    ',AL1(20,20,00)                                  
         DC    X'02',C'EXTRACT ',AL1(32,32,00)                                  
         DC    X'02',C'SNAPSHOT',AL1(34,34,00)                                  
         DC    X'02',C'TRANSFER',AL1(36,36,00)                                  
         DC    X'02',C'DDS     ',AL1(38,38,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         SPACE 2                                                                
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
         SPACE 1                                                                
* X'03' ENTRIES ARE OK REC/ACT COMBOS                                           
* CL1 RECORD NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 PHASE NUMBER FOR SCREEN                                                   
* CL1 PHASE NUMBER FOR EDIT                                                     
* CL1 PHASE NUMBER FOR SPECS                                                    
* CL1 PHASE NUMBER FOR REPORT                                                   
* CL1 WHEN OK BITS 80=SCREEN 40=NOW 20=SOON 10=OV 08=DDS                        
* CL2 CODE FOR REPORTS                                                          
* CL2 CODE FOR EOD HANDLING                                                     
         SPACE 1                                                                
*                                 SC  SP  AC                                    
*                                   OV  RP                                      
         DC    X'03',AL1(01,01),X'F1010000C0',C'    '  AGENCY   MAINT           
         DC    X'03',AL1(01,10),X'E101000080',C'    '           LIST            
         DC    X'03',AL1(02,01),X'F2020000C0',C'    '  CLIENT   MAINT           
         DC    X'03',AL1(02,10),X'E202000280',C'    '           LIST            
         DC    X'03',AL1(02,12),X'F202000278',C'CLCL'           REPORT          
         DC    X'03',AL1(03,01),X'F3030000C0',C'    '  PRODUCT  MAINT           
         DC    X'03',AL1(03,10),X'E303000380',C'    '           LIST            
         DC    X'03',AL1(03,12),X'F303000378',C'PRPR'           REPORT          
         DC    X'03',AL1(04,01),X'F4040000C0',C'    '  PLAN     MAINT           
         DC    X'03',AL1(04,10),X'E404000480',C'    '           LIST            
         DC    X'03',AL1(04,12),X'F404000478',C'PLPL'           REPORT          
         DC    X'03',AL1(04,20),X'C414001418',C'PCPC'           COPY            
         DC    X'03',AL1(05,01),X'F5050000C0',C'    '  OUTLINE  MAINT           
         DC    X'03',AL1(05,10),X'E515001580',C'    '           LIST            
         DC    X'03',AL1(05,12),X'F505000578',C'OUOU'           REPORT          
*&&US*&& DC    X'03',AL1(05,20),X'C525002581',C'    '           COPY            
         DC    X'03',AL1(06,01),X'F6060000C0',C'    '  DTYPE    MAINT           
         DC    X'03',AL1(06,10),X'E606000680',C'    '           LIST            
         DC    X'03',AL1(06,12),X'F606000678',C'DTDT'           REPORT          
         DC    X'03',AL1(06,20),X'C6160000C0',C'    '           COPY            
         DC    X'03',AL1(07,01),X'F7070000C0',C'    '  ENTRY    DIS             
         DC    X'03',AL1(07,04),X'F7070000C0',C'    '           OVER            
         DC    X'03',AL1(08,01),X'F8080000C0',C'    '  TEXT     MAINT           
         DC    X'03',AL1(08,10),X'E808000880',C'    '           LIST            
         DC    X'03',AL1(08,12),X'F808000878',C'TXTX'           REPORT          
         DC    X'03',AL1(09,32),X'B040004038',C'EXEX'  DATA     EXTRACT         
         DC    X'03',AL1(09,34),X'B130003018',C'SNSN'           SNAP            
         DC    X'03',AL1(09,36),X'B231003138',C'TRTR'           TRANS           
         DC    X'03',AL1(10,38),X'FA0A000A18',C'TETE'  TEST     DDS             
         DC    X'03',AL1(11,36),X'B535000081',C'    '  JOB      XFER            
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER OUTLINE LIST SCREEN                                            
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILE5D                                                       
         EJECT                                                                  
* LIST SCREEN PRE-EDIT VALUES                                                   
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
LSTVALS  DS    0CL30                                                            
LSTAFROM DS    A                   A(FROM FIELD)                                
LSTATO   DS    A                   A(TO FIELD)                                  
LSTFROMN DS    X                   FROM FIELD NUMBER                            
LSTTON   DS    X                   TO FIELD NUMBER                              
LSTOTH   DS    C                   Y=SELECT FIELDS PRESENT                      
LSTREST  DS    C                   Y=SAVE STORAGE HAS BEEN RESTORED             
LSTSVRER DS    C                   SAVED NDREREAD VALUE BEFORE REPO             
         DS    CL(L'LSTVALS-(*-LSTVALS)) SPARE                                  
         SPACE 2                                                                
* DSECT TO COVER KEY FIELD TABLES                                               
*                                                                               
KEYHEADD DSECT                     **KEY TABLE HEADER**                         
KEYSCR   DS    X                   SCREEN NUMBER (X'00'=EOT)                    
KEYDISP  DS    XL2                 DISPLACEMENT TO NEXT HEADER                  
KEYHEADL EQU   *-KEYHEADD                                                       
         SPACE 1                                                                
KEYTABD  DSECT                     **KEY FIELD TABLE ENTRY**                    
KEYFLD   DS    X                   FIELD NUMBER                                 
KEYIND   DS    X                   INDICATOR                                    
KEYCON   EQU   X'80'               CONTROLLER HEADLINE FIELD                    
KEYNUM   DS    XL2                 KEY FIELD NUMBER OR                          
*                                  DISP INTO CONTROLLER HEADER                  
KEYTABL  EQU   *-KEYTABD                                                        
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
PF1      EQU   1                                                                
PF2      EQU   2                                                                
PF3      EQU   3                                                                
PF4      EQU   4                                                                
PF5      EQU   5                                                                
PF6      EQU   6                                                                
PF7      EQU   7                                                                
PF8      EQU   8                                                                
PF9      EQU   9                                                                
PF10     EQU   10                                                               
PF11     EQU   11                                                               
PF12     EQU   12                                                               
*                                                                               
KEYFCLT  EQU   1                   CLIENT                                       
KEYFPRD  EQU   2                   PRODUCT                                      
KEYFPLAN EQU   3                   PLAN                                         
KEYFOUT  EQU   4                   OUTLINE                                      
KEYFDTYP EQU   5                   DATATYPE                                     
KEYFTEXT EQU   6                   TEXT CODE                                    
*                                                                               
KEYFOOPT EQU   10                  OUTLINE OPTIONS                              
KEYFEOPT EQU   11                  ENTRY OPTIONS                                
*                                                                               
KEYFMODE EQU   20                  EXTRACT MODE                                 
KEYFYR   EQU   21                  EXTRACT YEAR                                 
KEYFPER  EQU   22                  EXTRACT PERIOD                               
*                                                                               
KEYFWHEN EQU   100                 HEADER WHEN                                  
         SPACE 2                                                                
* CTGENFILE                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDACTIVD                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDACTIVD                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* FAFACTS                                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 2                                                                
* FATIOB                                                                        
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
       ++INCLUDE SPSTAPACKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025BUFIL00   05/01/02'                                      
         END                                                                    
