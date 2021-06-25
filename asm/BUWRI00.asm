*          DATA SET BUWRI00    AT LEVEL 056 AS OF 05/01/02                      
*PHASE T50300A                                                                  
*INCLUDE MONVAL                                                                 
*INCLUDE BURNEM                                                                 
*INCLUDE BUILDER                                                                
*INCLUDE KHDUMMY                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  T50300 - BUDGET WRITER CONTROLLER                                  *         
*                                                                     *         
*  CALLED FROM: MONITOR                                               *         
*                                                                     *         
*  CALLS TO:    GENCON                                                *         
*                                                                     *         
*  INPUTS: SCREEN BUWRIFF (T503FF)                                    *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - TWA                                                   *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - SECOND BASE                                           *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'T50300 - BUDGET WRITER CONTROLLER'                              
T50300   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**T50300,RA,RR=R3,CLEAR=YES                              
         USING GEND,RC                                                          
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
*                                                                               
         ST    R3,RELO                                                          
         LR    R8,RC               R8=A(SPOOLD)                                 
         LA    RC,SPOOLEND         RC=A(GENCON STORAGE)                         
         LA    R9,IO               START OF IO 1 + TOTAL IO AREA LEN            
         AH    R9,=Y(LENIOAS)      R9=A(BUDGET SYSTEM WORKNG STORAGE)           
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
         ST    RD,SYSRD            SET RD SO GENCON WILL RETURN                 
*                                                                               
         L     RF,CALLOV           RF=V(CALLOV)                                 
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A30'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF               GO TO CALLOV                                 
         MVC   VGENCON,DMCB        SET A(GENERAL CONTROLLER)                    
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A70'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF               GO TO CALLOV                                 
         MVC   VNODIO,DMCB         SET A(NODIO)                                 
*                                                                               
         GOTO1 VGENCON,DMCB,(R8)   CALL GENCON                                  
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* INITIALIZE SYSTEM DEPENDENT VALUES                                            
*                                                                               
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
*                                                                               
         LA    R2,SYSV                                                          
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
         LA    R2,ADTAB            SET ADCONS FOR EXTENDED ADDRESSING           
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
         L     RF,ANODBLK                                                       
         SH    RF,=H'8'            BACK UP FOR LABEL                            
         MVC   0(8,RF),=C'*NODBLK*'                                             
         EJECT                                                                  
* SET SYSTEM DEPENDENT VALUES                                                   
*                                                                               
         LA    R1,TWA1SAVE         LET GENCON KEEP SAVE STORAGE IN TWA0         
         ST    R1,ASTARTSV                                                      
         MVC   LSVTWA0,=Y(TWA1SAVX-TWA1SAVE)                                    
*                                                                               
         MVI   SYSTEM,C'B'         BUDGET                                       
         MVI   GETMSYS,22          USES GETMSG FOR SYSTEM 22                    
*                                                                               
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2000 BYTES                       
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
         MVC   SYSPHASE,=X'D9050300'    PRESET FOR SYSTEM CALLOVS               
         L     R1,=A(RECACTS)           RECORD/ACTION DIRECTORY                 
         A     R1,RELO                                                          
         ST    R1,ARECACT                                                       
         MVI   FILTIDNO,60              FILTER ID NUMBER IS 60                  
*                                                                               
* SET UP CERTAIN ROUTINE ADDRESSES-  (CANT WAIT FOR GENCON)                     
*                                                                               
         L     R1,SYSPARMS                                                      
         LM    R2,R4,8(R1)         A(SYSLIST) A(TIA) A(COMFACS)                 
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         USING ACCFACSD,R2                                                      
         MVC   VRECUP,ARECUP                                                    
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                                
*                                                                               
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         L     R7,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD            RESTORE POINTER TO SYSTEM STORAGE            
         L     RA,SYSRA            2ND BASE REG - R9 MUST BE SET FIRST          
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
*                                                                               
VBRANCH  B     GETAGY                                                           
         B     GETFLD                                                           
         B     CLEARF                                                           
         B     FVAL                                                             
         B     NODERR                                                           
         B     ACTIV               ADDACTV                                      
         B     ACTIV1              CHAACTV                                      
         B     SETADD                                                           
         B     GETVAL                                                           
         B     SCANERR                                                          
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
* GETAGY - GET AGENCY DATA FROM CONTROL FILE USER ID RECORD                     
*                                                                               
GETAGY   CLI   SVUSRNAM,C' '       DO ONE TIME ONLY                             
         BNH   GETAGY1                                                          
         MVC   USERNAME,SVUSRNAM   GET SAVED AGENCY NAME/ADDRESS                
         MVC   USERADDR,SVUSRADD                                                
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
GETAGY2  MVI   SVUSRNAM,C'*'                                                    
         GOTO1 HELLO,PARAS,(C'G',FILENAME),(X'36',AIO),0                        
         CLI   12(R1),0                                                         
         BNE   GETAGY4                                                          
         L     R6,12(R1)                                                        
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSRNAM,USERNAME                                                
         MVC   SVUSRADD,USERADDR                                                
*                                                                               
GETAGY4  XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         BAS   RE,INITNOD                                                       
*                                                                               
GETAGYX  B     XIT                                                              
         DROP  R6                                                               
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
*                                                                               
         MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
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
*                                                                               
GETFLD1  CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLD2                                                          
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,GETFLD1                                                       
*                                                                               
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
*                                                                               
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
*                                                                               
GETFLD4  NI    FLDH+4,X'FF'-X'08'  TURN OFF NUMERIC BIT                         
*                                                                               
GETFLDX  MVI   FZERO,NO            TURN OFF ZERO FILL SWITCH                    
         XIT1  REGS=(R0)                                                        
*                                                                               
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
*                                                                               
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
*                                                                               
CLEARF4  LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    CLEARF2             NO-CONTINUE                                  
         B     XIT                 YES-ALL DONE                                 
*                                                                               
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
*                                                                               
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
*                                                                               
NODERRTB DS    0X                                                               
         DC    AL1(LEVERR)         NDLEVERR                                     
         DC    AL1(0)              NDCDLERR                                     
         DC    AL1(NOTFOUND)       NDRNFERR                                     
         DC    AL1(DUPLICAT)       NDRENERR                                     
         DC    AL1(0)              NDPMDERR                                     
         DC    AL1(TOOLONG)        NDOVFERR                                     
         DC    AL1(0)                                                           
         DC    AL1(0)              NDRESERR                                     
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
*                                                                               
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
*                                                                               
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
         MVC   NDIOA,AIO3          NORMALLY USE 2ND AND 3RD IOAREAS             
         MVC   NDIOA2,AIO2                                                      
         MVI   NDUPDTSW,0          CLEAR READ FOR UPDATE SWITCH                 
         MVI   NDDELRSW,0                                                       
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
*                                                                               
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
*                                                                               
         USING BUPROD,R6                                                        
         MVC   PRDCODE,BUKCODE                                                  
         MVC   PRDNAM,BUPRONAM                                                  
         B     XIT                                                              
*                                                                               
GETVAL6  MVI   ELCODE,BUPLNELQ     PLAN RECORD                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING BUPLND,R6                                                        
         MVC   PLANCODE,BUKCODE                                                 
         MVC   PLANNAM,BUPLNNAM                                                 
         MVC   PLANST,BUPLNST      PLAN START/END (YM)                          
         MVC   PLANEND,BUPLNEND                                                 
         MVC   PLANCNT,BUPLNCNT    OUTLINE RECORD COUNT                         
         MVC   PLANLOW,BUPLNLOW                                                 
         B     XIT                                                              
*                                                                               
GETVAL8  MVI   ELCODE,BUOUTELQ     OUTLINE RECORD                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING BUOUTD,R6                                                        
         MVC   OUTCOD,BUKCODE                                                   
         MVC   OUTNAM,BUOUTNAM                                                  
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* SCANERR - SET CURSOR TO ERROR POSITION AND EXIT TO ERROR ROUTINE              
*                                                                               
* AT ENTRY, P1 BYTE  1   = LENGTH OF 2ND HALF OF SCANNER FIELDS                 
*              BYTES 2-4 = A(SCANNER BLOCK)                                     
*           P2 BYTE  1   = 0 -- GOTO1 ERREX                                     
*                        = 2 -- GOTO1 ERREX2                                    
*              BYTES 2-4 = A(XL1 CONTAINING INVALID FIELD NUMBER)               
*           R2 POINTS TO OFFENDING FIELD HEADER                                 
*                                                                               
SCANERR  SR    R4,R4                                                            
         ICM   R4,7,1(R1)          A(SCANNER BLOCK)                             
         ZIC   RE,0(R1)            LENGTH OF 2ND HALF OF SCANNER FIELDS         
         LA    RE,22(RE)           TOTAL LENGTH OF EACH SCANNER FIELD           
         LA    R3,8(R2)            A(NEW CURSOR POSITION)                       
         SR    R5,R5                                                            
         MVC   BYTE,4(R1)          ERROR ROUTINE SWITCH                         
         ICM   R5,7,5(R1)          A(INVALID FIELD NUMBER)                      
         CLI   0(R5),1             TEST FIRST FIELD IS INVALID                  
         BE    SC100               CURSOR NEED NOT BE ADJUSTED                  
         LA    RF,1                                                             
*                                                                               
SC20     ZIC   R1,0(R4)            LENGTH OF FIRST HALF OF FIELD                
         LA    R3,1(R1,R3)         ADD LENGTH TO POSITION PLUS ','              
         LA    RF,1(RF)                                                         
         CLM   RF,1,0(R5)                                                       
         BE    SC100                                                            
*                                                                               
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    SC40                LENGTH OF SECOND HALF OF FIELD               
         LA    R3,1(R1,R3)         ADD LENGTH TO POSITION PLUS '='              
         LA    RF,1(RF)                                                         
         CLM   RF,1,0(R5)                                                       
         BE    SC100                                                            
*                                                                               
SC40     LA    R4,0(RE,R4)         NEXT SCANNER ENTRY                           
         B     SC20                                                             
*                                                                               
SC100    L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         LR    RF,R2               COMPUTE DISPLACEMENT OF ERROR FLDH           
         S     RF,ATWA             FROM TWA START                               
         STCM  RF,3,TIOBCURD                                                    
         LR    RF,R2                                                            
         LA    RF,8(RF)            RF=A(FIELD START)                            
         SR    R3,RF               COMPUTE INDEX INTO FIELD FOR CURSOR          
         STC   R3,TIOBCURI                                                      
         OI    TIOBINDS,TIOBSETC                                                
*                                                                               
         CLI   BYTE,0              GO TO PROPER ERROR ROUTINE                   
         BE    SC200                                                            
         CLI   BYTE,2                                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 ERREX2                                                           
SC200    GOTO1 ERREX                                                            
         DROP  R1                                                               
         EJECT                                                                  
* INITNOD - INITIALIZE NODIO BLOCK AND READ MASTER RECORD                       
*                                                                               
INITNOD  NTR1                                                                   
         L     R5,ANODBLK                                                       
         USING NODBLKD,R5                                                       
*                                                                               
         LR    RE,R5                                                            
         LA    RF,NODBLKX-NODBLK                                                
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
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
         MVC   BUKAGY,TWAAGY                                                    
         MVI   BUKRTYP,BUKRTYPQ                                                 
*                                                                               
         MVC   NDIOA2,AIO3         READ MASTER RECORD INTO AIO2                 
         MVC   NDIOA,AIO2                                                       
         MVC   NODKEY,SPACES                                                    
         GOTO1 VNODIO,DMCB,ANODBLK,=C'READ',NODKEY                              
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   NDIOA,AIO3          RESET TO READ INTO AIO3                      
         MVC   NDIOA2,AIO2                                                      
*                                                                               
INITNODX B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
* SUPPORTING SMALL SUB-ROUTINES                                                 
*                                                                               
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 5                                                                
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
         SPACE 5                                                                
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
XIT      XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(MONVAL)                                                        
         DC    V(BURNEM)                                                        
         DC    V(BUILDER)                                                       
         DC    V(DUMMY)                                                         
*                                                                               
NVTYPES  EQU   (*-SYSVCON)/4                                                    
*                                                                               
ADTAB    DS    0AL3                                                             
         DC    AL3(NODBLK-SYSD)                                                 
NADCONS  EQU   (*-ADTAB)/L'ADTAB                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
RECACTS  DS    0D                                                               
*                                                                               
* X'04' ENTRIES ARE SAVED PROGRAM RECORDS                                       
*                                                                               
* CL8 EXPANDED PROGRAM NAME                                                     
* CL1 RECORD NUMBER                                                             
* CL1 PHASE NUMBER FOR DATA DICTIONARY                                          
* CL1 PHASE NUMBER FOR HELP SCREEN                                              
*                                                                               
         DC    X'04',C'BUDGET  ',AL1(01),X'0000'                                
         DC    X'04',C'XBUDGET ',AL1(02),X'0000'                                
*                                                                               
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
*                                                                               
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
*                                                                               
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
*                                                                               
*                                 SC  SP  AC                                    
*                                   OV  RP                                      
         DC    X'03',AL1(01,12),X'F202000238',C'WRWR'  BUDGET   REPORT          
         DC    X'03',AL1(02,12),X'F404000438',C'WRWR'  XBUDGET  REPORT          
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE BUWRIWORKD                                                     
*                                                                               
* CTGENFILE                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
* DDCOMFACS                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*                                                                               
* DDACCFACS                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDACCFACS                                                      
         PRINT ON                                                               
*                                                                               
* DDACTIVD                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDACTIVD                                                       
         PRINT ON                                                               
*                                                                               
* FAFACTS                                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*                                                                               
* FATIOB                                                                        
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056BUWRI00   05/01/02'                                      
         END                                                                    
