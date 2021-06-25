*          DATA SET PLLFM00    AT LEVEL 002 AS OF 05/01/02                      
*PHASE T50200,+0                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE MOBILE                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T50200 - PLAN FILE MAINTENANCE'                                 
T50200   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**T50200,RA,RR=R2,CLEAR=YES                              
         LR    R8,RC               R8=A(SPOOLD)                                 
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND         RC=A(GENCON STORAGE)                         
         USING GEND,RC                                                          
         ST    R1,SYSPARMS         SAVE A(FACPAK PARMS)                         
         MVC   ATWA,4(R1)                                                       
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         LA    R9,IO                                                            
         AH    R9,=Y(NIOS*LIOS)                                                 
         LA    R9,NIOS*8(R9)       R9=A(SYSTEM GLOBAL WORKING STORAGE)          
         USING SYSD,R9                                                          
*                                                                               
         ST    R2,RELO                                                          
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RA,SYSRA            SECOND BASE REGISTER                         
         ST    RD,SYSRD                                                         
         L     R2,ACOMFACS                                                      
         USING COMFACSD,R2                                                      
         L     RF,CCALLOV          A(CALLOV)                                    
         ST    RF,CALLOV                                                        
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A30'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF               GO TO CALLOV                                 
         L     RF,DMCB             PICK UP A(GENERAL CONTROLLER)                
         GOTO1 (RF),DMCB,(R8)      GO THERE - PASS A(WORKING STORAGE)           
         B     XIT                 THEN WE'RE THROUGH                           
         DROP  R2                                                               
         EJECT                                                                  
*              INITIALIZE SYSTEM DEPENDENT VALUES                               
         SPACE 3                                                                
SYSINIT  NTR1                                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
*                                  GET TERMINAL VALUES                          
         L     RE,ATWA             RE=A(TWA)                                    
         USING TWAD,RE                                                          
         MVI   DDS,NO                                                           
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,YES                                                          
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAUSRID                                                  
         MVC   AGENCY,TWAAGY                                                    
*                                                                               
         LA    R2,BASEFACS                                                      
         LA    R3,SYSVCON                                                       
         LA    R4,VCOUNT                                                        
         SPACE 1                                                                
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,BASECOM                                                       
         LA    R5,VCOUNT                                                        
         SPACE 1                                                                
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
         SPACE 1                                                                
*                                                                               
SYS6     LA    R2,CORETAB          OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
SYS7     MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,SYS7                                                          
*                                                                               
SYS8     LA    R2,ADTAB            SET ADCONS FOR EXTENDED ADDRESSING           
         LA    R0,EXADCONS         COUNTER                                      
         LA    R3,EXTADS           POINT TO FIRST EXTENDED ADCON                
*                                                                               
SYS9     ICM   R1,7,0(R2)          GET DISPLACEMENT                             
         LA    R1,SYSD(R1)         INDEX INTO STORAGE                           
         ST    R1,0(R3)                                                         
         LA    R2,L'ADTAB(R2)      NEXT TABLE ENTRY                             
         LA    R3,4(R3)            NEXT WORKING STORAGE FIELD                   
         BCT   R0,SYS9                                                          
*                                                                               
SYS10    DS    0H                  SET SYSTEM DEPENDENT VALUES                  
*        ST    R1,ASTARTSV         SET START OF SAVED VALUES                    
         MVI   SYSTEM,C'B'         BUDGET                                       
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 1024 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,VGETAGY     ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=Y(L'BUKEY)    DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=Y(L'BUKCTL)                                             
         MVC   DATADISP,=Y(BUFRSTEL-BUKEY)                                      
         MVC   SYSFIL,=C'PLANFILE'                                              
         MVC   SYSDIR,=C'PLANDIR '                                              
         MVI   GETMSYS,5           USES GETMSG FOR SYSTEM 5                     
         MVC   LWORK,=AL4(LENWORK)                                              
         MVC   RCPROG(2),=C'PL'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9050200'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                   
         SPACE 3                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         L     RA,SYSRA            SECOND BASE = RA                             
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     GETAGY                                                           
         B     GETFLD                                                           
         B     CLEARF                                                           
         B     FVAL                                                             
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
* GETAGY - GET AGENCY DATA FROM CONTROL FILE USER ID RECORD                     
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
GETAGY   MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),USERID                                               
         GOTO1 READ                                                             
         GOTO1 HELLO,PARAS,(C'G',FILENAME),(X'36',AIO),0                        
         CLI   12(R1),0            TEST FOR ELEMENT FOUND                       
         BNE   GETAGY2                                                          
         L     R6,12(R1)                                                        
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         SPACE 1                                                                
GETAGY2  XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* GETFLD - EXTRACT DATA FROM SCREEN FIELD                                       
*                                                                               
* ON ENTRY R1 POINTS TO FIELD HEADER                                            
* ON EXIT                                                                       
*        FADDR = A(FIELD HEADER)                                                
*        FLDH  CONTAINS FIELD HEADER                                            
*        FLD   CONTAINS EXTRACTED FIELD DATA SPACE FILLED                       
*        R0    CONTAINS BINARY VALUE OF DATA IF FIELD IS NUMERIC                
*                                                                               
GETFLD   ST    R1,FADDR                                                         
         LR    R2,R1                                                            
         MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         MVC   FLDH,0(R2)                                                       
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL WITH SPACES                             
         SR    R0,R0               PRE-CLEAR REGISTER FOR NUMERIC VALUE         
         ZIC   R1,FLDH                                                          
         SH    R1,=H'9'                                                         
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
         BZ    GETFLDX                                                          
         TM    FLDH+4,X'08'        TEST FOR NUMERIC FIELD                       
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
GETFLDX  XIT1  REGS=(R0)                                                        
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
*              = ZERO TO FORCE EDIT TO START AT FLAST                           
*        FTERM = LIST OF UP TO 6 SCAN TERMINATORS ENDED BY X'00'                
*                                                                               
* ON EXIT                                                                       
*        FLDH  = FIELD HEADER BUILT BY FVAL - CONTAINS DATA LENGTH              
*                AND VALIDITY BITS                                              
*        FLD   = EXTRACTED DATA STRING IN SPACE FILLED FIELD                    
*        FSTOP = STOP CHARACTER FOUND BY FVAL OR X'FF' FOR NO MORE DATA         
*        DUB   = CONTAINS PACKED VALUE OF DATA STRING FOR NUMERIC FIELD         
*                                                                               
FVAL     XC    FLDH,FLDH           CLEAR OUTPUT FIELD HEADER                    
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL OUTPUT FIELD WITH SPACES                
         MVI   FLDH,L'FLDH+L'FLD   SET DUMMY HEADER LENGTH                      
         MVI   FSTOP,X'FF'         SET FSTOP TO NO DATA FOUND                   
         L     R2,FADDR                                                         
         ZIC   R3,0(R2)                                                         
         SH    R3,=H'8'            R3 CONTAINS FIELD DATA LENGTH                
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
         CLI   FLEN,0              TEST IF RE-EDITING                           
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
         TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
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
         B     XIT                                                              
         EJECT                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
* TABLE OF RELOCATABLE ADDRESSES                                                
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
         DC    V(GETBROAD)                                                      
         DC    V(MOBILE)                                                        
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 2                                                                
* TABLE OF EXTRA CORE-RESIDENT PHASE LOOKUPS                                    
*                                                                               
CORETAB  DS    0C                                                               
         DC    X'70'                                                            
CORES    EQU   *-CORETAB                                                        
         SPACE 2                                                                
* TABLE OF DISPLACEMENTS FOR EXTENDED WORKING STORAGE                           
*                                                                               
ADTAB    DS    0AL3                                                             
         DC    AL3(NODBLKA-SYSD)                                                
         DC    AL3(OVWORK-SYSD)                                                 
EXADCONS EQU   (*-ADTAB)/L'ADTAB                                                
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE 3                                                                
RECACT   DS    0D                                                               
         SPACE 1                                                                
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
         SPACE 1                                                                
         DC    X'01',C'CLIENT  ',AL1(02),X'00C2'                                
         DC    X'01',C'PRODUCT ',AL1(03),X'00C2'                                
         DC    X'01',C'PLAN    ',AL1(04),X'00C2'                                
         DC    X'01',C'OUTLINE ',AL1(04),X'00C2'                                
         DC    X'01',C'DTYPE   ',AL1(04),X'00C2'                                
         DC    X'01',C'ENTRY   ',AL1(04),X'00C2'                                
         DC    X'01',C'TEXT    ',AL1(04),X'00C2'                                
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'COPY    ',AL1(12,12,00)                                  
         DC    X'02',C'REPORT  ',AL1(14,14,00)                                  
         DC    X'02',C'RENAME  ',AL1(16,16,00)                                  
         DC    X'02',C'ATTACH  ',AL1(18,18,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
         SPACE 3                                                                
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
         SPACE 1                                                                
         DC    X'03',AL1(02,01),X'F202000080',C'    '  PERSON    MAINT          
         DC    X'03',AL1(02,10),X'E202000280',C'    '            LIST           
         DC    X'03',AL1(02,12),X'D212001238',C'MRMM'            REPORT         
         DC    X'03',AL1(03,01),X'F303000080',C'    '  USER      MAINT          
         DC    X'03',AL1(03,10),X'E303000380',C'    '            LIST           
         DC    X'03',AL1(03,12),X'0000000378',C'URMU'            REPORT         
         DC    X'03',AL1(04,01),X'F404000080',C'    '  SYSTEM    MAINT          
         DC    X'03',AL1(04,10),X'E404000480',C'    '            LIST           
         DC    X'03',AL1(04,12),X'0000000478',C'SRMR'            REPORT         
         DC    X'03',AL1(05,01),X'F505000080',C'    '  PROJECT   MAINT          
         DC    X'03',AL1(05,10),X'E505000580',C'    '            LIST           
         DC    X'03',AL1(05,12),X'D505000578',C'PRMP'            REPORT         
         DC    X'03',AL1(05,16),X'C515001578',C'PMMP'            MANAGE         
         DC    X'03',AL1(06,01),X'F606000080',C'    '  TASK      MAINT          
         DC    X'03',AL1(06,10),X'E606000680',C'    '            LIST           
         DC    X'03',AL1(06,12),X'D606000678',C'TRMT'            REPORT         
         DC    X'03',AL1(06,14),X'D616001638',C'CAMC'            CAL            
         DC    X'03',AL1(09,01),X'F909000080',C'    '  ENTRY     MAINT          
         DC    X'03',AL1(09,10),X'E909000980',C'    '            LIST           
         DC    X'03',AL1(09,12),X'D909000978',C'MRMM'            REPORT         
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE PLLFMWRKD                                                      
         EJECT                                                                  
* FATWA                                                                         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* CTGENFILE                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PLLFM00   05/01/02'                                      
         END                                                                    
