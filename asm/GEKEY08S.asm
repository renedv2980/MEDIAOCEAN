*          DATA SET GEKEY08S   AT LEVEL 142 AS OF 05/01/02                      
*PHASE  TF0508A,*                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TF0508 - PFM INTERFACE OVERLAY FOR REP SYSTEM               *         
*                                                                     *         
*  CALLED FROM: PFM INTERFACE CONTROLLER (TF0500), WHICH CALLS        *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  OUTPUTS: KEY FOR THE RECORD TYPE ACCORDING TO USER INPUT           *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE REGISTER                                  *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
*  HISTORY OF CHANGES                                                 *         
*                                                                     *         
*  JUL01/97 (BU ) --- UPGRADE BINARY DATE ROUTINE TO ACCOUNT FOR      *         
*                     YR-2000.                                        *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         TITLE 'TF0508 - PFM INTERFACE OVERLAY FOR REP SYSTEM'                  
TF0508   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TF0508*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVC   LKEY,=H'27'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   REQFILE,=C'REPREQ '                                              
         MVI   GETMSYS,8           USES GETMSG FOR SYSTEM 8                     
         MVC   RCPROG(2),=C'RE'    PREFIX FOR REPORT NO.                        
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
*                                                                               
EXIT08   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE SCREEN                                                 *         
***********************************************************************         
VR       DS    0H                                                               
         LA    R2,CONRCRDH         POINT TO RECORD FIELD FIRST                  
         CLI   5(R2),0             NO RECORD?                                   
         BE    MISSFLD             MISSING RECORD                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LA    R4,RECTABLE                                                      
         CLI   8(R2),C'?'          USER ASKED FOR HELP?                         
         BNE   VKRECLP             NOPE,CHECK ENTRY                             
*                                                                               
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         OI    STATFLAG,X'01'      HELP INVOKED                                 
         USING CONHEADH-64,RA                                                   
         XC    CONRCRD,CONRCRD     NULL THE DATA                                
         OI    CONRCRDH+6,X'80'    TRANSMIT THE FIELD                           
         GOTO1 CLRSCN,DMCB,CONP0H  CLEAR THE SCREEN                             
         GOTO1 DISPHELP,DMCB,RECTABLE  PRINT HELP TABLE                         
         B     EXIT08                                                           
*                                                                               
VKRECLP  CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    INVLFLD                                                          
         EX    R1,VKREC10                                                       
         B     VKREC12                                                          
VKREC10  CLC   8(0,R2),0(R4)                                                    
VKREC12  BE    VKPARTS             PARTS OF THE KEY                             
         LA    R4,L'RECTABLE(R4)                                                
         B     VKRECLP                                                          
         EJECT                                                                  
VKPARTS  DS    0H                                                               
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         TM    STATFLAG,X'01'      WAS HELP INVOKED?                            
         USING CONHEADH-64,RA                                                   
         BZ    VKPARTS5                                                         
         GOTO1 CLRSCN,DMCB,CONP0H  YES, CLEAR THE SCREEN                        
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         NI    STATFLAG,X'FF'-X'01'   HELP NOT INVOKED ANYMORE                  
         USING CONHEADH-64,RA                                                   
VKPARTS5 L     R4,8(R4)            GET ADDRESS OF KEY COMPONENTS                
         L     R1,RELO             ADDRESS IS RELATIVE                          
         AR    R4,R1                    TO RELOCATION FACTOR                    
         NI    6(R2),X'FF'-X'40'   DON'T PUT CURSOR HERE AGAIN                  
         LA    R2,CONP0H           POINT TO 1ST PROTECTED FIELD                 
         ZIC   R1,0(R2)            GET LENGTH TO NEXT FIELD                     
         AR    R1,R2               POINT TO THE 1ST INPUT FIELD                 
         OI    6(R1),X'40'         CURSOR SHOULD BE HERE                        
         LR    R3,R4               SAVE POSITION IN TABLE                       
VKPARTLP CLI   0(R4),X'FF'         NO MORE PARTS OF THE KEY?                    
         BNE   VKPART10            THERE'S MORE                                 
         LR    R4,R3               DONE, NOW VALIDATE COMPONENTS                
*                                                                               
VKCLR    DS    0H                                                               
         XC    8(L'CONP0,R2),8(R2) CLEAR FIELD                                  
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)            NO, BUMP TO INPUT FIELD                      
         AR    R2,R1                                                            
         XC    8(L'CONP0,R2),8(R2) CLEAR FIELD                                  
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R1,CONPFH           ARE WE AT THE END YET?                       
         CR    R2,R1                                                            
         BNL   VK00                YES, GO VALIDATE INPUT                       
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               BUMP TO NEXT PROTECTED FIELD                 
         B     VKCLR                                                            
*                                                                               
VKPART10 OI    6(R2),X'80'         TRANSMIT                                     
         OI    1(R2),X'08'         SET TO HIGH INTENSITY                        
         MVC   8(8,R2),0(R4)       LOAD FIELD NAME TO SCREEN                    
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO THE INPUT FIELD                         
         NI    1(R2),X'FF'-X'20'   UNPROTECT FIELD                              
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO THE INPUT FIELD                         
         LA    R4,LTAB(R4)         NEXT COMPONENT IN KEY                        
         B     VKPARTLP                                                         
         EJECT                                                                  
***********************************************************************         
* CODE TO EXPEDITE THE NEW TABLES FOR RECORD ENTRIES                  *         
***********************************************************************         
VK00     LA    R2,CONI0H           LOAD A(FIRST INPUT FIELD)                    
VK02     CLI   0(R4),X'FF'         LAST FIELD DONE?                             
         BE    VK50                YES - X'FF' FOUND - FINISH KEY               
         TM    12(R4),X'02'        BASIC STRING IN ENTRY?                       
         BNO   VK12                NO = ALREADY SET                             
         L     R5,8(R4)            LOAD A(BASIC STRING)                         
         L     R1,RELO             RELOCATION FACTOR                            
         AR    R5,R1                                                            
         XC    INTKEY,INTKEY       REINITIALIZE INTKEY (IN TWA)                 
         LA    R3,INTKEY           A(SCREEN KEY)                                
         L     R1,0(R5)            GET L(BASIC STRING)                          
         BCTR  R1,0                SUBTRACT 1 FOR EX                            
         EX    R1,VK10                                                          
         LA    R1,1(R1)            ADD 1 BACK TO LENGTH                         
         AR    R3,R1               ADD LENGTH TO STRING                         
         ST    R3,SAVER3           SAVE A(CHAR STRING)                          
         B     VK12                                                             
VK10     MVC   0(0,R3),4(R5)       MOVE LITERAL TO KEY                          
*                                                                               
*    BASIC KEY IS SET UP - NOW SET UP INDIVIDUAL FIELDS                         
*                                                                               
*    NOTE:  THE FOLLOWING TESTS SEEM REDUNDANT, BUT ARE NOT.  ONCE              
*           THE ORIGINAL SCREEN IS DISPLAYED, THEY INSURE THAT THE              
*           VALIDATION PASS FINDS THE FIRST ENTRY MISSING, AND                  
*           TERMINATES WITH AN ERROR, RATHER THAN PASSING A POSSIBLY            
*           GARBAGE KEY.                                                        
*                                                                               
VK12     TM    12(R4),X'01'        MUST FIELD BE ENTERED?                       
         BNO   VK15                NO  - DON'T TEST FOR ENTRY                   
         CLI   5(R2),0             TEST LENGTH ATTRIBUTE                        
         BNH   MISSFLD             NO CODE - TERMINATE                          
*                                                                               
*  NOTE:  IF FIELD CONTAINS NO DATA, NO FURTHER FIELD CHECKS ARE                
*         DONE.  THE KEY TO THAT POINT IS PASSED ALONG. NO CHECK                
* FOR ZERO-LENGTH FIELDS NEED BE DONE LATER.                                    
*                                                                               
VK15     TM    12(R4),X'08'        PUT OUT FILLER BYTE?                         
         BO    VK20                YES                                          
         CLI   5(R2),0             IF NO DATA, END INPUT                        
         BE    VK50                                                             
         ZIC   R5,13(R4)           GET L(FIELD)                                 
         ZIC   R1,5(R2)            GET L(INPUT)                                 
         CR    R5,R1               INPUT TOO LONG?                              
         BL    INVLFLD             YES - TERMINATE WITH ERROR                   
VK20     L     RF,14(R4)           GET A(BUILD ROUTINE)                         
         L     R1,RELO             ADDRESS IS RELATIVE                          
         AR    RF,R1                  TO RELOCATION FACTOR                      
         BASR  RE,RF               GET FIELD, INSERT INTO KEY                   
         LA    R4,LTAB(R4)         NEXT KEY COMPONENT                           
         LA    R2,DENTRY(R2)       BUMP TO NEXT SCREEN HEADER                   
         B     VK02                                                             
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
VK50     MVC   DISKADDR(L'DISKA+L'INTKEY),DISKA                                 
         USING CONHEADH-64,RA                                                   
         B     EXIT08                                                           
         EJECT                                                                  
CHAR     NTR1                                                                   
         L     R3,SAVER3           RESET A(INTKEY)                              
         MVC   0(2,R3),=C'C'''     STRING IN CHARACTER INDICATOR                
         LA    R3,2(R3)            BUMP ADDRESS                                 
         MVI   0(R3),C' '          SPACE FIRST POSITION OF FIELD                
         BCTR  R5,0                SUBTRACT 1 FOR EX                            
         EX    R5,CHAR20           SPACE OUT FIELD                              
         LA    R5,1(R5)            ADD 1 BACK IN                                
         ZIC   R6,5(R2)            TAKE LENGTH OF INPUT                         
         BCTR  R6,0                SUBTRACT 1 FOR EX                            
         EX    R6,CHAR22           MOVE DATA BY LENGTH                          
         AR    R3,R5               BUMP ADDRESS BY FIELD LENGTH                 
         MVC   0(1,R3),=C''''                                                   
         LA    R3,1(R3)            BUMP ADDRESS                                 
         ST    R3,SAVER3           SAVE A(INTKEY)                               
         B     EXIT08                                                           
CHAR20   MVC   1(0,R3),0(R3)       SPACE FILL FOR LENGTH                        
CHAR22   MVC   0(0,R3),8(R2)       MOVE FIELD TO KEY                            
         EJECT                                                                  
CDEF     NTR1                                                                   
         CLI   5(R2),3             INPUT LENGTH = 3?                            
         BNE   CDEF20              NO  -                                        
         CLC   8(3,R2),=C'DEF'     ASKING FOR 'DEFAULT'?                        
         BNE   CDEF20              NO                                           
         L     R3,SAVER3                                                        
         MVC   0(9,R3),=C'X''FFFFFF'''                                          
         LA    R3,9(R3)            BUMP ADDRESS                                 
         ST    R3,SAVER3                                                        
         B     EXIT08                                                           
CDEF20   BAS   RE,CHAR             NOT DEFAULT: TREAT AS CHARACTERS             
         B     EXIT08                                                           
         EJECT                                                                  
BIN      NTR1                                                                   
         L     R3,SAVER3           RESET INSERT ADDRESS IN STRING               
         MVC   0(2,R3),=C'X'''                                                  
         LA    R3,2(R3)            BUMP ADDRESS                                 
         ZIC   R6,5(R2)            GET LENGTH OF INPUT FIELD                    
         BCTR  R6,0                DECREMENT LENGTH BY ONE                      
         XC    CONWK2,CONWK2       WIPE OUT FIELD                               
         LA    R1,CONWK2                                                        
         EX    R6,BIN16                                                         
         B     BIN20                                                            
BIN16    PACK  0(8,R1),8(0,R2)                                                  
BIN20    CVB   R1,CONWK2                                                        
         TM    12(R4),X'20'        ADJUST YEAR-2000 DATE?                       
         BNO   BIN30               NO                                           
         LA    R1,100(R1)          YES - ADD 100 TO DATE                        
BIN30    EQU   *                                                                
         XC    CONWK2(12),CONWK2                                                
         ST    R1,CONWK2           MOVE BINARY TO STORAGE DOUBLEWORD            
         GOTO1 HEXOUT,PAR1,CONWK2,CONWK2+4,4,=C'TOG'                            
         ZIC   R1,8(R4)            GET LENGTH OF OUTPUT                         
         SLA   R1,1                DOUBLE THE LENGTH                            
         LA    R2,CONWK2+12        POINT PAST HEXOUT FIELD                      
         SR    R2,R1               SET A(START OF MOVE)                         
         BCTR  R1,0                SUBTRACT 1 FOR EX                            
         EX    R1,BIN40            MOVE VALUE TO FIELD                          
         LA    R1,1(R1)            ADD 1 BACK                                   
         AR    R3,R1               BUMP ADDRESS BY L(FIELD)                     
         MVC   0(1,R3),=C''''                                                   
         LA    R3,1(R3)            BUMP ADDRESS                                 
         ST    R3,SAVER3           SAVEA(INTKEY)                                
         B     EXIT08                                                           
BIN40    MVC   0(0,R3),0(R2)                                                    
         EJECT                                                                  
*                                                                               
*  PUTS ONE BYTE OF X'00' INTO KEY                                              
*                                                                               
NOENT    NTR1                                                                   
         L     R3,SAVER3           RESET INSERT ADDRESS IN STRING               
         MVC   0(5,R3),=C'X''00'''                                              
         LA    R3,5(R3)            BUMP ADDRESS                                 
         ST    R3,SAVER3                                                        
         B     EXIT08              EXIT                                         
         EJECT                                                                  
DTE      NTR1                                                                   
         L     R3,SAVER3           RESET INSERT ADDRESS IN STRING               
         MVC   0(2,R3),=C'X'''                                                  
         LA    R3,2(R3)                                                         
         XC    CONWK2,CONWK2       WIPE OUT FIELD                               
         TM    12(R4),X'04'        O/P = 3 BYTE BINARY?                         
         BNO   DTE20               NO                                           
         GOTO1 DATCON,PAR1,(4,8(R2)),(3,CONWK2)                                 
         GOTO1 HEXOUT,PAR1,CONWK2,CONWK2+8,3,=C'TOG'                            
         MVC   0(6,R3),CONWK2+8    MOVE TO STRING                               
         LA    R3,6(R3)            BUMP ADDRESS                                 
         MVC   0(1,R3),=C''''                                                   
         LA    R3,1(R3)            BUMP ADDRESS                                 
         ST    R3,SAVER3           SAVE A(INTKEY)                               
DTE20    EQU                       *                                            
         B     EXIT08                                                           
SRCE     NTR1                                                                   
         B     EXIT08                                                           
         EJECT                                                                  
*                                                                               
*   CREATE A 9'S COMPLEMENT CONTRACT NUMBER                                     
*                                                                               
*   R3 CONTAINS THE RECEIVING ADDRESS OF THE FIELD WITHIN 'INTKEY'              
*                                                                               
CNUMB    NTR1                                                                   
         XC    REGNUM,REGNUM                                                    
         TM    12(R4),X'10'        REGULAR CONTRACT #                           
         BNO   CNUMB06                                                          
         MVI   REGNUM,X'01'        YES - SET FLAG                               
CNUMB06  ST    R4,SAVER4           SAVE R4 FOR LATER USE                        
         L     R3,SAVER3           RESET A(INTKEY)                              
         MVC   0(2,R3),=C'X'''                                                  
         LA    R3,2(R3)            BUMP ADDRESS                                 
         OC    REGNUM,REGNUM       REGULAR CONTRACT #?                          
         BZ    CNUMB08             NO                                           
         MVC   CONWK2(8),=8C'0'    LOAD WITH EBCDIC ZEROS                       
         B     CNUMB09                                                          
CNUMB08  MVC   CONWK2(8),=8C'9'    FILL WITH 9S                                 
CNUMB09  LA    R6,8(R2)            POINT TO THE NUMBER                          
         ZIC   R4,5(R2)            LOAD LENGTH                                  
         LA    R5,8                                                             
         SR    R5,R4               OFFSET OF WHERE TO START CHANGING            
         LA    R5,CONWK2(R5)                                                    
CNUMB10  MVC   0(1,R5),0(R6)       COPY A CHARACTER                             
         OC    REGNUM,REGNUM       REGULAR CONTRACT #?                          
         BNZ   CNUMB11             YES                                          
         NI    0(R5),X'FF'-X'F0'   TAKE OFF ZONE DIGIT                          
         ZIC   R1,0(R5)                                                         
         LA    R1,COMP9(R1)        COMPLEMENT OF 9 LINE                         
         MVC   0(1,R5),0(R1)                                                    
         OI    0(R5),X'F0'         PUT BACK THE ZONE                            
CNUMB11  LA    R6,1(R6)                                                         
         LA    R5,1(R5)                                                         
         BCT   R4,CNUMB10                                                       
         L     R4,SAVER4           RESET R4 (POINTS TO TABLE ENTRY)             
         TM    12(R4),X'04'        REVERSE 9'S COMPLEMENT?                      
         BNO   CNUMB16                                                          
         LA    R1,CONWK2           A(9'S COMPLEMENT)                            
         LA    R2,CONWK2+15        A(LAST POSITION OF FIELD)                    
         LA    R4,8                COUNT                                        
CNUMB12  MVC   0(1,R2),0(R1)       MOVE CHARACTER BY CHARACTER                  
         LA    R1,1(R1)            INCREMENT 'FROM' FIELD                       
         BCTR  R2,0                DECREMENT 'TO' FIELD                         
         BCT   R4,CNUMB12          LOOP FOR 8 CHARS                             
         MVC   CONWK2(8),CONWK2+8  RESET ORIGINAL FIELD                         
CNUMB16  MVC   0(8,R3),CONWK2      9S COMPLEMENT                                
         LA    R3,8(R3)            BUMP A(R3) FOR CONTRACT #                    
         MVC   0(1,R3),=C''''      INSERT CLOSE QUOTE                           
         LA    R3,1(R3)            BUMP A(R3) FOR CLOSE QUOTE                   
         ST    R3,SAVER3           SAVE A(INTKEY)                               
         B     EXIT08              FINISHED VALIDATING KEY                      
         EJECT                                                                  
*                                                                               
*  TEST DUMMY                                                                   
*                                                                               
ATEST    B     EXIT08                                                           
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MYERROR  GOTO1 ERREX2                                                           
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
         EJECT                                                                  
*                                                                               
*                                CL8 - NAME OF THE RECORD TYPE                  
*                                AL4 - ADDRESS OF KEY PORTIONS                  
*                                                                               
RECTABLE DS    0CL12                                                            
         DC    C'CON8C   ',AL4(CON8C)                                           
         DC    C'CON0C   ',AL4(CON0C)                                           
         DC    C'CON9C   ',AL4(CON9C)                                           
         DC    C'CONAC   ',AL4(CONAC)                                           
         DC    C'CONBC   ',AL4(CONBC)                                           
         DC    C'CONCC   ',AL4(CONCC)                                           
         DC    C'CONDC   ',AL4(CONDC)                                           
         DC    C'CONEC   ',AL4(CONEC)                                           
         DC    C'REP     ',AL4(REP)                                             
         DC    C'STA     ',AL4(STA)                                             
         DC    C'REG     ',AL4(REG)                                             
         DC    C'OFF     ',AL4(OFF)                                             
         DC    C'TEAM    ',AL4(TEAM)                                            
         DC    C'SAL     ',AL4(SAL)                                             
         DC    C'GRP     ',AL4(GRP)                                             
         DC    C'ADV     ',AL4(ADV)                                             
         DC    C'PROD    ',AL4(PROD)                                            
         DC    C'AGY     ',AL4(AGY)                                             
         DC    C'AGY8A   ',AL4(AGY8A)                                           
         DC    C'BUY     ',AL4(BUY)                                             
         DC    C'CLS     ',AL4(CLS)                                             
         DC    C'CTG     ',AL4(CTG)                                             
         DC    C'INV     ',AL4(INV)                                             
         DC    C'IDP     ',AL4(IDP)                                             
         DC    C'BUD     ',AL4(BUD)                                             
         DC    C'AVL     ',AL4(AVL)                                             
         DC    C'PRP     ',AL4(PRP)                                             
         DC    C'EOM     ',AL4(EOM)                                             
         DC    C'OVR     ',AL4(OVR)                                             
         DC    C'DEM     ',AL4(DEM)                                             
         DC    C'DPT     ',AL4(DPT)                                             
         DC    C'SDD     ',AL4(SDD)                                             
         DC    C'ATN     ',AL4(ATN)                                             
         DC    C'SWI     ',AL4(SWI)                                             
         DC    C'COMM    ',AL4(COMM)                                            
         DC    C'OWN     ',AL4(OWN)                                             
         DC    C'MKT     ',AL4(MKT)                                             
         DC    C'SBB     ',AL4(SBB)                                             
         DC    C'CMT     ',AL4(CMT)                                             
         DC    C'PWC     ',AL4(PWC)                                             
         DC    C'TYP     ',AL4(TYP)                                             
         DC    C'PTP     ',AL4(PTP)                                             
         DC    C'CTY     ',AL4(CTY)                                             
         DC    C'RDA     ',AL4(RDA)                                             
         DC    C'OCM     ',AL4(OCM)                                             
         DC    C'DIR     ',AL4(DIR)                                             
         DC    C'LAB     ',AL4(LAB)                                             
         DC    C'GOL     ',AL4(GOL)                                             
         DC    C'SET     ',AL4(SET)                                             
         DC    C'STR     ',AL4(STR)                                             
         DC    C'DEVSAL  ',AL4(DEVSAL)                                          
         DC    C'DEVKTYPE',AL4(DEVKT)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*                                CL8 - NAME OF THE FIELD TO INPUT               
*                                AL4 - ADDRESS OF VALIDATING ROUTINE            
*                                                                               
*********************************************************************           
*  TABLE DEFINITION:                                                            
*    FIELD  DESCRIPTION                      COMMENTS                           
*     NUM                                                                       
*           (LINE ENTRY FOR SCREEN FIELDS)                                      
*      1    8 BYTE FIELD NAME                DISPLAY NAME                       
*           LAST ENTRY = X'FF'                                                  
*      2    4 BYTE FIELD:                                                       
*           IF FIELD # 3  = X'02':           X'02' = BIT 14 ON                  
*             ADDRESS OF STRING OUTPUT      (MUST BE 'ON' FOR FIRST             
*                                            ENTRY OF GROUP)                    
*           IF FIELD # 3  NOT = X'02':                                          
*              FOR 'BIN' RTN:                                                   
*                   BYTE 1 CONTAINS LENGTH OF OUTPUT                            
*                                                                               
*                                                                               
*      3    ONE BYTE FOR FLAGS:                                                 
*           X'01'  =  FIELD MUST BE PRESENT, ELSE ERROR                         
*           X'02'  =  A(BASIC STRING) IN ENTRY                                  
*           X'04'  =  IF RTN=CNUMB, REVERSE THE NUMBER                          
*           X'04'  =  IF RTN=DTE, OUTPUT IS YMD (BINARY)                        
*           X'08'  =  CALL NO-ENTRY RTN FOR 'EMPTY FIELD' REGARDLESS            
*                     OF DATA                                                   
*           X'10'  =  IF RTN=CNUMB, NO 9'S COMPLEMENT                           
*           X'20'  =  VALUE IS YEAR.  IF < 51, ADD 100 FOR YR-2000.             
*                                                                               
*                                                                               
*      4    ONE BYTE MAX INPUT LENGTH                                           
*      5    ADDRESS OF KEY STRING ROUTINE                                       
* (LINE ENTRY FOR STRING OUTPUT)                                                
*      1    ONE WORD LENGTH OF INITIAL KEYSTRING                                
*      2    INITIAL KEY STRING                                                  
*********************************************************************           
*   CONTRACT 8C TABLE ENTRY  *                                                  
CON8C    DC    C'REP CODE',AL4(STRCON8C),X'03',X'02',AL4(CHAR)                  
         DC    C'NUMBER  ',4X'00',X'00',X'08',AL4(CNUMB)                        
         DC    X'FF'                                                            
STRCON8C DC    F'14',C'K,X''8C''20X''00'''                                      
*   CONTRACT 0C TABLE ENTRY  *                                                  
CON0C    DC    C'REP CODE',AL4(STRCON0C),X'03',X'02',AL4(CHAR)                  
         DC    C'STN GRP ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    C'OFFICE  ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'AGY+OFF ',4X'00',X'00',X'06',AL4(CHAR)                         
         DC    C'ADVRTSR ',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    C'NUMBER  ',4X'00',X'10',X'08',AL4(CNUMB)                        
         DC    X'FF'                                                            
STRCON0C DC    F'09',C'K,X''0C00'''                                             
*   CONTRACT 9C TABLE ENTRY  *                                                  
CON9C    DC    C'REP CODE',AL4(STRCON9C),X'03',X'02',AL4(CHAR)                  
         DC    C'OFFICE  ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'STN GRP ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    C'AGENCY  ',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    C'AGY OFF ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'NUMBER  ',4X'00',X'10',X'08',AL4(CNUMB)                        
         DC    X'FF'                                                            
STRCON9C DC    F'09',C'K,X''9C00'''                                             
*   CONTRACT AC TABLE ENTRY  *                                                  
CONAC    DC    C'REP CODE',AL4(STRCONAC),X'03',X'02',AL4(CHAR)                  
         DC    C'OFFICE  ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'TEAM    ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'SALESMAN',4X'00',X'00',X'03',AL4(CHAR)                         
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    C'AGENCY  ',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    C'ADVRTSR ',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    C'NUMBER  ',4X'00',X'10',X'08',AL4(CNUMB)                        
         DC    X'FF'                                                            
STRCONAC DC    F'07',C'K,X''AC'''                                               
*   CONTRACT BC TABLE ENTRY  *                                                  
CONBC    DC    C'REP CODE',AL4(STRCONBC),X'03',X'02',AL4(CHAR)                  
         DC    C'CATEGORY',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'OFFICE  ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    C'AGENCY  ',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    C'AGY OFF ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'ADVRTSR ',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    C'NUMBER  ',4X'00',X'10',X'08',AL4(CNUMB)                        
         DC    X'FF'                                                            
STRCONBC DC    F'09',C'K,X''BC00'''                                             
*   CONTRACT CC TABLE ENTRY  *                                                  
CONCC    DC    C'REP CODE',AL4(STRCONCC),X'03',X'02',AL4(CHAR)                  
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    C'OFFICE  ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'DIV/TEAM',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'SALESMAN',4X'00',X'00',X'03',AL4(CHAR)                         
         DC    C'ADVRTSR ',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    C'AGENCY  ',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    C'NUMBER  ',4X'00',X'10',X'08',AL4(CNUMB)                        
         DC    X'FF'                                                            
STRCONCC DC    F'07',C'K,X''CC'''                                               
*   CONTRACT DC TABLE ENTRY  *                                                  
CONDC    DC    C'REP CODE',AL4(STRCONDC),X'03',X'02',AL4(CHAR)                  
         DC    C'ADVRTSR ',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    C'BOP DATE',4X'00',X'04',X'08',AL4(DTE)                          
         DC    C'REF CON#',4X'00',X'10',X'08',AL4(CNUMB)                        
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    C'NUMBER  ',4X'00',X'10',X'08',AL4(CNUMB)                        
         DC    X'FF'                                                            
STRCONDC DC    F'14',C'K,X''DC''04X''00'''                                      
*   CONTRACT 0C TABLE ENTRY  *                                                  
CONEC    DC    C'REP CODE',AL4(STRCONEC),X'03',X'02',AL4(CHAR)                  
         DC    C'NUMBER  ',4X'00',X'04',X'08',AL4(CNUMB)                        
         DC    X'FF'                                                            
STRCONEC DC    F'14',C'K,X''EC''20X''00'''                                      
*   REP TABLE ENTRY  *                                                          
REP      DC    C'REP CODE',AL4(STRREP),X'03',X'02',AL4(CHAR)                    
         DC    X'FF'                                                            
STRREP   DC    F'14',C'K,X''01''24X''00'''                                      
*   STATION TABLE ENTRY *                                                       
STA      DC    C'REP CODE',AL4(STRSTA),X'03',X'02',AL4(CHAR)                    
STA1     DC    C'STA     ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    X'FF'                                                            
STRSTA   DC    F'14',C'K,X''02''19X''00'''                                      
*   REGION TABLE ENTRY *                                                        
REG      DC    C'REP CODE',AL4(STRREG),X'03',X'02',AL4(CHAR)                    
         DC    C'REG     ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STRREG   DC    F'14',C'K,X''03''22X''00'''                                      
*   OFFICE TABLE ENTRY *                                                        
OFF      DC    C'REP CODE',AL4(STROFF),X'03',X'02',AL4(CHAR)                    
         DC    C'OFF     ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STROFF   DC    F'14',C'K,X''04''22X''00'''                                      
*   TEAM TABLE ENTRY   *                                                        
TEAM     DC    C'REP CODE',AL4(STRTEAM),X'03',X'02',AL4(CHAR)                   
         DC    C'TEAM    ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STRTEAM  DC    F'14',C'K,X''05''22X''00'''                                      
*   SALESMAN TABLE ENTRY  *                                                     
SAL      DC    C'REP CODE',AL4(STRSAL),X'03',X'02',AL4(CHAR)                    
         DC    C'SAL     ',4X'00',X'00',X'03',AL4(CHAR)                         
         DC    X'FF'                                                            
STRSAL   DC    F'14',C'K,X''06''21X''00'''                                      
*   GROUP TABLE ENTRY  *                                                        
GRP      DC    C'REP CODE',AL4(STRGRP),X'03',X'02',AL4(CHAR)                    
         DC    C'GRP     ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STRGRP   DC    F'14',C'K,X''07''22X''00'''                                      
*   ADVERTISER TABLE ENTRY  *                                                   
ADV      DC    C'ADV     ',AL4(STRADV),X'03',X'04',AL4(CHAR)                    
         DC    C'REP CODE',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STRADV   DC    F'14',C'K,X''08''20X''00'''                                      
*   PRODUCT TABLE ENTRY   *                                                     
PROD     DC    C'ADV     ',AL4(STRPROD),X'03',X'04',AL4(CHAR)                   
         DC    C'PROD    ',4X'00',X'00',X'03',AL4(CHAR)                         
         DC    C'REP CODE',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STRPROD  DC    F'14',C'K,X''09''17X''00'''                                      
*   AGENCY TABLE ENTRY   *                                                      
AGY      DC    C'AGY     ',AL4(STRAGY),X'03',X'04',AL4(CHAR)                    
         DC    C'AGY OFF ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'REP CODE',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STRAGY   DC    F'14',C'K,X''0A''18X''00'''                                      
*   AGENCY PASSIVE TABLE ENTRY   *                                              
AGY8A    DC    C'AGY NAME',AL4(STRAGY8A),X'03',X'12',AL4(CHAR)                  
         DC    C'AGY CODE',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    C'AGY OFF ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'REP CODE',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STRAGY8A DC    F'07',C'K,X''8A'''                                               
*   BUY TABLE ENTRY  *                                                          
BUY      DC    C'REP CODE',AL4(STRBUY),X'03',X'02',AL4(CHAR)                    
         DC    C'CONTRACT',4X'00',X'05',X'08',AL4(CNUMB)                        
         DC    C'PLAN/DEF',4X'00',X'00',X'03',AL4(CDEF)                         
         DC    C'MSTR LN#',X'01',3X'00',X'00',X'02',AL4(BIN)                    
         DC    C'LINE #  ',X'01',3X'00',X'00',X'02',AL4(BIN)                    
         DC    X'FF'                                                            
STRBUY   DC    F'14',C'K,X''0B''15X''00'''                                      
*   CLASS TABLE ENTRY  *                                                        
CLS      DC    C'REP CODE',AL4(STRCLS),X'03',X'02',AL4(CHAR)                    
         DC    C'CLASS   ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STRCLS   DC    F'14',C'K,X''0D''22X''00'''                                      
*   CATEGORY TABLE ENTRY  *                                                     
CTG      DC    C'REP CODE',AL4(STRCTG),X'03',X'02',AL4(CHAR)                    
         DC    C'CATEGORY',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STRCTG   DC    F'14',C'K,X''0F''22X''00'''                                      
*   INVENTORY TABLE ENTRY   *                                                   
INV      DC    C'REP CODE',AL4(STRINV),X'03',X'02',AL4(CHAR)                    
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    C'QTR HR# ',X'01',3X'00',X'00',X'02',AL4(BIN)                    
         DC    C'DAY CODE',4X'00',X'00',X'01',AL4(CHAR)                         
         DC    C'PRG LEN ',4X'00',X'00',X'01',AL4(CHAR)                         
         DC    C'NO ENTRY',4X'00',X'08',X'01',AL4(NOENT)                        
         DC    C'STRT MDY',4X'00',X'04',X'08',AL4(DTE)                          
         DC    C'SOURCE  ',4X'00',X'00',X'01',AL4(SRCE)                         
         DC    C'BOOK Y/M',4X'00',X'00',X'02',AL4(DTE)                          
         DC    X'FF'                                                            
STRINV   DC    F'14',C'K,X''12''09X''00'''                                      
*   INVENTORY DAYPART PASSIVE TABLE ENTRY   *                                   
IDP      DC    C'REP CODE',AL4(STRIDP),X'03',X'02',AL4(CHAR)                    
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    C'DAYPART ',4X'00',X'00',X'01',AL4(CHAR)                         
         DC    C'NO ENTRY',4X'00',X'08',X'02',AL4(NOENT)                        
         DC    C'DAY CODE',4X'00',X'00',X'01',AL4(CHAR)                         
         DC    C'EFF DATE',4X'00',X'00',X'02',AL4(DTE)                          
         DC    C'QTR HR# ',X'01',3X'00',X'00',X'02',AL4(BIN)                    
         DC    C'PRG LEN ',X'01',3X'00',X'00',X'02',AL4(BIN)                    
         DC    X'FF'                                                            
STRIDP   DC    F'14',C'K,X''92''02X''00'''                                      
*   BUDGET TABLE ENTRY   *                                                      
BUD      DC    C'REP CODE',AL4(STRBUD),X'03',X'02',AL4(CHAR)                    
         DC    C'YEAR    ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    C'TEAM    ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STRBUD   DC    F'14',C'K,X''13''15X''00'''                                      
*   AVAIL TABLE ENTRY   *                                                       
AVL      DC    C'REP CODE',AL4(STRAVL),X'03',X'02',AL4(CHAR)                    
         DC    C'CONTRACT',4X'00',X'05',X'08',AL4(CNUMB)                        
         DC    C'NO ENTRY',4X'00',X'08',X'01',AL4(NOENT)                        
         DC    C'AVL #   ',X'01',3X'00',X'00',X'02',AL4(BIN)                    
         DC    C'SOURCE  ',4X'00',X'00',X'01',AL4(CHAR)                         
         DC    C'DETAIL #',X'01',3X'00',X'00',X'02',AL4(BIN)                    
         DC    X'FF'                                                            
STRAVL   DC    F'14',C'K,X''14''16X''00'''                                      
* PROPOSAL/PLAN TABLE ENTRY   *                                                 
PRP      DC    C'REP CODE',AL4(STRPRP),X'03',X'02',AL4(CHAR)                    
         DC    C'CONTRACT',4X'00',X'05',X'08',AL4(CHAR)                         
         DC    C'NO ENTRY',4X'00',X'08',X'01',AL4(NOENT)                        
         DC    C'PROP#   ',X'01',3X'00',X'00',X'02',AL4(BIN)                    
         DC    C'SOURCE  ',4X'00',X'00',X'01',AL4(CHAR)                         
         DC    C'PKG CODE',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'NO ENTRY',4X'00',X'08',X'01',AL4(NOENT)                        
         DC    C'DETAIL #',X'01',3X'00',X'00',X'02',AL4(BIN)                    
         DC    X'FF'                                                            
STRPRP   DC    F'14',C'K,X''16''13X''00'''                                      
*   EOM TABLE ENTRY  *                                                          
EOM      DC    C'REP CODE',AL4(STREOM),X'03',X'02',AL4(CHAR)                    
         DC    C'YEAR    ',X'01',3X'00',X'20',X'02',AL4(BIN)                    
         DC    X'FF'                                                            
STREOM   DC    F'14',C'K,X''18''23X''00'''                                      
*   OVERRIDE TABLE ENTRY   *                                                    
OVR      DC    C'REP CODE',AL4(STROVR),X'03',X'02',AL4(CHAR)                    
         DC    C'REPT CDE',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'SVC CODE',4X'00',X'00',X'03',AL4(CHAR)                         
         DC    C'MKT CODE',4X'00',X'00',X'03',AL4(CHAR)                         
         DC    C'SCREEN# ',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    X'FF'                                                            
STROVR   DC    F'14',C'K,X''22''12X''00'''                                      
*   DEMOGRAPHIC TABLE ENTRY   *                                                 
DEM      DC    C'REP CODE',AL4(STRDEM),X'03',X'02',AL4(CHAR)                    
         DC    C'MENU CDE',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STRDEM   DC    F'14',C'K,X''23''22X''00'''                                      
*   DAYPART TABLE ENTRY   *                                                     
DPT      DC    C'REP CODE',AL4(STRDPT),X'03',X'02',AL4(CHAR)                    
         DC    C'DAYPART ',4X'00',X'00',X'01',AL4(CHAR)                         
         DC    X'FF'                                                            
STRDPT   DC    F'14',C'K,X''24''23X''00'''                                      
*   SDD TABLE ENTRY   *                                                         
SDD      DC    C'REP CODE',AL4(STRSDD),X'03',X'02',AL4(CHAR)                    
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    X'FF'                                                            
STRSDD   DC    F'14',C'K,X''26''19X''00'''                                      
*   ATN TABLE ENTRY   *                                                         
ATN      DC    C'REP CODE',AL4(STRATN),X'03',X'02',AL4(CHAR)                    
         DC    C'GROUP   ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    C'TYPE    ',4X'00',X'00',X'01',AL4(BIN)                          
         DC    C'CATEGORY',4X'00',X'00',X'09',AL4(CHAR)                         
*                                                                               
*  THIS ENTRY IS NOT COMPLETE - MAY REQUIRE TWO TABLE ENTRIES,                  
*       BY SUB-TYPE OF RECORD                                                   
*                                                                               
         DC    X'FF'                                                            
STRATN   DC    F'14',C'K,X''27''00X''00'''                                      
*   SWITCH TABLE ENTRY   *                                                      
SWI      DC    C'REP CODE',AL4(STRSWI),X'03',X'02',AL4(CHAR)                    
         DC    C'MON. DTE',4X'00',X'00',X'03',AL4(DTE)                          
         DC    C'TYPE    ',4X'00',X'00',X'01',AL4(BIN)                          
         DC    C'OLD CODE',4X'00',X'00',X'07',AL4(CHAR)                         
         DC    C'SEQ #   ',4X'00',X'00',X'01',AL4(BIN)                          
         DC    X'FF'                                                            
STRSWI   DC    F'14',C'K,X''28''12X''00'''                                      
*   COMMENT TABLE ENTRY   *                                                     
COMM     DC    C'REP CODE',AL4(STRCOMM),X'03',X'02',AL4(CHAR)                   
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    C'OFFICE  ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'ADVTSR  ',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    C'TYPE    ',4X'00',X'00',X'01',AL4(CHAR)                         
         DC    C'DATE Y/M',4X'00',X'00',X'01',AL4(DTE)                          
         DC    X'FF'                                                            
STRCOMM  DC    F'14',C'K,X''29''10X''00'''                                      
*   OWNER TABLE ENTRY   *                                                       
OWN      DC    C'REP CODE',AL4(STROWN),X'03',X'02',AL4(CHAR)                    
         DC    C'OWNR CDE',4X'00',X'00',X'03',AL4(CHAR)                         
         DC    X'FF'                                                            
STROWN   DC    F'14',C'K,X''2A''21X''00'''                                      
*   MARKET TABLE ENTRY   *                                                      
MKT      DC    C'REP CODE',AL4(STRMKT),X'03',X'02',AL4(CHAR)                    
         DC    C'MKT  CDE',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    X'FF'                                                            
STRMKT   DC    F'14',C'K,X''2B''20X''00'''                                      
*   SBB TABLE ENTRY   *                                                         
SBB      DC    C'REP CODE',AL4(STRSBB),X'03',X'02',AL4(CHAR)                    
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    C'ADVTSR  ',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    C'OFFICE  ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'DATE Y/M',4X'00',X'00',X'01',AL4(DTE)                          
         DC    X'FF'                                                            
STRSBB   DC    F'14',C'K,X''2D''11X''00'''                                      
*   CMT TABLE ENTRY                                                             
CMT      DC    C'REP CODE',AL4(STRCMT),X'03',X'02',AL4(CHAR)                    
         DC    C'OFF CODE',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'CMMT CDE',4X'00',X'00',X'10',AL4(CHAR)                         
         DC    X'FF'                                                            
STRCMT   DC    F'14',C'K,X''2E''14X''00'''                                      
*   PWC TABLE ENTRY   *                                                         
PWC      DC    C'REP CODE',AL4(STRPWC),X'03',X'02',AL4(CHAR)                    
         DC    C'CONTRACT',4X'00',X'00',X'04',AL4(BIN)                          
         DC    X'FF'                                                            
STRPWC   DC    F'14',C'K,X''2F''20X''00'''                                      
*   TYPE TABLE ENTRY   *                                                        
TYP      DC    C'REP CODE',AL4(STRTYP),X'03',X'02',AL4(CHAR)                    
         DC    C'REC CODE',4X'00',X'00',X'08',AL4(CHAR)                         
         DC    X'FF'                                                            
STRTYP   DC    F'14',C'K,X''30''16X''00'''                                      
*   PTP TABLE ENTRY                                                             
PTP      DC    C'REP CODE',AL4(STRPTP),X'03',X'02',AL4(CHAR)                    
         DC    C'REC CODE',4X'00',X'00',X'03',AL4(CHAR)                         
         DC    X'FF'                                                            
STRPTP   DC    F'14',C'K,X''31''21X''00'''                                      
*   CTY TABLE ENTRY   *                                                         
CTY      DC    C'REP CODE',AL4(STRCTY),X'03',X'02',AL4(CHAR)                    
         DC    C'TYPE CDE',4X'00',X'00',X'01',AL4(CHAR)                         
         DC    X'FF'                                                            
STRCTY   DC    F'14',C'K,X''32''23X''00'''                                      
*   RDA TABLE ENTRY   *                                                         
RDA      DC    C'REP CODE',AL4(STRRDA),X'03',X'02',AL4(CHAR)                    
         DC    C'CODE    ',4X'00',X'00',X'08',AL4(CHAR)                         
         DC    X'FF'                                                            
STRRDA   DC    F'14',C'K,X''33''16X''00'''                                      
*   OCM TABLE ENTRY   *                                                         
OCM      DC    C'REP CODE',AL4(STROCM),X'03',X'02',AL4(CHAR)                    
         DC    C'OFF CODE',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'NUMBER  ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'PAGE    ',4X'00',X'00',X'01',AL4(CHAR)                         
         DC    X'FF'                                                            
STROCM   DC    F'14',C'K,X''34''19X''00'''                                      
*   DIR TABLE ENTRY   *                                                         
DIR      DC    C'REP CODE',AL4(STRDIR),X'03',X'02',AL4(CHAR)                    
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    X'FF'                                                            
STRDIR   DC    F'14',C'K,X''35''12X''00'''                                      
*   LAB TABLE ENTRY   *                                                         
LAB      DC    C'REP CODE',AL4(STRLAB),X'03',X'02',AL4(CHAR)                    
         DC    C'LABEL   ',4X'00',X'00',X'08',AL4(CHAR)                         
         DC    X'FF'                                                            
STRLAB   DC    F'14',C'K,X''36''16X''00'''                                      
*   GOL TABLE ENTRY   *                                                         
GOL      DC    C'REP CODE',AL4(STRGOL),X'03',X'02',AL4(CHAR)                    
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    X'FF'                                                            
STRGOL   DC    F'14',C'K,X''37''16X''00'''                                      
*   SET TABLE ENTRY   *                                                         
SET      DC    C'REP CODE',AL4(STRSET),X'03',X'02',AL4(CHAR)                    
         DC    C'SET TYPE',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    C'IDENTIFI',4X'00',X'00',X'04',AL4(CHAR)                         
         DC    X'FF'                                                            
STRSET   DC    F'14',C'K,X''38''18X''00'''                                      
*   STR TABLE ENTRY   *                                                         
STR      DC    C'REP CODE',AL4(STRSTR),X'03',X'02',AL4(CHAR)                    
         DC    C'STATION ',4X'00',X'00',X'05',AL4(CHAR)                         
         DC    X'FF'                                                            
STRSTR   DC    F'12',C'K,X''39''X''01'''                                        
*   DEVSAL TABLE ENTRY   *                                                      
DEVSAL   DC    C'REP CODE',AL4(STRDS),X'03',X'02',AL4(CHAR)                     
         DC    C'INITIALS',4X'00',X'00',X'03',AL4(CHAR)                         
         DC    X'FF'                                                            
STRDS    DC    F'14',C'K,X''3A''21X''00'''                                      
*   DEVKT TABLE ENTRY   *                                                       
DEVKT    DC    C'REP CODE',AL4(STRKT),X'03',X'02',AL4(CHAR)                     
         DC    C'K TYPE  ',4X'00',X'00',X'02',AL4(CHAR)                         
         DC    X'FF'                                                            
STRKT    DC    F'14',C'K,X''3B''22X''00'''                                      
COMP9    DC    C'9876543210'                                                    
*                                                                               
DCODE    EQU   CONP1H-CONP0H                                                    
DENTRY   EQU   CONI1H-CONI0H                                                    
LTAB     EQU   STA1-STA                                                         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         PRINT ON                                                               
       ++INCLUDE GEKEYFFD          (BASE SCREEN FOR SYSTEM)                     
*                                                                               
*  FOLLOWING FIELDS ARE INCLUDED IN THE TWA                                     
*                                                                               
         EJECT                                                                  
       ++INCLUDE GEPFMSAVE         (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE GEKEYWORKD                                                     
         PRINT ON                                                               
* MY STORAGE AREA                                                               
         ORG   SYSSPARE                                                         
DISKA    DS    XL4                                                              
INTKEY   DS    CL60                                                             
SAVEKEY  DS    CL60                                                             
PREVKEY  DS    CL60                                                             
CONWK2   DS    2D                  16 BYTES OF WORK AREA                        
PAR1     DS    6F                  PARAMETER LIST                               
SAVER3   DS    F                                                                
SAVER4   DS    F                                                                
REGNUM   DC    XL1'00'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'142GEKEY08S  05/01/02'                                      
         END                                                                    
