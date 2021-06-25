*          DATA SET SPEZF04    AT LEVEL 018 AS OF 01/27/16                      
*PHASE T23004A                                                                  
         TITLE 'T23004 - OFFICE NAME RECORD'                                    
***********************************************************************         
*                                                                     *         
*  TITLE: T23004 - EASI OFFICE NAME RECORDS (KEPT ON AGENCY NAME REC) *         
*  COMMENTS: THIS PROGRAM DOES MAINT AND LIST FOR OFFICE NAME RECS    *         
*            WHICH ARE STORED ON GENDIR/GENFIL, AND ONLY MAINTAINED   *         
*            BY DATA CONTROL.                                         *         
*  OUTPUTS: UPDATED AGENCY NAME RECORDS WITH OFFICE NAME ELEMS        *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 -                                                       *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO PROGRAM/UNIT TABLES             *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - NOT USED                                              *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 -                                                  *         
*             AIO3 -                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*  LEV 05    NOV07/91 CHANGE TO FASWITCH                              *         
*  LEV 06    MAR05/92 ADD MEDIA N                                     *         
*  LEV 07    JUL16/92 ADD MEDIA R FOR BOTH AM & FM                    *         
*  LEV 08    AUG07/92 ADD MEDIA A FOR ALL MEDIA'S                     *         
*  LEV 09    APR07/93 FIX STATION VALIDATION - INCLUDE ALL            *         
*  LEV 10    MAR01/94 ADD MEDIA X DISPLAY                             *         
*  LEV 11    APR13/94 LOCK OUT NON-DDS TERMINALS                      *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T23004   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3004**,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVC   AIO,AIO1                                                         
         MVI   CURSYST,C'C'        SWITCH TO CONTROL (GENDIR/FIL)               
         GOTO1 VALIFAS                                                          
         MVI   QMED,C'A'                                                        
         XC    F04WORK,F04WORK                                                  
*                                                                               
         BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,RECDEL         DELETING RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                  VALIDATE KEY                                 
VKEY     DS    0H                                                               
         NI    FLAG,X'FF'-FKEYCHGQ                                              
*                                                                               
         CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BNE   INVALER                                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      IGNORE IF NOT LIST                           
         BE    VK300                                                            
*                                                                               
         LA    R2,DONNAMH           NAME MANDATORY                              
         TM    4(R2),X'80'         FIELD WAS CHANGED?                           
         BZ    *+8                                                              
         OI    FLAG,FKEYCHGQ                                                    
         CLI   5(R2),0                                                          
         BNE   VK100                                                            
         MVC   CONHEAD(13),=C'NAME REQUIRED'                                    
         B     MYERR                                                            
*                                                                               
VK100    XC    KEY,KEY             SET UP KEY/SVKEY                             
         MVC   KEY(2),=C'ZA'                                                    
         MVC   KEY+7(25),SPACES                                                 
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCNAM                                                        
         MVC   SVKEY,KEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    VK200                                                            
         MVC   CONHEAD(29),=C'AGENCY NAME RECORD MUST EXIST'                    
         B     MYERR                                                            
*                                                                               
VK200    DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VKXIT                                                            
         MVI   KEY+7,X'FF'         TRICK GENCON ON ADD NO DUPLICATE             
         B     VKXIT                                                            
*                                                                               
* ACTION LIST ONLY FROM HERE                                                    
*                                                                               
VK300    DS    0H                                                               
         XC    OPTNAM,OPTNAM                                                    
         LA    R2,DONSRTH                                                       
         CLI   5(R2),0                                                          
         BE    VK400                                                            
         MVC   OPTNAM,8(R2)                                                     
*                                                                               
VK400    DS    0H                                                               
         XC    CUUID,CUUID         USER ID FILTER                               
         XC    CUSTA,CUSTA         STATION FILTER                               
         LA    R2,DONOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VKXIT                                                            
         MVI   ERRFLD,1                                                         
         LA    R6,2                                                             
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         MVC   CONHEAD(14),=C'INVALID OPTION'                                   
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    MYERR                                                            
         SPACE                                                                  
VK500    CLI   0(R4),0                                                          
         BZ    VKXIT                                                            
         CLI   0(R4),1                                                          
         BNE   MYERR                                                            
         SPACE                                                                  
*                                  USER CODE                                    
         CLI   12(R4),C'U'                                                      
         BNE   VK600                                                            
         MVC   CONHEAD(15),=C'INVALID USER ID'                                  
         CLI   1(R4),8                                                          
         BH    VKERR                                                            
         MVC   CUUID,22(R4)                                                     
         B     VK700                                                            
         SPACE                                                                  
*                                  STATION NO REAL VALIDATION                   
VK600    CLI   12(R4),C'S'                                                      
         MVC   CONHEAD(15),=C'INVALID STATION'                                  
         CLI   1(R4),7                                                          
         BH    VKERR                                                            
         MVC   CUSTA(4),22(R4)                                                  
         MVI   CUSTA+4,C' '                                                     
         LA    RE,25(R4)                                                        
         CLI   0(RE),C'-'                                                       
         BNE   VK620                                                            
         LA    RE,1(RE)                                                         
         MVI   CUSTA+3,C' '                                                     
         B     *+8                                                              
VK620    LA    RE,2(RE)                                                         
         CLI   0(RE),C'T'                                                       
         BE    VK700                                                            
         MVC   CUSTA+4(1),0(RE)                                                 
         SPACE                                                                  
*                                  UP ERROR FIELD & NEXT BLOCK                  
VK700    ZIC   RE,ERRFLD                                                        
         LA    RE,1(RE)                                                         
         STC   RE,ERRFLD                                                        
         LA    R4,32(R4)                                                        
         BCT   R6,VK500                                                         
         MVC   CONHEAD,SPACES                                                   
         SPACE                                                                  
VKXIT    B     EXIT                                                             
MVCUID   MVC   KEY+15(0),8(R2)                                                  
MVCNAM   MVC   KEY+7(0),8(R2)                                                   
         SPACE                                                                  
VKERR    OI    ERRFLD,X'F0'                                                     
         MVC   CONHEAD+17(5),=C'FIELD'                                          
         MVC   CONHEAD+23(1),ERRFLD                                             
MYERR    LA    R2,DONOPTH                                                       
MYERR2   MVI   GENSTAT2,USMYOK                                                  
         MVI   GCMODE,C' '         IF ERROR TURN OFF SLAVE & EXIT               
         GOTO1 ERREX2                                                           
         EJECT                                                                  
*                                                                               
VREC     DS    0H                                                               
         NI    FLAG,X'FF'-FDETCHGQ                                              
*                                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VR02                IF ACTION DEL, GET THE RECORD                
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR03                IF ACTION ADD, GET THE RECORD                
*                                                                               
VR02     DS    0H                                                               
         MVI   IOOPT,C'Y'                                                       
         MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
VR03     DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BNE   VR04               IF ACTION DEL, DELETE 05 ELEMENTS             
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'05',AIO),0,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VRXIT                                                            
*                                                                               
VR04     DS    0H                                                               
         LA    R2,DONOFNH          VALIDATE USER ID                             
         USING ENTRYD,R2                                                        
*                                                                               
VR05     DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR10                                                             
*                                                                               
         TM    ENTOFNH+4,X'20'                                                  
         BZ    VR10                                                             
         TM    ENTUSRH+4,X'20'                                                  
         BZ    VR10                                                             
         TM    ENTSTAH+4,X'20'                                                  
         BO    VR60                NO CHANGES - NEXT OFFNAME ENTRY              
*                                                                               
VR10     DS    0H                  VALIDATE HERE                                
         BRAS  RE,GETNUM                                                        
         MHI   RF,MAXELLQ                                                       
         LA    R3,NAMELST                                                       
         LA    R3,0(RF,R3)         INDEX INTO TABLE                             
*                                                                               
         LA    R1,ELEM                                                          
         XC    ELEM,ELEM                                                        
         BRAS  RE,VALNTRY                                                       
         CLI   ELEM,X'05'                                                       
         BE    VR50                                                             
*                                                                               
         CLI   ELEM,X'00'                                                       
         BE    VR50                                                             
* ERRORS HERE                                                                   
         CLI   ELEM,X'01'                                                       
         BNE   *+14                                                             
         MVC   CONHEAD(20),=CL20'OFFICE NAME REQUIRED'                          
         B     MYERR2                                                           
         CLI   ELEM,X'02'                                                       
         BNE   *+14                                                             
         MVC   CONHEAD(20),=CL20'USER ID REQUIRED'                              
         B     MYERR2                                                           
         CLI   ELEM,X'12'                                                       
         BNE   *+14                                                             
         MVC   CONHEAD(20),=CL20'INVALID USER ID'                               
         B     MYERR2                                                           
         CLI   ELEM,X'03'                                                       
         BNE   *+14                                                             
         MVC   CONHEAD(20),=CL20'STATION REQUIRED'                              
         B     MYERR2                                                           
         CLI   ELEM,X'13'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CONHEAD(20),=CL20'INVALID STATION'                               
         B     MYERR2                                                           
*                                                                               
VR50     DS    0H                                                               
* R3 HAS THE ADDRESS OF CURRENT ELEMENT                                         
         ZIC   R0,1(R3)                                                         
         SHI   R0,2                                                             
         STC   R0,BYTE                                                          
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'05',AIO),(BYTE,2(R3))            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    ENTOFNH+4,X'20'                                                  
         OI    ENTOFNH+6,X'80'                                                  
         OI    ENTUSRH+4,X'20'                                                  
         OI    ENTUSRH+6,X'80'                                                  
         OI    ENTSTAH+4,X'20'                                                  
         OI    ENTSTAH+6,X'80'                                                  
*                                                                               
         CLI   ELEM,X'00'          DELETING?                                    
         BE    VR60                YES - SKIP THE ADD CALL                      
*                                                                               
         LA    R6,ELEM                                                          
         GOTO1 HELLO,DMCB,(C'P',=C'GENFIL'),AIO,ELEM,0                          
*                                                                               
*                                                                               
VR60     DS    0H                  MOVE TO NEXT OFFICE NAME                     
         LA    RF,DONFLGH                                                       
         LHI   R0,ENTRYDLQ                                                      
         AR    R2,R0                                                            
         CR    R2,RF                                                            
         BL    VR05                                                             
*                                                                               
VRXIT    DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VRX05               IF ACTION DEL, JUST DO A PUT                 
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VRX10               IF ACTION ADD, JUST DO A PUT                 
*                                                                               
VRX05    DS    0H                                                               
         MVI   IOOPT,C'Y'                                                       
         MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VRX08               IF ACTION DEL, LEAVE ACTIVITY ELEM           
*                                                                               
* GET RID OF THE ACTIVITY ELEMENT                                               
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'F1',AIO),0                       
*                                                                               
VRX08    DS    0H                                                               
         GOTO1 PUTREC             SINCE AGYNAME RECORD IS THERE ALREADY         
         B     VRXX                                                             
*                                                                               
VRX10    DS    0H                                                               
         TM    FLAG,FDETCHGQ       DETAILS CHANGED?                             
         BZ    *+8                                                              
*                                                                               
VRXX     B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                  DISPLAY KEY                                  
DKEY     DS    0H                                                               
         LA    R2,DONNAMH                                                       
         MVC   8(25,R2),KEY+7                                                   
         OI    6(R2),X'80'                                                      
         MVC   SVKEY,KEY                                                        
*                                                                               
         CLC   OPTNAM,SPACES                                                    
         BNH   DKXIT                                                            
         MVC   DONSRT,OPTNAM                                                    
         OI    DONSRTH+6,X'80'                                                  
*                                                                               
DKXIT    B     EXIT                                                             
         EJECT                                                                  
*                                  DISPLAY RECORD                               
DREC     DS    0H                                                               
*                                                                               
*                                                                               
         LA    R2,DONOFNH                                                       
         BAS   RE,CLRSCRN                                                       
*                                                                               
         XCEF  NAMELST,'NAMELSLQ'                                               
         LA    R2,DONOFNH                                                       
         ST    R2,CURRFLD                                                       
*                                                                               
         TM    FLAG,FKEYCHGQ       DID THE KEY CHANGE?                          
         BZ    *+10                                                             
         XC    NEXTDTL,NEXTDTL                                                  
*                                                                               
         TM    FLAG,FDETCHGQ       HAVE ANY DETAILS CHANGED?                    
         BZ    *+14                NO                                           
         LA    R1,NAMELST          YES - STAY ON THIS SCREEN                    
         MVC   NEXTDTL,0(R1)                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    DR110                                                            
         LA    R2,DONNAMH                                                       
         MVC   CONHEAD(25),=CL25'RECORD DOES NOT EXIST'                         
         B     MYERR2                                                           
*                                                                               
DR100    BRAS  RE,NEXTEL                                                        
         BNE   DR280                                                            
DR110    OC    NEXTDTL,NEXTDTL                                                  
         BZ    DR130                                                            
         CLC   NEXTDTL,0(R6)                                                    
         BNE   DR100                                                            
         XC    NEXTDTL,NEXTDTL           TO SKIP SUBSEQUENT COMPARISONS         
*                                                                               
DR130    DS    0H                                                               
         BRAS  RE,DISNTRY                                                       
         BRAS  RE,GETNUM                                                        
         MHI   RF,MAXELLQ                                                       
         LA    R1,NAMELST                                                       
         LA    R1,0(RF,R1)               INDEX INTO TABLE                       
         MVC   0(MAXELLQ,R1),0(R6)       SAVE ELEMENT                           
*                                                                               
DR200    DS    0H                        ADVANCE BY 3 FIELDS                    
         LA    RF,DONFLGH                                                       
*                                                                               
         LHI   RE,ENTRYDLQ                                                      
         AR    R2,RE                                                            
         CR    R2,RF                                                            
         BL    DR100                                                            
*                                                                               
         MVC   NEXTDTL,0(R6)             SAVE LAST DETAIL                       
         B     DRXIT                                                            
*                                                                               
DR280    DS    0H                        HIT END OF SCREEN                      
         XC    NEXTDTL,NEXTDTL                                                  
*                                                                               
DRXIT    DS    0H                                                               
         NI    FLAG,X'FF'-(FKEYCHGQ+FDETCHGQ)                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                  LIST RECORDS                                 
LIST     DS    0H                                                               
*                                                                               
         LA    R2,LONSELH                                                       
         BAS   RE,CLRSCRN                                                       
         CLI   MODE,PRINTREP                                                    
         BNE   LS100                                                            
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
LS100    LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LS120                                                            
         MVC   KEY(2),=C'ZA'                                                    
         MVC   KEY+7(8),OPTNAM     AGY NAME                                     
         SPACE                                                                  
LS120    GOTO1 HIGH                                                             
         CLC   KEY(2),=C'ZA'        KEY ID                                      
         BNE   EXIT                                                             
         B     LS220                                                            
*                                                                               
LS200    GOTO1 SEQ                                                              
*                                                                               
LS220    CLC   KEY(2),KEYSAVE       KEY ID                                      
         BNE   EXIT                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
LS300    BAS   RE,NEXTEL                                                        
         BNE   LS200                                                            
         USING EZOIDEL,R6                                                       
*                                                                               
         OC    CUUID,CUUID                                                      
         BZ    *+14                                                             
         CLC   CUUID,EZOUID                                                     
         BNE   LS300                                                            
         OC    CUSTA,CUSTA                                                      
         BZ    *+14                                                             
         CLC   CUSTA,EZOSTA                                                     
         BNE   LS300                                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING LISTD,R3                                                         
         MVC   LNAME,KEY+7         AGENCY NAME                                  
*                                                                               
LS500    CLI   MODE,PRINTREP                                                    
         BE    LS600                                                            
         MVC   DMDSKADD,KEY+36                                                  
         MVI   USEIO,C'N'                                                       
         GOTO1 LISTMON                                                          
         B     LS200                                                            
         SPACE                                                                  
LS600    MVC   P(37),LISTAR                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    LISTAR,LISTAR                                                    
         B     LS200                                                            
         SPACE                                                                  
HDRTN    NTR1                                                                   
         LA    R3,H8                                                            
         MVC   0(11,R3),LONLIT                                                  
         MVC   132(25,R3),=25C'-'                                               
         B     EXIT                                                             
         EJECT                                                                  
CLRSCRN  NTR1                                                                   
         SPACE                                                                  
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
         SPACE                                                                  
         SR    RE,RE                                                            
         SPACE                                                                  
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     EXIT                                                             
         SPACE                                                                  
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
BDMEDERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDMEDMS),BDMEDMS                                       
         GOTO1 ERREX2                                                           
         SPACE                                                                  
INVALER  MVI   ERROR,INVALID                                                    
         LA    R2,CONRECH                                                       
         SPACE                                                                  
TRAPERR  MVI   GCMODE,C' '         IF ERROR TURN OFF SLAVE & EXIT               
         GOTO1 ERREX                                                            
         DC    H'0'                                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE                                                                  
BDMEDMS  DC    C'* ERROR * MEDIA MUST BE T, R, N, X, OR A *'                    
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,48,C'OFFICE NAME RECORDS'                                     
         SSPEC H2,46,C'-----------------------'                                 
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* R2 TO ADDRESS OFFICE NAME FIELD                                               
* R6 TO ADDRESS OFFICE IDENTIFIER ELEMENT                                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
DISNTRY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING EZOIDEL,R6                                                       
* OFFICE NAME                                                                   
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'20'                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),EZONAM                                                   
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
* UID                                                                           
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(8,R2),EZOUID                                                   
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
* STATION                                                                       
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         OC    EZOSTA,EZOSTA                                                    
         BZ    DISNTRYX                                                         
*                                                                               
         CLC   =C'ALL ',EZOSTA                                                  
         BNE   DISN20                                                           
         MVC   8(5,R2),EZOSTA      STATION                                      
         MVI   11(R2),C'-'                                                      
         CLI   12(R2),C' '                                                      
         JH    EXIT                                                             
         MVI   12(R2),C'A'         BLANK MEANS ALL MEDIA                        
         J     EXIT                                                             
*                                                                               
DISN20   LA    R1,EZOSTA                                                        
         GOTO1 VPRNTSTA                                                         
         MVC   8(L'PRTSTA7C,R2),PRTSTA7C    STATION                             
*                                                                               
DISNTRYX J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
* R2 EXPECTED TO ADDRESS CURRENT OFFICE NAME ENTRY                              
* RF WILL HAVE NUMBER OF THE FIELD (POSITION IN NAME TABLE)                     
GETNUM   DS    0H                                                               
         SR    R0,R0                                                            
         LR    R1,R2               FIELD ADDRESS                                
         LA    RF,DONOFNH          FIRST FIELD                                  
         SR    R1,RF               DISPLACEMENT OF CURR FIELD                   
         LHI   RF,DONON2H-DONOFNH  L' ONE DISPLAY SLOT(OFF,UID,STA)             
         DR    R0,RF                                                            
         LR    RF,R1                                                            
         LTR   R0,R0               ANY REMAINDER                                
         BR    RE                  NO - OK                                      
         DC    H'0'                DIE OTHERWISE                                
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* R2 TO ADDRESS OFFICE NAME FIELD                                               
* R1 TO ADDRESS OFFICE IDENTIFIER ELEMENT                                       
* ON EXIT EQUAL CONDITION RETURNED IF FIELDS ARE OK                             
* OR UNEQUAL CONDITION SET, ERROR PUT IN HEADER AND CURSOR ADDRESS SET          
* TO INVALID FIELD.                                                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
VALNTRY  NTR1  BASE=*,LABEL=*                                                   
         LR    R4,R2                                                            
         USING ENTRYD,R4                                                        
         USING EZOIDEL,R3                                                       
         LR    R3,R1                                                            
* IF ALL 3 INPUT LENGTHS ARE 0 - WE ARE DELETING THIS ENTRY                     
         CLI   ENTSTAH+5,X'00'                                                  
         BNE   VALN10                                                           
         CLI   ENTUSRH+5,X'00'                                                  
         BNE   VALN10                                                           
         CLI   ENTOFNH+5,X'00'                                                  
         BE    VALN80                                                           
*                                                                               
VALN10   DS    0H                                                               
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVI   EZOIDEL,X'05'                                                    
*                                                                               
* OFFICE NAME                                                                   
         ZIC   R1,ENTOFNH+5                                                     
         CHI   R1,0                                                             
         BH    *+12                                                             
         MVI   EZOIDEL,X'01'                                                    
         B     VALNTRYX                                                         
*                                                                               
         LHI   R0,EZONAM-EZOIDEL                                                
         AR    R0,R1                                                            
         STC   R0,EZOIDEL+1                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   EZONAM(0),ENTOFN                                                 
*                                                                               
* USER ID                                                                       
         CLI   ENTUSRH+5,X'00'                                                  
         BNE   *+12                                                             
         MVI   EZOIDEL,X'02'                                                    
         B     VALNTRYX                                                         
*                                                                               
         LA    R1,KEY                                                           
         USING CTIREC,R1                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,CTIKTYPQ                                                 
         ZIC   RE,ENTUSRH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CTIKID(0),ENTUSR                                                 
         OC    CTIKID,SPACES                                                    
         DROP  R1                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'CTFILE ',KEY,AIO2                    
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         MVI   EZOIDEL,X'12'                                                    
         B     VALNTRYX                                                         
         L     R6,AIO2                                                          
         CLC   KEY(L'CTIKEY),0(R6)                                              
         BE    *+12                                                             
         MVI   EZOIDEL,X'12'                                                    
         B     VALNTRYX                                                         
*                                                                               
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+12                                                             
         MVI   EZOIDEL,X'12'                                                    
         B     VALNTRYX                                                         
*                                                                               
         MVC   EZOUID,KEY+15      SAVE USER ID                                  
         MVC   EZOORIG,2(R6)      BINARY USER ID (ORIGIN)                       
*                                                                               
* STATION                                                                       
         CLI   ENTSTAH+5,X'00'                                                  
         BNE   *+12                                                             
         MVI   EZOIDEL,X'03'                                                    
         B     VALNTRYX                                                         
*                                                                               
         CLC   =C'ALL-',ENTSTA     THIS ALL STATIONS                            
         BNE   VALN60                                                           
*                                                                               
         CLI   ENTSTA+4,C'A'       ALL MEDIA?                                   
         BE    VALN50                                                           
*                                                                               
         LA    R1,ENTSTA+4         MEDIA CHARACTER                              
         ICM   R1,8,=AL1(EZMTMEDQ) LOOK FOR MEDIA                               
         GOTO1 VGETMED                                                          
         BE    VALN50                                                           
         MVI   EZOIDEL,X'13'       INVALID STATION                              
         B     VALNTRYX                                                         
*                                                                               
VALN50   MVC   QSTA(4),=C'ALL '                                                 
         MVC   QSTA+4(1),ENTSTA+4                                               
         CLI   QSTA+4,C'A'         THIS ALL MEDIAS                              
         BNE   *+8                                                              
         MVI   QSTA+4,C' '                                                      
         B     VALN70                                                           
*                                                                               
VALN60   DS    0H                                                               
         LA    R2,ENTSTAH                                                       
         GOTO1 VREADSTA            VALID STATION RETURNED IN WORK               
         BE    *+12                                                             
         MVI   EZOIDEL,X'13'       INVALID STATION                              
         B     VALNTRYX                                                         
*                                                                               
         MVC   QSTA,FLDRDSTA                                                    
                                                                                
VALN70   MVC   EZOSTA,QSTA                                                      
*                                                                               
* FIRST RESTORE DATAMGR READ/GETREC SEQUENCE                                    
* SKIP RESTORE IF ADDING                                                        
         CLI   ACTNUM,ACTADD                                                    
         BE    VALN80                                                           
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         MVC   KEYSAVE,SAVEKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'GENDIR ',KEY,KEY                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY+EZADDA-EZAKEY,   X        
               AIO2,WORK                                                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATADISP,=H'42'                                                  
*                                                                               
VALN80   DS    0H                                                               
         OI    FLAG,FDETCHGQ       DETAILS CHANGED                              
*                                                                               
VALNTRYX J     EXIT                                                             
         LTORG                                                                  
         DROP                                                                   
*                                                                               
*                                                                               
SETUP    NTR1  BASE=*,LABEL=*                                                   
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
*                                                                               
         CLC   TWASCR,CALLSTCK                                                  
         BNE   *+12                                                             
         MVI   CALLSTCK,0                                                       
         MVI   CALLSP,0                                                         
*                                                                               
         XC    DONPFKY,DONPFKY     CLEAR PFKEY DISPLAY FIELD ON SCREEN          
*                                                                               
         OI    CONSERVH+6,X'80'                                                 
         OI    CONSERVH+1,X'01'                                                 
*                                                                               
         MVC   DONPFKY+50(11),=C'PF12=RETURN'  DISPLAY PF KEY INFO              
         OI    DONPFKYH+6,X'80'    TRANSMIT                                     
*                                                                               
         OI    GENSTAT2,RETEQSEL                                                
         CLI   PFKEY,12                                                         
         BNE   SETUPX                                                           
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         OI    GENSTAT2,NEXTSEL                                                 
         MVI   PFKEY,0                                                          
*                                                                               
SETUPX   J     EXIT                                                             
         DROP                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPEZFFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
* SPEZFFBD                                                                      
       ++INCLUDE SPEZFFBD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
* SPEZFEBD                                                                      
       ++INCLUDE SPEZFEBD                                                       
       ++INCLUDE DDGENTWA                                                       
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
* SPGENEZ                                                                       
       ++INCLUDE SPGENEZ                                                        
         EJECT                                                                  
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPEZDSCTS                                                      
*                                                                               
*                                                                               
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
RELO     DS    A                                                                
F04WORK  DS    CL32                                                             
ERRFLD   DS    CL1                 ERROR FIELD                                  
CUUID    DS    CL8                 USER ID FILTER                               
CUSTA    DS    CL5                 STATION FILTER                               
OPTNAM   DS    CL8                 START NAME                                   
SVUID    DS    CL8                 USER ID FROM CONTROL SYSTEM                  
SVORG    DS    CL2                 ORIGIN FROM CONTROL SYSTEM                   
SAVEKEY  DS    XL32                                                             
*                                                                               
MAXENTQ  EQU   30                  MAX NUMBER OF ENTRIES                        
MAXELLQ  EQU   37                  MAX OFFICE IDENTIFIER ELEM LENGTH            
*                                                                               
FLAG     DS    X                                                                
FKEYCHGQ EQU   X'01'                                                            
FDETCHGQ EQU   X'02'                                                            
*                                                                               
CURRFLD  DS    A                                                                
NEXTDTL  DS    XL(MAXELLQ)         NEXT DETAIL TO DISPLAY                       
*                                                                               
NAMELST  DS    (MAXENTQ*MAXELLQ)X                                               
NAMELSLQ EQU   *-NAMELST                                                        
*                                                                               
*                                                                               
*                                                                               
ENTRYD   DSECT                                                                  
ENTOFNH  DS    CL8                                                              
ENTOFN   DS    CL18                                                             
ENTUSRH  DS    CL8                                                              
ENTUSR   DS    CL8                                                              
ENTSTAH  DS    CL8                                                              
ENTSTA   DS    CL7                                                              
ENTRYDLQ EQU   *-ENTRYD                                                         
*                                                                               
LISTD    DSECT                                                                  
LNAME    DS    CL25                                                             
*                                                                               
WRKFEND  EQU   *                                                                
*                                                                               
         SPACE 2                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 2                                                                
PRNTD    DSECT                                                                  
PNAME    DS    CL25                                                             
         DS    CL2                                                              
PUSERID  DS    CL6                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018SPEZF04   01/27/16'                                      
         END                                                                    
