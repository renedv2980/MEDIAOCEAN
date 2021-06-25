*          DATA SET SPEZF02    AT LEVEL 011 AS OF 05/01/02                      
*PHASE T23002A                                                                  
         TITLE 'T23002 - CLIENT NAME RECORD'                                    
***********************************************************************         
*                                                                     *         
*  TITLE: T23002 - EASI CLIENT NAME RECORDS                           *         
*  COMMENTS: THIS PROGRAM DOES MAINT AND LIST FOR CLIENT IDENT RECS   *         
*  OUTPUTS: UPDATED CLIENT NAME RECORDS                               *         
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
*  LEV  9    APR07/93 FIX STATION VALIDATION - INCLUDE ALL            *         
*  LEV 10    DEC16/99 FIX VALIFAS                                     *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T23002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3002**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         MVC   AIO,AIO1                                                         
         MVI   CURSYST,C'P'        SWITCH TO MEDIA PLANNING (MPL)               
         GOTO1 VALIFAS             SWITCH                                       
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* READ CLIENT FROM MEDIA MPL FILE                                               
         SPACE                                                                  
VKEY     DS   0H                                                                
         LA    R2,DCLNAMH                                                       
         CLI   ACTNUM,ACTLIST      UNLESS A LIST NOT MANDATORY                  
         BE    VK200                                                            
         CLI   5(R2),0             NAME MANDATORY                               
         BNE   VK100                                                            
         MVC   CONHEAD(20),=C'CLIENT NAME REQUIRED'                             
         B     MYERR                                                            
         SPACE                                                                  
VK100    XC    KEY,KEY             SET UP KEY/SVKEY                             
         MVC   KEY(2),=C'ZC'                                                    
         MVC   KEY+2(2),AGENCY                                                  
         MVC   KEY+7(25),SPACES                                                 
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCNAM                                                        
         MVC   SVKEY,KEY                                                        
         B     VK600                                                            
*                                                                               
VK200    LA    R2,DCLSRTH                                                       
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   OPNAM,8(R2)                                                      
*                                                                               
         LA    R2,DCLOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VK600                                                            
         MVI   ERRFLD,1                                                         
         LA    R6,2                                                             
         LA    R4,WORK                                                          
         MVC   CONHEAD(14),=C'INVALID OPTION'                                   
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    MYERR                                                            
         SPACE                                                                  
VK300    CLI   0(R4),0                                                          
         BZ    VK600                                                            
         CLI   0(R4),1                                                          
         BNE   MYERR                                                            
         SPACE                                                                  
         CLI   12(R4),C'C'         CLIENT CODE                                  
         BNE   VK400                                                            
         MVC   CONHEAD(14),=C'INVALID CLIENT'                                   
         CLI   1(R4),2                                                          
         BL    VKERR                                                            
         MVC   OPCLI,22(R4)                                                     
         SPACE                                                                  
VK400    CLI   12(R4),C'S'         STATION CODE                                 
         BNE   VKERR                                                            
         MVC   CONHEAD(15),=C'INVALID STATION'                                  
         CLI   1(R4),7                                                          
         BH    VKERR                                                            
         MVC   OPSTA(4),22(R4)                                                  
         MVI   OPSTA+4,C' '                                                     
         LA    RE,25(R4)                                                        
         CLI   0(RE),C'-'                                                       
         BNE   VK420                                                            
         LA    RE,1(RE)                                                         
         MVI   OPSTA+3,C' '                                                     
         B     *+8                                                              
VK420    LA    RE,2(RE)                                                         
         CLI   0(RE),C'T'                                                       
         BE    VK500                                                            
         MVC   OPSTA+4(1),0(RE)                                                 
*                                                                               
*                                  UP ERROR FIELD & NEXT BLOCK                  
VK500    ZIC   RE,ERRFLD                                                        
         LA    RE,1(RE)                                                         
         STC   RE,ERRFLD                                                        
         LA    R4,32(R4)                                                        
         BCT   R6,VK300                                                         
*                                                                               
VK600    MVC   CONHEAD,SPACES                                                   
*                                                                               
VKXIT    B     EXIT                                                             
MVCNAM   MVC   KEY+7(0),8(R2)                                                   
*                                                                               
VKERR    OI    ERRFLD,X'F0'                                                     
         MVC   CONHEAD+17(5),=C'FIELD'                                          
         MVC   CONHEAD+23(1),ERRFLD                                             
MYERR    LA    R2,DCLOPTH                                                       
MYERR2   MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         EJECT                                                                  
*              VALIDATE AND DISPLAY RECORD                                      
VREC     DS    0H                                                               
         MVC   SVKEY,KEY                                                        
         L     R0,AIO1                                                          
         LA    R1,2000                                                          
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         SPACE                                                                  
         MVI   CURSYST,C'M'        SWITCH TO MEDIA SPTFIL/DIR                   
         GOTO1 VALIFAS             SWITCH                                       
         LA    R5,32               MAX SCREEN ENTRIES                           
         L     R6,AIO2                                                          
         USING EZCNMD,R6                                                        
         MVC   EZCKTYP(32),SVKEY   KEY                                          
         MVC   EZCLEN,=H'43'       LENGTH = 42(KEY) +1(END 0)                   
         LA    R4,EZCELS                                                        
         LA    R2,DCLCDEH          VALIDATE CLIENT CODE                         
         CLI   5(R2),0             1ST REQUIRED                                 
         BNE   VR100                                                            
         MVC   CONHEAD(15),=C'CLIENT REQUIRED'                                  
         B     MYERR2                                                           
*                                                                               
VR100    GOTO1 VALIMED                                                          
VR120    CLI   5(R2),0                                                          
         BNE   VR140                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         CLI   5(R2),0                                                          
         BE    VR220                                                            
         MVC   CONHEAD(23),=C'STATION REQUIRES CLIENT'                          
         B     MYERR2                                                           
*                                                                               
VR140    GOTO1 VALICLT                                                          
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         OI    6(R2),X'80'                                                      
         MVC   8(20,R2),CLTNM        TRANSMIT NAME                              
         XC    0(12,R4),0(R4)                                                   
         MVC   0(2,R4),=X'020C'                                                 
         MVC   2(3,R4),QCLT                                                     
         SR    RE,RE                                                            
         ICM   RE,3,EZCLEN                                                      
         AH    RE,=H'12'                                                        
         STCM  RE,3,EZCLEN                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         SPACE                                                                  
* VALIDATE STATION (DOES NOT READ FROM STATION FILE)                            
         SPACE                                                                  
         CLI   5(R2),0                                                          
         BE    VR200                                                            
         SPACE                                                                  
         MVI   QMED,C'A'           ALLOW ALL MEDIAS                             
         SPACE                                                                  
         CLC   =C'ALL-',8(R2)      THIS ALL STATIONS                            
         BNE   VR160                                                            
         CLI   12(R2),C'T'         THIS TV                                      
         BE    VR150                                                            
         CLI   12(R2),C'R'         THIS RADIO                                   
         BE    VR150                                                            
         CLI   12(R2),C'X'         THIS NETWORK RADIO                           
         BE    VR150                                                            
         CLI   12(R2),C'N'         THIS NETWORK                                 
         BE    VR150                                                            
         CLI   12(R2),C'A'         THIS ALL MEDIAS                              
         BNE   BDMEDERR                                                         
         SPACE                                                                  
VR150    MVC   QSTA(4),=C'ALL '                                                 
         MVC   QSTA+4(1),12(R2)                                                 
         B     VR164                                                            
         SPACE                                                                  
VR160    GOTO1 VALISTA             VALID STATION RETURNED IN WORK               
         SPACE                                                                  
VR164    MVC   5(5,R4),QSTA                                                     
         SPACE                                                                  
VR200    LA    R4,12(R4)                                                        
VR220    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R5,VR120                                                         
*                                                                               
VR500    MVI   CURSYST,C'P'        SWITCH TO MEDIA PLANNING (MPL)               
         GOTO1 VALIFAS             SWITCH                                       
         L     R0,AIO1                                                          
         LA    R1,2000                                                          
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         SPACE                                                                  
         MVC   KEY,SVKEY           RESET KEY                                    
         SPACE                                                                  
         CLI   ACTNUM,ACTADD                                                    
         BE    VRXIT                                                            
         GOTO1 HIGH                CHANGE RE-READ KEY                           
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
VRXIT    DS   0H                                                                
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                  DISPLAY KEY                                  
DKEY     DS    0H                                                               
         LA    R2,DCLNAMH                                                       
         MVC   8(25,R2),KEY+7                                                   
         OI    6(R2),X'80'                                                      
         MVC   SVKEY,KEY                                                        
DKXIT    B     EXIT                                                             
         EJECT                                                                  
*                                  DISPLAY RECORD                               
DREC     DS    0H                                                               
         MVC   SVKEY,KEY                                                        
         L     R0,AIO1                                                          
         LA    R1,2000                                                          
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         SPACE                                                                  
         MVI   CURSYST,C'M'        SWITCH TO MEDIA SPTFIL/DIR                   
         GOTO1 VALIFAS                                                          
         GOTO1 VALIMED                                                          
         LA    R2,DCLCDEH                                                       
         BAS   RE,CLRSCRN                                                       
         L     R6,AIO2                                                          
         MVC   DATADISP,=H'42'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    DR110                                                            
         DC    H'0'                                                             
DR100    BAS   RE,NEXTEL                                                        
         BNE   DR300                                                            
         USING EZCIDEL,R6                                                       
DR110    MVI   5(R2),3             SET LENGTH FOR VALICLT                       
         MVC   AIO,AIO2            USE SECOND IO AREA                           
         MVC   8(3,R2),EZCCOD      CLIENT CODE                                  
         OI    6(R2),X'80'                                                      
         CLC   EZCCOD,=C'***'      THIS UNKNOWN CLIENT                          
         BNE   DR120                                                            
         MVC   CLTNM,=CL20'UNKNOWN CLIENT'                                      
         B     DR140                                                            
         SPACE                                                                  
DR120    GOTO1 VALICLT                                                          
         SPACE                                                                  
DR140    MVC   DATADISP,=H'42'                                                  
         MVC   AIO,AIO1            RESET TO 1ST IO AREA                         
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(20,R2),CLTNM                                                   
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         OI    6(R2),X'80'                                                      
         OC    EZCSTA,EZCSTA                                                    
         BZ    DR200                                                            
         MVC   8(4,R2),EZCSTA      STATION                                      
         LA    RE,11(R2)                                                        
         CLI   0(RE),C' '                                                       
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'-'                                                       
         LA    RF,MEDTBL                                                        
         LA    R0,4                                                             
DR160    CLC   EZCMED,0(RF)                                                     
         BE    DR180                                                            
         LA    RF,3(RF)                                                         
         BCT   R0,DR160                                                         
         DC    H'0'                                                             
DR180    MVC   1(2,RE),1(RF)       MEDIA                                        
*                                                                               
DR200    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     DR100                                                            
*                                                                               
DR300    MVI   CURSYST,C'P'        SWITCH TO MEDIA PLANNING (MPL)               
         GOTO1 VALIFAS                                                          
         MVC   KEY,SVKEY           RESET & READ KEY                             
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
*              LIST RECORDS                                                     
LIST     DS    0H                                                               
         LA    R2,LCLSELH                                                       
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
         MVC   KEY(2),=C'ZC'                                                    
         MVC   KEY+2(2),AGENCY                                                  
         MVC   KEY+7(8),OPNAM      CLT NAME                                     
*                                                                               
LS120    GOTO1 HIGH                                                             
         CLC   KEY(2),=C'ZC'        KEY ID                                      
         BNE   EXIT                                                             
         B     LS220                                                            
*                                                                               
LS200    GOTO1 SEQ                                                              
LS220    CLC   KEY(4),KEYSAVE       KEY ID/CLT                                  
         BNE   EXIT                                                             
         GOTO1 GETREC                                                           
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING LISTD,R3                                                         
         MVC   LNAME,KEY+7         AGENCY NAME                                  
         LA    R4,LCLI                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LS300    BAS   RE,NEXTEL                                                        
         BNE   LS500                                                            
         USING EZCIDEL,R6                                                       
         SPACE                                                                  
* IF FILTERS MATCH ON FILTER                                                    
         SPACE                                                                  
         OC    OPSTA,OPSTA         STATION                                      
         BZ    *+14                                                             
         CLC   EZCSTA,OPSTA                                                     
         BNE   LS300                                                            
         OC    OPCLI,OPCLI         USER ID                                      
         BZ    *+14                                                             
         CLC   EZCCOD,OPCLI                                                     
         BNE   LS300                                                            
*                                                                               
         LA    R0,LCLI                                                          
         CR    R4,R0               END OF LINE                                  
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         MVC   0(3,R4),EZCCOD                                                   
         LA    R4,2(R4)                                                         
         CLI   EZCCOD+2,C' '                                                    
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         OC    EZCSTA,EZCSTA                                                    
         BZ    LS400                                                            
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         MVC   0(4,R4),EZCSTA      STATION                                      
         LA    R4,3(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'-'                                                       
         LA    RF,MEDTBL                                                        
         LA    R0,3                                                             
LS340    CLC   EZCMED,0(RF)                                                     
         BE    LS360                                                            
         LA    RF,3(RF)                                                         
         BCT   R0,LS340                                                         
         DC    H'0'                                                             
LS360    MVC   1(2,R4),1(RF)       MEDIA                                        
         LA    R4,2(R4)                                                         
LS400    LA    R0,LCLI+67                                                       
         CR    R4,R0               END OF LINE                                  
         BL    LS300                                                            
*                                                                               
LS500    CLI   MODE,PRINTREP                                                    
         BE    LS600                                                            
         MVC   DMDSKADD,KEY+36                                                  
         GOTO1 LISTMON                                                          
         B     LS200                                                            
         SPACE                                                                  
LS600    MVC   P+2(46),LISTAR                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    LISTAR,LISTAR                                                    
         B     LS200                                                            
         DROP  R6                                                               
         EJECT                                                                  
HDRTN    NTR1                                                                   
         B     EXIT                                                             
         SPACE 3                                                                
CLRSCRN  NTR1                                                                   
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     EXIT                                                             
*                                                                               
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
TRAPERR  GOTO1 ERREX                                                            
         DC    H'0'                                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE                                                                  
MEDTBL   DC    CL3' T '                                                         
         DC    CL3'TT '                                                         
         DC    CL3'AAM'                                                         
         DC    CL3'FFM'                                                         
BDMEDMS  DC    C'* ERROR * MEDIA MUST BE T, N, R, X, OR A *'                    
         SPACE                                                                  
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,48,C'CLIENT NAME RECORDS'                                     
         SSPEC H2,46,C'-----------------------'                                 
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'NAME'                                                     
         SSPEC H9,3,C'----'                                                     
         SSPEC H8,30,C'CLIENT STATION LIST'                                     
         SSPEC H9,30,C'-------------------'                                     
         DC    X'00'                                                            
         EJECT                                                                  
LISTD    DSECT                                                                  
LNAME    DS    CL25                                                             
         DS    CL2                                                              
LCLI     DS    CL6                                                              
         DS    CL1                                                              
LSTA     DS    CL7                                                              
         EJECT                                                                  
* DDCOMFACS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPEZFFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
* SPEZFFDD                                                                      
       ++INCLUDE SPEZFFDD                                                       
         ORG   CONTAGH                                                          
* SPEZFFDD                                                                      
       ++INCLUDE SPEZFEDD                                                       
         PRINT OFF                                                              
         EJECT                                                                  
* SPGENCLI                                                                      
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
* SPGENEZ                                                                       
       ++INCLUDE SPGENEZ                                                        
         EJECT                                                                  
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
         PRINT ON                                                               
         ORG   SYSSPARE                                                         
* WORK AREAS FOR THIAS PROG                                                     
         SPACE                                                                  
ERRFLD   DS    CL1                 ERROR FIELD                                  
OPNAM    DS    CL8                                                              
OPCLI    DS    CL3                                                              
OPSTA    DS    CL5                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPEZF02   05/01/02'                                      
         END                                                                    
