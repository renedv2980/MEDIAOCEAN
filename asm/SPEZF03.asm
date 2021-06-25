*          DATA SET SPEZF03    AT LEVEL 022 AS OF 05/01/02                      
*PHASE T23003A                                                                  
         TITLE 'T23003 - PRODUCT NAME RECORD'                                   
***********************************************************************         
*                                                                     *         
*  TITLE: T23003 - EASI PRODUCT NAME RECORDS                          *         
*  COMMENTS: THIS PROGRAM DOES MAINT AND LIST FOR PRODUCT IDEN RECS   *         
*  OUTPUTS: UPDATED PRODUCT NAME RECORDS.                             *         
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
*  LEV 21    NOV07/91 CHANGE TO FASWITCH                              *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T23003   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3003**                                                       
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
         XC    F03WORK,F03WORK                                                  
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
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                  VALIDATE KEY                                 
VKEY     DS    0H                                                               
         LA    R2,DPRCLTH                                                       
         CLI   5(R2),0             NAME MANDATORY                               
         BNE   *+14                                                             
         MVC   CONHEAD(20),=C'CLIENT CODE REQUIRED'                             
         B     MYERR2                                                           
         SPACE                                                                  
         MVI   CURSYST,C'M'        SWITCH TO MEDIA (SPT)                        
         GOTO1 VALIFAS                                                          
         GOTO1 VALIMED                                                          
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         LA    R2,DPRNAMH                                                       
         CLI   ACTNUM,ACTLIST      UNLESS A LIST NOT MANDATORY                  
         BE    VK100                                                            
         CLI   5(R2),0             NAME MANDATORY                               
         BNE   VK600                                                            
         MVC   CONHEAD(21),=C'PRODUCT NAME REQUIRED'                            
         B     MYERR2                                                           
         SPACE                                                                  
         SPACE                                                                  
VK100    LA    R2,DPRSRTH                                                       
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   OPNAM,8(R2)                                                      
         SPACE                                                                  
VK200    LA    R2,DPROPTH                                                       
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
*                                  PRODUCT CODE                                 
         CLI   12(R4),C'P'                                                      
         BNE   VK400                                                            
         MVC   CONHEAD(15),=C'INVALID PRODUCT'                                  
         CLI   1(R4),2                                                          
         BL    VKERR                                                            
         MVC   OPPRD,22(R4)                                                     
         SPACE                                                                  
*                                  STATION NO REAL VALIDATION                   
VK400    CLI   12(R4),C'S'                                                      
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
         SPACE                                                                  
*                                  UP ERROR FIELD & NEXT BLOCK                  
VK500    ZIC   RE,ERRFLD                                                        
         LA    RE,1(RE)                                                         
         STC   RE,ERRFLD                                                        
         LA    R4,32(R4)                                                        
         BCT   R6,VK300                                                         
         SPACE                                                                  
         LA    R2,DPRSRTH          POINT TO START FIELD                         
         SPACE                                                                  
VK600    MVC   CONHEAD,SPACES                                                   
         MVI   CURSYST,C'P'        SWITCH TO MEDIA PLANNING (MPL)               
         GOTO1 VALIFAS             SWITCH                                       
         SPACE                                                                  
         XC    KEY,KEY             SET UP KEY/SVKEY                             
         MVC   KEY(2),=C'ZP'                                                    
         MVC   KEY+2(2),AGENCY                                                  
         MVC   KEY+4(3),QCLT                                                    
         MVC   KEY+7(25),SPACES                                                 
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    VK610                                                            
         BCTR  RE,0                                                             
         EX    RE,MVCNAM                                                        
VK610    MVC   SVKEY,KEY                                                        
         B     EXIT                                                             
MVCNAM   MVC   KEY+7(0),8(R2)                                                   
         SPACE                                                                  
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE                                                                  
VREC     DS    0H                                                               
         MVI   CURSYST,C'M'        SWITCH TO MEDIA (SPT)                        
         GOTO1 VALIFAS             SWITCH TO SPOT TO VALIDATE PRODUCT           
         L     R6,AIO1                                                          
         USING EZPNMD,R6                                                        
         MVC   EZPKTYP(32),SVKEY   KEY                                          
         MVC   EZPLEN,=H'43'       LENGTH = 42(KEY) +1(END 0)                   
         LA    R4,EZPELS                                                        
         USING EZPIDEL,R4                                                       
         LA    R2,DPRCDEH          VALIDATE PRODUCT CODE                        
         CLI   5(R2),0             1ST REQUIRED                                 
         BNE   VR120                                                            
         MVC   CONHEAD(16),=C'PRODUCT REQUIRED'                                 
         B     MYERR2                                                           
         SPACE                                                                  
VR100    CLI   5(R2),0             ANY PROD CODE(S) ENTERED                     
         BNE   VR120                YES                                         
         ZIC   RE,0(R2)                                                         
         LA    RF,0(RE,R2)         STATION FIELD                                
         CLI   5(RF),0                                                          
         BNE   MISPRDER                                                         
         AR    R2,RE               STATION FIELD                                
         IC    RE,0(R2)                                                         
         AR    R2,RE               PRODUCT NAME 1                               
         IC    RE,0(R2)                                                         
         AR    R2,RE               PRODUCT NAME 2                               
         IC    RE,0(R2)                                                         
         AR    R2,RE               NEXT LINE (PRD/PTR/EST                       
         B     VR300                                                            
         SPACE                                                                  
VR120    XC    0(EZPELEN+2,R4),0(R4)                                            
         MVI   0(R4),X'02'                                                      
         MVI   1(R4),EZPELEN                                                    
         CLI   5(R2),3                                                          
         BH    VR130                                                            
         GOTO1 VALIPRD                                                          
         ZIC   RE,0(R2)                                                         
         LA    RF,0(RE,R2)         STATION                                      
         IC    RE,0(RF)                                                         
         AR    RF,RE               PRODUCT NAME                                 
         OI    6(RF),X'80'                                                      
         MVC   8(20,RF),WORK+4       TRANSMIT NAME                              
         MVC   EZPCOD,WORK                                                      
         B     VR180                                                            
         SPACE                                                                  
VR130    LA    R3,8(,R2)                                                        
         ZIC   R5,5(R2)                                                         
         MVI   BYTE,0                                                           
         SPACE                                                                  
VR140    XC    ELEM,ELEM                                                        
         LA    R1,ELEM+8                                                        
VR150    CLI   0(R3),C'-'                                                       
         BE    VR160                                                            
         CLI   0(R3),C','                                                       
         BE    VR160                                                            
         MVC   0(1,R1),0(R3)                                                    
         LA    R1,1(,R1)                                                        
         ZIC   RF,ELEM+5                                                        
         LA    RF,1(,RF)                                                        
         STC   RF,ELEM+5                                                        
         CH    RF,=H'3'                                                         
         BH    PRDSIZER                                                         
         LA    R3,1(,R3)                                                        
         BCT   R5,VR150                                                         
VR160    ZIC   RF,BYTE                                                          
         LA    RF,1(,RF)                                                        
         STC   RF,BYTE                                                          
         ZIC   RF,ELEM+5                                                        
         BCTR  RF,0                                                             
         EX    RF,VRMVN                                                         
         EX    RF,VRCLC            CK IF NUMERIC                                
         BE    VR170                                                            
         CLI   BYTE,2                                                           
         BH    MORPRDER                                                         
         ST    R2,SVR2                                                          
         LA    R2,ELEM                                                          
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
         L     R2,SVR2                                                          
         CLI   ERROR,0                                                          
         BNE   TRAPERR                                                          
         CLI   BYTE,1                                                           
         BNE   VR164                                                            
         MVC   EZPCOD,WORK                                                      
         ZIC   RE,0(R2)                                                         
         LA    RF,0(RE,R2)         NOW AT STATION FIELD                         
         IC    RE,0(RF)                                                         
         AR    RF,RE               NOW AT PROD 1 NAME FIELD                     
         OI    6(RF),X'80'                                                      
         MVC   8(20,RF),WORK+4       TRANSMIT NAME                              
         B     VR166                                                            
VR164    CLI   BYTE,2                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   EZPCOD2,WORK                                                     
         ZIC   RE,0(R2)                                                         
         LA    RF,0(RE,R2)         NOW AT STATION FIELD                         
         IC    RE,0(RF)                                                         
         AR    RF,RE               NOW AT PROD 1 NAME FIELD                     
         IC    RE,0(RF)                                                         
         AR    RF,RE               NOW AT PROD 2 NAME FIELD                     
         OI    6(RF),X'80'                                                      
         MVC   8(20,RF),WORK+4       TRANSMIT NAME                              
VR166    LTR   R5,R5                                                            
         BZ    VR180                                                            
         LA    R3,1(,R3)                                                        
         BCT   R5,VR140                                                         
         B     VR180                                                            
VRMVN    MVN   ZERO7(0),ELEM+8                                                  
VRCLC    CLC   ELEM+8(0),ZERO7                                                  
VRPACK   PACK  DUB,ELEM+8(0)                                                    
         SPACE                                                                  
VR170    EX    RF,VRPACK                                                        
         CVB   R0,DUB                                                           
         STC   R0,EZPEST                                                        
         LTR   R5,R5                                                            
         BNZ   ESTENTER                                                         
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD                                                  
         MVC   KEY+4(3),EZPCOD                                                  
         MVC   KEY+7(1),EZPEST                                                  
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOESTFND                                                         
         OC    EZPCOD2,EZPCOD2                                                  
         BZ    VR180                                                            
         MVC   KEY+4(3),EZPCOD2                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOESTFND                                                         
         SPACE                                                                  
VR180    XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               STATION FIELD                                
         SPACE                                                                  
         SR    RE,RE                                                            
         ICM   RE,3,EZPLEN                                                      
         AH    RE,=AL2(EZPELEN)                                                 
         STCM  RE,3,EZPLEN                                                      
         SPACE                                                                  
* READ STATION FROM STATION FILE                                                
         SPACE                                                                  
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 VALISTA             VALID STATION RETURNED IN WORK               
         MVC   EZPSTA,QSTA                                                      
VR200    LA    R3,EZPELS                                                        
         SPACE                                                                  
VR210    CR    R3,R4                                                            
         BE    VR214                                                            
         CLC   EZPSTA-EZPIDEL(,R3),EZPSTA-EZPIDEL(R4)                           
         BE    EQSTAERR                                                         
         LA    R3,EZPELEN(,R3)                                                  
         B     VR210                                                            
VR214    LA    R4,EZPELEN(,R4)                                                  
         SPACE                                                                  
         ZIC   RE,0(R2)            PRODUCT NAME                                 
         AR    R2,RE                                                            
         IC    RE,0(R2)            PARTNER NAME                                 
         AR    R2,RE                                                            
         IC    RE,0(R2)            NEXT LINE (PRD/PTR/EST)                      
         AR    R2,RE                                                            
VR300    CLI   0(R2),9             AT END OF SCREEN                             
         BH    VR100                NOT YET                                     
         SPACE                                                                  
VR500    MVC   KEY,SVKEY           RESET KEY                                    
         B     DREC                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                  DISPLAY KEY                                  
DKEY     DS    0H                                                               
         LA    R2,DPRCLTH                                                       
         MVC   8(3,R2),KEY+4                                                    
         OI    6(R2),X'80'                                                      
         MVI   5(R2),3                                                          
         LA    R2,DPRNAMH                                                       
         MVC   8(25,R2),KEY+7                                                   
         OI    6(R2),X'80'                                                      
         MVC   SVKEY,KEY                                                        
DKXIT    B     EXIT                                                             
         EJECT                                                                  
*                                  DISPLAY RECORD                               
DREC     DS    0H                                                               
         MVC   SVKEY,KEY                                                        
         MVI   CURSYST,C'M'        SWITCH TO MEDIA (SPT)                        
         GOTO1 VALIFAS                                                          
         GOTO1 VALIMED                                                          
         LA    R2,DPRCLTH                                                       
         GOTO1 VALICLT                                                          
         LA    R2,DPRCDEH                                                       
         BAS   RE,CLRSCRN                                                       
         L     R6,AIO                                                           
         MVC   DATADISP,=H'42'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    DR110                                                            
         DC    H'0'                                                             
DR100    BAS   RE,NEXTEL                                                        
         BNE   DR300                                                            
         USING EZPIDEL,R6                                                       
DR110    MVC   8(3,R2),EZPCOD      PRODUCT CODE                                 
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
         LA    R1,11(R2)                                                        
         CLI   10(R2),0                                                         
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         OC    EZPCOD2,EZPCOD2     CK IF P/B PROD                               
         BZ    DR120                                                            
         MVI   0(R1),C'-'                                                       
         MVC   1(3,R1),EZPCOD2                                                  
         LR    RF,R1                                                            
         LA    R1,4(,R1)                                                        
         CLI   3(RF),0                                                          
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         SPACE                                                                  
DR120    CLI   EZPEST,0            ANY ESTIMATE                                 
         BE    DR130                                                            
         ZIC   R0,EZPEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(R1),C','                                                       
         UNPK  1(3,R1),DUB                                                      
         SPACE                                                                  
* DISPLAY STATION IF ANY                                                        
         SPACE                                                                  
DR130    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         OI    6(R2),X'80'                                                      
         OC    EZPSTA,EZPSTA                                                    
         BZ    DR200                                                            
         MVC   8(4,R2),EZPSTA      STATION                                      
         CLC   =C'ALL',EZPSTA      STATION ALL                                  
         BE    DR200                                                            
         LA    RE,11(R2)                                                        
         CLI   0(RE),C' '                                                       
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'-'                                                       
         LA    RF,MEDTBL                                                        
         LA    R0,4                                                             
DR140    CLC   EZPMED,0(RF)                                                     
         BE    DR150                                                            
         LA    RF,3(RF)                                                         
         BCT   R0,DR140                                                         
         DC    H'0'                                                             
DR150    MVC   1(2,RE),1(RF)       MEDIA                                        
         SPACE                                                                  
DR200    ZIC   RE,0(R2)            DISPLAY PROD 1 NAME                          
         AR    R2,RE                                                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),EZPCOD                                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         LA    R5,=CL20'PRODUCT NOT FOUND'                                      
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DR210                                                            
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         USING PRDHDR,R4                                                        
         LA    R5,PNAME                                                         
         MVC   AIO,AIO1            RESET TO 1ST IO AREA                         
         DROP  R4                                                               
DR210    XC    FILENAME,FILENAME                                                
         MVC   8(20,R2),0(R5)                                                   
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
         ZIC   RE,0(R2)            DISPLAY PROD 2 NAME                          
         AR    R2,RE                                                            
         OC    EZPCOD2,EZPCOD2     CK IF P/B PROD                               
         BZ    DR230                                                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),EZPCOD2                                                 
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         LA    R5,=CL20'PRODUCT NOT FOUND'                                      
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DR220                                                            
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         USING PRDHDR,R4                                                        
         LA    R5,PNAME                                                         
         MVC   AIO,AIO1            RESET TO 1ST IO AREA                         
         DROP  R4                                                               
         SPACE                                                                  
DR220    XC    FILENAME,FILENAME                                                
         MVC   8(20,R2),0(R5)                                                   
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
DR230    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     DR100                                                            
         SPACE                                                                  
DR300    MVI   CURSYST,C'P'        SWITCH TO MEDIA PLANNING (MPL)               
         GOTO1 VALIFAS                                                          
         MVC   KEY,SVKEY           RESET & READ KEY                             
         CLI   ACTNUM,ACTADD                                                    
         BE    DRXIT                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
DRXIT    B     EXIT                                                             
         EJECT                                                                  
*                                  LIST RECORDS                                 
         SPACE                                                                  
LIST     CLI   MODE,PRINTREP                                                    
         BNE   LS100                                                            
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         SPACE                                                                  
LS100    LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LS120                                                            
         MVC   KEY(2),=C'ZP'                                                    
         MVC   KEY+2(2),AGENCY                                                  
         MVC   KEY+4(3),QCLT                                                    
         MVC   KEY+7(8),OPNAM      AGY NAME                                     
         SPACE                                                                  
LS120    GOTO1 HIGH                                                             
         CLC   KEY(2),=C'ZP'                                                    
         BNE   EXIT                                                             
         CLC   KEY+2(2),AGENCY                                                  
         BNE   EXIT                                                             
         CLC   KEY+4(3),QCLT                                                    
         BNE   EXIT                                                             
         B     LS220                                                            
         SPACE                                                                  
LS200    GOTO1 SEQ                                                              
         SPACE                                                                  
LS220    CLC   KEY(7),KEYSAVE       KEY ID/AGENCY/CLIENT                        
         BNE   EXIT                                                             
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   P1,SPACES                                                        
         MVC   LNAME,KEY+7         AGENCY NAME                                  
         LA    R4,LPRD                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LS300    BAS   RE,NEXTEL                                                        
         BNE   LS500                                                            
         USING EZPIDEL,R6                                                       
         SPACE                                                                  
* IF FILTERS MATCH ON FILTER                                                    
         SPACE                                                                  
         OC    OPSTA,OPSTA         STATION                                      
         BZ    *+14                                                             
         CLC   EZPSTA,OPSTA                                                     
         BNE   LS300                                                            
         OC    OPPRD,OPPRD         PRODUCT FILTER                               
         BZ    *+14                                                             
         CLC   EZPCOD,OPPRD                                                     
         BNE   LS300                                                            
         SPACE                                                                  
         LA    R0,LPRD+30          ROOM FOR ANOTHER PRD,PTR,EST/WABC-T          
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         LA    R0,LPRD+65          ROOM FOR ANOTHER PRD,PTR,EST/WABC-T          
         OC    EZPCOD2,EZPCOD2                                                  
         BNZ   *+8                                                              
         AH    R0,=H'4'                                                         
         OC    EZPSTA,EZPSTA                                                    
         BNZ   *+8                                                              
         AH    R0,=H'7'                                                         
         CR    R4,R0               CURR SPOT IN LINE                            
         BH    LS440                                                            
         SPACE                                                                  
         LA    R0,LPRD                                                          
         CR    R4,R0               END OF LINE                                  
         BE    *+12                                                             
         MVI   0(R4),C':'                                                       
         LA    R4,1(R4)                                                         
         MVC   0(3,R4),EZPCOD                                                   
         LA    R4,2(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         OC    EZPCOD2,EZPCOD2                                                  
         BZ    LS310                                                            
         MVI   0(R4),C'-'                                                       
         MVC   1(3,R4),EZPCOD2                                                  
         LA    R4,3(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
LS310    CLI   EZPEST,0                                                         
         BE    LS320                                                            
         MVI   0(R4),C','                                                       
         ZIC   RF,EZPEST                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R4),DUB                                                      
         LA    R4,4(R4)                                                         
LS320    LA    R1,EZPSTA                                                        
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         SPACE                                                                  
         MVC   0(4,R4),0(R1)       STATION                                      
         SPACE                                                                  
         OC    0(5,R1),0(R1)       ALL STATIONS                                 
         BNZ   LS330                NO                                          
         MVC   0(3,R4),=C'ALL'                                                  
         LA    R4,3(R4)                                                         
         B     LS400                                                            
         SPACE                                                                  
LS330    CLC   =C'ALL',0(R1)       ALL STATIONS                                 
         BE    LS400                                                            
         LA    R4,3(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'-'                                                       
         LA    RF,MEDTBL                                                        
         LA    R0,3                                                             
LS340    CLC   4(1,R1),0(RF)                                                    
         BE    LS360                                                            
         LA    RF,3(RF)                                                         
         BCT   R0,LS340                                                         
         DC    H'0'                                                             
LS360    MVC   1(2,R4),1(RF)       MEDIA                                        
         LA    R4,2(R4)                                                         
LS400    B     LS300                                                            
         SPACE                                                                  
LS440    MVC   0(5,R4),=C',MORE'                                                
         SPACE                                                                  
LS500    CLI   MODE,PRINTREP                                                    
         BE    LS600                                                            
         MVC   DMDSKADD,KEY+36                                                  
         MVC   LISTAR,P1+2                                                      
         GOTO1 LISTMON                                                          
         B     LS200                                                            
         SPACE                                                                  
LS600    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LS200                                                            
         DROP  R6                                                               
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
VKERR    OI    ERRFLD,X'F0'                                                     
         MVC   CONHEAD+17(5),=C'FIELD'                                          
         MVC   CONHEAD+23(1),ERRFLD                                             
MYERR    LA    R2,DPROPTH                                                       
         GOTO1 ERREX2                                                           
MISPRDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MISPRDMS),MISPRDMS                                     
MYERR2   MVI   GENSTAT2,USMYOK                                                  
         B     ERREXIT                                                          
NOESTFND XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOESTMS),NOESTMS                                       
         B     ERREXIT                                                          
ESTENTER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ESTENTMS),ESTENTMS                                     
         B     ERREXIT                                                          
MORPRDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MORPRDMS),MORPRDMS                                     
         B     ERREXIT                                                          
PRDSIZER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRDSIZMS),PRDSIZMS                                     
         B     ERREXIT                                                          
PRDENTER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRDENTMS),PRDENTMS                                     
         B     ERREXIT                                                          
EQSTAERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'EQSTAMS),EQSTAMS                                       
ERREXIT  GOTO1 ERREX2                                                           
         SPACE                                                                  
MISSERR  MVI   ERROR,MISSING                                                    
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         DC    H'0'                                                             
         SPACE 3                                                                
         LTORG                                                                  
ZERO7    DC    C'0000000'                                                       
         SPACE                                                                  
MEDTBL   DC    CL3' T '                                                         
         DC    CL3'TT '                                                         
         DC    CL3'AAM'                                                         
         DC    CL3'FFM'                                                         
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,48,C'PRODUCT NAME RECORDS'                                    
         SSPEC H2,46,C'------------------------'                                
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'NAME'                                                     
         SSPEC H9,3,C'----'                                                     
         SSPEC H8,3,C'PRD/PTR/EST/STATION LIST'                                 
         SSPEC H9,3,C'------------------------'                                 
         DC    X'00'                                                            
         SPACE                                                                  
MISPRDMS DC    C'* ERROR * STATION REQUIRES PRODUCT *'                          
NOESTMS  DC    C'* ERROR * NO ESTIMATE FOR THIS PROD(S) *'                      
ESTENTMS DC    C'* ERROR * ESTIMATE MUST BE ENTERED LAST *'                     
MORPRDMS DC    C'* ERROR * ONLY PRD AND PTR ALLOWED *'                          
PRDSIZMS DC    C'* ERROR * PRD, PTR, EST MAX SIZE = 3 *'                        
EQSTAMS  DC    C'* ERROR * SAME STATION ENTERED TWICE *'                        
PRDENTMS DC    C'* ERROR * ENTER AS PRD OR PRD,EST OR PRD,PTR,EST *'            
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
* SPEZFFCD                                                                      
       ++INCLUDE SPEZFFCD                                                       
         ORG   CONTAGH                                                          
* SPEZFECD                                                                      
       ++INCLUDE SPEZFECD                                                       
         PRINT OFF                                                              
         EJECT                                                                  
* SPGENCLI                                                                      
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
* SPGENPRD                                                                      
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
* SPGENEZ                                                                       
         PRINT ON                                                               
       ++INCLUDE SPGENEZ                                                        
         EJECT                                                                  
*SPEZFWORKD                                                                     
*        PRINT OFF                                                              
       ++INCLUDE SPEZFWORKD                                                     
         PRINT ON                                                               
         SPACE                                                                  
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
         DS    0F                                                               
F03WORK  DS   0CL21                                                             
SVR2     DS    F                                                                
OPNAM    DS    CL8                                                              
ERRFLD   DS    CL1                 ERROR FIELD                                  
OPPRD    DS    CL3                                                              
OPSTA    DS    CL5                                                              
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P1                                                               
         DS    CL2                                                              
LNAME    DS    CL25                                                             
         DS    CL2                                                              
LPRD     DS    CL48                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022SPEZF03   05/01/02'                                      
         END                                                                    
