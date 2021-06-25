*          DATA SET SPEZF06    AT LEVEL 043 AS OF 05/01/02                      
*PHASE T23006A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: T23006 - EASI INVOICE LIST AND MAINT                        *         
*  COMMENTS: THIS PROGRAM LISTS INVOICES AND CAN OVERIDE CLIENT       *         
*            AND/OR PRODUCT FOR EACH INVOICE.                         *         
*                                                                     *         
*  OUTPUTS: UPDATED INVOICE BATCHES                                   *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 -                                                       *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO INVOICE RECORD                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*                  - WORKER INDEX                                     *         
*             AIO2 - WORKER RECORD FOR UPDATE                         *         
*             AIO3 -                                                            
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*  LEV 38-39 DEC19/89 DO NOT DATE HEADER COMMENTS AREA OF WORKER REC  *         
*  LEV 40    JAN25/90 SAVE AND RESTORE WORKER COMMENT FIELD           *         
*  LEV 41    APR24/90 CK FOR DELETED INVOICES                         *         
*  LEV 42    SEP12/91 ADD NETWORK                                     *         
*                                                                     *         
***********************************************************************         
         TITLE 'T23006 - INVOICE LIST'                                          
T23006   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3006**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         MVC   AIO,AIO1                                                         
         GOTO1 VALIMED                                                          
         CLI   SPOTNETS,C'S'       IS THIS SPOT SYSTEM                          
         BNE   INIT10                                                           
         MVI   QMED,C'T'                                                        
         B     INIT30                                                           
INIT10   CLI   SPOTNETS,C'N'       IS THIS NET SYSTEM                           
         BNE   INIT20                                                           
         MVI   QMED,C'N'                                                        
         B     INIT30                                                           
INIT20   DC    H'0'                                                             
         SPACE                                                                  
INIT30   MVI   IOOPT,C'Y'          USER DOING ALL I/O                           
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    INVAL                                                            
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    INVAL                                                            
         SPACE                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
*                                  VALIDATE KEY                                 
VKEY     LA    R2,CONACTH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   INVAL                                                            
         SPACE                                                                  
         MVI   NEWDISP,C'N'                                                     
         LA    R2,LINSTAH          STATION                                      
         GOTO1 VALISTA                                                          
         CLC   SVSTA,QSTA                                                       
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'                                                     
         MVC   SVSTA,QSTA                                                       
         SPACE                                                                  
VK100    LA    R2,LINBDTH          BATCH                                        
         MVI   DTPLUS,C'N'                                                      
         CLI   5(R2),0             IF NO DATE                                   
         BNE   VK120                                                            
         MVI   NEWDISP,C'Y'        THEN NEW BATCH                               
         XC    SVDTE,SVDTE                                                      
         B     VK200                                                            
         SPACE                                                                  
VK120    ZIC   RF,5(R2)               LOOK FOR FINAL +                          
         LA    RE,FHDRLEN-1(R2,RF)    POINT TO LAST CHAR                        
         CLI   0(RE),C'+'                                                       
         BNE   VK130                                                            
         MVI   0(RE),C' '                                                       
         BCTR  RF,R0               DECREMENT LENGTH                             
         STC   RF,5(R2)                                                         
         MVI   DTPLUS,C'Y'         LOOK FOR THIS DATE OR LATER                  
         SPACE                                                                  
VK130    MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,(0,FHDRLEN(R2)),WORK                                 
         OC    DMCB,DMCB                                                        
         BZ    TRAPERR                                                          
         GOTO1 DATCON,(R1),(0,WORK),(1,DUB)                                     
         CLC   SVDTE,DUB           TEST SAME AS LAST                            
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'                                                     
         MVC   SVDTE,DUB                                                        
         SPACE                                                                  
VK200    LA    R2,LINBSQH          SEQ                                          
         CLI   5(R2),0             ANY INPUT                                    
         BNE   VK210                                                            
         MVI   NEWDISP,C'Y'        NO, NEW BATCH                                
         XC    SVSEQ,SVSEQ                                                      
         XC    SVBSEQ,SVBSEQ                                                    
         B     VK300                                                            
         SPACE                                                                  
VK210    MVI   ERROR,NOTNUM                                                     
         SPACE                                                                  
         MVC   WORK(8),=8C'0'                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VKMVN                                                         
         EX    R1,VKCLC                                                         
         BNE   TRAPERR                                                          
         EX    R1,VKPK                                                          
         OI    DUB+7,X'0F'                                                      
         CVB   R0,DUB                                                           
         UNPK  FULL,DUB                                                         
         SPACE                                                                  
         CLC   SVSEQ,FULL          TEST SAME AS BEFORE                          
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'                                                     
         MVC   SVSEQ,FULL                                                       
         STCM  R0,3,SVBSEQ                                                      
         B     VK300                                                            
         SPACE                                                                  
VKMVN    MVN   WORK(0),8(R2)                                                    
VKCLC    CLC   WORK(0),8(R2)                                                    
VKPK     PACK  DUB,8(0,R2)                                                      
*        GOTO1 SCANNER,DMCB,(R2),(1,WORK+32),C',=,:'                            
*        CLI   4(R1),0             TEST FOR SCAN ERROR                          
*        BE    TRAPERR                                                          
*        CLI   WORK+34,X'80'                                                    
*        BZ    TRAPERR                                                          
*        CLI   WORK+35,X'80'                                                    
*        BZ    TRAPERR                                                          
*        L     RE,WORK+36                                                       
*        C     RE,=F'24'           24:59 MAX                                    
*        BH    TRAPERR                                                          
*        STC   RE,FULL                                                          
*        L     RE,WORK+40                                                       
*        C     RE,=F'59'                                                        
*        BH    TRAPERR                                                          
*        STC   RE,FULL+1                                                        
         SPACE                                                                  
*                                  OPTIONS LINE                                 
VK300    XC    SVCLI,SVCLI         CLEAR OPTS                                   
         XC    SVPRD,SVPRD                                                      
         XC    SVUIDNUM,SVUIDNUM                                                
         LA    R2,LINOPTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK400                                                            
         B     VKXIT                                                            
         SPACE                                                                  
VK400    MVI   BYTE,1                                                           
         LA    R4,SCNWRK                                                        
         XC    SCNWRK,SCNWRK                                                    
         LA    R0,25               NON-STANDARD LENGTH                          
         GOTO1 SCANNER,DMCB,((R0),(R2)),(2,(R4)),0                              
         CLI   4(R1),0                                                          
         BE    MYERR                                                            
         SPACE                                                                  
VK500    CLI   0(R4),0             END                                          
         BE    VKXIT                                                            
*                                  CLIENT FILTER                                
         CLI   12(R4),C'C'                                                      
         BNE   VK502                                                            
         CLC   SVCLI,22(R4)        NEW FILTER                                   
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'        MEANS NEW DISPLAY                            
         MVC   SVCLI,22(R4)                                                     
         LA    RF,SVCLI+L'SVCLI-1  GET LENGTH OF VALUE                          
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R0,SVCLI                                                         
         SR    RF,R0                                                            
         BNM   *+6                                                              
         SR    RF,RF                                                            
         STC   RF,SVCLILEN         LENGTH - 1                                   
         B     VK600                                                            
         SPACE                                                                  
*                                  PRODUCT FILTER                               
VK502    CLI   12(R4),C'P'                                                      
         BNE   VK504                                                            
         CLC   SVPRD,22(R4)        NEW FILTER                                   
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'        MEANS NEW DISPLAY                            
         MVC   SVPRD,22(R4)                                                     
         LA    RF,SVPRD+L'SVPRD-1  GET LENGTH OF VALUE                          
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R0,SVPRD                                                         
         SR    RF,R0                                                            
         BNM   *+6                                                              
         SR    RF,RF                                                            
         STC   RF,SVPRDLEN         LENGTH - 1                                   
         B     VK600                                                            
*                                  USER CODE (DDS TESTING ONLY)                 
VK504    CLI   12(R4),C'U'                                                      
         BNE   VK590                                                            
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   VK590                                                            
         MVC   CONHEAD(15),=C'INVALID USER ID'                                  
         CLI   1(R4),8                                                          
         BH    VKERR                                                            
         MVC   SVUID,22(R4)                                                     
         SPACE                                                                  
         MVI   SVUIDNUM,X'FF'      ALL IDS                                      
         CLC   SVUID(3),=C'ALL'                                                 
         BE    VK600                                                            
         SPACE                                                                  
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),SPACES                                                
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCUID                                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,AIO                     
         MVC   AIO,AIO1                                                         
         CLI   8(R1),0                                                          
         BE    VK520                                                            
         MVC   CONHEAD(15),=C'INVALID USER ID'                                  
         B     MYERR2                                                           
         SPACE                                                                  
VK520    L     R6,AIO2                                                          
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   SVUIDNUM,2(R6)      BINARY USER ID (ORIGIN)                      
         B     VK600                                                            
         SPACE                                                                  
VK590    B     VKERR                                                            
         SPACE                                                                  
*                                  UP ERROR FIELD & NEXT BLOCK                  
VK600    ZIC   RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         LA    R4,47(R4)           NOTE- NON-STANDARD LENGTH                    
         B     VK500                                                            
         SPACE                                                                  
VKXIT    XC    KEY,KEY                                                          
         B     XIT                                                              
         SPACE                                                                  
MVCUID   MVC   KEY+15(0),22(R4)                                                 
         SPACE                                                                  
VKERR    OI    BYTE,X'F0'                                                       
         MVC   CONHEAD(14),=C'INVALID OPTION'                                   
         MVC   CONHEAD+17(5),=C'FIELD'                                          
         MVC   CONHEAD+23(1),BYTE                                               
MYERR    LA    R2,LINOPTH                                                       
MYERR2   MVI   GENSTAT2,USMYOK                                                  
         MVI   GCMODE,C' '         IF ERROR TURN OFF SLAVE & EXIT               
         GOTO1 ERREX2                                                           
         EJECT                                                                  
***********************************************************************         
*   VREC - VALIDATE RECORD                                            *         
***********************************************************************         
         SPACE                                                                  
VREC     DS    0H                                                               
         TM    DIVCCDH+4,X'20'     TEST NEED TO VALIDATE CLIENT                 
         BZ    VR010                                                            
         TM    DIVPCDH+4,X'20'     OR PRODUCT                                   
         BZ    VR010                                                            
         TM    DIVCSTH+4,X'20'     OR RECONVERT                                 
         BZ    VR010                                                            
         SPACE                                                                  
         NI    GENSTAT2,X'FF'-RETEQSEL   SET NO RETURN THIS SELECTION           
         B     XIT                                                              
         SPACE                                                                  
VR010    CLI   DIVCCDH+5,0         CLIENT ENTERED                               
         BNE   VR020                YES                                         
         CLI   DIVPCDH+5,0         PRODUCT ENTERED                              
         BNE   VR020                YES                                         
         SPACE                                                                  
         OI    DIVCCDH+4,X'20'     SET CLIENT VALIDATED                         
         XC    DIVCNMD,DIVCNMD                                                  
         OI    DIVCNMH+6,X'80'                                                  
         OI    DIVPCDH+4,X'20'     SET PRODUCT VALIDATED                        
         XC    DIVPCD,DIVPCD                                                    
         XC    DIVPNMD,DIVPNMD                                                  
         OI    DIVPNMDH+6,X'80'                                                 
         XC    DIVPN2D,DIVPN2D                                                  
         OI    DIVPN2DH+6,X'80'                                                 
         XC    SVIHCVAD,SVIHCVAD                                                
         MVI   SVIHCVPR,0                                                       
         MVI   SVIHCVP2,0                                                       
         MVI   SVIHCVES,0                                                       
         B     VR250                                                            
         SPACE                                                                  
VR020    LA    R2,DIVCCDH                                                       
         CLI   DIVCCDH+5,0         CLIENT NOT ENTERED                           
         BE    VR030                                                            
         SPACE                                                                  
         GOTO1 VALIFAS             SWITCH TO SPOT FILE                          
         SPACE                                                                  
         GOTO1 VALIMED             DUMMY, SETS TO TV ONLY                       
         SPACE                                                                  
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         MVC   SVIHCVAD,BCLT       2 BYTE PACKED OVERRIDE CLT                   
         MVC   DIVCNMD(20),CLTNM                                                
         OI    DIVCNMDH+6,X'80'                                                 
         OI    DIVCCDH+4,X'20'     SET VALIDATED                                
         B     VR100                                                            
         SPACE                                                                  
VR030    DS    0H                  FIND CLIENT FROM AGENCY ADV CODE             
*                                       OR FROM 25 CHAR CODE                    
         OC    SVIHAAID,SVIHAAID                                                
         BNZ   VR060                                                            
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING EZCNMD,R6                                                        
         SPACE                                                                  
         MVC   EZCKTYP,=C'ZC'                                                   
         MVC   EZCKAGY,AGENCY                                                   
         MVC   EZCKNAM,SVIHADVN                                                 
         SPACE                                                                  
         GOTO1 VALIFAM             SWITCH TO MPL SYSTEM                         
         MVC   SYSDIR(3),=C'MPL'                                                
         MVC   SYSFIL(3),=C'MPL'                                                
         SPACE                                                                  
         GOTO1 HIGH                                                             
         OC    EZCKNAM,SPACES                                                   
         OC    KEYSAVE+EZCKNAM-EZCNMD(25),SPACES                                
         CLC   KEY(32),KEYSAVE                                                  
         BE    VR034                                                            
*                                  NOT ON FILE ERROR                            
         GOTO1 VALIFAS             SWITCH TO SPT SYSTEM                         
         MVC   SYSDIR(3),=C'SPT'                                                
         MVC   SYSFIL(3),=C'SPT'                                                
         B     NOCLTFND                                                         
         SPACE                                                                  
VR034    DS    0H                                                               
         GOTO1 GETREC                                                           
         SPACE                                                                  
         GOTO1 VALIFAS             SWITCH TO SPT SYSTEM                         
         MVC   SYSDIR(3),=C'SPT'                                                
         MVC   SYSFIL(3),=C'SPT'                                                
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,EZCELS                                                        
         MVI   DUB,0               CLEAR CONTROL                                
*                                                                               
VR040    DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    VR050                                                            
         CLI   0(R6),X'02'                                                      
         BE    VR044                                                            
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VR040                                                            
*                                                                               
VR044    DS    0H                                                               
         USING EZCIDEL,R6                                                       
         MVI   BYTE,0                                                           
         CLC   =C'ALL',EZCSTA      UNLESS ALL STATION ELEM                      
         BE    VR046                                                            
         CLI   EZCSTA,C' '         OR BLANK                                     
         BNH   VR046                                                            
         CLC   EZCCALL,QSTA        SPECIFIC STATION                             
         BNE   VR040               MUST AGREE WITH WKR FILE KEY                 
         OI    BYTE,X'80'          SET HAVE SPECIFIC STATION                    
*                                                                               
VR046    DS    0H                                                               
         CLI   EZCMED,C' '         TEST HAVE SPECIFIC MEDIA                     
         BNH   VR048                                                            
         CLC   EZCMED,QSTA+4       MUST AGREE WITH WKR FILE KEY                 
         BNE   VR040                                                            
         OI    BYTE,X'40'          YES                                          
*                                                                               
VR048    DS    0H                                                               
         CLC   BYTE,DUB            TEST NEW AS SPECIFIC AS OLD                  
         BL    VR040               NO - SKIP IT                                 
*                                                                               
VR050    DS    0H                                                               
         CLC   EZCCOD,=C'***'      TEST 'UNKNOWN'                               
         BE    NOCLTFND                                                         
         MVC   QCLT,EZCCOD         SET CLIENT CODE                              
         OC    QCLT,SPACES                                                      
         TM    BYTE,X'C0'          BOTH STAT AND MED SPECIFIC                   
         BO    VR054               YES - QUIT LOOKING                           
         MVC   DUB(1),BYTE         SAVE LAST STATUS                             
         B     VR040                                                            
*                                                                               
VR054    DS    0H                                                               
         OC    QCLT,QCLT           TEST FOUND CLIENT                            
         BZ    NOCLTFND                                                         
         GOTO1 CLPACK,DMCB,QCLT,SVIHCVAD                                        
         B     VR064                                                            
         SPACE                                                                  
VR060    DS    0H                                                               
         MVC   QCLT,SVIHAAID                                                    
         GOTO1 CLPACK,DMCB,SVIHAAID,SVIHCVAD                                    
         SPACE                                                                  
VR064    OI    DIVCCDH+4,X'20'     SET CLIENT VALIDATED                         
         SPACE                                                                  
* NOW DOUBLE CHECK IF CLIENT IS ON FILE *                                       
         SPACE                                                                  
VR070    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVIHCVAD                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOSPTCLT                                                         
         MVC   BCLT,SVIHCVAD                                                    
         SPACE                                                                  
VR100    LA    R2,DIVPCDH                                                       
         MVI   BPRD,0                                                           
         MVI   BPRD2,0                                                          
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
         MVI   SVIHCVPR,0                                                       
         MVI   SVIHCVP2,0                                                       
         MVI   SVIHCVES,0                                                       
         SPACE                                                                  
         XC    DIVPNMD,DIVPNMD                                                  
         OI    DIVPNMDH+6,X'80'                                                 
         XC    DIVPN2D,DIVPN2D                                                  
         OI    DIVPN2DH+6,X'80'                                                 
         SPACE                                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR200                NO                                          
         SPACE                                                                  
         CLI   5(R2),3                                                          
         BH    VR130                                                            
         SPACE                                                                  
         GOTO1 VALIFAS             SWITCH TO SPT SYSTEM                         
         SPACE                                                                  
         GOTO1 VALIPRD                                                          
         ZIC   RE,0(R2)                                                         
         LA    RF,0(RE,R2)           PRODUCT NAME                               
         OI    6(RF),X'80'                                                      
         MVC   8(20,RF),WORK+4       TRANSMIT NAME                              
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         MVC   SVIHCVPR,WORK+3                                                  
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
         MVC   ZEROS,=C'000'                                                    
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
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         MVC   SVIHCVPR,WORK+3                                                  
         ZIC   RE,0(R2)                                                         
         LA    RF,0(RE,R2)           NOW AT PROD 1 NAME FIELD                   
         OI    6(RF),X'80'                                                      
         MVC   8(20,RF),WORK+4       TRANSMIT NAME                              
         B     VR166                                                            
VR164    CLI   BYTE,2                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   QPRD2,WORK                                                       
         MVC   BPRD2,WORK+3                                                     
         MVC   SVIHCVP2,WORK+3                                                  
         ZIC   RE,0(R2)                                                         
         LA    RF,0(RE,R2)         NOW AT PROD 1 NAME FIELD                     
         IC    RE,0(RF)                                                         
         AR    RF,RE               NOW AT PROD 2 NAME FIELD                     
         OI    6(RF),X'80'                                                      
         MVC   8(20,RF),WORK+4       TRANSMIT NAME                              
VR166    LTR   R5,R5                                                            
         BZ    VR180                                                            
         LA    R3,1(,R3)                                                        
         BCT   R5,VR140                                                         
         B     VR180                                                            
VRMVN    MVN   ZEROS(0),ELEM+8                                                  
VRCLC    CLC   ELEM+8(0),ZEROS                                                  
VRPACK   PACK  DUB,ELEM+8(0)                                                    
         SPACE                                                                  
VR170    EX    RF,VRPACK                                                        
         CVB   R0,DUB                                                           
         STC   R0,SVIHCVES                                                      
         LTR   R5,R5                                                            
         BNZ   ESTENTER                                                         
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVIHCVAD                                                
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),SVIHCVES                                                
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOESTFND                                                         
         OC    QPRD2,QPRD2                                                      
         BZ    VR180                                                            
         MVC   KEY+4(3),QPRD2                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOESTFND                                                         
         SPACE                                                                  
VR180    XC    FILENAME,FILENAME                                                
         SPACE                                                                  
VR200    OI    4(R2),X'20'         SET VALIDATED                                
         OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
         SPACE                                                                  
VR250    TM    DIVCSTH+4,X'20'     OR RECONVERT                                 
         BO    VR260                                                            
         LA    R2,DIVCSTH                                                       
         CLI   5(R2),0                                                          
         BE    VR256                                                            
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,VR2CLCA          CANCEL                                       
         BE    VR254                                                            
         EX    RF,VR2CLC                                                        
         BNE   CONVRTER                                                         
         CLI   CONVRTST,C'Y'       WAS THIS CONVERTED                           
         BNE   CONRECER                                                         
         MVI   RECONVSW,C'Y'                                                    
         OI    DIVCSTSH+6,X'80'                                                 
         MVC   DIVCSTS,=C'RECONVERT'                                            
         OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
         B     VR256                                                            
         SPACE                                                                  
VR254    CLI   RECONVSW,C'Y'                                                    
         BNE   NOCONVER                                                         
         MVI   RECONVSW,C'N'                                                    
VR256    OI    DIVCSTH+4,X'20'                                                  
         B     VR260                                                            
         SPACE                                                                  
         SPACE                                                                  
VR2CLC   CLC   8(0,R2),=C'RECONVERT '                                           
VR2CLCA  CLC   8(0,R2),=C'CANCEL '                                              
         SPACE                                                                  
VR260    CLI   DIVCCDH+5,0         CLIENT NOT ENTERED                           
         BE    VR270                                                            
         XC    BCLT,BCLT                                                        
         SPACE                                                                  
VR270    BAS   RE,PREZ             FIND RECORD, CALL EZMOD, ETC.                
         SPACE                                                                  
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'* IF CHANGES OK, HIT ENTER TO RESUME *'           
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
***********************************************************************         
*   DREC - DISPLAY RECORD                                             *         
***********************************************************************         
         SPACE                                                                  
DREC     DS    0H                                                               
         BAS   RE,PREZ             FIND RECORD, CALL EZMOD, ETC.                
         SPACE                                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*   PREZ - FIND RECORD, CALL EZMOD                                    *         
***********************************************************************         
         DS    0H                                                               
PREZ     NTR1                                                                   
         GOTO1 CALLOV,DMCB,(X'10',0),ATWA    GET EZMOD                          
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RF,VEZMOD                                                        
         MVI   USEIO,C'Y'                                                       
         SPACE                                                                  
         LA    R4,SVWEZIND                                                      
         USING EZWKRIXD,R4                                                      
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'INDEX',=CL8'FACWRK',SVWEZIND,AIO1,AIO2           
         TM    DMCB+8,X'80'        TEST EOF                                     
         BZ    *+6                                                              
         DC    H'0'                BATCH HAS DISAPPEARED                        
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'READ',=CL8'FACWRK',SVWEZIND,AIO1,AIO2            
         TM    DMCB+8,X'80'        TEST EOF ON FIRST READ                       
         BZ    *+6                                                              
         DC    H'0'                BATCH HAS BEEN CLOBBERED                     
         SPACE                                                                  
         L     R5,AIO2                                                          
         USING WKRECD,R5                                                        
         LA    R5,WKCOMNT                                                       
         MVC   SVWKCMNT,WKCOMNT                                                 
         DROP  R5                                                               
         SPACE                                                                  
         XC    INVCNT,INVCNT                                                    
         ZIC   R5,SELLISTN         RELATIVE LINE NUMBER                         
         SLL   R5,1                                                             
         LA    R5,INVLNOS(R5)                                                   
         MVC   INVSTRT,0(R5)       RELATIVE INVOICE NUMBER TO USE               
         SPACE                                                                  
         LA    R6,EZBLKSTR         SET EZBLOCK                                  
         USING EZBLOCKD,R6                                                      
         LR    RE,R6               CLEAR EZBLOCK                                
         LA    RF,EZBLOCKL                                                      
         XCEF                                                                   
*                                                                               
         LA    RE,DRHOOK           DISPLAY HOOK ROUTINE                         
         CLI   MODE,DISPREC                                                     
         BE    *+8                                                              
         LA    RE,VRHOOK           VALIDATE HOOK ROUTINE                        
         ST    RE,EZHOOK                                                        
         MVC   EZWKRFIL,=CL8'FACWRK'                                            
         MVC   EZWKRIND,SVWEZIND                                                
         L     RE,AIO2                                                          
         ST    RE,EZWKRBUF                                                      
         L     RE,AIO1                                                          
         ST    RE,EZWKRREC                                                      
         LA    RE,2048(RE)                                                      
         ST    RE,EZAREC                                                        
         L     RE,ACOMFACS                                                      
         ST    RE,EZCOMFCS                                                      
         MVI   EZLOOKSW,X'E0'                                                   
         MVI   EZTEST,C'Y'                                                      
         MVI   EZWRITE,C'N'                                                     
         GOTO1 VEZMOD,DMCB,(R6)                                                 
         B     XIT  ONLY RET AT BATCH END-DONE AFTER RETURN FROM EZMOD          
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*   DRHOOK - DISPLAY REC EZMOD HOOK                                   *         
***********************************************************************         
         DS    0H                                                               
DRHOOK   NTR1                                                                   
         LA    R3,EZBLKSTR         SET EZBLOCK                                  
         USING EZBLOCKD,R3                                                      
         XC    LISTAR,LISTAR                                                    
         CLI   EZMODE,EZINVP       PROCESS INVOICE                              
         BNE   DRHX                                                             
         SPACE                                                                  
         TM    EZIHCVQ,EZIHCDEL    THIS A DELETED INVOICE                       
         BO    DRHX                 YES                                         
         SPACE                                                                  
         LH    R1,INVCNT                                                        
         LA    R1,1(R1)                                                         
         STH   R1,INVCNT                                                        
         CLC   INVCNT,INVSTRT      ARE WE AT RIGHT INVOICE                      
         BL    DRH360                                                           
         BH    DRH300                                                           
         SPACE                                                                  
* SAVE FOR VR RTN                                                               
         SPACE                                                                  
         MVC   SVIHADVN,EZIHADVN   25 CHAR ADVERTISER NAME                      
         MVC   SVIHAAID,EZIHAAID    8 CHAR ADVERTISER CODE                      
         MVC   SVIHCVAD,EZIHCVAD    2 BYTE PACKED OVERRIDE CLT                  
         MVC   SVIHCVPR,EZIHCVPR                                                
         MVC   SVIHCVP2,EZIHCVP2                                                
         MVC   SVIHCVES,EZIHCVES                                                
         SPACE                                                                  
         GOTO1 VALIFAS             SWITCH TO SPOT FILE                          
         SPACE                                                                  
         CLC   DIVCNM,EZIHADVN                                                  
         BE    *+14                                                             
         MVC   DIVCNM,EZIHADVN                                                  
         OI    DIVCNMH+6,X'80'                                                  
         CLC   DIVCDE,EZIHAAID                                                  
         BE    *+14                                                             
         MVC   DIVCDE,EZIHAAID                                                  
         OI    DIVCDEH+6,X'80'                                                  
         CLC   DIVPNM,EZIHPRDN                                                  
         BE    *+14                                                             
         MVC   DIVPNM,EZIHPRDN                                                  
         OI    DIVPNMH+6,X'80'                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZIHAPID),EZIHAPID                                        
         SPACE                                                                  
* ONLY SHOW ESTIMATE IF PRESENT *                                               
         SPACE                                                                  
         LA    RE,L'EZIHAPID                                                    
         LA    RF,WORK-1+L'EZIHAPID                                             
         CLI   0(RF),C' '          FIND LAST NON-BLANK CHAR                     
         BH    *+14                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         B     DRH050              NO ROOM FOR EST                              
         SPACE                                                                  
         MVI   1(RF),C'-'                                                       
         MVC   2(L'EZIHEST,RF),EZIHEST AND MOVE IN ESTIMATE                     
         SPACE                                                                  
DRH050   CLC   DIVPPE,WORK                                                      
         BE    *+14                                                             
         MVC   DIVPPE,WORK                                                      
         OI    DIVPPEH+6,X'80'                                                  
         CLC   DIVINO(L'EZIHINV),EZIHINV                                        
         BE    *+14                                                             
         MVC   DIVINO(L'EZIHINV),EZIHINV                                        
         OI    DIVINOH+6,X'80'                                                  
         MVC   DIVMOS(3),EZIHDMOS                                               
         MVC   DIVMOS+3(2),EZIHDMOS+4                                           
         OI    DIVMOSH+6,X'80'                                                  
         MVC   DIVCST,SPACES                                                    
         TM    EZIHCVST,EZIHCVQ    TEST CONVERTED                               
         BNZ   DRH100                                                           
         MVC   DIVCST(2),=C'NO'                                                 
         MVI   CONVRTST,C'N'       SET NOT CONVERTED                            
         B     DRH150                                                           
         SPACE                                                                  
DRH100   MVI   CONVRTST,C'Y'       SET WAS CONVERTED                            
         GOTO1 DATCON,DMCB,(1,EZIHCVDT),(8,DIVCST)                              
         SPACE                                                                  
DRH150   OI    DIVCSTH+4,X'20'     VALIDATED                                    
         OI    DIVCSTH+6,X'80'                                                  
         OC    EZIHCVAD,EZIHCVAD   TEST HAVE CLIENT CODE                        
         BZ    DRH180                                                           
         LA    R2,DIVCCDH                                                       
         SPACE                                                                  
         GOTO1 VALIMED                                                          
         SPACE                                                                  
*        GOTO1 VALICLT                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),EZIHCVAD                                                
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     BIG BAD ERROR                                
         BE    *+6                  NO, OKAY                                    
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         USING CLTHDR,R6                                                        
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
* SAVE CLIENT PRODUCT LIST *                                                    
         SPACE                                                                  
         LA    R4,CLIST                                                         
         LA    R5,880                                                           
         LA    RE,SVCLIST                                                       
         LR    RF,R5                                                            
         MVCL  RE,R4                                                            
         SPACE                                                                  
         MVC   CLTNM,CNAME         AND CLIENT NAME                              
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,(CPROF+6,EZIHCVAD),DIVCCD                            
         DROP  R6                                                               
         MVI   DIVCCDH+5,3         SET LENGTH                                   
         CLI   DIVCCD+2,C' '                                                    
         BH    *+8                                                              
         MVI   DIVCCDH+5,2                                                      
         OI    DIVCCDH+6,X'80'                                                  
         SPACE                                                                  
         OI    DIVCCDH+4,X'20'     SET VALIDATED                                
         MVC   DIVCNMD(20),CLTNM                                                
         OI    DIVCNMDH+6,X'80'    TRANS                                        
         B     DRH200                                                           
         SPACE                                                                  
DRH180   OI    DIVCCDH+4,X'20'     SET CLIENT VALIDATED                         
         OI    DIVCCDH+6,X'80'     TRANS                                        
         XC    DIVCCD,DIVCCD                                                    
         XC    DIVCNMD,DIVCNMD                                                  
         OI    DIVCNMDH+6,X'80'     TRANS                                       
         SPACE                                                                  
DRH200   DS    0H                                                               
         SPACE                                                                  
DRH210   MVI   BPRD,0                                                           
         MVI   BPRD2,0                                                          
         LA    R2,DIVPCDH                                                       
         CLI   EZIHCVPR,0          TEST HAVE PRODUCT                            
         BE    DRH440               NO                                          
         XC    DIVPCD,DIVPCD                                                    
         LA    RE,220                                                           
         LA    RF,SVCLIST                                                       
DRH212   CLC   EZIHCVPR,3(RF)                                                   
         BE    DRH214                                                           
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+6                                                              
         BCT   RE,DRH212                                                        
         DC    H'0'                                                             
DRH214   MVC   DIVPCD(3),0(RF)                                                  
         LA    R4,DIVPCD+3                                                      
         CLI   2(RF),C' '                                                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),EZIHCVAD                                                
         MVC   KEY+4(3),0(RF)                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DR216                                                            
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         USING PRDHDR,R6                                                        
         LA    R5,PNAME                                                         
         DROP  R6                                                               
DR216    XC    FILENAME,FILENAME                                                
         ZIC   RE,0(R2)                                                         
         LA    RF,0(R2,RE)                                                      
         MVC   8(20,RF),0(R5)                                                   
         OI    6(RF),X'80'                                                      
         SPACE                                                                  
         CLI   EZIHCVP2,0          TEST HAVE PRODUCT 2                          
         BE    DRH228                                                           
         LA    RE,220                                                           
         LA    RF,SVCLIST                                                       
DRH220   CLC   EZIHCVP2,3(RF)                                                   
         BE    DRH224                                                           
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+6                                                              
         BCT   RE,DRH220                                                        
         DC    H'0'                                                             
DRH224   MVI   0(R4),C','                                                       
         MVC   1(3,R4),0(RF)                                                    
         LA    R4,3(,R4)                                                        
         CLI   2(RF),C' '                                                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),EZIHCVAD                                                
         MVC   KEY+4(3),0(RF)                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         LA    R5,=CL20'PRODUCT NOT FOUND'                                      
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DRH226                                                           
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         USING PRDHDR,R6                                                        
         LA    R5,PNAME                                                         
         DROP  R6                                                               
DRH226   XC    FILENAME,FILENAME                                                
         ZIC   RE,0(R2)                                                         
         LA    RF,0(R2,RE)                                                      
         ZIC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         MVC   8(20,RF),0(R5)                                                   
         OI    6(RF),X'80'                                                      
         B     DRH230                                                           
         SPACE                                                                  
DRH228   XC    DIVPN2D,DIVPN2D                                                  
         OI    DIVPN2DH+6,X'80'                                                 
         SPACE                                                                  
DRH230   CLI   EZIHCVES,0                                                       
         BE    DRH240                                                           
         ZIC   RE,EZIHCVES                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(R4),C'-'                                                       
         UNPK  1(3,R4),DUB                                                      
         SPACE                                                                  
DRH240   OI    DIVPCDH+6,X'80'                                                  
         OI    DIVPCDH+4,X'20'     SET PRODUCT VALIDATED                        
         SPACE                                                                  
DRH250   DS    0H                                                               
         SPACE                                                                  
         XC    DIVCSTS,DIVCSTS                                                  
         OI    DIVCSTSH+6,X'80'                                                 
         MVI   RECONVSW,0                                                       
         TM    EZIHCVST,EZIHRCVQ   TEST RECONVERT                               
         BZ    DRH300                                                           
         MVC   DIVCSTS,=C'RECONVERT'                                            
         MVI   RECONVSW,C'Y'                                                    
         SPACE                                                                  
DRH300   LR    RD,R0               DON'T GO BACK TO EZMOD                       
         B     DRHX                                                             
         SPACE                                                                  
DRH360   MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
         SPACE                                                                  
DRHX     B     XIT                                                              
         SPACE                                                                  
DRH440   OI    DIVPCDH+4,X'20'     SET PRODUCT VALIDATED                        
         OI    DIVPCDH+6,X'80'     TRANS                                        
         XC    DIVPCD,DIVPCD                                                    
         CLI   EZIHCVPR,0          TEST HAVE PRODUCT                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   EZIHCVP2,0          TEST HAVE PRODUCT                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   EZIHCVES,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DIVPNMDH+6,X'80'     TRANS                                       
         XC    DIVPNMD,DIVPNMD                                                  
         OI    DIVPN2DH+6,X'80'     TRANS                                       
         XC    DIVPN2D,DIVPN2D                                                  
         SPACE                                                                  
         B     DRH250                                                           
         SPACE                                                                  
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*   VRHOOK - VALIDATE REC EZMOD HOOK                                  *         
***********************************************************************         
         DS    0H                                                               
VRHOOK   NTR1                                                                   
         LA    R6,EZBLKSTR         SET EZBLOCK                                  
         USING EZBLOCKD,R6                                                      
         XC    LISTAR,LISTAR                                                    
         CLI   EZMODE,EZINVP       PROCESS INVOICE                              
         BNE   VRHX                                                             
         SPACE                                                                  
         LH    R1,INVCNT                                                        
         LA    R1,1(R1)                                                         
         STH   R1,INVCNT                                                        
         CLC   INVCNT,INVSTRT      ARE WE AT RIGHT INVOICE                      
         BL    VRH360                                                           
         BH    VRH340                                                           
         SPACE                                                                  
         L     R5,EZWKRREC         INVOICE HEADER RECORD                        
         LA    R5,5(R5)            SKIP RECLEN(2),RECCODE(2),DELIM(1)           
         SPACE                                                                  
         OI    EZIHCVST-EZIHCNVS(R5),EZIHCOVR   SET OVERRIDE SWITCH             
         MVC   EZIHCVAD-EZIHCNVS(2,R5),SVIHCVAD                                 
         MVC   EZIHCVPR-EZIHCNVS(1,R5),SVIHCVPR                                 
         MVC   EZIHCVP2-EZIHCNVS(1,R5),SVIHCVP2                                 
         MVC   EZIHCVES-EZIHCNVS(1,R5),SVIHCVES                                 
         MVI   EZIHCVND-EZIHCNVS(R5),X'80' STOP DROP TRAILING BLANKS            
         CLI   RECONVSW,C'Y'       RECONVERT REQUEST                            
         BNE   *+8                                                              
         OI    EZIHCVST-EZIHCNVS(R5),EZIHRCVQ                                   
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(5,0),(1,EZIHCVDT-EZIHCNVS(R5))                      
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'WRITE',EZWKRFIL,EZWKRIND,EZWKRREC,      X        
               EZWKRBUF                                                         
         SPACE                                                                  
         CLI   RECONVSW,C'Y'       RECONVERT REQUEST                            
         BNE   VRH340               NO                                          
         SPACE                                                                  
         NI    SVWKCMNT,X'FF'-X'40' SET OFF ENTIRE BATCH CONVERTED              
         SPACE                                                                  
         L     RF,EZWKRREC         SET COMMENT                                  
         XC    0(96,RF),0(RF)                                                   
         LA    R3,28(,RF)                                                       
         USING WKRECD,R3                                                        
         MVC   WKCOMNT,SVWKCMNT    SAVED WORKER COMMENT AREA                    
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'COMMENT',EZWKRFIL,EZWKRIND,EZWKRREC,    X        
               EZWKRBUF                                                         
         SPACE                                                                  
VRH340   LR    RD,R0               DON'T GO BACK TO EZMOD                       
         B     VRHX                                                             
         SPACE                                                                  
VRH360   MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
         B     VRHX                                                             
         SPACE                                                                  
VRHX     B     XIT                                                              
         DROP  R6                                                               
         SPACE 2                                                                
***********************************************************************         
*   DKEY - DISPLAY KEY                                                *         
***********************************************************************         
         SPACE 1                                                                
DKEY     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*   LIST - LIST RECORDS                                               *         
***********************************************************************         
         SPACE 1                                                                
LIST     DS    0H                                                               
         LA    R2,LINSELH                                                       
         BAS   RE,CLRSCRN                                                       
         OC    SVUIDNUM,SVUIDNUM   HAS ID BEEN SET AS OPTION                    
         BNZ   *+10                                                             
         MVC   SVUIDNUM,TWAORIG    NO, USE SIGN ON                              
         GOTO1 CALLOV,DMCB,(X'10',0),ATWA                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RF,VEZMOD                                                        
         MVI   USEIO,C'Y'                                                       
         CLI   MODE,PRINTREP                                                    
         BNE   LS100                                                            
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         SPACE                                                                  
LS100    DS    0H                                                               
         LA    R4,SVWEZIND                                                      
         USING EZWKRIXD,R4                                                      
         XC    SVWEZIND,SVWEZIND                                                
         SPACE                                                                  
* LOOP THRU ALL INVOICES *                                                      
         SPACE                                                                  
LS120    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',=CL8'FACWRK',SVWEZIND,AIO1,AIO2           
         TM    DMCB+8,X'80'        TEST EOF                                     
         BO    NOBATER                                                          
         SPACE                                                                  
         CLI   SVUIDNUM,X'FF'      TEST 'ALL' IDS                               
         BE    *+14                                                             
         CLC   EZWIUID,SVUIDNUM    ELSE, TEST RIGHT ID                          
         BNE   LS120                                                            
         CLI   EZWIDAY,X'99'       MUST BE DAY 99                               
         BNE   LS120                                                            
         SPACE                                                                  
         MVC   WORK(4),EZWISTN     STATION                                      
         CLI   WORK+3,C' '                                                      
         BH    *+8                                                              
         MVI   WORK+3,C' '                                                      
         MVC   WORK+4(1),EZWIMED                                                
         OC    SVSTA,SVSTA         STATION FILTER                               
         BZ    *+14                                                             
         CLC   WORK(5),SVSTA                                                    
         BNE   LS120                                                            
         SPACE                                                                  
         CLI   SPOTNETS,C'S'       THIS SPOT                                    
         BE    LS114                                                            
         CLI   SPOTNETS,C'N'       THIS NET                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   EZWIMED,C'N'                                                     
         BE    LS116                                                            
         CLI   EZWIMED,C'C'                                                     
         BE    LS116                                                            
         CLI   EZWIMED,C'S'                                                     
         BE    LS116                                                            
         B     LS100                                                            
         SPACE                                                                  
LS114    CLI   EZWIMED,C'T'        CK OUT VARIOUS SPOT MEDIAS                   
         BE    LS116                                                            
         CLI   EZWIMED,C'A'                                                     
         BE    LS116                                                            
         CLI   EZWIMED,C'F'                                                     
         BE    LS116                                                            
         CLI   EZWIMED,C'X'                                                     
         BNE   LS100                                                            
         SPACE                                                                  
LS116    GOTO1 DATAMGR,DMCB,=C'READ',=CL8'FACWRK',SVWEZIND,AIO1,AIO2            
         TM    DMCB+8,X'80'        TEST EOF ON FIRST READ                       
         BNZ   LS120               YES, SKIP                                    
         SPACE                                                                  
         L     R3,AIO2                                                          
         USING WKRECD,R3                                                        
         SPACE                                                                  
         OC    SVDTE,SVDTE         DATE FILTER                                  
         BZ    LS140                                                            
         CLC   WKDATEC,SVDTE                                                    
         BL    LS120               LOW, SKIP                                    
         BE    LS140               EQUAL, OK                                    
         CLI   DTPLUS,C'Y'         ELSE, CHECK NEED EXACT DATE                  
         BNE   LS120                                                            
         SPACE                                                                  
LS140    DS    0H                                                               
         OC    SVSEQ,SVSEQ         SEQ FILTER                                   
         BZ    *+14                                                             
         CLC   WKFILNO,SVBSEQ                                                   
         BNE   LS120                                                            
*                                  DISPLAY FOUND DATE AND BATCH SEQ             
         GOTO1 DATCON,DMCB,(1,WKDATEC),(5,WORK)                                 
         MVC   LINBDT(8),WORK                                                   
         MVI   LINBDT+8,C' '       GET RID OF +                                 
         OI    LINBDTH+6,X'80'                                                  
         SR    R0,R0                                                            
         ICM   R0,3,WKFILNO                                                     
         EDIT  (R0),(4,LINBSQ),FILL=0                                           
*        ZIC   R0,WKTIMEC+1                                                     
*        EDIT  (R0),(2,LINBSQ+3),FILL=0                                         
*        MVI   LINBTM+2,C':'       COLON                                        
         OI    LINBSQH+6,X'80'                                                  
         MVC   SVDTE,WKDATEC       SAVE DATE AND BATCH SEQ                      
         MVC   SVBSEQ,WKFILNO                                                   
         SPACE                                                                  
         CLI   NEWDISP,C'Y'        IF NEW DISPLAY                               
         BNE   *+10                                                             
         MVC   INVSTRT,=F'1'       START AT FIRST INVOICE                       
         XC    INVCNT,INVCNT                                                    
         XC    INVLNOS,INVLNOS     CLEAR INVOICE NUMBER LIST                    
         LA    R5,INVLNOS          R5 TO START OF LIST                          
         ST    R5,INVLPTR                                                       
         SPACE                                                                  
         LA    R6,EZBLKSTR         SET EZBLOCK                                  
         USING EZBLOCKD,R6                                                      
         LR    RE,R6               CLEAR EZBLOCK                                
         LA    RF,EZBLOCKL                                                      
         XCEF                                                                   
         LA    RE,DISLIN                                                        
         ST    RE,EZHOOK                                                        
         MVC   EZWKRFIL,=CL8'FACWRK'                                            
         MVC   EZWKRIND,SVWEZIND                                                
         L     RE,AIO2                                                          
         ST    RE,EZWKRBUF                                                      
         L     RE,AIO1                                                          
         ST    RE,EZWKRREC                                                      
         LA    RE,2048(RE)                                                      
         ST    RE,EZAREC                                                        
         L     RE,ACOMFACS                                                      
         ST    RE,EZCOMFCS                                                      
         MVI   EZLOOKSW,X'E0'                                                   
         MVI   EZTEST,C'Y'                                                      
         MVI   EZWRITE,C'N'                                                     
         GOTO1 VEZMOD,DMCB,(R6)                                                 
*                                  ONLY HERE AT BATCH END                       
         XC    INVSTRT,INVSTRT     SET TO RESTART                               
         B     XIT                 DONE AFTER RETURN FROM EZMOD                 
         DROP  R3,R6                                                            
         EJECT                                                                  
*        DISLIN - DISPLAY LINE (EZHOOK)                                         
         DS    0H                                                               
DISLIN   NTR1                                                                   
         LA    R6,EZBLKSTR         SET EZBLOCK                                  
         USING EZBLOCKD,R6                                                      
         XC    LISTAR,LISTAR                                                    
         CLI   EZMODE,EZINVP       PROCESS INVOICE                              
         BNE   DLX                                                              
         SPACE                                                                  
         TM    EZIHCVST,EZIHCDEL   THIS A DELETED INVOICE                       
         BO    DLX                  YES                                         
         SPACE                                                                  
         LH    R1,INVCNT                                                        
         LA    R1,1(R1)                                                         
         STH   R1,INVCNT                                                        
         CLC   INVCNT,INVSTRT      ARE WE AT STARTING POINT                     
         BNH   DL360                                                            
         SPACE                                                                  
         STH   R1,INVSTRT          SET NEW START                                
         SPACE                                                                  
         CLI   SVCLI,C' '          TEST HAVE CLIENT FILTER                      
         BNH   DL220                                                            
         ZIC   RF,SVCLILEN         LENGTH - 1                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHADVN(0),SVCLI                                                
         BNE   DL360                                                            
         SPACE                                                                  
DL220    CLI   SVPRD,C' '          TEST HAVE PROD FILTER                        
         BNH   DL240                                                            
         ZIC   RF,SVPRDLEN         TEST PRODUCT FILTER                          
         EX    RF,DLCLC                                                         
         BNE   DL360                                                            
         SPACE                                                                  
DL240    L     R5,INVLPTR          POINTER INTO SAVE LIST                       
         MVC   0(2,R5),INVCNT      SAVE INVOICE NUMBER IN LIST                  
         LA    R5,2(R5)                                                         
         ST    R5,INVLPTR                                                       
         SPACE                                                                  
         MVC   LCLI,EZIHADVN                                                    
         MVC   LPRD,EZIHPRDN                                                    
         MVC   LINV,EZIHINV                                                     
         MVC   LMTH(3),EZIHDMOS                                                 
         MVC   LMTH+3(2),EZIHDMOS+4                                             
         TM    EZIHCVST,EZIHCVQ    TEST CONVERTED                               
         BNZ   *+8                                                              
         MVI   LCVSTAT,C'*'        NO                                           
         SPACE                                                                  
         TM    EZIHCVST,EZIHRCVQ   TEST RECONVERT                               
         BZ    *+8                                                              
         MVI   LCVSTAT+1,C'R'      YES                                          
         SPACE                                                                  
         CLI   MODE,PRINTREP                                                    
         BE    DL300                                                            
         MVC   DMDSKADD(4),=X'0000FFFF'   DUMMY DISK ADDRESS                    
         MVI   NLISTS,16                                                        
         GOTO1 LISTMON                                                          
         B     DL360                                                            
         SPACE                                                                  
DL300    MVC   P+2(70),LISTAR                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
DL360    MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
         SPACE                                                                  
DLX      B     XIT                                                              
DLCLC    CLC   EZIHPRDN(0),SVPRD                                                
         DROP  R6                                                               
         EJECT                                                                  
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
         SPACE                                                                  
         DS    0H                                                               
CLRSCRN  NTR1                                                                   
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
         B     XIT                                                              
         SPACE                                                                  
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         DS    0H                                                               
HDRTN    NTR1                                                                   
         B     XIT                                                              
         EJECT                                                                  
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
CONVRTER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CONVRTMS),CONVRTMS                                     
         B     ERREXIT                                                          
CONRECER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CONRECMS),CONRECMS                                     
         B     ERREXIT                                                          
NOCONVER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCONVMS),NOCONVMS                                     
         B     ERREXIT                                                          
NOCLTFND XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCLTMS),NOCLTMS                                       
         B     ERREXIT                                                          
NOSPTCLT XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOSPCLMS),NOSPCLMS                                     
         MVC   CONHEAD+22(3),QCLT                                               
         B     ERREXIT                                                          
MISSCLT  XC    CONHEAD,CONHEAD                                                  
         LA    R2,DIVCCDH                                                       
         MVC   CONHEAD(L'MISSCMS),MISSCMS                                       
         SPACE                                                                  
ERREXIT  GOTO1 ERREX2                                                           
         EJECT                                                                  
NOBATER  LA    R2,LINSTAH                                                       
         MVI   ERROR,NOBATCH       NO BATCH FOUND                               
         B     TRAPERR                                                          
INVAL    MVI   ERROR,INVACT                                                     
TRAPERR  GOTO1 ERREX                                                            
         DC    H'0'                                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 2                                                                
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,52,C'INVOICE LIST'                                            
         SSPEC H2,50,C'--------------'                                          
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'CLIENT'                                                   
         SSPEC H8,29,C'PRODUCT NAME'                                            
         SSPEC H8,55,C'INVOICE'                                                 
         SSPEC H8,66,C'MONTH'                                                   
         DC    X'00'                                                            
MISSCMS  DC    C'* ERROR * CLIENT MUST BE ENTERED FOR PRODUCT *'                
MISPRDMS DC    C'* ERROR * STATION REQUIRES PRODUCT *'                          
NOESTMS  DC    C'* ERROR * NO ESTIMATE FOR THIS PROD(S) *'                      
ESTENTMS DC    C'* ERROR * ESTIMATE MUST BE ENTERED LAST *'                     
MORPRDMS DC    C'* ERROR * ONLY PRD AND PTR ALLOWED *'                          
PRDSIZMS DC    C'* ERROR * PRD, PTR, EST MAX SIZE = 3 *'                        
PRDENTMS DC    C'* ERROR * ENTER AS PRD OR PRD,EST OR PRD,PTR,EST *'            
CONVRTMS DC    C'* ERROR * ENTER ''RECONVERT''/''CANCEL'', OR BLANKS *'         
NOCONVMS DC    C'* ERROR * NO CANCEL UNLESS RECONVERT REQUEST *'                
CONRECMS DC    C'* ERROR * INVOICE NOT CONVERTED YET *'                         
NOCLTMS  DC    C'* ERROR * NO RECORD FOR 25 CHAR ADVERTISER CODE *'             
NOSPCLMS DC    C'* ERROR * CLIENT CODE XXX NOT ON SPOTPAK *'                    
NUMLINS  EQU   16                                                               
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
* EZBLOCK                                                                       
* CTGENFILE                                                                     
       ++INCLUDE SPGENEZ                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE EZBLOCK                                                        
       ++INCLUDE SPEZFFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
* SPEZFF9D                                                                      
       ++INCLUDE SPEZFF9D                                                       
         SPACE 2                                                                
         ORG   CONTAGH                                                          
* SPEZFE9D                                                                      
       ++INCLUDE SPEZFE9D                                                       
         PRINT OFF                                                              
         EJECT                                                                  
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* DMWRKRD                                                                       
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
* SPGENPRD                                                                      
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
* SPGENCLT                                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
         PRINT ON                                                               
         ORG   SYSSPARE                                                         
SVLNNUM  DS    F                                                                
AEZMOD   DS    A                                                                
SVR2     DS    A                                                                
SVWKCMNT DS    CL16                SAVED WORKER COMMENT AREA                    
SVIHADVN DS    CL25                25 CHAR ADVERTISER NAME                      
SVIHAAID DS    CL8                 8 CHAR AGENCY ADVERTISER CODE                
SVIHCVAD DS    XL2                 2 BYTE PACKED OVERRIDE CLT                   
SVIHCVPR DS    XL1                 BPRD                                         
SVIHCVP2 DS    XL1                 BPRD2                                        
SVIHCVES DS    XL1                 ESTIMATE                                     
SVAIDCLT DS    CL3                                                              
SVADVBCL DS    XL2                                                              
ZEROS    DS    CL3                                                              
RECONVSW DS    CL1                                                              
CONVRTST DS    CL1                                                              
SVSTA    DS    CL5                                                              
SVDTE    DS    CL3                                                              
SVSEQ    DS    CL4                                                              
SVBSEQ   DS    XL2                                                              
SVUID    DS    CL8                                                              
SVWEZIND DS    CL16                                                             
SVUIDNUM DS    XL2                                                              
SVCLI    DS    CL25                                                             
SVPRD    DS    CL25                                                             
SVCLILEN DS    X                                                                
SVPRDLEN DS    X                                                                
DTPLUS   DS    CL1                                                              
NEWDISP  DS    CL1                                                              
INVSTRT  DS    H                                                                
INVCNT   DS    H                                                                
INVLPTR  DS    A                                                                
INVLNOS  DS    XL(NUMLINS*2)      LIST OF DISPLAYED INVOICE NUMBERS             
SCNWRK   DS    XL200                                                            
MPLKEY   DS    CL64                                                             
MPLKEYSV DS    CL64                                                             
EZBLKSTR DS    0D                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLI     DS    CL25                                                             
         DS    CL1                                                              
LPRD     DS    CL25                                                             
         DS    CL1                                                              
LINV     DS    CL10                                                             
         DS    CL1                                                              
LMTH     DS    CL5                                                              
LCVSTAT  DS    CL1                                                              
         DS    CL1                                                              
         DS    CL4                 SPARE                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043SPEZF06   05/01/02'                                      
         END                                                                    
