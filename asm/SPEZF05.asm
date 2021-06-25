*          DATA SET SPEZF05    AT LEVEL 053 AS OF 02/28/18                      
*PHASE T23005A                                                                  
         TITLE 'T23005 - BATCH LIST'                                            
***********************************************************************         
*                                                                     *         
*  TITLE: T23005 - EASI BATCH LIST/MAINT                              *         
*  COMMENTS: THIS PROGRAM LISTS BATCHES OF INVOICES.                  *         
*  OUTPUTS: LIST REPORT                                               *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO PROGRAM/UNIT TABLES             *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - EZBLOCK                                                         
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
*                                                                               
***********************************************************************         
*                                                                     *         
*  LEV 23    DEC14/89 FIX CONVERTED STATUS BUG                        *         
*  LEV 24    JAN25/90 SHOW SOURCE CODE FROM WORKER COMMENT FIELD      *         
*  LEV 25    APR24/90 SHOW SOURCE CODE OM PRINTED REPORT              *         
*  LEV 26    APR30/90 SHOW AGY NAME/ADDR TOGETHER                     *         
*  LEV 27    JUN25/91 SHOW AS DELETED NO CONVERT DATE, BUT BATCH COMPL*         
*  LEV 28    JUN28/91 CHANGE HEADINGS, ADD CTS, ALLOW DATE RANGE      *         
*  LEV 29    SEP11/91 ADD NET                                         *         
*  LEV 30    NOV18/91 ADD STATION EQUIVALENCY                         *         
*  LEV 31    AUG31/92 ALLOW CANADIAN AGENCIES TO SEE ALL MEDIAS       *         
*  LEV 32    FEB10/93 STOP ACCEPTING ALL FOR STATION REQUEST          *         
*  LEV 33    APR19/93 USE 00 PHASE INITIATED EASIWK                   *         
*  LEV 34    APR07/94 ADD MEDIA X TO MEDTBL                           *         
*  LEV 35    JUL18/94 CONVERT TO WRKF FILES                           *         
*  LEV 36    JUL17/96 FIX CONVERTED DATED, BYPASS U=ALL               *         
*  LEV 37    AUG12/96 FIX 3 CHAR STATION, ERR MSG FOR U=ALL           *         
*  LEV 38    FEB08/99 SHOW ALL 5 DIGITS OF BATCH SEQ                  *         
*  LEV 39 BG OCT29/01                                                 *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T23005   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3005**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         MVC   AIO,AIO1                                                         
         MVI   QMED,C'T'                                                        
*                                                                               
         L     R7,AIO2                                                          
         USING EZBLOCKD,R7                                                      
*                                                                               
         BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE ON SCREEN                
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    INVAL                                                            
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    INVAL                                                            
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    INVAL                                                            
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                  VALIDATE KEY                                 
VKEY     DS    0H                                                               
         MVI   OPTEXP,C'N'                                                      
         MVI   OPTMED,C' '                                                      
         LA    R2,CONACTH                                                       
         CLI   ACTNUM,ACTCHA                                                    
         BE    BADACTER                                                         
         CLI   ACTNUM,ACTADD                                                    
         BE    BADACTER                                                         
         CLI   ACTNUM,ACTDEL                                                    
         BE    BADACTER                                                         
         CLI   ACTNUM,ACTREST                                                   
         BE    BADACTER                                                         
         CLI   ACTNUM,ACTDIS                                                    
         BE    BADACTER                                                         
*&&DO                                                                           
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+22                                                             
         CLI   TWAOFFC,C'*'        DDS TERMINAL                                 
         BE    *+14                                                             
         XC    LBALT2+LUID-LISTAR(7),LBALT2+LUID-LISTAR  CLEAR USER ID          
         OI    LBALT2H+6,X'80'                                                  
*&&                                                                             
*                                                                               
         XC    ALLCTRS,ALLCTRS                                                  
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
         MVI   CURSYST,C'M'        SWITCH TO MEDIA (SPOT/NET)                   
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         LA    R2,DBASTAH          STATION                                      
         XC    SVSTA,SVSTA                                                      
         CLI   5(R2),0                                                          
         BE    VK100                                                            
*                                                                               
         GOTO1 VALISTA                                                          
         MVC   SVSTA,QSTA                                                       
*                                                                               
VK100    LA    R2,DBADTEH          DATE                                         
         XC    SVDTES,SVDTES                                                    
         CLI   5(R2),0                                                          
         BE    VK200                                                            
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         ICM   R3,15,DMCB                                                       
         BZ    BADATE                                                           
         GOTO1 DATCON,(R1),(0,WORK),(2,SVSDTE)                                  
*                                                                               
         CLM   R3,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VK160                YES                                         
         LA    R3,1+8(R2,R3)                                                    
*                                                                               
         GOTO1 DATVAL,DMCB,(0,(R3)),WORK                                        
         OC    DMCB,DMCB                                                        
         BZ    BADATE                                                           
         GOTO1 DATCON,(R1),(0,WORK),(2,SVEDTE)                                  
         B     VK200                                                            
*                                                                               
VK160    MVC   SVEDTE,SVSDTE                                                    
*                                                                               
VK200    LA    R2,DBASEQH          SEQ                                          
         XC    SVSEQ,SVSEQ                                                      
         XC    SVBSEQ,SVBSEQ                                                    
         CLI   5(R2),0                                                          
         BE    VK300                                                            
*                                                                               
         MVI   ERROR,NOTNUM                                                     
         MVC   WORK(8),=8C'0'                                                   
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VKMVN                                                         
         EX    R1,VKCLC                                                         
         BNE   TRAPERR                                                          
         EX    R1,VKPK                                                          
         OI    DUB+7,X'0F'                                                      
         CVB   R0,DUB                                                           
         UNPK  SVSEQ,DUB                                                        
*                                                                               
         STCM  R0,15,SVBSEQ                                                     
         B     VK300                                                            
*                                                                               
VKMVN    MVN   WORK(0),8(R2)                                                    
VKCLC    CLC   WORK(0),8(R2)                                                    
VKPK     PACK  DUB,8(0,R2)                                                      
*                                                                               
VK300    XC    SVUIDNUM,SVUIDNUM                                                
         LA    R2,DBAOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VKXIT                                                            
         MVI   BYTE,1                                                           
         LA    R4,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   CONHEAD(15),=CL15'INVALID OPTION'                                
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    MYERR                                                            
*                                                                               
VK500    CLI   0(R4),0                                                          
         BE    VK700                                                            
*                                  USER CODE                                    
         CLI   12(R4),C'U'                                                      
         BNE   VK530                                                            
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   VK590                                                            
         MVC   CONHEAD(15),=CL15'INVALID USER ID'                               
         CLI   1(R4),8                                                          
         BH    VKERR                                                            
*        MVC   SVUID,22(R4)                                                     
*                                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),SPACES                                                
         LLC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCUID                                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,AIO                     
         MVC   AIO,AIO1                                                         
         CLI   8(R1),0                                                          
         BE    VK520                                                            
         MVC   CONHEAD(15),=CL15'INVALID USER ID'                               
         B     MYERR2                                                           
*                                                                               
VK520    L     R6,AIO2                                                          
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVUIDNUM,2(R6)      BINARY USER ID (ORIGIN)                      
         B     VK600                                                            
*                                                                               
VK530    DS    0H                                                               
         CLC   =CL7'EXPIRED',12(R4)                                             
         BNE   VK540                                                            
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   VK590                                                            
         MVI   OPTEXP,C'Y'                                                      
         B     VK600                                                            
*                                                                               
VK540    DS    0H                                                               
         CLC   =CL5'MEDIA',12(R4)                                               
         BNE   VK590                                                            
*                                                                               
         MVC   CONHEAD(15),=CL15'INVALID MEDIA'                                 
         LA    R1,22(R4)                                                        
         ICM   R1,8,=AL1(EZMTMEDQ)                                              
         GOTO1 VGETMED                                                          
         BNE   VKERR                                                            
         MVC   OPTMED,22(R4)                                                    
                                                                                
         B     VK600                                                            
*                                                                               
VK590    B     VKERR                                                            
*                                  UP ERROR FIELD & NEXT BLOCK                  
VK600    LLC   RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         LA    R4,32(R4)                                                        
         B     VK500                                                            
*                                                                               
VK700    MVC   CONHEAD,SPACES                                                   
*                                                                               
VKXIT    B     EXIT                                                             
MVCUID   MVC   KEY+15(0),22(R4)                                                 
*                                                                               
VKERR    OI    BYTE,X'F0'                                                       
         MVC   CONHEAD+17(5),=C'FIELD'                                          
         MVC   CONHEAD+23(1),BYTE                                               
MYERR    LA    R2,DBAOPTH                                                       
MYERR2   MVI   GENSTAT2,USMYOK                                                  
         MVI   GCMODE,C' '         IF ERROR TURN OFF SLAVE & EXIT               
         GOTO1 ERREX2                                                           
*                                                                               
*                                                                               
*                                                                               
DKEY     MVI   KEY,0               END IF NOTHING TO DISPLAY                    
         LA    RE,LISTDIR          ADDRESS DIRECTORY                            
         LA    RF,LISTKEYS                                                      
         LA    R6,32                                                            
         SR    R0,R0                                                            
         ICM   R0,1,LISTNUM                                                     
         BZ    EXIT                                                             
*                                                                               
DK100    CLI   0(RE),C' '          LOOK FOR FIRST/NEXT SELECTION                
         BH    DK200                                                            
         LA    RE,6(RE)                                                         
         AR    RF,R6                                                            
         BCT   R0,DK100                                                         
         B     EXIT                                                             
*                                                                               
DK200    BCTR  R6,0                                                             
         EX    R6,MVCLKEY                                                       
*                                                                               
         LA    R2,DBASTAH                                                       
         LA    RE,KEY                                                           
         USING DKEYD,RE                                                         
         LLC   RF,DRDLIN                                                        
         MH    RF,=Y(SVEZLEN)                                                   
         LA    R3,SVEZLIS(RF)                                                   
*                                                                               
         MVC   8(7,R2),SVLSTA-SVEZLIS(R3)       STATION                         
         OI    6(R2),X'80'                                                      
         LA    R2,DBADTEH                                                       
         MVC   8(8,R2),SVLDATE-SVEZLIS(R3)       DATE                           
         OI    6(R2),X'80'                                                      
         LA    R2,DBASEQH          BATCH SEQ                                    
         MVC   8(6,R2),SVLSEQ-SVEZLIS(R3)                                       
         OI    6(R2),X'80'                                                      
*                                                                               
DKXIT    B     EXIT                                                             
MVCLKEY  MVC   KEY(0),0(RF)                                                     
         DROP  RE                                                               
*                                                                               
* DISPLAY RECORD SELECTED FROM LIST                                             
*                                                                               
DREC     DS    0H                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL                                 
         BNE   DR10                                                             
* DDS-ONLY                                                                      
* IM ROUTING INFO IS SET TO NORMAL INTENSITY (HIGH FOR THE TITLE)               
         NI    DBAIRIH+1,X'FF'-X'04'                                            
         OI    DBAIRIH+6,X'80'                                                  
         NI    DBAIANH+1,X'FF'-X'0C'                                            
         OI    DBAIANH+6,X'80'                                                  
         NI    DBAIADH+1,X'FF'-X'0C'                                            
         OI    DBAIADH+6,X'80'                                                  
*                                                                               
DR10     DS    0H                                                               
         LA    R4,SVEZKEY                                                       
         MVC   SVEZKEY(24),KEY+DWKKEY-DKEYD                                     
         USING EZWKRIXD,R4                                                      
*                                                                               
         LAY   R5,EZWRKIOB                                                      
         USING WRKIOB,R5                                                        
         CLI   OPTEXP,C'Y'                                                      
         BE    *+8                                                              
         OI    WRKINDS,WRKINOXQ                                                 
         MVI   WRKIACTN,WRKIANDX                                                
         MVC   WRKEZSQN,SVEZKEY+UKFILENO-UKRECD                                 
         OI    WRKINDS,WRKISEQQ                                                 
         MVC   WRKEZSQN,SVEZKEY+UKFILENO-UKRECD                                 
*                                                                               
         GOTO1 EZWRKIO,(R5)                                                     
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    DRXIT                                                            
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        GOTO1 DATAMGR,DMCB,=C'INDEX',EASIWK,SVEZKEY,AIO1,WRKFBUFR              
*        TM    DMCB+8,X'80'        TEST EOF                                     
*        BNZ   DRXIT                                                            
*                                                                               
         LLC   RF,KEY+DRDLIN-DKEYD                                              
         MH    RF,=Y(SVEZLEN)                                                   
         LA    R1,SVEZLIS                                                       
         AR    R1,RF                                                            
         LA    R2,DBAST2H                                                       
         CLI   0(R1),0                                                          
         BE    DRXIT                                                            
         MVC   8(7,R2),SVLSTA-SVEZLIS(R1)       STATION                         
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,DBADT2H                                                       
         MVC   8(8,R2),SVLDATE-SVEZLIS(R1)       DATE                           
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,DBASQ2H                                                       
         MVC   8(6,R2),SVLSEQ-SVEZLIS(R1)  BATCH SEQ                            
         OI    6(R2),X'80'                                                      
*                                                                               
*        LA    R2,DBADUSH                                                       
*        MVC   8(4,R2),SVLUID-SVEZLIS(R1)  USER ID                              
*        OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,DBACAOH                                                       
         MVC   8(13,R2),SVLSTAT-SVEZLIS(R1)  STATUS-DATE                        
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 AEZMOD,DMCB,(R7)                                                 
*                                                                               
DRXIT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R5                                                            
*                                                                               
*                                                                               
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LIST     MVI   USEIO,C'Y'                                                       
         MVI   LKEY+1,32                                                        
         OC    SVUIDNUM,SVUIDNUM   HAS ID BEEN SET AS OTPION                    
         BNZ   *+10                                                             
         MVC   SVUIDNUM,TWAORIG    NO, USE SIGN ON                              
*                                                                               
         LA    RE,SVEZLIS          CLEAR LIST OF SAVED ENTRIES                  
         STCM  RE,15,ASVEZLIS                                                   
         LHI   RF,SVEZLEN*SVEZLNQ                                               
         XCEFL                                                                  
*                                                                               
         LA    R4,SVEZKEY                                                       
         USING EZWKRIXD,R4                                                      
*                                                                               
         LAY   R1,EZWRKIOB                                                      
         USING WRKIOB,R1                                                        
*                                                                               
         OC    SVEZKEY+UKFILENO-UKRECD(4),SVEZKEY+UKFILENO-UKRECD               
         BZ    *+14                                                             
         MVC   WRKEZSQN,SVEZKEY+UKFILENO-UKRECD                                 
         OI    WRKINDS,WRKISEQQ                                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LS100                                                            
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
LS100    DS    0H                                                               
*        OI    UKFLAG-UKRECD+SVEZKEY,UKFLNOX                                    
*        GOTO1 DATAMGR,DMCB,=C'INDEX',EASIWK,SVEZKEY,AIO1,WRKFBUFR              
*                                                                               
*        TM    DMCB+8,X'80'        TEST EOF                                     
*        BZ    LS120                                                            
*                                                                               
         LAY   R3,EZWRKIOB                                                      
         USING WRKIOB,R3                                                        
*                                                                               
         CLI   OPTEXP,C'Y'                                                      
         BE    *+8                                                              
         OI    WRKINDS,WRKINOXQ                                                 
         MVI   WRKIACTN,WRKIANDX                                                
*                                                                               
         GOTO1 EZWRKIO,(R3)                                                     
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    LS115                                                            
         CLI   WRKIERRS,0                                                       
         BE    LS120                                                            
         DC    H'0'                                                             
         DROP  R3                  WRKIOB,3                                     
*                                                                               
LS115    LAY   RF,WRKEZKEY                                                      
         XC    0(L'WRKEZKEY,RF),0(RF)                                           
         XC    SVEZKEY,SVEZKEY                                                  
         CLI   MODE,PRINTREP                                                    
         BNE   EXIT                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  (B4,BATCTR),(9,P+1),COMMAS=YES                                   
         MVC   P+11(13),=C'TOTAL BATCHES'                                       
         EDIT  (B4,PRTCTR),(9,P+30),COMMAS=YES                                  
         MVC   P+40(15),=C'PRINTED BATCHES'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
LS120    DS    0H                                                               
         BRAS  RE,FAKEIND                                                       
*                                                                               
         CLI   EZWIDAY,X'99'                                                    
         BNE   LS100                                                            
*                                                                               
         CLC   EZWIUID,SVUIDNUM    TEST RIGHT ID                                
         BNE   LS100                                                            
*                                                                               
*        MVC   SVEZKEY,EZWIKEY                                                  
         MVC   SRCESTA(4),EZWISTN      FORMAT STATION                           
         CLI   SRCESTA+3,C' '                                                   
         BH    *+8                                                              
         MVI   SRCESTA+3,C' '                                                   
         MVC   SRCESTA+4(1),EZWIMED                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 VEQVSTA             FIND EQUIV STATION IN EQUISTA                
*                                                                               
         GOTO1 VFILTSYS                                                         
         BNE   LS100                                                            
*                                                                               
         CLI   OPTMED,C' '                                                      
         BNH   *+14                                                             
         CLC   OPTMED,EQVMED                                                    
         BNE   LS100                                                            
*                                                                               
         L     RF,BATCTR                                                        
         LA    RF,1(,RF)                                                        
         ST    RF,BATCTR                                                        
*                                                                               
* IF FILTERS MATCH ON FILTER                                                    
*                                                                               
         OC    SVSTA,SVSTA         STATION                                      
         BZ    LS160                                                            
         MVC   WORK(5),EQUISTA                                                  
         CLI   WORK+3,C'.'                                                      
         BNE   *+8                                                              
         MVI   WORK+3,C' '                                                      
*                                                                               
         CLC   SVSTA,WORK                                                       
         BNE   LS100                                                            
*                                                                               
         USING UKRECD,R4                                                        
LS160    OC    SVDTES,SVDTES       DATE FILTER                                  
         BZ    LS240                                                            
         CLC   UKAGELD,SVSDTE                                                   
         BL    LS100                                                            
         CLC   UKAGELD,SVEDTE                                                   
         BH    LS100                                                            
*                                                                               
LS240    OC    SVSEQ,SVSEQ         BATCH SEQ                                    
         BZ    *+14                                                             
         CLC   SVBSEQ,UKFILENO                                                  
         BH    LS100                                                            
         DROP  R4                                                               
*                                                                               
* READ FIRST RECORD                                                             
*                                                                               
LS200    DS    0H                                                               
*        GOTO1 DATAMGR,DMCB,=C'READ',EASIWK,SVEZKEY,AIO1,WRKFBUFR               
*        TM    DMCB+8,X'80'        IF EOF ON FIRST READ                         
*        BNZ   LS100                                                            
*                                                                               
         LAY   R3,EZWRKIOB                                                      
         USING WRKIOB,R3                                                        
*                                                                               
LS210    DS    0H                                                               
         MVI   WRKIACTN,WRKIAGET                                                
         GOTO1 EZWRKIO,(R3)                                                     
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    LS100                                                            
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         L     RF,AIO1                                                          
         AHI   RF,4                SKIP RECORD LENGTH                           
         CLC   =C'21',0(RF)                                                     
         BNE   LS210                                                            
*                                                                               
         LA    RF,3(RF)            IDB NUMBER FIELD                             
         LHI   R0,9                                                             
         CLI   0(RF),X'5E'         SEMICOLON                                    
         BE    *+14                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
*                                                                               
         LA    RF,1(RF)            ADVANCE PAST SEMICOLON                       
         LR    RE,RF               SAVE A(AGENCY NAME FIELD)                    
         LHI   R0,31                                                            
         CLI   0(RF),X'5E'         SEMICOLON                                    
         BE    *+14                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
*                                                                               
         SR    RF,RE               AGYNAME FIELD LENGTH                         
         CHI   RF,25                                                            
         BNH   *+8                                                              
         LHI   RF,25                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LAGYNAME(0),0(RE)                                                
*                                                                               
         LA    R3,SVEZKEY                                                       
         USING W_RECD,R3                                                        
*                                                                               
         L     RF,PRTCTR                                                        
         LA    RF,1(,RF)                                                        
         ST    RF,PRTCTR                                                        
*                                                                               
         MVC   LSTA,PRTSTA7C                                                    
*                                                                               
         OC    W_AGELD,W_AGELD                                                  
         BZ    LS340                                                            
         GOTO1 DATCON,DMCB,(2,W_AGELD),(8,LDATE)                                
*                                                                               
LS340    OC    W_FILENO(L'WRKEZSQN),W_FILENO                                    
         BZ    LS360                                                            
         SR    R0,R0                                                            
         ICM   R0,15,W_FILENO                                                   
         EDIT  (R0),(6,LSEQ),FILL=0                                             
*                                                                               
         DROP  R3                                                               
*                                                                               
LS360    LAY   R2,WRKEZDSC                                                      
         USING EZWKRCMD,R2                                                      
         OC    EZWCPDAT,EZWCPDAT      TEST CONVERTED AT ALL                     
         BZ    LS366                                                            
         CLC   EZWCPDAT,SPACES        TEST CONVERTED AT ALL                     
         BE    LS370                   NO                                       
*                                                                               
         NC    EZWCPDAT,=X'BFBFBF'  SET OFF SPACES                              
*                                                                               
         GOTO1 DATCON,DMCB,(1,EZWCPDAT),(8,LSTAT)                               
*                                                                               
         TM    EZWCSTAT,X'40'      TEST COMPLETELY CONVERTED                    
         BO    LS370                YES                                         
         MVC   LSTAT+9(3),=C'(P)'     NO, MARK PARTIAL                          
         B     LS370                                                            
*                                                                               
LS366    TM    EZWCSTAT,X'40'      TEST COMPLETELY CONVERTED                    
         BZ    LS370                                                            
         MVC   LSTAT+9(3),=C'(D)'    YES, MARK ALL DELETED                      
*                                                                               
LS370    OC    EZWCSRCE,EZWCSRCE                                                
         BZ    *+10                                                             
         MVC   LSRCE,EZWCSRCE                                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
LS400    CLI   MODE,PRINTREP                                                    
         BE    LS600                                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DKEYD,R6                                                         
         MVC   DWKKEY,SVEZKEY                                                   
         MVC   DRDLIN,LISTNUM                                                   
*                                                                               
         ICM   R5,15,ASVEZLIS                                                   
         USING SVEZLIS,R5                                                       
*                                                                               
         MVC   SVLSTA,LSTA                                                      
         MVC   SVLDATE,LDATE                                                    
         MVC   SVLSEQ,LSEQ                                                      
         MVC   SVLSTAT,LSTAT                                                    
*        MVC   SVLUID,LUID                                                      
         MVC   DWKKEY,SVEZKEY                                                   
         LA    R5,SVEZLEN(,R5)                                                  
         STCM  R5,15,ASVEZLIS                                                   
         MVC   DMDSKADD(4),=X'0000FFFF'                                         
         MVI   LKEY+1,32                                                        
         DROP  R6,5                                                             
*                                                                               
         GOTO1 LISTMON                                                          
         B     LS100                                                            
*                                                                               
LS600    MVC   P+2(LINELQ),LISTAR                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    LISTAR,LISTAR                                                    
         B     LS100                                                            
*                                                                               
*                                                                               
*                                                                               
* DISPLAY AGENCY NAME & ADDRESS *                                               
*                                                                               
DISAGY   NTR1                                                                   
         ST    R0,FULL                                                          
         L     R7,AIO2             EZBLOCK                                      
*                                                                               
         CLI   EZMODE,EZINVP                                                    
         JNE   EQXIT                                                            
*                                                                               
* DISPLAY AGENCY INFORMATION                                                    
*                                                                               
         LA    R2,DBAANMH                                                       
         MVC   8(30,R2),EZAGNAM                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   DBAAD1,SPACES                                                    
         OI    DBAAD1H+6,X'80'                                                  
         CLC   EZAGLIN1,SPACES                                                  
         BNH   *+10                                                             
         MVC   DBAAD1,EZAGLIN1                                                  
*                                                                               
         MVC   DBAAD2,SPACES                                                    
         OI    DBAAD2H+6,X'80'                                                  
         CLC   EZAGLIN2,SPACES                                                  
         BNH   *+10                                                             
         MVC   DBAAD2,EZAGLIN2                                                  
*                                                                               
         MVC   DBAAD3,SPACES                                                    
         OI    DBAAD3H+6,X'80'                                                  
         CLC   EZAGLIN3,SPACES                                                  
         BNH   *+10                                                             
         MVC   DBAAD3,EZAGLIN3                                                  
*                                                                               
         MVC   DBAAD4,SPACES                                                    
         OI    DBAAD4H+6,X'80'                                                  
         CLC   EZAGLIN4,SPACES                                                  
         BNH   *+10                                                             
         MVC   DBAAD4,EZAGLIN4                                                  
*                                                                               
* DISPLAY IM ROUTING INFO                                                       
*                                                                               
         LAY   R3,EZBLKX                                                        
         USING EZBLKX,R3                                                        
*                                                                               
         MVC   DBAIAN,SPACES                                                    
         LA    R2,DBAIANH                                                       
         CLC   EZIMNAM,SPACES                                                   
         BNH   *+10                                                             
         MVC   8(25,R2),EZIMNAM                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   DBAIAD,SPACES                                                    
         OI    DBAIADH+6,X'80'                                                  
         CLC   EZIMADR,SPACES                                                   
         BNH   *+10                                                             
         MVC   DBAIAD,EZIMADR                                                   
         DROP  R3                                                               
*                                                                               
DISAGYX  DS    0H                                                               
         L     RD,FULL             RETURN AFTER EZMOD CALL (I HOPE)             
         B     EXIT                                                             
*                                                                               
* HEAD HOOK RTN *                                                               
*                                                                               
HDHK     NTR1                                                                   
*&&DO                                                                           
         CLI   TWAOFFC,C'*'        DDS TERMINAL                                 
         BE    EXIT                                                             
         MVC   H8+LUID-LISTAR(7),SPACES                                         
         MVC   H9+LUID-LISTAR(7),SPACES                                         
*&&                                                                             
         B     EXIT                                                             
*                                                                               
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
CLRSCRN  NTR1                                                                   
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
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVAL    MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
BADATE   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
BADACTER MVI   ERROR,INVACT                                                     
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,32,C'BATCH LIST'                                              
         SSPEC H2,30,C'--------------'                                          
         SSPEC H1,53,AGYNAME                                                    
         SSPEC H2,53,AGYADD                                                     
         SSPEC H3,53,REPORT                                                     
         SSPEC H3,69,C'EASI'                                                    
         SSPEC H4,53,RUN                                                        
         SSPEC H5,63,PAGE                                                       
         SSPEC H8,3,C'STATION'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,12,C'DATE'                                                    
         SSPEC H9,12,C'--------'                                                
         SSPEC H8,22,C'SEQ'                                                     
         SSPEC H9,22,C'----'                                                    
         SSPEC H8,29,C'SRCE'                                                    
         SSPEC H9,29,C'----'                                                    
         SSPEC H8,36,C'CONVERTED'                                               
         SSPEC H9,36,C'---------'                                               
*        SSPEC H8,52,C'USER-ID'                                                 
*        SSPEC H9,52,C'-------'                                                 
         SSPEC H8,50,C'AGENCY NAME'                                             
         SSPEC H9,50,C'-------------------------'                               
         DC    X'00'                                                            
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* FAKEIND - ROUTINE TO FILL IN INDEX FIELDS WITH DDWRKIOD DATA                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
FAKEIND  NTR1  BASE=*,LABEL=*                                                   
         L     R7,AIO2                                                          
*                                                                               
         LAY   R1,WRKEZKEY                                                      
         USING WRKEZKEY,R1                                                      
         LA    R2,SVEZKEY                                                       
         USING UKINDEX,R2                                                       
*                                                                               
         MVC   UKUSRID,WRKEZUID                                                 
         MVC   UKSYSPRG(L'WRKEZSCL),WRKEZSCL                                    
         MVC   UKDAY,WRKEZDAY                                                   
         MVC   UKCLASS,WRKEZMED                                                 
* !!! UKTYPE AND UKATTB SACRIFICED TO FIT IN THE 4-BYTE SEQ NUMBER !!!          
         MVC   UKFILENO(L'WRKEZSQN),WRKEZSQN                                    
         MVC   UKSTAT,WRKEZSTA                                                  
         MVC   UKAGELD,WRKEZBDT                                                 
         MVC   UKUDATA,WRKEZUDT                                                 
*                                                                               
         DROP  R2,R1                                                            
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
SETUP    NTR1  BASE=*,LABEL=*                                                   
         L     R7,AIO2                                                          
*                                                                               
* INITIALIZE EZBLOCK FOR EZMOD                                                  
*                                                                               
         LARL  RE,DISAGY                                                        
         ST    RE,EZHOOK                                                        
         MVC   EZWKRFIL,EASIWK                                                  
         MVC   EZWKRIND,SVEZKEY                                                 
         LA    R1,WRKFBUFR                                                      
         ST    R1,EZWKRBUF                                                      
         MVC   EZWKRREC,AIO1                                                    
         LA    RE,2048(RE)                                                      
         ST    RE,EZAREC                                                        
         L     RE,ACOMFACS                                                      
         ST    RE,EZCOMFCS                                                      
         MVI   EZLOOKSW,X'E0'                                                   
         MVI   EZTEST,C'Y'                                                      
         MVI   EZWRITE,C'N'                                                     
*                                                                               
* INITIALIZE WORKIO BLOCK                                                       
*                                                                               
         LAY   R5,EZWRKIOB                                                      
         USING WRKIOB,R5                                                        
*                                                                               
         MVI   EZWRKIOF,C'Y'                                                    
         MVC   WRKEZUID,TWAORIG                                                 
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO1                                                    
         LA    RF,WRKFBUFR                                                      
         STCM  RF,15,WRKIABUF                                                   
         MVI   WRKIFTYP,WRKIFTEZ                                                
         DROP  R5                                                               
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A2C'        DDWRKIO                            
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   EZWRKIO,0(R1)                                                    
*                                                                               
         XC    DMCB,DMCB                                                        
         GOTO1 CALLOV,DMCB,(X'10',0),ATWA   EZMOD                               
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   AEZMOD,0(R1)                                                     
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
* EZBLOCK                                                                       
* CTGENFILE                                                                     
       ++INCLUDE SPGENEZ                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE EZBLOCK                                                        
*                                                                               
EZBLOCKD DSECT                                                                  
         ORG   EZWRKIOB                                                         
       ++INCLUDE DDWRKIOD                                                       
         ORG                                                                    
*                                                                               
       ++INCLUDE SPEZFFFD                                                       
         ORG   CONTAGH                                                          
* SPEZFFAD                                                                      
       ++INCLUDE SPEZFFAD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
* SPEZFEAD                                                                      
       ++INCLUDE SPEZFEAD                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* DMWRKFD                                                                       
       ++INCLUDE DMWRKFD                                                        
         EJECT                                                                  
* DMWRKFK                                                                       
       ++INCLUDE DMWRKFK                                                        
         EJECT                                                                  
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
       ++INCLUDE SPEZFSYSD                                                      
       ++INCLUDE SPEZDSCTS                                                      
*                                                                               
SYSD     DSECT                                                                  
         ORG   WRKFREC                                                          
SVLNNUM  DS    F                                                                
ALLCTRS  DS   0CL8                                                              
BATCTR   DS    F                                                                
PRTCTR   DS    F                                                                
AEZMOD   DS    A                                                                
SVDTES   DS   0XL4                                                              
SVSDTE   DS    XL2                                                              
SVEDTE   DS    XL2                                                              
SVSEQ    DS    CL5                                                              
SVBSEQ   DS    XL4                                                              
*SVUID    DS    CL8                                                             
SVUIDNUM DS    CL2                                                              
OPTEXP   DS    C                                                                
OPTMED   DS    C                   MEDIA FILTER                                 
SVEZKEY  DS    CL42                                                             
*                                                                               
ASVEZLIS DS    XL4                 SVEZLIS POINTER                              
*                                                                               
SVEZLIS  DS    0C                                                               
SVLSTA   DS    CL7                                                              
SVLDATE  DS    CL8                                                              
SVLSEQ   DS    CL6                                                              
SVLSTAT  DS    CL13                                                             
*SVLUID   DS    CL8                                                             
SVEZLEN  EQU   *-SVEZLIS                                                        
         DS    15CL(SVEZLEN)                                                    
SVEZLNQ  EQU   (*-SVEZLIS)/SVEZLEN                                              
*                                                                               
*                                                                               
*                                                                               
DKEYD    DSECT                                                                  
DWKKEY   DS    CL24                                                             
DRDLIN   DS    XL1                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTA     DS    CL7                                                              
         DS    CL2                                                              
LDATE    DS    CL8                                                              
         DS    CL2                                                              
LSEQ     DS    CL6                                                              
         DS    CL2                                                              
LSRCE    DS    CL4                                                              
         DS    CL2                                                              
LSTAT    DS    CL12                                                             
         DS    CL2                                                              
*LUID     DS    CL8                                                             
LAGYNAME DS    CL25                                                             
LINELQ   EQU   *-LSTA                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053SPEZF05   02/28/18'                                      
         END                                                                    
