*          DATA SET SPEZF12    AT LEVEL 047 AS OF 01/23/17                      
*PHASE T23012A                                                                  
         TITLE 'T23012 - STATION EQUIVALENCY RECORD'                            
***********************************************************************         
*                                                                     *         
*  TITLE: T23012 - EASI STATION EQUIVALENCY RECORDS                   *         
*  COMMENTS: THIS PROGRAM DOES MAINT AND LIST FOR STATION EQV RECS    *         
*            WHICH ARE STORED ON MPLDIR/MPLFIL.                       *         
*                                                                     *         
*  OUTPUTS: UPDATED STATION EQUIVALENCY RECORDS                       *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 -                                                       *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG                                              *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - NOT USED                                              *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 - DUMMY GETREC AT END OF VR RTN & CKSTA            *         
*             AIO3 - STATION RECS IN VR RTN                           *         
*                                                                     *         
***********************************************************************         
*                                                                               
***********************************************************************         
*                                                                     *         
*  LEV 10    JAN29/92 CK IF BASE STATION IS ALREADY EQUIVALENT        *         
*  LEV 11    FEB13/92 ALLOW -C AND -S MEDIAS                          *         
*  LEV 12    FEB18/92 BUILD KEY FOR RESTORE                           *         
*  LEV 13    APR27/92 DISALLOW CROSS SYSTEM STATIONS (NET TO SPOT)    *         
*  LEV 14    MAY05/92 CHECK IF STATION IS USED ELSEWHERE              *         
*  LEV 15    JUN09/92 CHANGE INVSTAT TO NOSTAFND                      *         
*  LEV 16    JUN17/92 FIX SVKEY RESTORE TO KEY                        *         
*  LEV 17    FEB10/95 DON'T ALLOW EQUAL BASE & EQUIVALENT STATIONS    *         
*  LEV 18    JAN14/97 FIX DISPLAY                                     *         
*  LEV 19    SEP10/97 FIX VALIDATE STATION (TO GET MEDIA)             *         
*  LEV 20    JAN20/98 ALLOW MORE THAN 1 EQUIVALENT STATION            *         
*  LEV 21    APR28/98 FIX ONLINE LIST                                 *         
*  LEV 22    FEB09/00 ALLOW ACROSS MEDIA - MUST BE ON SYSTEM WITH     *         
*                     ACTUAL EQUIVALENT STATION                       *         
*  LEV 23    APR24/00 ADD ADVERTISER NAME AND USED-ID FIELDS          *         
*                     SWITCH RECORDS FROM MPL TO CONTROL (GENDIR/FIL) *         
*  LEV 24    MAY24/00 ALLOW RENAME BY ADVERTISER TO SAME ID           *         
*  LEV 27    JUN23/00 ALLOW CROSS AGENCY MOVES                        *         
*  LEV 42    SEP09/14 SUPPORT 8 ADVERTISER NAMES AND TWO USER-IDS     *         
*  LEV 46    OCT31/16 ENHANCE STATION LIST                            *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T23012   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3012**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
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
         CLI   MODE,XRECADD        AFTER ADD RECORD                             
         BE    FORCE                                                            
         CLI   MODE,XRECPUT        AFTER PUT RECORD                             
         BE    FORCE                                                            
         CLI   MODE,XRECREST       AFTER RESTORE RECORD                         
         BE    FORCE                                                            
         CLI   MODE,XRECDEL        AFTER DELETE RECORD                          
         BE    FORCE                                                            
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     XIT                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
VKEY     DS    0H                                                               
         MVI   CURSYST,C'M'        SWITCH TO MEDIA (SPOT/NET)                   
         GOTO1 VALIFAS             SWITCH                                       
         GOTO1 VALIMED                                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      IGNORE IF LIST                               
         BE    VK400                                                            
*                                                                               
         LA    R2,EZFSTAH          STATION                                      
         CLI   5(R2),0                                                          
         BNE   VK100                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'* ERROR * STATION REQUIRED *'                     
         B     MYERR                                                            
*                                                                               
* VALIDATE STATION CALL LETTERS *                                               
*                                                                               
VK100    DS   0H                                                                
         MVC   CONHEAD(26),=C'* ERROR * INVALID STATION *'                      
         LA    R2,EZFSTAH                                                       
         LA    R1,OSTA                                                          
         MVC   OSTA,SPACES                                                      
         BRAS  RE,VSTA                                                          
         BNE   MYERR                                                            
*                                                                               
         CLI   ACTNUM,ACTADD       IGNORE UNLESS ADD                            
         BNE   VKXIT                                                            
         LA    R5,OSTA                                                          
         BRAS  RE,CKSTA            GO SEE IF ALREADY SET AS BASE                
         BNE   BASSTAER                                                         
*                                                                               
         B     VKXIT                                                            
*                                                                               
* VALIDATE FILTERS                                                              
*                                                                               
VK400    LA    R2,EZFFTRH                                                       
         XC    CUFTRS,CUFTRS       CLEAR FILTERS                                
         CLI   5(R2),0                                                          
         BE    VKXIT                                                            
         MVI   ERRFLD,1                                                         
         LA    R6,2                                                             
         LA    R4,WORK                                                          
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'* ERROR * INVALID OPTION *'                       
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    MYERR                                                            
*                                                                               
VK500    CLI   0(R4),0                                                          
         BE    VKXIT                                                            
         CLI   0(R4),1                                                          
         BNE   MYERR                                                            
*                                  USER CODE                                    
         CLI   12(R4),C'U'                                                      
         BNE   VK600                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'* ERROR * INVALID USER ID *'                      
         CLI   1(R4),8                                                          
         BH    VKERR                                                            
         MVC   CUUID,22(R4)                                                     
         B     VK400                                                            
*                                  STATION NO REAL VALIDATION                   
VK600    CLI   12(R4),C'S'                                                      
         BNE   VKERR                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'* ERROR * INVALID STATION *'                      
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
*                                                                               
*                                  UP ERROR FIELD & NEXT BLOCK                  
VK700    ZIC   RE,ERRFLD                                                        
         LA    RE,1(RE)                                                         
         STC   RE,ERRFLD                                                        
         LA    R4,32(R4)                                                        
         BCT   R6,VK500                                                         
         MVC   CONHEAD,SPACES                                                   
*                                                                               
VKXIT    MVI   CURSYST,C'C'        SWITCH TO CONTROL SYTEM                      
         GOTO1 VALIFAS             SWITCH                                       
         MVC   SYSDIR(3),=C'GEN'                                                
         MVC   SYSFIL(3),=C'GEN'                                                
*                                                                               
         XC    KEY,KEY             SET UP KEY/SVKEY                             
         LA    R4,KEY                                                           
         USING EZSTAD,R4                                                        
         MVC   EZSKEY,SPACES                                                    
         MVC   EZSKTYP,=C'ZS'                                                   
         MVC   EZSKAGY,AGENCY                                                   
         MVC   EZSKSTA,OSTA                                                     
         MVC   SVKEY,KEY                                                        
         OI    EZFESTAH+4,X'20'                                                 
         OI    EZFEADVH+4,X'20'                                                 
         TM    EZFEUIDH+4,X'20'    ANY ENTRY                                    
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VREC     L     R6,AIO1                                                          
         MVC   AIO,AIO1                                                         
         USING EZSTAD,R6                                                        
*                                                                               
         MVC   QSTA,OSTA                                                        
         BRAS  RE,ISDIGITL                                                      
         MVC   OSTADIG,BYTE                                                     
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR100                                                            
*                                                                               
         LR    RE,R6                                                            
         L     RF,SIZEIO                                                        
         XCEFL                                                                  
*                                                                               
         MVC   EZSKTYP(32),SVKEY   KEY                                          
         MVC   EZSLEN,=H'43'       LENGTH = 42(KEY) +1(END 0)                   
*                                                                               
VR100    DS   0H                                                                
*                                  NEED TO HAVE THE BASE ELEMENT                
         CLI   ACTNUM,ACTADD       MUST HAVE 02 ELEM                            
         BE    *+12                                                             
         TM    EZFESTAH+4,X'20'                                                 
         BO    VR130                                                            
*                                                                               
* EMPTY OUT OLD ELEMENT                                                         
*                                                                               
         MVI   ELCODE,02                                                        
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,EZFESTAH         VALIDATE EQUIVALENT STATION                  
         CLI   5(R2),0             1ST REQUIRED                                 
         BNE   VR110                                                            
*                                                                               
         MVC   CONHEAD(34),=C'* ERROR * EQUIV STATION REQUIRED *'               
         B     MYERR                                                            
*                                                                               
VR110    DS   0H                                                                
         GOTO1 VREADSTA            READ STATION FROM TWA TO QSTA                
         BNE   BADSTAT                                                          
         MVC   QSTA,FLDRDSTA                                                    
*                                                                               
         BRAS  RE,ISDIGITL                                                      
         CLC   OSTADIG,BYTE                                                     
         BNE   DIGIERR                                                          
*                                                                               
         LA    R5,QSTA                                                          
         BRAS  RE,CKSTA            SEE IF ALREADY SET AS BASE                   
         BNE   BASSTAER                                                         
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EZSIDEL,R6                                                       
         MVI   EZSIDEL,X'02'                                                    
         MVI   EZSIDEL+1,EZSELEN                                                
         MVC   EZSSTA,QSTA                                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         OI    EZFESTAH+4,X'20'                                                 
*                                                                               
VR130    DS    0H                                                               
         TM    EZFEADVH+4,X'20'    ANY ENTRY                                    
         BZ    VR140                YES, CHANGED                                
*                                                                               
         TM    EZFEUIDH+4,X'20'    ANY ENTRY                                    
         BO    VR160                NO                                          
*                                                                               
* VALIDATE ADVNAME & USER ID TOGETHER, 2 OR NONE REQUIRED                       
*                                                                               
VR140    DS    0H                                                               
* EMPTY OUT OLD ELEMENT                                                         
*                                                                               
         MVI   ELCODE,X'22'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,EZFEADVH         VALIDATE ADVERTISER                          
         CLI   5(R2),0                                                          
         BNE   VR150               IF NOT THERE, CAN'T HAVE USER ID             
*                                                                               
         CLI   EZFEUIDH+5,0                                                     
         BNE   MISSERR             IF NO ADVNAME, CAN'T HAVE USER ID            
         B     VR160                                                            
*                                                                               
VR150    DS    0H                                                               
         LA    R2,EZFEUIDH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR             IF ADVNAME ENTERED-MUST HAVE USER ID         
*                                                                               
         XC    LKAGYBLK,LKAGYBLK                                                
         LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LKAGYUID(0),8(R2)                                                
         GOTO1 VLKAGY,LKAGYBLK                                                  
         BNE   INVALUID                                                         
         MVC   SVORIG,LKAGYBID                                                  
         MVC   NEWAGY,LKAGYAGY                                                  
*                                                                               
         CLC   AGENCY,NEWAGY                                                    
         BE    *+12                                                             
         BRAS  RE,CKTRAN                                                        
         BNE   BADAGYER                                                         
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EZAUDEL,R6                                                       
         MVI   EZAUDEL,X'22'                                                    
         MVI   EZAUDEL+1,EZAULEN                                                
         MVC   EZAUADV,EZFEADV     SAVE ADVERTISER NAME                         
         MVC   EZAUUID,EZFEUID        & USER ID                                 
         MVC   EZAUORIG,SVORIG                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         OI    EZFEADVH+4,X'20'                                                 
         OI    EZFEUIDH+4,X'20'                                                 
*                                                                               
VR160    DS    0H                                                               
         LA    R2,EZFRUEQH         FIRST USER ID PROTECTED FIELD                
         LA    R3,EZFSAEQH         END OF USER-IDS                              
*                                                                               
VR165    CR    R2,R3               END OF USER IDS?                             
         BNL   VR200               YES - DONE                                   
         LA    R0,2                2 UNPROTECTED FIELDS                         
*                                                                               
VR165A   BAS   RE,NXTUNP           NEXT UNPROTECTED FIELD                       
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BZ    VR170               YES - REBUILD THE X'42' ELEMENTS             
         BCT   R0,VR165A           TEST IF CHANGED                              
*                                                                               
         LLC   RF,0(R2)            LENGTH OF FIELD                              
         AR    R2,RF               BUMP TO NEXT LINE                            
         B     VR165               TEST EOS/PROCESS NEXT LINE                   
*                                                                               
VR170    MVI   ELCODE,X'42'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,EZFRUEQH         FIRST USER ID PROTECTED FIELD                
         LA    R3,EZFSAEQH         END OF USER-IDS                              
*                                                                               
VR175    CR    R2,R3               END OF USER IDS?                             
         BNL   VR200               YES - DONE                                   
         BAS   RE,NXTUNP           NEXT UNPROTECTED FIELD                       
         LR    R4,R2               SAVE OFF A(ADVERTISER)                       
         CLI   5(R2),0             UID PRESENT?                                 
         BNE   VR180               YES - VALIDATE IT                            
*                                                                               
         BAS   RE,NXTUNP           NEXT UNPROTECTED FIELD                       
         CLI   5(R2),0             ANY STATION?                                 
         BE    *+10                NO - BUMP TO NEXT FIELD                      
         LR    R2,R4               A(ADVNAME)                                   
         B     MISSERR             IF NO ADVNAME, CAN'T HAVE USER ID            
         B     VR190               BUMP TO NEXT LINE                            
*                                                                               
VR180    DS    0H                  USER ID ENTERED AT THIS POINT                
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),SPACES                                                
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCUID                                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,AIO                     
         CLI   8(R1),0                                                          
         BNE   INVALUID                                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         MVC   DATADISP,=H'28'                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVORIG,2(R6)                                                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DATADISP,=H'42'                                                  
*                                                                               
         MVC   NEWAGY,2(R6)                                                     
         CLC   AGENCY,NEWAGY                                                    
         BE    VR185                                                            
*                                                                               
         BRAS  RE,CKTRAN                                                        
         BNE   BADAGYER                                                         
*                                                                               
VR185    DS    0H                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         BAS   RE,NXTUNP           NEXT UNPROTECTED FIELD                       
         CLI   5(R2),0             STATION THERE?                               
         BE    MISSERR             NO - ERROR, HAS TO BE HERE                   
*                                                                               
         GOTO1 VREADSTA            READ STATION FROM TWA TO QSTA                
         BNE   BADSTAT                                                          
         MVC   QSTA,FLDRDSTA                                                    
*                                                                               
         BRAS  RE,ISDIGITL                                                      
         CLC   OSTADIG,BYTE                                                     
         BNE   DIGIERR                                                          
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EZRDEL,R6                                                        
         MVI   EZRDEL,X'42'                                                     
         MVI   EZRDEL+1,EZRDLEN                                                 
         LR    R2,R4               A(USER ID)                                   
         MVC   EZRDUID,8(R2)       USER ID                                      
         OI    4(R2),X'20'         USER ID PREVIOUSLY VALIDATED                 
         BAS   RE,NXTUNP           NEXT UNPROTECTED FIELD                       
         OI    4(R2),X'20'         STATION PREVIOUSLY VALIDATED                 
         MVC   EZRDBUID,SVORIG     BINARY USER ID                               
         MVC   EZRDSTA,QSTA                                                     
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR190    LLC   R0,0(R2)            LENGTH OF FIELD                              
         AR    R2,R0               BUMP TO NEXT LINE                            
         B     VR175               PROCESS NEXT LINE                            
*                                                                               
VR200    TM    EZFSADVH+4,X'20'    ADVERTISER STILL SAME                        
         BZ    VR210                NO                                          
*                                                                               
         TM    EZFSUIDH+4,X'20'    USER ID STILL SAME                           
         BZ    VR210                NO                                          
*                                                                               
         TM    EZFSSTAH+4,X'20'    STATION STILL SAME                           
         BO    VR350                YES                                         
*                                                                               
* VALIDATE ADVNAME, USER ID, & STATION TOGETHER, 3 OR NONE REQUIRED             
*                                                                               
* EMPTY OUT OLD ELEMENT                                                         
*                                                                               
VR210    DS    0H                                                               
         MVI   ELCODE,X'32'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,EZFSADVH         VALIDATE ADVERTISER                          
         CLI   5(R2),0                                                          
         BNE   VR250               IF NOT THERE, CAN'T HAVE USER ID             
*                                                                               
         CLI   EZFSUIDH+5,0                                                     
         BNE   MISSERR             IF NO ADVNAME, CAN'T HAVE USER ID            
         B     VR350                                                            
*                                                                               
VR250    DS    0H                                                               
         LA    R2,EZFSUIDH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR             IF ADVNAME ENTERED-MUST HAVE USER ID         
*                                                                               
* ALLOW THIS AS RENAME BY CLIENT                                                
*                                                                               
         XC    LKAGYBLK,LKAGYBLK                                                
         LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LKAGYUID(0),8(R2)                                                
*                                                                               
         GOTO1 VLKAGY,LKAGYBLK                                                  
         BNE   INVALUID                                                         
         MVC   SVORIG,LKAGYBID                                                  
         MVC   NEWAGY,LKAGYAGY                                                  
*                                                                               
         CLC   AGENCY,NEWAGY                                                    
         BE    VR280                                                            
*                                                                               
         BRAS  RE,CKTRAN                                                        
         BNE   BADAGYER                                                         
*                                                                               
VR280    DS    0H                                                               
         LA    R2,EZFSSTAH         VALIDATE EQUIVALENT STATION                  
         CLI   5(R2),0             1ST REQUIRED                                 
         BNE   VR300                                                            
*                                                                               
         MVC   CONHEAD(34),=C'* ERROR * EQUIV STATION REQUIRED *'               
         B     MYERR                                                            
*                                                                               
VR300    DS   0H                                                                
         GOTO1 VREADSTA            READ STATION FROM TWA TO QSTA                
         BNE   BADSTAT                                                          
         MVC   QSTA,FLDRDSTA                                                    
*                                                                               
         BRAS  RE,ISDIGITL                                                      
         CLC   OSTADIG,BYTE                                                     
         BNE   DIGIERR                                                          
*                                                                               
         LA    R5,QSTA                                                          
         BRAS  RE,CKSTA            SEE IF ALREADY SET AS BASE                   
         BNE   BASSTAER                                                         
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EZAUSEL,R6                                                       
         MVI   EZAUSEL,X'32'                                                    
         MVI   EZAUSEL+1,EZAUSLEN                                               
*                                                                               
         MVC   EZAUSADV,EZFSADV     SAVE ADVERTISER NAME                        
         MVC   EZAUSUID,EZFSUID        & USER ID                                
         MVC   EZAUSORG,SVORIG                                                  
         MVC   EZSSSTA,QSTA                                                     
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         OI    EZFSADVH+4,X'20'                                                 
         OI    EZFSUIDH+4,X'20'                                                 
         OI    EZFSSTAH+4,X'20'                                                 
*                                                                               
VR350    LA    R2,EZFSEQUH         FIRST ADVERTISER'S EQUAL SIGN                
         LA    R3,EZFFLGH          END OF SCREEN                                
*                                                                               
VR350A   CR    R2,R3               END OF SCREEN?                               
         BNL   VR500               YES - DONE                                   
         LA    R0,3                3 UNPROTECTED FIELDS                         
*                                                                               
VR350B   BAS   RE,NXTUNP           NEXT UNPROTECTED FIELD                       
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BZ    VR360               YES - REBUILD THE X'34' ELEMENTS             
         BCT   R0,VR350B           TEST IF CHANGED                              
*                                                                               
         LLC   RF,0(R2)            LENGTH OF FIELD                              
         AR    R2,RF               BUMP TO NEXT LINE                            
         B     VR350A              TEST EOS/PROCESS NEXT LINE                   
*                                                                               
* VALIDATE ADVNAME, USER ID, & STATION TOGETHER, 3 OR NONE REQUIRED             
*                                                                               
* EMPTY OUT OLD ELEMENT                                                         
*                                                                               
VR360    DS    0H                                                               
         MVI   ELCODE,X'34'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,EZFSEQUH         FIRST ADVERTISER'S EQUAL SIGN                
         LA    R3,EZFFLGH          END OF SCREEN                                
*                                                                               
VR365    CR    R2,R3               END OF SCREEN?                               
         BNL   VR500               YES - DONE                                   
         BAS   RE,NXTUNP           NEXT UNPROTECTED FIELD                       
         LR    R4,R2               SAVE OFF A(ADVERTISER)                       
         CLI   5(R2),0             ADVERTISER INPUT?                            
         BNE   VR370               IF NOT THERE, CAN'T HAVE USER ID             
*                                                                               
         BAS   RE,NXTUNP           NEXT UNPROTECTED FIELD                       
         CLI   5(R2),0             ANY USER-ID?                                 
         BE    *+10                NO - BUMP TO NEXT FIELD                      
         LR    R2,R4               A(ADVNAME)                                   
         B     MISSERR             IF NO ADVNAME, CAN'T HAVE USER ID            
         BAS   RE,NXTUNP           NEXT UNPROTECTED FIELD                       
         B     VR380               BUMP TO NEXT LINE                            
*                                                                               
VR370    BAS   RE,NXTUNP           NEXT UNPROTECTED FIELD                       
         CLI   5(R2),0             HAVE USER-ID?                                
         BE    MISSERR             IF ADVNAME ENTERED-MUST HAVE USER ID         
*                                                                               
         XC    LKAGYBLK,LKAGYBLK                                                
         LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LKAGYUID(0),8(R2)                                                
*                                                                               
         GOTO1 VLKAGY,LKAGYBLK                                                  
         BNE   INVALUID                                                         
         MVC   SVORIG,LKAGYBID                                                  
         MVC   NEWAGY,LKAGYAGY                                                  
*                                                                               
         CLC   AGENCY,NEWAGY                                                    
         BE    *+12                                                             
         BRAS  RE,CKTRAN                                                        
         BNE   BADAGYER                                                         
*                                                                               
         BAS   RE,NXTUNP           NEXT UNPROTECTED FIELD                       
         CLI   5(R2),0             1ST EQUIVALENT STATION REQUIRED              
         BNE   *+14                                                             
         MVC   CONHEAD(34),=C'* ERROR * EQUIV STATION REQUIRED *'               
         B     MYERR                                                            
*                                                                               
         GOTO1 VREADSTA            READ STATION FROM TWA TO QSTA                
         BNE   BADSTAT                                                          
         MVC   QSTA,FLDRDSTA                                                    
*                                                                               
         BRAS  RE,ISDIGITL                                                      
         CLC   OSTADIG,BYTE                                                     
         BNE   DIGIERR                                                          
*                                                                               
         LA    R5,QSTA                                                          
         BRAS  RE,CKSTA            SEE IF ALREADY SET AS BASE                   
         BNE   BASSTAER                                                         
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EZA2SEL,R6                                                       
         MVI   EZA2SEL,X'34'                                                    
         MVI   EZA2SEL+1,EZA2SLEN                                               
*                                                                               
         LR    R2,R4                A(ADVERTISER NAME)                          
         MVC   EZA2SADV,8(R2)       SAVE ADVERTISER NAME                        
         OI    4(R2),X'20'          ADVERTISER PREVIOUSLY VALIDATED             
         BAS   RE,NXTUNP            NEXT UNPROTECTED FIELD                      
         MVC   EZA2SUID,8(R2)       SAVE USER ID                                
         OI    4(R2),X'20'          USER-ID PREVIOUSLY VALIDATED                
         BAS   RE,NXTUNP            NEXT UNPROTECTED FIELD                      
         OI    4(R2),X'20'          STATION PREVIOUSLY VALIDATED                
         MVC   EZA2SORG,SVORIG      SAVE ORIGIN                                 
         MVC   EZS2STA,QSTA         SAVE STATION                                
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR380    LLC   R0,0(R2)             LENGTH OF FIELD                             
         AR    R2,R0                BUMP TO NEXT LINE                           
         B     VR365                PROCESS NEXT LINE                           
*                                                                               
VR500    DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         CLI   ACTNUM,ACTADD                                                    
         BE    VRXIT                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
VRXIT    DS   0H                                                                
         MVI   CURSYST,C'C'        SWITCH TO CONTROL SYTEM                      
         GOTO1 VALIFAS             SWITCH                                       
         MVC   SYSDIR(3),=C'GEN'                                                
         MVC   SYSFIL(3),=C'GEN'                                                
*                                                                               
         B     XIT                                                              
*                                                                               
MVCUID   MVC   KEY+15(0),8(R2)                                                  
*                                                                               
NXTUNP   LLC   RF,0(R2)            LENGTH OF FIELD                              
         AR    R2,RF               BUMP TO NEXT FIELD                           
         TM    1(R2),X'20'         PROTECTED FIELD?                             
         BO    NXTUNP              NO - BUMP TO NEXT FIELD                      
         BR    RE                  RETURN                                       
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DKEY     EQU   *                                                                
         GOTO1 VPRTSTA,DMCB,KEY+EZSKSTA-EZSKEY,EZFSTA                           
         OI    EZFSTAH+6,X'80'                                                  
*                                                                               
         MVC   SVKEY,KEY                                                        
         MVC   OSTA,KEY+EZSKSTA-EZSKEY                                          
*                                                                               
DKXIT    B     XIT                                                              
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DREC     EQU   *                                                                
         TWAXC EZFESTAH,TRNS=Y,CLRINPUTLEN=Y                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EZSIDEL,R6                                                       
*                                                                               
         GOTO1 VPRTSTA,DMCB,EZSSTA,EZFESTA                                      
*                                                                               
         LA    R2,EZFESTAH                                                      
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
*                                                                               
         MVI   ELCODE,X'22'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   DR010                                                            
*                                                                               
         USING EZAUDEL,R6                                                       
         MVC   EZFEADV,EZAUADV     SHOW ADVERTISER NAME                         
         MVC   EZFEUID,EZAUUID        & USER ID                                 
*                                                                               
DR010    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'32'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR020                                                            
*                                                                               
         USING EZAUSEL,R6                                                       
         MVC   EZFSADV,EZAUSADV     SHOW ADVERTISER NAME                        
         MVC   EZFSUID,EZAUSUID        & USER ID                                
*                                                                               
         GOTO1 VPRTSTA,DMCB,EZSSSTA,EZFSSTA                                     
         OI    EZFSSTAH+6,X'80'                                                 
*                                                                               
DR020    DS    0H                                                               
         L     R6,AIO                                                           
         LA    R6,42(R6)                                                        
         LA    R2,EZFSEQUH                                                      
         MVI   ELCODE,X'34'                                                     
*                                                                               
DR020A   BAS   RE,NEXTEL                                                        
         BNE   DR050                                                            
*                                                                               
         USING EZA2SEL,R6                                                       
         BAS   RE,NXTUNP                 NEXT UNPROTECTED FIELD                 
         MVC   8(L'EZFSAD2,R2),EZA2SADV  ADVERTISER NAME                        
         BAS   RE,NXTUNP                 NEXT UNPROTECTED FIELD                 
         MVC   8(L'EZFSID2,R2),EZA2SUID  USER ID                                
         BAS   RE,NXTUNP                 NEXT UNPROTECTED FIELD                 
*                                                                               
         GOTO1 VPRTSTA,DMCB,EZS2STA,8(R2)                                       
         OI    6(R2),X'80'                                                      
         B     DR020A                                                           
*                                                                               
DR050    DS    0H                                                               
         L     R6,AIO                                                           
         LA    R6,42(R6)                                                        
         LA    R2,EZFRUEQH               FIRST USER ID PROTECTED FIELD          
         MVI   ELCODE,X'42'                                                     
*                                                                               
DR050A   BAS   RE,NEXTEL                                                        
         BNE   DRXIT                                                            
*                                                                               
         USING EZRDEL,R6                                                        
         BAS   RE,NXTUNP                 NEXT UNPROTECTED FIELD                 
         MVC   8(L'EZFRUID,R2),EZRDUID                                          
         OC    8(L'EZFRUID,R2),SPACES                                           
         BAS   RE,NXTUNP                 NEXT UNPROTECTED FIELD                 
*                                                                               
         GOTO1 VPRTSTA,DMCB,EZRDSTA,8(R2)                                       
         OI    6(R2),X'80'                                                      
         B     DR050A                                                           
*                                                                               
DRXIT    DS   0H                                                                
         OI    EZFESTAH+4,X'20'                                                 
         OI    EZFEADVH+4,X'20'                                                 
         OI    EZFEUIDH+4,X'20'                                                 
         OI    EZFSADVH+4,X'20'                                                 
         OI    EZFSUIDH+4,X'20'                                                 
         OI    EZFSSTAH+4,X'20'                                                 
         OI    EZFRUIDH+4,X'20'                                                 
         OI    EZFRSTAH+4,X'20'                                                 
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
* AFTER ADD, DELETE, CHANGE, OR RESTORE, FORCE CONTROLLER TABLE UPDATE          
*                                                                               
FORCE    B     XIT                                                              
*                                                                               
*                                  LIST RECORDS                                 
*                                                                               
LIST     DS   0H                                                                
         MVI   CURSYST,C'C'        SWITCH TO CONTROL SYTEM                      
         GOTO1 VALIFAS             SWITCH                                       
         MVC   SYSDIR(3),=C'GEN'                                                
         MVC   SYSFIL(3),=C'GEN'                                                
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LS100                                                            
*                                                                               
         LAY   R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
LS100    LA    R4,KEY                                                           
         USING EZSTAD,R4                                                        
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LS120                                                            
         MVC   EZSKTYP,=C'ZS'                                                   
         MVC   EZSKAGY,AGENCY                                                   
         MVC   L'EZSKTYP+L'EZSKAGY(8,R4),SPACES    SPACE PADDED SPARE           
* * * *  OC    OSTA,OSTA           HAVE VALIDATED STATION CODE?                 
* * * *  JZ    *+14                                                             
* * * *  MVC   EZSKSTA,OSTA                                                     
* * * *  J     LS120                                                            
         CLI   EZFSTAH+5,0         HAVE STATION INPUT?                          
         JNH   LS120                                                            
         MVC   EZSKSTA,EZFSTA                                                   
*                                                                               
LS120    GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE       KEY ID                                      
         BNE   XIT                                                              
         B     LS220                                                            
*                                                                               
LS200    GOTO1 SEQ                                                              
*                                                                               
LS220    CLC   KEY(4),KEYSAVE       KEY ID                                      
         BNE   XIT                                                              
         CLC   EZSKTYP,=C'ZS'                                                   
         BNE   XIT                                                              
         CLC   EZSKAGY,AGENCY                                                   
         BNE   XIT                                                              
*                                                                               
         MVC   LISTAR,SPACES                                                    
         GOTO1 VPRTSTA,DMCB,EZSKSTA,LSTA                                        
         DROP  R4                                                               
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         USING EZSTAD,R6                                                        
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EZSIDEL,R6                                                       
*                                                                               
* IF FILTERS MATCH ON FILTER                                                    
*                                                                               
         OC    CUSTA,CUSTA         STATION                                      
         BZ    *+14                                                             
         CLC   EZSSTA,CUSTA                                                     
         BNE   LS200                                                            
*                                                                               
         GOTO1 VPRTSTA,DMCB,EZSCALL,LESTA                                       
*                                                                               
         MVI   ELCODE,X'22'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   LS300                                                            
*                                                                               
         USING EZAUDEL,R6                                                       
*                                                                               
         MVC   LADV,EZAUADV                                                     
         MVC   LUID,EZAUUID                                                     
*                                                                               
LS300    DS   0H                                                                
         CLI   MODE,PRINTREP                                                    
         BE    LS600                                                            
         MVC   DMDSKADD,KEY+36                                                  
         GOTO1 LISTMON                                                          
         B     LS200                                                            
*                                                                               
LS600    MVC   P+10(7),LSTA                                                     
         MVC   P+35(7),LESTA                                                    
         MVC   P+50(25),LADV                                                    
         MVC   P+80(8),LUID                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    LISTAR,LISTAR                                                    
         B     LS200                                                            
         DROP  R6                                                               
*                                                                               
* CK IF STA ALREADY USED AS BASE                                                
*                                                                               
*                                                                               
* SOME DAY HEADHOOK ROUTINE                                                     
*                                                                               
HDHK     NTR1                                                                   
         B     XIT                                                              
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
VKERR    OI    ERRFLD,X'F0'                                                     
         MVC   CONHEAD+17(5),=C'FIELD'                                          
         MVC   CONHEAD+23(1),ERRFLD                                             
MYERRF   LA    R2,EZFFTRH                                                       
MYERR    MVI   GENSTAT2,USMYOK                                                  
         B     ERREXIT2                                                         
*                                                                               
BASSTAER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BASSTAMS),BASSTAMS                                     
         B     ERREXIT2                                                         
*                                                                               
SAMEIDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SAMEIDMS),SAMEIDMS                                     
         B     ERREXIT2                                                         
*                                                                               
INVALUID XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'** ERROR ** INVALID USER ID *'                    
         B     ERREXIT2                                                         
*                                                                               
BADAGYER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADAGYMS),BADAGYMS                                     
         B     ERREXIT2                                                         
DIGIERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DIGIERMS),DIGIERMS                                     
         B     ERREXIT2                                                         
*                                                                               
EQSTAER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'EQSTAMS),EQSTAMS                                       
*                                                                               
ERREXIT2 GOTO1 ERREX2                                                           
*                                                                               
BADSTAT  MVI   ERROR,NOSTAFND                                                   
         B     ERREXIT                                                          
MISSERR  MVI   ERROR,MISSING                                                    
*                                                                               
ERREXIT  GOTO1 ERREX                                                            
         DC    H'0'                                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
EQSTAMS  DC    C'* ERROR * STATION AND EQUIVALENT ARE EQUAL *'                  
BASSTAMS DC    C'* ERROR * STATION ALREADY IS BASE *'                           
SAMEIDMS DC    C'* ERROR * SAME ID AS SIGNED ON *'                              
BADAGYMS DC    C'* ERROR * CAN''T MOVE TO DIFFERENT AGENCY *'                   
DIGIERMS DC    C'* ERROR * CAN''T MIX DIGITAL, NON-DIGITAL STATIONS *'          
*                                                                               
*                                                                               
CLRSCRN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
CS010    IC    RE,0(R2)                                                         
         AHI   RE,-9               SUBTRACT FLD HEADER LEN +1                   
*                                                                               
         TM    1(R2),X'20'         THIS A PROTECTED FIELD                       
         BO    CS020                                                            
*                                                                               
         EX    RE,CSCLC            SPACES?                                      
         BE    CS020                                                            
         EX    RE,CSOC             NULLS?                                       
         BZ    CS020                                                            
         EX    RE,CSXC             CLEAR TO NULLS                               
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         J     XIT                                                              
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
VSTA     NTR1  BASE=*,LABEL=*                                                   
         LR    R4,R2               SAVE VALUE OF STA NAME FIELD                 
         LR    R5,R1                                                            
*                                                                               
         LA    R2,8(R2)                                                         
         LHI   R0,4                                                             
         MVI   4(R1),C'T'          ASSUME MEDIA IS T                            
*                                                                               
VSTA10   DS    0H                                                               
         CLI   0(R2),C' '                                                       
         BNH   VSTA60                                                           
         CLI   0(R2),C'-'                                                       
         BE    VSTA30                                                           
         CLI   0(R2),C'.'                                                       
         BE    VSTA20                                                           
         CLI   0(R2),C'A'                                                       
         JL    NEQXIT                                                           
         CLI   0(R2),C'Z'                                                       
         BNH   VSTA20                                                           
         CLI   0(R2),C'0'                                                       
         JL    NEQXIT                                                           
         CLI   0(R2),C'9'                                                       
         JH    NEQXIT                                                           
*                                                                               
VSTA20   MVC   0(1,R1),0(R2)                                                    
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,VSTA10                                                        
*                                                                               
* READ IN FOUR CHARACTERS                                                       
*                                                                               
         CLI   0(R2),C' '          NEXT CHARACTER - A SPACE?                    
         BNH   VSTA70              YES, SEE IF ANYTHING FOLLOWS IT              
         CLI   0(R2),C'-'          NEXT CHARACTER - A DASH?                     
         JNE   NEQXIT              NO - INVALID STATION                         
*                                                                               
* DASH HERE                                                                     
*                                                                               
VSTA30   DS    0H                                                               
         CHI   R0,4                ANY INPUT?                                   
         JNL   NEQXIT                                                           
         LA    R2,1(R2)            SKIP THE DASH                                
*                                                                               
* CHARACTER FOLLOWING THE DASH HERE.  PRESUMING IT IS MEDIA/BAND                
*                                                                               
         LLC   R3,5(R4)            LENGTH OF INPUT                              
         LA    R3,8(R3,R4)                                                      
         SR    R3,R2               RE = CHARACTERS REMAINING                    
*                                                                               
         LR    R1,R2               FOR GETMED CALL BELOW                        
*                                                                               
         CHI   R3,1                                                             
         BNE   VSTA40                                                           
*                                                                               
* 1 BYTE AFTER THE DASH.  PRESUME IT IS BAND, VALIDATE IT                       
         ICM   R1,8,=AL1(EZMTBNDQ) LOOK FOR BAND                                
         B     VSTA50                                                           
*                                                                               
VSTA40   DS    0H                                                               
         CHI   R3,2                                                             
         JNE   NEQXIT                                                           
* 2 CHARACTERS AFTER THE DASH HERE                                              
         ICM   R1,8,=AL1(EZMTPMDQ) LOOK FOR 2-BYTE MEDIA CODE                   
*                                                                               
VSTA50   DS    0H                                                               
         GOTO1 VGETMED                                                          
         JNE   NEQXIT                                                           
         MVC   4(1,R5),EZMTBAND-EZMEDTBD(RF) SAVE BAND                          
         B     VSTA100                                                          
*                                                                               
* REACHED SPACE CHARACTER HERE                                                  
*                                                                               
VSTA60   DS    0H                                                               
         CHI   R0,4                ANY INPUT                                    
         JNL   NEQXIT                                                           
*                                                                               
VSTA70   DS    0H                                                               
         LLC   R1,5(R4)            LENGTH OF INPUT                              
         LA    R1,8(R1,R4)         END OF INPUT                                 
         CR    R1,R2               COMPARE TO CURRENT POSITION                  
         JNE   NEQXIT                                                           
*                                                                               
VSTA100  DS    0H                                                               
         OC    0(5,R5),SPACES                                                   
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
* CHECK WHETHER TRANSFER LEGAL BETWEEN POWER CODES                              
* AGENCY - OLD POWER CODE                                                       
* NEWAGY - NEW POWER CODE                                                       
* EQUAL CONDITION IF OK, UNEQUAL OTHERWISE                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
CKTRAN   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,PCTABLE          POWERCODE MASTER TABLE                       
*                                                                               
CKTR10   DS    0H                                                               
         ZIC   R0,0(R2)            INDIVIDUAL TABLE LENGTH                      
         LA    R1,1(R2)            A(INDIVIDUAL TABLE)                          
         BRAS  RE,CKTAB            GO SEE IF TRANSFER IS LEGAL                  
         BE    CKTREQX                                                          
*                                                                               
         MHI   R0,2                LENGTH OF TABLE IN BYTES                     
         AHI   R0,1                PLUS ONE LENGTH BYTE                         
         AR    R2,R0                                                            
         CLI   0(R2),X'FF'         END OF TABLE?                                
         BNE   CKTR10                                                           
*                                                                               
CKTRNEQX J     NEQXIT                                                           
*                                                                               
CKTREQX  J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
* TABLE OF POWERCODE TABLES                                                     
* TRANSFER BETWEEN POWERCODES                                                   
* IN EACH INDIVIDUAL TABLE (PCTABX) IS LEGAL                                    
*                                                                               
* MASTER TABLE CONSISTS OF INDIVIDUAL TABLES, AND ENDS WITH X'FF'               
* INDIVIDUAL TABLE:                                                             
* 1 BYTE - NUMBER OF ENTRIES, FOLLOWED BY STRING OF POWER CODES                 
PCTABLE  DS    0H                                                               
         DC    AL1(PCTAB1LQ)                                                    
PCTAB1   DC    C'DFDTLFTH'                                                      
PCTAB1LQ EQU   (*-PCTAB1)/2                                                     
*                                                                               
         DC    AL1(PCTAB2LQ)                                                    
PCTAB2   DC    C'BSBTTH'                                                        
PCTAB2LQ EQU   (*-PCTAB2)/2                                                     
*                                                                               
         DC    AL1(PCTAB3LQ)                                                    
PCTAB3   DC    C'JWFR'                                                          
PCTAB3LQ EQU   (*-PCTAB3)/2                                                     
*                                                                               
         DC    AL1(PCTAB4LQ)                                                    
PCTAB4   DC    C'BDDM'                                                          
PCTAB4LQ EQU   (*-PCTAB4)/2                                                     
*                                                                               
         DC    AL1(PCTAB5LQ)                                                    
PCTAB5   DC    C'MIOU'                                                          
PCTAB5LQ EQU   (*-PCTAB5)/2                                                     
*                                                                               
         DC    AL1(PCTAB6LQ)                                                    
PCTAB6   DC    C'WIWR'                                                          
PCTAB6LQ EQU   (*-PCTAB6)/2                                                     
*                                                                               
         DC    AL1(PCTAB7LQ)                                                    
PCTAB7   DC    C'BNDM'                                                          
PCTAB7LQ EQU   (*-PCTAB7)/2                                                     
*                                                                               
         DC    AL1(PCTAB8LQ)                                                    
PCTAB8   DC    C'MIDN'                                                          
PCTAB8LQ EQU   (*-PCTAB8)/2                                                     
*                                                                               
         DC    AL1(PCTAB9LQ)                                                    
PCTAB9   DC    C'THTB'                                                          
PCTAB9LQ EQU   (*-PCTAB9)/2                                                     
*                                                                               
         DC    AL1(PCTABALQ)                                                    
PCTABA   DC    C'BNOO'                                                          
PCTABALQ EQU   (*-PCTABA)/2                                                     
*                                                                               
         DC    AL1(PCTABBLQ)                                                    
PCTABB   DC    C'YRHY'                                                          
PCTABBLQ EQU   (*-PCTABB)/2                                                     
*                                                                               
PCTABFF  DC    X'FFFF'                                                          
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
* SEE IF CROSS AGENCY MOVE IS LEGAL                                             
* R0 EXPECTED TO ADDRESS TABLE LENGTH                                           
* R1 EXPECTED TO ADDRESS TABLE ITSELF                                           
* EQUAL CONDITION IF OK, UNEQUAL OTHERWISE                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
CKTAB    NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R0                                                            
         LR    RF,R1                                                            
*                                                                               
CKTAB20  CLC   AGENCY,0(R1)                                                     
         BE    CKTAB40                                                          
         LA    R1,2(,R1)                                                        
         BCT   R0,CKTAB20                                                       
         B     CKTABNQX                                                         
*                                                                               
CKTAB40  CLC   NEWAGY,0(RF)                                                     
         BE    CKTABEQX                                                         
         LA    RF,2(,RF)                                                        
         BCT   RE,CKTAB40                                                       
*                                                                               
CKTABNQX J     NEQXIT                                                           
*                                                                               
CKTABEQX J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
ISDIGITL NTR1  BASE=*,LABEL=*                                                   
         LA    R1,QSTA+4           STATION'S BAND                               
         ICM   R1,8,=AL1(EZMTBNDQ)                                              
         GOTO1 VGETMED                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   BYTE,C'N'                                                        
         TM    EZMTFLAG-EZMEDTBD(RF),EZMTFDGQ IS THIS BAND DIGITAL?             
         JZ    EQXIT                                                            
         MVI   BYTE,C'Y'                                                        
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
CKSTA    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   CURSYST,C'C'        SWITCH TO CONTROL SYTEM                      
         GOTO1 VALIFAS             SWITCH                                       
         MVC   SYSDIR(3),=C'GEN'                                                
         MVC   SYSFIL(3),=C'GEN'                                                
*                                                                               
         XC    KEY,KEY             SET UP KEY/SVKEY                             
         LA    R4,KEY                                                           
         USING EZSTAD,R4                                                        
         MVC   EZSKEY,SPACES                                                    
         MVC   EZSKTYP,=C'ZS'                                                   
         MVC   EZSKAGY,AGENCY                                                   
*                                                                               
         GOTO1 HIGH                                                             
         B     CKSTA20                                                          
*                                                                               
CKSTA10  GOTO1 SEQ                                                              
*                                                                               
CKSTA20  CLC   KEY(4),KEYSAVE       KEY ID                                      
         JNE   EQXIT                                                            
*                                                                               
         CLC   EZSKSTA,KEYSAVE+EZSKSTA-EZSKEY                                   
         JE    NEQXIT                                                           
*                                                                               
         B     CKSTA10                                                          
         DROP  R4                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,30,C'STATION EQUIVALENCY RECORDS'                             
         SSPEC H2,30,C'---------------------------'                             
         SSPEC H1,65,AGYNAME                                                    
         SSPEC H2,65,AGYADD                                                     
         SSPEC H3,65,REPORT                                                     
         SSPEC H4,65,RUN                                                        
         SSPEC H5,65,PAGE                                                       
         SSPEC H8,10,C'STATION'                                                 
         SSPEC H9,10,C'-------'                                                 
         SSPEC H8,30,C'EQUIVALENT STATION'                                      
         SSPEC H9,30,C'------------------'                                      
         SSPEC H8,50,C'ADVERTISER NAME'                                         
         SSPEC H9,50,C'---------------'                                         
         SSPEC H8,80,C'USER ID'                                                 
         SSPEC H9,80,C'-------'                                                 
         DC    X'00'                                                            
*                                                                               
*                                                                               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* CTGENFILE                                                                     
* SPEZFFFD                                                                      
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPEZFFFD                                                       
*                                                                               
* SPEZFF5D                                                                      
         ORG   CONTAGH                                                          
       ++INCLUDE SPEZFF5D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
* SPEZFE5D                                                                      
       ++INCLUDE SPEZFE5D                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
* SPGENEZ                                                                       
       ++INCLUDE SPGENEZ                                                        
         EJECT                                                                  
*SPEZFWORKD                                                                     
*                                                                               
       ++INCLUDE SPEZFWORKD                                                     
       ++INCLUDE SPEZDSCTS                                                      
*                                                                               
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
ERRFLD   DS    CL1                 ERROR FIELD                                  
CUFTRS   DS    0CL13                                                            
CUUID    DS    CL8                 USER ID FILTER                               
CUSTA    DS    CL5                 STATION FILTER                               
*                                                                               
OSTA     DS    CL5                 ORIGIN STATION TO BE EQUIVALENCED            
SVSIGN   DS    CL8                 SIGNON ID FOR THIS TERMINAL                  
SVORIG   DS    XL2                 LOAD TO THIS SIGNON ID                       
NEWAGY   DS    CL2                 LOAD TO THIS AGENCY                          
OSTADIG  DS    C                   ORIGINAL STATION "DIGITAL" FLAG Y/N          
       ++INCLUDE EZLKAGYBLK                                                     
*                                                                               
*                                                                               
*                                                                               
WRKFEND  EQU   *                                                                
*                                                                               
*                                                                               
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL1                                                              
LSTA     DS    CL7                                                              
         DS    CL10                                                             
LESTA    DS    CL7                                                              
         DS    CL9                                                              
LADV     DS    CL25                                                             
         DS    CL6                                                              
LUID     DS    CL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047SPEZF12   01/23/17'                                      
         END                                                                    
