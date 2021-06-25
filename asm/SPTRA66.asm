*          DATA SET SPTRA66    AT LEVEL 016 AS OF 05/01/06                      
*PHASE T21666C                                                                  
*INCLUDE GETBROAD                                                               
*INCLUDE XSORT                                                                  
         TITLE 'T21666 (SPTRA66) - MAINTAIN A LIST OF NETWORK EQUIVALENX        
               T PROGRAMS'                                                      
***********************************************************************         
*                                                                     *         
* TO DO - FILTER OPTION BASE FOR LIST IN ORDER BY BASE                *         
*            PASSIVE KEYS - ALLOW FOR MULTI EQ BASE ELEMS             *         
*        SCHEME FOR LISTING BY BASE                                   *         
*                                                                     *         
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 -                                                  *         
*                                                                     *         
*             AIO2 - VR SAVED RECORD                                  *         
*                    LIST RTN, FCLT CLT REC                           *         
*                                                                     *         
*             AIO3 - END OF VR RTN -GETREC(POSITION FOR GENCON PUTREC)*         
*                                                                     *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - WORK REG                                                *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG                                                *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - SECOND BASE REG                                         *         
*        R8 - POINTER TO SPOOLD                                       *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO ATWA                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*  LEV 05    FEB05/91 CHANGE ERROR MSGS                               *         
*  LEV 06    DEC22/92 FIX FOR STATION FILE                            *         
*  LEV 07    MAR01/95 FIX BAD MISSING PROGRAM ERROR MESSAGE           *         
*  LEV 08    OCT12/95 CHK FOR CLIENT ALFA/NUMERIC PROFILE             *         
*  LEV 09    APR30/96 EQUIVALENCY RECS ACROSS ALL CLIENTS             *         
*  LEV 10    MAR19/98 IF ACCROSS ALL CLIENTS, BYPASS FCLT ROUTINE     *         
*  LEV 11 SM APR02/01 USE TRAFFIC OFFICE                              *         
*  LEV 12 SMUR JUN07/02 OFFICE AND OFFICE LIST SECURITY               *         
*  LEV 13 SMUR JUL09/02 CLIENT STRING SECURITY                        *         
*  LEV 14 SMUR NOV19/03 BRAND LEVEL SECURITY                          *         
*  LEV 15 SMUR JUL19/04 SOX                                           *         
*  LEV 16 SMUR APR24/06 MODIFY FOR MORE BRANDS PROJECT                *         
*                                                                     *         
***********************************************************************         
T21666   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**EPRG**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,SPTR66RR                                                      
         EJECT                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,XRECADD        AFTER ADD RECORD                             
         BE    AAR                                                              
         CLI   MODE,XRECPUT        AFTER PUT RECORD                             
         BE    APUT                                                             
         CLI   MODE,XRECDEL        AFTER DELETE RECORD                          
         BE    ADEL                                                             
         CLI   MODE,XRECREST       AFTER RESTORE RECORD                         
         BE    AREST                                                            
         CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BE    EXIT                                                             
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BE    DELREC                                                           
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VK:  VALIDATE KEY ROUTINE                                           *         
***********************************************************************         
         SPACE                                                                  
VK       CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BE    *+12                                                             
         CLI   ACTNUM,ACTDEL       ACTION DEL NG                                
         BE    DELREC                                                           
         SPACE                                                                  
         BAS   RE,INITSPOT                                                      
         SPACE                                                                  
         LA    R2,ELEM             FAKE VALIDATE MEDIA                          
         MVC   ELEM,=X'0A01000184010001'                                        
         MVI   ELEM+8,C'N'                                                      
         GOTO1 VALIMED                                                          
         SPACE                                                                  
* VALIDATE CLIENT                                                               
         SPACE                                                                  
         LA    R2,TRACLTH          PT R2 @CLIENT HEADER                         
         XC    BCLT,BCLT                                                        
         XC    SVBCLT,SVBCLT                                                    
         CLI   5(R2),0             WAS THERE INPUT                              
         BNE   VK10                                                             
         SPACE                                                                  
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK20                 NO                                          
         SPACE                                                                  
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
         SPACE                                                                  
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
         SPACE                                                                  
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
         SPACE                                                                  
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
         SPACE                                                                  
         MVI   ERROR,0                                                          
         B     VK20                NO, GO VALIDATE NETWORK                      
         SPACE                                                                  
VK10     DS    0H                                                               
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT             VALIDATE CLIENT                              
         MVI   ERROPT,0                                                         
         SPACE                                                                  
         CLI   ERROR,0                                                          
         BE    VK15                                                             
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    TRAPERR              NO                                          
         CLI   ERROR,SECLOCK       ONLY VALID ERROR SEC-LOCKOUT                 
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BO    TRAPERR                                                          
         SPACE                                                                  
VK15     MVC   SVBCLT,BCLT                                                      
         SPACE                                                                  
VK20     BAS   RE,GTPROF           GET TN PROFILE                               
         SPACE                                                                  
* VALIDATE NETWORK                                                              
         SPACE                                                                  
         LA    R2,TRANETH          PT R2 @NETWORK HEADER                        
         SPACE                                                                  
         BAS   RE,VNT                                                           
         SPACE                                                                  
* VALIDATE EQUIVALENT PROGRAM                                                   
         SPACE                                                                  
         LA    R2,TRAPROGH         PT R2 @PROGRAM HEADER                        
         SPACE                                                                  
         BAS   RE,VEPR             VALIDATE EQUIVALENT PROGRAM                  
         SPACE                                                                  
* VALIDATE OPTIONS - NO VALID OPTIONS                                           
         SPACE                                                                  
VK40     LA    R2,TRAOPTH          PT R2 @OPTIONS HEADER                        
         BAS   RE,VFTR                                                          
         EJECT                                                                  
* BUILD KEY                                                                     
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PGEKEY,R4                                                        
         MVI   PGEKID,X'24'                                                     
         MVC   PGEKAM,BAGYMD       XXXX|0011                                    
         MVC   PGEKCLT,BCLT                                                     
         MVC   PGEKNET,NETWORK                                                  
         MVC   PGEKPRG,EQUIVPGM                                                 
         SPACE                                                                  
         BAS   RE,INITNET                                                       
         SPACE                                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VR:  VALIDATE RECORD ROUTINE                                        *         
***********************************************************************         
         SPACE                                                                  
VR       DS    0H                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VRS01                                                            
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VRS01    DS    0H                                                               
         LA    R2,TRAPRGAH         START AT FIRST BASE PRG                      
         L     R6,AIO1                                                          
         MVC   SVUKEY,0(R6)                                                     
         XC    BASELIST,BASELIST   CLEAR LIST OF BASE PROGS                     
         SR    R5,R5               COUNTER FOR # OF PROGS                       
         SPACE                                                                  
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VR01                                                             
         SPACE                                                                  
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BZ    VR01                                                             
         SPACE                                                                  
         LA    R2,TRACLTH          CLIENT                                       
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     TRAPERR             CAN'T CHANGE THIS RECORD                     
         SPACE                                                                  
VR01     CLI   ACTNUM,ACTADD                                                    
         BE    VR10                                                             
         SPACE                                                                  
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PGEDTAEL,R6                                                      
VR02     LA    R3,BASELIST                                                      
         LA    R4,L'BASELIST/7                                                  
         SPACE                                                                  
VR04     OC    0(7,R3),0(R3)       EMPTY ENTRY                                  
         BZ    VR06                                                             
         CLC   PGEPROG,0(R3)       ALREADY IN LIST                              
         BE    VR08                                                             
         LA    R3,7(,R3)                                                        
         BCT   R4,VR04                                                          
         DC    H'0'                                                             
VR06     MVC   0(6,R3),PGEPROG                                                  
         SPACE                                                                  
VR08     BAS   RE,NEXTEL                                                        
         BE    VR02                                                             
         SPACE                                                                  
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
         SPACE                                                                  
VR10     TM    1(R2),X'20'         PROTECTED FIELD?                             
         BO    VR16                YES? CONTINUE                                
         SPACE                                                                  
         CLI   5(R2),0             WAS THERE INPUT?                             
         BNE   VR20                YES? CONTINUE                                
         ZIC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               BUMP TO DATES FIELDH                         
         CLI   5(R2),0             WAS THERE INPT IN EFF DATE?                  
         BE    VR40                IF NOT, NEXT FIELD                           
         SR    R2,R0               SET ERR TO PT TO CORRECT FIELD               
         B     MISPRGER                                                         
         SPACE                                                                  
VR16     MVC   5(1,R2),7(R2)                                                    
         ZIC   RF,0(R2)            GET FIELD LENGTH                             
         AR    RF,R2               BUMP TO DATES FIELDH                         
         MVC   5(1,RF),7(RF)                                                    
         EJECT                                                                  
* CHECK FOR VALID EFFECTIVE DATES FIRST, TO USE TO CK 0D20 PROG REC             
*                                                                               
VR20     ZIC   R0,0(R2)            GET PROG FIELD LENGTH                        
         AR    R2,R0               BUMP TO DATES HEADER                         
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         ICM   R4,15,DMCB                                                       
         BZ    BADATE                                                           
         LA    R4,9(R4,R2)                                                      
         CLC   =C'UFN',0(R4)                                                    
         BNE   *+14                                                             
         MVC   BENDATEB,=X'FFFFFF'                                              
         B     VR24                                                             
         SPACE                                                                  
         XC    BENDATEB,BENDATEB                                                
         GOTO1 (RF),(R1),(0,(R4)),WORK+6                                        
         ICM   R4,15,DMCB                                                       
         BZ    BADATE                                                           
         SPACE                                                                  
         CLC   WORK(6),WORK+6                                                   
         BH    DATSQERR                                                         
         EJECT                                                                  
* CONTINUE BUILDING ELEMENT                                                     
*                                                                               
VR24     GOTO1 DATCON,DMCB,(0,WORK),(3,BSTDATEB)                                
         GOTO1 (RF),(R1),(0,WORK),(2,BENDATEP)                                  
         CLC   BENDATEB,=X'FFFFFF'                                              
         BE    VR26                                                             
         GOTO1 (RF),(R1),(0,WORK+6),(3,BENDATEB)                                
         GOTO1 (RF),(R1),(0,WORK),(2,BENDATEP)                                  
         SPACE                                                                  
VR26     SR    R2,R0               MOVE BACK TO BASE PROGRAM FIELD              
         BAS   RE,VBPR                                                          
         SPACE                                                                  
         LA    R6,ELEM                                                          
         USING PGEDTAEL,R6                                                      
         MVI   PGEDTAEL,X'10'      ALL 10 ELEMENTS                              
         MVI   PGEDTALN,PGEDTAX-PGEDTAEL                                        
         MVC   PGESTR,BSTDATEB                                                  
         MVC   PGEEND,BENDATEB                                                  
         MVC   PGEPROG,BASEPGM                                                  
         SPACE                                                                  
         L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         SPACE                                                                  
         BAS   RE,GETEL                                                         
         BNE   VR36                                                             
         SPACE                                                                  
VR30     CLC   PGESTR,ELEM+PGEEND-PGEDTAEL                                      
         BH    VR34                                                             
         CLC   PGEEND,ELEM+PGESTR-PGEDTAEL                                      
         BNL   DATOVLER                                                         
         SPACE                                                                  
VR34     BAS   RE,NEXTEL                                                        
         BE    VR30                                                             
         SPACE                                                                  
VR36     MVC   SVPGESTR,PGESTR     SAVE EFFECTIVE START                         
         MVC   SVPGEEND,PGEEND      AND END DATES                               
         SPACE                                                                  
         LA    R6,ELEM                                                          
         SPACE                                                                  
         CLI   1(RA),C'*'          SEE IF DDS TERM                              
         BE    VR38                 IF SO BRANCH ELSE                           
         TM    1(R2),X'20'         IF PROTECTED FIELD                           
         BO    VR38                 JUST ADD ELEM                               
         BAS   RE,CKEREV           CK ANY EQUIV REV RECS FOR THIS DATE          
         BNE   REVRECER                                                         
         SPACE                                                                  
VR38     MVC   AIO,AIO1                                                         
         GOTO1 ADDELEM                                                          
         LA    R5,1(,R5)           INC COUNTER                                  
         SPACE                                                                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               NOW TO DATE FLD                              
VR40     ZIC   R0,0(R2)                                                         
         AR    R2,R0               NOW TO IDENTIFIER                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               NOW TO NEXT BASE PROG                        
         SPACE                                                                  
         LA    R0,TRATAGH          INITIAL STARTING ADDR                        
         CR    R2,R0               HAVE WE CHECKED ALL FIELDS                   
         BL    VR10                IF NOT, CONTINUE                             
         CH    R5,=H'0'            WERE THERE ANY ELEMS?                        
         BE    MSPRGER                                                          
         MVC   KEY(L'SVUKEY),SVUKEY                                             
         SPACE                                                                  
         CLI   ACTNUM,ACTADD                                                    
         BE    EXIT                                                             
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DK:  DISPLAY KEY ROUTINE                                            *         
***********************************************************************         
         SPACE                                                                  
DK       L     R4,AIO1                                                          
         USING PGERECD,R4                                                       
         CLC   KEY(20),0(R4)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         CLI   BCLT,0              WAS CLIENT ENTERED?                          
         BE    DK10                 NO                                          
         CLC   PGEKCLT,SVBCLT                                                   
         BE    DK10                                                             
         SPACE                                                                  
         MVC   SVBCLT,PGEKCLT                                                   
         BAS   RE,FCLT                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
DK10     GOTO1 CLUNPK,DMCB,PGEKCLT,TRACLT                                       
         OI    TRACLTH+6,X'80'     TRANSMIT FIELD                               
         CLC   TRANET(4),PGEKNET   SAVE RE-TRANSMIT OVERHEAD IF THERE           
         BE    DK20                                                             
         MVC   TRANET(4),PGEKNET                                                
         OI    TRANETH+6,X'80'                                                  
DK20     MVC   EQUIVPGM,PGEKPRG                                                 
         CLC   TRAPROG,PGEKPRG                                                  
         BE    DKX                                                              
         OI    TRAPROGH+6,X'80'                                                 
         MVC   TRAPROG,PGEKPRG                                                  
DKX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DR:  DISPLAY RECORD ROUTINE                                         *         
***********************************************************************         
         SPACE                                                                  
DR       BAS   RE,INITNET       INIT TO UNIT FILE                               
         MVC   SVUKEY,KEY                                                       
         SR    R3,R3               COUNTER OF ELEMS                             
         LA    R2,TRAPRGAH         1ST FIELD                                    
         L     R6,AIO1                                                          
         USING PGERECD,R6                                                       
         MVI   ELCODE,X'10'        ALL ELEM CODES 10                            
         BAS   RE,GETEL                                                         
         BE    *+6                 BR IF ELEM FOUND                             
         DC    H'0'                DIE BECAUSE NO ELEMS                         
         USING PGEDTAEL,R6                                                      
         SPACE                                                                  
DR10     MVC   BASEPGM,PGEPROG                                                  
         SPACE                                                                  
         MVC   SVPGESTR,PGESTR     SAVE EFFECTIVE START                         
         MVC   SVPGEEND,PGEEND      AND END DATES                               
         SPACE                                                                  
         BAS   RE,CKBREV           GO SEE IF REV REC FOR THIS                   
         BE    DR14                 NO REV REC                                  
         SPACE                                                                  
         CLI   1(RA),C'*'          SEE IF DDS TERM                              
         BE    DR14                 IF SO BRANCH ELSE                           
         TM    1(R2),X'20'         PROTECTED FIELD                              
         BO    *+16                                                             
         OI    1(R2),X'20'         SET ON PROTECT BIT                           
         NI    1(R2),X'FF'-X'01'   SET OFF MODIFY                               
         OI    6(R2),X'80'         TRANS                                        
         ZIC   RF,0(R2)                                                         
         AR    RF,R2                                                            
         TM    1(RF),X'20'         SET ON PROTECT BIT                           
         BO    *+16                                                             
         OI    1(RF),X'20'         SET ON PROTECT BIT                           
         NI    1(RF),X'FF'-X'01'   SET OFF MODIFY                               
         OI    6(RF),X'80'         TRANS                                        
         B     DR16                CONTINUE                                     
         SPACE                                                                  
DR14     NI    1(R2),X'FF'-X'20'   SET OFF PROTECT BIT                          
         ZIC   RF,0(R2)                                                         
         AR    RF,R2                                                            
         NI    1(RF),X'FF'-X'20'                                                
         SPACE                                                                  
DR16     LA    R3,1(,R3)           INC COUNTER                                  
         CH    R3,=H'10'           MORE THAN 10 ELEMS?                          
         BNH   *+6                 NO? CONTINUE                                 
         DC    H'0'                DIE BECAUSE MORE THAN 10 ELEMS               
         XC    8(L'TRAPRGA,R2),8(R2)                                            
         MVC   8(6,R2),BASEPGM     MV ELEM TO TWA                               
         OI    6(R2),X'80'         FORCE TRANSMIT ELEM                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         SPACE                                                                  
         XC    8(L'TRADTSA,R2),8(R2)                                            
         OI    6(R2),X'80'         FORCE TRANSMIT ELEM                          
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,SVPGESTR),(5,8(R2))                               
         MVI   16(R2),C'-'                                                      
         CLC   SVPGEEND,=X'FFFFFF'   THIS UFN                                   
         BNE   *+14                                                             
         MVC   17(3,R2),=C'UFN'                                                 
         B     DR18                                                             
         SPACE                                                                  
         GOTO1 (RF),(R1),(3,SVPGEEND),(5,17(R2))                                
         SPACE                                                                  
DR18     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BAS   RE,NEXTEL                                                        
         BE    DR10                DISPLAY THIS ELEM                            
         SPACE                                                                  
DR20     CH    R3,=H'10'           MORE THAN 10 ELEMS?                          
         BNL   DRX                                                              
         XC    8(L'TRAPRGA,R2),8(R2)                                            
         OI    6(R2),X'80'         FORCE TRANSMIT ELEM                          
         NI    1(R2),X'FF'-X'20'   SET OFF PROTECT BIT                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(L'TRADTSA,R2),8(R2)                                            
         OI    6(R2),X'80'         FORCE TRANSMIT ELEM                          
         NI    1(R2),X'FF'-X'20'   SET OFF PROTECT BIT                          
         SPACE                                                                  
         LA    R3,1(,R3)           INC COUNTER                                  
         SPACE                                                                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            JUMP PAST ID FIELD                           
         AR    R2,R0                                                            
         B     DR20                                                             
         SPACE                                                                  
DRX      MVC   KEY(L'SVUKEY),SVUKEY                                             
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LR:  LIST RECORDS (ONLINE & OFFLINE START SAME)                     *         
***********************************************************************         
         SPACE                                                                  
LR       LA    R4,KEY                                                           
         USING PGERECD,R4                                                       
         OC    KEY,KEY             HAVE WE BEEN HERE BEFORE?                    
         BNZ   LR30                 YES? DON'T REBUILD KEY                      
*                                   OR READ REC (GENCON DID ALREADY)            
         LA    R1,HEADINGA                                                      
         SR    R3,R3                                                            
         CLI   BASLSTSW,1          IS THIS BASE PROG LIST                       
         BNE   LR10                                                             
         LA    R3,BASEWORK                                                      
         LR    RE,R3                                                            
         LH    RF,=AL2(L'BASEWORK)                                              
         XCEFL                                                                  
         LA    R1,HEADINGB                                                      
         SPACE                                                                  
LR10     ST    R1,SPECS                                                         
         SPACE                                                                  
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   PGEKID,X'24'                                                     
         MVC   PGEKAM,BAGYMD                                                    
         MVC   PGEKCLT,BCLT        CLIENT, NETWORK, & PROGRAM MAY OR            
         MVC   PGEKNET,NETWORK     MAY NOT BE PRESENT                           
         MVC   PGEKPRG,EQUIVPGM                                                 
         GOTO1 HIGH                                                             
         B     LR30                SKIP SEQ                                     
         SPACE                                                                  
LR20     GOTO1 SEQ                                                              
         SPACE                                                                  
LR30     CLI   KEY,X'24'           IS THIS THE SAME RECORD ID?                  
         BNE   LRX                  NO? EXIT                                    
         SPACE                                                                  
         CLC   BAGYMD,PGEKAM       IS IT SAME AGENCY/MEDIA?                     
         BNE   LRX                                                              
         SPACE                                                                  
         OC    PGEKCLT,PGEKCLT     CLIENT SPECIFIC RECORD ?                     
         BZ    LR35                 NO                                          
         SPACE                                                                  
         MVC   SVBCLT,PGEKCLT                                                   
         BAS   RE,FCLT                                                          
         BNE   EXIT                                                             
         SPACE                                                                  
LR35     CLI   BCLT,0              WAS A CLIENT INPUT?                          
         BE    LR40                NO? CK NETWORK & PROGRAM                     
         CLC   BCLT,PGEKCLT        IS THIS SAME CLIENT?                         
         BNE   LRX                                                              
         SPACE                                                                  
LR40     OC    NETWORK,NETWORK     WAS A NETWORK INPUT?                         
         BZ    LR50                NO? CHECK PROGRAM                            
         CLC   PGEKNET,NETWORK     IS THIS SAME NETWORK?                        
         BE    LR50                YES? CK PRG                                  
         SPACE                                                                  
         CLI   BCLT,0              NO? WAS THERE A CLIENT?                      
         BE    LR20                NO? GET NEXT REC                             
         B     LRX                                                              
         SPACE                                                                  
LR50     OC    EQUIVPGM,EQUIVPGM   WAS A PROGRAM INPUT?                         
         BZ    LR60                                                             
         CLC   PGEKPRG,EQUIVPGM    IS THIS SAME PROGRAM?                        
         BE    LR60                                                             
         CLI   BCLT,0              DIFF PRG, WAS CLT ENTERED?                   
         BE    LR20                NO? GET NXT REC                              
         OC    NETWORK,NETWORK     NOT SAME CLT OR PRG, NET NTRED?              
         BZ    LR20                NO? THEN GET NEXT REC                        
         B     LRX                 YES? THEN EXIT                               
         SPACE                                                                  
LR60     DS    0H                                                               
         CLI   BCLT,0              WAS CLIENT ENTERED                           
         BNE   LR65                 YES                                         
         XC    CLTNM,CLTNM                                                      
         XC    SVBCLT,SVBCLT                                                    
         XC    QCLT,QCLT                                                        
         B     LR70                                                             
         SPACE                                                                  
LR65     CLC   SVBCLT,PGEKCLT                                                   
         BE    LR70                                                             
         MVI   FORCEHED,C'Y'       NO? NEW PAGE                                 
         DROP  R4                                                               
         SPACE                                                                  
LR70     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING PGERECD,R6                                                       
         GOTO1 GETREC                                                           
         CLI   MODE,LISTRECS                                                    
         BE    LRL                                                              
         CLI   MODE,PRINTREP                                                    
         BE    LRP                                                              
         DC    H'0'                DIE, NOT HERE FOR LSTREC/PRNTREP             
         SPACE                                                                  
LRX      CLI   BASLSTSW,0                                                       
         BE    EXIT                                                             
         B     LRB                 GO LIST BY BASE                              
         EJECT                                                                  
* DO ONLINE LIST                                                                
*                                                                               
LRL      MVC   LISTAR,SPACES                                                    
         GOTO1 CLUNPK,DMCB,PGEKCLT,LCLT                                         
         MVC   LNET,PGEKNET                                                     
         MVC   LPRG,PGEKPRG                                                     
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            GET ELEMENT                                  
         BE    *+6                                                              
         DC    H'0'                DIE, NO ELEMS                                
         USING PGEDTAEL,R6                                                      
         LA    R3,LBASE            ADDR OF OUTPUT LN                            
         SPACE                                                                  
LRL10    LA    R2,LBASE+L'LBASE-1  SEE IF OUTPUT LENGTH TOO LONG...             
         SPACE                                                                  
         CR    R3,R2                                                            
         BNL   LRL16               IF SO, ADD 'MORE' ON END                     
         MVC   0(6,R3),PGEPROG     PUT PROG NAME OUT                            
         LA    R3,5(,R3)                                                        
LRL12    CLI   0(R3),C' '          CLEAR OUT WHITE SPACE                        
         BH    *+8                 PAST WHITE SPACE?                            
         BCT   R3,LRL12            NO? DEC R3                                   
         LA    R3,1(,R3)                                                        
         SPACE                                                                  
         MVI   0(R3),C'('                                                       
         GOTO1 DATCON,DMCB,(3,PGESTR),(5,1(R3))                                 
         MVI   9(R3),C'-'                                                       
         CLC   PGEEND,=X'FFFFFF'   THIS UFN                                     
         BNE   *+22                                                             
         MVC   10(3,R3),=C'UFN'                                                 
         MVI   13(R3),C')'                                                      
         LA    R3,14(,R3)                                                       
         B     LRL14                                                            
         SPACE                                                                  
         GOTO1 (RF),(R1),(3,PGEEND),(5,10(R3))                                  
         MVI   18(R3),C')'                                                      
         LA    R3,19(,R3)                                                       
         SPACE                                                                  
LRL14    BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   LRLX                NO MORE? EXIT                                
         MVI   1(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         B     LRL10               ELSE START AGAIN                             
         SPACE                                                                  
LRL16    MVC   0(4,R3),=C'MORE'                                                 
         SPACE                                                                  
LRLX     GOTO1 LISTMON             SET UP OUTPUT                                
         B     LR20                GET NEXT RECORD                              
         DROP  R6                                                               
         EJECT                                                                  
* DO OFFLINE REPORT IN EQUIVALENT PROGRAM ORDER                                 
*                                                                               
         USING PGERECD,R4                                                       
         USING PGEDTAEL,R6                                                      
         SPACE                                                                  
LRP      MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE, NO ELEMS                                
         SPACE                                                                  
         CLI   BASLSTSW,1          IS THIS BASE PROG LIST                       
         BNE   LRP10                                                            
         USING BASLSTD,R3                                                       
LRP00    MVC   BASBCLT,PGEKCLT                                                  
         MVC   BASNET,PGEKNET                                                   
         MVC   BASBPRG,PGEPROG                                                  
         MVC   BASEPRG,PGEKPRG                                                  
         MVC   BASSDT,PGESTR                                                    
         MVC   BASEDT,PGEEND                                                    
         LA    R3,BASNEXT                                                       
         BAS   RE,NEXTEL                                                        
         BE    LRP00                                                            
         B     LR20                                                             
         DROP  R3                                                               
         SPACE                                                                  
LRP10    MVC   PNET,PGEKNET                                                     
         MVC   PPRG,PGEKPRG                                                     
         SPACE                                                                  
LRP20    MVC   PBASE,PGEPROG                                                    
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,PGESTR),(5,PBASESTR)                              
         SPACE                                                                  
         MVI   PBASESTR+8,C'-'                                                  
         SPACE                                                                  
         CLC   PGEEND,=X'FFFFFF'   THIS UFN                                     
         BNE   *+14                                                             
         MVC   PBASEEND(3),=C'UFN'                                              
         B     LRP24                                                            
         SPACE                                                                  
         GOTO1 (RF),(R1),(3,PGEEND),(5,PBASEEND)                                
         SPACE                                                                  
LRP24    GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         BAS   RE,NEXTEL                                                        
         BE    LRP20                                                            
         B     LR20                                                             
         SPACE                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
* DO OFFLINE REPORT IN BASE PROGRAM ORDER                                       
*                                                                               
         SPACE                                                                  
LRB      SR    R2,R2               CTR                                          
         LA    R3,BASEWORK                                                      
         LR    RF,R3                                                            
         USING BASLSTD,R3                                                       
LRB10    OC    BASENT,BASENT       AT END                                       
         BZ    LRB14                YES                                         
         LA    R3,BASNEXT                                                       
         BCTR  R2,0                                                             
         B     LRB10                                                            
         SPACE                                                                  
LRB14    LPR   R2,R2                                                            
         BZ    EXIT                                                             
         LR    R3,RF                                                            
         GOTO1 =V(XSORT),DMCB,(R3),(R2),L'BASENT,L'BASENT,0,RR=SPTR66RR         
         SPACE                                                                  
LRB20    DS    0H                                                               
         CLI   BCLT,0              WAS CLIENT ENTERED                           
         BE    LRB24                NO                                          
         CLC   SVBCLT,BASBCLT                                                   
         BE    LRB24                                                            
         MVI   FORCEHED,C'Y'       NO? NEW PAGE                                 
         MVC   SVBCLT,BASBCLT                                                   
         BAS   RE,FCLT                                                          
         BNE   EXIT                                                             
         SPACE                                                                  
LRB24    MVC   PNET,BASNET                                                      
         MVC   PPRG,BASBPRG                                                     
         MVC   PBASE,BASEPRG                                                    
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,BASSDT),(5,PBASESTR)                              
         SPACE                                                                  
         MVI   PBASESTR+8,C'-'                                                  
         SPACE                                                                  
         CLC   BASEDT,=X'FFFFFF'   THIS UFN                                     
         BNE   *+14                                                             
         MVC   PBASEEND(3),=C'UFN'                                              
         B     LRB30                                                            
         SPACE                                                                  
         GOTO1 (RF),(R1),(3,BASEDT),(5,PBASEEND)                                
         SPACE                                                                  
LRB30    GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LA    R3,BASNEXT                                                       
         OC    BASENT,BASENT                                                    
         BNZ   LRB20                                                            
         B     EXIT                                                             
         SPACE                                                                  
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* AAR:  AFTER ADD RECORD ROUTINE                                      *         
***********************************************************************         
         SPACE                                                                  
AAR      LA    R4,KEY                                                           
         USING PGERECD,R4                                                       
         MVI   PGEKID,X'24'                                                     
         MVC   PGEKAM,BAGYMD                                                    
         MVC   PGEKCLT,SVBCLT                                                   
         MVC   PGEKNET,NETWORK                                                  
         MVC   PGEKPRG,EQUIVPGM                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(14),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PGEPID,X'A4'                                                     
         MVC   PGEPEPRG,EQUIVPGM                                                
         L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE, NO ELEMS                                
         USING PGEDTAEL,R6                                                      
AAR10    MVC   PGEPBPRG,PGEPROG                                                 
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'UNTDIR',KEY,KEY                        
         BAS   RE,NEXTEL                                                        
         BE    AAR10                                                            
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* APUT:  AFTER PUT ROUTINE                                            *         
***********************************************************************         
*                                                                               
* CK AIO1 ELEMS FOR MEMBERSHIP IN AIO2                                          
*                                                                               
APUT     LA    R4,KEY                                                           
         USING PGERECD,R4                                                       
         L     R6,AIO1             CURRENT RECORD                               
         MVC   KEY(20),0(R6)                                                    
         MVI   KEY,X'A4'           PASSIVE KEY                                  
         MVC   KEY+14(6),KEY+8     MOVE EQUIV PROG                              
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE, NO ELEMS                                
         SPACE                                                                  
         USING PGEDTAEL,R6                                                      
AP10     LA    R2,10               MAX ENTRIES                                  
         LA    R5,BASELIST         SAVED BASE PROGS                             
AP20     OC    0(6,R5),0(R5)       AT END OF LIST                               
         BZ    AP24                                                             
         CLC   PGEPROG,0(R5)                                                    
         BE    AP26                                                             
         LA    R5,7(,R5)                                                        
         BCT   R2,AP20                                                          
         DC    H'0'                                                             
*                                                                               
* BASE PROG NOT IN BASELIST, ADD PASS PTR TO DIR                                
*                                                                               
AP24     MVC   PGEPBPRG,PGEPROG                                                 
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'UNTDIR',KEY,KEY                        
         DROP  R4                                                               
         MVC   0(6,R5),PGEPROG                                                  
AP26     MVI   6(R5),C'Y'                                                       
         SPACE                                                                  
         BAS   RE,NEXTEL                                                        
         BE    AP10                                                             
*                                                                               
* SEE IF ANY BASE PROGS NOT USED ANY MORE                                       
*                                                                               
AP30     LA    R2,10               MAX ENTRIES                                  
         LA    R5,BASELIST         SAVED BASE PROGS                             
AP32     OC    0(6,R5),0(R5)       AT END OF LIST                               
         BZ    APX                                                              
         CLI   6(R5),C'Y'          THIS BASE ACTIVE                             
         BNE   AP40                                                             
         SPACE                                                                  
AP34     LA    R5,7(,R5)                                                        
         BCT   R2,AP32                                                          
APX      B     EXIT                                                             
         SPACE                                                                  
AP40     LA    R4,KEY                                                           
         USING PGERECD,R4                                                       
         MVC   PGEPBPRG,0(R5)                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'UNTDIR',KEY,KEY                       
         SPACE                                                                  
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+20,X'80'        MARK AS DELETED                              
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR',KEY,KEY                        
         B     AP34                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ADEL:  AFTER DELETE ROUTINE                                         *         
***********************************************************************         
         SPACE                                                                  
ADEL     LA    R4,KEY                                                           
         USING PGERECD,R4                                                       
         MVI   PGEPID,X'A4'                                                     
         MVC   PGEPAM,BAGYMD                                                    
         MVC   PGEPCLT,SVBCLT                                                   
         MVC   PGEPNET,NETWORK                                                  
         MVC   PGEPEPRG,EQUIVPGM                                                
         L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE, NO ELEMS                                
         USING PGEDTAEL,R6                                                      
AD10     MVC   PGEPEPRG,PGEPROG                                                 
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'UNTDIR',KEY,KEY                       
         OI    KEY+20,X'80'        MARK AS DELETED                              
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR',KEY,KEY                        
         BAS   RE,NEXTEL                                                        
         BE    AD10                                                             
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* AREST: AFTER RESTORE ROUTINE                                        *         
***********************************************************************         
         SPACE                                                                  
AREST    LA    R4,KEY                                                           
         USING PGERECD,R4                                                       
         MVI   PGEPID,X'A4'                                                     
         MVC   PGEPAM,BAGYMD                                                    
         MVC   PGEPCLT,SVBCLT                                                   
         MVC   PGEPNET,NETWORK                                                  
         MVC   PGEPBPRG,EQUIVPGM                                                
         L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE, NO ELEMS                                
         USING PGEDTAEL,R6                                                      
AR10     MVC   PGEPEPRG,PGEPROG                                                 
         OI    DMINBTS,X'08'       READ DELETED RECS                            
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'UNTDIR',KEY,KEY                       
         NI    DMINBTS,X'F7'       DON'T READ DELETED RECS                      
         NI    KEY+20,X'7F'        UN-DELETE                                    
         NI    DMOUTBTS,X'FF'-X'02' WRITE DELETED REC                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR',KEY,KEY                        
         OI    DMOUTBTS,X'02'      DON'T WRITE DELETED RECS                     
         BAS   RE,NEXTEL                                                        
         BE    AR10                                                             
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* VNT:  VALIDATE NETWORK                                              *         
***********************************************************************         
         SPACE                                                                  
         DS    0H                                                               
VNT      NTR1                                                                   
         XC    NETWORK,NETWORK                                                  
         XC    NETMKT,NETMKT                                                    
         SPACE                                                                  
         CLI   5(R2),0                                                          
         BNE   VNT10                                                            
         CLI   ACTNUM,ACTLIST                                                   
         BE    VNTX                                                             
         B     MISSERR                                                          
VNT10    GOTO1 ANY                                                              
         MVC   NETWORK,WORK                                                     
         BAS   RE,GNM              GO GET NET MARKET                            
         L     R4,AIO3                                                          
         USING STARECD,R4                                                       
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,NETMKT                                                        
VNTX     B     EXIT                                                             
         SPACE                                                                  
         DROP  R4                                                               
* GET NETWORK MARKET                                                            
         SPACE                                                                  
GNM      NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         L     R4,AIO3                                                          
         CLC   KEY(15),0(R4)                                                    
         BE    EXIT                                                             
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO3                     
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         B     NETERRA                                                          
         SPACE                                                                  
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VEPR  VALIDATE EQUIVALENT PROGRAM                                   *         
***********************************************************************         
         SPACE                                                                  
         DS    0H                                                               
VEPR     NTR1                                                                   
         SPACE                                                                  
         XC    EQUIVPGM,EQUIVPGM                                                
         CLI   5(R2),0                                                          
         BNE   VEPR10                                                           
         CLI   ACTNUM,ACTLIST                                                   
         BE    VEPRX                                                            
         B     MISSERR                                                          
         SPACE                                                                  
VEPR10   OC    NETWORK,NETWORK                                                  
         BZ    MISSNET                                                          
         SPACE                                                                  
         GOTO1 ANY                                                              
         MVC   EQUIVPGM,WORK                                                    
         SPACE                                                                  
* GET PROGRAM INFO                                                              
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,EQUIVPGM                                                
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   PROGERR                                                          
         SPACE                                                                  
         BAS   RE,INITNET                                                       
*                                                                               
* CHECK EQV IS NOT A BASE                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PGEKEY,R4                                                        
         MVI   PGEPID,X'A4'                                                     
         MVC   PGEPAM(3),BAGYMD & BCLT                                          
         MVC   PGEPNET,NETWORK                                                  
         MVC   PGEPBPRG,EQUIVPGM                                                
         GOTO1 HIGH                                                             
         CLC   KEY(14),KEYSAVE                                                  
         BE    EQVBASER                                                         
VEPRX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VPROG: VALIDATE PROGRAM FOR BASE PROGRAMS                           *         
***********************************************************************         
         SPACE                                                                  
         DS    0H                                                               
VBPR     NTR1                                                                   
         SPACE                                                                  
         GOTO1 ANY                                                              
         MVC   BASEPGM,WORK                                                     
         SPACE                                                                  
         BAS   RE,INITSPOT                                                      
         SPACE                                                                  
         MVC   WORK(4),SVUKEY+PGEKNET-PGEKEY                                    
         SPACE                                                                  
         BAS   RE,GNM              GO GET NET MARKET                            
         L     R4,AIO3                                                          
         USING STARECD,R4                                                       
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         SPACE                                                                  
* GET PROGRAM INFO                                                              
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         STCM  R0,3,NPGKNET                                                     
         MVC   NPGKPROG,BASEPGM                                                 
         MVC   NPGKEND,BENDATEP                                                 
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   PROGERR                                                          
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PGEKEY,R4                                                        
         MVC   KEY(8),SVUKEY                                                    
         MVC   PGEPBPRG,BASEPGM                                                 
         SPACE                                                                  
         BAS   RE,INITNET                                                       
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(14),KEYSAVE                                                  
         BE    BASEEQU                                                          
         CLC   EQUIVPGM,BASEPGM                                                 
         BE    SAMERR                                                           
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VFTR:  VALIDATE FILTER (VALID FILTERS THIS PROG = BASE)             *         
***********************************************************************         
         SPACE                                                                  
VFTR     NTR1                                                                   
         MVI   BASLSTSW,0                                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    EXIT                NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTR20              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR10                                                           
         LA    R1,4                                                             
         B     VFTR14                                                           
VFTR10   ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
VFTR14   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL5'HELP'                                               
         BE    VFTR20                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL5'BASE'                                               
         BNE   VFTR30                                                           
         TM    WHEN,X'78'          MUST BE NOW, SOON, OV, OR DDS                
         BZ    BASPRTER                                                         
         SPACE                                                                  
         MVI   BASLSTSW,1                                                       
         B     EXIT                                                             
VFTR20   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRHELP),FTRHELP                                       
         B     ERREXIT                                                          
VFTR30   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRMSG+L'FTRHELP),FTRMSG                               
         B     ERREXIT                                                          
         EJECT                                                                  
***********************************************************************         
* INITSPOT: RESET FILES TO SPOT                                       *         
***********************************************************************         
         SPACE                                                                  
INITSPOT MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVC   SYSDIR(2),=C'SP'                                                 
         MVC   SYSFIL(2),=C'SP'                                                 
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* INITNET: RESET FILES TO NET                                         *         
***********************************************************************         
         SPACE                                                                  
INITNET  DS    0H                                                               
         MVI   DATADISP+1,27       SET FROM SPOT TO NET                         
         MVI   LKEY+1,20                                                        
         MVC   SYSDIR(2),=C'UN'                                                 
         MVC   SYSFIL(2),=C'UN'                                                 
         BR    RE                                                               
         SPACE 3                                                                
***********************************************************************         
* HDHK: HEADHOOK ROUTINE FOR OFFLINE REPORTS                          *         
***********************************************************************         
         SPACE                                                                  
         DS    0H                                                               
HDHK     NTR1                                                                   
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
         CLC   QCLT,SPACES                                                      
         BH    *+14                                                             
         MVC   H4+10(3),=C'ALL'                                                 
         B     EXIT                                                             
         SPACE                                                                  
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
         B     EXIT                                                             
         EJECT                                                                  
* CK IF REVISION REC EXISTS FOR THIS BASE PROGRAM, RETURN EQ CC IF SO           
         DS    0H                                                               
CKBREV   NTR1                                                                   
         SPACE                                                                  
* SAVE CURRENT RECORD                                                           
         SPACE                                                                  
         L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,20(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         LA    R1,BASEPGM                                                       
         B     CKREV                                                            
         SPACE                                                                  
* CK IF REVISION REC EXISTS FOR THIS EQV PROGRAM, RETURN EQ CC IF SO            
         DS    0H                                                               
CKEREV   NTR1                                                                   
         SPACE                                                                  
* SAVE CURRENT RECORD                                                           
         SPACE                                                                  
         L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,20(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         LA    R1,EQUIVPGM                                                      
         SPACE                                                                  
CKREV    SR    R2,R2                                                            
         USING PGEDTAEL,R6                                                      
         SPACE                                                                  
         MVC   SVUKEY,KEY                                                       
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY           PLACE @ OF KEY IN R4                            
         USING REVKEY,R4        REVKEY DSECT BASED ON @ OF KEY                  
         MVI   REVKID,X'21'     REV RECORD ID                                   
         MVC   REVKAM,BAGYMD    XXXX|0011                                       
         SPACE                                                                  
         CLI   BCLT,0              WAS CLIENT ENTERED                           
         BE    CKREV04                                                          
         SPACE                                                                  
         MVC   REVKCLT,SVUKEY+PGEKCLT-PGEKEY                                    
         MVC   REVKNET,SVUKEY+PGEKNET-PGEKEY                                    
         MVC   REVKPRG,0(R1)                                                    
         B     CKREV09                                                          
         SPACE                                                                  
CKREV04  GOTO1 HIGH                GET REV REC FOR A/M                          
         CLC   KEY(2),KEYSAVE      SAME AGY/MED                                 
         BNE   CKREVX              DONE                                         
         SPACE                                                                  
         CLC   REVKNET,SVUKEY+PGEKNET-PGEKEY   NETWORK?                         
         BE    *+20                                                             
         MVC   REVKNET,SVUKEY+PGEKNET-PGEKEY                                    
         MVC   REVKPRG,0(R1)                                                    
         B     CKREV04C            GO GET THE RECORD                            
         CLC   REVKPRG,0(R1)       PROGRAM?                                     
         BE    CKREV06                                                          
         MVC   REVKPRG,0(R1)                                                    
         XC    REVKPER(6),REVKPER  CLEAR THE REST OF THE KEY                    
         SPACE                                                                  
CKREV04C GOTO1 HIGH                GET REV REC - A/M/CLT/NET/PROG               
         SPACE                                                                  
         CLC   KEY(2),KEYSAVE      SAME AGY/MED                                 
         BNE   CKREVX              DONE                                         
         CLC   KEY(14),KEYSAVE     SAME AGY/MED/CLT/NET/PROG                    
         BE    CKREV06             GO FIND THE CLIENT                           
         SPACE                                                                  
CKREV05  DS    0H                                                               
         MVI   REVKNET,X'FF'       GET NEXT CLIENT                              
         B     CKREV04                                                          
         SPACE                                                                  
CKREV06  DS    0H                                                               
         MVC   SVBCLT,REVKCLT                                                   
         MVC   KEY(L'SVUKEY),SVUKEY RESTORE KEY                                 
*                                    AND RECORD BEFORE FCLT                     
         SPACE                                                                  
         L     R0,AIO1             MOVE BACK THE ORIGINAL RECORD                
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,20(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         BAS   RE,FCLT                                                          
         BNE   CKREVX                                                           
         SPACE                                                                  
CKREV09  GOTO1 DATCON,DMCB,(3,SVPGESTR),(0,STDATE) REFORMAT DATE                
         GOTO1 (RF),(R1),(3,SVPGEEND),(0,ENDATE)                                
         SPACE                                                                  
         CLI   SVTNPR2,C'C'                                                     
         BE    CKREV20                                                          
         SPACE                                                                  
         CLI   SVTNPR2,C'B'                                                     
         BE    CKREV10                                                          
         SPACE                                                                  
         CLI   SVTNPR2,C'W'                                                     
         BE    CKREV10                                                          
         SPACE                                                                  
         CLI   N0PROF+3,C'B'                                                    
         BNE   CKREV20                                                          
         SPACE                                                                  
CKREV10  DS   0H                                                                
         GOTO1 =V(GETBROAD),(R1),(1,STDATE),WORK,GETDAY,ADDAY,         C        
               RR=SPTR66RR                                                      
         SPACE                                                                  
         CLC   WORK(6),STDATE      IF BROADCAST SAME AS START                   
         BE    CKREV14              CONTINUE                                    
         BL    *+6                 OTHERWISE BETTER BE LESS                     
         DC    H'0'                                                             
         MVC   STDATE,WORK                                                      
         SPACE                                                                  
CKREV14  CLC   WORK+6(6),ENDATE    DOES THIS BRD MO COVER BOTH DATES            
         BNL   CKREV20                                                          
         GOTO1 =V(GETBROAD),(R1),(1,ENDATE),WORK,GETDAY,ADDAY,         C        
               RR=SPTR66RR                                                      
         SPACE                                                                  
CKREV20  DS   0H                                                                
         GOTO1 DATCON,DMCB,(0,STDATE),(3,STDATEB)                               
         GOTO1 (RF),(R1),(0,ENDATE),(3,ENDATEB)                                 
         GOTO1 (RF),(R1),(0,STDATE),(2,STDATEP)                                 
         GOTO1 (RF),(R1),(0,ENDATE),(2,ENDATEP)                                 
         SPACE                                                                  
         MVC   REVKPER,STDATEB  PLACE START DATE (YR & MO) IN KEY               
         SPACE                                                                  
         CLI   SVTNPR2,C'W'                                                     
         BNE   CKREV30                                                          
         MVC   REVKPER,STDATEP  PLACE START DATE PACKED DATE                    
         SPACE                                                                  
CKREV30  DS    0H                                                               
         GOTO1 HIGH             MOVE KEY INTO KEYSAVE AND READ NEW KEY          
         SPACE                                                                  
         CLC   KEY(14),KEYSAVE  IF KEY NE TO KEYSAVE REV RECORD DOES            
*                                NOT EXIST FOR THIS CLIENT                      
         BNE   CKREV35                                                          
         SPACE                                                                  
         CLI   SVTNPR2,C'W'                                                     
         BE    CKREV40                                                          
         CLC   REVKPER,ENDATEB  SEE IF DATE IS IN PERIOD                        
         BH    CKREV35           IF NOT, MAYBE CHK NEXT CLIENT                  
         B     CKREV50                                                          
         SPACE                                                                  
CKREV35  CLI   BCLT,0              IS IT CLT SPECIFIC                           
         BNE   CKREVX               YES, DONE                                   
         B     CKREV05                                                          
         SPACE                                                                  
CKREV40  CLC   REVKPER,ENDATEP                                                  
         BH    CKREV35                                                          
         SPACE                                                                  
CKREV50  BCTR  R2,0                                                             
         SPACE                                                                  
CKREVX   MVC   KEY(L'SVUKEY),SVUKEY RESTORE ORIGINAL KEY                        
         LTR   R2,R2               IF ZERO THEN NO REV REC FOUND                
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
         SPACE                                                                  
         DS    0H                                                               
FCLT     NTR1                                                                   
         SPACE                                                                  
         MVC   BBCLT,BCLT          SAVE                                         
         SPACE                                                                  
* SAVE CURRENT RECORD                                                           
         SPACE                                                                  
         L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,20(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
FCLT10   MVC   SVUKEY,KEY                                                       
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
         SPACE                                                                  
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
         SPACE                                                                  
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT15              NO, GET NEXT CLIENT                          
         SPACE                                                                  
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BZ    FCLT20              OFFICES MATCH, OK TO LIST                    
         SPACE                                                                  
FCLT15   MVC   KEY,SVUKEY                                                       
         MVI   KEY+4,X'FF'         GET NEXT CLIENT                              
         SPACE                                                                  
         BAS   RE,INITNET                                                       
         SPACE                                                                  
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(2),KEYSAVE      IF SAME REC TYPE & A/M                       
         BNE   FCLTNE                                                           
         SPACE                                                                  
         OC    KEY+2(2),KEY+2      ANY CLIENT                                   
         BNZ   *+14                 YES                                         
         XC    SVBCLT,SVBCLT                                                    
         B     FCLT20                                                           
         SPACE                                                                  
         MVC   SVBCLT,KEY+2        SAVE CLIENT                                  
         B     FCLT10                                                           
         SPACE                                                                  
FCLT20   DS    0H                                                               
         SPACE                                                                  
         L     R0,AIO1             MOVE BACK THE ORIGINAL RECORD                
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,20(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         BAS   RE,GTPROF                                                        
         SPACE                                                                  
         L     R4,AIO1                                                          
         MVC   KEY(20),0(R4)                                                    
         CLI   MODE,LISTRECS       LIST                                         
         BE    *+12                                                             
         CLI   MODE,PRINTREP       REPORT                                       
         BNE   *+10                                                             
         MVC   KEY(L'SVUKEY),SVUKEY  RESTORE KEY AND DISK ADDR                  
         SPACE                                                                  
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
         BAS   RE,INITNET                                                       
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         GOTO1 GETREC                                                           
         SPACE                                                                  
         CR    RB,RB                                                            
         B     FCLTX                                                            
         SPACE                                                                  
FCLTNE   LTR   RB,RB                                                            
         SPACE                                                                  
FCLTX    MVC   BCLT,BBCLT          RESTORE                                      
         XIT1                                                                   
         EJECT                                                                  
* GET TN PROFILE FOR THIS CLIENT                                                
         SPACE                                                                  
         DS    0H                                                               
GTPROF   NTR1                                                                   
         XC    WORK,WORK           * READ TN PROFILE *                          
         SPACE                                                                  
         MVC   WORK(4),=C'S0TN'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         CLI   BCLT,0              WAS CLIENT ENETERED                          
         BE    *+10                 NO                                          
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF CLIENT OFFICE                                
         SPACE                                                                  
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    GTPROF10                                                         
         SPACE                                                                  
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    GTPROF10                                                         
         SPACE                                                                  
         MVC   WORK+11(1),SVPRDOFF USE PROD OFFICE                              
         SPACE                                                                  
GTPROF10 GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         MVC   WORK+2(2),=C'N0'                                                 
         GOTO1 (RF),(R1),,N0PROF,DATAMGR                                        
         B     EXIT                                                             
         SPACE                                                                  
MISPRGER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MISPRG),MISPRG                                         
         B     ERREXIT                                                          
         SPACE                                                                  
REVRECER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'REVRECMS),REVRECMS                                     
         B     ERREXIT                                                          
         SPACE                                                                  
EQUPGMER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'EQUPGMMS),EQUPGMMS                                     
         B     ERREXIT                                                          
         SPACE                                                                  
DATOVLER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DATOVLMS),DATOVLMS                                     
         B     ERREXIT                                                          
         SPACE                                                                  
MSPRGER  LA    R2,TRAPRGAH         POINT AT FIRST BASE PRG                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MSPRG),MSPRG                                           
         B     ERREXIT                                                          
         SPACE                                                                  
NETERRA  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NETERMS),NETERMS                                       
         B     ERREXIT                                                          
         SPACE                                                                  
PROGERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PROGERMS),PROGERMS                                     
         B     ERREXIT                                                          
         SPACE                                                                  
EQVBASER XC    CONHEAD,CONHEAD                                                  
         MVC   EQVBAS,KEY+PGEPEPRG-PGERECD                                      
         MVC   CONHEAD(L'EQVBASMS),EQVBASMS                                     
         B     ERREXIT                                                          
         SPACE                                                                  
SAMERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SAMMSG),SAMMSG                                         
         B     ERREXIT                                                          
         SPACE                                                                  
BASEEQU  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BASEPG3),BASEPG3                                       
         B     ERREXIT                                                          
BASPRTER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BASPRTMS),BASPRTMS                                     
         B     ERREXIT                                                          
         SPACE                                                                  
DATSQERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DATSEQMS),DATSEQMS                                     
         SPACE                                                                  
ERREXIT  GOTO1 ERREX2                                                           
         SPACE                                                                  
BADATE   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
DELREC   MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
MISSNET  LA    R2,TRANETH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
FTRMSG   DC    C'* ERROR *'                                                     
FTRHELP  DC    C'NO VALID FILTERS'                                              
         SPACE                                                                  
EQVBASMS DC    C'* ERROR * PROGRAM IS BASE FOR XXXXXX *'                        
         ORG   *-8                                                              
EQVBAS   DS    CL6                                                              
         DS    CL2                                                              
NETERMS  DC    C'* ERROR * NO NETWORK FOUND *'                                  
BASPRTMS DC    C'* ERROR * BASE REPORT ONLY NOW, SOON, OV OR DDS *'             
REVRECMS DC    C'* ERROR * REVISIONS EXIST FOR EQUIV PROG FOR THESE DATC        
               ES *'                                                            
MISPRG   DC    C'* ERROR * MISSING PROGRAM *'                                   
BASMSG   DC    C'* ERROR * EQUIVALENT ALREADY IN USE AS BASE CODE *'            
PASMSG   DC    C'* ERROR * CAN''T USE EQUIVALENT, MUST USE BASE *'              
         ORG   *-8                                                              
PASPB    DS    CL6                                                              
         DS    CL2                                                              
SAMMSG   DC    C'* ERROR * EQUIVALENT PROGRAM IS SAME AS BASE TO BE ADDX        
               ED *'                                                            
PROGERMS DC    C'* ERROR * NO PROGRAM FOUND *'                                  
BASEPG3  DC    C'* ERROR * BASE PROGRAM EQUAL TO EQUIVALENT PGM *'              
DATSEQMS DC    C'* ERROR * DATES MUST BE IN SEQUENCE *'                         
DATOVLMS DC    C'* ERROR * DATES OVERLAP PREVIOUS ENTRY *'                      
EQUPGMMS DC    C'* ERROR * BASE PROGRAM EQUAL TO PREVIOUS ENTRY *'              
MSPRG    DC    C'* ERROR * PROGRAM(S) REQUIRED *'                               
         EJECT                                                                  
HEADINGA SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,35,C'E Q U I V A L E N T  L I S T'                            
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,35,C'----------------------------'                            
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'NETWORK'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,18,C'EQUIV-PROGRAM'                                           
         SSPEC H9,18,C'-------------'                                           
         SSPEC H8,40,C'BASE PROGRAMS    EFFECTIVE DATES'                        
         SSPEC H9,40,C'-------------    ---------------'                        
         DC    X'00'               END MARKER FOR SSPECS                        
         SPACE 3                                                                
HEADINGB SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,33,C'B A S E  P R O G R A M  L I S T'                         
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,33,C'-------------------------------'                         
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'NETWORK'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,18,C'BASE-PROGRAM'                                            
         SSPEC H9,18,C'------------'                                            
         SSPEC H8,40,C'EQUIV-PROGRAMS   EFFECTIVE DATES'                        
         SSPEC H9,40,C'--------------   ---------------'                        
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*                     INCLUDED FILES                                  *         
*        SPTRNEQPRG        DSECT FOR EQUIV RECS                       *         
*        SPGENSTA                                                     *         
*        SPGENPROG                                                    *         
*        SPTRNREV                                                     *         
*                     INCLUDED DSECTS                                 *         
*        DDSPOOLD                                                     *         
*        DDSPLWORKD                                                   *         
*        DDGENTWA                                                     *         
*        SPTRAWORKD                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
       ++INCLUDE SPTRNEQPRG        DSECT FOR EQUIV RECS                         
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPTRNREV                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
***********************************************************************         
*                T W A - BASE (SPTRAFF) AND SPTRA86                   *         
***********************************************************************         
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA86D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         SPACE                                                                  
         EJECT                                                                  
* MY STORAGE                                                                    
         SPACE                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR66RR DS    F                                                                
N0PROF   DS    CL16                                                             
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
         SPACE                                                                  
SVBCLT   DS    XL2                                                              
BBCLT    DS    XL2                                                              
         SPACE                                                                  
EQUIVPGM DS    CL6                 SET IN VK, DK RTNS                           
EQUIVDAY DS    XL1                                                              
EQUIVTIM DS    XL4                                                              
         SPACE                                                                  
SVUKEY   DS    CL25                                                             
         SPACE                                                                  
* BASE PROGRAM & DATES                                                          
         SPACE                                                                  
BASEPGM  DS    CL6                                                              
BSTDATEB DS    XL3                                                              
BENDATEB DS    XL3                                                              
BENDATEP DS    XL3                                                              
         SPACE                                                                  
SVPGESTR DS    XL3                 SAVE EFFECTIVE START                         
SVPGEEND DS    XL3                  AND END DATES                               
         SPACE                                                                  
NETMKT   DS    H                                                                
NETWORK  DS    CL4                                                              
BASLSTSW DS    XL1                 LIST RECS IN BASE PROGRAM CODE ORDER         
         SPACE                                                                  
BASELIST DS    CL70                BASE/EQV PROGS IN THIS REC                   
         SPACE                                                                  
* BASE PROGRAM DATES CHANGED TO BROADCAST IF NEEDED                             
         SPACE                                                                  
STDATE   DS    CL6                                                              
ENDATE   DS    CL6                                                              
STDATEB  DS    XL3                                                              
ENDATEB  DS    XL3                                                              
STDATEP  DS    XL2                                                              
ENDATEP  DS    XL2                                                              
         SPACE                                                                  
         DS   0D                                                                
BASEWORK DS    CL5400                                                           
         SPACE 3                                                                
BASLSTD  DSECT                                                                  
BASENT   DS   0CL24                                                             
BASBCLT  DS    XL2                                                              
BASNET   DS    CL4                                                              
BASBPRG  DS    CL6                                                              
BASEPRG  DS    CL6                                                              
BASSDT   DS    XL3                                                              
BASEDT   DS    XL3                                                              
BASNEXT  EQU   *                                                                
         SPACE 3                                                                
SPOOLD   DSECT                                                                  
         ORG   P1                                                               
         DS    CL4                                                              
PNET     DS    CL4                                                              
         DS    CL13                                                             
PPRG     DS    CL6                                                              
         DS    CL16                                                             
PBASE    DS    CL6                                                              
         DS    CL7                                                              
PBASESTR DS    CL6                                                              
         DS    CL3                                                              
PBASEEND DS    CL6                                                              
         SPACE 3                                                                
* ONLINE LIST                                                                   
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLT     DS    CL3                                                              
         DS    C                                                                
LNET     DS    CL3                                                              
         DS    C                                                                
LPRG     DS    CL6                                                              
         DS    CL2                                                              
LBASE    DS    CL58                                                             
*                                                                               
         DS    0D                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPTRA66   05/01/06'                                      
         END                                                                    
