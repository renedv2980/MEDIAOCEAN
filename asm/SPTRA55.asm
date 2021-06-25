*          DATA SET SPTRA55    AT LEVEL 002 AS OF 02/12/10                      
*PHASE T21655A                                                                  
*                                                                               
*  TITLE: T21655 - NETWORK TRAFFIC PGROUP LIST RECORD MAINTENANCE'    *         
*                                                                     *         
*  COMMENTS: THIS PROGRAM WILL MAINTAIN RECORDS CONTAINING LISTS OF   *         
*            PEOPLE TO BE NOTIFIED WHENENVER INSTRUCTIONS ARE RUN FOR *         
*            A GIVEN PRODUCT GROUP.                                   *         
*                                                                     *         
*  CALLED FROM: TRAFFIC CONTROLLER (T21600), WHICH CALLS              *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATA MANAGER                                          *         
*                                                                     *         
*                                                                     *         
*  INPUTS: SEE SCREEN SPTRAA5 (T216A5)                                *         
*          SPTRAWORKD (SYSD)                                          *         
*          DDSPLWORKD (GEND)                                          *         
*                                                                     *         
*  OUTPUTS: UPDATED PGROUP LIST RECORDS                               *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - ELEM CTR IN VR AND DR RTNS                            *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 -                                                  *         
*             AIO3 -                                                  *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
***********************************************************************         
*                        CHANGES LOG                                  *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21655 PGROUP LIST RECORD MAINTENANCE'                          
T21655   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21655*                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE LIST RECORDS                         
         BE    LR                                                               
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
*                                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
*                                                                               
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VK02     DS    0H                                                               
         LA    R2,FLDH             FAKE VALIDATE MEDIA                          
         MVC   FLDH,=X'0A01000184010001'                                        
         MVI   FLD,C'N'                                                         
         GOTO1 VALIMED                                                          
*                                                                               
         XC    BCLT,BCLT           CLIENT                                       
         LA    R2,TRACLTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK05                                                             
*                                                                               
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK03                 NO                                          
*                                                                               
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
*                                                                               
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
*                                                                               
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
*                                                                               
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
*                                                                               
         MVI   ERROR,0                                                          
*                                                                               
VK03     CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK10                                                             
*                                                                               
VK05     GOTO1 VALICLT                                                          
*                                                                               
VK10     XC    PGROUP,PGROUP       PRODUCT GROUP                                
         LA    R2,TRAPGRH                                                       
         CLI   5(R2),0             TEST PROD GROUP ENTERED                      
         BNE   VK20                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   MISSERR                                                          
         B     VK30                                                             
*                                                                               
VK20     OC    BCLT,BCLT           PGROUP REQUIRES CLIENT                       
         BNZ   VK25                                                             
         LA    R2,TRACLTH                                                       
         B     NOCLTERR                                                         
*                                                                               
VK25     BAS   RE,VPGR             VALIDATE PRODUCT GROUP                       
*                                                                               
VK30     LA    R2,TRANETH          NETWORK                                      
         XC    NETWORK,NETWORK                                                  
         CLI   5(R2),0             ANY NETWORK ENTERED                          
         BE    VK80                                                             
*                                                                               
         BAS   RE,VNT              VALIDATE NETWORK                             
*                                                                               
VK80     XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING PGRKEY,R4                                                        
         MVC   PGRKID,=XL2'0A45'   PGROUP NAME LIST RECORD                      
         MVC   PGRKAM,BAGYMD                                                    
         MVC   PGRKCLT,BCLT                                                     
         MVC   PGRKGRP,PGROUP      ALPHA NUM (PWOS)                             
         MVC   PGRKNET,NETWORK                                                  
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
*                                                                               
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VR01                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VR01     DS    0H                                                               
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         USING PGRKEY,R4                                                        
         MVC   BAGYMD,PGRKAM                                                    
         CLC   BCLT,PGRKCLT                                                     
         BE    VR02                YES                                          
*                                                                               
         BAS   RE,FCLT            GO GET CLIENT SVCLIST                         
         BE    *+6                 SHOULD BE OKAY                               
         DC    H'0'                                                             
VR02     DS    0H                                                               
         MVC   PGROUP,PGRKGRP                                                   
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PGRCONEL,R6                                                      
         MVI   PGRCONEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   PGRCONLN,86         ELEMENT LENGTH                               
*                                                                               
         LA    R2,TRACONTH                                                      
         CLI   5(R2),0             CONTACT NAME ?                               
         BE    VR05                 NO                                          
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         MVC   PGRCONNM,WORK                                                    
*                                                                               
         CLI   TRATELH+5,0         TELEPHONE # REQUIRED                         
         BE    PHONEERR                                                         
*                                                                               
VR05     LA    R2,TRATELH                                                       
         CLI   5(R2),0             TELEPHONE NUMBER?                            
         BE    VR10                 NO                                          
*                                                                               
         CLI   TRACONTH+5,0                                                     
         BE    CONTERR             MUST HAVE CONTACT NAME                       
*                                                                               
         GOTO1 ANY                                                              
         MVC   PGRCONTL,WORK                                                    
*                                                                               
VR10     LA    R2,TRAFAXH          FAX FIELD                                    
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR12                                                             
*                                                                               
         CLI   TRACONTH+5,0                                                     
         BE    CONTERR             MUST HAVE CONTACT NAME                       
*                                                                               
         GOTO1 ANY                                                              
         MVC   PGRFAXTL,WORK                                                    
*                                                                               
VR12     OC    2(84,R6),2(R6)      EMPTY ELEM?                                  
         BZ    VR13                 YES, NO NEED TO ADD ELEM                    
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR13     MVI   ELCODE,PGREMLEQ     X'30' ELEMENT                                
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         LA    R2,TRAEMLH          EMAIL FIELD                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR15                                                             
         GOTO1 ANY                                                              
         XC    ELEM,ELEM                                                        
         MVI   ELEM,PGREMLEQ                                                    
         MVI   ELEM+1,PGREMLNQ                                                  
         MVC   ELEM+2(L'PGREMLAD),WORK                                          
         GOTO1 ADDELEM                                                          
*                                                                               
VR15     LA    R2,TRANAMEH         NAME FIELD                                   
         MVI   ELCODE,X'20'        NAME ELEMENT CODE                            
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         LA    R5,1                SET NAME COUNT                               
         LA    R6,ELEM                                                          
         USING PGRLSTEL,R6                                                      
         MVI   SEQNUM,0                                                         
         MVI   NMFOUND,C'N'        NO NAMES FOUND YET                           
*                                                                               
* FOR ADD, FORMAT DUMMY ELEMENT HEADER                                          
*                                                                               
VR20     CLI   5(R2),0             TEST FOR A NAME IN THIS FIELD                
         BE    VR40                                                             
*                                                                               
         MVI   NMFOUND,C'Y'        SET NAME FOUND FLAG                          
VR30     XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM           INCREMENT ELEMENT SEQUENCE NUMBER            
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVI   PGRLSTEL,X'20'      ELEMENT IDENTIFIER                           
         MVI   PGRLSTLN,33         ELEMENT LENGTH                               
         MVC   PGRLSTSQ,SEQNUM     ELEMENT SEQUENCE NUMBER                      
*                                                                               
         CLM   R5,1,SEQNUM         NEED ANY BLANK PADDED ELEMS                  
         BE    VR36                                                             
         MVI   PGRLSTLN,4          ELEMENT LENGTH                               
         MVC   PGRLSTSQ,SEQNUM                                                  
         GOTO1 ADDELEM                                                          
         B     VR30                                                             
*                                                                               
VR36     GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         MVC   PGRLSTNM,WORK                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
VR40     ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    R5,1(,R5)           ADD TO NAME COUNT                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BL    VR20                                                             
*                                                                               
         CLI   NMFOUND,C'Y'        TEST FOR AT LEAST ONE NAME                   
         BNE   NONMERR             REQUIRED                                     
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         XC    TRACONT,TRACONT                                                  
         OI    TRACONTH+6,X'80'                                                 
         XC    TRATEL,TRATEL                                                    
         OI    TRATELH+6,X'80'                                                  
         XC    TRAFAX,TRAFAX                                                    
         OI    TRAFAXH+6,X'80'                                                  
*                                                                               
         XC    TRAEML,TRAEML                                                    
         OI    TRAEMLH+6,X'80'                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            GET CONTACT ELEMENT                          
         BNE   DR05                                                             
*                                                                               
         USING PGRCONEL,R6                                                      
*                                                                               
         MVC   TRACONT(L'PGRCONNM),PGRCONNM CONTACT NAME                        
         MVC   TRATEL(L'PGRCONTL),PGRCONTL TELEPHONE                            
         MVC   TRAFAX(L'PGRFAXTL),PGRFAXTL  FAX                                 
         DROP  R6                                                               
*                                                                               
DR05     DS    0H                                                               
         USING PGREMLEL,R6                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,PGREMLEQ     X'30' ELEMENT                                
         BAS   RE,GETEL            GET EMAIL ADDRESS ELEMENT                    
         BNE   DR10                                                             
         MVC   TRAEML(L'PGREMLAD),PGREMLAD  EMAIL ADDRESS                       
         DROP  R6                                                               
*                                                                               
DR10     L     R6,AIO                                                           
         USING PGRLSTEL,R6                                                      
*                                                                               
         LA    R2,TRANAMEH         FIRST NAME FIELD                             
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            TEST ANY NAMES                               
         BNE   DRX                                                              
*                                                                               
DR30     MVC   WORK(L'TRANAME),SPACES                                           
         CLI   PGRLSTLN,4                                                       
         BE    *+10                                                             
         MVC   WORK(L'PGRLSTNM),PGRLSTNM                                        
         MVC   8(L'TRANAME,R2),WORK   TRANSMIT NEW FIELD                        
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BNL   DRX                                                              
*                                                                               
         BAS   RE,NEXTEL           NEXT NAME                                    
         BE    DR30                                                             
*                                                                               
DR40     MVC   8(L'TRANAME,R2),SPACES    BLANK OUT REMAINING FIELDS             
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BL    DR40                                                             
*                                                                               
DRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              RECORD SELECTED                              
         USING PGRKEY,R4                                                        
*                                                                               
         BAS   RE,FCLT             GO GET CLIENT SVCLIST                        
         BNE   EXIT                                                             
*                                                                               
         GOTO1 CLUNPK,DMCB,PGRKCLT,TRACLT                                       
         OI    TRACLTH+6,X'80'     CLIENT                                       
*                                                                               
         MVC   TRAPGR(1),PGRKAID   PGROUP ALPHA                                 
*                                                                               
         MVC   WORK(3),PGRKGRP                                                  
         BAS   RE,VPGR             GET PGRP LEN                                 
*                                                                               
         MVC   QPGR,SPACES                                                      
         MVC   DUB(4),=C'0000'                                                  
         UNPK  DUB(5),PGRKNUM(3)                                                
         LLC   RE,PGRLEN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   QPGR+1(0),DUB       PRINTABLE PROD GROUP                         
*                                                                               
         MVC   TRAPGR+1(3),QPGR+1  GROUP NUMBER                                 
         OI    TRAPGRH+6,X'80'     TRANSMIT                                     
*                                                                               
         MVC   TRANET,PGRKNET      PRESET NETWORK                               
         OI    TRANETH+6,X'80'                                                  
*                                                                               
         CLI   PGRKNET,0           TEST IF MEDIA                                
         BNE   EXIT                                                             
         CLI   PGRKNET+2,X'FF'                                                  
         BNE   EXIT                                                             
         MVC   TRANET(2),=C'M='                                                 
         MVC   TRANET+2(1),PGRKNET+1                                            
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST ROUTINE                                                           
*                                                                               
LR       LA    R4,KEY                                                           
         USING PGRKEY,R4                                                        
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
*                                                                               
         MVC   PGRKID,=X'0A45'     PGROUP LIST RECORD KEY                       
         MVC   PGRKAM,BAGYMD                                                    
         MVC   PGRKCLT,BCLT                                                     
         MVC   PGRKGRP,PGROUP                                                   
         MVC   PGRKNET,NETWORK     PRESET NETWORK                               
         CLC   NETWORK,=C'M='      TEST IF MEDIA                                
         BNE   LR05                                                             
         XC    PGRKNET,PGRKNET                                                  
         MVI   PGRKNET+2,X'FF'                                                  
         MVC   PGRKNET+1(1),NETWORK+2                                           
*                                                                               
LR05     MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(3),SAVEKEY      TEST SAME TYPE/AGENCY/MEDIA                  
         BNE   LRX                                                              
*                                                                               
         OC    SAVEKEY+3(2),SAVEKEY+3      TEST CLIENT ENTERED                  
         BZ    LR50                                                             
         CLC   PGRKCLT,SAVEKEY+3           IF SO, TEST KEY MATCH                
         BNE   LRX                                                              
*                                                                               
LR50     OC    SAVEKEY+5(3),SAVEKEY+5      TEST PGROUP ENTERED                  
         BZ    LR55                                                             
         CLC   PGRKGRP,SAVEKEY+5           IF SO, TEST KEY MATCH                
         BNE   LRX                                                              
*                                                                               
LR55     DS    0H                                                               
         OC    SAVEKEY+8(4),SAVEKEY+8      TEST NETWORK ENTERED                 
         BZ    LR60                                                             
         CLC   PGRKNET,SAVEKEY+8           IF SO, TEST KEY MATCH                
         BNE   LRX                                                              
*                                                                               
LR60     DS    0H                                                               
         CLC   BCLT,PGRKCLT        IS CLIENT SAME                               
         BNE   LR65                                                             
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BO    LR65                                                             
         B     LR70                                                             
*                                                                               
LR65     BAS   RE,FCLT             GO GET CLIENT SVCLIST                        
         BNE   EXIT                 ALL DONE                                    
*                                                                               
LR70     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         CLI   MODE,LISTRECS       ONLINE LIST                                  
         BE    LRL                                                              
         CLI   MODE,PRINTREP       OFFLINE LIST                                 
         BE    LRR                                                              
         DC    H'0'                                                             
LRX      B     EXIT                                                             
         EJECT                                                                  
* ONLINE LIST *                                                                 
*                                                                               
LRL      LR    R6,R4                                                            
         MVI   ELCODE,X'20'        NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         USING PGRLSTEL,R6                                                      
         BE    *+6                 MUST BE THERE                                
         DC    H'0'                                                             
*                                                                               
         MVC   LISTAR,SPACES       FILL IN LIST LINE                            
         GOTO1 CLUNPK,DMCB,PGRKCLT,LSTCLT                                       
*                                                                               
         MVC   LSTPGR(1),PGRKAID   PGROUP ALPHA                                 
*                                                                               
         MVC   WORK(3),PGRKGRP                                                  
         BAS   RE,VPGR             GET PGRP LEN                                 
*                                                                               
         MVC   QPGR,SPACES                                                      
         MVC   DUB(4),=C'0000'                                                  
         UNPK  DUB(5),PGRKNUM(3)                                                
         LLC   RE,PGRLEN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   QPGR+1(0),DUB       PRINTABLE PGRP                               
*                                                                               
         MVC   LSTPGR+1(3),QPGR+1  GROUP NUMBER                                 
*                                                                               
         MVC   LSTNET,PGRKNET      PRESET NETWORK                               
*                                                                               
         CLI   PGRKNET,0           TEST IF MEDIA                                
         BNE   LRL05                                                            
         CLI   PGRKNET+2,X'FF'                                                  
         BNE   LRL05                                                            
         MVC   LSTNET(2),=C'M='                                                 
         MVC   LSTNET+2(1),PGRKNET+1                                            
*                                                                               
LRL05    CLI   PGRLSTLN,4                                                       
         BE    *+10                                                             
         MVC   LSTNAME,PGRLSTNM                                                 
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'        CONTACT ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   LRL20                                                            
*                                                                               
         MVC   LSTCONT,PGRCONNM                                                 
         MVC   LSTTEL,PGRCONTL                                                  
*                                                                               
LRL20    GOTO1 LISTMON             SEND LINE TO SCREEN                          
         B     LR20                                                             
         EJECT                                                                  
* OFFLINE LIST *                                                                
*                                                                               
LRR      DS    0H                                                               
*                                                                               
         CLC   PGRKCLT,BCLT                                                     
         BNE   LRR01                                                            
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BO    LRR01                                                            
         B     LRR02                                                            
*                                                                               
LRR01    MVC   BCLT,PGRKCLT                                                     
         BAS   RE,FCLT                                                          
         BNE   EXIT                                                             
*                                                                               
LRR02    MVC   P,SPACES                                                         
*                                                                               
         MVC   PPGROUP(1),PGRKAID  PGROUP ALPHA                                 
*                                                                               
         MVC   WORK(3),PGRKGRP                                                  
         BAS   RE,VPGR             GET PGRP LEN                                 
*                                                                               
         MVC   QPGR,SPACES                                                      
         MVC   DUB(4),=C'0000'                                                  
         UNPK  DUB(5),PGRKNUM(3)                                                
         LLC   RE,PGRLEN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   QPGR+1(0),DUB       PRINTABLE PGRP                               
*                                                                               
         MVC   PPGROUP+1(3),QPGR+1  GROUP NUMBER                                
*                                                                               
         OC    PGRKNET,PGRKNET     ANY NETWORK                                  
         BZ    LRR04                                                            
*                                                                               
         MVC   PPGROUP+132(4),=C'NET='                                          
         MVC   PPGROUP+132+4(4),PGRKNET  PRESET NETWORK                         
*                                                                               
         CLI   PGRKNET,0           TEST IF MEDIA                                
         BNE   LRR04                                                            
         CLI   PGRKNET+2,X'FF'                                                  
         BNE   LRR04                                                            
         MVC   PPGROUP+132(2),=C'M='                                            
         MVC   PPGROUP+132+2(1),PGRKNET+1                                       
         MVC   PPGROUP+132+2+1(5),SPACES                                        
*                                                                               
LRR04    LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRR08                                                            
*                                                                               
         USING PGRCONEL,R6                                                      
*                                                                               
         MVC   PNAME1(9),=C'CONTACT= '                                          
         MVC   PNAME1+9(L'PGRCONNM),PGRCONNM                                    
*                                                                               
         LA    R2,PNAME1+132                                                    
         MVC   0(5,R2),=C'TEL= '                                                
         CLC   PGRCONTL,SPACES                                                  
         BNH   *+14                                                             
         MVC   5(L'PGRCONTL,R2),PGRCONTL                                        
         LA    R2,PNAME2+132                                                    
*                                                                               
         MVC   0(5,R2),=C'FAX= '                                                
         CLC   PGRFAXTL,SPACES                                                  
         BNH   *+14                                                             
         MVC   5(L'PGRFAXTL,R2),PGRFAXTL                                        
         B     LRR06                                                            
*                                                                               
         XC    0(5,R2),0(R2)                                                    
*                                                                               
LRR06    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LRR08    LR    R6,R4                                                            
         MVI   ELCODE,X'20'        NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         USING PGRLSTEL,R6                                                      
         BE    *+6                 MUST BE THERE                                
         DC    H'0'                                                             
*                                                                               
         LA    R2,PNAME1                                                        
         LA    R3,PNAME2                                                        
LRR10    CLI   PGRLSTLN,4                                                       
         BE    *+10                                                             
         MVC   0(30,R2),PGRLSTNM                                                
*                                                                               
         LA    R2,33(R2)                                                        
         CR    R2,R3                                                            
         BNH   LRR14                                                            
         LA    R2,PNAME1                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
LRR14    BAS   RE,NEXTEL                                                        
         BE    LRR10                                                            
         CLC   P,SPACES                                                         
         BE    LRR16                                                            
         OC    P,P                                                              
         BZ    LRR16                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R6                                                               
LRR16    DS    0H                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'30'        NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         USING PGREMLEL,R6                                                      
         BNE   LRR18               MUST BE THERE                                
         XC    P,P                                                              
         MVC   PPGROUP(8),=C'EMAIL : '                                          
         MVC   PPGROUP+8(L'PGREMLAD),PGREMLAD                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LRR18    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* FIND CLIENT NAME *                                                            
*                                                                               
FCLT     NTR1                                                                   
*                                                                               
         USING PGRKEY,R4                                                        
*                                                                               
         CLC   PGRKCLT,BCLT        DIFFERENT CLIENT                             
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       YES, FORCE TO NEXT PAGE                      
*                                                                               
         MVC   BCLT,PGRKCLT                                                     
         DROP  R4                                                               
*                                                                               
* SAVE CURRENT RECORD                                                           
*                                                                               
         L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         MVC   SVKEY,KEY                                                        
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         MVC   ELEM(8),TRACLTH                                                  
         MVI   ELEM+5,3                                                         
         MVC   ELEM+8(3),QCLT                                                   
         LA    R2,ELEM                                                          
*                                                                               
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVI   ERROPT,0            CLEAR                                        
*                                                                               
* MOVE PGROUP REC BACK TO AIO1                                                  
*                                                                               
         L     R0,AIO1                                                          
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         CLI   ACTNUM,ACTADD       IF AN ADD                                    
         BE    FCLT20              BYPASS                                       
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
FCLT20   MVC   AIO,AIO1                                                         
         CR    RB,RB               SET COND CODE OKAY                           
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------                          
* VALIDATE PRODUCT GROUP                                                        
* (ANNN = ALPHA/1-3 NUMERIC)                                                    
*-----------------------------------------------------                          
*                                                                               
VPGR     NTR1                                                                   
*                                                                               
         MVC   MYKEY(13),KEY       SAVE KEY                                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   VPGR15               NO, JUST NEED PGRP LEN                      
*                                                                               
         LLC   R0,5(R2)            INPUT LEN                                    
         LA    R1,8(R2)            INPUT                                        
         CLI   0(R1),C'A'                                                       
         BL    BADPRGER                                                         
         CLI   0(R1),C'Z'                                                       
         BH    BADPRGER                                                         
         BCTR  R0,0                                                             
                                                                                
         MVC   WORK(1),0(R1)       SAVE ALPHA                                   
         LA    R1,1(R1)                                                         
*                                                                               
         LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
VPGR10   CLI   0(R1),C'0'                                                       
         BL    PGRPERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    PGRPERR                                                          
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   R0,VPGR10                                                        
*                                                                               
         PACK  WORK+1(3),DUB(5)                                                 
*                                                                               
VPGR15   XC    KEY,KEY             READ PGRDEF RECORD FOR LENGTHS               
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(1),WORK       PROD GROUP ALPHA                             
         MVI   RDUPDATE,C'N'                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY                       
*                                                                               
         CLC   KEY(6),KEYSAVE                                                   
         BNE   BDPGRPER                                                         
*                                                                               
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,AIO,DMWORK            
*                                                                               
         L     R6,AIO                                                           
         LA    R6,24(R6)           POINT TO 01 ELEMENT                          
         CLI   0(R6),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PRGEL01,R6                                                       
         LLC   RF,PRGBK1LN                                                      
         LLC   RE,PRGBK2LN                                                      
         AR    RF,RE                                                            
         LLC   RE,PRGBK3LN                                                      
         AR    RF,RE                                                            
         STC   RF,PGRLEN           SAVE TOTAL NUMBER OF DIGITS                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   VPGRX                NO, JUST NEEDED LEN                         
*                                                                               
         LLC   R0,5(R2)            INPUT LEN                                    
         BCTR  R0,0                MINUS 1 FOR ALPHA                            
         CR    RF,R0               PGROUP LENGTH TO INPUT LEN                   
         BNE   BDPGRPER                                                         
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D81'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
         MVC   KEY+5(3),WORK                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   BDPGRPER                                                         
*                                                                               
* GET PRODUCT GROUP NAME *                                                      
*                                                                               
VPGR20   MVC   SVKEY,KEY                                                        
         NI    KEY+1,X'7F'                                                      
         XC    KEY+8(5),KEY+8                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVPRGNAM,2(R6)                                                   
*                                                                               
         MVC   PGROUP,KEY+5        PGROUP (ANN - PWOS)                          
*                                                                               
VPGRX    MVC   AIO,AIO1                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JE    EXIT                                                             
*                                                                               
         MVC   KEY(13),MYKEY       RESTORE KEY                                  
         GOTO1 HIGH                DUMMY HIGH FOR SEQ                           
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                                                             
*                                                                               
BDPGRPER MVC   GERROR,=Y(PRDGRPNF) NO PRODUCT GROUP XXX FOUND                   
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   ELEM,7                                                           
         LLC   RE,5(R2)            INPUT LEN                                    
         BCTR  RE,0                MINUS 1 FOR EX                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+1(0),8(R2)                                                  
         B     ERR                                                              
*                                                                               
PGRPERR  MVC   GERROR,=Y(BDPGRPLN)                                              
         B     ERR                                                              
*                                                                               
BADPRGER MVC   GERROR,=Y(BADPRGR)  ENTER PROD GROUP AS SNN                      
ERR      GOTO1 VTRAERR                                                          
*                                                                               
*                                                                               
         EJECT                                                                  
* VALIDATE NETWORK                                                              
*                                                                               
         DS    0H                                                               
VNT      NTR1                                                                   
*                                                                               
         CLC   =C'M=',8(R2)        WAS MEDIA ENTERED                            
         BNE   VNT10                                                            
         CLI   5(R2),3                                                          
         BNE   BADMED                                                           
*                                                                               
         CLI   10(R2),C'N'                                                      
         BE    *+28                                                             
         CLI   10(R2),C'C'                                                      
         BE    *+20                                                             
         CLI   10(R2),C'S'                                                      
         BE    *+12                                                             
         CLI   10(R2),C'O'                                                      
         BNE   BADMED                                                           
*                                                                               
* PUT MEDIA IN NETWORK PART OF KEY AS  X'00MMFF00'                              
*                                                                               
         MVC   NETWORK+1(1),10(R2)                                              
         MVI   NETWORK+2,X'FF'                                                  
         J     EXIT                                                             
*                                                                               
VNT10    GOTO1 ANY                                                              
         MVC   NETWORK,WORK                                                     
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,BCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         JE    EXIT                                                             
         MVC   GERROR,=Y(NONET)                                                 
         B     VNTEREX2                                                         
*                                                                               
BADMED   MVC   GERROR,=Y(BADMEDIA)                                              
*                                                                               
VNTEREX2 GOTO1 VTRAERR                                                          
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
*                                                                               
HDHK     NTR1                                                                   
         MVC   H4+10(3),QCLT                                                    
         MVC   H4+15(20),CLTNM                                                  
         B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
NOCLTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCLTMS),NOCLTMS                                       
         GOTO1 ERREX2                                                           
NOCLTMS  DC    C'* ERROR * PGROUP REQUIRES CLIENT *'                            
*                                                                               
PHONEERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PHONEMS),PHONEMS                                       
         LA    R2,TRATELH                                                       
         GOTO1 ERREX2                                                           
PHONEMS  DC    C'* ERROR * TELEPHONE NUMBER REQUIRED *'                         
*                                                                               
CONTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CONTMS),CONTMS                                         
         LA    R2,TRACONTH                                                      
         GOTO1 ERREX2                                                           
CONTMS   DC    C'* ERROR * CONTACT NAME REQUIRED *'                             
*                                                                               
NONMERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NONMMS),NONMMS                                         
         LA    R2,TRANAMEH                                                      
         GOTO1 ERREX2                                                           
NONMMS   DC    C'* ERROR * AT LEAST ONE NAME REQUIRED *'                        
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,33,C'PGROUP DISTRIBUTION LIST'                                
         SSPEC H2,33,C'------------------------'                                
         SSPEC H1,65,AGYNAME                                                    
         SSPEC H2,65,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,78,RUN                                                        
         SSPEC H4,65,REPORT                                                     
         SSPEC H5,65,REQUESTOR                                                  
         SSPEC H5,95,PAGE                                                       
         SSPEC H8,3,C'PGROUP'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,46,C'NAMES'                                                   
         SSPEC H9,46,C'-----'                                                   
         SSPEC H8,79,C'NAMES'                                                   
         SSPEC H9,79,C'-----'                                                   
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
       ++INCLUDE SPTRNPGRP                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAB5D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
       ++INCLUDE SPGENPRG                                                       
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0D                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL64                                                             
NMFOUND  DS    CL1                                                              
SEQNUM   DS    XL1                                                              
SAVEKEY  DS    XL13                                                             
MYKEY    DS    XL13                                                             
NETWORK  DS    CL4                                                              
QPGR     DS    CL4                                                              
PGROUP   DS    XL3                                                              
SVPRGNAM DS    CL24                PRODUCT GROUP NAME                           
*                                                                               
* OFFLINE REPORT LINE                                                           
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL3                                                              
PPGROUP  DS    CL3                                                              
         DS    CL6                                                              
PPRODNM  DS    CL20                                                             
         DS    CL13                                                             
PNAME1   DS    CL30                                                             
         DS    CL3                                                              
PNAME2   DS    CL30                                                             
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCLT   DS    CL3                                                              
         DS    CL2                                                              
LSTPGR   DS    CL3                                                              
         DS    CL2                                                              
LSTNET   DS    CL4                                                              
         DS    CL2                                                              
LSTNAME  DS    CL18                                                             
         DS    CL3                                                              
LSTCONT  DS    CL20                                                             
         DS    CL2                                                              
LSTTEL   DS    CL12                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPTRA55   02/12/10'                                      
         END                                                                    
