*          DATA SET SPTRA18    AT LEVEL 024 AS OF 08/24/04                      
*PHASE T21618A                                                                  
*  TITLE: T21618 - NETWORK TRAFFIC PROGRAM LIST RECORD MAINTENANCE'   *         
*                                                                     *         
*  COMMENTS: THIS PROGRAM WILL MAINTAIN RECORDS CONTAINING LISTS OF   *         
*            PEOPLE TO BE NOTIFIED WHENENVER INSTRUCTIONS ARE RUN FOR *         
*            A GIVEN PROGRAM.                                         *         
*                                                                     *         
*  CALLED FROM: TRAFFIC CONTROLLER (T21600), WHICH CALLS              *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATA MANAGER                                          *         
*                                                                     *         
*                                                                     *         
*  INPUTS: SEE SCREEN SPTRAA8 (T216A8)                                *         
*          SPTRAWORKD (SYSD)                                          *         
*          DDSPLWORKD (GEND)                                          *         
*                                                                     *         
*  OUTPUTS: UPDATED PROGRAM LIST RECORDS                              *         
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
***********************************************************************         
*                                                                     *         
*  LEV  18-19 MAKE NETWORK ADDRESS AN OPTIONAL FIELD                  *         
*  LEV  20    AUG04/86 PRINT LAST DATE FOR EXPIRED PROGRAM            *         
*  LEV  21    APR09/87 FIX PROGRAM EXPIRED ERROR MESSAGE              *         
*  LEV  22    OCT30/87 FIX BUG - DOESN'T DISPLAY BLANK NETWORK ADDR   *         
*  LEV  23    SEP09/94 ELIMINATE DATE CHECK FOR VPROG                 *         
*  LEV  24    JUL26/04 SOX                                            *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21618 PROGRAM LIST RECORD MAINTENANCE'                         
T21618   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21618**                                                       
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
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
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
         SPACE                                                                  
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VK02     DS    0H                                                               
         LA    R2,FLDH             FAKE VALIDATE MEDIA                          
         MVC   FLDH,=X'0A01000184010001'                                        
         MVI   FLD,C'N'                                                         
         GOTO1 VALIMED                                                          
*                                                                               
         XC    NETWORK,NETWORK     NETWORK                                      
         LA    R2,TRANETH                                                       
         CLI   5(R2),0             TEST NETWORK ENTERED                         
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK20                                                             
         B     MISSERR                                                          
*                                                                               
VK10     BAS   RE,VNET                                                          
*                                                                               
VK20     XC    PROGRAM,PROGRAM     PROGRAM                                      
         LA    R2,TRAPRGH                                                       
         CLI   5(R2),0             TEST PROGRAM ENTERED                         
         BE    VK50                                                             
*                                                                               
VK30     OC    NETWORK,NETWORK     TEST NETWORK ENTERED                         
         BNZ   *+12                                                             
         LA    R2,TRANETH          PROGRAM REQUIRES NETWORK                     
         B     NONETERR                                                         
*                                                                               
         BAS   RE,VPROG                                                         
*                                                                               
VK50     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRGKEY,R4                                                        
         MVC   PRGKID,=XL2'0A43'   PROGRAM NAME LIST RECORD                     
         MVC   PRGKAM,BAGYMD                                                    
         MVC   PRGKNET,NETWORK                                                  
         MVC   PRGKPRG,PROGRAM                                                  
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
*                                                                               
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VR01                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VR01     DS    0H                                                               
         L     R4,AIO                                                           
         USING PRGKEY,R4                                                        
         MVC   BAGYMD,PRGKAM                                                    
         MVC   NETWORK,PRGKNET                                                  
         MVC   PROGRAM,PRGKPRG                                                  
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING PRGADREL,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   PRGADREL,X'10'      ELEMENT IDENTIFIER                           
         MVI   PRGADRLN,32         ELEMENT LENGTH                               
*                                                                               
         LA    R2,TRANADRH                                                      
         CLI   5(R2),0             TEST FOR A NETWORK ADDRESS                   
         BE    VR40                NO                                           
*                                                                               
         GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         MVC   PRGADRAD,WORK                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
VR40     LA    R2,TRANAMEH         NAME FIELD                                   
         MVI   ELCODE,X'20'        NAME ELEMENT CODE                            
         GOTO1 REMELEM                                                          
         LA    R5,1                SET NAME COUNT                               
         LA    R6,ELEM                                                          
         USING PRGLSTEL,R6                                                      
         MVI   SEQNUM,0                                                         
*                                                                               
* FOR ADD, FORMAT DUMMY ELEMENT HEADER                                          
*                                                                               
VR50     CLI   5(R2),0             TEST FOR A NAME IN THIS FIELD                
         BE    VR60                                                             
*                                                                               
VR54     XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM           INCREMENT ELEMENT SEQUENCE NUMBER            
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVI   PRGLSTEL,X'20'      ELEMENT IDENTIFIER                           
         MVI   PRGLSTLN,33         ELEMENT LENGTH                               
         MVC   PRGLSTSQ,SEQNUM     ELEMENT SEQUENCE NUMBER                      
*                                                                               
         CLM   R5,1,SEQNUM         NEED ANY BLANK PADDED ELEMS                  
         BE    VR56                                                             
         MVI   PRGLSTLN,4          ELEMENT LENGTH                               
         MVC   PRGLSTSQ,SEQNUM                                                  
         GOTO1 ADDELEM                                                          
         B     VR54                                                             
*                                                                               
VR56     GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         MVC   PRGLSTNM,WORK                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
VR60     ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    R5,1(,R5)           ADD TO NAME COUNT                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BL    VR50                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            GET NETWORK ADDRESS                          
         BNE   DR10                                                             
*                                                                               
         USING PRGADREL,R6                                                      
*                                                                               
         MVC   TRANADR,SPACES      NETWORK ADDRESS                              
         MVC   TRANADR(L'PRGADRAD),PRGADRAD                                     
         OI    TRANADRH+6,X'80'                                                 
         B     DR50                                                             
*                                                                               
DR10     MVC   TRANADR,SPACES      NETWORK ADDRESS                              
         OI    TRANADRH+6,X'80'                                                 
*                                                                               
DR50     LA    R2,TRANAMEH         FIRST NAME FIELD                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            TEST ANY NAMES                               
         BNE   DR70                                                             
*                                                                               
         USING PRGLSTEL,R6                                                      
*                                                                               
DR60     MVC   WORK(L'TRANAME),SPACES                                           
         CLI   PRGLSTLN,4                                                       
         BE    *+10                                                             
         MVC   WORK(L'PRGLSTNM),PRGLSTNM                                        
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
         BE    DR60                                                             
*                                                                               
DR70     MVC   8(L'TRANAME,R2),SPACES    BLANK OUT REMAINING FIELDS             
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BL    DR70                                                             
*                                                                               
DRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              RECORD SELECTED                              
         USING PRGKEY,R4                                                        
*                                                                               
         MVC   TRANET,PRGKNET                                                   
         OI    TRANETH+6,X'80'     NETWORK                                      
         MVC   TRAPRG,PRGKPRG                                                   
         OI    TRAPRGH+6,X'80'     PROGRAM                                      
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST ROUTINE                                                           
*                                                                               
LR       LA    R4,KEY                                                           
         USING PRGKEY,R4                                                        
         USING PRGADREL,R6                                                      
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
*                                                                               
         MVC   PRGKID,=X'0A43'     PROGRAM LIST RECORD KEY                      
         MVC   PRGKAM,BAGYMD                                                    
         MVC   PRGKNET,NETWORK                                                  
         MVC   PRGKPRG,PROGRAM                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         MVC   NETWORK,KEY+3                                                    
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(3),SAVEKEY      TEST SAME TYPE/AGENCY/MEDIA                  
         BNE   LRX                                                              
*                                                                               
         OC    SAVEKEY+3(4),SAVEKEY+3     TEST NETWORK ENTERED                  
         BZ    LR40                                                             
         CLC   PRGKNET,SAVEKEY+3          IF SO, TEST KEY MATCH                 
         BNE   LRX                                                              
*                                                                               
LR40     OC    SAVEKEY+7(6),SAVEKEY+7     TEST PROGRAM ENTERED                  
         BZ    LR50                                                             
         CLC   PRGKPRG,SAVEKEY+7          IF SO, TEST KEY MATCH                 
         BNE   LRX                                                              
*                                                                               
LR50     CLC   NETWORK,KEY+3       NETWORK CHANGE                               
         BE    LR60                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   NETWORK,KEY+3       NETWORK CHANGE                               
*                                                                               
LR60     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LR    R6,R4                                                            
*                                                                               
         CLI   MODE,LISTRECS       ONLINE LIST                                  
         BE    LRL                                                              
         CLI   MODE,PRINTREP       OFFLINE LIST                                 
         BE    LRR                                                              
         DC    H'0'                                                             
LRX      B     EXIT                                                             
         EJECT                                                                  
* ONLINE LIST *                                                                 
         SPACE                                                                  
LRL      MVC   LISTAR,SPACES       FILL IN LIST LINE                            
         MVC   LSTNET,PRGKNET                                                   
         MVC   LSTPROG,PRGKPRG                                                  
         MVI   ELCODE,X'10'        NETWORK ADDRESS ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   *+10                MUST BE THERE                                
         MVC   LSTNADR,PRGADRAD                                                 
*                                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
         B     LR20                                                             
         SPACE 3                                                                
* ONLINE LIST *                                                                 
         SPACE                                                                  
         USING PRGKEY,R4                                                        
LRR      MVC   P,SPACES                                                         
         MVC   PPROG,PRGKPRG                                                    
*                                                                               
         MVI   ELCODE,X'10'        NETWORK ADDRESS ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   *+10                MUST BE THERE                                
         MVC   PNETADDR,PRGADRAD                                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        NAME LIST ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   LRR20                                                            
         USING PRGLSTEL,R6                                                      
         LA    R2,PNAME1                                                        
LRR10    CLI   PRGLSTLN,4                                                       
         BE    *+10                                                             
         MVC   0(30,R2),PRGLSTNM                                                
*                                                                               
         LA    R2,33(R2)                                                        
         LA    R1,PNAME2                                                        
         CR    R2,R1                                                            
         BNH   LRR14                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,PNAME1                                                        
LRR14    BAS   RE,NEXTEL                                                        
         BE    LRR10                                                            
         CLC   P,SPACES                                                         
         BE    LRR16                                                            
         OC    P,P                                                              
         BZ    LRR16                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
LRR16    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
LRR20    GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 (RF),(R1),(R8)                                                   
         B     LR20                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* VALIDATE NETWORK                                                              
*                                                                               
VNET     NTR1                                                                   
         USING STARECD,R4          LOOK UP NETWORK IN STATION RECORD            
*                                                                               
         GOTO1 ANY                 PUTS INPUT INTO WORK LEFT-JUSTIFIED          
         XC    KEY,KEY             PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         MVI   STAKTYPE,C'S'       STATION RECORD TYPE                          
         MVI   STAKMED,C'N'        MEDIA NETWORK                                
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         L     R4,AIO                                                           
         CLC   0(9,R4),KEY         TEST NETWORK IS ON FILE                      
         BNE   NETERR                                                           
         MVC   NETWORK,WORK                                                     
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,BNET             SAVE NETWORK MARKET NUMBER                   
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE PROGRAM                                                              
*                                                                               
VPROG    NTR1                                                                   
*                                                                               
         GOTO1 ANY                 PUTS INPUT INTO WORK LEFT-JUSTIFIED          
         XC    KEY,KEY             PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING NPGRECD,R4                                                       
         MVC   NPGKTYP,=X'0D20'    NETWORK PROGRAM RECORD                       
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,BNET                                                     
         MVC   NPGKPROG,WORK       PROGRAM NAME                                 
         GOTO1 HIGH                                                             
         B     VPROG20                                                          
*                                                                               
VPROG10  GOTO1 SEQ                                                              
*                                                                               
VPROG20  CLC   KEY(11),KEYSAVE     TEST KEYS WITHOUT END DATE                   
         BNE   PROGERR              PROGRAM DOES NOT EXIST                      
*                                                                               
         MVC   PROGRAM,WORK        IT IS                                        
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
         SPACE                                                                  
HDHK     NTR1                                                                   
         MVC   H4+10(4),NETWORK                                                 
         B     EXIT                                                             
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
NETERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NETERRMS),NETERRMS                                     
         B     ERREXIT                                                          
*                                                                               
NONETERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NONETMS),NONETMS                                       
         B     ERREXIT                                                          
*                                                                               
PROGERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRGERRMS),PRGERRMS                                     
         LA    R2,TRAPRGH                                                       
*                                                                               
ERREXIT  GOTO1 ERREX2                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         B     EXIT                                                             
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
NETERRMS DC    C'* ERROR * NETWORK NOT FOUND *'                                 
NONETMS  DC    C'* ERROR * PROGRAM REQUIRES NETWORK *'                          
PRGERRMS DC    C'* ERROR * PROGRAM NOT FOUND *'                                 
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,33,C'PROGRAM DISTRIBUTION LIST'                               
         SSPEC H2,33,C'-------------------------'                               
         SSPEC H1,65,AGYNAME                                                    
         SSPEC H2,65,AGYADD                                                     
         SSPEC H4,3,C'NETWORK'                                                  
         SSPEC H4,78,RUN                                                        
         SSPEC H4,65,REPORT                                                     
         SSPEC H5,65,REQUESTOR                                                  
         SSPEC H5,95,PAGE                                                       
         SSPEC H8,3,C'PROGRAM'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,13,C'NETWORK ADDRESS'                                         
         SSPEC H9,13,C'---------------'                                         
         SSPEC H8,46,C'NAMES'                                                   
         SSPEC H9,46,C'-----'                                                   
         SSPEC H8,79,C'NAMES'                                                   
         SSPEC H9,79,C'-----'                                                   
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
       ++INCLUDE SPTRNPRG                                                       
         PRINT OFF                                                              
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAB8D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SEQNUM   DS    XL1                                                              
FLDH     DS    XL8                                                              
FLD      DS    CL64                                                             
NETWORK  DS    CL4                                                              
BNET     DS    XL2                                                              
PROGRAM  DS    CL6                                                              
SAVEKEY  DS    XL13                                                             
         SPACE 5                                                                
* OFFLINE REPORT LINE                                                           
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL3                                                              
PPROG    DS    CL6                                                              
         DS    CL3                                                              
PNETADDR DS    CL30                                                             
         DS    CL3                                                              
PNAME1   DS    CL30                                                             
         DS    CL3                                                              
PNAME2   DS    CL30                                                             
         SPACE 3                                                                
* ONLINE LIST LINE                                                              
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTNET   DS    CL4                                                              
         DS    CL4                                                              
LSTPROG  DS    CL6                                                              
         DS    CL5                                                              
LSTNADR  DS    CL30                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SPTRA18   08/24/04'                                      
         END                                                                    
