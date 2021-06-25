*          DATA SET NESFM26    AT LEVEL 054 AS OF 09/12/12                      
*PHASE T31C26A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T31C26 - STATION/CLIENT GROUP DEFINITION MAINT/LIST   *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T31C00 (NFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE, LIST  *         
*                                                                     *         
*  INPUTS       SCREEN T31CB5 (MAINTENANCE)                           *         
*               SCREEN T31CB6 (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED GROUP DEFINITION RECORDS                      *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- FREE                                            *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - GROUP DEFINITION RECORD                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T31C26 - STATION/CLIENT GROUP DEFINITION RECORDS'               
T31C26   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C26                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*        CLI   MODE,SETFILE        SET FILE NAME                                
*        BE    SF                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
*        CLI   MODE,XRECADD        AFTER ADD                                    
*        BE    DR                                                               
*        CLI   MODE,XRECPUT        AFTER PUT                                    
*        BE    DR                                                               
         CLI   MODE,RECDEL         DELETE RECORDS                               
         BNE   LRCHK                                                            
         BAS   RE,DL                                                            
LRCHK    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         B     XIT                                                              
*                                                                               
DL       NTR1                                                                   
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* SET FILE                                                                      
SF       DS    0H                                                               
         OI    GENSTAT4,NODELLST                                                
         BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF           SET FILENAME & OTHER DEFAULTS                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R6,SVKEY            CLEAR KEY                                    
         USING GRPKEY,R6                                                        
         XC    SVKEY,SVKEY                                                      
*                                                                               
         MVI   GRPKTYP,GRPKTYPQ    RECORD TYPE                                  
         MVI   GRPKSTYP,GRPKSTYQ                                                
*                                                                               
VK10     LA    R2,SFMMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   GRPKAGMD,BAGYMD                                                  
*                                                                               
         LA    R2,SFMIDH           GROUP ID                                     
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VKX                 YES - OPTIONAL                               
         MVI   ERROR,MISSING       OTHERWISE REQUIRED                           
         B     TRAPERR                                                          
*                                                                               
VK20     TM    4(R2),X'04'         ALPHABETIC CHARACTER?                        
         BO    *+12                                                             
         MVI   ERROR,NOTALPHA                                                   
         B     TRAPERR                                                          
         MVC   GRPKID,8(R2)        PUT ID IN KEY                                
*                                                                               
VKX      XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
*                                                                               
*        BAS   RE,SAVEDEF                                                       
*        BAS   RE,SETDEF                                                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         GOTO1 REMELEM                                                          
*                                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,SFMBK1H          BREAK 1 NAME REQUIRED                        
         CLI   5(R2),0                                                          
         BE    TRAPERR                                                          
         LA    R2,SFMLN1H          BREAK 1 LENGTH REQUIRED                      
         BAS   RE,CHECKLN                                                       
         BNE   NOCHANGE            BREAK LENGTH CANNOT BE CHANGED               
         CLI   5(R2),0                                                          
         BE    TRAPERR                                                          
         TM    4(R2),X'08'         NUMERIC?                                     
         BO    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         ZIC   R3,SFMLN1                                                        
         N     R3,=X'0000000F'                                                  
         CH    R3,=H'1'            WITHIN RANGE 1..4?                           
         BL    BADLENG                                                          
         CH    R3,=H'4'                                                         
         BH    BADLENG                                                          
*                                                                               
         SR    R4,R4               WILL CONTAIN BREAK 2 LENGTH                  
         LA    R2,SFMLN2H                                                       
         BAS   RE,CHECKLN                                                       
         BNE   NOCHANGE            BREAK LENGTH CANNOT BE CHANGED               
         CLI   SFMBK2H+5,0         BREAK 2 NAME OPTIONAL                        
         BNE   *+16                                                             
         CLI   SFMLN2H+5,0                                                      
         BE    VR10                BREAK 2 LENGTH INVALID WITHOUT NAME          
         B     NOLENG                                                           
         CLI   5(R2),0             BUT REQUIRED WITH NAME                       
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         TM    4(R2),X'08'         NUMERIC?                                     
         BO    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         ZIC   R4,SFMLN2                                                        
         N     R4,=X'0000000F'                                                  
         CH    R4,=H'1'            WITHIN RANGE 1..4?                           
         BL    BADLENG                                                          
         CH    R4,=H'4'                                                         
         BH    BADLENG                                                          
         LA    R1,0(R4,R3)                                                      
         CH    R1,=H'4'                                                         
         BH    BADSUML             SUM OF LENGTHS CANNOT EXCEED FOUR            
*                                                                               
VR10     XC    ELEM,ELEM           BUILD ELEMENT                                
         LA    R6,ELEM                                                          
         USING GRPBRKD,R6                                                       
         MVI   GRPBRKCD,GRPBRKCQ                                                
         MVI   GRPBRKLN,GRPBRKLQ                                                
         MVC   GRPBK1,SFMBK1                                                    
         STC   R3,GRPBK1LN                                                      
         MVC   GRPBK2,SFMBK2                                                    
         STC   R4,GRPBK2LN                                                      
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
******************                                                              
* VALIDATE SECURITY ID'S                                                        
         MVI   ELCODE,GRPSCMCQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GRPSCMD,R6                                                       
         MVI   GRPSCMCD,GRPSCMCQ   X'12' SECURITY MANAGER ID ELEMENT            
         MVI   GRPSCMLN,GRPSCKLQ                                                
         LA    R5,GRPSCM                                                        
****  WE WILL FILL THE CLGSCM'S ONE BY ONE A BIT LATER                          
*                                                                               
         LA    R2,SFMSCM1H                                                      
         LA    R3,SFMSCM6H                                                      
*                                                                               
VR30     DS    0H                                                               
         CR    R2,R3                                                            
         BH    VR60                WE'RE DONE                                   
*                                                                               
         LR    RE,R2                                                            
         TM    1(RE),X'02'         DO WE HAVE AN EXTENDED FIELD HEADER?         
         BZ    VR40                                                             
         XR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         SHI   RE,8                POINT TO START OF EXTENDED HEADER            
         CLI   0(RE),111           IS IT ONE OF THE PID FIELDS?                 
         BNE   VR40                                                             
         CLI   5(R2),0             ANYTHING HERE?                               
         BE    VR40                 - NOPE, MOVE TO THE NEXT SCMFIELD           
         MVC   PIDNAME,8(R2)                                                    
         OC    PIDNAME,=C'        '                                             
*                                                                               
         BRAS  RE,CHECKPID         WE NEED TO CHECK ALL THE PIDS                
         BNE   BADPID                                                           
         MVC   0(L'GRPSCM,R5),PIDNUM   SAVE OFF THE PID NUMBER                  
         LA    R5,L'GRPSCM(R5)                                                  
*                                                                               
VR40     XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VR30                                                             
*                                                                               
VR60     GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         B     DR                                                               
         EJECT                                                                  
*          DATA SET NESFM31    AT LEVEL 055 AS OF 07/17/12                      
***********************************************************************         
*        CHECKPID SUBROUTINE                                          *         
***********************************************************************         
CHECKPID NTR1                                                                   
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         USING SAPEREC,R6                                                       
         XC    SAPEKEY,SAPEKEY     BUILD PERSON KEY                             
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SECALPHA                                                 
         MVC   SAPEPID,PIDNAME                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI '),=C'CTFILE ',KEY2,AIO2               
         L     R6,AIO2                                                          
         CLC   KEY2(SAPEDEF-SAPEKEY),0(R6)                                      
         BNE   CHKPIDNO                                                         
*                                                                               
         USING SAPWDD,R6                                                        
         MVI   ELCODE,SAPWDELQ     X'C4' - PERSON PASSWORD ELEM                 
         BRAS  RE,GETEL2                                                        
         BNE   CHKPIDNO                                                         
         MVC   PIDNUM,SAPWDNUM                                                  
*                                                                               
CHKPIDYS CR    RE,RE                                                            
         B     *+6                                                              
*                                                                               
CHKPIDNO LTR   RE,RE                                                            
         J     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* IF ANY GROUPS EXIST FOR THIS GROUP ID, THEN BREAK LENGTHS                     
* CANNOT BE CHANGED.                                                            
*                                                                               
CHECKLN  NTR1                                                                   
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    YES                 DON'T BOTHER CHECKING                        
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R6,KEY                                                           
         USING GRPKEY,R6                                                        
         L     R1,AIO                                                           
         MVC   GRPKEY,0(R1)        GROUP ID KEY                                 
         MVC   GRPKCODE,=X'0001'   THE LOWEST POSSIBLE GROUP CODE               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(4),KEYSAVE      ANY GROUPS FOR THIS GROUP ID?                
         BNE   YES                 NO -- OK TO CHANGE BREAK LENGTHS             
*                                                                               
         LR    R1,R2               R1 = A(THIS LENGTH FIELD)                    
         ZIC   R0,0(R1)                                                         
         AR    R1,R0               R1 = A(ASSOCIATED INVISIBLE FIELD)           
         CLC   8(1,R2),8(R1)       WAS FIELD CHANGED?                           
         BE    YES                 NO                                           
*                                                                               
         MVC   8(1,R2),8(R1)       YES -- RESTORE OLD LENGTH                    
         OI    6(R2),X'80'         XMIT                                         
         B     NO                                                               
         EJECT                                                                  
         DROP  R6                                                               
*                                                                               
YES      SR    R0,R0               THEN THERE IS KEY DATA INPUT                 
         B     *+8                                                              
NO       LA    R0,1                                                             
         LTR   R0,R0                                                            
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
***************************************************************                 
* DISPLAY RECORD                                                                
*                                                                               
DR       CLI   ACTNUM,ACTSEL       SELECT FROM LIST?                            
         BNE   DR20                                                             
         LA    R3,LISTDIR          LIST SCREEN INFO                             
         CLI   SELLISTN,0          FIRST ON LIST SCREEN?                        
         BE    DR10                                                             
         ZIC   R0,SELLISTN         RELATIVE LINE NUMBER                         
         LA    R3,6(R3)            NEXT LIST ENTRY                              
         BCT   R0,*-4                                                           
DR10     CLI   0(R3),C'D'          ACTION DELETE?                               
         BE    DRX                 DON'T DISPLAY RECORD ON DELETE               
*                                                                               
DR20     L     R6,AIO                                                           
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING GRPBRKD,R6                                                       
         MVC   SFMBK1,GRPBK1       NAMES                                        
         OI    SFMBK1H+6,X'80'                                                  
         MVC   SFMBK2,GRPBK2                                                    
         OI    SFMBK2H+6,X'80'                                                  
         EDIT  GRPBK1LN,(1,SFMLN1) LENGTHS                                      
         OI    SFMLN1H+6,X'80'                                                  
         MVC   SFMZLN1,SFMLN1                                                   
         XC    SFMLN2,SFMLN2                                                    
         OC    GRPBK2LN,GRPBK2LN                                                
         BZ    DR30                                                             
         EDIT  GRPBK2LN,(1,SFMLN2)                                              
DR30     OI    SFMLN2H+6,X'80'                                                  
         MVC   SFMZLN2,SFMLN2                                                   
         DROP  R6                                                               
*                                                                               
*--DISPLAY PIDS                                                                 
         LA    R2,SFMSCM1H                                                      
         LA    R1,SFMSCM6H                                                      
*                                                                               
DR50     CR    R2,R1               DONE WITH THE FIELDS?                        
         BH    DR60                 - YUP, WE'RE DONE                           
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    DR55                                                             
         XC    8(8,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         RETRANSMIT                                   
*                                                                               
         XR    R0,R0                                                            
DR55     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR50                                                             
*                                                                               
*****      NEW SECURITY MANAGER ID ELEMENT      MHC  03/01/06                   
DR60     BAS   RE,SECMGREL                                                      
*                                                                               
DRX      BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF                                                        
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
****  CODE COPIED FROM SPSFM15 ADDRESS RECORD MAINTENANCE  MHC 03/01/06         
***********************************************************************         
*        SECMGREL SUBROUTINE                                          *         
***********************************************************************         
*****      NEW SECURITY MANAGER ID ELEMENT      MHC  03/01/06                   
SECMGREL NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,GRPSCMCQ     SECURITY MANAGER ID ELEMENT                  
         BRAS  RE,GETEL                                                         
         BNE   SMELX                                                            
*                                                                               
         USING GRPSCMD,R6                                                       
         LA    R2,SFMSCM1H                                                      
         LA    R3,SFMSCM6H                                                      
         LA    R4,GRPSCM1                                                       
*                                                                               
SECMGR10 OC    0(2,R4),0(R4)       ANYTHING IN THE CLGSCM?                      
         BZ    SMELX                - NOPE, WE'RE DONE                          
*                                                                               
         CR    R2,R3                                                            
         BH    SMELX               WE'RE DONE                                   
*                                                                               
         LR    RE,R2                                                            
         TM    1(RE),X'02'         DO WE HAVE AN EXTENDED FIELD HEADER?         
         BZ    SECMGRNX                                                         
         XR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         SHI   RE,8                POINT TO START OF EXTENDED HEADER            
         CLI   0(RE),111           IS IT ONE OF THE PID FIELDS?                 
         BNE   SECMGRNX             - NOPE                                      
*                                                                               
         GOTO1 SECMGRID,DMCB,(R2),(R4)                                          
*                                                                               
         LA    R4,L'GRPSCM(R4)     BUMP TO NEXT SECURITY MANAGER ID             
*                                                                               
SECMGRNX XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     SECMGR10                                                         
*                                                                               
SMELX    DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        SECMGRID SUBROUTINE                                          *         
*        P1 = SCREEN HEADER                                           *         
*        P2 = PID                                                     *         
***********************************************************************         
SECMGRID NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R4,4(R1)                                                         
* PERSONAL ID                                                                   
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING AUTHD,R3                                                         
         MVC   SECRAGY,SECALPHA                                                 
         MVC   PASSWD,0(R4)                                                     
         MVC   AIO,AIO2                                                         
         GOTO1 VALIAUTH,DMCB,WORK   GET PERSONAL ID                             
         MVC   AIO,AIO1                                                         
         MVC   8(8,R2),PRSNLID                                                  
         MVI   5(R2),8             INPUT LENGTH OF 8                            
         OI    6(R2),X'80'         RETRANSMIT                                   
         DROP  R3                                                               
*                                                                               
SMIDX    DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
********************====== DISPLAY KEY =========*****************               
DK       L     R6,AIO              RECORD SELECTED                              
         USING GRPKEY,R6                                                        
*                                                                               
         MVC   SFMMED,QMED         MEDIA                                        
         OI    SFMMEDH+6,X'80'                                                  
         MVC   SFMID,GRPKID        GROUP ID                                     
         OI    SFMIDH+6,X'80'                                                   
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
******************************************************************              
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R4,KEY                                                           
         USING GRPKEY,R4                                                        
*                                                                               
         OC    KEY(13),KEY         FIRST TIME THROUGH?                          
         BNZ   LR20                                                             
         MVI   GRPKTYP,GRPKTYPQ    RECORD TYPE                                  
         MVI   GRPKSTYP,GRPKSTYQ                                                
*                                                                               
LR10     MVC   GRPKAGMD,BAGYMD     AGY/MED                                      
         MVC   GRPKID,SFMID        PUT ID FILTER IN, IF ANY                     
         MVC   SAVEKEY,KEY         SAVE THE LIST FILTER                         
*                                                                               
LR20     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(3),SAVEKEY      SAME TYPE/AGY/MED?                           
         BNE   LRX                                                              
         OC    KEY+4(9),KEY+4      GROUP DESCRIPTION RECORD?                    
         BNZ   LR50                                                             
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         MVC   LSTID,GRPKID        GROUP ID                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         USING GRPBRKD,R6                                                       
         MVC   LSTBK1,GRPBK1       BREAK 1                                      
         MVI   LSTOPRN1,C'('                                                    
         MVC   LSTBKLN1,GRPBK1LN                                                
         OI    LSTBKLN1,X'F0'                                                   
         MVI   LSTCPRN1,C')'                                                    
*                                                                               
         CLI   GRPBK2LN,0          BREAK 2 (MAY NOT BE THERE)                   
         BE    LR30                                                             
         MVC   LSTBK2,GRPBK2                                                    
         MVI   LSTOPRN2,C'('                                                    
         MVC   LSTBKLN2,GRPBK2LN                                                
         OI    LSTBKLN2,X'F0'                                                   
         MVI   LSTCPRN2,C')'                                                    
         DROP  R6                                                               
*                                                                               
LR30     GOTO1 LISTMON             DISPLAY LINE                                 
*                                                                               
LR50     LA    R4,KEY                                                           
         XC    KEY+4(9),KEY+4                                                   
         ZIC   R1,KEY+3            GROUP ID                                     
         LA    R1,1(R1)                                                         
         STC   R1,KEY+3            FORCE NEXT KEY                               
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
******************************************************                          
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DEL ALLOWED                               
         OI    GENSTAT2,DISTHSPG                                                
*                                                                               
*  GET SECURITY AGENCY                                                          
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SECALPHA,FATAGYSC   SAVE SECURITY AGENCY                         
SETUPX   B     XIT                                                              
         DROP  R3,RF                                                            
*                                                                               
         EJECT                                                                  
*******************************************************************             
SAVEDEF  DS    0H                  SAVE DEFINITION BEFORE SETDEF                
         MVC   MYSYSDIR,SYSDIR                                                  
         MVC   MYSYSFIL,SYSFIL                                                  
*        MVC   MYUSEIO,USEIO                                                    
*        MVC   MYACELOP,ACTELOPT                                                
         MVC   MYLKEY,LKEY                                                      
         BR    RE                                                               
*                                                                               
SETDEF   MVC   SYSDIR,=C'SPTDIR  '      SET TO READ STATION FILE                
         MVC   SYSFIL,=C'SPTFILE '                                              
*        MVI   USEIO,0                                                          
*        MVI   ACTELOPT,C'N'            NO ACTIVITY ELEMENTS                    
         MVC   LKEY,=H'13'              SET LENGTH OF STATION KEY               
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         BR    RE                                                               
*                                                                               
RSTRDEF  DS    0H                  RESTORE DEFINITION AFTER SETDEF              
         MVC   SYSDIR,MYSYSDIR                                                  
         MVC   SYSFIL,MYSYSFIL                                                  
*        MVC   USEIO,MYUSEIO                                                    
*        MVC   ACTELOPT,MYACELOP                                                
         MVC   LKEY,MYLKEY                                                      
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
                                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
CTDISP   DC    Y(L'SAPEKEY+L'SAPELEN+L'SAPESTAT)                                
         GETEL2 R6,CTDISP,ELCODE                                                
         EJECT                                                                  
* SUB-ROUTINES FOR ELEMENT MAINTENANCE                                          
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(ELCODE,0(R7)),0                   
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
PUTEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFILE'),0(R7),ELEM                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE 2                                                                
BADLENG  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADLENGM),BADLENGM                                     
         GOTO1 ERREX2                                                           
BADLENGM DC    C'* ERROR * LENGTH MUST BE WITHIN RANGE 1 TO 4'                  
         SPACE 2                                                                
NOLENG   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOLENGM),NOLENGM                                       
         GOTO1 ERREX2                                                           
NOLENGM  DC    C'* ERROR * LENGTH INVALID WITHOUT BREAK NAME'                   
         SPACE 2                                                                
BADSUML  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADSUMLM),BADSUMLM                                     
         GOTO1 ERREX2                                                           
BADSUMLM DC    C'* ERROR * SUM OF BREAK LENGTHS MAY NOT EXCEED 4'               
         SPACE 2                                                                
NOCHANGE XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCHANGM),NOCHANGM                                     
         GOTO1 ERREX2                                                           
NOCHANGM DC    C'* ERROR * BREAK LENGTHS CANNOT BE CHANGED'                     
         SPACE 2                                                                
BADPID   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADPIDNM),BADPIDNM                                     
         GOTO1 ERREX2                                                           
BADPIDNM DC    C'* ERROR * USER ID DOES NOT EXIST'                              
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE NESFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMB5D                                                       
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMB6D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE NESFMWORKD                                                     
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 5                                                                
* WORK AREA                                                                     
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
MYSYSDIR DS    CL(L'SYSDIR)        SAVED SYSDIR VALUE BEFORE SETDEF RTN         
MYSYSFIL DS    CL(L'SYSFIL)          "   SYSFIL   "     "      "     "          
MYUSEIO  DS    CL(L'USEIO)           "   USEIO    "     "      "     "          
MYACELOP DS    CL(L'ACTELOPT)        "   ACTELOPT "     "      "     "          
MYLKEY   DS    CL(L'LKEY)            "   LKEY     "     "      "     "          
*                                                                               
SAVEKEY  DS    XL48                                                             
*                                                                               
PIDNAME  DS    CL8                                                              
PIDNUM   DS    CL2                                                              
SECALPHA DS    CL2                  SECURITY AGENCY ALPHA                       
KEY2     DS    CL50                                                             
         EJECT                                                                  
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTID    DS    C                                                                
         DS    CL4                                                              
LSTBK1   DS    CL12                                                             
         DS    C                                                                
LSTOPRN1 DS    C                                                                
LSTBKLN1 DS    C                                                                
LSTCPRN1 DS    C                                                                
         DS    CL3                                                              
LSTBK2   DS    CL12                                                             
         DS    C                                                                
LSTOPRN2 DS    C                                                                
LSTBKLN2 DS    C                                                                
LSTCPRN2 DS    C                                                                
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054NESFM26   09/12/12'                                      
         END                                                                    
