*          DATA SET SPSFM25    AT LEVEL 008 AS OF 09/26/13                      
*PHASE T21725A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T21725 - STATION/CLIENT GROUP DEFINITION MAINT/LIST   *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE, LIST  *         
*                                                                     *         
*  INPUTS       SCREEN T217C5 (MAINTENANCE)                           *         
*               SCREEN T217B5 (LIST)                                  *         
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
         TITLE 'T21725 - STATION/CLIENT GROUP DEFINITION RECORDS'               
T21725   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21725                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
****  CODE COPIED FROM SPSFM15 ADDRESS RECORD MAINTENANCE  MHC 03/01/06         
         BRAS  RE,INIT                                                          
****  CODE COPIED FROM SPSFM15 ADDRESS RECORD MAINTENANCE  MHC 03/01/06         
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,XRECADD        AFTER ADD                                    
         BE    DR                                                               
         CLI   MODE,XRECPUT        AFTER PUT                                    
         BE    DR                                                               
*                                                                               
         CLI   MODE,RECDEL         DELETE NOT ALLOWED                           
         BNE   *+16                                                             
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
XIT      ANSR                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       DS    0H                                                               
         CLI   ACTNUM,ACTLIST      IS IT LIST/REPORT? (REPORT=12>LIST)          
         BNL   VK00X                YEAH IT IS, NO NEED FOR NONSENSE            
*                                                                               
         CLI   RECNUM,36           CLIENT GROUP DEFINITION?                     
         BNE   VK00A                                                            
         BRAS  RE,TURNON           TURN ON SOME FIELDS AND OPEN UP!             
         B     VK00G                                                            
*                                                                               
VK00A    CLI   RECNUM,33           STATION GROUP DEFINITION?                    
         BE    *+6                                                              
         DC    H'0'                WHAT THE HECK IS IT??                        
         BRAS  RE,TURNOFF          TURN OFF ALL THE STUPID PID FIELDS           
*                                                                               
VK00G    BAS   RE,DISPSMG          DISPLAY THE SECURITY FIELDS                  
*                                                                               
VK00X    LA    R6,SVKEY            CLEAR KEY                                    
         USING GRPKEY,R6                                                        
         XC    SVKEY,SVKEY                                                      
*                                                                               
         MVI   GRPKTYP,GRPKTYPQ    RECORD TYPE                                  
*                                                                               
         MVI   GRPKSTYP,GRPKCTYQ                                                
         CLI   RECNUM,36           CLIENT GROUP DEFINITION?                     
         BE    VK10                                                             
*                                                                               
         MVI   GRPKSTYP,GRPKSTYQ                                                
         CLI   RECNUM,33           STATION GROUP DEFINITION?                    
         BE    VK10                                                             
         DC    H'0'                HOW DID WE GET HERE?                         
*                                                                               
VK10     LA    R2,SFMMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         OI    6(R2),X'80'         RETRANSMIT                                   
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
VK20     MVI   ERROR,NOTALPHA                                                   
         TM    4(R2),X'04'         VALID ALPHA                                  
         BZ    TRAPERR                                                          
         MVC   GRPKID,8(R2)        PUT ID IN KEY                                
         CLI   RECNUM,36           CLIENT GROUP DEFINITION?                     
         BNE   VKX                                                              
* TRANSLATE 2 CHAR CODE TO ONE CHAR                                             
         MVC   WORK(2),8(R2)                                                    
         OI    WORK+1,C' '                                                      
         LA    RE,SPCGRTAB                                                      
         LHI   RF,SPCGRTBX-SPCGRTAB                                             
*                                                                               
VK22     CLC   WORK(2),0(RE)                                                    
         BE    VK24                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,VK22                                                          
         B     BADGRPID                                                         
*                                                                               
VK24     MVC   GRPKID,2(RE)        SET 1 BYTE GROUP CODE                        
         OI    6(R2),X'80'         RETRANSMIT                                   
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
DISPSMG  OI    SFMSMIDH+6,X'80'                                                 
         OI    SFMSMI1H+6,X'80'                                                 
         OI    SFMSMI2H+6,X'80'                                                 
         OI    SFMSMI3H+6,X'80'                                                 
         OI    SFMSMI4H+6,X'80'                                                 
         OI    SFMSMI5H+6,X'80'                                                 
         OI    SFMSMI6H+6,X'80'                                                 
*                                                                               
         OI    SFMSCM1H+6,X'80'                                                 
         OI    SFMSCM2H+6,X'80'                                                 
         OI    SFMSCM3H+6,X'80'                                                 
         OI    SFMSCM4H+6,X'80'                                                 
         OI    SFMSCM5H+6,X'80'                                                 
         OI    SFMSCM6H+6,X'80'                                                 
*                                                                               
         OI    SFMBTKH+6,X'80'                                                  
*                                                                               
         BR    RE                                                               
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
*                                                                               
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
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
         CHI   R3,1                WITHIN RANGE 1..4?                           
         BL    BADLENG                                                          
         CHI   R3,4                                                             
         BH    BADLENG                                                          
*                                                                               
         SR    R4,R4               WILL CONTAIN BREAK 2 LENGTH                  
         LA    R2,SFMLN2H                                                       
         BAS   RE,CHECKLN                                                       
         BNE   NOCHANGE            BREAK LENGTH CANNOT BE CHANGED               
*                                                                               
         CLI   SFMBK2H+5,0         BREAK 2 NAME OPTIONAL                        
         BNE   *+16                                                             
         CLI   SFMLN2H+5,0                                                      
         BE    VR10                BREAK 2 LENGTH INVALID WITHOUT NAME          
         B     NOLENG                                                           
*                                                                               
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
         CHI   R4,1                WITHIN RANGE 1..4?                           
         BL    BADLENG                                                          
         CHI   R4,4                                                             
         BH    BADLENG                                                          
         LA    R1,0(R4,R3)                                                      
         CHI   R1,4                                                             
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
*                                                                               
VR50     DS    0H                                                               
         MVI   ELCODE,GRPSCMCQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GRPSCMD,R6                                                       
         MVI   GRPSCMCD,GRPSCMCQ   X'12' SECURITY MANAGER ID ELEMENT            
         MVI   GRPSCMLN,GRPSCKLQ                                                
         LA    R5,GRPSCM                                                        
****  WE WILL FILL THE GRPSCM'S ONE BY ONE A BIT LATER                          
*                                                                               
         LA    R2,SFMSCM1H                                                      
         LA    R3,SFMSCM6H                                                      
*                                                                               
VR60     DS    0H                                                               
         CR    R2,R3                                                            
         BH    VR80                WE'RE DONE                                   
*                                                                               
         LR    RE,R2                                                            
         TM    1(RE),X'02'         DO WE HAVE AN EXTENDED FIELD HEADER?         
         BZ    SCMNEXT                                                          
         XR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         SHI   RE,8                POINT TO START OF EXTENDED HEADER            
         CLI   0(RE),111           IS IT ONE OF THE PID FIELDS?                 
         BNE   SCMNEXT                                                          
         CLI   5(R2),0             ANYTHING HERE?                               
         BE    SCMNEXT              - NOPE, MOVE TO THE NEXT SCMFIELD           
         MVC   PIDNAME,8(R2)                                                    
         OC    PIDNAME,=C'        '                                             
*                                                                               
         BRAS  RE,CHECKPID         WE NEED TO CHECK ALL THE PIDS                
         BNE   BADPID                                                           
         MVC   0(L'GRPSCM,R5),PIDNUM   SAVE OFF THE PID NUMBER                  
         LA    R5,L'GRPSCM(R5)                                                  
*                                                                               
SCMNEXT  XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VR60                                                             
*                                                                               
VR80     GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         LA    R2,SFMBTKH         FOR BUYTRACKER FIELD HEADER                   
         CLI   5(R2),0             ANYTHING HERE?                               
         BE    VR85                NONE, CLEAR FLAG IN THE ELEM                 
         CLI   8(R2),C'N'                                                       
         BE    VR85                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   ERRINV              YES, NO, OR NOTHING                          
VR85     L     R1,AIO              BECAUSE X'01' ELEM ADDED TO AIO              
         LA    R1,GRPEL-GRPKEY(R1) WE HAVE TO JUST MODIFY DATA IN IT            
         CLI   0(R1),GRPBRKCQ      X'10' - BREAK DESC ELEM                      
         BE    *+6                                                              
         DC    H'0'                IT HAD BETTER BE                             
         MVC   GRPBKBTK-GRPBRKD(1,R1),8(R2)                                     
*                                                                               
VRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        TURNON - TURN THEM (PIDS) ALL ON, BABY!                      *         
***********************************************************************         
TURNON   NTR1                                                                   
         NI    SFMSMIDH+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         NI    SFMSMI1H+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         NI    SFMSMI2H+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         NI    SFMSMI3H+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         NI    SFMSMI4H+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         NI    SFMSMI5H+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         NI    SFMSMI6H+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
*                                                                               
         NI    SFMSCM1H+1,X'FF'-X'20'   TAKE OFF PROTECTION                     
         NI    SFMSCM2H+1,X'FF'-X'20'   TAKE OFF PROTECTION                     
         NI    SFMSCM3H+1,X'FF'-X'20'   TAKE OFF PROTECTION                     
         NI    SFMSCM4H+1,X'FF'-X'20'   TAKE OFF PROTECTION                     
         NI    SFMSCM5H+1,X'FF'-X'20'   TAKE OFF PROTECTION                     
         NI    SFMSCM6H+1,X'FF'-X'20'   TAKE OFF PROTECTION                     
         NI    SFMSCM1H+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         NI    SFMSCM2H+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         NI    SFMSCM3H+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         NI    SFMSCM4H+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         NI    SFMSCM5H+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         NI    SFMSCM6H+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        TURNOFF - OH PLEASE, TURN THEM OFF!!                         *         
***********************************************************************         
TURNOFF  NTR1                                                                   
         OI    SFMSMIDH+1,X'0C'    ZERO THIS THING UP PLEASE!                   
         OI    SFMSMI1H+1,X'0C'    ZERO THIS THING UP PLEASE!                   
         OI    SFMSMI2H+1,X'0C'    ZERO THIS THING UP PLEASE!                   
         OI    SFMSMI3H+1,X'0C'    ZERO THIS THING UP PLEASE!                   
         OI    SFMSMI4H+1,X'0C'    ZERO THIS THING UP PLEASE!                   
         OI    SFMSMI5H+1,X'0C'    ZERO THIS THING UP PLEASE!                   
         OI    SFMSMI6H+1,X'0C'    ZERO THIS THING UP PLEASE!                   
*                                                                               
         OI    SFMSCM1H+1,X'20'    GIMME SOME PROTECTION!                       
         OI    SFMSCM2H+1,X'20'    GIMME SOME PROTECTION!                       
         OI    SFMSCM3H+1,X'20'    GIMME SOME PROTECTION!                       
         OI    SFMSCM4H+1,X'20'    GIMME SOME PROTECTION!                       
         OI    SFMSCM5H+1,X'20'    GIMME SOME PROTECTION!                       
         OI    SFMSCM6H+1,X'20'    GIMME SOME PROTECTION!                       
         OI    SFMSCM1H+1,X'0C'    JUST ZERO IT UP MAN!                         
         OI    SFMSCM2H+1,X'0C'    JUST ZERO IT UP MAN!                         
         OI    SFMSCM3H+1,X'0C'    JUST ZERO IT UP MAN!                         
         OI    SFMSCM4H+1,X'0C'    JUST ZERO IT UP MAN!                         
         OI    SFMSCM5H+1,X'0C'    JUST ZERO IT UP MAN!                         
         OI    SFMSCM6H+1,X'0C'    JUST ZERO IT UP MAN!                         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
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
         BRAS  RE,GETEL                                                         
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
*                                                                               
         MVC   SFMBTK,GRPBKBTK                                                  
         OI    SFMBTKH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
DR40     DS    0H                                                               
         LA    R2,SFMSCM1H                                                      
         LA    R1,SFMSCM6H                                                      
*                                                                               
DR40G    CR    R2,R1               DONE WITH THE FIELDS?                        
         BH    DR45                 - YUP, WE'RE DONE                           
         TM    1(R2),X'20'         ARE WE PROTECTED?                            
         BNZ   DR40NX               - YES, DON'T TOUCH                          
         XC    8(8,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         RETRANSMIT                                   
*                                                                               
DR40NX   XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR40G                                                            
*                                                                               
*****      NEW SECURITY MANAGER ID ELEMENT      MHC  03/01/06                   
DR45     BRAS  RE,SECMGREL                                                      
*                                                                               
DR50     L     R6,AIO              *** THIS IS THE ENTRY FOR XREC ***           
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BRAS  RE,GETEL            MUST BE THERE                                
         BNE   DR70                END IT IF NOT THERE, FOR NOW                 
*        BE    *+6                                                              
*        DC    H'0'                                                             
         USING ACTVD,R6                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(5,SFMCDTE)   ADDED DATE                
         OI    SFMCDTEH+6,X'80'                                                 
*                                                                               
         OC    ACTVCHDT,ACTVCHDT   HAS RECORD BEEN CHANGED BEFORE?              
         BZ    DR60                 NO, SKIP NEXT DATCON CALL                   
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,SFMADTE)   CHANGED DATE              
         OI    SFMADTEH+6,X'80'                                                 
*                                                                               
***   WE'RE BORROWING THE SECMGRID SUBROUTINE                                   
DR60     GOTO1 SECMGRID,DMCB,SFMCRTRH,ACTVADID                                  
*                                                                               
         GOTO1 SECMGRID,DMCB,SFMCWHOH,ACTVCHID                                  
*                                                                               
DR70     DS    0H                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO              RECORD SELECTED                              
         USING GRPKEY,R6                                                        
*                                                                               
         MVC   SFMMED,QMED         MEDIA                                        
         OI    SFMMEDH+6,X'80'                                                  
*                                                                               
         XC    SFMID,SFMID                                                      
         MVC   SFMID(1),GRPKID     GROUP ID                                     
         OI    SFMIDH+6,X'80'                                                   
*                                                                               
         CLI   RECNUM,36           CLIENT GROUP DEFN ?                          
         BNE   DKX                                                              
         BAS   RE,TURNON           TURN ON SOME FIELDS AND OPEN UP!             
         BAS   RE,DISPSMG          DISPLAY THE SECURITY FIELDS                  
*                                                                               
* TRANSLATE 1 CHAR CODE TO DISPLAYABLE 2 CHARS                                  
*                                                                               
         LA    RE,SPCGRTAB                                                      
         LHI   RF,SPCGRTBX-SPCGRTAB                                             
*                                                                               
DK12     CLC   GRPKID,2(RE)                                                     
         BE    DK14                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,DK12                                                          
         B     BADGRPID                                                         
*                                                                               
DK14     MVC   SFMID,0(RE)         SET 1 BYTE GROUP CODE                        
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R4,KEY                                                           
         USING GRPKEY,R4                                                        
*                                                                               
         OC    KEY(13),KEY         FIRST TIME THROUGH?                          
         BNZ   LR30                                                             
*                                                                               
LR2      LA    R0,SORTDATA                                                      
         LHI   R1,SORTDATX-SORTDATA                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   SORTLAST,0                                                       
         LA    R5,SORTDATA                                                      
*                                                                               
         MVI   GRPKTYP,GRPKTYPQ    RECORD TYPE                                  
*                                                                               
         MVI   GRPKSTYP,GRPKCTYQ                                                
         CLI   RECNUM,36           CLIENT GROUP DEFINITION?                     
         BE    LR10                                                             
*                                                                               
         MVI   GRPKSTYP,GRPKSTYQ                                                
         CLI   RECNUM,33           STATION GROUP DEFINITION?                    
         BE    LR10                                                             
         DC    H'0'                HOW DID WE GET HERE?                         
*                                                                               
LR10     MVC   GRPKAGMD,BAGYMD     AGY/MED                                      
         MVC   SAVEKEY,KEY         SAVE TYPE/AGYMD                              
*                                                                               
LR12     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
         CLC   KEY(3),SAVEKEY      SAME TYPE/AGYMD                              
         BNE   LR20                                                             
         OC    KEY+4(9),KEY+4      GROUP DESCRIPTION RECORD?                    
         BZ    *+6                                                              
         DC    H'0'                                                             
* TRANSLATE 1 CHAR CODE TO 2 CHARS                                              
         LA    RE,SPCGRTAB                                                      
         LHI   RF,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
LR14     CLC   GRPKID,2(RE)                                                     
         BE    LR16                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,LR14                                                          
         DC    H'0'                                                             
*                                                                               
LR16     CLC   0(2,RE),SFMID       COMPARE TO START VALUE                       
         BL    LR18                                                             
         MVC   0(3,R5),0(RE)       SAVE BOTH CODES                              
         AHI   R5,3                                                             
*                                                                               
LR18     MVC   KEY+4(9),=9X'FF'    FORCE NEXT GROUP                             
         B     LR12                                                             
*                                                                               
LR20     LR    R1,R5                                                            
         LA    R0,SORTDATA                                                      
         SR    R1,R0               GIVES LENGTH USED                            
         BZ    LRX                                                              
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         LR    R0,R1                                                            
*                                                                               
         GOTO1 QSORT,DMCB,SORTDATA,(R0),3,2,0                                   
         EJECT                                                                  
*=============================================================                  
* NOW DISPLAY DATA FROM SORTED LIST                                             
*=============================================================                  
                                                                                
LR30     LA    R5,SORTDATA         FIND LAST ENTRY DISPLAYED                    
         LHI   R0,(SORTDATX-SORTDATA)/3  LOOP PREVENTION                        
         CLI   SORTLAST,0                                                       
         BE    LR34                                                             
*                                                                               
LR32     CLC   SORTLAST,2(R5)                                                   
         BE    LR34                                                             
         AHI   R5,3                                                             
         BCT   R0,LR32                                                          
         DC    H'0'                                                             
*                                                                               
LR34     MVC   SORTLAST,2(R5)      SET START POINT                              
*                                                                               
         OC    0(3,R5),0(R5)       TEST MORE DATA                               
         BZ    LRX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SAVEKEY                                                      
         MVC   GRPKID,2(R5)        MOVE 1 CHAR CODE                             
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         MVC   LSTID,0(R5)         2 CHAR GROUP ID                              
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         BRAS  RE,GETEL                                                         
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
         BE    LR38                                                             
         MVC   LSTBK2,GRPBK2                                                    
         MVI   LSTOPRN2,C'('                                                    
         MVC   LSTBKLN2,GRPBK2LN                                                
         OI    LSTBKLN2,X'F0'                                                   
         MVI   LSTCPRN2,C')'                                                    
         DROP  R6                                                               
*                                                                               
LR38     GOTO1 LISTMON             DISPLAY LINE                                 
*                                                                               
         AHI   R5,3                                                             
         OC    0(3,R5),0(R5)                                                    
         BNZ   LR34                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
CTDISP   DC    Y(L'SAPEKEY+L'SAPELEN+L'SAPESTAT)                                
         GETEL2 R6,CTDISP,ELCODE                                                
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
BADLENG  BAS   R1,MYERR                                                         
         DC    AL1(BADLENX-*-1)                                                 
         DC    C'LENGTH MUST BE WITHIN RANGE 1 TO 4'                            
BADLENX  EQU   *                                                                
*                                                                               
BADGRPID BAS   R1,MYERR                                                         
         DC    AL1(BADGRPX-*-1)                                                 
         DC    C'GROUP ID IS NOT VALID'                                         
BADGRPX  EQU   *                                                                
*                                                                               
NOLENG   BAS   R1,MYERR                                                         
         DC    AL1(NOLENGX-*-1)                                                 
         DC    C'LENGTH INVALID WITHOUT BREAK NAME'                             
NOLENGX  EQU   *                                                                
*                                                                               
BADSUML  BAS   R1,MYERR                                                         
         DC    AL1(BADSUMX-*-1)                                                 
         DC    C'SUM OF BREAK LENGTHS MAY NOT EXCEED 4'                         
BADSUMX  EQU   *                                                                
*                                                                               
NOCHANGE BAS   R1,MYERR                                                         
         DC    AL1(NOCHANGX-*-1)                                                
         DC    C'BREAK LENGTHS CANNOT BE CHANGED'                               
NOCHANGX EQU   *                                                                
*                                                                               
BADPID   BAS   R1,MYERR                                                         
         DC    AL1(BADPIDX-*-1)                                                 
         DC    C'USED ID DOES NOT EXIST'                                        
BADPIDX  EQU   *                                                                
*                                                                               
MYERR    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONHEAD+10(0),1(R1)                                              
         GOTO1 ERREX2                                                           
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
****  CODE COPIED FROM SPSFM15 ADDRESS RECORD MAINTENANCE  MHC 03/01/06         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                               INITIALIZE                            *         
*  GETFACT CALL                                                       *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
INIT     NTR1                                                                   
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SECALPHA,FATAGYSC   SAVE SECURITY AGENCY                         
         DROP  R3                                                               
INITX    J     EXIT                                                             
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
SECMGR10 OC    0(2,R4),0(R4)       ANYTHING IN THE GRPSCM?                      
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
         J     EXIT                                                             
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
         J     EXIT                                                             
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
EXIT     XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
         EJECT                                                                  
       ++INCLUDE SPCGRTAB                                                       
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMC5D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* WORK AREA - 6304 BYTES FROM SVSTART GIVES US ABOUT 2K                         
*                                                                               
         ORG   SYSSPARE                                                         
SAVEKEY  DS    XL48                                                             
*                                                                               
SORTLAST DS    XL1                 LAST 1-BYTE CODE DISPLAYED                   
SORTDATA DS    XL768               MAX 256 3 BYTE CODES                         
SORTDATX EQU   *                                                                
PIDNAME  DS    CL8                                                              
PIDNUM   DS    XL2                                                              
SECALPHA DS    CL2                 SECURITY AGENCY ALPHA                        
KEY2     DS    CL50                                                             
         EJECT                                                                  
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTID    DS    CL2                                                              
         DS    CL3                                                              
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
**PAN#1  DC    CL21'008SPSFM25   09/26/13'                                      
         END                                                                    
