*          DATA SET NESFM27    AT LEVEL 009 AS OF 09/12/12                      
*PHASE T31C27A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T31C27 - STATION/CLIENT GROUP DEFINITION MAINT/LIST   *         
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
         TITLE 'T31C27 - STATION/CLIENT GROUP DEFINITION RECORDS'               
T31C27   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C27,R7,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
         CLI   MODE,SETFILE        SET FILE NAME                                
         BE    SF                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,XRECADD        AFTER ADD                                    
         BE    XR                                                               
         CLI   MODE,XRECPUT        AFTER PUT                                    
         BE    XR                                                               
*        CLI   MODE,RECDEL         DELETE RECORDS                               
*        BNE   LRCHK                                                            
*        BAS   RE,DL                                                            
LRCHK    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         LA    RE,HEDSPECS                                                      
         ST    RE,SPECS                                                         
         LA    RE,HDHK                                                          
         ST    RE,HEADHOOK                                                      
         B     LR                                                               
*                                                                               
*L       NTR1                                                                   
*        MVI   ERROR,INVACT                                                     
*        LA    R2,CONACTH                                                       
*        B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* SET FILE                                                                      
SF       DS    0H                                                               
*        OI    GENSTAT4,NODELLST                                                
         BAS   RE,SAVEDEF                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       XC    SVKEY,SVKEY                                                      
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET SECURITY ALPHA                           
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SECALPHA,FATAGYSC   SAVE SECURITY AGENCY                         
         MVC   PIDNUM,FAPASSWD     2 CHARACTER PID                              
         DROP  R3                                                               
*                                                                               
         LA    R4,SVKEY            CLEAR KEY                                    
         USING GRPKEY,R4                                                        
*                                                                               
         MVI   GRPKTYP,GRPKTYPQ    RECORD TYPE                                  
         MVI   GRPKSTYP,GRPKSTYQ                                                
*                                                                               
         OC    SVSTRT,SPACES                                                    
         LA    R2,SFSSTRTH                                                      
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         MVC   SVSTRT,SFSSTRT                                                   
*                                                                               
VK20     LA    R2,SFSMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   GRPKAGMD,BAGYMD                                                  
*                                                                               
*                                                                               
VK25     LA    R2,SFSIDH           GROUP ID                                     
         CLI   5(R2),0             ENTERED                                      
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         TM    4(R2),X'04'         ALPHABETIC CHARACTER?                        
         BO    *+12                                                             
         MVI   ERROR,NOTALPHA                                                   
         B     TRAPERR                                                          
         MVC   GRPID,8(R2)                                                      
*                                                                               
         MVC   GRPKID,GRPID        PUT ID IN KEY                                
         MVC   KEY(13),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
VGD      MVC   AIO,AIO2            READ GROUP DEFINITION REC INTO IO2           
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING GRPBRKD,R6                                                       
         MVC   BREAK1LN,GRPBK1LN   SAVE BREAK LENGTHS                           
         MVC   BREAK2LN,GRPBK2LN                                                
*                                                                               
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   VK28                                                             
         MVC   SFLBK1,GRPBK1                                                    
         MVI   SFLBK1L,C'('                                                     
         MVC   SFLBK1L+1(1),GRPBK1LN                                            
         OI    SFLBK1L+1,X'F0'                                                  
         MVI   SFLBK1L+2,C')'                                                   
         OI    SFLBK1H+6,X'80'                                                  
         OI    SFLBK1LH+6,X'80'                                                 
*                                                                               
         MVC   SFLBK2,GRPBK2                                                    
         MVI   SFLBK2L,C'('                                                     
         MVC   SFLBK2L+1(1),GRPBK2LN                                            
         OI    SFLBK2L+1,X'F0'                                                  
         MVI   SFLBK2L+2,C')'                                                   
         OI    SFLBK2H+6,X'80'                                                  
         OI    SFLBK2LH+6,X'80'                                                 
         B     VK30                                                             
*                                                                               
VK28     MVC   SFSBK1,GRPBK1                                                    
         MVI   SFSBK1L,C'('                                                     
         MVC   SFSBK1L+1(1),GRPBK1LN                                            
         OI    SFSBK1L+1,X'F0'                                                  
         MVI   SFSBK1L+2,C')'                                                   
         OI    SFSBK1H+6,X'80'                                                  
         OI    SFSBK1LH+6,X'80'                                                 
*                                                                               
         MVC   SFSBK2,GRPBK2                                                    
         MVI   SFSBK2L,C'('                                                     
         MVC   SFSBK2L+1(1),GRPBK2LN                                            
         OI    SFSBK2L+1,X'F0'                                                  
         MVI   SFSBK2L+2,C')'                                                   
         OI    SFSBK2H+6,X'80'                                                  
         OI    SFSBK2LH+6,X'80'                                                 
*                                                                               
VK30     MVC   AIO,AIO1            RESTORE AIO                                  
         LA    R2,SFSCODEH         CODE                                         
         CLI   5(R2),0                                                          
         BNE   VK40                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         MVC   STRTCODE,=X'0001'   SET FOR FIRST GROUP RECORD                   
         XC    GRPCODE,GRPCODE                                                  
         ZIC   R0,BREAK1LN         NO. OF DIGITS MUST EQUAL THE. . .            
         ZIC   R1,BREAK2LN         . . . SUM OF THE BREAK LENGTHS               
         AR    R0,R1                                                            
         STC   R0,TOTBREAK                                                      
         B     VK60                                                             
*                                                                               
VK40     TM    4(R2),X'08'         NUMERIC?                                     
         BO    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         ZIC   R0,BREAK1LN         NO. OF DIGITS MUST EQUAL THE. . .            
         ZIC   R1,BREAK2LN         . . . SUM OF THE BREAK LENGTHS               
         AR    R0,R1                                                            
         STC   R0,TOTBREAK                                                      
         CLM   R0,1,5(R2)                                                       
         BNE   BADNUMDG                                                         
         MVC   FULL,8(R2)          GROUP CODES ARE LEFT-JUSTIFIED, PWOS         
         OC    FULL,=C'0000'                                                    
         PACK  DUB,FULL                                                         
         L     R0,DUB+4                                                         
         SRA   R0,4                                                             
         BZ    BADGRPCD            AND MUST BE NON-ZERO                         
         STCM  R0,3,GRPCODE                                                     
*        OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   *+14                                                             
         MVC   STRTCODE,GRPCODE                                                 
         B     VK60                                                             
         MVC   GRPKCODE,GRPCODE                                                 
*                                                                               
VK60     XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
VKX      BAS   RE,SAVEDEF                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
******************************************************************              
* VALIDATE RECORD   * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
VR       DS    0H                                                               
*                                                                               
         MVC   SAVEKEY(L'KEY),KEY                                               
*                                                                               
         BRAS  RE,USERCHK  SEE IF USER HAS ACCESS                               
         JNE   BADUSER                                                          
* REPOSITION THE POINTER                                                        
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
         LA    R2,SFSNM1H          NAME 1 FIELD                                 
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         LA    R2,SFSNM2H          NAME 2 FIELD                                 
         CLI   BREAK2LN,0                                                       
         BNE   *+16                                                             
         CLI   5(R2),0                                                          
         BE    VR10                                                             
         B     NOBREAK2                                                         
         CLI   5(R2),0                                                          
         BNE   VR10                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR10     MVI   ELCODE,GRPGRPCQ                                                  
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GRPGRPD,R6                                                       
         MVI   GRPGRPCD,GRPGRPCQ                                                
         MVC   GRPGNAM1,SFSNM1                                                  
         MVC   GRPGNAM2,SFSNM2                                                  
         MVI   GRPGRPLN,GRPGRPLQ                                                
         GOTO1 ADDELEM                                                          
**************************************************                              
*  NOW VALIDATE THE STATIONS ON ADD STATION LIST                                
***********************************************                                 
         MVI   STAB,X'FF'          INIT ENTIRE TABLE TO FF                      
         MVC   STAB+1(L'STAB-1),STAB                                            
         LA    R3,STAB                                                          
         LA    R2,SFSNEWH                                                       
*                                                                               
VR50     LA    R0,SFSENDLH                                                      
         CR    R2,R0               CHECK END OF LINE                            
         BH    VRX                                                              
         CLI   5(R2),0                                                          
         BE    VR100                                                            
*                                                                               
         MVC   QNET,SPACES                                                      
         MVC   QCLT,ZEROES                                                      
         OC    8(L'SFSNEW,R2),SPACES                                            
         MVC   QNET,8(R2)                                                       
         MVC   AIO,AIO2                                                         
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         XC    KEY,KEY                                                          
         MVC   STAKEY(STAKEYLN),ZEROES                                          
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVC   STAKMED,QMED        MEDIA                                        
         OC    STAKCALL(4),=XL4'40404040'                                       
         MVC   STAKCALL+4(1),QMED                                               
         MVC   STAKCALL(4),QNET                                                 
         MVC   STAKAGY,AGENCY      AGENCY                                       
         MVC   STAKCLT,QCLT        CLIENT EXCEPTION                             
*                                                                               
         BAS   RE,SETDEF                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
********************************************************************            
*  NOW LETS SEARCH FOR PASSIVE POINTER, IF EXISTS ALREADY THEN INVALID          
*********************************************************************           
*                                                                               
         BAS   RE,RSTRDEF                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPPKEY,R4                                                       
         MVI   GRPPTYP,GRPPTYPQ                                                 
         MVI   GRPPSTYP,GRPPSTYQ                                                
         MVC   GRPPAGMD,BAGYMD                                                  
         MVC   GRPPVAL,8(R2)                                                    
         MVC   GRPPID,SFSID                                                     
         MVC   GRPPCODE,GRPCODE                                                 
         MVC   AIO,AIO2                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'GRPPKEY),KEYSAVE                                           
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*        DROP  R4                                                               
                                                                                
         MVC   0(L'STATTAB,R3),8(R2)                                            
         LA    R3,L'STATTAB(R3)                                                 
*                                                                               
VR100    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     VR50                                                             
*  ON EXIT RESTORE KEY, AND FILENAMES                                           
VRX      MVC   AIO,AIO1                                                         
         MVI   USEIO,C'N'             THERE ARE ELEMENTS                        
         XC    KEY,KEY                RESTORE KEY                               
         MVC   KEY(L'SVKEY),SVKEY                                               
         B     DR                                                               
         DROP  R4                                                               
         EJECT                                                                  
******************************************************                          
* ---------- XRECADD XREPUT -------------------------*                          
******************************************************                          
XR       LA    R3,STAB               POINT TO TABLE OF STATIONS                 
XR05     CLI   0(R3),X'FF'                                                      
         BE    XRX                                                              
*                                                                               
*-------------LETS SEE IF STATION ALREADY BELONGS TO A GROUP                    
*-------------BY PASSIVE KEY POINTING TO GROUP DIRECTLY                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPPKEY,R4                                                       
         MVI   GRPPTYP,GRPPTYPQ                                                 
         MVI   GRPPSTYP,GRPPSTYQ                                                
         MVC   GRPPAGMD,BAGYMD                                                  
         MVC   GRPPVAL,0(R3)                                                    
         MVC   GRPPID,SFSID                                                     
         MVC   AIO,AIO2                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(GRPPCODE-GRPPKEY),KEYSAVE                                    
         BNE   XR30        IF NOT FOUND JUST ADD                                
*                                                                               
* ELSE IF FOUND GET THE GROUP RECORD POINTED TO AND DEL ELEMENT                 
         GOTO1 GETREC                                                           
         MVI   ELCODE,GRPVALCQ                                                  
         L     R6,AIO                                                           
         USING GRPVALD,R6                                                       
         BAS   RE,GETEL                                                         
         BNE   XR20                                                             
         B     *+12                                                             
XR10     BAS   RE,NEXTEL                                                        
         BNE   XR20                                                             
         CLC   GRPVALUE,0(R3)                                                   
         BNE   XR10                                                             
         MVI   GRPVALCD,X'FF'                                                   
         MVI   ELCODE,X'FF'                                                     
         L     R6,AIO                                                           
         GOTO1 REMELEM                                                          
         GOTO1 PUTREC                                                           
         DROP  R6                                                               
** NOW REMOVE PASSIVE KEY OF OLD GROUP                                          
XR20     OI    GRPKCNTL,X'80'                                                   
         GOTO1 WRITE                                                            
*                                                                               
*****------ THIS PART ADDS NEW PASSIVE KEY AND POINT IT TO GROUP                
*****------ FIRST LETS GET THE GROUP RECORD AND ADD THE ELEMENT                 
XR30     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING  GRPRECD,R4                                                      
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'GRPKEY),KEYSAVE     MUST EXIST                             
         BE    *+6                                                              
         DC    H'0'                                                             
*  SAVE ADDRESS                                                                 
         MVC   SAVEADDR,GRPKDA                                                  
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
* MOVE IN NEW ELEMENT                                                           
         LA    R6,ELEM                                                          
         USING GRPVALD,R6                                                       
         XC    ELEM,ELEM                                                        
         MVI   GRPVALCD,GRPVALCQ                                                
         MVI   GRPVALLN,GRPVALLQ                                                
         MVC   GRPVALUE,0(R3)                                                   
         GOTO1 ADDELEM                                                          
         GOTO1 PUTREC                                                           
* ----------- NOW WE BUILD THE PASSIVE POINTER IF DOESN'T EXIST                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPPKEY,R4                                                       
         MVI   GRPPTYP,GRPPTYPQ                                                 
         MVI   GRPPSTYP,GRPPSTYQ                                                
         MVC   GRPPAGMD,BAGYMD                                                  
         MVC   GRPPVAL,0(R3)                                                    
         MVC   GRPPID,SFSID                                                     
         MVC   GRPPCODE,GRPCODE                                                 
         OI    DMINBTS,X'88'                                                    
         XC    SVKEY2,SVKEY2                                                    
         MVC   SVKEY2,KEY                                                       
         GOTO1 HIGH                                                             
         CLI   DMCB+8,X'02'     IF RECORD FOUND IS MARKED FOR DEL               
         BE    *+14             ITS OKAY                                        
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    DMINBTS,X'FF'-X'88'                                              
         CLC   KEY(L'GRPPKEY),KEYSAVE                                           
         BNE   XR150                                                            
* WE FOUND PASSIVE POINTER TO BE MARKED FOR DELETION SO                         
* JUST UNMARK IT AND POINT IT TO GROUP RECORDS ANYWAYS                          
         NI    GRPKCNTL,X'FF'-X'80'                                             
         MVC   GRPKDA,SAVEADDR                                                  
         GOTO1 WRITE                                                            
         B     XR200                                                            
*                                                                               
*---- WE DIDN'T FIND PASSIVE POINTER SO ADD IT NOW                              
XR150    XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY2),SVKEY2                                             
         MVC   GRPKDA,SAVEADDR                                                  
         GOTO1 ADD                                                              
         DROP  R4                                                               
*                                                                               
XR200    LA    R3,L'STATTAB(R3)                                                 
         B     XR05                                                             
***************************************************************                 
*RX      B     XIT                                                              
XRX      B     DR                                                               
******************************************************                          
* ----------DISPLAY REC ----------------------------                            
******************************************************                          
DR       L     R6,AIO                                                           
         GOTO1 CLRSCRN                                                          
         MVI   ELCODE,GRPGRPCQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING GRPGRPD,R6                                                       
         MVC   SFSNM1,GRPGNAM1                                                  
         MVC   SFSNM2,GRPGNAM2                                                  
         OI    SFSNM1H+6,X'80'                                                  
         OI    SFSNM2H+6,X'80'                                                  
*                                                                               
         MVI   ELCODE,GRPVALCQ                                                  
         LA    R3,SFSLISTH                                                      
         L     R6,AIO                                                           
         USING GRPVALD,R6                                                       
         BAS   RE,GETEL                                                         
         BE    DR50                                                             
         B     DRX                                                              
DR30     BAS   RE,NEXTEL                                                        
         BE    DR50                                                             
         B     DRX                                                              
* PRINT STATIONS ON TO LIST                                                     
DR50     LA    R0,SFSPFKH                                                       
         CR    R3,R0                                                            
         BNL   DRX                                                              
*STRT STATION FILTER                                                            
         CLC   SVSTRT,GRPVALUE                                                  
         BNL   DR30                                                             
         MVC   8(L'GRPVALUE,R3),GRPVALUE                                        
         OI    6(R3),X'80'                                                      
         ZIC   RE,0(R3)                                                         
         AR    R3,RE                                                            
         B     DR30                                                             
*                                                                               
DRX      B     XIT                                                              
         DROP  R6                                                               
*                                                                               
******************************************************                          
*  ============== DISP KEY ===========================                          
******************************************************                          
DK       L     R6,AIO                                                           
         USING GRPRECD,R6                                                       
*                                                                               
         XC    FULL,FULL                                                        
         GOTO1 HEXOUT,DMCB,GRPKCODE,FULL,L'GRPKCODE,=C'TOG',0                   
         ZIC   RE,TOTBREAK                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SFSCODE(0),FULL                                                  
         OI    SFSCODEH+6,X'80'                                                 
*                                                                               
         MVC   GRPCODE,GRPKCODE                                                 
         XC    SVSTRT,SVSTRT                                                    
         DROP  R6                                                               
*                                                                               
         LA    R4,KEY                                                           
         USING GRPRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'GRPKEY),0(R6)                                              
         XC    GRPKCODE,GRPKCODE                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'GRPKEY),KEYSAVE                                            
         BNE   NOSGDEF                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,GRPBRKCQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING GRPBRKD,R6                                                       
         MVC   BREAK1LN,GRPBK1LN   SAVE BREAK LENGTHS                           
         MVC   BREAK2LN,GRPBK2LN                                                
         ZIC   R0,BREAK1LN         NO. OF DIGITS MUST EQUAL THE. . .            
         ZIC   R1,BREAK2LN         . . . SUM OF THE BREAK LENGTHS               
         AR    R0,R1                                                            
         STC   R0,TOTBREAK                                                      
*                                                                               
         MVC   SFSBK1,GRPBK1                                                    
         MVI   SFSBK1L,C'('                                                     
         MVC   SFSBK1L+1(1),GRPBK1LN                                            
         OI    SFSBK1L+1,X'F0'                                                  
         MVI   SFSBK1L+2,C')'                                                   
         OI    SFSBK1H+6,X'80'                                                  
         OI    SFSBK1LH+6,X'80'                                                 
*                                                                               
         MVC   SFSBK2,GRPBK2                                                    
         MVI   SFSBK2L,C'('                                                     
         MVC   SFSBK2L+1(1),GRPBK2LN                                            
         OI    SFSBK2L+1,X'F0'                                                  
         MVI   SFSBK2L+2,C')'                                                   
         OI    SFSBK2H+6,X'80'                                                  
         OI    SFSBK2LH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
* RESTORE THE KEY TO ORIGINAL FORM                                              
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'GRPKEY),0(R6)                                              
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY,KEY                                                        
*                                                                               
DKX      B     XIT                                                              
******************************************************                          
*  ============== LISTRECS ===========================                          
******************************************************                          
LR       OC    KEY,KEY                                                          
         BNZ   *+10                                                             
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR40                                                             
         DC    H'0'                                                             
*                                                                               
LR30     GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR40     CLC   KEY(2),=X'0D05'                                                  
         BNE   LRX                                                              
         CLC   KEY(4),KEYSAVE                                                   
         BNE   LRX                                                              
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING GRPRECD,R6                                                       
*                                                                               
         OC    GRPKCODE,GRPKCODE                                                
         BZ    LR30                                                             
         CLC   GRPCODE,GRPKCODE                                                 
         BH    LR30                                                             
         CLC   GRPKID,SFSID                                                     
         BNE   LRX                                                              
*                                                                               
         XC    LISTAR,LISTAR                                                    
         XC    FULL,FULL                                                        
         GOTO1 HEXOUT,DMCB,GRPKCODE,FULL,L'GRPKCODE,=C'TOG',0                   
         ZIC   RE,TOTBREAK                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LSTCODE(0),FULL                                                  
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         USING GRPGRPD,R6                                                       
         MVI   ELCODE,GRPGRPCQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LSTNAME1,GRPGNAM1                                                
         MVC   LSTNAME2,GRPGNAM2                                                
         DROP  R6                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR100                                                            
         XC    P,P                                                              
         MVC   PCODE,LSTCODE                                                    
         MVC   PNAME1,LSTNAME1                                                  
         MVC   PNAME2,LSTNAME2                                                  
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR30                                                             
*                                                                               
LR100    GOTO1 LISTMON                                                          
         B     LR30                                                             
LRX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
******************************************************                          
*========= HEAD HOOK ================================                           
******************************************************                          
HDHK     NTR1                                                                   
         MVC   H3(5),=C'MEDIA'                                                  
         MVC   H5+2(1),QMED                                                     
         MVC   H4(5),=C'-----'                                                  
         MVC   H3+10(2),=C'ID'                                                  
         MVC   H5+10(1),SFSID                                                   
         MVC   H4+10(2),=C'--'                                                  
         MVC   H5+20(12),SFLBK1                                                 
         MVC   H5+34(3),SFLBK1L                                                 
         MVC   H5+50(12),SFLBK2                                                 
         MVC   H5+64(3),SFLBK2L                                                 
HDHKX    XIT1                                                                   
******************************************************                          
CLRSCRN  NTR1                                                                   
         XC    SFSSTRT,SFSSTRT                                                  
         OI    SFSSTRTH+6,X'80'                                                 
         XC    SFSNM1,SFSNM1                                                    
         OI    SFSNM1H+6,X'80'                                                  
         XC    SFSNM2,SFSNM2                                                    
         OI    SFSNM2H+6,X'80'                                                  
* CLEAR THE ADD STATIONS                                                        
         LA    R2,SFSNEWH                                                       
         LA    R3,SFSENDLH                                                      
CLR20    CR    R2,R3                                                            
         BH    CLR25                                                            
         XC    8(L'SFSNEW,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     CLR20                                                            
*                                                                               
* CLEAR THE STATION LIST                                                        
CLR25    LA    R2,SFSLISTH                                                      
         LA    R3,SFSPFKH                                                       
CLR30    CR    R2,R3                                                            
         BNL   CLRX                                                             
         XC    8(L'SFSLIST,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     CLR30                                                            
CLRX     B     XIT                                                              
******************************************************                          
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*        OI    GENSTAT4,NODELLST   NO DEL ALLOWED                               
         OI    GENSTAT2,DISTHSPG                                                
SETUPX   B     XIT                                                              
*                                                                               
SETDEF   MVC   SYSDIR,=C'STATION '      SET TO READ STATION FILE                
         MVC   SYSFIL,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         MVI   ACTELOPT,C'N'            NO ACTIVITY ELEMENTS                    
         MVC   LKEY,=H'15'              SET LENGTH OF STATION KEY               
         BR    RE                                                               
*                                                                               
SAVEDEF  DS    0H                  SAVE DEFINITION BEFORE SETDEF                
         MVC   MYSYSDIR,SYSDIR                                                  
         MVC   MYSYSFIL,SYSFIL                                                  
         MVC   MYUSEIO,USEIO                                                    
         MVC   MYACELOP,ACTELOPT                                                
         MVC   MYLKEY,LKEY                                                      
         BR    RE                                                               
*                                                                               
RSTRDEF  DS    0H                  RESTORE DEFINITION AFTER SETDEF              
         MVC   SYSDIR,MYSYSDIR                                                  
         MVC   SYSFIL,MYSYSFIL                                                  
         MVC   USEIO,MYUSEIO                                                    
         MVC   ACTELOPT,MYACELOP                                                
         MVC   LKEY,MYLKEY                                                      
         BR    RE                                                               
*                                                                               
***************************************************************                 
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'STATION GROUP REPORT'                                    
         SSPEC H2,30,C'--------------------'                                    
         SPACE 3                                                                
         SSPEC H7,1,C'CODE'                                                     
         SSPEC H8,1,C'----'                                                     
         SSPEC H7,20,C' BREAK NAME1           '                                 
         SSPEC H8,20,C'-----------------------'                                 
         SSPEC H7,50,C' BREAK NAME2           '                                 
         SSPEC H8,50,C'-----------------------'                                 
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
CTDISP   DC    Y(L'SAPEKEY+L'SAPELEN+L'SAPESTAT)                                
         GETEL2 R6,CTDISP,ELCODE                                                
         SPACE 2                                                                
RELO     DS    F                                                                
ZEROES   DC    20C'0'                                                           
*                                                                               
         SPACE 2                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 2                                                                
BADNUMDG XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADNUMDM),BADNUMDM                                     
         GOTO1 ERREX2                                                           
BADNUMDM DC    C'* ERROR * NUMBER OF DIGITS MUST EQUAL SUM OF BREAK LEN+        
               GTHS'                                                            
         SPACE 2                                                                
BADGRPCD XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADGRPCM),BADGRPCM                                     
         GOTO1 ERREX2                                                           
BADGRPCM DC    C'* ERROR * GROUP CODE MUST BE NON-ZERO'                         
         SPACE 2                                                                
NOBREAK2 XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOBREAKM),NOBREAKM                                     
         GOTO1 ERREX2                                                           
NOBREAKM DC    C'* ERROR * BREAK 2 IS NOT DEFINED'                              
         SPACE 2                                                                
NODELETE XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODELETM),NODELETM                                     
         GOTO1 ERREX2                                                           
NODELETM DC    C'* ERROR * NOT A MEMBER OF THIS GROUP'                          
         SPACE 2                                                                
NOADD    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOADDM),NOADDM                                         
         GOTO1 ERREX2                                                           
NOADDM   DC    C'* ERROR * ALREADY A MEMBER OF THIS GROUP'                      
         SPACE 2                                                                
NODUP    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODUPM),NODUPM                                         
         GOTO1 ERREX2                                                           
NODUPM   DC    C'* ERROR * CANNOT HAVE DUPLICATES'                              
*                                                                               
NOSGDEF  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOSGDEFM),NOSGDEFM                                     
         GOTO1 ERREX2                                                           
NOSGDEFM DC    C'* ERROR * NO STATION GROUP DEF RECORD'                         
*                                                                               
BADUSER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADUSRNM),BADUSRNM                                     
         GOTO1 ERREX2                                                           
BADUSRNM DC    C'* ERROR * YOU ARE NOT AUTHORIZED TO MAKE CHANGES'              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        USERCHK SUBROUTINE                                           *         
***********************************************************************         
USERCHK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
**       OC    ASECBLK,ASECBLK                                                  
**       BZ    UCHKNO              NO SECURITY NO CHANGE                        
**       DC    H'0'                                                             
         L     R1,ASECBLK                                                       
         USING SECD,R1                                                          
         MVC   PIDNAME,SECPID                                                   
         DROP  R1                                                               
*                                                                               
*****    BRAS  RE,CHECKPID         NEED THE 2 BYTE PID NOW                      
*****    BNE   UCHKNO                                                           
*                                                                               
**********************************                                              
*                                                                               
*                                                                               
         LA    RE,KEY                                                           
         USING GRPRECD,RE                                                       
         CLC   KEY(2),=X'0D05'     MUST BE STATION GROUP                        
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    GRPKCODE,GRPKCODE                                                
         DROP  RE                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      SHOULD BE THERE                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=CL8'SPTFILE',KEY+14,AIO,MYDMWRK         
*******  GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         USING GRPKEY,R6                                                        
         L     R6,AIO2             WE NEED THE GROUP DEFINITION RECORD          
         MVI   ELCODE,GRPSCMCQ     X'12' SECURITY MANAGER ID ELEMENT            
         BRAS  RE,GETEL                                                         
         BNE   UCHKYES             NO RESTRICTION, WE'RE GOOD TO GO             
         USING GRPSCMD,R6                                                       
         OC    GRPSCM(12),GRPSCM   ANYTHING HERE?                               
         BZ    UCHKYES              - NOPE, WE OK                               
*                                                                               
         LA    RF,6                BCT LOOP 6 TIMES                             
         LA    R2,GRPSCM1          START WITH 1ST MANAGER                       
*                                                                               
UCHK50   CLC   PIDNUM,0(R2)        SAME PERSON?                                 
         BE    UCHKYES              - YUP, WE GOOD                              
         LA    R2,L'GRPSCM(R2)     BUMP NOW                                     
         BCT   RF,UCHK50                                                        
*                                                                               
UCHKNO   LTR   RE,RE                                                            
         B     *+6                                                              
*                                                                               
UCHKYES  CR    RE,RE                                                            
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE NESFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMB7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMB8D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE NESFMWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
MYSYSDIR DS    CL(L'SYSDIR)        SAVED SYSDIR VALUE BEFORE SETDEF RTN         
MYSYSFIL DS    CL(L'SYSFIL)          "   SYSFIL   "     "      "     "          
MYUSEIO  DS    CL(L'USEIO)           "   USEIO    "     "      "     "          
MYACELOP DS    CL(L'ACTELOPT)        "   ACTELOPT "     "      "     "          
MYLKEY   DS    CL(L'LKEY)            "   LKEY     "     "      "     "          
*                                                                               
MINIO    DS    V                   A(MINIO)                                     
ABK1FH   DS    A                   A(BREAK 1 NAME FIELD HEADER)                 
ABK1LFH  DS    A                   A(BREAK 1 LENGTH FIELD HEADER)               
ABK2FH   DS    A                   A(BREAK 2 NAME FIELD HEADER)                 
ABK2LFH  DS    A                   A(BREAK 2 LENGTH FIELD HEADER)               
GRPID    DS    C                   GROUP ID                                     
GRPCODE  DS    XL2                 GROUP CODE (PWOS)                            
STRTCODE DS    XL2                 START GROUP CODE FOR LIST                    
SAVEKEY  DS    XL48                                                             
CODECHAR DS    CL5                 GROUP CODE CHARACTER                         
BREAK1LN DS    X                   BREAK 1 LENGTH FROM DEFINITION REC           
BREAK2LN DS    X                   BREAK 2 LENGTH FROM DEFINITION REC           
TOTBREAK DS    X                   TOTAL BREAL LENGTH FROM DEF REC              
VALUES   DS    XL((15*VALUELNQ)+1) SAVED VALUES FROM SCREEN (MAX = 15)          
MYDMWRK  DS    XL96                                                             
SH7      DS    CL132                                                            
SVNMS    DS    CL55                                                             
DFLG     DS    C                   STUFF TO PRINT                               
CONT     DS    C                   CONTINUATION FLAG                            
GOTU     DS    C                   GOT ONE TO UNDERLINE FLAG                    
MINBLOCK DS    XL(MINBLKL)         MINIO PARAMETER BLOCK                        
STAB     DS    0CL57                                                            
STATTAB  DS    7CL8                STATION LIST TABLE FOR ADDING                
STATEND  DC    X'FF'                                                            
SVKEY2   DS    CL(L'SVKEY)                                                      
SAVEADDR DS    A                                                                
SVSTRT   DS    CL6                                                              
*                                                                               
PIDNAME  DS    CL8                  USER PID NAME                               
PIDNUM   DS    CL2                  PID                                         
SECALPHA DS    CL2                  ALPHA AGENCY                                
*                                  POINTERS TO STATIONS                         
         EJECT                                                                  
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE DDMINBLK                                                       
         EJECT                                                                  
VALUED   DSECT                                                                  
*                                                                               
VALUEVAL DS    CL(L'GRPPVAL)       VALUE TO BE STORED IN RECORD/POINTER         
VALUEACT DS    C                   USER ACTION (C'+' OR C'-')                   
VALUEDEL DS    XL2                 GRP CODE FROM WHICH WE'LL DELETE IT          
*                                                                               
VALUELNQ EQU   *-VALUED                                                         
         SPACE 3                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCODE  DS    CL4                                                              
         DS    CL2                                                              
LSTNAME1 DS    CL24                                                             
         DS    CL4                                                              
LSTNAME2 DS    CL24                                                             
         SPACE 3                                                                
*                                                                               
SPOOLD   DSECT                                                                  
         ORG  P                                                                 
PCODE    DS   CL4                                                               
         DS   CL15                                                              
PNAME1   DS   CL24                                                              
         DS   CL7                                                               
PNAME2   DS   CL24                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009NESFM27   09/12/12'                                      
         END                                                                    
