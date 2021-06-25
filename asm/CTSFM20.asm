*          DATA SET CTSFM20    AT LEVEL 089 AS OF 05/18/00                      
*PHASE TA0A20A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A20 - REPID MAINT/LIST                                   *         
*                                                                     *         
*  COMMENTS: MAINTAINS REPID RECORDS                                  *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFM93 (TA0A93) -- MAINTENANCE                    *         
*                  CTSFM94 (TA0A94) -- LIST                           *         
*                                                                     *         
*                                                                     *         
*  OUTPUTS: UPDATED REPID RECORDS                                     *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - 2ND BASE                                              *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
         TITLE 'TA0A20 REPID RECORDS'                                           
TA0A20   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A20*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         OI    GENSTAT4,NODELLST+CONFDEL  DISALLOW DELETE FROM LIST             
*                                         AND FORCE DELETE CONFIRM              
*                                                                               
         MVI   ACTELOPT,C'Y'       INSERT ACTIVITY ELEMENT                      
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VK                                                            
         B     MAINX                                                            
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+16                  (AND DISPLAY)                              
         BAS   RE,VR                                                            
         BAS   RE,DR                                                            
         B     MAINX                                                            
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BAS   RE,DR                                                            
         B     MAINX                                                            
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DK                                                            
         B     MAINX                                                            
*                                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BNE   *+12                                                             
         BAS   RE,LR                                                            
         B     MAINX                                                            
*                                                                               
MAINX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************                      
*                    VALIDATE KEY                        *                      
**********************************************************                      
VK       NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REPKEYD,R4                                                       
*                                                                               
         CLI   ACTNUM,ACTLIST      LIST ACTION?                                 
         BE    VKL                  THEN VALIDATE LIST KEY                      
*                                                                               
*           VALIDATE KEY FOR ALL OTHERS                                         
*                                                                               
VK5      LA    R2,SFMMEDAH            SET R2 FOR GENCON                         
         CLI   SFMMEDAH+5,1        IS THE MEDIA 1 CHAR?                         
         BNE   VKINVM                                                           
         CLI   SFMMEDA,C'T'        IS THE MEDIA T?                              
         BE    VK15                                                             
         CLI   SFMMEDA,C'R'        OR  MEDIA R?                                 
         BNE   VKINVM                                                           
*                                                                               
VK15     MVC   TMPMED,SFMMEDA      STORE THE VALIDATED MEDIA                    
         LA    R2,SFMREPCH            SET R2 FOR GENCON                         
         CLI   SFMREPCH+5,0        IF NO REP CODE ENTERED                       
         BE    VKBADREP             THEN ERROR                                  
         MVC   TMPCOD,SFMREPC       ELSE, STORE VALIDATED CODE                  
         OC    TMPCOD,SPACES        ENSURE SPACE PADDED                         
         B     VKBLDKEY             AND BUILD KEY                               
*                                                                               
*           VALIDATE KEY FOR LIST                                               
*                                                                               
VKL      LA    R2,SFLMEDAH            SET R2 FOR GENCON                         
         CLI   SFMMEDAH+5,0        IF MEDIA EMPTY                               
         BE    VKINVM                 THEN ERROR                                
         CLI   SFLMEDA,C'T'        IS TELEVISION?                               
         BE    VKL20                  YES, CONTINUE                             
         CLI   SFLMEDA,C'R'        IS RADIO?                                    
         BNE   VKINVM                 NO, ERROR (MUST BE T OR R)                
*                                                                               
VKL20    MVC   TMPMED,SFLMEDA      STORE THE VALIDATED MEDIA                    
         MVC   TMPCOD,SFLREPC      STORE CODE                                   
*                                                                               
*            FALL THROUGH TO BUILD KEY                                          
*                                                                               
**------------------------------------------------------**                      
*                    BUILD KEY                           *                      
* ------------------------------------------------------**                      
*                                                                               
VKBLDKEY MVI   REPKSYS,REPKSYSQ    BUILD KEY...                                 
         MVI   REPKTYP,REPKTYPQ                                                 
         MVC   REPKMEDA,TMPMED     MEDIA TYPE                                   
         MVC   REPKREP,TMPCOD      REP CODE                                     
*                                                                               
*                                                                               
VKX      B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
*      COMMON KEY VALIDATION ERRORS                                             
*                                                                               
VKINVM   MVC   GERROR,=AL2(INVMED) INVALID MEDIA                                
         B     SFMERROR                                                         
*                                                                               
VKBADREP MVC   GERROR,=AL2(BADREP) INVALID REP CODE                             
         LA    R2,SFMREPCH                                                      
         B     SFMERROR                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
**********************************************************                      
*                    VALIDATE RECORD                     *                      
**********************************************************                      
*                                                                               
VR       NTR1                                                                   
*                                                                               
         MVI   ELCODE,REPREPCQ     REP ELEMENT                                  
         GOTO1 REMELEM             REMOVE EXISTING ELEMENT                      
         LA    R6,ELEM                                                          
         USING REPREPD,R6          APPLY REP DSECT TO ELEM                      
         XC    ELEM,ELEM           CLEAR FOR BUILD                              
*                                                                               
         MVI   REPREPC,REPREPCQ    REP ELEMENT CODE                             
         MVI   REPREPLN,REPREPLQ   REP ELEMENT LENGTH                           
*                                                                               
         MVC   GERROR,=AL2(INVALID) SET GENCON ERROR                            
*                                                                               
*            BEGIN VALIDATING SCREEN FIELDS                                     
*                                                                               
         LA    R2,SFMREPNH                                                      
         GOTO1 ANY                 MAKE SURE NAME ENTERED                       
         MVC   REPNAME,SFMREPN     PUT IN ELEMENT                               
         OC    REPNAME,SPACES                                                   
*                                                                               
*            ALL FLAG FIELDS MUST BE MARKED N,Y OR BLANK=DEFAULT(N)             
*                                                                               
VR10     LA    R2,SFMFLG1H            CHECK FIRST FLAG FIELD                    
         CLI   SFMFLG1H+5,0           IF NOT ENTERED                            
         BE    VR20                     ACCEPT DEFAULT                          
         CLI   SFMFLG1,C'N'           IF 'N' ENTERED                            
         BE    VR20                     LEAVE BIT OFF                           
*                                                                               
         CLI   SFMFLG1,C'Y'           IF 'Y' ENTERED                            
         BNE   SFMERROR                 NO, ERROR                               
         OI    REPFLAG,REPTESTQ       ELSE,TURN ON TEST BIT                     
*                                                                               
VR20     LA    R2,SFMFLG2H            CHECK SECOND FLAG FIELD                   
         OI    REPFLAG,REPNOTDQ       TURN ON 'NOT DARE' BIT                    
*                                          (DEFAULT)                            
         CLI   SFMFLG2H+5,0           IF NOT ENTERED                            
         BE    VR30                     ACCEPT DEFAULT                          
         CLI   SFMFLG2,C'N'           IF 'N' ENTERED                            
         BE    VR30                     LEAVE BIT ON                            
*                                                                               
         CLI   SFMFLG2,C'Y'           IF 'Y' ENTERED                            
         BNE   SFMERROR                 NO,ERROR                                
         NI    REPFLAG,X'FF'-REPNOTDQ ELSE, TURN OFF 'NOT DARE' BIT             
*                                                                               
VR30     LA    R2,SFMFLG3H            CHECK THIRD FLAG FIELD                    
         CLI   SFMFLG3H+5,0           IF NOT ENTERED                            
         BE    VR40                     ACCEPT DEFAULT                          
         CLI   SFMFLG3,C'N'           IF 'N' ENTERED                            
         BE    VR40                     LEAVE BIT OFF                           
*                                                                               
         CLI   SFMFLG3,C'Y'           IF 'Y' ENTERED                            
         BNE   SFMERROR                 NO, ERROR                               
         OI    REPFLAG,REPASISQ       ELSE,TURN ON USE AS IS BIT                
*                                                                               
*            CHECK FOR REP PREFIX                                               
*                                                                               
VR40     LA    R2,SFMPRFXH                                                      
         GOTO1 ANY                 MAKE SURE PREFIX ENTERED                     
         MVC   REPPREF,SFMPRFX     PUT IN ELEMENT                               
         OC    REPPREF,SPACES       ENSURE SPACE PADDED                         
         MVC   REPPRELN,5(R2)      STORE LENGTH IN ELEMENT                      
         GOTO1 ADDELEM             PUT ELEMENT IN RECORD                        
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*******************************************************                         
*               DISPLAY RECORDS                       *                         
*******************************************************                         
*                                                                               
DR       NTR1                                                                   
         L     R4,AIO                                                           
         USING REPKEYD,R4                                                       
*                                                                               
         CLC   KEY(REPKLENQ),REPKEY    CHECK KEY AGAINST RECORD                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SFMREPN,SFMREPN                                                  
         OI    SFMREPNH+6,X'80'                                                 
         XC    SFMFLG1,SFMFLG1                                                  
         OI    SFMFLG1H+6,X'80'                                                 
         XC    SFMFLG2,SFMFLG2                                                  
         OI    SFMFLG2H+6,X'80'                                                 
         XC    SFMFLG3,SFMFLG3                                                  
         OI    SFMFLG3H+6,X'80'                                                 
         XC    SFMPRFX,SFMPRFX                                                  
         OI    SFMPRFXH+6,X'80'                                                 
*                                                                               
         L     R3,AIO                                                           
         USING REPREPD,R3                                                       
         MVI   ELCODE,REPREPCQ                                                  
         BAS   RE,GETEL                  GET REP ELEMENT                        
         BE    *+6                                                              
         DC    H'0'                      MUST BE THERE                          
*                                                                               
*           DISPLAY FIELDS (ALREADY TRANSMITTED ABOVE)                          
*                                                                               
         MVC   SFMREPN,REPNAME           SHOW REP NAME                          
         MVC   SFMPRFX,REPPREF           SHOW REP PREFIX                        
*                                                                               
         MVI   SFMFLG1,C'N'              INIT FLAG TO DEFAULT                   
         TM    REPFLAG,REPTESTQ          TEST REP?                              
         BZ    *+8                        NO                                    
         MVI   SFMFLG1,C'Y'               YES                                   
*                                                                               
         MVI   SFMFLG2,C'N'              INIT FLAG TO DEFAULT                   
         TM    REPFLAG,REPNOTDQ          DARE REP?                              
         BNZ   *+8                        NO                                    
         MVI   SFMFLG2,C'Y'               YES                                   
*                                                                               
         MVI   SFMFLG3,C'N'              INIT FLAG TO DEFAULT                   
         TM    REPFLAG,REPASISQ          USE AS IS?                             
         BZ    *+8                        NO                                    
         MVI   SFMFLG3,C'Y'               YES                                   
         DROP  R3                                                               
*                                                                               
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL            CHECK FOR ACTIVITY ELEMENT                   
         BNE   DRX                 MAY NOT BE THERE YET                         
         USING ACTVD,R3                                                         
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(5,SFMADAT)                             
         OI    SFMADATH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,SFMCDAT)                             
         OI    SFMCDATH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
DRX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*******************************************************                         
*                LIST RECORDS                         *                         
*******************************************************                         
*                                                                               
LR       NTR1                                                                   
         LA    R4,KEY                                                           
         USING REPKEYD,R4                                                       
*                                                                               
         OC    KEY,KEY             FIRST TIME CALLED?                           
         BNZ   LR10                                                             
*                                                                               
         MVI   REPKSYS,REPKSYSQ    BUILD SEARCH KEY WITH INPUT                  
         MVI   REPKTYP,REPKTYPQ                                                 
         MVC   REPKMEDA,TMPMED     INSERT PREV VALIDATED FLDS                   
         MVC   REPKREP,TMPCOD                                                   
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     GOTO1 SEQ                 NEXT REC                                     
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(REPKREP-REPKEY),SAVEKEY  SAME SYSTEM AND MEDIA?              
         BNE   LRX                          IF NO, END ROUTINE                  
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,REPREPCQ    REP ELEMENT                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'               MUST BE ON RECORD                             
*                                                                               
         USING REPREPD,R3                                                       
         MVC   LISTAR,SPACES      CLEAR LISTAR                                  
         MVC   LSTREPC,REPKREP    INSERT REP CODE(FROM KEY)                     
         MVC   LSTNAME,REPNAME    INSERT REP NAME                               
         MVC   LSTPREF,REPPREF    INSERT PREFIX                                 
*                                                                               
         MVI   LSTASIS,C'N'       INIT DEFAULT                                  
         TM    REPFLAG,REPASISQ   USE AS IS?                                    
         BZ    *+8                                                              
         MVI   LSTASIS,C'Y'                                                     
*                                                                               
         MVI   LSTTEST,C'N'       INIT DEFAULT                                  
         TM    REPFLAG,REPTESTQ   TEST REP?                                     
         BZ    *+8                                                              
         MVI   LSTTEST,C'Y'                                                     
*                                                                               
         MVI   LSTDARE,C'N'       INIT DEFAULT                                  
         TM    REPFLAG,REPNOTDQ   NOT DARE REP?                                 
         BNZ   *+8                                                              
         MVI   LSTDARE,C'Y'                                                     
*                                                                               
         DROP  R3                                                               
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL            CHECK FOR ACTIVITY ELEMENT                   
         BNE   LR40                MAY NOT BE THERE                             
         USING ACTVD,R3                                                         
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,LSTCDAT)                             
         DROP  R3                                                               
*                                                                               
*                                                                               
LR40     GOTO1 LISTMON                                                          
         B     LR20                                                             
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
************************************************************                    
*                  DISPLAY KEY                             *                    
************************************************************                    
*                                                                               
DK       NTR1                                                                   
         L     R4,AIO                                                           
         USING REPKEYD,R4                                                       
*                                                                               
         MVC   SFMMEDA,REPKMEDA    PUT KEY TO SCREEN                            
         OI    SFMMEDAH+6,X'80'                                                 
*                                                                               
         MVC   SFMREPC,REPKREP                                                  
         OI    SFMREPCH+6,X'80'                                                 
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************                     
*                    COMMON BRANCHES                      *                     
***********************************************************                     
*                                                                               
EXIT     XIT1                    COMMON EXIT                                    
*                                                                               
SFMERROR GOTO1 SFMERR                                                           
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
*                                                                               
*                                                                               
         GETEL  R3,DATADISP,ELCODE                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTGENREPD                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM93D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM94D                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
         ORG   SYSSPARE                                                         
TMPKEY   DS    0CL4                                                             
TMPMED   DS    CL1                                                              
TMPCOD   DS    CL3                                                              
SAVEKEY  DS    CL32                                                             
STATUS   DS    CL1                                                              
MYIO     DS    1000X                                                            
*                                                                               
         EJECT                                                                  
* ONLINE LIST (1 LINE)                                                          
* SEL  MED CODE REP NAME    REP PREFIX  AS-IS  TEST  DARE  CHANGED              
*      T   AAA  REPCOTRIPA  TAPREFIX    Y      Y     Y     JAN01/00             
*12345678901234567890123456789012345678901234567890123456789012345678           
GEND   DSECT                                                                    
         ORG   LISTAR                                                           
LSTREPC  DS    CL3                 REP CODE                                     
         DS    CL2                                                              
LSTNAME  DS    CL10                REP NAME                                     
         DS    CL1                                                              
LSTPREF  DS    CL10                REP PREFIX                                   
         DS    CL2                                                              
LSTASIS  DS    CL1                 USE AS IS?                                   
         DS    CL9                                                              
LSTTEST  DS    CL1                 TEST REP?                                    
         DS    CL8                                                              
LSTDARE  DS    CL1                 DARE REP?                                    
         DS    CL8                                                              
LSTCDAT  DS    CL8                 CHANGED ON DATE                              
*                                                                               
SORTKEYD DSECT                                                                  
SORTLEN  DS    CL2                 RECORD LENGTH                                
SORTSPAR DC    CL2'0000'                                                        
SORTDARE DS    CL1                 STATUS                                       
SORTCREP DS    CL3                 CURRENT REP                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089CTSFM20   05/18/00'                                      
         END                                                                    
