*          DATA SET NESFM34S   AT LEVEL 017 AS OF 05/01/02                      
*PHASE T31C34A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: NESFM34<==>T31C34 USERFIELD DEFINITION RECORDS MAINT.       *         
*                                                                     *         
*  COMMENTS:                                                          *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (T21700), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREEN NESFMEC (T31CEC) -- MAINTENANCE*                              
*                                                                     *         
*  OUTPUTS: UPDATED USERFIELD DEFINITION RECORDS                      *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM/WORK                                           *         
*          RF - SYSTEM/WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'NESFM34<==>T31C34 USERFIELD DEFINITION RECDS MAINT'             
***********************************************************************         
T31C34   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T31C34*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVI   ACTELOPT,C'N'                                                    
*                                                                               
* DISALLOW ACTION=ADD, DELETE, RESTORE, & REPORT                                
* REMOVE WHEN SECURITY SYSTEM IS INSTALLED, I.E. LET SECURITY HANDLE IT         
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTDEL                                                    
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTREST                                                   
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTREP                                                    
         BE    ACTERR                                                           
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*                                                                               
EXIT     XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
*                                                                               
VK       DS    0H                                                               
*                                                                               
*--------------------------- MEDIA FIELD -----------------------------*         
*                                                                               
         LA    R2,USMMEDIH         VALIMED CHECKS AND VALIDATES                 
         GOTO1 VALIMED              MEDIA INPUT.                                
*                                                                               
*-------------------------- CLIENT FIELD -----------------------------*         
*                                                                               
VK10     LA    R2,USMCLNTH          VALICLT CHECKS AND VALIDATES                
*        GOTO1 VALICLT               CLIENT FIELD.                              
*                                                                               
*---------------------------- BUILD KEY ------------------------------*         
*                                                                               
         GOTO1 ANY                                                              
         GOTO1 CLPACK,DMCB,WORK,BCLT                                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLTHDRD,R4          1ST BYTE OF CLIENT RECORDS IS X'00'.         
         MVC   CKEYAM,BAGYMD       AGENCY/MEDIA.                                
         MVC   CKEYCLT,BCLT        CLIENT CODE IN BINARY.                       
         DROP  R4                                                               
*                                                                               
XVK      B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== VALIDATE RECORD ROUTINE ======================*         
*                                                                               
VR       DS    0H                                                               
*                                                                               
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
*                                                                               
         LA    R2,USMP1H           R2-->PRODUCT1 HEADER ON SCREEN.              
         ST    R2,ADESCH           STORE ADDRESS OF DESC FIELD.                 
         MVC   TPLEN,MAXLFLD1                                                   
         BAS   RE,BLDBLK                                                        
         MVC   CPU1(CUSERLNQ),USERBLK                                           
*                                                                               
         LA    R2,USMP2H           R2-->PRODUCT2 HEADER ON SCREEN.              
         ST    R2,ADESCH           STORE ADDRESS OF DESC FIELD.                 
         MVC   TPLEN,MAXLFLD2                                                   
         BAS   RE,BLDBLK                                                        
         MVC   CPU2(CUSERLNQ),USERBLK                                           
*                                                                               
         LA    R2,USME1H           R2-->ESTIMATE1 HEADER ON SCREEN.             
         ST    R2,ADESCH           STORE ADDRESS OF DESC FIELD.                 
         MVC   TPLEN,MAXLFLD1                                                   
         BAS   RE,BLDBLK                                                        
         MVC   CEU1(CUSERLNQ),USERBLK                                           
*                                                                               
         LA    R2,USME2H           R2-->ESTIMATE2 HEADER ON SCREEN.             
         ST    R2,ADESCH           STORE ADDRESS OF DESC FIELD.                 
         MVC   TPLEN,MAXLFLD2                                                   
         BAS   RE,BLDBLK                                                        
         MVC   CEU2(CUSERLNQ),USERBLK                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
*#####################################################################*         
*   BECAUSE SPOT HEADER RECORDS (BEGIN W/. X'00') WERE FIXED LENGTH   *         
*   RECORDS AT THE TIME SPOT FILES WERE CREATED, THEY DO NOT REALLY   *         
*   USE GETREC/PUTREC.  HENCE, DMWORK+4 NEEDS TO BE FUDGED HERE TO    *         
*     PREVENT THE PUTREC DRAMA IN GENCON.  THE CAVIAT IS, HOWEVER,    *         
*     THAT WE NEED TO BE SURE NO I/O IS DONE (OR FILE IS REREAD IF    *         
*                  I/O WAS NECESSARY) WITHIN VALREC.                  *         
*                                                                     *         
         MVC   DMWORK+4(L'DMDSKADD),DMDSKADD                                    
*#####################################################################*         
*                                                                               
XVR      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*======================== BUILD DATA BLOCKS =========================*          
*                                                                               
BLDBLK   NTR1                                                                   
*         ON ENTRY, R2-->DESCRIPTION FIELD OF RESPECTIVE DATA                   
*                         SECTION ON SCREEN.                                    
*                   TPLEN=LENGTH OF RESPECTIVE TYPE.                            
*                                                                               
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
*                                                                               
         XC    USERBLK,USERBLK                                                  
         XC    FLAG,FLAG                                                        
*                                                                               
** DESCRIPTION FIELD                                                            
*                                                                               
         ZIC   R1,5(R2)            R1=L(DESCRIPTION INPUT).                     
         OR    R1,R1               ANY INPUT?                                   
         BZ    BLDBLK04             NO, CHECK OTHER FIELDS.                     
         BCTR  R1,0                                                             
         EXMVC R1,USRDESC,8(R2)    DESCRIPTION.                                 
         OI    FLAG,DESCQ          DESCRIPTION GIVEN.                           
*                                                                               
BLDBLK04 ZIC   R1,0(R2)            R1=L(ENTIRE FIELD).                          
         AR    R2,R1               R2-->A PROTECTED FIELD.                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->"REQUIRED?" FIELD.                      
*                                                                               
** "REQUIRED?" FIELD                                                            
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    BLDBLK05                                                         
*                                                                               
         TM    FLAG,DESCQ          DESCRIPTION GIVEN?                           
         BZ    DSERR1               NOPE, DOESN'T MAKE SENSE W/O DESC.          
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BNE   *+12                                                             
         OI    USRFLAG,CFLGREQQ    YES, REQUIRED INDICATED.                     
         B     BLDBLK05                                                         
         CLI   8(R2),C'N'          ONLY ALLOWING 'Y', 'N', OR ' '               
         BE    BLDBLK05             AS VALID INPUT.                             
         CLI   8(R2),C' '                                                       
         BNE   IPERR1                                                           
*                                                                               
BLDBLK05 ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD.                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->EDIT RULE FIELD.                        
*                                                                               
** EDIT RULE FIELD                                                              
*                                                                               
BLDBLK10 CLI   5(R2),0             ANY EDIT RULE INPUT?                         
         BE    BLDBLK11             NO, STORE INPUT ANYWAY                      
         TM    FLAG,DESCQ           YES, SO ANY DESCRIPTION INPUT?              
         BZ    DSERR1                NO, MISSING DESCRIPTION                    
*                                                                               
         CLI   8(R2),C'C'          CHECK CHARACTER TYPE.                        
         BE    BLDBLK11                                                         
         CLI   8(R2),C'D'          CHECK DATE TYPE.                             
         BE    BLDBLK11                                                         
         CLI   8(R2),C'N'          CHECK NUMERIC TYPE.                          
         BNE   TLERR2               IT'S NONE OF THE ABOVE, ERROR               
*                                                                               
BLDBLK11 MVC   USRTYPE,8(R2)                                                    
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD                         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->MAX LENGTH FIELD                        
*                                                                               
** MAX LENGTH FIELD                                                             
*                                                                               
BLDBLK12 TM    FLAG,DESCQ          ANY DESCRIPTION INPUT?                       
         BO    BLDBLK13             YES, GET INPUT                              
         CLI   5(R2),0              NO, BUT WAS LENGTH INPUTTED?                
         BNE   DSERR1                YEP, THUS, MISSING DESC.                   
         B     BLDBLK15              NOPE, DO NEXT FIELD                        
*                                                                               
BLDBLK13 GOTO1 ANY                 DESC. GIVEN, NEED LENGTH INPUT               
*                                                                               
         TM    4(R2),X'08'         VALID NUMERICS?                              
         BZ    TLERR3               NOPE                                        
*                                                                               
         ZIC   R1,5(R2)             YEP, CONVERT INPUT INTO BINARY              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  MYDUB,8(0,R2)                                                    
         CVB   R0,MYDUB            R0=L(DESCRIPTION) IN BINARY.                 
*                                                                               
         OR    R0,R0               CHECK IF INPUT = 0.                          
         BE    TLERR4                                                           
         CH    R0,TPLEN            CHECK IF INPUT EXCEEDS MAXIMUM               
         BH    TLERR4               ALLOWED.                                    
*                                                                               
         STC   R0,USRLEN           STORE AWAY L(TYPE).                          
*                                                                               
BLDBLK15 ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD.                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD.                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->"SHOW...?"  FIELD.                      
*                                                                               
** "SHOW...?" FIELD                                                             
*                                                                               
BLDBLK20 CLI   5(R2),0                                                          
         BE    BLDBLK50            NO INPUT IN THIS FIELD.                      
*                                                                               
         TM    FLAG,DESCQ          DESCRIPTION GIVEN?                           
         BZ    DSERR1               NOPE, DOESN'T MAKE SENSE W/O DESC.          
*                                                                               
         ZIC   R1,5(R2)            R1=L(SHOW FIELD)                             
         LA    RE,8(R2)                                                         
*                                                                               
BLDBLK30 CLI   0(RE),C'T'                                                       
         BNE   *+12                SHOW FOR TIME ONLY                           
         OI    USRFLAG,CFLGNTBQ                                                 
         B     BLDBLK40                                                         
         CLI   0(RE),C'I'                                                       
         BNE   *+12                SHOW FOR INTEGRATION ONLY                    
         OI    USRFLAG,CFLGNIBQ                                                 
         B     BLDBLK40                                                         
         CLI   0(RE),C'S'                                                       
         BNE   *+12                SHOW FOR SPECIALS ONLY                       
         OI    USRFLAG,CFLGNSBQ                                                 
         B     BLDBLK40                                                         
         CLI   0(RE),C'F'                                                       
         BNE   *+12                FRONT OF BILL                                
         OI    USRFLAG,CFLGBFRQ                                                 
         B     BLDBLK40                                                         
         CLC   AGENCY,=C'DF'       SPECIAL FEATURE FOR SAATCHI                  
         BE    *+14                                                             
         CLC   AGENCY,=C'TH'                                                    
         BNE   *+20                                                             
         CLI   0(RE),C'H'                                                       
         BNE   *+12                SHOW IN HEADLINES                            
         OI    USRFLAG,CFLGBHLQ                                                 
         B     BLDBLK40                                                         
         CLI   0(RE),C'N'                                                       
         BNE   *+12                SHOW FOR NONE                                
         NI    USRFLAG,X'FF'-CFLGNTBQ-CFLGNIBQ-CFLGNSBQ                         
         B     BLDBLK40                                                         
         CLI   0(RE),C'Y'                                                       
         BNE   IPERR1              SHOW FOR ALL                                 
         OI    USRFLAG,CFLGNTBQ+CFLGNIBQ+CFLGNSBQ                               
*                                                                               
BLDBLK40 LA    RE,1(RE)                                                         
         BCT   R1,BLDBLK30                                                      
*                                                                               
         TM    USRFLAG,CFLGBFRQ+CFLGBHLQ    IF 'FRONT' OR 'HEADS' ON            
         BZ    BLDBLK44                                                         
         TM    USRFLAG,CFLGNTBQ+CFLGNIBQ+CFLGNSBQ  BUT NOTHING ELSE             
         BNZ   BLDBLK44                                                         
         OI    USRFLAG,CFLGNTBQ+CFLGNIBQ+CFLGNSBQ  SET ON ALL TYPES             
*                                                                               
BLDBLK44 DS    0H                                                               
*                                                                               
BLDBLK50 DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD.                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->"MBI" FIELD.                            
*                                                                               
** "MBI" FIELD                                                                  
*                                                                               
         CLI   5(R2),0                                                          
         BE    XBLDBLK             NO INPUT IN THIS FIELD, EXIT.                
*                                                                               
         TM    FLAG,DESCQ          DESCRIPTION GIVEN?                           
         BZ    DSERR1               NOPE, DOESN'T MAKE SENSE W/O DESC.          
*                                                                               
         CLI   8(R2),C'N'                                                       
         BE    XBLDBLK             PROCESSING NOT NEEDED FOR FIELD.             
         CLI   8(R2),C'Y'                                                       
         BNE   IPERR1              NEEDED "Y" OR "N".                           
*                                                                               
         OI    USRFLAG,CFLGMXQ     SHOW MX.                                     
*                                                                               
*                                                                               
XBLDBLK  B     EXIT                                                             
         DROP  R6                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== DISPLAY KEY ROUTINE ========================*         
*                                                                               
DK       DS    0H                                                               
*                                                                               
         L     R4,AIO                                                           
         USING CLTHDRD,R4                                                       
*                                                                               
*------------------------------- MEDIA -------------------------------*         
*                                                                               
         LA    R2,MEDTABLE                                                      
DK10     CLI   0(R2),0             END OF ENTRIES IN TABLE?                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   CKEYAM,0(R2)        DOES AGY/MED MATCH?                          
         BE    DK15                 YEP, GET MEDIA CODE.                        
         LA    R2,2(R2)             NOPE, CHECK NEXT ENTRY.                     
         B     DK10                                                             
DK15     MVC   USMMEDI,1(R2)       MOVE MEDIA ONTO SCREEN.                      
         OI    USMMEDIH+6,X'80'                                                 
*                                                                               
*----------------------------- CLIENT CODE ---------------------------*         
*                                                                               
         GOTO1 CLUNPK,DMCB,CKEYCLT,USMCLNT                                      
         OI    USMCLNTH+6,X'80'                                                 
*                                                                               
         DROP  R4                                                               
*                                                                               
XDK      B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= DISPLAY RECORD ROUTINE ======================*         
*                                                                               
*-------------------- CLEAR OUT DATA FIELDS FIRST --------------------*         
*                                                                               
DR       DS    0H                                                               
         TWAXC USMP1H                                                           
*                                                                               
*----------------------- FILL UP DATA FIELDS ------------------------*          
*                                                                               
         L     R4,AIO                                                           
         USING CLTHDRD,R4                                                       
*                                                                               
         MVC   USERBLK,CPU1                                                     
         LA    R2,USMP1H                                                        
         BAS   RE,DISBLK                                                        
*                                                                               
         MVC   USERBLK,CPU2                                                     
         LA    R2,USMP2H                                                        
         BAS   RE,DISBLK                                                        
*                                                                               
         MVC   USERBLK,CEU1                                                     
         LA    R2,USME1H                                                        
         BAS   RE,DISBLK                                                        
*                                                                               
         MVC   USERBLK,CEU2                                                     
         LA    R2,USME2H                                                        
         BAS   RE,DISBLK                                                        
*                                                                               
         DROP  R4                                                               
XDR      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*========================= DISPLAY DATA BLOCK ========================*         
*                                                                               
DISBLK   NTR1                                                                   
*         ON ENTRY:                                                             
*            R2-->RESPECTIVE DESCRIPTION FIELD-HEADER, AND                      
*            USERBLK : CONTAINS DATA TO SHOW ON SCREEN.                         
*                                                                               
         OC    USRDESC,USRDESC     CHECK IF THERE ARE ANY DATA TO               
         BZ    XDISBLK              SHOW.                                       
*                                                                               
** DESCRIPTION FIELD                                                            
*                                                                               
         MVC   8(L'USRDESC,R2),USRDESC                                          
*                                                                               
         ZIC   R1,0(R2)            R1=L(ENTIRE FIELD).                          
         AR    R2,R1               R2-->A PROTECTED FIELD.                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->"REQUIRED?" FIELD.                      
*                                                                               
** "REQUIRED?" FIELD                                                            
*                                                                               
         MVI   8(R2),C'N'          ASSUME NO "REQUIRED".                        
         TM    USRFLAG,CFLGREQQ    IS "REQUIRED" INDICATED?                     
         BZ    DISBLK05             NOPE, ASSUMPTION IS CORRECT.                
         MVI   8(R2),C'Y'          "REQUIRED" INDICATED.                        
*                                                                               
DISBLK05 ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD.                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->EDIT RULE FIELD.                        
*                                                                               
** EDIT RULE FIELD                                                              
*                                                                               
DISBLK10 MVC   8(1,R2),USRTYPE     USER TYPE ( /C/D/N).                         
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2--PROTECTED FIELD.                         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2--MAX LENGTH FIELD.                        
*                                                                               
** MAX LENGTH FIELD                                                             
*                                                                               
         EDIT  USRLEN,(2,8(R2)),FILL=0                                          
*                                                                               
DISBLK15 ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD.                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD.                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->"SHOW...?"  FIELD.                      
*                                                                               
** "SHOW...?"  FIELD                                                            
*                                                                               
DISBLK20 LA    RE,8(R2)                                                         
         TM    USRFLAG,CFLGNTBQ+CFLGNIBQ+CFLGNSBQ                               
         BNO   *+16                 CHECK FOR ALL                               
         MVI   8(R2),C'Y'                                                       
         LA    RE,1(RE)                                                         
         B     DISBLK24                                                         
         BNZ   *+12                 CHECK FOR NONE                              
         MVI   8(R2),C'N'                                                       
         B     DISBLK40                                                         
         TM    USRFLAG,CFLGNTBQ                                                 
         BNO   *+12                 CHECK FOR TIME                              
         MVI   0(RE),C'T'                                                       
         LA    RE,1(RE)                                                         
         TM    USRFLAG,CFLGNIBQ                                                 
         BNO   *+12                 CHECK FOR INTEGRATION                       
         MVI   0(RE),C'I'                                                       
         LA    RE,1(RE)                                                         
         TM    USRFLAG,CFLGNSBQ                                                 
         BNO   *+12                 CHECK FOR SPECIALS                          
         MVI   0(RE),C'S'                                                       
         LA    RE,1(RE)                                                         
*                                                                               
DISBLK24 DS    0H                                                               
         TM    USRFLAG,CFLGBFRQ                                                 
         BNO   *+8                  CHECK FOR FRONT OF BILL                     
         MVI   0(RE),C'F'                                                       
         TM    USRFLAG,CFLGBHLQ                                                 
         BNO   *+8                  CHECK FOR HEADLINES OF BILL                 
         MVI   0(RE),C'H'                                                       
*                                                                               
DISBLK40 DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD.                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->"MBI" FIELD.                            
*                                                                               
** "MBI" FIELD                                                                  
*                                                                               
         MVI   8(R2),C'N'          ASSUME NO.                                   
         TM    USRFLAG,CFLGMXQ     SHOW MBI                                     
         BZ    XDISBLK              NOPE.                                       
         MVI   8(R2),C'Y'                                                       
*                                                                               
*                                                                               
XDISBLK  B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================== MESSAGES =============================*         
MISSERR  MVI   ERROR,MISSING       TELL USER OF MISSING INPUT.                  
         GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
INVLERR  MVI   ERROR,INVALID       TELL USER OF INVALID INPUT.                  
         GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
NUMBERR  MVI   ERROR,NOTNUM        TELL USER WE NEED NUMERIC INPUT.             
         GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
ERREXGO  GOTO1 ERREX                                                            
*                                                                               
ACTERR   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'ACTERRMS),ACTERRMS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
ACTERRMS DC    C'** ERROR ** CAN ONLY CHANGE/DISPLAY USER RECORDS'              
*                                                                               
DSERR1   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'DSERR1MS),DSERR1MS                                     
         OI    CONHEADH+6,X'80'                                                 
         L     R2,ADESCH           POINT TO PLACE OF ERROR.                     
         GOTO1 ERREX2                                                           
DSERR1MS DC    C'** ERROR ** DESCRIPTION NEEDED'                                
*                                                                               
IPERR1   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'IPERR1MS),IPERR1MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
IPERR1MS DC    C'** ERROR ** VALID INPUTS ARE "Y","N","T","I","S",ONLY'         
*                                                                               
TLERR2   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'TLERR2MS),TLERR2MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
TLERR2MS DC    C'** ERROR ** VALID TYPES ARE "C", "D", "N", OR BLANK'           
*                                                                               
TLERR3   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'TLERR3MS),TLERR3MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
TLERR3MS DC    C'** ERROR ** LENGTH SPECIFIED IS NOT NUMERIC'                   
*                                                                               
TLERR4   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'TLERR4MS),TLERR4MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
TLERR4MS DC    C'** ERROR ** INVALID LENGTH INPUTTED.'                          
*                                                                               
RCERR3   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'RCERR3MS),RCERR3MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
RCERR3MS DC    C'** ERROR ** U.S. ZIP CODE MUST BE NUMERIC'                     
*                                                                               
RCERR4   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'RCERR4MS),RCERR4MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
RCERR4MS DC    C'** ERROR ** ZIP CODE DOES NOT MATCH COUNTRY'                   
*                                                                               
WTERR1   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'WTERR1MS),WTERR1MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
WTERR1MS DC    C'** ERROR ** MAX VALUE IS 99.99'                                
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== MISCELLANEOUS ===========================*         
         DS    0F                                                               
BLANKS   DC    CL132' '                                                         
**                                                                              
MAXLFLD1 DC    H'32'               MAXIMUM LENGTH OF PROD/EST 1.                
MAXLFLD2 DC    H'16'               MAXIMUM LENGTH OF PROD/EST 2.                
***********************************************************************         
         SPACE 4                                                                
**********************************************************************          
*=========================== LITERAL POOL ===========================*          
         LTORG                                                                  
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*======================= CLIENT RECORDS DSECT =======================*          
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*========================== USEFUL DSECTS ===========================*          
*       ++INCLUDE CTGENFILE                                                     
*       ++INCLUDE DDBIGBOX                                                      
*       ++INCLUDE DDCOMFACS                                                     
*       ++INCLUDE DDSPLWORKD                                                    
*       ++INCLUDE DDSPOOLD                                                      
*       ++INCLUDE FASYSFAC                                                      
*       ++INCLUDE FAPGMLST                                                      
*       ++INCLUDE FASELIST                                                      
*       ++INCLUDE FAFACTS                                                       
*       ++INCLUDE FALANG                                                        
*       ++INCLUDE FATIOB                                                        
*       ++INCLUDE FASYSLSTD                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*=============================== TWA ================================*          
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------*          
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMECD          TWA DSECT FOR RECORD MAINTENANCE.            
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------*          
*                                                                               
       ++INCLUDE DDGENTWA                                                       
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*========================= NESFM WORK AREA ==========================*          
       ++INCLUDE NESFMWORKD                                                     
         SPACE 5                                                                
*                                                                               
*--------------- PUT MY STORAGE DSECT HERE IF NEEDED ----------------*          
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
** STORAGE USED BY NESFM35 AS WELL                                              
*                                                                               
MEDTABLE DS    6CL2                       MEDTABLE                              
*                                     AGY/MED   MEDIA                           
*                                    |--------º-------|                         
*                                    |   +0   º  +1   |                         
*                                    |--------º-------|                         
*                                    |   +2   º  +3   |                         
*                                    :        :       :                         
*                                    :        :       :                         
*                                    |   +11  º  +12  |                         
*                                    |--------º-------|                         
*                                                                               
** CAN PUT ANY STORAGE HERE                                                     
*                                                                               
FLAG     DS    C                                                                
DESCQ    EQU   X'80'               DESCRIPTION GIVEN.                           
**                                                                              
ADESCH   DS    A                   A(DESC FIELD).                               
MYDUB    DS    D                                                                
RELO     DS    F                   RELOCATION FACTOR.                           
TPLEN    DS    H                   MAXIMUM LENGTH OF DESCRIPTION.               
**                                                                              
USERBLK  DS    0CL(CUSERLNQ)                                                    
USRDESC  DS    CL20                USER FIELD DESRIPTION.                       
USRTYPE  DS    CL1                 FIELD TYPE.                                  
USRLEN   DS    XL1                 FIELD LENGTH.                                
USRFLAG  DS    XL1                 USER FLAG.                                   
USRFLG2  DS    XL1                 USER FLAG # 2.                               
USERBLKQ EQU   *-USERBLK           SHOULD = CUSERLNQ.                           
         DS    0CL(USERBLKQ+1-CUSERLNQ)                                         
         DS    0CL(CUSERLNQ+1-USERBLKQ)                                         
*                                                                               
**********************************************************************          
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017NESFM34S  05/01/02'                                      
         END                                                                    
