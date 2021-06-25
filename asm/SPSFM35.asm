*          DATA SET SPSFM35    AT LEVEL 027 AS OF 05/06/15                      
*PHASE T21735A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: SPSFM35<==>T21735 USERFIELD DEFINITION RECORDS MAINT.       *         
*                                                                     *         
*  COMMENTS:                                                          *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (T21700), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREEN SPSFMB0 (T217B0) -- MAINTENANCE                     *         
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
         TITLE 'SPSFM35<==>T21735 USERFIELD DEFINITION RECDS MAINT'             
***********************************************************************         
T21735   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21735*,R7,RR=R3                                              
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
         MVI   NETSW,C'N'                                                       
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         USING COMFACSD,RF                                                      
                                                                                
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           SEE IF NETPAK                                
         BNE   *+8                                                              
         MVI   NETSW,C'Y'                                                       
         DROP  RF                                                               
*                                                                               
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
         CLI   SVAPROF+7,C'C'      FOR CANADIAN AGENCIES, DISALLOW              
         BNE   VK10                 MEDIAS 'C' & 'N'.                           
         MVI   ERROR,INVMED        ASSUME INVALID AT FIRST.                     
         CLI   QMED,C'C'                                                        
         BE    ERREXGO                                                          
         CLI   QMED,C'N'                                                        
         BE    ERREXGO                                                          
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
         MVC   SVKEY,KEY                                                        
         CLI   SVAPROF+7,C'C'     IF CANADIAN AGENCY                            
         BNE   VR10                                                             
         CLI   QMED,C'T'          AND MEDIA T                                   
         BNE   VR10                                                             
         BAS   RE,CKCAN           CHANGE MEDIAS N & C                           
*                                                                               
*####################################################################*          
*  BECAUSE SPOT HEADER RECORDS (BEGIN W/. X'00') WERE FIXED LENGTH   *          
*  RECORDS AT THE TIME SPOT FILES WERE CREATED, THEY DO NOT REALLY   *          
*  USE GETREC/PUTREC.  HENCE, DMWORK+4 NEEDS TO BE FUDGED HERE TO    *          
*    PREVENT THE PUTREC DRAMA IN GENCON.  THE CAVIAT IS, HOWEVER,    *          
*    THAT WE NEED TO BE SURE NO I/O IS DONE (OR FILE IS REREAD IF    *          
*                 I/O WAS NECESSARY) WITHIN VALREC.                  *          
*#####################################################################          
*                                                                               
VR10     BAS   RE,SETUDEF                                                       
         MVC   DMWORK+4(L'DMDSKADD),DMDSKADD                                    
*                                                                               
XVR      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
SETUDEF  NTR1                                                                   
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
*                                                                               
         LA    R2,USMP1H           R2-->PRODUCT1 HEADER ON SCREEN.              
         ST    R2,ADESCH           STORE ADDRESS OF DESC FIELD.                 
         MVI   RECSW,C'P'          SET DOING PRODUCT USER                       
         MVC   TPLEN,MAXLFLD1                                                   
         BAS   RE,BLDBLK                                                        
         MVC   CPU1(CUSERLNQ),USERBLK                                           
*                                                                               
         LA    R2,USMP2H           R2-->PRODUCT2 HEADER ON SCREEN.              
         ST    R2,ADESCH           STORE ADDRESS OF DESC FIELD.                 
         MVI   RECSW,C'P'          SET DOING PRODUCT USER                       
         MVC   TPLEN,MAXLFLD2                                                   
         BAS   RE,BLDBLK                                                        
         MVC   CPU2(CUSERLNQ),USERBLK                                           
*                                                                               
         LA    R2,USME1H           R2-->ESTIMATE1 HEADER ON SCREEN.             
         ST    R2,ADESCH           STORE ADDRESS OF DESC FIELD.                 
         MVI   RECSW,C'E'          SET DOING ESTIMATE USER                      
         MVC   TPLEN,MAXLFLD1                                                   
         BAS   RE,BLDBLK                                                        
         MVC   CEU1(CUSERLNQ),USERBLK                                           
*                                                                               
         LA    R2,USME2H           R2-->ESTIMATE2 HEADER ON SCREEN.             
         ST    R2,ADESCH           STORE ADDRESS OF DESC FIELD.                 
         MVI   RECSW,C'E'          SET DOING ESTIMATE USER                      
         MVC   TPLEN,MAXLFLD2                                                   
         BAS   RE,BLDBLK                                                        
         MVC   CEU2(CUSERLNQ),USERBLK                                           
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*======================== BUILD DATA BLOCKS =========================*          
*                                                                               
BLDBLK   NTR1                                                                   
*         ON ENTRY, R2-->DESCRIPTION FIELD OF RESPECTIVE DATA                   
*                         SECTION ON SCREEN.                                    
*                   TPLEN=LENGTH OF RESPECTIVE TYPE.                            
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
         CLI   RECSW,C'P'          SEE IF DOING PRODUCT USER                    
         BE    BLDBLK02                                                         
*                                                                               
         CLI   NETSW,C'Y'          IS THIS NETPAK?                              
         BE    BLDBLK02           IF SO, B NOT ALLOWED (YET?)                   
*                                                                               
         CLI   8(R2),C'B'          B ONLY FOR ESTIMATE                          
         BNE   *+12                                                             
         OI    USRFLG2,CFLGREQB    YES, REQUIRED FOR BILLING                    
         B     BLDBLK05                                                         
BLDBLK02 CLI   8(R2),C'N'          ONLY ALLOWING 'Y', 'N', OR ' '               
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
         BE    BLDBLK25            NO INPUT IN THIS FIELD.                      
*                                                                               
         TM    FLAG,DESCQ          DESCRIPTION GIVEN?                           
         BZ    DSERR1               NOPE, DOESN'T MAKE SENSE W/O DESC.          
*                                                                               
         CLI   8(R2),C'N'                                                       
         BE    BLDBLK25            PROCESSING NOT NEEDED FOR FIELD.             
         CLI   8(R2),C'Y'                                                       
         BNE   BLDBLK22            PROCESSING NOT NEEDED FOR FIELD.             
         OI    USRFLAG,CFLGSPQ     SHOW ON BILLS FOR SPOTPACK.                  
         B     BLDBLK25                                                         
*                                                                               
BLDBLK22 DS    0H                                                               
         CLI   8(R2),C'F'          UDEF IN 'FRONT' OF BILL                      
         BNE   *+12                NEEDED "Y","N","F","H"                       
         OI    USRFLAG,CFLGSPQ+CFLGBFRQ                                         
         B     BLDBLK25                                                         
*                                                                               
*        CLC   AGENCY,=C'DF'       SPECIAL FEATURE FOR SAATCHI                  
*        BE    *+24                                                             
*        CLC   AGENCY,=C'YN'       AND YNR                                      
*        BE    *+14                                                             
*        CLC   AGENCY,=C'TH'                                                    
*        BNE   IPERR2                                                           
*                                                                               
         CLI   QMED,C'T'           FOR MEDIA T, R, X. . .                       
         BE    *+20                                                             
         CLI   QMED,C'R'                                                        
         BE    *+12                                                             
         CLI   QMED,C'X'                                                        
         BNE   BLDBLK23                                                         
*                                                                               
         CLI   8(R2),C'H'          UDEF IN HEADLINES ON BILL                    
         BNE   IPERR2              NEEDED "Y","N","F","H"                       
         OI    USRFLAG,CFLGSPQ+CFLGBHLQ                                         
         B     BLDBLK25                                                         
*                                                                               
BLDBLK23 B     IPERR2              FOR NETWORK, INVALID FOR NOW                 
*                                                                               
BLDBLK25 ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD.                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->"A2" FIELD.                             
*                                                                               
** "A2" FIELD                                                                   
*                                                                               
BLDBLK30 CLI   5(R2),0                                                          
         BE    BLDBLK35            NO INPUT IN THIS FIELD.                      
*                                                                               
         TM    FLAG,DESCQ          DESCRIPTION GIVEN?                           
         BZ    DSERR1               NOPE, DOESN'T MAKE SENSE W/O DESC.          
*                                                                               
         CLI   8(R2),C'N'                                                       
         BE    BLDBLK35            PROCESSING NOT NEEDED FOR FIELD.             
         CLI   8(R2),C'Y'                                                       
         BNE   IPERR1              NEEDED "Y" OR "N".                           
*                                                                               
         OI    USRFLAG,CFLGA2Q     SHOW A2.                                     
*                                                                               
BLDBLK35 DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD.                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->"MBI" FIELD.                            
*                                                                               
** "MBI" FIELD                                                                  
*                                                                               
BLDBLK40 CLI   5(R2),0                                                          
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
XBLDBLK  B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*    CHECK MEDIA C AND N RECORDS TOO                                            
*    SETS HALF(1)= AGY/MED FOR C AND HALF+1(1)= AGY/MED FOR N                   
CKCAN    NTR1                                                                   
         MVC   AIO,AIO2                                                         
         MVC   HALF,=H'0'                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,6              AGENCY RECORD                                 
         MVC   KEY+1(2),AGENCY    AGENCY ALPHA                                  
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'               MUST BE ABLE TO FIND AGENCY RECORD            
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
         MVI   ELCODE,2                                                         
*                                                                               
CKCN10   BAS   RE,NEXTEL                                                        
         BNE   CKCN30                                                           
*                                                                               
         CLI   2(R6),C'C'                                                       
         BNE   CKCN20                                                           
         MVC   HALF(1),3(R6)      SAVE BINARY AGY/MED FOR MED 'C'               
         B     CKCN10                                                           
*                                                                               
CKCN20   CLI   2(R6),C'N'                                                       
         BNE   CKCN10                                                           
         MVC   HALF+1(1),3(R6)    SAVE BINARY AGY/MED FOR MED C'N'              
         B     CKCN10                                                           
*                                                                               
CKCN30   CLI   HALF,0                                                           
         BE    CKCN40                                                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),HALF                                                    
         MVC   KEY+2(2),SVKEY+2                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CKCN40                                                           
         GOTO1 GETREC                                                           
         BAS   RE,SETUDEF                                                       
         MVC   DMWORK+4(L'DMDSKADD),DMDSKADD                                    
         GOTO1 PUTREC                                                           
*                                                                               
CKCN40   CLI   HALF+1,0                                                         
         BE    CKCNX                                                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),HALF+1                                                  
         MVC   KEY+2(2),SVKEY+2                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CKCNX                                                            
         GOTO1 GETREC                                                           
         BAS   RE,SETUDEF                                                       
         MVC   DMWORK+4(L'DMDSKADD),DMDSKADD                                    
         GOTO1 PUTREC                                                           
*                                                                               
CKCNX    MVC   AIO,AIO1            GET RECORD BACK                              
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         XIT1                                                                   
         EJECT                                                                  
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
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),USMCLNT                            
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
         CLI   ACTNUM,ACTSEL      TEST IF FROM LIST SCREEN                      
         BNE   DR10                                                             
         LA    R3,LISTDIR          LIST SCREEN INFO                             
         CLI   SELLISTN,0          SEE IF FIRST                                 
         BE    DR3                                                              
         ZIC   R0,SELLISTN         RELATIVE LINE NUMBER                         
         LA    R3,6(R3)            NEXT LIST ENTRY                              
         BCT   R0,*-4                                                           
*                                                                               
DR3      CLI   0(R3),C'D'          SEE IF TRYING TO DELETE FROM LIST            
         BNE   DR10                                                             
         B     ACTERR                                                           
*                                                                               
DR10     DS    0H                                                               
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
         BNO   DISBLK04             NOPE, ASSUMPTION IS CORRECT.                
         MVI   8(R2),C'Y'          "REQUIRED" INDICATED.                        
         B     DISBLK05                                                         
*                                                                               
DISBLK04 DS    0H                                                               
         TM    USRFLG2,CFLGREQB    REQUIRED FOR BILLING?                        
         BNO   *+8                                                              
         MVI   8(R2),C'B'          "REQUIRED" INDICATED.                        
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
DISBLK20 MVI   8(R2),C'N'          ASSUME NO.                                   
         TM    USRFLAG,CFLGSPQ     SHOW ON BILLS FOR SPOTPACK?                  
         BZ    DISBLK25            NO                                           
         MVI   8(R2),C'Y'          YES                                          
         TM    USRFLAG,CFLGBFRQ    TEST UDEF AT 'FRONT'                         
         BZ    *+12                                                             
         MVI   8(R2),C'F'          YES                                          
         B     DISBLK25                                                         
         TM    USRFLAG,CFLGBHLQ    TEST UDEF IN HEADLINES ON BILL               
         BZ    *+8                                                              
         MVI   8(R2),C'H'          YES                                          
*                                                                               
DISBLK25 ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD.                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->"A2" FIELD.                             
*                                                                               
** "A2" FIELD                                                                   
*                                                                               
DISBLK30 MVI   8(R2),C'N'          ASSUME NO.                                   
         TM    USRFLAG,CFLGA2Q     SHOW A2?                                     
         BZ    DISBLK35             NOPE.                                       
         MVI   8(R2),C'Y'                                                       
*                                                                               
DISBLK35 DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED FIELD.                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->"MBI" FIELD.                            
*                                                                               
** "MBI" FIELD                                                                  
*                                                                               
DISBLK40 MVI   8(R2),C'N'          ASSUME NO.                                   
         TM    USRFLAG,CFLGMXQ     SHOW MBI                                     
         BZ    XDISBLK              NOPE.                                       
         MVI   8(R2),C'Y'                                                       
*                                                                               
XDISBLK  B     EXIT                                                             
         EJECT                                                                  
*============================== MESSAGES =============================*         
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
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
IPERR1MS DC    C'** ERROR ** VALID INPUTS ARE "Y", "N", OR " " ONLY'            
*                                                                               
IPERR2   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'IPERR2MS),IPERR2MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
IPERR2MS DC    C'** ERROR ** VALID INPUTS ARE "Y", "N", "F", OR " " ONL+        
               Y'                                                               
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
NETSW    DS    CL1                 N, Y = NETPAK                                
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
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------*          
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMB0D          TWA DSECT FOR RECORD MAINTENANCE.            
*                                                                               
*--------------------------------------------------------------------*          
*                                                                               
       ++INCLUDE DDGENTWA                                                       
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*========================= SPSFM WORK AREA ==========================*          
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
*                                                                               
*--------------- PUT MY STORAGE DSECT HERE IF NEEDED ----------------*          
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
** STORAGE USED BY SPSFM36 AS WELL                                              
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
RECSW    DS    CL1                 P=PRD,E=EST USER FIELDS                      
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027SPSFM35   05/06/15'                                      
         END                                                                    
