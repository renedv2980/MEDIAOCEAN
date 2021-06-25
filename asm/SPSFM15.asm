*          DATA SET SPSFM15    AT LEVEL 031 AS OF 11/06/18                      
*PHASE T21715A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: SPSFM15<==>T21715 SPOT ADDRESS RECORDS MAINTENANCE.         *         
*                                                                     *         
*  COMMENTS: USING SFM TO HANDLE SPOT STATION-FILES.                  *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (T21700), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREEN SPSFMB2 (T217B2) -- MAINTENANCE                     *         
*                                                                     *         
*  OUTPUTS: UPDATED ADDRESS RECORDS                                   *         
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
         TITLE 'SPSFM15<==>T21715 SPOT ADDRESS RECORDS MAINTENANCE'             
***********************************************************************         
T21715   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21715*,R7,RR=R3                                              
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
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    MAIN10                                                           
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    MAIN10                                                           
         MVI   ERROR,NOTAUTH       NOT AUTHORIZED FOR THIS FUNCTION             
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    NOTAUTHD                                                         
         CLI   ACTNUM,ACTCHA       ACTION CHANGE?                               
         BE    NOTAUTHD                                                         
         CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         BNE   MAIN10                                                           
*                                                                               
NOTAUTHD MVI   GETMSYS,2           CHANGE TO X'02' ERROR SYSTEM                 
         LA    R2,CONACTH          ACTION ERROR                                 
         B     ERREXGO                                                          
*                                                                               
MAIN10   BAS   RE,INIT                                                          
         CLI   MODE,SETFILE        SET FILE                                     
         BE    SF                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECDEL         DELETE RECORD.                               
         BE    DELR                                                             
         CLI   MODE,XRECREST       RESTORE RECORD.                              
         BE    RSTR                                                             
*                                                                               
EXIT     XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================== SET FILE ROUTINE =========================*         
SF       DS    0H                                                               
         MVI   ACTELOPT,C'N'                                                    
         MVI   USEIO,C'Y'                                                       
*                                                                               
         MVC   SYSDIR,=C'STATION '                                              
         MVC   SYSFIL,=C'STATION '                                              
         SR    R1,R1                                                            
         LA    R1,ADDKEYLQ                                                      
         STH   R1,LKEY                                                          
*                                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
*                                                                               
VK       DS    0H                                                               
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   LKEY,=H'13'         L(AGENCY RECORD)=13.                         
         MVI   USEIO,C'N'                                                       
*                                                                               
*--------------------------- MEDIA FIELD -----------------------------*         
*                                                                               
         LA    R2,ADMMEDIH         CHECK FOR ANY MEDIA INPUT.                   
         GOTO1 ANY                                                              
         CLI   ADMMEDI,C'*'        CHECK FOR MEDIA = 'ALL'                      
         BNE   VK05                                                             
         MVI   QMED,C'T'                                                        
         B     VK10                                                             
*                                                                               
VK05     GOTO1 VALIMED                                                          
         CLI   SVAPROF+7,C'C'      FOR CANADIAN AGENCIES, DISALLOW              
         BNE   VK10                 MEDIAS 'C' & 'N'.                           
         MVI   ERROR,INVMED                                                     
         CLI   QMED,C'C'                                                        
         BE    ERREXGO                                                          
         CLI   QMED,C'N'                                                        
         BE    ERREXGO                                                          
*                                                                               
*-------------------------- STATION FIELD ----------------------------*         
*                                                                               
VK10     LA    R2,ADMSTATH                                                      
         GOTO1 ANY                                                              
         CLI   ADMMEDI,C'*'        CHECK FOR MEDIA = 'ALL'                      
         BNE   VK20                                                             
*                                                                               
** MEDIA = '*' (='ALL')                                                         
*                                                                               
         MVI   ERROR,INVSTAT       ASSUME INVALID STATION                       
         CLI   ADMSTATH+5,4        L'INPUT = 4                                  
         BNE   ERREXGO                                                          
         CLC   =C'BOX',ADMSTAT     FOR MEDIA=ALL, STATION                       
         BNE   ERREXGO              IS BOX*, WHERE * = A..Z, 0..9               
         LA    R3,ALPHANUM         R3-->VALID ALPHANUMERICS                     
VK12     CLC   ADMSTAT+3(1),0(R3)                                               
         BE    VK15                                                             
         LA    R3,1(R3)                                                         
         CLI   0(R3),0             X'00' IS EOTABLE MARKER                      
         BE    INVLERR                                                          
         B     VK12                                                             
VK15     MVC   QSTANEW(4),ADMSTAT                                               
         MVI   QSTANEW+4,C'T'      APPEND 'T' AT END                            
         B     VKKEY               GO BUILD KEY                                 
*                                                                               
** MEDIA <> ALL                                                                 
*                                                                               
VK20     MVI   ERROR,INVSTAT       ASSUME INVALID STATION ERROR                 
*                                                                               
         CLC   ADMSTAT(3),=C'BOX'  STATION = 'BOX*' IS ONLY FOR                 
         BE    ERREXGO              MEDIA 'T'                                   
*                                                                               
         CLI   8(R2),C'0'          CHECK IF IT IS CABLE                         
         BL    VK25                                                             
         CLI   8(R2),C'9'          IF IT IS CABLE, THEN INPUT SHOULD            
         BH    VK15                                                             
         CLI   5(R2),4              NOT HAVE MORE THAN 4 DIGITS                 
         BH    ERREXGO                                                          
*                                                                               
VK25     GOTO1 VALISTA                                                          
*                                                                               
         CLI   8(R2),C'0'          IF CABLE                                     
         BL    *+8                                                              
         MVI   QSTANEW+4,C'T'      APPEND 'T' AT END                            
*---------------------------- BUILD KEY ------------------------------*         
*                                                                               
VKKEY    XC    KEY,KEY                                                          
         MVC   KEY,ZEROES                                                       
         LA    R4,KEY                                                           
         USING ADRRECD,R4                                                       
         MVI   ADDKTYPE,ADDKTYPQ   ADDRESS RECORDS ARE TYPE-'A'.                
         MVC   ADDKMED,QMED        MEDIA.                                       
         MVC   ADDKCALL,QSTANEW    CALL LETTERS.                                
         MVC   ADDKAGY,AGENCY      AGENCY.                                      
         DROP  R4                                                               
*                                                                               
XVK      B     SF                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== VALIDATE RECORD ROUTINE ======================*         
*                                                                               
VR       DS    0H                                                               
         NI    RECFLAG,X'FF'-RECCHNGD                                           
*                                                                               
*----------------------- VALIDATE STATION NAME -----------------------*         
*                                                                               
         LA    R2,ADMNAMEH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VR10                                                             
         OI    RECFLAG,RECCHNGD    RECORD HAS CHANGED                           
         GOTO1 ANY                                                              
         OI    4(R2),X'20'                                                      
*                                                                               
*---------------------- VALIDATE STREET ADDRESS ----------------------*         
*                                                                               
VR10     LA    R2,ADMADDRH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VR15                                                             
         OI    RECFLAG,RECCHNGD    RECORD HAS CHANGED                           
         GOTO1 ANY                                                              
         OI    4(R2),X'20'                                                      
*                                                                               
*--------------------------- VALIDATE CITY ---------------------------*         
*                                                                               
VR15     LA    R2,ADMCITYH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VR17                                                             
         OI    RECFLAG,RECCHNGD    RECORD HAS CHANGED                           
         GOTO1 ANY                                                              
         OI    4(R2),X'20'                                                      
*                                                                               
*-------------------------- VALIDATE COUNTRY -------------------------*         
*                                                                               
VR17     TM    ADMCTRYH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BO    *+8                                                              
         OI    RECFLAG,RECCHNGD    RECORD HAS CHANGED                           
         CLI   ADMCTRYH+5,0        ANY INPUT?                                   
         BNE   VR20                 YEP                                         
         OI    ADMCTRYH+6,X'80'     TRANSMIT                                    
         MVI   ADMCTRY,C'U'        NO INPUT-->DEFAULT TO U.S.                   
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   VR20                 NO, LEAVE AS U.S.                           
         MVI   ADMCTRY,C'C'        DEFAULT TO CANADA                            
VR20     CLI   ADMCTRY,C'C'        IS IT CANADA?                                
         BE    VR30                                                             
         CLI   ADMCTRY,C'U'        IS IT U.S.?                                  
         BNE   RCERR1                                                           
         OI    4(R2),X'20'                                                      
*                                                                               
*--------------------- VALIDATE STATE AND ZIP CODE -------------------*         
*                                                                               
VR30     LA    R2,ADMSTTEH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VR35                                                             
         OI    RECFLAG,RECCHNGD    RECORD HAS CHANGED                           
         GOTO1 ANY                                                              
         CLI   5(R2),2             AT LEAST A 2-LETTER STATE.                   
         BL    INVLERR                                                          
*                                                                               
         CLI   5(R2),3             IF 3 CHARACTERS WERE INPUTTED,               
         BNE   VR35                                                             
         CLI   ADMCTRY,C'U'         IT HAD BETTER NOT BE FOR U.S.               
         BE    INVLERR                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VR35     LA    R2,ADMBZIPH                                                      
         GOTO1 ANY                                                              
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VR40                                                             
         OI    RECFLAG,RECCHNGD    RECORD HAS CHANGED                           
         OI    4(R2),X'20'                                                      
*                                                                               
VR40     LA    R2,ADMSAPH                                                       
         CLI   SAPAGY,C'Y'         TEST SAP AGY                                 
         BNE   VR40X                                                            
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VR40X                                                            
         GOTO1 ANY                                                              
VR40X    DS    0H                                                               
*                                                                               
*---------------------------- BUILD RECORD ---------------------------*         
*                                                                               
VR100    L     R4,AIO                                                           
         USING ADRRECD,R4                                                       
         MVC   SAVEDATE,ADDRCHDT                                                
         MVC   SVPASSWD,ADDRCHBY                                                
         XCEF  (R4),2000                                                        
         MVC   ADDRKEY(ADDKEYLQ),KEY      KEY OF RECORD.                        
*                                                                               
         LHI   R0,ADRREC2Q                                                      
         CLI   SAPAGY,C'Y'         TEST SAP AGY                                 
         BNE   *+8                                                              
         LHI   R0,ADRREC3Q                                                      
         STCM  R0,3,ARECL          SET LENGTH OF RECORD.                        
*                                                                               
         MVC   ANAME,ADMNAME       STATION NAME.                                
*                                                                               
         MVC   A1LINE,ADMADDR      STREET ADDRESS.                              
*                                                                               
         MVC   A2LINE,ADMCITY      CITY.                                        
*                                                                               
         MVC   A3LINE,BLANKS       BLANK PAD STATE.                             
         ZICM  R1,ADMSTTEH+5,(1)                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         EXMVC R1,A3LINE,ADMSTTE   STATE.                                       
*                                                                               
VR110    XC    AZIP,AZIP                                                        
         CLI   ADMCTRY,C'C'                                                     
         BNE   *+10                                                             
         MVC   AZIP(5),=C'CANAD'                                                
*                                                                               
         MVC   ABIGZIP,BLANKS      BLANKS PAD ZIP CODE.                         
         ZICM  R1,ADMBZIPH+5,(1)                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         EXMVC R1,ABIGZIP,ADMBZIP                                               
*                                                                               
         MVC   ADDRCOM,ADMCOMM                                                  
         TM    ADMCOMMH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BO    *+12                                                             
         OI    RECFLAG,RECCHNGD    RECORD HAS CHANGED                           
         OI    ADMCOMMH+4,X'20'    PREVIOUSLY VALIDATED?                        
*                                                                               
         MVC   ADREMAIL,ADMMAIL    E-MAIL                                       
         TM    ADMMAILH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BO    *+12                                                             
         OI    RECFLAG,RECCHNGD    RECORD HAS CHANGED                           
         OI    ADMMAILH+4,X'20'    PREVIOUSLY VALIDATED?                        
*                                                                               
         CLI   SAPAGY,C'Y'         TEST SAP AGY                                 
         BNE   VR112                                                            
         MVC   ADRSAP,SPACES                                                    
         LLC   RE,ADMSAPH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ADRSAP(0),ADMSAP                                                 
*                                                                               
         TM    ADMSAPH+4,X'20'     PREVIOUSLY VALIDATED                         
         BO    *+12                                                             
         OI    RECFLAG,RECCHNGD    RECORD HAS CHANGED                           
         OI    ADMSAPH+4,X'20'     SET PREVIOUSLY VALIDATED                     
*                                                                               
VR112    CLI   ACTNUM,ACTADD                                                    
         BE    VR114                                                            
         MVC   ADDRCHDT,SAVEDATE                                                
         MVC   ADDRCHBY,SVPASSWD                                                
         TM    RECFLAG,RECCHNGD    HAS REC PREVIOUSLY BEEN CHANGED?             
         BZ    VR115                                                            
VR114    GOTO1 DATCON,DMCB,(5,0),(3,ADDRCHDT)   GET CURRENT DATE                
         MVC   ADDRCHBY,SVMYPSWD   PERSONAL ID                                  
*                                                                               
VR115    DS    0H                                                               
         DROP  R4                                                               
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR130                                                            
         GOTO1 CNRECS,DMCB,C'A'                                                 
         B     VR140                                                            
VR130    GOTO1 CNRECS,DMCB,C'C'                                                 
*                                                                               
VR140    BAS   RE,REQREC                                                        
         B     DR                                                               
*                                                                               
XVR      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*======================== DISPLAY KEY ROUTINE ========================*         
DK       CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    DK10                                                             
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    DK10                                                             
         MVI   ERROR,NOTAUTH       NOT AUTHORIZED FOR THIS FUNCTION             
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE?                           
         BE    NOTAUTHD                                                         
*                                                                               
DK10     L     R4,AIO                                                           
         USING ADRRECD,R4                                                       
*                                                                               
*------------------------ STATION CALL LETTERS -----------------------*         
*                                                                               
         MVC   QSTANEW,BLANKS                                                   
         MVC   QSTANEW(L'ADDKCALL),ADDKCALL     NEEDED FOR  REQREC.             
         MVC   ADMSTAT(L'ADDKCALL-1),QSTANEW                                    
         OI    ADMSTATH+6,X'80'                                                 
*                                                                               
*------------------------------- MEDIA -------------------------------*         
*                                                                               
         MVC   ADMMEDI(1),ADDKMED                                               
         CLC   ADMSTAT(3),=C'BOX'  IF STATION STARTS WITH 'BOX',                
         BNE   *+8                                                              
         MVI   ADMMEDI,C'*'         MEDIA = '*' (='ALL')                        
         OI    ADMMEDIH+6,X'80'                                                 
*                                                                               
         DROP  R4                                                               
XDK      B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= DISPLAY RECORD ROUTINE ======================*         
*                                                                               
*-------------------- CLEAR OUT DATA FIELDS FIRST --------------------*         
*                                                                               
DR       DS    0H                                                               
         TWAXC ADMNAMEH                                                         
*                                                                               
*----------------------- FILL UP DATA FIELDS ------------------------*          
*                                                                               
         L     R4,AIO                                                           
         USING ADRRECD,R4                                                       
         MVC   ADMNAME,ANAME       STATION NAME.                                
         OI    ADMNAMEH+4,X'20'    PREVIOUSLY VALIDATED?                        
         MVC   ADMADDR,A1LINE      STREET ADDRESS.                              
         OI    ADMADDRH+4,X'20'    PREVIOUSLY VALIDATED?                        
         MVC   ADMCITY,A2LINE      CITY.                                        
         OI    ADMCITYH+4,X'20'    PREVIOUSLY VALIDATED?                        
         MVC   ADMSTTE,A3LINE      STATE.                                       
         OI    ADMSTTEH+4,X'20'    PREVIOUSLY VALIDATED?                        
         MVC   ADMBZIP,ABIGZIP     ZIP CODE.                                    
         OI    ADMBZIPH+4,X'20'    PREVIOUSLY VALIDATED?                        
         MVI   ADMCTRY,C'U'        ASSUME IT'S U.S.                             
         CLC   =C'CANAD',AZIP                                                   
         BNE   *+8                                                              
         MVI   ADMCTRY,C'C'                                                     
         OI    ADMCTRYH+4,X'20'    PREVIOUSLY VALIDATED?                        
         GOTO1 DATCON,DMCB,(3,ADDRCHDT),(5,ADMDATE)    LAST DATE                
         OI    ADMDATEH+6,X'80'                                                 
*                                                                               
* PERSONAL ID                                                                   
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING AUTHD,R3                                                         
         MVC   SECRAGY,SVSECAGY                                                 
         OC    ADDRCHBY,ADDRCHBY   ANY PID HERE?                                
         BNZ   DR25                 - YUP WE HAVE ONE                           
         CLI   QMED,C'T'           IS IT TV?                                    
         BNE   DR25                 - NOPE                                      
         CLI   ADMSTAT,C'0'                                                     
         BNL   DR30                WE NEED THE NCC/DDS DISPLAY                  
*                                                                               
DR25     MVC   PASSWD,ADDRCHBY                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 VALIAUTH,DMCB,WORK   GET PERSONAL ID                             
         MVC   AIO,AIO1                                                         
         MVC   ADMUSID,PRSNLID                                                  
         OI    ADMUSIDH+6,X'80'                                                 
         DROP  R3                                                               
         B     DR50                                                             
*                                                                               
DR30     MVC   ADMUSID,=C'NCC/DDS '                                             
         OI    ADMUSIDH+6,X'80'                                                 
*                                                                               
* COMMENTS                                                                      
DR50     MVC   ADMCOMM,ADDRCOM                                                  
         OI    ADMCOMMH+4,X'20'                                                 
* E-MAIL                                                                        
         ICM   R0,3,ARECL                                                       
         CHI   R0,ADRREC2Q         NEWEST RECORD LENGTH?                        
         BNE   DR60                IF NOT, DON'T DISPLAY E-MAIL                 
         MVC   ADMMAIL,ADREMAIL                                                 
         OI    ADMMAILH+6,X'80'                                                 
         OI    ADMMAILH+4,X'20'                                                 
* SAP INTERFACE CODE                                                            
DR60     OI    ADMSAPH+6,X'80'     SET TO XMT                                   
         XC    ADMSAP,ADMSAP       AND CLEAR FIELD                              
         ICM   R0,3,ARECL                                                       
         CHI   R0,ADRREC3Q         NEWEST RECORD LENGTH?                        
         BL    XDR                 IF NOT, DON'T DISPLAY SAP                    
         CLI   SAPAGY,C'Y'         TEST SAP AGY                                 
         BNE   XDR                                                              
         MVC   ADMSAP,ADRSAP                                                    
*                                                                               
XDR      B     EXIT                                                             
         DROP  R4                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== DELETE RECORD ===========================*         
DELR     DS    0H                                                               
*                                                                               
         GOTO1 CNRECS,DMCB,C'D'                                                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*=========================== RESTORE RECORD ==========================*         
RSTR     DS    0H                                                               
*                                                                               
         GOTO1 CNRECS,DMCB,C'R'                                                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                               INITIALIZE                            *         
*  GETFACT CALL                                                       *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
INIT     NTR1                                                                   
**NOP    TM    RECFLAG,INITLZED    ALWAYS GET PERSON                            
**NOP    BO    INITX                                                            
         XC    SVMYPSWD,SVMYPSWD                                                
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SVSECAGY,FATAGYSC   SAVE SECURITY AGENCY                         
         TM    FATFLAG,X'08'                                                    
         BZ    *+10                                                             
         MVC   SVMYPSWD,FAPASSWD                                                
         DROP  R3                                                               
***      OI    RECFLAG,INITLZED                                                 
INITX    B     EXIT                                                             
         EJECT                                                                  
         EJECT                                                                  
*=============================== REQREC ==============================*         
REQREC   NTR1                                                                   
*                                                                               
         L     R3,AIO2                                                          
         XC    0(110,R3),0(R3)         GENERATE REQUEST RECORD                  
         MVI   10(R3),45                                                        
         MVI   14(R3),106                                                       
         MVC   26(80,R3),SPACES                                                 
         MVC   26(2,R3),=C'45'                                                  
         MVC   28(2,R3),AGENCY                                                  
         MVC   30(1,R3),QMED                                                    
         MVC   31(3,R3),=C'ALL'                                                 
         CLC   KEY+9(3),=C'000'                                                 
         BE    *+10                                                             
         MVC   31(3,R3),KEY+9                                                   
         MVI   36(R3),ADDKTYPQ                                                  
         MVC   44(5,R3),QSTANEW        CALL LETTERS.                            
         MVC   94(7,R3),=C'CONTROL'                                             
         MVI   93(R3),C'A'                                                      
         CLI   ACTNUM,ACTADD                                                    
         BE    *+8                                                              
         MVI   93(R3),C'C'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD   ',=C'REQUEST ',(R3),(R3)                 
         TM    8(R1),X'50'                                                      
         BZ    EXIT                                                             
*                                                                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*============================== MESSAGES =============================*         
MISSERR  MVI   ERROR,MISSING       TELL USER OF MISSING INPUT.                  
         GOTO1 ERREX                                                            
*                                                                               
INVLERR  MVI   ERROR,INVALID       TELL USER OF INVALID INPUT.                  
         GOTO1 ERREX                                                            
*                                                                               
ERREXGO  GOTO1 ERREX                                                            
*                                                                               
RCERR1   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'RCERR1MS),RCERR1MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
RCERR1MS DC    C'PLEASE ENTER "U" FOR U.S. OR "C" FOR CANADA'                   
*                                                                               
RCERR2   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'RCERR2MS),RCERR2MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
RCERR2MS DC    C'** ERROR ** U.S. STATE MUST HAVE 2 LETTERS'                    
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
RCERR4MS DC    C'** ERROR ** NEED 5 DIGITS FOR U.S. ZIP CODE'                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== MISCELLANEOUS ===========================*         
         DS    0F                                                               
ZEROES   DC    (L'KEY)CL1'0'                                                    
BLANKS   DC    CL132' '                                                         
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'00'                    
NOTAUTH  EQU   0175                ERR-NOT AUTHORIZED FOR THIS FUNCTION         
SAVEDATE DS    XL3                                                              
RELO     DS    F                   RELOCATION FACTOR                            
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*====================== ADDRESS RECORDS DSECT =======================*          
ADRRECD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
         EJECT                                                                  
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
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMB2D          TWA FOR RECORD MAINTENANCE                   
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
*******************************************************************             
         ORG   SYSSPARE                                                         
**************************** WARNING ******************************             
* IF YOU WANT TO ADD ANYTHING TO SYSSPARE ADD IT IN SPSFMADR      *             
**************************** WARNING ******************************             
       ++INCLUDE SPSFMADR                                                       
         EJECT                                                                  
       ++INCLUDE SPSTABLK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031SPSFM15   11/06/18'                                      
         END                                                                    
