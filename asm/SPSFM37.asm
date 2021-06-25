*          DATA SET SPSFM37    AT LEVEL 090 AS OF 08/07/14                      
*PHASE T21737A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: SPSFM37<==>T21737 SPOT REP RECORDS MAINTENANCE.             *         
*                                                                     *         
*  COMMENTS: USING SFM TO HANDLE SPOT STATION-FILES.                  *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (T21700), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREEN SPSFMD7 (T217D7) -- MAINTENANCE                     *         
*                                                                     *         
*  OUTPUTS: UPDATED REP RECORDS                                       *         
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
         TITLE 'SPSFM37<==>T21737 SPOT REP RECORDS MAINTENANCE'                 
***********************************************************************         
T21737   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21737*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
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
         LA    R2,CONACTH                                                       
         B     ERREXGO                                                          
*                                                                               
MAIN10   MVI   ACTELOPT,C'N'                                                    
*                                                                               
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
         CLI   MODE,XRECADD        ADD PASSIVE RECORD                           
         BE    XR                                                               
         CLI   MODE,XRECPUT        CHANGE PASSIVE RECORD                        
         BE    XR                                                               
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
         MVC   SYSDIR,=C'STATION '      SET FILENAME.                           
         MVC   SYSFIL,=C'STATION '                                              
         SR    R1,R1                                                            
         LA    R1,REPKEYLQ                                                      
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
         LA    R2,RPMMEDIH         CHECK FOR ANY MEDIA INPUT.                   
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         CLI   SVAPROF+7,C'C'      FOR CANADIAN AGENCIES, DISALLOW              
         BNE   VK10                 MEDIAS 'C' & 'N'.                           
         MVI   ERROR,INVMED                                                     
         CLI   QMED,C'C'                                                        
         BE    ERREXGO                                                          
         CLI   QMED,C'N'                                                        
         BE    ERREXGO                                                          
*                                                                               
*---------------------------- REP FIELD ------------------------------*         
*                                                                               
** ALLOW REP-CODE TO BE ANY 3 CHARS BUT  'ALL'                                  
*                                                                               
VK10     LA    R2,RPMREPH           CHECK FOR ANY REP-CODE INPUT.               
         GOTO1 ANY                                                              
         CLI   5(R2),3                                                          
         BNE   KYERR1                                                           
         CLC   =C'ALL',8(R2)                                                    
         BE    KYERR2                                                           
         MVC   REP,8(R2)                                                        
*                                                                               
         CLC   SVCTAGY,=C'  '      DOING CODE COORDINATION ?                    
         BNH   VK20                NO                                           
* --> TEMPORARY CODE TO ENABLE REP CHECKING FOR NETWORK ONLY <--                
         CLI   QMED,C'N'           TEST NETWORK                                 
         BNE   VK20                                                             
         BAS   RE,CHKCTREP                                                      
*---------------------------- BUILD KEY ------------------------------*         
*                                                                               
VK20     MVC   KEY,ZEROES                                                       
         LA    R4,KEY                                                           
         USING REPRECD,R4                                                       
         MVI   REPKTYPE,C'R'       ADDRESS RECORDS ARE TYPE-'R'.                
         MVC   REPKMED,QMED        MEDIA.                                       
         MVC   REPKREP,REP         REP CODE.                                    
         MVC   REPKAGY,AGENCY      AGENCY.                                      
         DROP  R4                                                               
*                                                                               
XVK      B     SF                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== VALIDATE RECORD ROUTINE ======================*         
*                                                                               
VR       DS    0H                                                               
         L     R4,AIO              MAKE SURE WE HAVE RIGHT RECORD               
         USING REPRECD,R4                                                       
         CLI   0(R4),C'B'          REP PASSIVE RECORD?                          
         BNE   VR05                                                             
         MVC   KEY,ZEROES          GET ACTIVE REORD                             
         MVI   KEY,C'R'            ADDRESS RECORDS ARE TYPE-'R'.                
         MVC   KEY+1(1),REBKMED    MEDIA.                                       
         MVC   KEY+2(3),REBKREP    REP CODE.                                    
         MVC   KEY+5(2),REBKAGY    AGENCY.                                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'REPKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR05     XC    OLDRNAME,OLDRNAME        SAVE OLD REP NAME                       
         L     R4,AIO                                                           
         USING REPRECD,R4                                                       
         MVC   OLDRNAME,RNAME                                                   
         DROP  R4                                                               
*                                                                               
*------------------------- VALIDATE REP NAME -------------------------*         
*                                                                               
VR10     LA    R2,RPMNAMEH                                                      
         GOTO1 ANY                                                              
         MVC   NAME,WORK                                                        
*                                                                               
*---------------------- VALIDATE STREET ADDRESS ----------------------*         
*                                                                               
         LA    R2,RPMADDRH                                                      
         GOTO1 ANY                                                              
         MVC   STREET,WORK                                                      
*                                                                               
*--------------------------- VALIDATE CITY ---------------------------*         
*                                                                               
         LA    R2,RPMCITYH                                                      
         GOTO1 ANY                                                              
         MVC   CITY,WORK                                                        
*                                                                               
*-------------------------- VALIDATE COUNTRY -------------------------*         
*                                                                               
         LA    R2,RPMCTRYH                                                      
         GOTO1 ANY                                                              
         MVC   STATE,RPMSTTE       ASSUME CANADA STATE.                         
         CLI   RPMCTRY,C'C'        IS IT CANADA?  IF YES, THEN NO NEED          
         BE    VR95                 TO VALIDATE STATE AND ZIP.                  
         CLI   RPMCTRY,C'U'        IS IT U.S.?                                  
         BNE   RCERR1                                                           
*                                                                               
*--------------------- VALIDATE STATE AND ZIP CODE -------------------*         
*                                                                               
** U.S.                                                                         
*                                                                               
         LA    R2,RPMSTTEH                                                      
         GOTO1 ANY                                                              
         CLI   5(R2),2             2-LETTER STATE.                              
         BNE   RCERR2                                                           
         MVI   ERROR,NOTALPHA                                                   
         TM    4(R2),X'04'         TEST ALPHABETIC.                             
         BZ    ERREXGO                                                          
         MVC   STATE,WORK                                                       
*                                                                               
         LA    R2,RPMBZIPH                                                      
         GOTO1 ANY                                                              
VR95     XC    ZIP,ZIP             ALLOW ANYTHING FOR ZIP CODE.                 
         ZIC   R1,RPMBZIPH+5                                                    
         BCTR  R1,0                                                             
         EXMVC R1,ZIP,RPMBZIP                                                   
*                                                                               
*--------------------------- UNWIRED NETWORK -------------------------*         
*                                                                               
         LA    R2,RPMNTWKH                                                      
         MVI   UNETWORK,C'N'                                                    
         CLI   8(R2),C'Y'                                                       
         BNE   VR97                                                             
         MVI   UNETWORK,C'Y'                                                    
*                                                                               
*-------------------------------OPTIONS-------------------------------*         
*                                                                               
VR97     LA    R2,RPMOPTNH                                                      
         XC    OPTIONS,OPTIONS                                                  
         MVI   ERROR,INVALID                                                    
         GOTO1 SCANNER,DMCB,(R2),BLOCK   SCAN FOR OPTIONS                       
*                                                                               
         LA    R5,BLOCK                                                         
         CLI   0(R5),0                   NO OPTIONS ENTERED?                    
         BE    VR100                                                            
         CLC   =C'TRADE',12(R5)          ONLY ONE OPTION SO FAR                 
         BNE   ERREXGO                                                          
         OI    OPTIONS,RPOPT1_TRADE      VALID TRADE PAYEE                      
*                                                                               
*---------------------------- BUILD RECORD ---------------------------*         
*                                                                               
VR100    L     R4,AIO                                                           
         XCEF  (R4),2000                                                        
         USING REPRECD,R4                                                       
         MVC   REPKEY(REPKEYLQ),KEY      KEY OF RECORD.                         
         MVC   REPRLEN,=H'144'           LENGTH OF RECORD.                      
         MVC   RNAME,NAME                STATION NAME.                          
         MVC   R1LINE,STREET             STREET ADDRESS.                        
         MVC   R2LINE,CITY               CITY.                                  
         MVC   R3LINE,STATE                                                     
         MVC   RBIGZIP,ZIP                                                      
*                                                                               
VR110    CLI   RPMCTRY,C'C'                                                     
         BNE   *+10                                                             
         MVC   RZIP,=C'CANAD'            COUNTRY IS CANADA.                     
*                                                                               
         MVC   RUNWNET,UNETWORK                                                 
*                                                                               
         MVC   RPOPT1,OPTIONS                                                   
*                                                                               
         CLI   SAPAGY,C'Y'                                                      
         BNE   VR125                                                            
         LA    R2,RPMSAPH                                                       
         GOTO1 ANY                                                              
         MVC   RSAPCODE,SPACES                                                  
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RSAPCODE(0),8(R2)                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
VR125    CLI   ACTNUM,ACTADD                                                    
         BNE   VR130                                                            
         GOTO1 CNRECS,DMCB,C'A'                                                 
         B     VR140                                                            
VR130    GOTO1 CNRECS,DMCB,C'C'                                                 
*                                                                               
VR140    BAS   RE,REQREC                                                        
*                                                                               
XVR      B     DR                                                               
*VR      B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== DISPLAY KEY ROUTINE ========================*         
DK       CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    DK5                                                              
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    DK5                                                              
         MVI   ERROR,NOTAUTH       NOT AUTHORIZED FOR THIS FUNCTION             
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE?                           
         BE    NOTAUTHD                                                         
*                                                                               
DK5      L     R4,AIO                                                           
         USING REPRECD,R4                                                       
         CLI   0(R4),C'B'          REP PASSIVE RECORD?                          
         BNE   DK20                                                             
         MVC   KEY,ZEROES          GET ACTIVE REORD                             
         MVI   KEY,C'R'            ADDRESS RECORDS ARE TYPE-'R'.                
         MVC   KEY+1(1),REBKMED    MEDIA.                                       
         MVC   KEY+2(3),REBKREP    REP CODE.                                    
         MVC   KEY+5(2),REBKAGY    AGENCY.                                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'REPKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*------------------------------- MEDIA -------------------------------*         
*                                                                               
DK20     MVC   RPMMEDI,REPKMED                                                  
         OI    RPMMEDIH+6,X'80'                                                 
*                                                                               
*------------------------------ REP CODE -----------------------------*         
*                                                                               
         MVC   RPMREP,REPKREP                                                   
         OI    RPMREPH+6,X'80'                                                  
         MVC   REP,REPKREP              COPY TO SAVED REP CODE FOR XREC         
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
         TWAXC RPMNAMEH                                                         
*                                                                               
*----------------------- FILL UP DATA FIELDS ------------------------*          
*                                                                               
         L     R4,AIO                                                           
         USING REPRECD,R4                                                       
         MVC   RPMNAME,RNAME       REP NAME.                                    
         MVC   RPMADDR,R1LINE      STREET ADDRESS.                              
         MVC   RPMCITY,R2LINE      CITY.                                        
         MVC   RPMSTTE,R3LINE      STATE.                                       
         MVC   RPMBZIP,RBIGZIP     ASSUME IT'S CANADA.                          
         MVI   RPMCTRY,C'U'        ASSUME IT'S U.S.                             
         CLC   =C'CANAD',RZIP                                                   
         BNE   DR10                                                             
         MVI   RPMCTRY,C'C'                                                     
*                                                                               
DR10     OI    RPMSAPH+6,X'80'      TRANSMIT SAP FIELD                          
         XC    RPMSAP,RPMSAP                                                    
         CLI   SAPAGY,C'Y'                                                      
         BNE   DR20                                                             
         MVC   RPMSAP,RSAPCODE                                                  
*                                                                               
DR20     MVI   RPMNTWK,C'N'                                                     
         CLI   RUNWNET,C'Y'                                                     
         BNE   DR30                                                             
         MVI   RPMNTWK,C'Y'                                                     
*                                                                               
DR30     TM    RPOPT1,RPOPT1_TRADE                                              
         BNO   XDR                                                              
         MVC   RPMOPTN(5),=C'TRADE'                                             
         DROP  R4                                                               
*                                                                               
XDR      B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= XRECADD/PUT    ROUTINE ======================*         
*                                                                               
XR       DS    0H                                                               
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'            READ FOR DELETED RECS                   
         CLI   ACTNUM,ACTADD            ADD?                                    
         BE    XR10                     YES, DON'T DELETE ANY P-RECS            
         CLC   OLDRNAME,NAME            OLD = NEW REP NAME?                     
         BE    XR10                     YES,DON'T DELETE OLD P-REC              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REPREC,R4                                                        
         MVC   REBKTYPE,=C'B'                                                   
         MVC   REBKAGY,AGENCY                                                   
         MVC   REBKNAME,OLDRNAME        LOOK FOR OLD REP P-REC                  
         MVC   REBKMED,QMED                                                     
         MVC   REBKREP,REP                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'REPKEY),KEYSAVE                                            
         BNE   XR10                     REC NOT FOUND, ADD NEW                  
*                                                                               
         L     R4,AIO                                                           
         OI    17(R4),X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                                                            
*                                                                               
XR10     DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   REBKTYPE,=C'B'                                                   
         MVC   REBKAGY,AGENCY                                                   
         MVC   REBKNAME,NAME            LOOK FOR NEW REP P-REC                  
         MVC   REBKMED,QMED                                                     
         MVC   REBKREP,REP                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'REPKEY),KEYSAVE                                            
         BNE   XR20                     REC NOT FOUND, ADD NEW REP              
*                                                                               
         L     R4,AIO                                                           
         NI    17(R4),X'FF'-X'80'       UNDELETE PREV REP P-REC                 
         MVC   REBNAME,NAME               NAME AFTER 8 BYTES CAN CHANGE         
         GOTO1 WRITE                                                            
         B     XXR                                                              
*                                                                               
XR20     DS    0H                                                               
         L     R4,AIO                                                           
         XC    0(REBREQLQ,R4),0(R4)                                             
         MVC   0(L'REPKEY,R4),KEYSAVE                                           
         XC    REBRLEN,REBRLEN                                                  
         MVI   REBRLEN+1,REBREQLQ                                               
         MVC   REBNAME,NAME                                                     
         GOTO1 ADD                                                              
         DROP  R4                                                               
*                                                                               
XXR      MVI   RDUPDATE,C'N'                                                    
         NI    DMINBTS,X'FF'-X'08'                                              
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== DELETE RECORD ===========================*         
DELR     DS    0H                                                               
*                                                                               
         GOTO1 CNRECS,DMCB,C'D'                                                 
*                                                                               
         B     EXIT                                                             
***********************************************************************         
         SPACE 4                                                                
***********************************************************************         
*=========================== RESTORE RECORD ==========================*         
RSTR     DS    0H                                                               
*                                                                               
         GOTO1 CNRECS,DMCB,C'R'                                                 
*                                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
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
         MVI   36(R3),C'R'                                                      
         MVC   44(5,R3),KEY+2          REP CODE.                                
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
*===================================================================            
* FOR CLIENTS DOING CODE CO-ORDINATION, NEED TO SEE IF CODE HAS                 
* BEEN ADDED TO CTFILE                                                          
*===================================================================            
         SPACE 1                                                                
CHKCTREP NTR1                                                                   
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   CHKX                NO - FORGET IT                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ZENRECD,R4                                                       
         MVI   ZENKCODE,ZENKCODQ                                                
         MVI   ZENKTYP,ZENREPQ                                                  
         MVI   ZENKSYS,C'S'        SPOT                                         
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   ZENKSYS,C'N'                                                     
         MVC   ZENKMED,QMED                                                     
         MVC   ZENKAGY,SVCTAGY                                                  
         MVC   ZENKREP,REP                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO                       
*                                                                               
         L     R4,AIO                                                           
         USING ZENRECD,R4                                                       
*                                                                               
         CLC   KEY(25),0(R4)                                                    
         BNE   CHKCTERR                                                         
*                                                                               
         LA    R4,ZENFIRST                                                      
         USING ZENELEM,R4                                                       
*                                                                               
         LA    R2,RPMNAMEH                                                      
         MVC   8(22,R2),ZENREPNM                                                
         OI    6(R2),X'80'                                                      
         OI    1(R2),X'01'         SET MODIFIED FLAG                            
*                                                                               
         LA    R2,RPMADDRH                                                      
         OI    6(R2),X'80'                                                      
         MVC   8(24,R2),ZENREPAD                                                
*                                                                               
         LA    R2,RPMCITYH                                                      
         OI    6(R2),X'80'                                                      
         MVC   8(24,R2),ZENRADR2                                                
*                                                                               
         LA    R2,RPMSTTEH                                                      
         OI    6(R2),X'80'                                                      
         MVC   8(3,R2),ZENRADR3                                                 
*                                                                               
         LA    R2,RPMBZIPH                                                      
         XC    8(10,R2),8(R2)                                                   
         OI    6(R2),X'80'                                                      
         MVC   8(5,R2),ZENRADR4                                                 
*                                                                               
CHKX     XIT1                                                                   
         DROP R4                                                                
*                                                                               
CHKCTERR MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'NOCTCODE),NOCTCODE                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
NOCTCODE DC    C'REP CODE HAS NOT BEEN SET UP ON CONTROL FILE'                  
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================== MESSAGES =============================*         
MISSERR  MVI   ERROR,MISSING       TELL USER OF MISSING INPUT.                  
         GOTO1 ERREX                                                            
*                                                                               
ERREXGO  GOTO1 ERREX                                                            
*                                                                               
KYERR1   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'KYERR1MS),KYERR1MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
KYERR1MS DC    C'REP-CODE IS 3 CHARACTERS LONG ONLY'                            
*                                                                               
KYERR2   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'KYERR2MS),KYERR2MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
KYERR2MS DC    C'REP-CODE CAN NOT BE "ALL"'                                     
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
NOTAUTH  EQU   0175                ERR-NOT AUTHORIZED FOR THIS FUNCTION         
***********************************************************************         
         EJECT                                                                  
**********************************************************************          
*=========================== LITERAL POOL ===========================*          
         LTORG                                                                  
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*======================== REP RECORDS DSECT =========================*          
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
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
       ++INCLUDE SPSFMD7D          TWA FOR RECORD MAINTENANCE.                  
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
MYFILNME DS    CL(L'FILENAME)                                                   
*                                                                               
** REP-RECORD DATA **                                                           
REP      DS    CL3                                                              
NAME     DS    CL22                                                             
STREET   DS    CL24                                                             
CITY     DS    CL24                                                             
STATE    DS    CL3                                                              
ZIP      DS    CL10                                                             
COUNTRY  DS    CL1                                                              
UNETWORK DS    CL1                                                              
OPTIONS  DS    X                                                                
*                                                                               
RELO     DS    F                   RELOCATION FACTOR.                           
OLDRNAME DS    CL22                                                             
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*============================= STABLOCK =============================*          
*                                                                               
       ++INCLUDE SPSTABLK                                                       
**********************************************************************          
         PRINT OFF                                                              
       ++INCLUDE CTGENZEN                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'090SPSFM37   08/07/14'                                      
         END                                                                    
