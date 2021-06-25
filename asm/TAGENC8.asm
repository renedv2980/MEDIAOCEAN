*          DATA SET TAGENC8    AT LEVEL 085 AS OF 07/20/12                      
*PHASE T702C8C,*                                                                
         TITLE 'T702C8 - EPISODE LIST'                                          
T702C8   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702C8                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     MAINX                                                            
         SPACE 1                                                                
         CLI   MODE,LVALKEY        VALIDATE KEY OF LISTED RECORD                
         BNE   *+12                                                             
         BAS   RE,LVKEY                                                         
         B     MAINX                                                            
         SPACE 1                                                                
         CLI   MODE,LVALREC        VALIDATE LISTED RECORD                       
         BNE   *+12                                                             
         BAS   RE,LVREC                                                         
         B     MAINX                                                            
         SPACE 1                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   MAIN30                                                           
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R0,LRHOOK                                                        
         B     MAIN40                                                           
         SPACE 1                                                                
MAIN30   CLI   MODE,PRINTREP                                                    
         BNE   MAINX                                                            
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R0,PRHOOK                                                        
         SPACE 1                                                                
MAIN40   BAS   RE,LREC             GO LIST THE RECORDS                          
         SPACE 1                                                                
MAINX    B     XIT                                                              
         SPACE 3                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         MVI   NLISTS,16           SET N'LIST LINES                             
         MVC   LLIST,=Y(LLNQ)      SET L'DATA LINE                              
         OI    GLSTSTAT,APPLCDSP+RETEXTRA+CHNGLIST+OKTOADD                      
*                                                                               
         LA    R2,SEPAGYH          AGENCY                                       
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BNE   VK5                                                              
         TM    4(R2),X'20'         TEST PREVIOUSLY VALIDATED                    
         BO    VK10                                                             
VK5      NI    SEPEPIH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',(R2))                                 
         MVC   TIFAGY,TGAGY                                                     
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
VK10     LA    R2,SEPEPIH          EPISODE NUMBER                               
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         XC    TIQSTART,TIQSTART                                                
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BNO   FLDINV                                                           
         LA    R3,TGEPI                                                         
         MVC   0(5,R3),=5C'0'                                                   
         ZIC   RE,5(R2)                                                         
         LA    R1,L'TGEPI                                                       
         SR    R1,RE                                                            
         AR    R3,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       SET TGEPI                                    
         MVC   TIQSTART,TGEPI      START AT PARTICULAR EPI NUMBER               
         XC    TIQSTART,COMPLM     COMPLEMENT EPISODE NUMBER                    
VK20     OI    4(R2),X'20'                                                      
*                                                                               
         XC    KEY,KEY             INITIALIZE KEY                               
         NI    STATUS,X'FF'-ADDPFK                                              
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LREC     NTR1                                                                   
         ST    R0,TIHOOK           HOOK TO SYSIO                                
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLEPCDQ                                                   
*                                  VALID,CLEAR,UNPROTECTED                      
         GOTO1 FLDVAL,DMCB,(X'27',SEPL1H),SEPLSTH                               
         CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BNE   LR20                                                             
         CLI   PFAID,14            AND USER PRESSED 'ADD' PFKEY                 
         BNE   LR20                NO -RETURN                                   
         MVC   TGEPI,COMPLM                                                     
         GOTO1 RECVAL,DMCB,TLEPCDQ,0                                            
         CLC   KEY(TLEPEPI-TLEPD),KEYSAVE TEST FOUND EPI FOR THIS AGY           
         BNE   LR10                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         BAS   RE,DISPLAY                                                       
         GOTO1 LISTMON                                                          
         AH    R2,=Y(LINNEXT)      PT TO NEXT AVAILABLE LINE                    
*                                                                               
LR10     OI    STATUS,ADDPFK       SET CORRES. STATUS BIT FOR NEXT TIME         
         XC    TIQSKEY,TIQSKEY     SIM END OF LIST - CLEAR CONTINUE KEY         
         B     PLSENTER                                                         
*                                                                               
LR20     TM    STATUS,ADDPFK       IF ADD PFKEY PRESSED LAST TIME               
         BZ    *+14                                                             
         XI    STATUS,ADDPFK       TURN IT OFF                                  
         XC    KEY,KEY             AND SET TO RESTART START LIST                
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         XC    TIQSKEY,TIQSKEY     END OF LIST - CLEAR CONTINUE KEY             
*                                                                               
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       AND IF ANYTHING REPORTED                     
         BE    LRX                                                              
         GOTO1 SPOOL,DMCB,(R8)     SKIP LINE BEFORE TOTAL                       
         EDIT  COUNTER,(4,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(15,R1),=C'EPISODE RECORDS'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         TWAXC SEPL1H,PROT=Y       CLEAR THE SCREEN FOR RETURN                  
         SPACE 1                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS                                           
         SPACE 1                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHX                                                             
*                                                                               
         CLC   LISTNUM,NLISTS      IF ALREADY DISPLAYED MAX                     
         BE    LRH60               GO BACK TO LISTMON                           
         MVC   AIO,TIAREC                                                       
         BAS   RE,DISPLAY          OTHERWISE GO DISPLAY IT                      
         MVC   AIO,AIO1                                                         
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
*                                                                               
LRH60    GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
LRHX     B     XIT                                                              
         SPACE 2                                                                
*               PROCESS SYSIO RECORDS                                           
         SPACE 1                                                                
PRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   PRHX                                                             
*                                                                               
         LA    R3,SEPEPH           SET A(CURRENT LINE TO 1ST LINE)              
         ST    R3,ATHISLST                                                      
         USING LINED,R3                                                         
*                                                                               
         MVC   AIO,TIAREC                                                       
         BAS   RE,DISPLAY                                                       
         MVC   AIO,AIO1                                                         
*                                                                               
         MVI   BYTE,C'P'           PRINT                                        
         GOTO1 PRTSCRN,DMCB,(R3),LINADT,P                                       
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
PRHX     B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
*              DISPLAY RECORD IN AIO                                            
*                                                                               
DISPLAY  NTR1                                                                   
         L     R3,ATHISLST                                                      
         USING LINED,R3            R3 = A(OUTPUT AREA)                          
         L     R4,AIO                                                           
         USING TLEPD,R4                                                         
         MVC   LINEEPI,TLEPEPI     EPISODE NUMBER                               
         XC    LINEEPI,COMPLM                                                   
         MVI   LINEEPIH+5,L'LINEEPI SET L'I/P FOR COPY DOWN                     
         OI    LINEEPIH+1,X'20'    PROTECT IT                                   
*                                                                               
         MVI   ELCODE,TAEIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISPX                                                            
         USING TAEID,R4                                                         
         OC    TAEIWDT,TAEIWDT     WORK DATE                                    
         BZ    DISP5                                                            
         GOTO1 DATCON,DMCB,(1,TAEIWDT),(5,LINWDT)                               
*                                                                               
DISP5    OC    TAEIADT,TAEIADT     AIR DATE                                     
         BZ    DISPX                                                            
         GOTO1 DATCON,DMCB,(1,TAEIADT),(5,LINADT)                               
*                                                                               
DISPX    B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE VALIDATES KEY OF LISTED RECORDS                          
*                                                                               
LVKEY    NTR1                                                                   
         L     R3,ATHISLST         R3=A(THIS SCREEN LINE)                       
         USING LINED,R3                                                         
*                                                                               
         LA    R2,LINEEPIH                                                      
         CLI   5(R2),1                                                          
         BNE   LVKEY10                                                          
         CLI   8(R2),C','                                                       
         BNE   FLDINV                                                           
         BAS   RE,CPYDWN           COPY EPISODE FROM PREVIOUS LINE              
         BNE   FLDINV                                                           
         PACK  DUB(8),8(L'TGEPI,R2)                                             
         AP    DUB,=P'1'           ADD ONE TO NUMBER                            
         OI    DUB+7,X'0F'                                                      
         UNPK  TGEPI(5),DUB                                                     
         B     LVKEY20                                                          
*                                                                               
LVKEY10  TM    4(R2),X'08'         TEST NUMERIC                                 
         BNO   FLDINV                                                           
         CLI   5(R2),5                                                          
         BNE   FLDINV                                                           
         LA    R3,TGEPI                                                         
         MVC   0(L'TGEPI,R3),=5C'0'                                             
         ZIC   RE,5(R2)                                                         
         LA    R1,L'TGEPI                                                       
         SR    R1,RE                                                            
         AR    R3,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
*                                                                               
LVKEY20  MVC   8(L'TGEPI,R2),TGEPI SET SCREEN AND TRANSMIT                      
*******  MVC   TIFEPI,TGEPI        SET SYSIO FILTER                             
         GOTO1 RECVAL,DMCB,TLEPCDQ,(X'40',0)                                    
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE VALIDATES LISTED RECORDS                                 
*                                                                               
LVREC    NTR1                                                                   
         L     R3,ATHISLST         R3=A(THIS SCREEN LINE)                       
         USING LINED,R3                                                         
         MVI   ELCODE,TAEIELQ      DELETE OLD EPISODE DATE ELEMENT              
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R4,ELEMENT          ADD NEW ELEMENT                              
         USING TAEID,R4                                                         
         MVI   TAEIEL,TAEIELQ                                                   
         MVI   TAEILEN,TAEILNQ                                                  
*                                                                               
         LA    R2,LINWDTH          VALIDATE WORK DATE                           
         CLI   5(R2),0                                                          
         BE    LVREC10                                                          
         GOTO1 DTVAL,DMCB,TAEIWDT                                               
         B     LVREC20                                                          
LVREC10  BAS   RE,CPYDWN           COPY DATE FROM PREVIOUS LINE                 
         BNE   FLDINV                                                           
         BAS   RE,GTNXTD           GET NEXT DAY INTO WORK                       
         GOTO1 DATCON,DMCB,(0,WORK),(1,TAEIWDT)                                 
         GOTO1 DATCON,DMCB,(1,TAEIWDT),(5,8(R2)) DISP NEW DATE TO SCRN          
*                                                                               
LVREC20  LA    R2,LINADTH          VALIDATE AIR DATE                            
         CLI   5(R2),0                                                          
         BE    LVREC30                                                          
         GOTO1 DTVAL,DMCB,TAEIADT                                               
         B     LVREC40                                                          
LVREC30  BAS   RE,CPYDWN           COPY DATE FROM PREVIOUS LINE                 
         BNE   FLDINV                                                           
         BAS   RE,GTNXTD           GET NEXT DAY INTO WORK                       
         GOTO1 DATCON,DMCB,(0,WORK),(1,TAEIADT)                                 
         GOTO1 DATCON,DMCB,(1,TAEIADT),(5,8(R2)) DISP NEW DATE TO SCRN          
*                                                                               
LVREC40  GOTO1 ADDELEM                                                          
         GOTO1 ACTVIN,DMCB,0       LAST CHANGED                                 
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              COPY DOWN FROM LINE ABOVE                                        
*                                                                               
CPYDWN   NTR1                                                                   
         LR    R1,R2               R2 -> CURRENT LINE                           
         SH    R1,=Y(LINNEXT)      R1 -> PREVIOUS LINE                          
         LA    RF,SEPL1H                                                        
         CR    R1,RF                                                            
         BNH   NO                  FIRST LINE - NO PLACE TO COPY FROM           
         SR    RE,RE                                                            
         ICM   RE,1,5(R1)                                                       
         BZ    NO                                                               
         STC   RE,5(R2)            SET DATA LENGTH                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R1)       COPY FROM PREVIOUS LINE                      
         OI    6(R2),X'80'                                                      
         B     YES                                                              
         SPACE 2                                                                
* GETS NEXT DAY (GETS NEXT BUSINESS DAY) - R2 PTS TO START DAY                  
*                                                                               
GTNXTD   NTR1                                                                   
         GOTO1 DTVAL,DMCB,WORK                                                  
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+6)                                  
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         ZIC   RE,0(R1)            DAY OF WEEK                                  
         CH    RE,=H'5'            GET NEXT BUSINESS DAY(SKIP WEEKENDS)         
         BL    GTNXTD5                                                          
         LA    R2,8                                                             
         SR    R2,RE                                                            
         GOTO1 ADDAY,DMCB,WORK+6,WORK,(R2)                                      
         B     GTNXTDX                                                          
*                                                                               
GTNXTD5  GOTO1 ADDAY,DMCB,WORK+6,WORK,F'1'                                      
*                                                                               
GTNXTDX  B     XIT                                                              
         SPACE 2                                                                
*              HOOK                                                             
*                                                                               
HOOK     NTR1                                                                   
         MVI   BYTE,C'H'                                                        
         GOTO1 PRTSCRN,DMCB,EFHTAG,SEPL1H,H5                                    
         MVI   BYTE,C'P'           PRINT LINES                                  
         B     XIT                                                              
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE                                                                  
COMPLM   DC    X'FFFFFFFFFFFF'                                                  
         SPACE                                                                  
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
*                                                                               
PLSENTER MVI   MYMSGNO1,2          PLEASE ENTER REQUIRED FIELDS                 
         MVI   MYMSYS,X'FF'                                                     
         B     INFEND                                                           
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'EPISODE LIST'                                            
         SSPEC H2,32,C'------------'                                            
         SPACE 1                                                                
         SSPEC H8,2,C'--- -------  ---------  --------'                         
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 2                                                                
LINED    DSECT                                                                  
LINEEPIH DS    CL8                                                              
LINEEPI  DS    CL5                 EPISODE NUMBER                               
         DS    CL8                                                              
LINWDTH  DS    CL8                                                              
LINWDT   DS    CL8                 WORK DATE                                    
         DS    CL8                                                              
LINADTH  DS    CL8                                                              
LINADT   DS    CL8                 AIR DATE                                     
         DS    CL8                                                              
LLNQ     EQU   *-LINED                                                          
*                                                                               
LSELH    DS    CL8                 A(SELECT FIELD ON NEXT LINE)                 
LSEL     DS    CL3                                                              
         DS    CL8                                                              
LINNEXT  EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRC8D                                                       
         SPACE 3                                                                
COUNTER  DS    PL4                 RECORD COUNTER                               
STATUS   DS    XL1                                                              
ADDPFK   EQU   X'80'               ADD PFKEY PRESSED                            
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085TAGENC8   07/20/12'                                      
         END                                                                    
