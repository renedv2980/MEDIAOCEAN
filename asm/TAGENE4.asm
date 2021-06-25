*          DATA SET TAGENE4    AT LEVEL 010 AS OF 02/05/16                      
*PHASE T702E4C                                                                  
         TITLE 'T702E4 - JOB LIST'                                              
T702E4   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702E4                                                         
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
         CLI   MODE,VALKEY         IF MODE VALKEY                               
         BNE   *+12                                                             
         BAS   RE,VKEY             VALIDATE KEY                                 
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,LISTRECS       IF MODE IS LIST RECORDS                      
         BNE   JB30                                                             
         MVI   NLISTS,15           SET N'LIST LINES                             
         OI    GLSTSTAT,RETEXTRA   SET OKAY TO RETURN EXTRA FOR EOL             
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR           R2=A(DISPLAY LINE)                           
         B     JB40                                                             
         SPACE 1                                                                
JB30     CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS          SET A(SPECS)                                 
         ST    R2,SPECS                                                         
         LA    R2,HDHOOK           SET A(HEADLINE HOOK)                         
         ST    R2,HEADHOOK                                                      
         LA    R2,P+1              R2=A(PRINT LINE)                             
         SPACE 1                                                                
JB40     BAS   RE,LREC             GO LIST THE RECORDS                          
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,SJBAGYH          R2=A(AGENCY)                                 
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BNE   *+12                                                             
         TM    4(R2),X'20'         OR NOT PREVIOUSLY VALIDATED                  
         BO    VK20                                                             
         NI    SJBDTEH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SJBAGYNH                        
         MVC   TIFAGY,TGAGY        SET SYSIO FILTER                             
*                                                                               
         NI    SJBDTEH+1,X'DF'                                                  
         NI    SJBCLHDH+1,X'F3'    SHOW JOB FIELD AND PROTECT IT                
         NI    SJBCLIH+1,X'DF'                                                  
         NI    SJBPRHDH+1,X'F3'    SHOW JOB FIELD AND PROTECT IT                
         NI    SJBPRDH+1,X'DF'                                                  
         OI    SJBJBHDH+1,X'0C'    HIDE JOB FIELD AND PROTECT IT                
         OI    SJBJOBH+1,X'2C'                                                  
*                                                                               
         MVC   SJBHD1,=CL40'Sel Date     Client Product Jobs'                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL            GET AGENCY ELEMENT                           
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   TGJWBUNT,TAAYBUNT                                                
*                                                                               
         OC    TGJWBUNT,TGJWBUNT   J WALTER?                                    
         BZ    VK05                                                             
         OI    SJBDTEH+1,X'20'                                                  
         NI    SJBCLHDH+1,X'F3'    SHOW JOB FIELD AND PROTECT IT                
         OI    SJBCLIH+1,X'20'                                                  
         NI    SJBPRHDH+1,X'F3'    SHOW JOB FIELD AND PROTECT IT                
         OI    SJBPRDH+1,X'20'                                                  
         NI    SJBJBHDH+1,X'F3'    SHOW JOB FIELD AND PROTECT IT                
         NI    SJBJOBH+1,X'D3'                                                  
*                                                                               
         MVC   SJBHD1,=CL40'Sel Job     Date     BU    Cli    Prd'              
*                                                                               
VK05     TM    TGAYSTA7,TAAYSBBD   BBDO JOBS NOT FROM ACCPAK                    
         BZ    VK10                                                             
         OI    SJBDTEH+1,X'20'                                                  
         OI    SJBCLHDH+1,X'0C'    HIDE CLI FIELD AND PROTECT IT                
         OI    SJBCLIH+1,X'2C'                                                  
         OI    SJBPRHDH+1,X'0C'    HIDE PRD FIELD AND PROTECT IT                
         OI    SJBPRDH+1,X'2C'                                                  
         OI    SJBJBHDH+1,X'0C'    HIDE JOB FIELD AND PROTECT IT                
         OI    SJBJOBH+1,X'2C'                                                  
*        NI    SJBJBHDH+1,X'F3'    SHOW JOB FIELD AND PROTECT IT                
*        NI    SJBJOBH+1,X'D3'                                                  
*                                                                               
         MVC   SJBHD1,=CL40'Sel Job               Date'                         
VK10     OI    SJBHD1H+6,X'80'                                                  
         OI    SJBDTEH+6,X'80'                                                  
         OI    SJBCLHDH+6,X'80'                                                 
         OI    SJBCLIH+6,X'80'                                                  
         OI    SJBPRHDH+6,X'80'                                                 
         OI    SJBPRDH+6,X'80'                                                  
         OI    SJBJBHDH+6,X'80'                                                 
         OI    SJBJOBH+6,X'80'                                                  
*                                                                               
VK20     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SJBDTEH          R2=A(DATE)                                   
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK30                                                             
         NI    SJBCLIH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIQPSTR,TIQPSTR                                                  
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 DTVAL,DMCB,TIQPSTR                                               
         XC    TIQPSTR(3),HEXFFS                                                
VK30     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         SPACE 1                                                                
         LA    R2,SJBCLIH          R2=A(CLIENT)                                 
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    VK40                                                             
         NI    SJBPRDH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIFCLI,TIFCLI                                                    
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'04',(R2))                                 
         MVC   TIFCLI,TGCLI                                                     
*                                                                               
VK40     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SJBPRDH          R2=A(PRODUCT)                                
         TM    4(R2),X'20'                                                      
         BO    VK50                                                             
         XC    TIFPRD,TIFPRD                                                    
         CLI   5(R2),0                                                          
         BE    VK50                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'04',(R2))                                 
         MVC   TIFPRD,TGPRD                                                     
*                                                                               
VK50     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SJBJOBH          R2=A(PRODUCT)                                
         TM    4(R2),X'20'                                                      
         BO    VK60                                                             
         XC    TIFEST,TIFEST                                                    
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         MVC   TIFEST(L'SJBJOB),8(R2)                                           
*                                                                               
VK60     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
VK90     DS    0H                                                               
         XC    KEY,KEY             RE-INITIALIZE LIST                           
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
*                                                                               
LREC     NTR1                                                                   
*                                                                               
         NI    TIQFLAG3,X'FF'-TIQFJWTJ                                          
         NI    TIQFLAG4,X'FF'-TIQFBBDJ                                          
         OC    TGJWBUNT,TGJWBUNT   IS AGENCY A JWT AGENCY?                      
         BZ    LR200                                                            
         OI    TIQFLAG3,TIQFJWTJ   READ JWT JOBS                                
*                                                                               
LR200    TM    TGAYSTA7,TAAYSBBD   IS AGENCY A BBDO JOB AGENCY?                 
         BZ    LR300                                                            
         OI    TIQFLAG4,TIQFBBDJ   READ BBDO JOBS                               
*                                                                               
LR300    LA    R0,IOHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
*                                                                               
         MVI   TIREAD,TLJBCDQ      SET TO READ JOB RECORDS                      
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         XC    TIQSKEY,TIQSKEY     END OF LIST - CLEAR CONTINUE KEY             
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         BAS   RE,PRNTIT                                                        
         EDIT  COUNTER,(3,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(11,R1),=C'JOB RECORDS'                                         
         BAS   RE,PRNTIT                                                        
         B     LR900                                                            
*                                                                               
LR500    DS    0H                                                               
*                                                                               
LR900    TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         TWAXC SJBSELH,PROT=Y                                                   
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS                                           
         SPACE 1                                                                
         USING LINED,R2            R2=A(OUTPUT AREA)                            
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   IOHX                                                             
         L     R6,TIAREC           R6=A(RECORD)                                 
         USING TLJBD,R6                                                         
*                                                                               
         CLI   TLJBSEQ,0           IF BASE RECORD                               
         BNE   IOHX                                                             
         BAS   RE,DISPLAY          DISPLAY RECORD                               
*                                                                               
         CLI   MODE,PRINTREP       IF DISPLAYING TO SCREEN                      
         BE    IOH10                                                            
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             DISPLAY LINE TO SCREEN                       
         B     IOHX                                                             
*                                                                               
IOH10    BAS   RE,PRNTIT           ELSE, PRINT ON REPORT                        
         AP    COUNTER,=P'1'       AND ADD TO COUNTER                           
*                                                                               
IOHX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY ONE LINE OF RECORD INFO                       
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         XC    LINED(LINLNQ),LINED                                              
*                                                                               
         OC    TGJWBUNT,TGJWBUNT   IS AGENCY A JWT AGENCY?                      
         BNZ   DISP300                                                          
         TM    TGAYSTA7,TAAYSBBD   IS AGENCY A BBDO JOB AGENCY?                 
         BO    DISP400                                                          
*                                                                               
         MVC   WORK(3),TLJBDTE     DATE                                         
         XC    WORK(3),HEXFFS      UNCOMPLEMENT                                 
         MVC   TGDATE,WORK                                                      
         GOTO1 DATCON,DMCB,(1,WORK),(8,LINDTE)                                  
*                                                                               
         MVC   LINCLI,TICLI        CLIENT                                       
         MVC   TGCLI,TICLI                                                      
         MVC   LINPRD,TIPRD        PRODUCT                                      
         MVC   TGPRD,TIPRD                                                      
*                                                                               
         LA    R3,LINJOBS                                                       
*                                                                               
         L     R3,AIO2                                                          
         XR    R0,R0                                                            
*                                                                               
         LR    R4,R6                                                            
         MVI   ELCODE,TAGLELQ      GET JOBS                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISP5    BAS   RE,NEXTEL                                                        
         BNE   DISP10                                                           
         USING TAGLD,R4                                                         
         MVC   0(20,R3),SPACES     PRE-CLEAR ENTRY                              
         ZIC   R1,TAGLLEN          SET FOR EXECUTED MOVE                        
         SH    R1,=AL2(TAGLLNQ+1)                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TAGLDATA    MOVE DATA TO BLOCK                           
*                                                                               
         LA    R3,20(R3)           BUMP TO NEXT ENTRY IN BLOCK                  
         AHI   R0,1                BUMP COUNT                                   
         CHI   R0,90               CAN'T FIT MORE THAN 90                       
         BH    DISP10                                                           
         B     DISP5                                                            
*                                                                               
DISP10   LTR   R0,R0               DISPLAY AS MANY JOBS AS WILL FIT             
         BZ    DISPX                                                            
         MVC   BLOCK(80),SPACES                                                 
         GOTO1 UNSCAN,DMCB,((R0),AIO2),(C'C',BLOCK),0,0                         
         MVC   LINJOBS,BLOCK                                                    
         CLI   DMCB,0              IF ANYTHING LEFT TO DISPLAY                  
         BE    DISPX                                                            
         MVI   LINCONT,C'*'        SHOW THEM THERE IS MORE                      
         B     DISPX                                                            
*                                                                               
DISP300  MVC   LINPRJI,TLJBPRJI                                                 
*                                                                               
         MVC   WORK(3),TLJBDATE    DATE                                         
         XC    WORK(3),HEXFFS      UNCOMPLEMENT                                 
         MVC   TGDATE,WORK                                                      
         GOTO1 DATCON,DMCB,(1,WORK),(8,LINDTADD)                                
*                                                                               
         MVC   LINBUNIT,TLJBBUNT                                                
         MVC   LINCSTI,TLJBCSTI                                                 
         MVC   LINPRDCT,TLJBPRDI                                                
         B     DISPX                                                            
*                                                                               
DISP400  MVC   LINPROJ,TLJBPROJ                                                 
*                                                                               
         MVC   WORK(3),TLJBDAT     DATE                                         
         XC    WORK(3),HEXFFS      UNCOMPLEMENT                                 
         MVC   TGDATE,WORK                                                      
         GOTO1 DATCON,DMCB,(1,WORK),(8,LINDAT)                                  
*                                                                               
DISPX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              HEADLINE HOOK                                                    
         SPACE 1                                                                
HDHOOK   NTR1                                                                   
         MVI   BYTE,C'H'           SET PRINTING HEADINGS                        
         GOTO1 PRTSCRN,DMCB,CONTAGH,SJBHD1H,H5                                  
         GOTO1 (RF),(R1),SJBHD1H,SJBSELH,H7-4                                   
         MVC   H7-4(5),SPACES      CLEAR SELECT FIELD                           
         MVI   BYTE,C'P'           RESET                                        
         B     XIT                                                              
         SPACE 2                                                                
*              EXITS, ETC.                                                      
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
HEXFFS   DC    3X'FF'                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SSPEC H1,33,C'JOB LIST'                                                
         SSPEC H2,33,C'--------'                                                
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 2                                                                
LINED    DSECT                                                                  
LINDTE   DS    CL8                 EFFECTIVE DATE                               
         DS    CL1                                                              
LINCLI   DS    CL6                 CLIENT                                       
         DS    CL1                                                              
LINPRD   DS    CL6                 PRODUCT                                      
         DS    CL1                                                              
LINCONT  DS    CL1                 * MEANS MORE JOBS THAN WILL FIT              
LINJOBS  DS    CL52                JOBS                                         
LINLNQ   EQU   *-LINED                                                          
*                                                                               
         ORG   LINDTE              (JWT JOBS)                                   
LINPRJI  DS    CL7                 PROJECT ID                                   
         DS    CL1                                                              
LINDTADD DS    CL8                 DATE ADDED                                   
         DS    CL1                                                              
LINBUNIT DS    CL5                 BUSINESS UNIT                                
         DS    CL1                                                              
LINCSTI  DS    CL6                 CUSTOMER ID                                  
         DS    CL1                                                              
LINPRDCT DS    CL3                 PRODUCT                                      
*                                                                               
         ORG   LINDTE              (BBDO JOBS)                                  
LINPROJ  DS    CL16                PROJECT ID                                   
         DS    CL2                                                              
LINDAT   DS    CL8                 DATE ADDED                                   
         DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRE4D                                                       
         SPACE 3                                                                
COUNTER  DS    PL4                 RECORD COUNTER                               
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
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TAGENE4   02/05/16'                                      
         END                                                                    
