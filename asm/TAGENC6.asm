*          DATA SET TAGENC6    AT LEVEL 033 AS OF 07/20/12                      
*PHASE T702C6C,*                                                                
         TITLE 'T702C6 - CAST EPISODE LIST'                                     
T702C6   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702C6                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB                                               
         MVC   SECSHED(7),=C'Pid Num'                                           
         OI    SECSHEDH+6,X'80'                                                 
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     SECIX                                                            
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BNE   SECI30                                                           
         MVI   NLISTS,16           SET N'LIST LINES                             
         OI    GLSTSTAT,RETEXTRA   SET OKAY TO RETURN EXTRA FOR EOL             
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR           R2 = A(DISPLAY LINE)                         
         B     SECI50                                                           
*                                                                               
SECI30   CLI   MODE,PRINTREP                                                    
         BNE   SECIX                                                            
         XC    KEY,KEY             START REPORT FROM BEGINING                   
         ZAP   COUNTER,=P'0'       LINE COUNTER                                 
         LA    R2,HOOK             SET A(HEADLINE HOOK)                         
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                R2 =A(DISPLAY LINE)                          
*                                                                               
SECI50   BAS   RE,LREC             GO LIST THE RECORDS                          
*                                                                               
SECIX    B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VKEY     NTR1                                                                   
         LA    R2,SECSSNH                                                       
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SECAGYH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         CLI   SECSSNH+5,6                                                      
         BH    VK02                                                             
         MVC   TGPID,SECSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK02                                                             
         MVC   SECSSN,TGSSN                                                     
         MVI   SECSSNH+5,9                                                      
VK02     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SECNAMEH                        
         MVC   TIFSSN,TGSSN        SET SYSIO FILTER                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SECSSN,SPACES                                                    
         MVC   SECSSN(L'TGPID),TGPID                                            
         MVI   SECSSNH+5,6                                                      
         OI    SECSSNH+6,X'80'                                                  
*                                                                               
VK10     LA    R2,SECAGYH          AGENCY                                       
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         MVC   SECAGYN,SPACES                                                   
         OI    SECAGYNH+6,X'80'    TRANSMIT                                     
         NI    SECEPIH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIFAGY,TIFAGY                                                    
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',(R2)),SECAGYNH    AGENCY              
         MVC   TIFAGY,TGAGY        SET SYSIO FILTER                             
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK20     LA    R2,SECEPIH          EPISODE NUMBER                               
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         NI    SECAGYH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIQSTART,TIQSTART                                                
         CLI   5(R2),0             IF EPISODE INPUT                             
         BE    VK30                                                             
         CLI   SECAGYH+5,0         AGENCY INPUT ALSO REQUIRED                   
         BNE   VK25                                                             
         LA    R2,SECAGYH                                                       
         B     FLDMISS                                                          
VK25     GOTO1 RECVAL,DMCB,TLEPCDQ,(R2)                                         
         MVC   TIQSTART,TGEPI      START AT PARTICULAR EPI NUMBER               
         XC    TIQSTART,COMPLM     COMPLEMENT EPISODE NUMBER                    
*                                                                               
VK30     XC    KEY,KEY             DEFAULT TO EQUAL ZERO                        
VKX      B      XIT                                                             
         EJECT                                                                  
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
LREC     NTR1                      SET HOOK TO SYSIO                            
         LA    R0,LRHOOK                                                        
         ST    R0,TIHOOK                                                        
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIUSERID,TWAORIG                                                 
         MVI   TIREAD,TLECCCDQ                                                  
         MVC   TIKHOOK,SETLSTK                                                  
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LRECX                                                            
         CP    COUNTER,=P'0'       ANYTHING TO REPORT?                          
         BE    LRECX                                                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(20,R1),=C'CAST EPISODE RECORDS'                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRECX                                                            
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         TWAXC SECL1H,PROT=Y       CLEAR THE SCREEN                             
*                                                                               
LRECX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS SYSIO RECORDS                                                  
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHX                                                             
*                                                                               
         BAS   RE,DISPLAY                                                       
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LRH30                                                            
         MVC   DMDSKADD,TIDSKADD                                                
         GOTO1 LISTMON             DISPLAY LINE TO SCREEN                       
         B     LRHX                                                             
*                                                                               
LRH30    GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    COUNTER,=P'1'       DISPLAY LINE TO REPORT                       
*                                                                               
LRHX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        DISPLAY LINE                                                           
*                                                                               
         USING LINED,R2            R2 =A(OUTPUT AREA)                           
DISPLAY  NTR1                                                                   
         L     R6,TIAREC                                                        
         USING TLECD,R6                                                         
         MVC   TGCOM,TICOM                                                      
*                                                                               
         GOTO1 XNAME,DMCB,TLCOCCDQ,WORK,TIKEY                                   
         L     R1,AIO                                                           
         USING TLCOD,R1                                                         
         OC    TIFAGY,TIFAGY                                                    
         BZ    DISP5                                                            
         CLC   TIFAGY,TLCOAGY      IF AGENCY NOT REQUEST AGENCY                 
         BNE   DISPX               SKIP IT DISPLAYING                           
DISP5    MVC   LINEAGY,TLCOAGY     AGENCY                                       
         MVC   LINECID,WORK        COMMERCIAL ID                                
         DROP  R1                                                               
         MVC   LINEEPI,TLECEPI     EPISODE                                      
         XC    LINEEPI,COMPLM      COMPLEMENT EPISODE NUMBER                    
         MVC   LINECAT,TICAT       CATEGORY                                     
*                                                                               
         LR    R4,R6                                                            
         MVI   ELCODE,TACAELQ      CAST DETAILS ELEMENT                         
         USING TACAD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   DISP20                                                           
         MVC   LINECAM,TACAONOF    CAMERA                                       
         MVC   LINETAX,TACAUNIT    TAX                                          
         XC    LINEACDE,LINEACDE                                                
         OC    TACANCDE,TACANCDE   AGENT                                        
         BZ    DISP10                                                           
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),LINEACDE                           
*                                                                               
DISP10   MVC   LINEUN,TACAUN       UNION                                        
         MVC   LINELOCL,TACALOCL   LOCAL                                        
         MVC   LINEYEAR,TACAYEAR   YEAR                                         
         MVC   LINECORP,TACACORP   CORP                                         
*                                                                               
DISP20   XC    LINEINV,LINEINV     INVOICE NUMBER                               
         OC    TIINV,TIINV                                                      
         BZ    DISPX                                                            
         GOTO1 TINVCON,DMCB,TIINV,LINEINV,DATCON                                
*                                                                               
DISPX    B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*                                                                               
HOOK     NTR1                                                                   
         MVI   BYTE,C'H'           MOVE KEY FIELDS TO HEADS                     
         GOTO1 PRTSCRN,DMCB,CONTAGH,SECHD1H,H4-1                                
         GOTO1 (RF),(R1),SECHD1H,SECSELH,H6-5                                   
         MVC   H6-5(5),SPACES      CLEAR SELECT FIELD                           
         B     XIT                                                              
         SPACE 2                                                                
FLDMISS  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPP2)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     ERRXIT                                                           
                                                                                
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'ECAST   ',CL8'LIST   '                                
PF13     DC    AL1(KEYTYCUR,L'LINEAGY-1),AL2(LINEAGY-LINED)                     
         DC    AL1(KEYTYCUR,L'LINECID-1),AL2(LINECID-LINED)                     
         DC    AL1(KEYTYCUR,L'LINEEPI-1),AL2(LINEEPI-LINED)                     
PF13X    EQU   *                                                                
         SPACE 2                                                                
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3' ',CL8'HISTORY ',CL8'DISPLAY'                                
PF16     DC    AL1(KEYTYCUR,L'LINEAGY-1),AL2(LINEAGY-LINED)                     
         DC    AL1(KEYTYCUR,L'LINEINV-1),AL2(LINEINV-LINED)                     
PF16X    EQU   *                                                                
         SPACE 2                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
COMPLM   DC    6X'FF'                                                           
         EJECT                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'CAST EPISODE LIST'                                       
         SSPEC H2,32,C'-----------------'                                       
         SSPEC H7,1,C'------- ------ -------      ---'                          
         SSPEC H7,33,C'--- --- ---- ------- --  --- -----'                      
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
LINED    DSECT                                                                  
LINEEPI  DS    CL5                                                              
         DS    CL3                                                              
LINEAGY  DS    CL6                                                              
         DS    CL1                                                              
LINECID  DS    CL12                                                             
         DS    CL1                                                              
LINECAT  DS    CL3                                                              
         DS    CL1                                                              
LINECAM  DS    CL3                                                              
         DS    CL1                                                              
LINETAX  DS    CL3                                                              
         DS    CL1                                                              
LINEACDE DS    CL4                                                              
         DS    CL1                                                              
LINEUN   DS    CL3                                                              
         DS    CL1                                                              
LINELOCL DS    CL3                                                              
         DS    CL1                                                              
LINEYEAR DS    CL3                                                              
         DS    CL1                                                              
         DS    CL1                                                              
LINECORP DS    CL1                                                              
         DS    CL1                                                              
         DS    CL1                                                              
LINEINV  DS    CL6                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRC6D                                                       
         EJECT                                                                  
*                                                                               
COUNTER  DS    PL4                 LINE COUNTER                                 
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033TAGENC6   07/20/12'                                      
         END                                                                    
