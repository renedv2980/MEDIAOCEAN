*          DATA SET TAGENFE    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T702FEA                                                                  
         TITLE 'T702FE - TALENT FILE FIX PROGRAM'                               
T702FE   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702FE                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T702FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASYSD                                                         
         USING SYSD,R9             R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,TWAHOLE                                                       
         USING TFD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 3                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 3                                                                
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         XC    TFD(TFLNQ),TFD      CLEAR LOCAL W/S                              
         XC    TIFILTS,TIFILTS     AND SYSIO W/S                                
         SPACE 1                                                                
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         SPACE 1                                                                
         MVI   TIREAD,TLCKCDQ      READ CHECK RECORDS                           
         MVI   TIQDTYPE,TIQDCHK    PERIOD IS CHECK DATE                         
         SPACE 1                                                                
         MVC   TIQPSTR,=X'901030'  SET PWOS START AND END DATE                  
         MVC   TIQPEND,=X'901030'                                               
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
         SPACE 1                                                                
         MVC   P+1(18),=C'INVOICES CHANGED ='                                   
         EDIT  INVCNT,(12,P+20),ALIGN=LEFT,COMMAS=YES                           
         MVC   P2+1(16),=C'CHECKS CHANGED ='                                    
         EDIT  CHKCNT,(12,P2+18),ALIGN=LEFT,COMMAS=YES                          
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS RECORDS FROM SYSIO AND WRITE TO SORT                     
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC                                                   
         BNE   XIT                                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         SPACE 1                                                                
         MVC   AIO,TIAREC                                                       
         GOTO1 SAVPTRS,DMCB,CHKPTRS                                             
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACDD,R4                                                         
         CLC   TACDCHK,SPACES      DON'T BOTHER IF NO CHECK NUMBER              
         BNH   XIT                                                              
         PACK  TGDUB,TACDCHK       FIX CHECK NUMBER                             
         CP    TGDUB,=P'6000000'   IF NUMBER GT 6,000,000                       
         BL    XIT                                                              
         SP    TGDUB,=P'2897919'   SUBTRACT THIS NUMBER                         
         EDIT  (P8,TGDUB),(8,TACDCHK),FILL=0                                    
         SPACE 1                                                                
         BAS   RE,WRITEIT          WRITE IT BACK                                
         SPACE 1                                                                
         MVC   AIO,TIAREC                                                       
         MVC   DMDSKADD,TIDSKADD                                                
         XR    RF,RF                                                            
         CP    CHKCNT,NCHK                                                      
         BNL   *+8                                                              
         LA    RF,X'10'                                                         
         GOTO1 ADDPTRS,DMCB,((RF),CHKPTRS)  UPDATE PASSIVE POINTERS             
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         MVC   KEY,TIKEY           SET SYSIO'S KEY                              
         GOTO1 HIGH                AND RE-READ                                  
         SPACE 1                                                                
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE INSURES WE HAVE TP OFFICE FOR CURRENT AGENCY             
         SPACE 1                                                                
SETOFF   NTR1                                                                   
         LA    R2,OFFTAB           R2=A(TABLE)                                  
SETO2    CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         OC    0(6,R2),0(R2)       NEXT SLOT                                    
         BZ    SETO4                                                            
         CLC   TIAGY,0(R2)         ELSE MATCH ON AGENCY                         
         BE    SETOX               WE'VE SEEN THIS AGENCY ALREADY               
         LA    R2,6+1(R2)                                                       
         B     SETO2               KEEP ON LOOKING                              
         SPACE 1                                                                
SETO4    MVC   0(6,R2),TIAGY       SET NEW AGENCY IN TABLE                      
         SPACE 1                                                                
         OI    TFSTAT,TFREREAD     SET TO RE-READ SYSIO'S KEY                   
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',TIAGY)  READ AGENCY REC.              
         BE    *+12                                                             
         BAS   RE,NOAGY                                                         
         B     SETOX                                                            
         GOTO1 MYTRACE,DMCB,=C'AGENCY'                                          
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY DETAILS EL.                       
         BAS   RE,GETEL                                                         
         BNE   SETOX                                                            
         USING TAAYD,R4                                                         
         MVC   6(1,R2),TAAYTPOF    SET TP OFFICE IN TABLE                       
         SPACE 1                                                                
SETOX    MVC   TGOFF,6(R2)         SET CURRENT TP OFFICE                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE INSURES WE HAVE CLIENT/PRODUCT FOR CURRENT COMML         
         SPACE 1                                                                
SETCLIPR NTR1                                                                   
         CLC   TGCOM,TICOM         TEST COMMERCIAL CHANGED                      
         BE    SETCX                                                            
         OI    TFSTAT,TFREREAD     SET TO RE-READ SYSIO'S KEY                   
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TICOM)  READ COMMERCIAL REC.         
         BE    *+12                                                             
         BAS   RE,NOCOM            PRINT MISSING COMMERCIAL DETAILS             
         B     SETCX                                                            
         SPACE 1                                                                
         GOTO1 MYTRACE,DMCB,=C'COMMERCIAL'                                      
         SPACE 1                                                                
         L     R4,AIO              R4=A(ACTIVE KEY)                             
         USING TLCOD,R4                                                         
         MVC   TGCLI,TLCOCLI       SET CLIENT                                   
         MVC   TGPRD,TLCOPRD       AND PRODUCT                                  
         SPACE 1                                                                
SETCX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PRINTS DETAILS OF MISSING COMMERCIAL                     
         SPACE 1                                                                
NOAGY    NTR1                                                                   
         MVC   P+1(20),=C'*** MISSING AGENCY -'                                 
         MVC   P+22(6),TIAGY                                                    
         GOTO1 TINVCON,DMCB,TIINV,P+29,DATCON                                   
         B     NOALL                                                            
         SPACE 3                                                                
*              ROUTINE PRINTS DETAILS OF MISSING COMMERCIAL                     
         SPACE 1                                                                
NOCOM    NTR1                                                                   
         MVC   P+1(24),=C'*** MISSING COMMERCIAL -'                             
         GOTO1 HEXOUT,DMCB,TICOM,P+26,4,0                                       
         MVC   P+35(6),TIAGY                                                    
         GOTO1 TINVCON,DMCB,TIINV,P+42,DATCON                                   
         SPACE 3                                                                
NOALL    GOTO1 HEXOUT,DMCB,TIKEY,P+49,36,0  DISPLAY KEY, D/A                    
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE WRITES BACK RECORDS                                      
         SPACE 1                                                                
WRITEIT  NTR1                                                                   
         CLI   TIMODE,PROCINV      IF PROCESSING INVOICE RECORD                 
         BNE   WRI4                                                             
         AP    INVCNT,=P'1'        ADD TO INVOICE COUNT                         
         MVC   FILENAME,=CL8'TALFIL'                                            
         B     WRI10                                                            
         SPACE 1                                                                
WRI4     AP    CHKCNT,=P'1'        ELSE ADD TO CHECK COUNT                      
         MVC   FILENAME,=CL8'CHKFIL'                                            
         SPACE 1                                                                
WRI10    MVC   KEY+TLDRDA-TLDRD(4),TIDSKADD  SET D/A OF SYSIO RECORD            
         GOTO1 GETREC              GET SYSIO'S RECORD AGAIN                     
         SPACE 1                                                                
         MVC   AIO,TIAREC          CHANGED RECORD IS HERE                       
         GOTO1 MYTRACE,DMCB,(6,FILENAME)                                        
         GOTO1 PUTREC              WRITE BACK RECORD                            
         MVC   AIO,AIO1                                                         
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE OPTIONALLY PRINTS TRACE                                  
         SPACE 1                                                                
MYTRACE  NTR1                                                                   
         L     R2,0(R1)            R2=A(LITERAL)                                
         ZIC   R3,0(R1)            R3=L'LITERAL                                 
         SPACE 1                                                                
         L     R4,AIO                                                           
         CLI   0(R4),TLAYCDQ       IF THIS IS AGENCY RECORD                     
         BNE   MYTR2                                                            
         CP    AGYCNT,NAGY         TEST IF PAST MAX N'RECORDS TO TRACE          
         BNL   MYTRX                                                            
         AP    AGYCNT,=P'1'                                                     
         B     MYTR14                                                           
         SPACE 1                                                                
MYTR2    CLI   0(R4),TLCOCDQ       IF THIS IS COMMERCIAL RECORD                 
         BNE   MYTR4                                                            
         CP    COMCNT,NCOM         TEST IF PAST MAX N'RECORDS TO TRACE          
         BNL   MYTRX                                                            
         AP    COMCNT,=P'1'                                                     
         B     MYTR14                                                           
         SPACE 1                                                                
MYTR4    CLI   0(R4),TLINCDQ       IF THIS IS INVOICE RECORD                    
         BNE   MYTR6                                                            
         CP    INVCNT,NINV         TEST IF PAST MAX N'RECORDS TO TRACE          
         BNL   MYTRX                                                            
         B     MYTR14                                                           
         SPACE 1                                                                
MYTR6    CLI   0(R4),TLCKCDQ       IF THIS IS CHECK RECORD                      
         BNE   MYTR14                                                           
         CP    CHKCNT,NCHK         TEST IF PAST MAX N'RECORDS TO TRACE          
         BNL   MYTRX                                                            
         SPACE 1                                                                
MYTR14   GOTO1 TRACE,DMCB,(R4),0,(R2),(R3)                                      
         SPACE 1                                                                
MYTRX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   H4+10(L'USERNAME),USERNAME  COMPANY                              
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
NAGY     DC    PL6'10'                                                          
NCOM     DC    PL6'10'                                                          
NINV     DC    PL6'10'                                                          
NCHK     DC    PL6'10'                                                          
         SPACE 1                                                                
AGYCNT   DC    PL6'0'                                                           
COMCNT   DC    PL6'0'                                                           
INVCNT   DC    PL6'0'                                                           
CHKCNT   DC    PL6'0'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
*                                                                               
         SSPEC H1,53,C'TALENT FILE FIX PROGRAM'                                 
         SSPEC H2,53,24X'BF'                                                    
*                                                                               
         SSPEC H4,2,C'COMPANY'                                                  
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              AGENCY/OFFICE TABLE                                              
         SPACE 3                                                                
         DS    0D                                                               
         DC    CL8'*OFFTAB*'                                                    
OFFTAB   DC    (3000*(6+1))X'00'                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
TFD      DSECT                                                                  
TFPERIOD DS    CL17                DISPLAYABLE REQUEST PERIOD                   
*                                                                               
TFOPTS   DS    XL1                 OPTIONS                                      
TFTRACE  EQU   X'80'               TRACE ACTIVE                                 
*                                                                               
TFSTAT   DS    XL1                 STATUS                                       
TFREREAD EQU   X'80'               NEED TO RE-READ SYSIO'S KEY                  
*                                                                               
CHKPTRS  DS    CL(6*L'TLDRREC+1)                                                
*                                                                               
TFLNQ    EQU   *-TFD                                                            
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRFED                                                       
         EJECT                                                                  
         ORG   CONTAGH+X'300'                                                   
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006TAGENFE   05/01/02'                                      
         END                                                                    
