*          DATA SET TAGEN4B    AT LEVEL 015 AS OF 04/08/14                      
*PHASE T7024BA                                                                  
         TITLE 'T7024B - LOCAL LIST'                                            
T7024B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7024B                                                         
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
         SPACE                                                                  
         GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY         FIRST TIME IN, VALIDATE KEY                  
         BNE   LCLL20                                                           
         BAS   RE,INIT             INITIALIZE INFO FOR SYSIO                    
         B     XIT                                                              
         SPACE 3                                                                
LCLL20   CLI   MODE,LISTRECS       IF MODE IS LISTRECS                          
         BNE   LCLL30                                                           
         LA    R2,LISTAR                                                        
         B     LSTREC                                                           
         SPACE 3                                                                
LCLL30   CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'                                                    
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LSTREC                                                           
         SPACE 3                                                                
LSTREC   EQU   *                                                                
         LA    R0,HOOK             SET ADDRESS OF HOOK FOR SYSIO                
         ST    R0,TIHOOK                                                        
         MVC   TIKHOOK,SETLSTK                                                  
         MVC   TIACOMFC,ACOMFACS                                                
         MVI   NLISTS,16           GET CONTROL BACK AT END OF PAGE              
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15           RESET AT END OF LIST                         
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)     SKIP LINE                                    
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(13,R1),=C'LOCAL RECORDS'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    XIT                                                              
         XC    CONSERV,CONSERV     SET AUTO $DQU                                
         MVC   CONSERV(4),=C'$DQU'                                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              PROCESS SYSIO RECORDS                                            
         SPACE                                                                  
         USING LISTD,R2                                                         
HOOK     NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         MVC   LISDET,SPACES       CLEAR PREVIOUS LINE                          
         MVC   LISCODE,TILOCL      LOCAL CODE                                   
         MVC   LISUN,TIUN          UNION                                        
         MVC   LISNAME,TINAME      TRUNCATED LONG NAME                          
         SPACE                                                                  
         USING TALOD,R4                                                         
         MVI   ELCODE,TALOELQ      GET LOCAL ELEMENT                            
         L     R4,TIAREC                                                        
         BAS   RE,GETEL                                                         
         BNE   HK1                                                              
         TM    TALOSTAT,TALOSHNW   IF HAS H&W FUND BIT ON                       
         BZ    *+8                                                              
         MVI   LISHNWF,C'*'        PUT * AFTER UNION                            
         SPACE                                                                  
         USING TANUD,R4                                                         
HK1      MVC   AIO,TIAREC                                                       
         MVI   ELCODE,TANUELQ      GET FREE FORM NUMBER ELEMENT                 
         LA    R1,TANUTLOC         FOR LOCAL ACCOUNT NUMBER                     
         STC   R1,BYTE                                                          
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BNE   HK3                                                              
         L     R4,TGELEM                                                        
         XC    WORK(L'LISACC),WORK                                              
         ZIC   R1,TANULEN                                                       
         SH    R1,=H'4'            R1=L'DATA-1 (SKIP TYPE)                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TANUMBER     ACCOUNT NUMBER                              
         MVC   LISACC,WORK                                                      
         SPACE                                                                  
HK3      L     R4,TIAREC                                                        
         USING TAADD,R4                                                         
         MVI   ELCODE,TAADELQ      POINT TO ADDRESS ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   HK8                                                              
         ZIC   R1,2(R4)            NUMBER OF LINES IN ELEMENT                   
         BCTR  R1,0                   LESS 1ST LINE                             
         LA    R4,3(R4)            POINT TO 1ST ACTUAL ADDRESS LINE             
         MVC   LISADDR1,0(R4)      MOVE INTO LINE - 1 ADDRESS LINE              
         CH    R1,=H'0'            IF ONLY 1 LINE OF ADDRESS                    
         BE    HK8                 SKIP LOOP                                    
         SPACE                                                                  
HK5      LA    R4,L'TAADADD(R4)    POINT TO NEXT LINE                           
         BCT   R1,HK5                                                           
         SPACE                                                                  
         MVC   LISADDR2,0(R4)      MOVE INTO LINE - LAST ADDRESS LINE           
         SPACE                                                                  
HK8      MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   HK10                                                             
         GOTO1 CATCHIOS            ENSURE IO OVERUSE FOR REPORTS                
         GOTO1 SPOOL,DMCB,(R8)     SPOOL IT                                     
         AP    COUNTER,=P'1'       COUNT LINES OUTPUT                           
         B     XIT                                                              
         SPACE                                                                  
HK10     CLI   LISTNUM,15                                                       
         BE    ENDPAGE             GET OUT IF ALREADY FILLED PAGE               
         GOTO1 LISTMON             ELSE CALL LISTMON                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND                                 
*              INITIALIZES INFO NECESSARY FOR SYSIO                             
         SPACE                                                                  
INIT     NTR1                                                                   
         MVI   TIREAD,TLLOCDQ      LIST BY LOCAL CODE                           
         MVI   ALLVAL,C'Y'         INIT ALL FIELDS PREV VALIDATED FLAG          
         LA    R2,SLOUNH                                                        
         TM    4(R2),X'20'         IF UNION NOT PREV VALIDATED                  
         BO    INIT3                                                            
         MVI   ALLVAL,C'N'                                                      
         XC    TIFUN,TIFUN         CLEAR FILTER                                 
         NI    SLOSTRH+4,X'DF'     MUST VALIDATE START FIELD                    
         CLI   5(R2),0                                                          
         BE    INIT2                                                            
         MVC   FULL(L'SLOUN),8(R2)                                              
         OC    FULL(L'SLOUN),SPACES PAD UNION INPUT WITH SPACES                 
         GOTO1 UNIVAL,DMCB,FULL     VALIDATE UNION                              
         BNE   FLDINV                                                           
         MVC   TIFUN,FULL          FILTER ON UNION                              
INIT2    OI    4(R2),X'20'                                                      
         SPACE                                                                  
INIT3    LA    R2,SLOSTRH                                                       
         TM    4(R2),X'20'         IF START AT FIELD NOT PREV VALIDATED         
         BO    INIT7                                                            
         MVI   ALLVAL,C'N'                                                      
         XC    TIQSTART,TIQSTART   RESET START AT                               
         ZIC   R3,5(R2)            IF THERE'S START INPUT                       
         LTR   R3,R3                                                            
         BZ    INIT6                                                            
         CLI   SLOUNH+5,0                                                       
         BE    NOINPUT             ERROR IF NO UNION INPUT                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),8(R2)                                                
INIT6    OI    4(R2),X'20'         SET PREV VALIDATED                           
         SPACE                                                                  
INIT7    LA    R2,SLOOPTSH         OPTIONS                                      
         TM    4(R2),X'20'         IF NOT PREV. VALIDATED                       
         BO    INIT19                                                           
         MVI   ALLVAL,C'N'                                                      
         CLI   5(R2),0                                                          
         BE    INIT18A                                                          
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0             INVALID INPUT                                
         BE    FLDINV                                                           
         ZIC   R0,4(R1)                                                         
         SPACE                                                                  
INIT10   EQU   *                   VALIDATE OPTIONS                             
         SPACE                                                                  
INIT18   LA    R3,SCANNEXT         GET NEXT                                     
         BCT   R0,INIT10                                                        
INIT18A  OI    4(R2),X'20'         SET PREV VALIDATED                           
         SPACE                                                                  
INIT19   CLI   ALLVAL,C'Y'         IF NOT ALL FIELDS PREV VALIDATED             
         BE    XIT                                                              
         XC    KEY,KEY             DEFAULT TO EQUAL ZERO FIRST TIME IN          
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF   GLOBAL STORAGE                               
         B     XIT                                                              
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
FLDMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
NOINPUT  MVI   ERROR,ERNOINP                                                    
         B     THEEND                                                           
         SPACE                                                                  
ENDPAGE  MVC   MYMSGNO1,OKNO       SET MESSAGE - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SLOSELH                                                       
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,34,C'LOCAL LIST'                                              
         SSPEC H2,34,C'----------'                                              
         SPACE 1                                                                
         SSPEC H4,1,C'UNI'                                                      
         SSPEC H4,5,C'LCL'                                                      
         SSPEC H4,9,C'ACCOUNT'                                                  
         SSPEC H4,22,C'LOCAL NAME'                                              
         SSPEC H4,49,C'ADDR1'                                                   
         SSPEC H4,63,C'ADDR2'                                                   
         SPACE 1                                                                
         SSPEC H5,1,C'---'                                                      
         SSPEC H5,5,C'---'                                                      
         SSPEC H5,9,C'-------'                                                  
         SSPEC H5,22,C'----------'                                              
         SSPEC H5,49,C'-----'                                                   
         SSPEC H5,63,C'-----'                                                   
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE                                  
         SPACE                                                                  
LISTD    DSECT                                                                  
LISDET   DS    0CL74                                                            
LISUN    DS    CL3                                                              
LISHNWF  DS    CL1                                                              
LISCODE  DS    CL3                                                              
         DS    CL1                                                              
LISACC   DS    CL12                                                             
         DS    CL1                                                              
LISNAME  DS    CL26                                                             
         DS    CL1                                                              
LISADDR1 DS    CL13                                                             
         DS    CL1                                                              
LISADDR2 DS    CL13                                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR4BD                                                       
         SPACE   3                                                              
COUNTER  DS    PL4                 COUNTER OF NUM OF OUTPUT LINES               
ALLVAL   DS    CL1                 Y=ALL FIELDS WERE PREV. VALIDATED            
         SPACE 3                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015TAGEN4B   04/08/14'                                      
         END                                                                    
