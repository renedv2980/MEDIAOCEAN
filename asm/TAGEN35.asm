*          DATA SET TAGEN35    AT LEVEL 046 AS OF 04/08/14                      
*PHASE T70235A                                                                  
         TITLE 'T70235 - CLIENT GROUP LIST'                                     
T70235   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70235                                                         
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
         CLI   MODE,VALKEY         IF MODE IS VALIDATE KEY                      
         BNE   CLGL20                                                           
         BAS   RE,INIT                                                          
         B     XIT                                                              
         SPACE 3                                                                
CLGL20   CLI   MODE,LISTRECS       IF MODE IS LISTRECS                          
         BNE   CLGL30                                                           
         MVC   LISTAR,SPACES       CLEAR PREV LINE                              
         LA    R2,LISTAR                                                        
         B     LSTREC                                                           
         SPACE 3                                                                
CLGL30   CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'                                                    
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,HOOKHEAD                                                      
         ST    R2,HEADHOOK                                                      
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
         MVC   0(20,R1),=C'CLIENT GROUP RECORDS'                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    XIT                                                              
         XC    CONSERV,CONSERV     SET AUTO $DQU                                
         MVC   CONSERV(4),=C'$DQU'                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND                                 
*              SETS UP INFO FOR SYSIO                                           
         SPACE                                                                  
INIT     NTR1                                                                   
         MVI   ALLVAL,C'Y'         INIT ALL FIELDS PREV VALIDATED FLAG          
         TM    SCGSTRH+4,X'20'     IF START AT FIELD NOT PREV VALIDATED         
         BO    INIT4                                                            
         MVI   ALLVAL,C'N'                                                      
         XC    TIQSTART,TIQSTART   RESET START AT                               
         ZIC   R3,SCGSTRH+5                                                     
         LTR   R3,R3                                                            
         BZ    INIT3                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),SCGSTR                                               
INIT3    OI    SCGSTRH+4,X'20'     SET PREV VALIDATED                           
         SPACE                                                                  
INIT4    LA    R2,SCGFMTH          VALIDATE FORMAT                              
         TM    4(R2),X'20'         IF NOT PREV VALIDATED                        
         BO    INIT7                                                            
         MVI   ALLVAL,C'N'                                                      
         MVI   TIREAD,TLCGNCDQ     DEFAULT TO LIST BY CLIENT GROUP NAME         
         CLI   5(R2),0                                                          
         BE    INIT6                                                            
         CLI   8(R2),C'A'                                                       
         BE    INIT6                                                            
         CLI   8(R2),C'C'          IF INPUT=C                                   
         BNE   FLDINV                                                           
         MVI   TIREAD,TLCGCDQ      LIST BY CLIENT GROUP CODE                    
INIT6    OI    4(R2),X'20'         SET PREV VALIDATED                           
         SPACE                                                                  
INIT7    LA    R2,SCGOPTSH         OPTIONS                                      
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
*              PROCESS SYSIO RECORDS                                            
         SPACE                                                                  
         USING LISTD,R2                                                         
HOOK     NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         MVC   LISCODE,TICLG       CLIENT GROUP CODE                            
         MVC   LISNAME,TINAME      DEFAULT TO TRUNCATED LONG NAME               
         CLC   TISHORT,SPACES      IF THERE'S A SHORT NAME                      
         BE    *+10                                                             
         MVC   LISNAME,TISHORT     USE IT                                       
         SPACE                                                                  
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
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
         SPACE 3                                                                
         EJECT                                                                  
HOOKHEAD NTR1                                                                   
         CLI   SCGSTRH+5,0                                                      
         BE    XIT                                                              
         MVC   H4+6(L'SCGSTR),SCGSTR                                            
         B     XIT                                                              
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE                                                                  
ENDPAGE  MVC   MYMSGNO1,OKNO       SET MESSAGE - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SCGSELH                                                       
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
XIT      XIT1                                                                   
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
         SSPEC H1,33,C'CGROUP LIST'                                             
         SSPEC H2,33,C'-----------'                                             
         SSPEC H4,1,C'START'                                                    
         SPACE 1                                                                
         SSPEC H6,1,C'CLIENT GROUP'                                             
         SSPEC H6,15,C'NAME'                                                    
         SPACE 1                                                                
         SSPEC H7,1,C'------------'                                             
         SSPEC H7,15,C'----'                                                    
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE                                  
         SPACE                                                                  
LISTD    DSECT                                                                  
LISCODE  DS    CL6                                                              
         DS    CL8                                                              
LISNAME  DS    CL16                                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR35D                                                       
         SPACE 3                                                                
ALLVAL   DS    CL1                 Y=ALL FIELDS WERE PREV. VALIDATED            
COUNTER  DS    PL4                 COUNTER OF NUM OF OUTPUT LINES               
         SPACE 3                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046TAGEN35   04/08/14'                                      
         END                                                                    
