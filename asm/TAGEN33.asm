*          DATA SET TAGEN33    AT LEVEL 005 AS OF 04/08/14                      
*PHASE T70233A                                                                  
         TITLE 'T70233 - AGENCY GROUP LIST'                                     
T70233   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70233                                                         
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
         BNE   AGRL20                                                           
         BAS   RE,INIT             INITIALIZE INFO FOR SYSIO                    
         B     XIT                                                              
         SPACE 3                                                                
AGRL20   CLI   MODE,LISTRECS       IF MODE IS LISTRECS                          
         BNE   AGRL30                                                           
         MVC   LISTAR,SPACES       CLEAR PREV LINE                              
         LA    R2,LISTAR                                                        
         B     LSTREC                                                           
         SPACE 3                                                                
AGRL30   CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
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
         MVC   0(20,R1),=C'AGENCY GROUP RECORDS'                                
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
         TM    SAGSTRH+4,X'20'     IF START AT FIELD NOT PREV VALIDATED         
         BO    INIT4                                                            
         MVI   ALLVAL,C'N'                                                      
         XC    TIQSTART,TIQSTART   RESET START AT                               
         ZIC   R3,SAGSTRH+5                                                     
         LTR   R3,R3                                                            
         BZ    INIT3                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),SAGSTR                                               
INIT3    OI    SAGSTRH+4,X'20'     SET PREV VALIDATED                           
         SPACE                                                                  
INIT4    LA    R2,SAGFMTH          VALIDATE FORMAT                              
         TM    4(R2),X'20'         IF NOT PREV VALIDATED                        
         BO    INIT7                                                            
         MVI   ALLVAL,C'N'                                                      
         MVI   TIREAD,TLAGNCDQ     DEFAULT TO LIST BY AGENCY GROUP NAME         
         CLI   5(R2),0                                                          
         BE    INIT6                                                            
         CLI   8(R2),C'A'                                                       
         BE    INIT6                                                            
         CLI   8(R2),C'C'          CODE SEQUENCE                                
         BNE   FLDINV                                                           
         MVI   TIREAD,TLAGCDQ      LIST BY AGENCY GROUP CODE                    
INIT6    OI    4(R2),X'20'         SET PREV VALIDATED                           
         SPACE                                                                  
INIT7    LA    R2,SAGOPTSH         OPTIONS                                      
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
         MVC   LISCODE,TIAGG       AGENCY GROUP CODE                            
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
         SPACE                                                                  
         GOTO1 LISTMON             ELSE CALL LISTMON                            
         B     XIT                                                              
         SPACE 2                                                                
         DROP  R2                                                               
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE                                                                  
ENDPAGE  MVC   MYMSGNO1,OKNO       SET MESSAGE - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SAGSELH                                                       
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
         SSPEC H1,32,C'AGENCY GROUP LIST'                                       
         SSPEC H2,32,C'-----------------'                                       
         SPACE 1                                                                
         SSPEC H4,1,C'AGENCY GROUP'                                             
         SSPEC H4,23,C'NAME'                                                    
         SPACE 1                                                                
         SSPEC H5,1,C'------------'                                             
         SSPEC H5,23,C'----'                                                    
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE                                  
         SPACE                                                                  
LISTD    DSECT                                                                  
LISCODE  DS    CL6                                                              
         DS    CL16                                                             
LISNAME  DS    CL16                                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR33D                                                       
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
**PAN#1  DC    CL21'005TAGEN33   04/08/14'                                      
         END                                                                    
