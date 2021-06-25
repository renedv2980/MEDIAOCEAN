*          DATA SET TAGEN84    AT LEVEL 007 AS OF 05/01/02                      
*PHASE T70284A                                                                  
         TITLE 'T70284 - OFFICE LIST'                                           
T70284   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70284                                                         
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
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   OFF30                                                            
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     OFF40                                                            
         SPACE 1                                                                
OFF30    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         SPACE 1                                                                
OFF40    BAS   RE,LREC             GO LIST THE RECORDS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,SOFSTRH          START AT SPECIFIC OFFICE                     
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK10                                                             
         NI    SOFOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         XC    TIQSTART,TIQSTART   START FROM BEGINING                          
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   TIQSTART(1),8(R2)                                                
         OI    4(R2),X'20'         VALIDATED                                    
         SPACE 1                                                                
VK10     LA    R2,SOFOPTSH         OPTIONS                                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         OI    4(R2),X'20'         VALIDATED                                    
         SPACE 1                                                                
         BAS   RE,INIT             RE-INITIALIZE LIST                           
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 1                                                                
INIT     NTR1                                                                   
         XC    KEY,KEY             INITIALIZE KEY                               
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLOFCDQ                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LREC     NTR1                                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         SPACE 1                                                                
         MVI   NLISTS,16           IN ORDER TO GET CONTROL                      
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15           BACK AFTER 1 FULL PAGE                       
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         EDIT  COUNTER,(3,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(14,R1),=C'OFFICE RECORDS'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS                                           
         SPACE 1                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BE    PRREC                                                            
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS RECORD                                                   
         SPACE 1                                                                
         USING LINED,R2            R2=A(OUTPUT AREA)                            
PRREC    DS    0H                                                               
         MVC   LINOFF,TIOFF        OFFICE CODE                                  
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAOFELQ      LOOK FOR OFFICE DETAILS EL.                  
         BAS   RE,GETEL                                                         
         BNE   PRR10                                                            
         USING TAOFD,R4                                                         
         MVC   LINID,TAOFIDCD      DISPLAY ID CODE                              
         SPACE 1                                                                
         MVI   LINTEL,C'('         TELEPHONE                                    
         MVC   LINTEL+1(3),TAOFTEL                                              
         MVI   LINTEL+4,C')'                                                    
         MVC   LINTEL+5(3),TAOFTEL+3                                            
         MVI   LINTEL+8,C'-'                                                    
         MVC   LINTEL+9(4),TAOFTEL+6                                            
         SPACE 1                                                                
PRR10    XC    BLOCK(8),BLOCK      SET UP DUMMY HEADER FOR ADDRESS              
         MVI   BLOCK,4*30+8                                                     
         MVC   AIO,TIAREC                                                       
         GOTO1 CHAROUT,DMCB,TAADELQ,BLOCK   MOVE IT TO BLOCK                    
         MVC   AIO,AIO1                                                         
         OC    BLOCK+8(4*30),SPACES                                             
         GOTO1 SQUASHER,DMCB,BLOCK+8,4*30   AND SQUASH IT                       
         MVC   LINADDR,BLOCK+8                                                  
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   PRR20                                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         B     PRRX                                                             
         SPACE 1                                                                
PRR20    CLI   LISTNUM,15          END OF 1 PAGE                                
         BNE   PRR30                                                            
         MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SOFSELH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
PRR30    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         SPACE 1                                                                
PRRX     B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
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
         SPACE 1                                                                
         SSPEC H1,33,C'OFFICE LIST'                                             
         SSPEC H2,33,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H4,2,C'OFF  ID      ADDRESS'                                     
         SSPEC H5,2,C'---  --      -------'                                     
         SSPEC H4,62,C'TELEPHONE'                                               
         SSPEC H5,62,C'---------'                                               
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 2                                                                
LINED    DSECT                                                                  
         DS    CL2                                                              
LINOFF   DS    CL1                                                              
         DS    CL3                                                              
LINID    DS    CL6                                                              
         DS    CL2                                                              
LINADDR  DS    CL45                                                             
         DS    CL2                                                              
LINTEL   DS    CL13                                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR84D                                                       
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
**PAN#1  DC    CL21'007TAGEN84   05/01/02'                                      
         END                                                                    
