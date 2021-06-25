*          DATA SET TAGENB5    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T702B5A                                                                  
         TITLE 'T702B5 - DEAL LIST'                                             
T702B5   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702B5                                                         
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
         CLI   MODE,LISTRECS                                                    
         BNE   MAIN30                                                           
         MVI   NLISTS,16           SET N'LIST LINES                             
         OI    GLSTSTAT,RETEXTRA   SET OK TO RETURN EXTRA FOR EOL               
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR           R2=A(DISPLAY AREA)                           
         B     MAIN40                                                           
         SPACE 1                                                                
MAIN30   CLI   MODE,PRINTREP                                                    
         BNE   MAINX                                                            
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                R2=A(DISPLAY AREA)                           
         SPACE 1                                                                
MAIN40   BAS   RE,LREC             GO LIST THE RECORDS                          
         SPACE 1                                                                
MAINX    B     XIT                                                              
         SPACE 3                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,SDLAGYH          VALIDATE AGENCY                              
         TM    4(R2),X'20'         TEST PREVIOUSLY VALIDATED                    
         BO    VK10                                                             
         NI    SDLSTRTH+4,X'DF'    SET TO VALIDATE CLIENT                       
         XC    TIFAGY,TIFAGY                                                    
         XC    SDLAGYN,SDLAGYN                                                  
         OI    SDLAGYNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK8                                                              
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SDLAGYNH                        
         MVC   TIFAGY,TGAGY                                                     
VK8      OI    4(R2),X'20'         SET VALIDATED                                
         SPACE 1                                                                
VK10     LA    R2,SDLSTRTH         START AT (CLIENT)                            
         SPACE 1                                                                
         TM    4(R2),X'20'         TEST PREVIOUSLY VALIDATED                    
         BO    VKX                                                              
         XC    TIQSTART,TIQSTART   START FROM BEGINING                          
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   TIQSTART(L'SDLSTRT),8(R2)                                        
         OI    4(R2),X'20'         SET VALIDATED                                
         SPACE 1                                                                
         XC    KEY,KEY             INITIALIZE KEY                               
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLDLCDQ      SET TO READ DEAL RECORDS                     
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LREC     NTR1                                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         EDIT  COUNTER,(4,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(12,R1),=C'DEAL RECORDS'                                        
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
         USING LINED,R2            R2=A(OUTPUT AREA)                            
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHX                                                             
         MVC   LINAGY,TIAGY        AGENCY                                       
         MVC   LINCLI,TICLI        CLIENT                                       
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF PRINTING                                  
         BNE   LRH20                                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         B     LRHX                                                             
         SPACE 1                                                                
LRH20    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         SPACE 1                                                                
LRHX     B     XIT                                                              
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
         SSPEC H1,34,C'DEAL LIST'                                               
         SSPEC H2,34,C'---------'                                               
         SPACE 1                                                                
         SSPEC H4,2,C'AGENCY  CLIENT'                                           
         SSPEC H5,2,C'------  ------'                                           
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 2                                                                
LINED    DSECT                                                                  
         DS    CL1                                                              
LINAGY   DS    CL6                                                              
         DS    CL2                                                              
LINCLI   DS    CL6                                                              
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRB5D                                                       
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
**PAN#1  DC    CL21'006TAGENB5   05/01/02'                                      
         END                                                                    
