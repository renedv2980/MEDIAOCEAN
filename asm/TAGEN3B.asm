*          DATA SET TAGEN3B    AT LEVEL 014 AS OF 04/08/14                      
*PHASE T7023BA,*                                                                
         TITLE 'T7023B - EMPLOYER LIST'                                         
T7023B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7023B                                                         
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
EMP10    GOTO1 INITIAL,DMCB,PFTAB                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
EMP20    CLI   MODE,LISTRECS                                                    
         BNE   EMP30                                                            
         MVC   LISTAR,SPACES       CLEAR PREVIOS LINE                           
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
EMP30    CLI   MODE,PRINTREP                                                    
         BNE   EMPX                                                             
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
EMPX     B     XIT                                                              
*                                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       LA    R2,SEMSTRH          START AT SPECIFIC EMPLOYER                   
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK10                                                             
         NI    SEMOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         XC    TIQSTART,TIQSTART   START FROM BEGINING                          
         CLI   5(R2),0                                                          
         BE    VK10                                                             
*                                                                               
VK5      ZIC   R3,5(R2)                                                         
         BCTR  R3,0                SUB 1 FOR MOVE                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),8(R2)                                                
*                                                                               
VK10     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SEMOPTSH         OPTIONS                                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0             INVALID INPUT                                
         BE    INVERR                                                           
         ZIC   R0,4(R1)                                                         
         EJECT                                                                  
*                                                                               
VK20     DS    0H                  LEAVE ROOM FOR OPTIONS                       
*                                                                               
VK50     LA    R3,SCANNEXT                                                      
         BCT   R0,VK20                                                          
*                                                                               
VK60     OI    4(R2),X'20'         VALIDATED                                    
         XC    KEY,KEY             DEFAULT TO EQUAL ZERO                        
         BAS   RE,INIT                                                          
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVI   TIREAD,TLEMCDQ                                                   
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,16           IN ORDER TO GET CONTROL                      
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15              BACK AFTER 1 FULL PAGE                    
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(16,R1),=C'EMPLOYER RECORDS'                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS SYSIO RECORDS                                                  
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BE    PRREC                                                            
*                                                                               
LRHX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS RECORD                                                         
*                                                                               
PRREC    DS    0H                                                               
         USING LISTD,R2                                                         
         MVC   EMCODE,TIEMP        EMPLOYER CODE                                
         MVC   EMNAME,TINAME       EMPLOYER NAME                                
*                                                                               
PRR10    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP                                                    
         BNE   PRR20                                                            
         GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         B     PRRX                                                             
*                                                                               
PRR20    CLI   LISTNUM,15          END OF 1 PAGE                                
         BNE   PRR30                                                            
         MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SEMSELH                                                       
         B     ERRXIT                                                           
*                                                                               
PRR30    GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRRX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                 PFK TABLE                                     
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'EMTAX   ',CL8'DISPLAY '                               
PF13     DC    AL1(KEYTYCUR,L'EMCODE),AL2(EMCODE-LISTD)                         
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'EMPLOYER LIST'                                           
         SSPEC H2,32,C'-------------'                                           
         SPACE 1                                                                
         SSPEC H4,1,C'EMPLOYER'                                                 
         SSPEC H4,15,C'NAME'                                                    
         SPACE 1                                                                
         SSPEC H5,1,C'--------'                                                 
         SSPEC H5,15,C'----'                                                    
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
         SPACE 3                                                                
*                                                                               
LISTD    DSECT                                                                  
EMCODE   DS    CL3                                                              
         DS    CL11                                                             
EMNAME   DS    CL36                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR3BD                                                       
         EJECT                                                                  
*                                                                               
COUNTER  DS    PL4                 LINE COUNTER                                 
         SPACE 3                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014TAGEN3B   04/08/14'                                      
         END                                                                    
