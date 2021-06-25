*          DATA SET TAGEN82    AT LEVEL 028 AS OF 04/08/14                      
*PHASE T70282A,*                                                                
         TITLE 'T70282 - BALANCE RECORDS - LIST'                                
T70282   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70282                                                         
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
         GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   BAR10                                                            
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
BAR10    CLI   MODE,LISTRECS                                                    
         BNE   BARX                                                             
         LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         B     LR                                                               
*                                                                               
BARX     B     XIT                                                              
*                                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       XC    TIQSTART,TIQSTART                                                
         LA    R2,SBASTRH                                                       
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VK10                                                             
         CLI   5(R2),0             NO INPUT                                     
         BE    VK10                                                             
         NI    SBAOPTSH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD              
         GOTO1 DATVAL,DMCB,(0,8(R2)),EBCDATE                                    
         GOTO1 DATCON,DMCB,(0,EBCDATE),(1,PWOSDATE)                             
         XC    PWOSDATE,=X'FFFFFF'                                              
         MVC   TIQSTART,PWOSDATE                                                
*                                                                               
VK10     OI    4(R2),X'20'                                                      
         LA    R2,SBAOPTSH         OPTIONS                                      
         TM    4(R2),X'20'         ALREADY VALIDATED                            
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
*                                                                               
VK20     DS    0H                  LEAVE ROOM FOR OPTIONS                       
*                                                                               
VK50     LA    R3,SCANNEXT                                                      
         BCT   R0,VK20                                                          
*                                                                               
VK60     OI    4(R2),X'20'                                                      
         XC    KEY,KEY            DEFAULT TO EQUAL ZERO                         
         BAS   RE,INIT                                                          
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVI   TIREAD,TLBACDQ      BALANCE RECORDS                              
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,16           IN ORDER TO GET CONTROL BACK                 
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15              IF LIST EXACTLY = 1 PAGE                  
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(21,R1),=C'BALANCE RECORDS'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING & NOW                            
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     GO STRAIGHT TO $DQU                          
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
*                                                                               
PR10     LA    R3,TIKEY                                                         
         USING TLBAD,R3                                                         
         CLI   TLBASEQ,0           ONLY LIST FIRST RECORD OF SEQUENCES          
         BNE   PRX                                                              
         MVC   COMPDATE,TLBADATE                                                
         XC    COMPDATE,=X'FFFFFF'                                              
         GOTO1 DATCON,DMCB,(1,COMPDATE),(8,BADATE)                              
*                                  BALANCE RECORD DATE                          
         CLI   TLBACURR,C'U'       US DOLLARS                                   
         BNE   PR20                                                             
         MVC   BACURR,=C'  US  '                                                
         B     PR30                                                             
*                                                                               
PR20     MVC   BACURR,=C'CANADA'                                                
*                                                                               
PR30     MVC   BAEMP,TLBAEMP       EMPLOYER                                     
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP                                                    
         BNE   PR60                                                             
         GOTO1 CATCHIOS            ENSURE NOT TOO MANY IOS                      
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'                                                    
         B     PRX                                                              
*                                                                               
PR60     CLI   LISTNUM,15          IF ALREADY FILLED PAGE                       
         BNE   PR70                                                             
         MVC   MYMSGNO1,OKNO       HIT ENTER FOR NEXT - MSG                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SBASELH                                                       
         B     ERRXIT                                                           
*                                                                               
PR70     GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRX      B     XIT                                                              
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
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        SPECS FOR SPOOLING                                                     
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'BALANCE RECORD LIST'                                     
         SSPEC H2,32,C'-------------------'                                     
         SPACE 1                                                                
         SSPEC H4,1,C'DATE'                                                     
         SSPEC H4,14,C'CURRENCY'                                                
         SSPEC H4,28,C'EMPLOYER'                                                
         SPACE 1                                                                
         SSPEC H5,1,C'----'                                                     
         SSPEC H5,14,C'--------'                                                
         SSPEC H5,28,C'--------'                                                
         SPACE 1                                                                
         DC    X'00'                                                            
*                                                                               
*                                                                               
LISTD    DSECT                                                                  
BADATE   DS    CL8                                                              
         DS    CL6                                                              
BACURR   DS    CL6                                                              
         DS    CL10                                                             
BAEMP    DS    CL3                                                              
         DS    CL3                                                              
BAEMPN   DS    CL36                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR82D                                                       
         EJECT                                                                  
*                                                                               
COUNTER  DS    PL4                 COUNT OUTPUT LINES                           
SAVKEY   DS    CL48                SAVED KEY                                    
EBCDATE  DS    CL6                 EBCDIC DATE                                  
COMPDATE DS    PL3                 UNCOMPLEMENTED PWOS DATE                     
PWOSDATE DS    PL3                 PWOS DATE                                    
TNAMEH   DS    CL44                NAME + HEADER                                
         SPACE 2                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028TAGEN82   04/08/14'                                      
         END                                                                    
