*          DATA SET TAGENF4    AT LEVEL 013 AS OF 07/09/14                      
*PHASE T702F4C,*                                                                
         TITLE 'T702F4 - T4 + RL1 LIST'                                         
T702F4   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702F4                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=TWAHOLE                                   
         USING TWAD,R7                                                          
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
W210     GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,SETFILE        SET FILE FOR PAGING                          
         BNE   W215                                                             
         BAS   RE,SETCHK                                                        
         B     XIT                                                              
*                                                                               
W215     CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
W220     CLI   MODE,LISTRECS                                                    
         BNE   W230                                                             
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
W230     CLI   MODE,PRINTREP                                                    
         BNE   W2X                                                              
         XC    TIKEY,TIKEY         ENSURE START REPORT FROM BEG OF LIST         
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
W2X      B     XIT                                                              
*                                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       DS    0H                                                               
         LA    R2,ST4YEARH         YEAR FILTER                                  
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK10                                                             
         CLI   5(R2),0                                                          
         BNE   VK05                                                             
         GOTO1 ADDAY,DMCB,(C'Y',TGTODAY0),WORK,-1                               
         GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+6)                                 
         MVC   ST4YEAR,WORK+6      CCYY                                         
         OI    ST4YEARH+6,X'80'                                                 
         MVC   TIFYEAR(2),WORK+8                                                
         B     VK10                                                             
*                                                                               
VK05     CLI   5(R2),4             INPUT MUST BE 4 LONG                         
         BNE   INVERR                                                           
         MVC   WORK(4),=4X'F0'     INSURE VALID NUMERIC                         
         MVZ   WORK(4),8(R2)                                                    
         CLC   WORK(4),=4X'F0'                                                  
         BNE   INVERR                                                           
         MVC   TIFYEAR(2),ST4YEAR+2                                             
*                                                                               
VK10     OI    4(R2),X'20'                                                      
                                                                                
         BAS   RE,SETTAL           SET TALENT FILE                              
         LA    R2,ST4EMPH          EMP FILTER                                   
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK15                                                             
         MVC   ST4EMPN,SPACES                                                   
         OI    ST4EMPNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BNE   VK13                                                             
         MVC   ST4EMP,TGTPEMP      DEFAULT TO TP EMPLOYER                       
         MVI   ST4EMPH+5,3                                                      
         OI    ST4EMPH+6,X'80'                                                  
*                                                                               
VK13     BAS   RE,SETTAL           SET TALENT FILE                              
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'28',(R2)),ST4EMPNH                        
         BAS   RE,SETCHK           SET CHECK FILE                               
*                                                                               
VK15     MVC   TIFEMP,TGEMP                                                     
         OI    4(R2),X'20'                                                      
         LA    R2,ST4STRH          START AT                                     
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK20                                                             
         XC    TIQSTART,TIQSTART                                                
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         ZIC   R1,5(R5)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(R1),8(R2)                                               
*                                                                               
VK20     OI    4(R2),X'20'                                                      
         LA    R2,ST4OPTSH         OPTIONS                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VKX                                                              
         MVI   SCROPT,0                                                         
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0             INVALID INPUT                                
         BE    INVERR                                                           
         ZIC   R0,4(R1)                                                         
         CLC   =C'ALL',SCDATA1                                                  
         BNE   INVERR                                                           
         OI    SCROPT,SCRALL       SET OPTION BIT                               
*                                                                               
VK40     DS    0H                                                               
*                                                                               
VK50     LA    R3,SCANNEXT                                                      
         BCT   R0,VK40                                                          
*                                                                               
VK60     OI    4(R2),X'20'                                                      
         BAS   RE,INIT                                                          
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         XC    SVSSN,SVSSN         CLEAR LAST SSN READ                          
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF   GLOBAL STORAGE                               
         MVI   TIREAD,TLT4CDQ      LIST T4 RECORDS                              
         MVI   TIRDSUBT,TLT4SCDQ                                                
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK     LIST KEY HOOK                                
         MVI   NLISTS,16           IN ORDER TO GET CONTROL BACK                 
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15              AFTER 1 FULL PAGE                         
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(10,R1),=C'T4 RECORDS'                                          
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
         TM    SCROPT,SCRALL       IF WE ONLY WANT TO SHOW THE LATEST           
         BO    PR02                                                             
         CLC   SVSSN,TISSN         W2 - COMPARE TO PREVIOUS SSN                 
         BE    PRRX                IF IT'S THE SAME - EXIT                      
*                                                                               
PR02     MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         USING LISTD,R2                                                         
         L     R4,TIAREC                                                        
         USING TLT4D,R4                                                         
         MVC   W2SSN,TISSN                SS NUMBER                             
         MVC   TGSSN,TISSN                                                      
*                                                                               
PR05     MVC   AIO,AIO2            DON'T CREAM COMMERCIAL RECORD                
         BAS   RE,SETTAL           SET TALENT FILE                              
         MVI   SSNAMEH,40          SET LENGTH OF FAKE FIELD                     
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A8',0),SSNAMEH                            
         MVC   W2SSNN,SSNAME                                                    
         BAS   RE,SETCHK           SET CHECK FILE                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         MVI   ELCODE,TAWSELQ      GET W2 SUBSIDARY ELEMENT                     
         USING TAWSD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   PR30                                                             
         OC    TAWSDPRT,TAWSDPRT   CHECK IF W2 WAS PRINTED                      
         BZ    PR30                                                             
         GOTO1 DATCON,DMCB,(1,TAWSDPRT),(8,W2PRINT)                             
*                                                                               
PR30     MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP                                                    
         BNE   PR40                                                             
         GOTO1 CATCHIOS            ENSURE DON'T DO TOO MANY I/O-S               
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'                                                    
         B     PRRX                                                             
*                                                                               
PR40     CLI   LISTNUM,15          IF END OF PAGE                               
         BNE   PR50                BUT THERE ARE MORE RECS                      
         MVC   MYMSGNO1,OKNO           PUT MSG - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,ST4SELH                                                       
         B     ERRXIT                                                           
*                                                                               
PR50     GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRRX     MVC   SVSSN,TISSN         SET LAST SSN READ                            
         B     XIT                                                              
         DROP  R4,R2                                                            
         EJECT                                                                  
*                                                                               
*        ROUTINE TO SET SYSFIL/DIR TO TALENT FILE                               
*                                                                               
SETTAL   NTR1                                                                   
         MVC   SYSFIL,=C'TALFIL'                                                
         MVC   SYSDIR,=C'TALDIR'                                                
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*        ROUTINE TO SET SYSFIL/DIR TO CHECK FILE                                
*                                                                               
SETCHK   NTR1                                                                   
         MVC   SYSFIL,=C'CHKFIL'                                                
         MVC   SYSDIR,=C'CHKDIR'                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
*              CONSTANTS, ETC.                                                  
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'T4 LIST'                                                 
         SSPEC H2,32,C'-------'                                                 
         SPACE 1                                                                
         SSPEC H4,1,C'SIN'                                                      
         SSPEC H4,11,C'NAME'                                                    
         SSPEC H4,44,C'CHANGED'                                                 
         SSPEC H4,53,C'PRINTED'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'---'                                                      
         SSPEC H5,11,C'----'                                                    
         SSPEC H5,44,C'-------'                                                 
         SSPEC H5,53,C'-------'                                                 
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
LISTD    DSECT                                                                  
W2SSN    DS    CL9                                                              
         DS    CL1                                                              
W2SSNN   DS    CL32                                                             
         DS    CL1                                                              
W2CHANGE DS    CL8                                                              
         DS    CL1                                                              
W2PRINT  DS    CL8                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR05D                                                       
         EJECT                                                                  
*                                                                               
COUNTER  DS    PL4                 LINE COUNTER                                 
FILTDATE DS    CL6                 EBCDIC DATE FILTER                           
KEYCHG   DS    CL1                 FLAG IF KEY CHANGED                          
TEMP     DS    PL3                 TEMP DATE                                    
MYCURR   DS    CL1                 CURRENCY                                     
*                                                                               
SSNAMEH  DS    XL8                 FAKE FIELD HEADER                            
SSNAME   DS    CL32                OUTPUT FIELD                                 
*                                                                               
SVSSN    DS    CL9                 LAST SSN READ                                
SCROPT   DS    XL1                 OPTIONS                                      
SCRALL   EQU   X'80'               LIST ALL RECORD                              
*                                                                               
SVKEY    DS    CL48                SAVED KEY                                    
         SPACE 5                                                                
TWAD     DSECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
         SPACE 3                                                                
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013TAGENF4   07/09/14'                                      
         END                                                                    
