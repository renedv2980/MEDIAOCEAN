*          DATA SET TAGEN45    AT LEVEL 065 AS OF 08/08/05                      
*PHASE T70245A,*                                                                
         TITLE 'T70245 - INTERFACE LIST'                                        
T70245   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70245                                                         
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
INT10    GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
INT20    CLI   MODE,LISTRECS                                                    
         BNE   INT30                                                            
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
INT30    CLI   MODE,PRINTREP                                                    
         BNE   INTX                                                             
         XC    KEY,KEY             ENSURE START REPORT FROM BEG OF LIST         
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
INTX     B     XIT                                                              
*                                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       LA    R2,SIFAGYH          AGENCY                                       
         TM    4(R2),X'20'            PREVIOUSLY VALIDATED                      
         BO    VK20                                                             
         NI    SIFSTRH+4,X'DF'                                                  
         CLI   5(R2),0             ELSE DON'T REQUIRE AGENCY                    
         BE    VK20                                                             
*                                                                               
VK10     GOTO1 RECVAL,DMCB,TLAYCDQ,(R2)                                         
         OC    SIFAGY,SPACES                                                    
*                                                                               
VK20     OI    4(R2),X'20'                                                      
         LA    R2,SIFSTRH          CLIENT - TO START AT                         
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK40                                                             
         NI    SIFOPTSH+4,X'DF'                                                 
         CLI   5(R2),0             ONLY ALLOW START AT IF AGY PRESENT           
         BE    VK40                                                             
         CLI   SIFAGYH+5,0                                                      
         BNE   VK30                                                             
         LA    R2,SIFAGYH          SET R2 FOR ERROR MESSAGE                     
         B     MISSERR                                                          
*                                                                               
VK30     DS    0H                                                               
*                                                                               
VK40     OI    4(R2),X'20'                                                      
*                                                                               
         BAS   RE,VALOPTS                                                       
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
VALOPTS  NTR1                                                                   
         LA    R2,SIFOPTSH         OPTIONS                                      
         TM    4(R2),X'20'                                                      
         BO    VOPTX                                                            
*                                                                               
         XC    IDFILT,IDFILT                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BE    VOPT60                                                           
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0               INVALID INPUT                                
         BZ    INVERR                                                           
*                                                                               
VOPT45   MVC   ERRDISP,SCDISP1                                                  
         CLC   =C'ID',SCDATA1      ID FILTER ONLY VALID FILTER                  
         BNE   INVERR                                                           
*                                                                               
         MVC   ERRDISP,SCDISP2                                                  
         CLI   SCLEN2,6            2ND HALF LENGTH TEST                         
         BH    INVERR                                                           
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 USERVAL,DMCB,(X'A0',SCDATA2)                                     
         BNE   USERERR                                                          
*                                                                               
         MVC   AIO,AIO1                                                         
         CLI   TGACCHX,0           IF NO HEX RETURNED                           
         BE    USERERR             GIVE ERROR                                   
*                                                                               
         MVI   ERRDISP,0           VALIDATED                                    
         MVC   IDFILT,SCDATA2                                                   
VOPT50   LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT45                                                        
*                                                                               
VOPT60   OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
         XC    KEY,KEY             DEFAULT TO EQUAL ZERO                        
         BAS   RE,INIT                                                          
VOPTX    B     XIT                                                              
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
         MVC   TIFAGY,SPACES       AGENCY                                       
         XC    TIQSTART,TIQSTART   START AT CLIENT                              
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF      SCREEN                                    
         CLC   SIFAGY,SPACES       AGENCY                                       
         BNH   INIT10                                                           
         MVC   TIFAGY,SIFAGY       AGENCY                                       
*                                                                               
INIT10   MVC   TIQSTART(L'SIFSTR),SIFSTR                                        
         MVI   TIREAD,TLIFCDQ                                                   
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,16           GET CONTROL BACK AFTER                       
*                                                                               
LR5      GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15               A FULL PAGE                              
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         LA    R2,P                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(17,R1),=C'INTERFACE RECORDS'                                   
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
         XC    JOBSW,JOBSW         CLEAR NUMBER OF JOBS SW                      
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         USING LISTD,R2                                                         
         MVC   INTAGY,TIAGY        AGENCY                                       
         MVC   INTCLT,TICLI        CLIENT                                       
*                                                                               
         L     R4,TIAREC                                                        
         USING TAIFD,R4                                                         
         MVI   ELCODE,TAIFELQ      POINT TO INTERACE ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    IDFILT,IDFILT      IF ID FILTER SET, ONLY LIST                   
         BZ    PRREC05            INTERFACE RECORD WITH MATCHING                
         CLC   TAIFAGY,IDFILT     ID CODE                                       
         BNE   PRRX                                                             
*                                                                               
PRREC05  MVC   INTID,TAIFAGY       PRD AGY USER ID                              
         MVC   INTACC,TAIFVEND     VENDOR ACCOUNT                               
         OC    TAIFTV,TAIFTV       DEFAULT JOB TV                               
         BZ    PR10                                                             
         MVC   INTJOB,TAIFTV                                                    
         MVI   JOBSW,C'Y'                                                       
*                                                                               
PR10     OC    TAIFRAD,TAIFRAD     DEFAULT JOB RADIO                            
         BZ    PR20                                                             
         CLI   JOBSW,C'Y'          MORE THAN 1 JOB                              
         BE    PR15                                                             
         MVC   INTJOB,TAIFRAD                                                   
         B     PR20                                                             
*                                                                               
PR15     MVC   INTJB,=C'(*)'                                                    
*                                                                               
PR20     MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP                                                    
         BNE   PR30                                                             
         GOTO1 CATCHIOS            ENSURE DON'T DO TOO MANY I/O-S               
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'                                                    
         B     PRRX                                                             
*                                                                               
PR30     CLI   LISTNUM,15          IF END OF PAGE                               
         BNE   PR40                BUT THERE ARE MORE RECS                      
         MVC   MYMSGNO1,OKNO           PUT MSG - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SIFSELH                                                       
         B     ERRXIT                                                           
*                                                                               
PR40     GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRRX     B     XIT                                                              
         DROP  R4,R2                                                            
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
USERERR  MVI   ERROR,ERNOID                                                     
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
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,34,C'INTERFACE LIST'                                          
         SSPEC H2,34,C'--------------'                                          
         SPACE 1                                                                
         SSPEC H4,1,C'AGENCY'                                                   
         SSPEC H4,12,C'CLIENT'                                                  
         SSPEC H4,24,C'ID CODE'                                                 
         SSPEC H4,37,C'VENDOR ACCOUNT'                                          
         SSPEC H4,58,C'DEFAULT JOB'                                             
         SPACE 1                                                                
         SSPEC H5,1,C'------'                                                   
         SSPEC H5,12,C'------'                                                  
         SSPEC H5,24,C'-------'                                                 
         SSPEC H5,37,C'--------------'                                          
         SSPEC H5,58,C'-----------'                                             
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
LISTD    DSECT                                                                  
INTAGY   DS    CL6                                                              
         DS    CL6                                                              
INTCLT   DS    CL3                                                              
         DS    CL8                                                              
INTID    DS    CL6                                                              
         DS    CL7                                                              
INTACC   DS    CL14                                                             
         DS    CL7                                                              
INTJOB   DS    CL12                                                             
         DS    CL1                                                              
INTJB    DS    CL3                                                              
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR45D                                                       
         EJECT                                                                  
*                                                                               
COUNTER  DS    PL4                 LINE COUNTER                                 
JOBSW    DS    CL1                 SWITCH - NUMBER OF DEFAULT JOBS              
IDFILT   DS    CL6                 ID CODE FILTER                               
ERNOID   EQU   221                 USER ID NOT ON ACC                           
         SPACE 3                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
         SPACE 3                                                                
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065TAGEN45   08/08/05'                                      
         END                                                                    
