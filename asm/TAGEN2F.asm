*          DATA SET TAGEN2F    AT LEVEL 073 AS OF 11/10/10                      
*PHASE T7022FD,*                                                                
         TITLE 'T7022F - AGENT NUMBER CODE CHANGE'                              
T7022F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7022F                                                         
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
*                                                                               
         OI    SAGSSNPH+6,X'80'                                                 
         MVC   SAGSSNP,=CL9'PID'                                                
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         SPACE 2                                                                
**NO-OP**CLI   MODE,VALREC                                                      
**NO-OP**BNE   AGX                                                              
         CLI   MODE,PRINTREP       IF PROCESSING OFFLINE                        
         BNE   AGX                                                              
         BAS   RE,INIT                                                          
         ZAP   COUNTER,=P'0'                                                    
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         B     LR                                                               
*                                                                               
AGX      B     XIT                                                              
         SPACE 2                                                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VALIDATE KEY                                                           
*                                                                               
VK       DS    0H                                                               
         LA    R2,SAGCCODH                                                      
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SAGSSNH+4,X'DF'     FORCE VALIDATION                             
         XC    OLDCODE,OLDCODE                                                  
         XC    FILTAGT,FILTAGT                                                  
         MVC   SAGCNAM,SPACES      CLEAR AGENT NAME                             
         OI    SAGCNAMH+6,X'80'                                                 
         CLC   8(2,R2),=C'NO'      FROM NO AGENT TO AN AGENT                    
         BE    VK10                                                             
         GOTO1 RECVAL,DMCB,TLANCCDQ,(X'38',0(R2)),SAGCNAMH                      
         MVC   FILTAGT,8(R2)       SET AGENT FILTER                             
         GOTO1 TRNSAGT,DMCB,(X'80',8(R2)),OLDCODE                               
*                                                                               
VK10     OI    4(R2),X'20'                                                      
         LA    R2,SAGSSNH                                                       
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         XC    FILTSSN,FILTSSN     CLEAR SSN FILTER                             
         NI    SAGNCODH+4,X'DF'    FORCE VALIDATION                             
         MVC   SAGSSNM,SPACES      CLEAR NAME                                   
         OI    SAGSSNMH+6,X'80'                                                 
         CLI   5(R2),0             REQUIRE INPUT                                
         BE    MISSERR                                                          
         CLC   =C'ALL',8(R2)       IF INPUT 'ALL' SS NUMS                       
         BNE   VK20                                                             
         OC    FILTAGT,FILTAGT     THEN MUST HAVE AN ACTUAL AGENT               
         BZ    INVERR                                                           
         B     VK30                                                             
*                                                                               
VK20     CLI   5(R2),6             IF INPUT IS NOT A PID                        
         BE    VK25                                                             
         MVC   WORK(9),=9X'F0'     INSURE VALID NUMERIC                         
         MVZ   WORK(9),8(R2)                                                    
         CLC   WORK(9),=9X'F0'                                                  
         BNE   INVERR                                                           
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',0(R2)),SAGSSNMH                       
         MVC   FILTSSN,TGSSN       SET SS NUMBER FILTER                         
         B     VK30                                                             
*                                                                               
VK25     MVI   PIDVALID,C'N'                                                    
         MVC   TGPID,SAGSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,SAGSSN                                        
         BNE   VK26                                                             
         MVI   PIDVALID,C'Y'                                                    
         MVI   SAGSSNH+5,9                                                      
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',0(R2)),SAGSSNMH                       
         MVC   FILTSSN,TGSSN                                                    
VK26     XC    SAGSSN,SAGSSN                                                    
         MVC   SAGSSN(L'TGPID),TGPID                                            
         CLI   PIDVALID,C'Y'                                                    
         BNE   INVERR                                                           
*                                                                               
VK30     OI    4(R2),X'20'                                                      
         LA    R2,SAGNCODH         NEW AGENT CODE                               
         TM    4(R2),X'20'                                                      
         BO    VK50                                                             
         XC    NEWCODE,NEWCODE                                                  
         MVC   SAGNNAM,SPACES      CLEAR AGENT NAME                             
         OI    SAGNNAMH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLI   8(R2),C'X'                                                       
         BE    VK40                                                             
         GOTO1 RECVAL,DMCB,TLANCCDQ,(X'08',0(R2)),SAGNNAMH                      
         GOTO1 TRNSAGT,DMCB,(X'80',8(R2)),NEWCODE                               
*                                                                               
VK40     OI    4(R2),X'20'                                                      
         CLC   NEWCODE,OLDCODE                                                  
         BE    INVERR              CANNOT HAVE SAME CODES                       
**NO-OP**BAL   RE,INIT                                                          
         CLI   OFFLINE,C'Y'        IF ON-LINE ONLY                              
         BE    XIT                                                              
         OI    TRNSTAT,OKINTPFK                                                 
         B     INFMSG              GIVE PFKEY MESSAGE                           
*                                                                               
VK50     CLI   PFAID,13                                                         
         BNE   INFMSG              CONTINUE GIVING MESSAGE UNTIL                
         B     XIT                 HIT PF13                                     
         EJECT                                                                  
*                                                                               
*        INITIALIZE                                                             
*                                                                               
INIT     NTR1                                                                   
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF      SCREEN                                    
         OI    TIQFLAGS,TIQFUPRC   READ RECORD FOR UPDATE                       
         MVC   TIFSSN,FILTSSN      SSN FILTER                                   
         MVI   TIREAD,TLCAACDQ                                                  
         MVC   TIFAGT,FILTAGT      AGENT FILTER                                 
         OC    FILTAGT,FILTAGT                                                  
         BNZ   INX                                                              
         MVI   TIREAD,TLCACCDQ                                                  
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CALL SYSIO                                                             
*                                                                               
LR       DS    0H                                                               
**NO-OP**MVI   WHEN,X'40'          SET AS IF 'NOW' REPORT                       
*        MVI   TWAWHEN,0                                                        
*        MVC   REMUSER(2),TGCTSTAF SET ID                                       
*        MVI   REMUSER+2,C' '                                                   
**NO-OP* GOTO1 OPENPQ              OPEN PQ MYSELF                               
*                                                                               
         LA    R0,GETCAST          SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(20,R1),=C'CAST RECORDS CHANGED'                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
**NO-OP**CLI   TIERROR,TINOTOLN    JOB TOO BIG TO DO AT ONCE                    
*        BNE   LR10                                                             
*        MVI   TIERROR,0           CLEAR ERROR                                  
*        MVC   P(38),=C'* WARNING * EXCEEDED MAXIMUM ALLOWABLE'                 
*        MVC   P+39(28),=C'I/OS - REPORT NOT COMPLETE *'                        
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*        MVI   SPMODE,X'FF'        CLOSE PQ MYSELF                              
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*        B     CONTMSG                                                          
*LR10    XC    CONSERV,CONSERV     AUTO $DQU                                    
*        MVC   CONSERV(4),=C'$DQU'                                              
*        MVI   SPMODE,X'FF'        CLOSE PQ MYSELF                              
**NO-OP* GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        GET ALL CAST RECORDS AND CHANGE AGENT CODE                             
*                                                                               
GETCAST  NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   GETX                                                             
         MVC   HEAD3+11(4),SAGCCOD                                              
         MVC   HEAD3+16(36),SAGCNAM                                             
         MVC   HEAD4+11(4),SAGNCOD                                              
         MVC   HEAD4+16(36),SAGNNAM                                             
*                                                                               
         L     R4,TIAREC           GET CAST DETAILS ELEMENT                     
         ST    R4,AIO              SET AIO TO POINT TO RECORD TOO               
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      FROM CAST RECORD                             
         BAS   RE,GETEL                                                         
         BE    *+6                 WHICH MUST EXIST                             
         DC    H'0'                                                             
*                                                                               
         CLC   TACANCDE,OLDCODE    IF THIS IS THE AGENT NUMBER WE WANT          
         BNE   GETX                   TO CHANGE                                 
         MVC   TACANCDE,NEWCODE                                                 
         GOTO1 ACTVIN,DMCB,0                                                    
         MVC   DMDSKADD,TIDSKADD                                                
         GOTO1 ADDPTRS,DMCB,PTRBLK CHANGE POINTERS                              
         BAS   RE,PRNT             PRINT OUT A REPORT                           
         MVC   KEY,TIKEY           (ADDPTRS DOES HIGH'S)                        
         OI    DMINBTS,X'08'       READ POINTER JUST DELETED                    
         GOTO1 HIGH                RESTORE READ SEQUENCE                        
         CLI   TWAWRITE,C'N'       IF WRITING TO FILE                           
         BE    *+8                                                              
         MVI   TIMODE,PROCPTRC     ASK SYSIO TO PUT RECORD                      
*                                                                               
GETX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        PRINT OUT REPORT                                                       
*                                                                               
PRNT     NTR1                                                                   
         USING LISTD,R2                                                         
         LA    R2,P                                                             
         MVC   LISDET,SPACES                                                    
         L     R4,TIAREC                                                        
         USING TLCAD,R4                                                         
         MVC   LISSSN,TLCASSN      SS NUMBER                                    
         MVC   LISCAT,TLCACAT      CATEGORY                                     
*                                  GET OTHER INFO                               
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      FROM CAST RECORD                             
         BAS   RE,GETEL                                                         
         BE    *+6                 WHICH MUST EXIST                             
         DC    H'0'                                                             
*                                                                               
         MVC   LISCAM,TACAONOF     CAMERA                                       
         MVC   LISUNI,TACAUN       UNION                                        
         MVC   LISLCL,TACALOCL     LOCAL                                        
         MVC   LISYR,TACAYEAR      CONTRACT YEAR                                
*                                                                               
         USING TLCAD,R4                                                         
         L     R4,TIAREC                                                        
         MVC   AIO,AIO2                                                         
         MVC   TGCOM,TLCACOM       MOVE INTO GLOBAL FOR XNAME                   
         GOTO1 XNAME,DMCB,(X'80',TLCOCCDQ),LISCID,TIKEY                         
*                                                                               
         L     R1,AIO                                                           
         USING TLCOD,R1                                                         
         MVC   LISAGY,TLCOAGY      AGENCY                                       
         MVC   AIO,TIAREC          RESTORE AIO                                  
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    COUNTER,=P'1'       COUNT RECORDS                                
         B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
MISSERR  MVI   ERROR,MISSING       MISSING INPUT                                
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT                                
         B     ERRXIT                                                           
*                                                                               
INFMSG   MVI   MYMSGNO1,35         PRESS PF13 TO MAKE CHANGES                   
         LA    R2,CONRECH                                                       
         OI    GENSTAT2,USGETTXT                                                
         B     ERRXIT                                                           
*                                                                               
CONTMSG  MVI   ERROR,EREXCIO       EXCEEDED IOS HIT ENTER TO CONTINUE           
         LA    R2,CONRECH                                                       
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
*              CONSTANTS, ETC.                                                  
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,33,C'CAGENT NEW'                                              
         SSPEC H2,33,C'----------'                                              
         SPACE 1                                                                
         SSPEC H3,1,C'FROM AGENT'                                               
         SSPEC H4,1,C'TO AGENT'                                                 
         SPACE 1                                                                
         SSPEC H6,1,C'S/S NUM'                                                  
         SSPEC H6,12,C'AGENCY'                                                  
         SSPEC H6,20,C'COMM ID'                                                 
         SSPEC H6,34,C'CAT'                                                     
         SSPEC H6,39,C'CAM'                                                     
         SSPEC H6,44,C'UNI'                                                     
         SSPEC H6,49,C'LCL'                                                     
         SSPEC H6,54,C'YR'                                                      
         SPACE 1                                                                
         SSPEC H7,1,C'-------'                                                  
         SSPEC H7,12,C'------'                                                  
         SSPEC H7,20,C'-------'                                                 
         SSPEC H7,34,C'---'                                                     
         SSPEC H7,39,C'---'                                                     
         SSPEC H7,44,C'---'                                                     
         SSPEC H7,49,C'---'                                                     
         SSPEC H7,54,C'--'                                                      
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
*              DSECT FOR SCREEN AND PRINT LINE                                  
*                                                                               
LISTD    DSECT                                                                  
LISDET   DS    0CL80                                                            
LISSSN   DS    CL9                                                              
         DS    CL2                                                              
LISAGY   DS    CL6                                                              
         DS    CL2                                                              
LISCID   DS    CL12                                                             
         DS    CL2                                                              
LISCAT   DS    CL3                                                              
         DS    CL2                                                              
LISCAM   DS    CL3                                                              
         DS    CL2                                                              
LISUNI   DS    CL3                                                              
         DS    CL2                                                              
LISLCL   DS    CL3                                                              
         DS    CL2                                                              
LISYR    DS    CL2                                                              
         SPACE                                                                  
       ++INCLUDE TAGENFFD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR2FD                                                       
         SPACE 4                                                                
*                                                                               
         ORG   SAGWORK                                                          
*                                                                               
OLDCODE  DS    CL2                 OLD AGENT CODE - HEX                         
NEWCODE  DS    CL2                 NEW AGENT CODE - HEX                         
COUNTER  DS    PL4                 COUNT OF RECORDS                             
FILTSSN  DS    CL9                 SSN FILTER                                   
FILTAGT  DS    CL4                 AGENT FILTER                                 
PIDVALID DS    C                   VALID PID?                                   
*                                                                               
PTRBLK   DS    CL((20*L'TLDRREC)+1)                                             
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         SPACE 2                                                                
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073TAGEN2F   11/10/10'                                      
         END                                                                    
