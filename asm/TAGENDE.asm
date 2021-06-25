*          DATA SET TAGENDE    AT LEVEL 020 AS OF 05/01/02                      
*PHASE T702DEA,*                                                                
         TITLE 'T702DE - ADVICE OFFICE CODE CHANGE'                             
T702DE   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702DE                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=A(PROGRAM SAVED STORAGE)                  
         USING LOCALD,R7                                                        
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         SPACE 2                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VK       DS    0H                                                               
*                                  IF AUTOMATIC TDO REQUESTS                    
*                                  MOVE OUT DATA FROM SPECIAL LONG FLD          
         MVI   SDVFOFFH+5,1                                                     
         MVC   SDVFOFF,SDVLONG     OLD OFFICE CODE                              
         MVI   SDVAGYH+5,6                                                      
         MVC   SDVAGY,SDVLONG+1    AGENCY CODE                                  
*                                                                               
         LA    R2,SDVFOFFH         R2=A(OLD OFFICE CODE FIELD)                  
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SDVAGYH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         GOTO1 RECVAL,DMCB,TLOFCDQ,(R2)                                         
         MVC   FROMOFF,TGOFF       SET FROM OFFICE CODE                         
*                                                                               
VK10     OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         LA    R2,SDVAGYH          R2=A(AGENCY FIELD)                           
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'0A',(R2)),SDVAYNMH                        
         MVC   FILTAGY,TGAGY                                                    
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   NEWOFF,TAAYTPOF     SET NEW OFFICE CODE FROM AGENCY              
*                                                                               
         MVC   SDVTOFF,NEWOFF      AND MOVE TO SCREEN AND TRANSMIT              
         OI    SDVTOFFH+6,X'80'                                                 
         NI    SDVAYOFH+1,X'F3'    SET AGY OFF LITERAL TO NORMAL/XMIT           
         OI    SDVAYOFH+6,X'80'                                                 
*                                                                               
         CLC   NEWOFF,FROMOFF      MAKE SURE OFFICE CODES NOT THE SAME          
         BE    INVOFF                                                           
*                                                                               
         OI    4(R2),X'20'                                                      
         CLI   OFFLINE,C'Y'        FOR ON-LINE ONLY                             
         BE    XIT                                                              
         OI    TRNSTAT,OKINTPFK                                                 
         B     INFMSG              GIVE PFKEY MESSAGE                           
*                                                                               
VK30     CLI   PFAID,13                                                         
         BNE   INFMSG              CONTINUE GIVING MESSAGE UNTIL                
         B     XIT                 HIT PF13                                     
         EJECT                                                                  
*              PROCESS REPORT                                                   
         SPACE 1                                                                
PREP     NTR1                                                                   
         ZAP   COUNTER,=P'0'                                                    
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         LA    R2,MYSPECS          SET UP SPECS & HOOK FOR REPORT               
         ST    R2,SPECS                                                         
         LA    R2,HDHOOK                                                        
         ST    R2,HEADHOOK                                                      
         MVI   FORCEHED,C'Y'                                                    
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF      SCREEN                                    
         MVC   TIACOMFC,ACOMFACS                                                
         OI    TIQFLAGS,TIQFUPRC   READ RECORD FOR UPDATE                       
         MVI   TIREAD,TLDVCDQ      ADVICE RECORDS                               
         MVC   TIFAGY,FILTAGY      AGENCY CODE                                  
         LA    R0,IOHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         MVI   SPACING,3                                                        
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(22,R1),=CL22'ADVICE RECORDS CHANGED'                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              SYSIO HOOK                                                       
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   IOHKX                                                            
*                                                                               
         L     R4,TIAREC           GET CAST DETAILS ELEMENT                     
         MVI   ELCODE,TADVELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADVD,R4                                                         
*                                                                               
         CLC   TADVOFF,FROMOFF     IF MATCH ON OLD TP OFFICE CODE               
         BNE   IOHKX                                                            
*                                                                               
         MVC   AIO,TIAREC          SET AIO TO POINT TO RECORD TOO               
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         CLI   TWAWRITE,C'N'       IF NOT WRITING TO FILE                       
         BNE   IOHOOK3                                                          
         GOTO1 MYTRACE,DMCB,=C'ADVICE BEFORE',AIO,0                             
*                                                                               
IOHOOK3  MVC   TADVOFF,NEWOFF      SET NEW OFFICE CODE                          
         MVC   DMDSKADD,TIDSKADD                                                
         MVI   BYTE,0                                                           
*                                                                               
         CLI   TWAWRITE,C'N'       IF NOT WRITING TO FILE                       
         BNE   IOHOOK8                                                          
         GOTO1 MYTRACE,DMCB,=C'ADVICE AFTER',AIO,0                              
         MVI   BYTE,X'10'                                                       
*                                                                               
IOHOOK8  GOTO1 ADDPTRS,DMCB,(BYTE,PTRBLK) CHANGE POINTERS                       
         MVC   AIO,AIO1            RESET AIO                                    
         AP    COUNTER,=P'1'       COUNT RECORDS                                
**NO-OP**BAL   RE,PRNT             PRINT OUT A REPORT                           
*                                                                               
         MVC   KEY,TIKEY           RE-READ SYSIO'S KEY                          
         GOTO1 HIGH                RESTORE READ SEQUENCE                        
*                                                                               
         CLI   TWAWRITE,C'N'       IF WRITING TO FILE                           
         BE    *+8                                                              
         MVI   TIMODE,PROCPTRC     ASK SYSIO TO PUT RECORD                      
*                                                                               
IOHKX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              PRINT OUT REPORT                                                 
         SPACE 1                                                                
PRNT     NTR1                                                                   
         L     R4,TIAREC           R4=A(ADVICE RECORD)                          
         USING TLDVD,R4                                                         
         LA    R2,P                R2=A(PRINT LINE)                             
         USING PRTD,R2                                                          
         MVC   PRTCID,TLDVCID      COMMERCIAL ID                                
         MVC   PRTADV,TLDVADV      ADVICE NUMBER                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         DROP  R4,R2                                                            
         SPACE 2                                                                
*              TRACE ROUTINE                                                    
         SPACE 1                                                                
MYTRACE  NTR1                                                                   
         LM    R2,R3,0(R1)         R2=A(LITERAL), R3=A(I/O AREA)                
         ZIC   R4,0(R1)            R4=L'LITERAL                                 
         GOTO1 TRACE,DMCB,(R3),0,(R2),(R4)                                      
         B     XIT                                                              
         SPACE 2                                                                
*              HEAD HOOK ROUTINE                                                
         SPACE 1                                                                
HDHOOK   NTR1                                                                   
         MVC   HEAD4+12(L'FROMOFF),FROMOFF                                      
         OC    SDVAYNM,SPACES                                                   
         MVC   HEAD5+12(36),SDVAYNM                                             
         MVC   HEAD6+12(L'NEWOFF),NEWOFF                                        
         B     XIT                                                              
         EJECT                                                                  
*              ERROR EXISTS, ETC..                                              
         SPACE 1                                                                
INVOFF   MVC   MYMSGNO,=Y(EROFFCD) AGENCY OFFICE CANNOT BE SAME AS              
         B     NERRXIT             OLD OFFICE                                   
*                                                                               
INFMSG   MVI   MYMSGNO1,35         PRESS PF13 TO MAKE CHANGES                   
         LA    R2,CONRECH                                                       
         OI    GENSTAT2,USGETTXT                                                
         B     ERRXIT                                                           
*                                  HANDLE TWO BYTE ERROR MESSAGES               
NERRXIT  OI    GENSTAT2,USGETTXT   SET GETTXT BLOCK                             
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              PRINTING SPECS                                                   
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'ADVICE OFFICE CHANGE'                                    
         SSPEC H2,30,C'--------------------'                                    
         SPACE 1                                                                
         SSPEC H4,1,C'FROM OFFICE'                                              
         SSPEC H5,1,C'AGENCY'                                                   
         SSPEC H6,1,C'TO OFFICE'                                                
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER PROGRAM SAVED STORAGE                             
*                                                                               
LOCALD   DSECT                                                                  
FROMOFF  DS    CL1                 OLD OFFICE CODE                              
NEWOFF   DS    CL1                 NEW OFFICE CODE                              
FILTAGY  DS    CL6                 AGENCY FILTER                                
COUNTER  DS    PL4                 COUNT OF RECORDS                             
*                                                                               
PTRBLK   DS    CL((3*L'TLDRREC)+1)                                              
         SPACE 2                                                                
*              DSECT FOR PRINT LINE                                             
*                                                                               
PRTD     DSECT                                                                  
PRTCID   DS    CL12                                                             
         DS    CL2                                                              
PRTADV   DS    CL6                                                              
         SPACE                                                                  
       ++INCLUDE TAGENFFD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRDED                                                       
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
* TAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE TAGENEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020TAGENDE   05/01/02'                                      
         END                                                                    
