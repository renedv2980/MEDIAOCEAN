*          DATA SET TAGENBE    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T702BEA                                                                  
         TITLE 'T702BE - SECURITY LIST'                                         
T702BE   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702BE                                                         
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
SEC10    GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
SEC20    CLI   MODE,LISTRECS                                                    
         BNE   SEC30                                                            
         OI    GLSTSTAT,RETEXTRA   SET OK TO RETURN EXTRA FOR EOL               
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
SEC30    CLI   MODE,PRINTREP                                                    
         BNE   SECX                                                             
         ZAP   COUNTER,=P'0'                                                    
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
SECX     B     XIT                                                              
*                                                                               
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       LA    R2,SECPROGH         REQUEST A SPECIFIC PROGRAM                   
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SECRECH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVI   FILTPROG,02                                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'GEN'                                                  
         BNE   VK05                                                             
         MVC   8(3,R2),=C'GEN'                                                  
         OI    6(R2),X'80'         TRANSMIT                                     
         B     VK10                                                             
*                                                                               
VK05     MVI   FILTPROG,03                                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'REP'                                                  
         BNE   INVERR                                                           
         MVC   8(3,R2),=C'REP'                                                  
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
VK10     OI    4(R2),X'20'                                                      
         LA    R2,SECRECH          REQUEST A SPECIFIC RECORD                    
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         NI    SECACTH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    FILTREC,FILTREC                                                  
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         ZIC   R1,5(R2)                                                         
         CH    R1,=H'3'            ALLOW TO FILTER ON 'ALL'                     
         BNE   VK15                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK15                                                             
         MVC   FILTREC(3),=C'ALL'                                               
         B     VK20                                                             
*                                                                               
VK15     BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FILTREC(0),8(R2)       USE AS START FIELD                        
         OC    FILTREC,SPACES                                                   
*                                                                               
VK20     OI    4(R2),X'20'                                                      
         LA    R2,SECACTH          FILTER ON ACTION                             
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         NI    SECCATH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    FILTACT,FILTACT                                                  
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         ZIC   R1,5(R2)                                                         
         CH    R1,=H'3'            ALLOW TO FILTER ON 'ALL'                     
         BNE   VK25                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK25                                                             
         MVC   FILTACT(3),=C'ALL'                                               
         B     VK30                                                             
*                                                                               
VK25     STC   R1,FILTALN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FILTACT(0),8(R2)                                                 
         OC    FILTACT,SPACES                                                   
*                                                                               
VK30     OI    4(R2),X'20'                                                      
         LA    R2,SECCATH          FILTER ON CATEGORY                           
         TM    4(R2),X'20'                                                      
         BO    VK40                                                             
         NI    SECOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         XC    FILTCAT,FILTCAT                                                  
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         BAS   RE,VALCAT                                                        
*                                                                               
VK40     OI    4(R2),X'20'                                                      
         LA    R2,SECOPTSH         OPTIONS                                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         CLI   5(R2),0                                                          
         BNE   INVERR                                                           
         OI    4(R2),X'20'         VALIDATED                                    
         XC    KEY,KEY                                                          
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE SECURITY CODES                                                
*                                                                               
VALCAT   NTR1                                                                   
         LA    R3,8(R2)                                                         
         ZIC   R0,5(R2)                                                         
         CH    R0,=H'3'                                                         
         BNE   VC10                                                             
         CLC   =C'ALL',8(R2)       FILTER ON ALL                                
         BNE   VC10                                                             
         MVC   FILTCAT,=C'ALL'                                                  
         B     VCX                                                              
*                                                                               
VC10     CLI   0(R3),C','          IF THIS IS A COMMA - IGNORE IT               
         BE    VC30                                                             
         GOTO1 STAFVAL,DMCB,0(R3),0                                             
         BE    VC20                                                             
         SR    R3,R2                                                            
         SH    R3,=H'8'                                                         
         B     ERRINV                                                           
*                                                                               
VC20     ZIC   R1,TGSTDSP          DISPLACEMENT TO SECURITY BYTE                
         LA    R6,FILTCAT(R1)      R6 = CORRECT BYTE                            
         OC    0(1,R6),TGSTBIT     OR IN BIT                                    
*                                                                               
VC30     LA    R3,1(R3)            BUMP R3 TO NEXT CATEGORY                     
         BCT   R0,VC10             REPEAT                                       
*                                                                               
VCX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
LR       LA    R4,KEY              R4=A(KEY)                                    
         USING TLSED,R4                                                         
         OC    TLSEKEY,TLSEKEY     IF FIRST TIME IN                             
         BNZ   LR05                                                             
         MVI   TLSECD,TLSECDQ      SET RECORD CODE                              
         MVC   TLSEPROG,FILTPROG   SET PROGRAM                                  
*                                                                               
         CLC   FILTREC,SPACES                                                   
         BNH   LR05                                                             
         CLC   =C'ALL',FILTREC                                                  
         BE    LR05                                                             
         MVC   TLSEREC,FILTREC     SET KEY                                      
*                                                                               
LR05     GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR10     GOTO1 SEQ                                                              
*                                                                               
LR20     CLC   TLSEKEY(TLSEPROG-TLSED+L'TLSEPROG),KEYSAVE                       
         BNE   LR30                                                             
         CLC   =C'ALL',FILTREC     IF FILTERING 'ALL' RECORDS                   
         BNE   LR21                                                             
         OC    TLSEREC,TLSEREC     AND PASSED THE LAST ONE                      
         BNZ   LR30                                                             
*                                                                               
LR21     CLC   FILTACT,SPACES      IF FILTERING ON ACTION                       
         BNH   LR25                                                             
         CLC   =C'ALL',FILTACT                                                  
         BNE   LR22                                                             
         OC    TLSEACT,TLSEACT                                                  
         BNZ   LR10                                                             
         B     LR25                                                             
*                                                                               
LR22     ZIC   R1,FILTALN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TLSEACT(0),FILTACT                                               
         BNE   LR10                                                             
*                                                                               
LR25     GOTO1 GETREC              GET THE RECORD                               
         BAS   RE,PRREC                                                         
         B     LR10                                                             
*                                                                               
LR30     CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(16,R1),=C'SECURITY RECORDS'                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS RECORD                                                         
*                                                                               
PRREC    NTR1                                                                   
         USING LISTD,R2                                                         
         L     R4,AIO                                                           
         USING TLSED,R4                                                         
         MVC   SECRREC(3),=C'ALL'                                               
         OC    TLSEREC,TLSEREC     RECORD NAME                                  
         BZ    *+10                                                             
         MVC   SECRREC,TLSEREC                                                  
*                                                                               
         MVC   SECRACT(3),=C'ALL'                                               
         OC    TLSEACT,TLSEACT     ACTION NAME                                  
         BZ    *+10                                                             
         MVC   SECRACT,TLSEACT                                                  
*                                                                               
         L     R4,AIO                                                           
         USING TASED,R4                                                         
         MVI   ELCODE,TASEELQ      POINT TO SECURITY ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   PRR10                                                            
         BAS   RE,FILCAT                                                        
         BNE   PRRX                                                             
         BAS   RE,DISCAT                                                        
*                                                                               
PRR10    MVI   LASTHEAD,25                                                      
         GOTO1 ACTVOUT,DMCB,LASTHEAD                                            
         MVC   SECRLCH,LASTCHG     LAST CHANGED INFO                            
         CLI   MODE,PRINTREP                                                    
         BNE   PRR20                                                            
         GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    COUNTER,=P'1'                                                    
         B     PRRX                                                             
*                                                                               
PRR20    GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRRX     MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         MVC   P,SPACES            CLEAR PREVIOUS LINE                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              FILTER ON SECURITY MASK                                          
*                                                                               
FILCAT   NTR1                                                                   
         OC    TASEMASK,TASEMASK   IF MASK = ALL                                
         BZ    FCYES               THEN EVERYONE IS ACCESSED                    
         MVC   FULL,TASEMASK                                                    
         NC    FULL,SECMASKS       TEST IF CURRENTLY ACCESSED                   
         BZ    FCNO                                                             
*                                                                               
         OC    FILTCAT,FILTCAT     IF ANY FILTERS                               
         BZ    FCYES                                                            
         CLC   =C'ALL',FILTCAT                                                  
         BNE   FC10                                                             
         OC    TASEMASK,TASEMASK                                                
         BNZ   FCNO                                                             
*                                                                               
FC10     MVC   FULL,TASEMASK                                                    
         NC    FULL,FILTCAT        TEST IF THIS FALLS UNDER FILTER              
         BZ    FCNO                                                             
         B     FCYES                                                            
*                                                                               
FCNO     B     NO                                                               
*                                                                               
FCYES    B     YES                                                              
         EJECT                                                                  
*                                                                               
*              DISPLAY SECURITY MASK                                            
*                                                                               
         USING TASED,R4                                                         
DISCAT   NTR1                                                                   
         LA    R2,SECRCAT                                                       
         LA    R3,TASEMASK                                                      
         OC    TASEMASK,TASEMASK                                                
         BNZ   DCAT10                                                           
         MVC   0(3,R2),=C'ALL'                                                  
         B     DCATX                                                            
*                                                                               
DCAT10   L     R4,TGASTAFS         LOOP THROUGH STAFF TYPE TABLE                
         USING STAFTABD,R4                                                      
*                                                                               
DCAT20   ZIC   R1,STAFDSP          DISPLACEMENT TO MASK BYTE                    
         LA    RF,0(R3,R1)                                                      
         IC    R1,STAFBIT          BIT VALUE                                    
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(RF),0             IF BIT IS ON                                 
         BZ    DCAT30                                                           
         MVC   0(1,R2),STAFEQU     DISPLAY EQUATE CHARACTER                     
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
*                                                                               
DCAT30   LA    R4,STAFNEXT                                                      
         CLI   0(R4),X'FF'         IF WE HAVEN'T REACHED END                    
         BNE   DCAT20              CONTINUE                                     
*                                                                               
         BCTR  R2,0                                                             
         CLI   0(R2),C','          CHECK IF WE NEED TO CLEAR                    
         BNE   *+8                                                              
         MVI   0(R2),C' '          TRAILING COMMA                               
*                                                                               
DCATX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              HEADLINE ROUTINE (HEADHOOK)                                      
*                                                                               
HOOK     NTR1                                                                   
         MVC   HEAD3+8(3),=C'GEN'                                               
         CLI   FILTPROG,2                                                       
         BE    HEADX                                                            
         MVC   HEAD3+8(3),=C'REP'                                               
*                                                                               
HEADX    B     XIT                                                              
         SPACE 2                                                                
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         STC   R3,ERRDISP                                                       
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
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'SECURITY LIST'                                           
         SSPEC H2,32,C'-------------'                                           
         SPACE 1                                                                
         SSPEC H3,1,C'PROGRAM'                                                  
         SPACE 1                                                                
         SSPEC H5,1,C'RECORD'                                                   
         SSPEC H5,10,C'ACTION'                                                  
         SSPEC H5,19,C'SECURITY'                                                
         SSPEC H5,58,C'LAST CHANGED'                                            
         SPACE 1                                                                
         SSPEC H6,1,C'-------'                                                  
         SSPEC H6,10,C'-------'                                                 
         SSPEC H6,19,C'---------'                                               
         SSPEC H6,58,C'------------'                                            
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
LISTD    DSECT                                                                  
SECRREC  DS    CL8                                                              
         DS    CL1                                                              
SECRACT  DS    CL8                                                              
         DS    CL1                                                              
SECRCAT  DS    CL36                                                             
         DS    CL3                                                              
SECRLCH  DS    CL17                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRBED                                                       
         EJECT                                                                  
*                                                                               
COUNTER  DS    PL4                 LINE COUNTER                                 
FILTPROG DS    CL1                 PROGRAM FILTER                               
FILTREC  DS    CL8                 RECORD FILTER                                
FILTACT  DS    CL8                 ACTION FILTER                                
FILTALN  DS    CL1                 L'ACTION FILTER                              
FILTCAT  DS    XL4                 CATEGORY FILTERS                             
LASTHEAD DS    CL8                 FAKE HEADER                                  
LASTCHG  DS    CL17                                                             
         EJECT                                                                  
         SPACE 3                                                                
*                                                                               
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
**PAN#1  DC    CL21'003TAGENBE   05/01/02'                                      
         END                                                                    
