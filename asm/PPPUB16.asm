*          DATA SET PPPUB16    AT LEVEL 057 AS OF 05/13/02                      
*PHASE T40616A                                                                  
*INCLUDE SRCHCALL                                                               
         TITLE 'T40616  PUBFILE MAINTENANCE ADVERTISER SCREEN'                  
*                                                                               
*    CHANGE LOG                                                                 
*                                                                               
* SMYE 05/02    PUBVAL AND PUBEDIT CORE-RESIDENT                                
*                                                                               
* BPLA  1/97    IF GETAOR HAS ERROR DO BETTER MESSAGE                           
*                                                                               
* SMYE  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                    
*               ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                  
*                                                                               
* BPLA 4/12/95  BUG FIXED IN PUB ALREADY ASSIGNED MESSAGE                       
*                                                                               
* BPLA 8/11/93  BUG FIXED - IF PUB NOT CHANGED GO TO DONE                       
*                           NOT EDITX                                           
*                                                                               
* BPLA 8/10/93  ACCEPT BPUNLINK TO "UNLINK" A PUB                               
*                                                                               
T40616   CSECT                                                                  
         NMOD1 0,T40616,RR=R9                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         LA    R8,T40616+4095     R8 = SECOND BASE REGISTER                     
         LA    R8,1(R8)                                                         
         USING T40616+4096,R8                                                   
*                                                                               
         USING T406FFD,RA                                                       
         LA    R9,PUBIO                                                         
         USING PUBREC,R9                                                        
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
*                                                                               
CKREPEL  LA    R7,PUBREC+33                                                     
         USING PUBADVEL,R7                                                      
*                                                                               
         MVI   ADVIND,0                                                         
         XC    OLDPUB,OLDPUB                                                    
CKREP1   CLI   0(R7),X'80'                                                      
         BNE   NEXTEL                                                           
         CLC   BADV(3),2(R7)    ADV IS 3 BYTES                                  
         BNE   NEXTEL                                                           
         CLC   BAOFR(2),5(R7)   AOR IS 2 BYTES                                  
         BNE   NEXTEL                                                           
         OI    ADVIND,X'01'  SET ADV ELEM FOUND                                 
         MVC   OLDPUB,PUBADVPC                                                  
         B     CKIND                                                            
*                                                                               
NEXTEL   CLI   0(R7),0                                                          
         BE    CKIND        END OF RECORD                                       
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     CKREP1                                                           
*                                                                               
*                                                                               
CKIND    DS    0H                                                               
         XC    ELEAREA,ELEAREA                                                  
         CLI   BACT,1              ADD                                          
         BNE   ADVSCRN                                                          
         TM    ADVIND,X'01'                                                     
         BZ    ADVSCRN                                                          
         LA    R3,COMBERR                                                       
         LA    R2,PBLACTH                                                       
         B     ERROR                                                            
*                                                                               
ADVSCRN  CLI   BYTE2,1                                                          
         BE    FORMATP                                                          
         XC    ADVAPUN,ADVAPUN                                                  
         FOUT  ADVAPUNH                                                         
*                                  ADV SCREEN IN TWA SO EDIT IT                 
*                                  UNLESS ACTION=DISPLAY                        
         CLI   BACT,2                                                           
         BH    FORMATP                                                          
EDIT     DS    0H                                                               
*                                                                               
CKVEN    DS    0H                                                               
         XC    ADVAPUN,ADVAPUN                                                  
         FOUT  ADVAPUNH                                                         
         TM    ADVIND,X'01'                                                     
         BZ    CKVEN5                                                           
*                                  DELETE OLD ELEMENT                           
         GOTO1 VRECUP,DMCB,(1,PUBREC),PADVELD,0                                 
*                                                                               
CKVEN5   DS    0H                                                               
         LA    R2,ADVAPUBH                                                      
         LA    R7,ELEAREA         BUILD NEW ELEMENT                             
*                                                                               
*        INPUT MUST BE A PUB ON ADV FILE                                        
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         LA    R3,1          MISSING FIELD                                      
         B     ERROR                                                            
*                                                                               
         CLC   8(8,R2),=C'BPUNLINK'     SECRET WORD                             
         BE    CKVEN20A                 SKIP AOR LOGIC                          
*                                                                               
*                                                                               
*       PUB NAME SEARCHING ON ADVERTISER PUBFILE                                
*       MUST SWITCH TO CONTROL TO FIND AOR SYSTEM                               
*                                                                               
         BAS   RE,GETAOR                                                        
         CLI   AORSYS,0                                                         
         BNE   CKVEN5X                                                          
         MVC   PBLMSG,=CL60'*** AOR SYSTEM INVALID ***'                         
         B     CKAORERR                                                         
*                                                                               
CKVEN5X  DS    0H                                                               
*                                                                               
*       NOW SWITCH TO AOR                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),AORSYS                                                   
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    CKVEN0                                                           
         MVC   PBLMSG,=CL60'*** AOR SYSTEM NOT ACTIVE ***'                      
CKAORERR LA    R2,PBLMEDH                                                       
         NI    PBLMEDH+4,X'DF'     UNVALIDATE MEDIA                             
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
*                                                                               
CKVEN0   DS    0H                                                               
         ST    R2,FULL                                                          
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         MVC   DSPARM(DSPARML),SPACES                                           
         MVC   DSMEDCOD,BMED                                                    
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
         L     R2,FULL        RESTORE R2                                        
*                                                                               
*              NOTE USE OF WORK                                                 
*                                                                               
*NOP*    GOTO1 =V(PUBVAL),DMCB,(5(R2),ADVAPUB),(0,WORK),RR=RELO                 
         GOTO1 VPUBVAL,DMCB,(5(R2),ADVAPUB),(0,WORK)                            
         CLI   DMCB,X'FF'                                                       
         BNE   CKVEN2                                                           
CKVENERR DS    0H                                                               
*                                                                               
*        MUST RETURN TO MY SYSTEM                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,PUBERR                                                        
         B     ERROR                                                            
*                                                                               
CKVEN2   DS    0H                                                               
         CLC   OLDPUB(6),WORK     SEE IF PUB CHANGED                            
         BNE   CKVEN3                                                           
*                                NO - JUST EXIT                                 
*        MUST RETURN TO MY SYSTEM                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DONE                                                             
*                                                                               
*                                                                               
CKVEN3   MVC   SAVEKEY,KEY         SAVE KEY AND DMWORK                          
         MVC   DMWORK1(96),DMWORK                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),BMED                                                      
         MVC   KEY+1(6),WORK                                                    
         MVC   KEY+7(2),BAOFR      AGY OF REC                                   
         MVI   KEY+9,X'81'                                                      
         BAS   RE,READPUB                                                       
*                                                                               
         ST    R9,SAVERE                                                        
         LA    R9,PUBIO2                                                        
         BAS   RE,GETPUB                                                        
         L     R9,SAVERE           RESTORE R9                                   
*                                                                               
*        MUST RETURN TO MY SYSTEM                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
*                                                                               
*                                  DISPLAY ADV PUB NAME                         
         FOUT  ADVAPUNH,PUBIO2+35,20                                            
*                                                                               
*        NOW SEE IF THERE ALREADY IS A PASSIVE POINTER FOR                      
*        THIS ADV PUB                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBVDS,R4                                                        
         MVI   PUBVID,X'FD'                                                     
         MVC   PUBVMED,BMED                                                     
         MVC   PUBVAGY,AGYALPHA                                                 
         MVC   PUBVADV,BADV                                                     
         MVC   PUBVAOR,BAOFR                                                    
         MVC   PUBVVPUB,WORK                                                    
         GOTO1 HIGHPUB                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CKVEN8             NO ASSIGNMENT                                 
         CLC   PUBVAPUB,PUBIO+1   SEE IF ASSIGN TO THIS PUB                     
         BE    CKVEN8                                                           
         DROP  R4                                                               
*                                ADV PUB ALREADY ASSIGNED                       
         XC    ADVAPUN,ADVAPUN                                                  
         MVC   ADVAPUN(30),=CL30'* LINKED TO                  *'                
*NOP*    GOTO1 =V(PUBEDIT),DMCB,(C'0',KEY+15),(0,ADVAPUN+13),RR=RELO            
         GOTO1 VPUBEDIT,DMCB,(C'0',KEY+15),(0,ADVAPUN+13)                       
         LA    R3,PUBERR                                                        
         B     ERROR                                                            
*                                                                               
CKVEN8   MVC   KEY,SAVEKEY         RESTORE KEY AND DMWORK                       
         MVC   DMWORK(96),DMWORK1                                               
         XC    PUBADVPC,PUBADVPC                                                
         MVC   PUBADVPC(6),WORK        WORK HAS NEW ASSGN                       
         B     CKVEN20E                                                         
*                                                                               
CKVENER1 LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
CKVEN20A CLI   5(R2),8                                                          
         BNE   CKVEN20B                                                         
         CLC   ADVAPUB(8),=C'BPUNLINK'                                          
         BNE   CKVEN20B                                                         
         XC    PUBADVPC,PUBADVPC                                                
         MVI   PUBADVPC,X'FF'                                                   
         B     CKVEN20E                                                         
*                                                                               
CKVEN20B MVC   PUBADVPC,ADVAPUB                                                 
         OC    PUBADVPC,PUBADVPC                                                
         BZ    CKVEN20X                                                         
         CLI   PUBADVPC,X'99'                                                   
         BL    CKVENER1                                                         
*                                                                               
CKVEN20E DS    0H                                                               
*                                                                               
CKVEN20X B     UPDATER                                                          
*                                                                               
*                                                                               
UPDATER  DS    0H                                                               
         CLI   PUBADVPC,X'FF'                                                   
         BE    UPDATE10                                                         
*                                                                               
UPDATE2  MVC   PUBADVEL(2),=X'800F'                                             
         MVC   PUBADVCD(3),BADV                                                 
         MVC   PUBAORCD(2),BAOFR                                                
         LA    R5,PUBREC+33  SET R5 TO WHERE  I WANT TO ADD ELEM                
         CR    R5,R4              WILL BE EQUAL IF REC HAD NO ELEMS             
         BE    UPDATE4        GO ADD ELEM                                       
*                                                                               
UPDATE3  CLI   0(R5),0                                                          
         BE    UPDATE4                                                          
         CLI   0(R5),X'80'                                                      
         BNE   NEXTR1                                                           
         CLC   2(3,R5),BADV                                                     
         BH    UPDATE4                                                          
*                                                                               
NEXTR1   SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     UPDATE3                                                          
*                                                                               
UPDATE4  GOTO1 VRECUP,DMCB,(1,PUBREC),PADVELD,0(R5)                             
*                                                                               
*                                                                               
UPDATE10 DS    0H                                                               
         OC    OLDPUB,OLDPUB                                                    
         BZ    UPDATE20                                                         
*        NOW DELETE OLDPUB'S PASSIVE POINTERS                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBVDS,R4                                                        
         MVI   PUBVID,X'FD'                                                     
         MVC   PUBVMED,BMED                                                     
         MVC   PUBVAGY,AGYALPHA                                                 
         MVC   PUBVADV,BADV                                                     
         MVC   PUBVAOR,BAOFR                                                    
         MVC   PUBVVPUB,OLDPUB                                                  
         MVC   PUBVAPUB,PUBIO+1                                                 
         GOTO1 HIGHPUB                                                          
         CLC   KEY(21),KEYSAVE                                                  
         BE    UPDATE15                                                         
         DC    H'0',C'&&ABEND'        MISSING ASSIGNMENT                        
         DROP  R4                                                               
UPDATE15 OI    KEY+25,X'FF'                                                     
         BAS   RE,WRITEPUB                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBADS,R4                                                        
         MVI   PUBAID,X'FE'                                                     
         MVC   PUBAMED,BMED                                                     
         MVC   PUBAAGY,AGYALPHA                                                 
         MVC   PUBAADV,BADV                                                     
         MVC   PUBAAOR,BAOFR                                                    
         MVC   PUBAAPUB,PUBIO+1                                                 
         MVC   PUBAVPUB,OLDPUB                                                  
         GOTO1 HIGHPUB                                                          
         CLC   KEY(21),KEYSAVE                                                  
         BE    UPDATE18                                                         
         DC    H'0',C'&&ABEND'        MISSING ASSIGNMENT                        
UPDATE18 OI    KEY+25,X'FF'                                                     
         BAS   RE,WRITEPUB                                                      
         DROP  R4                                                               
*                                                                               
*                                 NOW I MUST DELETE EDITION POINTERS            
*                                 FOR OLD PUB                                   
         LA    R5,PUBEDTS                                                       
         LA    R6,15              FOR BCT                                       
UPDATE19 DS    0H                                                               
         CLI   0(R5),C' '                                                       
         BNH   UPDATE20                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBVDS,R4                                                        
         MVI   PUBVID,X'FD'                                                     
         MVC   PUBVMED,BMED                                                     
         MVC   PUBVAGY,AGYALPHA                                                 
         MVC   PUBVADV,BADV                                                     
         MVC   PUBVAOR,BAOFR                                                    
         MVC   PUBVVPUB,OLDPUB                                                  
         MVC   PUBVVPUB+5(1),0(R5)     SET EDITION CODE                         
         MVC   PUBVAPUB,PUBIO+1                                                 
         MVC   PUBVAPUB+5(1),0(R5)     SET EDITION CODE                         
         GOTO1 HIGHPUB                                                          
         CLC   KEY(21),KEYSAVE                                                  
         BE    UPD19D                                                           
         DC    H'0',C'&&ABEND'        MISSING ASSIGNMENT                        
         DROP  R4                                                               
UPD19D   OI    KEY+25,X'FF'                                                     
         BAS   RE,WRITEPUB                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBADS,R4                                                        
         MVI   PUBAID,X'FE'                                                     
         MVC   PUBAMED,BMED                                                     
         MVC   PUBAAGY,AGYALPHA                                                 
         MVC   PUBAADV,BADV                                                     
         MVC   PUBAAOR,BAOFR                                                    
         MVC   PUBAAPUB,PUBIO+1                                                 
         MVC   PUBAAPUB+5(1),0(R5)        SET EDITION CODE                      
         MVC   PUBAVPUB,OLDPUB                                                  
         MVC   PUBAVPUB+5(1),0(R5)        SET EDITION CODE                      
         GOTO1 HIGHPUB                                                          
         CLC   KEY(21),KEYSAVE                                                  
         BE    UPD19G                                                           
         DC    H'0',C'&&ABEND'        MISSING ASSIGNMENT                        
UPD19G   OI    KEY+25,X'FF'                                                     
         BAS   RE,WRITEPUB                                                      
         LA    R5,1(R5)                                                         
         BCT   R6,UPDATE19                                                      
*                                                                               
         DROP  R4                                                               
*                                                                               
UPDATE20 DS    0H                      ADD NEW PASSIVE POINTERS                 
         CLC   ADVAPUB(8),=C'BPUNLINK'  SEE IF JUST UNLINKING                   
         BE    EDITX                   SKIP TO WRITING BACK PUB                 
*                                                                               
         OI    DMINBTS,X'08'           SET TO PASS DELETES                      
*                                      SO I CAN RE-USE OLD POINTERS             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBVDS,R4                                                        
         MVI   PUBVID,X'FD'                                                     
         MVC   PUBVMED,BMED                                                     
         MVC   PUBVAGY,AGYALPHA                                                 
         MVC   PUBVADV,BADV                                                     
         MVC   PUBVAOR,BAOFR                                                    
         MVC   PUBVVPUB,PUBADVPC                                                
         MVC   PUBVAPUB,PUBIO+1                                                 
         GOTO1 HIGHPUB                                                          
         CLC   KEY(21),KEYSAVE                                                  
         BNE   UPDATE25                                                         
         MVI   KEY+25,X'01'                                                     
         BAS   RE,WRITEPUB                                                      
         B     UPD25X                                                           
*                                                                               
UPDATE25 DS    0H                                                               
         MVC   KEY,KEYSAVE             RESTORE KEY                              
         OI    KEY+25,X'01'           PASSIVE INDICATOR                         
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,ADDPUBD             ADD PASSIVE POINTER                       
         DROP  R4                                                               
*                                                                               
UPD25X   DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBADS,R4                                                        
         MVI   PUBAID,X'FE'                                                     
         MVC   PUBAMED,BMED                                                     
         MVC   PUBAAGY,AGYALPHA                                                 
         MVC   PUBAADV,BADV                                                     
         MVC   PUBAAOR,BAOFR                                                    
         MVC   PUBAAPUB,PUBIO+1                                                 
         MVC   PUBAVPUB,PUBADVPC                                                
         GOTO1 HIGHPUB                                                          
         CLC   KEY(21),KEYSAVE                                                  
         BNE   UPDATE30                                                         
         MVI   KEY+25,X'01'           RE-USE                                    
         BAS   RE,WRITEPUB                                                      
         B     UPD30X                                                           
*                                                                               
UPDATE30 DS    0H                                                               
         MVC   KEY,KEYSAVE             RESTORE KEY                              
         OI    KEY+25,X'01'           PASSIVE INDICATOR                         
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,ADDPUBD             ADD PASSIVE POINTER                       
*                                                                               
         DROP  R4                     FOR THIS PUB                              
*                                                                               
*                                     NOW ADD PASSIVE POINTER FOR               
*                                     EACH EDITION IN PUBEDTS                   
UPD30X   DS    0H                                                               
         LA    R5,PUBEDTS                                                       
         LA    R6,15                  FOR BCT                                   
UPDATE40 DS    0H                                                               
         CLI   0(R5),C' '                                                       
         BNH   UPDATE60                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBVDS,R4                                                        
         MVI   PUBVID,X'FD'                                                     
         MVC   PUBVMED,BMED                                                     
         MVC   PUBVAGY,AGYALPHA                                                 
         MVC   PUBVADV,BADV                                                     
         MVC   PUBVAOR,BAOFR                                                    
         MVC   PUBVVPUB,PUBADVPC                                                
         MVC   PUBVVPUB+5(1),0(R5)      SET EDITION CODE                        
         MVC   PUBVAPUB,PUBIO+1                                                 
         MVC   PUBVAPUB+5(1),0(R5)    SET EDITION CODE                          
         GOTO1 HIGHPUB                                                          
         CLC   KEY(21),KEYSAVE                                                  
         BNE   UPD40D                                                           
         MVI   KEY+25,X'01'           RE-USE                                    
         BAS   RE,WRITEPUB                                                      
         B     UPD40DX                                                          
*                                                                               
UPD40D   DS    0H                                                               
         MVC   KEY,KEYSAVE             RESTORE KEY                              
         OI    KEY+25,X'01'           PASSIVE INDICATOR                         
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,ADDPUBD             ADD PASSIVE POINTER                       
         DROP  R4                                                               
*                                                                               
UPD40DX  DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBADS,R4                                                        
         MVI   PUBAID,X'FE'                                                     
         MVC   PUBAMED,BMED                                                     
         MVC   PUBAAGY,AGYALPHA                                                 
         MVC   PUBAADV,BADV                                                     
         MVC   PUBAAOR,BAOFR                                                    
         MVC   PUBAAPUB,PUBIO+1                                                 
         MVC   PUBAAPUB+5(1),0(R5)     SET EDITION CODE                         
         MVC   PUBAVPUB,PUBADVPC                                                
         MVC   PUBAVPUB+5(1),0(R5)                                              
         GOTO1 HIGHPUB                                                          
         CLC   KEY(21),KEYSAVE                                                  
         BNE   UPD40F                                                           
         MVI   KEY+25,X'01'           RE-USE                                    
         BAS   RE,WRITEPUB                                                      
         B     UPD40X                                                           
*                                                                               
UPD40F   DS    0H                                                               
         MVC   KEY,KEYSAVE             RESTORE KEY                              
         OI    KEY+25,X'01'           PASSIVE INDICATOR                         
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,ADDPUBD             ADD PASSIVE POINTER                       
*                                                                               
UPD40X   DS    0H                                                               
         LA    R5,1(R5)               NEXT EDITION                              
         BCT   R6,UPDATE40                                                      
         DROP  R4                                                               
*                                                                               
UPDATE60 DS    0H                                                               
         MVI   DMINBTS,X'C0'          RESET DMINBTS                             
*                                                                               
EDITX    DS    0H                                                               
*                                                                               
*                                                                               
WRITEIT  OC    PUBADDR,PUBADDR                                                  
         BZ    ADDIT                                                            
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,PUTPUB                                                        
         B     DONE                                                             
*                                                                               
ADDIT    DS    0H                                                               
         B     DONE                                                             
         EJECT                                                                  
*                                                                               
FORMATP  DS    0H                                                               
         CLI   SAVSCRN,X'16'                                                    
         BNE   FMT2                                                             
         CLI   BACT,1                                                           
         BNE   FMT5                                                             
         MVI   BYTE2,0             SWITCH TO EDIT MODE                          
         B     EDIT                                                             
*                                                                               
FMT2     LA    R6,PBLLAST                                                       
         GOTO1 VCALLOV,WORK,(R6),X'D90406E6'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRGERR                                                          
         MVI   SAVSCRN,X'16'                                                    
*                                                                               
FMT5     DS    0H                                                               
         XC    ADVAPUB,ADVAPUB                                                  
         XC    ADVAPUN,ADVAPUN                                                  
         FOUT  ADVAPUBH                                                         
         FOUT  ADVAPUNH                                                         
         TM    ADVIND,X'01'                                                     
         BNZ   PUTFLDS                                                          
         LA    R2,ADVAPUBH                                                      
         B     EXIT                                                             
*                                                                               
PUTFLDS  DS    0H                                                               
         LA    R7,PUBREC+33                                                     
PUTFLD1  CLI   0(R7),X'80'                                                      
         BNE   NEXT1                                                            
         CLC   2(3,R7),BADV                                                     
         BNE   NEXT1                                                            
         CLC   5(2,R7),BAOFR                                                    
         BNE   NEXT1                                                            
         B     PUTVEN                                                           
*                                                                               
PUTVEN   DS    0H                                                               
*              SHOULD BE PUB NUMBER TRY TO DISPLAY                              
*              AND READ PUB TO DISPLAY NAME                                     
*                                                                               
*       MUST SWITCH TO CONTROL TO FIND AOR SYSTEM                               
*                                                                               
         BAS   RE,GETAOR                                                        
         CLI   AORSYS,0                                                         
         BNE   PUTV5                                                            
         MVC   PBLMSG,=CL60'*** AOR SYSTEM INVALID ***'                         
         B     PAORERR                                                          
*                                                                               
PUTV5    DS    0H                                                               
*                                                                               
*                                                                               
*       NOW SWITCH TO AOR                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),AORSYS                                                   
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    PUTVEN0                                                          
         MVC   PBLMSG,=CL60'*** AOR SYSTEM NOT ACTIVE ***'                      
*                                                                               
PAORERR  LA    R2,PBLMEDH                                                       
         NI    PBLMEDH+4,X'DF'     UNVALIDATE MEDIA                             
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
*                                                                               
PUTVEN0  DS    0H                                                               
         MVC   SAVEKEY(32),KEY                                                  
         MVC   DMWORK1(96),DMWORK                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),BMED                                                      
         MVC   KEY+1(6),PUBADVPC                                                
         MVC   KEY+7(2),BAOFR      AGY OF REC                                   
         MVI   KEY+9,X'81'                                                      
         BAS   RE,HIGHPUB                                                       
         CLC   KEYSAVE(10),KEY                                                  
         BE    PUTVEN1                                                          
         FOUT  ADVAPUNH,=C'** AGY OF REC PUB NOT FOUND **',30                   
         CLI   PUBADVPC,X'99'         NOT A PUB NUMBER                          
         BH    PUTVEN4                                                          
         B     PUTVEN2                                                          
*                                                                               
*                                                                               
PUTVEN1  ST    R9,SAVERE                                                        
         LA    R9,PUBIO2                                                        
         BAS   RE,GETPUB                                                        
         L     R9,SAVERE                                                        
*                                                                               
         FOUT  ADVAPUNH,PUBIO2+35,20                                            
*                                                                               
PUTVEN2  DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         MVC   DMWORK(96),DMWORK1                                               
         IC    R5,APROF13                                                       
*NOP*    GOTO1 =V(PUBEDIT),DMCB,((R5),PUBADVPC),(0,ADVAPUB),RR=RELO             
         GOTO1 VPUBEDIT,DMCB,((R5),PUBADVPC),(0,ADVAPUB)                        
         FOUT  ADVAPUBH                                                         
         B     PUTVENX                                                          
*                                                                               
PUTVEN4  CLI   PUBADVPC,X'FF'                                                   
         BNE   PUTVEN5                                                          
         FOUT  ADVAPUBH,=C'NONE',4                                              
         B     PUTVENX                                                          
*                                                                               
PUTVEN5  FOUT  ADVAPUBH,PUBADVPC,12                                             
*                                                                               
*                                                                               
*        MUST RETURN TO MY SYSTEM                                               
*                                                                               
PUTVENX  DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
         CLI   BACT,2                                                           
         BH    DONE                                                             
         LA    R2,ADVAPUBH                                                      
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
GETAOR   NTR1                                                                   
*                                                                               
         MVI   AORSYS,0                                                         
*                                                                               
         MVC   SAVEKEY,KEY         SAVE KEY                                     
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'0A'          CONTROL SYSTEM GENDIR                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    GETAOR5                                                          
         MVC   PBLMSG,=CL60'*** CONTROL SYSTEM NOT ACTIVE ***'                  
         LA    R2,PBLMEDH                                                       
         NI    PBLMEDH+4,X'DF'     UNVALIDATE MEDIA                             
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
*                                                                               
GETAOR5  LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING ADVREC,R6                                                        
         XC    ADVKEY,ADVKEY                                                    
         MVI   ADVREC,ADVRECQ                                                   
         MVI   ADVTYP,ADVTYPQ                                                   
         MVI   ADVSYS,C'P'                                                      
         MVC   ADVMED,BMED                                                      
         MVC   ADVAOR,BAOFR                                                     
         MVC   ADVADV,BADV                                                      
         MVC   ADVAGY,BAOFR                                                     
         MVC   WORK(32),KEY                                                     
         GOTO1 VDATAMGR,DMCB,(0,=CL8'DMRDHI'),=CL8'GENDIR',KEY,KEY,    X        
               (TERMNAL,0)                                                      
         CLC   KEY(32),WORK                                                     
         BE    VALADV5                                                          
         B     GETAORX            NOT FOUND                                     
*                                                                               
         DROP  R6                                                               
VALADV5  DS    0H                                                               
*                                                                               
         LA    R6,PUBNAMEL                                                      
         USING PUBNAMEL,R6                                                      
         LA    R3,APPLWRK                                                       
         ST    R3,ADVIO                                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,=CL8'GETREC',=CL8'GENFIL',KEY+36,ADVIO,   X        
               (TERMNAL,DMWORK1)                                                
*                                                                               
         MVC   KEY,SAVEKEY                                                      
*                                                                               
         L     R6,ADVIO                                                         
         USING ADVKEYD,R6                                                       
         LA    R4,ADVFRST                                                       
*                                                                               
VALADV6  CLI   0(R4),X'10'                                                      
         BE    VALADV7                                                          
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         CLI   0(R4),0                                                          
         BE    GETAORX                                                          
         B     VALADV6                                                          
*                                                                               
VALADV7  MVC   DUB(1),AGYPRT+4-AGYD(R4) "N" OF PRNTN (N=1-6)                    
         LA    R4,PRINTSES                                                      
VALADV8  CLC   DUB(1),0(R4)                                                     
         BE    VALADV9                                                          
         CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R4,2(R4)                                                         
         B     VALADV8                                                          
*                                                                               
VALADV9  MVC   AORSYS,1(R4)                                                     
*                                                                               
*        MUST SWITCH BACK BEFORE SWITCHING TO AOR                               
*                                                                               
GETAORX  L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                                                                   
*                                                                               
       ++INCLUDE PRINTSES                                                       
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
*                                                                               
ADVIND   DS    CL1                                                              
ELCOD    DS    CL1                                                              
ASWITCH  DS    CL1                                                              
ESWITCH  DS    CL1                                                              
OLDPUB   DS    CL6                                                              
FLDINV   EQU   2                                                                
COMBERR  EQU   112                                                              
REPERR   EQU   122                                                              
PUBERR   EQU   18                                                               
CLTAUTH  EQU   33                                                               
*                                                                               
ELCNT    DC    H'0'                                                             
VIRGERR  DC    H'0'                                                             
SPACES   DC    40C' '                                                           
DMWORK1  DS    12D                                                              
SAVERE   DS    F                                                                
SAVEKEY  DS    CL32                                                             
         EJECT                                                                  
*                                                                               
         SPACE 3                                                                
NEXT1    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BE    PUTVEN                                                           
         B     PUTFLD1                                                          
         SPACE 3                                                                
       ++INCLUDE PUGENEROL                                                      
*                                                                               
         LTORG                                                                  
ADVIO    DS    A              ADDRESS OF APPLWRK                                
AORSYS   DS    CL1            AOR SYSTEM NUMBER                                 
*                                                                               
ELEAREA  DS    CL30                                                             
*                                                                               
PUBIO2   DS    4000C                                                            
*                                                                               
       ++INCLUDE PPPUBWRK                                                       
*                                                                               
         EJECT                                                                  
         ORG   PBLLAST                                                          
       ++INCLUDE PPPUBE6D                                                       
*                                                                               
*                                                                               
PADVELD  DSECT                                                                  
       ++INCLUDE PPPUBADVEL                                                     
*                                                                               
         SPACE 2                                                                
* ADVPUBPP                                                                      
PUBVDS   DSECT                                                                  
       ++INCLUDE PPADVPUBPP                                                     
         EJECT                                                                  
*                                                                               
* PUBADVPP                                                                      
PUBADS   DSECT                                                                  
       ++INCLUDE PPPUBADVPP                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSRCHPARM                                                     
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
       ++INCLUDE CTGENADVD                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057PPPUB16   05/13/02'                                      
         END                                                                    
