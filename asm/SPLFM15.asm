*          DATA SET SPLFM15    AT LEVEL 054 AS OF 05/01/02                      
*PHASE T21915A                                                                  
         TITLE 'SPLFM15 - PRODUCT GROUP ASSIGNMENTS'                            
         PRINT NOGEN                                                            
T21915   CSECT                                                                  
         NMOD1 0,T21915                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING PRGRECD,R8                                                       
         EJECT                                                                  
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         XC    KEY,KEY             READ GRP DEF RECORD                          
         MVC   KEY(PRGKGRP-PRGKEY),SVKEY                                        
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
* DISPLAY BREAK NAME                                                            
         LA    R6,PRGEL                                                         
         USING PRGEL01,R6                                                       
         LA    R2,LFMBK1H                                                       
         FOUT  (R2),PRGBK1,12                                                   
*                                                                               
         LA    R2,LFMBK2H                                                       
         FOUT  (R2),PRGBK2,12                                                   
         DROP  R6                                                               
*                                                                               
         FOUT  LFMNM1H,SPACES,24                                                
*                                                                               
         FOUT  LFMNM2H,SPACES,24                                                
*                                                                               
         FOUT  LFMUSERH,SPACES,3                                                
*                                                                               
         FOUT  LFMADD1H,SPACES,30                                               
         FOUT  LFMADD2H,SPACES,30                                               
         FOUT  LFMADD3H,SPACES,30                                               
         FOUT  LFMADD4H,SPACES,30                                               
*                                                                               
         CLI   SVFMTSW,1           TEST EDIT                                    
         BE    *+12                YES                                          
         CLI   SVACT,C'A'          TEST FMT BEFORE ADD                          
         BE    FMT3                YES - SKIP READ                              
* READ RECORD                                                                   
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,PRGEL                                                         
         USING PRGEL10,R6                                                       
         LA    R2,LFMNM1H                                                       
         FOUT  (R2),PRGNAM1,24                                                  
         OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,LFMNM2H                                                       
         FOUT  (R2),PRGNAM2,24                                                  
         OI    4(R2),X'20'                                                      
*                                                                               
FMT1     ZIC   R1,1(R6)                                                         
         AR    R6,R1               POINT TO NEXT ELEMENT                        
         CLI   0(R6),0                                                          
         BE    FMT2                                                             
         CLI   0(R6),X'20'         ADDRESS ELEMENT                              
         BNE   FMT1                                                             
*                                                                               
         USING PRGEL20,R6                                                       
         LA    R2,LFMADD1H                                                      
         FOUT  (R2),PRGADDR1,24                                                 
         OI    4(R2),X'20'                                                      
         LA    R2,LFMADD2H                                                      
         FOUT  (R2),PRGADDR2,24                                                 
         OI    4(R2),X'20'                                                      
         LA    R2,LFMADD3H                                                      
         FOUT  (R2),PRGADDR3,24                                                 
         OI    4(R2),X'20'                                                      
         LA    R2,LFMADD4H                                                      
         FOUT  (R2),PRGADDR4,24                                                 
         OI    4(R2),X'20'                                                      
         DROP  R6                                                               
         EJECT                                                                  
FMT2     LA    R6,PRGEL                                                         
*                                                                               
FMT2A    ZIC   R1,1(R6)                                                         
         AR    R6,R1               POINT TO NEXT ELEMENT                        
         CLI   0(R6),0                                                          
         BE    FMT3                                                             
         CLI   0(R6),X'30'         USER FIELD ELEMENT                           
         BNE   FMT2A                                                            
*                                                                               
         USING PRGEL30,R6                                                       
         LA    R2,LFMUSERH                                                      
         FOUT  (R2),PRGUSER,3                                                   
         OI    4(R2),X'20'                                                      
*                                                                               
* CLEAR ADDED PRODUCT FIELDS                                                    
FMT3     LA    R2,LFMPRD1H         FIRST PRD                                    
         LA    R0,LFMPRDXH         LAST PRD                                     
*                                                                               
FMT3A    OC    8(3,R2),8(R2)                                                    
         BZ    FMT3B                                                            
         XC    8(3,R2),8(R2)                                                    
         FOUT  (R2)                                                             
FMT3B    SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         CR    R2,R0                                                            
         BNH   FMT3A                                                            
*                                                                               
* CLEAR DISPLAY LINES                                                           
*                                                                               
         LA    R4,LFMSEPAH                                                      
         LA    R2,LFMLST1H                                                      
         SR    R0,R0                                                            
FMT4A    OC    8(72,R2),8(R2)                                                   
         BZ    FMT4B                                                            
         XC    8(72,R2),8(R2)                                                   
         FOUT  (R2)                                                             
FMT4B    IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R2,R4                                                            
         BNE   FMT4A                                                            
***      CLI   0(R2),0                                                          
***      BNE   FMT4A                                                            
         EJECT                                                                  
* NOW DISPLAY PRDS IN GROUP                                                     
* SET TO READ PASSIVE POINTERS                                                  
         XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         MVC   PRGPTYP,=X'0D81'                                                 
         MVC   PRGPAGMD,SVAGYMD                                                 
         MVC   PRGPCLT,SVCLT                                                    
         LA    RE,SVKEY+PRGKID-PRGKEY                                           
         MVC   PRGPID(3),0(RE)                                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         LA    R2,LFMLST1H                                                      
         LA    R5,LFMSEPAH                                                      
*                                                                               
         LA    R4,8(R2)                                                         
         ZAP   HALF,=P'18'         SET FOR 18 PRDS PER LINE                     
         B     FMT14                                                            
*                                                                               
FMT12    GOTO1 SEQ                                                              
*                                                                               
FMT14    CLC   KEY(PRGPPRD-PRGPTYP),KEYSAVE   TEST SAME THRU PRDGRP             
         BNE   FMTX                                                             
*                                                                               
         MVC   0(3,R4),PRGPPRD                                                  
         FOUT  (R2)                                                             
         LA    R4,4(R4)                                                         
         FOUT  (R2)                                                             
         SP    HALF,=P'1'          ADJUST COUNTER                               
         BP    FMT12               CONTINUE IF MORE ROOM                        
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R2,R5               TEST E-O-PRD LIST                            
         BE    FMTX                                                             
***      CLI   0(R2),0             TEST E-O-S                                   
***      BE    FMTX                                                             
         LA    R4,8(R2)                                                         
         ZAP   HALF,=P'18'                                                      
         B     FMT12                                                            
*                                                                               
FMTX     B     EXXMOD                                                           
         EJECT                                                                  
EDT      DS    0H                                                               
         CLI   SVACT,C'A'          TEST ADD                                     
         BNE   EDT2                                                             
*                                                                               
         XC    REC(256),REC                                                     
         XC    REC+256(256),REC+256                                             
         MVC   PRGKEY,SVKEY                                                     
         MVC   PRGLEN,=H'98'                                                    
         MVI   PRGEL,X'10'                                                      
         MVI   PRGEL+1,74                                                       
         B     EDT4                                                             
         SPACE 2                                                                
EDT2     DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         MVC   PRGLEN,=H'98'                                                    
         XC    REC+98(250),REC+98                                               
         EJECT                                                                  
EDT4     LA    R6,PRGEL                                                         
         USING PRGEL10,R6                                                       
*                                                                               
         LA    R2,LFMNM1H                                                       
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   PRGNAM1,WORK                                                     
*                                                                               
         LA    R2,LFMNM2H                                                       
         CLI   SVBKLNS+1,0                                                      
         BNE   EDT4A                                                            
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),0                                                          
         BNE   LFMERR                                                           
         B     EDT5                                                             
*                                                                               
EDT4A    GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   PRGNAM2,WORK                                                     
         OI    4(R2),X'20'         NOW VALID                                    
*                                                                               
EDT5     LA    R2,LFMUSERH         VALIDATE MASTER PRD USER FIELD               
         CLI   5(R2),0                                                          
         BE    EDT05X                                                           
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING PRGEL30,R4                                                       
         MVI   ELEM,X'30'                                                       
         MVI   ELEM+1,5                                                         
         MVC   ELEM+2(3),WORK                                                   
*                                                                               
         MVI   ERRCD,INVERR                                                     
* VALIDATE PRD ON FILE                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LFMERR                                                           
         OI    4(R2),X'20'         NOW VALID UNLESS CHANGED AGAIN               
         SR    R1,R1                                                            
         ICM   R1,3,PRGLEN                                                      
         AH    R1,=H'5'                                                         
         STCM  R1,3,PRGLEN                                                      
         LA    R6,REC+24           POINT TO 1ST ELEMENT                         
         BAS   RE,GETEND           FIND END OF RECORD                           
         MVC   0(5,R6),ELEM        AND MOVE IN ELEMENT                          
         EJECT                                                                  
*                                                                               
EDT05X   XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING PRGEL20,R4                                                       
         MVI   ELEM,X'20'                                                       
         MVI   ELEM+1,122                                                       
         MVI   ADDR,C'N'                                                        
         LA    R2,LFMADD1H                                                      
         CLI   5(R2),0                                                          
         BE    EDT5A                                                            
*                                                                               
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   PRGADDR1,WORK                                                    
         MVI   ADDR,C'Y'                                                        
                                                                                
EDT5A    LA    R2,LFMADD2H                                                      
         CLI   5(R2),0                                                          
         BE    EDT5B                                                            
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   PRGADDR2,WORK                                                    
         MVI   ADDR,C'Y'                                                        
                                                                                
EDT5B    LA    R2,LFMADD3H                                                      
         CLI   5(R2),0                                                          
         BE    EDT5C                                                            
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   PRGADDR3,WORK                                                    
         MVI   ADDR,C'Y'                                                        
                                                                                
EDT5C    LA    R2,LFMADD4H                                                      
         CLI   5(R2),0                                                          
         BE    EDT5X                                                            
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   PRGADDR4,WORK                                                    
         MVI   ADDR,C'Y'                                                        
                                                                                
EDT5X    CLI   ADDR,C'Y'                                                        
         BNE   EDT5XX                                                           
         SR    R1,R1                                                            
         ICM   R1,3,PRGLEN                                                      
         AH    R1,=H'122'                                                       
         STCM  R1,3,PRGLEN                                                      
         LA    R6,REC+24           POINT TO 1ST ELEMENT                         
         BAS   RE,GETEND           FIND END OF RECORD                           
         MVC   0(122,R6),ELEM      AND MOVE IN ELEMENT                          
*                                                                               
EDT5XX   CLI   SVACT,C'A'                                                       
         BNE   EDT6                                                             
         GOTO1 ADDREC                                                           
         MVC   SVKEY+14(4),KEY+14  SAVE DISK ADDRESS                            
         GOTO1 CNADDSPT                                                         
         B     EDT8                                                             
*                                                                               
EDT6     B     EDT6A                                                            
         TM    LFMNM1H+4,X'20'     TEST NAME 1 CHANGED                          
         BZ    EDT6A               YES                                          
         TM    LFMNM2H+4,X'20'     TEST NAME 2 CHANGED                          
         BZ    EDT6A               YES                                          
         TM    LFMADD1H+4,X'20'    TEST ADDRESS CHANGED                         
         BZ    EDT6A               YES                                          
         TM    LFMADD2H+4,X'20'                                                 
         BZ    EDT6A                                                            
         TM    LFMADD3H+4,X'20'                                                 
         BZ    EDT6A                                                            
         TM    LFMADD4H+4,X'20'                                                 
         BZ    EDT6A                                                            
         TM    LFMUSERH+4,X'20'    MASTER PRD USER FIELD                        
         BO    EDT8                NO - SKIP PUT                                
                                                                                
EDT6A    GOTO1 PUTREC                                                           
         GOTO1 CNCHASPT                                                         
*                                                                               
EDT8     OI    4(R2),X'20'         NOW VALID UNLESS CHANGED AGAIN               
         LA    R2,LFMPRD1H                                                      
         B     EDT20B                                                           
         EJECT                                                                  
*                                                                               
EDT10    LA    R8,REC              RESTORE                                      
*                                                                               
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         BCTR  R5,0                                                             
         MVC   WORK,SPACES                                                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2) *EXECUTED*                                         
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLC   =C'POL',WORK                                                     
         BE    LFMERR                                                           
* VALIDATE PRD ON FILE                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),WORK                                                    
EDT12    GOTO1 READ                                                             
         LA    R8,REC       RESET R8                                            
         MVC   SVMKT(1),KEY+1      SAVE AGYMED IN SVMKT                         
*                                                                               
         MVC   WORK2(20),KEY       SAVE PRDHDR KEY/DISK ADDRESS                 
*                                                                               
         LA    R5,REC2                                                          
         ST    R5,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
* TEST FOR AN EXISTING ASSIGNMENT TO THIS GROUP                                 
*                                                                               
         USING PRDHDRD,R5                                                       
         XC    ELEM(20),ELEM       PUT ALL PGROUP ASSGNS                        
         MVC   ELEM(9),PGRP1       IN ELEM                                      
         MVC   ELEM+9(6),PGRP4                                                  
         LA    R0,5                                                             
         LA    R7,ELEM             ONE LEVEL NOW                                
EDT14    CLC   0(1,R7),PRGKID      TEST SAME GROUP ID                           
         BE    EDT16                                                            
         LA    R7,3(R7)                                                         
         BCT   R0,EDT14                                                         
*                                  OKAY TO ADD IF ENOUGH ROOM IN TABLE          
         LA    R0,5                                                             
         LA    R7,ELEM             ONE LEVEL NOW                                
EDT15    OC    0(3,R7),0(R7)       TEST LAST ENTRY                              
         BZ    EDT18                                                            
         LA    R7,3(R7)                                                         
         BCT   R0,EDT15                                                         
         MVI   ERRCD,GRPOVFLW      OVERFLOW CAN'T ADD MORE THAN 5               
         B     LFMERR                                                           
         EJECT                                                                  
EDT16    MVI   ERRCD,GRPASSGN                                                   
         LA    R8,REC                                                           
         CLC   0(3,R7),PRGKID      TEST DUP ASSGN                               
         BE    LFMERR                                                           
* DELETE OLD PASSIVE POINTER                                                    
         XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         MVC   PRGPTYP,=X'0D81'                                                 
         MVC   PRGPAGMD,SVMKT                                                   
         MVC   PRGPCLT,SVCLT                                                    
         MVC   PRGPID(3),0(R7)     MOVE OLD GRP FROM PRDHDR                     
         MVC   PRGPPRD,WORK2+4     WORK2 HAS PRDKEY                             
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
         EJECT                                                                  
* ADD A NEW PASSIVE POINTER                                                     
EDT18    XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         MVC   PRGPTYP,=X'0D81'                                                 
         MVC   PRGPAGMD,SVMKT                                                   
         MVC   PRGPCLT,SVCLT                                                    
         LA    RE,SVKEY+PRGKID-PRGKEY                                           
         MVC   PRGPID(3),0(RE)                                                  
         MVC   PRGPPRD,WORK2+4      WORK2 HAS PRDHDR KEY                        
* SEE IF ON FILE                                                                
EDT18C   OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
*                                                                               
         MVC   COMMAND,=C'DMWRT'                                                
         NI    KEY+13,X'7F'                                                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    EDT19                                                            
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEY+14(4),WORK2+14       INSERT DISK ADDR                        
         MVC   COMMAND,=C'DMADD'                                                
EDT19    GOTO1 DIR                                                              
*                                                                               
EDT20    DS    0H                                                               
* CHANGE PRDHDR                                                                 
         MVC   KEY,WORK2                                                        
         LA    RE,SVKEY+PRGKID-PRGKEY                                           
         MVC   0(3,R7),0(RE)       MOVE NEW PRDGRP INTO ELEM                    
         MVC   PGRP1(9),ELEM       RESTORE PGROUP ASSGNS                        
         MVC   PGRP4(6),ELEM+9                                                  
*                                                                               
*                                  PRDHDR IS COVERED BY R5                      
         GOTO1 PUTREC                                                           
*                                                                               
         DROP  R5                                                               
         OI    4(R2),X'20'         SET VALID                                    
*                                                                               
*        IF CANADIAN TV - REPEAT STEPS EDT12 THRU EDT20 FOR                     
*                       THE OTHER MEDIAS                                        
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   EDT20A                                                           
         CLI   SVEBCMED,C'T'                                                    
         BNE   EDT20A                                                           
*                                                                               
         MVC   BYTE,KEY+1          SEE WHICH MEDIA I JUST FINISHED              
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'08'          LAST WAS COMBINED - SO DONE                  
         BE    EDT20A                                                           
         CLI   BYTE,X'03'                                                       
         BE    DOCOMB              LAST WAS NETWORK - DO COMBINED               
*                                                                               
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'         LAST WAS TV - DO NETWORK                     
         B     EDT12                                                            
*                                                                               
DOCOMB   NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'         LAST WAS NETW - DO COMBINED                  
         B     EDT12                                                            
*                                                                               
EDT20A   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R1,LFMSEPAH         TEST REACHED ADDRESS FIELD                   
         CR    R1,R2                                                            
         BE    EDTX                                                             
***      CLI   0(R2),9             TEST E-O-S                                   
***      BE    EDTX                                                             
EDT20B   CLI   5(R2),0             TEST DATA                                    
         BE    EDT20A              NO-TRY NEXT                                  
         TM    4(R2),X'20'         TEST PREVIOUSLY VALIDATED                    
         BO    EDT20A                                                           
         B     EDT10               GO EDIT                                      
*                                                                               
EDTX     LA    R8,REC                                                           
         ST    R8,AREC                                                          
         B     FMT                 GO REDISPLAY                                 
         EJECT                                                                  
GETEND   ZIC   R1,1(R6)                                                         
         AR    R6,R1               POINT TO NEXT ELEMENT                        
         CLI   0(R6),0                                                          
         BNE   GETEND                                                           
         BR    RE                                                               
*                                                                               
         SPACE 2                                                                
LFMERR   GOTO1 ERROR                                                            
*                                                                               
         LTORG                                                                  
ADDR     DS    CL1                 FLAG - ADDRESS ENTERED                       
       ++INCLUDE SPLFMWRK                                                       
*                                                                               
         EJECT                                                                  
         ORG   LFMTABH                                                          
* SPLFMF5D                                                                      
       ++INCLUDE SPLFMF5D                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
*                                                                               
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
 END                                                                            
