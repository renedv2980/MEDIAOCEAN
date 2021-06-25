*          DATA SET SPLFM45    AT LEVEL 016 AS OF 05/01/02                      
*PHASE T21945A,+0                                                               
         TITLE 'SPLFM45 - SPTFILE MAINT - AGENCY HUT REC'                       
T21945   CSECT                                                                  
         NMOD1 0,T21945                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING NHHRECD,R8                                                       
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         LA    R7,REC+24                                                        
         CLI   0(R7),X'52'                                                      
         BE    FMT2                                                             
         MVI   ELCODE,X'52'                                                     
         BAS   RE,NEXTEL                                                        
         BE    FMT2                                                             
         DC    H'0'                MUST FIND 52 ELEM                            
FMT2     DS    0H                                                               
         USING NHHEL,R7                                                         
         LA    R2,HUTHOL1H                                                      
         LA    R5,MAXHUTS                                                       
         LA    R6,NHHADJ                                                        
*                                                                               
FMT4     XC    8(L'HUTHOL1,R2),8(R2)                                            
         OC    0(2,R6),0(R6)                                                    
         BZ    FMT4B                                                            
         LH    R0,0(R6)                                                         
         CVD   R0,DUB                                                           
         EDIT  (P8,DUB),(6,8(R2)),2,ALIGN=LEFT,FLOAT=-                          
FMT4B    FOUT  (R2)                                                             
         BAS   RE,NEXTUN                                                        
         LA    R6,2(R6)            NEXT HUT                                     
         BCT   R5,FMT4                                                          
*                                                                               
FMT5     DS    0H                  DISPLAY MONTH TOTALS                         
*                                                                               
         XC    HUTACT,HUTACT                                                    
         LA    R7,REC+24                                                        
         CLI   0(R7),X'01'                                                      
         BE    FMT6                                                             
         MVI   ELCODE,X'01'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   FMT6X                                                            
*                                                                               
FMT6     MVC   HUTACT(13),=C'LAST ACTIVITY'                                     
         USING NHHEL01,R7                                                       
         GOTO1 VDATCON,DMCB,(3,NHHACTD),(5,HUTACT+14)                           
         MVC   HUTACT+24(3),=C'ADD'                                             
         CLI   NHHACT,C'A'                                                      
         BE    *+10                                                             
         MVC   HUTACT+24(3),=C'CHA'                                             
*                                                                               
FMT6X    FOUT  HUTACTH                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
EDT      DS    0H                                                               
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    EDT1C                                                            
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC              REREAD REC                                   
*                                                                               
         B     EDT1E                                                            
*                                                                               
EDT1C    LA    R7,REC+24                                                        
         MVC   NHHLEN,=H'24'      INITIALIZE REC LEN                            
*                                                                               
EDT1E    DS    0H                                                               
*                                                                               
EDT2     LA    R2,HUTHOL1H         CURSOR TO FIRST FIELD                        
         GOTO1 ANY                 REQUIRED                                     
         CLI   SVACT,C'A'          NO DELETE FOR ADD                            
         BE    EDT6                                                             
         CLC   8(6,R2),=C'DELETE'                                               
         BNE   EDT3                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY                                                        
         MVI   KEY+13,X'80'                                                     
         MVC   COMMAND,=CL8'DMWRT'                                              
         GOTO1 DIR                                                              
*                                                                               
         MVI   REC+15,X'C0'                                                     
         GOTO1 PUTREC                                                           
         FOUT  LFMMSGH,=C'** HOLIHUT DELETED **',21                             
         MVI   ERRAREA,01                                                       
         B     EXXMOD                                                           
*                                                                               
EDT3     LA    R7,REC+24                                                        
         MVI   ELCODE,X'52'                                                     
         CLI   0(R7),X'52'                                                      
         BE    EDT6                                                             
         BAS   RE,NEXTEL                                                        
         BE    EDT6                                                             
         DC    H'0'                MUST FIND 02 ELEM                            
*                                                                               
EDT6     XC    ELEM(150),ELEM                                                   
         MVC   ELEM(2),=X'5218'       SET CODE AND LENGHT                       
EDT6A    LA    R7,ELEM                                                          
         USING NHHEL,R7                                                         
         LA    R2,HUTHOL1H                                                      
         CLI   5(R2),0                                                          
         BNE   EDT6A5                                                           
         MVI   ERRCD,MSSNGERR                                                   
         B     LFMERR                                                           
EDT6A5   LA    R5,MAXHUTS                                                       
         LA    R6,NHHADJ                                                        
*                                                                               
EDT6B    CLI   5(R2),0                                                          
         BNE   EDT6C                                                            
         B     EDT6E               NO INPUT SKIP TO NEXT DAYPART                
*                                                                               
EDT6C    ZIC   R3,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,8(R2),(R3)                                         
         CLI   DMCB,0                                                           
         BNE   EDTERR                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'10000'                                                     
         BH    EDTERR                CAN'T EXCEED 100 PCT                       
         STH   R0,0(R6)                                                         
*                                                                               
EDT6E    LA    R6,2(R6)            NEXT DAYPART                                 
         BAS   RE,NEXTUN           SET R2 TO NEXT UNPROTECTED FLD               
         BCT   R5,EDT6B                                                         
*                                                                               
EDT7     DS    0H                                                               
         EJECT                                                                  
EDT20    DS    0H                                                               
*                                                                               
EDT20B   LA    R7,REC+24                                                        
         MVI   ELCODE,X'52'                                                     
         CLI   0(R7),X'52'                                                      
         BE    EDT20D                                                           
         BAS   RE,NEXTEL                                                        
         BE    EDT20D                                                           
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    EDT20F                                                           
         DC    H'0'                MUST FIND 52 ELEM                            
*                                                                               
EDT20D   DS    0H                                                               
         GOTO1 VRECUP,DMCB,(0,REC),0(R7),0      DELETE OLD 52                   
*                                                                               
EDT20F   GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R7)       ADD NEW ONE                 
WRITE    DS    0H                                                               
         BAS   RE,ACTIVITY         ADD OR UPDATE ACTIVITY ELEM                  
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         MVC   KEY,SVKEY                                                        
         MVC   REC(13),SVKEY                                                    
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    ADDNHUT                                                          
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         GOTO1 GETREC              REREAD REC                                   
         ST    R8,AREC                                                          
         GOTO1 PUTREC                                                           
         B     FMT                                                              
*                                                                               
ADDNHUT  GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         B     FMT                 GO REFORMAT RECORD                           
         EJECT                                                                  
ACTIVITY NTR1                                                                   
         XC    ELEM+200(10),ELEM+200                                            
         MVC   ELEM+200(2),=X'0108'                                             
         LA    R6,ELEM+200                                                      
         USING NHHEL01,R6                                                       
         GOTO1 VDATCON,DMCB,(5,0),(3,NHHACTD)                                   
*                                  SET ACTIVITY DATE - TODAY                    
         MVC   NHHACT,SVACT        SAVE ACTION                                  
         LA    R7,REC+24                                                        
         CLI   0(R7),X'01'                                                      
         BE    ACT2                                                             
         MVI   ELCODE,X'01'                                                     
         BAS   RE,NEXTEL                                                        
         BE    ACT2                                                             
         LA    R7,REC+24           ADD AS FIRST ELEM+200                        
         GOTO1 VRECUP,DMCB,(0,REC),ELEM+200,0(R7)                               
         B     ACTX                                                             
*                                                                               
ACT2     MVC   0(8,R7),ELEM+200    SWITCH OLD AND NEW ELEMS                     
*                                                                               
ACTX     XIT1                                                                   
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R7)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  LTR   R7,R7                                                            
         BR    RE                                                               
         SPACE 2                                                                
NEXTUN   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUN                                                           
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
EDTERR   MVI   ERRCD,INVERR                                                     
*                                                                               
LFMERR   GOTO1 ERROR                                                            
*                                                                               
MAXHUTS  EQU   8                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFMC5                                                                        
       ++INCLUDE SPLFMC5D                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENHHUT                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPLFM45   05/01/02'                                      
         END                                                                    
