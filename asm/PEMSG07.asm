*          DATA SET PEMSG07    AT LEVEL 086 AS OF 05/01/02                      
*PHASE TE1D07A                                                                  
         TITLE 'TE1D07 - ADD ADDRESS LISTS'                                     
TE1D07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TE1D07                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TE1DFFD,RA                                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R4,KEY                                                           
         USING MSGKEYD,R4                                                       
         SPACE 3                                                                
*                                                                               
         CLI   ADLCMMDH+5,0                                                     
         BNE   UPDT                                                             
*                                                                               
         CLI   MSADFLG,X'FF'                                                    
         BNE   U1                                                               
         OI    ADLDKTSH+6,X'80'                                                 
         MVC   ADLDKTS+9(6),=C'S-SEND'                                          
         MVI   MSADFLG,X'F0'                                                    
         XC    KEY,KEY               BUILD KEY                                  
         MVI   MSGSYS,C'M'                                                      
         MVC   MSGAGY,TWAORIG                                                   
         MVC   MSGNAM,SGNONUN                                                   
         MVI   MSGKTYPE,ADLKTYPQ                                                
         MVC   ADLKNAM,=C'TEMPLIST'                                             
         GOTO1 HIGH                                                             
         MVC   SVMSGDA,MSGDA                                                    
         CLC   KEY(36),KEYSAVE                                                  
         BE    TOP                                                              
         MVC   KEY,KEYSAVE                                                      
         MVC   ADLNAME,ADLKNAM                                                  
         B     ADD6A                                                            
*                                                                               
U1       CLI   ADLCMMD,C'A'                                                     
         BE    ADD1                                                             
         CLI   ADLCMMD,C'T'                                                     
         BE    VECT                                                             
UPDT     CLI   SYSKEY,0                                                         
         BE    ADD1                                                             
         BAS   RE,UPDATE                                                        
         CLI   SGNMSG,C'U'                                                      
         BE    XIT1                                                             
*                                                                               
         CLI   MSADFLG,0                                                        
         BE    VECT                                                             
         CLI   ADLCMMD,C'K'                                                     
         BNE   *+8                                                              
         MVI   ADLCMMD,C' '                                                     
         CLI   ADLCMMD,C'S'                                                     
         BNE   *+8                                                              
         MVI   ADLCMMD,C'K'                                                     
         CLI   ADLCMMD,C'D'                                                     
         BNE   VECT                                                             
         CLI   ADLKPAG,0                                                        
         BNE   VECT                                                             
         MVI   ADLCMMD,C' '                                                     
         MVC   SGNMSG(23),=C'CAN''T DELETE FIRST PAGE'                          
*                                                                               
VECT     CLI   ADLCMMD,C'T'                                                     
         BE    TOP                                                              
         CLI   ADLCMMD,C'N'                                                     
         BE    NEXT                                                             
         CLI   ADLCMMD,C'P'                                                     
         BE    PREV                                                             
         CLI   ADLCMMD,C'D'                                                     
         BE    DELETE                                                           
         CLI   ADLCMMD,C'A'                                                     
         BE    ADD1                                                             
         CLI   ADLCMMD,C'K'                                                     
         BE    XIT1                                                             
         B     DREC                                                             
         EJECT                                                                  
*                               UPDATE PAGE                                     
         SPACE 3                                                                
UPDATE   NTR1                                                                   
*                                                                               
         MVC   KEY,SYSKEY                                                       
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         MVI   MAX,1               DESCRIPTION                                  
         LA    R2,ADLDESCH                                                      
         MVI   ELCODE,X'40'                                                     
         GOTO1 VALICHAT                                                         
*                                                                               
         MVI   ELCODE,X'41'                                                     
         GOTO1 REMELEM                                                          
         LA    R3,12                 SEND TOS                                   
         LA    R2,ADLFSTGH                                                      
VRLP1    CLI   5(R2),0                                                          
         BE    VRLP1B                                                           
         MVC   ELEMENT,SPACES                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,ELEMENT+2,8(R2)                                               
         BAS   RE,BUMP                                                          
         MVC   WORK,SPACES                                                      
         MVC   ELEMENT+10(2),TWAORIG                                            
         CLI   5(R2),0                                                          
         BE    VRLP1A                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,WORK+1,8(R2)                                                  
         MVI   WORK,2                                                           
         GOTO1 GETSGN                                                           
         MVC   ELEMENT+10(2),WORK                                               
VRLP1A   MVI   ELEMENT+12,0 CLEAR NC                                            
         MVI   ELEMENT,X'41'                                                    
         MVI   ELEMENT+1,13                                                     
*                            VALIDATE NAME                                      
         XC    MSGKEY,MSGKEY                                                    
         MVI   MSGKEY,C'M'                                                      
         MVC   MSGAGY,ELEMENT+10                                                
         MVC   MSGNAM,=C'SYS     '                                              
         MVI   MSGKTYP,X'11'                                                    
         MVC   USRNAME,ELEMENT+2                                                
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    VRLP1A1                                                          
         MVC   KEY,KEYSAVE                                                      
         MVI   MSGKTYP,X'12'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    VRLP1A1                                                          
VRLPER   MVC   SGNMSG(14),=C'USER NOT FOUND'                                    
         SH    R2,=H'16'                                                        
         OI    6(R2),X'40'                                                      
         B     XIT1                                                             
*                                                                               
VRLP1A1  BAS   RE,BUMP      VALIDATE NC                                         
         CLI   5(R2),0                                                          
         BE    VRLP1A3                                                          
         SR    R0,R0                                                            
         NI    8(R2),X'0F'                                                      
         ZIC   R1,8(R2)                                                         
         CLI   5(R2),1                                                          
         BE    VRLP1A2                                                          
         M     R0,=F'10'                                                        
         NI    9(R2),X'0F'                                                      
         ZIC   R0,9(R2)                                                         
         AR    R1,R0                                                            
VRLP1A2  STC   R1,ELEMENT+12                                                    
*                                                                               
VRLP1A3  GOTO1 ADDELEM                                                          
         B     *+12                                                             
VRLP1B   BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BCT   R3,VRLP1                                                         
*                                                                               
VR2      MVI   ELCODE,X'42'                                                     
         GOTO1 REMELEM                                                          
         LA    R3,12                 COPY TOS                                   
         LA    R2,ADLNXTGH                                                      
VRLP2    CLI   5(R2),0                                                          
         BE    VRLP2B                                                           
         MVC   ELEMENT,SPACES                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,ELEMENT+2,8(R2)                                               
         BAS   RE,BUMP                                                          
         MVC   WORK,SPACES                                                      
         MVC   ELEMENT+10(2),TWAORIG                                            
         CLI   5(R2),0                                                          
         BE    VRLP2A                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,WORK+1,8(R2)                                                  
         MVI   WORK,2                                                           
         GOTO1 GETSGN                                                           
         MVC   ELEMENT+10(2),WORK                                               
VRLP2A   MVI   ELEMENT+12,0 ZERO NC                                             
         MVI   ELEMENT,X'42'                                                    
         MVI   ELEMENT+1,13                                                     
*                            VALIDATE NAME                                      
         XC    MSGKEY,MSGKEY                                                    
         MVI   MSGKEY,C'M'                                                      
         MVC   MSGAGY,ELEMENT+10                                                
         MVC   MSGNAM,=C'SYS     '                                              
         MVI   MSGKTYP,X'11'                                                    
         MVC   USRNAME,ELEMENT+2                                                
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    VRLP2A1                                                          
         MVC   KEY,KEYSAVE                                                      
         MVI   MSGKTYP,X'12'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   VRLPER                                                           
*                                                                               
VRLP2A1  BAS   RE,BUMP      VALIDATE NC                                         
         CLI   5(R2),0                                                          
         BE    VRLP2A3                                                          
         SR    R0,R0                                                            
         NI    8(R2),X'0F'                                                      
         ZIC   R1,8(R2)                                                         
         CLI   5(R2),1                                                          
         BE    VRLP2A2                                                          
         M     R0,=F'10'                                                        
         NI    9(R2),X'0F'                                                      
         ZIC   R0,9(R2)                                                         
         AR    R1,R0                                                            
VRLP2A2  STC   R1,ELEMENT+12                                                    
*                                                                               
VRLP2A3  GOTO1 ADDELEM                                                          
         B     *+12                                                             
VRLP2B   BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BCT   R3,VRLP2                                                         
*                                                                               
         MVC   KEY,SYSKEY                                                       
         GOTO1 PUTREC                                                           
*                                                                               
         MVC   SGNMSG(12),=C'LIST UPDATED'                                      
         B     XIT1                                                             
         EJECT                                                                  
*                               SET TO FIRST PAGE                               
         SPACE 3                                                                
TOP      XC    KEY,KEY        GET RECORD                                        
         MVC   MSGDA,SVMSGDA                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R1,AIO         SAVE KEY                                          
         MVC   KEY,0(R1)                                                        
         MVC   SYSKEY,KEY                                                       
*                                                                               
         B     DREC                                                             
         EJECT                                                                  
*                              SET TO NEXT PAGE                                 
NEXT     MVC   KEY,SYSKEY                                                       
         ZIC   R1,ADLKPAG                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ADLKPAG                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   ADD11                                                            
         MVC   SYSKEY,KEY                                                       
         GOTO1 GETREC                                                           
         B     DREC                                                             
         EJECT                                                                  
*                              SET TO PREV PAGE                                 
PREV     MVC   KEY,SYSKEY                                                       
         ZIC   R1,ADLKPAG                                                       
         LTR   R1,R1                                                            
         BZ    PREV1                                                            
         BCTR  R1,0                                                             
         STC   R1,ADLKPAG                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    *+6         WHERE'D IT GO                                        
         DC    H'0'                                                             
         MVC   SYSKEY,KEY                                                       
         GOTO1 GETREC                                                           
         B     DREC                                                             
PREV1    MVC   SGNMSG(10),=C'FIRST PAGE'                                        
         B     XIT                                                              
         EJECT                                                                  
*                              DISPLAY PAGE                                     
DREC     OI    ADLNAMEH+1,X'20'                                                 
         NI    ADLDESCH+1,X'DF'                                                 
         CLI   ADLKPAG,0                                                        
         BE    *+8                                                              
         OI    ADLDESCH+1,X'20'                                                 
*                                                                               
         MVI   MAX,1               DESCRIPTION                                  
         MVI   OPTION,C' '                                                      
         LA    R2,ADLDESCH                                                      
         MVI   ELCODE,X'40'                                                     
         GOTO1 DISPCHAT                                                         
*                                                                               
         MVC   ADLNAME,ADLKNAM     NAME                                         
         OI    ADLNAMEH+6,X'80'                                                 
*                                                                               
         ZIC   R1,ADLKPAG          PAGE                                         
         OI    ADLPAGEH+6,X'80'                                                 
         XC    ADLPAGE,ADLPAGE                                                  
         LA    R1,1(R1)                                                         
         CVD   R1,DUB                                                           
         UNPK  WORK(5),DUB+6(3)                                                 
         MVC   ADLPAGE,WORK                                                     
*                                                                               
DRST     MVI   ELCODE,X'41'        SEND TOS                                     
         LA    R3,12                                                            
         LA    R2,ADLFSTGH                                                      
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DRLP1    BAS   RE,NEXTEL                                                        
         BNE   DRLP1A                                                           
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(8,R2),2(R6)                                                    
         BAS   RE,BUMP                                                          
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(8,R2),SPACES                                                   
         CLC   TWAORIG,10(R6)                                                   
         BE    DRLP1B                                                           
         MVI   WORK,1                                                           
         MVC   WORK+1(2),10(R6)                                                 
         GOTO1 GETSGN                                                           
         MVI   ELCODE,X'41'                                                     
         MVC   8(8,R2),WORK                                                     
DRLP1B   BAS   RE,BUMP                                                          
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(2,R2),SPACES                                                   
         CLI   12(R6),0                                                         
         BE    DRLP1C                                                           
         ZIC   R1,12(R6)                                                        
         EDIT  (R1),(2,8(R2))                                                   
DRLP1C   BAS   RE,BUMP                                                          
         BCT   R3,DRLP1                                                         
         B     DRLP2S                                                           
DRLP1A   MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(8,R2),SPACES                                                   
         BAS   RE,BUMP                                                          
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(8,R2),SPACES                                                   
         BAS   RE,BUMP                                                          
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(2,R2),SPACES                                                   
         BAS   RE,BUMP                                                          
         BCT   R3,DRLP1A                                                        
*                                                                               
DRLP2S   MVI   ELCODE,X'42'        COPY TOS                                     
         LA    R3,12                                                            
         LA    R2,ADLNXTGH                                                      
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DRLP2    BAS   RE,NEXTEL                                                        
         BNE   DRLP2A                                                           
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(8,R2),2(R6)                                                    
         BAS   RE,BUMP                                                          
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(8,R2),SPACES                                                   
         CLC   TWAORIG,10(R6)                                                   
         BE    DRLP2B                                                           
         MVI   WORK,1                                                           
         MVC   WORK+1(2),10(R6)                                                 
         GOTO1 GETSGN                                                           
         MVI   ELCODE,X'42'                                                     
         MVC   8(8,R2),WORK                                                     
DRLP2B   BAS   RE,BUMP                                                          
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(2,R2),SPACES                                                   
         CLI   12(R6),0                                                         
         BE    DRLP2C                                                           
         ZIC   R1,12(R6)                                                        
         EDIT  (R1),(2,8(R2))                                                   
DRLP2C   BAS   RE,BUMP                                                          
         BCT   R3,DRLP2                                                         
         B     DRLP3                                                            
DRLP2A   MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(8,R2),SPACES                                                   
         BAS   RE,BUMP                                                          
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(8,R2),SPACES                                                   
         BAS   RE,BUMP                                                          
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(2,R2),SPACES                                                   
         BAS   RE,BUMP                                                          
         BCT   R3,DRLP2A                                                        
DRLP3    CLI   ADLFSTG,C' '          CHECK BLANK PG                             
         BH    DRLP3A                                                           
         CLI   ADLNXTG,C' '                                                     
         BH    DRLP3A                                                           
         CLI   ADLCMMD,C'N'          CHECK JUST ADDED                           
         BE    DRLP3A                                                           
         CLI   ADLKPAG,0             CHECK ONLY PG                              
         BNE   DELETE                                                           
DRLP3A   MVC   ADLLPMS,SPACES                                                   
         OI    ADLLPMSH+6,X'80'                                                 
         ZIC   R1,ADLKPAG                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ADLKPAG                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    XIT                                                              
         MVC   ADLLPMS,=C'LAST PAGE'                                            
         B     XIT                                                              
         EJECT                                                                  
*              DELETE RECORD                                                    
         SPACE 3                                                                
DELETE   ZIC   R1,ADLKPAG               LOOK FOR NEXT RECORD                    
         LA    R1,1(R1)                                                         
         STC   R1,ADLKPAG                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   DELDONE                                                          
         MVC   SYSKEY,KEY                                                       
*                                                                               
         MVC   AIO,AIO2                 COPY RECORD                             
         GOTO1 GETREC                                                           
         L     R6,AIO   NEW                                                     
         L     R1,AIO1  OLD                                                     
         MVC   36(2,R1),36(R6)                                                  
         MVC   0(44,R6),0(R1)                                                   
         ZIC   R1,ADLKPAG                                                       
         BCTR  R1,0                                                             
         STC   R1,ADLKPAG                                                       
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   KEY,SYSKEY                                                       
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         B     DELETE                   DO AGAIN                                
*                                                                               
DELDONE  MVC   KEY,KEYSAVE                                                      
         ZIC   R1,ADLKPAG               DELETE KEY                              
         BCTR  R1,0                                                             
         STC   R1,ADLKPAG                                                       
         GOTO1 READ                                                             
         OI    MSGKSTAT,X'80'                                                   
         GOTO1 WRITE                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                   DELETE RECORD                           
         OI    38(R6),X'80'                                                     
         GOTO1 PUTREC                                                           
*                                                                               
         MVI   ADLCMMD,C'N'             DO NEXT IF ALL DELETED                  
         CLI   ADLKPAG,0                                                        
         BE    XIT1                                                             
*                                                                               
         MVI   ADLKPAG,0                DISPLAY FIRST PAGE                      
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   SYSKEY,KEY                                                       
         B     DREC                                                             
         EJECT                                                                  
*              ADD NEW PAGE                                                     
         SPACE 3                                                                
ADD1     CLI   SYSKEY,0                NEW LIST ADD                             
         BE    ADD5                                                             
         CLI   ADLCMMDH+5,0                                                     
         BE    ADD4                                                             
         DC    H'0'                                                             
*                                                                               
ADD11    OI    DMINBTS,X'08'          CHECK IF DELETED                          
         MVC   AIO,AIO2                                                         
         MVC   KEY,KEYSAVE                                                      
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(36),KEYSAVE                                                  
         BNE   ADD2                                                             
*                                                                               
         GOTO1 GETREC                 UNDELETE                                  
         NI    KEY+36,X'7F'                                                     
         GOTO1 WRITE                                                            
         L     R6,AIO                                                           
         NI    38(R6),X'7F'                                                     
         B     ADD3                                                             
*                                                                               
ADD2     MVC   KEY,KEYSAVE            ADD NEW                                   
         L     R6,AIO                                                           
         MVC   0(36,R6),KEY                                                     
         MVC   ELEMENT(4),=X'41040000'                                          
         GOTO1 ADDELEM                                                          
         GOTO1 ADDREC                                                           
         MVC   MSGDA,KEY                                                        
         MVC   KEY(36),KEYSAVE                                                  
         GOTO1 GETREC                                                           
*                                                                               
ADD3     L     R1,AIO1                BUILD BLANK REC                           
         MVC   36(2,R6),36(R1)                                                  
         MVC   0(44,R1),0(R6)                                                   
         MVC   AIO,AIO1                                                         
         MVI   ELCODE,X'41'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'42'                                                     
         GOTO1 REMELEM                                                          
         GOTO1 PUTREC                                                           
         MVC   SYSKEY,KEY                                                       
         B     DREC                                                             
*                                                                               
ADD4     XC    SYSKEY,SYSKEY         SET UP FOR RETURN                          
         MVC   SGNMSG(10),=C'ENTER DATA'                                        
         OI    ADLNAMEH+6,X'40'                                                 
         MVI   ADLCMMD,0                                                        
         B     XIT                                                              
*                                                                               
ADD5     CLI   ADLCMMD,C'A'                                                     
         BE    ADD4                                                             
         LA    R2,ADLNAMEH           GET NAME                                   
         CLI   5(R2),0                                                          
         BNE   ADD6                                                             
         MVC   SGNMSG(15),=C'MUST ENTER NAME'                                   
         OI    ADLNAMEH+6,X'40'                                                 
         B     XIT                                                              
*                                                                               
ADD6     GOTO1 ANY                                                              
         XC    KEY,KEY               BUILD KEY                                  
         MVI   MSGSYS,C'M'                                                      
         MVC   MSGAGY,TWAORIG                                                   
         MVC   MSGNAM,SGNONUN                                                   
         MVI   MSGKTYPE,ADLKTYPQ                                                
         MVC   ADLKNAM,WORK                                                     
*                                                                               
ADD6A    OI    DMINBTS,X'08'          CHECK FOR DUPLICATE                       
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(36),KEYSAVE                                                  
         BE    ADD7                                                             
*                                                                               
         MVC   KEY,KEYSAVE            BUILD RECORD                              
         L     RE,AIO                                                           
         XCEF  (RE),128                                                         
         L     R6,AIO                                                           
         MVC   0(36,R6),KEY                                                     
*                                                                               
         MVC   SYSKEY,KEY                                                       
         MVC   ELEMENT(4),=X'41040000'                                          
         GOTO1 ADDELEM                                                          
         GOTO1 ADDREC                                                           
         MVC   SVMSGDA,KEY                                                      
*                                                                               
         BAS   RE,UPDATE                                                        
         B     ADD7X                                                            
*                                                                               
ADD7     TM    KEY+36,X'80'           CHECK DELETED                             
         BNO   ADD8                                                             
         NI    KEY+36,X'7F'                                                     
         GOTO1 WRITE                                                            
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         NI    38(R6),X'7F'                                                     
         GOTO1 PUTREC                                                           
         MVC   SVMSGDA,MSGDA                                                    
         MVC   SYSKEY,KEY                                                       
         BAS   RE,UPDATE                                                        
*                                                                               
ADD7X    MVC   SGNMSG(18),=C'ADDRESS LIST ADDED'                                
         B     DREC                                                             
*                                                                               
ADD8     MVC   SGNMSG(19),=C'LIST ALREADY EXISTS'                               
         B     XIT                                                              
         EJECT                                                                  
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
XIT      MVI   ADLCMMD,0                                                        
         OI    ADLCMMDH+6,X'C0'                                                 
XIT1     XIT1                                                                   
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
         SPACE 1                                                                
RELO     DS    A                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMSGFILE                                                      
       ++INCLUDE PEMSGFFD                                                       
         PRINT ON                                                               
         ORG   SGNIH                                                            
       ++INCLUDE PEMSGF8D                                                       
         PRINT OFF                                                              
CONHEADH EQU   SGNMSGH                                                          
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE PEMSGWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086PEMSG07   05/01/02'                                      
         END                                                                    
