*          DATA SET SPINF12    AT LEVEL 002 AS OF 05/31/07                      
*PHASE T21A12A,+0,NOAUTO                                                        
         TITLE 'T21A12 - SPOTPAK INFO PRODUCT HEADER DISPLAY'                   
T21A12   CSECT                                                                  
         NMOD1 0,T21A12                                                         
         USING FLDHDRD,R2                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
*                                                                               
         MVI   FLTSW,0             INITIALIZE FILTER FIELD SWITCH               
*                                                                               
         LA    RE,REC2             CLEAR SAVE AREA                              
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
CKDT     DS    0H                  CHK FOR DATA= KEYWORDS                       
         MVI   USESW,0                                                          
*                                                                               
         GOTO1 USER1,DMCB,(64,SINIFLT),(5,=C'DATA=')     GETFLTR                
         OC    4(4,R1),4(R1)                                                    
         BZ    CKDTX                                                            
         L     R4,4(R1)                                                         
         L     R6,4(R1)                                                         
         LA    R4,5(R4)                                                         
         LA    RE,5                                                             
*                                                                               
CKDT10   CLC   0(5,R4),=C'USER1'                                                
         BNE   CKDT20                                                           
         MVI   USESW,1                                                          
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
         B     CKDTX                                                            
*                                                                               
CKDT20   CLC   0(5,R4),=C'USER2'                                                
         BNE   CKDT30                                                           
         MVI   USESW,2                                                          
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
         B     CKDTX                                                            
*                                                                               
CKDT30   DS    0H                                                               
         B     FLTERR                                                           
*                                                                               
CKDTX    DS    0H                                                               
         LA    R2,SINHDRH          BUILD HEADLINES                              
         CLI   USESW,0             DISPLAYING USER DATA?                        
         BE    PR5                                                              
         BAS   RE,GETCLTU          GET USER DESC FROM CLT                       
         MVC   FLDDATA+1(4),=CL17'PRD/'                                         
         MVC   FLDDATA+5(20),CUSER                                              
         MVC   FLDDATA+40(24),FLDDATA+1                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(24),DASH                                               
         MVC   FLDDATA+40(24),DASH                                              
*                                                                               
* SCAN BACKWARDS FOR FIRST NON-SPACE TO FIX UNDERLINING OF CUSER                
*                                                                               
         LA    RE,CUSER+19                                                      
         LA    RF,FLDDATA+24                                                    
         LA    R6,19               FOR BCT                                      
PR2      CLI   0(RE),C' '                                                       
         BH    PR4                                                              
         BCTR  RE,0                                                             
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R6,PR2                                                           
*                                                                               
* SCAN BACKWARDS FOR FIRST NON-SPACE TO FIX UNDERLINING OF CUSER                
*                                                                               
PR4      LA    RE,CUSER+19                                                      
         LA    RF,FLDDATA+63                                                    
         LA    R6,19               FOR BCT                                      
PR4C     CLI   0(RE),C' '                                                       
         BH    PR4X                                                             
         BCTR  RE,0                                                             
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R6,PR4C                                                          
*                                                                               
PR4X     LA    R7,28                                                            
         B     PR20                                                             
*                                                                               
PR5      MVC   FLDDATA+1(17),=C'PRODUCT CODE/NAME'                              
         MVC   FLDDATA+27(17),FLDDATA+1                                         
         MVC   FLDDATA+53(17),FLDDATA+1                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),DASH                                               
         MVC   FLDDATA+27(17),DASH                                              
         MVC   FLDDATA+53(17),DASH                                              
         LA    R7,42                                                            
*                                                                               
PR20     FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
         LA    R6,REC2                                                          
         LA    R5,KEY              BUILD FIRST KEY                              
         USING PRDHDRD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   PKEYAM,SVAGYMD                                                   
         MVC   PKEYCLT,SVCLT                                                    
         MVC   PKEYPRD,SVEBCPRD                                                 
         OC    PREVKEY,PREVKEY     FIRST TIME                                   
         BZ    *+10                                                             
         MVC   KEY,PREVKEY          NO - RESTORE PREV KEY                       
         XC    PREVKEY,PREVKEY                                                  
RHI      GOTO1 HIGH                                                             
         B     HAVREC                                                           
*                                                                               
RSEQ     GOTO1 SEQ                                                              
HAVREC   LA    R5,KEY                                                           
         CLC   KEY(4),KEYSAVE                                                   
         BNE   REND                                                             
         OC    PKEYPRD,PKEYPRD                                                  
         BZ    RSEQ                                                             
         OC    PKEYPRD+3(5),PKEYPRD+3                                           
         BZ    *+14                                                             
         MVC   PKEYPRD+3(6),=6X'FF'                                             
         B     RHI                                                              
         GOTO1 GETREC                                                           
         L     R5,AREC             BUILD TABLE FOR ALPHA LIST                   
         MVC   0(3,R6),PKEYPRD                                                  
         MVC   3(20,R6),PNAME                                                   
*                                                                               
         CLI   USESW,0             SEE IF DISPLAYING USER DATA                  
         BE    HAVR10                                                           
         XC    3(20,R6),3(R6)      CLEAR NAME                                   
         CLI   USESW,1                                                          
         BNE   HAVR5                                                            
         MVC   3(32,R6),PUSER1                                                  
         B     HAVR5X                                                           
*                                                                               
HAVR5    MVC   3(16,R6),PUSER2                                                  
*                                                                               
HAVR5X   LA    R6,35(R6)                                                        
         B     HAVR11                                                           
*                                                                               
HAVR10   LA    R6,23(R6)                                                        
HAVR11   BCT   R7,RSEQ                                                          
         MVC   PREVKEY,KEY         SAVE KEY FOR NEXT READ                       
*                                                                               
         MVC   PKEYPRD+3(6),=6X'FF'                                             
*                                                                               
* END OF PRODUCT HEADERS SO FORMAT SCREEN                                       
REND     CLI   REC2,0              ANY DATA                                     
         BNE   FORMAT                                                           
         MVI   ERRCD,NOFNDERR       NO - SEND MESSAGE                           
         GOTO1 ERROR                                                            
         B     MODEXIT                                                          
*                                                                               
FORMAT   XC    DMWORK(20),DMWORK                                                
         LA    R6,REC2                                                          
         LA    R7,0                                                             
FORMAT1  CLI   0(R6),0             COUNT NUMBER OF ENTRIES                      
         BE    FORMAT2                                                          
*                                                                               
         CLI   USESW,0             SEE IF DOING USER DATA                       
         BE    FORM1               NO                                           
         LA    R6,35(R6)                                                        
         B     *+8                                                              
FORM1    LA    R6,23(R6)                                                        
         LA    R7,1(R7)                                                         
         B     FORMAT1                                                          
*                                                                               
FORMAT2  CLI   USESW,0             SEE IF DOING USER DATA                       
         BE    FORM2               NO                                           
*                                  USER2 = FRMTALPH                             
         GOTO1 USER2,DMCB,(35,REC2),(R7),14,(2,DMWORK)                          
         B     FORMAT3                                                          
*                                                                               
FORM2    GOTO1 USER2,DMCB,(23,REC2),(R7),14,(3,DMWORK)                          
*                                                                               
FORMAT3  LA    R6,DMWORK                                                        
         LA    RF,FLDDATA+1                                                     
         CLI   0(R6),0                                                          
         BE    FRMTEND                                                          
FORMAT4  CLI   0(R6),0                                                          
         BE    FRMTSEND                                                         
         L     R7,0(R6)                                                         
         MVC   0(3,RF),0(R7)       MOVE DATA TO SCREEN LINE                     
         MVI   3(RF),C'/'                                                       
*                                                                               
         CLI   USESW,0             SEE IF DOING USER DATA                       
         BE    FORMAT20            NO                                           
         MVC   4(32,RF),3(R7)                                                   
         B     *+10                                                             
*                                                                               
FORMAT20 MVC   4(20,RF),3(R7)                                                   
         SR    RE,RE               DECREMENT COUNT                              
         IC    RE,0(R6)                                                         
         BCTR  RE,0                                                             
         L     R5,0(R6)                                                         
*                                                                               
         CLI   USESW,0             SEE IF DOING USER DATA                       
         BE    FORMAT30            NO                                           
         LA    R5,35(R5)                                                        
         B     *+8                                                              
FORMAT30 LA    R5,23(R5)                                                        
         ST    R5,0(R6)                                                         
         STC   RE,0(R6)                                                         
         LA    R6,4(R6)            NEXT COLUMN                                  
*                                                                               
         CLI   USESW,0             SEE IF DOING USER DATA                       
         BE    FORMAT40            NO                                           
         LA    RF,40(RF)                                                        
         B     *+8                                                              
FORMAT40 LA    RF,26(RF)                                                        
         B     FORMAT4                                                          
*                                                                               
FRMTSEND FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     FORMAT3                                                          
*                                                                               
FRMTEND  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXIT  OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* SEND FILTER ERROR MESSAGE                                                     
*                                                                               
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)   * EXECUTED *                                   
         LA    RE,3(RE)                                                         
         LA    RF,SINMSG(RE)                                                    
         MVC   0(22,RF),=C'- INVALID FILTER FIELD'                              
         LA    R2,SINIFLTH                                                      
         FOUT  (R2)                                                             
         MVI   ERRAREA,X'FF'                                                    
         B     MODEXIT                                                          
         EJECT                                                                  
GETCLTU  NTR1                                                                   
         MVC   CUSER,SPACES                                                     
         MVC   CUSER(13),=C'* UNDEFINED *'                                      
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERRCD,NOCLTERR                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETCERR                                                          
         LA    R4,REC                                                           
         USING CLTHDRD,R4                                                       
         ST    R4,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R1,CPU1                                                          
         CLI   USESW,1             SET R1 TO CORRECT USERDATA FIELD             
         BE    *+8                 NO                                           
         LA    R1,CPU2                                                          
         OC    0(20,R1),0(R1)      SEE IF DEFINED                               
         BZ    GCX                                                              
         MVC   CUSER,0(R1)                                                      
*                                                                               
GCX      XIT1  1                                                                
*                                                                               
GETCERR  GOTO1 ERROR                                                            
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
DASH     DC    40C'-'                                                           
LINLEN   EQU   88                                                               
*                                                                               
USESW    DS    CL1                                                              
USERDATA DS    0CL40                                                            
USERDAT1 DS    CL20                                                             
USERDAT2 DS    CL20                                                             
CUSER    DS    CL20                                                             
*                                                                               
FLTSW    DS    XL1                 FILTER FIELD SWITCH                          
*                                                                               
         EJECT                                                                  
* SPINFWORK                                                                     
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPINF12   05/31/07'                                      
         END                                                                    
