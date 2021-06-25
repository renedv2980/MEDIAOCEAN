*          DATA SET SPREPFXGP2 AT LEVEL 015 AS OF 02/06/98                      
*PHASE SPFX02B,+0                                                               
         TITLE 'SPFX02 - FIX CANADIAN NETWORK B2 PROFILE'                       
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
FX10     DS    0H                                                               
         MVI   FCRDBUYS,C'N'                                                    
         XC    LASTAGY,LASTAGY                                                  
*                                                                               
         L     R7,ADBUY                                                         
         XC    0(256,R7),0(R7)                                                  
         USING CTUREC,R7                                                        
*                                                                               
         MVI   CTUKTYP,C'U'                                                     
         MVI   CTUKSYS,C'S'                                                     
         MVC   CTUKPROG+1(2),=C'B2'                                             
         MVC   KEYSAVE,0(R7)                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',(R7),(R7),0                   
         B     FX11B                                                            
*                                                                               
FX11     DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'CTFILE',(R7),(R7),0                   
*                                                                               
FX11B    DS    0H                                                               
         CLC   KEYSAVE(15),0(R7)                                                
         BNE   EXIT                                                             
         CLI   CTUKAGY,0                                                        
         BE    FX11                                                             
         CLI   QOPT1,C'Y'          ONE AGENCY ONLY?                             
         BNE   FX11D                                                            
         CLC   QAGY,CTUKAGY                                                     
         BNE   FX11                                                             
*                                                                               
FX11D    DS    0H                                                               
         LA    R6,CTUDATA                                                       
*                                                                               
FX12     DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    FX11                                                             
         CLI   0(R6),X'72'                                                      
         BE    FX20                                                             
*                                                                               
FX12D    DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     FX12                                                             
*                                                                               
FX20     DS    0H                                                               
         CLI   16(R6),0            SKIPIF PROF BYTE = 0                         
         BE    FX11                                                             
         CLI   QOPT2,C'Y'          PRINT RESULTS                                
         BNE   FX22                                                             
         MVC   P(2),CTUKAGY                                                     
         MVC   P+3(1),CTUKMED                                                   
         CLI   CTUKMED,C' '                                                     
         BH    *+10                                                             
         MVC   P+3(3),=C'ALL'                                                   
         MVC   P+7(3),CTUKCLT                                                   
         CLI   CTUKCLT,C' '                                                     
         BH    *+10                                                             
         MVC   P+7(3),=C'ALL'                                                   
*                                                                               
         LA    R4,16(R6)                                                        
         NI    0(R4),X'F0'                                                      
         ZIC   R3,0(R4)                                                         
         SRL   R3,4                                                             
         LA    R3,NEWLIST(R3)                                                   
         GOTO1 HEXOUT,DMCB,(R4),P+13,1,=C'N'                                    
         GOTO1 (RF),(R1),(R3),P+16                                              
         GOTO1 REPORT                                                           
*                                                                               
FX22     DS    0H                                                               
         B     FX11                                                             
*                                                                               
NEWLIST  DC    X'0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFFF'                              
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTELX                                                          
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
* CONSTANTS                                                                     
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
ELCODE   DS    CL1                                                              
LASTAGY  DS    CL2                                                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREPFXGP202/06/98'                                      
         END                                                                    
