*          DATA SET SPCBCHK    AT LEVEL 008 AS OF 11/01/93                      
*PHASE SPFX02B,+0                                                               
         TITLE 'SPFX02 - FIND STATION LEVEL CANADIAN BILL BUCKETS'              
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
         CLI   MODE,CLTFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
* CLTFRST - READ ALL BILL RECORDS                                               
*                                                                               
FX10     XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING STABUCKD,R2                                                      
         MVC   0(2,R2),=X'0E01'                                                 
         MVC   2(1,R2),BAGYMD                                                   
         MVC   3(2,R2),BCLT                                                     
*                                                                               
         XC    HALF,HALF                                                        
*                                                                               
FX11     GOTO1 HIGH                                                             
         B     FX13                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX13     CLC   KEY(5),KEYSAVE                                                   
         BNE   FX20                                                             
*                                                                               
         OC    STABKMKT,STABKMKT   ANY MARKET?                                  
         BZ    FX12                                                             
*                                                                               
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
         L     R3,AREC                                                          
         LA    R3,STABELEM-STABUCKD(R3)                                         
         USING STABELEM,R3                                                      
*                                                                               
FX14     DS    0H                                                               
         CLI   0(R3),0             EOR                                          
         BE    FX15                                                             
         CLC   STABPER,HALF                                                     
         BNH   *+10                                                             
         MVC   HALF,STABPER                                                     
*                                                                               
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FX14                                                             
*                                                                               
FX15     DS    0H                                                               
         B     FX12                                                             
*                                                                               
FX20     DS    0H                                                               
         MVC   P(3),CLT                                                         
         GOTO1 DATCON,DMCB,(3,HALF),DUB                                         
         MVC   P+5(4),DUB                                                       
         GOTO1 REPORT                                                           
*                                                                               
         MVI   MODE,CLTLAST                                                     
         B     EXIT                                                             
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
         EJECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPCBCHK   11/01/93'                                      
         END                                                                    
