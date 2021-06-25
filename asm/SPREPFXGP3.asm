*          DATA SET SPREPFXGP3 AT LEVEL 010 AS OF 09/03/98                      
*PHASE SPFX02B,+0                                                               
         TITLE 'SPFX02 - WILA 0E01 FIX'                                         
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
* CLTFRST - READ 0E01 RECS                                                      
*                                                                               
FX10     DS    0H                                                               
**************************************************************                  
*                                  DATE TEST****                                
         MVC   WORK(3),=X'010101'                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK+3)                                  
         GOTO1 DATCON,DMCB,(0,WORK+3),(3,WORK+9)                                
         GOTO1 HEXOUT,DMCB,WORK,P,12,=C'N'                                      
         GOTO1 REPORT                                                           
*                                                                               
         MVC   WORK(3),=AL1(05,01,01)                                           
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK+3)                                  
         GOTO1 DATCON,DMCB,(0,WORK+3),(3,WORK+9)                                
         GOTO1 HEXOUT,DMCB,WORK,P,12,=C'N'                                      
         GOTO1 REPORT                                                           
*                                                                               
         MVC   WORK(3),=AL1(25,01,01)                                           
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK+3)                                  
         GOTO1 DATCON,DMCB,(0,WORK+3),(3,WORK+9)                                
         GOTO1 HEXOUT,DMCB,WORK,P,12,=C'N'                                      
         GOTO1 REPORT                                                           
*                                                                               
         MVC   WORK(3),=AL1(35,01,01)                                           
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK+3)                                  
         GOTO1 DATCON,DMCB,(0,WORK+3),(3,WORK+9)                                
         GOTO1 HEXOUT,DMCB,WORK,P,12,=C'N'                                      
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
**************************************************************                  
         MVI   DMINBTS,X'08'       ALLOW DELETES                                
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING STABUCKD,R2                                                      
         MVC   STABKCOD,=X'0E01'                                                
         MVC   STABKAM,BAGYMD                                                   
         MVC   STABKCLT,BCLT                                                    
*                                                                               
FX11     GOTO1 HIGH                                                             
         B     FX13                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX13     CLC   KEY(5),KEYSAVE                                                   
         BNE   FX30                                                             
*                                                                               
         CLI   STABKCUR,0          ONLY PW RECS                                 
         BE    FX12                                                             
*                                                                               
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
         L     R4,AREC                                                          
         LA    R4,24(R4)                                                        
*                                                                               
FX14     DS    0H                                                               
         CLI   0(R4),X'0E'         NORMAL ELEMENTS ONLY                         
         BE    FX12                PRESERVE RECORD                              
         CLI   0(R4),0             EOR                                          
         BE    FX20                KILL IT                                      
*                                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,0                                                             
         B     FX14                                                             
*                                                                               
FX20     DS    0H                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+2,20,=C'N'                                     
         GOTO1 REPORT                                                           
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
         B     FX12                                                             
*                                                                               
FX30     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
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
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREPFXGP309/03/98'                                      
         END                                                                    
