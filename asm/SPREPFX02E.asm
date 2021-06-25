*          DATA SET SPREPFX02E AT LEVEL 014 AS OF 07/03/01                      
*PHASE SPFX02U                                                                  
         TITLE 'SPFX02 - LOOK AT BILL HEADER RECORDS'                           
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
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
* REQFRST                                                                       
*                                                                               
FX10     DS    0H                                                               
*                                                                               
         SR    R6,R6                                                            
*                                                                               
         XC    KEY,KEY                                                          
         GOTO1 HIGH                                                             
         B     FX14                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX14     CLI   KEY,0               MUST BE HEADER RECORD                        
         BNE   FXX                                                              
         CLI   KEY+1,0             A/M MUST BE THERE                            
         BE    FX12                                                             
         OC    KEY+8(5),KEY+8      BILL HEADER?                                 
         BZ    FX12                                                             
         CLC   =C'AAA',KEY+4       PRODUCT AAA (IMPOSSIBLE!)                    
         BNE   FX12                                                             
*                                                                               
         MVC   P(3),=C'DA='                                                     
         GOTO1 HEXOUT,DMCB,KEY+14,P+3,4,=C'TOG'                                 
         GOTO1 REPORT                                                           
*                                                                               
         MVC   AREC,ADBILL                                                      
         GOTO1 GET                                                              
         L     R3,AREC                                                          
         SR    R0,R0                                                            
         ICM   R0,3,13(R3)         RECORD LENGTH                                
         GOTO1 PRNTBL,DMCB,0,(R3),C'DUMP',(R0),=C'1D'                           
         GOTO1 REPORT                                                           
*                                                                               
         B     FX12                                                             
*                                                                               
FXX      GOTO1 AENDREQ                                                          
         B     EXIT                                                             
         EJECT                                                                  
* STORAGE                                                                       
*                                                                               
         DS    0D                                                               
RECTOT   DS    PL8                                                              
*                                                                               
IO       DS    2000X                                                            
*                                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPFX02E07/03/01'                                      
         END                                                                    
