*          DATA SET SPREPFXSP  AT LEVEL 025 AS OF 12/07/94                      
*          DATA SET SPREPFXDF  AT LEVEL 020 AS OF 12/05/94                      
*PHASE SPFX02P                                                                  
         TITLE 'SPFX02 - CHECK CANADIAN NETWORK RECORDS'                        
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
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
*                                                                               
         EJECT                                                                  
FX10     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         GOTO1 HIGH                                                             
         B     FX20                                                             
*                                                                               
FX15     GOTO1 SEQ                                                              
*                                                                               
FX20     CLC   KEY(2),=X'0D11'                                                  
         BNE   FX100                                                            
         OC    KEY+8(5),KEY+8                                                   
         BNZ   FX15                                                             
* NEED TO READ STATION MASTER RECORD                                            
         XC    STAKEY,STAKEY                                                    
         MVI   STAKEY,C'S'                                                      
         MVI   STAKEY+1,C'N'                                                    
         MVC   STAKEY+2(4),KEY+4   MOVE NETWORK CALL LETTERS                    
         MVI   STAKEY+6,C'N'       SET BAND = MEDIA                             
         MVC   STAKEY+7(2),KEY+2   MOVE AGENCY CODE                             
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',STAKEY,ADSTAT                
*                                                                               
         L     R6,ADSTAT                                                        
         MVC   P(2),KEY+2                                                       
         MVC   P+6(4),KEY+4                                                     
                                                                                
         CLC   STAKEY(9),0(R6)                                                  
         BE    *+10                                                             
         MVC   P+20(9),=C'NOT FOUND'                                            
         GOTO1 REPORT                                                           
         B     FX15                                                             
*                                                                               
FX100    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'0F'                                                       
         MVI   PMED,C'T'                                                        
         CLI   BYTE,X'01'                                                       
         BE    PB10                                                             
         MVI   PMED,C'R'                                                        
         CLI   BYTE,X'02'                                                       
         BE    PB10                                                             
         MVI   PMED,C'N'                                                        
         CLI   BYTE,X'03'                                                       
         BE    PB10                                                             
         MVI   PMED,C'C'                                                        
         CLI   BYTE,X'08'                                                       
         BE    PB10                                                             
         MVI   PMED,C' '                                                        
*                                                                               
PB10     MVC   PAGY(2),BUYALPHA                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(5),WORK                                                     
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PEST,WORK+7                                                      
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PLIN,WORK+7                                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY+14,PLDA,4,=C'N'                                  
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
STAKEY   DS    XL17                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
PMED     DS    CL1                                                              
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PLDA     DS    CL8                                                              
         DS    CL1                                                              
PLELDSP  DS    CL10                                                             
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPREPFXSP 12/07/94'                                      
         END                                                                    
