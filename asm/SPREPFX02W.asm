*          DATA SET SPREPFX02W AT LEVEL 054 AS OF 10/08/99                      
*PHASE SPFX02W                                                                  
*INCLUDE NETWEEK                                                                
         TITLE 'SPFX02W - READ RECORDS AND WRITE TO SP- FILE'                   
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
         BE    FX05                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    FXCL                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
* REQFRST                                                                       
*                                                                               
FX05     DS    0H                                                               
         MVC   WORK(6),=X'FAF0F0F1F3F1'                                         
         GOTO1 =V(NETWEEK),DMCB,WORK,GETDAY,ADDAY                               
         GOTO1 HEXOUT,PARAM,DMCB,P,12                                           
         GOTO1 REPORT                                                           
         B     FXX                                                              
PARAM    DS    6A                                                               
*&&DO                                                                           
         OPEN  (FILEOUT,OUTPUT)     OPEN DATASET                                
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
FX10     DS    0H                                                               
         XC    KEY,KEY                                                          
         USING CLTHDR,R6                                                        
         LA    R6,KEY                                                           
         MVC   CKEYCLT,BCLT                                                     
         MVC   CKEYAM,BAGYMD                                                    
*                                                                               
FX11     GOTO1 HIGH                                                             
*                                                                               
FX14     CLC   KEY(2),KEYSAVE                                                   
         BNE   FXX                                                              
         CLI   KEY+4,0                                                          
         BNE   FXNXT                                                            
*                                                                               
         USING DCLIENT,R3                                                       
         LA    R3,CLTREC                                                        
*** UNPACK CLIENT CODE                                                          
         GOTO1 CLUNPK,DMCB,KEY+2,DCLTC                                          
*** STORE MEDIA CODE                                                            
         MVC   DMEDC,QMED                                                       
*** STORE AGENCY CODE                                                           
         MVC   DAGYC,QAGY                                                       
*** STORE SYSTEM                                                                
         MVI   DSYS,DSPOT                                                       
*** STORING CLIENT NAME                                                         
         L     R6,ADCLT                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         MVC   DCLTNM,CNAME                                                     
**** WRITE TO DATASET                                                           
         PUT   FILEOUT,CLTREC                                                   
         DROP  R3,R6                                                            
*                                                                               
FXNXT    DS    0H                                                               
         MVC   KEY+4(2),=2X'FF'                                                 
         B     FX11                                                             
*&&                                                                             
FXX      DS    0H                                                               
         GOTO1 AENDREQ                                                          
*                                                                               
FXCL     DS    0H                                                               
         CLOSE FILEOUT                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
         LTORG                                                                  
         EJECT                                                                  
* STORAGE                                                                       
*                                                                               
         DS    0D                                                               
RECTOT   DS    PL8                                                              
CLTREC   DS    CL27           CLIENT RECORD TO WRT TO DATASET                   
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=FB,LRECL=27,MACRF=PM,     +        
               BLKSIZE=13500                                                    
*                                                                               
DCLIENT  DSECT                DSECT FOR RECORD TO WRITE TO DATASET              
DCLTC    DS    CL3                                                              
DCLTNM   DS    CL20                                                             
DAGYC    DS    CL2                                                              
DSYS     DS    CL1                                                              
DSPOT    EQU   C'S'           FOR SPOT SYSTEM                                   
DMEDC    DS    C                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPNWSDTL                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054SPREPFX02W10/08/99'                                      
         END                                                                    
