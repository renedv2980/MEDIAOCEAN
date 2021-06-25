*          DATA SET SPREPFXAN2 AT LEVEL 043 AS OF 03/23/98                      
*PHASE SPFX025                                                                  
         TITLE 'SPFX02 - COUNT CABLE STATION RECS'                              
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
FX10     DS    0H                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         GOTO1 HIGHSTA                                                          
         B     FX20                                                             
FX15     GOTO1 SEQSTA                                                           
*                                                                               
         USING STAREC,R3                                                        
FX20     L     R3,ADSTAT                                                        
         CLI   0(R3),C'S'                                                       
         BNE   FX100                                                            
*        CLC   STAKAGY,=C'AG'      ACBO                                         
*        BNE   FX15                                                             
         CLI   STAKCALL,C'Z'                                                    
         BNH   FX15                                                             
         L     RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         ST    RE,COUNT                                                         
         B     FX15                                                             
*                                                                               
FX100    MVC   P(17),=C'NUMBER OF RECORDS'                                      
         EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
SAVEKEY  DS    XL13                                                             
SAVEKEY2 DS    XL18                                                             
SVKEY    DS    XL10                AM(1),CLT(2),PRD(1),MKSTA(5),EST(1)          
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
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
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043SPREPFXAN203/23/98'                                      
         END                                                                    
