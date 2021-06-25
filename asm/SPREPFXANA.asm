*          DATA SET SPREPFXANA AT LEVEL 047 AS OF 10/07/98                      
*PHASE SPFX025                                                                  
         TITLE 'SPFX02 - PRINT CLIENTS IN OFFICE Z'                             
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
         BE    REQF                                                             
*        CLI   MODE,CLTFRST                                                     
*        BE    CLTF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* REQFRST                                                                       
REQF     DS    0H                                                               
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(4),=X'00110001' WIMC MEDIA T - FIRST CLIENT                  
*                                                                               
REQF10   GOTO1 HIGH                                                             
REQF15   CLC   KEY(2),=X'0012'                                                  
         BH    REQFX                                                            
         OC    KEY+4(9),KEY+4      CLT REC                                      
         BZ    REQF20                                                           
         GOTO1 SEQ                                                              
         B     REQF15                                                           
*                                                                               
REQF20   GOTO1 GETBUY                                                           
         USING CLTHDR,R3                                                        
         L     R3,ADBUY                                                         
         CLI   COFFICE,C'Z'                                                     
         BNE   REQF30                                                           
         MVI   P+8,C'T'                                                         
         CLI   KEY+1,X'11'                                                      
         BE    *+8                                                              
         MVI   P+8,C'R'                                                         
         GOTO1 CLUNPK,DMCB,CKEYCLT,P+10                                         
         GOTO1 REPORT                                                           
         DROP  R3                                                               
*                                                                               
REQF30   MVI   KEY+4,X'FF'         FORCE NEXT CLT                               
         B     REQF10                                                           
*                                                                               
REQFX    GOTO1 AENDREQ                                                          
         B     EXIT                                                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
REQL     MVC   P(17),=C'NUMBER OF RECORDS'                                      
         EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
NETNUM   DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
ELCODE   DS    X                                                                
CHANGED  DS    X                                                                
COUNT    DS    F                                                                
TEMP     DS    CL80                                                             
*                                                                               
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PIND2    DS    CL1                                                              
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047SPREPFXANA10/07/98'                                      
         END                                                                    
