*          DATA SET PRSFM1B    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T41C1BA                                                                  
         TITLE 'PRSFM1B USERFIELD LIST'                                         
*                                                                               
T41C1B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41C1B*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   EXIT                                                             
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         B     LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VALIDATE KEY ROUTINE                                                   
*                                                                               
VK       XC    MYKEY,MYKEY                                                      
         LA    R2,SUSMEDH          MEDIA                                        
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         LA    R2,SUSCLTH                                                       
         XC    QCLT,QCLT                                                        
         CLI   5(R2),0             CLIENT                                       
         BE    VK10                                                             
         GOTO1 VALICLT                                                          
*                                                                               
VK10     LA    R4,KEY              BUILD KEY                                    
         USING CLTRECD,R4                                                       
         XC    PCLTKEY,PCLTKEY                                                  
         MVC   PCLTKAGY,AGENCY     AGENCY                                       
         MVC   PCLTKMED,QMED       MEDIA                                        
         MVI   PCLTKRCD,X'02'      RECORD CODE                                  
         MVC   PCLTKCLT,QCLT       CLIENT                                       
         MVC   MYKEY,KEY                                                        
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        LIST RECORDS                                                           
*                                                                               
         USING LISTD,R2                                                         
LR       OI    GLSTSTAT,RETEXTRA                                                
         LA    R2,LISTAR                                                        
         CLI   MODE,LISTRECS                                                    
         BE    *+8                                                              
         LA    R2,P                                                             
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   *+10                                                             
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR10     GOTO1 SEQ                                                              
*                                                                               
LR20     CLC   KEY(4),KEYSAVE   SAME MEDIA/RECORD TYPE                          
         BNE   LRX                                                              
         L     R4,AIO                                                           
         USING PCLTUDEF,R4                                                      
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR10                                                             
*                                                                               
         OC    PCLTPU1,PCLTPU1                                                  
         BZ    LR70                                                             
         MVC   LSTDSC1,PCLTPU1                                                  
*                                                                               
LR70     OC    PCLTPU2,PCLTPU2                                                  
         BZ    LR80                                                             
         MVC   LSTDSC2,PCLTPU2                                                  
*                                                                               
LR80     OC    PCLTEU1,PCLTEU1                                                  
         BZ    LR90                                                             
         MVC   LSTDSC3,PCLTEU1                                                  
*                                                                               
LR90     OC    PCLTEU2,PCLTEU2                                                  
         BZ    LR100                                                            
         MVC   LSTDSC4,PCLTEU2                                                  
*                                                                               
LR100    L     R4,AIO                                                           
         USING CLTRECD,R4                                                       
         MVC   LSTMED,PCLTKMED     MEDIA                                        
         MVC   LSTCLT,PCLTKCLT     CLIENT                                       
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BNE   LR110                                                            
         GOTO1 LISTMON                                                          
         B     LR120                                                            
*                                                                               
LR110    GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR120    MVC   0(L'SPACES,R2),SPACES                                            
         B     LR10                                                             
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,60,REQUESTOR                                                  
         SSPEC H2,60,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'USER DEFINITION LIST'                                    
         SSPEC H2,30,C'--------------------'                                    
         SPACE 1                                                                
         SSPEC H4,1,C'M'                                                        
         SSPEC H4,3,C'CLT'                                                      
         SSPEC H4,8,C'PRD DESC 1  TYP'                                          
         SSPEC H4,26,C'PRD DESC 2  TYP'                                         
         SSPEC H4,44,C'EST DESC 1  TYP'                                         
         SSPEC H4,62,C'EST DESC 2  TYP'                                         
         SPACE 1                                                                
         SSPEC H5,1,C'-'                                                        
         SSPEC H5,3,C'---'                                                      
         SSPEC H5,8,C'----------  ---'                                          
         SSPEC H5,26,C'----------  ---'                                         
         SSPEC H5,44,C'----------  ---'                                         
         SSPEC H5,62,C'----------  ---'                                         
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMEBD                                                       
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
         SPACE 3                                                                
         ORG  SYSSPARE                                                          
MYKEY    DS   CL(L'KEY)                                                         
         EJECT                                                                  
CLTRECD  DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
*                                                                               
LISTD    DSECT                                                                  
LSTMED   DS    CL1                 MEDIA                                        
         DS    CL1                                                              
LSTCLT   DS    CL3                 CLIENT                                       
         DS    CL1                                                              
LSTDSC1  DS    CL16                PRODUCT1 DESCRIPTION                         
         DS    CL2                                                              
LSTDSC2  DS    CL16                PRODUCT2 DESCRIPTION                         
         DS    CL2                                                              
LSTDSC3  DS    CL16                ESTIMATE1 DESCRIPTION                        
         DS    CL2                                                              
LSTDSC4  DS    CL15                ESTIMATE2 DESCRIPTION                        
         EJECT                                                                  
*       ++INCLUDE DDSPLWORKD                                                    
*       ++INCLUDE DDSPOOLD                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PRSFM1B   05/01/02'                                      
         END                                                                    
