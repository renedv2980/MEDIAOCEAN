*          DATA SET ACLFM01    AT LEVEL 010 AS OF 08/10/00                      
*PHASE T60301A                                                                  
*INCLUDE HEXIN                                                                  
         TITLE 'MODULE FOR UNIT RECORDS'                                        
T60301   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM1**                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         EJECT                                                                  
*              CONTROL MODE SETTINGS                                            
         SPACE 3                                                                
         MVI   ERROR,X'FF'                                                      
         CLI   MODE,BUILDKEY       BUILD A KEY                                  
         BNE   UN2                                                              
         LA    R2,LOGCOMPH                                                      
         GOTO1 ANY                                                              
         MVC   WORK(1),LOGCOMP                                                  
         CLI   5(R2),1                                                          
         BE    UN1                                                              
         GOTO1 =V(HEXIN),DMCB,LOGCOMP,WORK,2,RR=RB                              
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   UN1                                                              
         MVI   ERROR,2                                                          
         B     XIT                                                              
         SPACE 1                                                                
UN1      EQU   *                                                                
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),WORK                                                      
         GOTO1 READ                                                             
         LA    R2,LOGUNITH                                                      
         MVC   KEY+1(1),LOGUNIT                                                 
         GOTO1 ANY                                                              
         TM    4(R2),X'20'                                                      
         BO    XIT                                                              
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,C'Y'                                                      
         B     XIT                                                              
         SPACE 2                                                                
UN2      CLI   MODE,DSPLYREC                                                    
         BNE   UN4                                                              
         LA    R2,LOGNMDSH                                                      
         GOTO1 NAMOUT                                                           
         LA    R2,LOGADD1H                                                      
         GOTO1 ADDROUT                                                          
         LA    R2,LOGUNAMH                                                      
         B     XIT                                                              
         SPACE 2                                                                
UN4      LA    R2,LOGUNAMH                                                      
         GOTO1 ANY                                                              
         GOTO1 NAMIN                                                            
         LA    R2,LOGADD1H                                                      
         GOTO1 ADDRIN                                                           
         SPACE 2                                                                
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMFED                                                       
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACLFM01   08/10/00'                                      
         END                                                                    
