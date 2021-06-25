*          DATA SET ACLFM09    AT LEVEL 030 AS OF 05/01/02                      
*PHASE T60309A,+0                                                               
*INCLUDE SCANNER                                                                
*INCLUDE UNSCAN                                                                 
         TITLE 'MODULE TO HANDLE PRODUCT RECORD'                                
T60309   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM9**                                                       
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T60309+4096,R7                                                   
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         LA    R8,SAVECOMP                                                      
         USING ACCOMPD,R8                                                       
         LA    R9,SAVEHEIR                                                      
         USING ACHEIRD,R9                                                       
         EJECT                                                                  
*              BUILD KEY FOR PRODUCT                                            
         SPACE 3                                                                
         CLI   MODE,BUILDKEY                                                    
         BNE   PROD10                                                           
         BAS   RE,ANYCOMP                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPJOB                                                 
         LA    R2,LOGCLIH                                                       
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   KEY+3(12),WORK                                                   
         CLC   5(1,R2),ACHRLEVA                                                 
         BNH   PROD2                                                            
         MVI   ERROR,ACTOOLNG                                                   
         B     XIT                                                              
         SPACE 2                                                                
PROD2    GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         TM    4(R2),X'20'                                                      
         BO    PROD4                                                            
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,C'Y'                                                      
         SPACE 2                                                                
PROD4    LA    R2,LOGPRODH                                                      
         GOTO1 ANY                                                              
         CLI   8(R2),C' '          DOES PRODUCT START WITH A BLANK ?            
         BNE   PROD5               NO                                           
         MVI   ERROR,INVALID       YES, ERROR                                   
         B     XIT                                                              
         SPACE 2                                                                
PROD5    SR    R3,R3                                                            
         IC    R3,ACHRLEVA                                                      
         LA    R5,KEY+3(R3)                                                     
         SR    R1,R1                                                            
         IC    R1,ACHRLEVB                                                      
         SR    R1,R3                                                            
         LR    R3,R1                                                            
         STC   R3,WORK                                                          
         CLC   5(1,R2),WORK                                                     
         BNH   PROD6                                                            
         MVI   ERROR,ACTOOLNG                                                   
         B     XIT                                                              
         SPACE 2                                                                
PROD6    GOTO1 MOVE                                                             
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK                                                     
         TM    4(R2),X'20'                                                      
         BO    XIT                                                              
         MVI   ANYKEY,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY PRODUCT RECORD                                           
         SPACE 3                                                                
PROD10   OI    LOGPRODH+4,X'20'                                                 
         CLI   MODE,DSPLYREC                                                    
         BNE   PROD20                                                           
         LA    R2,LOGNMDSH                                                      
         GOTO1 NAMOUT                                                           
         LA    R2,LOGPNAMH                                                      
         BAS   RE,HANDLPRF                                                      
         BAS   RE,HANDLEX                                                       
         OI    4(R2),X'20'                                                      
*&&US                                                                           
         XC    LOGSALE,LOGSALE     CLEAR CODE                                   
         OI    LOGSALEH+6,X'80'    AND TRANSMIT                                 
         XC    LOGSALN,LOGSALN     CLEAR NAME                                   
         OI    LOGSALNH+6,X'80'    AND TRANSMIT                                 
         LA    R4,IO2              DISPLAY SALES ANALYSIS ELEMENT               
         AH    R4,DATADISP                                                      
         SR    R5,R5                                                            
PROD12   CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),X'3D'                                                      
         BE    PROD14                                                           
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     PROD12                                                           
         SPACE 1                                                                
         USING ACSAND,R4                                                        
PROD14   MVC   LOGSALE(12),ACSACODE+3                                           
         MVC   KEY(15),ACSACODE                                                 
         LA    R2,LOGSALEH                                                      
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
         LA    R2,LOGPNAMH                                                      
*&&                                                                             
         B     XIT                                                              
         EJECT                                                                  
*              BUILD PRODUCT RECORD                                             
         SPACE 3                                                                
PROD20   LA    R2,LOGPNAMH                                                      
         GOTO1 ANY                                                              
         CLI   LOGACT,C'A'         FOR ACTION AMEND, SAVE NAME                  
         BNE   PROD20A                                                          
         GOTO1 CHKNAM,DMCB,(C'B',IO),NAMESAVE                                   
         LA    RF,NAMESAVE-LOCALS  SAVE DISP TO SAVED NAME                      
         STCM  RF,3,DSAVNAM        FOR SEARCH                                   
*                                                                               
PROD20A  GOTO1 NAMIN                                                            
         BAS   RE,HANDLPRF                                                      
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
         BAS   RE,HANDLEX                                                       
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
**T                                                                             
         CLI   ACCEMU,C'Y'         IS IT THE NEW FILE                           
         BNE   PROD21                                                           
         CLI   LOGACT,C'A'         MUST BE AMEND                                
         BNE   PROD21                                                           
         TM    4(R2),X'20'         HAS NAME CHANGED                             
         BO    PROD21                                                           
         GOTO1 CHKNAM,DMCB,(C'A',IO2),NAMESAVE                                  
PROD21   DS    0H                                                               
**T                                                                             
*&&US                                                                           
         GOTO1 REMANEL,DMCB,(X'3D',0)                                           
         LA    R2,LOGSALEH                                                      
         CLI   5(R2),0                                                          
         BE    PROD22                                                           
         LA    R4,ELEMENT                                                       
         USING ACSAND,R4                                                        
         MVC   ACSAEL(2),=X'3D35'                                               
         MVC   ACSACODE(1),COMPANY                                              
         MVC   ACSACODE+1(2),ACMPJOB                                            
         GOTO1 MOVE                                                             
         MVC   ACSACODE+3(12),WORK                                              
         MVC   KEY(15),ACSACODE                                                 
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),4             MUST HAVE 3 FOR CLI AND AT LEAST             
         BL    XIT                 1 FOR PRODUCT                                
         MVI   ERROR,X'FF'                                                      
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
         MVC   ACSANAME,LOGSALN                                                 
         OC    ACSANAME,SPACES                                                  
         GOTO1 ADDANEL                                                          
PROD22   LA    R2,LOGPNAMH                                                      
*&&                                                                             
         GOTO1 STATIN                                                           
         SPACE 2                                                                
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
         DROP  R8                                                               
       ++INCLUDE ACLFM089A                                                      
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMF6D                                                       
SAVECOMP DS    CL64                                                             
SAVEHEIR DS    CL66                                                             
THISPROF DS    CL255                                                            
THISADD  DS    CL105                                                            
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
SAVEADDR DS    CL107                                                            
SAVEPROF DS    CL255                                                            
SAVELEM  DS    CL255                                                            
LOGGRUPH DS    CL8                 DUMMY                                        
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACLFM09   05/01/02'                                      
         END                                                                    
