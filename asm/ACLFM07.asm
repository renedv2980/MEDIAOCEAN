*          DATA SET ACLFM07    AT LEVEL 034 AS OF 11/07/96                      
*PHASE T60307A,+0                                                               
         TITLE 'MODULE CONTROLLING WORK-CODES/ANALYSIS ELEMENTS'                
T60307   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM7**                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         EJECT                                                                  
*              BUILD KEY                                                        
         SPACE 3                                                                
         CLI   MODE,BUILDKEY                                                    
         BNE   AN10                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         LA    R2,LOGUNITH                                                      
         GOTO1 ANY                                                              
         MVC   KEY+1(1),LOGUNIT                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         LA    R2,LOGLEDGH                                                      
         GOTO1 ANY                                                              
         MVC   KEY+2(1),LOGLEDG                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         LA    R2,LOGWRKH                                                       
         GOTO1 ANY                                                              
         MVC   WORK(1),KEY         SAVE COMPANY                                 
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(1),WORK                                                    
         MVC   KEY+2(1),LOGUNIT                                                 
         MVC   KEY+3(1),LOGLEDG                                                 
         GOTO1 MOVE                                                             
         CLC   WORK(2),=C'99'      BILLING WORKCODE ?                           
         BE    INVAL               YES                                          
         CLC   WORK(2),=C'**'      ORDER WORKCODE ?                             
         BE    INVAL               YES                                          
         MVC   KEY+4(2),WORK                                                    
         TM    4(R2),X'20'                                                      
         BO    XIT                                                              
         MVI   ANYKEY,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY OR CHANGE WORK-CODE NAME                                 
         SPACE                                                                  
AN10     OI    LOGWRKH+4,X'20'                                                  
         CLI   MODE,DSPLYREC                                                    
         BNE   AN20                                                             
         LA    R2,LOGDESCH                                                      
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         USING ACANALD,R4                                                       
         FOUT  (R2),ACANDESC,15                                                 
         LA    R2,LOGSTATH                                                      
         FOUT  (R2),SPACES,50                                                   
         CLI   ACANLEN,X'18'                                                    
         BL    AN19A                                                            
         LA    R6,BLOCK                                                         
         SR    R0,R0                                                            
         MVC   0(20,R6),SPACES                                                  
         TM    ACANSTAT,X'80'                                                   
         BNO   AN11                                                             
         MVC   0(6,R6),=C'DETAIL'                                               
         MVC   10(2,R6),=C'NO'                                                  
         AH    R0,=H'1'                                                         
         LA    R6,20(R6)                                                        
AN11     MVC   0(20,R6),SPACES                                                  
         CLI   ACANMED,0                                                        
         BE    AN12                                                             
         MVC   0(5,R6),=C'MEDIA'                                                
         MVC   10(1,R6),ACANMED                                                 
         AH    R0,=H'1'                                                         
         LA    R6,20(R6)                                                        
AN12     MVC   0(20,R6),SPACES                                                  
         TM    ACANSTAT,X'40'                                                   
         BNO   AN18                                                             
         MVC   0(8,R6),=C'P1DETAIL'                                             
         MVC   10(2,R6),=C'NO'                                                  
         AH    R0,=H'1'                                                         
         LA    R6,20(R6)                                                        
AN18     MVC   0(20,R6),SPACES                                                  
         TM    ACANSTAT,X'20'                                                   
         BNO   AN18A                                                            
         MVC   0(6,R6),=C'TRAVEL'                                               
         MVC   10(3,R6),=C'YES'                                                 
         AH    R0,=H'1'                                                         
         LA    R6,20(R6)                                                        
AN18A    MVC   0(20,R6),SPACES                                                  
         TM    ACANSTAT,X'10'                                                   
         BNO   AN18C                                                            
         MVC   0(9,R6),=C'ENTERTAIN'                                            
         MVC   10(3,R6),=C'YES'                                                 
         AH    R0,=H'1'                                                         
         LA    R6,20(R6)                                                        
AN18C    MVC   0(20,R6),SPACES                                                  
         TM    ACANSTAT,X'08'                                                   
         BNO   AN18E                                                            
         MVC   0(8,R6),=C'ORIGINAL'                                             
         MVC   10(5,R6),=C'HOURS'                                               
         AH    R0,=H'1'                                                         
         LA    R6,20(R6)                                                        
AN18E    MVC   0(20,R6),SPACES                                                  
         TM    ACANSTAT,X'04'                                                   
         BZ    AN18G                                                            
         MVC   0(8,R6),=C'ESTPRINT'                                             
         MVC   10(2,R6),=C'NO'                                                  
         AH    R0,=H'1'                                                         
         LA    R6,20(R6)                                                        
AN18G    MVC   0(20,R6),SPACES                                                  
         TM    ACANSTAT,X'01'                                                   
         BZ    AN18H                                                            
         MVC   0(8,R6),=C'TRANSFER'                                             
         MVC   10(3,R6),=C'YES'                                                 
         AH    R0,=H'1'                                                         
         LA    R6,20(R6)                                                        
AN18H    MVC   0(20,R6),SPACES                                                  
AN19     LTR   R0,R0                                                            
         BZ    AN19A                                                            
         L     R8,COMFACS                                                       
         USING COMFACSD,R8                                                      
         GOTO1 CUNSCAN,DMCB,((R0),BLOCK),LOGSTATH                               
         SPACE 2                                                                
AN19A    DS    0H                                                               
         LA    R2,LOGDESCH                                                      
*&&US*&& B     XIT                                                              
*&&UK                                                                           
         MVC   LOGXNAM,SPACES      DISPLAY EXTRA NAME                           
         OI    LOGXNAMH+6,X'80'                                                 
         LA    R2,IO2                                                           
         AH    R2,DATADISP                                                      
         SR    R1,R1                                                            
AN19C    CLI   0(R2),0                                                          
         BE    AN19F                                                            
         CLI   0(R2),X'20'                                                      
         BE    AN19E                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     AN19C                                                            
         USING ACNAMED,R2                                                       
AN19E    IC    R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     AN19F                                                            
         MVC   LOGXNAM(0),ACNMNAME                                              
AN19F    LA    R2,LOGDESCH                                                      
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
AN20     LA    R2,LOGDESCH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         CLC   WORK(6),=C'DELETE'                                               
         BE    AN40                                                             
         LA    R4,ELEMENT                                                       
         USING ACANALD,R4                                                       
         MVI   ACANEL,X'12'                                                     
         MVI   ACANLEN,X'18'                                                    
         MVC   ACANCODE,LOGWRK                                                  
         OC    ACANCODE,SPACES                                                  
         MVC   ACANDESC,WORK                                                    
         XC    ACANSTAT(5),ACANSTAT                                             
         LA    R2,LOGSTATH                                                      
         CLI   5(R2),0                                                          
         BE    AN30                                                             
         L     R8,COMFACS                                                       
         USING COMFACSD,R8                                                      
         GOTO1 CSCANNER,DMCB,(R2),(3,BLOCK)                                     
         ZIC   R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BZ    INVAL                                                            
         LA    R6,BLOCK                                                         
AN21     CLC   12(7,R6),=C'DETAIL '                                             
         BNE   AN22                                                             
         CLC   22(3,R6),=C'NO '                                                 
         BNE   INVAL                                                            
         OI    ACANSTAT,X'80'                                                   
         B     AN29                                                             
AN22     CLC   12(6,R6),=C'MEDIA '                                              
         BNE   AN23                                                             
         CLI   1(R6),1                                                          
         BNE   INVAL                                                            
         MVC   ACANMED,22(R6)                                                   
         B     AN29                                                             
AN23     CLC   12(9,R6),=C'P1DETAIL '                                           
         BNE   AN24                                                             
         CLC   22(3,R6),=C'NO '                                                 
         BNE   INVAL                                                            
         OI    ACANSTAT,X'40'                                                   
         B     AN29                                                             
AN24     CLC   12(6,R6),=C'TRAVEL'                                              
         BNE   AN25                                                             
         CLC   22(3,R6),=C'YES'                                                 
         BNE   INVAL                                                            
         OI    ACANSTAT,X'20'                                                   
         B     AN29                                                             
AN25     CLC   12(9,R6),=C'ENTERTAIN'                                           
         BNE   AN26                                                             
         CLC   22(3,R6),=C'YES'                                                 
         BNE   INVAL                                                            
         OI    ACANSTAT,X'10'                                                   
         B     AN29                                                             
AN26     CLC   12(4,R6),=C'ORIGINAL'                                            
         BNE   AN27                                                             
         CLC   22(4,R6),=C'HOURS'                                               
         BNE   INVAL                                                            
         OI    ACANSTAT,X'08'                                                   
         B     AN29                                                             
AN27     CLC   12(8,R6),=C'ESTPRINT'                                            
         BNE   AN28                                                             
         CLC   22(2,R6),=C'NO'                                                  
         BNE   INVAL                                                            
         OI    ACANSTAT,X'04'                                                   
         B     AN29                                                             
AN28     CLC   12(8,R6),=C'TRANSFER'                                            
         BNE   INVAL                                                            
         CLC   22(3,R6),=C'YES'                                                 
         BNE   INVAL                                                            
         OI    ACANSTAT,X'01'                                                   
         B     AN29                                                             
AN29     LA    R6,32(R6)                                                        
         BCT   R3,AN21                                                          
         SPACE 1                                                                
AN30     GOTO1 REMANEL,DMCB,(X'12',0)                                           
         GOTO1 ADDANEL                                                          
*&&UK                                                                           
         LA    R2,LOGXNAMH         HANDLE OPTIONAL EXTRA NAME                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 NAMIN                                                            
*&&                                                                             
         B     XIT                                                              
         EJECT                                                                  
AN40     LR    R3,RA                                                            
         USING TWAD,R3                                                          
         CLI   TWAOFFC,C'*'                                                     
         BNE   INVAL                                                            
         LA    RF,IO2                                                           
         OI    44(RF),X'80'                                                     
         SPACE 2                                                                
XIT      XIT1  REGS=(R2)                                                        
         SPACE 1                                                                
INVAL    MVI   ERROR,INVALID                                                    
         B     XIT                                                              
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMF8D                                                       
         SPACE 2                                                                
*ACLFMWORK                                                                      
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
*ACLFMEQU                                                                       
*DDFLDIND                                                                       
*FATWA                                                                          
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATWA                                                          
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034ACLFM07   11/07/96'                                      
         END                                                                    
