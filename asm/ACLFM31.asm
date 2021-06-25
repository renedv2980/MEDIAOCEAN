*          DATA SET ACLFM31    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T60331A,+0                                                               
         TITLE 'MODULE TO HANDLE MEDIA INTERFACE DATA'                          
T60331   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LF31**                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         EJECT                                                                  
*              BUILD KEY                                                        
*                                                                               
         LA    R2,LOGCODEH                                                      
         CLI   MODE,BUILDKEY                                                    
         BNE   MI10                                                             
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'08'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),2             MUST BE 2 BYTE MEDIA CODE                    
         BNE   XIT                                                              
         MVC   KEY+2(2),LOGCODE                                                 
         MVI   ERROR,X'FF'                                                      
         TM    LOGCODEH+4,X'20'                                                 
         BO    XIT                                                              
         MVI   ANYKEY,C'Y'                                                      
         OI    LOGCODEH+4,X'20'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
*                                                                               
MI10     CLI   MODE,DSPLYREC                                                    
         BNE   MI30                                                             
         TWAXC LOGDESCH                                                         
         MVC   LOGCOMN,SPACES                                                   
         OI    LOGCOMNH+6,X'80'                                                 
         MVC   LOGCSDN,SPACES                                                   
         OI    LOGCSDNH+6,X'80'                                                 
         MVC   LOGLSDN,SPACES                                                   
         OI    LOGLSDNH+6,X'80'                                                 
         MVC   LOGCSRN,SPACES                                                   
         OI    LOGCSRNH+6,X'80'                                                 
         MVC   LOGCCRN,SPACES                                                   
         OI    LOGCCRNH+6,X'80'                                                 
         MVC   LOGMEDN,SPACES                                                   
         OI    LOGMEDNH+6,X'80'                                                 
         MVC   LOGONLN,SPACES                                                   
         OI    LOGONLNH+6,X'80'                                                 
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
*                                                                               
MI11     CLI   0(R4),ACMIELQ                                                    
         BE    MI13                                                             
         CLI   0(R4),0                                                          
         BE    XIT                                                              
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     MI11                                                             
*                                                                               
MI13     DS    0H                                                               
         USING ACMID,R4                                                         
         MVC   LOGDESC,ACMIDESC    DESCRIPTION                                  
         MVC   KEY,SPACES                                                       
*                                                                               
         MVC   LOGCOM,ACMICOMM     COMMISSION ACCOUNT                           
         LA    R2,LOGCOMH                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACMICOMM                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
*                                                                               
         CLI   ACMICSHD,0                                                       
         BE    MI15                                                             
         MVC   LOGCSD,ACMICSHD    C.D. ACCOUNT                                  
         LA    R2,LOGCSDH                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACMICSHD                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
*                                                                               
MI15     CLI   ACMILOST,0                                                       
         BE    MI20                                                             
         MVC   LOGLSD,ACMILOST    LOST CASH DISCOUNT                            
         LA    R2,LOGLSDH                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACMILOST                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
*                                                                               
MI20     CLI   ACMICSHR,0                                                       
         BE    MI22                                                             
         MVC   LOGCSR,ACMICSHR    CASH RECEIPTS                                 
         LA    R2,LOGCSRH                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACMICSHR                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
*                                                                               
MI22     CLI   ACMICCSR,0                                                       
         BE    MI25                                                             
         MVC   LOGCCR,ACMICCSR    CANADIAN CASH RECEIPTS                        
         LA    R2,LOGCCRH                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACMICCSR                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
*                                                                               
MI25     CLI   ACMICNTL,0                                                       
         BE    MI26                                                             
         MVC   LOGMED,ACMICNTL    MEDIA CONTROL                                 
         LA    R2,LOGMEDH                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACMICNTL                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
*                                                                               
MI26     CLI   ACMICONL,0                                                       
         BE    MI28                                                             
         MVC   LOGONL,ACMICONL    COMMISSION ONLY ACCOUNT                       
         LA    R2,LOGONLH                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACMICONL                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
*                                                                               
MI28     MVC   LOGANL,ACMICOST    COST ANALYSIS ACCOUNT                         
*                                                                               
         LA    R2,LOGDESCH                                                      
         B     XIT                                                              
         EJECT                                                                  
*              BUILD (OR AMEND) ELEMENT                                         
*                                                                               
MI30     LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING ACMID,R4                                                         
         MVI   ACMIEL,ACMIELQ                                                   
         MVI   ACMILEN,ACMILNQ                                                  
         MVC   ACMICODE,LOGCODE                                                 
         OC    ACMICODE,SPACES                                                  
*                                                                               
         LA    R2,LOGDESCH         GET MEDIA DESCRIPTION                        
         GOTO1 ANY                                                              
         OC    LOGDESC,SPACES                                                   
         MVC   ACMIDESC,LOGDESC                                                 
*                                                                               
         LA    R2,LOGCOMH         VALIDATE COMMISSION ACCOUNT                   
         GOTO1 ANY                                                              
         OC    LOGCOM,SPACES                                                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGCOM                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL            FIND BALANCE ELEMENT                         
         MVC   ACMICOMM,KEY+1                                                   
*                                                                               
         LA    R2,LOGCSDH         VALIDATE CASH DISCOUNT ACCOUNT                
         CLI   5(R2),0                                                          
         BE    MI33                                                             
         OC    LOGCSD,SPACES                                                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGCSD                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL            FIND BALANCE ELEMENT                         
         MVC   ACMICSHD,KEY+1                                                   
*                                                                               
MI33     LA    R2,LOGLSDH         VALIDATE LOST CASH DISCOUNT ACCOUNT           
         CLI   5(R2),0                                                          
         BE    MI34                                                             
         OC    LOGLSD,SPACES                                                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGLSD                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL            FIND BALANCE ELEMENT                         
         MVC   ACMILOST,KEY+1                                                   
*                                                                               
MI34     LA    R2,LOGCSRH         VALIDATE CASH RECEIPTS ACCOUNT                
         CLI   5(R2),0                                                          
         BE    MI35                                                             
         OC    LOGCSR,SPACES                                                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGCSR                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL            FIND BALANCE ELEMENT                         
         MVC   ACMICSHR,KEY+1                                                   
*                                                                               
MI35     LA    R2,LOGCCRH        CANADIAN CASH RECEIPTS ACCOUNT                 
         CLI   5(R2),0                                                          
         BE    MI36                                                             
         OC    LOGCCR,SPACES                                                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGCCR                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL            FIND BALANCE ELEMENT                         
         MVC   ACMICCSR,KEY+1                                                   
*                                                                               
MI36     LA    R2,LOGMEDH         VALIDATE MEDIA CONTROL ACCOUNT                
         CLI   5(R2),0                                                          
         BE    MI37                                                             
         OC    LOGMED,SPACES                                                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGMED                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL            FIND BALANCE ELEMENT                         
         MVC   ACMICNTL,KEY+1                                                   
*                                                                               
MI37     LA    R2,LOGONLH         VALIDATE COMMISSION ONLY                      
         CLI   5(R2),0                                                          
         BE    MI39                                                             
         OC    LOGONL,SPACES                                                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGONL                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL            FIND BALANCE ELEMENT                         
         MVC   ACMICONL,KEY+1                                                   
*                                                                               
MI39     LA    R2,LOGANLH          COST ANALYSIS                                
         CLI   5(R2),0                                                          
         BE    MI40                                                             
         OC    LOGANL,SPACES                                                    
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),=C'11'                                                  
         MVC   KEY+3(L'LOGANL),LOGANL                                           
         GOTO1 READ                                                             
         BAS   RE,BALEL            FIND BALANCE ELEMENT                         
         MVC   KEY+1(2),=C'12'                                                  
         GOTO1 READ                                                             
         BAS   RE,BALEL            FIND BALANCE ELEMENT                         
         MVC   ACMICOST,LOGANL                                                  
*                                                                               
MI40     DS    0H                                                               
         GOTO1 REMANEL,DMCB,(X'19',0)                                           
         GOTO1 ADDANEL                                                          
         LA    R2,LOGCODEH                                                      
         MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              FIND BALANCE ELEMENT                                             
*                                                                               
         USING ACKEYD,R3                                                        
BALEL    LA    R3,IO                                                            
         LA    R3,ACRECORD                                                      
         SR    R1,R1                                                            
*                                                                               
BAL2     CLI   0(R3),X'32'                                                      
         BER   RE                  FOUND 32 - OK TO RETURN                      
         CLI   0(R3),0                                                          
         BE    BAL3                NO 32 IS AN ERROR                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     BAL2                                                             
*                                                                               
BAL3     MVI   ERROR,18            SET - INVALID FOR POSTING- AND EXIT          
*                                                                               
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMD4D                                                       
       ++INCLUDE ACLFMWORK                                                      
         PRINT OFF                                                              
* ACGENBOTH                                                                     
* ACGENFILE                                                                     
* ACLFMEQU                                                                      
* DDFLDIND                                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACLFM31   05/01/02'                                      
         END                                                                    
