*          DATA SET ACLFM12    AT LEVEL 015 AS OF 08/10/00                      
*PHASE T60312A                                                                  
         TITLE 'PROGRAM FOR STANDARD COMMENTS'                                  
T60312   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM12*                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         EJECT                                                                  
*        BUILD KEY                                                              
         SPACE 2                                                                
         CLI   MODE,BUILDKEY                                                    
         BNE   COM10                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(6),SPACES                                                  
         LA    R2,COMNUMH                                                       
         GOTO1 ANY                                                              
         TM    COMNUMH+4,X'20'                                                  
         BO    COM2                                                             
         OI    COMNUMH+4,X'20'                                                  
         MVI   ANYKEY,C'Y'                                                      
COM2     SR    R1,R1               RIGHT ALIGN COMMENT NUMBER                   
         IC    R1,COMNUMH+5                                                     
         LA    RE,6                                                             
         SR    RE,R1                                                            
         LA    RE,KEY+2(RE)                                                     
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,RE),COMNUM                                                   
         EJECT                                                                  
*              DISPLAY THE COMMENT RECORD                                       
         SPACE 2                                                                
COM10    CLI   MODE,DSPLYREC                                                    
         BNE   COM20                                                            
         LA    R2,COMFRSTH                                                      
         ZIC   RE,0(R2)                                                         
         LR    R1,RE                                                            
         SH    R1,=H'9'                                                         
         LA    RF,COMTABH                                                       
         BCTR  RF,0                                                             
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         SR    R5,R5                                                            
COM12    CLI   0(R4),0                                                          
         BE    COM15                                                            
         IC    R5,1(R4)                                                         
         LR    R6,R4               POINT R6 TO THIS                             
         AR    R4,R5               AND R4 TO NEXT                               
         CLI   0(R6),X'3E'                                                      
         BNE   COM12                                                            
         SH    R5,=H'5'                                                         
         CR    R5,R1                                                            
         BNH   *+6                                                              
         LR    R5,R1                                                            
         MVC   WORK,SPACES                                                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),4(R6)                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK                                                     
         OI    6(R2),X'80'                                                      
         BXLE  R2,RE,COM12                                                      
         B     COM16                                                            
         SPACE 1                                                                
COM15    EX    R1,COMOC            CLEAR REMAINDER                              
         EX    R1,COMCLC                                                        
         BE    *+12                                                             
         EX    R1,COMXC                                                         
         OI    6(R2),X'80'                                                      
         BXLE  R2,RE,COM15                                                      
         SPACE 2                                                                
COM16    LA    R2,COMFRSTH                                                      
         B     XIT                                                              
         SPACE 1                                                                
COMOC    OC    8(0,R2),SPACES                                                   
COMCLC   CLC   8(0,R2),SPACES                                                   
COMXC    XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
*              BUILD A RECORD FOR NEW OR AMEND                                  
         SPACE 2                                                                
COM20    LA    R2,COMFRSTH                                                      
         SR    R3,R3               FIND OUT HOW MANY LINES                      
         LA    R4,12               WE WILL HANDLE                               
         SR    R5,R5                                                            
COM22    LA    R3,1(R3)                                                         
*&&US                                                                           
         OC    8(72,R2),SPACES                                                  
         CLC   8(72,R2),SPACES                                                  
*&&                                                                             
*&&UK                                                                           
         OC    8(60,R2),SPACES                                                  
         CLC   8(60,R2),SPACES                                                  
*&&                                                                             
         BE    COM24                                                            
         LR    R5,R3               LEAVES R5 WITH NO OF LINES                   
COM24    DS    0H                                                               
*&&US*&& LA    R2,80(R2)                                                        
*&&UK*&& LA    R2,68(R2)                                                        
         BCT   R4,COM22                                                         
         LA    R2,COMFRSTH                                                      
         LTR   R5,R5                                                            
         BNZ   COM26                                                            
         MVI   ERROR,MISSING                                                    
         B     XIT                                                              
         SPACE 1                                                                
COM26    DS    0H                                                               
         CLI   LOGACT,C'A'         DELETE CODE                                  
         BNE   COM28                                                            
         CLI   COMFRSTH+5,6                                                     
         BNE   COM28                                                            
         CLC   COMFRST(6),=C'DELETE'                                            
         BNE   COM28                                                            
         LA    RF,IO2                                                           
         OI    44(RF),X'80'                                                     
         MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
         SPACE 2                                                                
COM28    DS    0H                                                               
         LA    R9,ELEMENT                                                       
         USING ACOMMD,R9                                                        
         XC    ELEMENT,ELEMENT                                                  
         GOTO1 REMANEL,DMCB,(X'3E',0)                                           
         LA    R6,1                FOR SUB-LINE NO                              
COM30    DS    0H                                                               
*&&US*&& CLC   8(72,R2),SPACES                                                  
*&&UK*&& CLC   8(60,R2),SPACES                                                  
         BNE   COM32                                                            
         MVC   ACOMEL(5),=X'3E05000040'                                         
         STC   R6,ACOMSEQ                                                       
         B     COM34                                                            
         SPACE 1                                                                
COM32    DS    0H     2)           FIND LAST NON-BLANK IN INPUT                 
*&&US*&& LA    R3,79(R2)                                                        
*&&UK*&& LA    R3,67(R2)                                                        
         CLI   0(R3),C' '                                                       
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         LA    RE,8(R2)                                                         
         SR    R3,RE                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ACOMMENT(0),8(R2)                                                
         STC   R6,ACOMSEQ                                                       
         MVI   ACOMEL,X'3E'                                                     
         LA    R3,5(R3)                                                         
         STC   R3,ACOMLEN                                                       
         MVI   ACOMTYPE,0                                                       
         SPACE 1                                                                
COM34    GOTO1 ADDANEL                                                          
         LA    R6,1(R6)                                                         
*&&US*&& LA    R2,80(R2)                                                        
*&&UK*&& LA    R2,68(R2)                                                        
         BCT   R5,COM30                                                         
         SPACE 2                                                                
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMEDD                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACLFM12   08/10/00'                                      
         END                                                                    
