*          DATA SET ACLFM0F    AT LEVEL 010 AS OF 08/10/00                      
*PHASE T6030FA                                                                  
*INCLUDE HEXIN                                                                  
         TITLE 'MODULE TO HANDLE GROUP ELEMENTS'                                
T6030F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFMF**                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         EJECT                                                                  
*              BUILD KEY                                                        
         SPACE 3                                                                
         MVI   ERROR,X'FF'                                                      
         CLI   MODE,BUILDKEY                                                    
         BNE   GR10                                                             
         LA    R2,LOGCOMPH                                                      
         GOTO1 ANY                                                              
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),LOGCOMP                                                   
         CLI   5(R2),1                                                          
         BE    GR01                                                             
         GOTO1 =V(HEXIN),DMCB,LOGCOMP,KEY,2,RR=RB                               
         BNZ   GR01                                                             
         MVI   ERROR,2                                                          
         B     XIT                                                              
         SPACE 1                                                                
GR01     EQU   *                                                                
         TM    4(R2),X'20'                                                      
         BO    XIT                                                              
         MVI   ANYKEY,C'Y'                                                      
         OI    4(R2),X'20'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              LOCATE A GROUP                                                   
         SPACE 3                                                                
GR10     LA    R2,LOGGRUPH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         SR    R5,R5                                                            
         SPACE 2                                                                
GR12     CLI   0(R4),0                                                          
         BE    GR18                                                             
         CLI   0(R4),X'13'                                                      
         BNE   GR14                                                             
         GOTO1 MOVE                                                             
         USING ACGRUPD,R4                                                       
         CLC   ACGRCODE,WORK                                                    
         BE    GR16                                                             
         SPACE 2                                                                
GR14     IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     GR12                                                             
         SPACE 2                                                                
GR16     MVI   ERROR,RECONFLE                                                   
         CLI   MODE,NEWELEM                                                     
         BE    XIT                                                              
         CLI   MODE,DSPLYREC                                                    
         BE    GR20                                                             
         MVI   0(R4),X'FF'                                                      
         GOTO1 REMANEL,DMCB,0                                                   
         B     GR30                                                             
         SPACE 2                                                                
GR18     CLI   MODE,NEWELEM                                                     
         BE    GR30                                                             
         MVI   ERROR,NOELEM                                                     
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY A GROUP                                                  
         SPACE 3                                                                
GR20     FOUT  LOGNAMEH,ACGRNAME,20                                             
         FOUT  LOGLISTH,SPACES,20                                               
         SR    R5,R5                                                            
         IC    R5,ACGRLEN                                                       
         SH    R5,=H'24'                                                        
         LA    R2,ACGRLIST                                                      
         LA    R3,LOGLIST                                                       
         SPACE 2                                                                
GR22     MVC   0(1,R3),0(R2)                                                    
         MVI   1(R3),C','                                                       
         LA    R2,1(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R5,GR22                                                          
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         LA    R2,LOGNAMEH                                                      
         MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              BUILD AN ELEMENT                                                 
         SPACE 3                                                                
GR30     MVI   ACGREL,X'13'                                                     
         GOTO1 MOVE                                                             
         CLI   WORK,C'C'           CODE MUST START C,U OR L                     
         BE    GR32                                                             
         CLI   WORK,C'U'                                                        
         BE    GR32                                                             
         CLI   WORK,C'L'                                                        
         BE    GR32                                                             
         MVI   ERROR,NOTVLCDE                                                   
         B     XIT                                                              
         SPACE 2                                                                
GR32     CLI   WORK+1,C' '                                                      
         BNE   GR34                                                             
         MVI   ERROR,NOTVLCDE                                                   
         B     XIT                                                              
         SPACE 2                                                                
GR34     MVC   ACGRCODE,WORK                                                    
         LA    R2,LOGNAMEH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   ACGRNAME,WORK                                                    
         SPACE 2                                                                
         LA    R2,LOGLISTH                                                      
         LA    R3,LOGLIST                                                       
         LA    R5,25                                                            
         LA    R6,ACGRLIST                                                      
         SPACE 2                                                                
GR36     MVC   0(1,R6),0(R3)                                                    
         STC   R5,ACGRLEN                                                       
         CLI   1(R3),C' '                                                       
         BE    XIT                                                              
         CLI   1(R3),0                                                          
         BE    XIT                                                              
         CLI   1(R3),C','                                                       
         BE    GR38                                                             
         MVI   ERROR,INVALID                                                    
         B     XIT                                                              
         SPACE 2                                                                
GR38     LA    R3,2(R3)                                                         
         LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         B     GR36                                                             
         SPACE 2                                                                
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMF0D                                                       
       ++INCLUDE ACLFMWORK                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACLFM0F   08/10/00'                                      
         END                                                                    
