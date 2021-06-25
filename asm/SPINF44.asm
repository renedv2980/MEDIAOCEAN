*          DATA SET SPINF44    AT LEVEL 002 AS OF 08/11/00                      
*PHASE T21A44A                                                                  
         TITLE 'T21A44 - SPOTPAK INFO DEMOGRAPHIC MENU DISPLAY'                 
T21A44   CSECT                                                                  
         NMOD1 30,T21A44                                                        
         USING FLDHDRD,R2                                                       
         LR    R3,RC                                                            
         USING MENUWK,R3                                                        
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         LA    R5,REC                                                           
         LA    RF,1600                                                          
         LR    RE,R5                                                            
         XCEF                                                                   
         LA    R2,SINHDRH          BUILD HEADLINES                              
         MVC   FLDDATA+1(4),=C'MENU'                                            
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(4),=C'----'                                            
         MVC   FLDDATA+6(30),=C'******************************'                 
         MVC   FLDDATA+35(12),=C'DEMOGRAPHICS'                                  
         MVC   FLDDATA+47(30),=C'******************************'                
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
POSEX    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D26'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         MVC   KEY+3(4),SVKEY+3                                                 
         LA    R7,0                                                             
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
         GOTO1 HIGH                                                             
         B     HAVKEY                                                           
RSEQ     GOTO1 SEQ                                                              
HAVKEY   CLC   KEY(2),=X'0D26'                                                  
         BNE   MODEXITA                                                         
         CLC   KEY+2(1),SVAGYMD                                                 
         BNE   MODEXITA                                                         
         GOTO1 GETREC              RECORD IS GOOD - PROCESS                     
         XC    DEMTAB,DEMTAB                                                    
         USING DMNRECD,R5                                                       
         LA    R5,REC                                                           
         LA    R4,DMNEL01                                                       
         USING DMNEL05,R4                                                       
         LA    RE,DEMTAB                                                        
PROCEL   CLI   0(R4),0             SET UP DEMOS IN SAVE AREA                    
         BE    SEND                                                             
         CLI   0(R4),5                                                          
         BE    PROCEL2                                                          
PROCEL1  ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PROCEL                                                           
         SPACE 2                                                                
PROCEL2  MVC   0(7,RE),DMNRTG                                                   
         LA    RE,7(RE)                                                         
         B     PROCEL1                                                          
         SPACE 2                                                                
SEND     LA    R1,DEMTAB           COUNT LINES NEEDED                           
         CLI   0(R1),0             TO DISPLAY THIS RECORD                       
         BE    RSEQ                                                             
         LA    R7,1(R7)                                                         
         CLI   56(R1),0                                                         
         BE    SEND2                                                            
         LA    R7,1(R7)                                                         
         CLI   102(R1),0                                                        
         BE    SEND2                                                            
         LA    R7,1(R7)                                                         
         SPACE 2                                                                
SEND2    CH    R7,=H'15'           EXIT IF IT WONT FIT ON SCREEN                
         BH    MODEXIT                                                          
         MVC   FLDDATA+1(4),DMNKCODE                                            
         LA    R1,DEMTAB                                                        
SEND3    LA    R5,8                                                             
         LA    R8,FLDDATA+6                                                     
SEND4    CLI   0(R1),0             SEND OUT DEMO NAMES                          
         BE    SEND5                                                            
         MVC   0(7,R8),0(R1)                                                    
         CLI   7(R1),0             SET COMMA IF NOT LAST IN LIST                
         BE    *+8                                                              
         LA    R8,8(R8)            GET NEXT NAME AND ASLOT                      
         LA    R1,7(R1)                                                         
         BCT   R5,SEND4                                                         
SEND5    FOUT  (R2)                SEND LINE TO SCREEN                          
         LA    R2,LINLEN(R2)       SET TO NEXT LINE                             
         CLI   0(R1),0             END OF THIS RECORD                           
         BNE   SEND3                NO - RETURN TO GET MORE                     
         MVC   PREVKEY,KEY         SET UP NEXT KEY                              
         LA    R5,PREVKEY                                                       
         MVI   DMNKCODE+4,X'FF'                                                 
         B     RSEQ                                                             
*                                                                               
MODEXITA XC    PREVKEY,PREVKEY     RESET READ                                   
MODEXIT  LA    R2,SINIRECH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
         OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
DASH     DC    40C'-'                                                           
LINLEN   EQU   88                                                               
         LTORG                                                                  
MENUWK   DSECT                                                                  
DEMTAB   DS    CL240                                                            
         EJECT                                                                  
* SPINFWORK                                                                     
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
       ++INCLUDE SPGENDMN                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPINF44   08/11/00'                                      
         END                                                                    
