*          DATA SET SPINF32    AT LEVEL 001 AS OF 08/06/97                      
*PHASE T21A32A,+0,NOAUTO                                                        
         TITLE 'T21A32 - SPOTPAK INFO AGENCY HEADER DISPLAY'                    
T21A32   CSECT                                                                  
         NMOD1 30,T21A32                                                        
         USING FLDHDRD,R2                                                       
         LR    R3,RC                                                            
         USING AGYWRK,R3                                                        
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         LA    R2,SINHDRH          BUILD HEADLINES                              
         MVC   FLDDATA+1(14),=C'AGENCY ID/NAME'                                 
         MVC   FLDDATA+62(7),=C'REQUEST'                                        
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+40(14),=C'AGENCY PROFILE'                                
         MVC   FLDDATA+64(4),=C'CODE'                                           
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(14),DASH                                               
         MVC   FLDDATA+40(14),DASH                                              
         MVC   FLDDATA+62(7),DASH                                               
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         XC    APROFLT,APROFLT                                                  
         LA    R5,SINIFLT                                                       
POS1     GOTO1 USER1,DMCB,(64,(R5)),(4,=C'CHAR')                                
         XC    WORK,WORK                                                        
         MVC   WORK(7),=C'CHARNN='                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   POS1A                                                            
         GOTO1 USER1,DMCB,(64,(R5)),(4,=C'BYTE')                                
         MVC   WORK(7),=C'BYTENN='                                              
         OC    4(4,R1),4(R1)                                                    
         BZ    POSEX                                                            
*                                                                               
POS1A    L     R5,4(R1)            SET FOR NEXT SCAN                            
         LR    R6,R5               POINT TO FIELD                               
         LA    RE,8                SET FIELD LENGTH FOR ERROR                   
         LA    R5,1(R5)                                                         
         MVC   HALF,=C'00'                                                      
         CLI   5(R6),C'='                                                       
         BNE   *+14                                                             
         MVC   HALF+1(1),4(R6)                                                  
         B     POS2                                                             
         CLI   6(R6),C'='                                                       
         BNE   FLTERR              ERROR                                        
         MVC   HALF(2),4(R6)                                                    
*                                                                               
POS2     CLI   HALF,C'0'           EDIT FIELD NUMBER                            
         BL    FLTERR                                                           
         CLI   HALF,C'9'                                                        
         BH    FLTERR                                                           
         CLI   HALF+1,C'0'                                                      
         BL    FLTERR                                                           
         CLI   HALF+1,C'9'                                                      
         BH    FLTERR                                                           
         PACK  DUB,HALF                                                         
         CVB   R9,DUB                                                           
         LTR   R9,R9                                                            
         BZ    FLTERR                                                           
         CH    R9,=H'20'                                                        
         BH    FLTERR                                                           
         BCTR  R9,0                                                             
         LA    R9,APROFLT(R9)                                                   
POS3     CLI   0(R6),C'='                                                       
         BE    *+12                                                             
         LA    R6,1(R6)                                                         
         B     POS3                                                             
         LA    R6,1(R6)                                                         
POS4     MVC   0(1,R9),0(R6)                                                    
         LA    R6,1(R6)                                                         
         CLI   0(R6),0                                                          
         BE    POS1                                                             
         CLI   0(R6),C','                                                       
         BE    POS1                                                             
         B     POS4                                                             
         EJECT                                                                  
POSEX    XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         LA    R7,14                                                            
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
         GOTO1 HIGH                                                             
         B     HAVKEY                                                           
RSEQ     GOTO1 SEQ                                                              
HAVKEY   CLI   KEY,6                                                            
         BNE   MODEXIT                                                          
         GOTO1 GETREC                                                           
         L     R5,AREC                                                          
         USING AGYHDRD,R5                                                       
         LA    R4,AGYEL                                                         
         USING AGYEL,R4                                                         
         OC    APROFLT,APROFLT                                                  
         BZ    PFLTEND                                                          
         LA    RF,AGYPROF                                                       
         LA    RE,APROFLT                                                       
         LA    R9,15                                                            
PFLTR    CLI   0(RE),0                                                          
         BE    PFLTR1                                                           
         CLC   0(1,RF),0(RE)                                                    
         BNE   RSEQ                                                             
PFLTR1   LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R9,PFLTR                                                         
*                                                                               
PFLTEND  MVC   FLDDATA+1(3),AGYID                                               
         MVI   FLDDATA+4,C'/'                                                   
         MVC   FLDDATA+5(33),AGYNAME                                            
         MVC   FLDDATA+40(20),AGYPROF                                           
         MVC   FLDDATA+65(2),AGYKAGY                                            
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         BCT   R7,RSEQ                                                          
         GOTO1 SEQ                                                              
         CLI   KEY,6                                                            
         BNE   MODEXIT                                                          
         MVC   PREVKEY,KEY                                                      
         B     MODEXIT                                                          
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)                                                  
         LA    RE,3(RE)                                                         
         LA    RF,SINMSG(RE)                                                    
         MVC   0(22,RF),=C'- INVALID FILTER FIELD'                              
         LA    RF,23(RF)                                                        
         MVC   0(20,RF),WORK                                                    
         LA    R2,SINIFLTH                                                      
         B     *+8                                                              
*                                                                               
MODEXIT  LA    R2,SINIRECH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
         OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
DASH     DC    40C'-'                                                           
LINLEN   EQU   88                                                               
         LTORG                                                                  
AGYWRK   DSECT                                                                  
APROFLT  DS    CL15                AGENCY PROFILE FILTERS                       
         EJECT                                                                  
* SPINFWORK                                                                     
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPINF32   08/06/97'                                      
         END                                                                    
