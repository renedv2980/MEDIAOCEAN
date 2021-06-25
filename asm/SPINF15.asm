*          DATA SET SPINF15    AT LEVEL 002 AS OF 05/01/02                      
*PHASE T21A15A,+0,NOAUTO                                                        
*INCLUDE CLUNPK                                                                 
         TITLE 'PRODUCT GROUP IDENTIFICATION ROUTINES'                          
T21A15   CSECT                                                                  
         NMOD1 40,T21A15                                                        
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         LA    R5,KEY                                                           
         USING PRGRECD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,SVAGYMD                                                 
         MVC   PRGKCLT,SVCLT                                                    
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY(13),PREVKEY                                                  
         XC    PREVKEY,PREVKEY                                                  
         GOTO1 HIGH                                                             
         LA    R2,SINHDRH                                                       
         LA    RE,FLDDATA          HL 1                                         
         LA    RF,LINLEN(RE)       HL 2                                         
         LA    R9,LINLEN(RF)       HL 3                                         
         MVC   0(16,RF),=C'CLIENT CODE/NAME'                                    
         MVC   0(16,R9),DASH                                                    
         MVC   27(5,RE),=C'GROUP'                                               
         MVC   28(2,RF),=C'ID'                                                  
         MVC   27(5,R9),DASH                                                    
         MVC   34(13,RF),=C'LEVEL 1 TITLE'                                      
         MVC   34(13,R9),DASH                                                   
         MVC   48(13,RF),=C'LEVEL 2 TITLE'                                      
         MVC   48(13,R9),DASH                                                   
         MVC   62(13,RF),=C'LEVEL 3 TITLE'                                      
         MVC   62(13,R9),DASH                                                   
         LA    R3,4                                                             
         LA    R4,15                                                            
SENDTIT  FOUT  (R2)                SEND TITLES                                  
         LA    R2,LINLEN(R2)                                                    
         BCT   R3,SENDTIT                                                       
         B     HAVREC                                                           
PISEQ    GOTO1 SEQ                                                              
HAVREC   LA    R5,KEY                                                           
         CLC   KEY(2),=X'0D01'                                                  
         BNE   PIEND                                                            
         CLC   PRGKAGMD,SVAGYMD                                                 
         BNE   PIEND                                                            
         CLI   SVEST,0                                                          
         BE    *+14                                                             
         CLC   PRGKID,SVEST                                                     
         BNE   PISEQ                                                            
         CLI   SVCLT,0                                                          
         BE    *+14                                                             
         CLC   PRGKCLT,SVCLT                                                    
         BNE   PIEND                                                            
         GOTO1 GETREC                                                           
         CLC   PRGKCLT,PREVKEY+3                                                
         BE    SENDID                                                           
         BAS   RE,GETCNAME                                                      
         MVC   FLDDATA+1(24),WORK                                               
SENDID   L     R5,AREC                                                          
         LA    R7,PRGEL                                                         
         USING PRGEL01,R7                                                       
GETEL    CLI   0(R7),1                                                          
         BE    HAVEL                                                            
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
         AR    R7,RE                                                            
         CLI   0(R7),0                                                          
         BE    PISEQ                                                            
         B     GETEL                                                            
*                                                                               
HAVEL    MVC   FLDDATA+29(1),PRGKID                                             
         MVC   FLDDATA+35(12),PRGBK1                                            
         MVC   FLDDATA+48(12),PRGBK2                                            
         MVC   FLDDATA+62(12),PRGBK3                                            
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   PREVKEY,KEY                                                      
         BCT   R4,PISEQ                                                         
         B     MODEXIT                                                          
*                                                                               
PIEND    XC    PREVKEY,PREVKEY                                                  
         B     MODEXIT                                                          
MODEXIT  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
         OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
GETCNAME NTR1                                                                   
         MVC   WORK2,KEY                                                        
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
         LA    R9,REC2                                                          
         XC    KEY,KEY                                                          
         ST    R9,AREC                                                          
         LA    R5,WORK2                                                         
         MVC   CKEYAM,SVAGYMD                                                   
         MVC   CKEYCLT,PRGKCLT                                                  
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R6,AREC                                                          
         XC    WORK,WORK                                                        
         GOTO1 =V(CLUNPK),DMCB,CKEYCLT,WORK,RR=RB                               
         MVI   WORK+3,C'/'                                                      
         MVC   WORK+4(20),CNAME                                                 
         MVC   KEY,WORK2                                                        
         GOTO1 HIGH                                                             
         LA    R9,REC                                                           
         ST    R9,AREC                                                          
         XIT1  1                                                                
         LTORG                                                                  
LINLEN   EQU   88                                                               
DASH     DC    40C'-'                                                           
         EJECT                                                                  
*SPINFWORK                                                                      
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
*SPGENPRG                                                                       
       ++INCLUDE SPGENPRG                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPINF15   05/01/02'                                      
         END                                                                    
