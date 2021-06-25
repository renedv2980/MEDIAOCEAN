*          DATA SET SPREPLR20  AT LEVEL 031 AS OF 10/10/90                      
*PHASE SPLR02C,+0,NOAUTO                                                        
         TITLE 'SPREPLR19 CREATE NTI PROGRAM INDEX POINTERS'                    
         PRINT NOGEN                                                            
SPLR02   CSECT                                                                  
         NMOD1 0,SPLR02,RR=R5                                                   
*                                                                               
         LA    R4,2048(RB)         R4 IS SECOND BASE REGISTER                   
         LA    R4,2048(R4)                                                      
         USING SPLR02,RB,R4                                                     
*                                                                               
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         ST    R5,RELO                                                          
         XC    DBLOCK,DBLOCK                                                    
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELAGY,=C'SJ'                                                  
         DROP  R5                                                               
         LA    R6,1                                                             
L1       STC   R6,DNO+2                                                         
         CVD   R6,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P(3),DUB+6(2)                                                    
         GOTO1 DEMOCON,DMCB,(1,DNO),(6,P+4),DBLOCK                              
         GOTO1 REPORT                                                           
         LA    R6,1(R6)                                                         
         C     R6,=F'256'                                                       
         BL    L1                                                               
EXIT     XMOD1 1                                                                
         PRINT NOGEN                                                            
         SPACE 2                                                                
DNO      DC    X'00',C'I',X'00'                                                 
         DC    X'FF'                                                            
         SPACE 2                                                                
         EJECT                                                                  
RELO     DC    F'0'                                                             
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031SPREPLR20 10/10/90'                                      
         END                                                                    
