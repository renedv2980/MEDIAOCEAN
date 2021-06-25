*          DATA SET SPREPFXGPA AT LEVEL 007 AS OF 02/05/99                      
*PHASE SPFX02G,+0                                                               
         TITLE 'SPFXGPA- OMNY NETPAK BILLING FIX'                               
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         LA    RC,SPACEND                                                       
         USING SPFXWRKD,RC                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
FX10     DS    0H                                                               
*                                                                               
         L     R3,=F'2055199322'                                                
         A     R3,=F'1100000000'                                                
         EDIT  (R3),(15,P+5)                                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         LTORG                                                                  
*                                                                               
         MVI   FCRDBUYS,C'N'                                                    
         OPEN  (UNTFIN(INPUT))                                                  
*                                                                               
IN2      GET   UNTFIN,UNTREC                                                    
         LA    RE,UNTREC                                                        
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)                                                    
*                                                                               
         LA    R2,UNTREC                                                        
         USING NURECD,R2                                                        
         CLI   NUKEY,X'04'                                                      
         BL    IN2                                                              
         BH    ENDIN                                                            
         CLC   NUKAM,BAGYMD                                                     
         BL    IN2                                                              
         BH    ENDIN                                                            
         CLC   NUCLT,=X'C891'      CLIENT=SER                                   
         BL    IN2                                                              
         BH    ENDIN                                                            
         CLI   NUKEST,4                                                         
         BNE   IN2                                                              
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTELX                                                          
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
ENDIN    DS    0H                                                               
         CLOSE UNFTIN                                                           
*                                                                               
         GOTO1 REPORT                                                           
         EDIT  (P6,TASSGN),(15,PLASSGN),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P6,TACT),(15,PLACT),2,COMMAS=YES,MINUS=YES                      
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
UNTFIN   DCB   DDNAME=UNTFIN,DSORG=PS,RECFM=VB,LRECL=4100,             X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
UNTREC   DS    5000X                                                            
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
SPFXWRKD DSECT                                                                  
ELCODE   DS    CL1                                                              
LASTAGY  DS    CL2                                                              
*                                                                               
UASSGN   DS    PL6                                                              
UACT     DS    PL6                                                              
TASSGN   DS    PL6                                                              
TACT     DS    PL6                                                              
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPFXGPA02/05/99'                                      
         END                                                                    
