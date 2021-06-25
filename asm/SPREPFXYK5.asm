*          DATA SET SPREPFXYK5 AT LEVEL 001 AS OF 10/02/00                      
*PHASE SPFX02Y5                                                                 
         TITLE 'SPFX02 - CANADIAN NETWORK EST RECORD MEDIA TRACK'               
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* ESTFRST                                                                       
ESTF     DS    0H                                                               
*                                                                               
*  CODE HERE                                                                    
*                                                                               
*                                                                               
         USING ESTHDRD,R6                                                       
         L     R6,ADEST            WE HAVE ESTMATE REC ALREADY                  
*                                                                               
         MVC   BYTE,EKEYAM         GET MEDIA                                    
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'01'          MED T = X'10'                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'EKEY),EKEY    GET KEY TO GET NXT MEDIA                     
         NI    KEY+1,X'F0'         TURN OFF MEDIA BITS                          
         OI    KEY+1,X'03'         TURN ON MEDIA 'N'                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'EKEY),KEYSAVE DO WE HAVE MEDIA T ?                         
         BE    *+12                                                             
         BAS   RE,PRTREC           NO, PRINT OUT                                
         B     ESTL                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'EKEY),EKEY    GET KEY TO GET NXT MEDIA                     
         NI    KEY+1,X'F0'         TURN OFF MEDIA BITS                          
         OI    KEY+1,X'08'         TURN ON MEDIA 'C'                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'EKEY),KEYSAVE DO WE HAVE MEDIA C ?                         
         BE    ESTL                                                             
         BAS   RE,PRTREC           NO, PRINT OUT                                
*                                                                               
ESTL     B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          PRINT RECORD                               *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRTREC   NTR1                                                                   
         MVC   P(73),=C'THE FOLLOWING REC DOES NOT HAVE MEDIA N AND/OR X        
               C RECORD ASOCIATED WITH IT'                                      
         GOTO1 REPORT                                                           
         L     R6,ADEST                                                         
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         MVC   PAGY,AGY                                                         
*                                                                               
         MVI   PMED,C'T'                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,EKEYCLT,PCLT                                         
         MVC   PPRD,EKEYPRD                                                     
*                                                                               
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
*        GETEL R5,24,ELCODE                                                     
*                                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
ELCODE   DS    X                                                                
*                                                                               
IOAREA   DS    2000X                                                            
*                                                                               
         DS    0F                                                               
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPFXYK510/02/00'                                      
         END                                                                    
