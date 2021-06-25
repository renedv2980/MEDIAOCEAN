*          DATA SET SPREPFXAMP AT LEVEL 012 AS OF 02/26/99                      
*PHASE SPFX02C                                                                  
SPFX02C  TITLE 'SPFX02C - FEUR SAMANTHA'                                        
         SPACE 1                                                                
SPFX02   CSECT                                                                  
         DS    6000C                                                            
         ORG   SPFX02                                                           
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02,RB,RC                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX2                                                              
*                                                                               
YES      CR    RB,RB               SET CC EQUAL                                 
         B     XIT                                                              
*                                                                               
NO       LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*=========================================================*                     
* REQFRST PROCESSING                                      *                     
*=========================================================*                     
         SPACE 1                                                                
FX2      DS    0H                                                               
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,IO                                                            
         ST    R1,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(5),=X'0C0111D908'  SID REC SCHEME=WI9                        
         GOTO1 HIGH                                                             
         B     FX10                                                             
FX10SEQ  GOTO1 SEQ                                                              
FX10     CLC   KEY(5),KEYSAVE                                                   
         BNE   FX100                                                            
         GOTO1 GET                 GET REC INTO IO                              
         L     R3,AREC                                                          
         MVI   2(R3),X'91'         CHANGE TO QA X'9'                            
         SR    R1,R1                                                            
         ICM   R1,3,13(R3)         LEN FOR PUT                                  
         AHI   R1,4                                                             
         STH   R1,IOLEN                                                         
         LA    R0,IOLEN                                                         
         PUT   FILEOUT,(0)                                                      
         B     FX10SEQ                                                          
*                                                                               
*                                                                               
FX100    XC    KEY,KEY                                                          
         MVC   KEY(4),=X'0C11D908'  SID REC SCHEME=WI9                          
         GOTO1 HIGH                                                             
         B     FX110                                                            
FX110SEQ GOTO1 SEQ                                                              
FX110    CLC   KEY(4),KEYSAVE                                                   
         BNE   FX200                                                            
         GOTO1 GET                 GET REC INTO IO                              
         L     R3,AREC                                                          
         MVI   1(R3),X'91'         CHANGE TO QA X'9'                            
         SR    R1,R1                                                            
         ICM   R1,3,13(R3)         LEN FOR PUT                                  
         AHI   R1,4                                                             
         STH   R1,IOLEN                                                         
*                                                                               
         LA    R0,IOLEN                                                         
         PUT   FILEOUT,(0)                                                      
         B     FX110SEQ                                                         
*                                                                               
FX200    XC    KEY,KEY                                                          
         MVC   KEY(4),=X'0D6511'    NWS BUYER RECS                              
         GOTO1 HIGH                                                             
         B     FX210                                                            
FX210SEQ GOTO1 SEQ                                                              
FX210    CLC   KEY(2),KEYSAVE                                                   
         BNE   FX300                                                            
         MVC   BYTE,KEY+2          A/M                                          
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          STILL WILA                                   
         BNE   FX300                                                            
         GOTO1 GET                 GET REC INTO IO                              
         L     R3,AREC                                                          
         MVC   BYTE,2(R3)          A/M                                          
         NI    BYTE,X'0F'                                                       
         OI    BYTE,X'90'          CHANGE TO X'9'                               
         MVC   2(1,R3),BYTE                                                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,13(R3)         LEN FOR PUT                                  
         AHI   R1,4                                                             
         STH   R1,IOLEN                                                         
*                                                                               
         LA    R0,IOLEN                                                         
         PUT   FILEOUT,(0)                                                      
         B     FX210SEQ                                                         
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
FX300    MVC   P(14),=C'DONE          '                                         
         GOTO1 REPORT                                                           
*                                                                               
         CLOSE FILEOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
MYCOUNT  DC    PL4'0'                                                           
MAXCOUNT DC    PL4'9999999'                                                     
SEQNUM   DC    PL4'0'                                                           
SAVEP129 DS    A                                                                
TEMPRECD DS    0CL80                                                            
TEMPHEAD DS    CL4                                                              
TEMPBAGY DS    CL1                                                              
TEMPAGY  DS    CL2                                                              
TEMPSEQ  DS    XL64                                                             
         DS    CL9                                                              
AGYTABLE DS    30CL3                                                            
VAGYTAB  DS    A                                                                
IOLEN    DS    XL4                                                              
IO       DS    CL2000                                                           
ELCODE   DS    CL1                                                              
         LTORG                                                                  
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,LRECL=2048,            X        
               BLKSIZE=25000,MACRF=PM                                           
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPREPFXAMP02/26/99'                                      
         END                                                                    
