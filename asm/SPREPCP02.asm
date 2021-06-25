*          DATA SET SPREPCP02  AT LEVEL 045 AS OF 01/30/02                      
*PHASE SPCP02A                                                                  
         TITLE 'SPCP02 - CPPRS RECORD REPORT'                                   
SPCP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPCP02                                                         
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         USING SPCP02,RB,R8                                                     
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,CLTLAST                                                     
         BE    CLTL                                                             
         CLI   MODE,PRDFRST                                                     
         BE    PRDF                                                             
         CLI   MODE,PRDLAST                                                     
         BE    PRDL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         USING PLINED,P                                                         
         LA    R0,HDHOOK                                                        
         ST    R0,HEADHOOK                                                      
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         MVI   RQALLPOL,C'Y'                                                    
         MVI   FLAGBYTE,X'00'                                                   
         MVI   RCSUBPRG,1                                                       
         B     EXIT                                                             
*                                                                               
CLTF     DS    0H                                                               
*                                                                               
         XC    LASTPR,LASTPR                                                    
*                                                                               
         L     R3,ADCLT                                                         
         USING CLTHDRD,R3                                                       
         CLI   CCPPRS,C'N'                                                      
         BNE   EXIT                                                             
*                                                                               
         OI    FLAGBYTE,CLTQ                                                    
*                                                                               
         MVC   PLINCLT,CLT                                                      
         MVC   PLINCLTN,CLTNM                                                   
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
CLTL     DS    0H                                                               
*                                                                               
         NI    FLAGBYTE,X'FF'-CLTQ                                              
         B     EXIT                                                             
*                                                                               
PRDF     DS    0H                                                               
*                                                                               
         TM    FLAGBYTE,CLTQ       CLIENT ALREADY EXCLUDED?                     
         BNZ   EXIT                                                             
*                                                                               
         L     R3,ADPRD                                                         
         USING PRDHDR,R3                                                        
*                                                                               
         CLC   LASTPR,4(R3)           SAME PRODUCT AS LAST ONE?                 
         BE    EXIT                   IF YES-EXIT                               
         MVC   LASTPR,4(R3)           IF NOT-SAVE IT                            
*                                                                               
         CLI   PCPPRS,C'N'                                                      
         BNE   PRDF20                                                           
*                                                                               
         MVC   PLINCLT,CLT                                                      
         MVC   PLINCLTN,CLTNM                                                   
         MVC   PLINPRD,PRD                                                      
         MVC   PLINPRDN,PRDNM                                                   
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
PRDF20   DS    0H                                                               
         BAS   RE,GETESTS                                                       
*                                                                               
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
PRDL     DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
REQL     DS    0H                                                               
         MVI   RCSUBPRG,2                                                       
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'CONTROL',CTFLIST                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CTXREC,R2                                                        
         MVI   CTXKTYP,CTXKTYPQ                                                 
         MVC   CTXKAGY,AGY                                                      
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                    
         CLC   SAVEKEY(25),KEY                                                  
         BNE   EXIT                                                             
*                                                                               
         MVI   COLUMN,X'01'                                                     
         MVI   ELCODE,CTPQELQ                                                   
         LA    R6,IO                                                            
*                                                                               
         BAS   RE,GETEL                                                         
         B     REQL20                                                           
REQL10   BAS   RE,NEXTEL                                                        
REQL20   BNE   REQLXIT                                                          
*                                                                               
         LA    R3,P                                                             
*                                                                               
         CLI   COLUMN,2                                                         
         BNE   *+8                                                              
         LA    R3,PL2DLQ(R3)                                                    
*                                                                               
         CLI   COLUMN,3                                                         
         BNE   *+8                                                              
         LA    R3,(2*PL2DLQ)(R3)                                                
*                                                                               
         ZIC   R0,COLUMN                                                        
         AHI   R0,1                                                             
         STC   R0,COLUMN                                                        
         CLI   COLUMN,3                                                         
         BNH   *+8                                                              
         MVI   COLUMN,1                                                         
*                                                                               
         USING PL2D,R3                                                          
         USING CTPQD,R6                                                         
*                                                                               
         MVC   BYTE,CTPQCODE                                                    
         LA    R1,PL2TYPE                                                       
         BAS   RE,GETCOD                                                        
*                                                                               
         MVC   PL2DPTC(L'CTPQDP),CTPQDP                                         
         MVC   PL2CODE(L'CTPQCODE),CTPQCODE                                     
         CLI   COLUMN,1                                                         
         BNE   REQL30                                                           
         GOTO1 REPORT                                                           
REQL30   DS    0H                                                               
         DROP  R6,R3                                                            
         B     REQL10                                                           
*                                                                               
REQLXIT  GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
GETESTS  NTR1                                                                   
*                                                                               
         MVC   SAVEKEY2,KEY                                                     
         XC    KEY+7(L'KEY-7),KEY+7     CLEAR UP TO ESTIMATE                    
*                                                                               
         LA    R3,KEY                                                           
         GOTO1 HIGH                                                             
         B     GETEST20                                                         
GETEST10 GOTO1 SEQ                                                              
GETEST20 DS    0H                                                               
         CLC   KEY(EKEYEST-ESTHDR),KEYSAVE    SAME AGY, CLT, PRODUCT?           
         BNE   GETESTX                                                          
         CLI   7(R3),0                MAKE SURE THIS IS NOT PRODUCT REC         
         BE    GETEST10                                                         
         CLC   KEY+8(5),=5X'00'       MAKE SURE THIS IS NOT BILL REC            
         BNE   GETEST10                                                         
*                                                                               
         LA    R4,IO                                                            
         ST    R4,AREC                                                          
         GOTO1 GET                                                              
         USING ESTHDR,R4                                                        
*                                                                               
         CLI   ECPPRS,C'N'                                                      
         BNE   GETEST10                                                         
*                                                                               
         MVC   PLINCLT,CLT                                                      
         MVC   PLINCLTN,CLTNM                                                   
         MVC   PLINPRD,PRD                                                      
         MVC   PLINPRDN,PRDNM                                                   
*                                                                               
         EDIT  EKEYEST,PLINEST                                                  
         MVC   PLINESSD,EDESC                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(0,ESTART),(11,PLINESSD)                             
         GOTO1 DATCON,DMCB,(0,EEND),(11,PLINESED)                               
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     GETEST10                                                         
*                                                                               
         DROP  R4                                                               
GETESTX  DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY2    RESTORE KEY                                  
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
* BYTE EXPECTED TO CONTAIN CODE                                                 
* R1 EXPECTED TO ADDRESS OUTPUT AREA                                            
GETCOD   NTR1                                                                   
         LA    R2,CODETAB                                                       
GETC10   CLI   0(R2),X'FF'                                                      
         BE    EXIT                                                             
         CLC   BYTE,0(R2)                                                       
         BE    *+12                                                             
*                                                                               
         LA    R2,CODETLQ(R2)                                                   
         B     GETC10                                                           
*                                                                               
         MVC   0(6,R1),1(R2)                                                    
         B     EXIT                                                             
*                                                                               
*                                                                               
HDHOOK   NTR1                                                                   
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         GETEL R6,DATAD,ELCODE                                                  
DATAD    DC    H'28'                                                            
ELCODE   DS    X                                                                
*                                                                               
CTFLIST  DC    C'NCTFILE X'                                                     
*                                                                               
CODETAB  DC    C'S',C'SPORTS'                                                   
CODETLQ  EQU   *-CODETAB                                                        
         DC    C'K',C'KIDS  '                                                   
         DC    C'F',C'BONUS '                                                   
         DC    C'N',C'NEWS  '                                                   
         DC    X'FF'                                                            
*                                                                               
COLUMN   DS    X                                                                
SAVEKEY  DS    XL32                                                             
SAVEKEY2 DS    XL13                                                             
LASTPR   DS    XL3                                                              
FLAGBYTE DS    X                                                                
CLTQ     EQU   X'01'                                                            
PRDQ     EQU   X'02'                                                            
IO       DS    2000X                                                            
*                                                                               
PLINED   DSECT                                                                  
PLINCLT  DS    CL3                                                              
         DS    CL1                                                              
PLINCLTN DS    CL24                                                             
         DS    CL1                                                              
PLINPRD  DS    CL3                                                              
         DS    CL1                                                              
PLINPRDN DS    CL24                                                             
         DS    CL1                                                              
PLINEST  DS    CL3                                                              
         DS    CL3                                                              
PLINESSD DS    CL8                                                              
         DS    CL1                                                              
PLINESED DS    CL8                                                              
*                                                                               
PLINDLQ  EQU   *-PLINED                                                         
*                                                                               
*                                                                               
PL2D     DSECT                                                                  
PL2DPTC  DS    CL6                                                              
         DS    CL(SPC)                                                          
PL2TYPE  DS    CL6                                                              
         DS    CL(SPC)                                                          
PL2CODE  DS    CL4                                                              
         DS    CL(SPC)                                                          
*                                                                               
PL2DLQ   EQU   *-PL2D                                                           
SPC      EQU   3                                                                
*                                                                               
*                                                                               
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
*                                                                               
       ++INCLUDE CTGENFILE                                                      
*                                                                               
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPREPCP02 01/30/02'                                      
         END                                                                    
