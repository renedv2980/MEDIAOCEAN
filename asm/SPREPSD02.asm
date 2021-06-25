*          DATA SET SPREPSD02  AT LEVEL 022 AS OF 08/29/00                      
*PHASE SPSD02A                                                                  
         TITLE 'SPSD02 - REPORT ESD RECORDS'                                    
SPSD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSD02                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPSD02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         STM   R9,RC,SPSDR9                                                     
         USING ESDD,R8                                                          
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    SP10                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
*                                                                               
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
         MVC   P(22),=C'NUMBER OF SID RECORDS:'                                 
         L     R1,NUMRECS                                                       
         EDIT  (R1),(8,P+25),ALIGN=LEFT                                         
         GOTO1 REPORT                                                           
         MVC   P(23),=C'LARGEST INVENTORY SIZE:'                                
         L     R1,HIGHVAL                                                       
         EDIT  (R1),(8,P+25),ALIGN=LEFT                                         
         GOTO1 REPORT                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* REQFRST                                                                       
*                                                                               
SP10     MVI   FORCEHED,C'Y'                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'0D'           RECORD TYPE                                  
         MVI   KEY+1,X'59'         RECORD SUB-TYPE                              
         MVC   KEY+2(1),BAGYMD     A-M                                          
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      TEST ANY RECORDS OUT THERE                   
         BNE   EXIT                                                             
*                                                                               
         GOTO1 SEQ                 SKIP THE HEADER RECORD                       
*                                                                               
         LA    R8,KEY                                                           
         GOTO1 MSUNPK,DMCB,ESDKMKT,P,P+5                                        
         GOTO1 REPORT                                                           
         LA    R4,P                                                             
*                                                                               
SP22     CLI   DONE,C'Y'                                                        
         BE    EOJ                                                              
         MVC   KEYSAVE,KEY                                                      
*                                                                               
SP24     GOTO1 SEQ                                                              
*                                                                               
         CLI   KEY+8,0                                                          
         BE    SP24                                                             
         CLI   KEY+10,0                                                         
         BE    SP24                                                             
*                                                                               
         CLC   KEY(3),KEYSAVE      TEST SAME TYPE/SUB/A-M                       
         BE    *+12                                                             
         MVI   DONE,C'Y'                                                        
         B     SP30                                                             
*                                                                               
         CLC   KEY+3(5),KEYSAVE+3  TEST SAME MKT/STA                            
         BE    SP25                                                             
         GOTO1 REPORT                                                           
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
         LA    R8,KEY                                                           
         GOTO1 MSUNPK,DMCB,ESDKMKT,P,P+5                                        
         GOTO1 REPORT                                                           
         LA    R4,P                                                             
*                                                                               
SP25     CLC   KEY+8(1),KEYSAVE+8  TEST SAME PERIOD                             
         BNE   SP30                                                             
*                                                                               
         CLC   KEY+10(1),KEYSAVE+10 TEST SAME DAYPART                           
         BE    SP40                                                             
*                                                                               
SP30     LA    R8,KEYSAVE                                                       
         MVC   0(1,R4),ESDKNUM                                                  
         MVC   1(1,R4),ESDKDPT                                                  
         EDIT  INVCOUNT,(3,2(R4))                                               
         LA    R4,6(R4)                                                         
*                                                                               
         L     R1,INVCOUNT                                                      
         C     R1,HIGHVAL                                                       
         BL    *+8                                                              
         ST    R1,HIGHVAL                                                       
         XC    INVCOUNT,INVCOUNT                                                
*                                                                               
SP40     L     R1,INVCOUNT                                                      
         LA    R1,1(R1)            INCREMENT INVENTORY COUNT                    
         ST    R1,INVCOUNT                                                      
         L     R1,NUMRECS                                                       
         LA    R1,1(R1)            INCREMENT TOTAL COUNT                        
         ST    R1,NUMRECS                                                       
         B     SP22                                                             
*                                                                               
EOJ      GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* HEADLINE HOOK                                                                 
*                                                                               
         USING *,RF                                                             
SPSDHL   NTR1                                                                   
         LM    R9,RC,SPSDR9                                                     
         B     HDHK2                                                            
*                                                                               
SPSDR9   DC    4F'0'                                                            
         DROP  RF                                                               
*                                                                               
HDHK2    B     EXIT                                                             
*                                                                               
NUMRECS  DC    F'0'                                                             
INVCOUNT DC    F'0'                                                             
HIGHVAL  DC    F'0'                                                             
DONE     DC    C'N'                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
ESDD     DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENESD                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022SPREPSD02 08/29/00'                                      
         END                                                                    
