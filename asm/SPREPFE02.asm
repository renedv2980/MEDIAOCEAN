*          DATA SET SPREPFE02  AT LEVEL 009 AS OF 08/29/00                      
*PHASE SPFE02A                                                                  
         TITLE 'SPFE02 - FIX SPTFILE 0E01 RECORDS'                              
SPFE02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFE02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
*                                                                               
         CLI   MODE,PRDFRST                                                     
         BE    SPT0                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
SPT0     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),BPRD                                                    
*                                                                               
SPT1C    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     SPT1F                                                            
*                                                                               
SPT1D    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
SPT1F    DS    0H                                                               
         CLC   KEY(6),KEYSAVE      TEST THRU PRODUCT                            
         BNE   EXIT                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+1(3),KEY+2     AGYMD AND CLT                                
         MVC   WORK+4(3),PRD                                                    
         MVC   WORK+7(1),KEY+6     EST FROM STABUCK                             
*                                                                               
         MVC   HLDKEY,KEY          READ FOR EST                                 
         MVC   KEY,WORK                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    SPT3                                                             
*                                  EST NOT ON FILE                              
         MVC   KEY,HLDKEY                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 READ                                                             
*                                                                               
SPT2     DS    0H                                                               
         OI    KEY+13,X'80'        DELETE                                       
         GOTO1 WRITE                                                            
         AP    DELCNT,=P'1'                                                     
*                                                                               
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PAGY,AGY                                                         
         MVC   PMED,MED                                                         
         MVC   PCLT,CLT                                                         
         MVC   PPRD,PRD                                                         
         EDIT  (B1,KEY+6),(3,PEST)                                              
         GOTO1 MSUNPK,DMCB,KEY+7,PMKT,PSTA                                      
         GOTO1 REPORT                                                           
         GOTO1 SEQ                 CONTINUE THRU ESTIMATE                       
         CLC   KEY(7),KEYSAVE                                                   
         BE    SPT2                                                             
*                                                                               
SPT3     DS    0H                  BUMP TO NEXT EST                             
         MVC   KEY,HLDKEY                                                       
         MVC   KEY+7(6),=6X'FF'                                                 
         B     SPT1C                                                            
*                                                                               
REQL     OI    DELCNT+3,X'0F'                                                   
         MVC   P+30(16),=C'RECORDS DELETED='                                    
         UNPK  P+46(6),DELCNT                                                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
DELCNT   DC    PL4'0'                                                           
         EJECT                                                                  
WORKD    DSECT                                                                  
HLDKEY   DS    CL32                                                             
WORKDL   EQU   *-WORKD                                                          
         SPACE 2                                                                
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
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL1                                                              
PSTA     DS    CL7                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
 END                                                                            
