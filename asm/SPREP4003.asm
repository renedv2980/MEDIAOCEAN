*          DATA SET SPREP4003  AT LEVEL 005 AS OF 06/09/99                      
*PHASE SP4003T,*,NOAUTO                                                         
         TITLE 'SP4003 - ACTIVE MARKET LISTING - SUBCONTROLLER'                 
SP4003   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP4003                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         SPACE 1                                                                
*NOP     GOTO1 CLPACK,DMCB,QCLT,SVCLT                                           
SP05     GOTO1 FCNXTCLT                                                         
         BNE   SPEXIT                                                           
         MVI   MODE,CLTFRST        INITIALIZE                                   
         GOTO1 GO                                                               
         SPACE 1                                                                
* BUY RECORDS *                                                                 
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),SVAGYMD      A-M                                          
         MVC   KEY+1(2),SVCLT      CLT                                          
SP10     DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      SAME A-M/CLT?                                
         BNE   SP20                                                             
         MVI   MODE,PROCBUY                                                     
         GOTO1 GO                                                               
         SPACE 1                                                                
         MVC   KEY+6(3),=3X'FF'    FORCE READ OF NEXT MARKET                    
         B     SP10                                                             
         SPACE 1                                                                
* MARKET GROUPS *                                                               
         SPACE 1                                                                
SP20     DS    0H                                                               
         MVI   MODE,MGR1LAST                                                    
         GOTO1 GO                                                               
         SPACE 1                                                                
* GOAL RECORDS *                                                                
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,2               GOAL RECORD                                  
         MVC   KEY+1(1),SVAGYMD    A-M                                          
         MVC   KEY+2(2),SVCLT      CLT                                          
SP30     DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      SAME 02/A-M/CLT?                             
         BNE   SP40                                                             
         MVI   MODE,PROCGOAL                                                    
         GOTO1 GO                                                               
         SPACE 1                                                                
         MVC   KEY+7(3),=3X'FF'    FORCE READ OF NEXT MARKET                    
         B     SP30                                                             
         SPACE 1                                                                
SP40     DS    0H                                                               
         MVI   MODE,CLTLAST                                                     
         GOTO1 GO                                                               
         B     SP05                GET NEXT CLIENT                              
         SPACE 1                                                                
SPEXIT   XIT1                                                                   
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPREP4003 06/09/99'                                      
         END                                                                    
