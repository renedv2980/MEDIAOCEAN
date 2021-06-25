*          DATA SET PPREPJX02  AT LEVEL 129 AS OF 05/01/02                      
*PHASE PPJX02A                                                                  
         PRINT NOGEN                                                            
         TITLE 'PPJX02  CLIENT PRODUCT ESTIMATE LIST CHANGE LOG'                
*                                                                  L02          
*   ROSA 7/11/88 ADD NEW FIELD FOR CLIENT HEADER // BILLING GROUP  L02          
*                                                                  L02          
*   ROSA 4/27/88 CHECK FIRST POSITION OF PCLTNUM FOR X'FF' - IF SO L01          
*           THE NEXT 2 BYTES ARE PACKED UNSIGNED.                               
*                                                                               
         TITLE 'PPJX02  CLIENT PRODUCT ESTIMATE LISTING'                        
PPJX02   CSECT                                                                  
         NMOD1 0,PPJX02,RR=R9                                                   
*                                                                               
*******************************************************************             
*  QOPT1 = SUPRESS PROFILES                                       *             
*  QOPT2 = SUPRESS BILLING FORMULA                                *             
*  QOPT3 = SUPRESS PRODUCTS AND CLIENTS NOT LINKED TO ESTIMATES   *             
*  QOPT4 = SUPRESS ADDRESSES FOR CLIENTS AND PRODUCTS             *             
*                                                                 *             
*******************************************************************             
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LR    R5,RC                                                            
         A     R5,=F'4096'                                                      
         USING PPFILED,RC,R5                                                    
*                                                                               
*                                                                               
         CLI   MODE,FESTREQ                                                     
         BNE   CKM2                                                             
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDACTV,C'Y'          TO GET ZZZ ESTS                           
         XC    PCLI,PCLI                                                        
         XC    PPROD,PPROD                                                      
*                                                                               
         CLC   QOPT1(2),=C'YY'                                                  
         BNE   CKM1C                                                            
         MVI   RCSUBPRG,30                                                      
         B     EXT                                                              
CKM1C    CLI   QOPT1,C'Y'                                                       
         BNE   CKM1E                                                            
         MVI   RCSUBPRG,10                                                      
         B     EXT                                                              
CKM1E    CLI   QOPT2,C'Y'                                                       
         BNE   EXT                                                              
         MVI   RCSUBPRG,20                                                      
         B     EXT                                                              
*                                                                               
CKM2     CLI   MODE,FESTPRO                                                     
         BNE   EXT                                                              
         MVC   PPGKEY(32),KEY      SAVE PPG'S KEY                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   KEY(32),PPRDKEY                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   PPRDBILL,C'@'                                                    
         BNE   CKM3                                                             
*                                                                               
         MVC   P+1(3),PCLTKCLT     CLIENT CODE                                  
         MVC   P+5(3),PPRDKPRD     PRODUCT CODE                                 
         MVC   P+10(20),PPRDBILL   PRINT OLD NAME                               
         CLI   PPRDLIN1,C'@'                                                    
         BE    CKM2D                                                            
         MVC   PPRDBILL(20),PCLTNAME MOVE IN NEW NAME                           
CKM2D    MVC   P+50(20),PPRDBILL   PRINT NEW NAME                               
         BAS   R8,PRINTIT                                                       
         MVC   P+10(30),PPRDLIN1   ADDRESS -LINE1                               
         MVC   PSECOND+10(30),PPRDLIN2  ADDRESS -LINE2                          
         BAS   R8,PRINTIT          PRINT NAMES                                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   CKM3                                                             
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTO1 PUTPRT              SAVE NEW PRODUCT KEY                         
CKM3     MVC   KEY(32),PPGKEY      RESTORE PPG'S KEY                            
         GOTO1 HIGH                                                             
         B     EXT                                                              
         EJECT                                                                  
PRINTIT  CLI   QCLIENT,C'*'        SEE IF DOING AN OFFICE                       
         BNE   PRNT1                                                            
         MVC   HEAD3(6),=C'OFFICE'                                              
         MVC   HEAD3+7(2),QCLIENT+1                                             
*                                                                               
PRNT1    GOTO1 REPORT                                                           
         BR    R8                                                               
*                                                                               
EXT1L    XIT1  1                                                                
EXT      XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
PCLI     DS    CL3                                                              
PPROD    DS    CL3                                                              
BILSW    DS    CL1                                                              
CESW     DS    CL1                                                              
ESPSW    DS    CL1                                                              
PROFILE  DS    CL32                                                             
PPGKEY   DS    CL32                                                             
BILLINES CSECT                                                                  
         DS    6CL75                                                            
         DC    X'00'                                                            
PROLINES CSECT                                                                  
         DS    32CL75                                                           
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
BILPROFD DSECT                                                                  
       ++INCLUDE PBILPROF                                                       
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'129PPREPJX02 05/01/02'                                      
         END                                                                    
