*          DATA SET PPREPX302  AT LEVEL 052 AS OF 05/01/02                      
*PHASE PPX302A,+0,NOAUTO                                                        
*INCLUDE PPBVAL                                                                 
*INCLUDE MININAM                                                                
         TITLE 'PPX302 - COPY CONTRACT PROGRAM'                                 
*                                                                               
*        THIS PROGRAM COPYS CONTRACT FROM ONE CLIENT TO ANOTHER                 
*                                                                               
*        FROM CLT IN QCLIENT TO CLT IN QPAY(3) COL 53                           
*                                                                               
*        QOPT2 D= DELETE OLD CONTRACT                                           
*                                                                               
*                                                                               
         PRINT NOGEN                                                            
PPX302   CSECT                                                                  
         NMOD1 0,PPX302                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'                                                    
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   CONCNT,=P'0'    CONTRACTS READ                                   
         ZAP   CLTCPY,=P'0'    CLIENT CODE COPIED                               
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   P1+5(11),=C'OLD CLIENT='                                         
         MVC   P1+17(3),QCLIENT                                                 
         MVC   P1+40(11),=C'NEW CLIENT='                                        
         MVC   P1+52(3),QPAY                                                    
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT                                                          
         MVC   P1+10(3),=C'PUB'                                                 
         MVC   P1+25(8),=C'CONTRACT'                                            
         MVC   P1+40(6),=C'ACTION'                                              
         BAS   RE,RPRT                                                          
         MVC   P1+10(3),=C'---'                                                 
         MVC   P1+25(8),=C'--------'                                            
         MVC   P1+40(6),=C'------'                                              
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT                                                          
*                                                                               
         LA    R0,PCONREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      START AT AGENCY/MED                          
         MVI   KEY+3,X'10'         CONTRACTS                                    
         MVC   KEY+4(3),QCLIENT    CLIENT CODE                                  
*                                                                               
         GOTO1 HIGH                                                             
         B     CON4                                                             
*                                                                               
CON3     DS    0H                                                               
         GOTO1 SEQ                                                              
CON4     DS    0H                                                               
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    EXIT                                                             
         CLC   KEY(7),KEYSAVE      AGY/MED/CLT                                  
         BNE   EXIT                END OF CONTRACTS                             
*                                                                               
         GOTO1 GETPRT                                                           
         AP    CONCNT,=P'1'                                                     
         MVI   FLAGPRD,C'Y'        PRODUCT FOUND ERROR                          
         MVI   FLAGRATE,C'Y'       AND RATE ...                                 
*                                                                               
         LA    R2,PCONREC+33       1ST ELEM                                     
         MVI   ELCODE,X'10'        CONTRACT DESCRIPTION ELEM                    
         CLC   ELCODE,0(R2)                                                     
         BE    CON7                                                             
         BAS   RE,NEXTEL                                                        
         BNE   *+14                                                             
CON7     CLC   (PCONPRD-PCONDESC)(3,R2),=3X'40' ANY PRODUCT CONTRACT?           
         BH    CON7D2                           YES                             
*                                                                               
         MVI   FLAGPRD,C'N'                                                     
         CLI   KEY+2,C'N'          IS MEDIA=NEWSPAPER ?                         
         BE    CON7D0              YES                                          
*                                                                               
         LA    R2,PCONREC+33                                                    
         MVI   ELCODE,X'20'        CONTRACT RATE BASIS ELEM (CURRENT)           
         CLC   ELCODE,0(R2)                                                     
         BE    CON7B                                                            
CON7A    BAS   RE,NEXTEL                                                        
         BNE   *+18                                                             
CON7B    CLC   (PRBOPEN-PRBELEM)(5,R2),=5X'40' ANY PROD RATE?                   
         BH    CON7D2                                                           
         B     CON7A                                                            
*                                                                               
         LA    R2,PCONREC+33                                                    
         MVI   ELCODE,X'21'        CONTRACT RATE BASIS ELEM (LOWER LEV)         
         CLC   ELCODE,0(R2)                                                     
         BE    CON7C                                                            
CON7BX   BAS   RE,NEXTEL                                                        
         BNE   *+18                                                             
CON7C    CLC   (PRBOPEN-PRBELEM)(5,R2),=5X'40' ANY PRODUCT RATE?                
         BH    CON7D2                          YES                              
         B     CON7BX                                                           
*                                                                               
         LA    R2,PCONREC+33                                                    
         MVI   ELCODE,X'22'        CONTRACT RATE BASIS ELEM (HIGHER LEV         
         CLC   ELCODE,0(R2)                                                     
         BE    CON7D                                                            
CON7CX   BAS   RE,NEXTEL                                                        
         BNE   *+18                                                             
CON7D    CLC   (PRBOPEN-PRBELEM)(5,R2),=5X'40'  ANY PROD RATE ?                 
         BH    CON7D2                           YES                             
         B     CON7CX                                                           
*                                                                               
CON7D0   MVI   FLAGRATE,C'N'                                                    
         AP    CLTCPY,=P'1'                                                     
*                                                                               
         MVC   OLDKEY,KEY                                                       
         CLI   QOPT2,C'D'          SEE IF DELETING OLD CONTRACT                 
         BNE   CON7D1              NO                                           
         OI    KEY+25,X'80'                  TURN ON DELETE BIT                 
         GOTO1 WRT                                                              
*                                                                               
         MVC   P1+40(5),=C'*DEL*'                                               
*                                             COPY CLIENT CODE                  
CON7D1   XC    KEY,KEY                                                          
         MVC   KEY(25),PCONREC                                                  
         MVC   PCONKCLT,QPAY                  NEW CLT                           
         GOTO1 ADDPRT                                                           
*                                                                               
CON7D2   GOTO1 PUBEDIT,DMCB,PCONKPUB,P1+10    PUB#                              
*                                                                               
         LA    R4,KEY                                                           
         LA    R4,(PCONNUM-PCONKEY)(R4)                                         
         EDIT  (B2,0(R4)),(5,P1+25)           CONTRACT#                         
*                                                                               
         CLI   FLAGPRD,C'Y'                                                     
         BE    PRDERROR                                                         
         CLI   FLAGRATE,C'Y'                                                    
         BE    RATERROR                                                         
*                                                                               
         MVC   KEY,OLDKEY                                                       
         BAS   RE,RPRT                                                          
CON7E    GOTO1 HIGH                                                             
         B     CON3                                                             
*                                                                               
PRDERROR MVC   P1+40(36),=C'*PRODUCT CONTRACT FOUND CANNOT COPY*'               
         LA    R2,(PCONPRD-PCONDESC)(R2)                                        
         MVC   P1+77(3),0(R2)                                                   
         BAS   RE,RPRT                                                          
         B     CON7E                                                            
*                                                                               
RATERROR MVC   P1+40(32),=C'*PRODUCT RATE FOUND CANNOT COPY*'                   
         LA    R2,(PRBOPEN-PRBELEM)(R2)                                         
         MVC   P1+77(5),0(R2)                                                   
         BAS   RE,RPRT                                                          
         B     CON7E                                                            
*                                                                               
RUNL     DS    0H                                                               
         BAS   RE,RPRT                                                          
         MVC   P1(14),=C'CONTRACTS READ'                                        
         EDIT  (P8,CONCNT),(9,P1+26),COMMAS=YES                                 
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(23),=C'CONTRACT COPIED/DELETED'                               
         EDIT  (P8,CLTCPY),(9,P1+26),COMMAS=YES                                 
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
CONCNT   DS    PL8                                                              
CLTCPY   DS    PL8                                                              
FLAGPRD  DS    CL1                                                              
FLAGRATE DS    CL1                                                              
SAVKEY   DS    CL32                                                             
OLDKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
PPBVWORK DS    0D                                                               
       ++INCLUDE PPBVALD                                                        
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052PPREPX302 05/01/02'                                      
         END                                                                    
