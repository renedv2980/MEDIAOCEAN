*          DATA SET SPREPFX02D AT LEVEL 007 AS OF 12/01/98                      
*          DATA SET SPREPFX02B AT LEVEL 004 AS OF 11/25/98                      
*PHASE SPFX02D                                                                  
         TITLE 'SPFX02 - DELETE CANADIAN NETWORK RECORDS FOR PTTO'              
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
FX10     LA    R4,NETLIST                                                       
*                                                                               
FX20     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),QAGY                                                    
         MVC   KEY+4(4),0(R4)                                                   
         CLI   4(R4),C' '                                                       
         BE    FX25                                                             
         GOTO1 CLPACK,DMCB,4(R4),KEY+8                                          
FX25     MVC   KEY+10(1),7(R4)     ESTIMATE                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         BAS   RE,PRTIT                                                         
*                                                                               
FX30     LA    R4,8(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   FX20                                                             
*                                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTIT    NTR1                                                                   
         MVC   P+6(4),KEY+4                                                     
         MVC   P+10(3),4(R4)                                                    
         SR    R0,R0                                                            
         ICM   R0,1,7(R4)                                                       
         BZ    PRTIT2                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+14(3),DUB                                                      
PRTIT2   EQU   *                                                                
         GOTO1 HEXOUT,DMCB,KEY,P+20,13,=C'TOG'                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
NETLIST  DS    0D                                                               
         DC    C'CIIICBP',X'00'                                                 
         DC    C'CIIICBP',X'05'                                                 
         DC    C'CIIICBP',X'06'                                                 
         DC    C'CIIICBP',X'07'                                                 
         DC    C'CIIICBP',X'08'                                                 
         DC    C'CBCACWR',X'02'                                                 
         DC    C'BBS COH',X'00'                                                 
         DC    C'ATV CWR',X'00'                                                 
         DC    C'ATV CWR',X'02'                                                 
         DC    C'ATV CWR',X'05'                                                 
         DC    C'ATV CWR',X'06'                                                 
         DC    C'ATV CWR',X'07'                                                 
         DC    C'ATV CWR',X'08'                                                 
         DC    C'ASN CWR',X'00'                                                 
         DC    C'ASN CWR',X'02'                                                 
         DC    C'ASN CWR',X'07'                                                 
         DC    C'ASN CWR',X'08'                                                 
         DC    C'CBCM   ',X'00'                                                 
         DC    C'SASK   ',X'00'                                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
PMED     DS    CL1                                                              
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PLDA     DS    CL8                                                              
         DS    CL1                                                              
PLELDSP  DS    CL10                                                             
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPFX02D12/01/98'                                      
         END                                                                    
