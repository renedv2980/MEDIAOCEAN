*          DATA SET ACREP4202  AT LEVEL 016 AS OF 01/27/95                      
*PHASE AC4202A,+0                                                               
         TITLE ' GENERAL BALANCE REMOVAL'                                       
AC4202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC4202,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC42D,RC                                                         
         EJECT                                                                  
*              MAIN CONTROL                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   RB2                                                              
         MVI   FORCEHED,C'Y'                                                    
         ZAP   ACCTOT,=P'0'       TOTAL COUNTER                                 
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
*                                                                               
RB2      CLI   MODE,PROCACC                                                     
         BNE   RB50                                                             
         CLI   QOPT1,C'Y'          LOCKED ONLY                                  
         BNE   RB5                                                              
         L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         TM    ACSTSTAT,X'20'                                                   
         BZ    XIT                                                              
                                                                                
RB5      DS    0H                                                               
         L     R4,ADCMPEL          DOES COMPANY SUPPORT 2 BYTE OFFICE?          
         USING ACCOMPD,R4                                                       
         TM    ACMPSTA4,X'01'      SUPPORTED?                                   
         BZ    RB45                NO, DON'T DO IT                              
*                                                                               
         USING ACKEYD,R5                                                        
         L     R4,ADACC                                                         
         LA    R5,IO                                                            
         MVC   SVKEY,0(R4)                                                      
         MVC   IO(L'SVKEY),SVKEY                                                
         MVI   IO+16,X'41'         HAVE TO GET FIRST OFFICE                     
RB10     GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,ACKEYACC,IO                           
RB13     CLC   SVKEY(15),IO        SEE IF SAME ACCOUNT                          
         BNE   RB45                NO, DONE                                     
*                                                                               
         CLC   17(L'SVKEY-17,R5),SPACES    REST HAS TO BE SPACES                
         BE    RB20                                                             
RB15     MVC   17(L'SVKEY-17,R5),SPACES    GET NEXT OFFICE                      
         ZIC   R1,16(R5)                                                        
         LA    R1,1(R1)                                                         
         STC   R1,16(R5)                                                        
         B     RB10                                                             
*                                                                               
RB20     LA    RF,ACRECORD          POINT TO FIRST ELEMENT                      
         GOTO1 =A(SHOWAMT),DMCB,(1,(RF)),(R5)                                   
         CLI   RCWRITE,C'N'                                                     
         BE    RB15                                                             
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,ACKEYACC,IO                            
         B     RB15                CONTINUE LOOKING                             
*                                                                               
RB45     GOTO1 =A(SHOWAMT),DMCB,(0,ADACCBAL),ADACC                              
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         MVI   MODE,WRITACC                                                     
         B     XIT                                                              
*                                                                               
RB50     CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         GOTO1 ACREPORT                                                         
         MVC   P+32(17),=C'TOTAL FOR REQUEST'                                   
         EDIT  ACCTOT,(14,P+54),2,MINUS=YES                                     
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
* ROUTINE TO SHOW AMOUNTS AND HANDLE TOTAL                          *           
*-------------------------------------------------------------------*           
         SPACE 1                                                                
SHOWAMT  NTR1                                                                   
         SR    R3,R3                                                            
         CLI   0(R1),0             ACCOUNT                                      
         BE    *+6                                                              
         LR    R3,R1               R3 SET FOR OFFICE ACCOUNT                    
*                                                                               
         L     R4,0(R1)                                                         
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING ACBALD,R4                                                        
         L     R2,4(R1)                                                         
         CP    ACBLFRWD,=P'0'                                                   
         BE    XIT                                                              
         MVC   P+32(12),3(R2)                                                   
*                                                                               
         LTR   R3,R3                                                            
         BZ    SAMT10                                                           
         MVC   P+46(2),15(R2)      SHOW OFFICE                                  
         B     *+10                                                             
*                                                                               
SAMT10   AP    ACCTOT,ACBLFRWD                                                  
         EDIT  ACBLFRWD,(14,P+54),2,MINUS=YES                                   
         GOTO1 ACREPORT                                                         
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         ZAP   ACBLFRWD,=P'0'                                                   
         B     XIT                                                              
*                                                                               
XIT      XMOD1 1                                                                
         EJECT                                                                  
ACCFIL   DC    CL8'ACCOUNT'                                                     
         LTORG                                                                  
         EJECT                                                                  
AC42D    DSECT                                                                  
OFFICE2  DS    CL1                                                              
ACCTOT   DS    PL8                                                              
SVKEY    DS    CL42                                                             
IO       DS    2000C                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACREP4202 01/27/95'                                      
         END                                                                    
