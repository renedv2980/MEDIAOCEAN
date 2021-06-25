*          DATA SET SPREPK503  AT LEVEL 002 AS OF 05/15/07                      
*PHASE SPK503A                                                                  
SPK503   TITLE '- Sub-controller for Network Allocation Program'                
SPK503   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,**SPK503,CLEAR=YES                                         
         USING WORKD,RC                                                         
         L     RA,0(R1)                                                         
         LR    R8,RA                                                            
         AHI   R8,4096                                                          
         USING SPWORKD,RA,R8                                                    
                                                                                
         GOTOR FCNXTCLT                                                         
         BNE   EXIT                                                             
         MVI   MODE,CLTFRST                                                     
         GOTOR GO                                                               
                                                                                
         GOTOR FCNXTPRD                                                         
         BNE   EXIT                                                             
                                                                                
NXTEST   GOTOR FCNXTEST                                                         
         BNE   EXIT                                                             
         MVI   MODE,ESTFRST                                                     
         GOTOR GO                                                               
                                                                                
         L     RF,ADEST            EXTRACT ESTIMATE                             
         MVC   EST#,EKEYEST-ESTHDR(RF)                                          
                                                                                
         CLC   =C'ALL',QMKT        TEST 'ALL' MARKET REQUEST                    
         BE    NXTGOL                                                           
         CLC   QMKT,SPACES                                                      
         BE    NXTGOL                                                           
         PACK  DUB,QMKT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,MKT#                                                        
                                                                                
         USING GXKEY,WKEY                                                       
NXTGOL   XC    GXKEY,GXKEY         INITIALIZE GOAL KEY                          
         MVI   GXKEYTYP,GKEYTYPQ                                                
         MVC   GXKEYAM,BAGYMD                                                   
         MVC   GXKEYCLT,BCLT                                                    
         MVC   GXKEYMKT,MKT#                                                    
         MVC   GXKEYEST,EST#                                                    
                                                                                
NXTGOL02 MVC   WKEYSAVE,WKEY       READ HIGH FOR GOAL KEY                       
         GOTOR DATAMGR,DMCB,DMRDHI,XSPDIR,WKEY,WKEY                             
         B     NXTGOL06                                                         
                                                                                
NXTGOL04 MVC   WKEYSAVE,WKEY       READ SEQUENTIAL FOR GOAL KEY                 
         GOTOR DATAMGR,DMCB,DMREAD,XSPDIR,WKEY,WKEY                             
         GOTOR TRACEIO                                                          
         GOTOR DATAMGR,DMCB,DMRSEQ,XSPDIR,WKEY,WKEY                             
                                                                                
NXTGOL06 GOTOR TRACEIO                                                          
         CLC   GXKEY(GXKEYPRD-GXKEY),WKEYSAVE                                   
         BNE   ENDEST                                                           
                                                                                
         OC    MKT#,MKT#           TEST 'ALL' MARKET REQUEST                    
         BZ    NXTGOL10                                                         
         CLC   GXKEYMKT,MKT#       TEST MARKET MATCHES                          
         BE    NXTGOL10                                                         
         BH    NXTGOL08                                                         
         MVC   GXKEYMKT,MKT#                                                    
         B     NXTGOL12                                                         
                                                                                
NXTGOL08 SR    R0,R0               BUMP PRODUCT                                 
         IC    R0,GXKEYPRD                                                      
         CHI   R0,255                                                           
         BE    ENDEST                                                           
         AHI   R0,1                                                             
         STC   R0,GXKEYPRD                                                      
         MVC   GXKEYMKT,MKT#                                                    
         B     NXTGOL12                                                         
                                                                                
NXTGOL10 CLC   GXKEYEST,EST#       TEST ESTIMATE MATCHES                        
         BE    NXTGOL14                                                         
         BL    NXTGOL12                                                         
         OC    MKT#,MKT#           TEST 'ALL' MARKET REQUEST                    
         BNZ   NXTGOL08                                                         
         SR    R0,R0               BUMP MARKET                                  
         ICM   R0,3,GXKEYMKT                                                    
         AHI   R0,1                                                             
         STCM  R0,3,GXKEYMKT                                                    
                                                                                
NXTGOL12 MVC   GXKEYEST,EST#       SET ESTIMATE                                 
         XC    GXKEYDPT(GXKCNTRL-GXKEYDPT),GXKEYDPT                             
         B     NXTGOL02                                                         
                                                                                
NXTGOL14 TM    GXKEYAGY,GXKEYTAR   IGNORE 'PLANNED' GOALS                       
         BNZ   NXTGOL04                                                         
                                                                                
         GOTOR DATAMGR,DMCB,GETREC,XSPFIL,GXKDA,ADGOAL,DMWORK                   
         GOTOR TRACEIO                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MODE,PROCNGOL                                                    
         GOTOR GO                                                               
         B     NXTGOL04                                                         
                                                                                
ENDEST   CLC   =C'ALL',QEST                                                     
         BNE   NXTEST                                                           
         B     EXIT                                                             
         EJECT                                                                  
TRACEIO  CLI   RCTRACE,C'Y'        TEST TRACE ACTIVE                            
         BNER  RE                                                               
                                                                                
TRACE    NTR1  ,                                                                
         LM    R2,R5,DMCB                                                       
         MVC   P,SPACES                                                         
         MVC   P(8),0(R2)                                                       
         MVC   P+10(8),0(R3)                                                    
                                                                                
         CLC   DMREAD,0(R2)                                                     
         BE    TRACED                                                           
         CLC   DMRDHI,0(R2)                                                     
         BE    TRACED                                                           
         CLC   DMRSEQ,0(R2)                                                     
         BE    TRACED                                                           
         CLC   GETREC,0(R2)                                                     
         BE    TRACEF                                                           
         DC    H'0'                                                             
                                                                                
TRACED   CLC   SPTDIR,0(R3)                                                     
         BNE   TRACED02                                                         
         MVC   P+20(20),WKEYSAVE                                                
         GOTOR HEXOUT,HEXP,WKEYSAVE,P+45,20,=C'N'                               
         GOTOR REPORT                                                           
         MVC   P+20(20),WKEY                                                    
         GOTOR HEXOUT,HEXP,WKEY,P+45,20,=C'N'                                   
         GOTOR REPORT                                                           
         B     EXIT                                                             
                                                                                
TRACED02 CLC   XSPDIR,0(R3)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+20(32),WKEYSAVE                                                
         GOTOR HEXOUT,HEXP,WKEYSAVE,P+45,32,=C'N'                               
         GOTOR REPORT                                                           
         MVC   P+20(32),WKEY                                                    
         GOTOR HEXOUT,HEXP,WKEY,P+45,32,=C'N'                                   
         GOTOR REPORT                                                           
         B     EXIT                                                             
                                                                                
TRACEF   GOTOR HEXOUT,HEXP,(R4),P+20,4,=C'N'                                    
         GOTOR HEXOUT,HEXP,(R5),P+30,40,=C'N'                                   
         GOTOR REPORT                                                           
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
XSPDIR   DC    C'XSPDIR  '                                                      
XSPFIL   DC    C'XSPFIL  '                                                      
                                                                                
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
HEXP     DS    6F                                                               
MKT#     DS    XL(L'GKEYMKT)                                                    
EST#     DS    XL(L'EKEYEST)                                                    
WKEY     DS    XL64                                                             
WKEYSAVE DS    XL64                                                             
WORKL    EQU   *-WORKD                                                          
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPK503 05/15/07'                                      
         END                                                                    
