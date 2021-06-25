*          DATA SET SPLFM19    AT LEVEL 008 AS OF 08/11/00                      
*PHASE T21919A                                                                  
         TITLE 'SPLFM19 - PRDHDR LOCK/UNLOCK'                                   
T21919   CSECT                                                                  
         NMOD1 0,T21919                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         EJECT                                                                  
         MVC   ELEM(6),=C'000000'                                               
         MVC   ELEM+6(6),=X'FFFFFFFFFFFF'                                       
         LA    R2,PRDSTDTH                                                      
         CLI   5(R2),3                                                          
         BNE   PRL2                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    PRL10                                                            
*                                                                               
* EDIT START/END DATES                                                          
*                                                                               
PRL2     DS    0H                                                               
         GOTO1 ANY                                                              
         MVI   ERRCD,INVERR                                                     
         GOTO1 VDATVAL,DMCB,8(R2),ELEM                                          
         OC    0(4,R1),0(R1)                                                    
         BZ    LFMERR                                                           
*                                                                               
         LA    R2,PRDNDDTH                                                      
         GOTO1 ANY                                                              
         GOTO1 VDATVAL,DMCB,8(R2),ELEM+6                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    LFMERR                                                           
*                                                                               
PRL10    MVI   ERRCD,EBSERR                                                     
         CLC   ELEM+6(6),ELEM      TEST END BEFORE START                        
         BL    LFMERR                                                           
*                                                                               
         ZAP   HALF,=P'0'                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       A-M/CLT/PRD                                  
         GOTO1 HIGH                                                             
*                                                                               
PRL12    GOTO1 SEQ                                                              
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   PRLX                                                             
         OC    KEY+8(5),KEY+8      TEST BILL DATA PRESENT                       
         BNZ   PRL12                                                            
*                                                                               
         GOTO1 GETREC              GET ESTHDR                                   
*                                                                               
         USING ESTHDRD,R8                                                       
*                                                                               
         CLC   EEND,ELEM           EST END BEFORE REQ START                     
         BL    PRL12               YES                                          
         CLC   ESTART,ELEM+6       EST START AFTER REQ END                      
         BH    PRL12               YES                                          
*                                                                               
         CLI   SVACT,C'L'                                                       
         BE    PRL16                                                            
* ACTION IS UNLOCK                                                              
         TM    ECNTRL,X'0C'        TEST HELD/LOCKED                             
         BZ    PRL12               NO                                           
         NI    ECNTRL,X'F3'                                                     
         NI    KEY+13,X'F3'                                                     
         B     PRL20                                                            
*                                                                               
* ACTION IS LOCK                                                                
*                                                                               
PRL16    TM    ECNTRL,X'0C'                                                     
         BNZ   PRL12                                                            
         OI    ECNTRL,X'0C'                                                     
         OI    KEY+13,X'0C'                                                     
*                                                                               
PRL20    AP    HALF,=P'1'          BUMP COUNTER                                 
         GOTO1 PUTREC                                                           
         MVC   SVKEY,KEY           SO EST WILL BE IN SVKEY                      
         GOTO1 CNCHASPT                                                         
         MVC   KEY(13),SVKEY                                                    
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 DIR                                                              
         MVC   KEY+13(1),ECNTRL                                                 
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                WRITE BACK  POINTER                           
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   PRL22                                                            
         CLI   SVEBCMED,C'T'                                                    
         BNE   PRL22                                                            
*                                  NEED TO WRITE BACK NWRK AND COMB             
*                                  POINTERS                                     
         MVC   KEY,SVKEY                                                        
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'         NWRK                                         
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 DIR                                                              
         MVC   KEY+13(1),ECNTRL                                                 
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         MVC   KEY,SVKEY                                                        
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'         COMB                                         
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 DIR                                                              
         MVC   KEY+13(1),ECNTRL                                                 
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
*                                                                               
PRL22    MVC   KEY,SVKEY           RESET FOR SEQ READ                           
         GOTO1 HIGH                                                             
         B     PRL12                                                            
*                                                                               
         EJECT                                                                  
PRLX     MVI   ERRCD,NOSTCHG                                                    
         XC    SVKEY+7(5),SVKEY+7         TO CLEAR EST - USED FOR CAN           
         CP    HALF,=P'0'                                                       
         BE    LFMERR                                                           
*                                                                               
         OI    HALF+1,X'0F'                                                     
         UNPK  LFMMSG+24(3),HALF                                                
         MVC   LFMMSG+28(14),=C'STATUS CHANGES'                                 
         B     EXXMOD                                                           
*                                                                               
LFMERR   GOTO1 ERROR                                                            
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
* SPLFMWRK                                                                      
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
* SPLFMF9D                                                                      
       ++INCLUDE SPLFMF9D                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
* SPGENEST                                                                      
       ++INCLUDE SPGENEST                                                       
 END                                                                            
