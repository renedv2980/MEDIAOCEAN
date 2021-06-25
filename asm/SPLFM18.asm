*          DATA SET SPLFM18    AT LEVEL 030 AS OF 05/07/98                      
*PHASE T21918A,+0                                                               
         TITLE 'SPLFM18 - PRODUCT HEADER DELETE'                                
T21918   CSECT                                                                  
         NMOD1 0,T21918                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         CLC   SVEBCPRD,=C'POL'                                                 
         BNE   PDEL0                                                            
*                                  ALLOW POL DELETE ONLY IF LAST PRD            
*              FIRST READ CLTHDR                                                
         USING CLTHDRD,R8                                                       
         MVC   KEY+14(4),SVCLTDA                                                
         GOTO1 GETREC                                                           
         CLC   CLIST(3),=C'POL'                                                 
         BE    PDEL0                                                            
*                                                                               
         LA    R2,LFMACTH                                                       
         MVI   ERRCD,INVERR        CAN'T DELETE PRD POL                         
         B     LFMERR                                                           
         DROP  R8                                                               
*                                                                               
PDEL0    DS    0H                                                               
         USING PRDHDRD,R8                                                       
         OC    SVADVDA,SVADVDA           TEST ADV CLIENT                        
         BZ    PDEL4                                                            
         MVC   KEY,SVKEY                                                        
         NI    KEY+1,X'0F'                                                      
         OC    KEY+1(1),SVADVAGY                                                
         CLC   KEY(13),SVKEY             TEST DOING ADV                         
         BNE   PDEL4             NO                                             
*                                                                               
*                CHECK IF PRODUCT IS OPEN ON ANY AGY                            
*               IF SO CANN'T DELETE IT                                          
*                                                                               
         MVC   KEY+14(4),SVADVDA                                                
         GOTO1 GETREC                                                           
         LA    R7,REC                                                           
         USING ADVHDRD,R7                                                       
         MVC   ELEM(30),ADVLIST        SAVE LIST OF BUYING AGYS                 
*                                                                               
         DROP  R7                                                               
         LA    R6,ELEM-1                                                        
         SR    R0,R0                                                            
PDEL1    LA    R6,1(R6)                                                         
         IC    R0,0(R6)                                                         
         LTR   R0,R0                                                            
         BZ    PDEL4                                                            
         SLL   R0,4                                                             
         STC   R0,DUB                                                           
         MVC   KEY,SVKEY                                                        
         NI    KEY+1,X'0F'                                                      
         OC    KEY+1(1),DUB                                                     
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BNE   PDEL1                                                            
         MVI   ERRCD,APRDERR              PRD FOUND - CANN'T DELETE             
         B     LFMERR                                                           
*                                                                               
PDEL4    ST    R8,AREC        RESET AREC                                        
         CLI   SVFMTSW,0                                                        
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         FOUT  PRDNAMEH,PNAME,20                                                
         B     EXXMOD                                                           
         EJECT                                                                  
EDT      LA    R2,PRDNAMEH                                                      
         GOTO1 ANY                                                              
         CLC   8(6,R2),=C'DELETE'                                               
         BE    EDT1                                                             
         MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
         EJECT                                                                  
*                                                                               
EDT1     EQU   *                                                                
*                                                                               
* MAKE SURE PRODUCT DOESN'T EXIST IN A PRODUCT GROUP                            
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY(2),=X'0D81'     BUILD PGRP PASSIVE POINTER                   
         MVC   KEY+2(1),SVAGYMD    AGENCY/MEDIA                                 
         MVC   KEY+3(2),SVCLT      CLIENT                                       
*                                                                               
         GOTO1 HIGH                READ THE KEY                                 
*                                                                               
EDT1A    EQU   *                                                                
*                                                                               
         CLC   KEY(5),KEYSAVE      SAME BASE KEY (A/M, CLT)?                    
         BNE   EDT1B               NO - SO PRODUCT NOT IN GROUP                 
*                                                                               
         CLC   KEY+8(3),SVEBCPRD   ELSE - SAME PRODUCT?                         
         BE    GRPERR              YES - SO ERROR                               
*                                                                               
         GOTO1 SEQ                 ELSE - GET NEXT KEY                          
         B     EDT1A               LOOP BACK TO COMPARE IT                      
*                                                                               
EDT1B    EQU   *                                                                
*                                                                               
*                            DELETE NOT ALLOWED IF                              
*                            CLIENT HAS MASTER TRAFFIC REFERENCE                
         SPACE                                                                  
         MVC   KEY+14(4),SVCLTDA                                                
         LA    R7,REC2                                                          
         ST    R7,AREC            READ CLTHDR INTO REC2                         
         USING CLTHDRD,R7                                                       
         GOTO1 GETREC                                                           
         OC    CMCLTCOD,CMCLTCOD                                                
         BNZ   MTRAFERR                                                         
         EJECT                                                                  
*                                                                               
*                            SEE IF THERE ARE ANY ORDERED OR PAID               
*                            DOLLARS ON ANY EST AND BE SURE ALL                 
*                            BILLS SUM TO ZERO                                  
*                                                                               
         ZAP   DUB,=P'0'            USED TO TOTAL BILLS                         
         MVC   KEY,SVKEY                                                        
         MVI   KEY+7,X'01'                                                      
         GOTO1 HIGH                                                             
         B     EDT1S                                                            
EDT1H    GOTO1 SEQ                                                              
EDT1S    CLC   KEYSAVE(7),KEY                                                   
         BNE   EDT1X                                                            
         CLI   KEY+8,0                                                          
         BNE   EDTB        MUST BE BILLREC                                      
*                                                                               
EDTE     DS    0H               READ ESTIMATES INTO REC                         
         LA    R7,REC                                                           
         ST    R7,AREC                                                          
         USING  ESTHDRD,R7                                                      
         GOTO1 GETREC                                                           
         LA    R3,EORDN                                                         
         LA    R4,52         FOR BCT                                            
EDTE1    CLC   0(4,R3),=F'0'                                                    
         BE    EDTE2                                                            
         MVI   ERRCD,EDOLERR          ORDERED OR PAID DOLLARS ON AN EST         
         B     LFMERR                                                           
*                                                                               
EDTE2    LA    R3,4(R3)                                                         
         BCT   R4,EDTE1                                                         
         B     EDT1H                                                            
         EJECT                                                                  
EDTB     DS    0H                 BILLRECS                                      
         MVI   ERRCD,BILLONFL     BILL ON FILE - CANNOT DELETE PRD              
         B     LFMERR                                                           
*                                                                               
         LA    R9,REC2               READ INTO REC2                             
         ST    R9,AREC                                                          
         USING BILRECD,R9                                                       
         GOTO1 GETREC                                                           
         PACK  WORK(8),BAMT                                                     
         AP    DUB,WORK(8)                                                      
         B     EDT1H                                                            
         SPACE 2                                                                
EDT1X    DS    0H                                                               
         CP    DUB,=P'0'     BILL RECS SHOULD SUM TO ZERO                       
         BE    EDTG                                                             
         MVI   ERRCD,BILLSNZ                                                    
         B     LFMERR                                                           
*                                  CHECK FOR GOAL RECS                          
*                                  IF FOUND THEN NO DELETE                      
EDTG     MVI   KEY,X'02'                                                        
         MVC   KEY+1(3),KEYSAVE+1     A/M CLT                                   
         MVC   KEY+4(1),SVPCODE                                                 
         XC    KEY+5(7),KEY+5                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   EDTBUY              NO GOALS - GO CHECK FOR BUYS                 
         MVI   ERRCD,PGOALERR                                                   
         B     LFMERR                                                           
*                                                                               
EDTBUY   XC    KEY,KEY                                                          
         MVC   KEY(4),KEYSAVE+1      A/M CLT PRD                                
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   EDT1XCN             NO BUYS - OK TO DELETE                       
         MVI   ERRCD,PGOALERR                                                   
         B     LFMERR                                                           
*                                                                               
*                                                                               
EDT1XCN  CLI   SVAPROF+7,C'C'      CANADIAN AGENCY                              
         BNE   EDT2                                                             
         CLI   SVEBCMED,C'T'       T V                                          
         BNE   EDT2                                                             
*                                                                               
*                                  FOR CANADIAN TV MUST CHECK OTHER             
*                                  MEDIAS - NETWORK AND COMBINED                
*                                                                               
         MVC   WORK(1),KEYSAVE        SEE WHICH I JUST FINISHED                 
         NI    WORK,X'0F'                                                       
         CLI   WORK,X'08'          LAST WAS COMBINED - DONE                     
         BE    EDT2                                                             
         CLI   WORK,X'03'          LAST WAS NETWORK - SO DO COMBINED            
         BNE   EDT1XCN2                                                         
         MVC   KEY,SVKEY                                                        
         MVI   KEY+7,X'01'                                                      
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         B     EDT1S                                                            
*                                                                               
EDT1XCN2 MVC   KEY,SVKEY           LAST WAS TV - SO DO NETWORK                  
         MVI   KEY+7,X'01'                                                      
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         B     EDT1S                                                            
*                                                                               
         DROP  R9                                                               
         DROP  R7                                                               
         EJECT                                                                  
EDT2     MVC   KEY+14(4),SVCLTDA                                                
         LA    R7,REC2               READ CLTHDR INTO REC2                      
         ST    R7,AREC                                                          
         USING CLTHDRD,R7                                                       
         GOTO1 GETREC                                                           
         LA    R4,CLIST                                                         
EDT2A    CLC   0(3,R4),SVKEY+4                                                  
         BE    EDT2B                                                            
         LA    R4,4(R4)                                                         
         CLI   3(R4),0                                                          
         BNE   EDT2A                                                            
         DC    H'0'          PRD NOT IN CLIST - FATAL ERROR                     
*                                                                               
EDT2B    MVC   0(4,R4),4(R4)                                                    
         LA    R4,4(R4)                                                         
         CLI   3(R4),0                                                          
         BNE   EDT2B                                                            
         GOTO1 PUTREC         WRITE BACK CLTHDR                                 
*                                                                               
*        IF CANADIAN TV - UPDATE NWK AND COMBINED CLTHDRS                       
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   PUTCLT8                                                          
         CLI   SVEBCMED,C'T'                                                    
         BNE   PUTCLT8                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),SVKEY                                                     
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PUTCLT2                                                          
         MVC   REC2(13),KEYSAVE                                                 
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 ADDREC                                                           
         B     PUTCLT4                                                          
*                                                                               
PUTCLT2  LA    R0,REC+500                                                       
         ST    R0,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         MVC   REC2(13),REC+500      SWITCH KEYS                                
         GOTO1 PUTREC                                                           
*                                                                               
PUTCLT4  XC    KEY,KEY                                                          
         MVC   KEY(4),SVKEY                                                     
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PUTCLT6                                                          
         MVC   REC2(13),KEYSAVE                                                 
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 ADDREC                                                           
         B     PUTCLT7                                                          
*                                                                               
PUTCLT6  LA    R0,REC+500                                                       
         ST    R0,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         MVC   REC2(13),REC+500      SWITCH KEYS                                
         GOTO1 PUTREC                                                           
*                                                                               
PUTCLT7  MVC   KEY,SVKEY                                                        
*                                                                               
PUTCLT8  DS    0H                                                               
*                                                                               
EDT3     DS    0H         NOW DELETE PRD,ESTS,BILLS                             
         MVC   KEY,SVKEY                                                        
         ST    R8,AREC                                                          
EDT30    GOTO1 HIGH                                                             
         B     EDT3B                                                            
EDT3A    GOTO1 SEQ                                                              
EDT3B    CLC   KEYSAVE(7),KEY                                                   
         BNE   EDT3X                                                            
         OI    KEY+13,X'80'          MARK DELETED                               
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     EDT3A                                                            
*                                                                               
EDT3X    CLI   SVAPROF+7,C'C'                                                   
         BNE   EDT3XX                                                           
         CLI   SVEBCMED,C'T'                                                    
         BNE   EDT3XX                                                           
         MVC   WORK(1),KEYSAVE+1                                                
         NI    WORK,X'0F'                                                       
         CLI   WORK,X'08'          LAST WAS COMBINED - DONE                     
         BE    EDT3XX                                                           
         CLI   WORK,X'03'          LAST WAS NETWORK - DO COMBINED               
         BNE   EDT3X2                                                           
         MVC   KEY,SVKEY                                                        
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         B     EDT30                                                            
*                                                                               
EDT3X2   MVC   KEY,SVKEY           LAST WAS TV - NOW DO NETWORK                 
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         B     EDT30                                                            
*                                                                               
EDT3XX   NI    LFMKEYH+4,X'DF'     SET KEY NOT VALID (ANY MORE)                 
         B     EXXMOD                                                           
         EJECT                                                                  
*                   ERROR EXITS, EQUATES, ETC..                                 
         SPACE                                                                  
LFMERR   GOTO1 ERROR                                                            
*                                                                               
DIVERR   LA    R2,LFMMSGH                                                       
         MVC   8(36,R2),=C'* ERROR * PRODUCT IS IN DIVISION 999'                
         MVC   41(3,R2),REC2+7                                                  
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,LFMRECH                                                       
         OI    6(R2),X'40'         CURSOR                                       
         B     EXXMOD                                                           
*                                                                               
GRPERR   LA    R2,LFMMSGH                                                       
         MVC   8(43,R2),=C'* ERROR * PRODUCT EXISTS FOR A GROUP IN ID '         
         MVC   51(1,R2),KEY+5      PRODUCT GROUP ID                             
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,LFMRECH                                                       
         OI    6(R2),X'40'         CURSOR                                       
         B     EXXMOD                                                           
*                                                                               
MTRAFERR LA    R2,LFMMSGH                                                       
         MVC   8(38,R2),=C'* ERROR * MASTER TRAFFIC CLIENT EXISTS'              
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,LFMRECH                                                       
         OI    6(R2),X'40'         CURSOR                                       
         B     EXXMOD                                                           
*                                                                               
SVPCODE  DS    CL1                                                              
PGOALERR EQU   239                                                              
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFMF8                                                                        
       ++INCLUDE SPLFMF8D                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
BILRECD  DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
ADVHDRD  DSECT                                                                  
       ++INCLUDE SPGENADV                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SPLFM18   05/07/98'                                      
         END                                                                    
