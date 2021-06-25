*          DATA SET SPREPBT03  AT LEVEL 018 AS OF 08/29/00                      
*PHASE SPBT03A                                                                  
         TITLE 'SPBT03 - SUB-CON FOR BILLING REPORT'                            
         PRINT NOGEN                                                            
SPBT03   CSECT                                                                  
         NMOD1 0,SPBT03                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R8                                                    
         LA    R8,2048(RA)                                                      
         LA    R8,2048(R8)                                                      
*                                                                               
         EJECT                                                                  
NXTCLT   CLI   QCLT,C'$'           TEST OFFICE LIST REQUEST                     
         BNE   NXTCLT4                                                          
         CLI   MODE,CLTLAST        TEST PREVIOUS MODE = CLTLAST                 
         BNE   NXTCLT4                                                          
* OFFICE LIST PROCESSING *                                                      
         GOTO1 FCNXTCLT            GET NEXT CLIENT                              
         BE    NXTCLT2             THERE ARE MORE - CONTINUE                    
         MVI   MODE,OFCLAST        ELSE GIVE LAST OFFICE LAST                   
         GOTO1 GO                                                               
         B     EXIT                                                             
*                                                                               
NXTCLT2  CLI   SVNEWOFC,C'Y'       TEST FOR OFFICE BREAK                        
         BNE   NXTCLT6             NO - CONTINUE                                
         MVI   MODE,OFCLAST                                                     
         GOTO1 GO                                                               
         B     NXTCLT6             ALREADY HAVE NEXT CLT                        
*                                                                               
NXTCLT4  GOTO1 FCNXTCLT                                                         
         BNE   EXIT                                                             
*                                                                               
NXTCLT6  CLI   QCLT,C'$'           TEST OFFICE LIST REQUEST                     
         BNE   NXTCLT8                                                          
         CLI   SVNEWOFC,C'Y'                                                    
         BNE   NXTCLT8                                                          
         MVI   MODE,OFCFRST                                                     
         GOTO1 GO                                                               
*                                                                               
NXTCLT8  MVI   MODE,CLTFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,CLTLAST                                                     
         BE    NXTCLT                                                           
*                                                                               
         CLI   QPGR,C' '           TEST PRDGRPS                                 
         BE    NXTPRD              NO                                           
*                                                                               
NXTPGR   GOTO1 FCNXTPGR                                                         
         BNE   ENDCLT                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,PGR1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVOLDPGR(0),PGR1+1  * EXECUTED *                                 
         BE    NXTPGR2                                                          
*                                                                               
         MVI   MODE,PGR1FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTPGR2  CLC   PGR1LEN,PGR2LEN     TEST 2 PRDGRP LEVELS                         
         BE    NXTPRD              NO.                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,PGR2LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVOLDPGR(0),PGR2+1  * EXECUTED *                                 
         BE    NXTPGR4                                                          
*                                                                               
         MVI   MODE,PGR2FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTPGR4  CLC   PGR2LEN,PGR3LEN     TEST 3 PRDGRP LEVELS                         
         BE    NXTPRD              NO.                                          
         EJECT                                                                  
* ALWAYS HAVE LEVEL 3 FIRST IF 3 LEVELS                                         
         MVI   MODE,PGR3FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTPRD   GOTO1 FCNXTPRD                                                         
         BNE   ENDPGR                                                           
*                                                                               
NXTPRD2  MVI   MODE,PRDFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,PRDLAST                                                     
         BE    NXTPRD                                                           
*                                                                               
NXTEST   GOTO1 FCNXTEST                                                         
         BNE   ENDPRD                                                           
*                                                                               
NXTEST2  MVI   MODE,ESTFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,ESTLAST                                                     
         BE    NXTEST                                                           
*                                                                               
         EJECT                                                                  
*        READ BILLS                                                             
NXTBILL  DS    0H                                                               
         L     RF,ADEST                                                         
         MVC   KEY(13),0(RF)                                                    
         GOTO1 HIGH                                                             
         B     NXTB2B                                                           
*                                                                               
NXTB2    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
NXTB2B   DS    0H                                                               
         CLC   KEY(7),KEYSAVE      TEST THRU PRODUCT                            
         BNE   ENDEST                                                           
         CLI   KEY+8,0             SKIP IF NOT BILL                             
         BE    NXTB2                                                            
*                                                                               
         CLI   BEST,0                                                           
         BE    NXTB4                                                            
         CLC   BEST,KEY+7                                                       
         BE    NXTB5                                                            
         BH    NXTB2                                                            
         CLI   BESTEND,0           EST RANGE?                                   
         BE    NXTB4                                                            
         CLC   BESTEND,KEY+7                                                    
         BL    NXTB2                                                            
         B     NXTB5                                                            
*                                                                               
NXTB4    DS    0H                                                               
         SR    RE,RE               IS ESTIMATE IN LIST                          
         IC    RE,KEY+7                                                         
         LA    RE,ESTLST(RE)                                                    
         CLI   0(RE),0                                                          
         BE    NXTB2               NO, SKIP                                     
NXTB5    DS    0H                                                               
*                                  TEST BILL MONTH                              
         B     NXTB7               **NO-OP**                                    
         MVC   BYTE,KEY+10                                                      
         NI    BYTE,X'F0'                                                       
         ZIC   R3,BYTE                                                          
         SRL   R3,4                                                             
         STC   R3,FULL+1                                                        
         OI    FULL+1,X'F0'                                                     
         MVI   FULL,C'8'                                                        
*                                                                               
         MVC   BYTE,KEY+10                                                      
         NI    BYTE,X'0F'                                                       
         ZIC   R3,BYTE                                                          
         CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL+2(2),DUB                                                    
*                                                                               
         CLC   FULL,QSTART                                                      
         BL    NXTB2                                                            
         CLC   FULL,QEND                                                        
         BH    NXTB2                                                            
*                                                                               
         GOTO1 GETBILL                                                          
         L     R3,ADBILL                                                        
         USING BILLREC,R3                                                       
         CLC   BDATE,QSTART                                                     
         BL    NXTB2                                                            
         CLC   BDATE,QEND                                                       
         BH    NXTB2                                                            
*                                                                               
NXTB7    DS    0H                                                               
         MVI   MODE,PROCBILL                                                    
         GOTO1 GO                                                               
*                                                                               
         B     NXTB2                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
ENDEST   MVI   MODE,ESTLAST                                                     
         GOTO1 GO                                                               
         B     NXTEST                                                           
*                                                                               
ENDPRD   MVI   MODE,PRDLAST                                                     
         GOTO1 GO                                                               
         B     NXTPRD                                                           
*                                                                               
ENDPGR   CLI   QPGR,C' '           TEST PRDGRPS                                 
         BE    ENDCLT                                                           
*                                                                               
         CLC   PGR2LEN,PGR3LEN     TEST 3 PRDGRP LEVELS                         
         BE    ENDPGR2             NO                                           
* ALWAYS HAVE LEVEL 3 BREAK IF 3 LEVELS                                         
         MVI   MODE,PGR3LAST                                                    
         GOTO1 GO                                                               
*                                                                               
ENDPGR2  CLC   PGR1LEN,PGR2LEN     TEST 2 PRDGRP LEVELS                         
         BE    ENDPGR4             NO                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,PGR2LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVNXTPGR(0),PGR2+1  * EXECUTED *                                 
         BE    ENDPGR4                                                          
*                                                                               
         MVI   MODE,PGR2LAST                                                    
         GOTO1 GO                                                               
*                                                                               
ENDPGR4  SR    RE,RE                                                            
         IC    RE,PGR1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVNXTPGR(0),PGR1+1  * EXECUTED *                                 
         BE    ENDPGR6                                                          
*                                                                               
         MVI   MODE,PGR1LAST                                                    
         GOTO1 GO                                                               
*                                                                               
ENDPGR6  B     NXTPGR                                                           
         SPACE 2                                                                
ENDCLT   MVI   MODE,CLTLAST                                                     
         GOTO1 GO                                                               
*                                                                               
         B     NXTCLT                                                           
*                                                                               
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
 END                                                                            
