*          DATA SET SPREPK403  AT LEVEL 002 AS OF 08/29/00                      
*PHASE SPK403A                                                                  
SPK403   TITLE '- SUB-CON FOR ALLOCATIONS (GOALS THEN BUYS)'                    
SPK403   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPK403                                                       
         L     RA,0(R1)                                                         
         LA    R8,1(RA)                                                         
         LA    R8,4095(R8)                                                      
         USING SPWORKD,RA,R8                                                    
                                                                                
NXTCLT   GOTO1 FCNXTCLT                                                         
         BNE   EXIT                                                             
         MVI   MODE,CLTFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,CLTLAST                                                     
         BE    NXTCLT                                                           
         CLI   QPGR,C' '           TEST PRDGRPS                                 
         BE    NXTPRD              NO                                           
                                                                                
NXTPGR   GOTO1 FCNXTPGR                                                         
         BNE   ENDCLT                                                           
         SR    RE,RE                                                            
         IC    RE,PGR1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    NXTPGR2                                                          
         CLC   SVOLDPGR(0),PGR1+1                                               
         MVI   MODE,PGR1FRST                                                    
         GOTO1 GO                                                               
                                                                                
NXTPGR2  CLC   PGR1LEN,PGR2LEN     TEST 2 PRDGRP LEVELS                         
         BE    NXTPRD              NO                                           
         SR    RE,RE                                                            
         IC    RE,PGR2LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    NXTPGR4                                                          
         CLC   SVOLDPGR(0),PGR2+1                                               
         MVI   MODE,PGR2FRST                                                    
         GOTO1 GO                                                               
                                                                                
NXTPGR4  CLC   PGR2LEN,PGR3LEN     TEST 3 PRDGRP LEVELS                         
         BE    NXTPRD              NO                                           
         MVI   MODE,PGR3FRST       ALWAYS HAVE LEVEL 3 FIRST IF 3 LVLS          
         GOTO1 GO                                                               
                                                                                
NXTPRD   GOTO1 FCNXTPRD                                                         
         BNE   ENDPGR                                                           
                                                                                
NXTPRD2  MVI   MODE,PRDFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,PRDLAST                                                     
         BE    NXTPRD                                                           
                                                                                
NXTEST   GOTO1 FCNXTEST                                                         
         BNE   ENDPRD                                                           
         MVI   MODE,ESTFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,ESTLAST                                                     
         BE    NXTEST                                                           
         CLI   QMGR,C' '           TEST MKTGRPS                                 
         BE    NXTMKT              NO                                           
                                                                                
NXTMGR   GOTO1 FCNXTMGR                                                         
         BNE   ENDEST                                                           
         SR    RE,RE                                                            
         IC    RE,MGR1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    NXTMGR2                                                          
         CLC   SVOLDMGR(0),MGR1+1                                               
         MVI   MODE,MGR1FRST                                                    
         GOTO1 GO                                                               
                                                                                
NXTMGR2  CLC   MGR1LEN,MGR2LEN     TEST 2 MKTGRP LEVELS                         
         BE    NXTMKT              NO.                                          
         SR    RE,RE                                                            
         IC    RE,MGR2LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    NXTMGR4                                                          
         CLC   SVOLDMGR(0),MGR2+1                                               
         MVI   MODE,MGR2FRST                                                    
         GOTO1 GO                                                               
                                                                                
NXTMGR4  CLC   MGR2LEN,MGR3LEN     TEST 3 MKTGRP LEVELS                         
         BE    NXTMKT              NO                                           
         MVI   MODE,MGR3FRST       ALWAYS HAVE LEVEL 3 FIRST IF 3 LVLS          
         GOTO1 GO                                                               
                                                                                
NXTMKT   GOTO1 FCNXTMKT                                                         
         BNE   ENDMGR                                                           
         MVI   MODE,MKTFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,MKTLAST                                                     
         BE    NXTMKT                                                           
                                                                                
NXTGL    OC    SVGLKEY,SVGLKEY     TEST GOALS THIS MKT                          
         BZ    NXTGL4                                                           
         MVC   KEY(13),SVGLKEY                                                  
         GOTO1 FCFRSGL                                                          
         BNE   NXTGL4                                                           
                                                                                
NXTGL2   MVI   MODE,PROCGOAL                                                    
         GOTO1 GO                                                               
         GOTO1 FCNXTGL                                                          
         BE    NXTGL2                                                           
                                                                                
NXTGL4   OC    SVBUYKEY,SVBUYKEY   TEST BUYS THIS MKT                           
         BZ    ENDMKT                                                           
         MVC   KEY(13),SVBUYKEY                                                 
         GOTO1 FCFRSBUY                                                         
         B     NXTSTA2                                                          
                                                                                
NXTSTA   GOTO1 FCNXTSTA                                                         
         BNE   ENDMKT                                                           
                                                                                
NXTSTA2  MVI   MODE,STAFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,STALAST                                                     
         BE    NXTSTA                                                           
                                                                                
NXTBUY   MVI   MODE,PROCBUY                                                     
         GOTO1 GO                                                               
         GOTO1 FCNXTBUY                                                         
         BE    NXTBUY                                                           
         MVI   MODE,STALAST                                                     
         GOTO1 GO                                                               
         CLI   MODE,REREAD         REREAD STATION                               
         BNE   NXTSTA                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(9),SVBUYKEY                                                  
         GOTO1 FCFRSBUY                                                         
         B     NXTBUY                                                           
                                                                                
ENDMKT   MVI   MODE,MKTLAST                                                     
         GOTO1 GO                                                               
         B     NXTMKT                                                           
                                                                                
ENDMGR   CLI   QMGR,C' '           TEST MKTGRPS                                 
         BE    ENDEST              NO.                                          
         CLC   MGR2LEN,MGR3LEN     TEST 3 MKTGRP LEVELS                         
         BE    ENDMGR2             NO                                           
         MVI   MODE,MGR3LAST                                                    
         GOTO1 GO                                                               
                                                                                
ENDMGR2  CLC   MGR1LEN,MGR2LEN     TEST 2 MKTGRP LEVELS                         
         BE    ENDMGR4             NO                                           
         SR    RE,RE                                                            
         IC    RE,MGR2LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    ENDMGR4                                                          
         CLC   SVNXTMGR(0),MGR2+1                                               
         MVI   MODE,MGR2LAST                                                    
         GOTO1 GO                                                               
                                                                                
ENDMGR4  SR    RE,RE                                                            
         IC    RE,MGR1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    ENDMGR6                                                          
         CLC   SVNXTMGR(0),MGR1+1                                               
         MVI   MODE,MGR1LAST                                                    
         GOTO1 GO                                                               
                                                                                
ENDMGR6  B     NXTMGR                                                           
                                                                                
ENDEST   MVI   MODE,ESTLAST                                                     
         GOTO1 GO                                                               
         CLC   =C'ALL',QEST                                                     
         BNE   NXTEST                                                           
         MVI   MODE,PRDLAST                                                     
         GOTO1 GO                                                               
         B     NXTPRD2                                                          
                                                                                
ENDPRD   MVI   MODE,PRDLAST                                                     
         GOTO1 GO                                                               
         B     NXTPRD                                                           
                                                                                
ENDPGR   CLI   QPGR,C' '           TEST PRDGRPS                                 
         BE    ENDCLT                                                           
         CLC   PGR2LEN,PGR3LEN     TEST 3 PRDGRP LEVELS                         
         BE    ENDPGR2             NO                                           
         MVI   MODE,PGR3LAST                                                    
         GOTO1 GO                                                               
                                                                                
ENDPGR2  CLC   PGR1LEN,PGR2LEN     TEST 2 PRDGRP LEVELS                         
         BE    ENDPGR4             NO                                           
         SR    RE,RE                                                            
         IC    RE,PGR2LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    ENDPGR4                                                          
         CLC   SVNXTPGR(0),PGR2+1                                               
         MVI   MODE,PGR2LAST                                                    
         GOTO1 GO                                                               
                                                                                
ENDPGR4  SR    RE,RE                                                            
         IC    RE,PGR1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    ENDPGR6                                                          
         CLC   SVNXTPGR(0),PGR1+1                                               
         MVI   MODE,PGR1LAST                                                    
         GOTO1 GO                                                               
                                                                                
ENDPGR6  B     NXTPGR                                                           
                                                                                
ENDCLT   MVI   MODE,CLTLAST                                                     
         GOTO1 GO                                                               
         B     NXTCLT                                                           
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPK403 08/29/00'                                      
         END                                                                    
