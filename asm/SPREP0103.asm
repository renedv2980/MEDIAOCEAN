*          DATA SET SPREP0103  AT LEVEL 004 AS OF 08/29/00                      
*PHASE SP0103A                                                                  
         TITLE 'SP0103 - TEST PROGRAM SUBCONTROLLER'                            
         PRINT NOGEN                                                            
SP0003   CSECT                                                                  
         NMOD1 0,SP0003                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R8                                                    
         LA    R8,2048(RA)                                                      
         LA    R8,2048(R8)                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
NXTCLT   CLI   QCLT,C'$'           TEST OFFICE LIST REQUEST                     
         BNE   NXTCLT4                                                          
         CLI   MODE,CLTLAST        TEST PREVIOUS MODE = CLTLAST                 
         BNE   NXTCLT4                                                          
* OFFICE LIST PROCESSING                                                        
         GOTO1 FCNXTCLT            GET NEXT CLIENT                              
         BE    NXTCLT2             THERE ARE MORE - CONTINUE                    
         MVI   MODE,OFCLAST        ELSE GIVE LAST OFFICE LAST                   
         GOTO1 GO                                                               
         B     EXIT                                                             
*                                                                               
NXTCLT2  CLI   SVNEWOFC,C'Y'       TEST FOR OFFICE BREAK                        
         BNE   NXTCLT4             NO - CONTINUE                                
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
         MVI   MODE,ESTFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,ESTLAST                                                     
         BE    NXTEST                                                           
         EJECT                                                                  
         CLI   QMGR,C' '           TEST MKTGRPS                                 
         BE    NXTMKT              NO                                           
*                                                                               
NXTMGR   GOTO1 FCNXTMGR                                                         
         BNE   ENDEST                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,MGR1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVOLDMGR(0),MGR1+1  * EXECUTED *                                 
         BE    NXTMGR2                                                          
*                                                                               
         MVI   MODE,MGR1FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTMGR2  CLC   MGR1LEN,MGR2LEN     TEST 2 MKTGRP LEVELS                         
         BE    NXTMKT              NO.                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,MGR2LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVOLDMGR(0),MGR2+1  * EXECUTED *                                 
         BE    NXTMGR4                                                          
*                                                                               
         MVI   MODE,MGR2FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTMGR4  CLC   MGR2LEN,MGR3LEN     TEST 3 MKTGRP LEVELS                         
         BE    NXTMKT              NO                                           
* ALWAYS HAVE LEVEL 3 FIRST IF 3 LEVELS                                         
         MVI   MODE,MGR3FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTMKT   GOTO1 FCNXTMKT                                                         
         BNE   ENDMGR                                                           
*                                                                               
         MVI   MODE,MKTFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,MKTLAST                                                     
         BE    NXTMKT                                                           
*                                                                               
         OC    SVBUYKEY,SVBUYKEY   TEST BUYS THIS MKT                           
         BZ    NXTGL               NO.                                          
         CLI   FCRDGOAL,C'Y'       TEST READ GOALS                              
         BNE   *+12                                                             
         CLI   KEY,2               TEST GOAL KEY (NO BUYS)                      
         BE    NXTGL                                                            
         CLI   FCRDGOAL,C'B'       OR BILLS                                     
         BNE   *+12                                                             
         CLI   KEY,X'0E'                                                        
         BE    NXTGL                                                            
         CLI   FCRDGOAL,C'P'       OR PEPSI                                     
         BNE   *+14                                                             
         CLC   KEY(2),=X'0D15'                                                  
         BE    NXTGL                                                            
         B     NXTSTA2                                                          
*                                                                               
NXTSTA   GOTO1 FCNXTSTA                                                         
         BNE   NXTGL                                                            
*                                                                               
NXTSTA2  MVI   MODE,STAFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,STALAST                                                     
         BE    NXTSTA                                                           
         EJECT                                                                  
NXTBUY   MVI   MODE,PROCBUY                                                     
         GOTO1 GO                                                               
*                                                                               
         GOTO1 FCNXTBUY                                                         
         BE    NXTBUY                                                           
*                                                                               
         MVI   MODE,STALAST                                                     
         GOTO1 GO                                                               
*                                                                               
         CLI   MODE,REREAD         REREAD STATION                               
         BNE   NXTSTA                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(9),SVBUYKEY                                                  
         CLI   FCRDBUYS,C'I'         IF READING ID POINTERS                     
         BNE   *+10                                                             
         MVC   KEY+9(2),SVBUYKEY+9   RESTORE 11 BYTES                           
         GOTO1 FCFRSBUY                                                         
         B     NXTBUY                                                           
*                                                                               
NXTGL    OC    SVGLKEY,SVGLKEY     TEST GOALS THIS MKT                          
         BZ    ENDMKT                                                           
*                                                                               
         MVC   KEY(13),SVGLKEY                                                  
         GOTO1 FCFRSGL                                                          
         BNE   ENDMKT                                                           
*                                                                               
NXTGL2   MVI   MODE,PROCGOAL                                                    
         GOTO1 GO                                                               
*                                                                               
         GOTO1 FCNXTGL                                                          
         BE    NXTGL2                                                           
*                                                                               
         B     ENDMKT                                                           
         EJECT                                                                  
ENDMKT   MVI   MODE,MKTLAST                                                     
         GOTO1 GO                                                               
         B     NXTMKT                                                           
*                                                                               
ENDMGR   CLI   QMGR,C' '           TEST MKTGRPS                                 
         BE    ENDEST              NO.                                          
*                                                                               
         CLC   MGR2LEN,MGR3LEN     TEST 3 MKTGRP LEVELS                         
         BE    ENDMGR2             NO                                           
* ALWAYS HAVE LEVEL 3 BREAK IF 3 LEVELS                                         
         MVI   MODE,MGR3LAST                                                    
         GOTO1 GO                                                               
*                                                                               
ENDMGR2  CLC   MGR1LEN,MGR2LEN     TEST 2 MKTGRP LEVELS                         
         BE    ENDMGR4             NO                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,MGR2LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVNXTMGR(0),MGR2+1  * EXECUTED *                                 
         BE    ENDMGR4                                                          
*                                                                               
         MVI   MODE,MGR2LAST                                                    
         GOTO1 GO                                                               
*                                                                               
ENDMGR4  SR    RE,RE                                                            
         IC    RE,MGR1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVNXTMGR(0),MGR1+1  * EXECUTED *                                 
         BE    ENDMGR6                                                          
*                                                                               
         MVI   MODE,MGR1LAST                                                    
         GOTO1 GO                                                               
*                                                                               
ENDMGR6  B     NXTMGR                                                           
         EJECT                                                                  
ENDEST   MVI   MODE,ESTLAST                                                     
         GOTO1 GO                                                               
         CLC   =C'ALL',QEST                                                     
         BNE   NXTEST                                                           
         MVI   MODE,PRDLAST                                                     
         GOTO1 GO                                                               
         B     NXTPRD2                                                          
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
         BNE   ENDPGR2X                                                         
         CLC   =C'999',SVNXTPGR    IF NEXT IS DUMMY                             
         BNE   ENDPGR4             ALWAYS FORCE BREAK                           
*                                                                               
ENDPGR2X MVI   MODE,PGR2LAST                                                    
         GOTO1 GO                                                               
*                                                                               
ENDPGR4  SR    RE,RE                                                            
         IC    RE,PGR1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVNXTPGR(0),PGR1+1  * EXECUTED *                                 
         BNE   ENDPGR4X                                                         
         CLC   =C'999',SVNXTPGR    IF NEXT IS DUMMY                             
         BNE   ENDPGR6             ALWAYS FORCE A BREAK                         
*                                                                               
ENDPGR4X MVI   MODE,PGR1LAST                                                    
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
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREP0103 08/29/00'                                      
         END                                                                    
