*          DATA SET SPREPI203  AT LEVEL 040 AS OF 08/10/06                      
*PHASE SPI203B                                                                  
         TITLE 'SPI203 - I2 PROGRAM SUBCONTROLLER'                              
         PRINT NOGEN                                                            
SPI203   CSECT                                                                  
         NMOD1 0,SPI203                                                         
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
         XC    MYKEYSV,MYKEYSV   CLEAR KEY FOR CBL STATION/NET COMPARE          
         MVI   CBHLSW,0                                                         
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
         CLI   MODE,PRDLAST        DID USER FORCE PRDLAST                       
         BE    ENDPRD              (IT WAS PROBABLY SAAD)                       
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
***                                                                             
         CLC   =C'ALL ',QSTA       IF SPECIFIC SYSCODE/NETWORK REQ              
         BE    NXTSTA2                                                          
         CLI   QCBLNET,C' '                                                     
         BNH   NXTSTA2                                                          
         CLC   SVSTA,KEY+6         GET REQUESTED STATION                        
         BNE   NXTGL                                                            
***                                                                             
*                                                                               
NXTSTA2  DS    0H                                                               
         CLI   BIGSTA+4,C'/'       IF DOING CABLE HEADS                         
         BNE   NS04                CHECK IF CBHFRST                             
         MVC   WORK(13),KEY                                                     
         NI    WORK+8,X'80'        FORGET NETWORK - STATION+2, 7 BITS           
         CLC   MYKEYSV(9),WORK     TEST NEW CABLE HEAD                          
         BE    NS04                                                             
***                                                                             
* THIS CODE IS MOVED DOWN AFTER WE PROCESS ALL THE BUYS.  WE SHOULD             
* NEVER DO MODE CBHLAST HERE BECAUSE FCNXTSTA GETS THE NEXT BUY, SO             
* THE SUMMARY IS SCREWED UP AND TO MAKE MATTERS WORSE, THIS MODE                
* WILL OVERWRITE THE BUY RECORD WITH A TRAFFIC RECORD, WHICH WILL MAKE          
* PROCBUY THINK WE ARE DOING SPILL AND IGNORE THE BUYS THAT BUY                 
***                                                                             
*****    CLI   CBHLSW,C'N'         HAVE WE DONE CBHLAST?                        
*****    BNE   NS02                                                             
*****    MVI   MODE,CBHLAST        LAST FOR CABLE HEAD                          
*****    GOTO1 GO                                                               
*****    MVI   CBHLSW,C'Y'         HAVE WE DONE CBHLAST?                        
****                                                                            
NS02     DS    0H                                                               
         CLI   QCBLNET,C' '        SINGLE NETWORK REQUEST                       
         BH    NS04                THEN SKIP CABLE STUFF                        
****                                                                            
         MVI   MODE,CBHFRST                                                     
         GOTO1 GO                                                               
         MVI   CBHLSW,C'N'         SET HAVE NOT DONE CBHLAST                    
         MVC   MYKEYSV,KEY                                                      
*                                                                               
NS04     DS    0H                                                               
         MVI   MODE,STAFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,STALAST                                                     
         BE    NXTSTA                                                           
         EJECT                                                                  
NXTBUY   DS    0H                                                               
         MVC   MYKEYSV,KEY       SAVE KEY                                       
         NI    MYKEYSV+8,X'80'   FORGET NETWORK - STATION+2, 7 BITS             
*                                                                               
         MVI   MODE,PROCBUY                                                     
         GOTO1 GO                                                               
*                                                                               
         GOTO1 FCNXTBUY                                                         
         BE    NXTBUY                                                           
*                                                                               
         MVI   MODE,STALAST                                                     
         GOTO1 GO                                                               
         CLI   BIGSTA+4,C'/'       IF DOING CABLE HEADS                         
         BNE   NB05                                                             
         MVC   WORK(13),KEY        NOTE- KEY WILL HAVE NEXT 'STATION'           
         NI    WORK+8,X'80'        FORGET NETWORK - STATION+2, 7 BITS           
         CLC   MYKEYSV(9),WORK     TEST NEW CABLE HEAD                          
         BNE   NB03                                                             
****                                                                            
         B     NB04                SKIP EST CHECK - HANDLED BY FILCON           
****                                                                            
*&&DO                                                                           
         CLI   BEST,0              EVEN IF STATION OK, LOOK AT EST              
         BE    NB02                                                             
         CLC   KEY+9(1),BEST                                                    
         BNE   NB03                                                             
*                                                                               
NB02     DS    0H                                                               
         CLI   BESTEND,0                                                        
         BE    NB04                                                             
         CLC   KEY+9(1),BESTEND                                                 
         BNH   NB04                                                             
         B     NB05                                                             
*&&                                                                             
*                                                                               
NB03     DS    0H                                                               
         CLI   CBHLSW,C'N'        DO WE NEED CBHLAST?                           
         BE    NB03A              YES! - WE DID CABLE FIRST!                    
         CLC   =C'ALL ',QSTA      IF DOING ALL STATIONS                         
**>      CLC   =C'ALL /',QSTA      IF DOING ALL STATIONS                        
         BNE   NB05                                                             
*                                                                               
NB03A    MVI   CBHLSW,C'Y'         THIS WILL FIX MATCH BUG-BUT PRINTS           
         MVI   MODE,CBHLAST        LAST FOR CABLE HEAD          MORE            
         GOTO1 GO                                                               
         XC    MYKEYSV,MYKEYSV     CLEAR                                        
         B     NB05                                                             
*                                                                               
NB04     DS    0H                                                               
         MVC   MYKEYSV,KEY         CBL/NET                                      
         NI    MYKEYSV+8,X'80'     CLEAR NETWORK                                
*                                                                               
NB05     DS    0H                                                               
         B     NXTSTA                                                           
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
ENDMKT   DS    0H                                                               
         CLI   CBHLSW,C'Y'         IF HAVEN'T DONE CBH LAST                     
         BE    ENDMKT2                                                          
         CLI   BIGSTA+4,C'/'       BUT DOING CABLE HEADS                        
         BNE   ENDMKT2                                                          
         MVI   MODE,CBHLAST        DO IT NOW                                    
         GOTO1 GO                                                               
         MVI   CBHLSW,C'Y'         WE HAVE DONE CBHLAST                         
ENDMKT2  DS    0H                                                               
         MVI   MODE,MKTLAST                                                     
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
*                                                                               
MYKEYSV  DC    XL13'00'                                                         
CBHLSW   DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040SPREPI203 08/10/06'                                      
         END                                                                    
