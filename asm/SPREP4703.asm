*          DATA SET SPREP4703  AT LEVEL 121 AS OF 07/27/16                      
*PHASE SP4703A                                                                  
*INCLUDE QSORT                                                                  
*                                                                               
*  AKAT       APR03/12 FIX MGROUP BUG                                           
*  SCHO       APR08/99 ADD CHECK FOR OFFICE NUMBER                              
*  LEV  30    MAR09/88 FIX SPECIFIC MGR REQUEST DROPPING MKTS                   
*  LEV  31    MAR09/88 CHANGE CANCEL TO ABEND                                   
*  LEV  32    SEP19/88 FIX CLT SPEC MGRP SPEC REQ FROM RUNNING AWAY             
*                        TO ALL CLTS                                            
*                                                                               
         TITLE 'SP4703 - SP47 SUB-CONTROLLER'                                   
SP4703   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP4703,RR=R9                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R8                                                    
         LA    R8,2048(RA)                                                      
         LA    R8,2048(R8)                                                      
         USING OFFICED,OFCBLK                                                   
*                                                                               
         L     R5,=A(LDSP4712)                                                  
         ST    R5,ASP4712                                                       
         GOTO1 LOADER,DMCB,=C'SP4712  ',(R5)                                    
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
*&&OS                                                                           
*        MVS JUST TELLS ME WHERE IT WAS LOADED                                  
         MVC   ASP4712,DMCB+4                                                   
*&&OS                                                                           
*                                                                               
         CLI   QCLT,C'$'           OFFICE LIST?                                 
         BNE   TSTPRD                                                           
         GOTO1 =A(LSTOFC)          GET THE LIST IN MYOFCLST                     
*                                                                               
TSTPRD   CLI   QOPT1,C'N'          TEST PRDGRP'S                                
         BE    NXTMCLT                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D81'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         CLC   =C'ALL',QCLT                                                     
         BE    NXTPCLT2                                                         
         SPACE                                                                  
* ONE CLIENT                                                                    
         SPACE                                                                  
         CLI   QCLT,C'$'                                                        
         BE    NXTPCLT2                                                         
         CLI   QCLT,C'*'                                                        
         BE    NXTPCLT2                                                         
*                                                                               
         GOTO1 CLPACK,DMCB,QCLT,SVCLT                                           
         MVC   KEY+3(2),SVCLT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE      A-M/CLT                                      
         BE    NXTPCLT4                                                         
         B     NXTMCLT                                                          
         EJECT                                                                  
* 'ALL' CLIENTS                                                                 
         SPACE                                                                  
NXTPCLT2 GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      A-M                                          
         BNE   NXTMCLT                                                          
*                                                                               
* READ CLTHDR                                                                   
*                                                                               
NXTPCLT4 MVC   SVPGRKEY,KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVPGRKEY+2 A-M/CLT                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETCLT                                                           
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
*                                                                               
         CLI   QCLT,C'$'           TEST OFFICE LIST                             
         BE    NXTPC10                                                          
*                                                                               
         CLI   QCLT,C'*'           TEST OFFICE REQUEST (NEW STYLE)              
         BNE   NXTPGRID                                                         
*                                                                               
         CLI   QCLT+1,C'-'         TEST 'ALL BUT'                               
         BE    NXTPC30                                                          
*                                                                               
         CLC   COFFICE,QCLT+1      OFFICE MATCH?                                
         BE    NXTPGRID            GOOD                                         
         B     NXTPC40             NO GOOD                                      
*                                                                               
NXTPC10  DS    0H                  TEST OFFICE LIST                             
         LA    RE,MYOFCLST                                                      
         LHI   RF,L'MYOFCLST       LENGTH OF OFFICE LIST                        
*                                                                               
NXTPC20  CLC   COFFICE,0(RE)       IS THE OFFICE IN THE LIST?                   
         BE    NXTPGRID                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,NXTPC20                                                       
         B     NXTPC40             NEXT OFFICE ON THE LIST                      
*                                                                               
NXTPC30  DS    0H                  'ALL BUT'                                    
         CLC   COFFICE,QCLT+2      OFFICE MATCH?                                
         BNE   NXTPGRID            NO? GOOD                                     
         B     NXTPC40             YES? NO GOOD                                 
*                                                                               
NXTPC40  MVC   KEY,SVPGRKEY                                                     
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(3),KEYSAVE                                                   
         BNE   NXTMCLT                                                          
         B     NXTPCLT4                                                         
         DROP  R6                                                               
*                                                                               
NXTPGRID XC    SVOLDPGR,SVOLDPGR                                                
*                                                                               
* READ PGRDEF                                                                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   KEY(13),SVPGRKEY                                                 
         MVI   KEY+1,1                                                          
         XC    KEY+6(7),KEY+6      CLEAR PRDGRP                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETPRDGR                                                         
*                                                                               
* READ PRDGRP                                                                   
*                                                                               
NXTPGRP  MVC   KEY(13),SVPGRKEY                                                 
         MVI   KEY+1,1                                                          
         XC    KEY+8(5),KEY+8      CLEAR PRD                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETPRDGR                                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,PGR1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVOLDPGR(0),PGR1+1  * EXECUTED *                                 
         BE    NXTPGRP2                                                         
*                                                                               
         MVI   MODE,PGR1FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTPGRP2 CLC   PGR1LEN,PGR2LEN     TEST 2 PRDGRP LEVELS                         
         BE    NXTPRD                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,PGR2LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVOLDPGR(0),PGR2+1 *EXECUTED*                                    
         BE    NXTPGRP4                                                         
*                                                                               
         MVI   MODE,PGR2FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTPGRP4 CLC   PGR2LEN,PGR3LEN     TEST 3 PRDGRP LEVELS                         
         BE    NXTPRD              NO                                           
         SPACE                                                                  
* ALWAYS HAVE LEVEL 3 FIRST IF 3 LEVELS                                         
         SPACE                                                                  
         MVI   MODE,PGR3FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTPRD   MVC   SVOLDPGR,PGR3+1     SAVE THIS PRDGRP                             
*                                                                               
* READ PRDHDR                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVPGRKEY+2  A-M/CLT                                     
         MVC   KEY+4(3),SVPGRKEY+8 PRD                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NXTPRD4             NOT FOUND - SKIP                             
         GOTO1 GETPRD                                                           
*                                                                               
         MVI   MODE,PRDFRST                                                     
         GOTO1 GO                                                               
*                                                                               
NXTPRD4  MVC   KEY(13),SVPGRKEY                                                 
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         MVC   SVPGRKEY,KEY        SAVE NEW KEY                                 
         SPACE                                                                  
* CHECK BREAK                                                                   
         SPACE                                                                  
         CLC   KEY(8),KEYSAVE      A-M/CLT/PRDGRP                               
         BE    NXTPRD                                                           
         CLC   KEY(6),KEYSAVE      A-M/CLT/PGRPID                               
         BE    NXTPGRP                                                          
         CLC   KEY(5),KEYSAVE      A-M/CLT                                      
         BE    NXTPGRID                                                         
         CLC   =C'ALL',QCLT                                                     
         BE    NXTPRD5                                                          
         CLI   QCLT,C'$'                                                        
         BE    NXTPRD5                                                          
         CLI   QCLT,C'*'                                                        
         BNE   NXTMCLT             CLIENT NO LONGER MATCH                       
NXTPRD5  CLC   KEY(3),KEYSAVE      NOT DOING CLIENT SPECIFIC                    
         BE    NXTPCLT4            IS IT STILL THE SAME A/M?                    
         EJECT                                                                  
NXTMCLT  CLI   QOPT2,C'N'          TEST NO MKTGRPS                              
         BE    EXIT                                                             
         SPACE                                                                  
* TEST MKTBUFF HAS RIGHT AGY/MD                                                 
         SPACE                                                                  
         L     R4,=A(MKTBUFF)                                                   
         CLC   QAGY(3),0(R4)                                                    
         BE    NXTMCLTA                                                         
         SPACE                                                                  
* BUILD NEW MARKET LIST                                                         
         SPACE                                                                  
         MVC   0(3,R4),QAGY                                                     
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
*                                                                               
         L     R6,ADMARKET                                                      
         LA    R4,4(R4)            TABLE POINTER                                
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,(R6)                             
*                                                                               
         LA    RE,DMRSEQ                                                        
         ST    RE,0(R1)                                                         
         B     *+6                                                              
*                                                                               
BLDMKT2  BASR  RE,RF                                                            
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),C'M'          TEST MARKET REC                              
         BNE   BLDMKTX             NO                                           
         CLC   QAGY,6(R6)          TEST RIGHT AGY                               
         BNE   BLDMKT2                                                          
         SPACE                                                                  
* ADD TO TABLE                                                                  
         SPACE                                                                  
         PACK  DUB,2(4,R6)                                                      
         CVB   R0,DUB                                                           
         STH   R0,0(R4)                                                         
         MVC   2(24,R4),18(R6)                                                  
         LA    R4,26(R4)                                                        
         L     R5,=A(MKTBUFFX)                                                  
         CR    R4,R5                                                            
         BL    BLDMKT2                                                          
*                                                                               
         GOTO1 LOGIO,DMCB,1,(44,MKTMSG)                                         
*        CANCEL                                                                 
         ABEND 999                                                              
         SPACE                                                                  
MKTMSG   DC    C'**SP47** INSUFFICIENT MARKET BUFFER SPACE **'                  
*                                                                               
BLDMKTX  MVC   0(2,R4),=X'FFFF'    SET E-O-L FLAG                               
*                                                                               
* SORT SPMGRTAB                                                                 
         LA    R0,MGRSORT                                                       
         LHI   R1,MGRSORTX-MGRSORT                                              
         L     RE,=A(SPMGRTAB)                                                  
         LHI   RF,SPMGRTBX-SPMGRTAB                                             
         MVCL  R0,RE                                                            
*                                                                               
         LHI   R0,(SPMGRTBX-SPMGRTAB)/3                                         
         GOTO1 =V(QSORT),DMCB,MGRSORT,(R0),3,2,0                                
NXTMCLTA LA    RC,MGRSORT                                                       
         MVC   THREE,0(RC)                                                      
*                                                                               
NXTMCLT1 XC    SVPGRKEY,SVPGRKEY                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         CLC   =C'ALL',QCLT                                                     
         BE    NXTMCLT2                                                         
         MVC   SVCLT,QCLT                                                       
         CLI   QCLT,C'*'          TEST OFFICE REQ                               
*        BE    NXTMCL1A                                                         
         BE    NXTMCLT2                                                         
         CLI   QCLT,C'$'                                                        
         BE    NXTMCLT2                                                         
         SPACE                                                                  
* ONE CLIENT                                                                    
         SPACE                                                                  
         GOTO1 CLPACK,DMCB,QCLT,SVCLT                                           
NXTMCL1A MVC   KEY+3(2),SVCLT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BE    NXTMCLT4                                                         
         B     EXIT                                                             
*                                                                               
* 'ALL' CLIENTS                                                                 
*                                                                               
NXTMCLT2 GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
* READ CLTHDR                                                                   
*                                                                               
NXTMCLT4 MVC   SVMGRKEY,KEY                                                     
         OC    SVMGRKEY+3(2),SVMGRKEY+3  TEST CLT PRESENT                       
         BNZ   NXTMCLT6                                                         
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         XC    CKEY,CKEY                                                        
         XC    CLT,CLT                                                          
         MVC   CLTNM,=CL24'ALL CLIENTS'                                         
         B     NXTMGRID                                                         
*                                                                               
NXTMCLT6 DS    0H                                                               
*                                                                               
         CLI   SVMGRKEY+3,C'*'     OFFICE                                       
         BNE   NXTMCLT8                                                         
         MVC   CLT(2),SVMGRKEY+3                                                
         MVI   CLT+2,C' '                                                       
         MVC   CLTNM,=CL24'** OFFICE 9  **'                                     
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVMGRKEY+4                                                
         L     RF,ADCONLST                                                      
         L     RF,VOFFICER-SPADCONS(RF)                                         
         GOTO1 (RF),DMCB,(C'2',OFFICED),(0,ACOMFACS)                            
         MVC   CLTNM+10(2),OFCOFC2                                              
         B     NXTMCLT9                                                         
*                                                                               
NXTMCLT8 XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVMGRKEY+2 A-M/CLT                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    NXTMCLT9                                                         
         B     NMGRID1L            CLT WAS PROBABLY DELETED                     
*                                  SKIP TO NEXT SCHEME                          
NXTMCLT9 GOTO1 GETCLT                                                           
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
*                                                                               
         CLI   QCLT,C'$'           TEST OFFICE LIST                             
         BE    NXTMC10                                                          
*                                                                               
         CLI   QCLT,C'*'           TEST OFFICE REQUEST (NEW STYLE)              
         BNE   NXTMGRID                                                         
*                                                                               
         CLI   QCLT+1,C'-'         TEST 'ALL BUT'                               
         BE    NXTMC30                                                          
*                                                                               
         CLC   COFFICE,QCLT+1      OFFICE MATCH?                                
         BE    NXTMGRID            GOOD                                         
         B     NXTMC40             NO GOOD                                      
*                                                                               
NXTMC10  DS    0H                  TEST OFFICE LIST                             
         LA    RE,MYOFCLST                                                      
         LHI   RF,L'MYOFCLST       LENGTH OF OFFICE LIST                        
*                                                                               
NXTMC20  CLC   COFFICE,0(RE)       IS THE OFFICE IN THE LIST?                   
         BE    NXTMGRID                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,NXTMC20                                                       
         B     NXTMC40             NEXT OFFICE ON THE LIST                      
*                                                                               
NXTMC30  DS    0H                  'ALL BUT'                                    
         CLC   COFFICE,QCLT+2      OFFICE MATCH?                                
         BNE   NXTMGRID            NO? GOOD                                     
         B     NXTMC40             YES? NO GOOD                                 
*                                                                               
NXTMC40  MVC   KEY,SVMGRKEY                                                     
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(3),KEYSAVE                                                   
         BNE   EXIT                                                             
         B     NXTMCLT4                                                         
         DROP  R6                                                               
*                                                                               
NXTMGRID CLI   QMGR,C' '           TEST SCHEME FILTER                           
         BNE   NMGRID1J                                                         
*                                                                               
         CLC   SVMGRKEY+8(1),THREE+2                                            
         BE    NMGRID1M                                                         
NMGRID1D MVC   THREE,0(RC)         READ NEXT/FIRST SCHEME                       
         MVC   KEY(8),SVMGRKEY                                                  
         MVC   KEY+8(1),THREE+2                                                 
         LA    RC,3(RC)            INC TO NEXT SCHEME                           
         MVC   KEY+9(4),=4X'00'                                                 
         GOTO1 HIGH                READ FOR CURR SCHEME                         
*                                                                               
NMGRID1F CLC   KEY(8),KEYSAVE      SAME TY/A-M/CLT/PRDGRP?                      
         BNE   *+10                NO, DON'T SAVE IT                            
         MVC   SVMGRKEY,KEY                                                     
         CLC   KEY(9),KEYSAVE      SAME TY/A-M/CLT/PRDGRP/MGRPID?               
         BE    NMGRID1M            YES                                          
NMGRID1G CLI   0(RC),X'FF'         END OF TABLE?                                
         BE    NMGRID1I            YES                                          
NMGRID1H MVC   KEY(8),SVMGRKEY     RESTORE TY/A-M/CLT/PRDGRP                    
         B     NMGRID1D                                                         
*                                                                               
NMGRID1I LA    RC,MGRSORT                                                       
         MVC   THREE,0(RC)                                                      
         MVC   KEY,SVMGRKEY                                                     
         MVC   KEY+8(5),=5X'FF'    FORCE NEXT PRDGRP                            
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      DID MEDIA CHANGE?                            
         BNE   EXIT                YES, EXIT                                    
         MVC   SVMGRKEY,KEY                                                     
         B     NXTMCLT4                                                         
*                                                                               
NMGRID1J CLC   SVMGRKEY+8(1),QMGR                                               
         BE    NMGRID1M            ID'S MATCH                                   
*                                                                               
NMGRID1L MVC   KEY,SVMGRKEY                                                     
         MVC   KEY+9(4),=4X'FF'    FORCE NEXT SCHEME                            
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BNE   EXIT                DONE                                         
         CLC   =C'ALL',QCLT                                                     
         BE    NXTMCLT4                                                         
         CLI   QCLT,C'$'                                                        
         BE    NXTMCLT4                                                         
         CLI   QCLT,C'*'                                                        
         BE    NXTMCLT4                                                         
         CLC   KEY(5),KEYSAVE                                                   
         BNE   EXIT                                                             
         BE    NXTMCLT4                                                         
*                                                                               
NMGRID1M XC    SVOLDMGR,SVOLDMGR                                                
         MVC   PGR1(42),SPACES                                                  
         MVC   PGR2(42),SPACES                                                  
         MVC   PGR3(42),SPACES                                                  
*                                                                               
* READ PGRDEF IF NEEDED                                                         
*                                                                               
         OC    SVMGRKEY+5(3),SVMGRKEY+5                                         
         BZ    NMGRID4                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(4),SVMGRKEY+2 A-M/CLT/PGRPID                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    NMGRID1O                                                         
         MVC   BYTE,USRSW1                                                      
         MVI   USRSW1,0                                                         
         MVC   P(32),=C'** ERROR ** PRDGRPID X NOT FOUND'                       
         MVC   P+21(1),SVMGRKEY+5                                               
         GOTO1 REPORT                                                           
         MVC   USRSW1,BYTE                                                      
*                                                                               
NMGRID1N CLI   QMGR,C' '           TEST SCHEME FILTER                           
         BE    NMGRID1I                                                         
         MVC   KEY,SVMGRKEY                                                     
         MVC   KEY+8(5),=5X'FF'    FORCE NEXT PRDGRP                            
         GOTO1 HIGH                                                             
         B     NXTMGRID                                                         
*                                                                               
NMGRID1O GOTO1 GETPRDGR                                                         
*                                                                               
NMGRID2  OC    SVMGRKEY+6(2),SVMGRKEY+6 TEST PGRP SPECIFIED                     
         BZ    NMGRID4             NO                                           
         SPACE                                                                  
* READ PRGDRP DETAILS                                                           
         SPACE                                                                  
         MVC   KEY+6(2),SVMGRKEY+6                                              
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE      SAME TY/A-M/CLT/PGRPID                       
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,PGR1LEN          GET PRDGRP BREAK 1 DIGITS                    
         BCTR  RE,0                                                             
         UNPK  DUB,KEY+6(3)                                                     
         UNPK  WORK(8),SVMGRKEY+6(3)                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   DUB+3(0),WORK+3  **EXECUTED**                                    
         BE    NMGRID2C            SHOULD AGREE FOR BREAK 1 DIGITS              
*                                                                               
         MVC   BYTE,USRSW1                                                      
         MVI   USRSW1,0                                                         
         MVC   P(33),=C'** ERROR ** PRDGRP      NOT FOUND'                      
         MVC   P+19(4),WORK+3                                                   
         GOTO1 REPORT                                                           
         MVC   USRSW1,BYTE                                                      
         B     NMGRID1N                                                         
*                                                                               
NMGRID2C GOTO1 GETPRDGR                                                         
         EJECT                                                                  
*                                                                               
* READ MGRDEF                                                                   
*                                                                               
NMGRID4  XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),SVMGRKEY+2 A-M                                          
*                                                                               
         CLI   SVMGRKEY+8,C' '     LESS THAN X'40' IS ALL CLIENT                
         BL    NMGRID5                                                          
         CLI   SVMGRKEY+8,C'F'     ID'S A-F NEED CLIENT                         
         BH    NMGRID5                                                          
         MVC   KEY+3(2),SVMGRKEY+3 CLT                                          
*                                                                               
NMGRID5  MVC   KEY+8(1),SVMGRKEY+8 MGRPID                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    NMGRID6                                                          
         MVC   BYTE,USRSW1                                                      
         MVI   USRSW1,0                                                         
         MVC   P(34),=C'** ERROR ** MKTRGPID XX NOT FOUND'                      
         MVC   P+21(2),MGR1N                                                    
         GOTO1 REPORT                                                           
         MVC   USRSW1,BYTE                                                      
*                                                                               
         CLI   QMGR,C' '           TEST SCHEME FILTER                           
         BNE   NMGRID1L                                                         
         MVC   KEY,SVMGRKEY                                                     
         B     NMGRID1D                                                         
*                                                                               
NMGRID6  GOTO1 GETMKTGR                                                         
*                                                                               
         CLI   QOPT3,C'Y'          SEE IF LISTING MKTS                          
         BNE   NMGRID8                                                          
         MVC   SVHDHOOK,HEADHOOK      SAVE MY HEADHOOK                          
         MVI   MODE,PROCREC                                                     
         GOTO1 ASP4712,DMCB,(RA)                                                
         MVC   HEADHOOK,SVHDHOOK                                                
*                                                                               
*                                  FORCE NEW PAGE FOR CHANGE OF SCHEME          
NMGRID8  MVI   FORCEHED,C'Y'                                                    
         XC    SVOLDMGR,SVOLDMGR                                                
*                                                                               
* READ MKTGRP                                                                   
*                                                                               
NXTMGRP  XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(9),SVMGRKEY+2  A-M/CLT/PRDGRP/MKTGRP                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    NXTMGRP1                                                         
         MVC   BYTE,USRSW1                                                      
         MVI   USRSW1,0                                                         
         MVC   P(28),=C'** ERROR ** MKTGRP NOT FOUND'                           
         GOTO1 HEXOUT,DMCB,KEYSAVE,P+30,13                                      
         GOTO1 REPORT                                                           
         MVC   USRSW1,BYTE                                                      
*                                                                               
         CLC   KEY(9),KEYSAVE      SAME A-M/CLT/PRDGRP/MGRPID?                  
         BNE   NMGRID1L            NO, GET NEXT SCHEME                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVMGRKEY                                                 
         MVC   KEY+11(2),=2X'FFFF'                                              
         GOTO1 HIGH                                                             
         B     NMGRID1F                                                         
*                                                                               
NXTMGRP1 GOTO1 GETMKTGR                                                         
*                                                                               
***      SR    RE,RE                                                            
***      IC    RE,MGR1LEN                                                       
***      BCTR  RE,0                                                             
***      EX    RE,*+8                                                           
***      B     *+10                                                             
***      CLC   SVOLDMGR(0),MGR1+1 *EXECUTED*                                    
***      BE    NXTMGRP2                                                         
*                                                                               
         MVI   MODE,MGR1FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTMGRP2 CLC   MGR1LEN,MGR2LEN     TEST 2 LEVELS OF MKTGRP                      
         BE    NXTMKT                                                           
*                                                                               
***      SR    RE,RE                                                            
***      IC    RE,MGR2LEN                                                       
***      BCTR  RE,0                                                             
***      EX    RE,*+8                                                           
***      B     *+10                                                             
***      CLC   SVOLDMGR(0),MGR2+1 *EXECUTED*                                    
***      BE    NXTMGRP4                                                         
*                                                                               
         MVI   MODE,MGR2FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTMGRP4 CLC   MGR2LEN,MGR3LEN     TEST 3 MKTGRP LEVELS                         
         BE    NXTMKT                                                           
* ALWAYS HAVE LEVEL 3 FIRST IF 3 LEVELS                                         
         MVI   MODE,MGR3FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTMKT   MVC   SVOLDMGR,MGR3+1     SAVE THIS MKTGRP                             
*                                                                               
* FIND MKTNAME IN BUFFER                                                        
*                                                                               
         LH    R0,SVMGRKEY+11                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKT,DUB                                                          
         MVC   MKTNM(24),=CL24'*** U N K N O W N ***'                           
*                                                                               
         L     R4,=A(MKTBUFF)                                                   
         LA    R4,4(R4)                                                         
*                                                                               
NMKT2    CLC   0(2,R4),SVMGRKEY+11                                              
         BE    NMKT4                                                            
         BH    NMKT6                                                            
         LA    R4,26(R4)                                                        
         B     NMKT2                                                            
*                                                                               
NMKT4    MVC   MKTNM,2(R4)                                                      
*                                                                               
NMKT6    DS    0H                                                               
         MVC   KEY(13),SVMGRKEY                                                 
*                                                                               
         MVI   MODE,MKTFRST                                                     
         GOTO1 GO                                                               
*                                                                               
         MVC   KEY(13),SVMGRKEY                                                 
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(3),KEYSAVE                                                   
         BNE   NMKT8E              END OF AGY/MED                               
         MVC   SVMGRKEY,KEY                                                     
         SPACE                                                                  
* CHECK BREAK                                                                   
         SPACE                                                                  
         CLC   KEY(11),KEYSAVE     A-M/CLT/PRDGRP/MKTGRP                        
         BE    NXTMKT                                                           
         CLC   KEY(9),KEYSAVE      A-M/CLT/PRDGRP/MGRPID                        
         BE    NXTMGRP                                                          
*                                                                               
         CLI   QMGR,C' '          SEE IF DOING ONE SCHEME ID                    
         BE    NMKT6F                                                           
         CLC   KEY(5),KEYSAVE      A-M/CLT                                      
         BE    NMGRID1J            YES GO TO NEXT SCHEME                        
         CLC   =C'ALL',QCLT                                                     
         BE    NMKT6D                                                           
         CLI   QCLT,C'$'                                                        
         BE    NMKT6D                                                           
         CLI   QCLT,C'*'                                                        
         BNE   EXIT                                                             
NMKT6D   CLC   KEY(3),KEYSAVE      A-M                                          
         BNE   EXIT                                                             
*        B     NMGRID1J            YES GO TO NEXT SCHEME                        
         B     NXTMCLT4                                                         
         SPACE                                                                  
NMKT6F   CLC   KEY(8),KEYSAVE      A-M/CLT/PRDGRP                               
         BE    NMGRID1G            MGRPID CHANGED, CHECK TABLE                  
         CLC   KEY(5),KEYSAVE      A-M/CLT                                      
         BE    NMGRID1G            PRDGRP CHANGED, CHECK TABLE                  
NMKT8    CLC   =C'ALL',QCLT        CLIENT CHANGED!!!                            
         BE    NMKT8C                                                           
         CLI   QCLT,C'$'                                                        
         BE    NMKT8C                                                           
         CLI   QCLT,C'*'                                                        
         BNE   NMKT8E                                                           
NMKT8C   CLC   KEY(3),KEYSAVE      A-M                                          
         BNE   NMKT8E                                                           
         CLI   0(RC),X'FF'                                                      
         BNE   NMKT8D                                                           
         LA    RC,MGRSORT                                                       
         MVC   THREE,0(RC)                                                      
         B     NXTMCLT4                                                         
NMKT8D   CLC   0(3,RC),MGRSORT     BEGINNING OF NEW TABLE?                      
         BE    NXTMGRID                                                         
         MVC   KEY,KEYSAVE                                                      
         MVC   SVMGRKEY,KEYSAVE                                                 
         B     NMGRID1G                                                         
*                                                                               
NMKT8E   CLI   QMGR,C' '           TEST SCHEME FILTER                           
         BNE   EXIT                                                             
         CLI   0(RC),X'FF'                                                      
         BNE   NMGRID1H                                                         
*                                                                               
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*****  BUILD OFFICE LIST  *****                                                 
LSTOFC   NTR1                                                                   
         L     R3,ADCONLST                                                      
         USING SPADCONS,R3                                                      
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
*                                                                               
         XC    MYOFCLST,MYOFCLST                                                
*                                                                               
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,QCLT                                                     
         MVC   OFCLMT(3),QCLT                                                   
         MVI   OFCLMT+3,C' '                                                    
         MVC   OFCAGY,SVAGY                                                     
         MVC   OFCOFC,COFFICE                                                   
         GOTO1 VOFFICER,DMCB,OFFICED,(X'D0',ACOMFACS),MYOFCLST                  
LSTOFCX  XIT1                                                                   
         DROP  R3                                                               
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*****************************                                                   
ASP4712  DS    A                                                                
SVHDHOOK DS    F                                                                
OFCBLK   DS    XL(OFCLENQ)                                                      
         DS    0D                                                               
MYOFCLST DS    CL128                                                            
*                                                                               
MGRSORT  DS    0F                                                               
         DS    XL(SPMGRTBX-SPMGRTAB) SORTED MGRTAB                              
MGRSORTX EQU   *                                                                
         DC    X'FF'               END OF TABLE                                 
*                                                                               
MKTBUFF  DC    F'0'                                                             
         DS    110500C             4250 X 26                                    
MKTBUFFX EQU   *-1                                                              
         DS    CL2                                                              
         DS    0D                                                               
LDSP4712 DS    12000C                                                           
*                                                                               
       ++INCLUDE SPMGRTAB                                                       
*                                                                               
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
 END                                                                            
