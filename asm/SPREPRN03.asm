*          DATA SET SPREPRN03  AT LEVEL 054 AS OF 02/25/15                      
*PHASE SPRN03A                                                                  
         TITLE 'SPREPRN03    NETWORK REPORT CONTROLLER'                         
SPRN03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPRN03                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         MVC   PAGE,=H'1'                                                       
NXTCLT   GOTO1 FCNXTCLT                                                         
         BNE   EXIT                                                             
         MVI   MODE,CLTFRST                                                     
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
         B     *+10                                                             
         CLC   SVOLDPGR(0),PGR2+1  * EXECUTED *                                 
         BE    NXTPGR4                                                          
*                                                                               
         MVI   MODE,PGR2FRST                                                    
         GOTO1 GO                                                               
NXTPGR4  CLC   PGR2LEN,PGR3LEN     TEST 3 PRDGRP LEVELS                         
         BE    NXTPRD              NO.                                          
         EJECT                                                                  
* ALWAY HAVE LEVEL 3 FIRST IF 3 LEVELS                                          
         MVI   MODE,PGR3FRST                                                    
         GOTO1 GO                                                               
*                                                                               
NXTPRD   GOTO1 FCNXTPRD                                                         
         BNE   ENDPGR                                                           
NXTPRD2  MVI   MODE,PRDFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,PRDLAST                                                     
         BE    NXTPRD                                                           
*                                                                               
NXTEST   GOTO1 FCNXTEST                                                         
         BNE   ENDPRD                                                           
* CLEAR TABLE OF BUYIDS                                                         
         L     R0,BUYLIST                                                       
         L     R1,=A(BUYLISTL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    IDCOUNT,IDCOUNT                                                  
         MVI   IDPASS,C'S'         (GETS RESET TO 'B' IF NOT QBYID)             
*                                                                               
         MVI   MODE,ESTFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,ESTLAST                                                     
         BE    NXTEST                                                           
*                                                                               
         MVC   PAGE,=H'1'                                                       
         B     NETNTR                                                           
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
*                                                                               
ENDCLT   MVI   MODE,CLTLAST                                                     
         GOTO1 GO                                                               
         B     NXTCLT                                                           
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*                                  READ PRD HDR                                 
         XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   BCLT,2(R6)          NEED TO SET BCLT FOR MEDPRDRD                
         MVC   KEY(4),0(R6)                                                     
         MVC   KEY+4(3),QPRD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRDFND                                                           
         MVC   P(19),=C'PRODUCT NOT ON FILE'                                    
         GOTO1 REPORT                                                           
         B     NETXX                                                            
*                                                                               
PRDFND   DS    0H                                                               
         L     R6,ADPRD                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING PRDHDRD,R6                                                       
         MVC   BPRD,PCODE+1                                                     
         MVC   PRD,KEY+4                                                        
         MVC   PRDNM(20),PNAME                                                  
*                                  READ ESTIMATE                                
         PACK  DUB,QEST(3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,KEY+7                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    ESTFND                                                           
         MVC   P(20),=C'ESTIMATE NOT ON FILE'                                   
         GOTO1 REPORT                                                           
         B     NETXX                                                            
*                                                                               
ESTFND   DS    0H                                                               
         L     R6,ADEST                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING ESTHDRD,R6                                                       
         MVC   BEST,KEY+7                                                       
         MVC   EST,QEST                                                         
         MVC   ESTNM(20),EDESC                                                  
         MVC   ESTDATES,ESTART                                                  
         CLI   QSTART,C' '                                                      
         BNE   *+10                                                             
         MVC   QSTART(12),ESTART                                                
*                                                                               
*                                  NEED TO READ EQUIVHDR                        
         MVC   DUB(3),QAGY                                                      
         MVC   DUB+3(2),BCLT                                                    
         GOTO1 EQVRD,DMCB,DUB,DPEQTAB,ADBUY,DATAMGR                             
         L     RE,ADBUY                                                         
         MVC   EQTAB,0(RE)                                                      
*                                  READ DAYPART HDR                             
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB(3),QAGY                                                     
         MVC   DMCB+3(1),EDAYMENU                                               
         GOTO1 DPTRD,DMCB,,ADDPTTAB                                             
         DROP  R6                                                               
*                                                                               
NETNTR   XC    BMKT(5),BMKT                                                     
         CLC   QSTA(4),=C'ALL '    CHECK IF DOING ALL NETWORKS                  
         BE    NETA1                                                            
*                                                                               
         CLI   QSTA+4,C' '                                                      
         BNE   *+10                                                             
         MVC   QSTA+4(1),QMED                                                   
         GOTO1 MSPACK,DMCB,=C'0000',QSTA,BMKT                                   
         MVI   MODE,ESTFRST                                                     
         GOTO1 GO                                                               
NETA1    MVC   SVSTA,BSTA          ENSURE REREAD CORRECT STN 4 ALL NWK          
*                                                                               
*                                  NOW READ FIRST OR ONLY BUY                   
NETA     XC    KEY,KEY                                                          
         MVC   KEY(1),BAGYMD                                                    
         GOTO1 CLPACK,DMCB,QCLT,KEY+1                                           
         MVC   KEY+3(1),BPRD                                                    
         MVC   KEY+6(3),SVSTA                                                   
         MVC   KEY+9(1),BEST                                                    
         MVI   NETACT,0            RESET ACTIVITY SWITCH                        
         CLI   QOPT1,C'0'                                                       
         BL    NET1                NO LINE NUMBER IN REQ                        
         PACK  DUB,QOPT1(3)                                                     
         CVB   R0,DUB                                                           
         STC   R0,KEY+11                                                        
*                                                                               
NET1     DS    0H                                                               
         GOTO1 HIGH                                                             
         CLI   QBYID,C'Y'                                                       
         BE    NET4                                                             
         MVI   IDPASS,C'B'                                                      
         XC    CURRID,CURRID                                                    
         XC    IDWORK,IDWORK                                                    
         B     NET4                                                             
*                                                                               
NET2     GOTO1 SEQ                                                              
*                                                                               
NET4     CLC   KEY(9),KEYSAVE                                                   
         BE    NET4C                                                            
         CLI   IDPASS,C'S'                                                      
         BNE   *+14                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    NET4C               CONTINUE FINDING IDS TO SAVE                 
         MVC   SVSTA,KEYSAVE+6     REMEMBER THIS STATION DONE                   
         B     NETBX               END OF MKT/STA                               
*                                                                               
NET4C    CLI   BEST,0              NO ESTIMATE REQUEST                          
         BNE   NET4E                                                            
         SR    RE,RE               CHECK IF IN REQUEST PERIOD                   
         IC    RE,KEY+9                                                         
         LA    RE,ESTLST(RE)                                                    
         CLI   0(RE),0             NO - SET FOR NEXT READ                       
         BNE   NET5                                                             
         MVC   KEY+10(3),=4X'FF'                                                
         B     NET1                                                             
         SPACE 2                                                                
NET4E    CLC   KEY+9(1),BEST       ESTIMATE GROUP OR SINGLE EST                 
         BE    NET5                EQUAL IS ALWAYS OK                           
         BNL   NET4K                                                            
         MVC   KEY+9(1),BEST       LOOK FOR THAT ESTIMATE                       
         XC    KEY+10(3),KEY+10                                                 
         B     NET1                                                             
*                                                                               
NET4K    CLC   KEY+9(1),BESTEND    FAILS HERE IF NOT A GROUP                    
         BNH   NET5                                                             
         OC    BMKT(5),BMKT        SEE IF NETWORK GIVEN                         
         BNZ   NETBX                                                            
         MVC   KEY+9(4),=4X'FF'    NO, GET NEXT STATION                         
         B     NET1                                                             
*                                                                               
NET5     OC    BMKT(5),BMKT        SEE IF NETWORK GIVEN                         
         BZ    NET6                                                             
         CLI   QOPT1,C'0'                                                       
         BL    NET6                                                             
         CLC   KEY+11(1),KEYSAVE+11                                             
         BE    NET6                LINES MATCH                                  
         MVC   P(31),=C'BUY-LINE DELETED OR NOT ON FILE'                        
         GOTO1 REPORT                                                           
         B     NETXX                                                            
*                                                                               
NET6     DS    0H                                                               
         MVC   STA(5),QSTA                                                      
         OC    BMKT(5),BMKT        SEE IF NETWORK GIVEN                         
         BNZ   NET20                                                            
         GOTO1 MSUNPK,DMCB,KEY+4,WORK,STA                                       
NET20    MVI   MODE,STAFRST        (MAY NEED STATION NAME)                      
         GOTO1 GO                                                               
         L     R7,ADBUY                                                         
         ST    R7,AREC                                                          
         GOTO1 GET                                                              
         XC    SVPKGEL,SVPKGEL                                                  
         BAS   RE,PKGTEST                                                       
         B     PKGB                SLAVE RETURN                                 
         B     PROCESS             REG OR MASTER RETURN                         
*                                                                               
PKGB     CLI   QOPT1,C'0'          SEE IF DOING ONE LINE                        
         BNL   PROCESS             YES - PROCESS SLAVE                          
         B     NET2                NO - BYPASS SLAVES                           
         EJECT                                                                  
PROCESS  DS    0H                                                               
         CLC   SVSTA,KEY+6                                                      
         BE    *+14                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVSTA,KEY+6                                                      
         MVI   MODE,PROCBUY                                                     
* NOTE - USED TO SET 'NETACT' HERE BUT CASES WHERE BUYLINE ACTIVITY             
* SPANS REQUEST PERIOD BUT NOT ACTUALLY BE ANY BUYS FOR THE PERIOD!             
* CODE ADDED AT PROC10 TO CHECK FOR THIS BEFORE SETTING NETACT                  
* *** HOWEVER *** THIS BROKE 'QBYID' REPORTS SO NOW SET NETACT HERE             
* FOR THESE REQUESTS - NOT GOT TIME TO FATHOM OUT WHY (19MAR02 - PWES)          
* E.G. OMDTOT REQ=RNOUNPSC  YPOLALL NNETN032           020318020324             
*        MVI   NETACT,1                                                         
         MVC   MYBUYKEY,KEY                                                     
*                                                                               
         CLI   QBYID,C'Y'                                                       
         BNE   PROC10                                                           
         MVI   NETACT,1            QBYID CAN'T RELY ON PROC10 SETTING!          
         BAS   RE,IDSCAN                                                        
         CLI   IDPASS,C'S'                                                      
         BNE   *+12                                                             
         BAS   RE,SAVID                                                         
         B     NEXTBUY                                                          
         CLI   IDPASS,C'B'                                                      
         BE    *+14                                                             
         CLC   CURRID,IDWORK                                                    
         BNE   NEXTBUY                                                          
         MVC   BUYID,CURRID                                                     
PROC10   EQU   *                                                                
*                                                                               
         GOTO1 GO                                                               
* BUYLINE ACTIVITY SPANS REQUEST PERIOD BUT MAY NOT ACTUALLY BE ANY             
* BUYS FOR THE ACTUAL REQUEST PERIOD - CHECK VALUES RETURNED                    
         L     R1,MEDBUFF                                                       
         LA    RF,MEDPERD-MEDBLOCK(R1)                                          
         L     RE,4(RF)                                                         
         USING MEDDATA,RE                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    *+8                                                              
         MVI   NETACT,1            BUY ACTIVITY - SET NETWORK ACTIVE            
         DROP  RE                                                               
*                                                                               
         CLI   QOPT1,C'0'                                                       
         BNL   NETX                DONE                                         
         OC    SVPKGEL,SVPKGEL                                                  
         BNZ   NEXTPKG                                                          
*                                                                               
NEXTBUY  DS    0H                                                               
         MVC   KEY(13),MYBUYKEY                                                 
         GOTO1 HIGH                RESET FOR SEQ READ                           
         B     NET2                                                             
*                                                                               
NETBX    CLI   IDPASS,C'S'                                                      
         BNE   NETBX2                                                           
         L     RF,IDCOUNT          (COUNT NOT INCLUDE TERMINATOR X'FF')         
         C     RF,=AL4(1)                                                       
         BNH   NETBX1                                                           
         GOTO1 XSORT,DMCB,BUYLIST,(RF),12,12,0     SORT THE ID LIST             
NETBX1   L     RF,BUYLIST                                                       
         ST    RF,IDPTR                                                         
         MVI   IDPASS,C'R'                                                      
         MVC   SVSTA,BSTA          ENSURE REREAD CORRECT STN 4 ALL NWK          
         B     NETBX3                                                           
*                                                                               
NETBX2   CLI   NETACT,1                                                         
         BNE   NETX                                                             
         MVI   MODE,STALAST                                                     
         MVC   MYSVKEY,KEY         KEY/KEYSAVE (& ADBUY) CORRUPTED IF           
         MVC   MYSVKSV,KEYSAVE     PRODUCT LEGEND PRINTED IN STALAST            
         GOTO1 GO                                                               
         MVC   KEY,MYSVKEY         RESTORE PRE STALAST KEY/KEYSAVE              
         MVC   KEYSAVE,MYSVKSV                                                  
         CLI   MODE,REREAD                                                      
         BE    NETA                                                             
         CLI   IDPASS,C'R'                                                      
         BNE   NETX                                                             
NETBX3   L     RF,IDPTR                                                         
         MVC   IDWORK(12),0(RF)                                                 
         LA    RF,12(RF)                                                        
         ST    RF,IDPTR                                                         
         CLI   IDWORK,X'FF'                                                     
         BNE   NETA                                                             
*                                                                               
NETX     CLI   IDPASS,C'R'                                                      
         BNE   NETX1                                                            
         L     RF,BUYLIST      RESTART ID LIST FOR NEXT STATION                 
         MVC   IDWORK(12),0(RF)                                                 
         LA    RF,12(RF)                                                        
         ST    RF,IDPTR                                                         
*        MVC   SVSTA,KEYSAVE+6                                                  
NETX1    CLI   NETACT,0                                                         
         BNE   NETX1A                                                           
* IF NET NOT ACTIVE WE WILL NOT HAVE DONE A STALAST SO WE MUST ISSUE            
* A DUMMY CBHLAST TO GET 02 TO RESET ITS FLAGS ETC AS IT WOULD HAVE             
* DONE ON A STALAST (IF DO STALAST ON NON ACTIVE NWK WE GET A PAGE              
* PRINTING FOR THE NWK WITH ZERO ROTATION SCHEDULE IN IT!)                      
         MVI   MODE,CBHLAST    GET 02 TO RESET STORAGE AS PER STALAST           
         GOTO1 GO                                                               
NETX1A   MVI   NETACT,0        DONE NETWORK SO RESET ACTIVITY                   
         OC    BMKT(5),BMKT                                                     
         BNZ   *+14            DONE FOR SINGLE NETWORK                          
         CLC   KEY(6),KEYSAVE  END OF A/M,CLI,PRO,MKT                           
         BE    NET4C           NO - CONTINUE WITH NEXT NWK/STN                  
         B     NXTEST                                                           
*                                                                               
IDSCAN   NTR1                                                                   
         L     R7,AREC                                                          
         LA    R7,24(R7)                                                        
         MVC   CURRID,=C'**UNKNOWN** '                                          
         CLI   QLANG,C'F'                                                       
         BNE   *+10                                                             
         MVC   CURRID,=C'**INCONNU** '                                          
IDSCAN1  CLI   0(R7),0                                                          
         BE    IDSCANX                                                          
         CLI   0(R7),X'70'                                                      
         BE    IDSCAN2                                                          
         ZIC   RE,1(R7)                                                         
         AR    R7,RE                                                            
         B     IDSCAN1                                                          
IDSCAN2  MVC   CURRID,3(R7)                                                     
IDSCANX  XIT1                                                                   
         SPACE 2                                                                
SAVID    NTR1                                                                   
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         LA    R2,BDELEM                                                        
         USING IDELEM,R2                                                        
SAVID2   CLI   0(R2),0                                                          
         BE    SAVID4                                                           
         CLI   0(R2),X'70'                                                      
         BE    SAVID6                                                           
         ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     SAVID2                                                           
SAVID4   LA    R2,IDWORK                                                        
         MVC   IDELEM+3(12),=C'**UNKNOWN** '                                    
         CLI   QLANG,C'F'                                                       
         BNE   *+10                                                             
         MVC   IDELEM+3(12),=C'**INCONNU** '                                    
SAVID6   MVC   IDWORK,IDELEM+3                                                  
         L     RF,BUYLIST                                                       
         ICM   RE,15,IDCOUNT                                                    
         BZ    SAVID10                                                          
*                                                                               
SAVID8   CLC   0(12,RF),IDWORK                                                  
         BE    SAVIDX                                                           
         LA    RF,12(RF)                                                        
         BCT   RE,SAVID8                                                        
*                                                                               
SAVID10  MVC   0(12,RF),IDWORK                                                  
         MVI   12(RF),X'FF'                                                     
         L     RE,IDCOUNT                                                       
         LAY   R1,BUYLISTL          BUY LIST TABLE LENGTH                       
         XR    R0,R0                CLEAR R0                                    
         D     R0,=F'12'            DIVIDE BY 12                                
         CR    RE,R1                ABOUT TO BLOW PAST THE BUFFER?              
         BL    *+6                  NO                                          
         DC    H'0'                 YES - DEATH                                 
         LA    RE,1(RE)                                                         
         ST    RE,IDCOUNT                                                       
SAVIDX   XIT1                                                                   
         DROP  R2,R6                                                            
*                                                                               
NETXX    B     EXIT                                                             
         EJECT                                                                  
*=================================================================              
* TEST IF LINE IS A PACKAGE MASTER OR SLAVE                                     
* IF MASTER, SAVE PACKAGE ELEMENT                                               
* IF SLAVE, TELL CALLER TO PROCESS LATER                                        
*=================================================================              
                                                                                
PKGTEST  DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         LA    R2,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
PKGT2    IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    4(RE)               TAKE RETURN 2                                
         CLI   QBYID,C'Y'          CANNOT HANDLE DIFF. ID'S                     
         BE    PKGT2               ON PKG MST/PKG SLAVE                         
         CLI   0(R2),5                                                          
         BNE   PKGT2                                                            
         TM    2(R2),X'01'         TEST MASTER OR SLAVE                         
         BZR   RE                  SLAVE - RETURN 1 - SKIP FOR NOW              
* LINE IS A MASTER                                                              
         MVC   SVPKGKEY,KEY        SAVE MASTER KEY                              
         LLC   RF,1(R2)                                                         
         BCTR  RF,0                SET FOR EX                                   
         EX    RF,*+8              SAVE                                         
         B     *+10                                                             
         MVC   SVPKGEL(0),0(R2)  *EXECUTED*                                     
         B     4(RE)               TAKE RETURN 2 - PKG DATA SAVED               
         EJECT                                                                  
         EJECT                                                                  
*=========================================================                      
* FETCH NEXT LINE IN PKG                                                        
*=========================================================                      
                                                                                
NEXTPKG  MVC   KEY,MYBUYKEY        MOVE LAST BUY KEY                            
         LA    R2,KEY+12           1-BYTE LINE NUM FOR POL BRND                 
         CLI   KEY+10,X'FF'        TEST FOR IT                                  
         BE    *+8                                                              
         LA    R2,KEY+11           LINE NUM FOR POL                             
*                                                                               
         LA    R5,SVPKGEL                                                       
         TM    2(R5),X'10'         TEST 2-BYTE LINE NUMS IN PKGEL               
         BO    NPKG10                                                           
*                                                                               
         LA    R5,3(R5)            POINT TO FIRST LINE NUMBER                   
         CLC   SVPKGKEY,KEY        TEST FIRST TIME (HAVE MASTER)                
         BE    NPKG8                                                            
*                                                                               
NPKG4    CLC   0(1,R2),0(R5)                                                    
         BE    NPKG6                                                            
         LA    R5,1(R5)                                                         
         CLI   0(R5),0                                                          
         BNE   NPKG4                                                            
         DC    H'0'                                                             
*                                                                               
NPKG6    LA    R5,1(R5)                                                         
*                                                                               
NPKG8    CLI   0(R5),0             TEST E-O-L                                   
         BE    NPKG22                                                           
*                                                                               
         MVC   0(1,R2),0(R5)       MOVE LINE TO KEY                             
         L     RE,ADBUY                                                         
         TM    15(RE),BUYRLN2      TEST 2-BYTE LINE IN BUYREC                   
         BZ    NPKG9                                                            
         MVI   KEY+11,0                                                         
         MVC   KEY+12(1),0(R5)                                                  
                                                                                
NPKG9    GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(12),KEYSAVE                                                  
         BE    NPKG20                                                           
         MVC   KEY(13),KEYSAVE     RESTORE LINE NOT FOUND AND TRY NEXT          
         B     NPKG6                                                            
                                                                                
* THIS CODE FOR 2-BYTE LINE NUMBERS *                                           
                                                                                
NPKG10   LA    R5,3(R5)            FIRST PACKAGE LINE NUMBER                    
         CLC   SVPKGKEY,KEY        TEST FIRST TIME (HAVE MASTER)                
         BE    NPKG16                                                           
*                                                                               
NPKG12   CLC   KEY+11(2),0(R5)                                                  
         BE    NPKG14                                                           
         LA    R5,2(R5)                                                         
         OC    0(2,R5),0(R5)                                                    
         BNE   NPKG12                                                           
         DC    H'0'                                                             
*                                                                               
NPKG14   LA    R5,2(R5)            NEXT LINE NUMBER                             
*                                                                               
NPKG16   OC    0(2,R5),0(R5)       TEST E-O-L                                   
         BZ    NPKG22                                                           
* SET FOR NEXT PKG LINE                                                         
         MVC   KEY+11(2),0(R5)                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    NPKG20                                                           
         MVC   KEY(13),KEYSAVE     RESTORE LINE NOT FOUND AND TRY NEXT          
         B     NPKG14                                                           
*                                                                               
NPKG20   GOTO1 GETBUY                                                           
         MVC   SVBUYKEY,KEY                                                     
         B     PROCESS                                                          
*                                                                               
* END OF PKG                                                                    
*                                                                               
NPKG22   MVC   KEY(13),SVPKGKEY    RESTORE MASTER LINE                          
         XC    SVPKGEL,SVPKGEL                                                  
         GOTO1 HIGH                                                             
         B     NET2                                                             
*                                                                               
SVPKGEL  DC    XL256'00'                                                        
         EJECT                                                                  
MYBUYKEY DS    CL13                                                             
MYSVKEY  DS    CL(L'KEY)                                                        
MYSVKSV  DS    CL(L'KEYSAVE)                                                    
NETACT   DS    C                                                                
IDWORK   DS    CL16                                                             
CURRID   DS    CL12                                                             
IDPTR    DS    F                                                                
IDCOUNT  DS    F                                                                
IDPASS   DC    C'R'                S=SAVE, R=REPORT, B=???                      
         LTORG                                                                  
         PRINT OFF                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
         EJECT                                                                  
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054SPREPRN03 02/25/15'                                      
         END                                                                    
