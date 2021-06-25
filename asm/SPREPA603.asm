*          DATA SET SPREPA603  AT LEVEL 062 AS OF 05/10/04                      
*PHASE SPA603A,*                                                                
*INCLUDE BINSRCH2                                                               
         TITLE 'SPA603 - SPOT STATION SUMMARY - SUB-CONTROLLER'                 
SPA603   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPA603,RC,RR=R5                                                
         L     RA,0(R1)                                                         
         LA    R8,2048(RA)                                                      
         LA    R8,2048(R8)                                                      
         USING SPWORKD,RA,R8                                                    
         ST    R5,RELO                                                          
*                                                                               
STAGRP   DS    0H                                                               
         CLI   QRERATE,C'Y'        FILTER ON STATION GROUP                      
         BNE   STAGRPX                                                          
*                                                                               
         LA    R0,STATBMAX                                                      
         GOTO1 ,BINPARMS,,ASTATAB,0,L'STATAB,(0,3),(R0)                         
*                                                                               
         LA    R6,KEY                                                           
         USING GRPRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   GRPKTYP,GRPKTYPQ                                                 
         MVI   GRPKSTYP,GRPKSTYQ                                                
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,QBOOK1                                                    
         MVC   WORK(4),QBOOK1+1                                                 
         OC    WORK(4),=C'0000'                                                 
         PACK  WORK+5(3),WORK(5)                                                
         MVC   GRPKCODE,WORK+5                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(GRPKMSQL),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         USING GRPRECD,R6                                                       
         LA    R6,GRPEL                                                         
         DROP  R6                                                               
         MVI   ELCDLO,GRPVALCQ     VALUE ELEMENT                                
         MVI   ELCDHI,GRPVALCQ                                                  
         USING GRPVALD,R6                                                       
*                                                                               
STAGRP10 DS    0H                  PUT STATIONS IN STATN GRP IN TABLE           
         BAS   RE,NEXTEL                                                        
         BNE   STAGRPX                                                          
         GOTO1 MSPACK,DMCB,=C'0000',GRPVALUE,WORK                               
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(1,WORK+2)                                      
         OC    BINPARMS,BINPARMS   TABLE FULL?                                  
         BNZ   STAGRP10                                                         
         DC    H'0'                                                             
*                                                                               
STAGRPX  DS    0H                                                               
*                                                                               
         XC    QBMKT(5),QBMKT                                                   
         EJECT                                                                  
* CLEAR MGRTAB *                                                                
         SPACE 1                                                                
FC2      LA    R0,20000/250                                                     
         L     R1,AMGRTAB          GET MGRTAB ADDRESS                           
         A     R1,RELO                                                          
         ST    R1,SVMKTADR                                                      
*                                                                               
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         CLI   QMGR,C' '           TEST MKTGRP REQUEST                          
         BE    FC8                                                              
         SPACE 1                                                                
* BUILD MKTGRP ASSGN TABLE *                                                    
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+8(1),QMGR                                                    
*                                                                               
         CLC   =C'ALL',QCLT                                                     
         BE    FC3                                                              
         CLI   QMGR,C'F'                                                        
         BH    FC3                                                              
         SPACE 1                                                                
* FOR ONE CLIENT REQ, ALLOW CLT SPECIFIC MKTGRP *                               
         SPACE 1                                                                
         GOTO1 CLPACK,DMCB,QCLT,KEY+3                                           
*                                                                               
FC3      GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    FC6                                                              
         MVC   P(45),=C'** ERROR **NO MARKETS ASSIGNED TO THIS MKTGRP'          
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
FC4      GOTO1 SEQ                                                              
*                                                                               
FC6      CLC   KEY(9),KEYSAVE                                                   
         BNE   FC8                                                              
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,KEY+11         GET MKT NUM                                  
         AR    RE,RE               X 2                                          
         A     RE,SVMKTADR         + TABLE START                                
         MVC   0(2,RE),KEY+9       MOVE MKTGRP NUMBER                           
         B     FC4                                                              
         EJECT                                                                  
FC8      CLC   =C'ALL',QMKT                                                     
         BE    FC10                                                             
         PACK  DUB,QMKT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,QBMKT                                                       
*                                                                               
FC10     CLC   =C'ALL',QSTA                                                     
         BE    FC18                                                             
         CLI   QSTA,C'0'           TEST ALPHA STA                               
         BNL   FC12                HIGH IS MKTGRP                               
         GOTO1 MSPACK,DMCB,QMKT,QSTA,QBMKTSTA                                   
         B     FC18                                                             
         SPACE 1                                                                
* WORK OUT NUMBER OF DIGITS FOR MKTGRP FILTERING *                              
         SPACE 1                                                                
FC12     LA    R1,QSTA+3           POINT TO LAST CHAR                           
         LA    R0,3                PRESET TO MAX-1                              
*                                                                               
FC14     CLI   0(R1),C'*'                                                       
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   FC16                                                             
         BCTR  R1,0                                                             
         BCT   R0,FC14                                                          
*                                                                               
FC16     STH   R0,QMGRLEN                                                       
*                                                                               
FC18     CLI   QREPTYPE,C'S'       TEST SPECIAL REP REQUEST                     
         BNE   FC20                                                             
         CLC   QREP,=C'000'        TEST SINGLE REP REQUEST                      
         BH    FC100               YES - GO READ REP POINTERS                   
         EJECT                                                                  
FC20     DS    0H                                                               
         GOTO1 FCNXTCLT                                                         
         BNE   EXIT                                                             
*                                                                               
         L     R0,AMGRTAB          SET ADDRESS CLEARED BY NXTCLT                
         A     R0,RELO                                                          
         ST    R0,SVMKTADR         PASS IN UNUSED ADCON                         
*                                                                               
         MVI   MODE,CLTFRST                                                     
         GOTO1 GO                                                               
         CLI   MODE,CLTLAST        TEST CLIENT REJECTED BY 02                   
         BE    FC20                YES - GET NEXT                               
         SPACE 1                                                                
* READ FOR BUY RECORDS *                                                        
         SPACE 1                                                                
         L     R6,ADCLT                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(3),1(R6)                                                     
         CLI   QMED,C'C'           TEST CANAD COMBINED                          
         BNE   *+8                                                              
         MVI   KEY+3,X'FF'         YES - PROCESS POL ONLY                       
*                                                                               
FC22     GOTO1 HIGH                                                             
*                                                                               
FC24     CLC   KEY(3),KEYSAVE      SAME A-M/C                                   
         BE    FC26                                                             
FC24X    MVI   MODE,CLTLAST                                                     
         GOTO1 GO                                                               
         B     FC20                                                             
*                                                                               
FC26     OC    QBMKT,QBMKT         TEST ALL MARKETS                             
         BZ    FC30                YES                                          
         CLC   KEY+4(2),QBMKT      TEST RIGHT MARKET                            
         BE    FC30                YES                                          
         BH    FC28                                                             
         MVC   KEY+4(5),QBMKTSTA   READ FOR MARKET/STATION                      
         XC    KEY+9(4),KEY+9                                                   
         B     FC22                                                             
         SPACE 1                                                                
* READ FOR NEXT PRODUCT *                                                       
         SPACE 1                                                                
FC28     CLI   KEY+3,X'FF'         TEST HIGH PRD CODE                           
         BE    FC24X               YES - CLTLAST                                
         IC    RE,KEY+3                                                         
         LA    RE,1(RE)                                                         
         STC   RE,KEY+3                                                         
         MVC   KEY+4(5),QBMKTSTA                                                
         XC    KEY+9(4),KEY+9                                                   
         B     FC22                                                             
*                                                                               
FC30     CLI   QRERATE,C'Y'        FILTER ON STATION GROUP                      
         BE    FC35                                                             
         CLC   =C'ALL',QSTA        TEST ALL STATIONS                            
         BE    FC50                YES                                          
         CLI   QSTA,C'0'           TEST NUMERIC STA (MKTGRP)                    
         BL    FC40                NO                                           
         EJECT                                                                  
* FILTER ON MKTGRP *                                                            
         SPACE 1                                                                
         SR    RE,RE                                                            
         ICM   RE,3,KEY+4          GET MKT NUM                                  
         AR    RE,RE               X 2                                          
         A     RE,SVMKTADR         + TABLE START                                
         UNPK  DUB,0(3,RE)         UNPACK 1 EXTRA BYTE                          
         LH    RE,QMGRLEN                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   QSTA(0),DUB+3    *EXECUTED*                                      
         BE    FC50                                                             
*                                                                               
         MVC   KEY+6(7),XFF        READ FOR NEXT MARKET                         
         B     FC22                                                             
*                                                                               
FC35     GOTO1 BINSRCH,BINPARMS,(0,KEY+6)  CHECK TABLE FOR STATION              
         CLI   BINPARMS,X'01'      RECORD NOT FOUND?                            
         BE    FC61                FORCE NEXT STATION                           
         B     FC50                                                             
*                                                                               
FC40     CLC   KEY+6(3),QBSTA      TEST RIGHT STATION                           
         BE    FC50                YES                                          
         BH    FC28                IF HIGH, READ FOR NEXT PRD                   
         MVC   KEY+6(3),QBSTA      FORCE NEXT STA                               
         XC    KEY+9(4),KEY+9      ELSE READ FOR STATION                        
         B     FC22                                                             
         EJECT                                                                  
FC50     OC    KEY+4(2),KEY+4      TEST MARKET 0 (CANAD NTWK)                   
         BNZ   FC51                                                             
         MVC   KEY+6(7),XFF        FORCE NEXT MKT                               
         B     FC22                                                             
*                                                                               
FC51     CLI   COUNTRY,C'C'        TEST CANADA                                  
         BE    FC53                YES - NO CABLE TESTS                         
         CLI   QSTA+4,C'/'         TEST CABLE ONLY REQUEST                      
         BNE   FC52                NO                                           
         CLI   KEY+6,X'E8'         TEST THIS IS A CABLE STATION                 
         BNL   FC53                YUP                                          
FC51X    MVC   KEY+9(4),XFF        FORCE NEXT STATION                           
         B     FC22                                                             
*                                                                               
FC52     CLI   QSTA+4,C'-'         TEST EXCLUDE CABLE                           
         BNE   FC53                                                             
         CLI   KEY+6,X'E8'         TEST THIS IS A CABLE STATION                 
         BNL   FC51X               YUP - SKIP                                   
*                                                                               
FC53     CLI   KEY+3,X'FF'         TEST POL                                     
         BNE   FC54                NO                                           
         TM    KEY+10,X'80'        TEST SPILL POINTER                           
         BZ    FC60                NO - PROCESS                                 
         MVC   KEY+10(3),XFF       FORCE NEXT EST                               
         B     FC22                                                             
*                                                                               
FC54     CLI   KEY+10,0            TEST REG OR ACTIVE P/B                       
         BE    FC60                YES - PROCESS                                
         MVC   KEY+10(3),XFF       ELSE FORCE NEXT EST                          
         B     FC22                                                             
*                                                                               
FC60     CLC   QREP,=C'000'        TEST PAYING REP                              
         BL    FC62                NO                                           
         SPACE 1                                                                
* FILTER ON PAYING/TRAFFIC REP *                                                
         SPACE 1                                                                
         BAS   RE,GETSTA                                                        
*                                                                               
         L     R6,ADSTAT                                                        
         USING STARECD,R6                                                       
         LA    R1,SCONREP                                                       
         CLI   QREPTYPE,C'T'       TEST T/S REP                                 
         BE    *+8                                                              
         LA    R1,SPAYREP          NO - USE PAYING REP                          
         CLC   0(3,R1),QREP                                                     
         BE    FC70                                                             
FC61     MVC   KEY+9(4),XFF        FORCE NEXT STATION                           
         B     FC22                                                             
         DROP  R6                                                               
*                                                                               
FC62     CLI   QAFFIL,C' '         TEST AFFILIATE FILTER                        
         BE    FC70                NO                                           
         CLI   QAFFIL,C'*'         TEST CANAD ALL AFFIL REQ                     
         BE    FC70                YES                                          
         EJECT                                                                  
* FILTER ON AFFILIATE *                                                         
         SPACE 1                                                                
         BAS   RE,GETSTA                                                        
*                                                                               
         L     R6,ADSTAT                                                        
         USING STARECD,R6                                                       
*                                                                               
         L     RF,ADAGY                                                         
         LA    RF,AGYPROF-AGYHDR(RF)                                            
         CLI   7(RF),C'C'          TEST CANADIAN                                
         BE    FC64                                                             
*                                                                               
         CLC   QAFFIL,SNETWRK                                                   
         BE    FC70                                                             
         B     FC64X                                                            
         SPACE 1                                                                
FC64     CLC   QAFFIL,SSIZE        CANADA HAS AFF IN STA SIZE                   
         BE    FC70                                                             
FC64X    MVC   KEY+9(4),XFF        FORCE NEXT STATION                           
         B     FC22                                                             
         DROP  R6                                                               
         SPACE 1                                                                
FC70     MVI   MODE,STAFRST                                                     
         GOTO1 GO                                                               
*                                                                               
FC72     MVI   MODE,PROCBUY                                                     
         GOTO1 GO                                                               
*                                                                               
FC74     GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(9),KEYSAVE      A-M/C/P/MKT/STA                              
         BNE   FC76                                                             
*                                                                               
         CLI   KEY+3,X'FF'         TEST POL                                     
         BNE   FC75                NO                                           
         TM    KEY+10,X'80'        TEST SPILL POINTER                           
         BZ    FC72                NO - PROCESS                                 
         B     FC74                YES - SKIP                                   
*                                                                               
FC75     CLI   KEY+10,0            ELSE TEST REG OR ACTIVE P/B                  
         BE    FC72                                                             
         B     FC74                                                             
*                                                                               
FC76     MVI   MODE,STALAST                                                     
         GOTO1 GO                                                               
         B     FC24                                                             
         EJECT                                                                  
* READ STATION MASTER RECORD *                                                  
         SPACE 1                                                                
GETSTA   NTR1                                                                   
         MVC   WORK(20),KEY        SAVE KEY IN WORK                             
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         GOTO1 MSUNPK,DMCB,WORK+4,DUB,KEY+2                                     
         CLI   KEY+6,C' '                                                       
         BNE   *+8                                                              
         MVI   KEY+6,C'T'                                                       
         MVC   KEY+7(2),QAGY                                                    
         GOTO1 CLUNPK,DMCB,WORK+1,KEY+9                                         
         GOTO1 READSTA                                                          
*                                                                               
         MVC   KEY(20),WORK        RESTORE KEY                                  
         MVC   KEYSAVE,KEY          AND KEYSAVE                                 
         B     EXIT                                                             
         EJECT                                                                  
* SPECIAL REP POINTER PROCESSING - FOR SINGLE REP REQUESTS *'                   
         SPACE 1                                                                
FC100    DS    0H                                                               
         GOTO1 FCNXTCLT            GO HERE ONCE FOR MKTGRP INTLZTN              
         MVI   MODE,CLTFRST                                                     
         GOTO1 GO                  GIVE SONIA A CHANCE                          
*                                                                               
         L     R0,AMGRTAB          SET ADDRESS CLEARED BY NXTCLT                
         A     R0,RELO                                                          
         ST    R0,SVMKTADR         PASS IN UNUSED ADCON                         
*                                                                               
         CLC   =C'ALL',QCLT                                                     
         BNE   *+10                                                             
         XC    BCLT,BCLT                                                        
*                                                                               
         CLI   QCLT,C'$'           NO SUPPORT FOR OFFICE LISTS                  
         BNE   *+10                                                             
         XC    BCLT,BCLT           SO JUST DO ALL  ANYWAY                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,3                                                            
         MVC   KEY+1(1),BAGYMD                                                  
         NI    KEY+1,X'F0'                                                      
         PACK  DUB,QREP                                                         
         CVB   R0,DUB                                                           
         STH   R0,DUB                                                           
         OC    KEY+1(2),DUB                                                     
*                                                                               
FC102    GOTO1 HIGH                                                             
*                                                                               
FC104    CLC   KEY(3),KEYSAVE      SAME TYPE/REP                                
         BNE   EXIT                                                             
*                                                                               
FC106    OC    BCLT,BCLT           TEST ALL CLIENTS                             
         BZ    FC110               YES - SO GO CHECK MEDIA                      
         CLC   KEY+8(2),BCLT                                                    
         BE    FC110                                                            
         SPACE 1                                                                
* FORCE NEW CLIENT CODE *                                                       
         SPACE 1                                                                
         ICM   RE,3,KEY+8                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,KEY+8                                                       
         XC    KEY+10(3),KEY+10                                                 
         B     FC102                                                            
         EJECT                                                                  
* CHECK MEDIA CODE *                                                            
         SPACE 1                                                                
FC110    OC    QBMKT,QBMKT         TEST ALL MARKETS                             
         BZ    FC111               YES                                          
         CLC   QBMKT,KEY+3         ELSE MATCH MARKET                            
         BNE   FC120                                                            
*                                                                               
         OC    QBSTA,QBSTA         TEST ALL STATIONS                            
         BZ    FC111                                                            
         CLC   QBSTA,KEY+5         ELSE MATCH STATION                           
         BNE   FC120                                                            
*                                                                               
FC111    GOTO1 MSUNPK,DMCB,KEY+3,WORK+10,WORK                                   
         CLI   QMED,C'T'                                                        
         BNE   FC112                                                            
         CLI   WORK+4,C' '                                                      
         BE    FC116                                                            
         B     FC120                                                            
FC112    CLI   QMED,C'R'                                                        
         BNE   FC114                                                            
         CLI   WORK+4,C'A'                                                      
         BE    FC116                                                            
         CLI   WORK+4,C'F'                                                      
         BE    FC116                                                            
         B     FC120                                                            
FC114    CLC   QMED,WORK+4                                                      
         BNE   FC120                                                            
*                                                                               
FC116    MVC   KEYSAVE,KEY         SAVE FIRST KEY FOR STATION                   
         MVI   MODE,STAFRST                                                     
         GOTO1 GO                                                               
*                                                                               
FC118    MVI   MODE,PROCBUY                                                     
         GOTO1 GO                                                               
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(10),KEYSAVE     TYP/REP/MKT/STA/CLT                          
         BE    FC118                                                            
*                                                                               
         MVI   MODE,STALAST                                                     
         GOTO1 GO                                                               
         B     FC104                                                            
         SPACE 1                                                                
* FORCE NEXT STATION *                                                          
         SPACE 1                                                                
FC120    MVC   KEY+8(5),XFF                                                     
         B     FC102                                                            
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
GETEL    CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   0(1,R6),ELCDLO                                                   
         BL    NEXTEL                                                           
         CLC   0(1,R6),ELCDHI                                                   
         BH    NEXTEL                                                           
         CR    RE,RE                                                            
         BR    RE                  EXIT WITH CC EQUAL                           
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  EXIT WITH CC NOT EQUAL                       
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
RELO     DS    A                                                                
AMGRTAB  DC    A(MGRTAB)           RELOCATED AND SAVED IN SVMKTADR              
ASTATAB  DC    A(STATAB)                                                        
BINPARMS DS    6F                  BINSRCH PARAMETERS                           
QMGRLEN  DS    H                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
QBMKTSTA DS    0CL5                                                             
QBMKT    DS    XL2                                                              
QBSTA    DS    XL3                                                              
XFF      DC    16X'FF'                                                          
         LTORG                                                                  
MGRTAB   DS    2500D                                                            
STATAB   DS    2000XL3                                                          
STATBMAX EQU   (*-STATAB)/3        MAX ENTRIES                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENGRP                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062SPREPA603 05/10/04'                                      
         END                                                                    
