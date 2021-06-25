*          DATA SET SPREPLB02  AT LEVEL 040 AS OF 05/01/02                      
*PHASE SPLB02A                                                                  
         TITLE 'SPLB02 - STATION FILE LABELS'                                   
*INCLUDE SQUASHER                                                               
*INCLUDE LABELS3                                                                
SPLB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPLB02,RR=R8                                                   
         L     RC,0(R1)                                                         
         ST    R8,RELO                                                          
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         L     R1,=V(LABELS3)                                                   
         A     R1,RELO                                                          
         ST    R1,LABELS3                                                       
         MVC   LINE1,SPACES                                                     
         MVC   LINE2(156),LINE1                                                 
         SPACE 2                                                                
         L     R3,=V(SQUASHER)                                                  
         A     R3,RELO                                                          
         ST    R3,SQUASHER                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   CHKREQ                                                           
         MVI   RCREQREP,C'N'       RUNFRST                                      
         MVI   BYCLT,C'N'                                                       
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
CHKREQ   CLI   MODE,CLTFRST        CLIENT FIRST                                 
         BNE   LB2                                                              
         CLI   BYCLT,C'Y'                                                       
         BE    LB4                                                              
         B     XIT                                                              
*                                                                               
LB2      CLI   MODE,REQFRST        REQUEST FIRST                                
         BNE   CKRUN                                                            
         CLI   QMGR,C' '                                                        
         BNH   LB4                                                              
         CLC   QCLT,SPACES                                                      
         BNH   LB3                                                              
         CLC   QCLT,=C'ALL'                                                     
         BE    LB3                                                              
         MVI   BYCLT,C'Y'                                                       
         B     XIT                                                              
*                                                                               
LB3      CLI   QMGR,C'A'           TEST MARKET GROUP A-F                        
         BL    LB4                                                              
         CLI   QMGR,C'F'                                                        
         BH    LB4                                                              
         DC    H'0'                YES-BLOW IF NOT CLIENT SPECIFIC              
*                                                                               
LB4      MVI   RCREQSUM,C'N'                                                    
         L     R6,ADSTATAD                                                      
         USING ADDRRECD,R6                                                      
         BAS   RE,PROCOPT                                                       
         CLI   QOPT3,C'0'                                                       
         BNL   *+8                                                              
         MVI   QOPT3,C'3'                                                       
         PACK  DOUBLE,QOPT3                                                     
         CVB   R4,DOUBLE                                                        
         ZAP   DOUBLE,NOMBRE                                                    
         CVB   R3,DOUBLE                                                        
         L     R5,PRINT                                                         
         GOTO1 LABELS3,DMCB,=C'$OPEN',(R5),(R3),(R4)                            
*                                                                               
         MVI   MGRPSW,C'N'                                                      
         CLI   QMGR,C' '           TEST MARKET GROUP REQUEST                    
         BNH   LB10                                                             
         CLI   QMED,C' '                                                        
         BNH   LB10                                                             
         MVI   MGRPSW,C'Y'                                                      
         BAS   RE,MGROUP           YES                                          
         LH    R8,NSTAS            R8=N'STATIONS TO PROCESS                     
         LTR   R8,R8                                                            
         BZ    ENDREQ                                                           
         L     R3,=A(STALST)       R3=A(STATION LIST)                           
         MVC   QSTA,0(R3)          FILTER ON FIRST STAION                       
*                                                                               
LB10     XC    KEY,KEY                                                          
         MVI   KEY,C'A'                                                         
         GOTO1 HIGHSTAD                                                         
CHKATTN  CLI   QUESTOR,C'$'        IS REQ NAME AN 'ATTN' LINE VALUE             
         BNE   NEXTAD2             NO                                           
         MVC   LINE5(5),=C'ATTN-'  YES                                          
         MVC   LINE5+6(11),QUESTOR+1                                            
*                                                                               
NEXTAD   DC    0H'0'                                                            
         GOTO1 SEQSTAD                                                          
*                                                                               
NEXTAD2  CLI   0(R6),C'A'                                                       
         BE    NEXTAD4                                                          
         B     ENDREQ                                                           
*                                                                               
         SPACE 3                                                                
         EJECT                                                                  
NEXTAD4  CLC   ADDKAGY,QAGY                                                     
         BNE   NEXTAD                                                           
         CLI   QMED,C' '           ALL MEDIAS                                   
         BE    *+14                OR ONE                                       
         CLC   QMED,ADDKMED                                                     
         BNE   NEXTAD                                                           
*                                  TAKE THE RECORD FOR AGENCY                   
         MVC   LINE1(156),SPACES    CLEAR ALL BUT LINE 5.                       
         CLC   QSTA(3),=C'ALL'     ALL STATIONS                                 
         BE    TAKE1                                                            
*                                                                               
LB12     CLC   QSTA,ADDKCALL       NO, ONLY ONE                                 
         BE    TAKE1                                                            
         BH    NEXTAD                                                           
         CLI   MGRPSW,C'Y'         QSTA LOWER THAN CURRENT STATION              
         BNE   NEXTAD              TEST MARKET GROUPS                           
         LA    R3,5(R3)            YES-NEXT STATION                             
         BCT   R8,*+8                                                           
         B     ENDREQ                                                           
         MVC   QSTA,0(R3)                                                       
         B     LB12                                                             
*                                                                               
TAKE1    TM    ACNTL,X'80'         DELETED                                      
         BO    LB20                YES                                          
         MVC   LINE1(20),ANAME         NO - NAME                                
         MVC   LINE2(24),A1LINE        ST. ADDR                                 
         MVC   LINE3(24),A2LINE    CITY                                         
         MVC   LINE3+25(3),A3LINE  STATE CODE                                   
         MVC   LINE4,SPACES                                                     
         MVC   LINE4(10),ABIGZIP                                                
         ZAP   DOUBLE,NOMBRE                                                    
         CVB   R7,DOUBLE                                                        
LAGIN    BAS   RE,LABELS                                                        
*                                                                               
LB20     CLI   MGRPSW,C'Y'         TEST MARKET GROUPS                           
         BNE   NEXTAD                                                           
         LA    R3,5(R3)            YES-NEXT STATION                             
         MVC   QSTA,0(R3)                                                       
         BCT   R8,NEXTAD                                                        
         B     ENDREQ                                                           
         EJECT                                                                  
PROCOPT  NTR1                      HOW MANY OF EACH                             
         MVC   WORKA,=X'F0F0'                                                   
         MVZ   WORKA,QOPT1         QOPT1+2 CONTAIN VALUE                        
         CLC   WORKA,=X'F0F0'                                                   
         BE    OKOPT                                                            
         ZAP   NOMBRE,=P'1'        DEFAULTS TO 1 PER.                           
         B     XIT                                                              
OKOPT    PACK  NOMBRE,QOPT1(2)                                                  
         CP    NOMBRE,=P'0'        ZERO OR LOWER NOTHING PRINTS                 
         BH    XIT                                                              
         DC    H'0'                BOMB ALSO.                                   
         EJECT                                                                  
* ROUTINE TO DEAL WITH MARKET GROUP REQUEST                                     
*                                                                               
MGROUP   NTR1                                                                   
         CLI   QSTA,C'0'           TEST MARKET GROUP FILTEREING                 
         BL    MGRP2                                                            
         LA    R1,QSTA+3           YES-WORK OUT NO OF DIGITS                    
         LA    R0,3                                                             
*                                                                               
MGRP1    CLI   0(R1),C'*'                                                       
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,MGRP1                                                         
         STC   R0,QMGRLEN                                                       
*                                                                               
MGRP2    LA    RE,MKTTAB           CLEAR MARKET TABLE                           
         LH    RF,=H'10000'                                                     
         XCEF                                                                   
         XC    KEY,KEY             READ MARKET GROUP PASSIVES                   
         LA    R2,KEY                                                           
         USING MKGKEY,R2                                                        
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD,BAGYMD                                                  
         CLI   BYCLT,C'Y'                                                       
         BNE   *+10                                                             
         MVC   MKGPCLT,BCLT                                                     
         MVC   MKGPMID,QMGR                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(MKGPMGRP-MKGKEY),KEYSAVE                                     
         BE    MGRP4                                                            
         CLI   QMGR,C'F'                                                        
         BH    *+6                                                              
         DC    H'0'                A-F REQUIRE CLIENT                           
         CLI   BYCLT,C'Y'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
         XC    MKGPCLT,MKGPCLT     REMOVE CLIENT                                
         GOTO1 HIGH                                                             
         CLC   KEY(MKGPMGRP-MKGKEY),KEYSAVE                                     
         BE    MGRP4                                                            
         DC    H'0'                NO MARKETS ASSIGNED                          
*                                                                               
MGRP3    GOTO1 SEQ                                                              
*                                                                               
MGRP4    CLC   KEY(MKGPMGRP-MKGKEY),KEYSAVE                                     
         BNE   MGRP6                                                            
         CLI   QSTA,C'0'           TEST MGRP FILTERING                          
         BL    MGRP5               NO                                           
         UNPK  DUB,MKGPMGRP(3)                                                  
         ZIC   RE,QMGRLEN                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   QSTA(0),DUB+3       TEST IN REQUESTED MKTGRP(S)                  
         BE    MGRP5               YES                                          
         MVC   MKGPMKT,=X'FFFF'    NO-SKIP TO NEXT MKTGRP                       
         GOTO1 HIGH                                                             
         B     MGRP4                                                            
*                                                                               
MGRP5    SR    RE,RE                                                            
         ICM   RE,3,MKGPMKT        GET MKT NUM                                  
         LA    RE,MKTTAB(RE)                                                    
         MVI   0(RE),C'Y'          INDICATE MARKET'S VALID                      
         B     MGRP3                                                            
         DROP  R2                                                               
*                                                                               
MGRP6    XC    KEY,KEY             READ THE STATION RECORDS                     
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         LA    R0,MAXSTAS                                                       
         L     R2,ADSTAT                                                        
         USING STARECD,R2                                                       
         L     R3,=A(STALST)                                                    
         LR    RE,R3               CLEAR THE STATION LIST                       
         LA    RF,5                                                             
         MH    RF,=Y(MAXSTAS)                                                   
         XCEF                                                                   
         GOTO1 HIGHSTA                                                          
         B     MGRP8                                                            
*                                                                               
MGRP7    GOTO1 SEQSTA                                                           
*                                                                               
MGRP8    TM    DMCB+8,X'80'        TEST EOF                                     
         BO    MGRP10                                                           
         CLI   0(R2),C'S'                                                       
         BNE   MGRP10                                                           
         CLC   STAKAGY,QAGY        TEST CORRECT AGENCY                          
         BNE   MGRP7                                                            
         PACK  DUB,SMKT            GET THIS STATION'S MARKET                    
         CVB   RE,DUB                                                           
         LA    RE,MKTTAB(RE)                                                    
         CLI   0(RE),C'Y'          TEST THIS MARKET'S WANTED                    
         BNE   MGRP7               NO                                           
         MVC   0(5,R3),STAKCALL    YES-ADD TO STATION LIST                      
         LA    R3,5(R3)                                                         
         BCT   R0,MGRP7                                                         
         DC    H'0'                                                             
*                                                                               
MGRP10   SH    R0,=Y(MAXSTAS)      SAVE N'STATIONS IN LIST                      
         LPR   R0,R0                                                            
         STH   R0,NSTAS                                                         
*                                                                               
MGRPX    B     XIT                                                              
         EJECT                                                                  
ENDREQ   GOTO1 AENDREQ                                                          
         SPACE 3                                                                
XIT      XIT1                      XIT FROM EVERYWHERE                          
         SPACE 3                                                                
CKRUN    CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         GOTO1 LABELS3,DMCB,=C'$CLOSE'                                          
         B     XIT                                                              
         SPACE 3                                                                
LABELS   NTR1                      PRINT ROUTINE.                               
         GOTO1 SQUASHER,DMCB,LINE1,20   DEGAS NAME                              
         GOTO1 SQUASHER,DMCB,LINE2,24   DEGAS STREET ADDR                       
         GOTO1 SQUASHER,DMCB,LINE3,36  DEGAS CITY/ST/ZIP                        
         GOTO1 LABELS3,DMCB,LINE1                                               
         B     XIT                                                              
         EJECT                                                                  
LINE1    DS    CL39           NAME LINE                                         
LINE2    DS    CL39           STREET                                            
LINE3    DS    CL39           CITY/STATE/(ZIP FOR US)                           
LINE4    DS    CL39           ZIP FOR CANADA                                    
LINE5    DS    CL39           ATTN LINE                                         
         SPACE 2                                                                
LABELS3  DS    F                                                                
PCTL     DC    CL4'BC01'           PRINT CTL FOR 'PRINT' MODULE                 
FIRST    DC    C'F'                                                             
ACROSS   DC    PL1'0'                                                           
WORKA    DS    CL2                 FOR NUMERIC EDITING.                         
NOMBRE   DS    PL2                 FOR NO. OF LABELS PER STATION                
ENDX     DC    C' '                                                             
TESTNO   DC    PL2'0'                                                           
TESTCO   DC    PL2'0'                                                           
CTLN     DC    PL1'3'                                                           
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
NSTAS    DS    H                                                                
MGRPSW   DS    C                                                                
QMGRLEN  DS    X                                                                
BYCLT    DS    C                                                                
*                                                                               
         DS    0D                                                               
         DC    C'*MKTTAB*'                                                      
MKTTAB   DS    10000X                                                           
*                                                                               
         DC    C'*STALST*'                                                      
STALST   DS    (MAXSTAS)CL5                                                     
MAXSTAS  EQU   3000                                                             
         EJECT                                                                  
ADDRRECD DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
 END                                                                            
