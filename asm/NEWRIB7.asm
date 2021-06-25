*          DATA SET NEWRIB7    AT LEVEL 049 AS OF 03/14/18                      
*PHASE T320B7A,+0                                                               
         TITLE 'T320B7 - GOAL PERFORMANCE - PRINT MODULE'                       
T320B7   CSECT                                                                  
         SPACE 3                                                                
*                                                                               
*        THIS IS BASED ON P3 REPORT AND USES THE P3 SCREEN                      
*        THIS IS BASED ON P3 REPORT AND USES THE P3 SCREEN                      
*                                                                               
*              ORGANIZATION OF WORKING STORAGE                                  
*                                                                               
*              ANETWS1 IS A(STORAGE USED BY THIS PROGRAM)                       
*                                                                               
*              STORAGE IS ARRANGED AS FOLLOWS:                                  
*                                                                               
*              NET DEMO BLOCK      516 BYTES                                    
*              DBLOCK              256 BYTES                                    
*              NETGOAL BLOCK       100 BYTES                                    
*              GENERAL STORAGE                                                  
*              PRODUCT LIST       3200 BYTES                                    
*                                                                               
*              REGISTER USAGE                                                   
*                                                                               
*              COMMON DSECTS       RC,R9                                        
*              TWA                 RA                                           
*              SPOOL AREAS         R8                                           
*              STORAGE ABOVE       R7                                           
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**NGPR**                                                       
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING T320B7+4096,R6                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING NDDEMBLK,R7                                                      
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,HEDSPECS                                                      
         ST    R2,SPECS                                                         
*                                                                               
         L     RF,=A(CLTRECSV)     MOVE CLTREC FROM ANETWS3                     
         ST    RF,NBACLI                                                        
         L     RE,ANETWS3          TO PROGRMA STORAGE AREA                      
         MOVE  ((RF),1500),(RE)                                                 
         L     RE,ANETWS3          AND CLEAR AREA                               
         LA    RF,1500                                                          
         XCEF                                                                   
*                                                                               
         L     RE,=A(PRDTARG)                                                   
         ST    RE,APRDTRG                                                       
         LA    RF,1020                                                          
         XCEF                                                                   
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000AE0'    A(DEMOCON)                          
         MVC   DEMOCON,DMCB                                                     
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         ST    R1,RELO                                                          
         L     R1,=A(BUFFALOC)                                                  
         A     R1,RELO                                                          
         ST    R1,ABUFFC                                                        
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFFC                                      
         SPACE 1                                                                
         EJECT                                                                  
*              CONTROL OF REPORT - GOALS AND UNITS                              
         SPACE 3                                                                
GETS     XC    PERTYPE,PERTYPE                                                  
         MVI   PERTYPE,C'W'        USE WEEKS                                    
         MVI   PERTYPE+1,1         USE MONTHS IF TOO MANY WEEKS                 
         LA    R4,MAXWKS                                                        
         ST    R4,NUMWKS           MAX NUMBER OF WEEKS                          
         SPACE 1                                                                
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK    PROCESS DATES                           
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT                                                  
         BE    GETLIST                                                          
         B     PROCDAT                                                          
         SPACE 1                                                                
GETLIST  NETGO NVWKLST,DMCB,NUMWKS,WKLIST,PERTYPE   GET LIST                    
         SPACE 1                                                                
         L     RF,=A(NETBLKSV)     SAVE NETBLOCK                                
         LA    RE,NETBLOCK                                                      
         LA    R1,NBBLKEND-NETBLOCK                                             
         MOVE ((RF),(R1)),(RE)                                                  
*                                                                               
         XC    NGBLOCK,NGBLOCK     SET UP FOR NETGOAL                           
         L     R1,NUMWKS           CURRENT NUMBER OF WEEKS IN LIST              
         STC   R1,NGNWEEKS         AND A(LIST) PASSED TO NETGOAL                
         LA    R1,WKLIST                                                        
         ST    R1,NGAWLIST                                                      
         LA    R1,NETBLOCK         A(NETBLOCK)                                  
         ST    R1,NGANTBLK                                                      
         LA    R1,PRDLIST          PASS PRODLIST AREA TO NETGOAL                
         ST    R1,NGAPLIST                                                      
         LA    R1,GOALPOST                                                      
         ST    R1,NGAHOOK                                                       
         MVC   NGSELMKT,SAVMKT     OPTIONAL 'MARKET'                            
         MVC   NGSELDP,DPFILT      OPTIONAL DAYPART                             
         MVC   NGSELSL,NBSELLEN    OPTIONAL SPOT LENGTH                         
         MVI   NGMAXPRD,250        ALLOW 250 PRODUCTS                           
         MVI   NGEXTOPT,4                                                       
         GOTO1 NBCALLOV,DMCB,0,X'D9000A35'                                      
         L     RF,DMCB             PICK UP ADDRESS OF NETGOAL                   
         GOTO1 (RF),DMCB,NGBLOCK                                                
         SPACE 1                                                                
         LA    RF,NETBLOCK        RESTORE NETBLOCK                              
         L     RE,=A(NETBLKSV)                                                  
         LA    R1,NBBLKEND-NETBLOCK                                             
         MOVE ((RF),(R1)),(RE)                                                  
         MVI   NBACTOPT,C'Y'       GET ACTUAL DEMOS                             
         MVI   NBESTOPT,C'A'       GET 'ACTUAL' ESTIMATES                       
         MVI   NBSELUOP,C'A'       USE ACTUAL SCHEDULE                          
         MVI   NBSPLOPT,X'C0'      SPLIT BRANDS EVEN IF POL                     
         MVI   NBDATA,C'U'         GET UNITS                                    
         L     R1,=A(NTILIST)                                                   
         ST    R1,NBCNVNTI                                                      
         TM    OPTIONS,1           ASACT OPTION?                                
         BNO   *+8                                                              
         MVI   NBUSER+8,C'Y'                                                    
         SPACE 1                                                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   GETUNIT2                                                         
         BAS   RE,UNIT                                                          
         B     GETUNIT                                                          
         SPACE 1                                                                
GETUNIT2 CLI   NBMODE,NBREQLST                                                  
         BNE   GETUNIT                                                          
         BAS   RE,REPORT                                                        
         B     XIT                                                              
*                                                                               
PROCERR  DC    H'0'                                                             
         EJECT                                                                  
*              POST GOALS (HOOK FROM NETGOAL)                                   
         SPACE 3                                                                
GOALPOST NTR1                                                                   
         L     R1,NBAIO                                                         
         USING GOALREC,R1                                                       
         TM    GKEYAGY,X'20'       GOAL HIST REC?                               
         BO    XIT                                                              
         DROP  R1                                                               
         XCEF  BREC,312            CLEAR BUFFALO RECORD                         
*                                  FILL KEY                                     
         LA    R1,NETGOALD                                                      
         USING NETGOALD,R1                                                      
         MVC   BKEYPGR,NGOALPRD    PROD CODE (FOR ALPHABETICAL ORDER)           
         MVC   BKEYTARG,NGOALTRG   TARGET DEMO NUMBER                           
         TM    OPTIONS,2                                                        
         BNO   *+10                                                             
         MVC   BKEYTARG,NGOALTR2                                                
         MVC   BKEYPRD,NGOALPRD    PRODUCT CODE                                 
         DROP  R1                                                               
*                                                                               
         L     R1,APRDTRG          SET PRD/TARG INTO TABLE FOR UNITS            
TRGTBL10 CLI   0(R1),0                                                          
         BE    TRGTBL20                                                         
         CLC   0(4,R1),BKEYPGR                                                  
         BE    TRGTBL21                                                         
         LA    R1,4(R1)                                                         
         BCT   RE,TRGTBL10                                                      
         DC    H'0'                                                             
TRGTBL20 MVC   0(4,R1),BKEYPGR     SET PROD/TARG                                
*                                                                               
TRGTBL21 MVC   BKEYSLN,NGOALSL                                                  
         SPACE 1                                                                
         LA    R2,NGOALDOL         POST THE DOLLARS                             
         MVC   WEEKNO,NGOALWKN                                                  
         LA    R3,BGOALDOL                                                      
         BAS   RE,POST                                                          
         SPACE 1                                                                
         LA    R2,NGOALGRP         THEN THE POINTS                              
         TM    OPTIONS,2                                                        
         BNO   *+8                                                              
         LA    R2,NGOALG2                                                       
         MVC   WEEKNO,NGOALWKN                                                  
         LA    R3,BGOALGRP                                                      
         BAS   RE,POST                                                          
         SPACE 1                                                                
         BAS   RE,BUFFPUT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              GENERAL UTILITIES                                                
         SPACE 3                                                                
POST     NTR1                                                                   
*                                  GENERAL POST ROUTINE                         
*                                  R2=A(DATA)                                   
*                                  WEEKNO=COLUMN NUMBER                         
*                                  R3=A(LINE OF DATA)                           
         CLI   WEEKNO,14                                                        
         BL    *+8                                                              
         MVI   WEEKNO,14                                                        
         ZIC   R4,WEEKNO                                                        
         BCTR  R4,0                                                             
         SLL   R4,2                                                             
         AR    R4,R3               POSITION R4 TO EXACT COLUMN                  
         L     R1,0(R2)                                                         
         A     R1,0(R4)                                                         
         ST    R1,0(R4)            AND ADD DATA TO IT                           
         SPACE 1                                                                
         LA    R4,15               15 IS TOTAL COLUMN                           
         BCTR  R4,0                                                             
         SLL   R4,2                                                             
         AR    R4,R3               POSITION R4 TO TOTAL COLUMN                  
         L     R1,0(R2)                                                         
         A     R1,0(R4)                                                         
         ST    R1,0(R4)            AND ADD DATA TO IT                           
         B     XIT                                                              
         SPACE 1                                                                
BUFFPUT  NTR1                                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,BREC                                 
         MVI   BKEYSLN,X'FF'       FOR ALL SPOT LENGTHS                         
         BASR  RE,RF               PUT AGAIN FOR ALL SPOT LENS                  
*                                                                               
         MVI   BKEYTARG,0                                                       
         MVC   BKEYPRD,=X'FFFFFF'  (ALL PRODUCTS)                               
         MVC   BKEYPGR,=X'FFFFFF'  (ALL PRODUCTS)                               
         BASR  RE,RF               PUT AGAIN FOR GRAND TOTAL                    
         B     XIT                                                              
         EJECT                                                                  
*              POST UNITS - DATA FIELDS                                         
         SPACE 3                                                                
UNIT     NTR1                                                                   
         XCEF  BREC,312            CLEAR BUFFALO RECORD                         
         LA    R3,PRDLIST                                                       
         USING LISTD,R3                                                         
         L     R0,NGNPRDS                                                       
         MVC   BYTE,NBPRD                                                       
         CLI   NBSPLPRN,0                                                       
         BE    UNIT2                                                            
         CLI   NBSPLPRN,X'FF'                                                   
         BE    UNIT2                                                            
         MVC   BYTE,NBSPLPRN                                                    
         SPACE 1                                                                
UNIT2    CLC   LISTNO,BYTE         LOOK UP PRODUCT NUMBER                       
         BE    UNIT4                                                            
**       LA    R3,16(R3)                                                        
         ZIC   R1,NGEXTOPT                                                      
         SLL   R1,1                                                             
         LA    R1,LISTEQU(R1)                                                   
         AR    R3,R1                                                            
         BCT   R0,UNIT2                                                         
         B     XIT                                                              
         SPACE 1                                                                
UNIT4    ST    R3,ATHISLST                                                      
         MVC   BKEYPRD,LISTPRD                                                  
         MVC   BKEYPGR,LISTPRD                                                  
         MVC   BKEYTARG,LISTTARG                                                
         MVC   BKEYSLN,NBLEN                                                    
*                                                                               
         TM    OPTIONS,2            DO WE NEED SECOND TARGET?                   
         BNO   UNIT4F                                                           
         MVC   BKEYTARG,LISTDEMO+3                                              
         MVC   LISTTARG,LISTDEMO+3                                              
         B     UNIT4F                                                           
**********888                                                                   
*****    L     R1,APRDTRG          YES/GET PROD/TARGET FROM                     
UNIT4B   CLC   BKEYPRD,0(R1)           ALTERNATE LIST                           
****8    BE    UNIT4D                                                           
*****    LA    R1,4(R1)                                                         
****8    CLI   0(R1),0                                                          
*****    BNE   UNIT4B                                                           
*****    DC    H'0'                                                             
UNIT4D   MVC   BKEYTARG,3(R1)                                                   
********************888                                                         
UNIT4F   LA    R2,1                                                             
         LA    R3,WKLIST                                                        
         SPACE 1                                                                
UNIT6    CLC   NBACTDAT,2(R3)      LOOK UP WEEK NUMBER                          
         BNH   UNIT8                                                            
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         B     UNIT6                                                            
         SPACE 1                                                                
UNIT8    STC   R2,WEEKNO                                                        
         SPACE 1                                                                
         XC    NDDEMOS,NDDEMOS             CLEAR DEMO LIST                      
         L     R3,ATHISLST                                                      
         MVI   NDDEMOS+1,C'R'                                                   
         MVC   NDDEMOS+02(1),LISTTARG      RATINGS FOR THIS PRODUCT             
         MVI   NBWHERE,0                                                        
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         SPACE 1                                                                
         L     R0,NBCALCOS                                                      
         SRDA  R0,31                                                            
         D     R0,=F'100'          TO OF DOLLARS                                
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,DUB              COST IN DOLLARS                              
         LA    R2,DUB                                                           
         LA    R3,BACTDOL                                                       
         BAS   RE,POST                                                          
         SPACE 1                                                                
         XC    DUB,DUB                                                          
         MVC   DUB+2(2),NDESTDEM+2 ESTIMATED POINTS                             
         LA    R3,BESTGRP                                                       
         BAS   RE,POST                                                          
         SPACE 1                                                                
         MVC   DUB+2(2),NDACTDEM+2 ACTUAL POINTS                                
         LA    R3,BACTGRP                                                       
         BAS   RE,POST                                                          
         SPACE 1                                                                
         BAS   RE,BUFFPUT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO CONTROL REPORT                                       
         SPACE 3                                                                
REPORT   NTR1                                                                   
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    BKEY,BKEY                                                        
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ABUFFC),BREC,0                          
         B     REP4                                                             
         SPACE 1                                                                
REP2     GOTO1 BUFFALO,DMCB,=C'SEQ',(0,ABUFFC),BREC,0                           
         SPACE 1                                                                
REP4     TM    DMCB+8,X'80'                                                     
         BO    XIT                                                              
         CLI   LINE,50                                                          
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,CHUNK                                                         
         B     REP2                                                             
         EJECT                                                                  
*              CONTROL A CHUNK FOR PRODUCT/GRAND TOTALS                         
         SPACE 3                                                                
CHUNK    NTR1                                                                   
*        BAS   RE,SPLAT            BLANK LINE FIRST                             
         MVC   P+1(20),=C'*** GRAND TOTALS ***'                                 
         CLI   BKEYPRD,X'FF'                                                    
         BE    CHUNK2                                                           
         BAS   RE,GETPROD                                                       
         CLI   BKEYSLN,X'FF'                                                    
         BNE   CHUNK1C                                                          
         MVC   P+1(20),=C'TOTALS FOR          '                                 
         MVC   P2+1(3),BKEYPRD                                                  
         MVC   P2+5(16),PRODNAME                                                
         MVI   NXTPAGE,1                                                        
         B     CHUNK2                                                           
CHUNK1C  MVC   P+1(3),BKEYPRD                                                   
         MVC   P+5(16),PRODNAME                                                 
         MVI   P+21,C'-'                                                        
         EDIT  (B1,BKEYSLN),(2,P+22)                                            
         SPACE 1                                                                
CHUNK2   BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         MVC   P+5(20),=CL20'BUDGET ($000)'                                     
         LA    R2,BGOALDOL                                                      
         MVI   LINETYPE,0                                                       
         MVI   DATATYPE,C'$'                                                    
         BAS   RE,FORMAT                                                        
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         MVC   P+5(20),=CL20'EXPENDITURE ($000)'                                
         LA    R2,BACTDOL                                                       
         MVI   LINETYPE,0                                                       
         MVI   DATATYPE,C'$'                                                    
         BAS   RE,FORMAT                                                        
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         MVC   P+5(20),=CL20'EXPENDITURE-BUDGET'                                
         MVC   P+137(5),=C'INDEX'                                               
         LA    R2,BACTDOL                                                       
         LA    R3,BGOALDOL                                                      
         MVI   LINETYPE,1                                                       
         MVI   DATATYPE,C'$'                                                    
         BAS   RE,FORMAT                                                        
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         MVC   WORK,SPACES                                                      
         MVC   WORK(20),=C'GOAL DDDDDDD (GRPS) '                                
         MVC   WORK+5(7),TARGNAME                                               
         GOTO1 SQUASHER,DMCB,WORK,20                                            
         MVC   P+5(20),WORK                                                     
         LA    R2,BGOALGRP                                                      
         MVI   LINETYPE,0                                                       
         MVI   DATATYPE,C'G'                                                    
         BAS   RE,FORMAT                                                        
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         MVC   P+5(20),=CL20'ESTIMATE (GRPS)'                                   
         LA    R2,BESTGRP                                                       
         MVI   LINETYPE,0                                                       
         MVI   DATATYPE,C'G'                                                    
         BAS   RE,FORMAT                                                        
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         MVC   P+5(20),=CL20'ESTIMATE-GOAL (GRPS)'                              
         MVC   P+137(5),=C'INDEX'                                               
         LA    R2,BESTGRP                                                       
         LA    R3,BGOALGRP                                                      
         MVI   LINETYPE,1                                                       
         MVI   DATATYPE,C'G'                                                    
         BAS   RE,FORMAT                                                        
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         MVC   P+5(20),=CL20'ACTUAL (GRPS)'                                     
         LA    R2,BACTGRP                                                       
         MVI   LINETYPE,0                                                       
         MVI   DATATYPE,C'G'                                                    
         BAS   RE,FORMAT                                                        
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         MVC   P+5(20),=CL20'ACTUAL-GOAL (GRPS)'                                
         MVC   P+137(5),=C'INDEX'                                               
         LA    R2,BACTGRP                                                       
         LA    R3,BGOALGRP                                                      
         MVI   LINETYPE,1                                                       
         MVI   DATATYPE,C'G'                                                    
         BAS   RE,FORMAT                                                        
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
**       CLI   BKEYPRD,X'FF'                                                    
**       BNE   *+12                                                             
         CLI   NXTPAGE,1                                                        
***      BNE   *+16                                                             
         BNE   XIT                                                              
         MVI   NXTPAGE,0                                                        
*****    MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
**       BAS   RE,SPLAT                                                         
**       B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT A LINE OF NUMBERS                              
         SPACE 3                                                                
*              INPUTS              R2=A(ACCUMS)                                 
*                                  R3=A(ACCUMS TO BE SUBTRACTED)                
*                                  LINETYPE 0=REGULAR 1=DIFFERENCE              
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         LA    R4,P+25                                                          
         L     R0,NUMWKS                                                        
         STM   R2,R3,SAVER23                                                    
         SPACE 1                                                                
FORMAT2  BAS   RE,COLUMN           HANDLE EACH WEEK COLUMN                      
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,7(R4)                                                         
         BCT   R0,FORMAT2                                                       
         SPACE 1                                                                
         LM    R2,R3,SAVER23       THEN THE TOTAL COLUMN                        
         LA    R2,56(R2)                                                        
         LA    R3,56(R3)                                                        
         LA    R4,1(R4)                                                         
         BAS   RE,COLUMN                                                        
         B     XIT                                                              
         SPACE 1                                                                
COLUMN   NTR1                                                                   
         L     R1,0(R2)                                                         
         MVI   SIGN,C' '                                                        
         CLI   LINETYPE,0                                                       
         BE    COLUMN2                                                          
         S     R1,0(R3)                                                         
         MVI   SIGN,C'+'                                                        
         BP    COLUMN1C                                                         
         MVI   SIGN,C'-'                                                        
COLUMN1C CLC   =C'INDEX',P+137     INDEX?                                       
         BNE   COLUMN2             NO                                           
         LR    RF,R1               SAVE R1                                      
         L     RE,0(R2)            ACHIEVED                                     
         CVD   RE,DUB                                                           
         ZAP   PAKWRK,DUB                                                       
         MP    PAKWRK,=P'1000'                                                  
         L     RE,0(R3)            GOAL                                         
         LTR   RE,RE                                                            
         BZ    COLUMN1E                                                         
         CVD   RE,DUB                                                           
         DP    PAKWRK,DUB                                                       
         ZAP   DUB,PAKWRK(8)                                                    
         AP    DUB,=P'5'                                                        
         ZAP   PAKWRK,DUB                                                       
         DP    PAKWRK,=P'10'                                                    
         EDIT  (P6,PAKWRK+8),(4,132(R4))                                        
COLUMN1E LR    R1,RF               RESTORE R1                                   
         SPACE 1                                                                
COLUMN2  LTR   R1,R1                                                            
         BZ    XIT                                                              
         CLI   DATATYPE,C'$'       IF DATA IS DOLLARS                           
         BNE   COLUMN4                                                          
         M     R0,=F'2'            NEED TO CONVERT TO ($000)                    
         D     R0,=F'100'          WITH 1 DECIMAL PLACE                         
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SPACE 1                                                                
COLUMN4  EDIT  (R1),(7,0(R4)),1                                                 
         SPACE 1                                                                
COLUMN6  CLI   0(R4),C' '                                                       
         BNE   COLUMN8                                                          
         LA    R4,1(R4)                                                         
         B     COLUMN6                                                          
         SPACE 1                                                                
COLUMN8  BCTR  R4,0                                                             
         MVC   0(1,R4),SIGN                                                     
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DIG OUT PRODUCT NAME                                  
         SPACE 3                                                                
*              INPUT               BKEYPRD=PRODUCT CODE                         
*              OUTPUT              PRODNAME=PRODUCT NAME                        
*                                  TARGUNIV=EDITED UNIVERSE                     
*                                  TARGNAME=EDITED TARGET                       
         SPACE 1                                                                
GETPROD  NTR1                                                                   
         CLC   BKEYPRD,=C'POL'                                                  
         BNE   GETPROD2                                                         
         MVC   PRODNAME,=CL20'UNALLOCATED'                                      
         B     GETPROD4                                                         
         SPACE 1                                                                
         USING LISTD,R3                                                         
GETPROD2 DS    0H                                                               
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOTFILE                      
         LA    R4,KEY                                                           
         USING PRDHDR,R4                                                        
         XC    PKEY,PKEY                                                        
         MVC   PKEYAM,NBACTAM                                                   
         MVC   PKEYCLT,NBACTCLI                                                 
         MVC   PKEYPRD,BKEYPRD                                                  
         MVC   FILENAME,=C'SPTDIR '                                             
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL '                                             
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         MVC   PRODNAME,PNAME                                                   
         SPACE 1                                                                
GETPROD4 XC    DUB,DUB                                                          
         MVI   DUB+1,C'U'                                                       
         MVC   DUB+2(1),BKEYTARG                                                
         GOTO1 DEMOCON,DMCB,(0,DUB),(7,WORK),DBLOCK,0                           
         MVC   TARGNAME,WORK                                                    
         GOTO1 NBDEMOUT,DMCB,(C'D',DUB),DBLOCK,BLOCK                            
         EDIT  (4,BLOCK),(10,TARGUNIV),ALIGN=LEFT                               
         NETGO NVSETUNT,DMCB       RESET TO READ UNITFILE                       
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         SPACE 1                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINE HOOK - BOXES AND HEADLINES                              
         SPACE 3                                                                
HOOK     NTR1                                                                   
         NETGO NVTITOUT,DMCB       POSSIBLE USER TITLE                          
         L     R4,ABOX             SET UP BOXES                                 
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXROWS+07,C'T'                                                  
         MVI   BOXROWS+09,C'M'                                                  
         MVI   BOXROWS+24,C'M'                                                  
         MVI   BOXROWS+39,C'M'                                                  
         MVI   BOXROWS+55,C'B'                                                  
         MVI   BOXCOLS,C'L'                                                     
         LA    R2,WKLIST                                                        
         LA    R3,BOXCOLS+25                                                    
         LA    R5,H9+26                                                         
         L     R0,NUMWKS                                                        
         SPACE 1                                                                
HOOK2    MVI   0(R3),C'C'          VERTICAL LINE LEFT OF EACH WEEK              
         GOTO1 DATCON,DMCB,(2,(R2)),(4,1(R5))     MMMDD                         
         MVC   0(3,R5),1(R5)                                                    
         MVI   3(R5),C'/'                         (NOW MMM/DD)                  
         CLI   PERTYPE,C'M'                       FOR MONTHLY                   
         BNE   HOOK4                                                            
         GOTO1 DATCON,DMCB,(2,(R2)),(0,WORK)     ..BUMP MONTH BY 7              
         PRINT GEN                                                              
         GOTO1 ADDAY,DMCB,WORK,WORK+20,7         ..TO ENSURE CORRECT            
         PRINT NOGEN                                                            
         GOTO1 DATCON,DMCB,(0,WORK+20),(9,0(R5)) ..MMM/YY                       
         SPACE 1                                                                
HOOK4    LA    R2,4(R2)                                                         
         LA    R3,7(R3)                                                         
         LA    R5,7(R5)                                                         
         BCT   R0,HOOK2                                                         
         MVI   0(R3),C'C'                                                       
         MVI   8(R3),C'R'                                                       
         MVC   1(5,R5),=C'TOTAL'                                                
         SPACE 1                                                                
         MVC   H4+10(7),SPLCLI                                                  
         MVC   H5+10(7),SPLPRO                                                  
         MVC   H6+10(7),SPLEST                                                  
         MVC   H4+18(20),SPLCLIN                                                
         MVC   H5+18(20),SPLPRON                                                
         MVC   H6+18(24),SPLESTN                                                
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
         PRINT NOGEN                                                            
HEDSPECS SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,46,C'WEEKLY GOAL POST-BUY'                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,52,PERIOD                                                     
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,99,PAGE                                                       
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H9,2,C'BRAND'                                                    
         DC    X'00'                                                            
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
RELOC    DC    A(*)                                                             
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
         BUFF  LINES=500,ROWS=1,COLUMNS=75,FLAVOR=BINARY,              X        
               KEYLIST=(12,A)                                                   
*                                                                               
NETBLKSV DS    CL2000                                                           
PRDTARG  DS    CL1020     PROD/TARG  CL4X255=1020                               
CLTRECSV DS    CL1500     CLIENT RECORD                                         
NTILIST  DS    CL4000     NTI STATION LIST FOR 400 STATIONS                     
         EJECT                                                                  
*              DSECTS FOR PROGRAM                                               
         SPACE 3                                                                
LISTD    DSECT                     DSECT FOR STRUCTURE OF PRDLIST               
LISTNO   DS    XL1                                                              
LISTFLAG DS    XL1                                                              
LISTTGSM EQU   X'01'                                                            
LISTDIV  DS    XL2                                                              
LISTPRD  DS    CL3                                                              
LISTTARG DS    XL1                                                              
LISTUSER DS    XL1                                                              
LISTEQU  EQU   *-LISTD                                                          
LISTDEMO DS    CL16                                                             
*LISTREC  DS    0CL16                                                           
*LISTNO   DS    CL1                                                             
*LISTDIV  DS    CL2                                                             
*LISTPRD  DS    CL3                                                             
*LISTTARG DS    CL1                                                             
*LISTACNO DS    CL1                                                             
*LISTDEMO DS    CL8                                                             
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIF9D                                                       
*                                                                               
*                                                                               
*                                                                               
*                                  SPGENPRD                                     
*                                  DDBIGBOX                                     
*                                  NEGENINCLS                                   
*                                  NETDEMOD                                     
*                                  DEDBLOCK                                     
*                                  NENETGOALD                                   
         PRINT OFF                                                              
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NENETGOALD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*              WORKING STORAGE                                                  
         SPACE 3                                                                
SAVMKT   DS    CL2                 COMMON WITH EDIT PHASE                       
DPFILT   DS    CL1                                                              
OPTIONS  DS    CL1                                                              
         SPACE 1                                                                
RELO     DS    A                                                                
ABUFFC   DS    A                                                                
APRDTRG  DS    A                                                                
MAXWKS   EQU   13                  MAXIMUM NUMBER OF WEEKS                      
PERTYPE  DS    CL3                 PERIOD TYPE                                  
NUMWKS   DS    F                   NUMBER OF WEEKS                              
WKLIST   DS    CL(4*MAXWKS)        WEEK LIST                                    
DEMOCON  DS    A                                                                
LEVEL    DS    CL1                                                              
NXTPAGE  DS    CL1                                                              
         DS    0D                                                               
PAKWRK   DS    PL16                                                             
         SPACE 1                                                                
TARGUNIV DS    CL10                                                             
TARGNAME DS    CL7                                                              
PRODNAME DS    CL20                                                             
WEEKNO   DS    XL1                                                              
ATHISLST DS    A                                                                
SAVER23  DS    D                                                                
SIGN     DS    CL1                                                              
LINETYPE DS    XL1                 0=NUMERIC 1=DIFFERENCE                       
DATATYPE DS    CL1                 $=CASH G=GRPS                                
         EJECT                                                                  
*              BUFFALO RECORD                                                   
         SPACE 3                                                                
         DS    0D                                                               
BREC     DS    0C                  START OF BUFFALO RECORD                      
BKEY     DS    0CL12               BUFFALO RECORD                               
BKEYPGR  DS    XL3                 OPTIONAL PRODUCT GROUP                       
BKEYTARG DS    XL1                 TARGET DEMO CATEGORY                         
BKEYPRD  DS    CL3                 PRODUCT CODE                                 
BKEYSLN  DS    CL1                 SPOT LENGTH                                  
         DS    CL4                 SPARE                                        
         SPACE 1                                                                
BGOALDOL DS    15F                 BUDGET DOLLARS ($000) 1 DEC                  
BACTDOL  DS    15F                 ACTUAL DOLLARS ($000) 1 DEC                  
BGOALGRP DS    15F                 GOAL POINTS (1 DEC)                          
BESTGRP  DS    15F                 ESTIMATED POINTS (1 DEC)                     
BACTGRP  DS    15F                 ACTUAL POINTS (1 DEC)                        
         DS    CL4                                                              
PRDLIST  DS    3200X'00'           PROD LIST AREA FOR NETGOAL                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049NEWRIB7   03/14/18'                                      
         END                                                                    
