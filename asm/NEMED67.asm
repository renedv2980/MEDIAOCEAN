*          DATA SET NEMED67    AT LEVEL 003 AS OF 05/01/02                      
*          DATA SET NEMED67    AT LEVEL 084 AS OF 07/15/99                      
*PHASE T31E67A,+0                                                               
*INCLUDE NEGOALIO                                                               
         TITLE 'T31E67 - PERFORMANCE REPORT'                                    
T31E67   CSECT                                                                  
*                                                                               
******************************************************************              
*  THIS IS A NEW VERSION OF THE PERFORMANCE REPORT TO USE NETGOAL.              
*  THE OLD VERSION IS IN NEMED67S.                                              
*                                                                               
*                                                                               
*   ORGANIZATION OF W/S                                                         
*                                                                               
* ANETWS1 -> NET DEMO BLOCK   (516 BYTES)                                       
*                                                                               
*            DBLOCK            (256 BYTES)                                      
*                                                                               
*            ARGS FROM EDIT TO PRINT MODULE    (ABOUT 15 BYTES)                 
*                 DPFILT,OPTIONS,PACKED MARKET                                  
*                                                                               
*            NETGOAL BLOCK                           (100 BYTES)                
*                                                                               
*            800 BYTE AREA FOR NETGOAL PRODUCT LIST  (800 BYTES)                
*           1600 BYTE AREA FOR NETGOAL PRODUCT LIST  (1600 BYTES)               
*                                                                               
*            WORKING STORAGE                                                    
*              INCLUDES 3200 BYTES FOR ACCUMULATORS                             
*                                                                               
*** REGISTER USAGE:                                                             
*   RC,R9 - ADDRESSABILITY FOR COMMON DSECTS                                    
*   RA    - TWA                                                                 
*   R8    - SPOOL                                                               
*   R7    - NETDEMO BLOCK                                                       
*                                                                               
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BUPR**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING NDDEMBLK,R7                                                      
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         XC    NETLIST,NETLIST                                                  
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000AE0'    A(DEMOCON)                          
         MVC   DEMOCON,DMCB                                                     
         EJECT                                                                  
*              CONTROL OF REPORT - GOALS AND SPOTS                              
         SPACE 3                                                                
         XC    PERTYPE,PERTYPE                                                  
         MVI   PERTYPE,C'W'        USE WEEKS                                    
         MVI   PERTYPE+1,1         USE MONTHS IF TOO MANY WEEKS                 
         LA    R4,MAXWKS                                                        
         ST    R4,NUMWKS           MAX NUMBER OF WEEKS                          
*                                                                               
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK    PROCESS DATES                           
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT                                                  
         BE    GETLIST                                                          
         B     PROCDAT                                                          
*                                                                               
GETLIST  NETGO NVWKLST,DMCB,NUMWKS,WKLIST,PERTYPE   GET LIST                    
         SPACE 1                                                                
         XC    NGBLOCK,NGBLOCK     SET UP FOR NETGOAL                           
         MVI   NGMAXPRD,198                                                     
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
         CLI   SPLTYPE+2,X'41'     POSSIBLE PRODUCT GROUP SCHEME                
         BL    *+10                                                             
         MVC   NGSELSCH,SPLTYPE+2                                               
         MVC   NGSELMKT,SAVMKT     OPTIONAL 'MARKET'                            
         MVC   NGSELDP,DPFILT      OPTIONAL DAYPART                             
         GOTO1 NBCALLOV,DMCB,0,X'D9000A35'                                      
         L     RF,DMCB             PICK UP ADDRESS OF NETGOAL                   
         GOTO1 (RF),DMCB,NGBLOCK                                                
         MVC   GROUPA(13),NGOALBK1                                              
         MVC   GROUPB(13),NGOALBK2                                              
         SPACE 1                                                                
         MVI   NBACTOPT,C'Y'       GET ACTUAL DEMOS                             
         MVI   NBESTOPT,C'Y'       GET ESTIMATED DEMOS                          
         MVI   NBDATA,C'U'         GET UNITS                                    
         OI    NBSPLOPT,X'C0'                                                   
         SPACE 1                                                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   GETUNIT2                                                         
         BAS   RE,SPOT                                                          
         B     GETUNIT                                                          
         SPACE 1                                                                
GETUNIT2 CLI   NBMODE,NBREQLST                                                  
         BNE   GETUNIT                                                          
         BAS   RE,TOTS                                                          
         B     XIT                                                              
*                                                                               
PROCERR  DC    H'0'                                                             
         EJECT                                                                  
*              POST GOALS (HOOK FROM NETGOAL)                                   
         SPACE 3                                                                
GOALPOST NTR1                                                                   
         ZIC   R2,NGOALWKN         ACCUM NO IS WEEK NO                          
         CLI   SPLTYPE,C'W'        FOR WEEK TYPE                                
         BE    GP8                                                              
         ZIC   R2,NGOALUSR         PICK UP ACCUMULATOR NO                       
         CLI   SPLTYPE,C'N'        FOR BRAND TYPE                               
         BNE   GP8                                                              
         LA    R2,1                OTHERWISE ITS NETWORK NO                     
         LA    R0,20                                                            
         LA    R3,NETLIST                                                       
         SPACE 2                                                                
GP2      CLC   0(2,R3),NGOALMKT    MARKET TO NETWORK TRANS                      
         BE    GP4                                                              
         OC    0(2,R3),0(R3)                                                    
         BZ    GP4                                                              
         LA    R2,1(R2)                                                         
         LA    R3,7(R3)                                                         
         BCT   R0,GP2                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
GP4      MVC   0(2,R3),NGOALMKT                                                 
         STC   R2,6(R3)                                                         
         CLC   NGOALMKT,=H'1'      FLESH OUT ALPHA                              
         BNE   *+10                                                             
         MVC   2(4,R3),=C'ABC '                                                 
         CLC   NGOALMKT,=H'2'                                                   
         BNE   *+10                                                             
         MVC   2(4,R3),=C'CBS '                                                 
         CLC   NGOALMKT,=H'3'                                                   
         BNE   *+10                                                             
         MVC   2(4,R3),=C'NBC '                                                 
         CLC   NGOALMKT,=H'7777'                                                
         BNE   *+10                                                             
         MVC   2(4,R3),=C'ALL '                                                 
         CLC   NGOALMKT,=H'777'                                                 
         BNE   *+10                                                             
         MVC   2(4,R3),=C'ALL '                                                 
         SPACE 1                                                                
GP8      BCTR  R2,0                POSITION INTO ACCUM                          
         MH    R2,=H'32'                                                        
         LA    R2,BUDGAREA(R2)                                                  
         USING BUDGD,R2                                                         
         LM    R0,R1,ACGDOL        AND ADD IN GOALS                             
         A     R0,NGOALDOL                                                      
         C     R0,=F'50000000'                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
         A     R1,NGOALGRP                                                      
         STM   R0,R1,ACGDOL                                                     
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              POST SPOTS                                                       
         SPACE 3                                                                
SPOT     NTR1                                                                   
         LA    R2,1                                                             
         LA    R3,WKLIST                                                        
         SPACE 1                                                                
WKLOOP   CLC   NBACTDAT,2(R3)                                                   
         BNH   GOTWK                                                            
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         B     WKLOOP                                                           
         SPACE 1                                                                
GOTWK    LA    R3,PRDLIST                                                       
         CLI   SPLTYPE,C'W'                                                     
         BE    SP8                                                              
         LA    R2,1                                                             
         LA    R0,20                                                            
         LA    R3,NETLIST                                                       
         SPACE 1                                                                
SP2      CLC   0(2,R3),NBMARKET    LOOK UP MARKET NUMBER (TYPE N)               
         BE    SP4                                                              
         OC    0(2,R3),0(R3)                                                    
         BZ    SP4                                                              
         LA    R3,7(R3)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,SP2                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
SP4      MVC   0(2,R3),NBMARKET                                                 
         MVC   2(4,R3),NBACTNET    FLESH OUT WITH ALPHA                         
         STC   R2,6(R3)            PICK UP ACCUM NO                             
         LA    R3,PRDLIST          LOOK UP PRODUCT LIST FOR BRAND               
         CLI   SPLTYPE,C'N'                                                     
         BE    SP8                                                              
         L     R0,NGNPRDS               TYPE                                    
         SPACE 1                                                                
         USING LISTD,R3                                                         
SP6      ZIC   R2,LISTACNO                                                      
*        CLC   NBPRD,LISTNO                                                     
         CLC   NBSPLPRN,LISTNO                                                  
         BE    SP8                                                              
         LA    R3,8(R3)                                                         
         BCT   R0,SP6                                                           
*                                                                               
         L     R0,NGNPRDS          IF NO MATCH,CHECK IF UNALLOCATED             
         LA    R3,PRDLIST          (NBSPLPRN=X'FF',NBPRD=0)                     
SP7      ZIC   R2,LISTACNO                                                      
         CLI   LISTNO,0            AND SET IN POL                               
         BE    SP8                                                              
         LA    R3,8(R3)                                                         
         BCT   R0,SP7                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
SP8      XC    NDDEMOS,NDDEMOS       CLEAR DEMO LIST                            
         MVI   NDDEMOS+1,C'I'        REQUEST TO RELOOK UP DEMOS                 
         MVC   NDDEMOS+2(1),LISTTARG                                            
         MVI   NBWHERE,0                                                        
***      GOTO1 HEXOUT,DMCB,NBAIO,P,100                                          
***      GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         MVI   NBUPUNIT,0          RESET UPUNIT                                 
         EJECT                                                                  
*              NOW ADD INTO ACCUMULATORS                                        
         SPACE 3                                                                
SP10     BCTR  R2,0                POSITION INTO ACCUMS                         
         MH    R2,=H'32'                                                        
         LA    R2,BUDGAREA(R2)                                                  
         USING BUDGD,R2                                                         
         SPACE 1                                                                
         OC    NBESTUN,NBESTUN     ESTIMATED FIELDS                             
         BZ    SP12                                                             
         L     R1,NBACTUAL         ADD IN ESTIMATED DOLLARS                     
         M     R0,=F'1'            CONVERT TO DOLLARS                           
         D     R0,=F'100'                                                       
         A     R1,ACEDOL                                                        
         ST    R1,ACEDOL                                                        
         LH    R1,NBESTHOM+2       HOMES PNTS                                   
         A     R1,ACEHOME                                                       
         ST    R1,ACEHOME                                                       
         LH    R1,NDESTDEM+2       ADD IN DEMO PNTS                             
         A     R1,ACEPNTS                                                       
         ST    R1,ACEPNTS                                                       
         SPACE 1                                                                
SP12     OC    NBACTUN,NBACTUN     ACTUAL FIELDS                                
         BZ    XIT                                                              
*******  CLI   NBRESULT,C'E'       (NOW ALLOW ESTIMATED VALUES TO               
*******  BE    XIT                 SHOW IN ACTUAL COLUMNS 11/02/86)             
         L     R1,NBACTUAL         ADD IN ACTUAL DOLLARS                        
         M     R0,=F'1'            CONVERT TO DOLLARS                           
         D     R0,=F'100'                                                       
         A     R1,ACADOL                                                        
         ST    R1,ACADOL                                                        
         LH    R1,NBACTHOM+2       HOMES                                        
         A     R1,ACAHOME                                                       
         ST    R1,ACAHOME                                                       
         LH    R1,NDACTDEM+2       AND DEMOS                                    
         A     R1,ACAPNTS                                                       
         ST    R1,ACAPNTS                                                       
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINES TO CONTROL TOTALS                                       
         SPACE 3                                                                
TOTS     NTR1                                                                   
         LA    R5,BUDGAREA                                                      
         USING BUDGD,R5                                                         
         XC    DIVACS,DIVACS                                                    
         XC    ESTACS,ESTACS                                                    
         LA    R6,ESTACS                                                        
         CLI   SPLTYPE,C'B'                                                     
         BE    TOTS4                                                            
         CLI   SPLTYPE,C'N'                                                     
         BE    TOTS10                                                           
         LA    R2,WKLIST           WEEKLY ROUTINES                              
         L     R3,NUMWKS                                                        
         SPACE 2                                                                
TOTS2    GOTO1 DATCON,DMCB,(2,(R2)),(4,P+6)                                     
         CLI   PERTYPE,C'M'       SHOW MMMYY                                    
         BNE   TOTS3                                                            
         GOTO1 DATCON,DMCB,(2,2(R2)),(6,P+6)                                    
         MVC   P+9(3),P+10                                                      
         SPACE 2                                                                
TOTS3    BAS   RE,FORMAT                                                        
         BAS   RE,ADDEM                                                         
         LA    R2,4(R2)                                                         
         LA    R5,32(R5)                                                        
         BCT   R3,TOTS2                                                         
         B     TOTS16                                                           
         SPACE 2                                                                
TOTS4    LA    R3,PRDLIST          PRODLIST                                     
         L     R4,NGNPRDS                                                       
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         LR    R0,R4                                                            
         SR    R2,R2                                                            
         USING LISTD,R3                                                         
         XC    LASTDIV,LASTDIV                                                  
         GOTO1 XSORT,DMCB,(R3),(R4),8,5,1                                       
         SPACE 2                                                                
TOTS6    CLC   LISTDIV,LASTDIV     (DIVISION TOTALS IF APPLIC.)                 
         BE    TOTS8                                                            
         OC    LASTDIV,LASTDIV                                                  
         BZ    TOTS7                                                            
         LA    R5,DIVACS                                                        
         MVC   P+1(10),=C'TOTALS FOR'                                           
         MVC   P2+1(12),GROUPA                                                  
         CLC   GROUPB(12),SPACES                                                
         BNH   *+10                                                             
         MVC   P2+1(12),GROUPB                                                  
         CH    R2,=H'2'                                                         
         BL    *+8                                                              
         BAS   RE,FORMAT                                                        
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         SR    R2,R2                                                            
         XC    DIVACS,DIVACS                                                    
         SPACE 2                                                                
TOTS7    LA    R4,KEY              READ GROUP RECORD FOR NEW GROUP              
         XC    KEY,KEY                                                          
         USING PRGKEY,R4                                                        
         MVC   PRGKTYP(2),=X'0D01'                                              
         MVC   PRGKAGMD(3),NBACTAM                                              
         MVC   PRGKID,SPLTYPE+2                                                 
         MVC   PRGKGRP,LISTDIV                                                  
         MVC   GROUPA+13(24),SPACES                                             
         MVC   GROUPB+13(24),SPACES                                             
         CLC   LISTDIV,=X'9990'                                                 
         BE    TOTS7B                                                           
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         USING PRGEL10,R6                                                       
         MVC   GROUPA+13(24),PRGNAM1                                            
         MVC   GROUPB+13(24),PRGNAM2                                            
         SPACE 2                                                                
TOTS7B   MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
TOTS8    MVC   LASTDIV,LISTDIV                                                  
         MVC   P+1(3),LISTPRD                                                   
         XC    DUB,DUB             BUILD DEMO FOR DEMOCON                       
         MVI   DUB+1,C'R'          USE RATING                                   
         MVC   DUB+2(1),LISTTARG                                                
         PRINT GEN                                                              
         GOTO1 DEMOCON,DMCB,(0,DUB),(7,WORK),(C'S',DBLOCK)                      
         PRINT NOGEN                                                            
         MVC   P+5(7),WORK                                                      
         ZIC   R5,LISTACNO                                                      
         BCTR  R5,0                                                             
         MH    R5,=H'32'                                                        
         LA    R5,BUDGAREA(R5)                                                  
         BAS   RE,FORMAT                                                        
         XC    ACEPNTS,ACEPNTS     AVOID APPLES AND ORANGES                     
         XC    ACAPNTS,ACAPNTS                                                  
         LA    R6,DIVACS                                                        
         BAS   RE,ADDEM                                                         
         LA    R6,ESTACS                                                        
         BAS   RE,ADDEM                                                         
         LA    R3,8(R3)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,TOTS6                                                         
         CH    R2,=H'2'                                                         
         BL    TOTS16                                                           
         OC    LASTDIV,LASTDIV                                                  
         BZ    TOTS16                                                           
         LA    R5,DIVACS                                                        
         MVC   P+1(10),=C'TOTALS FOR'                                           
         MVC   P2+1(12),GROUPA                                                  
         CLC   GROUPB(12),SPACES                                                
         BNH   *+10                                                             
         MVC   P2+1(12),GROUPB                                                  
         BAS   RE,FORMAT                                                        
         B     TOTS16                                                           
         SPACE 2                                                                
TOTS10   LA    R2,NETLIST                                                       
         LA    R3,20                                                            
         SPACE 2                                                                
TOTS12   OC    0(7,R2),0(R2)       NETWORK ROUTINES                             
         BZ    TOTS14                                                           
         MVC   P+6(4),2(R2)                                                     
         ZIC   R5,6(R2)                                                         
         BCTR  R5,0                                                             
         MH    R5,=H'32'                                                        
         LA    R5,BUDGAREA(R5)                                                  
         BAS   RE,FORMAT                                                        
         BAS   RE,ADDEM                                                         
         SPACE 2                                                                
TOTS14   LA    R2,7(R2)                                                         
         BCT   R3,TOTS12                                                        
         SPACE 2                                                                
TOTS16   MVC   P+6(6),=C'TOTALS'                                                
         MVC   P2,SPACES                                                        
         LA    R5,ESTACS                                                        
         BAS   RE,FORMAT                                                        
         B     XIT                                                              
         SPACE 2                                                                
ADDEM    NTR1                                                                   
         LA    R0,8                                                             
         SPACE 2                                                                
ADD2     L     R1,0(R6)                                                         
         A     R1,0(R5)                                                         
         ST    R1,0(R6)                                                         
         LA    R6,4(R6)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,ADD2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT A LINE AND PRINT                               
         SPACE                                                                  
FORMAT   NTR1                                                                   
         LA    R2,ACGDOL           GOAL                                         
         LA    R3,P+14                                                          
         XC    CPPLIST(12),CPPLIST                                              
         LA    R4,CPPLIST                                                       
         BAS   RE,FORMAT1                                                       
         LA    R2,ACEDOL           EST                                          
         LA    R3,P+34                                                          
         LA    R4,CPPLIST+4                                                     
         BAS   RE,FORMAT2                                                       
         LA    R2,ACADOL           ACT                                          
         LA    R3,P+65                                                          
         LA    R4,CPPLIST+8                                                     
         BAS   RE,FORMAT2                                                       
         L     R2,CPPLIST+4                                                     
         L     R3,CPPLIST          EST V GOAL                                   
         LA    R4,P+96                                                          
         BAS   RE,FORMAT6                                                       
         L     R2,CPPLIST+8        ACT V GOAL                                   
         LA    R4,P+101                                                         
         BAS   RE,FORMAT6                                                       
         L     R3,CPPLIST+4        ACT V EST                                    
         LA    R4,P+106                                                         
         BAS   RE,FORMAT6                                                       
         CLC   P+14(96),SPACES                                                  
         BNE   *+14                                                             
         MVC   P,SPACES                                                         
         B     XIT                                                              
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
FORMAT1  NTR1                                                                   
         EDIT  (4,0(R2)),(8,0(R3)),ZERO=BLANK                                   
         L     R1,4(R2)                                                         
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(5,8(R3)),ZERO=BLANK                                        
         LTR   R1,R1                                                            
         BZ    FORMAT1X                                                         
         L     R1,0(R2)                                                         
         M     R0,=F'20'                                                        
         D     R0,4(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R4)                                                         
         EDIT  (R1),(5,14(R3)),ZERO=BLANK                                       
FORMAT1X B     XIT                                                              
         EJECT                                                                  
FORMAT2  NTR1                                                                   
         EDIT  (4,0(R2)),(8,0(R3)),ZERO=BLANK                                   
         L     R1,4(R2)                                                         
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(5,8(R3)),ZERO=BLANK                                        
         LTR   R1,R1                                                            
         BZ    FORMAT4                                                          
         L     R1,0(R2)                                                         
         M     R0,=F'20'                                                        
         D     R0,4(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R4)                                                         
         EDIT  (R1),(5,14(R3)),ZERO=BLANK                                       
         SPACE 2                                                                
FORMAT4  L     R1,8(R2)                                                         
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(5,19(R3)),ZERO=BLANK                                       
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         L     R1,0(R2)                                                         
         M     R0,=F'20'                                                        
         D     R0,8(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(5,25(R3)),ZERO=BLANK                                       
         B     XIT                                                              
         SPACE 2                                                                
FORMAT6  NTR1                                                                   
         LTR   R2,R2               INDICES ROUTINE                              
         BZ    XIT                                                              
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         LR    R1,R3                                                            
         M     R0,=F'200'                                                       
         DR    R0,R2                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         CH    R1,=H'999'                                                       
         BH    XIT                                                              
         EDIT  (R1),(3,1(R4))                                                   
         B     XIT                                                              
         EJECT                                                                  
         DROP  R5                                                               
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R4,ABOX             SET UP BOXES IF OFF LINE                     
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    HOOK1                                                            
         MVI   H12+130,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXROWS+07,C'T'                                                  
         MVI   BOXROWS+11,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         MVI   BOXCOLS+12,C'C'                                                  
         MVI   BOXCOLS+33,C'C'                                                  
         MVI   BOXCOLS+64,C'C'                                                  
         MVI   BOXCOLS+95,C'C'                                                  
         MVI   BOXCOLS+110,C'R'                                                 
         MVI   BOXCOLS+4,C'L'                                                   
         CLI   SPLTYPE,C'B'                                                     
         BNE   HOOK1                                                            
         MVI   BOXCOLS+4,C' '                                                   
         MVI   BOXCOLS,C'L'                                                     
         SPACE 1                                                                
HOOK1    MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(3),SPLPRO                                                  
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+14(20),SPLCLIN                                                
         MVC   H5+14(20),SPLPRON                                                
         MVC   H6+14(24),SPLESTN                                                
         MVC   H5+85(4),SPLNET                                                  
         CLI   SPLNETH+5,0                                                      
         BNE   *+10                                                             
         MVC   H5+85(4),=C'ALL '                                                
         MVC   H6+85(7),SPLDPTN                                                 
         CLI   SPLDPTH+5,0                                                      
         BNE   *+10                                                             
         MVC   H6+85(3),=C'ALL'                                                 
         MVC   H9+6(4),=C'WEEK'                                                 
         MVC   H10+6(4),=C'----'                                                
         CLI   PERTYPE,C'M'                                                     
         BNE   HOOK1B                                                           
         MVC   H9+6(5),=C'MONTH'                                                
         MVC   H10+6(5),=C'-----'                                               
         MVC   H1+40(7),=C'MONTHLY'                                             
         MVC   H2+40(7),=C'-------'                                             
         SPACE 1                                                                
HOOK1B   CLI   SPLTYPE,C'W'                                                     
         BE    HOOK2                                                            
         MVC   H1+40(7),=C'NETWORK'                                             
         MVC   H2+40(7),=C'-------'                                             
         MVC   H9+5(7),=C'NETWORK'                                              
         MVC   H10+5(7),=C'-------'                                             
         CLI   SPLTYPE,C'N'                                                     
         BE    HOOK2                                                            
         MVC   H1+40(7),=C'  BRAND'                                             
         MVC   H2+40(7),=C'  -----'                                             
         MVC   H9(12),=C' PRD TARGET '                                          
         MVC   H10(12),=C' --- ------ '                                         
         CLI   SPLTYPE+2,X'41'                                                  
         BL    XIT                                                              
*                                  R3 HAS A(LIST ENTRY)                         
         LA    R2,GROUPA                                                        
         ZIC   R5,GROUPA+12                                                     
         LA    R4,H6+35                                                         
         BAS   RE,GRPEDIT                                                       
         ZIC   R1,GROUPB+12                                                     
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         MVC   H5+35(39),H6+35                                                  
         MVC   H6+35(39),SPACES                                                 
         LA    R2,GROUPB                                                        
         AR    R5,R1                                                            
         BAS   RE,GRPEDIT                                                       
         B     XIT                                                              
         SPACE 1                                                                
HOOK2    XC    DUB,DUB             BUILD DEMO FOR DEMOCON                       
         MVI   DUB+1,C'R'          USE RATING                                   
         LA    R3,PRDLIST                                                       
         MVC   DUB+2(1),LISTTARG                                                
         GOTO1 DEMOCON,DMCB,(0,DUB),(7,WORK),(C'S',DBLOCK)                      
         GOTO1 CENTER,DMCB,WORK,7                                               
         MVC   H10+25(7),WORK                                                   
         MVC   H10+46(7),WORK                                                   
         MVC   H10+76(7),WORK                                                   
         B     XIT                                                              
         SPACE 2                                                                
GRPEDIT  NTR1                      ROUTINE TO EDIT GROUP EXPRESSION             
         USING LISTD,R3                                                         
         MVC   WORK,SPACES                                                      
         MVC   WORK(12),0(R2)                                                   
         MVC   DUB,LISTDIV                                                      
         LH    R1,DUB              (9990 FORMAT)                                
         SLL   R1,16                                                            
         SR    R6,R6               BUILD NUMBER IN R6                           
         SPACE 2                                                                
GRPEDIT2 SR    R0,R0                                                            
         SLDL  R0,4                SHIFT DIGIT INTO R0                          
         MH    R6,=H'10'                                                        
         AR    R6,R0                                                            
         BCT   R5,GRPEDIT2                                                      
         EDIT  (R6),(3,WORK+13),WRK=DMCB                                        
         MVC   WORK+17(24),13(R2)                                               
         GOTO1 SQUASHER,DMCB,WORK,40                                            
         MVC   0(39,R4),WORK                                                    
         CLI   DMCB+7,36                                                        
         BH    XIT                                                              
         GOTO1 CENTER,DMCB,(R4),36                                              
         B     XIT                                                              
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS FOR BUDGET COMPARISON                                     
         SPACE 3                                                                
BUDGD    DSECT                     COVERS ACCUMULATOR ENTRY                     
ACLINE   DS    0CL32                                                            
ACGDOL   DS    F                                                                
ACGPNTS  DS    F                                                                
ACEDOL   DS    F                                                                
ACEPNTS  DS    F                                                                
ACEHOME  DS    F                                                                
ACADOL   DS    F                                                                
ACAPNTS  DS    F                                                                
ACAHOME  DS    F                                                                
*                                                                               
*                                                                               
LISTD    DSECT                     DSECT FOR STRUCTURE OF PRDLIST               
LISTNO   DS    CL1                                                              
LISTDIV  DS    CL2                                                              
LISTPRD  DS    CL3                                                              
LISTTARG DS    CL1                                                              
LISTACNO DS    CL1                                                              
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE7D                                                       
*                                                                               
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
       ++INCLUDE NETDEMOT                                                       
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
**** ARGS FROM EDIT MODULE                                                      
DPFILT   DS    CL1                                                              
OPTIONS  DS    CL8                                                              
SAVMKT   DS    H                                                                
*                                                                               
*                                                                               
       ++INCLUDE NENETGOALD                                                     
*                                                                               
PRDLIST  DS    800CL1              PROD LIST AREA FOR NETGOAL                   
         DS    800CL1              PROD LIST AREA FOR NETGOAL                   
*                                                                               
** NORMAL W/S                                                                   
MAXWKS   EQU   16                  MAXIMUM NUMBER OF WEEKS                      
PERTYPE  DS    CL3                 PERIOD TYPE                                  
NUMWKS   DS    F                   NUMBER OF WEEKS                              
WKLIST   DS    CL(4*MAXWKS)        WEEK LIST                                    
NETLIST  DS    CL140               NET LIST                                     
NNETS    DS    F                                                                
CPPLIST  DS    3F                                                               
DIVACS   DS    CL32                                                             
ESTACS   DS    CL32                                                             
LASTDIV  DS    CL2                                                              
GROUPA   DS    CL37                                                             
GROUPB   DS    CL37                                                             
DEMOCON  DS    A                                                                
         DS    0D                                                               
BUDGAREA DS    3200CL1             AREA FOR STORING TOTALS                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003NEMED67   05/01/02'                                      
         END                                                                    
