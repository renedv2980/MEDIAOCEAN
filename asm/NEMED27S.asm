*          DATA SET NEMED27S   AT LEVEL 238 AS OF 05/01/02                      
*PHASE T31E27A,+0                                                               
         TITLE 'T31E27 - NTI PKTPIECE-UNIT SEED REPORT'                         
         PRINT NOGEN                                                            
************************************************************                    
* PKTPIECE-UNIT SEED REPORT                                *                    
*                                                          *                    
* THIS PROGRAM SEEDS UNITS WITH POCKETPIECE NTI CODES      *                    
* SYNDICATED BUYS ARE EXCLUDED                             *                    
*                                                          *                    
* GLOBALS: RA - A(TWA)                                     *                    
*          RB - BASE REG                                   *                    
*          RC - GEND                                       *                    
*          R9 - NETWORK SYSTEM DSECT                       *                    
*          R8 - A(DSECT FOR SPOOL PRINTING)                *                    
*          R7 - WORKING STORAGE                            *                    
*                                                          *                    
************************************************************                    
T31E27   CSECT                                                                  
         NMOD1 0,**SDPR**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)                                                      
         LA    RA,1(RA)                                                         
         USING T31E27,RB,RA                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
         L     R7,ANETWS2                                                       
         USING WORKD,R7                                                         
         SPACE                                                                  
         ST    R2,RELO                                                          
*                                                                               
         L     R1,=V(MATCHPNM)                                                  
         A     R1,RELO                                                          
         ST    R1,AMATCH                                                        
         L     R1,=A(NETBUFF)                                                   
         ST    R1,NBCNVNTI                                                      
                                                                                
* - NEED TO HAVE COPIES WRITTEN TO RECOVERY FILE                                
         L     R2,ATWA                                                          
         USING T31EFFD,R2                                                       
         ICM   R1,15,TWAMASTC                                                   
         BZ    ENDMST                                                           
         USING MCBLOCK,R1                                                       
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE RECORDS                      
         DROP  R1,R2                                                            
ENDMST   DS    0H                                                               
*                                                                               
         LA    R1,DBLOCKWS         SET A OF DBLOCK                              
         ST    R1,ADBLOCK                                                       
         SPACE                                                                  
         MVC   DMCB+4(4),=X'D9000A17'          GET VNETWEEK                     
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VNETWEEK,DMCB                                                    
         SPACE                                                                  
         MVC   DMCB+4(4),=X'D9000A26'          GET VDEFINE                      
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEFINE,DMCB                                                     
         SPACE                                                                  
         L     RE,ACOMFACS                     GET VDEMAND                      
         USING COMFACSD,RE                                                      
         MVC   VDEMAND,CDEMAND                                                  
         SPACE                                                                  
         ZAP   RECSTSRT,=P'0'                  ZAP COUNTERS                     
         ZAP   RECSFSRT,=P'0'                  (NOT PRINTED/FOR DEBUG)          
         SPACE                                                                  
         MVI   UPDATFLG,C'N'                                                    
         L     R3,ATWA                                                          
         USING T31EFFD,R3                                                       
         CLI   SPLTST,C'Y'                     TEST RUN OPTION                  
         BE    *+8                                                              
         MVI   UPDATFLG,C'Y'                                                    
         SPACE                                                                  
         MVI   SEEDIT,C'Y'                                                      
         SPACE                                                                  
         MVI   PRDBKFLG,0                      TEST IF BREAK ON PRD CHA         
         CLC   =C'POL',SPLPRO                  REQUEST PRD FIELD=POL            
         BE    MAINLINE                        THEN NO BREAK.                   
         MVI   PRDBKFLG,C'Y'                                                    
         DROP R3                                                                
         SPACE 3                                                                
MAINLINE DS    0H                                                               
         BAS   RE,PASTOSRT                                                      
         BAS   RE,FROMSRT                                                       
EXIT     XIT1                                                                   
         EJECT                                                                  
         SPACE                                                                  
***********************************************                                 
*  PASTOSRT ROUTINE                           *                                 
*                                             *                                 
*  ROUTINE WILL GET UNIT RECS THROUGH NETIO   *                                 
*  FILL IN SORTREC  USING NETBLOCK INFO       *                                 
*  PASS THESE RECS TO SORT                    *                                 
*                                             *                                 
*  INPUT: NETIO GETS UNIT REC                 *                                 
*               FILLS IN NETBLOCK             *                                 
*                                             *                                 
*  OUTPUT: RECS TO SORTER                     *                                 
***********************************************                                 
         SPACE                                                                  
PASTOSRT NTR1                                                                   
         SPACE                                                                  
* INITIALIZE SORTREC *                                                          
         DS    0H                                                               
         XC    DMCB(12),DMCB                                                    
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         B     PAS03                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,59,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=144'                                   
         SPACE 2                                                                
PAS03    MVI   NBDATA,C'U'                      SELECT UNIT RECORDS             
         MVI   NBSELUOP,C'A'                    ACTUAL SCHEDULE                 
         SPACE                                                                  
PAS05    NETGO NSNETIO,DMCB,NETBLOCK            GET  RECORD                     
         CLI   NBMODE,NBVALCLI     TEST IF CHANGE OF CLIENT                     
         BNE   PAS07                                                            
         L     R2,NBAIO            IF YES                                       
         USING CLTHDR,R2           GET CLIENT HEADER REC                        
         MVC   CLTNMSV,CNAME       SAVE CLIENT NAME                             
         SPACE                                                                  
PAS07    CLI   NBMODE,NBVALPRD     TEST IF PRODUCT CHANGE                       
         BNE   PAS08                                                            
         L     R2,NBAIO                                                         
         USING PRDHDR,R2                                                        
         MVC   PRDCDSV,PKEYPRD                                                  
         MVC   PRDNMSV,PNAME                                                    
         SPACE                                                                  
PAS08    CLI   NBMODE,NBREQLST                                                  
         BE    PASX                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BNE   PAS05                                                            
         CLC   =C'ABC',NBACTNET    TEST NETWRK                                  
         BE    PAS10                                                            
         CLC   =C'NBC',NBACTNET                                                 
         BE    PAS10                                                            
         CLC   =C'CBS',NBACTNET                                                 
         BE    PAS10                                                            
         CLC   =C'FOX ',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'FBC ',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'WBN ',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'WBT ',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'WAR ',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'WBTV',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'UPN ',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'UPNN',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'PNW ',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'PAX ',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'TWBN',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'PAR ',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'TEL ',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'UNI ',NBACTNET                                                
         BE    PAS10                                                            
         CLC   =C'UNIV',NBACTNET                                                
         BE    PAS10                                                            
         B     PAS05               REJECT SYNDICATED NETS                       
         EJECT                                                                  
* FILL IN SORTREC *                                                             
PAS10    DS    0H                               *=KEY FIELD                     
         LA    R2,SORTREC                                                       
         XC    0(SRTRECLN,R2),0(R2)                                             
         MVC   SRTCLT,NBCLICOD                       *                          
         MVC   SRTPRD,NBPRD                          *                          
         CLI   PRDBKFLG,C'Y'                                                    
         BE    *+8                                                              
         MVI   SRTPRD,0                                                         
         MVC   SRTEST,NBACTEST                       *                          
         MVC   SRTNET,NBACTNET                       *                          
         MVC   SRTDPT,NBACTDP                        *                          
         MVC   SRTPKG,NBPACK                         *                          
         MVC   SRTDTE(2),NBACTDAT                    *                          
         MVC   SRTDTE+2(1),NBACTSUB                  *                          
         MVC   SRTDAY,NBDAYNAM                       * DAY ALPHA                
         GOTO1 UNTIME,DMCB,NBTIME,SRTTIME            *                          
         MVC   SRTPRGCD,NBACTPRG                     *                          
         CLC   NBNTI(2),=2X'00'                                                 
         BE    PAS13                                                            
         SR    R5,R5                                                            
         ICM   R5,3,NBNTI                             *                         
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SRTNTI,DUB                                                       
PAS13    MVC   SRTPRGNM,NBPROGNM                     *                          
         MVC   SRTXNTI,NBNTI            BINARY NTI                              
         MVC   SRTXDAY,NBDAY            DAY BITS                                
         MVC   SRTXTIM,NBTIME           MILITARY START-END TIME                 
         MVC   SRTACTDT,NBACTDAT                                                
         MVC   SRTCLTNM,CLTNMSV                                                 
         MVC   SRTPRDCD,PRDCDSV                                                 
         MVC   SRTPRDNM,PRDNMSV                                                 
         MVC   SRTUNTK,NBKEY            UNT REC KEY                             
         SPACE                                                                  
         CLI   CORRFLG,C'Y'        CORRECTIONS ONLY                             
         BNE   PAS15                                                            
         BAS   RE,CHKCORR                                                       
         CLI   CORRSW,C'Y'                                                      
         BNE   PAS05                                                            
         MVI   CORRSW,0                                                         
         SPACE                                                                  
PAS15    GOTO1 SORTER,DMCB,=C'PUT',(R2)                                         
         SPACE                                                                  
         AP    RECSTSRT,=P'1'                                                   
         B     PAS05               GET NEXT REC                                 
         SPACE                                                                  
PASX     B     EXIT                                                             
         EJECT                                                                  
*********************************                                               
* ROUTINE CHECKS CORRECTIONS/ADDITIONS                                          
*                                                                               
CHKCORR  NTR1                                                                   
         L     R6,ADBLOCK                                                       
         USING DBLOCK,R6                                                        
         BAS   RE,FILLDBLK                                                      
         SPACE                                                                  
         MVI   DBFUNCT,DBGETNTI                                                 
         MVC   DBSELDAY,SRTXDAY                                                 
         CLI   DBSELDAY,X'7C'      IS IT M-F                                    
         BNE   CC2                                                              
         MVC   DBSEL1WK,SRTACTDT                                                
         MVI   DBBEST,C'L'                                                      
CC2      MVC   DBSELTIM,SRTXTIM                                                 
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'N'                                                    
         MVC   DBSELSTA,SRTNET                                                  
         BAS   RE,CNVNTI                                                        
         CLC   =C'FBC',DBSELSTA                                                 
         BNE   *+10                                                             
         MVC   DBSELSTA(3),=C'FOX'                                              
         MVC   DBSELSTA+3(2),=C' T'                                             
         GOTO1 VDEMAND,DMCB,ADBLOCK,CCRTN                                       
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
CCRTN    NTR1                                                                   
         XC    CORRTEMP,CORRTEMP                                                
         XC    CORRNAME,CORRNAME                                                
         L     RF,VDEFINE                                                       
         GOTO1 (RF),DMCB,=C'CORR',ADBLOCK,CORRTEMP                              
         GOTO1 (RF),DMCB,=C'PROGRAM',ADBLOCK,CORRNAME                           
         CLI   CORRTEMP,X'40'                                                   
         BE    EXIT                                                             
         MVI   CORRSW,C'Y'                                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         SPACE                                                                  
*************************************************                               
*                                               *                               
* FROM SORTER ROUTINE                           *                               
*  GET UNIT RECS FROM SORTER                    *                               
*  IF UNTREC HAS NTI CODE PASS THIS TO DEMAND   *                               
*     ELSE PASS DAY/TIME                        *                               
*  NTIMTCH FLAG SET:                            *                               
*   =Y  UNT REC HAD NTI/PKTPIECE  FOUND         *                               
*   =0  UNT REC HAD NO NTI/DAY-TIME READ FOUND  *                               
*       PKTPIECE                                *                               
*   =E  ERROR/ DAY-TIME READ NO PKTPIECE FOUND  *                               
*   =N  UNT REC HAD NTI/NO PKTPIECE FOUND/2ND   *                               
*       READ(DAY/TIME) FOUND PKTPIECE           *                               
*   =X  NTI CODE FROM BUY PROGRAM/LEAVE AS IS   *                               
*                                               *                               
*                                               *                               
*  INPUT: UNIT REC FROM SORTER                  *                               
*         POCKET PIECE DATA FROM DEMAND/DEFINE  *                               
*                                               *                               
*  OUTPUT: SEED UNIT RECS WITH NTI CODE         *                               
*          REPORT ALL UNIT RECS/SEEDED OR NOT   *                               
*************************************************                               
         SPACE                                                                  
FROMSRT  NTR1                                                                   
         MVI   FRST,C'Y'                                                        
FRM10    MVI   NTIMTCH,0                                                        
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R5,4(R1)            TEST E-O-F SORTER                            
         LTR   R5,R5                                                            
         BNZ   FRM12                                                            
         CLI   FRST,C'Y'                                                        
         BNE   FRMX                                                             
         MVC   P(29),=C'NO RECORDS RECEIVED FROM SORT'                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     FRMX                                                             
         SPACE                                                                  
FRM12    AP    RECSFSRT,=P'1'                                                   
         MVC   SORTREC(SRTRECLN),0(R5)                                          
         CLI   FRST,C'Y'                                                        
         BNE   FRM13                                                            
         MVI   FRST,C'N'                                                        
         MVC   OSRTCLT,SRTCLT                                                   
         MVC   OSRTNET,SRTNET                                                   
         MVC   OSRTEST,SRTEST                                                   
         MVC   OSRTPKG,SRTPKG                                                   
         MVC   OSRTPRD,SRTPRD                                                   
         SPACE                                                                  
FRM13    CLI   RESEED,C'Y'     IS RESEED OPTION ON                              
         BE    FRM30               YES/IGNORE NTI CODE ON UNIT                  
         CLI   CORRFLG,C'Y'        IS CORRECT OPTION ON                         
         BE    FRM30                   YES/IGNORE NTI CODE ON UNIT              
*                                                                               
         CLI   SRTNTI,0            NO/IS NTI CODE PRESENT                       
         BE    FRM30                     NO/DO DAY-TIME READ                    
         B     FRM20                     YES/DO NTI CODE READ                   
         EJECT                                                                  
         SPACE                                                                  
***************************                                                     
* UNIT REC HAS NTI NUMBER *                                                     
* PASS DEDBLOCK NTI CODE  *                                                     
*  IF PKTPIECE NOT FOUND  *                                                     
*  SET NTIMTCH=N AND      *                                                     
*   PASS TO DAY/TIME READ *                                                     
***************************                                                     
FRM20    DS    0H                                                               
         L     R6,ADBLOCK                                                       
         USING DBLOCK,R6                                                        
         SPACE                                                                  
         BAS   RE,FILLDBLK                                                      
         SPACE                                                                  
         MVI   DBFUNCT,DBGETNTI                                                 
         MVC   DBSELSTA,SRTNET                                                  
         BAS   RE,CNVNTI                                                        
         CLC   =C'FBC',DBSELSTA                                                 
         BNE   *+10                                                             
         MVC   DBSELSTA(3),=C'FOX'                                              
         MVC   DBSELSTA+3(2),=C' T'                                             
         MVC   DBSELPRG,SRTXNTI                                                 
         SPACE                                                                  
         LA    RE,DEFINESV                                                      
         L     RF,=F'16000'                                                     
         XCEF                                                                   
         XC    DEFRECS,DEFRECS                                                  
         LA    R3,DEFINESV                                                      
         USING DEFINED,R3                                                       
         ST    R3,ADFN             SAVE SINCE DFNRTN DOES NOT BUMP              
         SPACE                                                                  
         GOTO1 VDEMAND,DMCB,ADBLOCK,DFNRTN                                      
         OC    DBDIVSOR,DBDIVSOR   TEST REC FOUND                               
         BNZ   FRM22                                                            
         MVI   NTIMTCH,C'N'      NOT FOUND/SET NTIMATCH FLAG TO NO              
         B     FRM30             AND DO DAY/TIME                                
FRM22    CLC   SRTNTI,DFNNTI       TEST NTI MATCH                               
         BE    *+6                                                              
         DC    H'0'                                                             
*        CLC   SRTDAY,DFNDAY+2     TEST ALPHA DAY                               
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        CLC  SRTXTIM,DFNTIME+2    TEST MILITARY TIME                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   NTIMTCH,C'Y'        YES/SET NTIMATCH FLAG TO YES                 
         BAS   RE,WRITREC          WRITE REPORT                                 
         B     FRM10               GET NEXT REC                                 
         EJECT                                                                  
         SPACE                                                                  
******************************                                                  
* UNIT REC HAS NO NTI NUMBER *                                                  
* PASS DEDBLOCK DAY/TIME     *                                                  
*                            *                                                  
******************************                                                  
FRM30    DS    0H                                                               
         L     R6,ADBLOCK                                                       
         USING DBLOCK,R6                                                        
         SPACE                                                                  
         BAS   RE,FILLDBLK                                                      
         SPACE                                                                  
         MVI   DBFUNCT,DBGETNTI                                                 
         MVC   DBSELDAY,SRTXDAY                                                 
         CLI   DBSELDAY,X'7C'      IS IT M-F                                    
         BNE   FRM30C                                                           
         MVC   DBSEL1WK,SRTACTDT                                                
         MVI   DBBEST,C'L'                                                      
FRM30C   MVC   DBSELTIM,SRTXTIM                                                 
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'N'                                                    
         MVC   DBSELSTA,SRTNET                                                  
         BAS   RE,CNVNTI                                                        
         CLC   =C'FBC',DBSELSTA                                                 
         BNE   *+10                                                             
         MVC   DBSELSTA(3),=C'FOX'                                              
         MVC   DBSELSTA+3(2),=C' T'                                             
         SPACE                                                                  
         LA    RE,DEFINESV                                                      
         L     RF,=F'16000'                                                     
         XCEF                                                                   
         XC    DEFRECS,DEFRECS                                                  
         LA    R3,DEFINESV                                                      
         ST    R3,ADFN             SAVE SINCE DFNRTN DOES NOT BUMP              
         SPACE                                                                  
         GOTO1 VDEMAND,DMCB,ADBLOCK,DFNRTN                                      
         OC    DBDIVSOR,DBDIVSOR   TEST ANY RECS FOUND                          
         BNZ   FRM31                                                            
         MVI   NTIMTCH,C'E'        SET E(RROR)/NO REC FOUND                     
*        CLI   DBERROR,X'10'       TEST NO REC FOUND                            
*        BE    FRM32                                                            
         B     FRM32               LETS NOT DUMP 5/17/90                        
FRM31    CLI   DBERROR,X'80'       TEST DFNRTN HIT E-O-F                        
         BE    *+6                                                              
         DC    H'0'                                                             
FRM32    BAS   RE,WRITREC          WRITE REPORT                                 
         CLI   NTIMTCH,C'N'        TEST 2ND READ ON NO NTI MATCH                
         BE    FRM33               YES/SKIP SEEDING                             
         CLI   NTIMTCH,C'E'        TEST E(RROR)/NO REC FOUND                    
         BE    FRM33               YES/SKIP SEEDING                             
         CLI   SEEDIT,C'Y'         ARE MULT NTIS EQUAL                          
         BNE   FRM33               NO/SKIP SEEDING                              
         BAS   RE,SEEDUNT          SEED UNIT REC                                
FRM33    DS    0H                                                               
         MVI   SEEDIT,C'Y'                                                      
         B     FRM10               GET NEXT REC                                 
         SPACE 2                                                                
FRMX     GOTO1 SORTER,DMCB,=C'END'                                              
         B     EXIT                (XIT1)                                       
*                                                                               
         USING DBLOCK,R6                                                        
CNVNTI   DS    0H                  GET NTI STA FOR NETWORKS                     
         L     R5,NBCNVNTI                                                      
         LTR   R5,R5                                                            
         BZ    CNVX                                                             
         LA    R5,3(R5)            SKIP OVER AGY DATA                           
CNV5     CLC   SRTNET,0(R5)                                                     
         BNE   *+14                                                             
         MVC   DBSELSTA,5(R5)                                                   
         B     CNVX                                                             
         LA    R5,10(R5)                                                        
         CLI   0(R5),0                                                          
         BNE   CNV5                                                             
CNVX     BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
********************************                                                
*  SET UP DEDBLOCK             *                                                
*  WITH STANDARD PARAMS        *                                                
*  FOR ALL CALLS               *                                                
*                              *                                                
********************************                                                
         SPACE                                                                  
FILLDBLK NTR1                                                                   
         L     R6,ADBLOCK                                                       
         USING DBLOCK,R6                                                        
         XC    DBLOCK,DBLOCK          * CLEAR DBLOCK                            
         MVC   DBCOMFCS,ACOMFACS      * PASS ACOMFACS                           
         MVC   DBFILE,=C'NTI'        * SET DBFILE                               
         MVC   DBAREC,ANETWS1         * PASS I/O AREA                           
         MVI   DBSELDUR,X'FF'      RETURN ALL TIME DURATION                     
         CLI   ASCRIBD,C'Y'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'A'       ASCRIBED DATA                                 
         MVC   DBSELAGY,NBSELAGY                                                
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,SRTACTDT),(0,EBCDAT)                              
         GOTO1 VNETWEEK,DMCB,EBCDAT,GETDAY,ADDAY                                
         MVC   DEMOWK,DMCB+8                                                    
*        GOTO1 DATCON,DMCB,(2,SRTACTDT),(3,DUB)                                 
         MVC   DEMOYR,DMCB+4                                                    
*        CLI   DEMOWK,1            IF WEEK=1 AND MONTH=12                       
*        BNE   FLD5                THEN ADD 1 TO YEAR                           
*        CLI   DUB+1,12              SINCE 12/30/81 IS IN FIRST WEEK            
*        BNE   FLD5                  OF 82.                                     
*        ZIC   R1,DEMOYR                                                        
*        LA    R1,1(R1)                                                         
*        STC   R1,DEMOYR                                                        
FLD5     MVC   DBSELBK,DEMOYR          * SET DBSELBK                            
         SPACE                                                                  
FLDX     B     EXIT                (XIT1)                                       
         DROP R6                                                                
         EJECT                                                                  
*************************************                                           
* THIS ROUTINE IS CALLED BY DEMAND  *                                           
* TO EXTRACT REQUESTED DATA FROM    *                                           
* DEMO RECORDS                      *                                           
*  NOTE ADFN TO SAVE BUMPED ADDR    *                                           
*  SINCE DEMAND SAVES REGS BEFORE   *                                           
*  CALLING HOOK BUMPS IN HOOK ARE   *                                           
*  ALWAYS RETURNED TO PREHOOK REGS  *                                           
*                                   *                                           
* INPUT: DEMAND CALLS THE ROUTINE   *                                           
*   AND GIVES UP CONTROL WHEN NO    *                                           
*   MORE RECORDS                    *                                           
* OUTPUT: FILLS IN DEFINESV         *                                           
*************************************                                           
         DS    F                                                                
DFNRTN   DS    0H                                                               
         ST    RE,DFNRTN-4                                                      
         USING DEFINED,R3                                                       
         L     R3,ADFN                                                          
         L     RF,VDEFINE                                                       
         GOTO1 (RF),DMCB,=C'PROGRAM',ADBLOCK,DFNPRG                             
         GOTO1 (RF),DMCB,=C'DAY',ADBLOCK,DFNDAY                                 
         GOTO1 (RF),DMCB,=C'TIME',ADBLOCK,DFNTIME                               
         GOTO1 (RF),DMCB,=C'NTI',ADBLOCK,DFNNTI                                 
***      GOTO1 (RF),DMCB,=C'NTI',ADBLOCK,DFNNTI                                 
         LA    R3,DFNLENE(R3)           INCREMENT DEFINESV                      
         ST    R3,ADFN                                                          
         L     R1,DEFRECS               INCREMENT NUM OF RECS                   
         LA    R1,1(R1)                                                         
         CH    R1,=H'336'          336= MAX RECS IN DEFINESV(48X7)              
         BNH   *+6                      (48 HALF HRS X 7 DAYS)                  
         DC    H'0'                                                             
         ST    R1,DEFRECS                                                       
         L     RE,DFNRTN-4                                                      
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
         SPACE                                                                  
******************************                                                  
*                            *                                                  
*  WRITE REPORT              *                                                  
*                            *                                                  
******************************                                                  
WRITREC  NTR1                                                                   
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         LA    R3,DEFINESV                                                      
         USING DEFINED,R3                                                       
         MVI   SEEDFLG,0                                                        
         SPACE                                                                  
         BAS   RE,FILLPLN                                                       
         SPACE                                                                  
WRT02    CLI   NTIMTCH,C'E'        TEST ERROR/NO REC FOUND                      
         BNE   WRT05                                                            
         XC    PKPRGNM,PKPRGNM                                                  
         XC    PKDAY,PKDAY                                                      
         XC    PKTIME,PKTIME                                                    
         XC    PKNTI,PKNTI                                                      
         MVC   PKPRGNM(38),=C'  *** NO PKT PIECE MATCH AVAILABLE ***'           
         MVI   ENDWRT,C'Y'                                                      
         B     WRT50                                                            
         SPACE                                                                  
WRT05    CLI   NTIMTCH,C'Y'        TEST UNT NTI=PKTPC NTI                       
         BNE   WRT10                                                            
         MVC   PREMRKS,=CL19'UNIT ALREADY SEEDED'                               
         MVI   ENDWRT,C'Y'                                                      
         B     WRT50                                                            
         SPACE                                                                  
WRT10    CLI   NTIMTCH,C'N'        TEST UNT NTI NOT = PKTPC NTI                 
         BNE   WRT20                                                            
         MVC   PREMRKS,=CL19'CAN''T FIND'                                       
         MVC   PREMRKS+12(5),SRTNTI                                             
         MVI   ENDWRT,C'Y'                                                      
         B     WRT50                                                            
         SPACE                                                                  
WRT20    CLI   NTIMTCH,0           TEST UNT HAS NO NTI                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R3                                                            
         LA    RE,DFNLENE(RE)                                                   
         CLI   0(RE),0                     ANY MORE RECS IN DEFINESV            
         BNE   WRT25                       YES/SO GOTO MULT SEED                
*        GOTO1 AMATCH,DMCB,(RC)    TEST UNIT/PKTPIC PRGNM MATCH                 
*        BE    WRT23                                                            
*        MVI   ENDWRT,C'Y'                                                      
*        B     WRT50                                                            
WRT23    MVC   PREMRKS,=CL19'SEEDED WITH'          SINGLE SEED                  
         MVC   PREMRKS+13(5),PKNTI                                              
         MVC   NTISEED,PKNTI                                                    
         MVI   ENDWRT,C'Y'                                                      
         B     WRT50                                                            
         EJECT                                                                  
         SPACE                                                                  
*    MULTIPLE SEED CONDITION                                                    
WRT25    DS    0H                                                               
*        GOTO1 AMATCH,DMCB,(RC)    TEST UNIT/PKTPIC PROGNM MATCH                
*        BE    WRT25A                                                           
*        MVI   SEEDFLG,1           NO MATCH/FUDGE SEED FLAG                     
*        MVI   ENDWRT,C'N'                  SET MULT SEED COND SWITCH           
*        B     WRT50                        AND GO TO WRITE REPORT              
*                                                                               
WRT25A   MVC   PREMRKS,=CL19'MULTIPLE NTI CODES'    YES/MULT SEED               
         CLI   MLTSDFLG,C'Y'          TEST MULT SEED OPTION                     
         BE    WRT27                                                            
         BAS   RE,TSTMULT          CHK MULT NTI CODES =                         
         LTR   R1,R1               SET TO ZERO IF =                             
         BZ    WRT27                                                            
         MVI   SEEDIT,C'N'            SKIP SEEDING                              
         MVI   SEEDFLG,1                                                        
         MVI   ENDWRT,C'N'         SET MULT SEED COND SWITCH                    
         B     WRT50                                                            
WRT27    MVC   NTISEED,PKNTI                                                    
*        CLC   PKNTI,DFNNTI+DFNLENE  TEST IF NTI CODES OF MULT SEED             
*        BE    WRT23                 AND NEXT PKTPIECE =/YES SKIP MULT          
         MVI   ENDWRT,C'N'           (NOT USING THE ABOVE/SHOW MULTS)           
         B     WRT50                                                            
WRT30    BAS   RE,FILLPLN                                                       
         CLI   SEEDFLG,1                                                        
         BE    WRT50                                                            
         MVI   SEEDFLG,1                                                        
WRT40    MVC   PREMRKS,=CL19'SEEDED WITH'                                       
         MVC   PREMRKS+13(5),NTISEED                                            
         SPACE                                                                  
*    WRITE TO THE REPORT                                                        
WRT50    BAS   RE,BRKCHK                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   OSRTCLT,SRTCLT      SET OLD SORT KEY FOR FORCEHED                
         MVC   OSRTNET,SRTNET                                                   
         MVC   OSRTEST,SRTEST                                                   
         MVC   OSRTPKG,SRTPKG                                                   
         MVC   OSRTPRD,SRTPRD                                                   
         CLI   ENDWRT,C'Y'         EXIT IF ENDWRT=YES                           
         BE    WRTX                                                             
         LA    R3,DFNLENE(R3)      OR IF END OF DEFINESV                        
         CLI   0(R3),0                                                          
         BNE   WRT30                                                            
WRTX     MVI   ENDWRT,0                                                         
         B     EXIT                                                             
         EJECT                                                                  
*****************************************************                           
*  THIS RTN MUST COME DIRECTLY AFTER WRITREC SINCE  *                           
*  IT USES REGS(R3) SET BY WRITREC.                 *                           
* ROUTINE TO MOVE DATA FROM SORTREC (UNT REC DATA)  *                           
*  AND  FROM DEFINESV (DEMO REC DATA) TO P LINE.    *                           
*  (CALLED ONLY FROM WRITE ROUTINE)                 *                           
*****************************************************                           
         SPACE                                                                  
FILLPLN  NTR1                                                                   
         CLI   ENDWRT,C'N'         IF ENDWRT=N/MEANS MULT SEED                  
         BE    FLP5                SO SKIP MULT UNT REC PRINT                   
         GOTO1 DATCON,DMCB,(2,SRTDTE),(4,PUDTE)     DATE-                       
         MVI   PUDTE+5,C'-'                                                     
         LA    R4,PUDTE+6                                                       
         ZIC   R5,SRTDTE+2                                                      
         EDIT  (R5),(2,0(R4)),ALIGN=LEFT          SUBLINE                       
         MVC   PUDAY,SRTDAY                                                     
         MVC   PUTIME,SRTTIME                                                   
         MVC   PUPRGCD,SRTPRGCD                                                 
         MVC   PUNTI,SRTNTI                                                     
         MVC   PUPRGNM,SRTPRGNM                                                 
         SPACE                                                                  
FLP5     MVC   PKPRGNM,DFNPRG                                                   
         OC    PKPRGNM,SPACES                                                   
         MVC   PKDAY,DFNDAY+2                                                   
         GOTO1 UNTIME,DMCB,DFNTIME+2,PKTIME                                     
         MVC   PKNTI,DFNNTI                                                     
         SPACE                                                                  
         B     EXIT                                                             
         SPACE 2                                                                
*************************************************************                   
* KEEP IMMEDIATELY AFTER FILLPLN FOR IT USES SAME REGS      *                   
* TEST IF MULT NTI CODES ARE EQUAL (ONLY CALLED FROM WRT25) *                   
* 1ST NTI IS TESTED AGGAINST NTIS OF DFNSV  IF =, SET R1=0  *                   
*************************************************************                   
TSTMULT  DS    0H                                                               
         LA    R1,DFNNTI                                                        
TM05     CLC   PKNTI,0(R1)        ARE MULT NTI CODES =                          
         BNE   TMXX                                                             
         LA    R1,DFNLENE(R1)                                                   
         CLI   0(R1),0                                                          
         BNE   TM05                                                             
         SR    R1,R1             EOF REACHED/SO EQUAL/SET R1 TO ZERO            
TMXX     BR    RE                                                               
         DROP  R2                                                               
         DROP  R3                                                               
         EJECT                                                                  
*************************************                                           
* ROUTINE COMPARES OLD VS NEW SORT  *                                           
*   AND BREAKS ON CHANGE            *                                           
*                                   *                                           
*  IF PRD HAS CHANGED AND PRDBRK=Y  *                                           
*  GOTO GETPRD                      *                                           
*  TO GET NEW PRDCODE/PRDNAME       *                                           
*                                   *                                           
*  ONLY CALLED FROM WRT50           *                                           
*************************************                                           
         SPACE                                                                  
BRKCHK   NTR1                                                                   
         CLC   OSRTCLT,SRTCLT      CLIENT                                       
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLC   OSRTEST,SRTEST      ESTIMATE                                     
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLC   OSRTNET,SRTNET      NETWORK                                      
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLC   OSRTPKG,SRTPKG      PACKAGE                                      
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLC   OSRTPRD,SRTPRD      PRODUCT                                      
         BE    BRK5                                                             
         CLI   PRDBKFLG,C'Y'                                                    
         BNE   BRK5                                                             
         BAS   RE,GETPRD                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     BKX                                                              
BRK5     CLI   FORCEHED,C'Y'       TEST IF SORT BRK ON OTHER THAN PRD           
         BNE   BKX                                                              
         CLI   PRDBKFLG,C'Y'            IF PRDBKFLG=Y                           
         BNE   BKX                                                              
         BAS   RE,GETPRD                THEN GET PRD CODE/NAME                  
BKX      B     EXIT                                                             
         EJECT                                                                  
**************************************************                              
* ROUTINE READS CLIENT HEADER AND PRODUCT HEADER *                              
*   TO GET PRODCODE/PRODNAME                     *                              
*                                                *                              
* INPUT   SORTREC HAS UNIT REC KEY               *                              
**************************************************                              
GETPRD   NTR1                                                                   
         SPACE                                                                  
* READ CLIENT HEADER                                                            
         LA    R2,SRTUNTK          SORTREC HAS UNTKEY                           
         USING NUKEY,R2                                                         
         LA    R3,KEY                                                           
         USING CKEY,R3                                                          
         XC    KEY,KEY                                                          
         MVC   CKEYAM,NUKAM                                                     
         MVC   CKEYCLT,NUKCLT                                                   
         NETGO NVSETSPT,DMCB       SET TO READ SPOT FILE                        
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R3,NBAIO                                                         
         USING CLTHDR,R3                                                        
         LA    R2,CLIST                                                         
         OC    SRTPRD,SRTPRD       TEST IF PRD UNALLOCATED                      
         BNZ   GP10                                                             
         XC    SRTPRDCD,SRTPRDCD                                                
         MVC   SRTPRDNM,=CL20'PRODUCT UNALLOCATED'                              
         B     GPX                                                              
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                SET TO UNDEFINED                             
         XC    SRTPRD,SRTPRD                                                    
         MVC   SRTPRDNM,=CL20'PRODUCT UNDEFINED'                                
         B     GPX                                                              
GP12     CLC   3(1,R2),SRTPRD                                                   
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   SRTPRDCD,0(R2)      SET 3 CHAR PRINTABLE PRD CODE                
         SPACE                                                                  
* READ PRODUCT HEADER                                                           
         LA    R2,SRTUNTK                                                       
         USING NUKEY,R2                                                         
         LA    R3,KEY                                                           
         USING PKEY,R3                                                          
         MVC   PKEYAM,NUKAM                                                     
         MVC   PKEYCLT,NUKCLT                                                   
         MVC   PKEYPRD,SRTPRDCD                                                 
         NETGO NVSETSPT,DMCB                                                    
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R3,NBAIO                                                         
         USING PRDHDR,R3                                                        
         MVC   SRTPRDNM,PNAME                                                   
         SPACE                                                                  
GPX      NETGO NVSETUNT,DMCB       RESET TO UNIT FILE                           
         XC    FILENAME,FILENAME   CLEAR FILENAME                               
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
********************************************                                    
*                                          *                                    
*  SEED UNIT REC WITH PKTPIECE NTI CODE    *                                    
********************************************                                    
SEEDUNT  NTR1                                                                   
         MVI   NBNOWRIT,C'N'                                                    
         MVI   NBUPUNIT,C'N'                                                    
         CLI   UPDATFLG,C'Y'       TEST RUN OPTION                              
         BNE   SEEDX                                                            
         SPACE                                                                  
         MVI   NBUPUNIT,C'Y'       SET NETIO PUTREC SWITCH                      
         MVI   NBNOWRIT,C'Y'       SET NETIO WRITE SWITCH                       
         MVI   NBFUNCT,NBFGET      SET NETIO HAVE KEY/GET REC SWITCH            
         MVC   NBKEY,SRTUNTK       MOVE IN KEY FROM SORTREC                     
         LA    R2,NETIOHK          SET NETIO HOOK FOR PUTREC                    
         ST    R2,NBHOOK                                                        
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         B     SEEDX                                                            
         SPACE                                                                  
NETIOHK  NTR1                                                                   
         L     R6,NBAIO                                                         
         USING NUMAINEL,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         PACK  DUB,NTISEED                                                      
         CVB   R2,DUB                                                           
         STCM  R2,3,NUNTI                                                       
         MVI   NUPOSTDT,C'D'                                                    
         CLI   ASCRIBD,C'Y'                                                     
         BNE   *+8                                                              
         MVI   NUPOSTDT,C'A'                                                    
*                                                                               
         L     R6,NBAIO                                                         
         USING NUSDRD,R6                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   NET5                                                             
         OI    NUSDST3,X'20'       SEED SEEDED NTI                              
         B     SEEDX                                                            
NET5     L     R6,NBAIO            ELEM DOES NOT EXIST/ADD IT                   
         DROP  R6                                                               
         LA    R4,ELEM                                                          
         USING NUSDRD,R4                                                        
         XC    ELEM,ELEM                                                        
         MVC   NUSDREL,ELCODE                                                   
         MVI   NUSDRLEN,20                                                      
         OI    NUSDST3,X'20'                                                    
         BAS   RE,PUTELM                                                        
         B     SEEDX                                                            
         DROP  R4                                                               
         SPACE                                                                  
SEEDX    DS 0H                                                                  
         B     EXIT                (XIT1)                                       
         EJECT                                                                  
         SPACE                                                                  
* SUB-ROUTINE TO DELETE ELMENT (R6 POINTS TO RECORD)                            
*                                                                               
DELELM   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',UNTFILE),(ELCODE,(R6)),0                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TP PUT ELEMENT (R4 POINTS TO ELEMENT)                             
*                                                                               
PUTELM   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',UNTFILE),(ELCODE,(R6)),(R4)                     
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
         GETEL (R6),NBDTADSP,ELCODE                                             
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
HDHOOK   NTR1                                                                   
         L     R3,ATWA                                                          
         USING T31EFFD,R3                                                       
         MVC   H3+10(3),SRTCLT                                                  
         MVC   H3+16(20),SPLCLIN                                                
         MVC   H4+10(3),SRTPRDCD                                                
         MVC   H4+16(20),SPLPRON                                                
         MVC   H6+10(4),SRTNET                                                  
         LA    R3,H5+10                                                         
         EDIT  (B1,SRTEST),(3,0(R3))                                            
         LA    R3,H7+10                                                         
         EDIT  (B1,SRTPKG),(3,0(R3))                                            
         DROP  R3                                                               
         SPACE                                                                  
         LA    R2,H10                                                           
         USING PLINED,R2                                                        
         MVC   PUDTE(4),=C'UNIT'                                                
         MVC   PUDTE+132(4),=C'DATE'                                            
         MVC   PUDAY(3),=C'DAY'                                                 
         MVC   PUTIME(4),=C'TIME'                                               
         MVC   PUPRGCD(7),=C'PROGRAM'                                           
         MVC   PUPRGCD+133(4),=C'CODE'                                          
         MVC   PUNTI(4),=C'UNIT'                                                
         MVC   PUNTI+132(3),=C'NTI'                                             
         MVC   PUPRGNM(12),=C'UNIT PROGRAM'                                     
         MVC   PKPRGNM(12),=C'POCKET-PIECE'                                     
         MVC   PKPRGNM+132(12),=C'PROGRAM NAME'                                 
         MVC   PKDAY(3),=C'DAY'                                                 
         MVC   PKTIME(4),=C'TIME'                                               
         MVC   PKNTI(3),=C'NTI'                                                 
         MVC   PKNTI+132(4),=C'CODE'                                            
         MVC   PREMRKS(15),=C'SEEDING REMARKS'                                  
         SPACE                                                                  
*  SET UP BOXES PARAMETERS *                                                    
         CLI   BOXSET,C'Y'                                                      
         BE    HDX                                                              
         MVI   BOXSET,C'Y'                                                      
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R5,BOXCOLS                                                       
         MVI   0(R5),C'L'                                                       
         LA    R5,10(R5)           UNIT DATE                                    
         MVI   0(R5),C'C'                                                       
         LA    R5,6(R5)            DAY                                          
         MVI   0(R5),C'C'                                                       
         LA    R5,13(R5)           TIME                                         
         MVI   0(R5),C'C'                                                       
         LA    R5,8(R5)            PRGRM CODE                                   
         MVI   0(R5),C'C'                                                       
         LA    R5,6(R5)            UNIT NTI                                     
         MVI   0(R5),C'C'                                                       
         LA    R5,18(R5)           PRGRM NAME                                   
         MVI   0(R5),C'C'                                                       
         LA    R5,3(R5)            DOUBLE DIVIDER                               
         MVI   0(R5),C'C'                                                       
         LA    R5,18(R5)           PRGRM NAME                                   
         MVI   0(R5),C'C'                                                       
         LA    R5,5(R5)            DAY                                          
         MVI   0(R5),C'C'                                                       
         LA    R5,12(R5)            TIME                                        
         MVI   0(R5),C'C'                                                       
         LA    R5,7(R5)             PKTPIECE NTI                                
         MVI   0(R5),C'C'                                                       
         LA    R5,21(R5)                                                        
         MVI   0(R5),C'R'                                                       
         SPACE                                                                  
         LA    R5,BOXROWS                                                       
         LA    R5,8(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,3(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,46(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
*                                                                               
UNTFILE  DC    CL8'UNTFIL'                                                      
TRAPERR  EQU   ERREX                                                            
         SPACE                                                                  
*                                                                               
DEFINESV DS    CL16000   ****  W/S TO SAVE DEMO REC INFO  *****                 
NETBUFF  DS    CL2000              FOR NBCNVNTI                                 
         EJECT                                                                  
******************************************                                      
* ROUTINE MATCHES PROGRAM NAME OF UNIT RECORD                                   
*   WITH PROGRAM NAME FROM POCKET PIECE.                                        
*   IF ANY 3 LETTERS (OF 1ST 6) OF UNIT PROG NAME                               
*   MATCH IN SEQUENCE LETTERS OF THE FIRST WORD OF POCKET PIECE                 
*   PROG NAME THEN WE ASSUME A MATCH.                                           
* LOCAL   FULL/ BYTE/ ERROR/ DUB                                                
*         COND CODE SET TO = IF THERE IS A MATCH.                               
*                                                                               
MATCHPNM CSECT                                                                  
         NMOD1 0,**MTCH**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2                                                       
         USING WORKD,R7                                                         
*                                                                               
         LA    R5,P                                                             
         USING PLINED,R5                                                        
         MVI   BYTE,6              MAX LENGTH OF 1ST WORD(ASSUMED)              
         MVI   ERROR,0                                                          
         ZIC   R1,BYTE                                                          
         LA    R2,PUPRGNM          UNIT NAME                                    
         LA    R2,6(R2)                                                         
         ST    R2,FULL             SET END OF UNIT NAME IN FULL                 
         LA    R2,PUPRGNM                                                       
         LA    R3,PKPRGNM          PKTPIECE NAME                                
         ST    R3,DUB                                                           
MTCH5    CLC   0(1,R2),0(R3)     IS IT A LETTER MATCH                           
         BE    LMATCH              YES/GOTO LMATCH                              
         LA    R3,1(R3)            NO/BUMP PKTPIECE NAME                        
         BCT   R1,MTCH5                                                         
*                                                                               
         LA    R2,1(R2)            BUMP UNIT PROG NAME                          
         C     R2,FULL             IS IT BEYOND END OF NAME                     
         BH    NOMATCH                                                          
         L     R3,DUB              RESET R3 TO BEGN OF PKTPIECE NM              
         ZIC   R1,BYTE                                                          
         B     MTCH5                                                            
*                                                                               
LMATCH   ZIC   R4,ERROR            ERROR=NUM OF LETTER MATCHES                  
         LA    R4,1(R4)                                                         
         C     R4,=F'3'            IF 3 LETTER MATCHES                          
         BE    MTCHX               THEN GOTIT                                   
         STC   R4,ERROR                                                         
*                                                                               
         LA    R3,1(R3)           POINT R3 TO NXT LETTER TO BE MATCHED          
         ST    R3,DUB                                                           
         BCTR  R1,0               R1=POSITION NUM WHEN MATCHED                  
*                                 SO DECREASE IT FOR NEXT BCT                   
         LTR   R1,R1               IS IT ZERO/MINUS                             
         BNP   NOMATCH             YES/END OF TEST                              
         STC   R1,BYTE             NO/THIS IS NOW NEW BCT LIMIT                 
         LA    R2,1(R2)            BUMP UNIT PROG NAME                          
         B     MTCH5                                                            
*                                                                               
NOMATCH  DS    0H                                                               
         MVI   SEEDIT,0                                                         
         MVC   PREMRKS,=C'*** PRGNM ERROR ***'                                  
         LTR   RE,RE                                                            
MTCHX    XIT1                                                                   
         DROP  R5                                                               
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*** MY WORKING STORAGE USING ANETWSD2 + ANETWSD3 ***                            
*                                                                               
WORKD    DSECT                                                                  
MLTSDFLG DS    CL1                                                              
RESEED   DS    CL1                                                              
ASCRIBD  DS    CL1                                                              
CORRFLG  DS    CL1                                                              
CORRSW   DS    CL1                                                              
CORRTEMP DS    CL4                                                              
CORRNAME DS    CL16                                                             
RELO     DS    F                                                                
DEFRECS  DS    F                                                                
ADFN     DS    F                ADDRESS OF DEFINESV AS BUMPED IN DFNR           
ADBLOCK  DS    A                                                                
VNETWEEK DS    V                                                                
VDEMAND  DS    V                                                                
VDEFINE  DS    V                                                                
AMATCH   DS    F                                                                
RECSTSRT DS    CL8                 COUNTER FOR RECS TO AND FROM SORTER          
RECSFSRT DS    CL8                    "                                         
FRST     DS    CL1                                                              
NTIMTCH  DS    CL1                 NTI FLAG                                     
SEEDFLG  DS    CL1                 SEED FLAG                                    
SEEDIT   DS    CL1                 NO MULT SEED FLAG                            
PRDBKFLG DS    CL1                 PROD BREAK FLAG                              
UPDATFLG DS    CL1                 UPDATE UNT REC FLAG                          
BOXSET   DS    CL1                                                              
ENDWRT   DS    CL1                                                              
NTISEED  DS    CL5                                                              
DEMOYR   DS    CL1                                                              
DEMOWK   DS    CL1                                                              
EBCDAT   DS    CL6                 EBSDIC DATE                                  
*                                                                               
OLDSORT  DS    0CL10               PREVIOUS SORT KEY                            
OSRTCLT  DS    CL3                                                              
OSRTPRD  DS    CL1                                                              
OSRTEST  DS    CL1                                                              
OSRTNET  DS    CL4                                                              
OSRTPKG  DS    CL1                                                              
*                                                                               
CLTNMSV  DS    CL20                                                             
PRDCDSV  DS    CL3                                                              
PRDNMSV  DS    CL20                                                             
         EJECT                                                                  
* MY WORKING STORAGE CONTINUED *                                                
         SPACE                                                                  
SORTREC  DS    0CL143       *****   SORTRECORD  *****                           
SRTCLT   DS    CL3                 CLIENT CODE (PRINTABLE)                      
SRTPRD   DS    CL1                 PRODUCT   (NBPRD)                            
SRTEST   DS    CL1                 ESTIMATE  (NBACTEST)                         
SRTNET   DS    CL4                 NETWORK   (NBACTNET)                         
SRTDPT   DS    CL1                 DAYPART   (NBACTDP)                          
SRTPKG   DS    CL1                 PACKAGE   (NBPACK)                           
SRTPRGCD DS    CL6                 PROGRAM CODE   (NBACTPRG)                    
SRTDTE   DS    CL8                 DATE-SUBLINE (NBACTDAT+NBACTSUB)             
SRTDAY   DS    CL3                 DAY  (NBDAYNAM)                              
SRTTIME  DS    CL11                START-END TIME (NBTIME)                      
SRTNTI   DS    CL5                 NTI CODE OF UNIT (NBNTI)                     
SRTPRGNM DS    CL16                PROGRAM NAME (NBPRGNAM)                      
SRTKLENE EQU   *-SORTREC                                                        
SRTACTDT DS    CL2                                                              
SRTXNTI  DS    CL2                 BINARY NTI                                   
SRTXTIM  DS    CL4                 MILITARY TIME                                
SRTXDAY  DS    CL1                 DAY BITS                                     
SRTPRDCD DS    CL3                 PRINTABLE PRODUCT CODE                       
SRTPRDNM DS    CL20                PRODUCT NAME                                 
SRTCLTNM DS    CL20                CLIENT NAME                                  
SRTUNTK  DS    CL32                UNIT REC KEY (NBKEY)                         
SRTNTICK DS    CL1                 N=NTI CODE NOT FROM SEED (NOT USED)          
SRTRECLN EQU   *-SORTREC                                                        
SRTRECND EQU   *                                                                
         EJECT                                                                  
* MY WORKING STORAGE CONTINUED *                                                
         SPACE                                                                  
DBLOCKWS DS    0CL256              *** AREA FOR DEDBLOCK ***                    
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
DEFINED  DSECT *** DSECT FOR DEFINESV ***                                       
DFNPRG   DS    CL16                PROGRAM NAME                                 
DFNDAY   DS    CL5                 3-5 DAY ALPHA                                
DFNTIME  DS    CL6                 3-6 MILITARY TIME                            
DFNNTI   DS    CL5                 EDITED NTI PROGRAM CODE                      
DFNLENE  EQU   *-DEFINED                                                        
         EJECT                                                                  
*                                                                               
PLINED   DSECT            *** DSECT FOR PRINT LINE ***                          
         DS    CL2                                                              
PUDTE    DS    CL8                 UNIT DATE MMMDD-NN                           
         DS    CL2                                                              
PUDAY    DS    CL3                 UNIT DAY ALPHA                               
         DS    CL2                                                              
PUTIME   DS    CL11                UNIT TIME                                    
         DS    CL2                                                              
PUPRGCD  DS    CL6                 UNIT PROGRAM CODE                            
         DS    CL2                                                              
PUNTI    DS    CL5                 UNIT NTI                                     
         DS    CL1                                                              
PUPRGNM  DS    CL16                UNIT PROGRAM NAME                            
         DS    CL5                                                              
PKPRGNM  DS    CL16                PKTPIECE PROGRAM NAME                        
         DS    CL2                                                              
PKDAY    DS    CL3                 DAY ALPHA                                    
         DS    CL2                                                              
PKTIME   DS    CL11                                                             
         DS    CL2                                                              
PKNTI    DS    CL5                                                              
         DS    CL1                                                              
PREMRKS  DS    CL19                REMARCKS                                     
         DS    CL2                                                              
         EJECT                                                                  
         PRINT OFF                                                              
** NETINCLS                                                                     
** NEMEDFFD                                                                     
** NEMEDF7D                                                                     
** DDCOMFACS                                                                    
** DDBIGBOX                                                                     
** NEGENUNIT                                                                    
** SPGENCLT                                                                     
** SPGENPRD                                                                     
*                                                                               
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF7D                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'238NEMED27S  05/01/02'                                      
         END                                                                    
