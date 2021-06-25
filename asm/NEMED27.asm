*          DATA SET NEMED27    AT LEVEL 038 AS OF 03/10/21                      
*PHASE T31E27B,+0                                                               
*INCLUDE GETBROAD                                                               
         TITLE 'T31E27 - NTI PKTPIECE-UNIT SEED REPORT'                         
         PRINT NOGEN                                                            
************************************************************                    
* PKTPIECE-UNIT SEED REPORT 3 CHAR PROD VERSION            *                    
*                                                                               
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    *                    
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    *                    
* THE PRE-3 CHAR PROD VERISON IS NEMED27Z IN PZIR LIBRARY  *                    
* THE PRE-3 CHAR PROD VERISON IS NEMED27Z IN PZIR LIBRARY  *                    
* THE PRE-3 CHAR PROD VERISON IS NEMED27Z IN PZIR LIBRARY  *                    
* THE PRE-3 CHAR PROD VERISON IS NEMED27Z IN PZIR LIBRARY  *                    
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    *                    
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    *                    
*                                                          *                    
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
         L     R7,ANETWS3                                                       
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
         MVC   DMCB+4(4),=X'D9000A5A'          GET VCOMINTR                     
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VCOMINTR,DMCB                                                    
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
*                                                                               
         MVI   COMPASS,0                                                        
         L     RF,CMASTC-COMFACSD(RE)      RF=AMASTC                            
         ST    RF,AMASTC                                                        
         ICM   RF,15,MCAEXTRA-MASTD(RF)    EXTRA DATA AREA                      
         BZ    SETADCXX                                                         
         CLI   MCCSPASS-MCEXTRA(RF),MCCSP001                                    
         BNE   *+8                                                              
         MVI   COMPASS,1                                                        
         CLI   MCCSPASS-MCEXTRA(RF),MCCSP002                                    
         BNE   *+8                                                              
         MVI   COMPASS,2                                                        
*                                                                               
         SPACE                                                                  
SETADCXX ZAP   RECSTSRT,=P'0'                  ZAP COUNTERS                     
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
         MVI   SEEDITC,C'Y'                                                     
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
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=177'                                   
         SPACE 2                                                                
PAS03    MVI   NBDATA,C'U'                      SELECT UNIT RECORDS             
         MVI   NBSELUOP,C'A'                    ACTUAL SCHEDULE                 
         SPACE                                                                  
PAS05    NETGO NSNETIO,DMCB,NETBLOCK            GET  RECORD                     
         XC    VALIDSRC,VALIDSRC                                                
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
         MVC   FULL,NBACTNET                                                    
*                                                                               
         XC    SVNETNUM,SVNETNUM                                                
         BAS   RE,CHKNTWK          NETWK VALID FOR SEED?                        
****     BE    PAS10               YES                                          
*************************************                                           
* MATCHED ON NBACTNET TO NETLIST - NOW LOOK THROUGH NBCNVNTI                    
* TO FIND THE COMSCORE NETWORK NUMBER                                           
         BNE   PAS08K                                                           
         ICM   R1,15,NBCNVNTI         CHECK FOR NTI STATION                     
         BZ    PAS05                                                            
         LA    R1,3(R1)            BUMP TO START OF NTWKS                       
         LA    R2,799              MAX STATIONS IN LIST                         
PAS08C   CLC   NBACTNET,0(R1)                                                   
         BE    PAS08F                                                           
         LA    R1,20(R1)                                                        
         BCT   R2,PAS08C                                                        
         B     PAS10                                                            
PAS08F   MVC   SVNETNUM,10(R1)     SAVE COMSCORE NETWORK_NO                     
         B     PAS10               YES                                          
*************************************                                           
* DIDNT MATCH NBACTNET TO NETLIST.  LETS TRY AND MATCH ON                       
* NTI NETWORK CALL LETTER                                                       
PAS08K   ICM   R1,15,NBCNVNTI         CHECK FOR NTI STATION                     
         BZ    PAS05                                                            
         LA    R1,3(R1)            BUMP TO START OF NTWKS                       
         LA    R2,799              MAX STATIONS IN LIST                         
******   MVC   SVNETNUM,10(R1)     SAVE COMSCORE NETWORK_NO                     
PAS09    CLC   NBACTNET,0(R1)                                                   
         BE    PAS09B                                                           
***PAS09A   LA    R1,10(R1)                                                     
PAS09A   LA    R1,20(R1)                                                        
         BCT   R2,PAS09                                                         
* DIDN'T FIND A VALID NETWORK FOR NIELSEN TO MATCH AGAINST NETLIST              
* CHECK TO SE EIF COMSCORE NUMBER IS SET                                        
         OC    SVNETNUM,SVNETNUM   ANY COMSCORE NETWORK PASSED?                 
         BNZ   PAS10B                                                           
         B     PAS05               NET NOT VALID FOR COMSCORE OR NSI            
PAS09B   MVC   FULL,5(R1)          CHECK AGAINST EQUIV NTWK                     
         MVC   SVNETNUM,10(R1)     SAVE COMSCORE NETWORK_NO                     
         BAS   RE,CHKNTWK                                                       
         BE    PAS10               YES/OK                                       
         B     PAS09A              SEND BACK TO LOOP                            
*                                                                               
* EXPECTS FULL TO HAVE NETWORK TO BE CHECKED                                    
CHKNTWK  NTR1                                                                   
         LA    R1,NETLIST                                                       
CHK10    CLC   FULL,0(R1)                                                       
         JE    EXIT                                                             
         LA    R1,L'NETLIST(R1)                                                 
         CLI   0(R1),X'FF'                                                      
         BNE   CHK10                                                            
         LTR   RE,RE                                                            
         J     EXIT                                                             
*                                                                               
NETLIST  DS    0CL4                                                             
         DC    C'ABC NBC CBS FOX FBC WBN WBT WAR WBTVUPN '                      
         DC    C'UPNNPNW PAX TWBNPAR TEL UNI UNIVTFU TF  '                      
         DC    C'GAL UNV TELEMUN FSW WB  AZA CW  MNT CWN '                      
         DC    C'ION ETV UMA MFX MET BOU COZ ESC GRT LAF '                      
         DC    X'FF'                                                            
         EJECT                                                                  
         EJECT                                                                  
* FILL IN SORTREC *                                                             
PAS10    DS    0H                               *=KEY FIELD                     
         OI    VALIDSRC,VALIDNSI       SET FLAG NETWORK WAS VALID               
         OC    SVNETNUM,SVNETNUM       ANY COMSCORE NETWORK PASSED?             
         BNZ   PAS10B                                                           
         B     PAS10C                  FOR NIELSEN                              
PAS10B   DS    0H                                                               
         OI    VALIDSRC,VALIDCOM       VALID FOR COMSCORE                       
PAS10C   LA    R2,SORTREC                                                       
         XC    0(SRTRECLN,R2),0(R2)                                             
         MVC   SRTCLT,NBCLICOD                       *                          
***      MVC   SRTPRD,NBPRD                          *                          
         MVC   SRTPRD3,NBPR1CL3                       *                         
         CLI   PRDBKFLG,C'Y'                                                    
***      BE    *+8                                                              
***      MVI   SRTPRD,0                                                         
         BE    *+10                                                             
         XC    SRTPRD3,SRTPRD3                                                  
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
*                                                                               
*                                                                               
PAS13    DS    0H                                                               
         OC    NBEXTEND,NBEXTEND                                                
         BZ    PAS13A                                                           
         L     RF,NBEXTEND                                                      
         USING NBXTNDD,RF                                                       
         MVC   SRTSERNM,NBXCSN          SERIES IN UNIT                          
         DROP  RF                                                               
*                                                                               
PAS13A   MVC   SRTPRGNM,NBPROGNM                     *                          
         MVC   SRTXNTI,NBNTI            BINARY NTI                              
* NEED TO SAVE THE SERIES NUMBER IN SORTER FROM UNIT                            
         MVC   SRTXDAY,NBDAY            DAY BITS                                
         MVC   SRTXTIM,NBTIME           MILITARY START-END TIME                 
         MVC   SRTACTDT,NBACTDAT                                                
         MVC   SRTCLTNM,CLTNMSV                                                 
         MVC   SRTPRDCD,PRDCDSV                                                 
         MVC   SRTPRDNM,PRDNMSV                                                 
         MVC   SRTUNTK,NBKEY            UNT REC KEY                             
         MVC   SRTVALSR,VALIDSRC        VALID SOURCES FLAG                      
         MVC   SRTCNETN,SVNETNUM        COMSCORE NETWORK NUMBER                 
*                                                                               
***      MVC   SRTPTYPE,NBPOSTYP        ,,FOR HISPANIC                          
         MVC   SRTPTYPE,NBSURVEY        ,,USE NEW 3D BOOKTYPE FIELD             
         MVC   SRTSDROT,NBSDROT         ,,FROM MASTER STATION REC               
         MVC   SRTUSR13,NBUSER1+3       ,,                                      
*                                                                               
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
PASX     J     EXIT                                                             
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
         BAS   RE,HISPANIC                                                      
         GOTO1 VDEMAND,DMCB,ADBLOCK,CCRTN                                       
         J     EXIT                                                             
         SPACE 2                                                                
*                                                                               
CCRTN    NTR1                                                                   
         XC    CORRTEMP,CORRTEMP                                                
         XC    CORRNAME,CORRNAME                                                
         L     RF,VDEFINE                                                       
         GOTO1 (RF),DMCB,=C'CORR',ADBLOCK,CORRTEMP                              
         GOTO1 (RF),DMCB,=C'PROGRAM',ADBLOCK,CORRNAME                           
         CLI   CORRTEMP,X'40'                                                   
         JE    EXIT                                                             
         MVI   CORRSW,C'Y'                                                      
         J     EXIT                                                             
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
         MVI   CSERMTCH,0                                                       
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
*                                                                               
* IF WE ARE IN PASS ONE MODE - JUST CALL COMINTER WITH PUTS                     
* FOR EVERY UNIT WITH COMSCORE TURNED ON                                        
         CLI   COMPASS,1           IF PASS1 CALL COMINTER WITH PUT              
         BNE   FRM12A                                                           
         TM    SRTVALSR,VALIDCOM   COMSCORE ?                                   
         BZ    FRM10                                                            
         BRAS  RE,PUTCOM                                                        
         B     FRM10               GET NEXT RECORD FROM SORTER                  
*                                                                               
*                                                                               
FRM12A   CLI   FRST,C'Y'                                                        
         BNE   FRM13                                                            
         MVI   FRST,C'N'                                                        
         MVC   OSRTCLT,SRTCLT                                                   
         MVC   OSRTNET,SRTNET                                                   
         MVC   OSRTEST,SRTEST                                                   
         MVC   OSRTPKG,SRTPKG                                                   
**       MVC   OSRTPRD,SRTPRD                                                   
         MVC   OSRTPRD3,SRTPRD3                                                 
         SPACE                                                                  
FRM13    CLI   RESEED,C'Y'     IS RESEED OPTION ON                              
         BE    FRM30               YES/IGNORE NTI CODE ON UNIT                  
         CLI   CORRFLG,C'Y'        IS CORRECT OPTION ON                         
         BE    FRM30                   YES/IGNORE NTI CODE ON UNIT              
*                                                                               
* CALL GETCOM - CALL COMINTER TO GET PROGRAMS FOR COMSCORE                      
* IF UNIT PROCESSED COMSCORE                                                    
         XC    COMRECS,COMRECS                                                  
         TM    SRTVALSR,VALIDCOM                                                
         BZ    FRM18                                                            
*                                                                               
         L     RE,=A(COMPGTAB)                                                  
         L     RF,=F'16000'                                                     
         XCEF                                                                   
         L     R3,=A(COMPGTAB)                                                  
         ST    R3,ACMPGTAB         SAVE SINCE DFNRTN DOES NOT BUMP              
*                                                                               
         BRAS  RE,GETCOM                                                        
*                                                                               
         MVI   CSERMTCH,0                                                       
         OC    SRTSERNM,SRTSERNM                                                
         BZ    FRM17                                                            
* UNIT DOES HAVE SERIES NUMBER SEEDED IN IT                                     
         MVI   CSERMTCH,C'N'                                                    
         CLI   GETCOMF,C'Y'                                                     
         BNE   FRM17                                                            
         MVI   CSERMTCH,C'Y'                                                    
         B     FRM18                                                            
* UNIT DOES NOT HAVE SERIES NUMBER SEEDED IN IT                                 
FRM17    OC    COMRECS,COMRECS                                                  
         BNZ   *+8                                                              
         MVI   CSERMTCH,C'E'                                                    
*                                                                               
FRM18    CLI   SRTNTI,0            NO/IS NTI CODE PRESENT                       
         BE    FRM30                     NO/DO DAY-TIME READ                    
*&&DO                                                                           
* HAVE NTI IN UNIT                                                              
         OC    SRTSERNM,SRTSERNM   IF NOT NTI CODE, CHECK IF SERIES             
         BZ    FRM30               NUMBER IS ON THE UNIT                        
*&&                                                                             
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
*                                                                               
         LA    RE,DEFINESV                                                      
         L     RF,=F'16000'                                                     
         XCEF                                                                   
         XC    DEFRECS,DEFRECS                                                  
         LA    R3,DEFINESV                                                      
         USING DEFINED,R3                                                       
         ST    R3,ADFN             SAVE SINCE DFNRTN DOES NOT BUMP              
FRM21    BAS   RE,HISPANIC                                                      
         GOTO1 VDEMAND,DMCB,ADBLOCK,DFNRTN                                      
         OC    DBDIVSOR,DBDIVSOR   TEST REC FOUND                               
         BNZ   FRM22                                                            
         MVI   NTIMTCH,C'N'      NOT FOUND/SET NTIMATCH FLAG TO NO              
         B     FRM30             AND DO DAY/TIME                                
FRM22    CLC   SRTNTI,DFNNTI       TEST NTI MATCH                               
****     BE    *+6         REMOVED 8/10/05 WHEN FIX FOR MULTIPLE                
****     DC    H'0'        NTI CODES FOR SAME PROG CAUSED DEATH                 
*        CLC   SRTDAY,DFNDAY+2     TEST ALPHA DAY                               
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        CLC  SRTXTIM,DFNTIME+2    TEST MILITARY TIME                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   NTIMTCH,C'Y'        YES/SET NTIMATCH FLAG TO YES                 
*&&DO                                                                           
FRM25    OC    SRTSERNM,SRTSERNM   ANY COMSCORE SERIES NUMBER                   
         BZ    FRM28                                                            
         BRAS  RE,GETCOM                                                        
         MVI   CSERMTCH,C'N'                                                    
         CLI   GETCOMF,C'Y'                                                     
         BNE   *+8                                                              
         MVI   CSERMTCH,C'Y'                                                    
*&&                                                                             
*                                                                               
FRM28    BAS   RE,WRITREC          WRITE REPORT                                 
         OC    COMRECS,COMRECS     ANY COMSCORE?                                
         BZ    FRM10                                                            
         BAS   RE,WRITCOM          WRITE COMSCORE TO REPORT                     
         OC    SRTSERNM,SRTSERNM   IF UNIT DONT HAVE SERIES YET                 
         BNZ   FRM10               THEN SEED IT                                 
         MVI   SEEDSRC,SEEDCOM     ONLY SEED COMSCORE                           
         B     FRM32L                                                           
*                                                                               
***      B     FRM10               GET NEXT REC                                 
         EJECT                                                                  
         SPACE                                                                  
* NOTE FOR COMSCORE WHEN WE GET THE PROGRAMS BACK FOR EACH UNIT                 
* WE MATCH ON DATE, START TIM.  START TIME OF UNIT HAS TO BE <=                 
* START TIME OF PROGRAM.  ALSO SEED THE FIRST MATCH FOR EACH UNIT               
******************************                                                  
* UNIT REC HAS NO NTI NUMBER *                                                  
* PASS DEDBLOCK DAY/TIME     *                                                  
*                            *                                                  
******************************                                                  
FRM30    DS    0H                                                               
*                                                                               
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
*                                                                               
         BAS   RE,HISPANIC                                                      
*                                                                               
*                                                                               
*                                                                               
FRM30L   GOTO1 VDEMAND,DMCB,ADBLOCK,DFNRTN                                      
         OC    DBDIVSOR,DBDIVSOR   TEST ANY RECS FOUND                          
FRM30M   BNZ   FRM31                                                            
         MVI   NTIMTCH,C'E'        SET E(RROR)/NO REC FOUND                     
*        CLI   DBERROR,X'10'       TEST NO REC FOUND                            
*        BE    FRM32                                                            
         B     FRM32               LETS NOT DUMP 5/17/90                        
FRM31    CLI   DBERROR,X'80'       TEST DFNRTN HIT E-O-F                        
         BE    *+6                                                              
         DC    H'0'                                                             
FRM32    DS    0C                                                               
*&&DO                                                                           
* AFTER NIELSEN IS DONE, ALSO LOOK UP COMSCORE FOR PASS2                        
         TM    SRTVALSR,VALIDCOM   COMSCORE ?                                   
         BZ    FRM30L                                                           
         BRAS  RE,GETCOM           IF PASS2 CALL COMINTER WITH GET              
         MVI   CSERMTCH,C'E'       ERROR LOOKING UP COMSCORE                    
         CLI   GETCOMF,C'Y'                                                     
         BNE   *+8                                                              
         MVI   CSERMTCH,0          NO SERIES # ON UNIT BUT FOUND SERIES         
*                                  IN THE COMSCORE LOOKUP                       
*&&                                                                             
*                                                                               
FRM32D   BAS   RE,WRITREC          WRITE REPORT                                 
         OC    COMRECS,COMRECS     ANY COMSCORE TO REPORT ON?                   
         BZ    *+8                                                              
         BAS   RE,WRITCOM                                                       
         MVI   SEEDSRC,0           DEFAULT DONT SEE ANYTHING                    
* CHECK IF IF HAVE TO SEED COMSCORE                                             
         CLI   CSERMTCH,C'E'                                                    
         BE    FRM32K                                                           
         CLI   CSERMTCH,C'N'                                                    
         BE    FRM32K                                                           
         CLI   SEEDITC,C'Y'                                                     
         BNE   FRM32K                                                           
         OI    SEEDSRC,SEEDCOM                                                  
*                                                                               
FRM32K   CLI   NTIMTCH,C'N'        TEST 2ND READ ON NO NTI MATCH                
         BE    FRM33               YES/SKIP SEEDING                             
         CLI   NTIMTCH,C'E'        TEST E(RROR)/NO REC FOUND                    
         BE    FRM33               YES/SKIP SEEDING                             
FRM32L   TM    SEEDSRC,SEEDCOM                                                  
*******  BO    FRM32M                                                           
         CLI   SEEDIT,C'Y'         ARE MULT NTIS EQUAL                          
         BNE   FRM33               NO/SKIP SEEDING                              
         OI    SEEDSRC,SEEDNTI                                                  
*                                                                               
FRM32M   BAS   RE,SEEDUNT          SEED UNIT REC                                
FRM33    DS    0H                                                               
         MVI   SEEDIT,C'Y'                                                      
         B     FRM10               GET NEXT REC                                 
         SPACE 2                                                                
FRMX     GOTO1 SORTER,DMCB,=C'END'                                              
         J     EXIT                (XIT1)                                       
*                                                                               
         USING DBLOCK,R6                                                        
CNVNTI   DS    0H                  GET NTI STA FOR NETWORKS                     
         L     R5,NBCNVNTI                                                      
         LTR   R5,R5                                                            
         BZ    CNVX                                                             
         LA    R5,3(R5)            SKIP OVER AGY DATA                           
* IF ANY OF THE ENTRIES DONT HAVE NTI STATION AT 5(R5)                          
* DO NOT USE.  BEFORE WE SUPPORT COMSCORE THESE ENTRIE WE WERE NEVER            
* GETTING TABLE ENTIRES WHERE THE NTI STATION IS NOT FILLED IN                  
* IN THE MASTER RECORD.  WE ARE GETTING THEM NOW IF THE                         
* COMSCORE NETWORK NO IS FILLED IN IN THE MASTER RECORD                         
*                                                                               
CNV5     CLC   5(L'SRTNET,R5),=X'40404040'                                      
         BNH   CNV8                                                             
         CLC   SRTNET,0(R5)                                                     
         BNE   *+14                                                             
         MVC   DBSELSTA,5(R5)                                                   
         B     CNVX                                                             
***      LA    R5,10(R5)                                                        
CNV8     LA    R5,20(R5)                                                        
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
FLDX     J     EXIT                (XIT1)                                       
         DROP R6                                                                
         EJECT                                                                  
*                                                                               
HISPANIC NTR1                   HISPANIC GETS SPECIAL CONSIDERATION             
         L     R6,ADBLOCK                                                       
         USING DBLOCK,R6                                                        
         CLI   SRTPTYPE,C'H'                                                    
         BNE   HSPX                                                             
                                                                                
         MVC   DBSELSTA+3(2),=C' H'                                             
         MVI   DBBTYPE,0                                                        
                                                                                
**       JAMIE SAYS "NO" TO THE FOLLOWING                                       
**       OC    SRTNTI,SRTNTI       IF NO NTI                                    
**       BNZ   HSP10                                                            
**       MVC   DBSELDAY,SRTSDROT   USE NBSDROT                                  
**       XC    DBSEL1WK,DBSEL1WK   CLEAR THESE SET IN FILLDBLK                  
**       MVI   DBBEST,0                                                         
**       CLI   DBSELDAY,X'7C'      IF M-F                                       
**       BNE   HSP10                                                            
**       MVC   DBSEL1WK,SRTACTDT   SET THEM AGAIN                               
**       MVI   DBBEST,C'L'                                                      
                                                                                
HSP10    CLI   SRTUSR13,C'H'      ONLY NHTI WKLY (NBUSER1+3)                    
         BE    *+12                                                             
         CLI   SRTUSR13,C'B'      NHTI AND NAD WKLY                             
         BNE   HSP20                                                            
         MVI   DBSELMED,C'W'                                                    
         B     HSP30                                                            
                                                                                
*                                  IF NOT WEEKLY PASS YEAR/MONTH                
HSP20    DS    0H                                                               
**************************************************                              
*          DATA SET NEWRI82    AT LEVEL 178 AS OF 08/12/04                      
* USE MOBILE TO GET BROADCAST MONTH                                             
                                                                                
         GOTO1 DATCON,DMCB,(2,SRTACTDT),(0,MYWORK2)                             
         L     R2,GETDAY                                                        
         L     R4,ADDAY                                                         
         GOTO1 =V(GETBROAD),DMCB,(1,MYWORK2),MYWORK3,(R2),(R4)                  
         GOTO1 DATCON,DMCB,(0,MYWORK3+6),(3,DUB)                                
**************************************************                              
         MVC   DBSELBK,DUB         SET YEAR/MONTH                               
*                                                                               
HSP30    EQU   *                                                                
                                                                                
HSPX     J     EXIT                                                             
         DROP  R6                                                               
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
***********************************************************************         
* CALL COMINTER TO PUT REQUEST FOR LOOKUP                                       
***********************************************************************         
PUTCOM   NTR1                                                                   
         L     R6,ADBLOCK                                                       
         USING DBLOCK,R6                                                        
         MVC   DBCOMFCS,ACOMFACS      * PASS ACOMFACS                           
         MVC   SVEXTEND,DBEXTEND                                                
*                                                                               
         LA    R3,SEEDXT           COMINTER PARAM BLOCK                         
         USING DBSEEDD,R3                                                       
*                                                                               
         MVC   DBSEEID,=C'NSED'                                                 
         LA    RE,PRGBUFF                                                       
         ST    RE,DBSEAPGT                                                      
         CLC   SRTCNETN,=C'0000000000'                                          
         BH    *+8                                                              
         B     *+4                                                              
         MVC   DBSENETN,SRTCNETN   NETWORK NUMBER                               
         MVC   DBSERSDT,SRTDTE     REQUEST START DATE                           
         MVC   DBSEREDT,SRTDTE     REQUEST END DATE                             
         MVC   DBSEENE,DBEXTEND                                                 
         MVC   DBSERSTM(4),SRTXTIM  START/END TIME                              
         ST    R3,DBEXTEND                                                      
*                                                                               
         GOTO1 VCOMINTR,DMCB,=C'PUT',DBLOCK                                     
*                                                                               
PUTCOMX  MVC   DBEXTEND,SVEXTEND                                                
         J     EXIT                                                             
         DROP  R3,R6                                                            
         LTORG                                                                  
***********************************************************************         
* CALL COMINTER TO GET DEMOS AND SET PDDEMOS                                    
***********************************************************************         
GETCOM   NTR1                                                                   
         L     R6,ADBLOCK                                                       
         USING DBLOCK,R6                                                        
         MVC   DBCOMFCS,ACOMFACS      * PASS ACOMFACS                           
         MVC   SVEXTEND,DBEXTEND                                                
*                                                                               
         CLI   COMPASS,2           COMSCORE?                                    
         JNE   GETCOMX                                                          
*                                                                               
         LA    R3,SEEDXT           COMINTER PARAM BLOCK                         
         USING DBSEEDD,R3                                                       
*                                                                               
         MVC   DBSEEID,=C'NSED'                                                 
         LA    RE,PRGBUFF                                                       
         ST    RE,DBSEAPGT                                                      
                                                                                
         MVC   DBSENETN,SRTCNETN                                                
         MVC   DBSERSTM,SRTXTIM    UNIT START TIME                              
         MVC   DBSERETM,SRTXTIM+2  UNIT END TIME                                
         MVC   DBSERSDT,SRTDTE     REQUEST START DATE                           
         MVC   DBSEREDT,SRTDTE     REQUEST END DATE                             
         MVC   DBSEENE,DBEXTEND                                                 
         MVI   GETCOMF,C'N'                                                     
         ST    R3,DBEXTEND                                                      
*                                                                               
         GOTO1 VCOMINTR,DMCB,=C'GET',DBLOCK                                     
         JNE   GETCOMX                                                          
* LOOP THROUGH ALL THE PROGRAMS IN PROGRAMS TABLE                               
* AND STORE THEM IN DEFINESV  AS WE DO IN THE DEMAND HOOK DFNRTN                
*                                                                               
GETCOM20 ZICM  RE,DBSEAPGT,(15)    ADDRESS OF RETURNED PROGRAMS 4 UNIT          
         USING DBSEPGD,RE                                                       
         L     R3,=A(COMPGTAB)                                                  
         ST    R3,ACMPGTAB         SAVE SINCE DFNRTN DOES NOT BUMP              
* FIRST PUT ON A COMSCORE HEADER RECORD SO WE CAN                               
* BREAK LATER WHEN WE CALL WRITREC                                              
******   MVC   0(20,R3),=20X'FF'                                                
*****    LA    R3,DFNLENE(R3)           INCREMENT DEFINESV                      
*****    ST    R3,ADFN                                                          
         USING DEFINED,R3                                                       
         MVI   GETCOMF,C'N'                                                     
GETCOM40 CLI   0(RE),X'FF'         END OF COMINTER TABLE OF PROGRAMS            
         BE    GETCOMX                                                          
         MVC   DFNPRG,DBSEPNAM          SERIES NAME                             
         MVC   DFNDAY(L'DBSEPDAY),DBSEPDAY          SERIES DAY                  
****     MVC   DFNTIME+2(L'DBSEPSTM+L'DBSEPETM),DBSEPSTM                        
* CONVERT EBCDC TIME TO BINARY MILITARY                                         
         PACK  DUB,DBSEPSTM(4)                                                  
         CVB   R0,DUB                                                           
         STCM  R0,3,DFNTIME+2                                                   
         PACK  DUB,DBSEPETM(4)                                                  
         CVB   R0,DUB                                                           
         STCM  R0,3,DFNTIME+4                                                   
*                                                                               
**                                                                              
*                                                                               
****     MVC   DFNSTIME,DBSELSTM                                                
****     MVC   DFNETIME,DBSELETM                                                
         MVC   DFNCSNUM,DBSEPSNM        SERIES NUMBER                           
         MVI   DFNCOMSF,C'Y'            SET COMSCORE FLAG                       
*                                                                               
* SEE IF SRTSERNM IS SET (SERIES NUMBER ALREADY ON UNIT)                        
* IF SO ONLY LOOK FOR THAT ONE SERIES NUMBER                                    
* AND STORE IT IN THE BUFFER.                                                   
         OC    SRTSERNM,SRTSERNM                                                
         BZ    GETCOM50                                                         
         CLC   DBSEPSNM,SRTSERNM                                                
         BNE   GETCOM50                                                         
         MVI   GETCOMF,C'Y'             FOUND THE SERIES NUM FROM UNIT          
*                                                                               
*                                                                               
GETCOM50 LA    R3,DFNLENE(R3)           INCREMENT DEFINESV                      
*****    ST    R3,ACMPGTAB                                                      
         L     R1,COMRECS               INCREMENT NUM OF RECS                   
         LA    R1,1(R1)                                                         
         CH    R1,=H'336'          336= MAX RECS IN DEFINESV(48X7)              
         BNH   *+6                      (48 HALF HRS X 7 DAYS)                  
         DC    H'0'                                                             
         ST    R1,COMRECS                                                       
GETCOM70 AHI   RE,DBSEPLNQ                                                      
         B     GETCOM40                                                         
         DROP  R3,R6                                                            
*                                                                               
GETCOMX  MVC   SVEXTEND,DBEXTEND                                                
         J     EXIT                                                             
         LTORG                                                                  
******************************                                                  
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
*                                                                               
         MVI   WRITEFLG,WRITENTI                                                
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
         CLI   SUMMARY,C'Y'           ONLY REPORT UNMATCHED/MULTIPLE ?          
         BE    WRTX                    YES/SKIP                                 
         MVC   PREMRKS,=CL19'UNIT ALREADY SEEDED'  IPLE                         
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
WRT23    DS    0H                                                               
         MVC   NTISEED,PKNTI                                                    
         CLI   SUMMARY,C'Y'                REPORT UNMATCHED ONLY?               
         BE    WRTX                        YES-EXIT                             
         MVC   PREMRKS,=CL19'SEEDED WITH'          SINGLE SEED                  
         MVC   PREMRKS+13(5),PKNTI                                              
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
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   OSRTCLT,SRTCLT      SET OLD SORT KEY FOR FORCEHED                
         MVC   OSRTNET,SRTNET                                                   
         MVC   OSRTEST,SRTEST                                                   
         MVC   OSRTPKG,SRTPKG                                                   
**       MVC   OSRTPRD,SRTPRD                                                   
         MVC   OSRTPRD3,SRTPRD3                                                 
         CLI   ENDWRT,C'Y'         EXIT IF ENDWRT=YES                           
         BE    WRTX                                                             
         LA    R3,DFNLENE(R3)      OR IF END OF DEFINESV                        
         CLI   0(R3),X'FF'         NIELSEN ENDED - COMSCORE HEADER              
         BNE   *+8                 RECORD                                       
         B     WRITCOM             PRINT OUT FOR COMSCORE                       
         CLI   0(R3),0                                                          
         BNE   WRT30                                                            
WRTX     MVI   ENDWRT,0                                                         
         J     EXIT                                                             
         EJECT                                                                  
******************************                                                  
*                            *                                                  
*  WRITE COMSCORE REPORT     *                                                  
* R3 SHOULD BE SET           *                                                  
******************************                                                  
WRITCOM  NTR1                                                                   
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         USING DEFINED,R3                                                       
*                                                                               
         L     R3,ACMPGTAB                                                      
         USING DEFINED,R3                                                       
****     LA    R3,DFNLENE(R3)     SKIP FFFF HEADER RECORD                       
         MVI   SEEDFLGC,0                                                       
         SPACE                                                                  
         MVI   WRITEFLG,WRITECOM                                                
         MVI   PCOMIND,C'C'                                                     
         BAS   RE,FILLPLN                                                       
         SPACE                                                                  
WRTC02   CLI   CSERMTCH,C'E'      TEST ERROR/NO COMSCORE REC FOUND              
         BNZ   WRTC05                                                           
         XC    PKPRGNM,PKPRGNM                                                  
         XC    PKDAY,PKDAY                                                      
         XC    PKTIME,PKTIME                                                    
         XC    PKNTI,PKNTI                                                      
         XC    PCSERNUM,PCSERNUM                                                
         MVC   PKPRGNM(3),=C'  *** NO COMSCORE MATCH AVAILABLE ***'             
         MVI   ENDWRTC,C'Y'                                                     
         B     WRTC50                                                           
         SPACE                                                                  
WRTC05   CLI   CSERMTCH,C'Y'       TEST UNT NTI=PKTPC NTI                       
         BNE   WRTC10                                                           
         CLI   SUMMARY,C'Y'           ONLY REPORT UNMATCHED/MULTIPLE ?          
         BE    WRTCX                   YES/SKIP                                 
****     MVC   PREMRKCM,=CL19'UNIT ALREADY SEEDED'                              
         MVC   PREMRKS,=CL19'UNIT ALREADY SEEDED'                               
         MVI   ENDWRTC,C'Y'                                                     
         B     WRTC50                                                           
         SPACE                                                                  
WRTC10   CLI   CSERMTCH,C'N'        TEST UNT NTI NOT = PKTPC NTI                
         BNE   WRTC20                                                           
         MVC   PREMRKS,=CL19'CAN''T FIND'                                       
         MVC   PREMRKS+11(10),SRTSERNM                                          
         MVI   ENDWRTC,C'Y'                                                     
         B     WRTC50                                                           
         SPACE                                                                  
WRTC20   CLI   CSERMTCH,0           TEST UNT HAS NO SERIES NUMBER               
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R3                                                            
         LA    RE,DFNLENE(RE)                                                   
         CLI   0(RE),0                     ANY MORE RECS IN DEFINESV            
         BNE   WRTC25                      YES/SO GOTO MULT SEED                
*        GOTO1 AMATCH,DMCB,(RC)    TEST UNIT/PKTPIC PRGNM MATCH                 
*        BE    WRTC23                                                           
*        MVI   ENDWRT,C'Y'                                                      
*        B     WRTC50                                                           
WRTC23   DS    0H                                                               
         MVC   COMSEED,PCSERNUM                                                 
         CLI   SUMMARY,C'Y'                REPORT UNMATCHED ONLY?               
         BE    WRTCX                       YES-EXIT                             
         MVC   PREMRKS,=CL19'SEEDED W'          SINGLE SEED                     
         MVC   PREMRKS+9(10),PCSERNUM                                           
         MVI   ENDWRTC,C'Y'                                                     
         B     WRTC50                                                           
         EJECT                                                                  
         SPACE                                                                  
*    MULTIPLE SEED CONDITION                                                    
WRTC25   DS    0H                                                               
*        GOTO1 AMATCH,DMCB,(RC)    TEST UNIT/PKTPIC PROGNM MATCH                
*        BE    WRTC25A                                                          
*        MVI   SEEDFLGC,1           NO MATCH/FUDGE SEED FLAG                    
*        MVI   ENDWRT,C'N'                  SET MULT SEED COND SWITCH           
*        B     WRTC50                       AND GO TO WRITE REPORT              
*                                                                               
WRTC25A  MVC   PREMRKS,=CL19'MULTIPLE SERIES'    YES/MULT SEED                  
         CLI   MLTSDFLG,C'Y'          TEST MULT SEED OPTION                     
         BE    WRTC27                                                           
         BAS   RE,TSTMULT          CHK MULT COMSCORE SERIES NUMBERS             
         LTR   R1,R1               SET TO ZERO IF =                             
         BZ    WRTC27                                                           
         MVI   SEEDITC,C'N'            SKIP SEEDING                             
         MVI   SEEDFLGC,1                                                       
         MVI   ENDWRTC,C'N'         SET MULT SEED COND SWITCH                   
         B     WRTC50                                                           
WRTC27   MVC   COMSEED,PCSERNUM                                                 
*        CLC   PKNTI,DFNNTI+DFNLENE  TEST IF NTI CODES OF MULT SEED             
*        BE    WRTC23                AND NEXT PKTPIECE =/YES SKIP MULT          
         MVI   ENDWRTC,C'N'         (NOT USING THE ABOVE/SHOW MULTS)            
         B     WRTC50                                                           
WRTC30   MVI   PCOMIND,C'C'                                                     
         BAS   RE,FILLPLN                                                       
         CLI   SEEDFLGC,1                                                       
         BE    WRTC50                                                           
         MVI   SEEDFLGC,1                                                       
WRTC40   MVC   PREMRKS,=CL19'SEEDED W '                                         
         MVC   PREMRKS+9(10),COMSEED                                            
         SPACE                                                                  
*    WRITE TO THE REPORT                                                        
WRTC50   BAS   RE,BRKCHK                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   OSRTCLT,SRTCLT      SET OLD SORT KEY FOR FORCEHED                
         MVC   OSRTNET,SRTNET                                                   
         MVC   OSRTEST,SRTEST                                                   
         MVC   OSRTPKG,SRTPKG                                                   
**       MVC   OSRTPRD,SRTPRD                                                   
         MVC   OSRTPRD3,SRTPRD3                                                 
         CLI   ENDWRTC,C'Y'        EXIT IF ENDWRT=YES                           
         BE    WRTCX                                                            
         LA    R3,DFNLENE(R3)      OR IF END OF DEFINESV                        
         CLI   0(R3),X'FF'         NIELSEN ENDED - COMSCORE HEADER              
         BNE   *+8                 RECORD                                       
         B     WRITCOM             PRINT OUT FOR COMSCORE                       
         CLI   0(R3),0                                                          
         BNE   WRTC30                                                           
WRTCX    MVI   ENDWRTC,0                                                        
         J     EXIT                                                             
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
         CLI   WRITEFLG,WRITECOM                                                
         BNE   FLP10                                                            
         MVC   PCSERNUM,DFNCSNUM                                                
         MVC   PKDAY,SRTDAY                                                     
         MVC   PUNTSERN,SRTSERNM                                                
         J     EXIT                                                             
*                                                                               
FLP10    CLI   WRITEFLG,WRITENTI                                                
         BNE   *+10                                                             
         MVC   PKNTI,DFNNTI                                                     
         SPACE                                                                  
         J     EXIT                                                             
         SPACE 2                                                                
*************************************************************                   
* KEEP IMMEDIATELY AFTER FILLPLN FOR IT USES SAME REGS      *                   
* TEST IF MULT NTI CODES ARE EQUAL (ONLY CALLED FROM WRT25) *                   
* 1ST NTI IS TESTED AGGAINST NTIS OF DFNSV  IF =, SET R1=0  *                   
*************************************************************                   
TSTMULT  DS    0H                                                               
         LA    R1,DFNNTI                                                        
         CLI   WRITEFLG,WRITECOM                                                
         BNE   *+8                                                              
         LA    R1,DFNCSNUM                                                      
TM05     DS    0H                                                               
         CLI   WRITEFLG,WRITECOM                                                
         BNE   TM06                                                             
         CLC   PCSERNUM,0(R1)     ARE MULT SERIES CODE  =                       
         BNE   TMXX                                                             
         B     TM07                                                             
*                                                                               
TM06     CLI   WRITEFLG,WRITENTI                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PKNTI,0(R1)        ARE MULT NTI CODES =                          
         BNE   TMXX                                                             
*                                                                               
TM07     LA    R1,DFNLENE(R1)                                                   
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
**       CLC   OSRTPRD,SRTPRD      PRODUCT                                      
         CLC   OSRTPRD3,SRTPRD3     PRODUCT                                     
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
BKX      J     EXIT                                                             
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
                                                                                
*                                                                               
         CLI   SRTPRD3,0           UNALLOCATED ?                                
         BE    GP10                YES                                          
         MVC   SRTPRDCD,SRTPRD3                                                 
         B     GP16                NO                                           
*                                                                               
***      LA    R2,SRTUNTK          SORTREC HAS UNTKEY                           
***      USING NUKEY,R2                                                         
***      LA    R3,KEY                                                           
***      USING CKEY,R3                                                          
***      XC    KEY,KEY                                                          
***      MVC   CKEYAM,NUKAM                                                     
***      MVC   CKEYCLT,NUKCLT                                                   
***      NETGO NVSETSPT,DMCB       SET TO READ SPOT FILE                        
***      MVC   KEYSAVE,KEY                                                      
***      MVC   FILENAME,=C'SPTDIR  '                                            
***      GOTO1 HIGH                                                             
***      CLC   KEYSAVE(13),KEY                                                  
***      BE    *+6                                                              
***      DC    H'0'                                                             
***      MVC   FILENAME,=C'SPTFIL  '                                            
***      GOTO1 GETREC                                                           
***      L     R3,NBAIO                                                         
***      USING CLTHDR,R3                                                        
***      LA    R2,CLIST                                                         
***      LA    R4,220                                                           
***      MVI   BYTE,1                                                           
***      OC    SRTPRD,SRTPRD       TEST IF PRD UNALLOCATED                      
***      BNZ   GP12                                                             
***      XC    SRTPRDCD,SRTPRDCD                                                
GP10     MVC   SRTPRDNM,=CL20'PRODUCT UNALLOCATED'                              
         B     GPX                                                              
***GP12     CLC   3(1,R2),SRTPRD                                                
***         BE    GP14                                                          
***         LA    R2,4(R2)            INCREMENT CLIST                           
***         BCT   R4,GP12                RETURN TO LOOP                         
***         CLI   BYTE,2                                                        
***         BE    GP12D                                                         
***         MVI   BYTE,2                                                        
***         LA    R2,CLIST2                                                     
***         LA    R4,35                                                         
***         B     GP12                                                          
**GP12D  XC    SRTPRD,SRTPRD                                                    
**       MVC   SRTPRDNM,=CL20'PRODUCT UNDEFINED'                                
**       B     GPX                                                              
**GP14     MVC   SRTPRDCD,0(R2)      SET 3 CHAR PRINTABLE PRD CODE              
         SPACE                                                                  
* READ PRODUCT HEADER                                                           
GP16     LA    R2,SRTUNTK                                                       
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
         J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
         SPACE                                                                  
********************************************                                    
*                                          *                                    
*  SEED UNIT REC WITH PKTPIECE NTI CODE    *                                    
********************************************                                    
SEEDUNT  NTR1                                                                   
* YOU SHOULD ONLY BE HERE IT EITHER SEEDING NTI OR COMSCORE                     
         TM    SEEDSRC,SEEDNTI+SEEDCOM                                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
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
         TM    SEEDSRC,SEEDNTI                                                  
         BZ    NET02                                                            
         CLI   NTIMTCH,C'Y'                                                     
         BE    NET02                                                            
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
****                                                                            
*  COMSCORE SECTION                                                             
NET02    DS    0C                                                               
         TM    SEEDSRC,SEEDCOM                                                  
         BZ    NET60                                                            
         CLI   CSERMTCH,C'Y'                                                    
         BE    NET60                                                            
* ADD ELEMENTS FOR COMSCORE SERIES NUMBER                                       
*                                                                               
         L     R6,NBAIO                                                         
         USING NUSDRD,R6                                                        
         MVI   ELCODE,NUSDRELQ    NO COMSCORE SEEDED?                           
         BAS   RE,GETEL                                                         
         BNE   NET50                                                            
         TM    SEEDSRC,SEEDNTI                                                  
         BZ    *+8                                                              
         OI    NUSDST3,X'20'       SEED SEEDED NTI                              
         TM    SEEDSRC,SEEDCOM                                                  
         BZ    *+8                                                              
         OI    NUSDST4,NST4SCS     SEED SEEDED COMSCORE                         
* GET COMSCORE ELEMENT - SEED COMSCORE SERIES                                   
         L     R6,NBAIO                                                         
         USING NUCSELD,R6                                                       
         MVI   ELCODE,NUCSELQ                                                   
         BAS   RE,GETEL            WE SHOULD HAVE THIS ELEMENT                  
***      BE    *+6                 IF NUSDRELQ ELEMENT EXISTS                   
***      DC    H'0'                                                             
         BNE   NET50                                                            
         MVC   NUCSSN,COMSEED                                                   
         B     NET60                                                            
                                                                                
         DROP  R6                                                               
*  COMSCORE ELEMENTS DOES NOT EXISTS                                            
NET50    L     R6,NBAIO            ELEM DOES NOT EXIST/ADD IT                   
         LA    R4,ELEM                                                          
         USING NUCSELD,R4                                                       
         XC    ELEM,ELEM                                                        
         MVI   NUCSEL,NUCSELQ                                                   
         MVI   NUCSLEN,NUCSLENQ                                                 
         MVC   NUCSSN,COMSEED                                                   
         BAS   RE,PUTELM                                                        
*                                                                               
*  DO WE HAVE X'02' ELEMENT                                                     
NET60    L     R6,NBAIO                                                         
         USING NUSDRD,R6                                                        
         MVI   ELCODE,NUSDRELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   NET80                                                            
         TM    SEEDSRC,SEEDNTI                                                  
         BZ    *+8                                                              
         OI    NUSDST3,X'20'       SEED SEEDED NTI                              
         TM    SEEDSRC,SEEDCOM                                                  
         BZ    *+8                                                              
         OI    NUSDST4,NST4SCS     SEED SEEDED COMSCORE                         
         B     SEEDX                                                            
         DROP  R6                                                               
*                                                                               
NET80    LA    R4,ELEM                                                          
         USING NUSDRD,R4                                                        
         XC    ELEM,ELEM                                                        
         MVI   NUSDREL,NUSDRELQ                                                 
         MVI   NUSDRLEN,20                                                      
****     OI    NUSDST3,X'20'                                                    
         TM    SEEDSRC,SEEDNTI                                                  
         BZ    *+8                                                              
         OI    NUSDST3,X'20'       SEED SEEDED NTI                              
         TM    SEEDSRC,SEEDCOM                                                  
         BZ    *+8                                                              
         OI    NUSDST4,NST4SCS     SEED SEEDED COMSCORE                         
         BAS   RE,PUTELM                                                        
         B     SEEDX                                                            
         SPACE                                                                  
SEEDX    DS 0H                                                                  
         J     EXIT                (XIT1)                                       
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
         MVC   PUPRGCD(6),=C'PROGRM'                                            
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
         LA    R5,2(R5)            COMSCORE INDICATOR                           
         MVI   0(R5),C'C'                                                       
         LA    R5,9(R5)            UNIT DATE                                    
         MVI   0(R5),C'C'                                                       
         LA    R5,4(R5)            DAY                                          
         MVI   0(R5),C'C'                                                       
         LA    R5,12(R5)           TIME                                         
         MVI   0(R5),C'C'                                                       
         LA    R5,7(R5)            PRGRM CODE                                   
         MVI   0(R5),C'C'                                                       
         LA    R5,11(R5)            UNIT NTI                                    
         MVI   0(R5),C'C'                                                       
         LA    R5,17(R5)           PRGRM NAME                                   
         MVI   0(R5),C'C'                                                       
         LA    R5,1(R5)            DOUBLE DIVIDER                               
         MVI   0(R5),C'C'                                                       
         LA    R5,17(R5)           PRGRM NAME                                   
         MVI   0(R5),C'C'                                                       
         LA    R5,4(R5)            DAY                                          
         MVI   0(R5),C'C'                                                       
         LA    R5,13(R5)            TIME                                        
         MVI   0(R5),C'C'                                                       
         LA    R5,11(R5)             PKTPIECE NTI                               
         MVI   0(R5),C'C'                                                       
         LA    R5,20(R5)                                                        
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
HDX      J     EXIT                 (XIT1)                                      
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
*                                                                               
UNTFILE  DC    CL8'UNTFIL'                                                      
TRAPERR  EQU   ERREX                                                            
         SPACE                                                                  
*                                                                               
MYWORK   DS    CL40                                                             
MYWORK2  DS    CL40                                                             
MYWORK3  DS    CL30                                                             
MOBILADS DS    CL16                                                             
*                                                                               
DEFINESV DS    CL16000   ****  W/S TO SAVE DEMO REC INFO  *****                 
*NETBUFF  DS    CL2000              FOR NBCNVNTI                                
NETBUFF   DS    CL10000   ****  W/S FOR COMSCORE DATA IN DEFINESV               
COMPGTAB DS    CL16000   ****  W/S TO SAVE DEMO REC INFO  *****                 
         EJECT                                                                  
******************************************                                      
* IMPORTANT THIS ROUTINE IS NOT CALLED CURRENTLY, IF IT EVER                    
* IS CALLED AGAIN= IT WILL NOT WORK W COMSCORE                                  
******************************************                                      
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
         L     R7,ANETWS3                                                       
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
*** MY WORKING STORAGE USING ANETWSD3 + ANETWSD4 ***                            
*                                                                               
WORKD    DSECT                                                                  
SUMMARY  DS    CL1                                                              
MLTSDFLG DS    CL1                                                              
RESEED   DS    CL1                                                              
ASCRIBD  DS    CL1                                                              
CORRFLG  DS    CL1                                                              
CORRSW   DS    CL1                                                              
CORRTEMP DS    CL4                                                              
CORRNAME DS    CL16                                                             
RELO     DS    F                                                                
DEFRECS  DS    F                                                                
COMRECS  DS    F                                                                
ADFN     DS    F                ADDRESS OF DEFINESV AS BUMPED IN DFNR           
ACMPGTAB DS    F                ADDRESS OF DEFINESV AS BUMPED IN DFNR           
AMASTC   DS    A                   A(MASTC)                                     
COMPASS  DS    XL1                 X'01' =PASS 1 / X'02'=PASS2                  
SVNETNUM DS    CL10                X'01' =PASS 1 / X'02'=PASS2                  
SEEDSRC  DS    XL1                                                              
SEEDNTI  EQU   X'80'                                                            
SEEDCOM  EQU   X'40'                                                            
WRITEFLG DS    XL1                                                              
WRITENTI EQU   X'80'                                                            
WRITECOM EQU   X'40'                                                            
VALIDSRC DS    X                                                                
VALIDNSI EQU   X'80'                                                            
VALIDCOM EQU   X'40'                                                            
ADBLOCK  DS    A                                                                
VNETWEEK DS    V                                                                
VDEMAND  DS    V                                                                
VCOMINTR DS    V                                                                
VDEFINE  DS    V                                                                
AMATCH   DS    F                                                                
RECSTSRT DS    CL8                 COUNTER FOR RECS TO AND FROM SORTER          
RECSFSRT DS    CL8                    "                                         
FRST     DS    CL1                                                              
NTIMTCH  DS    CL1                 NTI FLAG                                     
CSERMTCH DS    CL1                 COMSCORE SERIES FLAG                         
GETCOMF  DS    CL1                 FOUND SERIES IN GETCOM CALL?                 
SEEDFLG  DS    CL1                 SEED FLAG                                    
SEEDFLGC DS    XL1                                                              
SEEDIT   DS    CL1                 NO MULT SEED FLAG                            
SEEDITC  DS    CL1                 NO MULT SEED FLAG FOR COMSCORE               
PRDBKFLG DS    CL1                 PROD BREAK FLAG                              
UPDATFLG DS    CL1                 UPDATE UNT REC FLAG                          
BOXSET   DS    CL1                                                              
ENDWRT   DS    CL1                                                              
ENDWRTC  DS    CL1                                                              
NTISEED  DS    CL5                                                              
COMSEED  DS    CL10                                                             
DEMOYR   DS    CL1                                                              
DEMOWK   DS    CL1                                                              
EBCDAT   DS    CL6                 EBSDIC DATE                                  
SVEXTEND DS    A                                                                
*                                                                               
OLDSORT  DS    0CL12               PREVIOUS SORT KEY                            
OSRTCLT  DS    CL3                                                              
*OSRTPRD  DS    CL1                                                             
OSRTPRD3 DS    CL3                                                              
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
SORTREC  DS    0CL178       *****   SORTRECORD  *****                           
SRTCLT   DS    CL3                 CLIENT CODE (PRINTABLE)                      
**SRTPRD   DS    CL1                 PRODUCT   (NBPRD)                          
SRTPRD3  DS    CL3                PRODUCT   (NBPR1CL3=NBPRD)                    
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
SRTPTYPE DS    CL1                 NBPOSTYPE                                    
SRTSDROT DS    CL1                 NBSDROT                                      
SRTUSR13 DS    CL1                 NBUSER1+3                                    
SRTCNETN DS    CL10                COMSCORE NETWORK NO                          
SRTVALSR DS    X                   VALID SOURCES                                
SRTSERNM DS    XL10                SERIES NUMBER ON UNIT                        
***      DS    CL27                SPARE                                        
         DS    CL6                 SPARE                                        
SRTRECLN EQU   *-SORTREC                                                        
SRTRECND EQU   *                                                                
         EJECT                                                                  
* MY WORKING STORAGE CONTINUED *                                                
         SPACE                                                                  
         DS    0F                                                               
DBLOCKWS DS    0CL256              *** AREA FOR DEDBLOCK ***                    
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
SEEDXT   DS    CL(DBSERLNQ)                                                     
PRGBUFF  DS    100CL(L'DBSEPGD)                                                 
*                                                                               
         EJECT                                                                  
*                                                                               
COMNLSTD DSECT                                                                  
COMCALL  DS    CL4                                                              
COMNETN  DS    XL4                                                              
COMNLSTQ EQU   *-COMNLSTD                                                       
*                                                                               
         SPACE 2                                                                
DEFINED  DSECT *** DSECT FOR DEFINESV ***                                       
DFNPRG   DS    CL16                PROGRAM NAME                                 
DFNDAY   DS    CL5                 3-5 DAY ALPHA                                
DFNTIME  DS    CL6                 3-6 MILITARY TIME                            
DFNNTI   DS    CL5                 EDITED NTI PROGRAM CODE                      
         DS    CL5                                                              
         ORG   DFNNTI                                                           
* COMSCORE SERIES NUMBER IS 10 CHARACTERS                                       
DFNCSNUM DS    CL10                COMSCORE SERIES NUMBER                       
DFNCOMSF DS    C                   COMSCORE FLAG                                
DFNLENE  EQU   *-DEFINED                                                        
         EJECT                                                                  
*                                                                               
PLINED   DSECT            *** DSECT FOR PRINT LINE ***                          
         DS    CL1                                                              
PCOMIND  DS    CL1                 COMSCORE INDICATOR                           
         DS    CL1                                                              
PUDTE    DS    CL8                 UNIT DATE MMMDD-NN                           
         DS    CL1                                                              
PUDAY    DS    CL3                 UNIT DAY ALPHA                               
         DS    CL1                                                              
PUTIME   DS    CL11                UNIT TIME                                    
         DS    CL1                                                              
PUPRGCD  DS    CL6                 UNIT PROGRAM CODE                            
         DS    CL1                                                              
PUNTI    DS    CL5                 UNIT NTI                                     
         ORG   PUNTI                                                            
PUNTSERN DS    CL10                                                             
         DS    CL1                                                              
PUPRGNM  DS    CL16                UNIT PROGRAM NAME                            
**       DS    CL4                                                              
         DS    CL2                                                              
PKPRGNM  DS    CL16                PKTPIECE PROGRAM NAME                        
         DS    CL1                                                              
PKDAY    DS    CL3                 DAY ALPHA                                    
         DS    CL2                                                              
PKTIME   DS    CL11                                                             
         DS    CL1                                                              
PKNTI    DS    CL5                                                              
         ORG   PKNTI                                                            
PCSERNUM DS    CL10                                                             
         DS    CL1                                                              
PREMRKS  DS    CL19                REMARCKS                                     
***      DS    CL2                                                              
***      ORG   PREMRKS                                                          
***MRKCM DS    CL21                REMARKS FOR COMSCORE                         
*&&DO                                                                           
PLINED   DSECT            *** DSECT FOR PRINT LINE ***                          
         DS    CL1                                                              
PCOMIND  DS    CL1                 COMSCORE INDICATOR                           
         DS    CL1                                                              
PUDTE    DS    CL8                 UNIT DATE MMMDD-NN                           
         DS    CL1                                                              
PUDAY    DS    CL3                 UNIT DAY ALPHA                               
         DS    CL1                                                              
PUTIME   DS    CL11                UNIT TIME                                    
         DS    CL1                                                              
PUPRGCD  DS    CL6                 UNIT PROGRAM CODE                            
         DS    CL1                                                              
PUNTI    DS    CL5                 UNIT NTI                                     
         DS    CL1                                                              
PUPRGNM  DS    CL16                UNIT PROGRAM NAME                            
         DS    CL4                                                              
PKPRGNM  DS    CL16                PKTPIECE PROGRAM NAME                        
         DS    CL1                                                              
PKDAY    DS    CL3                 DAY ALPHA                                    
         DS    CL2                                                              
PKTIME   DS    CL11                                                             
         DS    CL1                                                              
PKNTI    DS    CL5                                                              
         ORG   PKNTI                                                            
PCSERNUM DS    CL10                                                             
         DS    CL1                                                              
PREMRKS  DS    CL19                REMARCKS                                     
         DS    CL2                                                              
         ORG   PREMRKS                                                          
PREMRKCM DS    CL21                REMARKS FOR COMSCORE                         
*&&                                                                             
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
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DEDBEXTRAD                                                     
       ++INCLUDE NETINCLN                                                       
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
       ++INCLUDE FASSB                                                          
       ++INCLUDE NETBLKXTND                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038NEMED27   03/10/21'                                      
         END                                                                    
