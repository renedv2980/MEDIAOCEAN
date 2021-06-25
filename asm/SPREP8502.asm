*          DATA SET SPREP8502  AT LEVEL 062 AS OF 05/01/02                      
*PHASE SP8502B,+0                                                               
         TITLE 'SP085-02  ADJ FACTOR PRINT'                                     
SP8502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP8502                                                         
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING SP85WRKD,RC                                                      
         STM   R9,RC,REGSTR                                                     
         LA    R1,HEADHK                                                        
         ST    R1,HEADHOOK                                                      
         SPACE 3                                                                
*                                  THIS PROGRAM PRINTS THE TELEVISION           
*                                       ADJUSTMENT FACTORS.                     
*        REQUEST -                                                              
*           AATCCC      MMMM                                                    
*              AA      - AGENCY                                                 
*              CCC     - CLIENT/ALL                                             
*              MMMM    - MARKET/ALL                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*              RTG BOOK DATE (CC 50 - 53)                                       
*                        YYMM                                                   
*              OPTIONS (CC62-66)                                                
*                1     - N/A (NSI/ARB)  DEFAULT = ARB                           
*                2&3   - AUDIENCE TYPE                                          
*                        00-DEFAULT (PRINT ALL AUDIENCE TYPES)                  
*                        01-DMA HH                                              
*                        02-TOT WOM                                             
*                        03-W18-49                                              
*                        04-TOT MEN                                             
*                        05-M18-49                                              
*                        06-TEENS                                               
*                        07-CHLDRN                                              
*                        08-MET HH                                              
         EJECT                                                                  
*                                                                               
         CLI   MODE,PROCREC                                                     
         BNE   EXIT                                                             
         SPACE                                                                  
*                                                                               
MAINLN   DS    0H                                                               
         SPACE                                                                  
         BAS   RE,HUTHSKPG                                                      
         BAS   RE,GETMKTS                                                       
         BAS   RE,SRTMKTS                                                       
         BAS   RE,GETHUT                                                        
         BAS   RE,ERRTN                                                         
         SPACE                                                                  
HUTEX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         DS    F                                                                
HUTHSKPG ST    RE,*-4                                                           
         SPACE                                                                  
* CLEAR AREAS, INITIALIZE *                                                     
         LA    RE,STSRCE                                                        
         LA    RF,DMREC+999                                                     
         BAS   R1,CLR                                                           
         LA    RE,MKTTBLE                                                       
         LA    RF,3000(RE)                                                      
         MVC   0(4,RF),=4X'00'                                                  
         BAS   R1,CLR                                                           
         SPACE                                                                  
         LA    R1,ERRMKT                                                        
         ST    R1,ERRMAD                                                        
         LA    R1,ERRHUT                                                        
         ST    R1,ERRHAD                                                        
         SPACE                                                                  
* SET UP CLIENT                                                                 
         CLC   QCLT(3),=C'ALL'                                                  
         BE    HH0                                                              
         CLC   QCLT(3),=3C' '                                                   
         BNE   HH01                                                             
HH0      MVC   STCLT(11),=C'ALL CLIENTS'                                        
         B     HH21                                                             
HH01     TM    BCLT,X'80'                                                       
         BO    HH1                                                              
         EDIT  BCLT,(3,STCLT)                                                   
         B     HH2                                                              
         SPACE                                                                  
HH1      MVC   STCLT(3),BCLT                                                    
         SPACE                                                                  
HH2      L     R7,ADCLT                                                         
         USING CLTHDR,R7                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         GOTO1 CLPACK,DMCB,QCLT,KEY+2                                           
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CLTERR                                                           
         GOTO1 GETCLT                                                           
         MVC   STCLT+4(20),CNAME                                                
         SPACE 2                                                                
* SET UP RTG SOURCE *                                                           
HH21     MVC   STSRCE(3),=C'ARB'                                                
         CLI   QOPT1,C'N'                                                       
         BNE   HH3                                                              
         MVC   STSRCE(3),=C'NSI'                                                
         SPACE                                                                  
* SET UP RTG DATE *                                                             
HH3      CLC   QBOOK1(4),=4C' '                                                 
         BE    HHERR                                                            
         MVI   STSRCE+3,C'-'                                                    
         MVC   STSRCE+4(2),QBOOK1+2                                             
         MVI   STSRCE+6,C'/'                                                    
         MVC   STSRCE+7(2),QBOOK1                                               
         IC    R2,QBOOK1+1                                                      
         SLL   R2,28                                                            
         SRL   R2,28                                                            
         MVC   WORK(2),QBOOK1+2                                                 
         PACK  DUB,WORK(2)                                                      
         CVB   R3,DUB                                                           
         SLL   R3,28                                                            
         SRDL  R2,28                                                            
         ST    R3,WORK                                                          
         MVC   STDTE(1),WORK+3                                                  
         SPACE                                                                  
         MVC   WORK(4),QBOOK1      CONVERT YYMM TO YM. STORE IN BQBK1           
         MVC   WORK+4(2),=X'F0F1'                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK)                                    
         MVC   BQBK1,WORK                                                       
         SPACE                                                                  
* TEST OPTION VALIDITY *                                                        
         CLI   QOPT1,C'N'                                                       
         BE    *+8                                                              
         MVI   QOPT1,C'A'                                                       
         CLC   QOPT2(2),=C'01'                                                  
         BL    HH4                                                              
         CLC   QOPT2(2),=C'15'                                                  
         BH    HHERR                                                            
         B     HH5                                                              
HH4      MVC   QOPT2(2),=C'00'                                                  
HH5      CLC   QMKT(3),=3C' '                                                   
         BNE   *+10                                                             
         MVC   QMKT(3),=C'ALL'                                                  
         SPACE                                                                  
         L     RE,HUTHSKPG-4                                                    
         BR    RE                                                               
         SPACE                                                                  
*                                                                               
HHERR    MVC   P(18),=C'REQUEST CARD ERROR'                                     
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
CLTERR   MVC   P+10(27),=C'NO CLIENT HDR FOR CLIENT NO'                         
         MVC   P+40(3),KEY+9                                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                  READ STATION FILE FOR STATION CALL           
*                                       LETTERS & MKT NO (CL5-A)                
*                                  BUILD A LIST OF UNIQUE MKT NOS               
*                                  USING STA CALL LETTERS READ DEMFILE          
*                                       (RTG REC) TO GET RTG SOURCE             
*                                       MKT NO (CL2-B), WHICH IS STORED         
*                                       IN MKT LIST.                            
*                                                                               
         DS    F                                                                
GETMKTS  ST    RE,*-4                                                           
*                                                                               
*                                  SET UP STAKEY                                
         MVI   KEY,X'F0'                                                        
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),=5C'A'                                                  
         MVC   KEY+7(2),QAGY                                                    
         CLC   QCLT,=C'ALL'                                                     
         BE    *+10                                                             
         MVC   KEY+9(3),QCLT                                                    
         L     R8,ADSTAT                                                        
         USING STAREC,R8                                                        
         GOTO1 HIGHSTA                                                          
         TM    8(R1),X'50'                                                      
         BM    HUTEX                                                            
*                                       IF CLIENT NOT FOUND PRINT               
*                                            SPECIAL MESSAGE                    
GM0      CLI   STAKEY,C'S'                                                      
         BNE   GM9                                                              
GM01     CLC   STAKEY+7(5),KEY+7                                                
         BE    GM2                                                              
GM02     GOTO1 SEQSTA                                                           
         TM    8(R1),X'50'                                                      
         BM    HUTEX                                                            
         B     GM0                                                              
         SPACE 2                                                                
GM2      LA    R3,MKTTBLE                                                       
GM3      CLC   0(4,R3),=4X'00'                                                  
         BE    GM4                                                              
         CLC   0(4,R3),SMKT                                                     
         BE    GM02                                                             
         LA    R3,6(R3)                                                         
         B     GM3                                                              
         SPACE 2                                                                
*                                       IF QMKT NOT EQUAL 'ALL' COMPARE         
*                                            QMKT & SMKT. IF NO MATCH           
*                                            GET NEXT STATION ELSE              
*                                            STORE IN LIST.                     
GM4      CLC   QMKT(3),=C'ALL'                                                  
         BE    GM5                                                              
         CLC   SMKT,QMKT                                                        
         BNE   GM02                                                             
         SPACE 2                                                                
*                                       SET UP RTG KEY                          
GM5      XC    RKEY,RKEY                                                        
         LA    R7,RKEY                                                          
         USING DRKEY,R7                                                         
         MVI   DRCODE,C'R'                                                      
         MVC   DRMEDIA,QMED                                                     
         MVC   DRSRC,QOPT1              RATING SERVICE                          
         MVC   DRSTAT,STAKEY+2                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',RKEY,DMREC                   
         TM    8(R1),X'10'                                                      
         BZ    *+6                                                              
         DC    H'0'                DID NOT FIND STATION ON DEMFILE              
         CLI   DMREC,C'R'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R7,DMREC                                                         
         CLC   DRSTAT,STAKEY+2                                                  
         BNE   GM02                                                             
         OC    DRKMKT,DRKMKT                                                    
         BNZ   GM02                                                             
         MVC   FULL,DRNDXDA                                                     
         XC    DRKMKT(10),DRKMKT                                                
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMFIL ',FULL,DMREC                   
         TM    8(R1),X'50'                                                      
         BM    HUTEX                                                            
         TM    8(R1),X'80'                                                      
         BNO   *+8                                                              
         MVI   SWEOF,C'Y'                                                       
         SPACE 2                                                                
         LA    R7,DMREC                                                         
         CLC   DRSTAT,STAKEY+2                                                  
         BNE   GM02                                                             
         SPACE 2                                                                
         MVC   0(4,R3),SMKT                                                     
         DROP  R7                                                               
         LA    R7,23(R7)                                                        
         USING MARELEM,R7                                                       
         MVC   4(2,R3),MARNO                                                    
         DROP  R7                                                               
         CLC   QMKT(3),=C'ALL'                                                  
         BE    GM02                                                             
         SPACE 2                                                                
GM9      CLC   MKTTBLE(4),=4X'00'                                               
         BNE   GMEX                                                             
         MVC   P+10(26),=C'NO STATION RECS FOR CLT NO'                          
         MVC   P+40(3),KEY+9                                                    
         GOTO1 REPORT                                                           
         B     HUTEX                                                            
         SPACE 2                                                                
GMEX     L     RE,GETMKTS-4                                                     
         BR    RE                                                               
         EJECT                                                                  
*                                  SORT THE MARKET TABLE                        
*                                       SEQUENCE = SPOTPAK MKT NO               
*                                       NO OF ENTRIES = R3 / 6                  
*                                                                               
*                                                                               
*                                                                               
         DS    F                                                                
SRTMKTS  ST    RE,*-4                                                           
         XR    R2,R2                                                            
         LA    R3,MKTTBLE                                                       
SM1      CLI   0(R3),X'00'                                                      
         BE    SM2                                                              
         LA    R3,6(R3)                                                         
         B     SM1                                                              
SM2      LA    R4,MKTTBLE                                                       
         SR    R3,R4                                                            
         D     R2,=F'6'                                                         
         GOTO1 XSORT,DMCB,MKTTBLE,(R3),6,4,0                                    
         L     RE,SRTMKTS-4                                                     
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*                                  GET MARKET RECORD (FOR MKT NAME)             
*                                       GET HUTS                                
*                                       SET UP PRINT LINE                       
*                                                                               
*                                            R3 - MKTTBLE                       
*                                                                               
         DS    F                                                                
GETHUT   ST    RE,*-4                                                           
*                                                                               
*                                                                               
         LA    R3,MKTTBLE                                                       
GH1      CLC   0(4,R3),=4X'00'                                                  
         BE    GHEX                                                             
         MVC   STMKT(4),0(R3)                                                   
*                                                                               
*                                       SET UP MKTKEY                           
GH2      MVI   KEY,X'F0'                                                        
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),0(R3)                                                   
         MVC   KEY+6(2),QAGY                                                    
         CLC   QCLT,=C'ALL'                                                     
         BE    *+10                                                             
         MVC   KEY+8(3),QCLT                                                    
         L     R7,ADMARKET                                                      
         USING MKTREC,R7                                                        
         GOTO1 HIGHMKT                                                          
         TM    8(R1),X'50'                                                      
         BM    HUTEX                                                            
*                                       IF CLIENT NOT FOUND - USE               
*                                            DEFAULT MARKETS                    
         CLC   MKTKEY(11),KEY                                                   
         BE    GH3                                                              
         CLC   QCLT,=C'ALL'                                                     
         BE    GHERR1                                                           
         MVC   QCLT,=C'ALL'                                                     
         MVI   STMKT+4,C'*'                                                     
         B     GH2                                                              
GH3      MVC   STMKT+6(24),MKTNAME                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   DPPREV,=3X'FF'           FOR PRINT LINE SPACING                  
*                                                                               
*                                       SET UP HUTKEY                           
         XC    HKEY,HKEY                                                        
         LA    R8,HKEY                                                          
         USING SRKEYD,R8                                                        
         MVI   SRCODE,C'S'                                                      
         MVC   SRMED,QMED                                                       
         MVC   SRSRC,QOPT1                                                      
         EJECT                                                                  
* PREPARE TO USE PROC FROM GETDEM TO GET ADJ HUTS *                             
         SPACE                                                                  
         LA    RE,DMREC                                                         
         LA    RF,1000                                                          
         XCEF                                                                   
         SPACE                                                                  
         LA    R7,DMREC                                                         
         USING DBLOCK,R7                                                        
         LA    R2,DMREC+500                                                     
         USING COMFACSD,R2                                                      
         SPACE                                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBACTSRC,QOPT1                                                   
         SPACE                                                                  
         MVC   DBSELBK,BQBK1                                                    
         MVC   DBSELMED,QMED                                                    
         MVC   DBSELAGY,QAGY                                                    
         MVC   DBSELCLI,QCLT                                                    
         MVC   DBSELSRC,QOPT1                                                   
         SPACE                                                                  
* PROC FROM GETDEM ****                                                         
         SPACE                                                                  
         MVC   COMPBK1,BQBK1                                                    
         XC    COMPBK1,=X'FFFF'                                                 
GDHK18   DS    0H                                                               
         XC    DUB,DUB             SPECIAL CALL FOR SYSFACS                     
         ICM   RF,15,DBCOMFCS                                                   
         ICM   RF,15,CMASTC-COMFACSD(RF)                                        
         ICM   RF,15,MCSSB-MASTD(RF)       RF=ASSB                              
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(5),=X'D8000000FF'                                            
         L     R4,DBCOMFCS                                                      
         L     RF,CDEMADDR-COMFACSD(R4)                                         
         GOTO1 (RF),P1,(X'FF',DUB),(R4)                                         
         L     R4,DUB              R4 -> ADJ TABLE IN DATASPACE                 
         USING ADJTABD,R4                                                       
         LAM   R4,R4,ALET                                                       
         SAC   512                                                              
*                                                                               
         MVC   DUB(1),QOPT1                                                     
         MVC   DUB+1(1),DBSELMED                                                
         SR    R0,R0                                                            
*                                                                               
GDHK20   CLC   DUB(2),ADJSRC       MATCH SOURCE/MEDIA                           
         BNE   GDHK24                                                           
         CLC   ADJAGY,=X'FFFF'     TEST DEFAULT AGY                             
         BE    *+14                                                             
         CLC   DBSELAGY,ADJAGY     MATCH AGY                                    
         BNE   GDHK24                                                           
         CLI   ADJCODE,X'FF'                                                    
         BNE   GDHK24                                                           
* NON-DEFAULT CODES NOT IMPLEMENTED *                                           
         CLC   ADJCLI,=X'FFFFFF'   TEST DEFAULT CLIENT                          
         BE    *+14                                                             
         CLC   DBSELCLI,ADJCLI                                                  
         BNE   GDHK24                                                           
         MVC   DUB+2(2),ADJSBOOK                                                
         XC    DUB+2(2),=X'FFFF'   COMPLEMENT BOOK                              
         CLC   COMPBK1,DUB+2       TEST ACTUAL BOOK TO START BOOK               
         BNL   GDHK25                                                           
*                                                                               
GDHK24   ICM   R4,7,11(R4)         POINT TO END                                 
         LA    R4,1(R4)            THEN TO END                                  
         CLI   0(R4),0             TEST EOT                                     
         BNE   GDHK20              NO - CONTINUE                                
         LAM   R4,R4,=F'0'         CLR ACCESS REGISTER                          
         SAC   0                                                                
         B     GHEX                AGY NOT FOUND                                
GDHK25   MVC   SRSCODE,ADJHUT      AGY FOUND, SOURCE CODE                       
         LAM   R4,R4,=F'0'         CLR ACCESS REGISTER                          
         SAC   0                                                                
                                                                                
****** END OF GETDEM PROC ******                                                
                                                                                
*                                                                               
         MVC   SRHIMAR,4(R3)                                                    
         SPACE                                                                  
         DROP  R7                                                               
         DROP  R2                                                               
         LA    RE,DMREC                                                         
         LA    RF,1000                                                          
         XCEF                                                                   
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'HUTFL ',HKEY,DMREC                    
         B     GH3A3                                                            
GH3A2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'HUTFL ',DMREC,DMREC                   
GH3A3    TM    8(R1),X'50'                                                      
         BM    GHEX                                                             
         TM    8(R1),X'80'                                                      
         BO    GHEX                                                             
         MVC   HALF,DMREC+9        CLEAR END OF RECORD                          
         LH    RE,HALF                                                          
         LA    RF,DMREC                                                         
         AR    RF,RE                                                            
         MVI   0(RF),0                                                          
         CLC   HKEY(4),DMREC       SAME ADJUSTMENT SET                          
         BNE   GH20                                                             
         LA    R2,DMREC+11                                                      
         USING SVELEM,R2                                                        
         BAS   RE,GN1                                                           
         B     GH10                                                             
GH3B     LA    R8,DMREC                                                         
         CLC   SRHIMAR,4(R3)                                                    
         BE    GH4                                                              
         BH    GH20                                                             
GH3C     BAS   RE,GETNXT                                                        
         B     GH10                                                             
         B     GH3B                                                             
*                                  MKTS MATCH - TEST DEMOS                      
GH4      CLC   QOPT2(2),=C'00'                                                  
         BE    GH6                                                              
         PACK  DUB,QOPT2(2)                                                     
         CVB   R0,DUB                                                           
         STC   R0,DUB                                                           
         CLC   SVTYP,DUB                                                        
         BE    GH6                                                              
         BL    GH3C                                                             
         BH    GH3A2                                                            
         EJECT                                                                  
* SET UP PRINT LINE*                                                            
*                                            DAY                                
GH6      CLC   DPPREV,=3X'FF'      PRINT BLANK LINE BETWEEN                     
         BE    GH6A                DIFFERENT DAY PARTS                          
         CLC   DPPREV,SRHIDAY                                                   
         BE    GH6A                                                             
         MVI   P,X'40'                                                          
         MVC   P+1(131),P                                                       
         GOTO1 REPORT                                                           
GH6A     XR    R4,R4                                                            
         IC    R4,SRHIDAY                                                       
         SRL   R4,4                                                             
         MH    R4,=H'3'                                                         
         LA    R5,DAYTBL-3                                                      
         AR    R5,R4                                                            
         MVC   P(3),0(R5)                                                       
         XR    R4,R4                                                            
         IC    R4,SRHIDAY                                                       
         SLL   R4,28                                                            
         SRL   R4,28                                                            
         MH    R4,=H'3'                                                         
         LA    R5,DAYTBL-3                                                      
         AR    R5,R4                                                            
         MVC   P+4(3),0(R5)                                                     
         MVI   P+3,C'-'                                                         
         CLC   P(3),P+4                                                         
         BNE   *+10                                                             
         XC    P+3(4),P+3                                                       
*                                                                               
*                                            TIME                               
         SR    R6,R6                                                            
         IC    R6,SRLOWQH                                                       
         BAS   RE,MLTRY                                                         
         STH   R7,WORK                                                          
         SR    R6,R6                                                            
         IC    R6,5(R2)                                                         
         IC    R6,SRHIQH                                                        
         LA    R6,1(R6)                                                         
         BAS   RE,MLTRY                                                         
         STH   R7,WORK+2                                                        
         GOTO1 UNTIME,DMCB,WORK,P+8                                             
*                                                                               
*                                            AUD TYPE                           
         SR    RE,RE                                                            
         IC    RE,SVTYP                                                         
         MH    RE,=H'7'                                                         
         LA    R7,AUDTBLE                                                       
         AR    R7,RE                                                            
         MVC   P+28(7),0(R7)                                                    
*                                                                               
         MVC   HALF,STDTE          ADJUST TO BASE MONTH                         
         NI    HALF,X'0F'                                                       
         SR    R4,R4                                                            
         SR    R7,R7                                                            
         IC    R4,HALF                                                          
         BCTR  R4,0                                                             
         LA    R4,SVJAN(R4)                                                     
         MVC   HALF(1),0(R4)                                                    
         LA    R6,SVJAN                                                         
         LA    R1,12                                                            
         SR    R4,R4                                                            
CALCH    SR    RE,RE                                                            
         IC    R4,HALF                                                          
         LTR   R4,R4                                                            
         BZ    CALCNXT                                                          
         IC    RE,0(R6)                                                         
         LTR   RE,RE                                                            
         BZ    CALCNXT                                                          
         MH    RE,=H'1000'                                                      
         SRDA  RE,32                                                            
         DR    RE,R4                                                            
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         STC   RF,0(R6)                                                         
         CH    RF,=H'255'                                                       
         BL    *+8                                                              
         MVI   0(R6),255                                                        
CALCNXT  LA    R6,1(R6)                                                         
         BCT   R1,CALCH                                                         
*                                            HUTS                               
         LA    R6,SVJAN                                                         
         LA    R7,P+41                                                          
         LA    R1,12                                                            
GH7      EDIT  (1,0(R6)),(3,0(R7)),FILL=0                                       
         LA    R6,1(R6)                                                         
         LA    R7,8(R7)                                                         
         BCT   R1,GH7                                                           
*                                                                               
GH8      GOTO1 REPORT                                                           
         MVC   DPPREV,SRHIDAY                                                   
         MVI   SWPRNT,C'1'                                                      
         B     GH3C                                                             
*                                                                               
*                                                                               
GH10     B     GH3A2                                                            
*                                                                               
*                                                                               
GH20     CLI   SWPRNT,C'1'                                                      
         BNE   GHERR                                                            
         MVI   SWPRNT,C'0'                                                      
         LA    R3,6(R3)                                                         
         B     GH1                                                              
*                                                                               
*                                                                               
GHEX     L     RE,GETHUT-4                                                      
         BR    RE                                                               
*                                                                               
*                                  NO HUT RECORDS FOUND                         
GHERR    L     R4,ERRHAD                                                        
         MVC   0(4,R4),0(R3)                                                    
         MVI   SWHUT,1                                                          
         MVI   SWPRNT,C'1'                                                      
         LA    R4,4(R4)                                                         
         ST    R4,ERRHAD                                                        
         B     GH20                                                             
*                                                                               
*                                  NO MARKET RECS FOUND                         
GHERR1   L     R4,ERRMAD                                                        
         MVC   0(4,R4),0(R3)                                                    
         MVI   SWMKT,1                                                          
         LA    R4,4(R4)                                                         
         ST    R4,ERRMAD                                                        
         MVI   SWPRNT,C'1'                                                      
         B     GH20                                                             
         EJECT                                                                  
*                                                                               
*                                  GET NEXT DEMFILE HUT ELEM                    
*                                      R2 - A(DEMFILE ELEM)                     
*                                      R1 - L(DEMFILE ELEM)                     
*                                                                               
GETNXT   SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
GN1      CLI   SVCODE,2                                                         
         BE    4(RE)                                                            
         CLI   0(R2),0                                                          
         BNE   GETNXT                                                           
         BR    RE                                                               
*                                                                               
*                                                                               
*                                  CONVERT DEMFILE TIME TO                      
*                                       MILITARY TIME                           
*                                       R6 - DEMFILE TIME                       
*                                       R7 - ANSWER-MILITARY TIME               
MLTRY    MH    R6,=H'15'                                                        
         SRDA  R6,32                                                            
         L     R4,=F'60'                                                        
         DR    R6,R4                                                            
         MH    R7,=H'100'                                                       
         AH    R7,=H'600'                                                       
         AR    R7,R6                                                            
         CH    R7,=H'2399'                                                      
         BNH   ML02                                                             
         SH    R7,=H'2400'                                                      
ML02     BR    RE                                                               
         SPACE 2                                                                
*                                  CLEAR STORAGE ROUTINE                        
*                                       RE - START STORAGE                      
*                                       RF - END STORAGE                        
*                                                                               
CLR      SR    RF,RE                                                            
         BCTR  RF,0                                                             
CLR1     CH    RF,=H'256'                                                       
         BL    CLR2                                                             
         XC    0(256,RE),0(RE)                                                  
         LA    RE,256(RE)                                                       
         SH    RF,=H'256'                                                       
         BCR   4,R1                                                             
         B     CLR1                                                             
CLR2     EX    RF,CLRX                                                          
         BR    R1                                                               
CLRX     XC    0(0,RE),0(RE)                                                    
         EJECT                                                                  
*                                                                               
*                                                                               
*                                  ROUTINE TO PRINT OUT ANY                     
*                                       ERRORS FOUND                            
*                                                                               
*                                  MKT ERRORS                                   
         DS    F                                                                
ERRTN    ST    RE,*-4                                                           
         MVI   ERRSW,1                                                          
         CLI   SWMKT,1                                                          
         BNE   ER10                                                             
         LA    R4,ERRMKT                                                        
         LA    R5,10                                                            
         LA    R6,P+25                                                          
         LA    R7,25                                                            
ER02     CLC   0(4,R4),=4X'00'                                                  
         BE    ER04                                                             
         MVC   0(4,R6),0(R4)                                                    
         LA    R4,4(R4)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,ER02                                                          
ER04     MVC   P+1(22),=C'NO MKT REC FOUND FOR  '                               
         GOTO1 REPORT                                                           
         LA    R5,10                                                            
         LA    R6,P+25                                                          
         CLC   0(4,R4),=4X'00'                                                  
         BE    ER10                                                             
         BCT   R7,ER02                                                          
*                                                                               
*                                  HUT ERRORS                                   
ER10     MVI   LINE+1,99                                                        
         CLI   SWHUT,1                                                          
         BNE   EREX                                                             
         LA    R4,ERRHUT                                                        
         LA    R5,10                                                            
         LA    R6,P+25                                                          
         LA    R7,25                                                            
ER12     CLC   0(4,R4),=4X'00'                                                  
         BE    ER14                                                             
         MVC   0(4,R6),0(R4)                                                    
         LA    R4,4(R4)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,ER12                                                          
ER14     MVC   P+1(22),=C'NO HUT RECS FOR MKT   '                               
         GOTO1 REPORT                                                           
         LA    R5,10                                                            
         LA    R6,P+25                                                          
         CLC   0(4,R4),=4X'00'                                                  
         BE    EREX                                                             
         BCT   R7,ER12                                                          
*                                                                               
*                                                                               
EREX     LA    R4,ERRMKT                                                        
         ST    R4,ERRMAD                                                        
         LA    R4,ERRHUT                                                        
         ST    R4,ERRHAD                                                        
         MVI   ERRSW,0                                                          
         XC    SWHUT(2),SWHUT                                                   
         L     RE,ERRTN-4                                                       
         BR    RE                                                               
         SPACE                                                                  
*                                                                               
HEADHK   DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=REGSTR+8                                                    
         DROP  RF                                                               
         LM    R9,RC,REGSTR                                                     
         MVC   H1+11(1),QMED                                                    
         MVC   H2+11(9),STSRCE                                                  
         MVC   H3+11(25),STCLT                                                  
         MVC   H4+11(30),STMKT                                                  
         SPACE                                                                  
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
AUDTBLE  DS    0CL63                                                            
         DC    C'       DMA HH TOT WOMW18-49 TOT MENM18-49 TEENS  '             
         DC    CL7'CHLDRN'                                                      
         DC    CL7'MET HH'                                                      
         DC    CL7'VW12-34'                                                     
         DC    CL7'W25-54'                                                      
         DC    CL7'M25-54'                                                      
         DC    CL7'VW6-11'                                                      
         DC    CL7'A18-49'                                                      
DAYTBL   DC    C'MONTUEWEDTHUFRISATSUN'                                         
ERRMAD   DS    F                      A(ERRMKT)                                 
ERRHAD   DS    F                      A(ERRHUT)                                 
NSITBL   DC    C'JWN5JTN5T1N5VMN6DHTBTBTBRSN6'                                  
         DC    X'0000'                                                          
ARBTBL   DC    C'JWAQJTAQTRBOT1A5KEA5ARBODDBOYRBODIDFNHDFRSDFSFDFWLDF'          
         DC    X'0000'                                                          
*                                                                               
REGSTR   DS    4F                                                               
DPPREV   DS    CL3                                                              
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE                                                                  
*                                  TABLE OF SPOTPAK MKT NOS (CL4-N)             
*                                       VS RTG SOURCE MKT NOS (CL2-B)           
*                                  THERE IS A MAX OF 500 TABLE ENTRIES          
*                                  END OF TABLE IS 4X'00'                       
*                                                                               
MKTTBLE  DS    500CL6                                                           
MKTEND   DS    XL4                 MOVED IN AT START, 4X'00'                    
         SPACE 3                                                                
*                                                                               
SP85WRKD DSECT                                                                  
STSRCE   DS    CL9                 ARB OR NSI                                   
STKEY    DS    CL3                                                              
STMKT    DS    CL30                MKT NO & NAME  (4&24)                        
STCLT    DS    CL25                CLT NO & NAME  (3&20)                        
STDTE    DS    CL1                 HUT BOOK DATE (BINARY)                       
STPL     DS    CL133                                                            
RKEY     DS    CL18                                                             
HKEY     DS    CL18                                                             
ERRMKT   DS    250CL4                                                           
ERRHUT   DS    250CL4                                                           
FRSTSW   DS    CL1                                                              
ERRSW    DS    CL1                                                              
SWPRNT   DS    CL1                                                              
SWHUT    DS    CL1                                                              
SWMKT    DS    CL1                                                              
SWEOF    DS    CL1                                                              
BQBK1    DS    CL2                                                              
COMPBK1  DS    CL2                                                              
ALET     DS    A                                                                
DMREC    DS    CL1000                                                           
*                                                                               
         SPACE                                                                  
       ++INCLUDE RZSVIFILE                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE FASYSFAC                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062SPREP8502 05/01/02'                                      
         END                                                                    
