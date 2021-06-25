*          DATA SET SPREPBL02  AT LEVEL 088 AS OF 12/19/13                      
*PHASE SPBL02A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE SORTER                                                                 
         SPACE 2                                                                
*=================================================================*             
* CREATE DOWNLOAD FILE OF AGENCY BILLING RECORDS                  *             
*                                                                 *             
* QOPT1 = Y TO RUN FOR AGENCY IN QAGY FIELD ONLY                  *             
*           ELSE ALL AGENCIES ON THIS SPTFILE                     *             
*                                                                 *             
* QOPT2 = Y TO RUN ONE CLIENT TEST FOR QAGY/QMED                  *             
*           CLIENT CODE TO RUN FOR IS IN QPRD                     *             
*                                                                               
* QOPT3 = D BREAKOUT NET SUBMEDIA D                                             
*         O BREAKOUT NET SUBMEDIA O                                             
*         B BREAKOUT BOTH NET SUBMEDIA D AND O                                  
*=================================================================*             
         TITLE 'SPBL02 - DOWNLOAD DDS BILLING RECORDS'                          
SPBL02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPBL02                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPBL02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    BL10                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    BL300                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*============================================================*                  
* REQFRST - FIRST BUILD AGENCY TRANSLATION TABLE             *                  
*============================================================*                  
BL10     DS    0H                                                               
         L     RE,VMASTC                                                        
         CLI   MCNETPAK-MASTD(RE),C'Y'                                          
         BNE   BL11                                                             
         CLI   FLISTSW,C'Y'                                                     
         BE    BL11                                                             
         MVI   FLISTSW,C'Y'                                                     
*                                                                               
         LA    R2,FLIST                                                         
         L     R3,ADBUY                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=CL8'DMOPEN',=C'SPOT',(R2),(R3)                     
         B     BL11                                                             
*                                                                               
FLIST    DC    CL8' UNTDIR '                                                    
         DC    CL8' UNTFILE'                                                    
         DC    CL8'X       '                                                    
FLISTSW  EQU   *-1                                                              
*                                                                               
BL11     DS    0H                                                               
         XC    THISREC,THISREC                                                  
*                                                                               
         MVC   QSTART+4(2),=C'01'  FORCE THE DAY TO 01                          
         GOTO1 DATCON,DMCB,QSTART,(3,DUB)                                       
         MVC   STRTBILL,DUB        START OF MONTH                               
         MVC   ENDBILL(2),DUB                                                   
         MVI   ENDBILL+2,X'1F'     END OF MONTH                                 
* NEED PACKED DATES FOR NET TRAFFIC                                             
         GOTO1 (RF),(R1),(3,STRTBILL),(2,BQSTARTP)                              
         GOTO1 (RF),(R1),(3,ENDBILL),(2,BQENDP)                                 
*                                                                               
         ZIC   R0,QSTART+1         GET DECIMAL YEAR DIGIT                       
         N     R0,=X'0000000F'                                                  
         SLL   R0,4                                                             
*                                                                               
         ZIC   R1,DUB+1            GET HEX MONTH DIGIT                          
         N     R1,=X'0000000F'                                                  
         OR    R0,R1                                                            
         STC   R0,BBILLYM          SET BILL YEAR/MONTH FILTER                   
*                                                                               
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(3,DUB),(20,WORK)                                    
         MVC   THISBLYM(4),WORK     MOVE YYYY                                   
         MVI   THISBLYM+4,C'/' /                                                
         MVC   THISBLYM+5(2),WORK+4 MM                                          
*&&DO                                                                           
         GOTO1 ADDAY,DMCB,(C'M',QSTART),DUB,F'-1'                               
         GOTO1 DATCON,DMCB,DUB,(3,THREE)                                        
         MVC   PREVYRMO,THREE      REMEMBER PREVIOUS YEAR/MONTH                 
*&&                                                                             
         OPEN  (CLTFILE,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             BUILD TABLE OF AGENCIES                      
         MVI   KEY,6                                                            
         GOTO1 HIGH                                                             
         CLI   KEY,6                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     BL16                                                             
*                                                                               
BL14     GOTO1 SEQ                                                              
*                                                                               
BL16     CLI   KEY,6                                                            
         BNE   BL20                                                             
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         USING AGYHDRD,R6                                                       
*                                                                               
         CLI   QOPT1,C'Y'          TEST RUN FOR ONE AGENCY ONLY                 
         BNE   *+14                                                             
         CLC   QAGY,1(R6)          MATCH AGENCY CODE                            
         BNE   BL14                                                             
*                                                                               
         TM    AGYFLAG1,AGYTESTQ  IS THIS A TEST AGENCY                         
         BO    BL14               YES - IGNORE                                  
*                                                                               
         ZIC   RE,AGYPROF+19                                                    
*                                                                               
         LA    R0,240              X'F1' (241) BECOMES X'01'                    
         CLI   AGYPROF+19,C'F'                                                  
         BH    *+8                                                              
         LA    R0,183              X'C1' (193) BECOMES X'0A'                    
         SR    RE,R0                                                            
         LTR   R0,RE               SAVE RESULT                                  
         BNP   BL14                IGNORE AGENCY 0                              
         BCTR  RE,0                ARITHMETIC                                   
         SLL   RE,3                X 8                                          
         A     RE,=A(AGYTAB)                                                    
         MVC   0(2,RE),AGYKAGY                                                  
         STC   R0,2(RE)            SAVE NUMERIC CODE                            
*                                                                               
         MVC   4(2,RE),AGYKAGY     DEFAULT CORP ID IS AGY CODE                  
         MVC   6(1,RE),AGYFLAG1    SAVE AGENCY FLAG BYTE                        
         B     BL14                                                             
         DROP  R6                                                               
         EJECT                                                                  
*============================================================*                  
* OPEN A REPORT ON THE PRTQUE AND INITALIZE FOR DOWNLOADING  *                  
*============================================================*                  
*                                                                               
BL20     DS    0H                                                               
         XC    DLCB,DLCB                                                        
D        USING DLCBD,DLCB                                                       
*                                                                               
         MVI   D.DLCBACT,C'I'      START AND INITIALIZE REPORT                  
         MVC   D.DLCBAPR,=A(BLPRINT) PRINT ROUTINE ADDRESS                      
         LA    R0,P                                                             
         ST    R0,D.DLCBAPL        PRINT LINE ADDRESS                           
         OI    D.DLCBFLG1,DLCBFXTN                                              
         MVC   D.DLCXTND(7),XTENSION                                            
         MVI   FORCEHED,C'Y'       POUR GASTON                                  
*                                                                               
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
*========================================================                       
* OUTPUT A SYSNUM IDENTIFYING RECORD AND                                        
* A RECORD FOR EACH AGENCY IN THE TABLE                                         
*========================================================                       
*                                                                               
         MVC   D.DLCBFLD(8),=C'*SYSNUM='                                        
         L     RE,UTL              GET UTL ENTRY ADDRESS                        
         ZIC   R0,4(RE)            GET SYSTEM NUMBER                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  D.DLCBFLD+8(3),DUB                                               
*                                                                               
         MVI   D.DLCBTYP,C'T'                                                   
         MVI   D.DLCBACT,DLCBPUT                                                
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 (RF),(R1)                                                        
*                                                                               
         L     RE,=A(AGYTAB)                                                    
         OC    0(AGYTABL,RE),0(RE)        TEST DATA IN AGYTAB                   
         BZ    BADREQ                                                           
*                                                                               
         L     R4,=A(AGYTAB)                                                    
         LA    R5,16                                                            
BL22     CLI   0(R4),C' '                                                       
         BNH   BL24                                                             
         LA    R6,D.DLCBFLD                                                     
         MVC   0(7,R6),=C'*AGENCY'               RECORD ID                      
         LA    R6,8(R6)                                                         
*                                                                               
         MVC   0(2,R6),0(R4)                     AGENCY ALPHA                   
         LA    R6,3(R6)                                                         
         GOTO1 HEXOUT,DMCB,2(R4),(R6),1,=C'TOG'  AGENCY BINARY                  
         LA    R6,3(R6)                                                         
*                                                                               
         MVC   0(2,R6),4(R4)                     CORPID                         
         LA    R6,3(R6)                                                         
*                                                                               
         ZIC   R0,6(R4)                          AGYFLAG                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R6),DUB         DISPLAY AS DECIMAL NUMBER                    
*                                                                               
         MVI   D.DLCBTYP,C'T'                                                   
         MVI   D.DLCBACT,DLCBPUT                                                
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 (RF),(R1)                                                        
*                                                                               
BL24     LA    R4,8(R4)                                                         
         BCT   R5,BL22                                                          
         B     BL30                                                             
*                                                                               
BADREQ   MVC   D.DLCBFLD(30),=C'*ERROR - NO AGENCIES SELECTED'                  
         MVI   D.DLCBTYP,C'T'                                                   
         MVI   D.DLCBACT,DLCBPUT                                                
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 (RF),(R1)                                                        
         GOTO1 AENDREQ                                                          
         DROP  D                                                                
         EJECT                                                                  
*===============================================================*               
* NOW READ HEADER RECORDS FOR SELECTED AGENCIES                 *               
*===============================================================*               
*                                                                               
BL30     GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),=X'000001'   START AT AGY0                                
         CLI   QOPT1,C'Y'          TEST RUN ONE AGENCY ONLY                     
         BNE   BL40                                                             
* FIND THE AGENCY IN THE TABLE                                                  
         LA    R0,16                                                            
         L     R1,=A(AGYTAB)                                                    
*                                                                               
         CLI   0(R1),C' '                                                       
         BH    BL35                                                             
         LA    R1,L'AGYTAB(R1)                                                  
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
*                                                                               
BL35     ZIC   R0,2(R1)            GET AGENCY NUMBER                            
         SLL   R0,4                LEFT ALIGN                                   
         STC   R0,KEY+1                                                         
*                                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   BL40                                                             
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD     FORCE AGENCY/MEDIA                           
         GOTO1 CLPACK,DMCB,QPRD,KEY+2                                           
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    BL50                                                             
         DC    H'0'                                                             
*                                                                               
BL40     GOTO1 HIGH                                                             
         B     BL50                                                             
*                                                                               
BL45     MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
BL50     DS    0H                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   *+14                                                             
         CLC   KEY(4),KEYSAVE      ONE CLIENT TEST                              
         BNE   BL100                                                            
*                                                                               
         CLC   KEY(2),KEYSAVE      TEST CHANGE OF AGY/MED                       
         BE    BL60                NO                                           
*                                                                               
         CLI   KEY,0               TEST STILL IN HEADER RECORDS                 
         BNE   BL100                                                            
*                                                                               
         ZIC   RE,KEY+1            GET AGENCY NUMBER                            
         SRL   RE,4                DROP MEDIA BITS                              
         BCTR  RE,0                                                             
         SLL   RE,3                X 8                                          
         A     RE,=A(AGYTAB)                                                    
*                                                                               
         CLI   0(RE),C' '          TEST AGENCY IN ACTIVE LIST                   
         BH    BL55                YES                                          
*                                                                               
         CLI   QOPT1,C'Y'          TEST PROCESS ONLY ONE AGENCY                 
         BE    BL100               YES - DONE                                   
* SKIP THIS AGENCY                                                              
         ZIC   RE,KEY+1            GET AGY/MED                                  
         N     RE,=X'000000F0'     DROP MEDIA BITS                              
         LA    RE,X'10'(RE)        SET FOR NEXT AGY                             
         XC    KEY,KEY                                                          
         STC   RE,KEY+1                                                         
         CLI   KEY+1,0             TEST PASSED AGY 0F                           
         BE    BL100                                                            
         B     BL40                                                             
*                                                                               
BL55     MVC   THISAGY,0(RE)                                                    
         MVC   THISCORP,4(RE)      MOVE CORP ID SAVED IN TABLE                  
         MVI   PRDBILL,C'N'        SET TO SUPPRESS PRODUCT CODES                
         TM    6(RE),AGYPRDQ       TEST AGENCY OPTION                           
         BZ    *+8                                                              
         MVI   PRDBILL,C'Y'                                                     
*                                                                               
BL60     OC    KEY+4(9),KEY+4      TEST CLIENT RECORD - SHOULD BE NULL          
         BNZ   BL70                NOT A CLIENT HEADER RECORD                   
*                                                                               
         MVI   CLTSW,C'N'          RESET CLIENT ACTIVE SWITCH FOR SPOT          
*                                                                               
         LA    RE,SUBMEDTB         NETPAK SUB-MEDIA TABLE                       
         USING SUBMEDTD,RE                                                      
*                                                                               
         MVI   SUBMEDTR+1,C'N'    BE SURE IT'S N TO START                       
         CLI   QOPT3,C'D'    SEE IF REPORTING SUBMEDIA D SEPARATELY             
         BNE   *+8                                                              
         MVI   SUBMEDTR+1,C'D'    ALTER REPORTING MEDIA TO D                    
         CLI   QOPT3,C'B'    OR BOTH D +O                                       
         BNE   *+8                                                              
         MVI   SUBMEDTR+1,C'D'    ALTER REPORTING MEDIA TO D                    
*                                                                               
         MVI   SUBMEDTO+1,C'N'    BE SURE IT'S N TO START                       
         CLI   QOPT3,C'O'    SEE IF REPORTING SUBMEDIA D SEPARATELY             
         BNE   *+8                                                              
         MVI   SUBMEDTO+1,C'O'    ALTER REPORTING MEDIA TO O                    
         CLI   QOPT3,C'B'    OR BOTH D +O                                       
         BNE   *+8                                                              
         MVI   SUBMEDTO+1,C'O'    ALTER REPORTING MEDIA TO O                    
*                                                                               
         CLI   0(RE),X'FF'         EOT?                                         
         BE    *+16                YES: CONTINUE                                
         MVI   SUBMEDCL,C'N'       CLIENT RECORD NOT YET GENERATED              
         LA    RE,SUBMEDLQ(RE)     BUMP TO NEXT SUB-MEDIA                       
         B     *-16                                                             
         DROP  RE                                                               
*                                                                               
         GOTO1 GETCLT                                                           
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),THISCLT                            
*                                                                               
         ZIC   RE,CKEYAM                 GET AGY/MED                            
         N     RE,=X'0000000F'           DROP AGENCY                            
         LA    RE,MEDTAB-1(RE)                                                  
         MVC   THISMED,0(RE)             MOVE MEDIA CODE                        
         MVC   THISOFFC,COFFICE          MEDIA OFFICE CODE                      
         MVI   THISOFFC+1,C' '                                                  
*                                                                               
         XC    OFCWORK,OFCWORK                                                  
         LA    R4,OFCWORK                                                       
         USING OFFICED,R4                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,THISAGY                                                   
         MVC   OFCOFC,COFFICE                                                   
         L     RF,ADCONLST                                                      
         L     RF,VOFFICER-SPADCONS(RF)                                         
         GOTO1 (RF),DMCB,(C'2',OFFICED),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   *+10                                                             
         MVC   THISOFFC,OFCOFC2    USE 2 CHAR OFFICE IF AVAILABLE               
         DROP  R4                                                               
                                                                                
* BUILD A CLIENT NAME RECORD IN CASE THERE IS BILLING                           
         XC    SVCLDATA,SVCLDATA                                                
         MVC   SVCLAGY,THISAGY                                                  
         MVI   SVCLSYS,C'S'                                                     
         MVC   SVCLMED,THISMED                                                  
*                                                                               
         L     RE,VMASTC                                                        
         CLI   MCNETPAK-MASTD(RE),C'Y'                                          
         BNE   *+12                                                             
         MVI   SVCLSYS,C'N'                                                     
         MVI   SVCLMED,C' '        SUPPRESS MEDIA FOR NET CLT REC               
*                                                                               
         MVC   SVCLCODE,THISCLT                                                 
         MVC   SVCLNAME,CNAME                                                   
*                                                                               
         L     RF,=A(TRFCHK)                                                    
         CLI   SVCLSYS,C'N'        TEST NETPAK                                  
         BNE   *+8                                                              
         L     RF,=A(NTRFCHK)      CHECK TRAFFIC DURING PERIOD                  
         BASR  RE,RF                                                            
*&&DO                                                                           
         LA    R4,XSPKEY                                                        
         USING UNBRECD,R4                                                       
         XC    UNBKEY,UNBKEY       DDS UNBILLING KEY                            
         MVI   UNBKSYS,UNBKSYSQ                                                 
         MVI   UNBKSTYP,UNBKSTYQ                                                
         MVC   UNBKAM,CKEYAM       AGY/MED                                      
         MVC   UNBKCLT,CKEYCLT     CLIENT                                       
         MVC   UNBKUNYR,PREVYRMO   PREVIOUS YEAR/MONTH                          
         MVC   UNBKUNMO,PREVYRMO+1                                              
         MVC   XSPKEYSV,XSPKEY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',XSPKEYSV,XSPKEY                   
*                                                                               
BL62     DS    0H                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   XSPKEY(UNBKUNDY-UNBKEY),XSPKEYSV SAME AGY/MED/CLT/YR/MO?         
         BNE   BL65                NO: NONE TO PROCESS                          
*                                                                               
*                                                                               
*          BUILD SORT RECORD                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',0,XSPKEY                          
         B     BL62                                                             
*                                                                               
BL65     DS    0H                                                               
*&&                                                                             
         B     BL45                                                             
         DROP  R6                                                               
*                                                                               
MEDTAB   DC    C'TRNX'                                                          
         EJECT                                                                  
BL70     MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         CLC   KEY(4),KEYSAVE      SAME A-M/CLT                                 
         BNE   BL50                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING BILLRECD,R6                                                      
*                                                                               
         CLC   BBILLYM,BKEYMBIL    TEST BILLED THIS MONTH?                      
         BNE   BL95                NO                                           
*                                                                               
         GOTO1 GETBUY              READ BILL RECORD                             
*                                                                               
         L     R6,ADBUY                                                         
         CLI   BRETAIL,X'41'       IGNORE RETAIL BILLS                          
         BE    BL95                                                             
         TM    BILSTAT,BSTTAORQ    IGNORE AOR BILLS                             
         BO    BL95                                                             
*                                                                               
* MAKE SURE BILLED THIS DECADE                                                  
         CLC   BDATE(4),QSTART     MATCH BILLING YY/MM                          
         BNE   BL95                                                             
*                                                                               
         MVC   THISPRD,SPACES                                                   
         CLI   PRDBILL,C'Y'        TEST OUTPUT PRODUCT DETAIL                   
         BNE   *+10                                                             
         MVC   THISPRD,BKEYPRD                                                  
*                                                                               
         SR    R0,R0                                                            
**NOP**  IC    R0,BKEYEST          SUPPRESS ESTIMATE DETAIL                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISEST,DUB                                                      
*                                                                               
         MVC   DUB(2),BKEYYSRV                                                  
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(3,DUB),(20,WORK)                                    
         MVC   THISSVYM(4),WORK     MOVE YYYY                                   
         MVI   THISSVYM+4,C'/'                                                  
         MVC   THISSVYM+5(2),WORK+4 MM                                          
*                                                                               
         L     RE,VMASTC                                                        
         CLI   MCNETPAK-MASTD(RE),C'Y'                                          
         BE    BL75                                                             
*                                                                               
* ADJUST BILLED AMOUNTS (COMMISSION ONLY BILLS)                                 
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'B',(R6)),SPBVALD,0                                
*                                                                               
         ZAP   THISBGRS,SPBVGRSP                                                
         ZAP   THISBNET,SPBVNETP                                                
*                                                                               
*==========================================================*                    
* PUT CLIENT DATA TO FILE AND BILL DATA TO SORT            *                    
*==========================================================*                    
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',THISREC                                  
*                                                                               
         CLI   CLTSW,C'Y'          TEST OUTPUT CLIENT DATA YET                  
         BE    BL95                YES                                          
         L     R1,=A(CLTFILE)                                                   
         LA    R0,SVCLDATA                                                      
         PUT   (1),(0)                                                          
         MVI   CLTSW,C'Y'                                                       
*                                                                               
         B     BL95                                                             
*                                                                               
* NETPAK BILLS: GET GROSS AND NET BY SUB-MEDIA                                  
*                                                                               
BL75     DS    0H                                                               
         LA    R3,SUBMEDTB         NETPAK SUB-MEDIA TABLE                       
         USING SUBMEDTD,R3                                                      
BL80     CLI   0(R3),X'FF'         EOT?                                         
         BE    BL95                                                             
*                                                                               
         ZIC   R0,SUBMEDIA         SUB-MEDIA                                    
         GOTO1 SPBVAL,DMCB,(C'B',(R6)),((R0),SPBVALD),0                         
         DROP  R6                                                               
*                                                                               
         CP    SPBVGRSP,=P'0'      MIGHT NOT BE ANY DATA...                     
         BNE   *+14                ...FOR THIS SUB-MEDIA                        
         CP    SPBVNETP,=P'0'                                                   
         BE    BL90                                                             
         ZAP   THISBGRS,SPBVGRSP   GROSS FOR THIS SUB-MEDIA ONLY                
         ZAP   THISBNET,SPBVNETP   NET FOR THIS SUB-MEDIA ONLY                  
         MVC   THISMED,SUBMEDAS    ADD VALUES TO THIS SUB-MEDIA                 
         MVC   SVCLMED,SUBMEDAS    SET MEDIA FOR CLIENT RECORD                  
         GOTO1 =V(SORTER),DMCB,=C'PUT',THISREC                                  
*                                                                               
         CLI   SUBMEDCL,C'Y'       DID WE OUTPUT CLIENT DATA YET?               
         BE    BL90                                                             
         L     R1,=A(CLTFILE)      NO                                           
         LA    R0,SVCLDATA                                                      
         PUT   (1),(0)                                                          
*                                                                               
         LA    RF,SUBMEDTB         NETPAK SUB-MEDIA TABLE                       
CLTN     USING SUBMEDTD,RF                                                      
BL85     CLI   0(RF),X'FF'         EOT?                                         
         BE    BL90                                                             
         CLC   SUBMEDAS,CLTN.SUBMEDAS FOR EACH SUBMEDIA GROUPED WITH...         
         BNE   *+8                    ...THIS SUBMEDIA, REMEMBER...             
         MVI   CLTN.SUBMEDCL,C'Y'     ...THAT WE OUTPUT THIS CLIENT             
         LA    RF,SUBMEDLQ(RF)     BUMP TO NEXT SUB-MEDIA                       
         B     BL85                                                             
         DROP  CLTN                                                             
*                                                                               
BL90     DS    0H                                                               
         LA    R3,SUBMEDLQ(R3)     BUMP TO NEXT SUB-MEDIA                       
         B     BL80                                                             
*                                                                               
BL95     B     BL70                                                             
         DROP  R3                                                               
         EJECT                                                                  
*================================================================*              
* GET RECORDS FROM SORT AND ADD BILLED AMOUNTS IF KEYS EQUAL     *              
*================================================================*              
*                                                                               
BL100    GOTO1 =V(SORTER),DMCB,=C'GET'   GET FIRST RECORD                       
         ICM   R6,15,4(R1)                                                      
         BNZ   BL101                                                            
         MVC   P(30),=C'** ERROR ** NO BILLING ON FILE'                         
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
BL101    MVC   NEWREC,0(R6)                                                     
         MVC   THISREC,NEWREC                                                   
*                                                                               
BL102    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R6,15,4(R1)                                                      
         BNZ   BL104                                                            
         MVI   NEWREC,X'FF'                                                     
         B     BL106                                                            
*                                                                               
BL104    MVC   NEWREC,0(R6)        MOVE RECORD SO CAN SEE IT                    
*                                                                               
BL106    CLC   THISKEY,NEWREC      TEST SAME KEYS                               
         BNE   BL120                                                            
* KEYS EQUAL, ADD BILLED AMOUNTS                                                
         ZAP   DUB,THISBGRS-THISREC+NEWREC(L'THISBGRS)                          
         AP    THISBGRS,DUB                                                     
*                                                                               
         ZAP   DUB,THISBNET-THISREC+NEWREC(L'THISBNET)                          
         AP    THISBNET,DUB                                                     
         B     BL102                                                            
         EJECT                                                                  
*=============================================================*                 
* OUTPUT BILL DATA TO PRTQUE REPORT                           *                 
*=============================================================*                 
*                                                                               
BL120    ZAP   DOUBLE,THISBGRS     CONVERT DOLLARS TO EBCDIC                    
         EDIT  (P8,DOUBLE),THISBGRS,MINUS=YES,ZERO=NOBLANK                      
*                                                                               
         ZAP   DOUBLE,THISBNET                                                  
         EDIT  (P8,DOUBLE),THISBNET,MINUS=YES,ZERO=NOBLANK                      
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         LA    R4,THISBTAB                                                      
*                                                                               
         MVC   THISSYS,SVCLSYS     SET SYSTEM CODE                              
*                                                                               
BL140    DS    0H                                                               
         L     RE,0(R4)            GET DATA ADDR                                
         ZIC   RF,4(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(RE)                                                 
*                                                                               
         MVC   DLCBTYP(1),5(R4)                                                 
         MVI   DLCBACT,DLCBPUT                                                  
*                                                                               
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         LA    R4,L'THISBTAB(R4)                                                
         CLI   0(R4),X'FF'                                                      
         BNE   BL140                                                            
*                                                                               
         MVI   DLCBACT,DLCBEOL                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         MVC   THISREC,NEWREC      SAVE 'NEW' RECORD                            
         CLI   THISREC,X'FF'                                                    
         BNE   BL102                                                            
* OUTPUT A GOOD EOF MESSAGE                                                     
         MVC   DLCBFLD(11),=C'*END OF RUN'                                      
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         MVI   DLCBACT,DLCBEOL                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R1                                                               
         EJECT                                                                  
*=============================================================*                 
* RUNLAST - OUTPUT CONTENTS OF CLTFILE                        *                 
*           AND CLOSE THE DOWNLOAD REPORT                                       
*=============================================================*                 
*                                                                               
BL300    L     RE,=A(AGYTAB)                                                    
         OC    0(AGYTABL,RE),0(RE)        TEST DATA IN AGYTAB                   
         BZ    BL330                      NO - EXIT                             
*                                                                               
         CLOSE CLTFILE                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
* NOW READ CLTFILE AND DOWNLOAD CONTENTS                                        
         OPEN  (CLTFILE,INPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         MVI   DLCBACT,C'I'        INITIALIZE CLIENT NAME REPORT                
         GOTO1 =V(DLFLD),(R1)                                                   
         DROP  R1                                                               
*                                                                               
BL310    L     R1,=A(CLTFILE)                                                   
         LA    R0,SVCLDATA                                                      
         GET   (1),(0)                                                          
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
*                                                                               
         LA    R4,SVCLTAB                                                       
*                                                                               
BL315    MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,5(R4)                                                    
*                                                                               
         L     RE,0(R4)            GET DATA ADDR                                
         ZIC   RF,4(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(RE)                                                 
*                                                                               
         GOTO1 =V(DLFLD),(R1)                                                   
*                                                                               
         LA    R4,L'SVCLTAB(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   BL315                                                            
*                                                                               
         MVI   DLCBACT,DLCBEOL                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 =V(DLFLD),(R1)                                                   
         B     BL310               GET NEXT RECORD                              
         DROP  R1                                                               
*                                                                               
* END-OF-INPUT FILE                                                             
*                                                                               
BL320    CLOSE CLTFILE                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BL330    LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         MVI   DLCBACT,C'R'        SET E-O-R                                    
         GOTO1 =V(DLFLD),(R1)                                                   
         DROP  R1                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*==============================================================*                
* CHECK SPOT TRAFFIC DURING PERIOD                             *                
* IF ANY TRAFFIC INSTRUCTIONS DONE DURING THIS BILLING MONTH,  *                
* TRAFFIC FLAG WILL BE ON FOR ALL RECORDS FOR THIS CLIENT      *                
*==============================================================*                
TRFCHK   NTR1                                                                   
         L     RE,UTL                                                           
         MVC   SVUTL,4(RE)                                                      
         MVC   4(1,RE),RCUTLTRF                                                 
*                                                                               
         MVI   THISTRF,C'0'        DEFAULT                                      
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,SAVEKEY                                                       
         USING BILLRECD,R6                                                      
         LA    R7,KEY                                                           
         USING INSRECD,R7                                                       
*                                                                               
         MVC   INSKID,=X'0A24'                                                  
         MVC   INSKAM,BKEYAM       AGENCY/MEDIA                                 
         MVC   INSKCLT,BKEYCLT     CLIENT                                       
*                                                                               
         L     RE,ADCLT                                                         
         USING CLTHDRD,RE                                                       
         OC    CMCLTCOD,CMCLTCOD   TRAFFIC CLIENT                               
         BZ    *+10                                                             
         MVC   INSKCLT,CMCLTCOD    YES - USE IT                                 
         DROP  RE                                                               
*                                                                               
         GOTO1 HIGH                                                             
         B     TRF20                                                            
*                                                                               
TRF10    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
TRF20    DS    0H                                                               
         CLC   KEY(5),KEYSAVE      SAME AGENCY/MEDIA/CLIENT?                    
         BNE   TRFCHKX                                                          
*                                                                               
         L     R7,ADBUY                                                         
         ST    R7,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         LA    RE,24(R7)                                                        
*                                                                               
         CLI   0(RE),X'10'         INSTRUCTION ELEMENT                          
         BNE   TRF24                                                            
*                                                                               
         USING INSDTAEL,RE                                                      
         CLC   BQSTARTP,INSDATE    WITHIN BILLING MONTH?                        
         BH    TRF24               NO - GET NEXT RECORD                         
         CLC   BQENDP,INSDATE                                                   
         BL    TRF24                                                            
         MVI   THISTRF,C'1'                                                     
         B     TRFCHKX                                                          
         DROP  RE                                                               
*                                                                               
TRF24    SR    R0,R0                                                            
         ICM   R0,1,1(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RE,R0                                                            
         CLI   0(RE),0             AT EOR, READ NEXT RECORD                     
         BNE   TRF10                                                            
*                                                                               
TRFCHKX  DS    0H                                                               
         L     RE,UTL                                                           
         MVC   4(1,RE),SVUTL                                                    
* RESTORE KEY FOR SEQUENTIAL READING ON SPTDIR                                  
         XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
*                                                                               
         B     EXIT                                                             
         DROP  R6,R7                                                            
         EJECT                                                                  
*===============================================================*               
* READ REV RECS FOR NETPAK TRAFFIC BILLING                      *               
*===============================================================*               
*                                                                               
NTRFCHK  NTR1                                                                   
         MVI   THISTRF,C'0'        DEFAULT                                      
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,SAVEKEY                                                       
         USING BILLRECD,R6                                                      
         LA    R7,KEY                                                           
         USING REVRECD,R7                                                       
*                                                                               
         MVI   REVKID,X'21'        SET FOR 1ST REVISION KEY                     
         MVC   REVKAM,BKEYAM       AGENCY/MEDIA                                 
         MVC   REVKCLT,BKEYCLT     CLIENT                                       
         MVC   KEYSAVE,KEY                                                      
*                                                                               
* DO FIRST READ                                                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,=CL8'UNTDIR',KEYSAVE,KEY                     
         B     NTRF20                                                           
*                                                                               
NTRF10   GOTO1 DATAMGR,DMCB,DMRSEQ,=CL8'UNTDIR',KEY,KEY                         
*                                                                               
NTRF20   CLC   KEY(4),KEYSAVE      TEST E-O-F                                   
         BNE   NTRFX                                                            
*                                                                               
         TM    REVKPER,X'80'       THIS WEEKLY PERIOD                           
         BO    NTRF30                                                           
         CLC   REVKPER,STRTBILL    THIS YR/MO TO START                          
         BL    NTRF10                                                           
         CLC   REVKPER,ENDBILL     THIS YR/MO TO END                            
         BH    NTRF10                                                           
         B     NTRF40                                                           
*                                                                               
NTRF30   CLC   REVKPER,BQSTARTP    THIS YR/MO TO START                          
         BL    NTRF10                                                           
         CLC   REVKPER,BQENDP      THIS YR/MO TO END                            
         BH    NTRF10                                                           
*                                                                               
NTRF40   MVI   THISTRF,C'1'                                                     
*                                                                               
* RESTORE KEY FOR SEQUENTIAL READING ON SPTDIR                                  
NTRFX    XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
         DROP  R6,R7                                                            
         EJECT                                                                  
*==============================================================*                
* USER PRINT ROUTINE EXIT CALLED BY DLFLD                      *                
* ALL DATA PRINTED HERE GOES ON PAGE 2                         *                
*==============================================================*                
BLPRINT  NTR1                                                                   
         MVI   LINE,0              FORCE NO PAGE BREAK                          
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
         B     EXIT                                                             
         EJECT                                                                  
* THESE FIELDS USED TO GET WIDE PRINT LINE OVERRIDES                            
* THEY GET MOVED INTO THE DLCB TO OVERRIDE MAXLINE                              
*                                                                               
XTENSION DS    0D                  EXTENSION AREA FOR DLFLD                     
         DC    H'132'              MAX LINE WIDTH                               
         DC    C' '                FIELD DELIMITER CHR                          
         DC    C'"'                END OF TEXT FIELD DELIMITER                  
         DC    C''''               END OF TEXT CHR ALTERNATE                    
         DC    X'5E'               END OF LINE CHAR - SEMICOLON                 
         DC    C':'                END OF REPORT CONTROL CHR                    
*                                                                               
         DS    0D                                                               
SVCLTAB  DS    0XL6                                                             
         DC    AL4(THISBEAC),AL1(L'THISBEAC),C'T'                               
         DC    AL4(SVCLAGY),AL1(L'SVCLAGY),C'T'                                 
         DC    AL4(SVCLSYS),AL1(L'SVCLSYS),C'T'                                 
         DC    AL4(SVCLMED),AL1(L'SVCLMED),C'T'                                 
         DC    AL4(SVCLCODE),AL1(L'SVCLCODE),C'T'                               
         DC    AL4(SVCLNAME),AL1(L'SVCLNAME),C'T'                               
         DC    X'FF'                                                            
THISBEAC DC    C'C'                                                             
*                                                                               
         DS    0D                                                               
THISBTAB DS    0XL6                                                             
         DC    AL4(THISBEAB),AL1(L'THISBEAB),C'T'                               
         DC    AL4(THISCORP),AL1(L'THISCORP),C'T'                               
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T'                                 
         DC    AL4(THISSYS),AL1(L'THISSYS),C'T'                                 
         DC    AL4(THISMED),AL1(L'THISMED),C'T'                                 
         DC    AL4(THISOFFC),AL1(L'THISOFFC),C'T'                               
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T'                                 
         DC    AL4(THISPRD),AL1(L'THISPRD),C'T'                                 
         DC    AL4(THISEST),AL1(L'THISEST),C'N'                                 
         DC    AL4(THISBLYM),AL1(L'THISBLYM),C'N'                               
         DC    AL4(THISSVYM),AL1(L'THISSVYM),C'N'                               
         DC    AL4(THISBGRS),AL1(L'THISBGRS),C'N'                               
         DC    AL4(THISBNET),AL1(L'THISBNET),C'N'                               
         DC    AL4(THISTRF),AL1(L'THISTRF),C'N'                                 
         DC    X'FF'                                                            
THISBEAB DC    C'B'                                                             
         SPACE 3                                                                
SVUTL    DS    X                                                                
*                                                                               
BBILLYM  DS    XL1                 BILLING YEAR/MONTH FILTER                    
PRDBILL  DS    CL1                 OUTPUT PRODUCT DETAILS                       
*                                                                               
STRTBILL DS    XL3                 START DATE IN BINARY                         
ENDBILL  DS    XL3                 END DATE IN BINARY                           
*&&DO                                                                           
PREVYRMO DS    XL2                 PREVIOUS YEAR/MONTH IN BINARY                
*&&                                                                             
*                                                                               
SVCLDATA DS    0XL32                                                            
SVCLAGY  DS    CL2                                                              
SVCLSYS  DS    CL1                                                              
SVCLMED  DS    CL1                                                              
SVCLCODE DS    CL3                                                              
SVCLNAME DS    CL20                                                             
         DS    CL5                 SPARE                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*THISREC'                                                    
THISREC  DS    0CL80                                                            
THISKEY  DS    0CL31                                                            
THISCORP DS    CL2     +00         CORPORATE ID                                 
THISAGY  DS    CL2     +02         ALPHA AGY                                    
THISSYS  DS    CL1     +04         SYSTEM CODE (SPOT/PRINT/ACC)                 
THISMED  DS    CL1     +05         MEDIA CODE                                   
THISOFFC DS    CL2     +06         MEDIA OFFICE CODE                            
THISCLT  DS    CL3     +08         ALPHA CLIENT CODE                            
THISPRD  DS    CL3     +11         ALPHA PRODUCT CODE                           
THISEST  DS    CL3     +14         NUMERIC ESTIMATE                             
*                                                                               
THISBLYM DS    CL7     +17         BILLING Y/M  YYYY/MM                         
THISSVYM DS    CL7     +24         Y/M OF SERVICE YYYY/MM                       
*                                                                               
THISBGRS DS    CL12    +31         GROSS BILLING IN PENNIES                     
THISBNET DS    CL12    +43         NET BILLING IN PENNIES                       
THISTRF  DS    CL1     +55         SPOT TRAFFIC FLAG                            
         DS    CL25                SPARE                                        
*                                                                               
         SPACE 2                                                                
SUBMEDTB DS    0C                  NETPAK SUB-MEDIA TABLE                       
         DC    C'NN '              NETWORK                                      
         DC    C'CC '              CABLE                                        
         DC    C'SS '              SYNDICATION                                  
SUBMEDTO DC    C'ON '              OTHER (ALLOCATED TO NETWORK)                 
SUBMEDTR DC    C'DN '              NETWORK RADIO (ALLOCATED TO NETWORK)         
*                                                                               
*        NOTE THAT QOPT3 MAY ALTER THE ALLOCATED MEDIA FOR THE                  
*        SUBMEDTO AND SUBMEDTR ENTRIES                                          
*                                                                               
         DC    C'VC '              REPORT SUBMEDIA V UNDER C                    
*                                                                               
         DC    X'FFD540'          EOT    X'FF',C'N '                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
DLCB     DS    XL256                                                            
         DC    CL8'*NEWREC*'                                                    
NEWREC   DS    CL80                                                             
SAVEKEY  DS    CL13                                                             
         DS    0D                                                               
XSPKEY   DS    XL40                                                             
         DS    0D                                                               
XSPKEYSV DS    XL40                                                             
         DS    0D                                                               
OFCWORK  DS    XL64                                                             
         EJECT                                                                  
* NOTE THERE IS NO DSECT ON THE FOLLOWING INCLUDE                               
       ++INCLUDE SPBVALD                                                        
         EJECT                                                                  
SORTCARD DC    CL80'SORT FIELDS=(1,31,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=80'                                    
*                                                                               
CLTFILE  DCB   DDNAME=CLTFILE,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=32,MACRF=(GM,PM),EODAD=BL320                               
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*AGYTAB*'                                                      
AGYTAB   DS    16XL8               AGYA(2)/AGYB(1)/(1)/AGYCID(2)/AGYF           
AGYTABL  EQU   (*-AGYTAB)                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
*                                                                               
PLINED   DSECT                                                                  
*                                                                               
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PSYSTEM  DS    CL1                                                              
PMEDIA   DS    CL1                                                              
         DS    CL1                                                              
POFFICE  DS    CL2                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PBILLYR  DS    CL4                                                              
         DS    CL1                                                              
PBILLMON DS    CL2                                                              
         DS    CL1                                                              
PYRSVC   DS    CL4                                                              
         DS    CL1                                                              
PMNSVC   DS    CL2                                                              
         DS    CL1                                                              
PBILLNUM DS    CL6                                                              
         DS    CL1                                                              
PGROSS   DS    CL10                                                             
         DS    CL1                                                              
PNET     DS    CL10                                                             
         DS    CL1                                                              
PACTUAL  DS    CL10                                                             
         SPACE 3                                                                
SUBMEDTD DSECT                                                                  
SUBMEDIA DS    C                   TRUE SUB-MEDIA CODE                          
SUBMEDAS DS    C                   ALLOCATE TO THIS SUB-MEDIA                   
SUBMEDCL DS    C                   CLIENT RECORD GENERATED? (Y/N)               
SUBMEDLQ EQU   *-SUBMEDTD                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
       ++INCLUDE SPTRNREV                                                       
         EJECT                                                                  
       ++INCLUDE SPGENUNBIL                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088SPREPBL02 12/19/13'                                      
         END                                                                    
