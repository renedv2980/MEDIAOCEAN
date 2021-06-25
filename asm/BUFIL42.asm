*          DATA SET BUFIL42    AT LEVEL 075 AS OF 08/14/02                      
*PHASE T50242A                                                                  
*INCLUDE MEDGET                                                                 
         TITLE 'T50242 - BUDGET EXTRACT - SPOTPAK'                              
T50242   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         NMOD1 0,T50242,RA,RR=RE                                                
         ST    RE,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R0,VADUMMY                                                       
         ST    R0,NEXTADDR         SET NEXT AVAILABLE CORE ADDRESS              
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A5F'                                        
         MVC   GETRATE,DMCB        GETRATE IS NOW CORE-RES                      
*                                                                               
         L     R3,ATWA                                                          
         USING T502FFD,R3                                                       
         MVC   VBUFFALO,TWAVBUFF                                                
*                                                                               
         L     RE,TWADCONS                                                      
         USING TWADCOND,RE                                                      
*                                                                               
         MVC   CLPACK,TCLPACK                                                   
         MVC   BINSRCH,TBINSRCH                                                 
         DROP  RE                                                               
*                                                                               
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 VBUFFALO,DMCB,=C'SET',BUFFBUFF                                   
*                                                                               
* FIND DATA TYPES TO PROCESS                                                    
*                                                                               
         LA    R1,SVDTYPES                                                      
         USING SVDTD,R1                                                         
         LA    R0,MAXDTYP          R0=COUNTER                                   
         XC    DATAINDS,DATAINDS   RESET EXTRACT SWITCH                         
         SR    R4,R4               R4=N'EXTRACT TYPES                           
         LA    R5,EXTYPS           R5=A(EXTRACT TYPE TABLE)                     
*                                                                               
SP2      CLI   SVDTEX,0            TEST FOR EOT                                 
         BE    SP6                 YES                                          
         LA    RE,EXTTBL           RE=A(VALID DATA TYPE TABLE)                  
         LA    RF,EXTYPES          RF=N'VALID DATA TYPES                        
*                                                                               
SP3      CLC   SVDTEX,0(RE)        TEST IF VALID FOR SPOT EXTRACT               
         BE    SP4                 YES                                          
         LA    RE,L'EXTTBL(RE)                                                  
         BCT   RF,SP3                                                           
         B     SP5                                                              
*                                                                               
SP4      OC    DATAINDS,1(RE)      UPDATE CUMULATIVE MASK                       
         MVC   0(1,R5),0(RE)       ADD EXTRACT TYPE                             
         LA    R4,1(R4)            INCREMENT EXTRACT TYPE COUNT                 
         LA    R5,1(R5)            AND NEXT TABLE POSITION                      
*                                                                               
SP5      LA    R1,SVDTL(R1)        NEXT DATA TYPE                               
         BCT   R0,SP2                                                           
         DROP  R1                                                               
*                                                                               
SP6      LTR   R4,R4               TEST ANYTHING TO EXTRACT                     
         BZ    EXIT                                                             
         STC   R4,NEXTYPS          SAVE COUNTER                                 
         TM    DATAIND1,EXGOALB    TEST FOR GOAL EXTRACT                        
         BZ    *+8                                                              
         BAS   RE,BLDMON           BUILD TABLE OF MONDAYS                       
*                                                                               
         L     R2,ARULDATA         GET ADDRESS OF FIRST RULE                    
         USING QRD,R2                                                           
*                                                                               
         GOTO1 =V(MEDGET),DMCB,(QRMED,QRAGYC),DATAMGR,WORK,RR=RELO              
         CLI   DMCB+8,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   QBAGYMD,WORK        SAVE AGY/MEDIA CODE                          
         MVC   AGYMED,QBAGYMD      AND SAVE AGAIN IN STORAGE                    
* NOW READ THE AGENCY HEADER TO GET THE COUNTRY !                               
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),QRAGYC                                                  
         BAS   RE,SPHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         BAS   RE,SPGET                                                         
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
         MVC   SVAPROF7,AGYPROF+7                                               
         MVC   SVQRMED,QRMED       MAKE MEDIA AVAILABLE TO 00                   
         DROP  R6                                                               
         SPACE 1                                                                
* READ CLIENT HEADER                                                            
         SPACE 1                                                                
         GOTO1 CLPACK,DMCB,QRCLT,QBCLT                                          
         MVC   CLT,QBCLT           SAVE PACKED CLIENT CODE                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
         MVC   CKEYAM,AGYMED                                                    
         MVC   CKEYCLT,CLT                                                      
         BAS   RE,SPHIGH                                                        
         CLC   CKEY,KEYSAVE                                                     
         BNE   BADCLT                                                           
         MVC   AIO,AIO3                                                         
         BAS   RE,SPGET                                                         
*                                                                               
         L     R6,AIO3                                                          
         MVC   SVCLNAME,CNAME                                                   
*                                                                               
         XC    B1XPROF,B1XPROF                                                  
         XC    WORK,WORK           GET B1X PROFILE                              
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'FF'-X'40'    MAKE SYSTEM LOWER CASE                       
         MVC   WORK+4(2),QRAGYC    FOR EXTRA PAGE                               
         MVC   WORK+6(1),QRMED                                                  
         MVC   WORK+7(3),QRCLT                                                  
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
         MVI   MGRSW,C'N'          SET MARKET GROUP IN KEY SWITCH               
         CLI   B1XPROF+3,C'A'                                                   
         BL    *+8                 NO                                           
         MVI   MGRSW,C'Y'                                                       
*                                                                               
         MVC   PRD,QRPRD           SET ACTIVE PRODUCT                           
*                                                                               
SP8      ICM   R2,15,QRNEXT                                                     
         BZ    SP8X                                                             
         MVC   QBAGYMD,CKEYAM      PROPAGATE A-M/CLT                            
         MVC   QBCLT,CKEYCLT                                                    
         B     SP8                                                              
         DROP  R6                                                               
*                                                                               
SP8X     L     R2,ARULDATA                                                      
*                                                                               
         XC    PGRKEY,PGRKEY                                                    
         CLI   QRPGR,C' '          TEST PRDGRP REQUEST                          
         BH    SP10                YES                                          
         XC    ALSTPRD,ALSTPRD     CLEAR LAST PRODUCT ENTRY POINTER             
         MVI   ALLSW,YES           INITIALIZE PRODUCT=ALL EXTRACT FLAG          
         CLC   PRD,=C'ALL'         TEST PRODUCT=ALL                             
         BE    SP25                YES-SPECIAL HANDLING                         
         MVI   ALLSW,NO                                                         
         B     SP20                                                             
*                                                                               
SP10     OC    PGRKEY,PGRKEY       TEST FIRST TIME                              
         BNZ   SP12                                                             
*                                                                               
         L     R8,ASPOOLD          SHOW PRODUCTS IN PGR                         
         USING SPOOLD,R8                                                        
         MVC   P+1(14),=C'PRODUCT GROUP='                                       
         MVC   P+15(1),QRPGR                                                    
         MVC   P+16(3),QRPRD                                                    
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
SP11     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D81'                                                  
         MVC   KEY+2(3),QBAGYMD    A-M/CLT                                      
         MVC   KEY+5(1),QRPGR                                                   
         MVC   FULL(3),QRPRD                                                    
         MVI   FULL+3,C'0'                                                      
         PACK  DUB,FULL(5)                                                      
         MVC   KEY+6(2),DUB+5                                                   
         BAS   RE,SPHIGH                                                        
         CLC   KEY(8),KEYSAVE                                                   
         BNE   BADPGR                                                           
         B     SP14                                                             
*                                                                               
SP12     MVC   KEY(13),PGRKEY                                                   
         BAS   RE,SPHIGH                                                        
         BAS   RE,SPSEQ                                                         
         CLC   KEY(8),KEYSAVE                                                   
         BNE   SP900                                                            
*                                                                               
SP14     MVC   PGRKEY,KEY          SAVE ACTUAL KEY                              
         MVC   PRD,KEY+8                                                        
         L     R8,ASPOOLD                                                       
         MVC   P+1(8),=C'PRODUCT='                                              
         MVC   P+9(3),PRD          PRINT THE SPECIFIC PRODUCT                   
         GOTO1 SPOOL,PARAS,(R8)                                                 
         B     SP20                                                             
         SPACE 1                                                                
* VALIDATE PRODUCT                                                              
         SPACE 1                                                                
SP20     L     R6,AIO3                                                          
         LA    R6,CLIST-CLTHDRD(R6)                                             
*                                                                               
SP22     CLC   PRD,0(R6)                                                        
         BE    SP24                                                             
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BNL   SP22                                                             
         B     BADPRD                                                           
*                                                                               
SP24     MVC   BPRD,3(R6)          SAVE BINARY PRD CODE                         
         B     SP30                                                             
*                                                                               
* PRODUCT HANDLING FOR PRODUCT=ALL EXTRACT                                      
*                                                                               
SP25     ICM   R6,15,ALSTPRD       GET POINTER TO LAST ENTRY                    
         BNZ   SP26                                                             
         L     R6,AIO3             SET POINTER TO FIRST ENTRY                   
         LA    R6,CLIST-CLTHDRD(R6)                                             
         SH    R6,=H'4'            BACK UP 1 ENTRY LENGTH                       
*                                                                               
SP26     LA    R6,4(R6)            ADVANCE TO NEXT ENTRY                        
         OC    0(4,R6),0(R6)       TEST FOR EOL                                 
         BZ    SP900               YES-WRAP UP                                  
         ST    R6,ALSTPRD          UPDATE LAST ENTRY POINTER                    
         MVC   PRD,0(R6)           GET PRODUCT CODE                             
         MVC   BPRD,3(R6)                                                       
*                                                                               
SP28     XC    KEY,KEY             NOW READ THE PRODUCT HEADER                  
         LA    R4,KEY                                                           
         USING PRDHDRD,R4                                                       
         MVC   PKEYAM,AGYMED                                                    
         MVC   PKEYCLT,CLT         PACKED CLIENT CODE                           
         MVC   PKEYPRD,PRD                                                      
         BAS   RE,SPHIGH                                                        
         CLC   PKEY,KEYSAVE        TEST PRODUCT HEADER FOUND                    
         BNE   SP26                NO-GET NEXT PRODUCT                          
         DROP  R4                                                               
         EJECT                                                                  
* BUILD LIST OF ESTIMATES COVERING ANY PART OF PLAN PERIOD                      
* AND SAVE THE ESTIMATE FILTER VALUES                                           
         SPACE 1                                                                
SP30     LA    R0,12               CLEAR ESTTAB (3K)                            
         L     R1,AESTTAB                                                       
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         LA    R4,KEY                                                           
         USING PRDHDRD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   PKEYAM,AGYMED                                                    
         MVC   PKEYCLT,CLT                                                      
         MVC   PKEYPRD,PRD                                                      
*                                                                               
SP32     BAS   RE,SPHIGH                                                        
         CLC   PKEY,KEYSAVE        FIND PRDHDR                                  
         BNE   BADPRD                                                           
         DROP  R4                                                               
*                                                                               
SP34     MVC   KEY+8(5),XFF                                                     
         BAS   RE,SPHIGH                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BNE   SP40                                                             
         CLI   KEY+7,0             TEST EST NUM PRESENT                         
         BE    SP34                                                             
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   SP34                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         BAS   RE,SPGET                                                         
*                                                                               
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
*                                                                               
         TM    DATAIND1,EXBAINVB                                                
         BNZ   SP35                YES-SKIP INVOICE DATE CHECK                  
*                                                                               
         CLI   MULTIYR,C'Y'        TEST FOR MULTI-YEAR PLAN                     
         BNE   SP34A               NO                                           
         TM    DATAIND,EXBDATB+EXBNDATB+EXBINVB+EXBNINVB                        
         BNZ   SP35                YES-SKIP DATE OVERLAP TEST                   
         TM    DATAIND1,EXBADATB                                                
         BNZ   SP35                                                             
*                                                                               
SP34A    CLC   ESTART,SVEXTEND     EST START AFTER PLAN END                     
         BH    SP34                                                             
         CLC   EEND,SVEXTST        EST END BEFORE PLAN START                    
         BL    SP34                                                             
         SPACE 1                                                                
* ADD ESTIMATE TO TABLE                                                         
         SPACE 1                                                                
SP35     ZIC   RE,KEY+7            GET EST NUM                                  
         MH    RE,=H'3'            X 3                                          
         A     RE,AESTTAB                                                       
         MVC   0(3,RE),EPROF       SAVE FILTER VALUES                           
         OC    0(3,RE),0(RE)                                                    
         BNZ   *+10                                                             
         MVC   0(3,RE),=C'   '                                                  
         B     SP34                                                             
         EJECT                                                                  
* EXPAND MKTGRP LISTS IF ANY RULES CONTAIN THEM *                               
         SPACE 1                                                                
SP40     DS    0H                                                               
         L     R2,ARULDATA                                                      
SP41     DS    0H                                                               
         OC    QRMGRDSP,QRMGRDSP   TEST MKTGRP LIST SPECIFIED                   
         BZ    SP46                                                             
         SPACE 1                                                                
* SET UP LIST AREA *                                                            
         SPACE 1                                                                
         L     R6,NEXTADDR                                                      
         MVC   0(8,R6),=C'*MGRLST*'                                             
         ST    R2,8(R6)            POINT BACK TO RULE                           
         LA    R6,12(R6)                                                        
         STCM  R6,15,QRADRMGR      SET MGRLIST ADDRESS IN RULE                  
*                                                                               
         SR    R7,R7                                                            
         ICM   R7,3,QRMGRDSP                                                    
         AR    R7,R2               POINT TO LIST OF MKTGRPS                     
         ZIC   R8,0(R7)            GET NUMBER OF MKTGRPS                        
         LA    R7,1(R7)            POINT TO FIRST ENTRY                         
         SPACE 1                                                                
* READ PASSIVE MKTGRP POINTERS *                                                
         SPACE 1                                                                
SP42     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),AGYMED     SET A-M/CLT                                  
         OC    PGRKEY,PGRKEY       TEST BY PRDGRP                               
         BZ    *+10                                                             
         MVC   KEY+5(3),PGRKEY+5                                                
         MVC   KEY+8(1),0(R7)      MGRP ID                                      
         PACK  DUB,1(5,R7)                                                      
         MVC   KEY+9(2),DUB+5                                                   
         BAS   RE,SPHIGH                                                        
         CLC   KEY(11),KEYSAVE                                                  
         BE    SP44                                                             
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+5(3),KEY+5      CLEAR PRDGRP                                 
         BAS   RE,SPHIGH                                                        
         CLC   KEY(11),KEYSAVE                                                  
         BE    SP44                                                             
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         CLI   KEY+8,C'F'          TEST MGRP ID A-F                             
         BNH   BADMGR              WHICH REQUIRE CLIENT                         
         XC    KEY+3(2),KEY+3      CLEAR CLIENT                                 
         BAS   RE,SPHIGH                                                        
         CLC   KEY(11),KEYSAVE                                                  
         BNE   BADMGR                                                           
*                                                                               
SP44     MVC   0(2,R6),KEY+11      ADD MARKET TO LIST                           
         LA    R6,2(R6)            BUMP LIST POINTER                            
         BAS   RE,SPSEQ                                                         
         CLC   KEY(11),KEYSAVE                                                  
         BE    SP44                                                             
         XC    0(2,R6),0(R6)       SET E-O-L FLAG                               
         LA    R7,5(R7)            NEXT MKTGRP                                  
         BCT   R8,SP42                                                          
*                                                                               
         LA    R6,2(R6)            INCLUDE EOL IN LIST                          
         ST    R6,NEXTADDR                                                      
*                                                                               
SP46     ICM   R2,15,QRNEXT        ADVANCE TO NEXT RULE                         
         BNZ   SP41                AND CONTINUE                                 
         EJECT                                                                  
SP50     DS    0H                                                               
         SPACE 1                                                                
* PROCESS BUY RECORDS *                                                         
         SPACE 1                                                                
SP60     TM    DATAIND,EXORDB+EXORDNB TEST EXTRACT ORDERED                      
         BZ    SP100               NO - SKIP BUYREC PROCESSING                  
         L     R2,ARULDATA                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),AGYMED       A-M/CLT                                      
         MVC   KEY+3(1),BPRD                                                    
*                                                                               
SP61     BAS   RE,SPHIGH                                                        
         B     SP64                                                             
*                                                                               
SP62     DS    0H                                                               
         BAS   RE,SPSEQ                                                         
*                                                                               
SP64     CLC   KEY(4),KEYSAVE      SAME A-M/C/P                                 
         BNE   SP100                                                            
*                                                                               
         OC    KEY+4(2),KEY+4      TEST FOR MARKET ZERO                         
         BNZ   *+14                                                             
         MVC   KEY+9(4),XFF        YES-SKIP OVER CANADIAN NETWORK BUY           
         B     SP61                                                             
*                                                                               
         CLC   KEY(9),KEYSAVE      SAME A-M/C/P/MKT/STA                         
         BE    *+14                                                             
         MVC   FLTMST,KEY+4                                                     
         BAS   RE,GETSTA           GO READ NEW STATION RECORD                   
*                                                                               
         CLC   KEY(10),KEYSAVE     SAME A-M/C/P/MKT/STA/EST                     
         BE    SP70                                                             
         MVC   FLTMKT,KEY+4                                                     
         MVC   FLTSTN,KEY+6                                                     
         MVC   FLTEST,KEY+9                                                     
         MVI   RECTYPE,BUY                                                      
         BAS   RE,SETSW            GO SET PROCESS SWITCHES                      
         SPACE 1                                                                
* SEE IF WE NEED TO PROCESS THIS STA OR EST *                                   
         SPACE 1                                                                
         CLI   STASW,C'Y'          ANY RULE PROCESS THIS STA                    
         BE    *+14                                                             
         MVC   KEY+9(4),XFF        FORCE NEXT STA                               
         B     SP61                                                             
*                                                                               
         CLI   ESTSW,C'Y'          ANY RULE PROCESS THIS EST                    
         BE    *+14                                                             
         MVC   KEY+10(3),XFF       FORCE NEXT EST                               
         B     SP61                                                             
*                                                                               
         L     R2,ARULDATA                                                      
*                                                                               
SP66     CLI   QRPROC,C'Y'                                                      
         BNE   SP68                                                             
         BAS   RE,FILTMST          FILTER ON MARKET/STATION                     
         BNE   SP68                NO                                           
         BAS   RE,FILTSTA          GO FILTER ON REP/AFF                         
         BE    SP70                  AND IF PASS,PROCESS                        
*                                                                               
SP68     ICM   R2,15,QRNEXT                                                     
         BNZ   SP66                                                             
         B     SP62                                                             
*                                                                               
SP70     MVC   AIO,AIO1                                                         
         BAS   RE,SPGET            READ BUY RECORD                              
         L     RE,AIO              RE=A(BUY RECORD)                             
         TM    BUYRCNTL-BUYKEY(RE),X'80'  TEST RECORD IS DELETED                
         BO    SP68                YES-SKIP IT                                  
         CLC   KEY+4(2),4(RE)      TEST FOR SPILL BUY                           
         BNE   SP68                YES-SKIP LINE                                
*                                                                               
         XC    BUFFRULE,BUFFRULE                                                
*                                                                               
SP71     CLI   QRPROC,C'Y'                                                      
         BNE   SP71X                                                            
*                                                                               
         BAS   RE,PROCBUY          EXTRACT BUY RECORD DATA                      
*                                                                               
         OC    BUFFRULE,BUFFRULE   DID THAT RULE USE THE RECORD                 
         BZ    SP71X               NO                                           
*                                                                               
         XC    BUFFRULE,BUFFRULE   YES-NOW JUMP AHEAD IN RULE TABLE             
         ICM   R2,15,QRJUMP                                                     
         BNZ   SP71                BEGIN AGAIN AT JUMP POINT                    
         B     SP62                GET NEXT BUY                                 
*                                                                               
SP71X    ICM   R2,15,QRNEXT        ELSE TRY ANOTHER RULE                        
         BNZ   SP71                IF ANY                                       
         B     SP62                                                             
         EJECT                                                                  
SP100    DS    0H                                                               
         TM    DATAIND,BILLEDB     TEST EXTRACT BILLED                          
         BZ    SP150                                                            
         L     R2,ARULDATA                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(3),AGYMED     A-M/CLT                                      
         MVC   KEY+5(1),BPRD                                                    
*                                                                               
SP101    BAS   RE,SPHIGH                                                        
         B     SP104                                                            
*                                                                               
SP102    DS    0H                                                               
         BAS   RE,SPSEQ                                                         
*                                                                               
SP104    CLC   KEY(6),KEYSAVE      SAME A-M/C/P                                 
         BNE   SP150                                                            
*                                                                               
         CLC   KEY+7(5),KEYSAVE+7  SAME MKT/STA                                 
         BE    SP104A                                                           
         CLI   MGRSW,C'Y'          TEST MARKET GROUP IN KEY                     
         BE    SP104A              YES                                          
         MVC   FLTMST,KEY+7                                                     
         BAS   RE,GETSTA           GO READ NEW STATION RECORD                   
*                                                                               
SP104A   CLC   KEY(12),KEYSAVE     SAME A-M/C/P/EST/MKT/STA                     
         BE    SP110                                                            
         SPACE 1                                                                
         MVC   FLTEST,KEY+6                                                     
         MVC   FLTMKT,KEY+7                                                     
         MVC   FLTSTN,KEY+9                                                     
         XC    FLTMGR,FLTMGR                                                    
         MVI   RECTYPE,BILL                                                     
         CLI   MGRSW,C'Y'          TEST MARKET GROUP IN KEY                     
         BNE   *+16                NO                                           
         XC    FLTMKT(5),FLTMKT    CLEAR MARKET AND STATION                     
         MVC   FLTMGR,KEY+7        SET MARKET GROUP                             
         BAS   RE,SETSW            GO SET PROCESS SWITCHES                      
         SPACE 1                                                                
* SEE IF WE NEED TO PROCESS THIS STA OR EST *                                   
         SPACE 1                                                                
         CLI   ESTSW,C'Y'          ANY RULE PROCESS THIS EST                    
         BE    *+14                                                             
         MVC   KEY+7(6),XFF        FORCE NEXT EST                               
         B     SP101                                                            
*                                                                               
         CLC   KEY(STABKMKT-STABUCK),KEYSAVE  TEST SAME ESTIMATE                
         BE    SP105               YES                                          
         TM    DATAIND,EXBINVB+EXBNINVB TEST BILL BY INVOICE DATE REQD          
         BZ    *+8                                                              
         BAS   RE,BLDINV           YES-BUILD INVOICE TABLE FOR ESTIMATE         
*                                                                               
SP105    CLI   STASW,C'Y'          ANY RULE PROCESS THIS STA                    
         BNE   SP102               NEXT STATION                                 
*                                                                               
         L     R2,ARULDATA                                                      
*                                                                               
SP106    CLI   QRPROC,C'Y'                                                      
         BNE   SP108                                                            
         BAS   RE,FILTMGR          FILTER ON MARKET GROUP                       
         BNE   SP108               NO                                           
         BAS   RE,FILTMST          FILTER ON MARKET/STATION                     
         BNE   SP108               NO                                           
         CLI   MGRSW,C'Y'          TEST IF MARKET GROUP IN KEY                  
         BE    SP110               YES-SKIP STATION FILTERING                   
         BAS   RE,FILTSTA          GO FILTER ON REP/AFF                         
         BE    SP110                 AND IF PASS,PROCESS                        
*                                                                               
SP108    ICM   R2,15,QRNEXT                                                     
         BNZ   SP106                                                            
         B     SP102                                                            
*                                                                               
SP110    MVC   AIO,AIO1                                                         
         BAS   RE,SPGET            READ STATION BILL RECORD                     
*                                                                               
         XC    BUFFRULE,BUFFRULE                                                
*                                                                               
SP111    CLI   QRPROC,C'Y'                                                      
         BNE   SP111X                                                           
*                                                                               
         BAS   RE,PROCBILL         EXTRACT BILL RECORD DATA                     
*                                                                               
         OC    BUFFRULE,BUFFRULE   DID THAT RULE USE THE RECORD                 
         BZ    SP111X              NO                                           
*                                                                               
         XC    BUFFRULE,BUFFRULE   YES                                          
         ICM   R2,15,QRJUMP        NOW JUMP AHEAD IN RULE TABLE                 
         BNZ   SP111               SEE IF BILL FITS ANOTHER RULE                
         B     SP102               NOWHERE TO GO-NEXT BILL RECORD               
*                                                                               
SP111X   ICM   R2,15,QRNEXT        ELSE TRY ANOTHER RULE                        
         BNZ   SP111               IF ANY                                       
         B     SP102                                                            
         EJECT                                                                  
* READ BILLING RECORDS TO EXTRACT ACTUAL BILL AMOUNT BY ADVERTISING             
* PERIOD, BILL DATE, AND INVOICE DATE                                           
*                                                                               
SP150    TM    DATAIND1,EXBAADVB+EXBADATB+EXBAINVB                              
         BZ    SP170                                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BILLRECD,R4                                                      
         MVC   BKEYAM,AGYMED                                                    
         MVC   BKEYCLT,CLT                                                      
         MVC   BKEYPRD,PRD                                                      
*                                                                               
SP151    BAS   RE,SPHIGH                                                        
         B     SP154                                                            
*                                                                               
SP152    BAS   RE,SPSEQ                                                         
*                                                                               
SP154    CLC   BKEY(BKEYEST-BKEY),KEYSAVE TEST SAME AGY/CLT/PRD                 
         BNE   SP170               NO-ALL DONE                                  
*                                                                               
         CLC   BKEY(BKEYYSRV-BKEY),KEYSAVE TEST SAME ESTIMATE                   
         BE    SP160               YES                                          
*                                                                               
         MVC   FLTEST,BKEYEST                                                   
         XC    FLTMKT,FLTMKT                                                    
         XC    FLTSTN,FLTSTN                                                    
         MVI   RECTYPE,INVOICE                                                  
         BAS   RE,SETSW                                                         
*                                                                               
SP155    CLI   ESTSW,C'Y'          TEST IF ESTIMATE SB PROCESSED                
         BNE   *+14                                                             
         MVC   KEYSAVE(L'BKEY),KEY YES-SAVE ESTIMATE KEY                        
         B     SP152               GET NEXT RECORD (1ST BILL)                   
*                                                                               
         MVC   BKEYYSRV(BLEN-BKEYYSRV),XFF  FORCE NEXT ESTIMATE                 
         B     SP151                                                            
*                                                                               
SP160    L     R2,ARULDATA                                                      
         CLI   QRPROC,C'Y'         TEST IF RULE SB PROCESSED                    
         BE    SP162                                                            
         ICM   R2,15,QRNEXT                                                     
         BNZ   *-12                                                             
         B     SP152                                                            
*                                                                               
SP162    MVC   AIO,AIO1            GET BILL RECORD                              
         BAS   RE,SPGET                                                         
         XC    BUFFRULE,BUFFRULE                                                
*                                                                               
SP164    CLI   QRPROC,C'Y'                                                      
         BNE   SP165                                                            
*                                                                               
         BAS   RE,PROCINV          LOOK AT INVOICE RECORD                       
         OC    BUFFRULE,BUFFRULE   TEST POSTED AGAINST A RULE                   
         BZ    SP165               NO                                           
*                                                                               
         XC    BUFFRULE,BUFFRULE                                                
         ICM   R2,15,QRJUMP                                                     
         BNZ   SP164               TRY FOR ANOTHER RULE                         
         B     SP152               READ NEXT RECORD                             
*                                                                               
SP165    ICM   R2,15,QRNEXT        BUMP TO NEXT RULE                            
         BNZ   SP164                                                            
         B     SP152               EOT-READ NEXT RECORD                         
         EJECT                                                                  
* GOAL RECORD PROCESSING                                                        
*                                                                               
SP170    TM    DATAIND1,EXGOALB    TEST IF GOALS SB EXTRACTED                   
         BZ    SP200               NO                                           
         CLI   THISFIS,0           TEST FOR BROADCAST FISCAL YEAR               
         BNE   SP200               NO-SKIP THIS EXTRACT                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GOALRECD,R4                                                      
         MVI   GKEYTYPE,X'02'                                                   
         MVC   GKEYAM,AGYMED                                                    
         MVC   GKEYCLT,CLT                                                      
         MVC   GKEYPRD,BPRD                                                     
*                                                                               
SP171    BAS   RE,SPHIGH                                                        
         B     SP174                                                            
*                                                                               
SP172    BAS   RE,SPSEQ                                                         
*                                                                               
SP174    CLC   GKEY(GKEYMKT-GKEY),KEYSAVE  TEST SAME A-M/CLT/PRD                
         BNE   SP200               NO-NEXT PRODUCT                              
*                                                                               
         CLC   GKEY(GKEYDPT-GKEY),KEYSAVE  TEST SAME MKT/EST                    
         BE    SP176               YES                                          
*                                                                               
         MVC   FLTMKT,GKEYMKT                                                   
         MVC   FLTEST,GKEYEST                                                   
         XC    FLTSTN,FLTSTN                                                    
         MVI   RECTYPE,GOAL                                                     
         BAS   RE,SETSW                                                         
*                                                                               
         CLI   STASW,C'Y'          TEST IF MARKET PASSES                        
         BE    *+14                YES                                          
         MVC   GKEYEST(GLENGTH-GKEYEST),XFF  FORCE NEXT MARKET                  
         B     SP171                                                            
*                                                                               
         CLI   ESTSW,C'Y'                                                       
         BE    *+14                                                             
         MVC   GKEYDPT(GLENGTH-GKEYDPT),XFF  FORCE NEXT EST                     
         B     SP171                                                            
*                                                                               
SP176    L     R2,ARULDATA                                                      
         CLI   QRPROC,C'Y'         TEST TO PROCESS AGAINST RULE                 
         BE    SP178               YES                                          
         ICM   R2,15,QRNEXT                                                     
         BNZ   *-12                                                             
         B     SP172               GET NEXT RECORD                              
*                                                                               
SP178    MVC   AIO,AIO1                                                         
         BAS   RE,SPGET                                                         
         XC    BUFFRULE,BUFFRULE                                                
*                                                                               
SP180    CLI   QRPROC,C'Y'         TEST TO PROCESS RULE                         
         BNE   SP182               NO-TRY NEXT RULE                             
*                                                                               
         BAS   RE,PROCGOAL                                                      
         OC    BUFFRULE,BUFFRULE   TEST IF POSTED REC VS. RULE                  
         BZ    SP182               NO-TRY NEXT RULE                             
*                                                                               
         XC    BUFFRULE,BUFFRULE   YES                                          
         ICM   R2,15,QRJUMP        PICK UP JUMP POINT                           
         BNZ   SP180                                                            
         B     SP172                                                            
*                                                                               
SP182    ICM   R2,15,QRNEXT                                                     
         BNZ   SP180                                                            
         B     SP172                                                            
         EJECT                                                                  
* CHANGE OF PRODUCT *                                                           
         SPACE 1                                                                
SP200    OC    PGRKEY,PGRKEY       TEST BY PRDGRP                               
         BNZ   SP10                YES - NEXT PRODUCT                           
         CLI   ALLSW,YES           TEST PRODUCT=ALL EXTRACT                     
         BE    SP25                YES-NEXT PRODUCT                             
         B     SP900               GO WRITE WORKER RECORDS                      
         EJECT                                                                  
* WRITE WORKER RECORDS *                                                        
         SPACE 1                                                                
SP900    L     R2,ARULDATA         R2=A(RULE ENTRY)                             
*                                                                               
SP902    ZIC   R4,NEXTYPS          R4=N'EXTRACT TYPES                           
         LA    R5,EXTYPS           R5=A(EXTRACT TYPE TABLE)                     
         XC    BUFFREC,BUFFREC                                                  
         ST    R2,BUFFRULE                                                      
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         GOTO1 VDATAHD                                                          
*                                                                               
SP904    MVC   BUFFTYPE,0(R5)      SET EXTRACT TYPE                             
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
*                                                                               
SP906    MVC   BUFFPER,SVEXPER     SET PERIOD                                   
         BAS   RE,BUFFGET                                                       
         CLI   DMCB+8,0            TEST IF ITEM FOUND                           
         BE    SP908               YES                                          
         ZAP   BUFFGRS,=P'0'       NO-MANUFACTURE ZERO RECORD                   
         ZAP   BUFFNET,=P'0'                                                    
         ZAP   BUFFSPTS,=P'0'                                                   
*                                                                               
SP908    MVI   DMCB,BUPPUT                                                      
         TM    WHEN,X'18'          TEST OVERNIGHT                               
         BNZ   SP910               YES                                          
         MVI   DMCB,BUPADD                                                      
         TM    WHEN,X'20'          TEST SOON                                    
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SP910    GOTO1 VBUPPER                                                          
         LA    R6,SVEXL(R6)        NEXT PERIOD                                  
         OC    SVEXPER,SVEXPER     TEST FOR LAST PERIOD                         
         BNZ   SP906               NO                                           
*                                                                               
         LA    R5,1(R5)            NEXT EXTRACT TYPE                            
         BCT   R4,SP904                                                         
*                                                                               
         MVI   DMCB,BUPFINAL       FINAL CALL FOR OUTLINE                       
         GOTO1 VBUPPER                                                          
         ICM   R2,15,QRNEXT                                                     
         BNZ   SP902                                                            
         B     EXIT                NO MORE RULES                                
         DROP  R6                                                               
         EJECT                                                                  
PROCBUY  NTR1                                                                   
         L     R6,AIO                                                           
         USING BUYRECD,R6                                                       
         BAS   RE,FILTMST                                                       
         BNE   NEQXIT              REJECT RULE                                  
         BAS   RE,FILTSTA          FILTER ON STATION REP/AFFILIATION            
         BNE   NEQXIT                                                           
*                                                                               
         SR    R7,R7                                                            
         ICM   R7,3,QRDPTDSP       TEST DAYPART FILTER                          
         BZ    PB4                                                              
         AR    R7,R2                                                            
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)                                                         
PB2      CLC   BDDAYPT,0(R7)                                                    
         BE    PB4                                                              
         LA    R7,1(R7)                                                         
         BCT   R8,PB2                                                           
         B     NEQXIT                                                           
*                                                                               
PB4      SR    R7,R7                                                            
         ICM   R7,3,QRSPRDSP                                                    
         BZ    PB10                                                             
         AR    R7,R2                                                            
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)                                                         
PB6      CLC   BDREP,0(R7)                                                      
         BE    PB10                                                             
         LA    R7,2(R7)                                                         
         BCT   R8,PB6                                                           
         B     NEQXIT                                                           
*                                                                               
PB10     DS    0H                                                               
         CLI   BUYREC+3,X'FF'      TEST POL BUY                                 
         BNE   PB30                                                             
         SPACE 1                                                                
* POL BUY PROCESSING *                                                          
         SPACE 1                                                                
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
*                                                                               
PB12     BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
*                                                                               
         CLC   2(2,R6),SVEXTSTP    TEST PRIOR TO PLAN START                     
         BL    PB12                                                             
         CLC   2(2,R6),SVEXTNDP    OR AFTER PLAN END                            
         BH    EXIT                                                             
*                                                                               
         LA    R4,10(R6)                                                        
         ZIC   R5,1(R6)                                                         
         SH    R5,=H'10'           IGNORE UNALL SPOTS                           
         BNP   PB12                                                             
         SRL   R5,2                SET FOR BCT                                  
PB12A    CLC   0(1,R4),BPRD        TEST RIGHT PRD                               
         BE    PB14                                                             
         LA    R4,4(R4)                                                         
         BCT   R5,PB12A                                                         
         B     PB12                                                             
* FILTER ON SPTLEN IF REQUIRED                                                  
PB14     SR    R7,R7                                                            
         ICM   R7,3,QRSLNDSP                                                    
         BZ    PB16                                                             
         AR    R7,R2                                                            
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)                                                         
PB14A    CLC   0(1,R7),1(R4)                                                    
         BE    PB16                                                             
         LA    R7,1(R7)                                                         
         BCT   R8,PB14A                                                         
         B     PB12                                                             
PB16     DS    0H                                                               
         GOTO1 GETRATE,DMCB,(BPRD,SPOTS),AIO,(R6)                               
*                                                                               
         LA    R1,SVEXTDTS                                                      
         USING SVEXD,R1                                                         
PB18     CLC   2(2,R6),SVEXSTC     TEST PRIOR TO PERIOD START                   
         BL    PB20                                                             
         CLC   2(2,R6),SVEXENDC    OR AFTER PERIOD END                          
         BH    PB20                                                             
         B     PB22                                                             
PB20     LA    R1,SVEXL(R1)        NEXT PERIOD                                  
         B     PB18                                                             
*                                                                               
PB22     MVC   BUFFPER,SVEXPER     SET PERIOD                                   
         TM    DATAIND,EXORDB                                                   
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXORD                                                   
         BAS   R8,ORDPUT                                                        
*                                                                               
         TM    DATAIND,EXORDNB                                                  
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXORDN                                                  
         BAS   R8,ORDPUT                                                        
*                                                                               
         B     PB12                                                             
         EJECT                                                                  
* NON-POL BUY PROCESSING *                                                      
         SPACE 1                                                                
PB30     DS    0H                                                               
         SR    R7,R7                                                            
         ICM   R7,3,QRSLNDSP       TEST TO FILTER ON SPTLEN                     
         BZ    PB40                                                             
         CLI   BDTIME,0            TEST PIGGYBACK                               
         BE    PB32                NO                                           
         MVC   BYTE,BDTIME         SET SLN FOR ACTIVE PARTNER                   
         CLC   BPRD,BUYREC+3       TEST PROCESSING ACTIVE                       
         BE    PB32                                                             
* FIND PBELEM                                                                   
         MVI   ELCDLO,4                                                         
         MVI   ELCDHI,4                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,4(R6)          SET PASSIVE SLN                              
         L     R6,AIO              RESTORE BUYREC POINTER                       
*                                                                               
PB32     AR    R7,R2               POINT TO LIST                                
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)                                                         
PB34     CLC   0(1,R7),1(R4)                                                    
         BE    PB40                                                             
         LA    R7,1(R7)                                                         
         BCT   R8,PB34                                                          
         B     EXIT                                                             
*                                                                               
PB40     MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
         LA    R6,BDELEM                                                        
*                                                                               
PB42     BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
*                                                                               
         CLC   2(2,R6),SVEXTSTP    TEST PRIOR TO PLAN START                     
         BL    PB42                                                             
         CLC   2(2,R6),SVEXTNDP    OR AFTER PLAN END                            
         BH    EXIT                                                             
*                                                                               
         GOTO1 GETRATE,DMCB,(BPRD,SPOTS),AIO,(R6)                               
*                                                                               
         LA    R1,SVEXTDTS                                                      
         USING SVEXD,R1                                                         
PB44     CLC   2(2,R6),SVEXSTC     TEST PRIOR TO PERIOD START                   
         BL    PB50                                                             
         CLC   2(2,R6),SVEXENDC    OR AFTER PERIOD END                          
         BH    PB50                                                             
         B     PB52                                                             
PB50     LA    R1,SVEXL(R1)        NEXT PERIOD                                  
         B     PB44                                                             
*                                                                               
PB52     MVC   BUFFPER,SVEXPER     SET PERIOD                                   
         TM    DATAIND,EXORDB      TEST GROSS ORDERED NEEDED                    
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXORD                                                   
         BAS   R8,ORDPUT                                                        
*                                                                               
         TM    DATAIND,EXORDNB                                                  
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXORDN                                                  
         BAS   R8,ORDPUT                                                        
*                                                                               
         B     PB42                                                             
         SPACE 2                                                                
ORDPUT   ST    R2,BUFFRULE                                                      
         L     R0,SPOTS                                                         
         CVD   R0,DUB                                                           
         ZAP   BUFFSPTS,DUB                                                     
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         ZAP   BUFFGRS,DUB                                                      
         L     R0,NET                                                           
         CVD   R0,DUB                                                           
         ZAP   BUFFNET,DUB                                                      
         CLI   BUFFTYPE,EXORDN                                                  
         BNE   *+10                                                             
         ZAP   BUFFGRS,BUFFNET                                                  
         BAS   RE,BUFFPUT                                                       
         BR    R8                                                               
         EJECT                                                                  
PROCBILL NTR1                                                                   
         L     R7,AIO                                                           
         USING STABUCKD,R7                                                      
         BAS   RE,FILTMST                                                       
         BNE   NEQXIT              REJECT RULE                                  
         BAS   RE,FILTSTA          FILTER ON STATION REP/AFFILIATION            
         BNE   NEQXIT                                                           
*                                                                               
         LA    R6,STABELEM                                                      
         USING STABELEM,R6                                                      
*                                                                               
         MVI   ELCDLO,X'0E'        LOOK FOR BILLING ELEMENTS                    
         MVI   ELCDHI,X'0E'                                                     
         CLI   0(R6),X'0E'                                                      
         BE    BL2A                                                             
*                                                                               
BL2      BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
BL2A     TM    DATAIND,EXBINVB+EXBNINVB TEST BILL BY INVOICE DATE REQD          
         BZ    BL3                 NO                                           
         BAS   RE,GETINV           GET INVOICE TABLE ENTRY                      
*                                                                               
BL3      CLC   STABPER,SVEXTSPE    TEST THAT ADVERTISING PERIOD                 
         BL    BL4                 FALLS WITHIN PLAN PERIOD                     
         CLC   STABPER,SVEXTNPE                                                 
         BH    BL4                                                              
         B     BL5                 WITHIN PLAN                                  
*                                  ADVERTISING PERIOD OUTSIDE PLAN              
BL4      CLI   MULTIYR,C'Y'        TEST FOR MULTI-YEAR PLAN                     
         BNE   BL2                 NO-SKIP BILLING                              
         TM    DATAIND,EXBDATB+EXBNDATB+EXBINVB+EXBNINVB                        
         BZ    BL2                 NO-SKIP BILLING                              
*                                                                               
BL5      LA    R1,SVEXTDTS                                                      
         USING SVEXD,R1                                                         
         MVI   BYTE,BILLEDB        INITIALIZE ALL BILLING BITS                  
         NC    BYTE,DATAIND        FILTER ONLY THOSE REQUESTED                  
*                                                                               
BL6      TM    BYTE,EXBADVB        ADVERTISING MONTH                            
         BZ    BL7                                                              
         CLC   STABPER,SVEXPER                                                  
         BNE   BL8                                                              
         MVI   BUFFTYPE,EXBADV                                                  
         BAS   RE,BILLPUT                                                       
         NI    BYTE,X'FF'-EXBADVB                                               
*                                                                               
BL7      TM    BYTE,EXBNADVB       TEST NET BILLING BY ADVERTISING MON          
         BZ    BL8                                                              
         CLC   STABPER,SVEXPER                                                  
         BNE   BL8                                                              
         MVI   BUFFTYPE,EXBNADV                                                 
         BAS   RE,BILLPUT                                                       
         NI    BYTE,X'FF'-EXBNADVB                                              
*                                                                               
BL8      TM    BYTE,EXBDATB        BILLING DATE                                 
         BZ    BL9                                                              
         CLC   STABBDT,SVEXSTC                                                  
         BL    BL10                                                             
         CLC   STABBDT,SVEXENDC                                                 
         BH    BL10                                                             
         MVI   BUFFTYPE,EXBDATE                                                 
         BAS   RE,BILLPUT                                                       
         NI    BYTE,X'FF'-EXBDATB                                               
*                                                                               
BL9      TM    BYTE,EXBNDATB       TEST NET BILLING BY BILLING DATE             
         BZ    BL10                                                             
         CLC   STABBDT,SVEXSTC                                                  
         BL    BL10                                                             
         CLC   STABBDT,SVEXENDC                                                 
         BH    BL10                                                             
         MVI   BUFFTYPE,EXBNDATE                                                
         BAS   RE,BILLPUT                                                       
         NI    BYTE,X'FF'-EXBNDATB                                              
*                                                                               
BL10     TM    BYTE,EXBINVB+EXBNINVB                                            
         BZ    BL15                                                             
         LA    R5,INVENT                                                        
         USING INVTABD,R5                                                       
         CLC   INVQPER,SVEXPER                                                  
         BNE   BL15                                                             
*                                                                               
BL11     TM    BYTE,EXBINVB                                                     
         BZ    BL12                                                             
         MVI   BUFFTYPE,EXBINV                                                  
         BAS   RE,BILLPUT                                                       
         NI    BYTE,X'FF'-EXBINVB                                               
*                                                                               
BL12     TM    BYTE,EXBNINVB                                                    
         BZ    BL15                                                             
         MVI   BUFFTYPE,EXBNINV                                                 
         BAS   RE,BILLPUT                                                       
         NI    BYTE,X'FF'-EXBNINVB                                              
*                                                                               
BL15     CLI   BYTE,0              TEST FOR DONE WITH THIS ELEM                 
         BE    BL2                                                              
         LA    R1,SVEXL(R1)                                                     
         CLI   0(R1),0             TEST FOR E-O-L                               
         BNE   BL6                 NO - CONTINUE                                
         B     BL2                 YES -  MONTH NOT FOUND, NEXT ELEM            
         SPACE 2                                                                
BILLPUT  NTR1                                                                   
         ST    R2,BUFFRULE                                                      
         MVC   BUFFPER,SVEXPER     SET PERIOD                                   
         L     R0,STABSPTS                                                      
         CVD   R0,DUB                                                           
         ZAP   BUFFSPTS,DUB                                                     
         L     R0,STABGRS                                                       
         CVD   R0,DUB                                                           
         ZAP   BUFFGRS,DUB                                                      
         L     R0,STABNET                                                       
         CVD   R0,DUB                                                           
         ZAP   BUFFNET,DUB                                                      
         CLI   BUFFTYPE,EXBNADV    TEST FOR NET BILLING                         
         BE    BILLPUT1                                                         
         CLI   BUFFTYPE,EXBNDATE                                                
         BE    BILLPUT1                                                         
         CLI   BUFFTYPE,EXBNINV                                                 
         BNE   BILLPUT2                                                         
BILLPUT1 ZAP   BUFFGRS,BUFFNET     SET GROSS BUCKET=NET BUCKET                  
*                                                                               
BILLPUT2 BAS   RE,BUFFPUT                                                       
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS AN INVOICE RECORD                                      
*                                                                               
* CALLED TO EXTRACT BILLING BY INVOICE DATE FOR ACTUAL BILL AND                 
* ACTUAL BILL BY BILLING DATE AND ADVERTISING MONTH.  NO TEST                   
* ON ADVERTISING PERIOD IS APPLIED FOR BILLING BY INVOICE DATE.                 
*                                                                               
PROCINV  NTR1                                                                   
         L     R4,AIO                                                           
         USING BILLRECD,R4                                                      
*                                                                               
* BY ADVERTISING PERIOD                                                         
*                                                                               
PI1      TM    DATAIND1,EXBAADVB   TEST ACTUAL BILL BY ADV MONTH                
         BZ    PI3                 NO                                           
         CLC   BKEYYSRV(2),SVEXTSPE TEST WITHIN EXTRACT                         
         BL    PI3                                                              
         CLC   BKEYYSRV(2),SVEXTNPE                                             
         BH    PI3                                                              
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
*                                                                               
PI2      CLC   BKEYYSRV(2),SVEXPER                                              
         BE    *+12                                                             
         LA    R6,SVEXL(R6)                                                     
         B     PI2                                                              
*                                                                               
         MVI   BUFFTYPE,EXBAADV                                                 
         BAS   RE,INVPUT                                                        
*                                                                               
* BY INVOICE DATE                                                               
*                                                                               
PI3      TM    DATAIND1,EXBAINVB                                                
         BZ    PI10                                                             
*                                                                               
         GOTO1 INVPER,BQDATE                                                    
         BNE   PI10                INVOICE DATE OUTSIDE EXTRACT                 
*                                                                               
PI4      TM    DATAIND1,EXBAINVB    TEST ACTUAL BILLING REQUIRED                
         BZ    PI10                                                             
         MVI   BUFFTYPE,EXBAINV                                                 
         BAS   RE,INVPUT                                                        
*                                                                               
* BY BILL DATE                                                                  
*                                                                               
PI10     TM    DATAIND1,EXBADATB   TEST ACTUAL BILL BY BILL DATE                
         BZ    PIX                 NO                                           
         GOTO1 INVPER,BDATE                                                     
         BNE   PIX                 BILLED OUTSIDE EXTRACT                       
         CLI   MULTIYR,C'Y'        TEST MULTI-YEAR PLAN                         
         BE    PI12                YES                                          
         CLC   BKEYYSRV(2),SVEXTSPE NO-TEST ALSO IF ADV PERIOD                  
         BL    PIX                 IS WITHIN EXTRACT                            
         CLC   BKEYYSRV(2),SVEXTNPE                                             
         BH    PIX                                                              
*                                                                               
PI12     MVI   BUFFTYPE,EXBADAT                                                 
         BAS   RE,INVPUT                                                        
*                                                                               
PIX      B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO FIND THE PERIOD, AT ENTRY R1=A(DATE), ON EXIT                  
* CC=EQ IF WITHIN EXTRACT AND R6=A(DATE TABLE ENTRY), CC=NEQ IF                 
* OUTSIDE EXTRACT.                                                              
*                                                                               
INVPER   ST    RE,SAVERE                                                        
         MVC   DUB(6),0(R1)                                                     
         CLC   DUB(6),SVEXTST      TEST IF WITHIN PLAN                          
         BL    INVPERN                                                          
         CLC   DUB(6),SVEXTEND                                                  
         BH    INVPERN                                                          
*                                                                               
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
         GOTO1 DATCON,PARAS,DUB,(3,FULL)                                        
*                                                                               
INVPER2  CLC   FULL(3),SVEXSTB     LOCATE THE PERIOD                            
         BL    INVPER4                                                          
         CLC   FULL(3),SVEXENDB                                                 
         BH    INVPER4                                                          
         B     INVPERY                                                          
*                                                                               
INVPER4  LA    R6,SVEXL(R6)        NEXT DATE ENTRY                              
         B     INVPER2                                                          
*                                                                               
INVPERY  CR    RB,RB               SET CC=EQ                                    
         B     INVPERX                                                          
*                                                                               
INVPERN  LTR   RB,RB                                                            
*                                                                               
INVPERX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
* SUB-ROUTINE TO PUT INVOICE DATA TO BUFFALO                                    
*                                                                               
INVPUT   NTR1                                                                   
         ST    R2,BUFFRULE                                                      
         MVC   BUFFPER,SVEXPER                                                  
         CLI   BUFFTYPE,EXBINV     TEST GROSS/NET BILLING NEEDED                
         BE    INVPUT2             YES                                          
         CLI   BUFFTYPE,EXBNINV                                                 
         BE    INVPUT2                                                          
*                                                                               
INVPUT1  ZAP   BUFFGRS,BACTP       GET ACTUAL BILLED FIGURE                     
         ZAP   BUFFNET,BUFFGRS                                                  
         B     INVPUT4                                                          
*                                                                               
INVPUT2  ZAP   BUFFGRS,BGRSP       GROSS/NET BILLING                            
         ZAP   BUFFNET,BNETP                                                    
         CLI   BUFFTYPE,EXBNINV                                                 
         BNE   *+10                                                             
         ZAP   BUFFGRS,BUFFNET                                                  
*                                                                               
INVPUT4  ZAP   BUFFSPTS,=P'1'                                                   
         BAS   RE,BUFFPUT                                                       
         B     EXIT                                                             
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS A GOAL RECORD                                          
*                                                                               
PROCGOAL NTR1                                                                   
         L     R6,AIO                                                           
         USING GOALRECD,R6                                                      
         BAS   RE,FILTMST          FILTER ON MARKET                             
         BNE   NEQXIT                                                           
*                                                                               
         SR    R7,R7                                                            
         ICM   R7,3,QRDPTDSP                                                    
         BZ    PG2                 NO DAYPART RULES                             
         AR    R7,R2                                                            
         ZIC   R0,0(R7)                                                         
         LA    R7,1(R7)                                                         
*                                                                               
PG1      CLC   GKEYDPT,0(R7)                                                    
         BE    PG2                                                              
         LA    R7,LRUDPT(R7)                                                    
         BCT   R0,PG1                                                           
         B     NEQXIT                                                           
*                                                                               
PG2      SR    R7,R7                                                            
         ICM   R7,3,QRSLNDSP                                                    
         BZ    PG4                 NO SLN RULES                                 
         AR    R7,R2                                                            
         ZIC   R0,0(R7)                                                         
         LA    R7,1(R7)                                                         
*                                                                               
PG3      CLC   GKEYSLN,0(R7)                                                    
         BE    PG4                                                              
         LA    R7,LRUSLN(R7)                                                    
         BCT   R0,PG3                                                           
         B     NEQXIT                                                           
*                                                                               
PG4      LA    R6,GDELEM                                                        
         MVI   ELCDLO,X'21'                                                     
         MVI   ELCDHI,X'21'                                                     
*                                                                               
PG5      BAS   RE,NEXTEL                                                        
         BNE   EXIT                NO MORE GOAL ELEMENTS                        
         USING GLEMENT,R6                                                       
         CLC   GLWEEK,MONST        TEST W/IN PLAN                               
         BL    PG5                 NO                                           
         CLC   GLWEEK,MONEND                                                    
         BH    PG5                                                              
*                                                                               
PG6      LA    R5,MONTAB                                                        
         LA    R0,13                                                            
*                                                                               
PG7      CLI   0(R5),0             TEST FOR EOT                                 
         BE    PG5                 YES-NOT IN PLAN                              
         CLC   GLWEEK,2(R5)        TEST FOR FIT W/IN PERIOD                     
         BL    PG8                                                              
         CLC   GLWEEK,4(R5)                                                     
         BH    PG8                                                              
         B     PG10                FOUND A HIT                                  
*                                                                               
PG8      LA    R5,L'MONTAB(R5)                                                  
         BCT   R0,PG7                                                           
         B     PG5                                                              
*                                                                               
PG10     ST    R2,BUFFRULE                                                      
         MVI   BUFFTYPE,EXGOAL                                                  
         MVC   BUFFPER,0(R5)                                                    
         ZAP   BUFFSPTS,=P'1'                                                   
         ICM   R0,15,GLBUDGET                                                   
         CVD   R0,DUB                                                           
         ZAP   BUFFGRS,DUB                                                      
         ZAP   BUFFNET,BUFFGRS                                                  
         BAS   RE,BUFFPUT                                                       
         B     PG5                 LOOK AT NEXT ELEMENT                         
         DROP  R6                                                               
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
NEXTELX  LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO FILTER MARKET AND STATION AGAINST A RULE ENTRY                 
* AT ENTRY, R2=A(RULE), FLTMKT=MARKET, FLTSTN=STATION                           
*                                                                               
* ON EXIT, CC=EQ TO ACCEPT RULE, CC=NEQ TO REJECT RULE                          
*                                                                               
FILTMST  NTR1                                                                   
         OC    FLTMKT,FLTMKT       TEST FOR MARKET FILTER                       
         BZ    FILTMST4            **NO NEED TO APPLY MGR AND MKT RULES         
         ICM   RE,15,QRADRMGR      TEST MGRP LIST IN THIS RULE                  
         BZ    FILTMST2                                                         
*                                                                               
FILTMST1 OC    0(2,RE),0(RE)       TEST EOL                                     
         BZ    FILTMSTN            REJECT THIS RULE                             
         CLC   FLTMKT,0(RE)                                                     
         BE    FILTMST2                                                         
         LA    RE,2(RE)                                                         
         B     FILTMST1                                                         
*                                                                               
FILTMST2 SR    R7,R7                                                            
         ICM   R7,3,QRMKTDSP       TEST MKT LIST                                
         BZ    FILTMST4                                                         
         AR    R7,R2               POINT TO LIST                                
         ZIC   R0,0(R7)                                                         
         LA    R7,1(R7)            POINT TO FIRST MARKET                        
*                                                                               
FILTMST3 CLC   FLTMKT,0(R7)        MATCH MKT                                    
         BE    FILTMST4                                                         
         LA    R7,2(R7)                                                         
         BCT   R0,FILTMST3                                                      
         B     FILTMSTN            REJECT IT                                    
*                                                                               
FILTMST4 OC    FLTSTN,FLTSTN       TEST FOR STATION FILTER                      
         BZ    FILTMSTY            NONE-PASS RULE                               
         SR    R7,R7                                                            
         ICM   R7,3,QRSTADSP       TEST STA LIST                                
         BZ    FILTMSTY            NONE-PASS RULE                               
         AR    R7,R2               POINT TO LIST                                
         ZIC   R0,0(R7)                                                         
         LA    R7,1(R7)            POINT TO FIRST STATION                       
*                                                                               
FILTMST5 CLC   FLTSTN,0(R7)        MATCH STA                                    
         BE    FILTMSTY            FOUND IT-TAKE RULE                           
         LA    R7,3(R7)                                                         
         BCT   R0,FILTMST5                                                      
         B     FILTMSTN                                                         
*                                                                               
FILTMSTN B     NEQXIT                                                           
*                                                                               
FILTMSTY B     EQXIT                                                            
         EJECT                                                                  
* SUBROUTINE TO READ STATION MASTER RECORD *                                    
         SPACE 1                                                                
GETSTA   NTR1                                                                   
         L     R2,ARULDATA         POINT TO FIRST RULE                          
         CLC   FLTMST,=X'270ED6B5A4'  TEST NEW MANUAL BILL                      
         BE    GETSTA2             YES-BLAME GRANT FOR THIS ONE                 
* CALL STAPACK DIRECTLY                                                         
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,QRAGYC                                                   
         MVC   STAPCTRY,SVAPROF7                                                
         MVC   STAPMED,QRMED                                                    
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,FLTMST                                                  
*                                                                               
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   STAPQSTA+4,C' '                                                  
         BNE   *+10                                                             
         MVC   STAPQSTA+4(1),QRMED                                              
*                                                                               
         MVC   WORK+32(5),STAPQSTA     SAVE STATION                             
         DROP  R4                                                               
*                                                                               
         MVI   WORK,C'0'                                                        
         MVC   WORK+1(16),WORK                                                  
         MVI   WORK,C'S'                                                        
         MVC   WORK+1(1),QRMED                                                  
         MVC   WORK+2(5),WORK+32   MOVE SAVED STATION                           
         MVC   WORK+7(2),QRAGYC    AGENCY CODE                                  
         MVC   WORK+9(3),QRCLT                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',WORK,ASTATION                
         CLI   8(R1),0                                                          
         BE    GETSTAX                                                          
         CLC   =C'ZZZZ',WORK+2     TEST FOR MANUAL BILLING RECORD               
         BE    *+6                                                              
         DC    H'0'                NO-DUMP SINCE SOMETHING WRONG                
GETSTA2  L     RE,ASTATION                                                      
         LA    RF,L'STATION                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
GETSTAX  B     EXIT                                                             
         SPACE 2                                                                
* FILTER STATION ON AFFILIATE OR PAYING REP IF REQUIRED *                       
         SPACE 1                                                                
FILTSTA  NTR1                                                                   
         L     R6,ASTATION                                                      
         USING STARECD,R6                                                       
         SR    R7,R7                                                            
         ICM   R7,3,QRAFFDSP       TEST AFFILIATE LIST SPECIFIED                
         BZ    FILTSTA4            NO                                           
         AR    R7,R2               POINT TO LIST                                
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)                                                         
*                                                                               
FILTSTA2 CLC   SNETWRK,0(R7)       MATCH AFFIL                                  
         BE    FILTSTA4                                                         
         LA    R7,3(R7)                                                         
         BCT   R8,FILTSTA2                                                      
         B     NEQXIT                                                           
*                                                                               
FILTSTA4 SR    R7,R7                                                            
         ICM   R7,3,QRPAYDSP       TEST PAYING REPS SPECIFIED                   
         BZ    EQXIT                                                            
         AR    R7,R2                                                            
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)                                                         
*                                                                               
FILTSTA6 CLC   SPAYREP,0(R7)                                                    
         BE    EQXIT                                                            
         LA    R7,3(R7)                                                         
         BCT   R8,FILTSTA6                                                      
         B     NEQXIT                                                           
         DROP  R6                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO APPLY THE MARKET GROUP FILTER                                  
*                                                                               
FILTMGR  NTR1  ,                                                                
         OC    FLTMGR,FLTMGR       TEST MARKET GROUP IN KEY                     
         BZ    EQXIT                                                            
         SR    R7,R7                                                            
         ICM   R7,3,QRMGRDSP       DISP TO MARKET GROUP RULE                    
         BZ    EQXIT                                                            
         AR    R7,R2                                                            
         ZIC   R0,0(R7)            R0=LOOP COUNTER                              
         LA    R7,1(R7)            R7=A(MARKET GROUP FILTER)                    
*                                                                               
FILTMGR2 PACK  DUB,1(4,R7)         GET MARKET GROUP NUMBER                      
         SRP   DUB,1,0             CHANGE IT TO A PWO                           
         CLC   FLTMGR,DUB+5                                                     
         BE    EQXIT                                                            
         LA    R7,LRUMGR(R7)                                                    
         BCT   R0,FILTMGR2                                                      
         B     NEQXIT                                                           
         EJECT                                                                  
* SUBROUTINE TO DETERMINE STATION/EST SHOULD BE PROCESSED *                     
         SPACE 1                                                                
SETSW    NTR1                                                                   
         MVI   ESTSW,C'N'          RESET MASTER SWITCHES                        
         MVI   STASW,C'N'                                                       
*                                                                               
         L     R2,ARULDATA                                                      
SETSW1   MVI   QRSTASW,C'N'                                                     
         MVI   QRESTSW,C'N'                                                     
         MVI   QRPROC,C'N'         RESET ALL RULE PROCESS SWITCHES              
         ICM   R2,15,QRNEXT                                                     
         BNZ   SETSW1                                                           
*                                                                               
         CLI   RECTYPE,INVOICE     TEST INVOICE                                 
         BE    SETSW48             YES-ONLY CAN FILTER ON ESTIMATE              
*                                                                               
         L     R2,ARULDATA                                                      
SETSW2   ICM   RE,15,QRADRMGR      TEST MGRP LIST IN THIS RULE                  
         BZ    SETSW10                                                          
         CLI   RECTYPE,BILL        TEST FOR STATION BILL REC                    
         BNE   SETSW4                                                           
         CLI   MGRSW,C'Y'          TEST MARKET GROUP IN KEY                     
         BE    SETSW8              YES                                          
*                                                                               
SETSW4   OC    0(2,RE),0(RE)       TEST EOL                                     
         BZ    SETSW10                                                          
         CLC   FLTMKT,0(RE)                                                     
         BNE   SETSW6                                                           
         MVI   QRSTASW,C'Y'        SET STA SWITCH                               
         B     SETSW10                                                          
*                                                                               
SETSW6   LA    RE,2(RE)                                                         
         B     SETSW4                                                           
*                                                                               
SETSW8   SR    R7,R7                                                            
         ICM   R7,3,QRMGRDSP                                                    
         LA    R7,0(R2,R7)                                                      
         ZIC   R8,0(R7)            R8=LOOP COUNTER                              
         LA    R7,1(R7)            R7=A(MGR RULE FILTER)                        
*                                                                               
SETSW9   PACK  DUB,1(4,R7)         MARKET GROUP NUMBER                          
         SRP   DUB,1,0                                                          
         CLC   FLTMGR,DUB+5                                                     
         BNE   *+12                                                             
         MVI   QRSTASW,C'Y'        NOTE PASSED STATION FILTER                   
         B     SETSW10                                                          
*                                                                               
         LA    R7,LRUMGR(R7)                                                    
         BCT   R8,SETSW9                                                        
*                                                                               
SETSW10  ICM   R2,15,QRNEXT        NEXT RULE                                    
         BNZ   SETSW2                                                           
         SPACE 1                                                                
* TEST FOR MARKET LIST *                                                        
         SPACE 1                                                                
         L     R2,ARULDATA                                                      
*                                                                               
SETSW12  SR    R7,R7                                                            
         ICM   R7,3,QRMKTDSP       TEST MKT LIST                                
         BZ    SETSW20                                                          
         AR    R7,R2               POINT TO LIST                                
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)            POINT TO FIRST MARKET                        
*                                                                               
SETSW14  CLC   FLTMKT,0(R7)        MATCH MKT                                    
         BNE   SETSW16                                                          
         MVI   QRSTASW,C'Y'                                                     
         B     SETSW20                                                          
*                                                                               
SETSW16  LA    R7,2(R7)                                                         
         BCT   R8,SETSW14                                                       
*                                                                               
SETSW20  ICM   R2,15,QRNEXT                                                     
         BNZ   SETSW12                                                          
         SPACE 1                                                                
* TEST FOR STATION LIST *                                                       
         SPACE 1                                                                
         CLI   RECTYPE,GOAL                                                     
         BE    SETSW32                                                          
         L     R2,ARULDATA                                                      
*                                                                               
SETSW22  SR    R7,R7                                                            
         ICM   R7,3,QRSTADSP       TEST STA LIST                                
         BZ    SETSW30                                                          
         AR    R7,R2               POINT TO LIST                                
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)            POINT TO FIRST STATION                       
*                                                                               
SETSW24  CLC   FLTSTN,0(R7)        MATCH STA                                    
         BNE   SETSW26                                                          
         MVI   QRSTASW,C'Y'                                                     
         B     SETSW30                                                          
*                                                                               
SETSW26  LA    R7,3(R7)                                                         
         BCT   R8,SETSW24                                                       
*                                                                               
SETSW30  ICM   R2,15,QRNEXT                                                     
         BNZ   SETSW22                                                          
*                                                                               
SETSW32  L     R2,ARULDATA                                                      
*                                                                               
SETSW42  OC    QRADRMGR,QRADRMGR   TEST MKTGRP LIST                             
         BNZ   SETSW44                                                          
         OC    QRMKTDSP,QRMKTDSP   TEST MKT LIST                                
         BNZ   SETSW44                                                          
         OC    QRSTADSP,QRSTADSP   TEST STA LIST                                
         BNZ   SETSW44                                                          
         MVI   QRSTASW,C'Y'        PROCESS BY DEFAULT                           
*                                                                               
SETSW44  ICM   R2,15,QRNEXT                                                     
         BNZ   SETSW42                                                          
         SPACE 1                                                                
* NOW SEE IF ESTIMATE IS ACTIVE *                                               
         SPACE 1                                                                
SETSW48  L     R2,ARULDATA                                                      
*                                                                               
SETSW50  OC    QRESTDSP,QRESTDSP   TEST ESTIMATE LIST                           
         BZ    SETSW54             NO - GO FILTER                               
         SR    R7,R7                                                            
         ICM   R7,3,QRESTDSP                                                    
         AR    R7,R2                                                            
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)                                                         
         ZIC   RF,FLTEST                                                        
*                                                                               
SETSW52  CLM   RF,3,0(R7)          TEST LESS THAN START                         
         BL    SETSW56                                                          
         CLM   RF,3,2(R7)          OR MORE THAN END                             
         BH    SETSW56                                                          
*                                                                               
SETSW54  ZIC   RF,FLTEST           GET EST IN RF                                
         BAS   RE,FLTREST                                                       
         BNE   SETSW58                                                          
         MVI   QRESTSW,C'Y'                                                     
         B     SETSW58                                                          
*                                                                               
SETSW56  LA    R7,4(R7)                                                         
         BCT   R8,SETSW52                                                       
*                                                                               
SETSW58  ICM   R2,15,QRNEXT        NEXT RULE                                    
         BNZ   SETSW50                                                          
         SPACE 1                                                                
* FINALLY, COMBINE STASW AND ESTSW AND SET PROCESS RULE TO Y/N *                
         SPACE 1                                                                
         L     R2,ARULDATA                                                      
SETSW60  CLI   RECTYPE,BUY                                                      
         BNE   SETSW61                                                          
         CLI   QRSTASW,C'Y'        TEST PROC STATION FIRST                      
         BNE   SETSW65             NO - IGNORE EST                              
         MVI   STASW,C'Y'          SET MASTER SWITCH                            
         CLI   QRESTSW,C'Y'        IF PROC STATION,TEST PROC EST                
         BNE   SETSW65                                                          
         MVI   ESTSW,C'Y'          SET MASTER SWITCH                            
         MVI   QRPROC,C'Y'         AND SET TO PROCESS THIS RULE                 
         B     SETSW65                                                          
         SPACE                                                                  
SETSW61  CLI   RECTYPE,BILL                                                     
         BNE   SETSW62                                                          
         CLI   QRESTSW,C'Y'        IF PROC STATION,TEST PROC EST                
         BNE   SETSW65                                                          
         MVI   ESTSW,C'Y'          SET MASTER SWITCH                            
         CLI   QRSTASW,C'Y'        TEST PROC STATION FIRST                      
         BNE   SETSW65             NO - IGNORE EST                              
         MVI   STASW,C'Y'          SET MASTER SWITCH                            
         MVI   QRPROC,C'Y'         AND SET TO PROCESS THIS RULE                 
         B     SETSW65                                                          
*                                                                               
SETSW62  CLI   RECTYPE,INVOICE                                                  
         BNE   SETSW63                                                          
         CLI   QRESTSW,C'Y'                                                     
         BNE   SETSW65                                                          
         MVI   ESTSW,C'Y'                                                       
         MVI   QRPROC,C'Y'                                                      
         B     SETSW65                                                          
*                                                                               
SETSW63  CLI   RECTYPE,GOAL                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   QRSTASW,C'Y'                                                     
         BNE   SETSW65                                                          
         MVI   STASW,C'Y'                                                       
         CLI   QRESTSW,C'Y'                                                     
         BNE   SETSW65                                                          
         MVI   ESTSW,C'Y'                                                       
         MVI   QRPROC,C'Y'                                                      
         B     SETSW65                                                          
*                                                                               
SETSW65  ICM   R2,15,QRNEXT                                                     
         BNZ   SETSW60                                                          
         B     EXIT                                                             
         EJECT                                                                  
FLTREST  NTR1                                                                   
         MH    RF,=H'3'            EST X 3                                      
         A     RF,AESTTAB                                                       
         OC    0(3,RF),0(RF)       TEST ESTIMATE ACTIVE                         
         BZ    NEQXIT              NO - DONE                                    
*                                                                               
         LA    R0,3                                                             
         LA    RE,QRFLT                                                         
*                                                                               
FLTREST2 CLI   0(RE),C'*'                                                       
         BE    FLTREST6                                                         
         CLI   0(RE),C' '                                                       
         BNH   FLTREST6                                                         
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    FLTREST4            YES                                          
*                                                                               
         CLC   0(1,RE),0(RF)       POSITIVE FILTER MUST MATCH                   
         BNE   NEQXIT                                                           
         B     FLTREST6                                                         
*                                                                               
FLTREST4 MVC   BYTE,0(RE)                                                       
         OI    BYTE,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   BYTE,0(RF)                                                       
         BE    NEQXIT                                                           
*                                                                               
FLTREST6 LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FLTREST2                                                      
         B     EQXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A TABLE OF INVOICES FOR THE ESTIMATE                     
*                                                                               
BLDINV   NTR1                                                                   
         MVC   SAVEKEY,KEY         SAVE PRESENT KEY                             
         MVC   SAVEAIO,AIO         AND IO AREA POINTER                          
         L     R5,=A(INVTAB)       R5=A(INVOICE TABLE)                          
         A     R5,RELO                                                          
         USING INVTABD,R5                                                       
         SR    R2,R2               R2=N'TABLE ENTRIES                           
*                                                                               
BLDINV2  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BILLRECD,R4                                                      
         MVC   BKEYAM,AGYMED                                                    
         MVC   BKEYCLT,CLT                                                      
         MVC   BKEYPRD,PRD                                                      
         MVC   BKEYEST,FLTEST                                                   
         BAS   RE,SPHIGH                                                        
         CLC   BKEY(BKEYYSRV-BKEY),KEYSAVE  TEST ESTIMATE FOUND                 
         BNE   BLDINV6             NO                                           
*                                                                               
BLDINV3  LA    R4,KEY                                                           
         BAS   RE,SPSEQ                                                         
*                                                                               
BLDINV4  CLC   BKEY(BKEYYSRV-BKEY),KEYSAVE  TEST SAME ESTIMATE                  
         BNE   BLDINV6                                                          
*                                                                               
         CLI   MULTIYR,C'Y'        TEST MULTI-YEAR PLAN                         
         BE    BLDINV5             YES                                          
         CLC   BKEYYSRV(2),SVEXTSPE TEST MOS WITHIN PLAN                        
         BL    BLDINV3             TOO LOW-GET NEXT RECORD                      
         CLC   BKEYYSRV(2),SVEXTNPE                                             
         BH    BLDINV6             PAST THE END-MUST BE ALL DONE                
*                                                                               
BLDINV5  MVC   AIO,AIO2                                                         
         BAS   RE,SPGET                                                         
         L     R4,AIO                                                           
         GOTO1 INVPER,BQDATE                                                    
         BNE   BLDINV3                                                          
         MVC   INVQDATE,FULL       SAVE BINARY QDATE                            
         MVC   INVQPER,SVEXPER-SVEXD(R6)  SAVE PERIOD VALUE FOUND               
*                                                                               
         C     R2,=A(MAXENT)       TEST TABLE LIMIT BLOWN                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   INVMOS,BKEYYSRV                                                  
         MVC   INVBM,BKEYMBIL                                                   
         MVC   INVNUM,BKEYINV                                                   
         LA    R5,INVTABL(R5)                                                   
         LA    R2,1(R2)                                                         
         B     BLDINV3             GET NEXT RECORD                              
*                                                                               
BLDINV6  ST    R2,NINVENT                                                       
         MVC   KEY,SAVEKEY         RESTORE KEY AT ENTRY                         
         BAS   RE,SPHIGH                                                        
         MVC   AIO,SAVEAIO                                                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET THE INVOICE TABLE ENTRY FOR THE BILL ELEMENT               
*                                                                               
* AT ENTRY, R6=A(STATION BILLING BUCKET ELEMENT)                                
* ON EXIT,  CC=NEQ IF NOT IN TABLE,CC=EQ AND INVENT SET IF ENTRY FOUND          
*                                                                               
GETINV   NTR1                                                                   
         USING STABELEM,R6                                                      
         XC    INVENT,INVENT                                                    
         OC    NINVENT,NINVENT                                                  
         BZ    GETINVN                                                          
*                                                                               
         LA    R5,INVENT                                                        
         USING INVTABD,R5                                                       
         MVC   INVMOS,STABPER      EXTRACT PERIOD                               
         GOTO1 DATCON,DMCB,(2,STABBDT),(3,FULL)                                 
         ZIC   RE,FULL+1           GET MONTH                                    
         ZIC   R1,FULL             GET YEAR                                     
         SR    R0,R0                                                            
         D     R0,=F'10'           FIND YEAR REMAINDER                          
         SLL   R0,4                                                             
         OR    R0,RE               COMBINE YEAR REMAINDER/MONTH                 
         STC   R0,INVBM                                                         
         MVC   INVNUM,STABINV      INVOICE NUMBER                               
*                                                                               
GETINV2  L     R0,NINVENT                                                       
         L     R4,=A(INVTAB)                                                    
         A     R4,RELO                                                          
         GOTO1 BINSRCH,DMCB,INVTABD,(R4),(R0),INVTABL,INVKEYL,A(MAXENT)         
         CLI   DMCB,0              TEST IF ENTRY FOUND                          
         BNE   GETINVN             NO                                           
         L     RE,DMCB                                                          
         MVC   INVENT,0(RE)        EXTRACT ENTRY                                
         B     GETINVY                                                          
*                                                                               
GETINVN  LTR   RB,RB                                                            
         B     GETINVX                                                          
*                                                                               
GETINVY  CR    RB,RB                                                            
*                                                                               
GETINVX  B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A TABLE OF MONDAYS OF START AND END WEEKS                
* IN EACH PERIOD                                                                
*                                                                               
BLDMON   NTR1                                                                   
         GOTO1 GETMON,DMCB,SVEXTST,MONST                                        
         GOTO1 GETMON,DMCB,SVEXTEND,MONEND                                      
         XC    MONTAB(13*L'MONTAB),MONTAB                                       
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
         LA    R5,MONTAB                                                        
*                                                                               
BLDMON2  OC    SVEXPER,SVEXPER                                                  
         BZ    BLDMONX                                                          
         GOTO1 DATCON,DMCB,(3,SVEXSTB),DUB                                      
         GOTO1 GETMON,DMCB,DUB,2(R5)                                            
         GOTO1 DATCON,DMCB,(3,SVEXENDB),DUB                                     
         GOTO1 GETMON,DMCB,DUB,4(R5)                                            
         MVC   0(2,R5),SVEXPER                                                  
         LA    R5,L'MONTAB(R5)                                                  
         LA    R6,SVEXL(R6)                                                     
         B     BLDMON2                                                          
*                                                                               
BLDMONX  B     EQXIT                                                            
         SPACE 2                                                                
GETMON   ST    RE,SAVERE                                                        
         L     R2,0(R1)            A(DATE)                                      
         L     R4,4(R1)            A(OUTPUT)                                    
         GOTO1 GETDAY,PARAS,0(R2),FULL                                          
         ZIC   R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LNR   R0,R0                                                            
         GOTO1 ADDAY,PARAS,0(R2),WORK,(R0)                                      
         GOTO1 DATCON,PARAS,WORK,(2,0(R4))                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
SPHIGH   MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   DMFILE,=C'SPTDIR'                                                
         TM    SVTRACE,X'80'                                                    
         BZ    SPDIR                                                            
* BUILD TRACE PARAMS                                                            
         LA    R0,KEYSAVE                                                       
         ST    R0,TRIO1                                                         
         MVI   TRIO1,13                                                         
         LA    R0,KEY                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,18                                                         
         B     SPDIR                                                            
*                                                                               
SPSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         MVC   DMFILE,=C'SPTDIR'                                                
         TM    SVTRACE,X'80'                                                    
         BZ    SPDIR                                                            
         LA    R0,KEY                                                           
         ST    R0,TRIO1                                                         
         MVI   TRIO1,13                                                         
         LA    R0,KEY                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,18                                                         
*                                                                               
SPDIR    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,KEY                    
         LR    RE,R0                                                            
         TM    SVTRACE,X'80'                                                    
         BZR   RE                                                               
         B     SPTRACE                                                          
*                                                                               
SPGET    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'SPTFILE',          X        
               KEY+14,AIO,DMWORK                                                
*                                                                               
         LR    RE,R0                                                            
         TM    SVTRACE,X'80'                                                    
         BZR   RE                                                               
         LA    R0,KEY+14                                                        
         ST    R0,TRIO1                                                         
         MVI   TRIO1,4                                                          
         L     R0,AIO                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,16                                                         
         B     SPTRACE                                                          
         EJECT                                                                  
SPTRACE  NTR1                                                                   
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     RE,DMCB                                                          
         MVC   P(6),0(RE)                                                       
         L     RE,DMCB+4                                                        
         MVC   P+8(6),0(RE)                                                     
*                                                                               
         LA    R4,P+16                                                          
         ZIC   R0,TRIO1                                                         
         GOTO1 HEXOUT,DMCB,TRIO1,(R4),(R0),=C'TOG'                              
         A     R4,DMCB+16                                                       
         LA    R4,2(R4)                                                         
*                                                                               
         ZIC   R0,TRIO2            GET OUTPUT REC LEN                           
         GOTO1 HEXOUT,DMCB,TRIO2,(R4),(R0),=C'TOG'                              
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                RETURN TO CALLER                             
         DROP  R8                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
BUFFPUT  MVC   COMMAND(8),=CL8'BPUT'                                            
         B     GOBUFF                                                           
*                                                                               
BUFFGET  MVC   COMMAND(8),=CL8'BGET'                                            
*                                                                               
GOBUFF   LR    R0,RE               SAVE CALLING REG                             
         GOTO1 VBUFFALO,DMCB,COMMAND+1,BUFFBUFF,BUFFREC,1                       
         LR    RE,R0               RESTORE CALLING REG                          
*                                                                               
         TM    SVTRACE,X'40'       TEST TRACE BUFFALO                           
         BZR   RE                                                               
*                                                                               
BUFFTRC  NTR1                                                                   
         MVC   WORK(16),DMCB       SAVE DMCB                                    
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVC   P(5),COMMAND                                                     
*                                                                               
         L     R4,BUFFRULE                                                      
         LA    R5,QRNODE-QRD(R4)                                                
         GOTO1 HEXOUT,DMCB,(R5),P+7,4,=C'TOG'                                   
*                                                                               
         LA    R5,QRCODE-QRD(R4)                                                
         MVC   P+17(8),0(R5)                                                    
*                                                                               
         GOTO1 HEXOUT,DMCB,BUFFREC,P+27,32,=C'TOG'                              
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK+8,P+92,1,=C'TOG'                                
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   DMCB(16),WORK       RESTORE DMCB                                 
         B     EXIT                                                             
*                                                                               
BUFFBUFF DS    A                                                                
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
BADCLT   LA    R4,CLTMSG                                                        
         B     BAD2                                                             
*                                                                               
BADPRD   LA    R4,PRDMSG                                                        
         B     BAD2                                                             
*                                                                               
BADEST   LA    R4,ESTMSG                                                        
         B     BAD2                                                             
*                                                                               
BADPGR   LA    R4,PGRMSG                                                        
         B     BAD2                                                             
*                                                                               
BADMGR   LA    R4,MGRMSG                                                        
         B     BAD2                                                             
*                                                                               
BAD2     GOTO1 VPRTRULE,PARAS,0                                                 
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         MVC   P(8),QRCODE                                                      
         MVC   P+9(7),INVWORD                                                   
         ZIC   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P+17(0),1(R4)                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
INVWORD  DC    C'INVALID'                                                       
*                                                                               
CLTMSG   DC    AL1(06),C'CLIENT'                                                
PRDMSG   DC    AL1(07),C'PRODUCT'                                               
ESTMSG   DC    AL1(08),C'ESTIMATE'                                              
PGRMSG   DC    AL1(13),C'PRODUCT GROUP'                                         
MGRMSG   DC    AL1(12),C'MARKET GROUP'                                          
         SPACE 2                                                                
* TABLE OF VALID EXTRACT TYPES FOR SPOT EXTRACT                                 
*                                                                               
EXTTBL   DS    0CL3                                                             
         DC    AL1(EXORD),AL1(EXORDB),AL1(0)                                    
         DC    AL1(EXBADV),AL1(EXBADVB),AL1(0)                                  
         DC    AL1(EXBDATE),AL1(EXBDATB),AL1(0)                                 
         DC    AL1(EXORDN),AL1(EXORDNB),AL1(0)                                  
         DC    AL1(EXBNADV),AL1(EXBNADVB),AL1(0)                                
         DC    AL1(EXBNDATE),AL1(EXBNDATB),AL1(0)                               
         DC    AL1(EXBINV),AL1(EXBINVB),AL1(0)                                  
         DC    AL1(EXBNINV),AL1(EXBNINVB),AL1(0)                                
         DC    AL1(EXGOAL),AL1(0),AL1(EXGOALB)                                  
         DC    AL1(EXBAADV),AL1(0),AL1(EXBAADVB)                                
         DC    AL1(EXBADAT),AL1(0),AL1(EXBADATB)                                
         DC    AL1(EXBAINV),AL1(0),AL1(EXBAINVB)                                
EXTYPES  EQU   (*-EXTTBL)/L'EXTTBL                                              
         SPACE 2                                                                
SPOTS    DS    F                                                                
GROSS    DS    F                                                                
NET      DS    F                                                                
ADJ      DS    F                                                                
*                                                                               
ASTATION DC    A(STATION)          A(STATION I/O AREA)                          
*                                                                               
AGYMED   DS    X                   AGENCY/MEDIA CODE                            
CLT      DS    CL2                 PACKED CLIENT CODE                           
PRD      DS    CL3                 PRODUCT CODE (ALPHA)                         
BPRD     DS    X                   PRODUCT NUMBER (BINARY)                      
*                                                                               
DATAINDS DS    0XL2                BIT SETTINGS FOR EXTRACTABLE                 
DATAIND  DS    X                   DATA TYPES                                   
DATAIND1 DS    X                                                                
NEXTYPS  DS    X                   N'EXTRACT TYPES                              
EXTYPS   DS    CL(MAXDTYP)         TABLE OF EXTRACT TYPES                       
*                                                                               
STASW    DS    C                                                                
ESTSW    DS    C                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
*                                                                               
B1XPROF  DS    XL16                                                             
MGRSW    DS    CL1                 Y=MARKET GROUP IN STATION BILL KEY           
PGRKEY   DS    CL13                                                             
*                                                                               
FLTMST   DS    XL5                                                              
FLTMGR   DS    CL2                                                              
FLTMKT   DS    XL2                                                              
FLTSTN   DS    XL3                                                              
FLTEST   DS    XL1                                                              
RECTYPE  DS    XL1                                                              
BILL     EQU   X'01'                                                            
BUY      EQU   X'02'                                                            
INVOICE  EQU   X'03'                                                            
GOAL     EQU   X'04'                                                            
ALLSW    DS    C                   PRODUCT=ALL EXTRACT (Y/N)                    
XFF      DC    16X'FF'                                                          
ALSTPRD  DS    A                   A(LAST PRODUCT TABLE ENTRY)                  
RELO     DC    A(0)                                                             
*                                                                               
MONST    DS    XL2                                                              
MONEND   DS    XL2                                                              
MONTAB   DS    13CL6                                                            
*                                                                               
VBUFFALO DS    V                                                                
BINSRCH  DS    V                                                                
NINVENT  DS    F                                                                
INVENT   DS    XL(INVTABL)                                                      
SAVEAIO  DS    A                                                                
SAVEKEY  DS    CL(L'KEY)                                                        
STATION  DS    CL1000              STATION RECORD AREA                          
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MAXENT   EQU   10000                                                            
BILLEDB  EQU   EXBADVB+EXBNADVB+EXBDATB+EXBNDATB+EXBINVB+EXBNINVB               
         SPACE 2                                                                
         BUFF  LINES=250,ROWS=1,COLUMNS=3,FLAVOR=PACKED,               X        
               KEYLIST=(8,A)                                                    
         SPACE 2                                                                
INVTAB   DC    (MAXENT)XL(INVTABL)'00'                                          
         SPACE 2                                                                
* DSECT TO COVER INVOICE TABLE ENTRIES                                          
*                                                                               
INVTABD  DSECT                     **INVOICE TABLE ENTRIES                      
INVMOS   DS    XL2                 BILLING PERIOD                               
INVBM    DS    X                   BILL YEAR/MONTH                              
INVNUM   DS    XL2                 INVOICE NUMBER                               
INVKEYL  EQU   *-INVTABD                                                        
INVQDATE DS    XL3                 INVOICE DATE (YMD)                           
INVQPER  DS    XL2                 EXTRACT PERIOD (YM)                          
INVTABL  EQU   *-INVTABD                                                        
         EJECT                                                                  
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
       ++INCLUDE BUEXTWORKD                                                     
         SPACE 2                                                                
* SPOT CLIENT, PRODUCT, ESTIMATE, STATION, BUY RECORD, STATION                  
* BILLING RECORD, BILLING RECORD, GOAL AND TALENT FACTOR RECORD DSECTS          
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE BUPPERD                                                        
       ++INCLUDE DDCOMFACS                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENSTAB                                                      
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPSTAPACKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075BUFIL42   08/14/02'                                      
         END                                                                    
