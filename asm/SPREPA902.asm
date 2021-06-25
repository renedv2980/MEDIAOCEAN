*          DATA SET SPREPA902  AT LEVEL 018 AS OF 02/07/15                      
*          DATA SET SPREPA902  AT LEVEL 014 AS OF 10/13/10                      
*PHASE SPA902C                                                                  
*INCLUDE SPBVAL                                                                 
*INCLUDE BUFFERIN                                                               
                                                                                
*===================================================================            
* 10AUG10 MHER IF B3 PROFILE SET, READ FILE TO BUCKET DOLLARS BY                
*              SPECIAL BILLING PERIODS                                          
* 20JUL06 MHER QOPT2=N FOR NET DOLLARS                                          
* 02FEB06 MHER AUTODATES (SEE SETDATES ROUTINE)                                 
* 04APR05 MHER IGNORE QSTART AND QEND - PROCESS ALL ESTIMATES                   
*===================================================================            
                                                                                
*===================================================================            
* QAREA+52(2)=QUARTERS BACK                                                     
* QAREA+54(2)=QUARTERS FORWARD                                                  
* QOPT2 =N TO EXTRACT NET DOLLARS                                               
* QOPT3 =Y EXCLUDE CLIENT IF COPT2 IS SET TO EXCLUDE FROM J1 REPORT             
* QOPT4 =Y TO INCLUDE STW ESTIMATES (ELSE EXCLUDED)                             
*===================================================================            
         TITLE 'SPA902 - CREATE AGENCY SUMMARY DATA FOR ACCENT'                 
SPA902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPA902                                                         
         L     RC,=A(SPA9WORK)                                                  
         USING SPA9WORK,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BNE   DS2                                                              
         BRAS  RE,CLTF                                                          
         MVI   MODE,CLTLAST                                                     
         J     EXIT                                                             
*                                                                               
DS2      CLI   MODE,REQFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,REQF                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         J     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*=============================================================                  
* OPEN A SEQUENTIAL DISK OUTPUT FILE                                            
*=============================================================                  
         SPACE 1                                                                
RUNF     DS    0H                                                               
         OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
         EJECT                                                                  
*=================================================================              
* AT RUNLAST, CLOSE FILE                                                        
*=================================================================              
                                                                                
RUNL     CLOSE FILEOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* REQUEST FIRST PROCESSING                                            *         
*===============================================================                
                                                                                
REQF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVRUNTYP,QCODE      SAVE RUN TYPE                                
*                                                                               
         MVC   FILEDATE,TODAY      CREATE HEADER FOR OUTPUT FILE                
         BRAS  RE,SETDATES         SET AUTO DATES IF REQUIRED                   
         MVC   FILEQSTR,QSTART                                                  
         MVC   FILEQEND,QEND                                                    
         CLI   QOPT2,C'N'          TEST NET REQUEST                             
         BNE   *+8                                                              
         MVI   FILEDOLS,C'N'       SET IN FILE HEADER                           
         L     R0,ADBUY                                                         
         LHI   R1,OUTRECLN         FIXED OUTPUT LENGTH                          
         LA    RE,FILEHDR                                                       
         LHI   RF,FILEHDRX-FILEHDR                                              
         MVCL  R0,RE                                                            
         L     R0,ADBUY                                                         
         PUT   FILEOUT,(0)                                                      
         J     EXIT                                                             
         EJECT                                                                  
CLTF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   THISAGY,AGY                                                      
         MVC   THISMED,MED                                                      
         MVC   THISAGMD,BAGYMD                                                  
         MVC   THISCLT,CLT                                                      
*                                                                               
         MVC   THISMDNM,SPACES                                                  
         MVC   THISMDNM(L'MEDNM),MEDNM                                          
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVC   THISMDOF(1),COFFICE MOVE 1 CHAR OFFICE                           
         MVI   THISMDOF+1,C' '                                                  
*                                                                               
         XC    OFCWORK,OFCWORK                                                  
         LA    R4,OFCWORK                                                       
         USING OFFICED,R4                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGY                                                       
         MVC   OFCOFC,COFFICE                                                   
         L     RF,ADCONLST                                                      
         L     RF,VOFFICER-SPADCONS(RF)                                         
         GOTO1 (RF),DMCB,(C'2',OFFICED),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   *+10                                                             
         MVC   THISMDOF,OFCOFC2    USE 2 CHAR OFFICE IF AVAILABLE               
         DROP  R4                                                               
*                                                                               
         MVC   THISACOF,CACCOFC                                                 
         MVC   THISCACC,CACCESS                                                 
         MVC   THISCLNM(L'CNAME),CNAME                                          
         OC    THISCLNM,SPACES                                                  
*                                                                               
         CLI   QOPT3,C'Y'          TEST FOLLOW J1 EXCLUSION OPT                 
         BNE   CLTF2               NO                                           
         TM    COPT2,COP2EXDB      TEST EXCLUDE FROM J1 REQUEST                 
         BZ    CLTF2                                                            
         MVI   MODE,CLTLAST                                                     
         J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
CLTF2    XC    KEY,KEY                                                          
         L     RE,ADCLT                                                         
         MVC   KEY(4),0(RE)        00/A-M/CLT                                   
         GOTO1 HIGH                                                             
                                                                                
*================================================================               
* READ B3 BILLING PROFILE TO CHECK FOR SPECIAL CALENDAR PERIODS                 
*================================================================               
                                                                                
         MVI   BUFFSW,C'N'         SET TO PROCESS ESTHDRS NORMALLY              
         MVI   SPCLCAL,C'N'        RESET FLAG                                   
         XC    PROFB3,PROFB3       CLEAR PREVIOUS PROFILE                       
*                                                                               
         OC    SVSPPROF,SVSPPROF   TEST SPOT PROFILE SAVED YET                  
         BZ    *+10                                                             
         MVC   SPOTPROF,SVSPPROF   IF YES, RESTORE IT NOW                       
         MVC   SVSPPROF,SPOTPROF   AND THEN SAVE IT                             
*                                                                               
         XC    WORK,WORK           READ B2 PROFILE                              
         LA    R6,WORK                                                          
         USING PROFKD,R6                                                        
         MVI   PROFKSYS,C'S'                                                    
         MVC   PROFKPGM,=C'OB3'                                                 
         MVC   PROFKAGN,AGY                                                     
         MVC   PROFKMED,MED                                                     
*                                                                               
         L     R5,ADCLT                                                         
         LA    R4,2(R5)               POINT TO PACKED CLIENT                    
         LA    R5,CPROF+6-CLTHDR(R5)  AND TO CPROF+6                            
         GOTO1 CLUNPK,DMCB,(0(R5),(R4)),PROFKCLI                                
*                                                                               
         L     R5,ADCLT                                                         
         MVI   PROFKOI2,C'*'       SET OFFICE CODE                              
         MVC   PROFKOCD,COFFICE-CLTHDR(R5)                                      
         GOTO1 GETPROF,DMCB,(X'A0',WORK),PROFB3,DATAMGR,,WORK+16                
*                                                                               
         OC    PROFB3,PROFB3       TEST PROFILE FOUND                           
         BZ    CLTF10                                                           
*                                                                               
         CLC   PROFB3(4),=X'00010101' THIS MEANS NO SPECIAL CALENDAR            
         BE    CLTF10                                                           
*                                                                               
         CLC   SPOTPROF+2(1),PROFB3      TEST MATCHES SPOTPROF                  
         BNE   CLTF6                                                            
         CLC   SPOTPROF+6(3),PROFB3+1                                           
         BNE   CLTF6                                                            
         B     CLTF10                                                           
         EJECT                                                                  
*===============================================================                
* READ BUYS FOR ALL POL ESTIMATES AND BUFFER RESULTS BY PRD/EST                 
*===============================================================                
*                                                                               
CLTF6    MVC   SPOTPROF+2(1),PROFB3      SAVE CALENDAR VALUES                   
         MVC   SPOTPROF+6(3),PROFB3+1                                           
         MVI   SPCLCAL,C'Y'              AND SET SPCL CAL FLAG                  
                                                                                
* INITIALIZE BUFFERIN                                                           
                                                                                
         GOTOR BUFFERIN,DMCB,('BUFFAINI',BUFFET),0,ACOMFACS                     
         MVI   BUFFSW,C'N'         SET TO NOT POST DOLLARS THIS PASS            
*                                                                               
         MVC   KEY+4(3),=C'POL'    READ POL ESTHDRS                             
         GOTO1 HIGH                                                             
*                                                                               
CLTF8    GOTO1 SEQ                 GET POL ESTHDR                               
*                                                                               
         CLC   KEY(7),KEYSAVE                                                   
         BNE   CLTF8X                                                           
*                                                                               
         GOTO1 GETEST              READ ESTIMATE HEADER                         
*                                                                               
         BAS   RE,PROEST           CHECK TO PROCESS AND BUILD MONLIST           
         BNE   CLTF8                                                            
*                                                                               
         BRAS  RE,READBUYS         READ POL BUYS FOR THIS EST                   
*                                                                               
         L     R8,ADEST                                                         
         MVC   KEY(13),0(R8)       RESTORE ESTHDR KEY                           
         GOTO1 HIGH                                                             
         B     CLTF8                                                            
*                                                                               
CLTF8X   MVI   BUFFSW,C'Y'         SET TO POST BUFFERIN DOLLARS                 
         L     RE,ADCLT                                                         
         MVC   KEY(13),0(RE)       RESTORE DIR TO CLTHDR                        
         GOTO1 HIGH                                                             
                                                                                
*=============================================================                  
* READ THROUGH ALL PRODUCTS/ESTIMATES/BILLS                                     
*=============================================================                  
                                                                                
CLTF10   GOTO1 SEQ                                                              
*                                                                               
CLTF12   CLC   KEY(4),KEYSAVE      SAME CLT                                     
         BNE   CLTFX                                                            
*                                                                               
CLTF14   OC    KEY+7(6),KEY+7      SHOULD BE A PRODUCT                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEYSAVE,KEY         SAVE PRODUCT KEY                             
         GOTO1 GETPRD                                                           
*                                                                               
         MVC   THISPRD,KEY+4       SET PRODUCT CODE                             
         L     R6,ADPRD                                                         
         USING PRDHDRD,R6                                                       
         MVC   THISPRNM(L'PNAME),PNAME                                          
         OC    THISPRNM,SPACES                                                  
         DROP  R6                                                               
*                                                                               
CLTF18   GOTO1 SEQ                 GET NEXT KEY                                 
*                                                                               
         CLC   KEY(4),KEYSAVE      SAME A-M/CLT                                 
         BNE   CLTFX                                                            
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   CLTF14                                                           
*                                                                               
CLTF20   CLI   KEY+7,0             SHOULD BE AN ESTHDR                          
         BE    CLTF22              NO - SKIP THIS ESTIMATE                      
*                                                                               
         OC    KEY+8(5),KEY+8      SHOULD BE X'00' FOR ESTHDR                   
         BNZ   CLTF22              ESTIMATE MUST BE MISSING                     
*                                                                               
         GOTO1 GETEST                                                           
*                                                                               
         LLC   R0,KEY+7                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISEST,DUB                                                      
*                                                                               
         MVI   ESTSW,C'N'          RESET ACTIVITY SWITCH                        
         MVC   KEYSAVE,KEY         SAVE ESTHDR KEY                              
         BAS   RE,PROEST                                                        
         BE    CLTF30              EST IN REQ PERIOD - PROCESS                  
                                                                                
* SKIP THIS ESTIMATE                                                            
                                                                                
CLTF22   GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         BNE   CLTF32                                                           
         B     CLTF22                                                           
                                                                                
*=============================================================                  
* PROCESS BILLS (IF ANY) UNTIL ESTIMATE CHANGES                                 
*=============================================================                  
*                                                                               
CLTF30   GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(8),KEYSAVE      TEST SAME EST                                
         BNE   CLTF32                                                           
*                                                                               
         GOTO1 GETBILL                                                          
*                                                                               
         BRAS  RE,PROBILL                                                       
         B     CLTF30                                                           
                                                                                
*===============================================================                
* NOW CREATE OUTPUT IF ESTIMATE ACTIVE                                          
*===============================================================                
                                                                                
CLTF32   CLI   ESTSW,C'Y'          TEST ESTIMATE ACTIVE                         
         BNE   CLTF34              NO-SKIP                                      
*                                                                               
         LA    R4,DRECTAB                                                       
         BRAS  RE,OUTPUT           PUT DATA FOR THIS ESTIMATE                   
*                                                                               
CLTF34   CLC   KEY(7),KEYSAVE      TEST SAME PRD                                
         BE    CLTF20                                                           
*                                                                               
         CLC   KEY(4),KEYSAVE      TEST SAME CLT                                
         BE    CLTF14                                                           
*                                                                               
CLTFX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* PROCESS ESTIMATE HEADER - BUILD LIST OF DATES                                 
*==============================================================                 
                                                                                
PROEST   NTR1  BASE=*,LABEL=*                                                   
         L     R8,ADEST                                                         
         USING ESTHDRD,R8                                                       
*&&DO                                                                           
         CLC   QEND,ESTART         REQ END BEFORE EST START                     
         JL    NEQXIT                                                           
         CLC   QSTART,EEND         REQ START AFTER EST END                      
         JH    NEQXIT                                                           
*&&                                                                             
         CLI   ETYPE,C'S'          TEST STW ESTIMATE                            
         BNE   *+12                                                             
         CLI   QOPT4,C'Y'          TEST INCLUDE STW                             
         JNE   NEQXIT                                                           
*                                                                               
         MVC   THISTYPE,ETYPE      SET ESTIMATE TYPE                            
         CLI   THISTYPE,C' '                                                    
         BH    *+8                                                              
         MVI   THISTYPE,C'A'                                                    
*                                                                               
         MVC   THISESNM(L'EDESC),EDESC                                          
         OC    THISESNM,SPACES                                                  
                                                                                
* ESTIMATE IS IN REQUEST PERIOD                                                 
                                                                                
         XC    SVESTST,SVESTST                                                  
         MVC   WORK(6),ESTART                                                   
         MVC   WORK+6(6),WORK                                                   
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+12,14   ADD 14 MONTHS                
                                                                                
*==============================================================                 
* FOR WI (OF COURSE) IF BILLING PERIOD=ESTIMATE PERIOD THEN                     
* THE MONTH IN THE ESTIMATE START DATE CAN BE PRIOR TO THE FIRST                
* BROADCAST MONTH IN THE PERIOD - BUT BILLING USES IT ANYWAY                    
*==============================================================                 
                                                                                
         TM    ECONTROL,EBILESTQ   E TYPE EST?                                  
         BZ    PROE2                                                            
         LHI   R0,-1                                                            
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,(R0) BACK UP ONE MONTH             
*                                                                               
PROE2    CLI   SPCLCAL,C'Y'                                                     
         BNE   PROE2X                                                           
         BRAS  RE,SETSPCL          DO SPECIAL DATE CALC                         
         B     PROE8                                                            
*                                                                               
PROE2X   GOTO1 MOBILE,DMCB,(13,WORK+6),(0,MONLISTP) GET 13 MONTH LIST           
*                                                                               
* BUILD LIST OF BROADCAST MONTHS IN 2-BYTE BINARY                               
                                                                                
PROE4    LA    R4,MONLISTP         MONTH START/END DATES                        
         LHI   R5,12               DO 12 MONTHS ONLY!                           
*                                                                               
PROE6    GOTO1 DATCON,DMCB,(2,2(R4)),WORK                                       
*                                                                               
         GOTO1 DATCON,DMCB,WORK,(3,WORK+6)                                      
*                                                                               
         OC    SVESTST,SVESTST                                                  
         BNZ   *+10                                                             
         MVC   SVESTST,WORK+6      SAVE ESTIMATE START Y/M                      
*                                                                               
         SR    RE,RE                                                            
         IC    RE,WORK+7           GET MONTH NUMBER                             
         BCTR  RE,0                                                             
         AR    RE,RE               X 2                                          
         ST    RE,WORK+12          SAVE 2-BYTE DISPLACEMENT                     
         LA    RE,MONLISTB(RE)                                                  
         MVC   0(2,RE),WORK+6      MOVE BINARY YEAR/MONTH                       
*                                                                               
         GOTO1 (RF),(R1),WORK,WORK+6     GET YYMMDD                             
         L     RE,WORK+12                                                       
         AR    RE,RE               CONVERT TO 4-BYTE DSPL                       
         LA    RE,THISYM01(RE)                                                  
         MVC   0(4,RE),WORK+6      MOVE EBCDIC YYMM                             
*                                                                               
         AHI   R4,4                                                             
         BCT   R5,PROE6                                                         
         MVI   MONLISTB+24,X'FF'                                                
         MVI   THISYM01+48,X'FF'                                                
*                                                                               
PROE8    ZAP   DUB,ECURPDN                                                      
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DUB+7,X'0F'                                                      
         ZAP   THISNPT,DUB         NET PAID TODAY                               
         ZAP   THISCMB,=P'0'       CLEAR CURRENT MONTH BILLED                   
                                                                                
* SINCE MONTH 13 ACCUMULATORS NOT USED, CLEAR THEM NOW!                         
                                                                                
         ZAP   THISOR13,=P'0'                                                   
         ZAP   THISPD13,=P'0'                                                   
         ZAP   THISBL13,=P'0'                                                   
*                                                                               
         LA    R1,EORD                                                          
         CLI   QOPT2,C'N'          TEST NET DOLLARS                             
         BNE   *+8                                                              
         LA    R1,EORDNET                                                       
*                                                                               
         MVC   BLOCK(78),0(R1)       MOVE ORDERED ACCUMS                        
         MVC   BLOCK+78(78),0(R1)     AND AGAIN                                 
*                                                                               
         CLI   SPCLCAL,C'Y'        TEST REPORTING DOLLARS FROM BUYS             
         BNE   PROE12              NO                                           
         CLI   BUFFSW,C'Y'         TEST POSTING DOLLARS THIS PASS               
         JNE   EQXIT                                                            
                                                                                
* GET ORDERED/PAID DOLLARS                                                      
         L     R7,ADBUY                                                         
         USING BUFFRECD,R7                                                      
*                                                                               
         XC    0(256,R7),0(R7)                                                  
         MVC   0(4,R7),KEY+4       MOVE PRD/EST                                 
*                                                                               
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),(R7),ACOMFACS                  
         TM    4(R1),X'90'         TEST EOF OR NOT FOUND                        
         BZ    *+10                                                             
         MVC   0(4,R7),=C'****'    SET FLAG FOR NOT FOUND                       
*                                                                               
         CLC   0(4,R7),KEY+4       TEST DATA FOUND                              
         BNE   PROE10              NO                                           
*                                                                               
         BAS   RE,GETHIMON         GET DSPL TO LAST MONTH IN FULL               
*                                                                               
         LA    R4,BLOCK                                                         
         LA    R1,BUFFORD                                                       
         LA    R0,13                                                            
*                                                                               
PROE8B   ZAP   0(6,R4),0(8,R1)     CONVERT PL8 TO PL6                           
         LA    R4,6(R4)                                                         
         LA    R1,8(R1)                                                         
         BCT   R0,PROE8B                                                        
         MVC   BLOCK+104(104),BLOCK AND COPY AGAIN                              
         B     PROE12                                                           
*                                                                               
PROE10   LA    R1,BLOCK            SET DOLLARS TO 0                             
         LA    R0,13                                                            
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
PROE12   SR    RE,RE                                                            
         IC    RE,MONLISTB+1       GET ESTIMATE START MONTH                     
         BCTR  RE,0                                                             
         MHI   RE,6                X MONTHLY ACCUM LEN                          
*                                                                               
         LA    R4,BLOCK(RE)        POINT TO FIRST ACCUM                         
         LA    R5,THISOR01                                                      
         LA    R6,12                                                            
*                                                                               
PROE14   ZAP   DUB,0(6,R4)                                                      
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DUB+7,X'0F'                                                      
*                                                                               
         CP    DUB,=P'0'                                                        
         BE    *+8                                                              
         MVI   ESTSW,C'Y'          SET ACTIVE FLAG                              
         ZAP   0(8,R5),DUB                                                      
*                                                                               
         AHI   R4,6                                                             
         AHI   R5,L'THISOR01                                                    
         BCT   R6,PROE14                                                        
*                                                                               
         ZAP   DUB,0(6,R4)                                                      
         CP    DUB,=P'0'           POST MONTH 13 TO MONTH 12                    
         BE    PROE16                                                           
         MVI   ESTSW,C'Y'                                                       
         L     R5,FULL             GET DSPL TO LAST MONTH ACCUM                 
         LA    R5,THISOR01(R5)                                                  
         AP    0(8,R5),DUB                                                      
*                                                                               
PROE16   LA    R1,EPAID                                                         
         CLI   QOPT2,C'N'          TEST NET REQUEST                             
         BNE   *+8                                                              
         LA    R1,EPDNET                                                        
*                                                                               
         MVC   BLOCK(78),0(R1)     MOVE BUCKETS                                 
         MVC   BLOCK+78(78),0(R1)    AND AGAIN                                  
*                                                                               
         CLI   SPCLCAL,C'Y'        TEST REPORTING DOLLARS FROM BUYS             
         BNE   PROE22              NO                                           
*                                                                               
         CLC   0(4,R7),KEY+4       TEST DATA FOUND                              
         BNE   PROE22              NO - LEAVE BLOCK WITH ZEROS                  
         DROP  R8                                                               
*                                                                               
         LA    R4,BLOCK                                                         
         LA    R1,BUFFPAID                                                      
         LA    R0,13                                                            
*                                                                               
PROE20   ZAP   0(6,R4),0(8,R1)     CONVERT PL8 TO PL6                           
         LA    R4,6(R4)                                                         
         LA    R1,8(R1)                                                         
         BCT   R0,PROE20                                                        
         MVC   BLOCK+104(104),BLOCK AND COPY AGAIN                              
*                                                                               
PROE22   SR    RE,RE                                                            
         IC    RE,MONLISTB+1       GET ESTIMATE START MONTH                     
         BCTR  RE,0                                                             
         MHI   RE,6                X MONTHLY ACCUM LEN                          
*                                                                               
         LA    R4,BLOCK(RE)        POINT TO FIRST ACCUM                         
         LA    R5,THISPD01                                                      
         LA    R6,12                                                            
*                                                                               
PROE24   ZAP   DUB,0(6,R4)                                                      
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DUB+7,X'0F'                                                      
*                                                                               
         CP    DUB,=P'0'                                                        
         BE    *+8                                                              
         MVI   ESTSW,C'Y'          SET ACTIVE FLAG                              
         ZAP   0(8,R5),DUB                                                      
*                                                                               
         AHI   R4,6                                                             
         AHI   R5,L'THISOR01                                                    
         BCT   R6,PROE24                                                        
*                                                                               
         ZAP   DUB,0(6,R4)                                                      
         CP    DUB,=P'0'           POST MONTH 13 TO MONTH 12                    
         BE    PROE26                                                           
         MVI   ESTSW,C'Y'                                                       
         L     R5,FULL             GET DSPL TO LAST MONTH ACCUM                 
         LA    R5,THISPD01(R5)                                                  
         AP    0(8,R5),DUB                                                      
         DROP  R7                                                               
                                                                                
* CLEAR BILLED ACCUMULATORS                                                     
                                                                                
PROE26   LA    R5,THISBL01                                                      
         LHI   R6,13                                                            
         ZAP   0(8,R5),=P'0'                                                    
         AHI   R5,L'THISBL01                                                    
         BCT   R6,*-10                                                          
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* PROCESS BILLING HEADER RECORD                                                 
*=============================================================                  
PROBILL NTR1   BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ESTSW,C'Y'          SET ESTIMATE ACTIVE                          
*                                                                               
         L     R8,ADBILL                                                        
         USING BILLRECD,R8                                                      
*                                                                               
         CLI   BRETAIL,X'41'       SKIP RETAIL CORP SUMMARIES                   
         JE    EXIT                                                             
*                                                                               
         LA    R4,MONLISTB                                                      
         LA    R5,THISBL01                                                      
         LA    R6,12                                                            
*                                                                               
         CLI   BKEYYSRV+1,13       TEST MONTH 13                                
         BNE   *+8                                                              
         MVI   BKEYYSRV+1,12       POST TO MONTH 12                             
*                                                                               
PROB2    CLC   BKEYYSRV(2),0(R4)                                                
         BE    PROB8                                                            
         LA    R4,2(R4)                                                         
         LA    R5,L'THISBL01(R5)                                                
         BCT   R6,PROB2                                                         
*                                                                               
         BRAS  RE,GETLOMON                                                      
         L     R5,FULL             GET DSPL TO LAST MONTH ACCUM                 
         LA    R5,THISBL01(R5)                                                  
*                                                                               
PROB8    GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD                               
                                                                                
         LA    R4,SPBVGRSP         POINT TO GROSS                               
         CLI   QOPT2,C'N'          TEST NET REQUEST                             
         BNE   *+8                                                              
         LA    R4,SPBVNETP                                                      
         AP    0(8,R5),0(6,R4)                                                  
*                                                                               
PROB10   GOTO1 DATCON,DMCB,BDATE,(3,DUB)                                        
         CLC   DUB(2),TODAYB       BILL DONE THIS MONTH                         
         BNE   *+10                                                             
         AP    THISCMB,0(6,R4)     ADD TO CURRENT MONTH BILLED                  
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* GET DSPL TO LAST MONTH IN CASE HAVE TO POST MONTH 13 DATA                     
*================================================================               
                                                                                
GETHIMON NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,MONLISTB                                                      
         LA    R0,12                                                            
         LR    RE,R1                                                            
*                                                                               
GETHIM2  CLC   0(2,R1),0(RE)       HIGHER THAN PREVIOUS                         
         BL    *+6                                                              
         LR    RE,R1               SAVE POINTER                                 
         LA    R1,2(R1)                                                         
         BCT   R0,GETHIM2                                                       
*                                                                               
         LLC   RF,1(RE)            GET MONTH NUMBER                             
         BCTR  RF,0                                                             
         MHI   RF,L'THISOR01       X MONTHLY ACCUM LEN                          
         ST    RF,FULL             SAVE DSPL TO MONTH 12 ACCUM                  
         J     EXIT                                                             
*                                                                               
GETLOMON NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,MONLISTB                                                      
         LA    R0,12                                                            
         LR    RE,R1                                                            
*                                                                               
GETLOM2  CLC   0(2,R1),0(RE)       HIGHER THAN PREVIOUS                         
         BH    *+6                                                              
         LR    RE,R1               SAVE POINTER                                 
         LA    R1,2(R1)                                                         
         BCT   R0,GETLOM2                                                       
*                                                                               
         LLC   RF,1(RE)            GET MONTH NUMBER                             
         BCTR  RF,0                                                             
         MHI   RF,L'THISOR01       X MONTHLY ACCUM LEN                          
         ST    RF,FULL             SAVE DSPL TO MONTH 12 ACCUM                  
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* BUILD DATES FOR SPECIAL CALENDAR PERIODS (THANK YOU GRANT PLATT)              
*==================================================================             
                                                                                
SETSPCL  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R0,ADBUY                                                         
         LA    R1,2000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R8,ADEST                                                         
         USING ESTHDRD,R8                                                       
*                                                                               
         GOTO1 DATCON,DMCB,ESTART,WORK  GET DDS FORM DATES                      
         MVC   ESTART(6),WORK                                                   
         MVC   WORK+4(2),=C'01'    FOR MOBILE CALL, USE DAY 01                  
         GOTO1 (RF),(R1),EEND,WORK+6                                            
         MVC   EEND(6),WORK+6                                                   
                                                                                
* GO BACK 3 YEARS AND FORWARD 1                                                 
                                                                                
         LA    R0,3                                                             
         LCR   R0,R0                                                            
         GOTO1 ADDAY,(R1),(C'Y',WORK),WORK,(R0)                                 
         LA    R0,1                                                             
         GOTO1 (RF),(R1),(C'Y',WORK+6),WORK+6,(R0)                              
*                                                                               
         LLC   R0,SPOTPROF+2                                                    
         GOTO1 MOBILE,DMCB,(60,WORK),((R0),ADBUY),0                             
                                                                                
* NOW WORK OUT PERIOD NUMBERS - FIND PLACES WHERE YEAR CHANGES                  
* USE ADBUY+1000 AS WORK AREA                                                   
                                                                                
         L     R4,ADBUY                                                         
         LR    R5,R4                                                            
         LA    R5,1000(R5)                                                      
*                                                                               
SETSP02  DS    0H                                                               
         BAS   RE,CKNEWYR          TEST START OF NEW YEAR                       
         BE    SETSP10             YES                                          
         LA    R4,4(R4)                                                         
         B     SETSP02                                                          
*                                                                               
SETSP10  LLC   R1,2(R4)            GET YEAR FROM END DATE                       
         SRL   R1,1                                                             
         STC   R1,BYTE                                                          
         LA    R1,1                                                             
*                                                                               
SETSP12  DS    0H                                                               
         MVC   0(1,R5),BYTE        YEAR                                         
         STC   R1,1(R5)            MONTH                                        
         MVC   2(4,R5),0(R4)       START-END OF PERIOD                          
                                                                                
* IF JUST DID MONTH 13, CHANGE END DATE OF PERIOD 12 AND ERASE                  
                                                                                
         CLI   1(R5),13                                                         
         BNE   SETSP14                                                          
         AHI   R5,-6               BACK UP TO PREVIOUS ENTRY                    
         MVC   4(2,R5),2(R4)       MOVE END OF MON13 TO MON12                   
         XC    6(6,R5),6(R5)       ERASE MON 13 ENTRY                           
*                                                                               
SETSP14  LA    R5,6(R5)                                                         
         LA    R4,4(R4)                                                         
*                                                                               
         CLI   0(R4),X'FF'         TEST E-O-L                                   
         BE    SETSP20                                                          
*                                                                               
         LA    R1,1(R1)                                                         
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BNE   SETSP12             NO - CONTINUE                                
         B     SETSP10                                                          
                                                                                
* NOW FIND THE ENTRY WITH THAT CONTAINS ESTART                                  
                                                                                
         L     R8,ADEST                                                         
         USING ESTHDRD,R8                                                       
                                                                                
SETSP20  GOTO1 DATCON,DMCB,ESTART,(2,HALF)                                      
         L     R5,ADBUY                                                         
         LA    R5,1000(R5)                                                      
         LA    R6,12               SET FOR 12 PERIODS                           
         DROP  R8                                                               
*                                                                               
SETSP22  CLC   HALF,4(R5)          PRIOR TO END DATE                            
         BNH   SETSP24                                                          
         LA    R5,6(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   SETSP22                                                          
         DC    H'0'                                                             
*                                                                               
SETSP24  LLC   RE,1(R5)            GET MONTH NUMBER                             
         BCTR  RE,0                                                             
         AR    RE,RE               X 2                                          
         LR    RF,RE               SAVE 2-BYTE DSPL                             
         LA    RE,MONLISTB(RE)     POINT TO MONTH SLOT                          
         MVC   0(2,RE),0(R5)       MOVE BINARY Y/M                              
*                                                                               
         OC    SVESTST,SVESTST     TEST FIRST TIME                              
         BNZ   *+10                                                             
         MVC   SVESTST,0(R5)       SAVE START MONTH HERE TOO                    
*                                                                               
         AR    RF,RF               GIVES 4-BYTE DSPL                            
         LR    R4,RF               SAVE IT                                      
         LA    RF,MONLISTP(RF)                                                  
         MVC   0(4,RF),2(R5)       MOVE PACKED START/END                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,(R5)),WORK                                        
         LA    R4,THISYM01(R4)                                                  
         MVC   0(4,R4),WORK        MOVE EBCDIC YYMM                             
*                                                                               
         LA    R5,6(R5)            NEXT PERIOD ENTRY                            
         BCT   R6,SETSP24                                                       
*                                                                               
         MVC   THISYM13,SPACES     NEVER BUILD MONTH 13                         
*====>                                                                          
         CLC   MONLISTP-8(8),=C'MONLISTP'  TEST CORE DAMAGED                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   MONLISTB-8(8),=C'MONLISTB'  TEST CORE DAMAGED                    
         BE    *+6                                                              
         DC    H'0'                                                             
*====>                                                                          
         J     EQXIT                                                            
                                                                                
*======================================================================         
* FIND START OF NEW YEAR                                                        
* 1) A PERIOD THAT SPANS YEAR CHANGE AND BEGINS NO FURTHER                      
*    AWAY FROM 12/31 THAN IT ENDS                                               
* --OR--                                                                        
* 2) A PERIOD THAT STARTS BEFORE 1/14                                           
*                                                                               
* B'YYYY|YYYM|MMMD|DDDD'                                                        
*======================================================================         
                                                                                
CKNEWYR  DS    0H                                                               
         MVC   DUB(4),0(R4)                                                     
         NI    DUB,X'01'           STRIP YEAR FROM START DATE                   
         CLC   DUB(2),JAN14        IS START DATE JAN01 TO JAN14                 
         BL    CKNYYES             YES THEN NEW YEAR                            
*                                                                               
         CLC   DUB(2),DEC00        IS START DATE LESS THEN DECEMBER?            
         BNH   CKNYNO              YES THEN NOT NEW YEAR                        
*                                                                               
         NI    DUB+2,X'01'         STRIP YEAR FROM END DATE                     
         CLC   DUB+2(2),DEC00      END IS IN DECEMBER                           
         BH    CKNYNO              YES SO NOT NEW YEAR                          
*                                                                               
         NI    DUB+1,X'1F'         ISOLATE START DAY                            
         ZIC   RF,DUB+1                                                         
         LA    R0,30                                                            
         SR    R0,RF               30 DAYS LESS CURRENT DAY                     
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4            1 TO 29 DAYS                                 
*                                                                               
         NI    DUB+3,X'1F'         ISOLATE END DAY                              
         CLC   DUB+4(1),DUB+3      1 TO 29 DAYS LEFT (ASSUMING JAN)             
         BNH   CKNYYES             SEE COMMENT 1 ABOVE                          
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,2(R4)          GET END DATE INTO RF                         
         NILL  GRF,X'01E0'         MASK OUT MONTH                               
         CHI   RF,X'0040'          IS IT FEBRUARY?                              
         BE    CKNYYES             DEC TO FEB RANGE                             
*                                                                               
CKNYNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
CKNYYES  DS    0H                                                               
         SR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
JAN14    DC    X'002E'             JAN14                                        
FEB00    DC    X'0040'             FEB00                                        
DEC00    DC    X'0180'             DEC00                                        
         EJECT                                                                  
*================================================================               
* READ BUY RECORDS FOR THIS ESTIMATE FOR SPECIAL BILLING CALENDAR               
* SAVE DOLLARS BY BRAND IN BUYTAB                                               
*================================================================               
                                                                                
READBUYS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,220              CLEAR BUYS/PRD BUFFER                        
         L     RE,=A(BUYDOLS)                                                   
*                                                                               
RB2      XC    0(8,RE),0(RE)       ROOM FOR PRD CODE                            
*                                                                               
         LA    RE,8(RE)            POINT TO FIRST ACCUM                         
         LA    RF,26                                                            
*                                                                               
         ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,*-10             26 ACCUMS/PRD                                
*                                                                               
         BCT   R0,RB2              220 PRDS                                     
*                                                                               
         L     R8,ADEST                                                         
         USING ESTHDR,R8                                                        
         MVC   BEST,EKEYEST        SAVE ESTIMATE NUMBER                         
         DROP  R8                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),1(R8)        MOVE A-M/CLT                                 
         MVI   KEY+3,X'FF'         SET POL                                      
*                                                                               
RB4      GOTO1 HIGH                                                             
*                                                                               
RB4CLC   CLC   KEY(4),KEYSAVE      SAME A-M/CLT/FF                              
         BNE   RB20                                                             
*                                                                               
         CLC   KEY+9(1),BEST                                                    
         BE    RB10                                                             
         BH    RB6                 HIGH -                                       
         MVC   KEY+9(1),BEST       LOW - READ FOR EST                           
         XC    KEY+10(3),KEY+10                                                 
         B     RB4                                                              
*                                                                               
RB6      MVC   KEY+9(4),=4X'FF'    READ NEXT STATION                            
         B     RB4                                                              
*                                                                               
RB10     CLI   KEY+10,0            SKIP PASSIVE POINTERS (SPILL, P/B)           
         BNE   RB14                                                             
*                                                                               
         GOTO1 GETBUY              READ BUY RECORD                              
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
*                                                                               
RB12     BRAS  RE,NEXTEL                                                        
         BNE   RB14                                                             
                                                                                
         MVI   HALF,X'FF'                                                       
         MVC   HALF+1(1),BDSEC                                                  
         CLI   1(R6),10            TEST UNALL                                   
         BNH   *+10                (WAS BE  RB12)                               
         MVC   HALF(2),10(R6)                                                   
         GOTO1 GETRATE,DMCB,(HALF,SPOTS),(HALF+1,(R8)),(R6)                     
*                                                                               
         BRAS  RE,POSTBUY                                                       
*                                                                               
         CLI   1(R6),14                                                         
         BNH   RB12                                                             
*                                                                               
         GOTO1 (RF),(R1),(14(R6),SPOTS),(15(R6),(R8)),(R6)                      
*                                                                               
         BRAS  RE,POSTBUY                                                       
         B     RB12                                                             
*                                                                               
RB14     GOTO1 SEQ                                                              
         B     RB4CLC                                                           
                                                                                
*===========================================================                    
* NOW OUTPUT RECORDS TO BUFFERIN                                                
*===========================================================                    
                                                                                
RB20     L     R8,ADBUY                                                         
         USING BUFFRECD,R8                                                      
         XC    0(256,R8),0(R8)                                                  
                                                                                
* FIND DOLLARS IN BUYDOLS BUFFER                                                
                                                                                
         L     R4,=A(BUYDOLS)                                                   
         LA    R5,220                                                           
*                                                                               
RB24     OC    0(4,R4),0(R4)       TEST FOR DATA                                
         BZ    RB26                                                             
         GOTOR BUFFERIN,DMCB,('BUFFAPUT',BUFFET),(R4),ACOMFACS                  
         MVC   4(4,R4),=F'-1'      SET FLAG DATA PROCESSED                      
*                                                                               
RB26     LA    R4,BUFFRECL(R4)                                                  
         BCT   R5,RB24                                                          
*                                                                               
RBX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* POST GETRATE OUTPUT TO MONTHLY ACCUM FOR THIS PRODUCT                         
*============================================================                   
                                                                                
POSTBUY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
                                                                                
         LA    R4,MONLISTP         MONTH START/END DATES                        
         LA    R5,12               DO 12 MONTHS ONLY!                           
*                                                                               
PB2      CLC   2(2,R6),0(R4)       SPOT PRIOR TO MONTH START                    
         BL    PB4                                                              
         CLC   2(2,R6),2(R4)       SPOT AFTER MONTH END                         
         BNH   PB6                                                              
*                                                                               
PB4      LA    R4,4(R4)                                                         
         BCT   R5,PB2                                                           
         LA    R4,MONLISTP+48      POINT TO LAST ENTRY                          
*                                                                               
PB6      LA    RE,MONLISTP                                                      
         SR    R4,RE               GIVES DSPL TO THIS PERIOD                    
         SRL   R4,2                GIVES PERIOD NUMBER - 1                      
         MHI   R4,L'BUFFORD        X SIZE OF ACCUM PER MONTH                    
                                                                                
         LLC   RE,DMCB             GET PRD CODE                                 
         CLI   DMCB,X'FF'                                                       
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MHI   RE,BUFFRECL         GIVES DSPL TO ROW FOR THIS PRD               
         A     RE,=A(BUYDOLS)      POINT TO ACCUM ROW                           
*                                                                               
         OC    0(4,RE),0(RE)       TEST HAVE PRD CODE                           
         BNZ   PB12                                                             
*                                                                               
         LA    RF,=C'POL'                                                       
         CLI   DMCB,X'FF'                                                       
         BE    PB10                                                             
*                                                                               
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
PB8      CLC   DMCB(1),3(RF)                                                    
         BE    PB10                                                             
         LA    RF,4(RF)                                                         
         CLI   0(RF),C'A'                                                       
         BNL   PB8                                                              
         LA    RF,=C'???'                                                       
*                                                                               
PB10     MVC   0(3,RE),0(RF)       MOVE PRD CODE TO BUFFER                      
         MVC   3(1,RE),BEST        AND ESTIMATE                                 
*                                                                               
PB12     LA    R4,BUFFORD-BUFFRECD(R4,RE)   POINT TO MONTHLY ACCUM              
         L     R0,GROSS                                                         
         CLI   QOPT2,C'N'          TEST REPORT NET                              
         BNE   *+8                                                              
         L     R0,NET                                                           
         CVD   R0,DUB                                                           
         AP    0(8,R4),DUB         ADD TO ORD DOLS                              
*                                                                               
         OC    4(2,R6),4(R6)       TEST SPOT PAID                               
         BZ    *+10                                                             
         AP    BUFFPAID-BUFFORD(8,R4),DUB   ADD TO PAID DOLS                    
*                                                                               
PBX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* CREATE OUTPUT RECORDS                                                         
*=============================================================                  
                                                                                
OUTPUT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ADBUY            BUILD OUTPUT RECORD IN BUYREC                
         LR    R0,R6                                                            
         LHI   R1,OUTRECLN         FIXED OUTPUT LENGTH                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
OUTPUT2  L     RE,0(R4)            GET DATA ADDR                                
         SR    RF,RF                                                            
         IC    RF,4(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(RE)                                                    
*                                                                               
         LA    R6,1(R6,RF)         NEXT OUTPUT POSITION                         
         AHI   R4,8                NEXT INPUT FIELD                             
         CLI   0(R4),X'FF'                                                      
         BNE   OUTPUT2                                                          
*                                                                               
         L     R0,ADBUY                                                         
         PUT   FILEOUT,(0)                                                      
         AP    RUNRECS,=P'1'                                                    
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* SET AUTO REQUEST DATES IF REQUIRED                                            
*=================================================================              
                                                                                
SETDATES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    AUTOSTRT,AUTOSTRT                                                
         XC    AUTOEND,AUTOEND                                                  
*                                                                               
         CLC   QAREA+52(2),=C'  '  TEST QUARTERS BACK                           
         JE    EXIT                NO                                           
         PACK  DUB,QAREA+52(2)                                                  
         CVB   R0,DUB                                                           
         MHI   R0,3                EACH QUARTER IS 3 MONTHS                     
         AHI   R0,2                GO BACK 2 EXTRA MONTHS                       
         LNR   R0,R0                                                            
         MVC   WORK(6),TODAY                                                    
         MVC   WORK+4(2),=C'15'                                                 
*                                                                               
SETDAT2  GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,(R0)                               
*                                                                               
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK+2(2),=C'01'   TEST FOR A MONTH THAT STARTS A QTR            
         BE    SETDAT4                                                          
         CLC   WORK+2(2),=C'04'                                                 
         BE    SETDAT4                                                          
         CLC   WORK+2(2),=C'07'                                                 
         BE    SETDAT4                                                          
         CLC   WORK+2(2),=C'10'                                                 
         BE    SETDAT4                                                          
         LHI   R0,1                SET TO ADVANCE A MONTH                       
         B     SETDAT2                                                          
                                                                                
* GET BROADCAST MONTH DATES FROM MOBILE                                         
                                                                                
SETDAT4  MVC   WORK+6(6),WORK      SET END DATE=TO DATE                         
         GOTO1 MOBILE,DMCB,(2,WORK),(1,WORK+16)                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,WORK+16),AUTOSTRT                                 
*                                                                               
         PACK  DUB,QAREA+54(2)                                                  
         CVB   R0,DUB                                                           
         BCTR  R0,0                ARITHMETIC                                   
         MHI   R0,3                X 3                                          
         AHI   R0,2                AND 2 MORE MONTHS                            
         MVC   WORK(6),TODAY                                                    
         MVC   WORK+4(2),=C'15'                                                 
*                                                                               
SETDAT6  GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,(R0)                               
*                                                                               
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK+2(2),=C'03'   TEST FOR A MONTH THAT ENDS A QTR              
         BE    SETDAT8                                                          
         CLC   WORK+2(2),=C'06'                                                 
         BE    SETDAT8                                                          
         CLC   WORK+2(2),=C'09'                                                 
         BE    SETDAT8                                                          
         CLC   WORK+2(2),=C'12'                                                 
         BE    SETDAT8                                                          
         LHI   R0,1                SET TO ADVANCE A MONTH                       
         B     SETDAT6                                                          
*                                                                               
SETDAT8  MVC   WORK+6(6),WORK      SET END DATE=TO DATE                         
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK+10(2),=C'31'                                                
         GOTO1 MOBILE,DMCB,(2,WORK),(1,WORK+16)                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,WORK+18),AUTOEND  GET BRDMON END DATE             
*                                                                               
         MVC   QSTART,AUTOSTRT                                                  
         MVC   QEND,AUTOEND                                                     
         J     EXIT                                                             
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
SPA9WORK DS    0D                                                               
         DC    CL8'SPA9WORK'                                                    
BUFFERIN DC    V(BUFFERIN)                                                      
AUTOSTRT DS    CL6                                                              
AUTOEND  DS    CL6                                                              
BUFFSW   DS    C                                                                
SPCLCAL  DS    CL1                                                              
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
         DS    CL1                 SPARE                                        
         DS    0D                                                               
PROFB3   DS    CL16                                                             
         DS    0D                                                               
         DC    C'MONLISTP'                                                      
MONLISTP DS    XL64                2 BYTE BRDCST MONTH ST/END DATES             
         DC    C'MONLISTB'                                                      
MONLISTB DS    XL96                YEAR/MONTH BINARY                            
         DS    0D                                                               
BLOCK    DS    XL256                                                            
         DS    0D                                                               
OFCWORK  DS    CL64                                                             
         DS    0D                                                               
       ++INCLUDE SPBVALD                                                        
*                                                                               
SVRUNTYP DS    CL2                                                              
SVESTST  DS    XL2                                                              
SVSPPROF DS    CL16                ORIGINAL SPOT PROFILE                        
*                                                                               
FILEHDR  DS    0D                                                               
FILEID   DC    CL6'SA9HDR'                                                      
FILESYS  DC    C'SPOT'                                                          
FILEDATE DC    C'YYMMDD'           FILE CREATION DATE                           
FILEQSTR DC    C'YYMMDD'           REQUEST START DATE                           
FILEQEND DC    C'YYMMDD'           REQUEST END DATE                             
FILEDOLS DC    C'G'                G=GROSS,N=NET                                
FILEHDRX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'**FLD***'                                                    
FLD      DS    CL32                                                             
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=FB,BLKSIZE=19200,         X        
               MACRF=PM,LRECL=768                                               
OUTRECLN EQU   768                                                              
                                                                                
         DS    0D                                                               
         DC    CL8'RUNTOTS'                                                     
RUNTOTS  DS    0D                                                               
RUNRECS  DC    PL8'0'                                                           
RUNORD   DC    PL8'0'                                                           
RUNPAID  DC    PL8'0'                                                           
RUNBLLD  DC    PL8'0'                                                           
*                                                                               
         EJECT                                                                  
* ENTRIES ARE                                                                   
* AL4(DATA)                                                                     
* AL1(L'DATA)                                                                   
* CL1'TYPE'                                                                     
* C'  '              IF NOT X'00' EOR IF FIELD NOT > THIS VALUE                 
* X'01'              CONVERT THE FIELD TO DECIMAL BEFORE WRITE                  
* OR IF TYPE=B OR P  LAST BYTE IS NUMBER OF DECIMAL PLACES                      
         SPACE 1                                                                
         DS    0D                                                               
RECTAB   DS    0XL8                                                             
         DC    CL8'DRECTAB*'                                                    
DRECTAB  DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISAGMD),AL1(L'THISAGMD),C'T',2X'00'                        
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISPRD),AL1(L'THISPRD),C'T',2X'00'                          
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISTYPE),AL1(L'THISTYPE),C'T',2X'00'                        
         DC    AL4(THISMDOF),AL1(L'THISMDOF),C'T',2X'00'                        
         DC    AL4(THISACOF),AL1(L'THISACOF),C'T',2X'00'                        
         DC    AL4(THISCACC),AL1(L'THISCACC),C'T',2X'00'                        
         DC    AL4(THISCMB),AL1(L'THISCMB),C'P',X'00',X'02'                     
         DC    AL4(THISNPT),AL1(L'THISNPT),C'P',X'00',X'02'                     
         DC    AL4(THISMDNM),AL1(L'THISMDNM),C'T',2X'00'                        
         DC    AL4(THISCLNM),AL1(L'THISCLNM),C'T',2X'00'                        
         DC    AL4(THISPRNM),AL1(L'THISPRNM),C'T',2X'00'                        
         DC    AL4(THISESNM),AL1(L'THISESNM),C'T',2X'00'                        
*                                                                               
         DC    AL4(THISYM01),AL1(L'THISYM01),C'T',2X'00'                        
         DC    AL4(THISOR01),AL1(L'THISOR01),C'P',X'00',X'02'                   
         DC    AL4(THISPD01),AL1(L'THISPD01),C'P',X'00',X'02'                   
         DC    AL4(THISBL01),AL1(L'THISBL01),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM02),AL1(L'THISYM02),C'T',2X'00'                        
         DC    AL4(THISOR02),AL1(L'THISOR02),C'P',X'00',X'02'                   
         DC    AL4(THISPD02),AL1(L'THISPD02),C'P',X'00',X'02'                   
         DC    AL4(THISBL02),AL1(L'THISBL02),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM03),AL1(L'THISYM03),C'T',2X'00'                        
         DC    AL4(THISOR03),AL1(L'THISOR03),C'P',X'00',X'02'                   
         DC    AL4(THISPD03),AL1(L'THISPD03),C'P',X'00',X'02'                   
         DC    AL4(THISBL03),AL1(L'THISBL03),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM04),AL1(L'THISYM04),C'T',2X'00'                        
         DC    AL4(THISOR04),AL1(L'THISOR04),C'P',X'00',X'02'                   
         DC    AL4(THISPD04),AL1(L'THISPD04),C'P',X'00',X'02'                   
         DC    AL4(THISBL04),AL1(L'THISBL04),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM05),AL1(L'THISYM05),C'T',2X'00'                        
         DC    AL4(THISOR05),AL1(L'THISOR05),C'P',X'00',X'02'                   
         DC    AL4(THISPD05),AL1(L'THISPD05),C'P',X'00',X'02'                   
         DC    AL4(THISBL05),AL1(L'THISBL05),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM06),AL1(L'THISYM06),C'T',2X'00'                        
         DC    AL4(THISOR06),AL1(L'THISOR06),C'P',X'00',X'02'                   
         DC    AL4(THISPD06),AL1(L'THISPD06),C'P',X'00',X'02'                   
         DC    AL4(THISBL06),AL1(L'THISBL06),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM07),AL1(L'THISYM07),C'T',2X'00'                        
         DC    AL4(THISOR07),AL1(L'THISOR07),C'P',X'00',X'02'                   
         DC    AL4(THISPD07),AL1(L'THISPD07),C'P',X'00',X'02'                   
         DC    AL4(THISBL07),AL1(L'THISBL07),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM08),AL1(L'THISYM08),C'T',2X'00'                        
         DC    AL4(THISOR08),AL1(L'THISOR08),C'P',X'00',X'02'                   
         DC    AL4(THISPD08),AL1(L'THISPD08),C'P',X'00',X'02'                   
         DC    AL4(THISBL08),AL1(L'THISBL08),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM09),AL1(L'THISYM09),C'T',2X'00'                        
         DC    AL4(THISOR09),AL1(L'THISOR09),C'P',X'00',X'02'                   
         DC    AL4(THISPD09),AL1(L'THISPD09),C'P',X'00',X'02'                   
         DC    AL4(THISBL09),AL1(L'THISBL09),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM10),AL1(L'THISYM10),C'T',2X'00'                        
         DC    AL4(THISOR10),AL1(L'THISOR10),C'P',X'00',X'02'                   
         DC    AL4(THISPD10),AL1(L'THISPD10),C'P',X'00',X'02'                   
         DC    AL4(THISBL10),AL1(L'THISBL10),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM11),AL1(L'THISYM11),C'T',2X'00'                        
         DC    AL4(THISOR11),AL1(L'THISOR11),C'P',X'00',X'02'                   
         DC    AL4(THISPD11),AL1(L'THISPD11),C'P',X'00',X'02'                   
         DC    AL4(THISBL11),AL1(L'THISBL11),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM12),AL1(L'THISYM12),C'T',2X'00'                        
         DC    AL4(THISOR12),AL1(L'THISOR12),C'P',X'00',X'02'                   
         DC    AL4(THISPD12),AL1(L'THISPD12),C'P',X'00',X'02'                   
         DC    AL4(THISBL12),AL1(L'THISBL12),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM13),AL1(L'THISYM13),C'T',2X'00'                        
         DC    AL4(THISOR13),AL1(L'THISOR13),C'P',X'00',X'02'                   
         DC    AL4(THISPD13),AL1(L'THISPD13),C'P',X'00',X'02'                   
         DC    AL4(THISBL13),AL1(L'THISBL13),C'P',X'00',X'02'                   
*                                                                               
         DC    X'FF'               END OF TABLE                                 
*                                                                               
DUMMYNAM EQU   *                                                                
THISBLNK DC   CL8' '                                                            
*                                                                               
THISREC  DS    0D                                                               
*                                                                               
THISAGY  DS    CL2                                                              
THISMED  DS    CL1                                                              
THISAGMD DS    XL1                                                              
THISCLT  DS    CL3                                                              
THISPRD  DS    CL3                                                              
THISEST  DS    CL3                                                              
*                                                                               
THISTYPE DS    CL1                                                              
THISMDOF DS    CL2                                                              
THISACOF DS    CL2                                                              
THISCACC DS    CL3                                                              
*                                                                               
         DS    0D                                                               
THISCMB  DS    PL8                 CURRENT MONTH BILLED                         
THISNPT  DS    PL8                 NET PAID TODAY                               
*                                                                               
THISMDNM DS    CL24                                                             
THISCLNM DS    CL24                                                             
THISPRNM DS    CL24                                                             
THISESNM DS    CL24                                                             
*                                                                               
THISYM01 DS    CL4                                                              
THISYM02 DS    CL4                                                              
THISYM03 DS    CL4                                                              
THISYM04 DS    CL4                                                              
THISYM05 DS    CL4                                                              
THISYM06 DS    CL4                                                              
THISYM07 DS    CL4                                                              
THISYM08 DS    CL4                                                              
THISYM09 DS    CL4                                                              
THISYM10 DS    CL4                                                              
THISYM11 DS    CL4                                                              
THISYM12 DS    CL4                                                              
THISYM13 DS    CL4                                                              
         DS    CL4                 ROOM FOR X'FF' TERMINATOR                    
         DS    XL6                                                              
*                                                                               
         DS    0D                                                               
THISOR01 DS    PL8                                                              
THISOR02 DS    PL8                                                              
THISOR03 DS    PL8                                                              
THISOR04 DS    PL8                                                              
THISOR05 DS    PL8                                                              
THISOR06 DS    PL8                                                              
THISOR07 DS    PL8                                                              
THISOR08 DS    PL8                                                              
THISOR09 DS    PL8                                                              
THISOR10 DS    PL8                                                              
THISOR11 DS    PL8                                                              
THISOR12 DS    PL8                                                              
THISOR13 DS    PL8                                                              
*                                                                               
         DS    0D                                                               
THISPD01 DS    PL8                                                              
THISPD02 DS    PL8                                                              
THISPD03 DS    PL8                                                              
THISPD04 DS    PL8                                                              
THISPD05 DS    PL8                                                              
THISPD06 DS    PL8                                                              
THISPD07 DS    PL8                                                              
THISPD08 DS    PL8                                                              
THISPD09 DS    PL8                                                              
THISPD10 DS    PL8                                                              
THISPD11 DS    PL8                                                              
THISPD12 DS    PL8                                                              
THISPD13 DS    PL8                                                              
*                                                                               
         DS    0D                                                               
THISBL01 DS    PL8                                                              
THISBL02 DS    PL8                                                              
THISBL03 DS    PL8                                                              
THISBL04 DS    PL8                                                              
THISBL05 DS    PL8                                                              
THISBL06 DS    PL8                                                              
THISBL07 DS    PL8                                                              
THISBL08 DS    PL8                                                              
THISBL09 DS    PL8                                                              
THISBL10 DS    PL8                                                              
THISBL11 DS    PL8                                                              
THISBL12 DS    PL8                                                              
THISBL13 DS    PL8                                                              
*                                                                               
THISRECX EQU   *                                                                
THISRECL EQU   THISRECX-THISREC                                                 
*                                                                               
BUFFET   BUFFD TYPE=P,KEYLEN=8,FILE=BUFFWK,BUFFERS=10,COLUMNS=26                
*                                                                               
         DS    0D                                                               
         DC    CL8'BUYDOLS'                                                     
BUYDOLS  DS    (220*BUFFRECL)X        PRD(4) +13ORD/13PD PL8 ACCUMS             
BUYDOLX  EQU   *                                                                
*                                                                               
BUFFRECD DSECT                                                                  
*                                                                               
BUFFKEY  DS    0XL4                                                             
BUFFPRD  DS    CL3                                                              
BUFFEST  DS    XL1                                                              
         DS    XL4                                                              
BUFFORD  DS    13PL8                                                            
BUFFPAID DS    13PL8                                                            
BUFFRECL EQU   *-BUFFKEY                                                        
         EJECT                                                                  
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
*                                                                               
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
*                                                                               
PMED     DS    CL1                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PMOS     DS    CL6                                                              
         DS    CL1                                                              
PORD     DS    CL10                                                             
         DS    CL1                                                              
PPAID    DS    CL10                                                             
         DS    CL1                                                              
PBILLED  DS    CL10                                                             
*                                                                               
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDGETPROFD                                                     
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018SPREPA902 02/07/15'                                      
         END                                                                    
