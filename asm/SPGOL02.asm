*          DATA SET SPGOL02    AT LEVEL 017 AS OF 11/19/19                      
*PHASE T20202C                                                                  
*INCLUDE HEXOUT                                                                 
         SPACE 2                                                                
*===========================================================*                   
* 05FEB92  IF DPT=+ OR - DO NOT CARRY TO NEXT LINE                              
* 22FEB89  IF DPT=$, DO NOT CARRY DPT/LEN TO NEXT LINE      *                   
* 26SEP95  IDR OPTION                                       *                   
*===========================================================*                   
         TITLE 'SPGOL02 - SPOTPAK GOALS - RECALL'                               
T20202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20202                                                         
         L     RC,0(R1)                                                         
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         L     RA,4(R1)                                                         
         USING T202FFD,RA                                                       
*                                                                               
         XC    BDATA,BDATA         CLEAR OLD DATA VALUES                        
         LA    R2,GOLACT1H                                                      
*                                                                               
         LA    R9,RTOT                                                          
         CLC   =C'RT',8(R2)                                                     
         BE    RCLEDT                                                           
         LA    R9,ACTV                                                          
         CLC   =C'RA',8(R2)                                                     
         BE    RCLEDT                                                           
         LA    R9,WKS                                                           
         CLC   =C'RW',8(R2)                                                     
         BE    RCLEDT                                                           
         CLI   SVEDAILY,C'Y'                                                    
         BNE   *+14                                                             
         CLC   =C'RD',8(R2)                                                     
         BE    RCLEDT                                                           
         LA    R9,TOT                                                           
         CLC   =C'RI',8(R2)        RECALL ID                                    
         BE    RCLEDT                                                           
         CLC   =C'RP',8(R2)        RECALL PURPOSE CODE                          
         BE    RCLEDT                                                           
         LA    R9,RCL                                                           
         CLC   =C'RM',8(R2)                                                     
         BE    RCLEDT                                                           
         CLI   8(R2),C'R'                                                       
         BE    RCLEDT                                                           
         LA    R9,TOT                                                           
         CLI   SVPRD,X'FF'         NO 'TOT' FOR CPP DATA                        
         BE    *+12                                                             
         CLI   8(R2),C'T'                                                       
         BE    RCLEDT                                                           
         CLI   8(R2),C'N'          TEST 'NEXT' REQUEST                          
         BE    RCLNEXT                                                          
         MVI   ERRCD,INVERR                                                     
         J     GLERR                                                            
         EJECT                                                                  
RCLNEXT  MVI   ERRCD,SEQERR                                                     
         LA    R2,ELEM             LAST ACTION SAVED HERE BY BASE               
         LA    R9,RCL                                                           
         CLC   =C'RM',8(R2)        MUST BE RM OR T                              
         BE    RCLNEXT2                                                         
         LA    R9,TOT                                                           
         CLC   =C'RI',8(R2)        RECALL ID                                    
         BE    RCLNEXT2                                                         
         CLC   =C'RP',8(R2)        RECALL PURPOSE CODE                          
         BE    RCLNEXT2                                                         
         CLI   8(R2),C'T'                                                       
         BE    RCLNEXT2                                                         
         LA    R2,GOLACT1H                                                      
         J     GLERR                                                            
*                                                                               
RCLNEXT2 MVI   ERRCD,NOMORERR                                                   
         OC    SVKEY,SVKEY         WERE WE FINISHED                             
         JZ    GLERR               YES - ERROR                                  
*                                                                               
         MVC   GOLACT0H(GOLACT1H-GOLACT0H),ELEM   RESTORE SAVED DATA            
         LA    R2,GOLPER0H                                                      
         MVI   5(R2),13            SET LENGTH TO MAX                            
         CLC   8(13,R2),SPACES     IS ANYTHING THERE                            
         BNE   *+8                 YES                                          
         MVI   5(R2),0             RESET LENGTH                                 
         GOTO1 USER3               'EDTPER'                                     
*                                                                               
         B     RCLEDT1                                                          
         EJECT                                                                  
RCLEDT   DS    0H                                                               
         XC    SVKEY,SVKEY         NOT 'NEXT' - CLEAR THIS                      
         FOUT  GOLACT0H,GOLACT1,3                                               
*                                                                               
* CLEAR THE SCREEN                                                              
*                                                                               
RCLEDT1  LA    R2,GOLMKT1H                                                      
         FOUT  GOLNAM1H,SPACES,16                                               
*                                                                               
RCLEDT1A XC    GOLDOL1,GOLDOL1                                                  
         FOUT  GOLDOL1H                                                         
*                                                                               
         XC    GOLPTS1,GOLPTS1                                                  
         FOUT  GOLPTS1H                                                         
* CLEAR LINES 2-14                                                              
         LA    R4,GOLPER1H                                                      
         SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         AR    R4,R5                                                            
*                                                                               
RCLEDT1B IC    R5,0(R4)                                                         
         SH    R5,=H'9'                                                         
         EX    R5,RCLEDTOC                                                      
         BZ    RCLEDT1C                                                         
         EX    R5,RCLEDTXC                                                      
         FOUT  (R4)                                                             
*                                                                               
RCLEDT1C LA    R4,9(R5,R4)                                                      
         CLI   0(R4),0                                                          
         BNE   RCLEDT1B                                                         
*                                                                               
         CLI   GOLACT1,C'N'        TEST NEXT                                    
         BE    RCLEDT12                                                         
*                                                                               
         B     RCLEDT1X                                                         
*                                                                               
RCLEDTOC OC    8(0,R4),8(R4)                                                    
RCLEDTXC XC    8(0,R4),8(R4)                                                    
         EJECT                                                                  
RCLEDT1X DS    0H                                                               
         MVI   ERRCD,MSSNGERR                                                   
         LA    R2,GOLMKT1H                                                      
         CLI   5(R2),0                                                          
         BNZ   RCLEDT2                                                          
         GOTO1 ERROR                                                            
*                                                                               
RCLEDT2  NI    4(R2),X'FF'-X'20'                                                
         GOTO1 USER1               'EDTMKT'                                     
*                                                                               
         FOUT  GOLNAM1H,SVMKTNAM,16                                             
*                                                                               
*                                                                               
***      CLI   GDDDLINK,C'Y'       TEST RECORD WAS UPLOADED                     
***      JNE   *+8                                                              
***      MVI   GOLNAM1+15,C'!'                                                  
*                                                                               
RCLEDT3  LA    R2,GOLDPT1H                                                      
         CLI   5(R2),0                                                          
         BE    RCLEDT4                                                          
         GOTO1 USER2               'EDTDPTLN'                                   
*                                                                               
RCLEDT4  CLC   =C'RM',GOLACT0                                                   
         BE    RCLEDT5                                                          
         CLC   =C'RA',GOLACT0                                                   
         BE    RCLEDT6                                                          
         CLC   =C'RI',GOLACT0                                                   
         BE    RCLEDT5                                                          
         CLC   =C'RP',GOLACT0                                                   
         BE    RCLEDT5                                                          
         CLI   GOLACT0,C'T'                                                     
         BE    RCLEDT5                                                          
* SINGLE LINE RECALL MUST SPECIFY DPT/LEN                                       
         MVI   ERRCD,MSSNGERR                                                   
         CLI   BDPT,0                                                           
         JE    GLERR                                                            
         CLI   SVPRD,X'FF'         TEST CPP                                     
         BE    RCLEDT4A                                                         
* NOT CPP                                                                       
         CLI   BSLN,0                                                           
         JE    GLERR                                                            
         B     RCLEDT5                                                          
* CPP                                                                           
RCLEDT4A MVI   ERRCD,INVERR                                                     
         CLI   BSLN,30             ** THIS IS FOR COMPATIBILITY **              
         BE    RCLEDT5                                                          
         CLI   BSLN,0                                                           
         JNE   GLERR                                                            
         MVI   BSLN,30                                                          
         EJECT                                                                  
RCLEDT5  LA    R2,GOLPER1H                                                      
         GOTO1 USER3                                                            
*                                                                               
RCLEDT6  DS    0H                                                               
*                                                                               
         OC    GOLMKT1,SPACES                                                   
         FOUT  GOLMKT0H,GOLMKT1,4                                               
*                                                                               
         OC    GOLDPT1,SPACES                                                   
         FOUT  GOLDPT0H,GOLDPT1,4                                               
*                                                                               
         CLC   =C'RA',GOLACT0                                                   
         BE    RCLEDT12                                                         
*                                                                               
         OC    GOLPER1,SPACES                                                   
         FOUT  GOLPER0H,GOLPER1,13                                              
         EJECT                                                                  
RCLEDT12 DS    0H                                                               
         MVI   FRSTSW,0            RESET FIRST TIME SWITCH                      
         MVC   KEY,SVKEY           MOVE KEY FOR 'NEXT'                          
         OC    KEY(13),KEY         IS IT THERE                                  
         BNZ   RCLHI               YES                                          
*                                                                               
* BUILD GOAL KEY                                                                
*                                                                               
         XC    GKEY,GKEY                                                        
         MVI   GKEYTYPE,2                                                       
         MVC   GKEYAM,SVAGYMD                                                   
         MVC   GKEYCLT,SVCLT                                                    
         MVC   GKEYPRD,SVPRD                                                    
         MVC   GKEYMKT,BMKT                                                     
         MVC   GKEYEST,SVEST                                                    
         MVC   GKEYDPT,BDPT                                                     
         CLI   BDPT,C'*'                                                        
         BNE   *+8                                                              
         MVI   GKEYDPT,0                                                        
         MVC   GKEYSLN,BSLN                                                     
         MVC   GKEYSEC,BTLN                                                     
         MVC   GKEYPRD2,SVPRD2                                                  
         CLC   =C'RI',GOLACT0      ACTION RECALL ID                             
         BE    RCLBGK                                                           
         CLC   =C'RP',GOLACT0       OR PURPOSE CODE                             
         BE    RCLBGK                                                           
         OC    SVIDR,SVIDR         ID WAS ENTERED?                              
         BZ    RCLBGK                                                           
         OI    GKEYAGY,X'40'       NOT A PRODUCT IN GKEYPRD2                    
*                                                                               
RCLBGK   MVC   KEY,GKEY                                                         
         MVI   IDCODE,0            INITIALIZE NUMBER OF LINES FOR RI            
*                                                                               
RCLHI    GOTO1 HIGH                                                             
*                                                                               
RCLHI2   LA    RE,8                SET FOR A/M/CLT/PRD/MKT/EST COMPARE          
         CLC   GOLACT0(2),=C'RA'                                                
         BE    RCLHI6                                                           
         CLC   GOLACT0(2),=C'RM'                                                
         BE    RCLHI6                                                           
         CLC   =C'RI',GOLACT0                                                   
         BE    RCLHI6                                                           
         CLC   =C'RP',GOLACT0                                                   
         BE    RCLHI6                                                           
         CLI   GOLACT0,C'T'                                                     
         BE    RCLHI6                                                           
         LA    RE,10               ADD DPT/LEN                                  
*                                                                               
RCLHI6   MVI   ERRCD,NODTAERR                                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE  ** EXECUTED **                                   
         BNE   RCLHI10                                                          
         TM    KEY+11,X'80'        TEST PASSSIVE                                
         BO    RCLHI8              YES - IGNORE                                 
         CLC   =C'RI',GOLACT0                                                   
         BE    RCLGET                                                           
         CLC   =C'RP',GOLACT0                                                   
         BE    RCLGET                                                           
         OC    SVIDR,SVIDR                                                      
         BZ    RCLHI7                                                           
         TM    KEY+11,X'40'        TEST GKEYPRD2 IS NOT A PRODUCT               
         BO    RCLGET              YES                                          
         B     RCLHI8              DOESN'T HAVE AN ID SO DON'T SHOW             
RCLHI7   CLC   KEY+12(1),SVPRD2    RIGHT PARTNER                                
         BE    RCLGET              YES - CONTINUE                               
*                                                                               
RCLHI8   GOTO1 SEQ                                                              
         B     RCLHI2                                                           
*                                                                               
RCLHI10  XC    SVKEY,SVKEY         DISABLE 'NEXT'                               
         LA    R2,GOLACT1H                                                      
         J     GLERR                                                            
*                                                                               
RCLSEQ   MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(8),KEYSAVE      A/M/CLT/PRD/MKT/EST                          
         BNE   RCLSEQ2                                                          
         TM    KEY+11,X'80'        TEST PASSSIVE                                
         BO    RCLSEQ              YES - IGNORE                                 
         CLC   =C'RI',GOLACT0                                                   
         BE    RCLGET                                                           
         CLC   =C'RP',GOLACT0                                                   
         BE    RCLGET                                                           
         OC    SVIDR,SVIDR                                                      
         BZ    RCLSEQ1                                                          
         TM    KEY+11,X'40'        TEST GKEYPRD2 IS NOT A PRODUCT               
         BO    RCLGET              YES                                          
         B     RCLSEQ              DOESN'T HAVE AN ID SO DON'T SHOW             
RCLSEQ1  CLC   KEY+12(1),SVPRD2    RIGHT PARTNER                                
         BNE   RCLSEQ                                                           
         B     RCLGET                                                           
*                                                                               
RCLSEQ2  CLI   0(R2),0             TEST POINTING TO E-O-S                       
         BE    *+10                YES - LEAVE SET FOR NEXT                     
         XC    SVKEY,SVKEY         INDICATE NO MORE FOR 'NEXT'                  
         CLC   =C'RI',GOLACT0                                                   
         BE    TOTLAST                                                          
         CLC   =C'RP',GOLACT0                                                   
         BE    TOTLAST                                                          
         CLI   GOLACT0,C'T'        TEST 'TOT'                                   
         BE    TOTLAST                                                          
EXXMOD   XIT1                                                                   
*                                                                               
         EJECT                                                                  
RCLGET   LA    RE,GOALREC                                                       
         ST    RE,AREC                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLC   =C'RM',GOLACT0                                                   
         BNE   RCLGET2                                                          
         CLI   BDPT,C'*'           TEST SLN FILTERING                           
         BNE   RCLGET2             NO                                           
         CLI   BSLN,0              TEST HAVE FILTER                             
         BE    RCLGET2                                                          
         CLC   GKEYSLN,BSLN                                                     
         BNE   RCLSEQ                                                           
*                                                                               
RCLGET2  CLC   =C'RI',GOLACT0      DOESN'T HAVE TO BE SAME ID FOR RI            
         BE    RCLGET10                                                         
         CLC   =C'RP',GOLACT0        OR RP                                      
         BE    RCLGET10                                                         
         CLC   GDIDR,SVIDR         COMPARE SAME ID                              
         BNE   RCLSEQ                                                           
         B     RCLGET16                                                         
*                                                                               
RCLGET10 ZIC   R1,IDCODE           INCREMENT NUMBER OF ID CODES FOUND           
         LA    R1,1(R1)                                                         
         STC   R1,IDCODE                                                        
         CLI   IDCODE,12           NO MORE THAN 10 ON THE SCREEN                
         BE    TOTLAST                                                          
         SPACE 1                                                                
*=========================================================                      
*CHECK FIRST GOAL ELEM A MONDAY OR ESTART                                       
*=========================================================                      
         SPACE 1                                                                
RCLGET16 LA    R6,GDELEM                                                        
*                                                                               
RCLGET18 ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    RCLGETX                                                          
         CLI   0(R6),X'21'                                                      
         BE    RCLGET19                                                         
*                                                                               
         CLI   SVOPT1,C'G'         TEST FOR GOAL LOCKIN                         
         BNE   RCLGET18                                                         
         CLI   0(R6),X'A1'                                                      
         BNE   RCLGET18                                                         
*                                                                               
RCLGET19 GOTO1 VDATCON,DMCB,(2,2(R6)),WORK  GET 6 BYTE YYMMDD                   
*                                                                               
         CLC   SVSTART,WORK        TEST EST START DATE                          
         BE    RCLGETX             YES                                          
*                                                                               
         CLI   SVEOWSDY,0          TEST OUT-OF-WEEK DATA                        
         BE    RCLGET20                                                         
         GOTO1 USER4               YES --FIX ELEMENT DATES IN RECORD            
         B     RCLGETX                                                          
*                                                                               
RCLGET20 GOTO1 VGETDAY,DMCB,WORK,WORK+6                                         
*                                                                               
         CLI   0(R1),1             TEST MONDAY                                  
         BE    RCLGETX                                                          
         CLI   SVEDAILY,C'Y'                                                    
         BE    RCLGETX                                                          
* BACK UP TO PREVIOUS MONDAY OR ESTART                                          
         ZIC   R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
*                                                                               
         CLC   SVSTART,WORK+6                                                   
         BL    *+10                                                             
         MVC   WORK+6(6),SVSTART                                                
*                                                                               
         GOTO1 VDATCON,DMCB,WORK+6,(2,2(R6))                                    
*                                                                               
RCLGETX  DS    0H                                                               
*                                                                               
         MVC   SVKEY,KEY           SAVE FOR 'NEXT'                              
         LA    R8,BWEEKS           POINT TO WEEK LIST                           
         BR    R9                  GO TO FORMATTING ROUTINE                     
         EJECT                                                                  
RCL      CLI   FRSTSW,0            TEST FIRST LINE                              
         BNE   RCL2                                                             
         MVI   FRSTSW,1                                                         
         XC    KEYSAVE,KEYSAVE     CLEAR FOR FORMAT LOGIC                       
         LA    R2,GOLACT1H                                                      
*                                                                               
         CLI   GOLACT1,C'N'        TEST 'NEXT'                                  
         BNE   *+8                                                              
         AH    R8,SVWKDSP          POINT TO START WEEK FOR 'NEXT'               
*                                                                               
         BAS   RE,FMTBUD                                                        
*                                                                               
RCL2     LA    R6,GDELEM                                                        
         MVI   ELCODE,X'21'                                                     
         CLI   SVOPT1,C'G'         TEST FOR GOAL LOCKIN                         
         BNE   *+8                                                              
         MVI   ELCODE,X'A1'                                                     
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   RCL14                                                            
*                                                                               
RCL6     ZAP   HALF,=P'0'          RESET WEEK COUNTER                           
         BAS   RE,FMT                                                           
*                                                                               
RCL8     AP    HALF,=P'1'                                                       
         LA    RE,2(R8)                                                         
         MVC   HALF2,0(RE)         SAVE NEXT BWEEK ENTRY                        
         LR    R3,R6               SAVE ELEMENT ADDRESS                         
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   RCL10                                                            
         CLC   0(2,R8),HALF2       IS THIS THE NEXT WEEK                        
         BNE   RCL10               NO-FINISH LAST DISPLAY                       
         CLI   SVEDAILY,C'Y'                                                    
         BE    RCL10                                                            
         CLC   4(8,R6),4(R3)       SAME POINTS AND DOLLARS                      
         BNE   RCL10               NO                                           
         B     RCL8                                                             
*                                                                               
* FINISH PREVIOUS DISPLAY LINE                                                  
*                                                                               
RCL10    CP    HALF,=P'0'          TEST NO DATA THIS LINE                       
         BE    RCL12               GO CHECK E-O-R                               
*                                                                               
         CLI   SVEDAILY,C'Y'                                                    
         BE    RCL11                                                            
*                                                                               
         EDIT  (P2,HALF),(3,WORK2),ALIGN=LEFT                                   
         MVI   13(R2),C'-'                                                      
         MVC   14(3,R2),WORK2                                                   
         LA    RE,14(R2)                                                        
         AR    RE,R0                                                            
         MVI   0(RE),C'W'                                                       
* POINT TO NEXT DISPLAY LINE                                                    
RCL11    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         SPACE 2                                                                
RCL12    CLI   0(R2),0             TEST END OF SCREEN                           
         BE    RCLX                                                             
         CLI   0(R6),0             TEST END OF REC                              
         BNE   RCL6                                                             
RCL14    CLC   =C'RM',GOLACT0                                                   
         BE    RCLSEQ                                                           
         XC    SVKEY,SVKEY         CLEAR 'NEXT'                                 
         B     EXXMOD                                                           
*                                                                               
RCLX     LA    R0,BWEEKS           SAVE BWEEK DISPLACEMENT                      
         SR    R8,R0                                                            
         STH   R8,SVWKDSP                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
TOT      CLI   FRSTSW,0            TEST FIRST TIME                              
         BNE   TOT1                                                             
         MVI   FRSTSW,1                                                         
         XC    KEYSAVE,KEYSAVE     CLEAR FOR FORMAT LOGIC                       
         LA    R2,GOLACT1H                                                      
         XC    ELEM(8),ELEM        CLEAR MARKET TOTAL AREA                      
*                                                                               
         BAS   RE,FMTBUD                                                        
*                                                                               
TOT1     LA    R6,GDELEM                                                        
         MVI   ELCODE,X'21'                                                     
         CLI   SVOPT1,C'G'         TEST LOCKED GOALS                            
         BNE   *+8                                                              
         MVI   ELCODE,X'A1'                                                     
         XC    WORK,WORK           CLEAR TOTAL AREA                             
*                                                                               
TOT2     BRAS  RE,NEXTEL                                                        
         BNE   TOT4                                                             
         ST    R6,WORK+12          SAVE LAST ELEM ADDRESS                       
         OC    WORK+8(4),WORK+8                                                 
         BNZ   *+8                                                              
         ST    R6,WORK+8           SAVE FIRST ELEM ADDRESS                      
* GET POINTS                                                                    
         L     R0,4(R6)                                                         
         N     R0,=X'00FFFFFF'                                                  
*                                                                               
         CLI   SVDEMOS+3,C'R'      PRIMARY DEMO IS RATINGS?                     
         JE    *+12                                                             
         CLI   SVDEMOS+3,C'E'                                                   
         JNE   TOT2A                                                            
         CLI   TWODEC,C'Y'         TEST 2-DEC GOAL RTGS                         
         JNE   TOT3A               NO                                           
         J     TOT2T                                                            
*                                                                               
TOT2A    CLI   TWODCIMPS,C'Y'      IF NOT RATINGS, THEN IMPS                    
         JNE   TOT3A                                                            
*                                                                               
TOT2T    TM    4(R6),GLGRP2DEC     TEST PTS ARE 2-DEC                           
         JO    *+8                                                              
         MHI   R0,10               SCALE TO 2-DEC                               
         J     TOT3B                                                            
*                                                                               
TOT3A    TM    4(R6),GLGRP2DEC     2-DEC NOT ACTIVE - CHECK ELEM                
         JZ    TOT3B                                                            
*                                                                               
         LR    RF,R0               SCALE GOAL VALUE TO 1-DEC                    
         SR    RE,RE                                                            
         A     RF,=F'5'                                                         
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
*                                                                               
TOT3B    A     R0,WORK                                                          
         ST    R0,WORK                                                          
* GET DOLLARS                                                                   
         L     R0,8(R6)                                                         
         A     R0,WORK+4                                                        
         ST    R0,WORK+4                                                        
         B     TOT2                                                             
*                                                                               
TOT4     L     R6,WORK+8           GET FIRST ELEM ADDRESS                       
         LTR   R6,R6               TEST ACTIVE                                  
         BZ    TOT10                                                            
         MVC   4(8,R6),WORK        MOVE TOTALS TO FIRST ELEMENT                 
*                                                                               
         CLI   SVDEMOS+3,C'R'      PRIMARY DEMO IS RATINGS?                     
         JE    *+12                                                             
         CLI   SVDEMOS+3,C'E'                                                   
         JNE   TOT4A                                                            
         CLI   TWODEC,C'Y'         TEST 2-DEC GOAL RTGS                         
         JNE   TOT5                                                             
         J     TOT4T                                                            
*                                                                               
TOT4A    CLI   TWODCIMPS,C'Y'      IF NOT RATINGS, THEN IMPS                    
         JNE   TOT5                                                             
TOT4T    OI    4(R6),GLGRP2DEC     SET 2-DEC FLAG                               
* ADD TO MARKET TOTALS                                                          
TOT5     LM    RE,RF,WORK                                                       
         A     RE,ELEM                                                          
         A     RF,ELEM+4                                                        
         STM   RE,RF,ELEM                                                       
*                                                                               
         MVC   FULL,WORK+12        SAVE LAST ELEMENT ADDRESS                    
         CLI   0(R2),0             IF SCREEN IS FULL                            
         BE    TOT10               DO NOT FORMAT - BUT KEEP GOING               
         BAS   RE,FMT                                                           
* FORMAT END DATE                                                               
         L     R6,FULL             GET LAST ELEM ADDRESS                        
         GOTO1 VDATCON,DMCB,(2,2(R6)),(4,WORK2)                                 
         MVI   13(R2),C'-'                                                      
         MVC   14(5,R2),WORK2                                                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
TOT10    B     RCLSEQ              DO NOT TEST FOR E-O-S HERE                   
         EJECT                                                                  
* DO MARKET TOTALS                                                              
*                                                                               
TOTLAST  LA    RE,2                ONLY NEED TO MOVE 9 PLACES IF RI             
         CLC   =C'RI',GOLACT0                                                   
         BE    TOTL2                                                            
         CLC   =C'RP',GOLACT0                                                   
         BE    TOTL2                                                            
         LA    R2,GOLNAM1H         POSITION TO NEXT MARKET NAME FIELD           
         LA    RE,7                                                             
*                                                                               
TOTL2    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
*                                                                               
         L     R0,ELEM+4           DOLLARS                                      
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(10,WORK2)                                                  
*                                                                               
         MVC   8(6,R2),=C'MKDOL='                                               
         MVC   14(10,R2),WORK2                                                  
         FOUT  (R2)                                                             
*                                                                               
         LA    RE,7                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
*                                                                               
         L     R0,ELEM             POINTS                                       
         EDIT  (R0),(10,WORK2),1                                                
*                                                                               
         CLI   SVDEMOS+3,C'R'      PRIMARY DEMO IS RATINGS?                     
         JE    *+12                                                             
         CLI   SVDEMOS+3,C'E'                                                   
         JNE   TOTL2A                                                           
         CLI   TWODEC,C'Y'         TEST 2-DEC GOAL RTGS                         
         JNE   TOTL4                                                            
         J     TOTL3                                                            
*                                                                               
TOTL2A   CLI   TWODCIMPS,C'Y'      IF NOT RATINGS, THEN IMPS                    
         JNE   TOTL4                                                            
TOTL3    EDIT  (R0),(10,WORK2),2                                                
*                                                                               
TOTL4    MVC   8(6,R2),=C'MKPTS='                                               
         MVC   14(10,R2),WORK2                                                  
         FOUT  (R2)                                                             
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
WKS      MVI   ERRCD,MSSNGERR                                                   
         LA    R2,GOLDPT1H                                                      
         CLI   BDPT,0                                                           
         JE    GLERR                                                            
         CLI   BSLN,0                                                           
         JE    GLERR                                                            
         LA    R2,GOLACT1H                                                      
*                                                                               
         BAS   RE,FMTBUD                                                        
*                                                                               
         XC    KEYSAVE,KEYSAVE     CLEAR FOR FORMAT LOGIC                       
         LA    R6,GDELEM                                                        
         MVI   ELCODE,X'21'                                                     
         CLI   SVOPT1,C'G'          TEST GOAL LOCKIN                            
         BNE   *+8                                                              
         MVI   ELCODE,X'A1'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    WKS4                                                             
         B     WKSX                                                             
*                                                                               
WKS2     BRAS  RE,NEXTEL                                                        
         BNE   EXXMOD                                                           
*                                                                               
WKS4     BAS   RE,FMT                                                           
         CLI   SVEDAILY,C'Y'                                                    
         BE    *+10                                                             
         MVC   13(3,R2),=C'-1W'                                                 
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   WKS2                                                             
WKSX     XC    SVKEY,SVKEY                                                      
         B     EXXMOD              NO MORE ROOM                                 
         EJECT                                                                  
RTOT     MVI   ERRCD,MSSNGERR                                                   
         LA    R2,GOLDPT1H                                                      
         CLI   BDPT,0                                                           
         JE    GLERR                                                            
         CLI   BSLN,0                                                           
         JE    GLERR                                                            
         LA    R2,GOLACT1H                                                      
*                                                                               
         XC    KEYSAVE,KEYSAVE     CLEAR FOR FORMAT LOGIC                       
         LA    R6,GDELEM                                                        
         MVI   ELCODE,X'42'        SEARCH FOR TOTAL ELEMENTS                    
         BRAS  RE,NEXTEL                                                        
         BE    RTOT4                                                            
         B     RTOTX                                                            
*                                                                               
RTOT2    BRAS  RE,NEXTEL                                                        
         BNE   EXXMOD                                                           
*                                                                               
RTOT4    BAS   RE,FMT                                                           
* FORMAT END WEEK DATE                                                          
         MVI   13(R2),C'-'                                                      
         GOTO1 VDATCON,DMCB,(2,4(R6)),(4,WORK2)                                 
         MVC   14(5,R2),WORK2                                                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   RTOT2                                                            
RTOTX    XC    SVKEY,SVKEY                                                      
         B     EXXMOD              NO MORE ROOM                                 
         EJECT                                                                  
* ACTIVITY DATE DISPLAY                                                         
*                                                                               
ACTV     DS    0H                                                               
         CLI   FRSTSW,0            TEST FIRST LINE                              
         BNE   ACTV2                                                            
         MVI   FRSTSW,1                                                         
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R2,GOLACT1H                                                      
*                                                                               
ACTV2    BAS   RE,FMT                                                           
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO DOLLARS                             
         XC    8(11,R2),8(R2)                                                   
         FOUT  (R2)                                                             
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO POINTS                              
         XC    8(11,R2),8(R2)                                                   
         FOUT  (R2)                                                             
*                                                                               
ACTV4    DS    0H                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO DATE                                
         MVC   8(5,R2),=C'ACTV='                                                
         GOTO1 VDATCON,DMCB,(2,GACTDATE),(5,13(R2))                             
         FOUT  (R2)                                                             
*                                                                               
         IC    R0,0(R2)            POINT TO NEXT LINE                           
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   RCLSEQ                                                           
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
* FORMAT BUDGET ELEM IF PRESENT                                                 
*                                                                               
FMTBUD   NTR1                                                                   
*                                                                               
         MVI   ELCODE,X'40'                                                     
         LA    R6,GDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   FMTX                                                             
         OC    8(4,R6),8(R6)       TEST ZERO DOLLARS                            
         JZ    FMTX                                                             
*                                                                               
* DISPLAY ONLY IF REQ START IS EST START                                        
         GOTO1 VDATCON,DMCB,SVSTART,(2,WORK)                                    
         CLC   WORK(2),0(R8)                                                    
         JNE   FMTX                                                             
*                                                                               
         BAS   RE,FMT                                                           
*                                                                               
         MVI   8(R2),C'B'                                                       
         MVC   9(8,R2),WORK2       INSERT 'B' BEFORE DOLLARS                    
         OC    8(11,R2),SPACES                                                  
         OI    4(R2),X'20'         SET VALID BIT                                
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0               SKIP POINTS                                  
         OI    4(R2),X'20'         SET VALID BIT                                
         IC    R0,0(R2)                                                         
         AR    R2,R0               AND PERIOD                                   
         OI    4(R2),X'20'         SET VALID BIT                                
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT DISPLAY LINE                   
         J     FMTX                EXIT WITH NEW R2                             
         LTORG                                                                  
         EJECT                                                                  
FMT      NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         ST    R2,BLNADDR          SAVE LINE START ADDRESS                      
         MVC   8(3,R2),=C'*  '     ACTION                                       
         TM    GDSTAT,X'80'                                                     
         BZ    *+8                                                              
         MVI   9(R2),C'-'                                                       
         FOUT  (R2)                                                             
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               SKIP MARKET                                  
         OI    4(R2),X'20'         SET FLAG                                     
         IC    R0,0(R2)                                                         
         AR    R2,R0               SKIP MARKET NAME                             
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLC   =C'RI',GOLACT0      IF RI PUT ID AT END OF MARKET NAME           
         BE    FMT2                                                             
         CLC   =C'RP',GOLACT0                                                   
         BNE   FMT10                                                            
*                                                                               
FMT2     SR    R2,R0               BACK UP                                      
         MVC   18(L'GDIDR,R2),=C'*NONE*'                                        
         OC    GDIDR,GDIDR         CHECK IF THERE IS AN ID                      
         BZ    *+10                                                             
         MVC   18(L'GDIDR,R2),GDIDR                                             
         OI    6(R2),X'80'                                                      
         AR    R2,R0                                                            
         B     FMT12                                                            
*                                                                               
FMT10    CLC   =C'TEST',GOLPLNR                                                 
         BNE   FMT12                                                            
*                                                                               
* FORMAT DISK ADDRESS AT END OF MARKET NAME                                     
*                                                                               
         SR    R2,R0               BACK UP                                      
         MVI   19(R2),C' '                                                      
         GOTO1 =V(HEXOUT),DMCB,KEY+14,16(R2),4,=C'TOG',RR=RB                    
*                                                                               
         AR    R2,R0                                                            
*                                                                               
FMT12    DS    0H                                                               
*                                                                               
         LA    R4,GDLKGOAL                                                      
         CLI   SVOPT1,C'G'         TEST LOCKIN GOAL DISPLAY                     
         BE    FMT14                                                            
         LA    R4,GDLKBUY                                                       
         CLI   SVOPT1,C'L'         TEST LOCKIN BUY DISPLAY                      
         BNE   FMT16                                                            
                                                                                
* DISPLAY LAST LOCKIN DATE AT END OF MARKET NAME                                
                                                                                
FMT14    SR    R2,R0                                                            
         MVI   19(R2),C' '                                                      
         GOTO1 VDATCON,DMCB,(2,(R4)),(5,16(R2))                                 
         AR    R2,R0                                                            
*                                                                               
FMT16    CLC   KEY(13),KEYSAVE     TEST SAME DPT/LN                             
         BE    FMT20                                                            
         MVC   KEYSAVE,KEY         FORCE EQUAL NEXT TIME                        
         MVC   8(1,R2),GKEYDPT                                                  
         MVC   9(5,R2),SPACES                                                   
         CLI   SVPRD,X'FF'         TEST CPP                                     
         BE    FMT18                                                            
         CLI   GKEYDPT,C'$'        NO SLN DISPLAY FOR THIS DUDE                 
         BNE   *+12                                                             
         CLI   GKEYSLN,1           TEST DUMMY LENGTH                            
         BE    FMT18                                                            
         SR    R0,R0                                                            
         IC    R0,GKEYSLN                                                       
         LA    R4,WORK2                                                         
         EDIT  (R0),(5,WORK2),ALIGN=LEFT                                        
         MVC   9(5,R2),WORK2                                                    
         CLC   GKEYSLN,GKEYSEC     TEST PIGGYBACK                               
         BE    FMT18                                                            
* FORMAT PARTNER LENGTH                                                         
         AR    R4,R0                                                            
         MVI   0(R4),C'-'                                                       
         ZIC   R0,GKEYSEC          GET TOTAL LENGTH                             
         ZIC   RE,GKEYSLN          LESS LEN OF ACTIVE PRD                       
         SR    R0,RE                                                            
         EDIT  (R0),(3,1(R4)),ALIGN=LEFT                                        
         MVC   9(5,R2),WORK2                                                    
FMT18    FOUT  (R2)                                                             
         EJECT                                                                  
FMT20    OI    4(R2),X'20'         SET FLAG                                     
         CLC   =C'RA',GOLACT0                                                   
         BE    FMTX                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   SVPRD,X'FF'                                                      
         BE    FMT50                                                            
*                                                                               
         L     R0,8(R6)            DOLLARS                                      
         LPR   R0,R0               MAKE SURE POSITIVE                           
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LTR   R0,R0               TEST REMAINDER 0                             
         BNZ   FMT22                                                            
         LR    R0,R1                                                            
         EDIT  (R0),(10,WORK2),ALIGN=LEFT,ZERO=NOBLANK                          
         B     FMT24                                                            
*                                                                               
FMT22    L     R0,8(R6)                                                         
         LPR   R0,R0               MAKE SURE POSITIVE                           
         EDIT  (R0),(10,WORK2),2,ALIGN=LEFT,ZERO=NOBLANK                        
*                                                                               
FMT24    CLI   0(R6),X'40'         TEST BUDGET ELEM                             
         BE    FMTX                                                             
*                                                                               
         LA    R4,8(R2)                                                         
         CLC   =C'RI',GOLACT0                                                   
         BE    FMT26                                                            
         CLC   =C'RP',GOLACT0                                                   
         BE    FMT26                                                            
         CLI   GOLACT0,C'T'                                                     
         BNE   FMT27                                                            
*                                                                               
FMT26    MVC   0(4,R4),=C'TOT='                                                 
         LA    R4,4(R4)                                                         
*                                                                               
FMT27    MVC   0(7,R4),WORK2                                                    
         OC    8(11,R2),SPACES                                                  
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET FLAG                                     
         SPACE 2                                                                
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   0(R6),X'42'         TEST TOTAL ELEMENT                           
         BE    FMT46                                                            
         CLI   GKEY+8,C'+'         TEST SPECIAL CCUSA DPT                       
         BE    FMT46                                                            
         CLI   GKEY+8,C'-'                                                      
         BE    FMT46                                                            
*                                                                               
         L     R0,4(R6)            POINTS                                       
         N     R0,=X'00FFFFFF'     DROP HOB                                     
         SRDL  R0,32                                                            
         LA    RE,10                                                            
         TM    4(R6),GLGRP2DEC     TEST 2-DECIMALS IN ELEM                      
         JZ    *+8                                                              
         LA    RE,100                                                           
         DR    R0,RE                                                            
*                                                                               
         LTR   R0,R0               TEST REMAINDER 0, WHOLE NUMBER?              
         BNZ   FMT28               NO - FORMAT WITH DECIMAL(S)                  
         LR    R0,R1                                                            
         EDIT  (R0),(10,WORK2),ALIGN=LEFT,ZERO=NOBLANK                          
         B     FMT40                                                            
*                                                                               
FMT28    L     R0,4(R6)            WE DON'T HAVE A WHOLE NUMBER                 
         N     R0,=X'00FFFFFF'     SO RE-ESTABLISH THE VALUE                    
*                                                                               
         TM    4(R6),GLGRP2DEC     WE HAVE A 2 DECIMAL VALUE?                   
         JNZ   FMT33               YES                                          
*                                  NO, THIS IS A 1 DEC VALUE                    
* IF AGENCY IS SET TO HAVE 2 DECIMAL RATINGS OR IMPRESSIONS                     
         CLI   SVDEMOS+3,C'R'      IF RATINGS OR EXTENDED RATINGS               
         JE    FMT30                                                            
         CLI   SVDEMOS+3,C'E'                                                   
         JNE   FMT30A                                                           
FMT30    CLI   TWODEC,C'Y'         AND 00 PROFILE SAYS 2 DEC RTGS               
         JNE   FMT30DC1                                                         
FMT30DC2 MHI   R0,10               MAKE THE 1 DEC INTO 2 DEC                    
         EDIT  (R0),(10,WORK2),2,ALIGN=LEFT                                     
         J     FMT40                                                            
*                                                                               
FMT30A   CLI   TWODCIMPS,C'Y'      AND 00A PROFILE SAYS 2 DEC IMPS              
         JE    FMT30DC2                                                         
FMT30DC1 EDIT (R0),(10,WORK2),1,ALIGN=LEFT                                      
         J     FMT40                                                            
*                                                                               
FMT33    DS    0H                  WE HAVE A 2 DEC VALUE                        
         CLI   SVDEMOS+3,C'R'      IF RATINGS OR EXTENDED RATINGS               
         JE    FMT36                                                            
         CLI   SVDEMOS+3,C'E'                                                   
         JNE   FMT36A                                                           
FMT36    CLI   TWODEC,C'Y'         AND 00 PROFILE SAYS 2 DEC RTGS               
         JNE   FMT36DC1                                                         
FMT36DC2 EDIT (R0),(10,WORK2),2,ALIGN=LEFT                                      
         J     FMT40                                                            
*                                                                               
FMT36A   CLI   TWODCIMPS,C'Y'      AND 00A PROFILE SAYS 2 DEC IMPS              
         JE    FMT36DC2                                                         
FMT36DC1 SRDL  R0,32                     THEN DIVIDE BY 10                      
         LA    RE,10                                                            
         DR    R0,RE                                                            
         IF  (CHI,R0,NL,5)         WE HAVE TO ROUND UP?                         
            LA  R1,1(R1)           YES                                          
         ENDIF                                                                  
         LR    R0,R1                                                            
         EDIT  (R0),(10,WORK2),1,ALIGN=LEFT,ZERO=NOBLANK                        
*                                                                               
FMT40    LA    R4,8(R2)                                                         
         CLC   =C'RI',GOLACT0                                                   
         BE    FMT42                                                            
         CLC   =C'RP',GOLACT0                                                   
         BE    FMT42                                                            
         CLI   GOLACT0,C'T'                                                     
         BNE   FMT44                                                            
*                                                                               
FMT42    MVC   0(4,R4),=C'TOT='                                                 
         LA    R4,4(R4)                                                         
         MVC   0(7,R4),WORK2                                                    
         J     FMT45                                                            
*                                                                               
FMT44    MVC   0(8,R4),WORK2     WE HAVE MORE ROOM WITHOUT TOT=                 
FMT45    OC    8(11,R2),SPACES                                                  
         FOUT  (R2)                                                             
*                                                                               
FMT46    OI    4(R2),X'20'         SET FLAG                                     
         B     FMT60                                                            
         EJECT                                                                  
* FORMAT CPP DATA                                                               
*                                                                               
FMT50    L     R0,8(R6)            GET DOLLARS                                  
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(9,WORK2),2,ALIGN=LEFT                                      
         MVC   8(7,R2),WORK2                                                    
         OC    8(11,R2),SPACES                                                  
         FOUT  (R2)                                                             
*                                                                               
         SR    R0,R0               SKIP POINTS FIELD                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
FMT60    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,(2,2(R6)),(4,WORK2)                                 
         MVC   8(13,R2),SPACES     BLANK PREVIOUS INPUT                         
         MVC   8(5,R2),WORK2                                                    
         OC    8(13,R2),SPACES                                                  
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET FLAG                                     
*                                                                               
FMTX     DS    0H                                                               
         XIT1  REGS=(R2)                                                        
         LTORG                                                                  
         EJECT                                                                  
* R6 POINTS TO PREVIOUS ELEMENT                                                 
* R8 POINTS TO PREVIOUS BWEEKS ENTRY                                            
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
*                                                                               
         CLC   ELCODE,0(R6)                                                     
         JNE   NEXTEL                                                           
*                                                                               
NEXTEL2  OC    0(2,R8),0(R8)       TEST END OF WEEK LIST                        
         JZ    NEXTEL              YES - SKIP TO END OF REC                     
*                                                                               
         CLC   2(2,R6),0(R8)       THIS WEEK TO WEEK LIST                       
         JL    NEXTEL                                                           
         BER   RE                                                               
         LA    R8,2(R8)            NEXT WEEK IN LIST                            
         J     NEXTEL2                                                          
*                                                                               
NEXTELX  LTR   RE,RE               EXIT WITH CC NOT=                            
         BR    RE                                                               
         SPACE 2                                                                
GLERR    GOTO1 ERROR                                                            
         PRINT OFF                                                              
       ++INCLUDE SPGOLWRK                                                       
         PRINT ON                                                               
 END                                                                            
