*          DATA SET SPBUY20    AT LEVEL 057 AS OF 08/18/20                      
*PHASE T21120C                                                                  
*==============================================================                 
*   AUG13 MHER FIX OTHER STATION MAKEGOOD CODES                                 
*   MAY07 MHER MORE BUY RECORDS                                                 
*   NOV06 MHER 6K BUY RECORDS                                                   
* 17FEB06 MHER CAN NET COST OVRDS ALWAYS CARRIED IN PENNIES BUT                 
*              DISPLAYED IN DOLLARS                                             
* 27MAY03 MHER OBSERVE BUY IS IN DOLLARS FLAG FOR CAN NET                       
*==============================================================                 
                                                                                
         TITLE 'T21120 - SPOTPAK BUY - LINE DISPLAY'                            
T21120   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21120,RR=R8                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21120+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         C     R8,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R8,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
HAVERELO DS    0H                                                               
         CLI   BUTRCODE,C'B'       IF NEW BUY                                   
         BNE   DISP0                                                            
         OC    ABUYINPH,ABUYINPH   AND MORE THAN 1 INPUT LINE?                  
         BNZ   *+8                                                              
DISP0    ST    R2,ABUYINPH         SAVE INPUT LINE ADDRESS                      
         MVI   PFKEYLEN,0                                                       
         XC    PFKEYADR,PFKEYADR                                                
*                                                                               
         L     RE,=A(PFKEYN)                                                    
         CLI   RCLOPT,C'N'                                                      
         BE    DISP1                                                            
         L     RE,=A(PFKEYS)                                                    
         CLI   RCLOPT,C'S'         TEST SORTED DISPLAY                          
         BNE   DISP2                                                            
*                                                                               
DISP1    TM    SVXFRCTL,SVXFR_DARE                                              
         BZ    *+8                                                              
         L     RE,=A(PFKEYND)                                                   
         A     RE,RELO                                                          
         ST    RE,PFKEYADR         SET ADDR OF PFKEY LINE DATA                  
         MVC   PFKEYLEN,1(RE)      AND LENGTH                                   
         B     DISP10                                                           
*                                                                               
DISP2    MVC   BYTE,RCLOPT                                                      
         NI    BYTE,X'7F'                                                       
         L     RE,=A(PFKEYR)                                                    
         CLI   BYTE,RCLROT                                                      
         BE    DISP4                                                            
         L     RE,=A(PFKEYP)                                                    
         CLI   BYTE,RCLPAY                                                      
         BE    DISP4                                                            
         L     RE,=A(PFKEYX)                                                    
         CLI   BYTE,RCLPAYDT                                                    
         BE    DISP4                                                            
         CLI   BYTE,RCLHIST        HISTORY USES SAME PFKEYS                     
         BE    DISP4                                                            
         B     DISP10                                                           
*                                                                               
DISP4    A     RE,RELO                                                          
         ST    RE,PFKEYADR                                                      
         MVC   PFKEYLEN,1(RE)                                                   
         B     DISP10                                                           
RELO     DC    A(0)                                                             
*                                                                               
* IF MORE THAN ONE BUY OR COPY/MOVE THIS TRANS, DISPLAY LINE ONLY               
*                                                                               
DISP10   LA    R4,BUYINP2H                                                      
         LA    R5,3                                                             
DISP20   CLC   =C'B,',8(R4)                                                     
         BE    DISPX                                                            
         CLC   =C'B,',9(R4)        CHECK FOR IT AFTER * TTOO                    
         BE    DISPX                                                            
         CLC   =C'COP',8(R4)                                                    
         BE    DISPX                                                            
         CLC   =C'COP',9(R4)                                                    
         BE    DISPX                                                            
         CLC   =C'MOV',8(R4)                                                    
         BE    DISPX                                                            
         CLC   =C'MOV',9(R4)                                                    
         BE    DISPX                                                            
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         BCT   R5,DISP20                                                        
         B     DISPX2                                                           
*                                                                               
DISPX    MVI   RCLOPT,0                                                         
*                                                                               
DISPX2   CLI   RCLOPT,C'N'                                                      
         BE    DISPX4                                                           
         CLI   RCLOPT,C'S'         NEVER COMPARE FOR SAME AS SVLIN !            
         BE    LD1A                                                             
         NI    RCLOPT,X'7F'                                                     
*                                                                               
* IF PREVIOUS LINE = THIS LINE, WRITE OVER OLD DSPLY                            
*                                                                               
DISPX4   CLC   PRVDSPLN,BUYKEY+10                                               
         BNE   LD1                                                              
         MVC   BLDROW(4),PRVROW                                                 
         L     RE,PRVEND                                                        
         XC    0(3,RE),0(RE)       AND RESET E-O-S IND                          
* SAVE START OF CURRENT DISPLAY                                                 
LD1      MVC   PRVDSPLN,SVLIN                                                   
*                                                                               
LD1A     MVC   SVRCLOPT,RCLOPT     SAVE LAST RECALL OPTION                      
         MVC   PRVROW(4),BLDROW                                                 
* AND NEED E-O-S ADDRESS TOO                                                    
         LA    R2,BUYOUTH                                                       
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   *-10                                                             
         ST    R2,PRVEND                                                        
* IF NOT ON FIRST DSPLY LINE, SUPPRESS OPTION                                   
         CLI   RCLOPT,C'N'                                                      
         BE    LD2                                                              
         CLI   RCLOPT,C'S'                                                      
         BE    LD2                                                              
         CLC   BLDROW(2),=X'000E'  TEST ON FIRST LINE                           
         BNH   LD2                                                              
         CLC   =C'ORB',BUYINP1+3                                                
         BNE   LD1G                                                             
         MVC   BLDROW(2),=X'000E'                                               
         LA    R2,BUYOUTH                                                       
         ST    R2,PRVEND                                                        
         XC    0(3,R2),0(R2)       AND RESET E-O-S HERE                         
         B     LD2                                                              
*                                                                               
LD1G     MVI   SVRCLOPT,0          SUPPRESS IT                                  
         MVI   RCLOPT,0                                                         
*                                                                               
LD2      SR    R0,R0               PASS GLOBBER CURRENT LINE NUMBER             
         ICM   R0,3,BUYKEY+10                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         GOTO1 VGLOBBER,DMCB,=C'PUTF',DUB,3,GLVSPBUY                            
*                                                                               
         MVI   MAXNPW,1                                                         
         XC    MAXCOSLN,MAXCOSLN                                                
         MVC   LDOPT,DSPOPT                                                     
         MVC   BYTE,SVOPTS                                                      
         NI    BYTE,X'C0'          DROP ALL BUT PB/UPB                          
         OC    LDOPT,BYTE                                                       
* SCAN RECORD TO GET DISPLAY DIMENSIONS                                         
         LA    R4,DSPAREA                                                       
         XC    0(80,R4),0(R4)                                                   
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,13                                                        
         XC    ELEMDT,ELEMDT                                                    
         LA    R6,BDELEM                                                        
*                                                                               
LD4      BAS   RE,NEXTEL                                                        
         BNE   LD6                                                              
*                                                                               
         CLC   ELEMDT,2(R6)                                                     
         BE    *+8                                                              
         MVI   ELEMNO,0                                                         
         MVC   ELEMDT,2(R6)                                                     
         IC    RE,ELEMNO                                                        
         TM    6(R6),X'80'                                                      
         BO    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,ELEMNO                                                        
*                                                                               
         CLI   BUYKEY+3,X'FF'      TEST POL                                     
         BNE   LD5                 NO                                           
         OC    STDTP(4),STDTP                                                   
         BZ    LD4A                                                             
         CLC   STDTP,2(R6)         TEST IN DISPLAY PERIOD                       
         BH    LD4B                                                             
         CLC   ENDDTP,2(R6)                                                     
         BL    LD4B                                                             
*                                                                               
         CLC   STDTP,2(R6)                                                      
         BNE   *+14                                                             
         CLC   BUELEMNO,ELEMNO                                                  
         BH    LD4B                                                             
*                                                                               
LD4A     CLI   1(R6),14            TEST PIGGYBACK                               
         BNH   LD4B                                                             
         OI    LDOPT,X'80'                                                      
         TM    6(R6),X'10'         TEST MAKEGOOD PENDING?                       
         BZ    *+8                 NO                                           
         OI    LDOPT,X'01'                                                      
         CLC   11(1,R6),15(R6)     TEST UNEQUAL SLNS                            
         BE    *+8                                                              
         OI    LDOPT,X'40'                                                      
         LR    R7,R6                                                            
LD4A2    ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),X'10'                                                      
         BL    LD4B                                                             
         CLI   0(R7),X'1F'                                                      
         BL    LD4A2                                                            
*                                                                               
LD4B     CLC   ELEMNO,MAXNPW                                                    
         BNH   *+10                                                             
         MVC   MAXNPW,ELEMNO                                                    
*                                                                               
         TM    6(R6),X'01'         TEST NO REALLOCATION                         
         BZ    *+8                                                              
         OI    LDOPT,X'01'                                                      
*                                                                               
         TM    6(R6),X'10'         MAKEGOOD PENDING?                            
         BZ    *+8                                                              
         OI    LDOPT,X'01'                                                      
*                                                                               
         TM    6(R6),X'20'         TEST RATE OVERRIDE                           
         BZ    LD5                                                              
         OI    LDOPT,X'20'                                                      
* WORK OUT HOW MANY DIGITS NEEDED                                               
         SR    R0,R0                                                            
         ICM   R0,7,7(R6)                                                       
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    *+8                                                              
         N     R0,=X'0003FFFF'     DROP SPOTS                                   
         SRDL  R0,32                                                            
         TM    BDSTAT,X'01'        TEST NETPAK                                  
         BZ    *+8                                                              
         M     R0,=F'100'          COST IS IN DOLLARS                           
         D     R0,=F'100'          DOLLARS IN R1, CENTS IN R0                   
         LA    RE,2                                                             
         C     R1,=F'100'                                                       
         BL    *+12                                                             
         LA    RE,3                                                             
         C     R1,=F'1000'                                                      
         BL    *+12                                                             
         LA    RE,4                                                             
         C     R1,=F'10000'                                                     
         BL    *+8                                                              
         LA    RE,6                SET TO MAX IF GE $10000                      
         LTR   R0,R0               TEST PENNIES NEEDED                          
         BZ    *+8                 NO                                           
         LA    RE,3(RE)                                                         
         LA    RE,1(RE)            ADD 1 FOR '$'                                
         CHI   RE,9                FIELD IS ONLY 9 CHARS WIDE                   
         BNH   *+8                                                              
         LHI   RE,9                SO THAT'S ABSOLUTLELY THE LIMIT              
         CH    RE,MAXCOSLN                                                      
         BNH   *+8                                                              
         STH   RE,MAXCOSLN                                                      
*                                                                               
LD5      BAS   RE,BPTEST           TEST SPOT PAID                               
         TM    BYTE,X'80'                                                       
         BZ    *+8                                                              
         MVI   0(R4),C'*'                                                       
         B     LD4                                                              
*                                                                               
LD6      LA    R4,2(R4)                                                         
*                                                                               
* DISPLAY START DATE                                                            
*                                                                               
LD7      DS    0H                                                               
         GOTO1 VDATCON,DMCB,(3,BDSTART),(7,(R4))                                
         MVI   5(R4),C'-'                                                       
         MVI   6(R4),C'E'                                                       
         CLI   BDINPUT,2           TEST INPUT METHOD                            
         BH    LD10                -E                                           
         BL    LD8                 WEEKS                                        
* MUST BE END DATE                                                              
         GOTO1 (RF),(R1),(3,BDEND),(7,6(R4))                                    
         B     LD10                                                             
*                                                                               
LD8      SR    R0,R0                                                            
         IC    R0,BDWKS                                                         
         EDIT  (R0),(2,6(R4)),ALIGN=LEFT                                        
         LA    RE,6(R4)                                                         
         AR    RE,R0                                                            
         MVI   0(RE),C'W'                                                       
         CLI   BDWKIND,C'O'                                                     
         BE    LD10                                                             
         MVC   1(1,RE),BDWKIND                                                  
*                                                                               
LD10     MVI   11(R4),C'*'                                                      
*                                                                               
* DISPLAY DAYS                                                                  
         LA    R4,12(R4)                                                        
         GOTO1 VCALLOV,DMCB,0,X'D9000A0F'  GET DAYUNPK ADDRESS                  
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(BDSEDAY,BDDAY),WORK                                   
         MVC   0(8,R4),WORK                                                     
         MVI   8(R4),C'*'                                                       
LD10A    LA    R4,8(R4)                                                         
         CLI   BDNOWK,99                                                        
         BH    *+8                                                              
         LA    R4,1(R4)                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BDNOWK                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         CLI   BDNOWK,99                                                        
         BH    LD10B                                                            
         UNPK  0(2,R4),DUB                                                      
         B     LD10D                                                            
*                                                                               
LD10B    UNPK  0(3,R4),DUB                                                      
         LA    R4,1(R4)                                                         
*                                                                               
LD10D    MVI   2(R4),C'*'                                                       
* TIME                                                                          
LD11     LA    R4,3(R4)                                                         
         MVC   WORK,SPACES                                                      
         GOTO1 VCALLOV,DMCB,0,X'D9000A11'  GET UNTIME ADDRESS                   
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),BDTIMST,WORK                                           
         MVC   0(11,R4),WORK                                                    
         MVI   11(R4),C'*'                                                      
* DPT                                                                           
         LA    R4,12(R4)                                                        
         MVC   0(1,R4),BDDAYPT                                                  
         MVI   1(R4),C'*'                                                       
* SPTLEN                                                                        
         LA    R4,2(R4)                                                         
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
         CLI   BDSEC,100                                                        
         BL    LD12                                                             
         BCTR  R4,0                                                             
         UNPK  0(3,R4),DUB                                                      
         LA    R4,1(R4)                                                         
LD12     MVI   2(R4),C'*'                                                       
*                                                                               
* PROGRAMMING                                                                   
*                                                                               
         LA    R4,3(R4)                                                         
         MVC   WORK(17),BDPROGRM                                                
LD14     MVC   0(17,R4),WORK                                                    
         MVI   19(R4),C'*'                                                      
* ADJ CODE                                                                      
         LA    R4,20(R4)                                                        
         CLI   SVCPROF+9,C'0'                                                   
         BE    LD20                                                             
         MVC   WORK(1),BDPROGT                                                  
         MVI   WORK+1,C' '                                                      
         CLI   SVCPROF+9,C'1'      TEST ALPHA ADJ                               
         BE    LD16                YES                                          
         SR    R0,R0                                                            
         IC    R0,BDPROGT                                                       
         SRDL  R0,4                                                             
         STC   R0,WORK                                                          
         OI    WORK,X'F0'                                                       
         SRL   R1,28                                                            
         STC   R1,WORK+1                                                        
         OI    WORK+1,X'F0'                                                     
LD16     SH    R4,=H'4'                                                         
         MVI   0(R4),C'*'                                                       
         MVC   1(2,R4),WORK                                                     
         MVI   3(R4),C'*'                                                       
         LA    R4,4(R4)                                                         
*                                                                               
* COST                                                                          
*                                                                               
LD20     MVI   WORK2,C'C'                                                       
         TM    BDCIND2,X'80'                                                    
         BO    LD22                                                             
         MVI   WORK2,C' '                                                       
         TM    BDCIND,X'20'                                                     
         BO    LD22                                                             
         MVI   WORK2,C'F'                                                       
         TM    BDCIND,X'80'                                                     
         BO    LD22                                                             
         MVI   WORK2,C'Q'                                                       
         TM    BDCIND,X'40'                                                     
         BO    LD22                                                             
         MVI   WORK2,C'N'                                                       
         TM    BDCIND,X'10'                                                     
         BO    LD22                                                             
         MVI   WORK2,C'V'                                                       
         TM    BDCIND,X'08'                                                     
         BO    LD22                                                             
         MVI   WORK2,C'S'                                                       
         TM    BDCIND,X'04'                                                     
         BO    LD22                                                             
         MVI   WORK2,C'X'                                                       
         TM    BDCIND,X'02'                                                     
         BO    LD22                                                             
         MVI   WORK2,C'P'                                                       
*                                                                               
LD22     SR    R0,R0                                                            
         ICM   R0,7,BDCOST                                                      
         BZ    LD22EDT                                                          
*                                                                               
         TM    BDCIND2,X'10'       TEST RATE IN DOLLARS                         
         BZ    *+8                                                              
         MHI   R0,100              CONVERT TO PENNIES                           
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    *+12                                                             
         TM    BDCIND2,X'01'       YES-TEST COST IN PENNIES                     
         BZ    LD22B                   NO                                       
         C     R0,=F'9999999'      IF COST TOO BIG                              
         BH    LD22A                DROP CENTS                                  
LD22EDT  EDIT  (R0),(8,(R4)),2                                                  
         B     LD22C                                                            
*                                                                               
LD22A    SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
*                                                                               
LD22B    EDIT  (R0),(8,(R4))                                                    
*                                                                               
LD22C    LA    R5,8(R4)                                                         
         LTR   R0,R0               TEST COST=0                                  
         BZ    LD24                YES - DON'T DISPLAY MINUS SIGN               
         TM    BDCIND,X'01'        TEST NEGATIVE COST                           
         BZ    *+12                                                             
         MVI   0(R5),C'-'                                                       
         LA    R5,1(R5)                                                         
*                                                                               
* FLOAT COST CHAR TO LEFT OF COST                                               
*                                                                               
LD24     CLI   WORK2,C' '                                                       
         BE    LD26                                                             
* FIND FIRST BLANK TO LEFT OF COST                                              
         LA    R1,6(R4)                                                         
         LA    R0,7                                                             
         CLI   0(R1),C' '                                                       
         BE    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         MVC   0(1,R1),WORK2       MOVE SPECIAL COST CODE                       
*                                                                               
LD26     TM    BDCIND2,X'02'                                                    
         BZ    LD30                                                             
* FIND FIRST BLANK TO LEFT OF COST                                              
         LA    R1,6(R4)                                                         
         LA    R0,7                                                             
         CLI   0(R1),C' '                                                       
         BE    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         MVI   0(R1),C'T'          SET TRADE FLAG                               
*                                                                               
LD30     CLI   9(R4),C'U'                                                       
         BE    *+8                                                              
         MVI   9(R4),C'*'                                                       
         LA    R4,10(R4)                                                        
* REF                                                                           
LD32     MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   LD35                                                             
*                                                                               
         SR    RE,RE                                                            
         IC    RE,2(R6)                                                         
         N     RE,=X'0000000F'     DROP 2-BYTE LINE FLAG                        
         BCTR  RE,0                                                             
         SLL   RE,1                X 2                                          
         LA    RE,REFTAB(RE)                                                    
         MVC   0(2,R4),0(RE)                                                    
         B     LD35                                                             
REFTAB   DC    C'PMPS....RMRSMMMS'                                              
                                                                                
* MAKEGOOD CODE                                                                 
                                                                                
LD35     CLI   BDMGDATE,X'C0'                                                   
         BNH   LD35A                                                            
         MVC   0(2,R4),BDMGDATE    MOVE MGCODE                                  
         B     LD36                                                             
*                                                                               
LD35A    MVI   ELCDLO,X'0B'       LOOK FOR MISSED SPOTS                         
         MVI   ELCDHI,X'0C'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
LD35B    BAS   RE,NEXTEL                                                        
         BNE   LD36                                                             
         CLI   1(R6),14            TEST ALLOCATED                               
         BL    LD35B                                                            
         CLI   13(R6),0            TEST MAKEGOOD CODE IN ELEM                   
         BE    LD35B               NO                                           
*                                                                               
         CLI   13(R6),X'FF'        TEST EXTENDED CODE                           
         BNH   LD35C               NO                                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'19'                                                      
         JNE   *+2                                                              
*                                                                               
LD35C     LR    R1,R6              POINT TO ELEM                                
          BAS   RE,TRANSCD                                                      
*                                                                               
LD35X     MVC   0(2,R4),HALF                                                    
          MVI   2(R4),C'-'                                                      
          B     *+8                                                             
*                                                                               
LD36     MVI   2(R4),C'*'                                                       
         LA    R4,3(R4)                                                         
* LINE NUM                                                                      
         SR    R0,R0                                                            
         ICM   R0,3,BUYKEY+10      ** LINE NUMBER **                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
*                                                                               
         CLI   RCLOPT,RCLSTA       TEST CAN NTWK STA DISPLAY                    
         BNE   LD36A                                                            
         LA    R2,BUYINP2H         DISPLAY OVER SECOND <<INPUT>> LINE           
         XC    0(3,R2),0(R2)       SET E-O-S FLAG                               
         MVC   BLDROW(4),=X'000B0000' DISPLAY STARTS IN ROW 11                  
         MVI   SVSCR,X'F1'            TELL BLDFLD WHAT'S UP                     
*                                                                               
LD36A    XC    BLDLIST,BLDLIST                                                  
         MVC   BLDLIST(4),=X'004F2000'                                          
         LA    RE,DSPAREA                                                       
         ST    RE,BLDLIST+4                                                     
         BAS   RE,GOBLDFLD                                                      
         BE    LD38                                                             
LD36X    MVI   ERRCD,X'FE'         INDICATE NO MORE ROOM                        
         B     EXIT                                                             
*                                                                               
*                                                                               
LD38     DS    0H                                                               
         CLC   =C'COP',BUTRCODE                                                 
         BE    LD38A                                                            
         CLC   =C'MOV',BUTRCODE                                                 
         BNE   *+12                                                             
                                                                                
LD38A    CLI   WORK2+20,0          TEST N= FEATURE                              
         BNE   LD50                                                             
*                                                                               
         CLI   RCLOPT,C'N'                                                      
         BE    LD38B                                                            
         CLI   RCLOPT,C'S'                                                      
         BE    LD38B                                                            
         CLI   SVCPROF+0,C'0'      TEST BRD POL                                 
         BNE   LD40                                                             
*                                                                               
LD38B    TM    SVOPTS,X'01'        MASPRD DSPLY                                 
         BO    LD40                                                             
         MVI   ELCDLO,X'66'        SEARCH FOR COMMENT ELEMENT                   
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    LD40                                                             
         MVI   ELCDLO,X'67'        SEARCH FOR ORBIT ELEMENT                     
         MVI   ELCDHI,X'67'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    LD40                                                             
         CLI   SVAPROF+7,C'C'      SEARCH FOR CANADIAN EXCHANGE                 
         BNE   LD39                RATE ELEMENT                                 
         MVI   ELCDLO,XCHCODEQ                                                  
         MVI   ELCDHI,XCHCODEQ                                                  
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    LD40                                                             
         EJECT                                                                  
*========================================================*                      
* TEST WHETHER A SECOND DISPLAY LINE IS NEEDED           *                      
*========================================================*                      
         SPACE 1                                                                
LD39     OC    BDREP,BDREP         TEST SPECIAL REP                             
         BNZ   LD40                                                             
         OC    BDNTAX,BDNTAX       OR TAX                                       
         BNZ   LD40                NO                                           
         CLI   BDTIME,0            OR PIGGYBACK                                 
         BNE   LD40                                                             
         CLC   =C'TEST',BUYBU                                                   
         BE    LD40                                                             
         TM    BDSTAT2,X'43'       TEST XOFF OR CFD                             
         BNZ   LD40                                                             
         CLI   SVNRGN,C'*'                                                      
         BE    LD40                                                             
         MVI   ELCDLO,X'61'        SPECIAL TRAFFIC                              
         MVI   ELCDHI,X'61'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    LD40                                                             
*                                                                               
         MVI   ELCDLO,X'67'        ORBIT                                        
         MVI   ELCDHI,X'67'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    LD40                                                             
*                                                                               
         MVI   ELCDLO,X'69'        NTWK TAX/GST/ID ELEM/PST                     
         MVI   ELCDHI,X'72'         INFML                                       
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    LD40                                                             
*                                                                               
         MVI   ELCDLO,X'73'        COS2                                         
         MVI   ELCDHI,X'73'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   LD39A                                                            
         OC    2(4,R6),2(R6)       HELP PO WALLER                               
         BNZ   LD40                                                             
*                                                                               
LD39A    DS    0H                                                               
*&&DO                                                                           
         MVI   ELCDLO,X'19'        DIFFERENT STATION MAKEGOOD                   
         MVI   ELCDHI,X'19'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    LD40                                                             
*                                                                               
         MVI   ELCDLO,X'29'        DIFFERENT STATION MAKEGOOD                   
         MVI   ELCDHI,X'29'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    LD40                                                             
*&&                                                                             
         MVI   ELCDLO,X'95'        CAN EXCH OR UPLOAD                           
         MVI   ELCDHI,X'96'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    LD40                                                             
*                                                                               
         LA    R6,BDELEM                                                        
         USING BDELEM,R6                                                        
         TM    BDSTAT2,X'80'       DAILY SKED                                   
         BO    LD40                                                             
         B     LD50                                                             
         DROP  R6                                                               
*                                                                               
LD40     BRAS  RE,BLDLINE2                                                      
         BAS   RE,GOBLDFLD                                                      
         BNE   LD36X               NO MORE ROOM                                 
         EJECT                                                                  
* BRANCH TO APPROPRIATE DISPLAY ROUTINE                                         
*                                                                               
LD50     DS    0H                                                               
         CLI   RCLOPT,RCLDEM                                                    
         BE    LD51                                                             
         CLI   RCLOPT,RCLPDEM                                                   
         BE    LD51                                                             
         XC    SVSPLMKT,SVSPLMKT                                                
*                                                                               
LD51     CLI   RCLOPT,C'N'                                                      
         BE    EXIT                                                             
         CLI   RCLOPT,C'S'                                                      
         BE    EXIT                                                             
* TEST FOR RECSIZE PROBLEMS                                                     
LD52     LHI   R0,3700                                                          
         CLI   SVBIGMAX,C'Y'                                                    
         BNE   *+8                                                              
         LHI   R0,5700                                                          
         CLM   R0,3,BUYREC+13                                                   
         BH    *+10                                                             
         MVC   BUYMSG(51),=C'** WARNING - RECORD LENGTH IS APPROACHING *        
               MAXIMUM **'                                                      
*                                                                               
*&&DO*&& BRAS  RE,CHKINPER         CHECK ALL SPOTS IN BUY PERIOD                
*                                                                               
         ZIC   RE,RCLOPT                                                        
         SLL   RE,25               DROP HOB                                     
         SRL   RE,23               X 4                                          
         L     RF,RCLTAB(RE)       GET ADDRESS                                  
         LA    RF,0(RF)            DROP HOB                                     
         LTR   RF,RF               TEST IN T21121                               
         BZ    EXIT                                                             
         A     RF,RELO                                                          
         BASR  RE,RF               SET RE FOR HELP IN DUMPS                     
* NO RETURN HERE *                                                              
*                                                                               
RCLTAB   DS    0F                                                               
         DC    AL1(0),AL3(LDX)                                                  
         DC    AL1(RCLROT),AL3(LD100)                                           
         DC    AL1(RCLPAY),AL3(LD100)                                           
         DC    AL1(RCLPAYDT),AL3(LD100)                                         
         DC    AL1(RCLINV),AL3(0)                                               
         DC    AL1(RCLREF),AL3(0)                                               
         DC    AL1(RCLCOM),AL3(0)                                               
         DC    AL1(RCLDEM),AL3(0)                                               
         DC    AL1(RCLACTV),AL3(0)                                              
         DC    AL1(RCLORB),AL3(0)                                               
         DC    AL1(RCLSPILL),AL3(0)                                             
         DC    AL1(RCLSTA),AL3(0)                                               
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(RCLFLM),AL3(0)                                               
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(RCLSCH),AL3(LD200)                                           
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(RCLRSVP),AL3(0)                                              
         DC    AL1(RCLDEMS),AL3(0)                                              
         DC    AL1(RCLPDEM),AL3(0)                                              
         DC    AL1(RCLPDEMX),AL3(0)                                             
         DC    AL1(RCLXCH),AL3(LD300)                                           
         DC    AL1(RCLDSK),AL3(0)                                               
         DC    AL1(RCLNET),AL3(0)                                               
         DC    AL1(RCLDT),AL3(0)                                                
         DC    AL1(RCLCUT),AL3(LD400)                                           
         DC    AL1(RCLCLST),AL3(0)                                              
         DC    AL1(RCLNLST),AL3(0)                                              
         DC    AL1(RCLZCUT),AL3(LD500)                                          
         DC    AL1(RCLHIST),AL3(0)                                              
         SPACE 1                                                                
*========================================================*                      
* IF ANY UNPROTECTED FIELDS DISPLAYED, ADD TAB FIELD     *                      
*========================================================*                      
         SPACE 1                                                                
LDX      BRAS  RE,CHKINPER         SEE IF SPOTS OUTSIDE BUY PER                 
*                                                                               
         LA    R2,BUYOUTH                                                       
         BAS   RE,FNDUF                                                         
         BNE   EXIT                                                             
*                                                                               
         XC    BLDLIST,BLDLIST                                                  
         MVC   BLDLIST(4),=X'01010000'                                          
         BAS   RE,GOBLDFLD                                                      
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
LDERR    DS    0H                                                               
         CLC   =C'002ORB',BUYINP1                                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SVRCLOPT,0          RESET LAST DISPLAY                           
         L     R2,ABUYINPH                                                      
         GOTO1 ERROR                                                            
         EJECT                                                                  
*                                                                               
* ROTATION                                                                      
*                                                                               
LD100    CLI   BUYKEY+3,X'FF'      TEST POOL BUY                                
         BNE   LD150                                                            
* POOL                                                                          
         CLI   RCLOPT,RCLPAYDT                                                  
         BNE   LD102A                                                           
*                                                                               
* LOOK FOR A MKTFIX ELEM OR A CALL LETTER CHANGE ELEM                           
* IGNORE SITUATIONS WHERE THEY HAVE DONE BOTH                                   
*                                                                               
         XC    BUMVDATA,BUMVDATA                                                
*                                                                               
         MVI   ELCDLO,X'9C'        LOOK FOR A MKTFIX ELEM                       
         MVI   ELCDHI,X'9C'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   LD101                                                            
*                                                                               
         USING MFXELEM,R6                                                       
         MVC   BUMVDATE,MFXDATE                                                 
         MVC   BUMVMKT,MFXMKT      UPDATE MARKET                                
         MVC   BUMVSTA,BUYKEY+6                                                 
         B     LD102                                                            
         DROP  R6                                                               
*                                                                               
LD101    MVI   ELCDLO,X'9D'        LOOK FOR A STA CALL LTR CHG                  
         MVI   ELCDHI,X'9D'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   LD102A                                                           
*                                                                               
         USING SFXELEM,R6                                                       
         MVC   BUMVDATE,SFXDATE                                                 
         MVC   BUMVMKT,BUYKEY+4                                                 
         MVC   BUMVSTA,SFXSTA      UPDATE STATION                               
         DROP  R6                                                               
*                                                                               
LD102    XC    BLDLIST,BLDLIST     DISPLAY PREVIOUS CLEARANCE DATA              
         MVC   BLDLIST(4),=X'004C2000'                                          
         LA    RE,ELEM                                                          
         ST    RE,BLDLIST+4                                                     
         MVC   ELEM(80),SPACES                                                  
         MVC   ELEM(40),=C'** CLEARANCES ON OR BEFORE JAN01/00 WERE'            
         MVC   ELEM+41(14),=C'FOR 1521/WABCA'                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(2,BUMVDATE),(5,ELEM+27)                            
         GOTO1 STAPACK,DMCB,(C'U',BUMVMKT),ELEM+45,ELEM+50                      
         BAS   RE,GOBLDFLD                                                      
*                                                                               
LD102A   XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         LA    RE,DSPDATE                                                       
         LA    RF,9                                                             
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    LD102B                                                           
         CLI   MAXNPW,1                                                         
         BNH   LD102B                                                           
         LA    RE,DSPBIGDT                                                      
         LA    RF,12                                                            
*                                                                               
LD102B   ST    RE,4(R1)                                                         
         TM    LDOPT,X'20'         TEST RATE O/R                                
         BZ    *+8                                                              
         AH    RF,MAXCOSLN                                                      
         STC   RF,1(R1)                                                         
         MVI   2(R1),X'20'         SET PROT                                     
*                                                                               
         LA    R1,8(R1)                                                         
         LA    RE,DSPPRD                                                        
         ST    RE,4(R1)                                                         
         MVC   0(4,R1),=X'00040001'                                             
*                                                                               
         LHI   RE,7                                                             
         TM    SVCOPT3,COP3SPOD    TEST SPODS ALLOWED                           
         BO    LD103                                                            
         LA    RE,4                                                             
         TM    LDOPT,X'80'         TEST P/B                                     
         BZ    *+8                                                              
         LA    RE,5(RE)                                                         
         TM    LDOPT,X'40'         TEST UNEQ SPLIT                              
         BZ    *+8                                                              
         LA    RE,6(RE)                                                         
         TM    LDOPT,X'01'         TEST NO REALLOCATION                         
         BZ    *+8                                                              
         LA    RE,1(RE)                                                         
*                                                                               
LD103    STC   RE,1(R1)                                                         
*                                                                               
         LA    R1,8(R1)                                                         
         SR    RF,RF                                                            
         CLI   RCLOPT,RCLROT       +R                                           
         BE    LD110                                                            
         LA    RE,DSPPBIND                                                      
         LA    RF,3                                                             
         CLI   RCLOPT,RCLPAY       +P                                           
         BE    LD104                                                            
         LA    RE,DSPPAYDT         +X                                           
         LA    RF,11                                                            
*                                                                               
LD104    ST    RE,4(R1)                                                         
         STC   RF,1(R1)            SET DATA LEN                                 
         MVI   2(R1),X'20'         SET PROTECTED                                
         MVI   3(R1),1                                                          
         LA    R1,8(R1)                                                         
*                                                                               
         CLI   RCLOPT,RCLPAYDT                                                  
         BNE   LD110                                                            
         LA    RE,DSPCHECK         REP=999  CK=999999 *JUL13/92                 
         ST    RE,4(R1)                                                         
         MVI   1(R1),27            SET LEN                                      
         MVI   2(R1),X'20'         SET PROT                                     
         MVI   3(R1),1                                                          
*                                                                               
LD110    XC    ELEMDT,ELEMDT                                                    
         MVI   ERRCD,NOELEMS                                                    
         LA    R6,BDELEM                                                        
         BAS   RE,POLEL                                                         
         BE    LD112                                                            
         CLC   =C'LI',BUTRCODE     TEST RECALL ACTION                           
         BE    LDERR               IF YES, ERROR                                
         B     LDX                 ELSE NO SPOTS LEFT IN PERIOD                 
LD112    BAS   RE,GOBLDFLD                                                      
         BNE   LDX                                                              
         MVC   SVRCLEND,2(R6)      SAVE DATE OF LAST SPOT DISPLAYEED            
         MVC   SVRCLEND+2(1),ELEMNO    AND SPOT NUMBER                          
         BAS   RE,POLEL                                                         
         BE    LD112                                                            
         XC    SVRCLEND,SVRCLEND                                                
         B     LDX                                                              
*                                                                               
         EJECT                                                                  
* NON-POOL                                                                      
LD150    XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         LA    RE,DSPDATE                                                       
         ST    RE,4(R1)                                                         
         MVC   0(4,R1),=X'01092001'                                             
         LA    R1,8(R1)                                                         
*                                                                               
         SR    RF,RF                                                            
         CLI   RCLOPT,RCLROT       +R                                           
         BE    LD152                                                            
         LA    RE,DSPPBIND                                                      
         LA    RF,3                                                             
         CLI   RCLOPT,RCLPAY       +P                                           
         BE    LD152                                                            
         LA    RE,DSPPAYDT                                                      
         LA    RF,11                                                            
*                                                                               
LD152    LTR   RF,RF                                                            
         BZ    LD160                                                            
         ST    RE,4(R1)            SET DATA ADDR                                
         STC   RF,1(R1)            SET DATA LEN                                 
         MVI   2(R1),X'20'         SET PROTECTED                                
         MVI   3(R1),1                                                          
         LA    R1,8(R1)                                                         
*                                                                               
         CLI   RCLOPT,RCLPAYDT                                                  
         BNE   LD160                                                            
         LA    RE,DSPCHECK         REP=999  CK=999999 *JUL13/92                 
         ST    RE,4(R1)                                                         
         MVI   1(R1),28            SET LEN                                      
         MVI   2(R1),X'20'         SET PROT                                     
         MVI   3(R1),1                                                          
*                                                                               
LD160    MVI   ERRCD,NOELEMS                                                    
         LA    R6,BDELEM                                                        
         BAS   RE,REGEL                                                         
         BE    LD162                                                            
         CLC   =C'LI',BUTRCODE     TEST RECALL ACTION                           
         BE    LDERR               IF YES, ERROR                                
         B     LDX                 ELSE NO SPOTS LEFT IN PERIOD                 
LD162    BAS   RE,GOBLDFLD                                                      
         BNE   LDX                                                              
         MVC   SVRCLEND,2(R6)      SAVE LAST DATE DISPLAYED                     
         BAS   RE,REGEL                                                         
         BE    LD162                                                            
         XC    SVRCLEND,SVRCLEND                                                
         B     LDX                                                              
         EJECT                                                                  
* SCHEDULE DISPLAY                                                              
*                                                                               
LD200    XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         LA    RE,DSPDATE                                                       
         ST    RE,4(R1)                                                         
         MVC   0(4,R1),=X'00052001'                                             
         LA    R1,8(R1)                                                         
         LA    RE,DSPPRD                                                        
         ST    RE,4(R1)                                                         
         MVC   0(4,R1),=X'00030001'                                             
         SPACE 1                                                                
* BUILD LIST OF DATES IN BUY DESC PERIOD                                        
* SIX BYTE ENTRIES ARE MONDAY/AIR DATE/SPOTS/INDS X'80'=OTO'S                   
         SPACE 1                                                                
         L     R4,AREC4                                                         
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK+6                                  
         GOTO1 (RF),(R1),(3,BDEND),WORK+12                                      
         MVC   WORK(6),WORK+6                                                   
         GOTO1 VGETDAY,(R1),WORK,WORK+18  FIND OUT START DAY                    
         CLI   0(R1),1             TEST MONDAY                                  
         BE    LD201                                                            
         ZIC   R0,0(R1)            ELSE BACK UP TO MONDAY                       
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
         GOTO1 VADDAY,(R1),WORK,WORK,(R0)                                       
*                                                                               
* WORK=MONDAY/ WORK+6=SCHEDULE/ WORK+12=SCHEDULE END                            
*                                                                               
LD201    DS    0H                                                               
         GOTO1 VDATCON,(R1),WORK,(2,(R4))                                       
         GOTO1 (RF),(R1),WORK+6,(2,2(R4))                                       
         MVI   4(R4),0                                                          
         MVI   5(R4),0                                                          
         LA    R4,6(R4)                                                         
         GOTO1 VADDAY,(R1),WORK,WORK,7   NEXT MODNAY                            
         GOTO1 (RF),(R1),WORK+6,WORK+6,7  NEXT SKED DATE                        
         CLC   WORK+6(6),WORK+12                                                
         BNH   LD201                                                            
*                                                                               
* NOW POST ACTUAL SPOTS PER WEEK TO LIST                                        
*                                                                               
LD220    MVI   0(R4),X'FF'         SET E-O-L FLAG                               
         LA    RE,X'0B'                                                         
         CLI   BUYKEY+3,X'FF'                                                   
         BE    *+8                                                              
         LA    RE,X'06'                                                         
         STC   RE,ELCDLO                                                        
         LA    RE,2(RE)                                                         
         STC   RE,ELCDHI                                                        
         LA    R6,BDELEM                                                        
*                                                                               
LD222    BAS   RE,NEXTEL                                                        
         BNE   LD240                                                            
* FIND WEEK IN LIST                                                             
         L     R4,AREC4                                                         
LD224    CLC   2(2,R6),6(R4)       ELEM TO NEXT LIST DATE                       
         BL    LD226                                                            
         LA    R4,6(R4)                                                         
         B     LD224                                                            
LD226    CLI   BUYKEY+3,X'FF'      TEST POL                                     
         BE    LD228                                                            
         ZIC   R0,7(R6)                                                         
         B     LD230                                                            
LD228    LA    R0,1                                                             
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    *+12                                                             
         IC    R0,7(R6)                                                         
         SRL   R0,2                                                             
LD230    TM    6(R6),X'80'         TEST MINUS SPOT                              
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         ZIC   RE,4(R4)                                                         
         AR    RE,R0                                                            
         STC   RE,4(R4)            UPDATE SPOT COUNT                            
         CLC   ELCDLO,0(R6)        TEST REGELEM                                 
         BE    *+8                 YES                                          
         OI    5(R4),X'80'         NO - SET OTO FLAG                            
         B     LD222                                                            
         EJECT                                                                  
* NOW DISPLAY LIST                                                              
*                                                                               
LD240    L     R4,AREC4                                                         
         B     *+8                                                              
LD242    LA    R4,6(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BE    LDX                                                              
         OC    STDTP(4),STDTP      TEST DATE FILTERS                            
         BZ    LD244                                                            
         CLC   STDTP,2(R4)                                                      
         BH    LD242                                                            
         CLC   ENDDTP,2(R4)                                                     
         BL    LD242                                                            
*                                                                               
LD244    MVC   BLDLIST+08(4),=X'00030001'                                       
*&&DO                                                                           
         CLI   5(R4),0             TEST NEED TO PROTECT                         
         BE    *+10                                                             
         MVC   BLDLIST+08(4),=X'01032002'  SET 1 MORE LDG/TRLG SPACE            
*&&                                                                             
         GOTO1 VDATCON,DMCB,(2,2(R4)),(4,DSPDATE)                               
*                                                                               
         ZIC   R0,4(R4)                                                         
**NOP**  EDIT  (R0),(3,DSPPRD),ALIGN=LEFT                                       
         EDIT  (R0),(3,DSPPRD),ALIGN=LEFT,ZERO=NOBLANK                          
         LA    RE,DSPPRD                                                        
         AR    RE,R0                                                            
*                                                                               
         CLI   5(R4),0             TEST ANY OTO'S                               
         BE    *+8                                                              
         MVI   0(RE),C'?'          SET FLAG FOR USER                            
*                                                                               
         BAS   RE,GOBLDFLD                                                      
         BNE   LDX                                                              
         B     LD242                                                            
         EJECT                                                                  
*=====================================================*                         
* DISPLAY EXCHANGE RATE ELEMENT                       *                         
*=====================================================*                         
         SPACE 1                                                                
LD300    DS    0H                                                               
         XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         LA    RE,DSPAREA          DEC20/89 = $1.2345                           
         LA    RF,30               0123456789012345678                          
         ST    RE,4(R1)                                                         
         STC   RF,1(R1)                                                         
         MVI   2(R1),X'20'         SET PROTECTED                                
         MVI   3(R1),5             SET TRAILING SPACES                          
*                                                                               
         MVI   ERRCD,NOTINBUY                                                   
         MVI   ELCDLO,XCHCODEQ                                                  
         MVI   ELCDHI,XCHCODEQ                                                  
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   LDERR                                                            
         USING XCHELEM,R6                                                       
*                                                                               
         LA    R4,9(R6)            POINT PAST FIXED PORTION                     
         ZIC   R5,1(R6)            GET ELEMENT LENGTH                           
         SH    R5,=H'9'                                                         
         BZ    EXIT                                                             
         SRL   R5,2                DIVIDE BY 4                                  
*                                                                               
LD302    DS    0H                                                               
         LA    R7,DSPAREA                                                       
         XC    DSPAREA,DSPAREA                                                  
         GOTO1 VDATCON,DMCB,(2,(R4)),(5,(R7))                                   
         MVI   10(R7),C'='                                                      
         LA    R7,12(R7)                                                        
         SR    R0,R0                                                            
         ICM   R0,3,2(R4)          GET RATE                                     
         EDIT  (R0),(7,(R7)),4,FLOAT=$                                          
*                                                                               
         BAS   RE,GOBLDFLD                                                      
*                                                                               
         LA    R4,4(R4)                                                         
         BCT   R5,LD302                                                         
         B     EXIT                                                             
         EJECT                                                                  
*=========================================================*                     
* DISPLAY CANADIAN NETWORK CUT-INS                        *                     
*=========================================================*                     
         SPACE 1                                                                
LD400    XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         LA    RE,DSPDATE                                                       
         ST    RE,4(R1)                                                         
         MVC   0(4,R1),=X'08082001'                                             
         LA    R4,SVCUTDTS                                                      
         SR    R5,R5                                                            
         ICM   R5,1,SVCUTLOW                                                    
         BNZ   *+8                                                              
         LA    R5,1                                                             
         XC    DSPDATE(8),DSPDATE                                               
*                                                                               
LD402    DS    0H                                                               
         GOTO1 VDATCON,DMCB,(2,(R4)),(4,DSPDATE)                                
         CH    R5,=H'1'                                                         
         BNH   LD404                                                            
         MVI   DSPDATE+5,C'-'                                                   
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPDATE+6(2),DUB                                                 
*                                                                               
LD404    BAS   RE,GOBLDFLD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DSPDATE(8),DSPDATE                                               
         MVI   BLDLIST,0           SET FOR NO LEADING SPACES                    
         LA    R5,1(R5)            BUMP COUNTER                                 
         CLC   0(2,R4),2(R4)       TEST NEXT DATE EQUAL                         
         BE    *+8                 YES                                          
         LA    R5,1                RESET COUNTER                                
         LA    R4,2(R4)            BUMP DATE LIST POINTER                       
         OC    0(2,R4),0(R4)       TEST MORE DATES                              
         BNZ   LD402                                                            
*                                                                               
         ZIC   R0,SVCUTELX                                                      
         ZIC   RE,SVCUTEL                                                       
         SR    R0,RE                                                            
         STH   R0,HALF             SAVE NUMBER OF DATES                         
*                                                                               
         LH    RE,BLDROW                                                        
         LA    RE,1(RE)            BUMP TO NEXT ROW                             
         SLL   RE,16                                                            
         ST    RE,BLDROW           AND SET COLUMN TO 0                          
*                                                                               
         LA    R4,SVCUTLST                                                      
*                                                                               
LD406    MVC   BLDLIST(4),=X'00042003'    DISPLAY STATION                       
         GOTO1 STAPACK,DMCB,(C'U',(R4)),WORK,(X'80',WORK+4)                     
         MVC   DSPDATE(4),WORK+4                                                
         BAS   RE,GOBLDFLD                                                      
         XC    DSPDATE,DSPDATE                                                  
         EJECT                                                                  
*========================================================*                      
* NOW DISPLAY ALLOCATIONS                                *                      
*========================================================*                      
         SPACE 1                                                                
LD408    LA    R7,8(R4)                                                         
         LH    R6,HALF                                                          
*                                                                               
LD410    ST    R7,FULL                  SAVE ADDRESS                            
         MVC   BLDLIST(4),=X'00070000'  START OUT UNPROT                        
         TM    0(R7),X'EF'              TEST ANY INDS ON                        
         BE    LD412                    NO                                      
         MVC   BLDLIST(4),=X'01072001'  IF YES, PROTECT                         
         TM    0(R7),1                  TEST NO SPOT                            
         BO    LD412                                                            
         MVC   DSPDATE(3),=C'---'       SET NOT ALLOCABLE                       
         B     LD418                                                            
*                                                                               
LD412    CLI   1(R7),0             TEST UNALL                                   
         BNE   LD414                                                            
         TM    5(R7),X'04'         TEST HIATUS                                  
         BZ    LD418                                                            
         MVC   DSPDATE(4),=C'HIAT'                                              
         B     LD418                                                            
*                                                                               
LD414    LA    R7,1(R7)            POINT TO PRD                                 
         BAS   RE,GETCD                                                         
         MVC   DSPDATE(3),0(R1)                                                 
*                                                                               
         LA    R7,1(R7)            POINT TO PRD 2                               
         CLI   0(R7),0             TEST IT IS THERE                             
         BE    LD418               NO                                           
         LA    RF,DSPDATE+2                                                     
         CLI   0(RF),C' '          TEST LENGTH OF PRD 1                         
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         MVI   1(RF),C'-'                                                       
         BAS   RE,GETCD                                                         
         MVC   2(3,RF),0(R1)                                                    
*                                                                               
LD418    BAS   RE,GOBLDFLD                                                      
         BE    *+6                                                              
         DC    H'0'                IT IS DESIGNED TO FIT !                      
         XC    DSPDATE,DSPDATE                                                  
*                                                                               
         L     R7,FULL                                                          
         LA    R7,6(R7)                                                         
         BCT   R6,LD410                                                         
*                                                                               
         LA    R4,L'SVCUTLST(R4)   NEXT STATION                                 
         LA    R0,SVCUTDTS                                                      
         CR    R4,R0                                                            
         BNL   LD420                                                            
         OC    0(5,R4),0(R4)       TEST ANY MORE IN LIST                        
         BZ    LD420               NO                                           
         LH    RE,BLDROW                                                        
         LA    RE,1(RE)                                                         
         SLL   RE,16                                                            
         ST    RE,BLDROW                                                        
         B     LD406                                                            
*                                                                               
LD420    B     LDX                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
*        DISPLAY CANADIAN NETWORK CUT-INS                            *          
**********************************************************************          
*                                                                               
LD500    LA    R7,SVCUTLST+8                                                    
*                                                                               
         XC    BLDLIST,BLDLIST       SET PARAMETERS FOR GOBLDFLD                
         LA    R1,BLDLIST                                                       
         LA    R2,ELEM                                                          
         ST    R2,4(R1)                                                         
         MVC   0(4,R1),=X'08462000'                                             
*                                                                               
         LA    R5,0                  AND COUNTER (R5) TO ZERO                   
*                                                                               
LD500A   LA    R4,SVZCUTST           IF STATIONS HAVE BEEN SELECTED             
         XC    ELEM,ELEM             ONLY SEND CUT STATIONS TO SCREEN           
*                                                                               
LD501A   GOTO1 STAPACK,DMCB,(C'U',(R4)),WORK,(X'80',WORK+4)                     
         MVC   0(4,R2),WORK+4                                                   
         LA    R2,5(R2)              MOVE STATION FROM SVNDEF INTO              
         LA    R5,1(R5)              ELEMENT                                    
*                                                                               
         CHI   R5,27                 WHEN 27 STATIONS REACHED ...               
         BNE   LD502                 PUT ++++ IN 28TH FIELD AND                 
         MVC   5(4,R2),=C'++++'      EXIT                                       
         B     LD503                                                            
*                                                                               
LD502    LA    R4,L'SVZCUTST(R4)     BUMP TO NEXT STATION                       
         CHI   R5,14                 WHEN 14 STATIONS REACHED ...               
         BE    LD503                 START SECOND ROW                           
*                                                                               
         OC    0(5,R4),0(R4)         WHEN NO MORE STATIONS LEFT ...             
         BNZ   LD501A                IF ANY WERE SELECTED THEN DONE             
*                                                                               
LD503    BAS   RE,GOBLDFLD           SEND ELEM TO GOBLDFLD                      
         BE    *+6                                                              
         DC    H'00'                                                            
         XC    ELEM,ELEM                                                        
*                                                                               
**********************************************************************          
*                                                                               
         LH    RE,BLDROW             WHEN DONE WITH A ROW ...                   
         LA    RE,1(RE)              BUMP TO NEXT ROW AND SET                   
         SLL   RE,16                 COLUMN TO 0                                
         ST    RE,BLDROW                                                        
         OC    0(5,R4),0(R4)         WHEN NO MORE STATIONS LEFT ...             
         BZ    LD505                 DONE                                       
         LA    R2,ELEM                                                          
         CHI   R5,14                                                            
         BE    LD501A                                                           
*                                                                               
**********************************************************************          
*                                                                               
LD505    LA    R4,SVZCUTDT                                                      
         SR    R0,R0                                                            
         MVI   BYTE,0                                                           
         SR    R5,R5                                                            
         ICM   R5,1,SVCUTLOW                                                    
         BNZ   *+8                                                              
         LA    R5,1                                                             
*                                                                               
LD506    XC    ELEM,ELEM                                                        
         MVC   BLDLIST(4),=X'02082003'                                          
         GOTO1 VDATCON,DMCB,(2,(R4)),(4,ELEM)                                   
         CHI   R5,1   '                                                         
         BNH   LD507                                                            
         MVI   ELEM+5,C'-'                                                      
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM+6(2),DUB                                                    
*                                                                               
LD507    BAS   RE,GOBLDFLD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    ELEM,ELEM                                                        
         AHI   R0,1                                                             
         LA    R5,1(R5)                                                         
*                                                                               
*========================================================*                      
* NOW DISPLAY ALLOCATIONS                                *                      
*========================================================*                      
*                                                                               
LD510    ST    R7,FULL                  SAVE LIST POINTER                       
         MVC   BLDLIST(4),=X'01070000'  START OUT UNPROT                        
         TM    0(R7),X'EF'              TEST ANY INDS ON                        
         BZ    LD512                    NO                                      
         MVC   BLDLIST(4),=X'02072001'  IF YES, PROTECT                         
*                                                                               
LD512    CLI   1(R7),0             TEST UNALL                                   
         BNE   LD514                                                            
         TM    5(R7),X'04'         TEST HIATUS                                  
         BZ    LD518                                                            
         MVC   ELEM(4),=C'HIAT'                                                 
         B     LD518                                                            
*                                                                               
LD514    LA    R7,1(R7)            POINT TO PRD                                 
         BAS   RE,GETCD                                                         
         MVC   ELEM(3),0(R1)                                                    
*                                                                               
         LA    R7,1(R7)            POINT TO PRD 2                               
         CLI   0(R7),0             TEST IT IS THERE                             
         BE    LD518               NO                                           
         LA    RF,ELEM+2                                                        
         CLI   0(RF),C' '          TEST LENGTH OF PRD 1                         
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         MVI   1(RF),C'-'                                                       
         BAS   RE,GETCD                                                         
         MVC   2(3,RF),0(R1)                                                    
*                                                                               
LD518    BAS   RE,GOBLDFLD                                                      
         BE    *+6                                                              
         DC    H'0'                IT IS DESIGNED TO FIT !                      
         XC    ELEM,ELEM                                                        
*                                                                               
         L     R7,FULL             RESTORE                                      
         LA    R7,6(R7)                                                         
*                                                                               
         CLC   0(2,R4),2(R4)                                                    
         BE    *+8                                                              
         LA    R5,1                                                             
         LA    R4,2(R4)            NEXT DATE                                    
         OC    0(2,R4),0(R4)       TEST ANY MORE IN LIST                        
         BZ    LD520               NO                                           
*                                                                               
         CHI   R0,3                                                             
         BNE   LD506                                                            
         LH    RE,BLDROW                                                        
         LA    RE,1(RE)                                                         
         SLL   RE,16                                                            
         ST    RE,BLDROW                                                        
         SR    R0,R0                                                            
         ZIC   RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         CLI   BYTE,6                                                           
         BE    LD520                                                            
         B     LD506                                                            
*                                                                               
LD520    B     LDX                                                              
         EJECT                                                                  
*                                                                               
* PROVIDE LINK TO BLDFLD WITH RETURN TO ORIGINAL CALLER                         
*                                                                               
GOBLDFLD NTR1                                                                   
         GOTO1 VBLDFLD                                                          
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
POLEL    NTR1                                                                   
*                                                                               
         TM    BLDLIST+10,X'20'    TEST ALLOC FIELD CHANGED TO PROT             
         BZ    POLEL1              NO                                           
         MVI   BLDLIST+10,0        RESTORE TO UNP                               
         IC    RE,BLDLIST+8        AND RESET LEADING/TRAILING SPACES            
         BCTR  RE,0                                                             
         STC   RE,BLDLIST+8                                                     
         IC    RE,BLDLIST+11                                                    
         BCTR  RE,0                                                             
         STC   RE,BLDLIST+11                                                    
*                                                                               
POLEL1   MVC   DSPAREA,SPACES                                                   
         LA    RE,DSPBIGDT+1                                                    
         MVI   0(RE),C'.'                                                       
         MVC   1(18,RE),0(RE)                                                   
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
*                                                                               
POLEL2   BAS   RE,NEXTEL                                                        
         BNE   POLELX                                                           
*                                                                               
         OC    STDTP(4),STDTP                                                   
         BZ    POLEL2A                                                          
         CLC   STDTP,2(R6)         FILTER ON REQUESTED DATES                    
         BH    POLEL2                                                           
         CLC   ENDDTP,2(R6)                                                     
         BL    POLEL2                                                           
*                                                                               
POLEL2A  CLC   ELEMDT,2(R6)                                                     
         BE    *+8                                                              
         MVI   ELEMNO,0                                                         
         MVC   ELEMDT,2(R6)                                                     
         IC    RE,ELEMNO                                                        
         TM    6(R6),X'80'         TEST MINUS SPOT                              
         BO    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,ELEMNO                                                        
*                                                                               
         L     R8,BLDLIST+4        GET DATE DISPLAY ADDRESS                     
*                                                                               
         CLC   STDTP,2(R6)                                                      
         BNE   *+14                                                             
         CLC   BUELEMNO,ELEMNO                                                  
         BH    POLEL2                                                           
*                                                                               
         MVI   0(R8),C' '                                                       
         CLI   0(R6),X'0B'                                                      
         BE    POLEL4                                                           
         MVI   0(R8),C'+'                                                       
         TM    6(R6),X'80'                                                      
         BZ    *+8                                                              
POLEL3   MVI   0(R8),C'-'                                                       
*                                                                               
POLEL4   GOTO1 VDATCON,DMCB,(2,2(R6)),(4,1(R8))                                 
*                                                                               
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BO    POLEL4A             YES                                          
         CLI   ELEMNO,1            NO-ONLY DISPLAY ELEM IF GT 1                 
         BE    POLEL4C                                                          
         B     *+12                                                             
POLEL4A  CLI   MAXNPW,1            FOR POL NPW ALWAYS DSPLY IF ANY              
         BNH   POLEL4B             WEEK HAS GT 1 ELEM                           
*                                                                               
         MVI   6(R8),C'-'                                                       
         ZIC   R0,ELEMNO                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(2,R8),DUB                                                      
         LA    R8,3(R8)                                                         
POLEL4B  TM    BDSTAT,X'80'                                                     
         BZ    POLEL4C                                                          
         MVI   6(R8),C'*'                                                       
         ZIC   R0,7(R6)            GET POL NPW                                  
         SRL   R0,2                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(2,R8),DUB                                                      
*                                                                               
POLEL4C  DS    0H                                                               
         TM    6(R6),X'20'         TEST O/R THIS ELEM                           
         BZ    POLEL5A                                                          
         SR    R0,R0                                                            
         ICM   R0,7,7(R6)                                                       
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    *+8                                                              
         N     R0,=X'0003FFFF'     DROP SPOTS                                   
         MVC   DSPCOST,SPACES                                                   
         LTR   R0,R0                                                            
         BNZ   *+14                                                             
         MVC   DSPCOST(2),=C'$0'                                                
         B     POLEL4X                                                          
         OC    SVNDEF(16),SVNDEF                                                
         BZ    POLEL4D                                                          
         LR    R1,R0               DISPLAY NET COST OVRD IN $                   
         M     R0,=F'2'                                                         
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LR    R0,R1                                                            
         B     POLEL4E                                                          
*                                                                               
POLEL4D  TM    BDSTAT,X'01'                                                     
         BO    POLEL4E                                                          
         EDIT  (R0),(9,DSPCOST),2,ALIGN=LEFT,FLOAT=$                            
         B     POLEL4X                                                          
*                                                                               
POLEL4E  EDIT  (R0),(9,DSPCOST),ALIGN=LEFT,FLOAT=$                              
*                                                                               
POLEL4X  LA    RE,DSPCOST                                                       
         AR    RE,R0                                                            
         AHI   RE,-3                                                            
         CLC   =C'.00',0(RE)                                                    
         BNE   *+10                                                             
         MVC   0(3,RE),SPACES                                                   
*                                                                               
         LR    RE,R6                                                            
POLEL4Y  SR    R0,R0                                                            
         ICM   R0,1,1(RE)                                                       
         AR    RE,R0               POINT TO NEXT ELEMENT                        
         CLI   0(RE),X'10'         TEST EXTENSION ELEM                          
         BL    POLEL5A                                                          
         CLI   0(RE),X'13'                                                      
         BL    POLEL4Y                                                          
         BH    POLEL5A                                                          
         TM    2(RE),X'80'         TEST COST1 OVERRIDE                          
         BZ    *+8                                                              
         MVI   DSPCOST,C'<'                                                     
         TM    2(RE),X'40'         TEST COST2 OVERRIDE                          
         BZ    *+8                                                              
         MVI   DSPCOST,C'>'                                                     
         SPACE 1                                                                
*                                                                               
POLEL5A  LA    R0,9                                                             
         LA    RE,DSPCOST+8                                                     
POLEL5B  CLI   0(RE),C' '                                                       
         BNE   POLEL6                                                           
         MVI   0(RE),C'.'                                                       
         BCTR  RE,0                                                             
         BCT   R0,POLEL5B                                                       
* PRODUCT CODE                                                                  
POLEL6   TM    6(R6),X'04'         TEST HIATUS                                  
         BZ    *+14                                                             
         MVC   DSPPRD(4),=C'HIAT'                                               
         B     POLEL12                                                          
*                                                                               
         BAS   RE,BPTEST                                                        
         TM    BYTE,X'C0'          TEST BILLED/PAID                             
         BNZ   *+12                                                             
         TM    6(R6),X'D0'         TEST MINUS/ED OR MKGD PENDING SPOT           
         BZ    POLEL7              NO                                           
*                                                                               
         MVI   BLDLIST+10,X'20'    SET ALLOCATION FIELD TO PROTECTED            
         IC    RE,BLDLIST+8        SET 1 MORE LEADING SPACE                     
         LA    RE,1(RE)                                                         
         STC   RE,BLDLIST+8                                                     
         IC    RE,BLDLIST+11       AND 1 MORE TRAILING SPACE                    
         LA    RE,1(RE)                                                         
         STC   RE,BLDLIST+11                                                    
         TM    6(R6),X'80'         TEST MINUS SPOT                              
         BZ    POLEL7                                                           
* FIND PRECEDING + SPOT TO SEE IF PREMPT OR MSSD                                
         ST    R6,FULL             R6 POINTS TO -ELEM                           
         LA    R6,BDELEM                                                        
*                                                                               
POLEL6A  BAS   RE,NEXTEL                                                        
         BNE   POLELERR                                                         
         C     R6,FULL                                                          
         BE    POLEL6B                                                          
         TM    6(R6),X'90'         TEST MINUS OR MAKEGOOD PENDING               
         BO    *+6                                                              
         LR    R5,R6                                                            
         B     POLEL6A                                                          
*                                                                               
POLEL6B  CLC   2(2,R5),2(R6)       TEST SAME DATE                               
         BE    *+6                                                              
         DC    H'0'                                                             
* R5 NOW POINTS TO PRECEDING +SPOT                                              
         MVC   DSPPRD(3),=C'(P)'                                                
         TM    6(R5),X'02'         TEST MAKEGOOD ON NEW LINE                    
         BZ    POLEL6X             NO - PREMPT                                  
         MVC   DSPPRD(3),=C'(M)'                                                
         CLI   13(R5),0            OLD STYLE MAKEGOOD                           
         BE    POLEL6X                                                          
         CLI   1(R5),10            TEST ALLOCATED                               
         BNH   POLEL6X                                                          
*                                                                               
         LR    R1,R5               POINT TO +ELEM                               
         BAS   RE,TRANSCD                                                       
*                                                                               
POLEL6D  MVI   DSPPRD,C'('                                                      
         MVC   DSPPRD+1(2),HALF                                                 
         MVI   DSPPRD+3,C')'                                                    
*                                                                               
POLEL6X  B     POLEL14                                                          
*                                                                               
POLEL7   LA    R7,10(R6)                                                        
         ZIC   R0,1(R6)                                                         
         AHI   R0,-10                                                           
         BNP   POLEL12                                                          
*                                                                               
         LA    R4,DSPPRD                                                        
         TM    6(R6),X'01'         TEST DO NOT REALLOCATE                       
         BZ    *+12                                                             
         MVI   0(R4),C'*'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         TM    6(R6),X'08'         TEST BRAND A PAYS ALL                        
         BZ    *+12                                                             
         MVI   0(R4),C'@'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         SRL   R0,2                                                             
         BAS   RE,GETCD                                                         
         MVC   0(3,R4),0(R1)                                                    
*                                                                               
         CLI   1(R6),14            TEST ONE ALLOCATION                          
         BNE   POLEL7A                                                          
         TM    SVCOPT3,COP3SPOD    TEST SPODS ALLOWED                           
         BZ    POLEL7A                                                          
         CLC   BDSEC,11(R6)        TEST SLN=BDSEC                               
         BE    POLEL7A                                                          
* DISPLAY SLN FOLLOWING PRD                                                     
         CLI   2(R4),C' '                                                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
         MVI   3(R4),C'-'                                                       
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(2,R4),DUB                                                      
         CHI   RE,99                                                            
         BNH   *+10                                                             
         UNPK  4(3,R4),DUB                                                      
         B     POLEL12                                                          
*                                                                               
POLEL7A  LA    R7,4(R7)            NEXT ALLOCATION                              
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         BNZ   *+16                                                             
         TM    6(R6),X'10'         MAKEGOOD PENDING                             
         BZ    POLEL12                                                          
         B     POLEL7B                                                          
* POL PIGGYBACK                                                                 
         CLI   2(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVI   3(R4),C'-'                                                       
         LA    R4,4(R4)                                                         
*                                                                               
         TM    6(R6),X'10'         MAKEGOOD PENDING                             
         BZ    POLEL8                                                           
*                                                                               
POLEL7B  LA    R4,DSPPRD                                                        
         TM    6(R6),X'01'         TEST DO NOT REALLOCATE                       
         BZ    *+12                                                             
         MVI   0(R4),C'*'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         MVI   0(R4),C'('                                                       
         MVC   1(3,R4),0(R1)                                                    
         CLI   3(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVI   4(R4),C')'                                                       
         CLI   1(R6),18            TEST SECOND PRODUCT                          
         BL    POLEL12             NO                                           
         MVI   5(R4),C'-'                                                       
         LA    R4,6(R4)                                                         
*                                                                               
POLEL8   DS    0H                                                               
         BAS   RE,GETCD                                                         
         MVC   0(3,R4),0(R1)                                                    
         LR    R0,R4               SAVE R4                                      
         CLI   2(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         LA    R4,3(R4)                                                         
*                                                                               
POLEL10  AHI   R7,-4               BACK UP TO PREVIOUS PRD                      
         CLC   1(1,R7),5(R7)       TEST EQUAL TIME SPLIT                        
         BE    POLEL12                                                          
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         ZIC   R0,1(R7)                                                         
         BAS   RE,POLSEC                                                        
         MVC   0(3,R4),WORK2                                                    
         AR    R4,R0                                                            
         MVI   0(R4),C'-'                                                       
         ZIC   R0,5(R7)                                                         
         BAS   RE,POLSEC                                                        
         MVC   1(3,R4),WORK2                                                    
         B     POLEL12                                                          
*                                                                               
POLEL12  DS    0H                                                               
*                                                                               
POLEL14  MVI   DSPPBIND,C'.'                                                    
         MVC   DSPPAYDT(3),=C'PD='                                              
*                                                                               
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BZ    POLEL20                                                          
         MVI   DSPPBIND,C'P'                                                    
         GOTO1 VDATCON,DMCB,(2,4(R6)),(8,DSPPAYDT+3)                            
*                                                                               
         CLI   RCLOPT,RCLPAYDT                                                  
         BNE   POLEL20                                                          
         CLI   T211FFD+1,C'*'      TEST DDS TERMINAL                            
         BO    POLEL18                                                          
         TM    T211FFD+12,X'20'    TEST NO CLEARANCE DATA ALLOWED               
         BO    *+8                                                              
POLEL18  BRAS  RE,GETCLST                                                       
*                                                                               
POLEL20  DS    0H                                                               
         CR    RB,RB               SET CC EQUAL FOR XIT                         
*                                                                               
POLELX   XIT1  REGS=(R6)                                                        
         SPACE 2                                                                
POLSEC   EDIT  (R0),(3,WORK2),ALIGN=LEFT                                        
         BR    RE                                                               
POLELERR XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(30),=C'CALL DDS - BUY ELEMENT SEQ ERR'                    
         MVI   ERRAREA,X'FF'       SET 'MESSAGE PRESENT'                        
         B     LDERR                                                            
         SPACE 2                                                                
GETCD    L     R1,ASVCLIST                                                      
GETCD2   CLC   0(1,R7),3(R1)                                                    
         BER   RE                                                               
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   GETCD2                                                           
         LA    R1,=C'***'                                                       
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        TRANSLATE CODE FROM X'01' - X'7F' TO A0 - Z9                           
*                                                                               
TRANSCD  NTR1                                                                   
         L     R5,ADBLOCK                                                       
         USING MGABLKD,R5                                                       
*                                                                               
         XC    0(MGALNQ,R5),0(R5)                                               
*                                                                               
         MVI   MGAACT,MGAQTRNS                                                  
         MVC   MGAACOM,VCOMFACS    SET A(COMFACS)                               
         LA    RE,BUYREC                                                        
         ST    RE,MGAIO                                                         
         ST    R1,MGAELEM                                                       
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000AC2'  BLDMGA                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),MGABLKD                                                     
*                                                                               
         CLI   MGAERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   HALF,MGQCODE                                                     
         J     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
REGEL    NTR1                                                                   
         XC    DSPAREA,DSPAREA                                                  
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
REGEL2   BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
*                                                                               
         OC    STDTP(4),STDTP                                                   
         BZ    REGEL3                                                           
         CLC   STDTP,2(R6)                                                      
         BH    REGEL2                                                           
         CLC   ENDDTP,2(R6)                                                     
         BL    REGEL2                                                           
*                                                                               
REGEL3   CLI   0(R6),6                                                          
         BE    REGEL4                                                           
         MVI   DSPDATE,C'+'                                                     
         TM    6(R6),X'80'                                                      
         BZ    *+8                                                              
         MVI   DSPDATE,C'-'                                                     
REGEL4   GOTO1 VDATCON,DMCB,(2,2(R6)),(4,DSPDATE+1)                             
*                                                                               
         MVI   DSPDATE+6,C'*'                                                   
         ZIC   R0,7(R6)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPDATE+7(2),DUB                                                 
*                                                                               
         MVI   DSPPBIND,C'.'                                                    
         MVC   DSPPAYDT(3),=C'PD='                                              
*                                                                               
         OC    4(2,R6),4(R6)                                                    
         BZ    REGEL8                                                           
         MVI   DSPPBIND,C'P'                                                    
         GOTO1 VDATCON,DMCB,(2,4(R6)),(8,DSPPAYDT+3)                            
*                                                                               
         CLI   RCLOPT,RCLPAYDT                                                  
         BNE   REGEL8                                                           
         CLI   T211FFD+1,C'*'      TEST DDS TERMINAL                            
         BO    REGEL6                                                           
         TM    T211FFD+12,X'20'    TEST NO CLEARANCE DATA ALLOWED               
         BO    *+8                                                              
REGEL6   BAS   RE,GETCLST                                                       
*                                                                               
REGEL8   DS    0H                                                               
*                                                                               
         CR    RB,RB               SET CC EQUAL FOR XIT                         
         XIT1  REGS=(R6)                                                        
         SPACE 2                                                                
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
FNDUF    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    FNDUFX                                                           
         TM    1(R2),X'20'                                                      
         BO    FNDUF                                                            
         CLI   0(R2),9                                                          
         BNH   FNDUF                                                            
         CR    RB,RB                                                            
         BR    RE                                                               
FNDUFX   LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
* TEST ELEM AT 0(R6) PAID                                                       
* SET BYTE = X'80' FOR PAID                                                     
*                                                                               
BPTEST   NTR1                                                                   
         MVI   BYTE,0                                                           
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BZ    *+8                                                              
         OI    BYTE,X'80'          SET PAID FLAG                                
BPTX     B     EXIT                                                             
         EJECT                                                                  
*=========================================================*                     
* EXTRACT CLEARANCE STATUS DATA FOR ELEMENT AT R6         *                     
*=========================================================*                     
         SPACE 1                                                                
GETCLST  NTR1  BASE=*,LABEL=*                                                   
         LA    R7,8(R6)            POINT TO NON-POL SEQNUM                      
         CLI   0(R6),X'0B'                                                      
         BL    *+8                                                              
         LA    R7,12(R6)           POINT TO POL SEQNUM                          
         CLI   0(R7),0             TEST FEATURE PRESENT                         
         BE    GETCLSX             NO - FORGET IT                               
* BUILD KEY OF FIRST STATUS RECORD                                              
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D76'                                                  
         MVC   KEY+2(3),SVKEY      A-M/CLT                                      
         MVC   KEY+5(5),SVKEY+4    MKT/STA                                      
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    GETCLS1                                                          
         CLI   KEY+7,X'E8'         IF THIS IS CABLE                             
         BL    *+8                                                              
         NI    KEY+9,X'80'         DROP NETWORK BITS                            
         B     GETCLS2                                                          
*                                                                               
GETCLS1  OC    SVNDEF,SVNDEF                                                    
         BZ    GETCLS2                                                          
         MVI   KEY+9,0             NETWORK REC HAS NO BITS                      
*                                                                               
GETCLS2  OC    BUMVDATE,BUMVDATE   TEST BUY WAS MOVED                           
         BZ    GETCLS4             NO                                           
         CLC   4(2,R6),BUMVDATE    TEST CLEARED BEFORE MOVE                     
         BH    GETCLS4             NO - AFTER                                   
         MVC   KEY+5(5),BUMVMKT    USE OLD MKT/STA                              
*                                                                               
GETCLS4  CLC   KEY(10),0(RE)       TEST HAVE ANY RECORD                         
         BE    GETCLS10            YES - PROCESS                                
         GOTO1 HIGH                NO - GET IT NOW                              
*                                                                               
GETCLS8  CLC   KEY(10),KEYSAVE     TEST SAME MKT/STA                            
         BNE   GETCLSX             NO - BIG PROBLEM HERE                        
         MVC   AREC,AREC4          USE REC4 AS IOAREA                           
         GOTO1 GETREC                                                           
*                                                                               
GETCLS10 DS    0H                                                               
         L     R1,AREC                                                          
         LA    R1,24(R1)                                                        
*                                                                               
GETCLS12 CLI   0(R1),0                                                          
         BNE   GETCLS14                                                         
* NOT IN THIS RECORD - TRY THE NEXT                                             
         L     R1,AREC                                                          
         MVC   KEY(13),0(R1)       MOVE KEY FROM RECORD                         
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         B     GETCLS8                                                          
*                                                                               
GETCLS14 CLI   0(R1),1                                                          
         BNE   GETCLS16                                                         
*                                                                               
         USING CLSTEL01,R1                                                      
         CLC   CLSTCLRD,4(R6)      MATCH CLEARANCE DATE                         
         BNE   GETCLS16                                                         
         CLC   CLSTCLSQ,0(R7)      MATCH SEQNUM                                 
         BE    GETCLS20                                                         
*                                                                               
GETCLS16 SR    R0,R0                                                            
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     GETCLS12                                                         
*                                                                               
GETCLS20 LA    R4,DSPCHECK         POINT TO OUTPUT AREA                         
         MVC   0(6,R4),=C'DIRECT'                                               
         OC    CLSTPYEE,CLSTPYEE                                                
         BZ    GETCLS22                                                         
         MVC   0(4,R4),=C'REP='                                                 
         CLI   CLSTREPT,C'P'                                                    
         BE    *+10                                                             
         MVC   0(4,R4),=C'SPR='                                                 
         MVC   4(3,R4),CLSTPYEE                                                 
*                                                                               
GETCLS22 OC    CLSTCHK,CLSTCHK                                                  
         BZ    GETCLSX                                                          
         LA    R4,DSPCHECK+8                                                    
         MVC   0(3,R4),=C'CK='                                                  
         MVC   3(6,R4),CLSTCHK                                                  
*                                                                               
         TM    CLSTSTAT,X'80'      TEST RECONCILED                              
         BZ    *+8                                                              
         MVI   10(R4),C'*'                                                      
*                                                                               
         MVC   HALF,CLSTCHDT       R1 POINTS TO ELEMENT, STUPID!                
         GOTO1 VDATCON,DMCB,(2,HALF),(8,11(R4))                                 
*                                                                               
GETCLSX  XIT1                                                                   
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================*                        
* BUILD SECOND DISPLAY LINE                            *                        
*======================================================*                        
         SPACE 1                                                                
BLDLINE2 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   DSPAREA,SPACES                                                   
         LA    R4,DSPAREA+2                                                     
*                                                                               
         CLI   BUYKEY+3,X'FF'                                                   
         BNE   BL10                                                             
         CLI   RCLOPT,C'N'                                                      
         BE    BL4                                                              
         CLI   RCLOPT,C'N'                                                      
         BE    BL4                                                              
         CLI   SVCPROF+0,C'0'                                                   
         BNE   *+12                                                             
BL4      TM    SVOPTS,X'01'                                                     
         BZ    BL10                                                             
*                                                                               
         MVC   0(2,R4),=C'M='                                                   
         LA    R4,2(R4)                                                         
         LA    R5,BDMASPRD                                                      
         BAS   RE,LDPRD                                                         
         LA    R4,2(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         LA    R5,BDMASPRD+1                                                    
         CLI   0(R5),0                                                          
         BNE   *+12                                                             
         LA    R4,1(R4)                                                         
         B     BL10                                                             
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
         BAS   RE,LDPRD                                                         
         LA    R4,4(R4)                                                         
*                                                                               
BL10     CLI   BDTIME,0            TEST PIGGYBACK                               
         BZ    BL20                                                             
* FIND PBELEM                                                                   
         MVI   ELCDLO,4                                                         
         MVI   ELCDHI,4                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(23,R4),=C'* PARTNER=XXX-001-030 *'                             
         MVC   10(3,R4),6(R6)      PARTNER CODE                                 
         ZIC   R0,3(R6)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  14(3,R4),DUB                                                     
         IC    R0,4(R6)            PRTNR SECS                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  18(3,R4),DUB                                                     
         LA    R4,25(R4)                                                        
*                                                                               
BL20     MVI   ELCDLO,X'71'        COS2 DOLLAR ELEMENT                          
         MVI   ELCDHI,X'71'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BE    BL20A                                                            
*                                                                               
         MVI   ELCDLO,X'73'        COS2 FACTOR ELEMENT                          
         MVI   ELCDHI,X'73'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BL22                                                             
         OC    2(4,R6),2(R6)                                                    
         BZ    BL22                                                             
*                                                                               
BL20A    MVC   0(5,R4),=C'COS2='                                                
         LA    R4,5(R4)                                                         
         ICM   R0,15,2(R6)                                                      
         CLI   0(R6),X'71'                                                      
         BNE   BL20D                                                            
         TM    BDCIND2,X'10'       TEST COST IS IN DOLLARS                      
         BZ    BL20B                                                            
         EDIT  (R0),(8,(R4)),0,ALIGN=LEFT,FLOAT=$,ZERO=NOBLANK                  
         B     BL20C                                                            
*                                                                               
BL20B    EDIT  (R0),(8,(R4)),2,ALIGN=LEFT,FLOAT=$,ZERO=NOBLANK                  
*                                                                               
BL20C    AR    R4,R0                                                            
         TM    BDCIND,X'01'        TEST COST NEGATIVE                           
         BZ    BL20F                                                            
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
         B     BL20F                                                            
*                                                                               
BL20D    EDIT  (R0),(8,(R4)),6,ALIGN=LEFT                                       
         AR    R4,R0                                                            
         AHI   R4,-4                                                            
         CLC   0(4,R4),=C'0000'                                                 
         BNE   BL20E                                                            
         XC    0(4,R4),0(R4)                                                    
         B     BL20F                                                            
*                                                                               
BL20E    AHI   R4,4                                                             
*                                                                               
BL20F    LA    R4,1(R4)                                                         
*                                                                               
BL22     MVI   ELCDLO,X'66'        LOOK FOR COMMENT ELEMENT                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BL24                                                             
         MVC   0(9,R4),=C'*COMMENT*'                                            
         LA    R4,10(R4)                                                        
*                                                                               
BL24     MVI   ELCDLO,X'67'        LOOK FOR ORBIT ELEMENT                       
         MVI   ELCDHI,X'67'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BL30                                                             
         MVC   0(7,R4),=C'*ORBIT*'                                              
         LA    R4,8(R4)                                                         
*                                                                               
BL30     OC    BDREP,BDREP                                                      
         BZ    BL32                                                             
         MVC   0(4,R4),=C'REP='                                                 
         LHI   RE,VRCPACK-BUYSAVE                                               
         AR    RE,RA                                                            
         L     RF,0(RE)                                                         
         GOTO1 (RF),DMCB,(C'U',BDREP),4(R4)                                     
         LA    R4,9(R4)                                                         
*                                                                               
BL32     MVI   ELCDLO,X'61'        LOOK FOR SPECIAL TRAFFIC ELEMENT             
         MVI   ELCDHI,X'61'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BL40                                                             
         MVC   0(4,R4),=C'TRF='                                                 
         USING MCLTELEM,R6                                                      
         GOTO1 VCALLOV,DMCB,0,X'D9000A15'  GET CLUNPK ADDRESS                   
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(SVCPROF+6,MCLTCODE),4(R4)                             
         LA    R4,8(R4)                                                         
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,1(R4)                                                         
*                                                                               
         CLI   MCLTPRD,0                                                        
         BNE   BL34A                                                            
         LA    R4,1(R4)                                                         
         B     BL40                                                             
*                                                                               
BL34A    MVI   0(R4),C'/'                                                       
         L     R1,ASVCLIST                                                      
*                                                                               
BL34B    CLC   MCLTPRD,3(R1)                                                    
         BE    BL34C                                                            
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   BL34B                                                            
         LA    R1,=C'***'                                                       
BL34C    MVC   1(3,R4),0(R1)                                                    
         LA    R4,4(R4)                                                         
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,2(R4)                                                         
         DROP  R6                                                               
* PST                                                                           
BL40     CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   BL42                                                             
         MVI   ELCDLO,X'6B'        SEARCH FOR PST ELEMENT                       
         MVI   ELCDHI,X'6B'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BL42                                                             
         BAS   RE,DISPPST          DISPLAY PST (SKIP TAX)                       
         B     BL46                                                             
* TAX                                                                           
BL42     OC    BDNTAX,BDNTAX                                                    
         BE    BL44                                                             
         MVC   0(4,R4),=C'TAX='                                                 
         SR    R0,R0                                                            
         ICM   R0,3,BDNTAX                                                      
         EDIT  (R0),(6,4(R4)),3,ALIGN=LEFT                                      
         AR    R4,R0                                                            
         LA    R4,5(R4)                                                         
         B     BL46                                                             
*                                                                               
BL44     CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   BL46                                                             
         CLI   BUYMD,C'N'          TEST NTWK                                    
         BNE   BL46                                                             
*                                                                               
         MVI   ELCDLO,X'69'        LOOK FOR TAX ELEM                            
         MVI   ELCDHI,X'69'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BL46                                                             
         MVC   0(4,R4),=C'TAX='                                                 
         L     R0,2(R6)                                                         
         EDIT  (R0),(7,4(R4)),2,ALIGN=LEFT,FLOAT=$                              
         AR    R4,R0                                                            
         LA    R4,6(R4)                                                         
*                                                                               
BL46     TM    BDSTAT2,X'03'       TEST ANY CFD INDS ON                         
         BZ    BL50                                                             
         MVC   0(4,R4),=C'CFD='                                                 
         IC    RE,BDSTAT2                                                       
         SLL   RE,30                                                            
         SRL   RE,30                                                            
         AR    RE,RE               X 2                                          
         LA    RE,CFDTAB(RE)                                                    
         MVC   4(2,R4),0(RE)                                                    
         LA    R4,7(R4)                                                         
         B     BL50                                                             
*                                                                               
CFDTAB   DC    C'UBCCCSCB'                                                      
*                                                                               
BL50     CLC   =C'TEST',BUYBU                                                   
         BNE   BL52                                                             
         MVC   0(3,R4),=C'DA='                                                  
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,KEY+14,3(R4),4,=C'TOG'                                 
         LA    R4,12(R4)                                                        
*                                                                               
         MVC   0(2,R4),=C'L='                                                   
         SR    R0,R0                                                            
         ICM   R0,3,BUYREC+13      GET LENGTH                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(4,R4),DUB                                                      
         LA    R4,7(R4)                                                         
*                                                                               
BL52     CLI   SVNRGN,C'*'                                                      
         BNE   BL54                                                             
         MVC   0(4,R4),=C'RGN='                                                 
         MVC   5(1,R4),BDNRGN                                                   
         LA    R4,7(R4)                                                         
*                                                                               
BL54     MVI   ELCDLO,X'70'        SEARCH FOR ID ELEMENT                        
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BL56                                                             
*                                                                               
         CLIY  SVB0PROF+9,C'Y'     TEST PURPOSE CODES REQ'D                     
         JE    BL54A                YES, GO DISPLAY IT                          
         CLIY  SVB0PROF+9,C'O'     TEST PURPOSE CODES OPT'L                     
         BNE   BL55A                NO, GO DISPLAY AS ID                        
*                                                                               
* FOR OPTIONAL PURPOSE CODES, READ&VALIDATE PURPOSE CODE                        
*                                                                               
         MVC   WORK(L'KEY),KEY     SAVE BUY KEY                                 
*                                                                               
K        USING PRPRECD,KEY         READ PURPOSE CODE RECORD                     
         XC    KEY,KEY                                                          
         MVI   K.PRPKTYP,PRPKTYPQ  X'0D19'|AGYALPH|MEDIA|PURPCODE               
         MVI   K.PRPKSUB,PRPKSUBQ                                               
         MVC   K.PRPKAGY,AGYALPHA                                               
         MVC   K.PRPKMED,BUYMD                                                  
         MVC   K.PRPCODE,3(R6)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST VALID PURPOSE CODE?                     
         BNE   BL55A                NO, GO DISPLAY AS ID                        
         DROP  K                                                                
*                                                                               
BL54A    MVC   0(4,R4),=C'PUR='    DISPLAY AS PURPOSE CODE                      
         MVC   4(6,R4),3(R6)                                                    
         B     BL55B                                                            
*                                                                               
BL55A    MVC   0(3,R4),=C'ID='     DISPLAY AS ID                                
         MVC   3(12,R4),3(R6)                                                   
*                                                                               
BL55B    CLIY  SVB0PROF+9,C'O'     DID WE RDHI FOR PURPOSE CODE REC?            
         BNE   BL55C                                                            
         MVC   KEY,WORK            YES, RESTORE BUY KEY                         
         GOTO1 HIGH                                                             
*                                                                               
BL55C    LA    R4,14(R4)           POINT TO LAST CHAR                           
         LA    R4,14(R4)           POINT TO LAST CHAR                           
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,2(R4)                                                         
*                                                                               
BL56     TM    BDSTAT2,X'40'       TEST CAN NET EXCEPTIONS OFF                  
         BZ    BL60                                                             
         MVC   0(4,R4),=C'XOFF'                                                 
         LA    R4,5(R4)                                                         
*                                                                               
BL60     CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   BL80                                                             
         MVI   ELCDLO,X'6A'        SEARCH FOR GST OVERRIDE ELEMENT              
         MVI   ELCDHI,X'6A'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BL64                                                             
         MVC   0(4,R4),=C'GST='                                                 
         MVC   4(1,R4),2(R6)       MOVE GST CODE                                
         LA    R4,6(R4)                                                         
*                                                                               
BL64     MVI   ELCDLO,XCHCODEQ     TEST EXCHANGE ELEMENT                        
         MVI   ELCDHI,XCHCODEQ                                                  
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BL70                                                             
         USING XCHELEM,R6                                                       
         OC    XCHRATE,XCHRATE     YES-TEST EXCHANGE RATE                       
         BZ    BL68                                                             
         CLI   XCHSTYP,C'C'        TEST CANADIAN STATION                        
         BNE   BL66                                                             
         CLI   XCHCTYP,C'C'        TEST CANADIAN CLIENT                         
         BNE   BL66                                                             
         CLI   XCHDTYP,C'C'        TEST CANADIAN DOLLARS                        
         BE    BL68                ALL CANADIAN - DO NOT DISPLAY                
*                                                                               
BL66     MVC   0(4,R4),=C'XCH='                                                 
         EDIT  (2,XCHRATE),(7,4(R4)),4,FLOAT=$                                  
         LA    R4,12(R4)                                                        
*                                                                               
BL68     OC    XCHC58,XCHC58       TEST C58 TAX                                 
         BZ    BL70                                                             
         MVC   0(4,R4),=C'C58='                                                 
         EDIT  (2,XCHC58),(6,4(R4)),2,TRAIL=C'%'                                
         LA    R4,11(R4)                                                        
*                                                                               
BL70     DS    0H                                                               
         DROP  R6                                                               
*                                                                               
BL80     LA    R6,BDELEM                                                        
         USING BDELEM,R6                                                        
         TM    BDSTAT2,X'80'       DAILY SKED                                   
         BNO   BL82                                                             
         MVC   0(5,R4),=C'DAILY'                                                
         LA    R4,6(R4)                                                         
         DROP  R6                                                               
BL82    DS     0H                                                               
*&&DO                                                                           
         MVI   ELCDLO,X'19'        SEARCH MAKEGOOD STATION                      
         MVI   ELCDHI,X'19'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BE    BL84                                                             
         MVI   ELCDLO,X'29'        SEARCH MAKEGOOD STATION                      
         MVI   ELCDHI,X'29'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BL86                                                             
*                                                                               
BL84     MVC   0(3,R4),=C'MG='                                                  
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),4(R6)                                                  
         GOTO1 STAPACK,DMCB,(C'U',WORK),WORK+10,(X'80',WORK+15)                 
         MVC   3(8,R4),WORK+15                                                  
         LA    R4,11(R4)           POINT TO LAST CHAR                           
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,2(R4)                                                         
*&&                                                                             
BL86     MVI   ELCDLO,X'95'        ID ELEMENT                                   
         MVI   ELCDHI,X'95'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BL88                                                             
         USING BUPELEM,R6                                                       
         OC    BUPUID,BUPUID                                                    
         BZ    BL88                                                             
         MVC   0(4,R4),=C'UID='                                                 
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R1,BUPUID                                                        
         LA    R0,8                                                             
*                                                                               
BL86A    CLI   0(R1),C' '                                                       
         BNH   BL86B                                                            
         MVC   0(1,R4),0(R1)                                                    
         LA    R4,1(R4)                                                         
*                                                                               
BL86B    LA    R1,1(R1)                                                         
         BCT   R0,BL86A                                                         
         LA    R4,1(R4)                                                         
*                                                                               
BL88     XIT1                                                                   
*                                                                               
LDPRD    L     R1,ASVCLIST                                                      
*                                                                               
LDPRD2   CLC   0(1,R5),3(R1)                                                    
         BE    LDPRD4                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   LDPRD2                                                           
         LA    R1,=C'***'                                                       
LDPRD4   MVC   0(3,R4),0(R1)                                                    
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        DISPLAY PST                                                            
*        R4 - START OF OUTPUT                                                   
*        R6 - PST ELEMENT                                                       
*                                                                               
DISPPST  NTR1                                                                   
         LA    R2,WORK                                                          
         USING PSTBLKD,R2                                                       
         XC    WORK,WORK           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,2(R6)                                                         
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    ELEM+100(64),ELEM+100                                            
         LA    R1,ELEM+100                                                      
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VCOMFACS    A(COMFACS)                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R2)                                                   
         MVC   0(4,R4),=C'PST '                                                 
         MVC   4(60,R4),ELEM+100                                                
         SR    R1,R1                                                            
         LA    R1,64(R4)                                                        
*                                                                               
DPST10   CLI   0(R1),C' '                                                       
         BH    DPST20                                                           
         BCTR  R1,0                                                             
         CR    R1,R4                                                            
         BNL   DPST10                                                           
*                                                                               
DPST20   LA    R4,2(R1)            SET R4 TO END OF STRING                      
*                                                                               
DPX      XIT1  REGS=(R4)                                                        
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
CHKINPER NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VDATCON,DMCB,(3,BDSTART),(2,FULL)                                
         GOTO1 (RF),(R1),(3,BDEND),(2,FULL+2)                                   
*                                                                               
         MVI   ELCDLO,11           CHECK REGELS ONLY                            
         CLI   BUYKEY+3,X'FF'                                                   
         BE    *+8                                                              
         MVI   ELCDLO,6                                                         
         MVC   ELCDHI,ELCDLO                                                    
         LA    R6,BDELEM                                                        
*                                                                               
CHKIN2   BRAS  RE,NEXTEL                                                        
         BNE   CHKINX                                                           
         CLC   2(2,R6),FULL        TEST PRIOR TO BDSTART                        
         BL    CHKINERR                                                         
         CLC   2(2,R6),FULL+2      TEST AFTER BDEND                             
         BH    CHKINERR                                                         
         B     CHKIN2                                                           
*                                                                               
CHKINERR MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(SOFBDP)   TELL USER SPOTS OUTSIDE BUY PERIOD           
         GOTO1 ERROR                                                            
*                                                                               
CHKINX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* PFKEYS ARE DONE AS ++INCLUDE SO CAN HAVE LOWERCASE WITHOUT AGONY              
       ++INCLUDE SPBUY20PFK                                                     
         EJECT                                                                  
       ++INCLUDE SPGENCLRST                                                     
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE SPMGADN                                                        
       ++INCLUDE SPGENPURP                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057SPBUY20   08/18/20'                                      
         END                                                                    
