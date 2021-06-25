*          DATA SET SPSDE10    AT LEVEL 013 AS OF 10/10/11                      
*PHASE T23110B                                                                  
*INCLUDE DPTRD                                                                  
*INCLUDE EQVRD                                                                  
T23110   TITLE 'SPSDE10 - SUPERDESK - GVP, INVOICES, AND % PAID'                
T23110   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSDE10*,R7,RR=R5                                              
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         ST    R5,OVRELO                                                        
*                                                                               
         USING TSARD,TSARBLK                                                    
*                                                                               
         MVI   OVFLAG1,0                                                        
         BAS   RE,FMTPRDS                FORMAT QPRD-QPRD2                      
*                                                                               
         CLI   SVRCVEL+1,X'20'           MIS SCREENS                            
         BE    RCV20H                                                           
*                                                                               
         CLI   SVRCVEL+1,X'21'           INVOICE INFORMATION SCREEN             
         BE    RCV21H                                                           
*                                                                               
         CLI   SVREASON,GLOBPAID         TEST RETURN FROM FIS FOR %PAID         
         BE    SND22H                                                           
*                                                                               
         CLI   SVRCVEL+1,X'22'           CALL FIS TO GET % PAID                 
         BE    RCV22H                                                           
*                                                                               
         CLI   SVREASON,GLOBGVP          TEST RETURN FROM MIS                   
         BE    SND20H                                                           
         B     EXIT                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* FORMAT PRODUCTS AS QPRD-QPRD2 IN FMTPRD                                       
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
FMTPRDS  MVC   FMTPRD,SPACES                                                    
         LA    R5,FMTPRD                 STORE QPRD-QPRD2 INTO FMTPRD           
         MVC   0(3,R5),QPRD              R5 = LENGTH                            
         CLI   2(R5),X'40'               IS THE LEN OF THE PRD CODE 2?          
         BH    *+6                                                              
         BCTR  R5,0                      IF LENGTH IS 2, (-) 1, THEN            
         AHI   R5,3                      ADD 3 FOR LENGTH OF PRD                
         OC    QPRD2,QPRD2                                                      
         BZ    FMTPRDSX                                                         
         MVI   0(R5),C'-'                                                       
         MVC   1(3,R5),QPRD2                                                    
FMTPRDSX BR    RE                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 20 HEADER: GLOBBER CALL TO MIS WITH OPTIONS AND FORMAT                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV20H   DS    0H                                                               
         MVI   SVXFROV,X'10'             RETURN CONTROL TO THIS OVERLAY         
         GOTO1 VGLOBBER,DMCB,=C'PUTD',QMED,1,GLVSPMD    MEDIA                   
         GOTO1 (RF),(R1),=C'PUTD',QCLT,3,GLVSPCLT       CLIENT                  
         GOTO1 (RF),(R1),=C'PUTD',FMTPRD,7,GLVSPPRD     PRODUCT                 
         GOTO1 (RF),(R1),=C'PUTD',QEST,3,GLVSPEST       ESTIMATE                
         GOTO1 (RF),(R1),=C'PUTD',QMKT,4,GLVSPMKT       MARKET                  
*                                                                               
         MVC   DUB(5),QSTA                                                      
         OC    QSTA,QSTA                                                        
         BNZ   R20H10                                                           
         MVC   DUB(5),=CL5'ALL'           STATION IS 'ALL'                      
         CLI   SVFMT,C'S'                 UNLESS IN STATION MODE                
         BNE   R20H10                                                           
         MVC   DUB(5),=CL5'LIST'          THEN LIST                             
R20H10   GOTO1 (RF),(R1),=C'PUTD',DUB,5,GLVSPSTA                                
*                                                                               
         CLI   SVTRANT,C'L'        STATION LOCKIN                               
         BNE   *+14                                                             
         MVC   DUB(5),=C'SLOCK'                                                 
         B     R20H20                                                           
         CLI   SVTRANT,C'M'        MARKET LOCKIN                                
         BNE   R20H30                                                           
         MVC   DUB(5),=C'MLOCK'                                                 
*                                                                               
** NOTE ** MLOCK & SLOCK ARE BOTH LENGTH 5 SO THIS WORKS                        
R20H20   GOTO1 (RF),(R1),=C'PUTD',DUB,5,GLVSPOPT                                
*                                                                               
R20H30   MVC   DUB(5),=CL5'GVP'          DAYPART ANALYSIS IS DEFAULT            
         CLI   SVFMT,C'W'                WEEKLY?                                
         BNE   *+10                                                             
         MVC   DUB+3(2),=C'-W'                                                  
         GOTO1 (RF),(R1),=C'PUTD',DUB,5,GLVSPFMT                                
*                                                                               
         GOTO1 VDATCON,(R1),(3,SVSTADT),(5,WORK)                                
         MVI   WORK+8,C'-'                                                      
         GOTO1 (RF),(R1),(3,SVENDDT),(5,WORK+9)                                 
         GOTO1 VGLOBBER,(R1),=C'PUTD',WORK,17,GLVSPPER                          
*                                                                               
         XC    BLOCK,BLOCK               TRANSFER CONTROL BLOCK                 
         LA    R5,BLOCK                                                         
         USING GLVXFRSY,R5                                                      
         MVC   GLVXFRSY,=C'SPO'          FROM THE SPOT SYSTEM                   
         MVC   GLVXFRPR,=C'SDE'          SUPERDESK PROGRAM                      
         MVC   GLVXTOSY,=C'SPO'          TO THE SPOT SYSTEM                     
         MVC   GLVXTOPR,=C'MIS'          MIS PROGRAM                            
         DROP  R5                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',BLOCK,24,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SVREASON,GLOBGVP          SET REASON FOR RETURN                  
         B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND 20 HEADER - PROCESS DATA IN TWA AND SEND TO PC                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND20H   DS    0H                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',BLOCK,60,GLVSMSG                          
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         XC    DMCB(16),DMCB                                                    
         L     R3,ATIA                   INPUT AREA                             
         MVC   DMCB+10(2),TWATRM         TERM NO.                               
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(TWASIZQ)    TWA PAGE SIZE                          
         MVI   DMCB+8,1                  PAGE ONE                               
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,(R3)                       
         LA    R1,X'0020'                                                       
         BAS   RE,SENDH                                                         
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
* GET EQUIVALENCE HEADER                                                        
         MVC   DUB(2),QAGY               AGENCY CODE                            
         MVC   DUB+2(1),QMED             MEDIA                                  
         MVC   DUB+3(2),BCLT             CLIENT                                 
         L     R5,VDATAMGR                                                      
*                                                                               
         GOTO1 =V(EQVRD),DMCB,DUB,EQUDPT,EQUSECT1,(R5),RR=OVRELO                
         CLI   DMCB+12,0                 ERROR ?                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   VERSION,=X'02000001'      LOWER THAN VER 2.0.0.1 ONLY            
         BL    S20H17                    DO OLD WAY                             
*                                                                               
         LA    R6,15                     15 ENTRIES                             
         LA    R5,EQUSECT1               SPOT LEN-VALUE|SPOT LEN-VALUE|         
         LA    R2,SLNTAB                                                        
S20H16   ZIC   R0,0(R2)                  SET SPOT LEN                           
         BRAS  RE,SETNUMCM               DELIMIT WITH C','                      
         LA    R2,1(R2)                  NEXT SLN                               
         LH    R0,0(R5)                  SET EQV VALUE                          
         BRAS  RE,SETNUM                 DELIMIT WITH |                         
         LA    R5,4(R5)                  NEXT EQV                               
         BCT   R6,S20H16                                                        
         B     S20H19                    SEND IT                                
*                                                                               
S20H17   LA    R6,15                     15 ENTRIES, 2 FOR EACH SPOTLEN         
         LA    R5,EQUSECT1               1ST ENTRY FOR SINGLE PRD, 2ND          
S20H18   LH    R0,0(R5)                  ENTRY FOR PB PRD                       
         BRAS  RE,SETNUM                 SPOT LENS:10,15,20,30,40,45,           
         LA    R5,4(R5)                  50,60,90,120,27,3,75,5,SPARE           
         BCT   R6,S20H18                                                        
*                                                                               
S20H19   LA    R1,MCEQVRD                SEND EQUVALENCY RECORD DATA            
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
         EJECT                                                                  
*                                                                               
         CLC   VERSION,=X'01010002'      LOWER THAN VER 1.1.0.2 ONLY            
         BNL   S20H15                                                           
         BAS   RE,DEMOCONV               CONVERT DEMO                           
         LA    R1,MCDEMO                                                        
         LA    R4,WORK+1                                                        
         ZIC   R5,WORK                                                          
         BRAS  RE,SENDD                                                         
*                                                                               
S20H15   L     R3,ATIA                                                          
         LA    R3,8(R3)                  SKIP FIRST 8 BYTES                     
         USING MISBUCKS,R3                                                      
         CLI   BUCCODE,0                                                        
         BNE   S20H20                                                           
         MVC   ERROR,=Y(NODATA)          NO DATA TO REPORT                      
         GOTO1 SENDMSG                                                          
*                                                                               
S20H20   XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         BAS   RE,DPTMENU                GET DAYPART MENU                       
         LA    R2,BUCDPTCD                                                      
         GOTO1 DPTEXP,DMCB,0(R2),FULL    DPT EXPANSION                          
         MVC   SVDPART,FULL              SAVE DAYPART                           
*                                                                               
         CLI   SVFMT,C'S'                FORMAT = STATION?                      
         BNE   S20H40                                                           
         MVC   WORK(8),=CL8'GOALS'                                              
*                                                                               
         CLC   BUCPRD,=X'000001'         GOALS FOR THE MARKET?                  
         BE    S20H30                                                           
         MVC   BSTA,BUCPRD               NO, GET STATION                        
         BAS   RE,STAUNPK                GET STATION CALL LETTERS               
S20H30   MVC   0(8,R4),WORK              SEND STATION CALL LETTERS              
         LA    R4,7(R4)                                                         
         BRAS  RE,SETDLM                                                        
         CLC   =C'GOALS',WORK            IF STATION ANALYSIS, NO GOALS          
         BE    S20H90                    EXCEPT FOR GOALS LINE                  
         B     S20H80                                                           
*                                                                               
S20H40   CLI   SVFMT,C'W'                FORMAT = WEEK?                         
         BNE   S20H50                                                           
         CLC   VERSION,=X'02000027'      VER 2.0.0.39 OR LOWER                  
         BNH   S20H44                    DO OLD WAY                             
         GOTO1 VDATCON,DMCB,(2,BUCDATE),(11,WORK)                               
         MVC   0(8,R4),WORK              DATE=MMMDD/YY                          
         LA    R4,7(R4)                                                         
         B     S20H46                                                           
S20H44   GOTO1 VDATCON,DMCB,(2,BUCDATE),(4,WORK)                                
         MVC   0(5,R4),WORK              DATE=MMMDD                             
         LA    R4,4(R4)                                                         
S20H46   BRAS  RE,SETDLM                                                        
         B     S20H70                                                           
*                                                                               
S20H50   CLC   QPRD,=C'POL'                                                     
         BNE   S20H60                                                           
         CLC   BUCPRD,=C'ZZZ'            IF POL PRD, ONLY PRINT LINES           
         BNE   S20H130                   WITH PRODUCT CODE 'ZZZ'                
S20H60   MVC   0(3,R4),SVDPART           DAYPART                                
         MVI   3(R4),C'-'                                                       
**       EDIT  BUCLEN,HALF                                                      
         EDIT  BUCLEN,(3,FULL),ALIGN=LEFT                                       
         AHI   R0,-1                                                            
         BM    S20H69                                                           
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R4),FULL              COMMERCIAL LENGTH                      
S20H69   LA    R4,6(R4)                                                         
         BRAS  RE,SETDLM                                                        
*                                                                               
S20H70   OC    QSTA,QSTA                 IF STATION SENT, THEN NO GOALS         
         BZ    S20H90                                                           
S20H80   BRAS  RE,SETDLM                                                        
         BRAS  RE,SETDLM                                                        
         B     S20H100                                                          
*                                                                               
S20H90   L     R0,BUCGPTS                GOAL POINTS                            
         BRAS  RE,SETNUM                                                        
*                                                                               
         L     R0,BUCGDOLS               GOAL DOLLARS                           
         BRAS  RE,SETNUM                                                        
*                                                                               
S20H100  L     R0,BUCPPTS                PURCHASE POINTS                        
         TM    BUCPPTS,X'40'       IN 2 DECIMAL FORMAT                          
         BNO   S20H108                                                          
         XR    RE,RE                                                            
         L     RF,BUCPPTS                                                       
         N     RF,=X'3FFFFFFF'     TURN OFF 40                                  
         M     RE,=F'2'            AND DIVIDE BY 10 W/ROUNDING                  
         D     RE,=F'10'                                                        
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         LR    R0,RF                                                            
S20H108  BRAS  RE,SETNUM                                                        
*                                                                               
         L     R0,BUCPDOLS               PURCHASE DOLLARS                       
         BRAS  RE,SETNUM                                                        
*                                                                               
         L     R0,BUCPSPTS               PURCHASE SPOTS                         
         BRAS  RE,SETNUM                                                        
*                                                                               
         L     R0,BUCLSPTS               LOCKIN SPOTS                           
         BRAS  RE,SETNUM                                                        
*                                                                               
* IF WEEKLY ANALYSIS, THE DAYPARTS FOR PURCHASED DATA IS NOT AVAILABLE,         
* SO SEND DAYPART=ALL & LEN=* INSTEAD.                                          
*                                                                               
         B     S20H110                   <---                                   
*                                                                               
* WE ARE TRYING TO NOW USE THE ZZZ/LEN ITEMS FOR WEEKLY                         
* SO THIS IS NOW COMMENTED OUT - FEB18/04                                       
*                                                                               
***      CLI   SVFMT,C'W'                WEEKLY?                                
***      BNE   S20H110                                                          
***      CLC   SVDPART,=C'ZZZ'           IF NOT ZZZ, THEN NOT A PROBLEM         
***      BNE   S20H110                                                          
***      MVC   0(5,R4),=C'*|ALL'                                                
***      LA    R4,4(R4)                                                         
***      BRAS  RE,SETDLM                                                        
***      B     S20H120                                                          
*                                                                               
S20H110  ZIC   R0,BUCLEN                 SPOT LENGTH                            
         BRAS  RE,SETNUM                                                        
*                                                                               
         MVC   0(3,R4),SVDPART           DAYPART                                
         LA    R4,2(R4)                                                         
         BRAS  RE,SETDLM                                                        
*                                                                               
S20H120  LA    R1,MCMIS                  SEND MIS DATA ELEMENT                  
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
S20H130  ZIC   RE,BUCELLEN                                                      
         AR    R3,RE                                                            
         CLI   BUCCODE,0                                                        
         BNE   S20H20                                                           
*                                                                               
SND20HX  B     EXIT                                                             
*        DROP  R3                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* INTERFACE TO STAPACK TO GET STATION CALL LETTERS                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
STAUNPK  NTR1                                                                   
         MVI   XSP,C'N'                                                         
         XC    WORK2,WORK2                                                      
         LA    R4,WORK2                                                         
         USING STAPACKD,R4                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,QAGY                                                     
         MVC   STAPMED,QMED                                                     
*                                                                               
         MVI   STAPCTRY,C'U'                                                    
         CLI   SVAPROF+7,C'C'            TEST CANADA                            
         BNE   *+8                                                              
         MVI   STAPCTRY,C'C'                                                    
*                                                                               
         MVC   STAPMKST,BMKTSTA          MOVE MARKET/STATION                    
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(5),STAPQSTA                                                 
         MVC   WORK+5(3),STAPQNET                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        DEMOCON                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
DEMOCONV NTR1                                                                   
         L     R4,AIO2                                                          
         LR    R0,R4                                                            
         L     R1,=A(LENIO)                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R5,AIO3                                                          
         LR    R0,R5                                                            
         L     R1,=A(LENIO)                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING DBLOCK,R4                 SET UP CALL TO DEMOCON                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '            SET DBFILE = NAD FOR NETWORK           
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'            SET DBSELMED = C IF CANADIAN           
         BNE   DCONV10                   AGENCY USING US DEMOS                  
         CLI   SVCXTRA,C'U'              SET DBSELMED = R OTHERWISE             
         BE    DCONV10                                                          
         MVI   DBSELMED,C'C'                                                    
DCONV10  MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 VCALLOV,DMCB              CALL DEMOCON                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(20,SVEDEMOS),(13,(R5)),(C'S',(R4)),SVEUSRNM           
         DROP  R4                                                               
*                                                                               
         L     RF,AIO3                                                          
         BAS   RE,FMTDEMO                                                       
         B     EXIT                                                             
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* FORMAT DEMO                                                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
FMTDEMO  NTR1                                                                   
         USING ESTHDR,R3                                                        
         MVC   WORK(11),SPACES           INITIALIZE WORK                        
         MVI   WORK,0                                                           
         CLI   0(RF),C' '                IF NO DEMO TO FORMAT ... EXIT          
         BNH   FMTDEMOX                                                         
*                                                                               
         LA    R1,11                                                            
         LA    R4,10(RF)                                                        
FMTD5    CLI   0(R4),C' '                SCAN BACKWARDS FOR NON-SPACE           
         BH    FMTD10                                                           
         BCTR  R4,0                                                             
         BCT   R1,FMTD5                                                         
*                                                                               
FMTD10   STC   R1,WORK                   LENGTH OF DEMO INTO WORK               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),0(RF)           DEMO DESCRIPTION INTO WORK+1           
*                                                                               
         CLC   WORK+1(7),SVEWGTNM                                               
         BNE   FMTDEMOX                                                         
         MVC   WORK+10(7),WORK+1         IF DEMO MATCHES WEIGHTED DEMO          
         MVC   WORK+1(2),=C'W/'          INSERT HEADER                          
         MVC   WORK+3(7),WORK+10                                                
         IC    R1,WORK                   UPDATE LENGTH                          
         AHI   R1,2                                                             
         STC   R1,WORK                                                          
*                                                                               
FMTDEMOX B     EXIT                                                             
         DROP  R3                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        GET DAYPART MENU                                                       
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
DPTMENU  NTR1                                                                   
         MVC   DMCB(2),QAGY              GET DAYPART MENU                       
         MVC   DMCB+2(1),QMED                                                   
         MVC   DMCB+3(1),SVEDAYMN        DAYPART MENU NUMBER                    
         L     R4,VDATAMGR                                                      
         GOTO1 =V(DPTRD),DMCB,,SAVMENU,(R4),RR=OVRELO                           
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        DPTEXP - DAYPART EXPANSION                                             
*            P1=A(DAYPART CODE - 1 BYTE)                                        
*            P2=A(3-BYTE OUTPUT AREA)                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
DPTEXP   NTR1                                                                   
         L     R2,0(R1)                  P1                                     
         L     R3,4(R1)                  P2                                     
*                                                                               
         LA    R5,SAVMENU                DAYPART MENU                           
         LA    RE,5                      ENTRY LENGTH                           
         LA    RF,SAVMENU+L'SAVMENU-1                                           
         MVC   0(3,R3),=C'ZZZ'           SLUSH                                  
*                                                                               
         CLI   0(R5),0                   LAST ENTRY?                            
         BE    DPTEXPX                                                          
         CLC   1(1,R5),0(R2)             SAME CODE?                             
         BE    *+12                                                             
         BXLE  R5,RE,*-18                                                       
         B     DPTEXPX                                                          
*                                                                               
         MVC   0(3,R3),2(R5)             RETURN 3-BYTE CODE                     
DPTEXPX  B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 21 HEADER: INVOICE INFORMATION SCREEN                                 
* SEND TO TSAR A LIST OF MM PASSIVES WITH INVOICE INFORMATION                   
*              AND LIST OF ALL STATION LEVEL AUTHORIZATIONS                     
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV21H   DS    0H                                                               
* BUILD TSAR BUFFER OF STATIONS                                                 
         MVI   TSACTN,TSAINI                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,HKEYX-HKEY                                                
         MVI   TSPAGN,8                  REQUEST 8 PAGES                        
         OI    TSINDS,TSINODSK           TEMPEST IS IN USE BY FALINK !!         
         OI    TSINDS,TSIXTTWA           AND IT HAS BIG PAGES !                 
         LHI   R0,HRECX-HREC                                                    
         STH   R0,TSRECL                                                        
         LA    R1,HREC                                                          
         ST    R1,TSAREC                                                        
         BRAS  RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING AUTRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY,SVAUKEY                                                      
         MVC   AUTKMKT,BMKT                                                     
         MVI   AUTKSTA+2,1         GET FIRST STATION                            
         GOTO1 HIGH                                                             
R21H10   CLC   KEY(AUTKSTA-AUTRECD),KEYSAVE                                     
         BNE   R21H20                                                           
         MVC   SVAUKEY,KEY                                                      
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVC   HSTA,AUTKSTA        STATION                                      
*                                                                               
         MVI   TSACTN,TSAADD                                                    
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC                                                        
         BRAS  RE,CALLTSAR                                                      
         GOTO1 SEQ                                                              
         B     R21H10                                                           
*                                                                               
         USING SNVKEYD,KEY                                                      
R21H20   LA    R6,KEY                                                           
         XC    KEY,KEY             SET TO READ SNV PASSIVE KEYS                 
         MVI   SNVPTYP,X'0E'                                                    
         MVI   SNVPSUB,X'83'                                                    
         MVC   SNVPAM,BAGYMD                                                    
         MVC   SNVPMKT,BMKT                                                     
         XC    SNVPSTA(27),SNVPSTA  CLEAR THE REST                              
         MVI   XSP,C'Y'             SET TO READ XSPDIR                          
         GOTO1 HIGH                                                             
*                                                                               
R21H30   CLC   SNVPKEY(SNVPSTA-SNVPKEY),KEYSAVE  SAME A/M,MKT?                  
         BNE   R21H80                                                           
*                                                                               
         MVC   HALF,SNVPMOS              CHECK IF MOS OVERLAPS AUTH             
         XC    HALF,=X'FFFF'                                                    
         GOTO1 VDATCON,DMCB,(2,HALF),(3,SVINVDT)                                
         CLC   SVINVDT(2),SVSTADT        AUTHORIZATION START DATE               
         BL    R21H70                                                           
         CLC   SVINVDT(2),SVENDDT        AUTHORIZATION END DATE                 
         BH    R21H70                                                           
*                                                                               
         CLC   SNVPCLT,BCLT              CLIENT                                 
         BNE   R21H70                                                           
*                                                                               
         CLC   QPRD,=C'POL'              IF AUTH IS PRD "POL"                   
         BE    R21H50                    THEN SHOW ALL PRODUCTS                 
         CLC   SNVPPRD,QPRD              SAME PRODUCT                           
         BE    R21H40                                                           
         CLC   SNVPPRD,=C'POL'           OR PRODUCT "POL"' ???                  
         BNE   R21H70                                                           
*                                                                               
R21H40   CLC   SNVPPRD2,QPRD2            SAME PIGGYBACK?                        
         BNE   R21H70                                                           
*                                                                               
R21H50   CLI   SNVPEST,0                 ESTIMATE "NO"?                         
         BE    R21H60                                                           
         CLC   SNVPEST,BEST              SAME ESTIMATE?                         
         BE    R21H60                                                           
         BH    R21H70                                                           
         CLI   SNVPEST2,0                IS THERE AN ESTIMATE RANGE?            
         BE    R21H70                                                           
         CLC   SNVPEST2,BEST             FALLS IN RANGE OF ESTIMATES?           
         BL    R21H70                                                           
*                                                                               
R21H60   BAS   RE,SNDTSAR                SEND THIS INVOICE TO TSAR              
*                                                                               
R21H70   MVI   XSP,C'Y'                  SET TO READ XSPDIR                     
         GOTO1 SEQ                                                              
         B     R21H30                                                           
*                                                                               
R21H80   BAS   RE,SND21H                 SEND INVOICES TO FALINK                
         B     EXIT                                                             
********************************************************************            
* ADD MATCHMAKER CODE                                                           
********************************************************************            
*&&DO                                                                           
*****    CLC   KEY+3(2),KEYSAVE+3  SAME MKT                                     
*****    BNE   S22                 NO - NEXT MARKET                             
*                                                                               
******   CLI   SVAPROF+7,C'C'      TEST CANADA                                  
******   BE    S35X                THERE IS NO CABLE                            
****     TM    K.SNVPSTA,X'F0'     TEST CABLE STATION                           
*****    BO    S35                 YES                                          
* THIS IS NOT A CABLE STATION                                                   
******   TM    SVBYROP1,BYROPT1_CBL THIS A CABLE BUYER                          
******   BZ    S35X                 NO - DO ALL NON-CABLE STATIONS              
*****    B     S30                  ELSE SKIP NON-CABLE STATIONS                
*                                                                               
S35      CLI   PROFMK+1,C'Y'       ALL BUYERS DO CABLE                          
         BE    S35X                YES                                          
* ONLY DESIGNATED BUYERS DO CABLE                                               
         TM    SVBYROP1,BYROPT1_CBL                                             
         BZ    S30                                                              
*                                                                               
         CLI   PROFMK+2,C'Y'       TEST EVERYONE DOES UNWIRED NTWK              
         BE    S40                 THEY ALL DO IT                               
         TM    K.SNVDSTAT+1,X'20'  TEST UNWIRED NETWORK BUY                     
         BO    S38                 YES                                          
*                                                                               
         TM    SVBYROP1,BYROPT1_UNW  NO - IS THIS THE UNWIRED DUDE              
         BO    S30                   YES -SKIP                                  
         B     S40                                                              
*                                                                               
S38      TM    SVBYROP1,BYROPT1_UNW  IS THIS THE UNWIRED DUDE                   
         BZ    S30                   NO - SKIP                                  
*                                                                               
         BAS   RE,STAUNPK                                                       
         CLI   WORK,X'E8'          TEST CABLE STATION                           
         BL    S55                  NO                                          
         CLC   WORK+5(3),SPACES    IS THERE A NETWORK?                          
         BNH   S55                  NO                                          
         MVI   WORK+4,C'/'                                                      
*                                                                               
S55      LA    R1,H03STA                                                        
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R1,H03ESRNG                                                      
         XC    WORK,WORK                                                        
         SR    R0,R0                                                            
         ICM   R0,1,HEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         ICM   R0,1,HEST2                                                       
         BZ    S56                                                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   WORK+3,C'-'                                                      
         UNPK  WORK+4(3),DUB                                                    
*                                                                               
S56      LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
* READ FOR POL ESTHDR AND SET POL/NON-POL FLAG                                  
         XC    WORK,WORK                                                        
         USING H03FLAGD,R4                                                      
*                                                                               
         MVI   H03F_POL,C'Y'       SET DEFAULT TO POL                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),HCLT       MOVE PACKED CLIENT CODE                      
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),HEST       SET LOW EST NUMBER                           
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      TEST POL EST FOUND                           
         BE    *+8                 YES                                          
         MVI   H03F_POL,C'N'                                                    
*                                                                               
         MVI   H03F_STAT,C'M'      SET MATCHED                                  
         TM    HSTAT,X'80'                                                      
         BO    S58                                                              
         MVI   H03F_STAT,C'W'      SET WIP                                      
         TM    HSTAT,X'40'                                                      
         BO    S58                                                              
         MVI   H03F_STAT,C' '                                                   
*                                                                               
S58      LA    R1,H03FLAGS                                                      
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
         B     S51                                                              
         DROP  K                                                                
*&&                                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND INVOICE TO TSAR                                                *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SNDTSAR  NTR1                                                                   
* BUILD TSAR RECORD                                                             
         XC    TSARREC,TSARREC                                                  
         MVC   HSTA,SNVPSTA        STATION                                      
         MVC   HPRD,SNVPPRD        PRODUCT                                      
         MVC   HPRD2,SNVPPRD2      PRODUCT2                                     
         MVC   HMOS,SVINVDT        MONTH                                        
         MVC   HEST,SNVPEST        ESTIMATE                                     
         MVC   HEST2,SNVPEST2      ESTIMATE 2                                   
         MVC   HSTAT,SNVPSTAT+1    INV STATUS                                   
         MVC   HINV,SNVPINV        INV NUMBER                                   
*                                                                               
         MVI   TSACTN,TSAADD                                                    
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC                                                        
         BRAS  RE,CALLTSAR                                                      
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* COMMON CALL TO TSAR                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0                  SET CC ON EXIT                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND INVOICE DETAIL                                                           
* TAKE LIST OF STATION LEVEL AUTHORIZATION AND MM I2 PASSIVES AND               
* CREATE LINES FOR INVOICE INFORMATION DIALOG, COMBINING INVOICES               
* ON ONE LINE FOR THE SAME STATION, MONTH, PRD, EST, AND STATUS                 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND21H   NTR1                                                                   
         MVI   TSACTN,TSAGET                                                    
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
*                                                                               
         BRAS  RE,CALLTSAR                                                      
         BE    S21H10                                                           
         MVC   ERROR,=Y(NODATA)          NO DATA TO REPORT                      
         GOTO1 SENDMSG                                                          
         B     EXIT                                                             
*                                                                               
S21H10   MVI   TSACTN,TSANXT                                                    
         LA    R1,X'0021'                                                       
         BAS   RE,SENDH                                                         
*                                                                               
S21H20   L     R5,AIO4             CLEAR IO4 TO KEEP TABLE OF INVOICES          
         ST    R5,ANXTINV                                                       
         LR    R0,R5                                                            
         L     R1,=A(LENIO)                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
S21H30   MVC   SVHREC,HREC                                                      
*                                                                               
* ADD INVOICE TO A STRING OF INVOICES LISTED FOR DIALOG                         
         CLC   SVHINV,SPACES             CHECK IF THERE IS AN INVOICE           
         BNH   S21H50                                                           
         MVC   0(L'SVHINV,R5),SVHINV                                            
         AHI   R5,L'SVHINV-1                                                    
S21H40   CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         BRCT  R5,S21H40                                                        
         MVC   1(2,R5),=C', '                                                   
         AHI   R5,3                                                             
         ST    R5,ANXTINV                                                       
*                                                                               
S21H50   BAS   RE,CALLTSAR                                                      
         BE    S21H60                                                           
         OI    OVFLAG1,LASTINV           NO MORE RECORDS IN TSAR                
         B     S21H70                                                           
*                                                                               
S21H60   CLC   HSTA,SVHSTA               COMPARE STATIONS                       
         BNE   S21H70                                                           
         OC    SVHINV,SVHINV             IF THERE ISN'T AN INV #                
         BZ    S21H30                     IGNORE THE SAVED ONE                  
         CLC   HREC(HINV-HREC),SVHREC    COMPARE EVERYTHING EXCEPT INV          
         BE    S21H30                                                           
*                                                                               
S21H70   XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   BSTA,SVHSTA                                                      
         BAS   RE,STAUNPK                GET STATION CALL LETTERS               
*                                                                               
* THINGS GET COMPLICATED FOR CABLE SYSTEMS BECAUSE THE I2'S DON'T BREAK         
* IT DOWN BY NETWORK AND SUPERDESK WILL IF THAT IS HOW THEY USE THE BUY         
* PROGRAM.  THEREFORE, WE HAVE TO UNPACK THE STATION AND CHECK IF THE           
* CABLE SYSTEM MATCHES, REGARDLESS OF NETWORK, AND DISPLAY BY CABLE             
* SYSTEM.                                                                       
         CLI   WORK,C'0'                 CHECK IF CABLE SYSTEM(NUMERIC)         
         BL    S21H100                   NO, CONTINUE AS IF REGLR STATN         
         CLI   WORK+5,C' '               CHECK IF THERE IS A NETWORK            
         BNH   S21H90                    NO, BUT SAVE CABLE SYSTEM              
         CLC   WORK(4),SVQSTA            ALREADY PROCESSED THIS SYSCODE         
         BNE   S21H80                    NO                                     
         TM    OVFLAG1,LASTINV           CHECK IF THIS WAS THE LAST INV         
         BO    EXIT                      YES, DONE                              
         B     S21H30                    NO, CONTINUE                           
S21H80   MVC   WORK+5(3),SPACES          NO, CLEAR OUT THE NETWORK              
S21H90   MVC   SVQSTA,WORK                                                      
*                                                                               
S21H100  MVC   0(8,R4),WORK              SEND STATION CALL LETTERS              
         LA    R4,7(R4)                                                         
         BRAS  RE,SETDLM                                                        
*                                                                               
         OC    SVHPRD,SVHPRD                                                    
         BZ    S21H150                                                          
         MVC   QPRD,SVHPRD                                                      
         MVC   QPRD2,SVHPRD2                                                    
         BAS   RE,FMTPRDS                FORMAT QPRD-QPRD2                      
         MVC   0(L'FMTPRD,R4),FMTPRD                                            
         LA    R4,L'FMTPRD(R4)                                                  
         BRAS  RE,SETDLM                                                        
*                                                                               
         GOTO1 VDATCON,DMCB,(3,SVHMOS),(10,WORK)   MOS                          
         MVC   0(8,R4),WORK                    MM/DD/YY                         
         LA    R4,7(R4)                                                         
         BRAS  RE,SETDLM                                                        
*                                                                               
         ZICM  R0,SVHEST,1                                                      
         BZ    *+12                      IF EST=0, DISPLAY "NO"                 
         BRAS  RE,SETNUM                                                        
         B     S21H110                                                          
*                                                                               
         MVC   0(2,R4),=C'NO'                                                   
         LA    R4,1(R4)                                                         
         BRAS  RE,SETDLM                                                        
*                                                                               
S21H110  TM    SVHSTAT,X'80'                                                    
         BNO   S21H120                                                          
         MVC   0(7,R4),=C'MATCHED'                                              
         LA    R4,6(R4)                                                         
         B     S21H140                                                          
*                                                                               
S21H120  TM    SVHSTAT+1,X'40'                                                  
         BNO   S21H130                                                          
         MVC   0(11,R4),=C'IN PROGRESS'                                         
         LA    R4,10(R4)                                                        
         B     S21H140                                                          
*                                                                               
S21H130  MVC   0(10,R4),=C'DISCREPANT'                                          
         LA    R4,9(R4)                                                         
S21H140  BRAS  RE,SETDLM                                                        
*                                                                               
S21H150  LA    R1,MCDLSTR                                                       
         LA    R5,BLOCK                                                         
         SR    R5,R4                                                            
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         L     R4,AIO4                                                          
         L     R5,ANXTINV                                                       
         SR    R5,R4                                                            
         BZ    S21H160                                                          
         SHI   R5,2                                                             
*                                                                               
         LA    R1,MCINVNM                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
S21H160  TM    OVFLAG1,LASTINV                                                  
         BNO   S21H20                                                           
         B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 22 HEADER: CALL TO FIS TO GET PERCENT PAID                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV22H   DS    0H                                                               
         MVI   SVXFROV,X'10'       RETURN CONTROL TO THIS OVERLAY               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',QMED,1,GLVSPMD                            
         GOTO1 (RF),(R1),=C'PUTD',QCLT,3,GLVSPCLT                               
         GOTO1 (RF),(R1),=C'PUTD',FMTPRD,7,GLVSPPRD                             
         GOTO1 (RF),(R1),=C'PUTD',QEST,3,GLVSPEST                               
         GOTO1 (RF),(R1),=C'PUTD',QMKT,4,GLVSPMKT                               
*                                                                               
         GOTO1 VDATCON,DMCB,(3,SVSTADT),(5,WORK)                                
         GOTO1 (RF),(R1),(3,SVENDDT),(5,WORK+9)                                 
         MVI   WORK+8,C'-'                                                      
         GOTO1 VPERVAL,DMCB,(17,WORK),BLOCK                                     
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,BLOCK                                                         
         USING PERVALD,R1                                                       
         CLI   PVALNWKS+1,15  15 WEEKS OR LESS WORKS WITH WKLY                  
**       CLI   PVALNWKS+1,52                                                    
         BL    R22H10                                                           
*                             FIS DOESN'T LIKE 52-53 WEEK PERIODS               
         MVC   WORK(2),=C'ES'           USE ES FOR ESTIMATE DATES               
         GOTO1 VGLOBBER,(R1),=C'PUTD',WORK,8,GLVSPPER                           
         B     R22H20                                                           
*                                                                               
*                                                                               
R22H10   GOTO1 VDATCON,DMCB,(3,SVSTADT),(5,WORK)                                
         GOTO1 (RF),(R1),(3,SVENDDT),(5,WORK+8)                                 
         GOTO1 VGLOBBER,(R1),=C'PUTD',WORK,8,GLVSPPER                           
         GOTO1 (RF),(R1),=C'PUTD',WORK+8,8,GLVBUY1                              
*                                                                               
         MVC   DUB(4),=C'WKLY'                                                  
         GOTO1 (RF),(R1),=C'PUTD',DUB,4,GLVSPOPT                                
*                                                                               
R22H20   XC    BLOCK,BLOCK                                                      
         LA    R5,BLOCK                                                         
         USING GLVXFRSY,R5                                                      
         MVC   GLVXFRSY,=C'SPO'    FROM THE SPOT SYSTEM                         
         MVC   GLVXFRPR,=C'SDE'    SUPERDESK PROGRAM                            
         MVC   GLVXTOSY,=C'SPO'    TO THE SPOT SYSTEM                           
         MVC   GLVXTOPR,=C'FIS'    FIS PROGRAM                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',BLOCK,24,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SVREASON,GLOBPAID         SET REASON FOR RETURN                  
         B     EXIT                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND PERCENT PAID                                                   *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND22H   LA    R1,X'0022'                SEND 22 HEADER                         
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R1,MCPCNPD                SEND %PAID                             
         LA    R4,SVPCNPD                                                       
         SR    R5,R5                                                            
         BAS   RE,SENDD                                                         
         B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* BACK UP TO LAST NONBLANK AND INSERT | AS A DELIMITER                          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SETDLM   CLI   0(R4),C' '                                                       
         JH    *+8                                                              
         BRCT  R4,SETDLM                                                        
         MVI   1(R4),C'|'                                                       
         AHI   R4,2                                                             
         BR    RE                                                               
         SPACE 1                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* TRANSMIT NUMERIC VALUE IN R0 AND INSERT | AS A DELIMITER                      
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SETNUM   DS    0H                                                               
         EDIT  (R0),(10,(R4)),ZERO=NOBLANK,ALIGN=LEFT                           
         AR    R4,R0                                                            
         MVI   0(R4),C'|'                                                       
         LA    R4,1(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* TRANSMIT NUMERIC VALUE IN R0 AND INSERT , AS A DELIMITER                      
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SETNUMCM DS    0H                                                               
         EDIT  (R0),(10,(R4)),ZERO=NOBLANK,ALIGN=LEFT                           
         AR    R4,R0                                                            
         MVI   0(R4),C','          COMMA BETWEEN SPOT LEN AND VALUE             
         LA    R4,1(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ON ENTRY R1 CONTAINS HEADER CODE                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR                    GET HEADER ADDRESS                     
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
SENDHX   LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN                              
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA                   GET DATA ITEM                          
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5                     CLEAR OVERRIDE LENGTH                  
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
EXIT     XIT1                                                                   
         GETEL2 R6,42,ELCODE                                                    
*                                                                               
SLNTAB   DS    0CL15                                                            
       ++INCLUDE SPSLNTAB                                                       
         DC    XL16'00'                                                         
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPSDEWRK                                                       
TWAD     DSECT                                                                  
         ORG   TSARREC                                                          
HREC     DS    0D                                                               
HKEY     DS    0XL24                                                            
HSTA     DS    XL3                                                              
HMOS     DS    XL3                                                              
HPRD     DS    CL3                                                              
HPRD2    DS    CL3                                                              
HEST     DS    XL1                                                              
HEST2    DS    XL1                                                              
HSTAT    DS    XL1                 STATUS (MATCHED, DISCREP, ...)               
HINV     DS    CL10                                                             
HKEYX    EQU   *                                                                
HRECX    EQU   *                                                                
HRECLN   EQU   *-HREC                                                           
         ORG                                                                    
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
* 512 BYTES TOTAL IN OVWORK                                                     
*                                                                               
TWASIZQ  EQU   18432                     TWA PAGE SIZE                          
SAVMENU  DS    XL182                                                            
EQUDPT   DS    CL16                      DAYPART SECTION TRANSLATE TAB          
EQUSECT1 DS    CL60                      DAYPART SECTION 1                      
SVDPART  DS    CL3                       DAYPART                                
SVINVDT  DS    XL3                       INVOICE MOS                            
FMTPRD   DS    CL7                       QPRD-QPRD2                             
ANXTINV  DS    A                         ADDRESS FOR NEXT INV IN TABLE          
*                                                                               
SVHREC   DS    0XL(HRECLN)                                                      
SVHSTA   DS    XL3                                                              
SVHMOS   DS    XL3                       MONTH OF SERVICE                       
SVHPRD   DS    CL3                                                              
SVHPRD2  DS    CL3                                                              
SVHEST   DS    XL1                                                              
SVHEST2  DS    XL1                                                              
SVHSTAT  DS    XL1                       STATUS (MATCHED, DISCREP, ...)         
SVHINV   DS    CL10                                                             
*                                                                               
SVQSTA   DS    CL8                                                              
*                                                                               
OVFLAG1  DS    X                         FLAG                                   
LASTINV  EQU   X'80'                     LAST INVOICE FROM TSAR                 
*                                                                               
         ORG                                                                    
*        PRINT OFF                                                              
       ++INCLUDE FLDIND                                                         
       ++INCLUDE MISBUCKSA                                                      
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPGENAUTH                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDPERVALD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPSDE10   10/10/11'                                      
         END                                                                    
