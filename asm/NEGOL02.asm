*          DATA SET NEGOL02    AT LEVEL 066 AS OF 04/18/16                      
*PHASE T31402A                                                                  
***************************************************************                 
*LEVEL   001  IMPLEMENT DAILY GOALS      4/16/98    BPOO                        
*        IF DAILY GOAL , NEW ROUTINE (MYVPER) WILL BE CALLED                    
*        IF NOT DAILY, VALIDTE IS CALLED AND ALL CODE REMAINED                  
*        THE SAME AS BEFORE.                                                    
*                                                                               
* LEV 3  ADD GOAL HISTORY RECORDS WHEN N0 PROFILE REQUIRES IT                   
*        TO SHOW REASONS FOR CHANGES IN GOAL RECORDS. -SPRI                     
*                                                                               
* LEV 4  CHANGE ERROR MESSAGE -SPRI                                             
***************************************************************                 
         TITLE 'NEGOL02 -  GOAL MAINTENANCE'                                    
         PRINT NOGEN                                                            
T31402   CSECT                                                                  
         NMOD1 0,T31402                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA     BASE SCREEN FOR SYSTEM + THIS PROG            
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING T31402,RB,R6                                                     
         MVI   SPFIL20,C' '                                                     
         MVC   SPFIL20+1(L'SPFIL20-1),SPFIL20                                   
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         MVI   IOOPT,C'Y'                                                       
         MVI   XSPACT,0            INITIALIZE ACTIONS                           
         MVI   SPTACT,0                                                         
*                                                                               
         CLC   =C'DELETE',CONACT                                                
         JE    INVACTN                                                          
*                                                                               
MAIN05   MVI   SYSTFLAG,C'X'                                                    
         GOTO1 VSETXSP                                                          
*                                                                               
MAIN10   MVC   AIO,AIO1                                                         
         L     R7,AIO1                                                          
         USING NGOLRECD,R7                                                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DELREC                                                           
EXIT     XIT1                                                                   
*                                                                               
DK       DS    0H                                                               
         CLI   SYSTFLAG,C'X'       XSPOT FILE?                                  
         BNE   *+12                                                             
         BRAS  RE,XDK                                                           
         J     EXIT                                                             
*                                                                               
         MVC   WORK2(48),KEY                                                    
*                                                                               
         MVI   GOLMED,C'N'                                                      
         FOUT  GOLMEDH                                                          
*                                                                               
         MVC   GOLCLI(3),QCLT                                                   
         FOUT  GOLCLIH                                                          
*                                                                               
         XC    GOLPRD,GOLPRD                                                    
         MVC   BPRD,WORK2+4                                                     
         BAS   RE,GETPRD                                                        
         MVC   GOLPRD(3),QPRD                                                   
         TM    WORK2+11,X'40'      WAS PACKAGE INPUTTED                         
         BZ    DK080                                                            
*--MOVE PACKAGE OUT                                                             
         LA    R2,GOLPRD+2                                                      
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'*'                                                       
         AHI   R2,1                                                             
         EDIT  (1,WORK2+12),(3,0(R2)),ALIGN=LEFT                                
DK080    FOUT  GOLPRDH                                                          
*                                                                               
         XC    GOLNET,GOLNET                                                    
*                                                                               
         LA    R3,24(R7)                                                        
         CLI   0(R3),X'20'                                                      
         BNE   DK090                                                            
         CLI   22(R3),X'40'                                                     
         BNH   DK090                                                            
         MVC   GOLNET(4),22(R3)                                                 
         B     DK095                                                            
*                                                                               
DK090    CLC   5(2,R7),=X'0309'       DEFAULT TO NETWORK                        
         BNE   *+14                   0777                                      
         MVC   GOLNET(3),=C'M=N'                                                
         B     DK095                                                            
*                                                                               
         CLC   5(2,R7),=X'0306'       SYNDICATION?                              
         BNE   *+14                                                             
         MVC   GOLNET(3),=C'M=S'                                                
         B     DK095                                                            
*                                                                               
         CLC   5(2,R7),=X'0307'       CABLE?                                    
         BNE   *+10                                                             
         MVC   GOLNET(3),=C'M=C'                                                
*                                                                               
DK095    FOUT  GOLNETH                                                          
*                                                                               
DK100    GOTO1 DISEST,WORK2+7                                                   
         MVC   GOLEST(3),QEST                                                   
         FOUT  GOLESTH                                                          
         L     RE,AIO                                                           
         USING ESTHDR,RE                                                        
         MVC   MYEDAILY,EDAILY                                                  
         MVC   TUSRNM,EUSRNMS                                                   
         DROP  RE                                                               
*                                                                               
DK200    XC    GOLDPTL,GOLDPTL                                                  
         XC    GOLDPT,GOLDPT                                                    
         LA    R2,GOLDPTH                                                       
         GOTO1 VALIDPT,DMCB,(1,WORK2+8)                                         
         MVC   GOLDPT,DPTCODE                                                   
         MVC   GOLDPTL,DPTNAME                                                  
DK250    FOUT  GOLDPTH                                                          
         FOUT  GOLDPTLH                                                         
*                                                                               
DK300    EDIT  (1,WORK2+9),(3,GOLLEN),ALIGN=LEFT                                
         FOUT  GOLLENH                                                          
*                                                                               
DK400    MVC   KEY,WORK2                                                        
         GOTO1 VSETSPT                                                          
*                                                                               
         B     EXIT                                                             
*                                                                               
DR       DS    0H                                                               
         CLI   SYSTFLAG,C'X'       XSPOT FILE?                                  
         BNE   *+12                                                             
         BRAS  RE,XDR                                                           
         J     EXIT                                                             
*                                                                               
         LA    R3,24(R7)                                                        
         USING GDELEM,R3                                                        
         MVC   SVNETWK,GDNETWK                                                  
         DROP  R3                                                               
*                                                                               
         TM    17(R3),X'80'        CHECK PRIORITY PRODUCT BIT                   
         BZ    DR010                                                            
         CLI   GOLPRD,C'*'                                                      
         BE    DR010                                                            
         MVC   FULL(3),GOLPRD                                                   
         MVC   GOLPRD+1(3),FULL                                                 
         MVI   GOLPRD,C'*'         SET INDICATOR                                
         FOUT  GOLPRDH                                                          
*                                                                               
DR010    LA    R4,SYSSPARE+500                                                  
         USING DBLOCKD,R4                                                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=CL3'NAD'                                                 
         MVI   DBSELMED,C'N'                                                    
         XC    WORK2,WORK2                                                      
*                                                                               
         CLC   AGENCY,=C'SJ'                                                    
         BE    *+14                                                             
         CLC   AGENCY,=C'MC'             ONLY FOR MC CANN                       
         BNE   DR01050                                                          
*                                                                               
         OC    ESTTRGL,ESTTRGL           ANY TARGET DEMOS?                      
         BZ    DR01050                                                          
*                                                                               
         LA    R2,GOLTARH                                                       
         GOTO1 DEMOCON,DMCB,(2,ESTTRGL),(10,WORK2),(C'S',DBLOCK),TUSRNM         
*                                                                               
         MVC   GOLTAR(10),WORK2          DISPLAY 1ST TARGET                     
         OI    GOLTARH+4,X'20'           SET PREVALID BIT                       
         FOUT  GOLTARH                                                          
*                                                                               
         LA    R2,GOLDM2H                                                       
         CLC   WORK2(10),WORK2+10        ARE T1 AND T2 THE SAME?                
         BE    DR01010                                                          
*                                                                               
         OC    WORK2+10(10),WORK2+10     IS THERE A 2ND TARGET?                 
         BZ    DR01010                   NO                                     
         MVC   GOLDM2(10),WORK2+10       YES - THEN DISPLAY IT                  
         MVI   GOLDM2H+5,10                                                     
         OI    GOLDM2H+4,X'20'           SET PREVALID BIT                       
         FOUT  GOLDM2H                                                          
         LA    R2,GOLDM3H                                                       
*                                                                               
DR01010  DS    0H                                                               
         LA    R3,ESTDEMO                                                       
         LA    R5,20                                                            
*                                                                               
DR01015  DS    0H                                                               
         CLC   ESTTRGL(3),0(R3)          CHECK IF DEMO TO DISPLAY               
         BE    *+14                      WAS DISPLAYED AS A TARGET              
         CLC   ESTTRGL+3(3),0(R3)                                               
         BNE   DR01020                                                          
*                                                                               
         LA    R3,3(R3)                                                         
         BCT   R5,DR01015                                                       
         B     DR025                                                            
*                                                                               
DR01020  MVC   DISDEMO,0(R3)                                                    
         GOTO1 DEMOCON,DMCB,(1,DISDEMO),(10,WORK2),(C'S',DBLOCK),TUSRNM         
         MVC   8(10,R2),WORK2                                                   
         OI    4(R2),X'20'               SET PREVALID BIT                       
         MVI   5(R2),10                                                         
         FOUT  (R2)                                                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                     BUMP TO NEXT FIELD                     
*                                                                               
         LA    RF,GOLDM3H                DID WE JUST DISPLAY THE                
         CR    R2,RF                     3RD DEMO?                              
         BH    DR025                     YES - EXIT                             
*                                                                               
         LA    R3,3(R3)                                                         
         BCT   R5,DR01015                NO - GET NEXT DEMO TO DISPLAY          
         B     DR025                                                            
*                                                                               
DR01050  DS    0H                                                               
         GOTO1 DEMOCON,DMCB,(3,ESTDEMO),(10,WORK2),(C'S',DBLOCK),TUSRNM         
         MVC   GOLTAR(10),WORK2                                                 
         MVC   GOLDM2(10),WORK2+10                                              
         MVC   GOLDM3(10),WORK2+20                                              
         OI    GOLTARH+4,X'20'     SET PREVALID BIT                             
         FOUT  GOLTARH                                                          
         OC    WORK2+10(10),WORK2+10                                            
         BZ    *+8                                                              
         MVI   GOLDM2H+5,10                                                     
         OC    WORK2+20(10),WORK2+20                                            
         BZ    DR020                                                            
         MVI   GOLDM3H+5,10                                                     
*                                                                               
DR020    OI    GOLDM2H+4,X'20'     SET PREVALID BIT                             
         FOUT  GOLDM2H                                                          
         OI    GOLDM3H+4,X'20'     SET PREVALID BIT                             
         FOUT  GOLDM3H                                                          
*                                                                               
DR025    DS    0H                                                               
         LA    R3,24(R7)                                                        
         CLI   0(R3),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*--SET THE START DAY                                                            
         MVC   STDAY,21(R3)                                                     
         CLI   STDAY,0                                                          
         BNE   *+8                                                              
         MVI   STDAY,1             IF ZERO INPUT SET FOR MONDAY                 
*                                                                               
         MVC   GOLPLN(12),5(R3)                                                 
         OI    GOLPLNH+4,X'20'     SET PREVALID BIT                             
         FOUT  GOLPLNH                                                          
*--SET PREVALID BITS ON                                                         
         LA    R2,GOLDT01H                                                      
*                                                                               
DR050    OI    4(R2),X'20'                                                      
         MVC   8(17,R2),SPACES                                                  
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(11,R2),SPACES                                                  
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         CLI   0(R2),9                                                          
         BNE   DR050                                                            
*                                                                               
DR070    LA    R2,GOLDT01H                                                      
*                                                                               
         MVI   ELCODE,X'21'                                                     
         CLI   ACTNUM,ACTDIS                                                    
         BE    DR090                                                            
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DR100                                                            
DR090    MVC   STSCREEN,ENSCREEN                                                
DR100    XC    ENSCREEN,ENSCREEN                                                
         BAS   RE,NEXTEL                                                        
         BNE   DREXIT                                                           
         CLC   2(2,R3),OPTPER      CHECK DATE FILTER                            
         BL    DR100                                                            
         OC    STSCREEN,STSCREEN                                                
         BZ    DR120                                                            
         CLC   2(2,R3),STSCREEN                                                 
         BL    DR100                                                            
*                                                                               
DR120    GOTO1 DATCON,DMCB,(2,2(R3)),(8,8(R2))                                  
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
*                                                                               
         MVI   8(R2),C'0'          DEFAULT ZERO                                 
         OC    8(4,R3),8(R3)                                                    
         BZ    DR130                                                            
         EDIT  (4,8(R3)),(11,8(R2)),2,ALIGN=LEFT,FLOAT=-                        
DR130    FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
*                                                                               
         OC    4(4,R3),4(R3)                                                    
         BZ    DR140                                                            
         EDIT  (4,4(R3)),(5,8(R2)),1,ALIGN=LEFT                                 
DR140    FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   1(R3),16                                                         
         BL    DR200                                                            
         OC    12(4,R3),12(R3)                                                  
         BZ    DR200                                                            
         EDIT  (4,12(R3)),(5,8(R2)),1,ALIGN=LEFT                                
DR200    FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   1(R3),21                                                         
         BL    DR250                                                            
         OC    17(4,R3),17(R3)                                                  
         BZ    DR250                                                            
         EDIT  (4,17(R3)),(5,8(R2)),1,ALIGN=LEFT                                
DR250    FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   1(R3),17                                                         
         BL    DR300                                                            
         OC    16(1,R3),16(R3)                                                  
         BZ    DR300                                                            
         XC    WORK2(4),WORK2                                                   
         GOTO1 UNDAY,DMCB,16(R3),WORK2                                          
         MVC   8(4,R2),WORK2                                                    
         B     DR400                                                            
*                                                                               
DR300    DS    0H                                                               
         CLI   1(R3),33           ANY WORKHORSE?                                
         BL    DR400                                                            
*                                                                               
         CLI   21(R3),C'W'        WORKHORSE?                                    
         BNE   DR400                                                            
         MVC   8(2,R2),=C'W='                                                   
         MVC   10(3,R2),22(R3)                                                  
*                                                                               
DR400    DS    0H                                                               
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   0(R2),9                                                          
         BNE   DR100                                                            
*                                                                               
DR500    BAS   RE,NEXTEL                                                        
         BNE   DREXIT                                                           
         MVC   ENSCREEN,2(R3)                                                   
         B     DREXIT                                                           
*                                                                               
DREXIT   OI    CONSRVH+6,X'81'                                                  
         B     CHKMSCRN                                                         
         DROP  R4                                                               
         EJECT                                                                  
CHKMSCRN CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   CHKM040                                                          
         OC    ENSCREEN,ENSCREEN   IS THERE ANOTHER SCREEN TO DISPLAY           
         BZ    CHKM040                                                          
         CLI   THISLSEL,C'S'       IS IT A SELECT                               
         BE    CHKM030                                                          
         CLI   THISLSEL,C'A'       IS IT AN ALTER                               
         BNE   CHKM040                                                          
         SPACE 1                                                                
         ZIC   RE,SELLISTN         FIND SELECT CODE IN LIST DIRECTORY           
         MH    RE,=H'6'                                                         
         LA    RE,LISTDIR(RE)                                                   
         MVI   0(RE),C'C'          AND RESET CONTROLLER'S SELECT CODE           
CHKM030  OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
*                                                                               
CHKM040  B     EXIT                                                             
*                                                                               
VK       DS    0H                                                               
         CLI   SYSTFLAG,C'X'       XSPOT FILE?                                  
         BNE   *+12                                                             
         BRAS  RE,XVK                                                           
         J     EXIT                                                             
*                                                                               
         MVI   KEYB4MKT,4                                                       
         MVI   KEYAFMKT,0                                                       
         MVI   NETFILT,0                                                        
         MVI   NLISTS,14           SET GENCON LIST NUMBER TO 14                 
         XC    SVKEY,SVKEY                                                      
         XC    SVMKT,SVMKT                                                      
         MVI   SVKEY,X'02'                                                      
         SPACE                                                                  
         LA    R2,GOLMEDH          * MEDIA                                      
         GOTO1 VALIMED                                                          
         MVC   GKEYAM,BAGYMD                                                    
         MVC   SVKEY+1(1),BAGYMD                                                
         SPACE                                                                  
         LA    R2,GOLCLIH          * CLIENT                                     
         GOTO1 VALIFLD                                                          
         BNZ   VK050                                                            
         B     INVINPT                                                          
VK050    GOTO1 VALICLT                                                          
         MVC   SVKEY+2(2),BCLT                                                  
         SPACE                                                                  
         MVI   NOPTFLG,0           SET OPTIONAL FLAG                            
         CLI   ACTNUM,ACTLIST      LIST/PRINT OPTIONAL                          
         BNE   *+8                                                              
         MVI   NOPTFLG,1           SET OPTIONAL FLAG                            
         SPACE                                                                  
VK100    MVI   BPAKG,0                                                          
         LA    R2,GOLPRDH          * PRODUCT                                    
         GOTO1 VALIFLD                                                          
         BNZ   VK150                                                            
         CLI   NOPTFLG,1                                                        
         BE    VK170                                                            
         B     INVINPT                                                          
VK150    GOTO1 VALIPRD                                                          
         MVC   SVKEY+4(1),BPRD                                                  
         ZIC   R5,KEYB4MKT                                                      
         AHI   R5,1                                                             
         STC   R5,KEYB4MKT                                                      
         SPACE                                                                  
VK170    XC    QNET,QNET                                                        
         MVI   NOPTFLG,1           SET FIELD AS OPTIONAL                        
         LA    R2,GOLNETH          * NETWORK                                    
         GOTO1 VALIFLD                                                          
         BZ    VK180                                                            
*                                                                               
         MVI   NETFILT,C'N'                                                     
         CLC   8(2,R2),=C'M='      MEDIA TYPE?                                  
         BNE   VK175                                                            
*                                                                               
         CLI   10(R2),C'N'         NETWORK?                                     
         BE    VK180                                                            
*                                                                               
         MVI   NETFILT,C'S'                                                     
         CLI   10(R2),C'S'         SYNDICATION?                                 
         BNE   *+14                                                             
         MVC   SVKEY+5(2),=X'0306'     SYNDICATION = 0774                       
         B     VK178                                                            
*                                                                               
         MVI   NETFILT,C'C'                                                     
         CLI   10(R2),C'C'         CABLE?                                       
         BNE   INVINPT                                                          
         MVC   SVKEY+5(2),=X'0307'     CABLE = 0775                             
         B     VK178                                                            
*                                                                               
         CLI   N2PROF+15,C'Y'      IS NETWORK REQUIRED ON ADDS                  
         BNE   VK210               NO                                           
*        CLI   N2PROF+15,C'B'      IS NETWORK ALLOWED ON ADDS                   
*        BNE   VK210               NO ERROR                                     
*                                                                               
VK175    GOTO1 VALINTWK                                                         
         MVC   SVKEY+5(2),QNETMKT                                               
VK178    ZIC   R5,KEYB4MKT                                                      
         LA    R5,2(R5)                                                         
         STC   R5,KEYB4MKT                                                      
         B     VK230                                                            
VK180    CLI   ACTNUM,ACTLIST      LIST/PRINT OPTIONAL                          
         BE    VK230                                                            
         CLI   N2PROF+15,C'Y'      IS NETWORK REQUIRED ON ADDS                  
         BE    VK210               YES ERROR                                    
*        CLI   N2PROF+15,C'B'      IS NETWORK REQUIRED ON ADDS                  
*        BE    VK210               YES ERROR                                    
         MVC   SVKEY+5(2),=XL2'0309'   MOVE MARKET '777' INTO KEY               
         B     VK230               VALIDATE ESTIMATE                            
VK210    MVI   ERROR,MISSING                                                    
         B     INVINPT                                                          
         SPACE                                                                  
VK230    MVI   NOPTFLG,0           RESET THE OPTION FLAG                        
         CLI   ACTNUM,ACTLIST      LIST/PRINT OPTIONAL                          
         BNE   *+8                                                              
         MVI   NOPTFLG,1           SET OPTIONAL FLAG                            
         SPACE                                                                  
         LA    R2,GOLESTH          * ESTIMATE                                   
         GOTO1 VALIFLD                                                          
         BNZ   VK250                                                            
         CLI   NOPTFLG,1                                                        
         BE    VK300                                                            
         B     INVINPT                                                          
VK250    GOTO1 VALIEST                                                          
         MVC   SVKEY+7(1),BEST                                                  
         ZIC   R5,KEYAFMKT                                                      
         AHI   R5,1                                                             
         STC   R5,KEYAFMKT                                                      
*        POINT R3 TO EST RECORD                                                 
         L     R3,AIO                                                           
         USING ESTHDR,R3                                                        
         MVC   MYEDAILY,EDAILY                                                  
         GOTO1 DATCON,DMCB,(0,SVBEGIN),(2,MYESTART)                             
         GOTO1 DATCON,DMCB,(0,SVEND),(2,MYEEND)                                 
         DROP  R3                                                               
         SPACE                                                                  
VK300    XC    GOLDPTL,GOLDPTL     * DAYPART                                    
         OI    GOLDPTLH+6,X'80'                                                 
         LA    R2,GOLDPTH                                                       
         GOTO1 VALIFLD                                                          
         BNZ   VK350                                                            
         CLI   NOPTFLG,1                                                        
         BE    VK400                                                            
         B     INVINPT                                                          
VK350    OI    GOLDPT+1,X'40'                                                   
         GOTO1 VALIDPT,DMCB,(0,GOLDPT)                                          
         MVC   QDPT,DPTVALUE                                                    
         MVC   SVKEY+8(1),QDPT                                                  
         MVC   GOLDPTL,DPTNAME                                                  
*                                                                               
         ZIC   R5,KEYAFMKT                                                      
         AHI   R5,1                                                             
         STC   R5,KEYAFMKT                                                      
         SPACE                                                                  
VK400    XC    QLEN,QLEN                                                        
         LA    R2,GOLLENH          * SPOT LENGTH                                
         GOTO1 VALIFLD                                                          
         LTR   R0,R0                                                            
         BNZ   VK450                                                            
         CLI   NOPTFLG,1                                                        
         BE    VK470                                                            
         B     INVINPT                                                          
VK450    STC   R0,SVKEY+9                                                       
         MVC   QLEN,GOLLEN                                                      
         ZIC   R5,KEYAFMKT                                                      
         AHI   R5,1                                                             
         STC   R5,KEYAFMKT                                                      
         SPACE                                                                  
VK470    CLI   BPAKG,0                                                          
         BE    VK500                                                            
         OI    SVKEY+11,X'40'                                                   
         MVC   SVKEY+12,BPAKG                                                   
         ZIC   R5,KEYAFMKT                                                      
         LA    R5,3(R5)                                                         
         STC   R5,KEYAFMKT                                                      
         SPACE                                                                  
VK500    CLI   NOPTFLG,1                                                        
         BE    VK600                                                            
         MVI   NOPTFLG,1                                                        
         XC    OPTPER,OPTPER       * PERIOD                                     
         LA    R2,GOLPERH                                                       
         GOTO1 VALIFLD                                                          
         BZ    VK550                                                            
*                                                                               
         GOTO1 VALIDAT                                                          
         GOTO1 DATCON,DMCB,(0,QDATE),(2,OPTPER)                                 
         MVC   SVKEY+2(2),BCLT                                                  
VK550    MVI   NOPTFLG,0                                                        
         SPACE                                                                  
*                                                                               
VK600    CLC   HOLDKEY(10),SVKEY                                                
         BNE   VK650                                                            
         CLC   HOLDPER(2),OPTPER                                                
         BNE   VK650                                                            
         CLC   LASTACT(2),CONACT                                                
         BE    VKEXT                                                            
VK650    MVC   HOLDKEY,SVKEY                                                    
         MVC   HOLDPER,OPTPER                                                   
         XC    STSCREEN,STSCREEN                                                
         XC    ENSCREEN,ENSCREEN                                                
         MVC   LASTACT,CONACT                                                   
         CLI   CONACT,C'A'                                                      
         BNE   VKEXT                                                            
         BRAS  RE,SETADD                                                        
*                                                                               
VKEXT    MVC   KEY,SVKEY                                                        
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    EXIT                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BE    EXIT                                                             
         MVC   SVMKT,SVKEY+5                                                    
         CLC   SVKEY(10),KEY                                                    
         BE    EXIT                                                             
*                                                                               
         MVC   SVMKT(2),=XL2'1E61'   TRY MARKET 7777                            
*                                                                               
         CLC   GOLNET(3),=C'M=N'                                                
         BNE   *+14                                                             
         MVC   SVMKT(2),=XL2'0309'   TRY MARKET 0777                            
         B     *+12                                                             
*                                                                               
VKEXT10  CLI   GOLNETH+5,0         WAS NETWORK INPUTTED                         
         BE    VKEXT30                                                          
         CLC   GOLNET(2),=C'M='                                                 
         BE    VKEXT30                                                          
         MVC   SVMKT,QNETMKT                                                    
         BNE   VKEXT50             YES DON'T TEST SECOND DEFAULT                
*                                                                               
VKEXT30  XC    KEY,KEY                                                          
         MVC   SVKEY+5(2),SVMKT                                                 
         MVC   KEY,SVKEY                                                        
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         CLC   SVKEY(10),KEY                                                    
         BE    EXIT                                                             
VKEXT50  MVC   SVKEY+5(2),SVMKT                                                 
         MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
*******************************************************************             
MISSERR  DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVACTN  DS    0H                                                               
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
INVINPT  DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
RECNFND  DS    0H                                                               
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
ESIZEERR DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'* ERROR, MORE THAN 54 PERIODS *'                  
         GOTO1 HEXOUT,DMCB,COUNT,CONHEAD+40,1,C'TOG'                            
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
CHGADD   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(49),=C'* INVALID OR NO DATA RECIEVED - PLEASE REX        
               -ENTER *'                                                        
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
INVESTM  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'* PRODUCT ESTIMATE NOT OPENED *'                  
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
INVDPT   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(19),=C'* INVALID DAYPART *'                              
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
NOHELPA  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(22),=C'* HELP NOT AVAILABLE *'                           
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
RSNCDERR DS    0H                                                               
         MVI   ERROR,RSNCDERQ                                                   
         B     TRAPERR                                                          
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'* REASON FOR CHANGE REQUIRED *'                   
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
DELREC   DS    0H                                                               
         CLI   SYSTFLAG,C'X'       XSPOT FILE?                                  
         BNE   *+12                                                             
         BRAS  RE,XDELR                                                         
         J     EXIT                                                             
*                                                                               
         GOTO1 VSETSPT                                                          
         MVC   SVSKEY,KEY                                                       
*                                                                               
         LA    R7,KEY                                                           
         OI    13(R7),X'80'                                                     
         GOTO1 WRITE                                                            
         L     R7,AIO                                                           
         OI    15(R7),X'80'                                                     
         GOTO1 PUTREC                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVSKEY                                                   
         OI    KEY+11,X'20'        GET HISTORY RECORD                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     ANY HISTORY RECORD?                          
         BNE   DELRX                                                            
         GOTO1 GETREC                                                           
*                                                                               
         LA    R7,KEY                                                           
         OI    13(R7),X'80'                                                     
         GOTO1 WRITE                                                            
         L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
         OI    15(R7),X'80'                                                     
         GOTO1 PUTREC                                                           
*                                                                               
DELRX    DS    0H                                                               
         GOTO1 VSETSPT                                                          
         J     EXIT                                                             
*                                                                               
VR       DS    0H                                                               
         CLI   SYSTFLAG,C'X'       XSPOT FILE?                                  
         BNE   VR010                                                            
         BRAS  RE,XVR                                                           
*                                                                               
         L     RF,AIO2                                                          
         CLI   4(RF),0             EXTENDED BRANDS?                             
         JE    EXIT                                                             
*                                                                               
         CLC   XSPACT,SPTACT       TO INSURE SYNC BETWEEN SPT/XSP               
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         J     EXIT                                                             
*                                                                               
VR010    BRAS  RE,SETDAY           SET THE START DAY FIELD                      
         MVI   TOTDSW,X'00'                                                     
         MVI   CHGSW,0                                                          
         MVI   INPSW,0                                                          
         XC    TOTDLRS,TOTDLRS                                                  
         XC    TOTPNTS,TOTPNTS                                                  
         LA    R2,GOLDT01H                                                      
         LA    R3,GOLDT01H                                                      
         LA    R5,14                                                            
         MVI   BLNKSW,0                                                         
         XC    WORK2(71),WORK2                                                  
         MVI   WORK2,X'FF'                                                      
*                                                                               
         CLI   CONACT,C'A'                                                      
         BNE   VR050                                                            
         BRAS  RE,CHKINPT                                                       
*                                                                               
*--VALIDATE PLANNER FIELD                                                       
VR050    LA    RE,GOLPLNH                                                       
         TM    4(RE),X'20'                                                      
         BNZ   VR100                                                            
         MVI   CHGSW,X'FF'                                                      
*                                                                               
*--VALIDATE OPTIONS FIELD                                                       
VR100    MVI   NOPTFLG,1           SET FIELD AS OPTIONAL                        
         LA    R2,GOLOPTH                                                       
         MVC   RSNCODE,SPACES                                                   
         TM    4(R2),X'20'                                                      
         BNZ   VR110                                                            
         GOTO1 =A(VOPTN),RR=Y                                                   
         LA    R2,GOLDT01H                                                      
*                                                                               
*--VALIDATE PERIOD FIELD                                                        
VR110    DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VR130                                                            
         MVI   INPSW,X'FF'         DATA IS INPUTTED ON SCREEN                   
         TM    4(R2),X'20'                                                      
         BNZ   VR120                                                            
         MVI   CHGSW,X'FF'                                                      
***** ----- NEW CODE TO ACCOMODATE DAILY GOALS (BPOO  4/17/98) ***              
VR120    DS    0X                                                               
         CLI   MYEDAILY,C'Y'                                                    
         BNE   VR125                                                            
*                                                                               
         GOTO1 =A(MYVPER),DMCB,RR=Y                                             
         BRAS  RE,NEXTUN                                                        
         B     VR150                                                            
*                                                                               
VR125    GOTO1 VALIDTE             VALIDATE PERIOD FIELD                        
         BRAS  RE,NEXTUN                                                        
         B     VR150                                                            
*--IF NO DATE INPUT REST OF LINE MUST BE BLANK                                  
VR130    LA    RF,4                                                             
VR140    BRAS  RE,NEXTUN                                                        
         CLI   5(R2),0                                                          
         BNE   INVINPT                                                          
         BCT   RF,VR140                                                         
         BRAS  RE,NEXTUN           POINT R2 TO NEXT LINE                        
         B     VR450                                                            
*                                                                               
*--VALIDATE DOLLAR FIELD                                                        
VR150    TM    4(R2),X'20'                                                      
         BNZ   VR160                                                            
         MVI   CHGSW,X'FF'                                                      
VR160    CLI   5(R2),0                                                          
         BE    VR200                                                            
         CLI   TOTDSW,X'FF'        DOLLAR AMOUNT NOT ALLOWED                    
         BE    INVINPT             WHEN TOTAL DOLLAR REQUESTED                  
*                                                                               
VR165    MVI   BLNKSW,X'FF'        THERE IS DATA ON THIS LINE                   
         CLC   8(2,R2),=CL2'DD'                                                 
         BNE   VR170                                                            
         LA    R2,GOLDT02H-GOLDL01H(R2)     POINT R2 TO NEXT LINE               
         B     VR350                                                            
VR170    CLI   8(R2),C'T'                                                       
         BNE   VR190                                                            
         CLI   5(R2),C'1'                                                       
         BE    INVINPT                                                          
         MVI   TOTDSW,X'FF'                                                     
VR190    GOTO1 =A(VALIDOL),DMCB,RR=Y                                            
*                                                                               
VR200    BRAS  RE,NEXTUN                                                        
*                                                                               
*--VALIDATE DEMO1 FIELD                                                         
         TM    4(R2),X'20'                                                      
         BNZ   VR210                                                            
         MVI   CHGSW,X'FF'                                                      
VR210    CLI   5(R2),0                                                          
         BNE   VR220                                                            
         CLI   TOTDSW,X'FF'        IF TOTAL DOLLARS REQUESTED                   
         BE    *+12                THERE MUST BE INPUT IN THIS FIELD            
         B     VR250                                                            
VR220    GOTO1 =A(VALIPTS),DMCB,RR=Y                                            
         MVI   BLNKSW,X'FF'        THERE IS DATA ON THIS LINE                   
VR250    BRAS  RE,NEXTUN                                                        
*                                                                               
*--VALIDATE DEMO2 FIELD                                                         
         TM    4(R2),X'20'                                                      
         BNZ   VR260                                                            
         MVI   CHGSW,X'FF'                                                      
VR260    CLI   5(R2),0                                                          
         BE    VR280                                                            
         MVI   BLNKSW,X'FF'        THERE IS DATA ON THIS LINE                   
         CLI   GOLDM2H+5,0                                                      
         BE    INVINPT                                                          
*        BAS   RE,VALIPTS          VALIDATE DEMO/CPP FIELD                      
         GOTO1 =A(VALIPTS),DMCB,RR=Y                                            
VR280    BRAS  RE,NEXTUN                                                        
*                                                                               
*--VALIDATE DEMO3 FIELD                                                         
         TM    4(R2),X'20'                                                      
         BNZ   VR285                                                            
         MVI   CHGSW,X'FF'                                                      
VR285    CLI   5(R2),0                                                          
         BE    VR290                                                            
         MVI   BLNKSW,X'FF'        THERE IS DATA ON THIS LINE                   
         CLI   GOLDM2H+5,0                                                      
         BE    INVINPT                                                          
*        BAS   RE,VALIPTS          VALIDATE DEMO/CPP FIELD                      
         GOTO1 =A(VALIPTS),DMCB,RR=Y                                            
VR290    BRAS  RE,NEXTUN                                                        
*                                                                               
*--VALIDATE DAY FIELD                                                           
         TM    4(R2),X'20'                                                      
         BNZ   VR300                                                            
         MVI   CHGSW,X'FF'                                                      
VR300    CLI   5(R2),0                                                          
         BE    VR320                                                            
*                                                                               
         CLC   8(2,R2),=C'W='                                                   
         BE    VR320                                                            
*                                                                               
         MVI   BLNKSW,X'FF'        THERE IS DATA ON THIS LINE                   
         ZIC   R4,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R4),8(R2)),FULL,FULL+1                             
         CLI   FULL,0                                                           
         BE    INVINPT                                                          
VR320    BRAS  RE,NEXTUN                                                        
*                                                                               
VR350    LA    R3,25(R3)           POINT R3 TO DOLLAR FIELD                     
         CLI   BLNKSW,X'FF'                                                     
         BE    VR400               THERE IS DATA FOR THIS LINE                  
         CLI   WORK2,X'FF'         FIRST LINE MUST HAVE DATA                    
         BE    INVINPT                                                          
         MVC   27(5,R3),WORK2+27   MOVE DEMO1                                   
         MVC   24(1,R3),WORK2+24   MOVE DEMO1 LENGTH OUT                        
         MVC   40(5,R3),WORK2+40   MOVE DEMO2                                   
         MVC   37(1,R3),WORK2+37   MOVE DEMO2 LENGTH OUT                        
         MVC   53(5,R3),WORK2+53   MOVE DEMO3                                   
         MVC   50(1,R3),WORK2+50   MOVE DEMO3 LENGTH OUT                        
         MVC   66(5,R3),WORK2+66   MOVE DAY                                     
         MVC   63(1,R3),WORK2+63   MOVE DAY LENGTH OUT                          
         CLI   TOTDSW,X'FF'        IS TOTAL DOLLARS USED                        
         BE    VR400               DONT CARRY OVER DOLLAR AMOUNT                
         MVC   8(11,R3),WORK2+8    MOVE DOLLAR VALUE OUT                        
         MVC   5(1,R3),WORK2+5     MOVE DOLLAR LENGTH OUT                       
*                                                                               
VR400    MVC   WORK2(71),0(R3)     MOVE LINE INFO TO SAVE LOCATION              
         LR    R3,R2               POSITION R3 TO NEXT LINE                     
         MVI   BLNKSW,0                                                         
VR450    BCT   R5,VR110                                                         
         LA    R2,GOLDT01H                                                      
         CLI   INPSW,X'FF'         IF NO INPUT ON SCREEN                        
         BNE   INVINPT             ERROR                                        
         CLI   CHGSW,X'FF'                                                      
         BE    VR600                                                            
         MVC   STSCREEN,ENSCREEN                                                
         B     VR940                                                            
*                                                                               
VR600    CLC   RSNCODE,SPACES      REASON CODE SUPPLIED?                        
         BNE   VR610                                                            
         CLI   ACTNUM,ACTADD       NO, BUT OK FOR ADD                           
         BE    VR610                                                            
         LA    R2,GOLOPTH                                                       
         CLI   N0PROF+7,C'Y'       REQUIRED IN PROFILE?                         
         BE    RSNCDERR            YES, PRINT ERROR MESSAGE                     
*--CREATE ELEMENTS PUT THEM IN GOALREC                                          
VR610    MVI   TOTDSW,0                                                         
         LA    R2,GOLDT01H                                                      
*                                                                               
         MVI   ELCODE,X'42'        DELETE NETWORK TOTAL ELEMENT                 
         BAS   RE,DELEL                                                         
*                                                                               
VR650    DS    0H                                                               
         CLI   5(R2),0                                                          
         BNE   VR680                                                            
         LA    R2,GOLDY01H-GOLDT01H(R2)                                         
         B     VR920                                                            
*                                                                               
VR680    DS    0H                                                               
         CLI   MYEDAILY,C'Y'                                                    
         BNE   VR683                                                            
*                                                                               
         GOTO1 =A(MYVPER),DMCB,RR=Y                                             
         B     VR686                                                            
*                                                                               
VR683    GOTO1 VALIDTE             VALIDATE PERIOD FIELD                        
*                                                                               
VR686    LA    R4,BWEEKS                                                        
*--DELETE ALL 21 ELEMENTS TO BE UPDATED                                         
VR700    MVI   ELCODE,X'21'                                                     
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(ELCODE,0(R7)),           X        
               (X'02',0(R4))                                                    
         LA    R4,2(R4)                                                         
         OC    0(2,R4),0(R4)                                                    
         BNZ   VR700                                                            
*                                                                               
         XC    ELEM(33),ELEM                                                    
         MVI   ELEM,X'21'                                                       
         MVI   ELEM+1,X'21'       GLEN5Q                                        
*                                                                               
         LA    R4,BWEEKS                                                        
*                                                                               
VR750    MVC   ELEM+2(2),0(R4)     MOVE WEEK FIELD TO ELEMENT                   
         BRAS  RE,NEXTUN                                                        
*                                                                               
         CLC   8(2,R2),=CL2'DD'    IS DELETE LINE REQUESTED                     
         BNE   VR770               NO BYPASS THIS CODE                          
         XC    ELEM+2(2),ELEM+2                                                 
         LA    R2,GOLDY01H-GOLDL01H(R2)     POINT R2 TO NEXT LINE               
         B     VR920                                                            
*                                                                               
VR770    BRAS  RE,CALCDOL          MOVE DOLLAR AMOUNT TO ELEMENT                
         BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   5(R2),0             MOVE DEMO 1 VALUE TO ELEMENT                 
         BNE   VR790                                                            
         CLI   TOTDSW,X'FF'        TOTAL DOLLARS REQUESTED                      
         BNE   VR800               NO GO TO NEXT FIELD                          
         XC    ELEM+8(4),ELEM+8    YES, CLEAR DOLLAR FIELD                      
         B     VR800                                                            
VR790    XC    DMCB+4(4),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(1,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM+4(4),DMCB+4                                                 
VR800    BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   5(R2),0             MOVE DEMO 2 VALUE TO ELEMENT                 
         BE    VR870                                                            
         XC    DMCB+4(4),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(1,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM+12(4),DMCB+4                                                
VR870    BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   5(R2),0             MOVE DEMO 3 VALUE TO ELEMENT                 
         BE    VR880                                                            
         XC    DMCB+4(4),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(1,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM+17(4),DMCB+4                                                
VR880    BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   5(R2),0             MOVE DAY VALUE TO ELEMENT                    
         BE    VR900                                                            
*                                                                               
         CLC   8(2,R2),=C'W='     WORKHORSE?                                    
         BNE   VR895                                                            
         MVI   ELEM+21,C'W'       WORKHORSE                                     
         MVC   ELEM+22(3),10(R2)                                                
         OC    ELEM+22(3),SPACES                                                
         B     VR900                                                            
*                                                                               
VR895    DS    0H                                                               
         ZIC   R5,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R5),8(R2)),ELEM+16,FULL                            
         CLI   ELEM+16,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR900    BAS   RE,PUTEL                                                         
         LA    R4,2(R4)                                                         
         MVC   ELEM+2(2),0(R4)                                                  
         OC    0(2,R4),0(R4)                                                    
         BNZ   VR900                                                            
*                                                                               
VR920    XC    ELEM(33),ELEM                                                    
         BRAS  RE,NEXTUN                                                        
         CLI   0(R2),9                                                          
         BNE   VR650                                                            
         CLI   TOTDSW,X'FF'                                                     
         BNE   VR940                                                            
         BAS   RE,ADDPTS                                                        
         GOTO1 =A(TOTDOLR),DMCB,RR=Y                                            
*--MOVE PLANNER CODE OUT                                                        
VR940    LA    R3,24(R7)                                                        
         CLI   0(R3),X'20'                                                      
         BNE   VR950                                                            
         MVC   5(12,R3),GOLPLN                                                  
         MVC   22(4,R3),QNET                                                    
         NI    17(R3),X'7F'        CHECK FOR PRIORITY BRAND                     
         CLI   GOLPRD,C'*'                                                      
         BNE   *+8                                                              
         OI    17(R3),X'80'        SET PRIORITY INDICATOR                       
         BAS   RE,ACTIVITY                                                      
         BAS   RE,NETTOT                                                        
         B     VR1000                                                           
VR950    XC    ELEM(76),ELEM                                                    
         MVC   ELEM(2),=XL2'204C'                                               
         MVC   ELEM+5(12),GOLPLN                                                
         MVC   ELEM+21(1),STDAY                                                 
         MVC   ELEM+22(4),QNET                                                  
         CLI   GOLPRD,C'*'                                                      
         BNE   *+8                                                              
         OI    ELEM+17,X'80'       SET PRIORITY INDICATOR                       
         BAS   RE,PUTEL                                                         
         BAS   RE,ACTIVITY                                                      
         BAS   RE,NETTOT                                                        
*                                                                               
VR1000   DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR1010                                                           
         GOTO1 ADDREC                                                           
         B     VR1020                                                           
*                                                                               
VR1010   DS    0H                                                               
         GOTO1 PUTREC                                                           
*                                                                               
VR1020   GOTO1 =A(HSTRYREC),RR=Y                                                
         CLI   ACTNUM,ACTSEL       IF SELECT HANDLE MULTI SCREEN                
         BE    CHKMSCRN                                                         
         B     DR                                                               
         EJECT                                                                  
* AFTER RECORD DELETED                                                          
XRD      DS    0H                                                               
         GOTO1 =A(DHSTRYRC),RR=Y                                                
         B     EXIT                                                             
* AFTER RECORD RESTORED                                                         
XRR      DS    0H                                                               
         GOTO1 =A(RHSTRYRC),RR=Y                                                
         B     EXIT                                                             
*                                                                               
*--ADD DEMO1 VALUES IF TOTAL DOLLARS REQUESTED                                  
*                                                                               
ADDPTS   NTR1                                                                   
         XC    TOTPNTS,TOTPNTS                                                  
*                                                                               
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'21'                                                     
*                                                                               
         CLI   0(R3),X'21'                                                      
         BE    ADDP150                                                          
*                                                                               
ADDP100  BAS   RE,NEXTEL                                                        
         BNE   ADDPEXT                                                          
*                                                                               
ADDP150  CLI   8(R3),C'T'          IF ELEMENT NOT FOR TOTAL DONT CALC           
         BNE   ADDP100                                                          
         ICM   R4,15,TOTPNTS                                                    
         ICM   R5,15,4(R3)                                                      
         AR    R4,R5                                                            
         STM   R4,15,TOTPNTS                                                    
         B     ADDP100                                                          
ADDPEXT  B     EXIT                                                             
         EJECT                                                                  
ACTIVITY NTR1                                                                   
         LA    R3,24(R7)                                                        
         CLI   0(R3),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(5,0),(2,32(R3))                                     
         OC    30(2,R3),30(R3)     DOES CREATE DATE EXIST                       
         BNZ   ACTX                YES EXIT                                     
         MVC   30(2,R3),32(R3)     MOVE LAST ACTIVE INTO CREATE DATE            
*                                                                               
ACTX     XIT1                                                                   
         EJECT                                                                  
NETTOT   NTR1                                                                   
         XC    LISTDOL,LISTDOL                                                  
         XC    TDOLELM,TDOLELM                                                  
         XC    FULL,FULL                                                        
         XC    ELEM(12),ELEM                                                    
         MVC   ELEM(2),=X'420C'                                                 
         MVI   ELCODE,X'21'                                                     
         SR    R5,R5                                                            
         LA    R3,24(R7)                                                        
NETT050  BAS   RE,NEXTEL                                                        
         BNE   NETT100                                                          
*                                                                               
         TM    4(R3),X'80'                                                      
         BZ    *+22                IF NOT TOTAL ELEM DONT SAVE                  
         LR    R4,R3               SAVE ADDRESS OF LAST ELEMENT                 
         ICM   R5,15,8(R3)         ACCUM TOTAL ELEMENTS ONLY                    
         A     R5,TDOLELM                                                       
         ST    R5,TDOLELM                                                       
         NI    4(R3),X'7F'         RESET TOTAL DOLLAR SWITCH                    
         CLI   FULL,0                                                           
         BNE   NETT070                                                          
         MVC   FULL(2),2(R3)                                                    
NETT070  MVC   FULL+2(2),2(R3)                                                  
         ICM   R5,15,8(R3)                                                      
         A     R5,LISTDOL                                                       
         ST    R5,LISTDOL                                                       
         B     NETT050                                                          
*                                                                               
NETT100  CLI   TOTDSW,X'FF'        WAS TOTAL DOLLAR OPTION USED                 
         BNE   NETT200             NO ROUNDING NOT REQUIRED                     
         ICM   R2,15,TDOLELM       SUM OF ELEMENTS                              
         ICM   R3,15,TOTDLRS       ACTUAL TOTAL AMOUNT                          
         SR    R3,R2                                                            
         ICM   R2,15,8(R4)         ADD DIFFERENCE TO LAST ELEMENT               
         AR    R2,R3                                                            
         STCM  R2,15,8(R4)                                                      
         ICM   R2,15,LISTDOL                                                    
         AR    R2,R3                                                            
         STCM  R2,15,LISTDOL                                                    
*                                                                               
NETT200  MVC   ELEM+2(4),FULL      MOVE DATES IN                                
         MVC   ELEM+8(4),LISTDOL   MOVE IN TOTAL DOLLARS                        
         BAS   RE,PUTEL            WRITE ELEMENT OUT                            
*                                                                               
NETTEX   XIT1                                                                   
         EJECT                                                                  
         EJECT                                                                  
*                                                                               
LR       DS    0H                                                               
         CLI   SYSTFLAG,C'X'       XSPOT FILE?                                  
         BNE   *+12                                                             
         BRAS  RE,XLR                                                           
         J     EXIT                                                             
*                                                                               
         SPACE                                                                  
         BRAS  RE,SETOPT                                                        
         MVC   AIO,AIO1                                                         
         MVI   NFILE,C'S'          SPOT FILE                                    
         OC    KEY(17),KEY                                                      
         BNZ   LR100                                                            
         MVC   KEY,SVKEY                                                        
*                                                                               
LR100    GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         B     LR220                                                            
*                                                                               
LR200    GOTO1 VSETSPT                                                          
         GOTO1 SEQ                                                              
LR220    CLC   SVKEY(4),KEY                                                     
         BNE   LREXT                                                            
LR225    DS    0H                                                               
         TM    KEY+11,X'20'              CHECK IF A HISTORY RECORD              
         BO    LR200                     YES, SO SKIP IT                        
         ZIC   RF,KEYB4MKT                                                      
         BCTR  RF,0                                                             
         EX    RF,FRONTCK                                                       
         BNE   LR200                                                            
*                                                                               
LR230    DS    0H                                                               
         OC    KEYAFMKT,KEYAFMKT                                                
         BZ    LR232                                                            
         ZIC   RF,KEYAFMKT                                                      
         BCTR  RF,0                                                             
         EX    RF,BACKCK                                                        
         BNE   LR200                                                            
*  CHECK THE FILTERS                                                            
LR232    OC    OPTEST,OPTEST                                                    
         BZ    LR235                                                            
         CLC   OPTEST,KEY+7                                                     
         BNE   LR200                                                            
LR235    OC    OPTDAYP,OPTDAYP                                                  
         BZ    LR240                                                            
         CLC   OPTDAYP,KEY+8                                                    
         BNE   LR200                                                            
LR240    OC    OPTLEN,OPTLEN                                                    
         BZ    LR250                                                            
         CLC   OPTLEN,KEY+9                                                     
         BNE   LR200                                                            
*                                                                               
LR250    DS    0H                                                               
         CLI   NETFILT,0                                                        
         BE    LR255                                                            
         CLI   NETFILT,C'N'        NETWORK FILTER?                              
         BNE   LR255                                                            
         CLC   KEY+5(2),=X'0309'   0777?                                        
         BE    LR260                                                            
         B     LR200                                                            
*                                                                               
LR255    DS    0H                                                               
         CLI   NETFILT,C'S'        SYNDICATION?                                 
         BNE   LR258                                                            
         CLC   KEY+5(2),=X'0306'   0774?                                        
         BE    LR260                                                            
         B     LR200                                                            
*                                                                               
LR258    DS    0H                                                               
         CLI   NETFILT,C'C'        CABLE?                                       
         BNE   LR260                                                            
         CLC   KEY+5(2),=X'0307'   0775?                                        
         BNE   LR200                                                            
*                                                                               
LR260    MVC   SVKEY,KEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
         LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
*                                                                               
         MVC   BPRD,GKEYPRD                                                     
         BAS   RE,GETPRD                                                        
         LA    R2,LRPROD                                                        
         LA    R3,24(R7)                                                        
         TM    17(R3),X'80'        CHECK PRIORITY PRODUCT BIT                   
         BZ    LR310                                                            
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
LR310    MVC   0(3,R2),QPRD                                                     
*--MOVE PACKAGE CODE OUT IF ON THE RECORD                                       
         TM    GKEYAGY,X'40'                                                    
         BZ    LR360                                                            
         LA    RE,3                                                             
LR320    CLI   0(R2),X'40'                                                      
         BE    LR330                                                            
         LA    R2,1(R2)                                                         
         BCT   RE,LR320                                                         
LR330    MVI   0(R2),C'/'                                                       
         EDIT  (1,GKEYPRD2),(3,1(R2)),ALIGN=LEFT                                
*--MOVE NETWORK CODE OUT IF ON THE RECORD                                       
LR360    LA    R3,24(R7)                                                        
         USING GDELEM,R3                                                        
         CLI   0(R3),X'20'                                                      
         BNE   LR370                                                            
         CLI   GDNETWK,X'40'                                                    
         BNH   LR370                                                            
         MVC   LRNET,GDNETWK                                                    
         B     LR380                                                            
         DROP  R3                                                               
*                                                                               
LR370    DS    0H                                                               
         CLC   5(2,R7),=X'0309'                                                 
         BNE   *+12                                                             
         MVI   LRNET,C'N'          DEFAULT FOR NETWORK                          
         B     LR380                                                            
*                                                                               
         CLC   5(2,R7),=X'0306'                                                 
         BNE   *+12                                                             
         MVI   LRNET,C'S'          SYNDICATION                                  
         B     LR380                                                            
*                                                                               
         CLC   5(2,R7),=X'0307'                                                 
         BNE   *+12                                                             
         MVI   LRNET,C'C'          CABLE                                        
         B     LR380                                                            
*                                                                               
*--MOVE ESTIMATE OUT TO THE LIST LINE                                           
LR380    XC    LREST,LREST                                                      
         EDIT  (1,GKEYEST),(3,LREST),ALIGN=LEFT                                 
*--MOVE DAYPART OUT TO THE LIST LINE                                            
         GOTO1 VALIDPT,DMCB,(1,GKEYDPT)                                         
         MVI   NFILE,C'S'                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(13),GOALREC                                                  
         GOTO1 HIGH                 REPOSITION THE POINTER                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LRDPT(8),DPTNAME                                                 
*--MOVE LENGTH OUT TO THE LIST LINE                                             
         XC    LRLEN,LRLEN                                                      
         EDIT  (1,GKEYSLN),(3,LRLEN),ALIGN=LEFT                                 
*--MOVE TOTAL COST AND DEMO1 TOTAL TO THE LIST LINE                             
         XC    WORK2(4),WORK2                                                   
         LA    R3,24(R7)                                                        
         XC    LISTDOL,LISTDOL                                                  
         XC    LISTPNT1,LISTPNT1                                                
         MVI   ELCODE,X'21'                                                     
LR400    BAS   RE,NEXTEL                                                        
         BNE   LR450                                                            
*  CHECK FILTERS                                                                
         OC    OPTSPER,OPTSPER     DATE FILTERS                                 
         BZ    LR420                                                            
         CLC   OPTSPER,2(R3)                                                    
         BH    LR400                                                            
         CLC   OPTEPER,2(R3)                                                    
         BL    LR400                                                            
*                                                                               
LR420    OC    WORK2(2),WORK2                                                   
         BNZ   LR440                                                            
         MVC   WORK2(2),2(R3)                                                   
LR440    MVC   WORK2+2(2),2(R3)                                                 
         ICM   RF,15,8(R3)                                                      
         A     RF,LISTDOL          ADD DOLLARS                                  
         ST    RF,LISTDOL                                                       
         L     RF,LISTPNT1                                                      
         A     RF,4(R3)            ADD POINTS                                   
         ST    RF,LISTPNT1                                                      
         B     LR400                                                            
LR450    OC    LISTDOL,LISTDOL                                                  
         BZ    LR460                                                            
         L     RF,LISTDOL                                                       
         SR    RE,RE                                                            
         D     RE,=F'100'          REMOVE PENNIES                               
         ST    RF,LISTDOL                                                       
         EDIT  (4,LISTDOL),(10,LRCOST),ALIGN=LEFT                               
LR460    OC    LISTPNT1,LISTPNT1                                                
         BZ    LR470                                                            
         EDIT  (4,LISTPNT1),(8,LRDEMO),1,ALIGN=LEFT                             
*--MOVE S-E PERIODS TO LIST LINE                                                
LR470    OC    WORK2(4),WORK2                                                   
         BZ    LR500                                                            
         GOTO1 DATCON,DMCB,(2,WORK2),(5,LRDAT)                                  
         MVI   LRDAT+8,C'-'                                                     
         GOTO1 DATCON,DMCB,(2,WORK2+2),(5,LRDAT+9)                              
*                                                                               
LR500    GOTO1 LISTMON                                                          
         B     LR200               GOTO READ SEQ                                
*                                                                               
LREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
FRONTCK  CLC   SVKEY(0),KEY                                                     
BACKCK   CLC   SVKEY+7(0),KEY+7                                                 
         EJECT                                                                  
* PRINTING THE LINE                                                             
PR       DS    0H                                                               
         MVI   NFILE,C'T'          STATION FILE                                 
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R4,P+10                                                          
         USING PLINED,R4                                                        
*                                                                               
         MVC   BPRD,GKEYPRD                                                     
         BAS   RE,GETPRD                                                        
         LA    RF,PRPRD                                                         
         LA    R3,24(R7)                                                        
         TM    17(R3),X'80'        CHECK PRIORITY PRODUCT BIT                   
         BZ    PR100                                                            
         MVI   0(RF),C'*'                                                       
         LA    RF,1(RF)                                                         
PR100    MVC   0(3,RF),QPRD                                                     
*                                                                               
         MVC   BPRD,GKEYPRD                                                     
         BAS   RE,GETPRD                                                        
         MVC   PRPRD(3),QPRD                                                    
*--MOVE ESTIMATE OUT TO THE PRINT LINE                                          
         EDIT  (1,GKEYEST),(3,PREST),ALIGN=LEFT                                 
*--MOVE DAYPART OUT TO THE PRINT LINE                                           
         GOTO1 VALIDPT,DMCB,(1,GKEYDPT)                                         
         MVI   NFILE,C'S'                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(13),GOALREC                                                  
         GOTO1 HIGH                REPOSITION THE POINTER                       
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PRDPT(8),DPTNAME                                                 
*--MOVE LENGTH OUT TO THE PRINT LINE                                            
         EDIT  (1,GKEYSLN),(3,PRLEN),ALIGN=LEFT                                 
*--MOVE TOTAL COST AND DEMO1 DEMO2 TOTAL TO THE PRINT LINE                      
         XC    WORK2(4),WORK2                                                   
         LA    R3,24(R7)                                                        
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         SR    R5,R5                                                            
         MVI   ELCODE,X'21'                                                     
PR400    BAS   RE,NEXTEL                                                        
         BNE   PR450                                                            
*  CHECK FILTERS                                                                
         OC    OPTSPER,OPTSPER     DATE FILTERS                                 
         BZ    PR420                                                            
         CLC   OPTSPER,2(R3)                                                    
         BL    PR400                                                            
         CLC   OPTEPER,2(R3)                                                    
         BH    PR400                                                            
*                                                                               
PR420    OC    WORK2(2),WORK2                                                   
         BNZ   PR440                                                            
         MVC   WORK2(2),2(R3)                                                   
PR440    MVC   WORK2+2(2),2(R3)                                                 
         ICM   RF,15,8(R3)                                                      
         ST    RF,FULL                                                          
         A     R1,FULL             ADD DOLLARS                                  
         MVC   FULL,4(R3)                                                       
         A     R2,FULL             ADD DEMO 1                                   
         CLI   1(R3),X'0C'         IF ELEMENT HAS LENGTH OF 12                  
         BE    PR400               DEMO 2 DOES NOT EXIST                        
         MVC   FULL,12(R3)                                                      
         A     R5,FULL             ADD DEMO 2                                   
         B     PR400                                                            
PR450    ST    R1,FULL                                                          
         OC    FULL,FULL                                                        
         BZ    PR460                                                            
         L     RF,FULL                                                          
         SR    RE,RE                                                            
         D     RE,=F'100'          REMOVE PENNIES                               
         ST    RF,FULL                                                          
         EDIT  (4,FULL),(10,PRCST),ALIGN=LEFT                                   
PR460    ST    R2,FULL                                                          
         OC    FULL,FULL                                                        
         BZ    PR470                                                            
         EDIT  (4,FULL),(8,PRDM1),1,ALIGN=LEFT                                  
PR470    ST    R5,FULL                                                          
         OC    FULL,FULL                                                        
         BZ    PR480                                                            
         EDIT  (4,FULL),(8,PRDM2),1,ALIGN=LEFT                                  
*--MOVE S-E PERIODS TO PRINT LINE                                               
PR480    OC    WORK2(4),WORK2                                                   
         BZ    PR500                                                            
         GOTO1 DATCON,DMCB,(2,WORK2),(5,PRPER)                                  
         MVI   PRPER+8,C'-'                                                     
         GOTO1 DATCON,DMCB,(2,WORK2+2),(5,PRPER+9)                              
*                                                                               
PR500    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR200                                                            
         SPACE                                                                  
PREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R3)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  LTR   R3,R3                                                            
         BR    RE                                                               
         SPACE 2                                                                
GETPRD   NTR1                                                                   
         LA    RE,255                                                           
         LA    RF,SVCLIST                                                       
*--MOVE PRODUCT OUT TO THE LIST LINE                                            
GP300    CLC   3(1,RF),BPRD                                                     
         BE    GP310                                                            
         LA    RF,4(RF)                                                         
         BCT   RE,GP300                                                         
         DC    H'0'                                                             
GP310    MVC   QPRD,0(RF)                                                       
GPEX     XIT1                                                                   
*                                                                               
NEXTUN   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BER   RE                                                               
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUN                                                           
         BR    RE                                                               
         SPACE 2                                                                
         SPACE 2                                                                
* SUB-ROUTINES FOR ELEMENT MAINTENANCE                                          
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(ELCODE,0(R7)),0                   
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
PUTEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFILE'),0(R7),ELEM                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* PACK A NUMBER FROM A  HEADER FIELD                                            
*   R0 = RESULT                                                                 
PACKSI   SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1               EXIT ON ZERO LENGTH                          
         BZ    PACKX                                                            
         TM    4(R2),X'08'         OR NON-NUMERIC                               
         BZ    PACKX                                                            
         BCTR  R1,0                                                             
         EX    R1,*+10                                                          
         CVB   R0,DUB                                                           
PACKX    BR    RE                                                               
         PACK  DUB,8(0,R2)   * EXECUTED *                                       
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,40,C'NETWORK GOAL RECORDS'                                    
         SSPEC H2,40,C'--------------------'                                    
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8+10                                                         
         USING PLINED,R2                                                        
         MVC   PRPRD(7),=C'PRODUCT'                                             
         MVC   PRPRD+132(7),=7C'-'                                              
         MVC   PREST(8),=C'ESTIMATE'                                            
         MVC   PREST+132(8),=8C'-'                                              
         MVC   PRDPT(7),=C'DAYPART'                                             
         MVC   PRDPT+132(8),=8C'-'                                              
         MVC   PRLEN(6),=C'LENGTH'                                              
         MVC   PRLEN+132(6),=6C'-'                                              
         MVC   PRCST+1(7),=C'DOLLARS'                                           
         MVC   PRCST+132(10),=10C'-'                                            
         MVC   PRDM1+1(5),=C'DEMO1'                                             
         MVC   PRDM1+132(8),=8C'-'                                              
         MVC   PRDM2+1(5),=C'DEMO2'                                             
         MVC   PRDM2+132(8),=8C'-'                                              
         MVC   PRPER+1(14),=C'START-END DATE'                                   
         MVC   PRPER+132(17),=17C'-'                                            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************                                                       
*                                                                               
INVINP   MVI   ERROR,INVALID                                                    
*                                                                               
         PRINT GEN                                                              
TRAPERR  GOTO1 ERREX                                                            
         PRINT NOGEN                                                            
         LTORG                                                                  
********************************************************************            
*  SET OPTION FILTERS FOR LIST SCREEN                                           
********************************************************************            
SETOPT   NTR1  BASE=*,LABEL=*                                                   
         XC    OPTSPER,OPTSPER                                                  
         XC    OPTDAYP,OPTDAYP                                                  
         XC    OPTEST,OPTEST                                                    
         XC    OPTLEN,OPTLEN                                                    
*                                                                               
         LA    R2,LGOOPTH          * OPTIONS                                    
         CLI   5(R2),0                                                          
         JE    EXIT                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BE    STO310                                                           
         CLI   ACTNUM,ACTLIST                                                   
         JNE   INVINP                                                           
STO310   L     R3,AIO2                                                          
         USING SCAND,R3                                                         
         XC    DMCB,DMCB                                                        
         GOTO1 SCANNER,DMCB,(20,(R2)),(6,0(R3))                                 
         CLI   DMCB+4,0                                                         
         JE    INVINP                                                           
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,DMCB+4                                                      
*                                                                               
STO320   CLI   FLD1,C'D'                                                        
         BE    STO420                                                           
*                                                                               
         CLI   FLD1,C'E'                                                        
         BE    STO440                                                           
*                                                                               
         CLI   FLD1,C'L'                                                        
         BE    STO460                                                           
*                                                                               
         CLI   FLD1,C'P'                                                        
         JNE   INVINP                                                           
* VALIDATE THE DATE FIELD                                                       
*                                                                               
         XC    SCANAREA,SCANAREA                                                
         MVC   SCANAREA+5(1),FLD2LEN                                            
         MVC   SCANAREA+8(20),FLD2                                              
         LA    R5,WORK2                                                         
         GOTO1 SCANNER,DMCB,SCANAREA,(2,0(R5)),C',=,-'                          
         CLI   4(R1),0                                                          
         JE    INVINP                                                           
         CLI   1(R5),0             ARE 2 DATES INPUTTED                         
         BNE   STO330                                                           
         MVC   WORK2+1(1),WORK2                                                 
         MVC   WORK2+8(4),WORK2+4                                               
         MVC   WORK2+22(10),WORK2+12                                            
STO330   CLI   0(R3),0             TEST FIRST DATE GIVEN                        
         JE    INVINP                                                           
         GOTO1 DATVAL,DMCB,(0,12(R5)),(0,50(R5))                                
         OC    0(4,R1),0(R1)                                                    
         JZ    INVINP                                                           
         CLC   3(1,R1),0(R5)                                                    
         JNE   INVINP                                                           
STO335   CLI   1(R3),0             TEST SECOND DATE GIVEN                       
         JE    INVINP                                                           
         GOTO1 DATVAL,DMCB,(0,22(R5)),(0,56(R5))                                
         OC    0(4,R1),0(R1)                                                    
         JZ    INVINP                                                           
         CLC   3(1,R1),1(R5)                                                    
         JNE   INVINP                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,50(R5)),(2,OPTSPER)                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,56(R5)),(2,OPTEPER)                               
*                                                                               
         CLC   OPTSPER(2),OPTEPER   START CANNOT BE GREATER THEN END            
         JH    INVINP                                                           
         B     STO500                                                           
*                                                                               
*  VALIDATE THE DAYPART FIELD                                                   
*                                                                               
STO420   MVC   OPTDAYP,FLD2                                                     
         B     STO500                                                           
*                                                                               
*  VALIDATE THE ESTIMATE FIELD                                                  
*                                                                               
STO440   CLI   FLD2LEN,0                                                        
         JE    INVINP                                                           
*                                                                               
         ZIC   RE,FLD2LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD2(0)                                                      
         CVB   RE,DUB                                                           
         STC   RE,OPTEST                                                        
         B     STO500                                                           
*                                                                               
*  VALIDATE THE LENGTH FIELD                                                    
*                                                                               
STO460   CLI   FLD2LEN,0                                                        
         JE    INVINP                                                           
*                                                                               
         ZIC   RE,FLD2LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD2(0)                                                      
         CVB   RE,DUB                                                           
         STC   RE,OPTLEN                                                        
         B     STO500                                                           
*                                                                               
STO500   LA    R3,42(R3)                                                        
         BCT   R4,STO320                                                        
         J     EXIT                                                             
********************************************************************            
********************************************************************            
SETADD   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK2(14),WORK2                                                  
         LA    R4,SYSSPARE+500                                                  
         USING DBLOCKD,R4                                                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=CL3'NTI'                                                 
         MVI   DBSELMED,C'N'                                                    
*                                                                               
         CLC   AGENCY,=C'SJ'                                                    
         BE    *+14                                                             
         CLC   AGENCY,=C'MC'             ONLY FOR MC CANN                       
         BNE   STA010                                                           
*                                                                               
         OC    ESTTRGL,ESTTRGL           ANY TARGET DEMOS?                      
         BZ    STA010                                                           
*                                                                               
         LA    R2,GOLTARH                                                       
         GOTO1 DEMOCON,DMCB,(2,ESTTRGL),(10,WORK2),(C'S',DBLOCK),TUSRNM         
*                                                                               
         MVC   GOLTAR(10),WORK2          DISPLAY 1ST TARGET                     
         OI    GOLTARH+4,X'20'           SET PREVALID BIT                       
         FOUT  GOLTARH                                                          
*                                                                               
         LA    R2,GOLDM2H                                                       
         CLC   WORK2(1),WORK2+10         ARE T1 AND T2 THE SAME?                
         BE    STA20                                                            
*                                                                               
         OC    WORK2+10(10),WORK2+10     IS THERE A 2ND TARGET?                 
         BZ    STA20                     NO                                     
         MVC   GOLDM2(10),WORK2+10       YES - THEN DISPLAY IT                  
         MVI   GOLDM2H+5,10                                                     
         OI    GOLDM2H+4,X'20'           SET PREVALID BIT                       
         FOUT  GOLDM2H                                                          
         LA    R2,GOLDM3H                                                       
*                                                                               
STA20    DS    0H                                                               
         LA    R3,ESTDEMO                                                       
         LA    R5,20                                                            
*                                                                               
STA25    DS    0H                                                               
         CLC   ESTTRGL(3),0(R3)          CHECK IF DEMO TO DISPLAY               
         BE    *+14                      WAS DISPLAYED AS A TARGET              
         CLC   ESTTRGL+3(3),0(R3)                                               
         BNE   STA30                                                            
*                                                                               
         LA    R3,3(R3)                                                         
         BCT   R5,STA25                                                         
         B     STA020                                                           
*                                                                               
STA30    MVC   DISDEMO,0(R3)                                                    
         GOTO1 DEMOCON,DMCB,(1,DISDEMO),(10,WORK2),(C'S',DBLOCK),TUSRNM         
         MVC   8(10,R2),WORK2                                                   
         OI    4(R2),X'20'               SET PREVALID BIT                       
         MVI   5(R2),10                                                         
         FOUT  (R2)                                                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                     BUMP TO NEXT FIELD                     
*                                                                               
         LA    RF,GOLDM3H                DID WE JUST DISPLAY THE                
         CR    R2,RF                     3RD DEMO?                              
         BH    STA020                    YES - EXIT                             
*                                                                               
         LA    R3,3(R3)                                                         
         BCT   R5,STA25                  NO - GET NEXT DEMO TO DISPLAY          
         B     STA020                                                           
*                                                                               
STA010   DS    0H                                                               
         GOTO1 DEMOCON,DMCB,(3,ESTDEMO),(2,WORK2),(0,DBLOCK)                    
         MVC   GOLTAR(7),WORK2                                                  
         FOUT  GOLTARH                                                          
         OC    WORK2+7(7),WORK2+7                                               
         BZ    STA020                                                           
         MVC   GOLDM2(7),WORK2+7                                                
         MVI   GOLDM2H+5,7                                                      
         FOUT  GOLDM2H                                                          
*                                                                               
STA020   LA    R2,GOLDT01H                                                      
         CLI   5(R2),0             SEE IF CARRY OVER INPUT                      
         BNE   STAEXT                                                           
*--SET PREVALID BITS ON                                                         
*                                                                               
STA050   OI    4(R2),X'20'                                                      
         MVC   8(17,R2),SPACES                                                  
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(11,R2),SPACES                                                  
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         CLI   0(R2),9                                                          
         BNE   STA050                                                           
*                                                                               
STAEXT   J     EXIT                                                             
         DROP  R4                                                               
*************************************************************                   
*  VALIDATE DOLLAR INPUT                                                        
*************************************************************                   
VALIDOL  NTR1  BASE=*,LABEL=*                                                   
         CLI   5(R2),0                                                          
         BNE   VALD050                                                          
         CLI   TOTDSW,X'FF'        IF TOTAL DOLLAR SET THEN ACCUM DEMO1         
         BNE   INVINP                                                           
         B     VALDEXT                                                          
*--CHECK DOLLARS                                                                
VALD050  LA    R3,8(R2)                                                         
         CLI   TOTDSW,X'FF'                                                     
         BNE   VALD070                                                          
         LA    R3,1(R3)                                                         
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         ST    RE,DMCB+4                                                        
         B     VALD100                                                          
*                                                                               
VALD070  XC    DMCB+4(4),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
VALD100  GOTO1 CASHVAL,DMCB,(2,(R3))                                            
         CLI   DMCB,X'FF'                                                       
         BE    INVINP                                                           
*                                                                               
         CLI   TOTDSW,X'FF'                                                     
         BNE   VALDEXT                                                          
         MVC   TOTDLRS,DMCB+4                                                   
*                                                                               
VALDEXT  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*--VALIDATE DEMO INPUT                                                          
*                                                                               
VALIPTS  NTR1  BASE=*,LABEL=*                                                   
         CLI   5(R2),0                                                          
         BE    INVINP                                                           
*--CHECK DEMO                                                                   
VALP070  XC    DMCB+4(4),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(1,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BE    INVINP                                                           
         B     VALPEXT                                                          
*                                                                               
VALPEXT  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*************************************************************                   
MYVPER   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    BWEEKS,BWEEKS                                                    
         XC    POPTION,POPTION                                                  
         MVC   MYWORKD(6),SVBEGIN  MOVE EST START/END                           
         MVC   MYWORKD+6(6),SVEND    MOVE EST START/END                         
         CLI   5(R2),0                                                          
         BE    MYPER30                                                          
*                                                                               
         MVC   WORK(17),8(R2)      MOVE DATA TO WORK                            
         LA    R4,WORK                                                          
* 3270'S LEAVE INPUT LEN 17 WITH NO DATA - SO CHECK FOR IT                      
         OC    WORK(17),SPACES                                                  
         CLC   WORK(17),SPACES                                                  
         BE    MYPER30                                                          
*                                                                               
         CLC   =C'S-',0(R4)                                                     
         BNE   MYPER2                                                           
         LA    R4,2(R4)                                                         
         B     MYPER7                                                           
*                                                                               
MYPER2   XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK),C',=,-'                              
*                                                                               
         GOTO1 DATVAL,DMCB,(0,BLOCK+12),MYWORKD                                 
         MVI   ERROR,SDTERR                                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   MYPER4                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,(1,BLOCK+12),MYWORKD                                 
         MVI   ERROR,SDTERR                                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    TRAPERR                                                          
*                                                                               
MYPER4   CLI   BLOCK+1,0           SEE IF SINGLE DATE REQUESTED                 
         BNE   MYPER5                                                           
         MVC   MYWORKD+6(6),MYWORKD                                             
         B     MYPER30                                                          
MYPER5   ZIC   RE,BLOCK                                                         
         AR    R4,RE               POINT TO SEPARATOR                           
         CLI   0(R4),C'-'                                                       
         BNE   TRAPERR                                                          
         LA    R4,1(R4)                                                         
*                                                                               
MYPER7   CLC   =C'E ',0(R4)                                                     
         BNE   MYPER8                                                           
         B     MYPER30                                                          
*                                                                               
MYPER8   GOTO1 DATVAL,DMCB,(0,BLOCK+22),MYWORKD+6                               
         OC    0(4,R1),0(R1)       IF NOT VALID,                                
         BZ    MYPER10               CHECK FOR OTHER EXPRESSIONS                
         B     MYPER30                                                          
*                                                                               
MYPER10  GOTO1 DATVAL,DMCB,(1,BLOCK+22),MYWORKD+6                               
         OC    0(4,R1),0(R1)       IF NOT VALID,                                
         BZ    MYPER12               CHECK FOR OTHER EXPRESSIONS                
         B     MYPER30                                                          
*                                                                               
MYPER12  LA    R0,3                CHECK FOR 'NW OR NNW'                        
         LR    R1,R4                                                            
         MVI   ERROR,EDTERR                                                     
MYPER13  CLI   0(R1),C' '                                                       
         BE    TRAPERR                                                          
         CLI   0(R1),C'D'                                                       
         BE    MYPER14                                                          
         CLI   0(R1),C'0'                                                       
         BL    TRAPERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    TRAPERR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,MYPER13                                                       
         B     TRAPERR                                                          
MYPER14  LA    R5,2                SET MAX LEN-1                                
         SR    R5,R0               SET FOR EX                                   
         BM    TRAPERR                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)    * EXECUTED *                                      
         CVB   R0,DUB                                                           
         STC   R0,BYTE             SAVE NUMBER OF WEEKS                         
         CLI   1(R1),C' '                                                       
         BNE   MYPER15                                                          
         LA    R0,1               1 DAY AFTER                                   
         MVI   POPTION,C'Y'                                                     
         B     MYPER16                                                          
*                                                                               
MYPER15  LA    R0,2               2 DAYS AFTER                                  
         CLI   1(R1),C'A'                                                       
         BE    MYPER16                                                          
         LA    R0,3               3 DAYS AFTER                                  
         CLI   1(R1),C'T'                                                       
         BE    MYPER16                                                          
*        LA    R0,28              4 WEEKS AFTER                                 
         LA    R0,4               4 DAYS AFTER                                  
         CLI   1(R1),C'F'                                                       
         BE    MYPER16                                                          
         MVI   ERROR,DTERR                                                      
         B     TRAPERR                                                          
MYPER16  ST    R0,FULL             SAVE DAYS SKIPPED                            
         CLC   MYWORKD(2),=C'00'   DO WE HAVE START YEAR                        
         BNE   MYPER18             YES                                          
         MVC   MYWORKD(2),SVBEGIN  MOVE EST START YEAR                          
         CLC   SVBEGIN(2),SVEND    TEST EST ALL IN ONE YEAR                     
         BE    MYPER18                                                          
         CLC   MYWORKD+2(4),SVBEGIN+2 INPUT MONTH TO EST START MONTH            
         BNL   *+10                  IF HI OR EQ USE START YEAR                 
         MVC   MYWORKD(2),SVEND      ELSE USE END YEAR                          
*                                                                               
MYPER18  MVI   ERROR,PERERR                                                     
         CLC   MYWORKD(6),SVBEGIN                                               
         BL    TRAPERR                                                          
         CLC   MYWORKD(6),SVEND                                                 
         BH    TRAPERR                                                          
         MVC   WORK(6),MYWORKD                                                  
* FIND START DAY                                                                
         GOTO1 GETDAY,DMCB,MYWORKD,MYWORKD+12                                   
         CLI   0(R1),1                                                          
         BE    MYPER20                                                          
         MVI   ERROR,SDAYERR                                                    
*                                                                               
***      CLC   MYWORKD(6),SVBEGIN      IN DAILYS, START WEEK IS NOT             
***      BNE   TRAPERR                 REQUIRED                                 
* GET PREVIOUS MONDAY IN 'WORK'                                                 
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
*        GOTO1 ADDAY,DMCB,MYWORKD,WORK,(R0) MESSES UP DAILY                     
         SR    R0,R0                                                            
         IC    R0,STDAY            START DAY OF GOALS                           
         BCTR  R0,0                                                             
*        GOTO1 ADDAY,DMCB,WORK NORK,(R0)    MESSES UP DAILY                     
* BUILD DATES - FIRST DATE FROM MYWORKD, REST FROM WORK                         
MYPER20  GOTO1 DATCON,DMCB,MYWORKD,(2,BWEEKS)                                   
*                                                                               
         LA    R5,BWEEKS+2                                                      
         SR    R0,R0                                                            
         IC    R0,BYTE             GET NUMBER OF WEEKS                          
         MVI   ERROR,PERERR                                                     
         B     MYPER24                                                          
MYPER22  MVC   DMCB+8(4),FULL                                                   
*        GOTO1 ADDAY,DMCB,WORK,WORK,7                                           
         CLI   POPTION,C'Y'                                                     
         BE    MYPER22A                                                         
         GOTO1 ADDAY,DMCB,WORK,WORK                                             
         B     MYPER22B                                                         
MYPER22A GOTO1 ADDAY,DMCB,WORK,WORK,1                                           
MYPER22B CLC   WORK(6),SVEND                                                    
         BH    TRAPERR                                                          
         GOTO1 DATCON,DMCB,WORK,(2,(R5))                                        
         LA    R5,2(R5)                                                         
MYPER24  BCT   R0,MYPER22                                                       
         B     MYPER42                                                          
*-------------------------------------------                                    
         EJECT                                                                  
MYPER30  CLC   MYWORKD(2),=C'00'   DO WE HAVE START YEAR                        
         BNE   MYPER32                                                          
         MVC   MYWORKD(2),SVBEGIN  MOVE EST START YEAR                          
         CLC   SVBEGIN(2),SVEND    TEST EST ALL IN ONE YEAR                     
         BE    MYPER32                                                          
         CLC   MYWORKD+2(4),SVBEGIN+2 INPUT MONTH TO EST START MONTH            
         BNL   *+10                  IF HI OR EQ USE START YEAR                 
         MVC   MYWORKD(2),SVEND      ELSE USE END YEAR                          
*                                                                               
MYPER32  CLC   MYWORKD+6(2),=C'00' DO WE HAVE END YEAR                          
         BNE   MYPER34                                                          
         MVC   MYWORKD+6(2),SVBEGIN                                             
         CLC   SVBEGIN(2),SVEND                                                 
         BE    MYPER34                                                          
         CLC   MYWORKD+8(4),SVBEGIN+2                                           
         BNL   *+10                                                             
         MVC   MYWORKD+6(2),SVEND                                               
*                                                                               
MYPER34  MVI   ERROR,STENDERR                                                   
         CLC   MYWORKD(6),MYWORKD+6                                             
         BH    TRAPERR                                                          
         MVI   ERROR,PERERR                                                     
         MVI   ERROR,MISSING                                                    
         CLC   MYWORKD(6),SVBEGIN                                               
         BL    TRAPERR                                                          
         CLC   MYWORKD+6(6),SVEND                                               
         BH    TRAPERR                                                          
         CLI   SVPRD2,0            TEST PIGGYBACK                               
         BE    MYPER34X                                                         
         SPACE 1                                                                
* TEST DATES IN PARTNER ESTIMATE PERIOD *                                       
         SPACE 1   *****                                                        
****     MVI   ERROR,BADPR2DT                                                   
****     CLC   MYWORKD(6),SVBEGIN2                                              
****     BL    TRAPERR                                                          
***      CLC   MYWORKD+6(6),SVEND2                                              
***      BH    TRAPERR                                                          
*                                                                               
MYPER34X MVC   WORK(6),MYWORKD     ASSUME MONDAY START                          
*                                                                               
         GOTO1 GETDAY,DMCB,MYWORKD,MYWORKD+12                                   
         CLI   0(R1),1                                                          
         BE    MYPER35                                                          
**       MVI   ERROR,SDAYERR       ERR DAILY OR WEEKLY                          
**       CLC   MYWORKD(6),SVBEGIN  DAILYS DON'T HAVE TO MATCH DAY OF            
**       BNE   TRAPERR             START WEEK                                   
* GET PREVIOUS MONDAY IN WORK                                                   
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
*        GOTO1 ADDAY,DMCB,MYWORKD,WORK,(R0)                                     
MYPER35  SR    R0,R0                                                            
         IC    R0,STDAY            START DAY OF GOALS                           
         BCTR  R0,0                                                             
*        GOTO1 ADDAY,DMCB,WORK,WORK,(R0)                                        
*        GOTO1 ADDAY,DMCB,WORK,WORK,7                                           
*                                                                               
*                                                                               
* NOW CHECK END DAY IS MONDAY OR EST END                                        
*                                                                               
MYPER36  GOTO1 GETDAY,DMCB,MYWORKD+6,MYWORKD+12                                 
         CLI   0(R1),1                                                          
         BE    MYPER38                                                          
         MVI   ERROR,EDAYERR                                                    
         MVI   ERROR,MISSING                                                    
         CLC   MYWORKD(6),MYWORKD+6                                             
         BE    MYPER38             IF SINGLE INPUT OK                           
**       CLC   MYWORKD+6(6),SVEND  DAILYS DONT HAVE TO HAVE END DATE            
**       BNE   TRAPERR             MACTH DAY OF END WEEK                        
* WAS COMMENTED                                                                 
MYPER38  DS    0H                                                               
         GOTO1 DATCON,DMCB,MYWORKD,(2,BWEEKS)                                   
*                                                                               
         LA    R5,BWEEKS+2                                                      
*                                                                               
MYPER40  GOTO1 ADDAY,DMCB,WORK,WORK,1                                           
         CLC   WORK(6),MYWORKD+6                                                
         BH    MYPER42                                                          
         GOTO1 DATCON,DMCB,WORK,(2,(R5))                                        
         LA    R5,2(R5)                                                         
         B     MYPER40                                                          
*                                                                               
MYPER42  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***************************************************************                 
* VALIDATE OPTIONS IN GOAL/CHANGE SCREEN                                        
***************************************************************                 
VOPTN    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VALIFLD                                                          
         BZ    VOPTNX                                                           
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),WORK2,C',=,-'                                  
         CLI   4(R1),0                                                          
         JE    INVINP                                                           
*                                                                               
         LA    R3,WORK2                                                         
         ZIC   R4,4(R1)            # OF ENTRIES                                 
*                                                                               
VOPTN10  DS    0H                                                               
VOPTN20  DS    0H                                                               
         CLC   =C'RS=',12(R3)                                                   
         JNE   INVINP                                                           
         CLI   N0PROF+7,C'Y'       REQUIRED IN PROFILE?                         
         JNE   INVINP              NO, DON'T ALLOW THIS OPTION                  
*                                                                               
* DETERMINE LENGTH OF REASON CODE AND STORE IT                                  
*                                                                               
         ZIC   R5,0(R3)            LENGTH OF INPUT                              
         LA    R6,12(R3)                                                        
         CLI   3(R6),C' '                                                       
         JNH   INVINP                                                           
         SHI   R5,3                SUBTRACT OFF RS=                             
         SHI   R5,1                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RSNCODE(0),3(R6)                                                 
         ZIC   RF,0(R3)                                                         
         STC   RF,RSNCDLN                                                       
*                                                                               
VOPTN50  DS    0H                                                               
         LA    R3,32(R3)           BUMP TO NEXT ENTRY IN SCANNER                
         BCT   R4,VOPTN10                                                       
*                                                                               
VOPTNX   XC    WORK2,WORK2                                                      
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
***************************************************************                 
* ADD GOAL HISTORY ELEMENT TO HISTORY RECORD IF CHANGE WAS MADE                 
* TO RECORD                                                                     
***************************************************************                 
HSTRYREC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   GOLOPTH+5,0               WAS A REASON ENTERED?                  
         BE    HSTRYX                    NO                                     
         CLC   =C'RS=',GOLOPT+7                                                 
         BNE   HSTRYX                                                           
*                                                                               
* BUILD HISTORY ELEMENT                                                         
HSTRY05  XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING GHSTEL,R3                                                        
         MVI   GHSTCODE,X'60'            HISTORY ELCODE                         
         GOTO1 DATCON,DMCB,(5,0),(2,TDAY)        CURRENT DATE                   
         MVC   GHSTDATE,TDAY                                                    
*                                                                               
* GET TIME OF CHANGE                                                            
         THMS  DDSTIME=YES                                                      
         STCM  R1,15,PACKWORK                                                   
         STCM  R0,15,PACKWORK+4                                                 
         AP    PACKWORK(4),PACKWORK+4(4)  DDS TIME IS OFFSET FROM 6AM           
         CP    PACKWORK(4),=P'240000'    PAST MIDNIGHT?                         
         BL    HSTRY10                                                          
         SP    PACKWORK(4),=P'240000'    YES, ADJUST TIME                       
HSTRY10  MVC   GHSTTIME,PACKWORK                                                
*                                                                               
         ZIC   R4,RSNCDLN               REASON CODE LEN - 1                     
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   GHSTRSN(0),RSNCODE                                               
*                                                                               
         LA    R4,1(R4)                 ADD 1 FOR REASON CODE LEN               
         AH    R4,=Y(GHSTRSN-GHSTEL)    ADD ELEMENT LEN W/O REASON CODE         
         STC   R4,GHSTLEN               STORE VARIABLE ELEMENT LEN              
         DROP  R3                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R7,AIO                                                           
         MVC   HOLDKEY,KEY                                                      
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(13),KEY                                                    
         OI    SVKEY+11,X'20'                                                   
         MVC   KEY,SVKEY                                                        
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),SVKEY                                                    
         BE    HSTRY20                                                          
*                                                                               
         LR    R0,R7                  CLEAR AIO                                 
         LH    R1,=H'2000'                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   KEY,SVKEY                                                        
         MVC   0(13,R7),KEY                                                     
         MVC   GLENGTH,=AL2(GDELEM-GOALREC)                                     
         MVC   GAGYALPH,TWAAGY                                                  
         BAS   RE,PUTEL                                                         
*                                                                               
* ADD GOAL DESCRIPTION ELEMENT                                                  
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING GDELEM,R3                                                        
         MVI   GOCODE,X'20'            ELEMENT CODE AND LENGTH                  
         MVI   GOLEN,GDLENQ                                                     
         MVC   GREDATE,TDAY            CREATION DATE                            
         MVC   GDNETWK,SVNETWK                                                  
         BAS   RE,PUTEL                                                         
*                                                                               
         GOTO1 ADDREC                                                           
         B     HSTRY70                                                          
*                                                                               
HSTRY20  DS    0H                                                               
         GOTO1 GETREC                                                           
* CHECK IF THERE'S IS ALREADY AN HISTORY ELEMENT WITH THE SAME EXACT            
* COMMENT ADDED WITHIN THE PAST 10 MINUTES                                      
         MVI   ELCODE,X'60'                                                     
         LA    R3,24(R7)                                                        
HSTRY30  BAS   RE,NEXTEL                                                        
HSTRY40  BNE   HSTRY60                                                          
*                                                                               
HSTRY50  DS    0H                                                               
         USING GHSTEL,R3                                                        
*                                                                               
* COMPARE ELEMENT LEN                                                           
         CLC   GHSTLEN,ELEM+1                                                   
         BNE   HSTRY30                                                          
*                                                                               
* COMPARE REASON CODE                                                           
         ZIC   R1,GHSTLEN                                                       
         SH    R1,=H'9'                  GET LEN OF (COMMENT-1) ONLY            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   GHSTRSN(0),ELEM+8         COMPARE REASON CODE                    
         BNE   HSTRY30                                                          
*                                                                               
* COMPARE TIME OF CHANGE                                                        
         MVC   PACKWORK(4),GHSTTIME      GET TIME OF OLD ELEM                   
         SRP   PACKWORK(4),62,3          REMOVE SECONDS: SHIFT RIGHT 2,         
         AP    PACKWORK(4),=P'10'        USE 3 TO ROUND, ADD 10 MINUTES         
         MVC   PACKWORK+4(4),ELEM+4      GET TIME OF NEW ELEMENT                
         SRP   PACKWORK+4(4),62,3                                               
         CP    PACKWORK(4),PACKWORK+4(4) WITHIN 10 MINUTES?                     
         BL    HSTRY30                   NO                                     
         B     HSTRY70                   YES, DON'T CHANGE RECORD               
         DROP  R3                                                               
*                                                                               
*                                                                               
HSTRY60  BAS   RE,PUTEL                                                         
         CLI   DMCB+12,X'05'             RECORD HAS BECOME TOO LONG?            
         BNE   HSTRY65                   NO                                     
*                                                                               
         LA    R2,GOLOPTH                YES                                    
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'* HISTORY RECORD HAS BECOME TOO LONG *'           
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
HSTRY65  GOTO1 PUTREC                                                           
*                                                                               
* REREAD GOAL RECORD                                                            
HSTRY70  L     R7,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),HOLDKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),HOLDKEY                                                  
         BNE   HSTRYX                    MUST BE AN ADD                         
         GOTO1 GETREC                                                           
HSTRYX   MVC   AIO,AIO1                  NOW POINT TO CHANGED RECORD            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
***************************************************************                 
* DELETE GOAL HISTORY RECORDS WHEN THE GOAL RECORDS ARE DELETED                 
***************************************************************                 
DHSTRYRC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R7,AIO                                                           
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(13),KEY                                                    
         OI    SVKEY+11,X'20'                                                   
         MVC   KEY,SVKEY                                                        
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),SVKEY                                                    
         BNE   DHSTRYX                THERE ISN'T A HISTORY RECORD              
         GOTO1 GETREC                                                           
         OI    KEY+13,X'80'           DELETE KEY                                
         OI    15(R7),X'80'           DELETE RECORD                             
         GOTO1 WRITE                                                            
         GOTO1 PUTREC                                                           
*                                                                               
DHSTRYX  XIT1                                                                   
*****************************************************************               
* RESTORE GOAL HISTORY RECORDS WHEN THE GOAL RECORDS ARE RESTORED               
*****************************************************************               
RHSTRYRC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R7,AIO                                                           
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(13),KEY                                                    
         OI    SVKEY+11,X'20'                                                   
         MVC   KEY,SVKEY                                                        
         GOTO1 VSETSPT                                                          
         OI    DMINBTS,X'08'          READ FOR DELETES                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),SVKEY                                                    
         BNE   RHSTRYX                THERE ISN'T A HISTORY RECORD              
         OI    DMINBTS,X'08'                  READ FOR DELETES                  
         GOTO1 GETREC                                                           
         NI    KEY+13,X'FF'-X'80'     DELETE KEY                                
         NI    15(R7),X'FF'-X'80'     DELETE RECORD                             
         GOTO1 WRITE                                                            
         GOTO1 PUTREC                                                           
*                                                                               
RHSTRYX  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*************************                                                       
* CALCULATE TOTAL DOLLAR*                                                       
*************************                                                       
TOTDOLR  NTR1  BASE=*,LABEL=*                                                   
         ICM   R1,15,TOTDLRS       TOTAL DOLLARS                                
         ZAP   PACKWORK(16),=PL1'0'                                             
         CVD   R1,PACKWORK+8                                                    
         MP    PACKWORK(16),=PL2'100'                                           
         ICM   R1,15,TOTPNTS       TOTAL POINTS                                 
         LTR   R1,R1               CANNOT HAVE ZERO TOTAL POINTS                
         BZ    TRAPERR                                                          
         CVD   R1,DUB                                                           
         MVC   PACKTPNT(6),DUB+2                                                
         DP    PACKWORK(16),PACKTPNT(6)                                         
         MVC   PACKCPP(10),PACKWORK    SAVE COST PER POINT                      
*                                                                               
         LA    R3,24(R7)                                                        
         CLI   SYSTFLAG,C'X'                                                    
         BNE   *+8                                                              
         LA    R3,42(R7)                                                        
*                                                                               
         MVI   ELCODE,X'21'                                                     
         CLI   0(R3),X'21'                                                      
         BE    TOTD150                                                          
*                                                                               
TOTD100  BAS   RE,NEXTEL                                                        
         BNE   TOTDEX                                                           
*                                                                               
TOTD150  CLI   8(R3),C'T'          IF ELEMENT NOT FOR TOTAL DONT CALC           
         BE    TOTD175                                                          
         CLI   TOTDSW,X'FF'        TOTAL DOLLARS?                               
         BNE   TOTD100                                                          
*                                                                               
TOTD175  DS    0H                                                               
         OC    SVTGWKC,SVTGWKC                                                  
         BZ    *+14                                                             
         CLC   2(2,R3),SVTGWKC                                                  
         BL    TOTD100                                                          
*                                                                               
         XC    8(4,R3),8(R3)       CLEAR BUDGET FIELD                           
         OC    4(4,R3),4(R3)       IF NO DEMO 1 INFO ERROR                      
         BZ    TOTD200                                                          
*        BZ    INVINPT                                                          
*                                                                               
         ICM   R1,15,4(R3)                                                      
         CVD   R1,DUB                                                           
         MVC   PACKPNTS(6),DUB+2                                                
         ZAP   PACKWORK(16),=PL1'0'                                             
         MVC   PACKWORK+6(10),PACKCPP                                           
         MP    PACKWORK(16),PACKPNTS(6)                                         
         AP    PACKWORK(16),=PL3'5000'                                          
         DP    PACKWORK(16),=PL3'10000'                                         
         MP    PACKWORK(13),=PL2'100'                                           
         CVB   RF,PACKWORK+5                                                    
         STCM  RF,15,8(R3)                                                      
TOTD200  OI    4(R3),X'80'         SET TOTAL INDICATOR IN ELEMENT               
         B     TOTD100                                                          
*                                                                               
TOTDEX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  R7                                                               
*                                                                               
*********************************************************************           
*        VALIDATE KEY                                                           
*********************************************************************           
XVK      NTR1  BASE=*,LABEL=*                                                   
         MVI   NETFILT,0                                                        
         MVI   NLISTS,14           SET GENCON LIST NUMBER TO 14                 
         NI    MYFLAG,X'FF'-PLANNED                                             
*                                                                               
         XC    SVMED,SVMED                                                      
         XC    SVCLI,SVCLI                                                      
         XC    SVPRD,SVPRD                                                      
         XC    SVMKT,SVMKT                                                      
         XC    SVEST,SVEST                                                      
         XC    SVDPT,SVDPT                                                      
         XC    SVSLN,SVSLN                                                      
         XC    SVSEC,SVSEC                                                      
         XC    SVAGY,SVAGY                                                      
         XC    SVABOVE,SVABOVE                                                  
         XC    SVPRDA,SVPRDA                                                    
*                                                                               
         LA    R2,GOLMEDH           MEDIA                                       
         CLI   8(R2),C'*'           PLANNED?                                    
         BNE   XVK20                                                            
         OI    SVAGY,X'10'                                                      
         OI    MYFLAG,PLANNED                                                   
         MVC   DUMMYH(8),=X'0900000000010000'                                   
         MVC   DUMMYH+8(1),9(R2)                                                
         LA    R2,DUMMYH                                                        
XVK20    GOTO1 VALIMED                                                          
         MVC   SVMED,BAGYMD                                                     
*                                                                               
         LA    R2,GOLCLIH           CLIENT                                      
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         GOTO1 VALICLT                                                          
         MVC   SVCLI,BCLT                                                       
*                                                                               
         MVI   BPAKG,0                                                          
*                                                                               
         LA    R2,GOLPRDH           PRODUCT                                     
         CLI   5(R2),0                                                          
         BNE   XVK40                                                            
         CLI   ACTNUM,ACTLIST                                                   
         BE    XVK50                                                            
         J     MISSERR                                                          
*                                                                               
XVK40    DS    0H                                                               
         GOTO1 VALIPRD                                                          
         MVC   SVPRD,BPRD                                                       
         MVC   SVPRDA,QPRD                                                      
*                                                                               
XVK50    DS    0H                                                               
         XC    QNET,QNET                                                        
         LA    R2,GOLNETH          NETWORK                                      
         CLI   5(R2),0                                                          
         BE    XVK80                                                            
*                                                                               
XVK60    DS    0H                                                               
         MVI   NETFILT,C'N'                                                     
         CLC   8(2,R2),=C'M='      MEDIA TYPE?                                  
         BNE   XVK65                                                            
*                                                                               
         CLI   10(R2),C'N'         NETWORK?                                     
         BE    XVK80                                                            
*                                                                               
         MVI   NETFILT,C'S'                                                     
         CLI   10(R2),C'S'         SYNDICATION?                                 
         BNE   *+14                                                             
         MVC   SVMKT,=X'0306'      SYNDICATION = 0774                           
         B     XVK80                                                            
*                                                                               
         MVI   NETFILT,C'C'                                                     
         CLI   10(R2),C'C'         CABLE?                                       
         JNE   INVINPT                                                          
         MVC   SVMKT,=X'0307'      CABLE = 0775                                 
         B     XVK80                                                            
*                                                                               
XVK65    DS    0H                                                               
         GOTO1 VALINTWK                                                         
         MVC   SVMKT,QNETMKT                                                    
         MVI   NETFILT,0                                                        
         B     XVK90                                                            
*                                                                               
XVK80    DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    XVK90                                                            
         CLI   N2PROF+15,C'Y'      IS NETWORK REQUIRED ON ADDS                  
         JE    MISSERR             YES ERROR                                    
         MVC   SVMKT,=XL2'0309'    MOVE MARKET '777' INTO KEY                   
*                                                                               
XVK90    DS    0H                                                               
         LA    R2,GOLESTH          ESTIMATE                                     
         CLI   5(R2),0                                                          
         BNE   XVK100                                                           
         CLI   ACTNUM,ACTLIST                                                   
         BE    XVK120                                                           
         J     MISSERR                                                          
*                                                                               
XVK100   DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   XVK110                                                           
         OC    QPRD,QPRD                                                        
         BNZ   XVK110                                                           
         MVC   QPRD,=C'POL'                                                     
*                                                                               
XVK110   GOTO1 VALIEST                                                          
         MVC   SVEST,BEST                                                       
*                                                                               
         L     R3,AIO                                                           
         USING ESTHDR,R3                                                        
         MVC   MYEDAILY,EDAILY                                                  
         GOTO1 DATCON,DMCB,(0,SVBEGIN),(2,MYESTART)                             
         GOTO1 DATCON,DMCB,(0,SVEND),(2,MYEEND)                                 
         DROP  R3                                                               
*                                                                               
XVK120   DS    0H                                                               
         XC    GOLDPTL,GOLDPTL     DAYPART                                      
         OI    GOLDPTLH+6,X'80'                                                 
         LA    R2,GOLDPTH                                                       
         CLI   5(R2),0                                                          
         BNE   XVK130                                                           
         CLI   ACTNUM,ACTLIST                                                   
         BE    XVK140                                                           
         J     INVINPT                                                          
*                                                                               
XVK130   DS    0H                                                               
         OI    GOLDPT+1,X'40'                                                   
         GOTO1 VALIDPT,DMCB,(0,GOLDPT)                                          
         MVC   QDPT,DPTVALUE                                                    
         MVC   SVDPT,QDPT                                                       
         MVC   GOLDPTL,DPTNAME                                                  
*                                                                               
XVK140   DS    0H                                                               
         XC    QLEN,QLEN                                                        
         MVI   NOPTFLG,1                                                        
         LA    R2,GOLLENH           SPOT LENGTH                                 
         GOTO1 VALIFLD                                                          
         LTR   R0,R0                                                            
         BNZ   XVK150                                                           
         CLI   ACTNUM,ACTLIST                                                   
         BE    XVK160                                                           
         J     INVINPT                                                          
*                                                                               
XVK150   DS    0H                                                               
         STC   R0,SVSLN                                                         
         MVC   QLEN,GOLLEN                                                      
*                                                                               
XVK160   DS    0H                                                               
         MVI   NOPTFLG,0                                                        
         CLI   BPAKG,0                                                          
         BE    XVK170                                                           
         OI    SVAGY,X'40'                                                      
         MVC   SVABOVE,BPAKG                                                    
*                                                                               
XVK170   DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    XVK180                                                           
         XC    OPTPER,OPTPER       PERIOD                                       
         LA    R2,GOLPERH                                                       
         CLI   5(R2),0                                                          
         BE    XVK180                                                           
*                                                                               
         GOTO1 VALIDAT                                                          
         GOTO1 DATCON,DMCB,(0,QDATE),(2,OPTPER)                                 
*                                                                               
XVK180   DS    0H                                                               
         LA    R2,GOLOPTH                                                       
         CLI   5(R2),0                                                          
         BE    XVK200                                                           
         MVI   NOPTFLG,1           SET FIELD AS OPTIONAL                        
         MVC   RSNCODE,SPACES                                                   
         BRAS  RE,VOPTN                                                         
*                                                                               
XVK200   DS    0H                                                               
         CLI   CONACT,C'A'                                                      
         BNE   *+8                                                              
         BRAS  RE,SETADD                                                        
*                                                                               
XVK300   DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING NGOLRECD,RF                                                      
*                                                                               
         MVI   GXKEYTYP,GKEYTYPQ                                                
         MVC   GXKEYAM,SVMED                                                    
         MVC   GXKEYCLT,SVCLI                                                   
         MVC   GXKEYPRD,SVPRD                                                   
         MVC   GXKEYMKT,SVMKT                                                   
         MVC   GXKEYEST,SVEST                                                   
         MVC   GXKEYDPT,SVDPT                                                   
         MVC   GXKEYSLN,SVSLN                                                   
         MVC   GXKEYSEC,SVSEC                                                   
         MVC   GXKEYAGY,SVAGY                                                   
         MVC   GXKEYAGY+1(1),SVABOVE                                            
         MVC   GXKPRDA,SVPRDA                                                   
         DROP  RF                                                               
*                                                                               
         MVC   SVXKEY,KEY                                                       
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   XVK320                                                           
*                                                                               
         GOTO1 VSETXSP                                                          
         GOTO1 HIGH                                                             
         B     XVKSPX                                                           
*                                                                               
XVK320   DS    0H                                                               
         GOTO1 VSETXSP                                                          
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(16),KEYSAVE                                                  
         BE    XVKSPX                                                           
*                                                                               
         MVC   SVMKT,=XL2'1E61'   TRY MARKET 7777                               
*                                                                               
         CLC   GOLNET(3),=C'M=N'                                                
         BNE   *+14                                                             
         MVC   SVMKT,=XL2'0309'   TRY MARKET 0777                               
         B     *+12                                                             
*                                                                               
XVK330   DS    0H                                                               
         CLI   GOLNETH+5,0         WAS NETWORK INPUTTED                         
         BE    XVK335                                                           
         CLC   GOLNET(2),=C'M='                                                 
         BE    XVK340                                                           
         MVC   SVMKT,QNETMKT                                                    
         B     XVK340              YES DON'T TEST SECOND DEFAULT                
*                                                                               
XVK335   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SVXKEY                                                       
         LA    RF,KEY                                                           
         USING NGOLRECD,RF                                                      
         MVC   GXKEYMKT,SVMKT                                                   
         DROP  RF                                                               
*                                                                               
         GOTO1 VSETXSP                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE                                                  
         BE    XVKSPX                                                           
*                                                                               
XVK340   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SVXKEY                                                       
         LA    RF,KEY                                                           
         USING NGOLRECD,RF                                                      
         MVC   GXKEYMKT,SVMKT                                                   
         DROP  RF                                                               
*                                                                               
XVKSPX   DS    0H                                                               
         MVC   SVXKEY,KEY                                                       
*                                                                               
XVKX     DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         JE    EXIT                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(32),SVXKEY           RESTORE XSPOT KEY                       
         GOTO1 VSETXSP                                                          
*                                                                               
         CLI   ACTNUM,ACTREST           ACTION RESTORE?                         
         BNE   *+8                                                              
         BRAS  RE,XREST                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE                                                  
         JNE   RECNFND                                                          
         J     EXIT                                                             
*************************************************************                   
*        RESTORE GOAL RECORDS                                                   
*************************************************************                   
XREST    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VSETXSP             RESTORE XSPOT RECORD                         
         MVC   SVXKEY,KEY                                                       
*                                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE                                                  
         JNE   RECNFND                                                          
         GOTO1 GETREC                                                           
*                                                                               
         LA    R7,KEY                                                           
         NI    32(R7),X'FF'-X'80'                                               
         GOTO1 WRITE                                                            
         L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
         NI    34(R7),X'FF'-X'80'                                               
         GOTO1 PUTREC                                                           
*                                                                               
         CLI   GXKEYPRD,0          EXTENDED BRANDS?                             
         BE    XREST50             YES - NO SPOT EXISTS                         
*                                                                               
         GOTO1 VSETSPT             RESTORE SPOT RECORD                          
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R7)                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XREST50                                                          
         GOTO1 GETREC                                                           
*                                                                               
         LA    R7,KEY                                                           
         NI    13(R7),X'FF'-X'80'                                               
         GOTO1 WRITE                                                            
         L     R7,AIO                                                           
         NI    15(R7),X'FF'-X'80'                                               
         GOTO1 PUTREC                                                           
*                                                                               
XREST50  GOTO1 VSETXSP             RESTORE XSPOT RECORD                         
         XC    KEY,KEY                                                          
         MVC   KEY(32),SVXKEY                                                   
         OI    KEY+11,X'20'        GET HISTORY RECORD                           
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE     ANY HISTORY RECORD?                          
         BNE   XRESTX                                                           
         GOTO1 GETREC                                                           
*                                                                               
         LA    R7,KEY                                                           
         NI    32(R7),X'FF'-X'80'                                               
         GOTO1 WRITE                                                            
         L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
         NI    34(R7),X'FF'-X'80'                                               
         GOTO1 PUTREC                                                           
*                                                                               
         CLI   GXKEYPRD,0          EXTENDED BRANDS?                             
         BE    XRESTX              YES - NO SPOT EXISTS                         
*                                                                               
         GOTO1 VSETSPT             RESTORE SPOT RECORD                          
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R7)                                                    
         OI    KEY+11,X'20'        GET HISTORY RECORD                           
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XRESTX                                                           
         GOTO1 GETREC                                                           
*                                                                               
         LA    R7,KEY                                                           
         NI    13(R7),X'FF'-X'80'                                               
         GOTO1 WRITE                                                            
         L     R7,AIO                                                           
         NI    15(R7),X'FF'-X'80'                                               
         GOTO1 PUTREC                                                           
*                                                                               
XRESTX   DS    0H                                                               
         GOTO1 VSETXSP                                                          
         J     EXIT                                                             
         DROP  R7                                                               
*************************************************************                   
*        DELETE GOAL RECORDS                                                    
*************************************************************                   
XDELR    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VSETXSP             DELETE XSPOT RECORD                          
         MVC   SVXKEY,KEY                                                       
*                                                                               
         LA    R7,KEY                                                           
         OI    32(R7),X'80'                                                     
         GOTO1 WRITE                                                            
         L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
         OI    34(R7),X'80'                                                     
         GOTO1 PUTREC                                                           
*                                                                               
         CLI   GXKEYPRD,0          EXTENDED BRANDS?                             
         BE    XDELR50             YES - NO SPOT EXISTS                         
*                                                                               
         GOTO1 VSETSPT             DELETE SPOT RECORD                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R7)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
*                                                                               
         LA    R7,KEY                                                           
         OI    13(R7),X'80'                                                     
         GOTO1 WRITE                                                            
         L     R7,AIO                                                           
         OI    15(R7),X'80'                                                     
         GOTO1 PUTREC                                                           
*                                                                               
XDELR50  GOTO1 VSETXSP             DELETE XSPOT RECORD                          
         XC    KEY,KEY                                                          
         MVC   KEY(32),SVXKEY                                                   
         OI    KEY+11,X'20'        GET HISTORY RECORD                           
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE     ANY HISTORY RECORD?                          
         BNE   XDELRX                                                           
         GOTO1 GETREC                                                           
*                                                                               
         LA    R7,KEY                                                           
         OI    32(R7),X'80'                                                     
         GOTO1 WRITE                                                            
         L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
         OI    34(R7),X'80'                                                     
         GOTO1 PUTREC                                                           
*                                                                               
         CLI   GXKEYPRD,0          EXTENDED BRANDS?                             
         BE    XDELRX              YES - NO SPOT EXISTS                         
*                                                                               
         GOTO1 VSETSPT             DELETE SPOT RECORD                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R7)                                                    
         OI    KEY+11,X'20'        GET HISTORY RECORD                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
*                                                                               
         LA    R7,KEY                                                           
         OI    13(R7),X'80'                                                     
         GOTO1 WRITE                                                            
         L     R7,AIO                                                           
         OI    15(R7),X'80'                                                     
         GOTO1 PUTREC                                                           
*                                                                               
XDELRX   DS    0H                                                               
         GOTO1 VSETXSP                                                          
         J     EXIT                                                             
         DROP  R7                                                               
*************************************************************                   
*        VALIDATE XSPOT RECORD                                                  
*************************************************************                   
XVR      NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2            BUILD XSP RECORD IN AIO2                     
         NI    SVXKEY+11,X'FF'-X'20'    MAKE SURE IT'S NOT HISTORY KEY          
         XC    KEY,KEY                                                          
         MVC   KEY,SVXKEY                                                       
         L     RF,AIO                                                           
         MVC   0(32,RF),KEY                                                     
         MVC   32(2,RF),=X'002A'                                                
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    XVR010                                                           
*                                                                               
         GOTO1 VSETXSP                                                          
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(16),KEY                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
XVR010   DS    0H                                                               
         L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
*                                                                               
         BRAS  RE,SETDAYX                                                       
         MVI   TOTDSW,X'00'                                                     
         MVI   CHGSW,0                                                          
         MVI   INPSW,0                                                          
         XC    TOTDLRS,TOTDLRS                                                  
         XC    SVTGWKC,SVTGWKC                                                  
         XC    TOTPNTS,TOTPNTS                                                  
         LA    R2,GOLDT01H                                                      
         LA    R3,GOLDT01H                                                      
         LA    R5,14                                                            
         MVI   BLNKSW,0                                                         
         XC    WORK2(71),WORK2                                                  
         MVI   WORK2,X'FF'                                                      
*                                                                               
         CLI   CONACT,C'A'                                                      
         BNE   XVR050                                                           
         BRAS  RE,CHKINPT                                                       
*                                                                               
*--VALIDATE PLANNER FIELD                                                       
XVR050   LA    RE,GOLPLNH                                                       
         TM    4(RE),X'20'                                                      
         BNZ   XVR100                                                           
         MVI   CHGSW,X'FF'                                                      
*                                                                               
*--VALIDATE OPTIONS FIELD                                                       
XVR100   DS    0H                                                               
         MVI   NOPTFLG,1           SET FIELD AS OPTIONAL                        
         LA    R2,GOLOPTH                                                       
         MVC   RSNCODE,SPACES                                                   
         TM    4(R2),X'20'                                                      
         BNZ   XVR110                                                           
         BRAS  RE,VOPTN                                                         
         LA    R2,GOLDT01H                                                      
*                                                                               
*--VALIDATE PERIOD FIELD                                                        
*                                                                               
XVR110   DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    XVR130                                                           
         MVI   INPSW,X'FF'         DATA IS INPUTTED ON SCREEN                   
         TM    4(R2),X'20'                                                      
         BNZ   XVR120                                                           
         MVI   CHGSW,X'FF'                                                      
*                                                                               
XVR120   DS    0X                                                               
         CLI   MYEDAILY,C'Y'                                                    
         BNE   XVR125                                                           
*                                                                               
         BRAS  RE,MYVPER                                                        
         BRAS  RE,NEXTUN                                                        
         B     XVR150                                                           
*                                                                               
XVR125   GOTO1 VALIDTE             VALIDATE PERIOD FIELD                        
         BRAS  RE,NEXTUN                                                        
         B     XVR150                                                           
*                                                                               
*--IF NO DATE INPUT REST OF LINE MUST BE BLANK                                  
XVR130   LA    RF,4                                                             
XVR140   BRAS  RE,NEXTUN                                                        
         CLI   5(R2),0                                                          
         JNE   INVINPT                                                          
         BCT   RF,XVR140                                                        
         BRAS  RE,NEXTUN           POINT R2 TO NEXT LINE                        
         B     XVR450                                                           
*                                                                               
*--VALIDATE DOLLAR FIELD                                                        
*                                                                               
XVR150   TM    4(R2),X'20'                                                      
         BNZ   XVR160                                                           
         MVI   CHGSW,X'FF'                                                      
XVR160   CLI   5(R2),0                                                          
         BE    XVR200                                                           
         CLI   TOTDSW,X'FF'        DOLLAR AMOUNT NOT ALLOWED                    
         JE    INVINPT             WHEN TOTAL DOLLAR REQUESTED                  
*                                                                               
XVR165   MVI   BLNKSW,X'FF'        THERE IS DATA ON THIS LINE                   
         CLC   8(2,R2),=CL2'DD'                                                 
         BNE   XVR170                                                           
         LA    R2,GOLDT02H-GOLDL01H(R2)     POINT R2 TO NEXT LINE               
         B     XVR350                                                           
XVR170   CLI   8(R2),C'T'                                                       
         BNE   XVR190                                                           
         CLI   5(R2),C'1'                                                       
         JE    INVINPT                                                          
         MVI   TOTDSW,X'FF'                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,MYWORKD),(2,SVTGWKC)                              
*                                                                               
XVR190   GOTO1 =A(VALIDOL),DMCB,RR=Y                                            
*                                                                               
XVR200   BRAS  RE,NEXTUN                                                        
*                                                                               
*--VALIDATE DEMO1 FIELD                                                         
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   XVR210                                                           
         MVI   CHGSW,X'FF'                                                      
XVR210   CLI   5(R2),0                                                          
         BNE   XVR220                                                           
         CLI   TOTDSW,X'FF'        IF TOTAL DOLLARS REQUESTED                   
         BE    *+12                THERE MUST BE INPUT IN THIS FIELD            
         B     XVR250                                                           
XVR220   GOTO1 =A(VALIPTS),DMCB,RR=Y                                            
         MVI   BLNKSW,X'FF'        THERE IS DATA ON THIS LINE                   
XVR250   BRAS  RE,NEXTUN                                                        
*                                                                               
*--VALIDATE DEMO2 FIELD                                                         
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   XVR260                                                           
         MVI   CHGSW,X'FF'                                                      
XVR260   CLI   5(R2),0                                                          
         BE    XVR280                                                           
         MVI   BLNKSW,X'FF'        THERE IS DATA ON THIS LINE                   
         CLI   GOLDM2H+5,0                                                      
         JE    INVINPT                                                          
         GOTO1 =A(VALIPTS),DMCB,RR=Y                                            
XVR280   BRAS  RE,NEXTUN                                                        
*                                                                               
*--VALIDATE DEMO3 FIELD                                                         
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   XVR285                                                           
         MVI   CHGSW,X'FF'                                                      
XVR285   CLI   5(R2),0                                                          
         BE    XVR290                                                           
         MVI   BLNKSW,X'FF'        THERE IS DATA ON THIS LINE                   
         CLI   GOLDM2H+5,0                                                      
         JE    INVINPT                                                          
         GOTO1 =A(VALIPTS),DMCB,RR=Y                                            
XVR290   BRAS  RE,NEXTUN                                                        
*                                                                               
*--VALIDATE DAY FIELD                                                           
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   XVR300                                                           
         MVI   CHGSW,X'FF'                                                      
XVR300   CLI   5(R2),0                                                          
         BE    XVR320                                                           
*                                                                               
         CLC   8(2,R2),=C'W='                                                   
         BE    XVR320                                                           
*                                                                               
         MVI   BLNKSW,X'FF'        THERE IS DATA ON THIS LINE                   
         ZIC   R4,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R4),8(R2)),FULL,FULL+1                             
         CLI   FULL,0                                                           
         JE    INVINPT                                                          
XVR320   BRAS  RE,NEXTUN                                                        
*                                                                               
XVR350   LA    R3,25(R3)           POINT R3 TO DOLLAR FIELD                     
         CLI   BLNKSW,X'FF'                                                     
         BE    XVR400              THERE IS DATA FOR THIS LINE                  
         CLI   WORK2,X'FF'         FIRST LINE MUST HAVE DATA                    
         JE    INVINPT                                                          
         MVC   27(5,R3),WORK2+27   MOVE DEMO1                                   
         MVC   24(1,R3),WORK2+24   MOVE DEMO1 LENGTH OUT                        
         MVC   40(5,R3),WORK2+40   MOVE DEMO2                                   
         MVC   37(1,R3),WORK2+37   MOVE DEMO2 LENGTH OUT                        
         MVC   53(5,R3),WORK2+53   MOVE DEMO3                                   
         MVC   50(1,R3),WORK2+50   MOVE DEMO3 LENGTH OUT                        
         MVC   66(5,R3),WORK2+66   MOVE DAY                                     
         MVC   63(1,R3),WORK2+63   MOVE DAY LENGTH OUT                          
         CLI   TOTDSW,X'FF'        IS TOTAL DOLLARS USED                        
         BE    XVR400              DONT CARRY OVER DOLLAR AMOUNT                
         MVC   8(11,R3),WORK2+8    MOVE DOLLAR VALUE OUT                        
         MVC   5(1,R3),WORK2+5     MOVE DOLLAR LENGTH OUT                       
*                                                                               
XVR400   MVC   WORK2(71),0(R3)     MOVE LINE INFO TO SAVE LOCATION              
         LR    R3,R2               POSITION R3 TO NEXT LINE                     
         MVI   BLNKSW,0                                                         
XVR450   BCT   R5,XVR110                                                        
         LA    R2,GOLDT01H                                                      
         CLI   INPSW,X'FF'         IF NO INPUT ON SCREEN                        
         JNE   INVINPT             ERROR                                        
         CLI   CHGSW,X'FF'                                                      
         BE    XVR600                                                           
         MVC   STSCREEN,ENSCREEN                                                
         B     XVR940                                                           
*                                                                               
XVR600   CLC   RSNCODE,SPACES      REASON CODE SUPPLIED?                        
         BNE   XVR610                                                           
         CLI   ACTNUM,ACTADD       NO, BUT OK FOR ADD                           
         BE    XVR610                                                           
         LA    R2,GOLOPTH                                                       
         CLI   N0PROF+7,C'Y'       REQUIRED IN PROFILE?                         
         JE    RSNCDERR            YES, PRINT ERROR MESSAGE                     
*--CREATE ELEMENTS PUT THEM IN GOALREC                                          
XVR610   MVI   TOTDSW,0                                                         
         LA    R2,GOLDT01H                                                      
*                                                                               
         MVI   ELCODE,X'42'        DELETE NETWORK TOTAL ELEMENT                 
         BRAS  RE,DELELX                                                        
*                                                                               
XVR650   DS    0H                                                               
         CLI   5(R2),0                                                          
         BNE   XVR680                                                           
         LA    R2,GOLDY01H-GOLDT01H(R2)                                         
         B     XVR920                                                           
*                                                                               
***-----NEW CODE TO ACCOMODATE DAILY GOALS  (BPOO  4/18/98) *****               
*                                                                               
XVR680   DS    0H                                                               
         CLI   MYEDAILY,C'Y'                                                    
         BNE   XVR683                                                           
*                                                                               
         BRAS  RE,MYVPER                                                        
         B     XVR686                                                           
*                                                                               
XVR683   GOTO1 VALIDTE             VALIDATE PERIOD FIELD                        
*                                                                               
XVR686   LA    R4,BWEEKS                                                        
*                                                                               
*--DELETE ALL 21 ELEMENTS TO BE UPDATED                                         
*                                                                               
XVR700   MVI   ELCODE,X'21'                                                     
         GOTO1 HELLO,DMCB,(C'D',=C'XSPFIL '),(ELCODE,0(R7)),           X        
               (X'02',0(R4))                                                    
         LA    R4,2(R4)                                                         
         OC    0(2,R4),0(R4)                                                    
         BNZ   XVR700                                                           
*                                                                               
XVR740   XC    ELEM(33),ELEM                                                    
         MVI   ELEM,X'21'                                                       
         MVI   ELEM+1,X'21'       GLEN5Q                                        
*                                                                               
         LA    R4,BWEEKS                                                        
*                                                                               
XVR750   MVC   ELEM+2(2),0(R4)     MOVE WEEK FIELD TO ELEMENT                   
         BRAS  RE,NEXTUN                                                        
*                                                                               
         CLC   8(2,R2),=CL2'DD'    IS DELETE LINE REQUESTED                     
         BNE   XVR770              NO BYPASS THIS CODE                          
         XC    ELEM+2(2),ELEM+2                                                 
         LA    R2,GOLDY01H-GOLDL01H(R2)     POINT R2 TO NEXT LINE               
         B     XVR920                                                           
*                                                                               
XVR770   BRAS  RE,CALCDOL          MOVE DOLLAR AMOUNT TO ELEMENT                
         BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   5(R2),0             MOVE DEMO 1 VALUE TO ELEMENT                 
         BNE   XVR790                                                           
         CLI   TOTDSW,X'FF'        TOTAL DOLLARS REQUESTED                      
         BNE   XVR800              NO GO TO NEXT FIELD                          
         XC    ELEM+8(4),ELEM+8    YES, CLEAR DOLLAR FIELD                      
         B     XVR800                                                           
XVR790   XC    DMCB+4(4),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(1,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM+4(4),DMCB+4                                                 
XVR800   BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   5(R2),0             MOVE DEMO 2 VALUE TO ELEMENT                 
         BE    XVR870                                                           
         XC    DMCB+4(4),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(1,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM+12(4),DMCB+4                                                
XVR870   BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   5(R2),0             MOVE DEMO 3 VALUE TO ELEMENT                 
         BE    XVR880                                                           
         XC    DMCB+4(4),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(1,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM+17(4),DMCB+4                                                
XVR880   BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   5(R2),0             MOVE DAY VALUE TO ELEMENT                    
         BE    XVR900                                                           
*                                                                               
         CLC   8(2,R2),=C'W='     WORKHORSE?                                    
         BNE   XVR895                                                           
         MVI   ELEM+21,C'W'       WORKHORSE                                     
         MVC   ELEM+22(3),10(R2)                                                
         OC    ELEM+22(3),SPACES                                                
         B     XVR900                                                           
*                                                                               
XVR895   DS    0H                                                               
         ZIC   R5,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R5),8(R2)),ELEM+16,FULL                            
         CLI   ELEM+16,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
XVR900   BRAS  RE,PUTELX                                                        
*                                                                               
         LA    R4,2(R4)                                                         
         MVC   ELEM+2(2),0(R4)                                                  
         OC    0(2,R4),0(R4)                                                    
         BNZ   XVR900                                                           
*                                                                               
XVR920   XC    ELEM(33),ELEM                                                    
         BRAS  RE,NEXTUN                                                        
         CLI   0(R2),9                                                          
         BNE   XVR650                                                           
         CLI   TOTDSW,X'FF'                                                     
         BNE   XVR940                                                           
         BRAS  RE,ADDPTSX                                                       
         BRAS  RE,TOTDOLR                                                       
*                                                                               
*--MOVE PLANNER CODE OUT                                                        
*                                                                               
XVR940   LA    R3,42(R7)                                                        
         CLI   0(R3),X'20'                                                      
         BNE   XVR950                                                           
         MVC   5(12,R3),GOLPLN                                                  
         MVC   22(4,R3),QNET                                                    
         NI    17(R3),X'7F'        CHECK FOR PRIORITY BRAND                     
         CLI   GOLPRD,C'*'                                                      
         BNE   *+8                                                              
         OI    17(R3),X'80'        SET PRIORITY INDICATOR                       
*                                                                               
XVR945   BRAS  RE,ACTIVITX                                                      
         BRAS  RE,NETTOTX                                                       
         B     XVR1000                                                          
XVR950   XC    ELEM(76),ELEM                                                    
         MVC   ELEM(2),=XL2'204C'                                               
         MVC   ELEM+5(12),GOLPLN                                                
         MVC   ELEM+21(1),STDAY                                                 
         MVC   ELEM+22(4),QNET                                                  
         CLI   GOLPRD,C'*'                                                      
         BNE   *+8                                                              
         OI    ELEM+17,X'80'       SET PRIORITY INDICATOR                       
         BRAS  RE,PUTELX                                                        
*                                                                               
         BRAS  RE,ACTIVITX                                                      
         BRAS  RE,NETTOTX                                                       
         B     XVR1000                                                          
*                                                                               
XVR1000  DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    XVR1020                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(32),SVXKEY                                                   
         MVC   AIO,AIO3                                                         
         GOTO1 VSETXSP                                                          
         GOTO1 HIGH                                                             
         GOTO1 GETREC              RESTORE GETREC/PUTREC                        
         MVC   AIO,AIO2                                                         
         BRAS  RE,TSTDATE                                                       
         GOTO1 PUTREC                                                           
         MVI   XSPACT,C'C'         CHANGING RECORD                              
         B     XVR1050                                                          
*                                                                               
XVR1020  DS    0H                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 VSETXSP                                                          
         BRAS  RE,TSTDATE                                                       
         GOTO1 ADDREC                                                           
         MVI   XSPACT,C'A'         ADDING RECORD                                
*                                                                               
XVR1050  DS    0H                                                               
         L     RF,AIO2             POINT TO XSPOT RECORD                        
         CLI   4(RF),0             ANY PRODUCT EQUATE                           
         BE    *+8                                                              
         BRAS  RE,ADDSPT           ADD SPOT RECORD AS WELL                      
*                                                                               
         BRAS  RE,XHSTREC                                                       
         CLI   ACTNUM,ACTSEL       IF SELECT HANDLE MULTI SCREEN                
         BNE   XVRX                                                             
*                                                                               
XVRSCRN  CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   XVRS040                                                          
         OC    ENSCREEN,ENSCREEN   IS THERE ANOTHER SCREEN TO DISPLAY           
         BZ    XVRS040                                                          
         CLI   THISLSEL,C'S'       IS IT A SELECT                               
         BE    XVRS030                                                          
         CLI   THISLSEL,C'A'       IS IT AN ALTER                               
         BNE   XVRS040                                                          
*                                                                               
         ZIC   RE,SELLISTN         FIND SELECT CODE IN LIST DIRECTORY           
         MH    RE,=H'6'                                                         
         LA    RE,LISTDIR(RE)                                                   
         MVI   0(RE),C'C'          AND RESET CONTROLLER'S SELECT CODE           
XVRS030  OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
*                                                                               
XVRS040  DS    0H                                                               
*                                                                               
XVRX     DS    0H                                                               
         BRAS  RE,XDR                                                           
         J     EXIT                                                             
         LTORG                                                                  
********************************************************************            
*        LIST XFILE RECORDS                                                     
********************************************************************            
XLR      NTR1  BASE=*,LABEL=*                                                   
         MVI   KEYB4MKT,4                                                       
         MVI   KEYAFMKT,0                                                       
*                                                                               
         BRAS  RE,SETOPT                                                        
         MVC   AIO,AIO2                                                         
*                                                                               
         OC    KEY(17),KEY                                                      
         BNZ   XLR100                                                           
         MVC   KEY,SVXKEY                                                       
*                                                                               
XLR100   GOTO1 VSETXSP                                                          
         GOTO1 HIGH                                                             
         B     XLR220                                                           
*                                                                               
XLR200   GOTO1 VSETXSP                                                          
         GOTO1 SEQ                                                              
*                                                                               
XLR220   DS    0H                                                               
         LA    R7,KEY                                                           
         USING NGOLRECD,R7                                                      
         CLC   GXKEYCLT,BCLT                                                    
         BNE   XLREXT                                                           
         CLC   SVXKEY(4),KEY                                                    
         BNE   XLREXT                                                           
         CLI   GOLMED,C'*'               PLANNED?                               
         BNE   XLR225                                                           
         TM    GXKEYAGY,GXKEYTAR                                                
         BZ    XLR200                                                           
*                                                                               
XLR225   DS    0H                                                               
         TM    GXKEYAGY,X'20'              CHECK IF A HISTORY RECORD            
         BO    XLR200                    YES, SO SKIP IT                        
         ZIC   RF,KEYB4MKT                                                      
         BCTR  RF,0                                                             
         EX    RF,FRONTCKX                                                      
         BNE   XLR200                                                           
*                                                                               
XLR230   DS    0H                                                               
         OC    KEYAFMKT,KEYAFMKT                                                
         BZ    XLR232                                                           
         ZIC   RF,KEYAFMKT                                                      
         BCTR  RF,0                                                             
         EX    RF,BACKCKX                                                       
         BNE   XLR200                                                           
*                                                                               
*  CHECK THE FILTERS                                                            
*                                                                               
XLR232   OC    OPTEST,OPTEST                                                    
         BZ    XLR235                                                           
         CLC   OPTEST,GXKEYEST                                                  
         BNE   XLR200                                                           
XLR235   OC    OPTDAYP,OPTDAYP                                                  
         BZ    XLR240                                                           
         CLC   OPTDAYP,GXKEYDPT                                                 
         BNE   XLR200                                                           
XLR240   OC    OPTLEN,OPTLEN                                                    
         BZ    XLR250                                                           
         CLC   OPTLEN,GXKEYSLN                                                  
         BNE   XLR200                                                           
*                                                                               
XLR250   DS    0H                                                               
         CLI   NETFILT,0                                                        
         BE    XLR255                                                           
         CLI   NETFILT,C'N'        NETWORK FILTER?                              
         BNE   XLR255                                                           
         CLC   GXKEYMKT,=X'0309'   0777?                                        
         BE    XLR260                                                           
         B     XLR200                                                           
*                                                                               
XLR255   DS    0H                                                               
         CLI   NETFILT,C'S'        SYNDICATION?                                 
         BNE   XLR258                                                           
         CLC   GXKEYMKT,=X'0306'   0774?                                        
         BE    XLR260                                                           
         B     XLR200                                                           
*                                                                               
XLR258   DS    0H                                                               
         CLI   NETFILT,C'C'        CABLE?                                       
         BNE   XLR260                                                           
         CLC   GXKEYMKT,=X'0307'   0775?                                        
         BNE   XLR200                                                           
*                                                                               
XLR260   MVC   SVXKEY,KEY                                                       
         OC    SVCLI,SVCLI                                                      
         BZ    *+14                                                             
         CLC   GXKEYCLT,SVCLI       SAME CLIENT?                                
         BNE   XLR200                                                           
*                                                                               
         OC    SVPRDA,SVPRDA                                                    
         BZ    *+14                                                             
         CLC   GXKPRDA,SVPRDA       SAME PRODUCT?                               
         BNE   XLR200                                                           
*                                                                               
         OC    SVMKT,SVMKT                                                      
         BZ    *+14                                                             
         CLC   GXKEYMKT,SVMKT                                                   
         BNE   XLR200                                                           
*                                                                               
         OC    SVDPT,SVDPT                                                      
         BZ    *+14                                                             
         CLC   GXKEYDPT,SVDPT                                                   
         BNE   XLR200                                                           
*                                                                               
         OC    SVEST,SVEST                                                      
         BZ    *+14                                                             
         CLC   GXKEYEST,SVEST                                                   
         BNE   XLR200                                                           
*                                                                               
         OC    SVSLN,SVSLN                                                      
         BZ    *+14                                                             
         CLC   GXKEYSLN,SVSLN                                                   
         BNE   XLR200                                                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R7,AIO                                                           
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
         LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
*                                                                               
         LA    R2,LRPROD                                                        
         LA    R3,42(R7)                                                        
         TM    17(R3),X'80'        CHECK PRIORITY PRODUCT BIT                   
         BZ    XLR310                                                           
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
XLR310   MVC   0(3,R2),GXKPRDA                                                  
*                                                                               
         TM    GXKEYAGY,X'10'      PLANNED GOAL?                                
         BZ    *+8                                                              
         MVI   LRPLAN,C'*'                                                      
*                                                                               
*--MOVE PACKAGE CODE OUT IF ON THE RECORD                                       
*                                                                               
         TM    GXKEYAGY,X'40'                                                   
         BZ    XLR360                                                           
         LA    RE,3                                                             
XLR320   CLI   0(R2),X'40'                                                      
         BE    XLR330                                                           
         LA    R2,1(R2)                                                         
         BCT   RE,XLR320                                                        
XLR330   MVI   0(R2),C'/'                                                       
         EDIT  (1,GXKEYPR2),(3,1(R2)),ALIGN=LEFT                                
*                                                                               
*--MOVE NETWORK CODE OUT IF ON THE RECORD                                       
*                                                                               
XLR360   LA    R3,42(R7)                                                        
         USING GDELEM,R3                                                        
         CLI   0(R3),X'20'                                                      
         BNE   XLR370                                                           
         CLI   GDNETWK,X'40'                                                    
         BNH   XLR370                                                           
         MVC   LRNET,GDNETWK                                                    
         B     XLR380                                                           
         DROP  R3                                                               
*                                                                               
XLR370   DS    0H                                                               
         CLC   GXKEYMKT,=X'0309'                                                
         BNE   *+12                                                             
         MVI   LRNET,C'N'         DEFAULT FOR NETWORK                           
         B     XLR380                                                           
*                                                                               
         CLC   GXKEYMKT,=X'0306'                                                
         BNE   *+12                                                             
         MVI   LRNET,C'S'         SYNDICATION                                   
         B     XLR380                                                           
*                                                                               
         CLC   GXKEYMKT,=X'0307'                                                
         BNE   *+12                                                             
         MVI   LRNET,C'C'         CABLE                                         
         B     XLR380                                                           
*                                                                               
*--MOVE ESTIMATE OUT TO THE LIST LINE                                           
*                                                                               
XLR380   XC    LREST,LREST                                                      
         EDIT  (1,GXKEYEST),(3,LREST),ALIGN=LEFT                                
*                                                                               
*--MOVE DAYPART OUT TO THE LIST LINE                                            
*                                                                               
         GOTO1 VALIDPT,DMCB,(1,GXKEYDPT)                                        
         GOTO1 VSETXSP                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(32),SVXKEY                                                   
         GOTO1 HIGH                 REPOSITION THE POINTER                      
         GOTO1 GETREC                                                           
*                                                                               
         MVC   LRDPT(8),DPTNAME                                                 
*                                                                               
*--MOVE LENGTH OUT TO THE LIST LINE                                             
*                                                                               
         XC    LRLEN,LRLEN                                                      
         EDIT  (1,GXKEYSLN),(3,LRLEN),ALIGN=LEFT                                
*                                                                               
*--MOVE TOTAL COST AND DEMO1 TOTAL TO THE LIST LINE                             
*                                                                               
         XC    WORK2(4),WORK2                                                   
         LA    R3,42(R7)                                                        
         XC    LISTDOL,LISTDOL                                                  
         XC    LISTPNT1,LISTPNT1                                                
         MVI   ELCODE,X'21'                                                     
XLR400   BRAS  RE,NEXTEL                                                        
         BNE   XLR450                                                           
*                                                                               
*  CHECK FILTERS                                                                
*                                                                               
         OC    OPTSPER,OPTSPER     DATE FILTERS                                 
         BZ    XLR420                                                           
         CLC   OPTSPER,2(R3)                                                    
         BH    XLR400                                                           
         CLC   OPTEPER,2(R3)                                                    
         BL    XLR400                                                           
*                                                                               
XLR420   OC    WORK2(2),WORK2                                                   
         BNZ   XLR440                                                           
         MVC   WORK2(2),2(R3)                                                   
*                                                                               
XLR440   MVC   WORK2+2(2),2(R3)                                                 
         ICM   RF,15,8(R3)                                                      
         A     RF,LISTDOL          ADD DOLLARS                                  
         ST    RF,LISTDOL                                                       
         L     RF,LISTPNT1                                                      
         A     RF,4(R3)            ADD POINTS                                   
         ST    RF,LISTPNT1                                                      
         B     XLR400                                                           
*                                                                               
XLR450   OC    LISTDOL,LISTDOL                                                  
         BZ    XLR460                                                           
         L     RF,LISTDOL                                                       
         SR    RE,RE                                                            
         D     RE,=F'100'          REMOVE PENNIES                               
         ST    RF,LISTDOL                                                       
         EDIT  (4,LISTDOL),(10,LRCOST),ALIGN=LEFT                               
*                                                                               
XLR460   OC    LISTPNT1,LISTPNT1                                                
         BZ    XLR470                                                           
         EDIT  (4,LISTPNT1),(8,LRDEMO),1,ALIGN=LEFT                             
*                                                                               
*--MOVE S-E PERIODS TO LIST LINE                                                
*                                                                               
XLR470   OC    WORK2(4),WORK2                                                   
         BZ    XLR500                                                           
         GOTO1 DATCON,DMCB,(2,WORK2),(5,LRDAT)                                  
         MVI   LRDAT+8,C'-'                                                     
         GOTO1 DATCON,DMCB,(2,WORK2+2),(5,LRDAT+9)                              
*                                                                               
XLR500   GOTO1 LISTMON                                                          
         B     XLR200              GOTO READ SEQ                                
*                                                                               
XLREXT   DS    0H                                                               
         J     EXIT                                                             
*                                                                               
FRONTCKX CLC   SVXKEY(0),KEY                                                    
BACKCKX  CLC   SVXKEY+7(0),KEY+7                                                
         LTORG                                                                  
*****************************************************************               
XDK      NTR1  BASE=*,LABEL=*                                                   
         OC    ENSCREEN,ENSCREEN   IS THERE ANOTHER SCREEN TO DISPLAY           
         BZ    XDK010                                                           
*                                                                               
         GOTO1 VSETXSP                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(32),HOLDXKEY                                                 
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
XDK010   L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
*                                                                               
         MVC   HOLDXKEY,KEY                                                     
*                                                                               
         MVC   GOLMED(2),=C'N '                                                 
         TM    GXKEYAGY,GXKEYTAR   PLANNED GOAL?                                
         BZ    *+10                                                             
         MVC   GOLMED(2),=C'*N'                                                 
         FOUT  GOLMEDH                                                          
*                                                                               
         MVC   GOLCLI(3),QCLT                                                   
         FOUT  GOLCLIH                                                          
*                                                                               
         XC    GOLPRD,GOLPRD                                                    
         MVC   GOLPRD(3),GXKPRDA                                                
*                                                                               
         TM    GXKEYAGY,X'40'      WAS PACKAGE INPUTTED                         
         BZ    XDK080                                                           
*                                                                               
*--MOVE PACKAGE OUT                                                             
*                                                                               
         LA    R2,GOLPRD+2                                                      
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'*'                                                       
         AHI   R2,1                                                             
         EDIT  (1,GXKEYAGY),(3,0(R2)),ALIGN=LEFT                                
*                                                                               
XDK080   FOUT  GOLPRDH                                                          
*                                                                               
         XC    GOLNET,GOLNET                                                    
*                                                                               
         LA    R3,42(R7)                                                        
         CLI   0(R3),X'20'                                                      
         BNE   XDK090                                                           
         CLI   22(R3),X'40'                                                     
         BNH   XDK090                                                           
         MVC   GOLNET(4),22(R3)                                                 
         B     XDK095                                                           
*                                                                               
XDK090   CLC   GXKEYMKT,=X'0309'       DEFAULT TO NETWORK                       
         BNE   *+14                   0777                                      
         MVC   GOLNET(3),=C'M=N'                                                
         B     XDK095                                                           
*                                                                               
         CLC   GXKEYMKT,=X'0306'       SYNDICATION?                             
         BNE   *+14                                                             
         MVC   GOLNET(3),=C'M=S'                                                
         B     XDK095                                                           
*                                                                               
         CLC   GXKEYMKT,=X'0307'       CABLE?                                   
         BNE   *+10                                                             
         MVC   GOLNET(3),=C'M=C'                                                
*                                                                               
XDK095   FOUT  GOLNETH                                                          
*                                                                               
XDK100   DS    0H                                                               
         EDIT  (1,GXKEYEST),(3,GOLEST),ALIGN=LEFT                               
         FOUT  GOLESTH                                                          
*                                                                               
         MVC   QPRD,GXKPRDA                                                     
         GOTO1 DISEST,GXKEYEST                                                  
         L     RE,AIO3                                                          
         USING ESTHDR,RE                                                        
         MVC   MYEDAILY,EDAILY                                                  
         MVC   TUSRNM,EUSRNMS                                                   
         DROP  RE                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
XDK200   XC    GOLDPTL,GOLDPTL                                                  
         XC    GOLDPT,GOLDPT                                                    
         LA    R2,GOLDPTH                                                       
         GOTO1 VALIDPT,DMCB,(1,GXKEYDPT)                                        
         MVC   GOLDPT,DPTCODE                                                   
         MVC   GOLDPTL,DPTNAME                                                  
XDK250   FOUT  GOLDPTH                                                          
         FOUT  GOLDPTLH                                                         
*                                                                               
         GOTO1 VSETXSP                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(32),HOLDXKEY                                                 
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
XDK300   EDIT  (1,GXKEYSLN),(3,GOLLEN),ALIGN=LEFT                               
         FOUT  GOLLENH                                                          
*                                                                               
XDK400   MVC   KEY,HOLDXKEY                                                     
         GOTO1 VSETXSP                                                          
         J     EXIT                                                             
         LTORG                                                                  
********************************************************************            
XDR      NTR1  BASE=*,LABEL=*                                                   
         L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
         XC    GOLOPT,GOLOPT                                                    
         FOUT  GOLOPTH                                                          
*                                                                               
         CLI   N0PROF+7,C'Y'       REQUIRED IN PROFILE?                         
         BNE   XDR005              YES, PRINT ERROR MESSAGE                     
         OC    RSNCODE,RSNCODE                                                  
         BZ    XDR005                                                           
         CLC   RSNCODE,SPACES                                                   
         BE    XDR005                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BE    XDR005                                                           
         MVC   GOLOPT(3),=C'RS='                                                
         MVC   GOLOPT+3(L'RSNCODE),RSNCODE                                      
*                                                                               
XDR005   LA    R3,42(R7)                                                        
         USING GDELEM,R3                                                        
         MVC   SVNETWK,GDNETWK                                                  
         DROP  R3                                                               
*                                                                               
         TM    17(R3),X'80'        CHECK PRIORITY PRODUCT BIT                   
         BZ    XDR010                                                           
         CLI   GOLPRD,C'*'                                                      
         BE    XDR010                                                           
         MVC   FULL(3),GOLPRD                                                   
         MVC   GOLPRD+1(3),FULL                                                 
         MVI   GOLPRD,C'*'         SET INDICATOR                                
         FOUT  GOLPRDH                                                          
*                                                                               
XDR010   LA    R4,SYSSPARE+500                                                  
         USING DBLOCKD,R4                                                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=CL3'NAD'                                                 
         MVI   DBSELMED,C'N'                                                    
         XC    WORK2,WORK2                                                      
*                                                                               
         CLC   AGENCY,=C'SJ'                                                    
         BE    *+14                                                             
         CLC   AGENCY,=C'MC'             ONLY FOR MC CANN                       
         BNE   XDR01050                                                         
*                                                                               
         OC    ESTTRGL,ESTTRGL           ANY TARGET DEMOS?                      
         BZ    XDR01050                                                         
*                                                                               
         LA    R2,GOLTARH                                                       
         GOTO1 DEMOCON,DMCB,(2,ESTTRGL),(10,WORK2),(C'S',DBLOCK),TUSRNM         
*                                                                               
         MVC   GOLTAR(10),WORK2          DISPLAY 1ST TARGET                     
         OI    GOLTARH+4,X'20'           SET PREVALID BIT                       
         FOUT  GOLTARH                                                          
*                                                                               
         LA    R2,GOLDM2H                                                       
         CLC   WORK2(10),WORK2+10        ARE T1 AND T2 THE SAME?                
         BE    XDR01010                                                         
*                                                                               
         OC    WORK2+10(10),WORK2+10     IS THERE A 2ND TARGET?                 
         BZ    XDR01010                   NO                                    
         MVC   GOLDM2(10),WORK2+10       YES - THEN DISPLAY IT                  
         MVI   GOLDM2H+5,10                                                     
         OI    GOLDM2H+4,X'20'           SET PREVALID BIT                       
         FOUT  GOLDM2H                                                          
         LA    R2,GOLDM3H                                                       
*                                                                               
XDR01010 DS    0H                                                               
         LA    R3,ESTDEMO                                                       
         LA    R5,20                                                            
*                                                                               
XDR01015 DS    0H                                                               
         CLC   ESTTRGL(3),0(R3)          CHECK IF DEMO TO DISPLAY               
         BE    *+14                      WAS DISPLAYED AS A TARGET              
         CLC   ESTTRGL+3(3),0(R3)                                               
         BNE   XDR01020                                                         
*                                                                               
         LA    R3,3(R3)                                                         
         BCT   R5,XDR01015                                                      
         B     XDR025                                                           
*                                                                               
XDR01020 MVC   DISDEMO,0(R3)                                                    
         GOTO1 DEMOCON,DMCB,(1,DISDEMO),(10,WORK2),(C'S',DBLOCK),TUSRNM         
         MVC   8(10,R2),WORK2                                                   
         OI    4(R2),X'20'               SET PREVALID BIT                       
         MVI   5(R2),10                                                         
         FOUT  (R2)                                                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                     BUMP TO NEXT FIELD                     
*                                                                               
         LA    RF,GOLDM3H                DID WE JUST DISPLAY THE                
         CR    R2,RF                     3RD DEMO?                              
         BH    XDR025                     YES - EXIT                            
*                                                                               
         LA    R3,3(R3)                                                         
         BCT   R5,XDR01015                NO - GET NEXT DEMO TO DISPLAY         
         B     XDR025                                                           
*                                                                               
XDR01050 DS    0H                                                               
         GOTO1 DEMOCON,DMCB,(3,ESTDEMO),(10,WORK2),(C'S',DBLOCK),TUSRNM         
         MVC   GOLTAR(10),WORK2                                                 
         MVC   GOLDM2(10),WORK2+10                                              
         MVC   GOLDM3(10),WORK2+20                                              
         OI    GOLTARH+4,X'20'     SET PREVALID BIT                             
         FOUT  GOLTARH                                                          
         OC    WORK2+10(10),WORK2+10                                            
         BZ    *+8                                                              
         MVI   GOLDM2H+5,10                                                     
         OC    WORK2+20(10),WORK2+20                                            
         BZ    XDR020                                                           
         MVI   GOLDM3H+5,10                                                     
*                                                                               
XDR020   OI    GOLDM2H+4,X'20'     SET PREVALID BIT                             
         FOUT  GOLDM2H                                                          
         OI    GOLDM3H+4,X'20'     SET PREVALID BIT                             
         FOUT  GOLDM3H                                                          
*                                                                               
XDR025   DS    0H                                                               
         LA    R3,42(R7)                                                        
         CLI   0(R3),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*--SET THE START DAY                                                            
         MVC   STDAY,21(R3)                                                     
         CLI   STDAY,0                                                          
         BNE   *+8                                                              
         MVI   STDAY,1             IF ZERO INPUT SET FOR MONDAY                 
*                                                                               
         MVC   GOLPLN(12),5(R3)                                                 
         OI    GOLPLNH+4,X'20'     SET PREVALID BIT                             
         FOUT  GOLPLNH                                                          
*--SET PREVALID BITS ON                                                         
         LA    R2,GOLDT01H                                                      
*                                                                               
XDR050   OI    4(R2),X'20'                                                      
         MVC   8(17,R2),SPACES                                                  
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(11,R2),SPACES                                                  
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         OI    4(R2),X'20'                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
         CLI   0(R2),9                                                          
         BNE   XDR050                                                           
*                                                                               
XDR070   LA    R2,GOLDT01H                                                      
*                                                                               
         MVI   ELCODE,X'21'                                                     
         CLI   ACTNUM,ACTDIS                                                    
         BE    XDR090                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   XDR100                                                           
XDR090   MVC   STSCREEN,ENSCREEN                                                
XDR100   XC    ENSCREEN,ENSCREEN                                                
         BAS   RE,NEXTEL                                                        
         BNE   XDREXIT                                                          
         CLC   2(2,R3),OPTPER      CHECK DATE FILTER                            
         BL    XDR100                                                           
         OC    STSCREEN,STSCREEN                                                
         BZ    XDR120                                                           
         CLC   2(2,R3),STSCREEN                                                 
         BL    XDR100                                                           
*                                                                               
XDR120   GOTO1 DATCON,DMCB,(2,2(R3)),(8,8(R2))                                  
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
*                                                                               
         MVI   8(R2),C'0'          DEFAULT ZERO                                 
         OC    8(4,R3),8(R3)                                                    
         BZ    XDR130                                                           
         EDIT  (4,8(R3)),(11,8(R2)),2,ALIGN=LEFT,FLOAT=-                        
XDR130   FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
*                                                                               
         OC    4(4,R3),4(R3)                                                    
         BZ    XDR140                                                           
         EDIT  (4,4(R3)),(5,8(R2)),1,ALIGN=LEFT                                 
XDR140   FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   1(R3),16                                                         
         BL    XDR200                                                           
         OC    12(4,R3),12(R3)                                                  
         BZ    XDR200                                                           
         EDIT  (4,12(R3)),(5,8(R2)),1,ALIGN=LEFT                                
XDR200   FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   1(R3),21                                                         
         BL    XDR250                                                           
         OC    17(4,R3),17(R3)                                                  
         BZ    XDR250                                                           
         EDIT  (4,17(R3)),(5,8(R2)),1,ALIGN=LEFT                                
XDR250   FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   1(R3),17                                                         
         BL    XDR300                                                           
         OC    16(1,R3),16(R3)                                                  
         BZ    XDR300                                                           
         XC    WORK2(4),WORK2                                                   
         GOTO1 UNDAY,DMCB,16(R3),WORK2                                          
         MVC   8(4,R2),WORK2                                                    
         B     XDR400                                                           
*                                                                               
XDR300   DS    0H                                                               
         CLI   1(R3),33           ANY WORKHORSE?                                
         BL    XDR400                                                           
*                                                                               
         CLI   21(R3),C'W'        WORKHORSE?                                    
         BNE   XDR400                                                           
         MVC   8(2,R2),=C'W='                                                   
         MVC   10(3,R2),22(R3)                                                  
*                                                                               
XDR400   DS    0H                                                               
         FOUT  (R2)                                                             
         BRAS  RE,NEXTUN                                                        
*                                                                               
         CLI   0(R2),9                                                          
         BNE   XDR100                                                           
*                                                                               
XDR500   BAS   RE,NEXTEL                                                        
         BNE   XDREXIT                                                          
         MVC   ENSCREEN,2(R3)                                                   
         B     XDREXIT                                                          
*                                                                               
XDREXIT  OI    CONSRVH+6,X'81'                                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 VSETXSP                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(32),0(R7)       RESTORE GOAL KEY                             
         GOTO1 READ                RESET KEY D/A                                
*                                                                               
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   XCHKM040                                                         
         OC    ENSCREEN,ENSCREEN   IS THERE ANOTHER SCREEN TO DISPLAY           
         BZ    XCHKM040                                                         
         CLI   THISLSEL,C'S'       IS IT A SELECT                               
         BE    XCHKM030                                                         
         CLI   THISLSEL,C'A'       IS IT AN ALTER                               
         BNE   XCHKM040                                                         
*                                                                               
         ZIC   RE,SELLISTN         FIND SELECT CODE IN LIST DIRECTORY           
         MH    RE,=H'6'                                                         
         LA    RE,LISTDIR(RE)                                                   
         MVI   0(RE),C'C'          AND RESET CONTROLLER'S SELECT CODE           
XCHKM030 OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
*                                                                               
XCHKM040 J     EXIT                                                             
         LTORG                                                                  
         DROP  R7                                                               
***************************************************************                 
*        ADD SPOT GOAL RECORD                                                   
*              AIO2 = XSPOT GOAL RECORD                                         
***************************************************************                 
ADDSPT   NTR1  BASE=*,LABEL=*                                                   
         L     R7,AIO2                                                          
         MVC   SVXKEY,0(R7)                                                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NGOLRECD,R4                                                      
*                                                                               
         MVC   0(13,R4),0(R7)       COPY XSPT KEY                               
         GOTO1 VSETSPT                                                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      FOUND IT?                                   
         BNE   ADDSP50              NOPE - IT'S A NEW ADD                       
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
ADDSP50  DS    0H                                                               
         L     R4,AIO3                                                          
         MVC   0(13,R4),0(R7)       COPY XSPT KEY                               
         MVC   GCNTRLS,36(R7)            CONTROL BITS                           
*                                                                               
         MVC   GLENGTH,=H'24'                                                   
         MVC   GAGYALPH,AGENCY                                                  
*                                                                               
         LA    R3,42(R7)              POINT TO FIRST ELEM IN XSP REC            
         LA    R5,24(R4)              POINT TO FIRST ELEM IN SPT REC            
*                                                                               
ADDSP100 DS    0H                                                               
         CLI   0(R3),0                ANY MORE ELEMS TO COPY?                   
         BE    ADDSP150                                                         
*                                                                               
         ZIC   R1,1(R3)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R3)          MOVE ELEM FROM XSP TO SPT                 
*                                                                               
         ZIC   RF,1(R3)                                                         
         SR    RE,RE                                                            
         ICM   RE,3,GLENGTH                                                     
         AR    RF,RE                                                            
         STCM  RF,3,GLENGTH           INCREMENT RECORD LENGTH                   
*                                                                               
         ZIC   RF,1(R5)               BUMP TO NEXT AVAILABLE ELEM               
         AR    R5,RF                  IN SPT                                    
         XC    0(2,R5),0(R5)                                                    
*                                                                               
         ZIC   RF,1(R3)               BUMP TO NEXT ELEM IN XSP                  
         AR    R3,RF                                                            
         B     ADDSP100                                                         
*                                                                               
ADDSP150 DS    0H                                                               
         MVC   AIO,AIO3                                                         
         L     R4,AIO                                                           
         CLC   KEY(13),0(R4)          RECORD ALREADY EXISTS?                    
         BNE   ADDSP160               NO - ADD IT                               
*                                                                               
         CLI   XSPACT,C'C'                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVI   SPTACT,C'C'            CHANGING RECORD                           
         GOTO1 PUTREC                                                           
         B     ADDSP200                                                         
*                                                                               
ADDSP160 DS    0H                                                               
         CLI   XSPACT,C'A'                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVI   SPTACT,C'A'            ADDING RECORD                             
         GOTO1 ADDREC                 ADD SPOT GOAL                             
*                                                                               
ADDSP200 DS    0H                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 VSETXSP                                                          
*                                                                               
         L     R7,AIO2                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(32),0(R7)                                                    
*                                                                               
         GOTO1 HIGH                                                             
         GOTO1 GETREC                 RESTORE XSPOT KEY                         
*                                                                               
ADDSPTX  DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
***************************************************************                 
*        ADD SPOT GOAL HISTORY RECORD                                           
*              AIO3 = XSPOT GOAL HISTORY RECORD                                 
***************************************************************                 
ADDHSPT  NTR1  BASE=*,LABEL=*                                                   
         L     R7,AIO3                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NGOLRECD,R4                                                      
         MVC   0(13,R4),0(R7)                                                   
*                                                                               
         GOTO1 VSETSPT                                                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      FOUND IT?                                   
         BNE   ADHSP50              NOPE - IT'S A NEW ADD                       
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
ADHSP50  DS    0H                                                               
         L     R7,AIO3                                                          
         L     R4,AIO1                                                          
         MVC   0(13,R4),0(R7)                                                   
         MVC   GCNTRLS,36(R7)            CONTROL BITS                           
*                                                                               
         MVC   GLENGTH,=H'24'                                                   
         MVC   GAGYALPH,AGENCY                                                  
*                                                                               
         LA    R3,42(R7)              POINT TO FIRST ELEM IN XSP REC            
         LA    R5,24(R4)              POINT TO FIRST ELEM IN SPT REC            
*                                                                               
ADHSP100 DS    0H                                                               
         CLI   0(R3),0                ANY MORE ELEMS TO COPY?                   
         BE    ADHSP150                                                         
*                                                                               
         ZIC   R1,1(R3)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R3)          MOVE ELEM FROM XSP TO SPT                 
*                                                                               
         ZIC   RF,1(R3)                                                         
         SR    RE,RE                                                            
         ICM   RE,3,GLENGTH                                                     
         AR    RF,RE                                                            
         STCM  RF,3,GLENGTH           INCREMENT RECORD LENGTH                   
*                                                                               
         ZIC   RF,1(R5)               BUMP TO NEXT AVAILABLE ELEM               
         AR    R5,RF                  IN SPT                                    
         XC    0(2,R5),0(R5)                                                    
*                                                                               
         ZIC   RF,1(R3)               BUMP TO NEXT ELEM IN XSP                  
         AR    R3,RF                                                            
         B     ADHSP100                                                         
*                                                                               
ADHSP150 DS    0H                                                               
         MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         CLC   KEY(13),0(R4)          RECORD ALREADY EXISTS?                    
         BNE   ADHSP160               NO - ADD IT                               
         GOTO1 PUTREC                                                           
         B     ADHSP200                                                         
*                                                                               
ADHSP160 DS    0H                                                               
         GOTO1 ADDREC                 ADD SPOT GOAL HISTORY                     
*                                                                               
ADHSP200 DS    0H                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 VSETXSP                                                          
*                                                                               
         L     R7,AIO3                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(32),0(R7)                                                    
*                                                                               
         GOTO1 HIGH                                                             
         GOTO1 GETREC                 RESTORE XSPOT KEY                         
*                                                                               
ADHSPTX  DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R4                                                               
***************************************************************                 
* ADD GOAL HISTORY ELEMENT TO HISTORY RECORD IF CHANGE WAS MADE                 
* TO RECORD                                                                     
***************************************************************                 
XHSTREC  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VSETXSP                                                          
*                                                                               
         CLI   GOLOPTH+5,0               WAS A REASON ENTERED?                  
         BE    XHSTX                     NO                                     
*                                                                               
* BUILD HISTORY ELEMENT                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING GHSTEL,R3                                                        
         MVI   GHSTCODE,X'60'            HISTORY ELCODE                         
         GOTO1 DATCON,DMCB,(5,0),(2,TDAY)        CURRENT DATE                   
         MVC   GHSTDATE,TDAY                                                    
*                                                                               
* GET TIME OF CHANGE                                                            
*                                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R1,15,PACKWORK                                                   
         STCM  R0,15,PACKWORK+4                                                 
         AP    PACKWORK(4),PACKWORK+4(4)  DDS TIME IS OFFSET FROM 6AM           
         CP    PACKWORK(4),=P'240000'    PAST MIDNIGHT?                         
         BL    XHST10                                                           
         SP    PACKWORK(4),=P'240000'    YES, ADJUST TIME                       
XHST10   MVC   GHSTTIME,PACKWORK                                                
*                                                                               
         ZIC   R4,RSNCDLN               REASON CODE LEN - 1                     
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   GHSTRSN(0),RSNCODE                                               
*                                                                               
         LA    R4,1(R4)                 ADD 1 FOR REASON CODE LEN               
         AH    R4,=Y(GHSTRSN-GHSTEL)    ADD ELEMENT LEN W/O REASON CODE         
         STC   R4,GHSTLEN               STORE VARIABLE ELEMENT LEN              
         DROP  R3                                                               
*                                                                               
         MVC   AIO,AIO3                                                         
         L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
*                                                                               
         OI    KEY+11,X'20'          MAKE SURE IT'S A HISTORY KEY               
         MVC   HOLDXKEY,KEY                                                     
*                                                                               
         GOTO1 VSETXSP                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    XHST20                                                           
*                                                                               
         LR    R0,R7                  CLEAR AIO                                 
         LH    R1,=H'2000'                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   KEY,SVXKEY                                                       
         OI    KEY+11,X'20'                                                     
         MVC   0(32,R7),KEY                                                     
         MVC   GXRLEN,=X'002A'                                                  
*!!!!!!! MVC   GAGYALPH,TWAAGY                                                  
         BRAS  RE,PUTELX                                                        
*                                                                               
* ADD GOAL DESCRIPTION ELEMENT                                                  
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING GDELEM,R3                                                        
         MVI   GOCODE,X'20'            ELEMENT CODE AND LENGTH                  
         MVI   GOLEN,GDLENQ                                                     
         MVC   GREDATE,TDAY            CREATION DATE                            
         MVC   GDNETWK,SVNETWK                                                  
         BRAS  RE,PUTELX                                                        
*                                                                               
         GOTO1 ADDREC                                                           
         B     XHST70                                                           
*                                                                               
XHST20   DS    0H                                                               
         GOTO1 GETREC                                                           
* CHECK IF THERE'S IS ALREADY AN HISTORY ELEMENT WITH THE SAME EXACT            
* COMMENT ADDED WITHIN THE PAST 10 MINUTES                                      
         MVI   ELCODE,X'60'                                                     
         LA    R3,42(R7)                                                        
XHST30   BRAS  RE,NEXTEL                                                        
XHST40   BNE   XHST60                                                           
*                                                                               
XHST50   DS    0H                                                               
         USING GHSTEL,R3                                                        
*                                                                               
* COMPARE ELEMENT LEN                                                           
         CLC   GHSTLEN,ELEM+1                                                   
         BNE   XHST30                                                           
*                                                                               
* COMPARE REASON CODE                                                           
         ZIC   R1,GHSTLEN                                                       
         SH    R1,=H'9'                  GET LEN OF (COMMENT-1) ONLY            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   GHSTRSN(0),ELEM+8         COMPARE REASON CODE                    
         BNE   XHST30                                                           
*                                                                               
* COMPARE TIME OF CHANGE                                                        
         MVC   PACKWORK(4),GHSTTIME      GET TIME OF OLD ELEM                   
         SRP   PACKWORK(4),62,3          REMOVE SECONDS: SHIFT RIGHT 2,         
         AP    PACKWORK(4),=P'10'        USE 3 TO ROUND, ADD 10 MINUTES         
         MVC   PACKWORK+4(4),ELEM+4      GET TIME OF NEW ELEMENT                
         SRP   PACKWORK+4(4),62,3                                               
         CP    PACKWORK(4),PACKWORK+4(4) WITHIN 10 MINUTES?                     
         BL    XHST30                    NO                                     
         B     XHST70                    YES, DON'T CHANGE RECORD               
         DROP  R3                                                               
*                                                                               
*                                                                               
XHST60   BRAS  RE,PUTELX                                                        
         CLI   DMCB+12,X'05'             RECORD HAS BECOME TOO LONG?            
         BNE   XHST65                    NO                                     
*                                                                               
         LA    R2,GOLOPTH                YES                                    
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'* HISTORY RECORD HAS BECOME TOO LONG *'           
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
XHST65   GOTO1 PUTREC                                                           
*                                                                               
XHST70   DS    0H                                                               
         CLI   SVPRD,0                                                          
         BE    *+8                                                              
         BRAS  RE,ADDHSPT                ADD HISTORY RECORD FOR SPT             
*                                                                               
* REREAD GOAL RECORD                                                            
         L     R7,AIO                                                           
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(32),HOLDXKEY                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(32),HOLDXKEY                                                 
         BNE   XHSTX                     MUST BE AN ADD                         
         GOTO1 GETREC                                                           
XHSTX    MVC   AIO,AIO2                  NOW POINT TO CHANGED RECORD            
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R7                                                               
****************************************************************                
ADDPTSX  NTR1  BASE=*,LABEL=*                                                   
         XC    TOTPNTS,TOTPNTS                                                  
*                                                                               
         LA    R3,42(R7)                                                        
         MVI   ELCODE,X'21'                                                     
*                                                                               
         CLI   0(R3),X'21'                                                      
         BE    ADDP150X                                                         
*                                                                               
ADDP100X BRAS  RE,NEXTEL                                                        
         BNE   ADDPEXTX                                                         
*                                                                               
ADDP150X CLI   8(R3),C'T'          IF ELEMENT NOT FOR TOTAL DONT CALC           
         BNE   ADDP100X                                                         
         ICM   R4,15,TOTPNTS                                                    
         ICM   R5,15,4(R3)                                                      
         AR    R4,R5                                                            
         STM   R4,15,TOTPNTS                                                    
         B     ADDP100X                                                         
*                                                                               
ADDPEXTX DS    0H                                                               
         J     EXIT                                                             
*************************************************************                   
ACTIVITX NTR1  BASE=*,LABEL=*                                                   
         LA    R3,42(R7)                                                        
         CLI   0(R3),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(5,0),(2,32(R3))                                     
         OC    30(2,R3),30(R3)     DOES CREATE DATE EXIST                       
         BNZ   *+10                YES EXIT                                     
         MVC   30(2,R3),32(R3)     MOVE LAST ACTIVE INTO CREATE DATE            
         J     EXIT                                                             
*************************************************************                   
NETTOTX  NTR1  BASE=*,LABEL=*                                                   
         L     R7,AIO2                                                          
         XC    LISTDOL,LISTDOL                                                  
         XC    TDOLELM,TDOLELM                                                  
         XC    FULL,FULL                                                        
         XC    ELEM(12),ELEM                                                    
         MVC   ELEM(2),=X'420C'                                                 
         MVI   ELCODE,X'21'                                                     
         SR    R5,R5                                                            
         LA    R3,42(R7)                                                        
NETT050X BRAS  RE,NEXTEL                                                        
         BNE   NETT100X                                                         
*                                                                               
         TM    4(R3),X'80'                                                      
         BZ    *+22                IF NOT TOTAL ELEM DONT SAVE                  
         LR    R4,R3               SAVE ADDRESS OF LAST ELEMENT                 
         ICM   R5,15,8(R3)         ACCUM TOTAL ELEMENTS ONLY                    
         A     R5,TDOLELM                                                       
         ST    R5,TDOLELM                                                       
         NI    4(R3),X'7F'         RESET TOTAL DOLLAR SWITCH                    
         CLI   FULL,0                                                           
         BNE   NETT070X                                                         
         MVC   FULL(2),2(R3)                                                    
NETT070X MVC   FULL+2(2),2(R3)                                                  
         ICM   R5,15,8(R3)                                                      
         A     R5,LISTDOL                                                       
         ST    R5,LISTDOL                                                       
         B     NETT050X                                                         
*                                                                               
NETT100X CLI   TOTDSW,X'FF'        WAS TOTAL DOLLAR OPTION USED                 
         BNE   NETT200X            NO ROUNDING NOT REQUIRED                     
         ICM   R2,15,TDOLELM       SUM OF ELEMENTS                              
         ICM   R3,15,TOTDLRS       ACTUAL TOTAL AMOUNT                          
         SR    R3,R2                                                            
         ICM   R2,15,8(R4)         ADD DIFFERENCE TO LAST ELEMENT               
         AR    R2,R3                                                            
         STCM  R2,15,8(R4)                                                      
         ICM   R2,15,LISTDOL                                                    
         AR    R2,R3                                                            
         STCM  R2,15,LISTDOL                                                    
*                                                                               
NETT200X MVC   ELEM+2(4),FULL      MOVE DATES IN                                
         MVC   ELEM+8(4),LISTDOL   MOVE IN TOTAL DOLLARS                        
         BRAS  RE,PUTELX           WRITE ELEMENT OUT                            
*                                                                               
NETTEXX  DS    0H                                                               
         J     EXIT                                                             
*************************************************************                   
*--SET THE START DAY FROM EITHER THE 20 ELEMENT ON A                            
*--CHANGE OR FROM THE N0 PRFILE ON AN ADD                                       
*                                                                               
SETDAY   NTR1  BASE=*,LABEL=*                                                   
         CLI   CONACT,C'A'                                                      
         BNE   SETDAY20                                                         
         MVC   STDAY,N0PROF+4                                                   
         B     SETDAYEX                                                         
*                                                                               
SETDAY20 LA    R3,24(R7)                                                        
*                                                                               
         CLI   0(R3),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*--SET THE START DAY                                                            
         MVC   STDAY,21(R3)                                                     
*                                                                               
         CLI   STDAY,0                                                          
         BNE   *+8                                                              
         MVI   STDAY,1             IF ZERO INPUT SET FOR MONDAY                 
*                                                                               
SETDAYEX J     EXIT                                                             
*************************************************************                   
*--SET THE START DAY FROM EITHER THE 20 ELEMENT ON A                            
*--CHANGE OR FROM THE N0 PRFILE ON AN ADD                                       
*                                                                               
SETDAYX  NTR1  BASE=*,LABEL=*                                                   
         CLI   CONACT,C'A'                                                      
         BNE   SETDX20                                                          
         MVC   STDAY,N0PROF+4                                                   
         B     SETDXEX                                                          
*                                                                               
SETDX20  LA    R3,42(R7)                                                        
*                                                                               
         CLI   0(R3),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*--SET THE START DAY                                                            
         MVC   STDAY,21(R3)                                                     
*                                                                               
         CLI   STDAY,0                                                          
         BNE   *+8                                                              
         MVI   STDAY,1             IF ZERO INPUT SET FOR MONDAY                 
*                                                                               
SETDXEX  J     EXIT                                                             
*                                                                               
*--SET THE START DAY FROM EITHER THE 20 ELEMENT ON A                            
*--CHANGE OR FROM THE N0 PRFILE ON AN ADD                                       
         EJECT                                                                  
*--ON ADD ACTION SEE IF ANY CHANGE TO LOWER SCEEN                               
*--IF NO CHANGE INVALID CONDITION SET                                           
*                                                                               
CHKINPT  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,GOLDT01H                                                      
*--CHECK DOLLARS                                                                
CHKINP50 TM    4(R2),X'20'         CHECK PREVALID BIT                           
         BZ    CHKINPEX                                                         
         BRAS  RE,NEXTUN                                                        
         CLI   0(R2),9                                                          
         BNE   CHKINP50                                                         
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    CHKINPEX                                                         
*                                                                               
         MVI   ERROR,CHGADDQ                                                    
         LA    R2,GOLPLNH                                                       
         TM    4(R2),X'20'         CHECK PREVALID BIT                           
         JNZ   TRAPERR                                                          
*                                                                               
*        JNZ   CHGADD              SCREEN HAS NOT BEEN CHANGED ERROR            
*                                                                               
CHKINPEX J     EXIT                                                             
         EJECT                                                                  
*                                                                               
DELELX   NTR1  BASE=*,LABEL=*                                                   
         LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'XSPFIL '),(ELCODE,AIO),0                     
         LR    RE,R0                                                            
         J     EXIT                                                             
*                                                                               
PUTELX   NTR1  BASE=*,LABEL=*                                                   
         LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',=C'XSPFIL '),AIO,ELEM                           
         LR    RE,R0                                                            
         J     EXIT                                                             
*                                                                               
*--CALCULATE DOLLAR AMOUNT                                                      
*                                                                               
CALCDOL  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM+4(4),ELEM+4                                                 
*--CHECK FOR TOTAL DOLLAR INPUT                                                 
         CLI   TOTDSW,X'FF'                                                     
         BE    CALD400                                                          
         CLI   8(R2),C'T'                                                       
         BE    CALD400                                                          
*                                                                               
         CLI   5(R2),0                                                          
         BE    CALDEXT                                                          
CALD200  XC    DMCB+4(4),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(0,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM+8(4),DMCB+4                                                 
         B     CALDEXT                                                          
*                                                                               
CALD400  MVI   TOTDSW,X'FF'                                                     
         MVI   ELEM+8,C'T'         SET TOTAL INDICATOR                          
*                                                                               
CALDEXT  J     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
* TEST WEEKLY GOALS FALL WITHIN ESTIMATE DATE RANGE                             
*************************************************************                   
TSTDATE  NTR1  BASE=*,LABEL=*                                                   
         L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
         LA    R7,GXDATA           POINT TO FIRST ELEMENT                       
*                                                                               
TSTD10   CLI   0(R7),0                                                          
         JE    EXIT                                                             
         CLI   0(R7),GLCODEQ       TEST WEEKLY ELEMENT                          
         JNE   TSTD20                                                           
*                                                                               
         CLC   2(2,R7),MYESTART    TEST WITHIN ESTIMATE DATE RANGE              
         JL    TSTDERR                                                          
         CLC   2(2,R7),MYEEND                                                   
         JH    TSTDERR                                                          
*                                                                               
TSTD20   ZIC   RF,1(R7)                                                         
         AR    R7,RF                                                            
         J     TSTD10                                                           
*                                                                               
TSTDERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'* ERROR - OUTSIDE OF EST RANGE *'                 
         GOTO1 HEXOUT,DMCB,COUNT,CONHEAD+40,1,C'TOG'                            
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
         EJECT                                                                  
*************************************************************                   
PLINED   DSECT                                                                  
PRPRD    DS    CL7                                                              
         DS    CL3                                                              
PREST    DS    CL8                                                              
         DS    CL3                                                              
PRDPT    DS    CL8                                                              
         DS    CL3                                                              
PRLEN    DS    CL6                                                              
         DS    CL3                                                              
PRCST    DS    CL10                                                             
         DS    CL3                                                              
PRDM1    DS    CL8                                                              
         DS    CL3                                                              
PRDM2    DS    CL8                                                              
         DS    CL3                                                              
PRPER    DS    CL17                                                             
         SPACE 2                                                                
LLINED   DSECT                                                                  
LRPLAN   DS    CL1                                                              
LRPROD   DS    CL7                                                              
         DS    CL1                                                              
LRNET    DS    CL4                                                              
         DS    CL1                                                              
LREST    DS    CL3                                                              
         DS    CL1                                                              
LRDPT    DS    CL8                                                              
         DS    CL1                                                              
LRLEN    DS    CL3                                                              
         DS    CL1                                                              
LRCOST   DS    CL10                                                             
         DS    CL2                                                              
LRDEMO   DS    CL8                                                              
         DS    CL2                                                              
LRDAT    DS    CL17                                                             
         EJECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
NGOLRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NEGOLFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEGOLF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEGOLF2D                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUSER                                                      
         EJECT                                                                  
       ++INCLUDE NEGOLWORK                                                      
         ORG   SYSSPARE                                                         
*                           *******  T31402 WORK AREA  *******                  
*                                                                               
INVINPTQ EQU   241                                                              
ESIZERRQ EQU   242                                                              
CHGADDQ  EQU   243                                                              
INVESTMQ EQU   244                                                              
INVDPTQ  EQU   245                                                              
NOHELPAQ EQU   246                                                              
RSNCDERQ EQU   247                                                              
*                                                                               
WORKAREA DS    0CL500                                                           
MYWORKD  DS    CL20                BASE MODULE WORK AREA                        
QLEN     DS    CL3                                                              
WORK2    DS    CL80                                                             
SVFMTSW  DS    H                                                                
OLDVPHS  DS    CL34                OLD VPHS                                     
SPFIL20  DS    CL20                20 SPACES                                    
ELEMCODE DS    CL1                 92 OR 93 ELEMNT CODE                         
OPTDAYP  DS    CL1                 DAYPART FILTER                               
OPTEST   DS    CL1                 ESTIMATE FILTER                              
OPTLEN   DS    CL1                 LENGTH FILTER                                
OPTPER   DS    CL2                 PERIOD FILTER                                
OPTSPER  DS    CL2                 OPTION START PERIOD                          
OPTEPER  DS    CL2                 OPTION END PERIOD                            
CHGSW    DS    CL1                 SCREEN CHANGE SWITCH                         
INPSW    DS    CL1                 SCREEN INPUT SWITCH                          
TOTDSW   DS    CL1                 TOTAL DOLLARS SWITCH                         
BLNKSW   DS    CL1                 BLANK LINE SWITCH                            
TOTDLRS  DS    F                   TOTAL DOLLLARS                               
TOTPNTS  DS    F                   TOTAL POINTS                                 
TOTCPP   DS    F                   COST PER POINT                               
LISTDOL  DS    F                                                                
TDOLELM  DS    F                   TOTAL DOLLAR ELEMENT TOTAL                   
LISTPNT1 DS    F                                                                
LISTPNT2 DS    F                                                                
ADEMOCON DS    A                                                                
HOLDKEY  DS    CL13                SECONDARY KEY SAVE AREA                      
HOLDPER  DS    CL2                 PERIOD FILTER HOLD AREA                      
SCANAREA DS    CL28                WORK AREA FOR OPTIONS                        
LASTACT  DS    CL2                 LAST ACTION                                  
KEYB4MKT DS    CL1                 LENGTH OF KEY CHECK FOR LIST                 
KEYAFMKT DS    CL1                 LENGTH OF KEY CHECK FOR LIST                 
KEYCHECK DS    CL1                 LENGTH OF KEY CHECK FOR LIST                 
*PACKED STORAGE AREA USED TO CALCULATE DOLLARS WHEN TOTAL IS USED               
PACKWORK DS    PL16                WORK AREA                                    
PACKCPP  DS    PL10                COST PER POINT                               
PACKTPNT DS    PL6                 TOTAL POINTS                                 
PACKPNTS DS    PL6                 ELEMENT POINTS                               
MYEDAILY DS    X                                                                
MYESTART DS    XL2                 COMPRESSED EST START DATE                    
MYEEND   DS    XL2                 COMPRESSED EST END DATE                      
COUNT    DS    X                                                                
POPTION  DS    X                                                                
RSNCODE  DS    CL62                REASON FOR GOAL CHNG (HISTORY ELEM)          
RSNCDLN  DS    X                   REASON CODE LENGTH-1                         
TDAY     DS    XL2                 TODAY'S DATE                                 
SVNETWK  DS    CL4                 SAVE NETWORK FOR GOAL RECORD                 
TUSRNM   DS    CL28                USER DEMO NAMES                              
*                                                                               
NETFILT  DS    XL1                 NETWORK FILTER FLAG (N,C,S)                  
*                                                                               
DISDEMO  DS    XL3                 DISPLAY THIS DEMO                            
*                                                                               
SVMED    DS    XL1                 MEDIA                                        
SVCLI    DS    XL2                 CLIENT                                       
SVPRD    DS    XL1                 PRODUCT                                      
SVMKT    DS    XL2                 MARKET                                       
SVEST    DS    XL1                 ESTIMATE                                     
SVDPT    DS    XL1                 DAYPART                                      
SVSLN    DS    XL1                 SPOT LENGTH                                  
SVSEC    DS    XL1                 SECONDS                                      
SVAGY    DS    XL1                 AGENCY CODE                                  
SVABOVE  DS    XL1                                                              
SVPRDA   DS    XL3                 PRODUCT ALPHA                                
*                                                                               
SVTGWKC  DS    XL2                 GOAL WEEK FOR TOTALS                         
*                                                                               
SYSTFLAG DS    CL1                 (S)POT, (X)FILE                              
*                                                                               
SVXKEY   DS    XL32                SAVED XFILE KEY                              
HOLDXKEY DS    CL32                XFILE AREA                                   
SVSKEY   DS    XL13                SAVED SPOT KEY                               
*                                                                               
MYFLAG   DS    XL1                                                              
PROCSPT  EQU   X'01'               PROCESS SPOT FILE                            
PLANNED  EQU   X'10'               PLANNED GOAL RECORD                          
*                                                                               
XSPACT   DS    CL1                 (C)HANGE, (A)DD                              
SPTACT   DS    CL1                 (C)HANGE, (A)DD                              
*                                                                               
DUMMYH   DS    XL9                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
SCAND    DSECT                                                                  
*              DSECT TO COVER SCANNER LINES                                     
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066NEGOL02   04/18/16'                                      
         END                                                                    
