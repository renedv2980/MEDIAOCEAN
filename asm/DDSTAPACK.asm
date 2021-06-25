*          DATA SET DDSTAPACK  AT LEVEL 103 AS OF 04/20/16                      
*PHASE T00A7AB                                                                  
*INCLUDE BINSR31                                                                
*INCLUDE ARREDIT                                                                
*INCLUDE QSORT                                                                  
         MACRO                                                                  
&NAME1   PROTON                                                                 
&NAME1   ICM   RF,15,VPROTON                                                    
         BZ    *+6                                                              
         BASR  RE,RF                                                            
         MEND                                                                   
         MACRO                                                                  
&NAME2   PROTOFF                                                                
&NAME2   ICM   RF,15,VPROTOFF                                                   
         BZ    *+6                                                              
         BASR  RE,RF                                                            
         MEND                                                                   
*===================================================================*           
* 23JUN14  AKAT  FIX ANOTHER BUG THAT PASSED BACK DUP SEQ NUMS      *           
* 19JUL13  AKAT  BUG FIX THAT PASSED BACK DUP SEQ NUMS FOR CANADA   *           
* 16MAY13  MHER  CREATE/SORT SECOND CABLETAB AND USE BINSRHC ON IT  *           
*                FOR SEARCH OF 3 BYTE ALPHA ENTRIES                 *           
* 24NOV09  AKAT  RELINK TO PICK UP NEW CABLETAB ENTRIES NE AND NF   *           
* 14FEB05  MHER  PICK UP SOFT LENGTH OF CABLETAB ENTRIES            *           
* 22AUG01  MHER  NEW CANADIAN CABLE SEQNUMS                                     
* 15MAY01  MHER  CHANGE GETMAINL 10.40 AM                           *           
* 04JAN99  EJOR  CHANGE GETMAINL 11.48 PM                           *           
* 18SEP98  MHER  CHANGE GETMAINL 11.19 PM                                       
* 02FEB98  MHER  CONVERTED TO USE RELATIVE SEQUENCE NUMS BY HEADEND *           
* 23AUG96  MHER  ALLOW -L FOR LOW POWER TV STATIONS                 *           
* 04MAR96  MHER  RETURN ACTUAL MEDIA CODE FOR MEDIA C REQUEST       *           
*===================================================================*           
         TITLE 'T00A7A - NEW MARKET/STATION PACK'                               
STAPACK  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 STAPDX-STAPD,STAPACKC,RR=R8                                      
         USING STAPD,RC                                                         
*                                                                               
         ST    R8,RELO                                                          
         USING STAPACKD,RA                                                      
         LR    RA,R1                                                            
*                                                                               
         CLC   =C'XD',STAPAGY      TEST AGENCY FOR VANESSA                      
         BNE   *+8                 IS CANADIAN BUT NOT CONVERTED                
         MVI   STAPCTRY,C'U'                                                    
*                                                                               
         MVI   STAPVRSN,C'N'       SET NEW VERSION                              
         CLI   STAPACT,C'V'        TEST VERSION REQUEST                         
         BE    MSX                                                              
*                                                                               
         MVI   OFFLINE,C'Y'                                                     
         LTR   R8,R8               OFFLINE IF RELO=0                            
         BZ    *+8                                                              
         MVI   OFFLINE,C'N'                                                     
*                                                                               
MS010    DS    0H                                                               
         USING COMFACSD,RF                                                      
         L     RF,STAPACOM                                                      
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VDMGR,CDATAMGR                                                   
         MVC   VPROTON,CPROTON                                                  
         MVC   VPROTOFF,CPROTOFF                                                
         L     RE,=V(BINSRCH)                                                   
         AR    RE,R8                                                            
         ST    RE,VBINSRCH                                                      
         L     RE,=V(ARREDIT)                                                   
         AR    RE,R8                                                            
         ST    RE,VARREDIT                                                      
         L     RE,=V(QSORT)                                                     
         AR    RE,R8                                                            
         ST    RE,VQSORT                                                        
         DROP  RF                                                               
*                                                                               
* SAVE T00A9E ADDRESS SO CAN USE SPOT CABLE NETWORK LIST                        
*                                                                               
* STAPACT=E OR QSTP_T00A9E ARE OFFLINE ONLY.                                    
*                                                                               
         CLI   STAPACT,C'E'         TEST SET ADDRESS REQUEST                    
         BE    *+12                                                             
         CLI   STAPACT,QSTP_T00A9E  TEST SET ADDRESS REQUEST                    
         BNE   MS020                                                            
*                                                                               
         ICM   RF,15,VT00A9E       ADDRESS ALREADY SET?                         
         BNZ   MSX                 YES - EXIT                                   
         MVC   VT00A9E,STAPACOM    SAVE PHASE ADDRESS                           
         B     MSX                                                              
*                                                                               
MS020    DS    0H                                                               
         CLI   STAPACT,C'Z'        RESET TABLE                                  
         BE    MSRESET             SKIP OVER CALLOV FOR T00A9E                  
*                                                                               
         ICM   RF,15,VT00A9E       ADDRESS ALREADY SET?                         
         BNZ   MS030               NO                                           
*                                                                               
MS022    BAS   RE,INITA9E                                                       
         L     RF,VT00A9E                                                       
*                                                                               
MS030    DS    0H                                                               
         TM    5(RF),X'80'         DID A NEW TABLE GET LOADED                   
         JZ    MS022               YES- NEED TO REINITIALIZE                    
*                                                                               
         MVI   STAPERR,0                                                        
         CLI   STAPACT,C'U'        MSUNPK                                       
         BE    MSUNPK                                                           
         CLI   STAPACT,C'P'        MSPACK                                       
         BE    MSPACK                                                           
         CLI   STAPACT,C'C'        VALIDATE CABLE NETWORK                       
         BE    MSPACK                                                           
         CLI   STAPACT,C'K'        FILTER CBLNET                                
         BE    MSUNPK                                                           
         CLI   STAPACT,C'N'        NEXT AVAIL SEQ NUM                           
         BE    MSNUM                                                            
         CLI   STAPACT,C'X'        TRANSLATE NETWORK DDS 3 TO NIELSEN 4         
         BE    MSTRAN                                                           
         CLI   STAPACT,C'Y'        TRANSLATE NETWORK NIELSEN 4 TO DDS 3         
         BE    MSNIEDDS                                                         
         CLI   STAPACT,C'A'        ADD A NEW STATION                            
         BE    ADDSTA                                                           
*                                                                               
         MVI   STAPERR,QSTP_SFC    STUPID COMMAND                               
*                                                                               
MSX      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  MSRESET - RESET TABLE IN SHARE MEMORY, STAPAGY=X'FFFF'-CLEAR ALL             
*                                                                               
MSRESET  DS    0H                                                               
         GOTO1 VDMGR,P0,=C'SHMUSS',(0,=CL8'ATTACH'),                   +        
               (0,=CL8'STAPACK'),0,0                                            
*                                                                               
         PROTOFF                                                                
         L     R3,DMCB+8           A(MSAGSHM)                                   
         ST    R3,AMSAGSHM                                                      
         USING MSAGSHMD,R3                                                      
         SAM31                                                                  
*                                                                               
         TS    MSAGTLCK            TRY TO GET THE MAIN TABLE LOCK               
         BZ    MSRST20                                                          
*                                  SOMEONE LOCKED IT, WAIT A MOMENT             
         GOTO1 VDMGR,DMCB,=C'OPMSG',=C'STAPACK,REBUILD - ENTER WAIT'            
         MVC   DMCB+8,=F'38400'     1 MILSEC (TU)                               
         GOTO1 VDMGR,DMCB,(0,=CL8'DMTIME'),C'WAIT'                              
         GOTO1 VDMGR,DMCB,=C'OPMSG',=C'STAPACK,REBUILD - FINISH WAIT'           
*                                                                               
MSRST20  DS    0H                                                               
         CLC   =X'FFFF',STAPAGY    GIVEN SPECIFIC AGY GIVEN?                    
         BNE   MSRST40             YES- JUST CLEAR THIS AGY TABLE               
*                                  NO - CLEAR TABLE LABEL                       
*                                     SO, NEXT STAPACK WILL REINIT IT           
         MVC   MSAGTLAB,=CL8' '                                                 
         MVI   MSAGTLCK,0          UNLOCK THE MAIN TABLE LOCK                   
         B     MSRSTX                                                           
*                                                                               
MSRST40  DS    0H                                                               
         LA    R2,MSAGTAB                                                       
         USING MSAGTABD,R2                                                      
*                                                                               
MSRST50  CLI   0(R2),X'FF'         END OF TABLE                                 
         BE    MSRST90                                                          
         CLC   STAPAGY,MSAGY       THIS AGENCY?                                 
         BE    MSRST80             AGENCY'S TABLE                               
         LA    R2,MSATLNQ(R2)      BUMP TO NEXT ENTRY                           
         B     MSRST50                                                          
*                                                                               
MSRST80  DS    0H                                                               
         OI    MSAFLAG,X'80'       SET A BIT TO REBUILD NEXT TIME               
MSRST90  MVI   MSAGTLCK,0          UNLOCK THE MAIN TABLE LOCK                   
*                                                                               
MSRSTX   PROTON                                                                 
         B     MSX                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
MSPACK   MVC   WORKTAB,MSPMTAB     MOVE TABLE OF VALID BANDS                    
         CLI   STAPMED,C'R'                                                     
         BE    *+8                                                              
         MVI   WORKTAB+6,C'D'      REPLACE S WITH D IF NOT RADIO                
*                                                                               
         LA    R2,STAPQMKT         PACK THE MARKET                              
         LA    R3,STAPQSTA                                                      
         PACK  STPDUB,0(4,R2)                                                   
         CVB   R1,STPDUB                                                        
         STCM  R1,3,STAPMKT                                                     
* TRY TO MAKE THE MEDIA VALID IN STATION FIELD                                  
         CLI   STAPQSTA+4,C' '                                                  
         BH    MSP05                                                            
         MVC   STAPQSTA+4(1),STAPMED                                            
*                                                                               
MSP05    CLI   STAPMED,C'T'        FOR CANADAIAN TV & NETWORK                   
         BE    MSP10                                                            
         CLI   STAPMED,C'C'         AND COMBINED, IDIOT                         
         BE    MSP10                                                            
         CLI   STAPMED,C'N'                                                     
         BNE   MSP20                                                            
*                                                                               
MSP10    CLI   STAPCTRY,C'C'                                                    
         BE    MSPCAN                                                           
*                                                                               
MSP20    CLI   STAPQSTA,C'0'       IS THIS A CABLE STATION                      
         BL    MSP80                                                            
* MAKE SURE STATION IS NUMERIC                                                  
         MVI   STAPQSTA+4,C'T'     ALWAYS MEDIA T                               
         LA    R0,4                                                             
         LA    R1,STAPQSTA                                                      
*                                                                               
MSP22    CLI   0(R1),C'0'          STAPQSTA HAS TO BE 4 DIGIT NUMERIC           
         BL    MSPCHERR               GOOD FOR THE COMPARE LATER                
         CLI   0(R1),C'9'                                                       
         BH    MSPCHERR                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,MSP22                                                         
*                                                                               
         PACK  STPDUB,STAPQSTA(4)                                               
         CVB   R1,STPDUB                                                        
*****                                                                           
         CHI   R1,8191             CAN WE STAY IN F00080 FORMAT?                
         BNH   *+8                                                              
         SHI   R1,8192             NO, USE E80080 FORMAT TO GET TO 9999         
*****                                                                           
         SLL   R1,7                                                             
         STCM  R1,7,STAPSTA        SAVE THE STATION NOW                         
*                                                                               
         CLC   STAPQNET,=C'   '    IS THERE A NETWORK                           
         BNH   MSP70                                                            
         CLC   STAPQNET,=C'ALL'                                                 
         BE    MSP70                                                            
         EJECT                                                                  
* SEARCH THE TABLE OF VALID CABLE NETWORK CODES                                 
*                                                                               
MSP40    OI    STAPQNET+2,C' '     MAKE SURE NOT X'00'                          
         LA    RE,STAPQNET         SET ADDRESS OF KEYARG                        
         ST    RE,DMCB             HOB=0 TO SEARCH FOR EXACT KEY                
         MVC   DMCB+4(16),A9EPAR2  A(TABLE)/RECCNT/RECLEN/KEYLEN                
         XC    DMCB+20(4),DMCB+20                                               
         GOTO1 VBINSRCH,DMCB                                                    
         TM    0(R1),X'80'         31 BIT NOT FOUND                             
         BZ    MSP42                                                            
         MVI   STAPERR,QSTP_INVCBL INVALID CABLE NETWORK                        
         B     MSP70               & SET PACKED STATION                         
*                                                                               
MSP42    L     R5,0(R1)            GET ENTRY ADDRESS                            
*                                                                               
MSP45    CLI   STAPACT,C'C'        VALIDATE CABLE NETWORK                       
         BNE   MSP50               NO - CONTINUE                                
         MVC   STAPNSEQ,3(R5)      RETURN NETWORK SEQ NUMBER FROM TABLE         
         MVC   STAPNFLG,6(R5)      RETURN NETWORK FLAG FROM TABLE               
         B     MSX                                                              
*                                                                               
MSP50    MVC   SEQNUM,3(R5)        MOVE SEQNUM FROM TABLE                       
         TM    6(R5),X'40'         TEST TOP 24                                  
         BZ    MSP52               NO                                           
         XC    SEQNUM,SEQNUM                                                    
         MVC   SEQNUM+1(1),6(R5)                                                
         NI    SEQNUM+1,X'1F'                                                   
         EJECT                                                                  
*=================================================================*             
* READ THE STATION MASTER RECORD FOR TWO PURPOSES                 *             
* FIRST - NEED TO VALIDATE THAT NETWORK IS VALID FOR HEADEND      *             
* SECOND  - IF NETWORK IS NOT IN TOP24, NEED TO GET THE REL SEQNUM*             
* ON ENTRY, R5 POINTS TO CABLELST ENTRY FOR THIS NETWORK          *             
*=================================================================*             
         SPACE 1                                                                
MSP52    BRAS  RE,GETSTA                                                        
*                                                                               
         LA    R4,IOA                                                           
         USING STARECD,R4                                                       
*                                                                               
         TM    6(R5),X'40'         TEST TOP 24                                  
         BZ    MSP60               NO                                           
*                                                                               
         L     R1,=X'01000000'     INITIALIZE BITMASK                           
         SR    R0,R0                                                            
         ICM   R0,3,SEQNUM                                                      
         SRL   R1,1                                                             
         BCT   R0,*-4                                                           
*                                                                               
         STCM  R1,7,STPDUB         SAVE BITMASK VALUE                           
         NC    STPDUB(3),SCBL24    TEST NET VALID THIS HEADEND                  
         BNZ   MSP68                                                            
*                                                                               
MSP54    MVI   STAPERR,QSTP_NOTACTV  INACTIVE CABLE NETWORK                     
         B     MSX                                                              
*                                                                               
* SEARCH SEQNUM LIST FOR NON-TOP24 NETWORKS                                     
*                                                                               
MSP60    LA    R1,SCBLSEQ          POINT TO ACTIVE NETWORK LIST                 
         LA    R0,25               INITIAZLIZE COUNTER (24+1)                   
*                                                                               
MSP62    DS    0H                                                               
         CHI   R0,127              END OF ACTIVE LIST                           
         BH    MSP54               YES - RETURN ERROR                           
         CLC   SEQNUM,0(R1)        MATCH NETWORK VALUE                          
         BE    MSP64                                                            
         LA    R1,2(R1)                                                         
         AHI   R0,1                                                             
         B     MSP62                                                            
*                                                                               
MSP64    DS    0H                                                               
         STCM  R0,3,SEQNUM         SET VALUE                                    
*                                                                               
MSP68    SR    R0,R0                                                            
         ICM   R0,3,SEQNUM                                                      
         SR    R1,R1                                                            
         ICM   R1,7,STAPSTA                                                     
         OR    R1,R0                                                            
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
MSP70    STCM  R1,7,STAPSTA                                                     
         CLC   STAPQSTA(4),=C'8191'   BIT BOUNDARY CONDITION?                   
         BNH   MSP75                  NO, X'F00000' IS STILL GOOD               
         OI    STAPSTA,X'E8'          YES, CAN ALSO BE X'E80000'                
         B     MSX                                                              
MSP75    OI    STAPSTA,X'F0'          SET ON HIGH BITS - CABLE                  
         B     MSX                                                              
*                                                                               
MSP80    SR    R0,R0                                                            
         SR    R1,R1                                                            
         LA    R5,26                                                            
         LA    R8,2                                                             
         BAS   RE,MSPCHAR                                                       
         AR    R1,R6                                                            
         MR    R0,R5                                                            
         LA    R3,1(R3)                                                         
         BCT   R8,*-12                                                          
         BAS   RE,MSPCHAR                                                       
         AR    R1,R6                                                            
         LA    R5,27                                                            
         MR    R0,R5                                                            
         LA    R3,1(R3)                                                         
         BAS   RE,MSPCHAR                                                       
         AR    R1,R6                                                            
         SLL   R1,5                                                             
         STCM  R1,7,STAPSTA         SAVE IN CASE OF MEDIA C ERROR               
*                                                                               
         LA    R6,NUMBANDQ                                                      
         LA    R7,WORKTAB                                                       
         LR    R8,R7                                                            
*                                                                               
MSP82    CLC   STAPQSTA+4(1),0(R8)                                              
         BE    MSP84                                                            
         LA    R8,1(R8)                                                         
         BCT   R6,MSP82                                                         
         CLI   STAPMED,C'C'                                                     
         BE    MSX                 NO ERROR FOR MEDIA C                         
MSPCHERR MVI   STAPERR,QSTP_INVMED                                              
         B     MSX                                                              
*                                                                               
MSP84    SR    R8,R7                                                            
         AR    R1,R8                                                            
         STCM  R1,7,STAPSTA                                                     
         B     MSX                                                              
*                                                                               
MSPCHAR  LR    R6,R5                                                            
         LA    R7,MSPCTAB                                                       
*                                                                               
MSPCH10  CLC   0(1,R3),0(R7)                                                    
         BE    MSPCH20                                                          
         LA    R7,1(R7)                                                         
         BCT   R6,MSPCH10                                                       
         MVI   STAPERR,QSTP_INVALID                                             
         B     MSX                                                              
*                                                                               
MSPCH20  SR    R6,R5                                                            
         LPR   R6,R6                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        MSPACK - CANADIAN AGENCY                                               
*                                                                               
MSPCAN   DS    0H                                                               
         BAS   RE,GETTABLE         GET THE A(ENTRY IN MSAGTAB)                  
         USING MSAGTABD,R2                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(5),STAPQSTA    SET STATION                                  
         MVI   WORK+4,C'T'         ALWAYS LOOK FOR MEDIA T                      
*                                                                               
         PROTOFF                                                                
         SAM31                                                                  
         L     R3,MSATAB                                                        
         A     R3,AMSAGSHM                                                      
         LA    R3,8(R3)                                                         
*                                                                               
         ICM   R4,15,MSACNT        MAKE SURE SOMETHING THERE                    
         BZ    MSPC15                                                           
*                                                                               
STA      USING LIBUFFD,LIBUFFR                                                  
         XC    LIBUFFR,LIBUFFR                                                  
         ST    R3,STA.LIABUFF          SET A(BUFFER)                            
         MVI   STA.LIACTN,LIAHIGH                                               
         MVC   RECORD,WORK                                                      
         LA    R1,RECORD           LOOK UP THIS STATION                         
         ST    R1,STA.LIAREC                                                    
         OI    STA.LIFLAG1,LIF1INI+LIF1FXD+LIF1BIG                              
         GOTO1 VARREDIT,DMCB,STA.LIBUFFD                                        
         CLI   STA.LIRTN,LIROK                                                  
         BE    MSPC10                                                           
         CLI   STA.LIRTN,LIREOF                                                 
         BE    MSPC15              END OF TABLE -- OK                           
         DC    H'0'                                                             
         DROP  STA                                                              
*                                                                               
MSPC10   CLC   WORK(5),RECORD                                                   
         BNE   MSPC15              REC NOT FOUND                                
         MVC   STAPSTA(2),RECORD+5 SET CODE                                     
         PROTON                                                                 
         B     MSPC18                                                           
*                                                                               
MSPC15   PROTON                                                                 
         B     MSPC40                                                           
*                                                                               
MSPC18   CLI   STAPMED,C'N'        TEST NETWORK                                 
         BNE   *+12                                                             
         CLI   STAPQNET,C' '       TEST CABLE SUFFIX                            
         BH    MSPC60                                                           
*                                                                               
         CLI   STAPMED,C'N'        MEDIA MUST MATCH STATION MEDIA               
         BNE   *+8                                                              
         MVI   STAPQSTA+4,C'N'                                                  
*                                                                               
         LA    R6,NUMBANDQ         SET MEDIA BITS                               
         LA    R7,WORKTAB                                                       
         LR    R8,R7                                                            
*                                                                               
MSPC20   CLC   STAPQSTA+4(1),0(R8)                                              
         BE    MSPC30                                                           
         LA    R8,1(R8)                                                         
         BCT   R6,MSPC20                                                        
         CLI   STAPMED,C'C'                                                     
         BE    MSPCX               NO ERROR FOR MEDIA C                         
         B     MSPC50                                                           
*                                                                               
MSPC30   SR    R8,R7                                                            
         STC   R8,STAPSTA+2                                                     
         B     MSPCX                                                            
*                                                                               
MSPC40   MVI   STAPERR,QSTP_CANSNF CANADIAN STATION NOT FOUND                   
         B     MSPCX                                                            
*                                                                               
MSPC50   MVI   STAPERR,QSTP_INVMED                                              
         B     MSPCX                                                            
*                                                                               
MSPCX    SAM24                                                                  
         B     MSX                                                              
         EJECT                                                                  
*===============================================================                
* SEARCH TABLE FOR CANADIAN CABLE NETWORK SUFFIX                                
*===============================================================                
         SPACE 1                                                                
MSPC60   LA    RE,STAPQNET         SET ADDRESS OF KEYARG                        
         ST    RE,DMCB                                                          
         MVI   DMCB,0              SET SEARCH FOR EXACT KEY                     
         BRAS  RE,SETSFX           GET TABLE ADDRESS                            
         ST    R1,DMCB+4                                                        
         LHI   R0,SUFXTABN                                                      
         ST    R0,DMCB+8                                                        
         LHI   R0,L'SUFXTAB        RECLEN                                       
         ST    R0,DMCB+12                                                       
         LHI   R0,2                KEYLEN                                       
         ST    R0,DMCB+16          (KEYDSPL=0)                                  
         GOTO1 VBINSRCH,DMCB                                                    
         TM    0(R1),X'80'         31 BIT NOT FOUND - YSFI !                    
         BO    MSPC40                                                           
         L     RE,0(R1)            GET ENTRY ADDRESS                            
         MVC   STAPSTA+2(1),2(RE)  MOVE SEQNUM                                  
         B     MSPCX                                                            
         EJECT                                                                  
*==================================================================             
* MSUNPK                                                                        
*==================================================================             
         SPACE 1                                                                
MSUNPK   MVC   WORKTAB,MSUMTAB     MOVE TABLE OF VALID BANDS                    
         CLI   STAPMED,C'R'                                                     
         BE    *+8                                                              
         MVI   WORKTAB+6,C'D'      REPLACE S WITH D IF NOT RADIO                
*                                                                               
         CLI   STAPACT,C'K'        TEST NETWORK FILTER                          
         BE    *+10                YOU SFI                                      
         MVC   STAPQNET,=C'   '                                                 
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,STAPMKT                                                     
         CVD   RE,STPDUB                                                        
         OI    STPDUB+7,X'0F'                                                   
         UNPK  STAPQMKT,STPDUB                                                  
*                                                                               
         CLI   STAPMED,C'T'        FOR CANADAIAN TV & NETWORK                   
         BE    MSU10                                                            
         CLI   STAPMED,C'C'         AND COMBINED, MORON                         
         BE    MSU10                                                            
         CLI   STAPMED,C'N'                                                     
         BNE   MSU20                                                            
*                                                                               
MSU10    CLI   STAPCTRY,C'C'      IS THIS CANADA ?                              
         BE    MSUCAN                                                           
*                                                                               
MSU20    SR    R9,R9                                                            
         ICM   R9,14,STAPSTA                                                    
         CLI   STAPSTA,X'E8'        E80000 OR GREATER, IT BE CABLE              
         BL    MSU50                                                            
         CLC   STAPSTA,=X'FFFFFD'   FFFFFE AND FFFFFF ARE GRANT                 
         BH    MSX                                                              
*                                                                               
         SLL   R9,4                 GET RID OF X'F0'/X'E8'                      
         CLI   STAPSTA,X'F0'        OLD STYLE (CABLE <= 8191)?                  
         BNL   *+12                 YES, WE'RE OKAY                             
         SLL   R9,1                 NO, GET RID OF X'08' OF THE X'E8'           
         SRL   R9,1                                                             
*                                                                               
         SRL   R9,19                SHIFT TO GET THE RAW NUMBER                 
         CLI   STAPSTA,X'F0'        OLD STYLE (CABLE <= 8191)?                  
         BNL   *+8                  YES, WE'RE OKAY                             
         AHI   R9,8192              NO, SPECIAL FOR 8192 AND ABOVE              
*                                                                               
         CVD   R9,STPDUB                                                        
         OI    STPDUB+7,X'0F'                                                   
         UNPK  STAPQSTA(4),STPDUB                                               
         MVI   STAPQSTA+4,C' '                                                  
*                                                                               
         SR    R9,R9               ISOLATE THE NETWORK BYTE                     
         ICM   R9,8,STAPSTA+2                                                   
         SLL   R9,1                                                             
         SRL   R9,25                                                            
         STC   R9,STPDUB                                                        
         LTR   R9,R9                                                            
         BZ    MSX                                                              
         EJECT                                                                  
*====================================================================*          
* FOR CABLE, DECODE NETWORK                                          *          
* IF SEQNUM IN RANGE 1-24, LOOK IT UP IN CABLELST                    *          
* ELSE READ STATION MASTER RECORD, TO CONVERT SEQNUM TO NETWORK NUM  *          
*====================================================================*          
         SPACE 1                                                                
         CLI   STPDUB,24           TEST IN TOP24                                
         BH    MSU30               NO                                           
*                                                                               
         L     R1,VT00A9E          POINT R1 TO CABLETAB                         
         LR    R5,R1                                                            
         SR    R0,R0                                                            
         IC    R0,STPDUB                                                        
*                                                                               
MSU22    TM    6(R5),X'40'         TEST TOP24 ENTRY                             
         BZ    *+12                NO                                           
         BCT   R0,*+8                                                           
         B     MSU24                                                            
         AH    R5,CBLTABLN                                                      
         B     MSU22                                                            
*                                                                               
MSU24    CLI   STAPACT,C'K'        TEST FILTER ON CBLNET                        
         BE    MSU26                                                            
         MVC   STAPQNET,0(R5)                                                   
         B     MSX                                                              
*                                                                               
MSU26    MVI   STAPERR,QSTP_YES                                                 
         CLC   STAPQNET,0(R5)                                                   
         BE    *+8                                                              
         MVI   STAPERR,QSTP_NO                                                  
         B     MSX                                                              
         EJECT                                                                  
* STATION NOT IN TOP24 - READ STATION MASTER RECORD                             
         SPACE 1                                                                
MSU30    BRAS  RE,GETSTA                                                        
*                                                                               
         LA    R4,IOA                                                           
         USING STARECD,R4                                                       
*                                                                               
         SR    RE,RE                                                            
         IC    RE,STPDUB           GET SEQNUM                                   
         SHI   RE,24                                                            
         CHI   RE,103                                                           
         BH    MSU40               CAN'T HAVE MORE THAN 127 NETWORKS            
         MHI   RE,2                                                             
         LA    RE,SCBLSEQ-2(RE)                                                 
*                                                                               
         MVC   SEQNUM,0(RE)        NETWORK NUM IN CABLE LIST                    
         CLC   SEQNUM,=XL2'0'                                                   
         BE    MSU40               ERROR                                        
         SR    RF,RF                                                            
         ICM   RF,3,SEQNUM                                                      
         CLC   SEQNUM,=X'FFFF'     TEST FOR ERROR SEQNUM                        
         BNE   MSU31                                                            
         MVC   STAPQNET,=C'ZYX'    SET INVALID CBLNET                           
         B     MSX                 BECAUSE DYING IS HORRIBLE                    
*                                                                               
MSU31    BCTR  RF,0                                                             
         MH    RF,CBLTABLN                                                      
         L     R1,VT00A9E          POINT R1 TO CABLELST                         
         AR    RF,R1                                                            
         CLI   STAPACT,C'K'        TEST FILTER ON CBLNET                        
         BE    MSU32                                                            
         MVC   STAPQNET,0(RF)                                                   
         B     MSX                                                              
*                                                                               
MSU32    MVI   STAPERR,QSTP_YES                                                 
         CLC   STAPQNET,0(RF)      MATCHES THE EBCDIC NETWORK?                  
         BE    MSX                 YES, WE'RE GOOD                              
*                                                                               
* THERE MIGHT BE ANOTHER ENTRY IN CABLELIST THAT HAS THE SAME SEQNUM            
MSU34    AH    RF,CBLTABLN         NEXT ENTRY IN CABLELIST                      
         CLC   =3X'FF',0(RF)       EOT?                                         
         BE    MSU38                                                            
         CLC   SEQNUM,3(RF)        MATCHES THE SEQNUM?                          
         BNE   MSU34               YES, WE'RE GOOD                              
         CLC   STAPQNET,0(RF)      MATCHES THE ALPHA NETWORK?                   
         BE    MSX                 YES, WE'RE GOOD                              
         B     MSU34               CAN WE HAVE MORE THAN 2 ENTRIES?             
*                                                                               
MSU38    MVI   STAPERR,QSTP_NO     NO MATCH ON EBCDIC NETWORK                   
         B     MSX                                                              
*                                                                               
MSU40    MVI   STAPERR,QSTP_INVCBL                                              
         B     MSX                                                              
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*=================================================================*             
* NOT CABLE                                                                     
*=================================================================*             
         SPACE 1                                                                
MSU50    SRL   R9,13                                                            
         LA    R5,STAPQSTA+3                                                    
         LA    R6,27                                                            
         SR    R8,R8                                                            
         DR    R8,R6                                                            
         LA    R8,MSPCTAB(R8)                                                   
         MVC   0(1,R5),0(R8)                                                    
         BCTR  R5,0                                                             
         BCTR  R6,0                                                             
         LA    R7,2                                                             
*                                                                               
MSU60    SR    R8,R8                                                            
         DR    R8,R6                                                            
         LA    R8,MSPCTAB(R8)                                                   
         MVC   0(1,R5),0(R8)                                                    
         BCTR  R5,0                                                             
         BCT   R7,MSU60                                                         
         LA    R9,MSPCTAB(R9)                                                   
         MVC   0(1,R5),0(R9)                                                    
         IC    R8,STAPSTA+2                                                     
         N     R8,=X'0000001F'     DROP ALL BUT LAST 5 BITS                     
         MVI   STAPQSTA+4,C'N'                                                  
         CHI   R8,NUMBANDQ                                                      
         BNL   MSX                                                              
         LA    R8,WORKTAB(R8)                                                   
         MVC   STAPQSTA+4(1),0(R8)                                              
         B     MSX                                                              
         EJECT                                                                  
*================================================================*              
* MSUNPK - CANADIAN MEDIA T/N/C                                                 
*================================================================*              
         SPACE 1                                                                
MSUCAN   DS    0H                                                               
         CLC   =X'270ED6B5A4',STAPMKT  SPECIAL MANUAL BILLING STATION           
         BNE   MSUC05                                                           
         MVC   STAPQMKT,=C'9998'                                                
         MVC   STAPQSTA,=C'YCKDX'                                               
         B     MSUC40                                                           
*                                                                               
         USING MSAGTABD,R2                                                      
*                                                                               
MSUC05   BAS   RE,GETTABLE         GET THE A(TABLE IN CORE)                     
         XC    WORK,WORK                                                        
*                                                                               
*IT WAS MVC(3), BUT I CAN'T SEE THE REASON WHY.                                 
*                                                                               
         MVC   WORK(2),STAPSTA     SET UP KEY LIKE IN TABLE                     
*                                                                               
         SAM31                                                                  
         PROTOFF                                                                
         L     R3,MSNTAB                                                        
         A     R3,AMSAGSHM                                                      
         LA    R3,8(R3)                                                         
         ICM   R4,15,MSNCNT           MAKE SURE SOMETHING THERE                 
         BZ    MSUC15                                                           
*                                                                               
SEQ      USING LIBUFFD,LIBUFFR2                                                 
         XC    LIBUFFR2,LIBUFFR2                                                
         ST    R3,SEQ.LIABUFF      SET A(BUFFER)                                
         MVI   SEQ.LIACTN,LIAHIGH                                               
         MVC   RECORD,WORK                                                      
         LA    R1,RECORD           LOOK UP THIS STATION                         
         ST    R1,SEQ.LIAREC                                                    
         OI    SEQ.LIFLAG1,LIF1INI+LIF1FXD+LIF1BIG                              
         GOTO1 VARREDIT,DMCB,SEQ.LIBUFFD                                        
         CLI   SEQ.LIRTN,LIROK                                                  
         BE    MSUC10                                                           
         CLI   SEQ.LIRTN,LIREOF                                                 
         BE    MSUC15              END OF TABLE -- OK                           
         DC    H'0'                                                             
         DROP  SEQ                                                              
*                                                                               
MSUC10   CLC   WORK(2),RECORD                                                   
         BNE   MSUC15              REC NOT FOUND                                
         MVC   STAPQSTA,RECORD+2   SET CODE                                     
         PROTON                                                                 
         B     MSUC18                                                           
*                                                                               
MSUC15   PROTON                                                                 
         B     MSUC30                                                           
************************************************************                    
*                                                                               
MSUC18   MVC   STAPQSTA+4(1),STAPMED  SET DEFAULT VALUE                         
         IC    R8,STAPSTA+2        EXTRACT THE MEDIA BITS                       
         N     R8,=X'0000001F'     DROP ALL BUT LAST 5 BITS                     
* I DON'T BELIEVE WE NEED THE NEXT INSTRUCTION MH 8/22/01                       
         CLI   STAPMED,C'R'                                                     
         BE    MSUC20                                                           
         SR    R8,R8                                                            
         IC    R8,STAPSTA+2        EXTRACT MEDIA BITS AGAIN                     
         CLI   STAPMED,C'C'                                                     
         BNE   MSUC40                                                           
* THIS CODE FOR MEDIA C                                                         
         MVI   STAPQSTA+4,C'N'                                                  
         CHI   R8,5                                                             
         BNL   MSUC40                                                           
         MVI   STAPQSTA+4,C'T'                                                  
         B     MSUC40                                                           
*                                                                               
MSUC20   LA    R8,MSUMTAB(R8)                                                   
         MVC   STAPQSTA+4(1),0(R8)                                              
         B     MSUC40                                                           
*                                                                               
MSUC30   MVI   STAPERR,QSTP_CANSNF CANADIAN STATION NOT FOUND                   
         MVI   STAPERR,0           *** IGNORE ERROR ***  MH 25JAN95             
         B     MSUCX                                                            
*                                                                               
MSUC40   CLI   STAPSTA+2,X'B0'     TEST CANADIAN CABLE SUFFIX                   
         BL    MSUCX                                                            
         BRAS  RE,SETSFX2          POINT R1 TO SUFFIX TABLE                     
         SR    R8,R8                                                            
         IC    R8,STAPSTA+2                                                     
         SHI   R8,X'B0'                                                         
         MHI   R8,L'SUFXTAB2                                                    
         AR    R8,R1               POINT TO ENTRY                               
         MVI   STAPQSTA+4,C'/'                                                  
         MVC   STAPQNET(2),0(R8)                                                
         MVI   STAPQNET+2,C' '                                                  
*                                                                               
MSUCX    SAM24                                                                  
         B     MSX                                                              
         EJECT                                                                  
*                              GET NEXT AVAIL SEQ NUM                           
MSNUM    DS    0H                                                               
         USING MSAGTABD,R2                                                      
         BAS   RE,GETTABLE         GET THE A(TABLE IN CORE)                     
*                                                                               
         PROTOFF                                                                
         SAM31                                                                  
         L     R3,MSNTAB                                                        
         A     R3,AMSAGSHM                                                      
         LA    R3,8(R3)                                                         
         ICM   R4,15,MSNCNT           MAKE SURE SOMETHING THERE                 
         BNZ   *+6                                                              
         DC    H'0'                   BETTER LUCK NEXT TIME                     
*                                                                               
         XC    RECORD,RECORD                                                    
         MVC   RECORD(2),STAPSTA      SET UP KEY LIKE IN TABLE                  
         LLH   R6,RECORD              KEEP SEQNUM FOR COMPARSION                
*                                                                               
SEQ      USING LIBUFFD,LIBUFFR2                                                 
         XC    LIBUFFR2,LIBUFFR2                                                
         ST    R3,SEQ.LIABUFF      SET A(BUFFER)                                
         MVI   SEQ.LIACTN,LIAHIGH                                               
         LA    R1,RECORD           LOOK UP THIS SEQNUM                          
         ST    R1,SEQ.LIAREC                                                    
         OI    SEQ.LIFLAG1,LIF1INI+LIF1FXD+LIF1BIG                              
         GOTO1 VARREDIT,DMCB,SEQ.LIBUFFD                                        
*                                                                               
MSNUM10  CLI   SEQ.LIRTN,LIROK                                                  
         BE    MSNUM11                                                          
         CLI   SEQ.LIRTN,LIREOF                                                 
         BE    MSNUM15             END OF TABLE -- OK                           
         DC    H'0'                                                             
MSNUM11  CLM   R6,3,RECORD                                                      
         BNE   MSNUM15             NOT FOUND -- OK                              
*                                                                               
MSNUM12  DS    0H                  ADD 1 TO SEQ NUM                             
         AHI   R6,1                                                             
         MVI   SEQ.LIACTN,LIASEQ                                                
         GOTO1 VARREDIT,DMCB,SEQ.LIBUFFD                                        
         B     MSNUM10                                                          
         DROP  SEQ                                                              
*                                                                               
MSNUM15  PROTON                                                                 
         STCM  R6,3,STAPSEQ        RETURN SEQ NUM                               
*                                                                               
         SAM24                                                                  
         B     MSX                                                              
         EJECT                                                                  
*===============================================================                
* TRANSLATE A 3 CHAR NETWORK CODE IN STAPQNET                                   
* TO A 4 CHAR NIELSEN CODE IN STAPQSTA                                          
*===============================================================                
                                                                                
MSTRAN   OI    STAPQNET+2,C' '     MAKE SURE NOT X'00'                          
         LA    RE,STAPQNET         SET ADDRESS OF KEYARG                        
         ST    RE,DMCB             HOB=0 TO SEARCH FOR EXACT KEY                
         MVC   DMCB+4(16),A9EPAR2  A(TABLE)/RECCNT/RECLEN/KEYLEN                
         XC    DMCB+20(4),DMCB+20                                               
         GOTO1 VBINSRCH,DMCB                                                    
*                                                                               
         L     RE,0(R1)            POINT TO ENTRY                               
         TM    0(R1),X'80'         31 BIT NOT FOUND                             
         BZ    *+8                                                              
         LA    RE,=11C' '                                                       
*                                                                               
MSTRAN4  MVC   STAPQSTA(4),7(RE)   MOVE 4 CHAR CODE                             
         MVI   STAPQSTA+4,C' '     AND A BLANK                                  
*                                                                               
         TM    6(RE),X'20'         TEST FUSION/NIELSEN EXCEPTION                
         BZ    MSX                                                              
*                                                                               
         LA    RE,FUSNIEL                                                       
MSTRAN5  CLI   0(RE),0             ONE OF FUSION/NIELSEN EXCEPTIONS?            
         BE    MSX                 NO, NCC CABLE UPDATE NETWORK IS FINE         
         CLC   STAPQSTA(4),0(RE)                                                
         BE    *+12                                                             
         AHI   RE,L'FUSNIEL                                                     
         B     MSTRAN5                                                          
*                                                                               
         MVC   STAPQSTA(4),4(RE)                                                
         B     MSX                                                              
                                                                                
*==============================================================                 
* IF YOU ADD AN ENTRY TO THIS TABLE, BE DEAD SURE YOU SET                       
* THE X'20' FLAG IN BYTE 6 OF THE ENTRY IN SPCBLLIST!                           
*==============================================================                 
                                                                                
FUSNIEL  DS    0CL8                                                             
***      DC    CL4'CRT ',CL4'TRU '     REMOVED BY HWON 9/29/2014                
         DC    CL4'SPSO',CL4'TSOU'                                              
         DC    CL4'WGN ',CL4'WGNA'                                              
         DC    X'00'               EOT                                          
         EJECT                                                                  
*===============================================================                
* TRANSLATE A 4 CHAR NIELSEN CODE IN STAPQSTA                                   
* TO A 3 CHAR NETWORK CODE IN STAPQNET                                          
*===============================================================                
                                                                                
MSNIEDDS L     RE,VT00A9E          SAVE PHASE ADDRESS                           
*                                                                               
         LA    RE,FUSNIEL                                                       
MSND01   CLI   0(RE),0             ONE OF FUSION/NIELSEN EXCEPTIONS?            
         BE    MSND02              NO, NCC CABLE UPDATE NETWORK IS FINE         
         CLC   STAPQSTA(4),4(RE)                                                
         BE    *+12                                                             
         AHI   RE,L'FUSNIEL                                                     
         B     MSND01                                                           
*                                                                               
         MVC   STAPQSTA(4),0(RE)                                                
*                                                                               
MSND02   L     RE,VT00A9E                                                       
*                                                                               
MSND03   CLC   STAPQSTA(4),7(RE)   MATCH 4 CHAR FUSION/NIELSEN CODE             
         BE    MSND04                                                           
         AH    RE,CBLTABLN                                                      
         CLI   0(RE),X'FF'                                                      
         BNE   MSND03                                                           
         LA    RE,=11C' '                                                       
*                                                                               
MSND04   MVC   STAPQNET,0(RE)      MOVE 3 CHAR DDS CODE                         
         B     MSX                                                              
         EJECT                                                                  
*===============================================================                
*        ADD A NEW STATION TO THE TABLE                                         
*===============================================================                
                                                                                
ADDSTA   DS    0H                                                               
         BAS   RE,GETTABLE         GET THE A(ENTRY IN MSAGTAB)                  
         USING MSAGTABD,R2                                                      
*                                                                               
         SAM31                                                                  
         PROTOFF                                                                
         L     R3,MSATAB                                                        
         A     R3,AMSAGSHM                                                      
         LA    R3,8(R3)                                                         
         XC    WORK,WORK                                                        
         MVC   WORK(5),STAPQSTA    SET STATION                                  
         MVC   WORK+5(2),STAPSEQ                                                
*                                                                               
STA      USING LIBUFFD,LIBUFFR                                                  
         XC    LIBUFFR,LIBUFFR                                                  
         MVI   STA.LIACTN,LIAADD   ADD RECORD TO 1ST ARREDIT TABLE              
         OI    STA.LIFLAG1,LIF1INI+LIF1FXD+LIF1BIG                              
         ST    R3,STA.LIABUFF      SET A(BUFFER)                                
         LA    R1,WORK             A(STATION/SEQNUM)                            
         ST    R1,STA.LIAREC       SET A(RECORD) - (STATION/SEQNUM)             
         MVC   STA.LIRECL,=AL2(7)  SET L'RECORD                                 
         GOTO1 VARREDIT,DMCB,STA.LIBUFFD                                        
         CLI   STA.LIRTN,LIROK                                                  
         BE    ADD20                                                            
*                                                                               
         CLI   STA.LIRTN,LIRFULLB                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   STAPERR,QSTP_TABFULL TABLE IS FULL                               
         B     ADD40                                                            
         DROP  STA                                                              
*                                                                               
ADD20    DS    0H                                                               
         L     R3,MSNTAB                                                        
         A     R3,AMSAGSHM                                                      
         LA    R3,8(R3)                                                         
         XC    WORK,WORK                                                        
         MVC   WORK(2),STAPSEQ                                                  
         MVC   WORK+2(5),STAPQSTA                                               
*                                                                               
SEQ      USING LIBUFFD,LIBUFFR2                                                 
         XC    LIBUFFR2,LIBUFFR2                                                
         MVI   SEQ.LIACTN,LIAADD   ADD RECORD TO 2ND ARREDIT TABLE              
         OI    SEQ.LIFLAG1,LIF1INI+LIF1FXD+LIF1BIG                              
         ST    R3,SEQ.LIABUFF      SET A(BUFFER)                                
         LA    R1,WORK             A(STATION/SEQNUM)                            
         ST    R1,SEQ.LIAREC       SET A(RECORD) - (SEQNUM/STATION)             
         MVC   SEQ.LIRECL,=AL2(7)  SET L'RECORD                                 
         GOTO1 VARREDIT,DMCB,SEQ.LIBUFFD                                        
         CLI   SEQ.LIRTN,LIROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  SEQ                                                              
*                                                                               
         USING LIBBUFFD,R3                                                      
         MVC   MSACNT,LIBRECN      SAVE COUNTER IN TABLE1                       
         MVC   MSNCNT,MSACNT       SAVE COUNTER IN TABLE2                       
         DROP  R3                                                               
*                                                                               
         BRAS  RE,CHKTBZ                                                        
*                                                                               
ADD40    SAM24                                                                  
         PROTON                                                                 
         B     MSX                                                              
         EJECT                                                                  
MSPCTAB  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ '                                   
MSPMTAB  DC    CL16'AFTNXLSC        '   <===== S ADDED MHER 18JUL12             
*******  DC    CL16'AFTNXLD         '   S FOR RADIO, D FOR TV                   
         SPACE 2                                                                
MSUMTAB  DC    CL16'AF NXLSCNNNNNNNN'                                           
*******             0123456789ABCDEF                                            
*                                                                               
NUMBANDQ EQU   8                        NUMBER OF VALID BANDS                   
VT00A9E  DC    A(0)                                                             
A9EPARS  DS    0XL24                                                            
A9EPAR1  DC    A(0)                A(SEARCH ARG)                                
A9EPAR2  DC    A(0)                A(TABLE)                                     
A9EPAR3  DC    A(0)                RECORD COUNT                                 
A9EPAR4  DC    A(0)                RECORD LEN                                   
A9EPAR5  DC    A(0)                DSPL OF KEY/LEN OF KEY                       
A9EPAR6  DC    A(0)                                                             
CBLTABLN DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* LOAD SPCBLLIST (T00A9E) AND COPY CABLE NETWORK TABLE TO THE     *             
* AREA THAT IMMEDIATELY FOLLOWS. THEN SORT THAT TABLE ON THE 3    *             
* CHARACTER NETWORK CODE AND BUILD THE BINSRCH PARAMS TO SEARCH IT*             
*=================================================================*             
                                                                                
INITA9E  NTR1                                                                   
         ICM   RF,15,VT00A9E       TEST ADDRESS ALREADY SET                     
         BNZ   INITA9E2                                                         
*                                                                               
         XC    DMCB(24),DMCB       GET PHASE ADDRESS                            
         MVC   DMCB+4(4),=X'D9000A9E'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CALLOV ERROR                                 
*                                                                               
         L     RF,0(R1)                                                         
*                                                                               
INITA9E2 ST    RF,SVRF             SAVE ADDRESS                                 
         PROTOFF                                                                
         MVC   VT00A9E,SVRF                                                     
                                                                                
* NEED TO COUNT NUMBER OF ENTRIES IN CBLTAB AND SET BINSRCH PARMS               
                                                                                
         XC    A9EPARS,A9EPARS                                                  
         SR    R0,R0               CLEAR COUNTER                                
         L     R1,VT00A9E          GET TABLE ADDRESS                            
         LLC   R4,5(R1)            GET TABLE ENTRY LENGTH                       
         LA    R5,X'7F'            IN CASE THE TABLE IS INITIALIZED             
         NR    R4,R5               TURN OFF THE X'80' BIT IN THE LENGTH         
         STH   R4,CBLTABLN         SET ENTRY LEN IN LOCAL STORAGE               
         ST    R4,A9EPAR4          AND IN BINSRCH PARMS                         
*                                                                               
         CLI   0(R1),X'FF'                                                      
         JE    *+10                                                             
         AR    R1,R4               NEXT ENTRY                                   
         JCT   R0,*-10                                                          
*                                                                               
         ICM   R4,15,3(R1)         GET CBLTAB2 DSPL FROM LAST ENTRY             
         A     R4,VT00A9E          ADD START OF CABLETAB                        
         ST    R4,A9EPAR2          AND SET ADDRESS (AND 'TO' ADDR)              
*                                                                               
         LPR   R0,R0               MAKE ENTRY COUNT POSITIVE                    
         ST    R0,A9EPAR3          SAVE ENTRY COUNT                             
*                                                                               
         LR    R1,R0               GET ENTRY COUNT                              
         MH    R1,CBLTABLN         GIVES 'FROM' LENGTH                          
         L     R0,VT00A9E          GET 'FROM' ADDR                              
         LR    R5,R1               'TO' LEN = 'FROM' LEN                        
         MVCL  R4,R0               MOVE CBLTAB TO CBLTAB2                       
*                                                                               
         L     RF,VQSORT                                                        
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+0(4),A9EPAR2   CBLTAB2 ADDRESS                              
         MVC   DMCB+4(4),A9EPAR3   NUMBER OF RECORDS                            
         MVC   DMCB+8(4),A9EPAR4   ENTRY LENGTH                                 
         LA    R0,3                                                             
         ST    R0,A9EPAR5          KEY LENGTH (KEY DSPL=0)                      
         MVC   DMCB+12(4),A9EPAR5  KEY LENGTH (KEY DSPL=0)                      
         GOTO1 (RF),DMCB                                                        
*                                                                               
         L     RE,VT00A9E          GET ADDRESS OF CBLTAB                        
         OI    5(RE),X'80'         MODIFY LEN IN FIRST ENTRY AS FLAG            
*                                                                               
         PROTON                                                                 
         XIT1                                                                   
         LTORG                                                                  
*=================================================================*             
*        RETURN R2 = A(ENTRY) IN MSAGTAB                          *             
*=================================================================*             
         SPACE 1                                                                
GETTABLE NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VDMGR,P0,=C'SHMUSS',(0,=CL8'ATTACH'),                   +        
               (0,=CL8'STAPACK'),0,0                                            
*                                                                               
* GET THE TABLE ADDRESS IN SHARED MEMORY (INITIALIZIED IF NEEDED)               
*                                                                               
         PROTOFF                                                                
         L     R3,DMCB+8           A(MSAGSHM)                                   
         ST    R3,AMSAGSHM                                                      
         USING MSAGSHMD,R3                                                      
*                                                                               
GT02     DS    0H                                                               
         SAM31                                                                  
         TM    MSAGTLCK,X'80'      IS THE ENTIRE TABLE LOCKED?                  
         BO    GTX1                YES - MUST WAIT                              
         CLC   MSAGTLAB,=CL8'*MSAGTAB'                                          
         BE    GT09                ALREADY INITIALIZED                          
*                                                                               
         TS    MSAGTLCK            TRY TO LOCK THE MAIN TABLE LOCK              
         BNZ   GTX1                SOMEONE LOCKED IT, WAIT AND RETRY            
*                                                                               
         LA    R0,MSAGTAB          A(MSAGTAB)                                   
         LA    R1,MSAGMAX*(L'MSAGTAB)                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR MSAGTAB                                
         MVI   MSAGTEND,X'FF'      MARK END OF TABLE                            
*                                                                               
         LA    R2,MSAGTAB                                                       
         USING MSAGTABD,R2                                                      
*                                                                               
         LHI   R1,MSAGT1-MSAGSHMD  DISP FROM BEGINNING OF SHM                   
         L     R0,=A(GETMAINL)                                                  
         L     RF,=A(MAXTAB)                                                    
         LA    RE,MSAGMAX                                                       
GT06     ST    R1,MSATAB                                                        
         AR    R1,R0                                                            
         ST    R1,MSNTAB                                                        
         AR    R1,R0                                                            
         ST    RF,MSAMAX                                                        
         LA    R2,MSATLNQ(,R2)                                                  
         BCT   RE,GT06                                                          
*                                                                               
         MVC   MSAGTLAB,=CL8'*MSAGTAB'                                          
         MVI   MSAGTLCK,0          UNLOCK THE MAIN TABLE LOCK                   
         DROP  R2                                                               
*                                                                               
GT09     DS    0H                                                               
         LA    R2,MSAGTAB                                                       
         USING MSAGTABD,R2                                                      
*                                                                               
         OC    STAPAGY,STAPAGY     MAKE SURE *SOME* AGY PASSED                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
GT10     CLI   0(R2),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                TOO MANY AGENCIES FOR TABLE                  
*                                                                               
         CLC   MSAGY,=X'0000'      EMPTY ENTRY?                                 
         BNE   GT13                NO                                           
*                                  YES                                          
         TS    MSAGTLCK            TRY TO LOCK THE MAIN TABLE LOCK              
         BNZ   GTX1                SOMEONE LOCKED IT, WAIT AND RETRY            
         MVC   MSAGY,STAPAGY       RESERVE ENTRY FOR THIS AGENCY                
         OI    MSAGLOCK,X'80'      LOCK THIS AGENCY TABLE                       
         MVI   MSAGTLCK,0          UNLOCK THE MAIN TABLE LOCK                   
         B     GT20                                                             
*                                                                               
GT13     CLC   STAPAGY,MSAGY       DID WE ALREADY BUILD AGY TABLE               
         BNE   GT15                NO - NEXT ENTRY                              
         TM    MSAFLAG,X'80'       YES- DO WE NEED TO REBUILD THIS              
         BO    GT20                                                             
         B     GTX                                                              
GT15     LA    R2,MSATLNQ(R2)      BUMP TO NEXT ENTRY                           
         B     GT10                                                             
*                                                                               
GT20     DS    0H                                                               
         NI    MSAFLAG,X'FF'-X'80' TURN OFF THE REBUILD BIT                     
         XC    MSACNT,MSACNT       CLEAR COUNTER STA TABLE                      
         XC    MSNCNT,MSNCNT       CLEAR COUNTER SEQ TABLE                      
*                                                                               
* SET EYE-CATCHER IN TABLE AND INITIALISE 2 ARREDIT TABLES                      
*                                                                               
         L     R1,MSATAB                                                        
         A     R1,AMSAGSHM                                                      
         MVC   0(8,R1),=C'***  ***'                                             
         MVC   3(2,R1),STAPAGY                                                  
         LA    R7,8(R1)            A(TABLE)                                     
*                                                                               
STA      USING LIBUFFD,LIBUFFR                                                  
         XC    LIBUFFR,LIBUFFR                                                  
         ST    R7,STA.LIABUFF            SET A(BUFFER)                          
         MVC   STA.LILBUFF,=A(GETMAINL-8)  SET L'BUFFER (-8 FOR LABEL)          
         MVI   STA.LIACTN,LIAINI         INITIALISE                             
         MVI   STA.LIFLAG1,LIF1BIG                                              
         MVC   STA.LIKEYL,=AL2(7)                                               
         MVC   STA.LIRECL,=AL2(7)                                               
         MVC   STA.LIRECLMX,=AL2(7)                                             
         GOTO1 VARREDIT,DMCB,STA.LIBUFFD                                        
         CLI   STA.LIRTN,LIROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  STA                                                              
*                                                                               
         L     R1,MSNTAB                                                        
         A     R1,AMSAGSHM                                                      
         MVC   0(8,R1),=C'***  N**'                                             
         MVC   3(2,R1),STAPAGY                                                  
         LA    R7,8(R1)            A(TABLE)                                     
*                                                                               
SEQ      USING LIBUFFD,LIBUFFR2                                                 
         XC    LIBUFFR2,LIBUFFR2                                                
         ST    R7,SEQ.LIABUFF            SET A(BUFFER)                          
         MVC   SEQ.LILBUFF,=A(GETMAINL-8)  SET L'BUFFER (-8 FOR LABEL)          
         MVI   SEQ.LIACTN,LIAINI             INITIALISE                         
         MVI   SEQ.LIFLAG1,LIF1BIG                                              
         MVC   SEQ.LIKEYL,=AL2(7)                                               
         MVC   SEQ.LIRECL,=AL2(7)                                               
         MVC   SEQ.LIRECLMX,=AL2(7)                                             
         GOTO1 VARREDIT,DMCB,SEQ.LIBUFFD                                        
         CLI   SEQ.LIRTN,LIROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  SEQ                                                              
*                                                                               
*==============================================================*                
* READ X POINTERS FROM STATION FILE TO BUILD TABLE             *                
*==============================================================*                
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STXKTYPE,C'X'       TYPE X                                       
         MVC   STXKAGY,STAPAGY                                                  
         MVC   SVKEY,KEY                                                        
*                                                                               
         SAM24                                                                  
         GOTO1 VDMGR,DMCB,(0,=CL8'DMRDHI'),(0,=CL8'STATION'),KEY,IOA            
         B     GT60                                                             
*                                                                               
GT50     SAM24                                                                  
         GOTO1 VDMGR,DMCB,(0,=CL8'DMRSEQ'),(0,=CL8'STATION'),KEY,IOA            
*                                                                               
GT60     SAM31                                                                  
         CLC   IOA(3),SVKEY        SAME TYPE/AGY                                
         BNE   GT100                                                            
         L     R1,MSACNT           BUMP COUNT OF RECORDS                        
         LA    R1,1(R1)                                                         
         ST    R1,MSACNT                                                        
         C     R1,MSAMAX           MAKE SURE NOT TOO MANY                       
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
STA      USING LIBUFFD,LIBUFFR                                                  
         MVI   STA.LIACTN,LIAADD   ADD RECORD TO 1ST ARREDIT TABLE              
         OI    STA.LIFLAG1,LIF1INS                                              
         LA    R6,STXKSTA-STXKTYPE+IOA   A(STATION/SEQNUM)                      
         ST    R6,STA.LIAREC       SET A(RECORD) - (STATION/SEQNUM)             
         MVC   STA.LIRECL,=AL2(7)  SET L'RECORD                                 
         GOTO1 VARREDIT,DMCB,STA.LIBUFFD                                        
         CLI   STA.LIRTN,LIROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  STA                                                              
*                                                                               
SEQ      USING LIBUFFD,LIBUFFR2                                                 
         MVI   SEQ.LIACTN,LIAADD   ADD RECORD TO 2ND ARREDIT TABLE              
         OI    SEQ.LIFLAG1,LIF1INS                                              
         LA    R1,RECORD           SWAP STATION/SEQNUM-->SEQNUM/STATION         
         MVC   0(2,R1),5(R6)                                                    
         MVC   2(5,R1),0(R6)                                                    
         ST    R1,SEQ.LIAREC       SET A(RECORD) - (SEQNUM/STATION)             
         MVC   SEQ.LIRECL,=AL2(7)  SET L'RECORD                                 
         GOTO1 VARREDIT,DMCB,SEQ.LIBUFFD                                        
         CLI   SEQ.LIRTN,LIROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     GT50                                                             
         DROP  SEQ                                                              
*                                                                               
GT100    DS    0H                                                               
*                                                                               
STA      USING LIBUFFD,LIBUFFR                                                  
         MVI   STA.LIACTN,LIAFIX       FIX ARRAY (TURN TO LINKED LIST)          
         GOTO1 VARREDIT,DMCB,STA.LIBUFFD                                        
         DROP  STA                                                              
*                                                                               
SEQ      USING LIBUFFD,LIBUFFR2                                                 
         MVI   SEQ.LIACTN,LIAFIX       FIX ARRAY (TURN TO LINKED LIST)          
         GOTO1 VARREDIT,DMCB,SEQ.LIBUFFD                                        
         DROP  SEQ                                                              
*                                                                               
         L     R1,MSATAB                                                        
         A     R1,AMSAGSHM                                                      
         LA    R7,8(R1)            A(TABLE)                                     
         USING LIBBUFFD,R7                                                      
         MVC   MSACNT,LIBRECN      SAVE COUNTER IN TABLE1                       
         MVC   MSNCNT,MSACNT       SAVE COUNTER IN TABLE2                       
         DROP  R7                                                               
*                                                                               
         MVI   MSAGLOCK,0           UNLOCK THIS AGENCY'S TABLE                  
*                                                                               
         BRAS  RE,CHKTBZ                                                        
*                                                                               
GTX      DS    0H                                                               
*                                                                               
*CHECK IF THE ENTIRE TABLE IS LOCKED FOR REBUILD                                
*                                                                               
         TM    MSAGTLCK,X'80'                                                   
         BNO   GTX2                                                             
GTX1     GOTO1 VDMGR,DMCB,=C'OPMSG',=C'STAPACK - ENTER WAIT'                    
         MVC   DMCB+8,=F'3840'     10 MILSEC (TU)                               
         GOTO1 VDMGR,DMCB,(0,=CL8'DMTIME'),C'WAIT'                              
         GOTO1 VDMGR,DMCB,=C'OPMSG',=C'STAPACK - FINISH WAIT'                   
         B     GT02                                                             
*                                                                               
*CHECK IF THIS INDIVIDUAL AGENCY TABLE IS LOCKED                                
*                                                                               
GTX2     TM    MSAGLOCK,X'80'                                                   
         BNO   GTX4                                                             
         GOTO1 VDMGR,DMCB,=C'OPMSG',=C'STAPACK,AGY - ENTER WAIT'                
         MVC   DMCB+8,=F'3840'     10 MILSEC (TU)                               
         GOTO1 VDMGR,DMCB,(0,=CL8'DMTIME'),C'WAIT'                              
         GOTO1 VDMGR,DMCB,=C'OPMSG',=C'STAPACK,AGY - FINISH WAIT'               
         B     GT02                                                             
*                                                                               
GTX4     DS    0H                                                               
         SAM24                                                                  
         PROTON                                                                 
         DROP  R2,R3                                                            
*                                                                               
         XIT1  REGS=(R2)                                                        
         LTORG                                                                  
         SPACE 2                                                                
         EJECT                                                                  
*================================================================*              
* READ STATION MASTER RECORD                                     *              
*================================================================*              
         SPACE 1                                                                
GETSTA   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
*                                                                               
         MVI   STAKTYPE,C'S'                                                    
         CLI   STAPMED,C' '        ANNIE HAS A GRANTY BUG                       
         BNE   *+8                                                              
         MVI   STAPMED,C'T'                                                     
         MVC   STAKMED,STAPMED                                                  
         MVC   STAKCALL,STAPQSTA                                                
         CLI   STAKCALL+4,C' '                                                  
         BNE   *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         CLI   STAKCALL+4,C'/'     THIS WON'T BE THERE EITHER !                 
         BNE   *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,STAPAGY                                                  
         MVC   STAKCLT,=C'000'     CLIENT EXC RECS NOT INVOLVED                 
         MVC   STAKFILL,=C'000'                                                 
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=CL8'DMRDHI'),(0,=CL8'STATION'),KEY,IOA            
*                                                                               
         CLC   KEY(15),IOA                                                      
         BE    GETSTAX                                                          
         MVI   STAPERR,QSTP_CBLNF  SET STATION NOT FOUND                        
GETSTAX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* CHECK TABLE SIZE, SEND WARNING EMAIL IF > 85%                  *              
* INPUT: R2 = A(ONE MSAGTAB)                                     *              
*================================================================*              
CHKTBZ   NTR1  BASE=*,LABEL=*                                                   
         USING MSAGTABD,R2                                                      
         SR    R0,R0               PREPARE FOR DIVIDE                           
         L     R1,MSACNT           CURRENT # ENTRY IN TABLE                     
         MHI   R1,100              * 100%                                       
         D     R0,MSAMAX           / MAX # ENTRY IN TABLE                       
*                                                                               
         CHI   R1,75               > 75%                                        
         BL    CHKTBZX                                                          
*                                                                               
         MVC   AUTOMSG,SPACES                                                   
         MVC   AUTOMSG(L'WARNMSG),WARNMSG                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  AUTOMSG+L'WARNMSG(3),DUB+6(2)                                    
         MVI   AUTOMSG+L'WARNMSG+3,C'%'                                         
         MVC   AUTOMSG+L'WARNMSG+3+1(5),=C' FOR '                               
         MVC   AUTOMSG+L'WARNMSG+3+1+5(2),MSAGY                                 
         GOTO1 VDMGR,DMCB,=C'OPMSG',(L'AUTOMSG,AUTOMSG)                         
         DROP  R2                                                               
*                                                                               
CHKTBZX  XIT1                                                                   
         LTORG                                                                  
SPACES   DC    CL128' '                                                         
WARNMSG  DC    C'AUTONOTE*US-MF_FAC_NOTIFY:STAPACK TABLE AT '                   
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
       ++INCLUDE SPCNCBLTAB                                                     
         EJECT                                                                  
STAPD    DSECT                                                                  
STPDUB   DS    PL8                                                              
P0       DS    F                   THIS MUST BE JUST BEFORE DMCB!               
DMCB     DS    6F                                                               
SVRF     DS    F                                                                
RELO     DS    F                                                                
DUB      DS    D                                                                
VCALLOV  DS    A                                                                
VDMGR    DS    A                                                                
VPROTON  DS    A                                                                
VPROTOFF DS    A                                                                
VBINSRCH DS    A                                                                
VARREDIT DS    A                                                                
VQSORT   DS    A                                                                
AMSAGSHM DS    A                                                                
KEY      DS    CL20                                                             
SVKEY    DS    CL20                                                             
WORK     DS    CL48                                                             
AUTOMSG  DS    CL80                                                             
         ORG   AUTOMSG                                                          
WORKTAB  DS    CL16                                                             
         ORG                                                                    
OFFLINE  DS    C                                                                
SEQNUM   DS    XL2                                                              
         DS    XL7                 SPARE                                        
         DS    0H                                                               
RECORD   DS    XL7                                                              
         DS    0D                                                               
LIBUFFR  DS    XL(LIBUFFL)                                                      
         DS    0D                                                               
LIBUFFR2 DS    XL(LIBUFFL)                                                      
*                                                                               
IOA      DS    XL1024                                                           
STAPDX   EQU   *                                                                
         SPACE 2                                                                
       ++INCLUDE DDARREDITD                                                     
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACSD                                                     
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'103DDSTAPACK 04/20/16'                                      
         END                                                                    
