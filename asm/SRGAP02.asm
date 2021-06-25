*          DATA SET SRGAP02    AT LEVEL 006 AS OF 02/15/13                      
*PHASE T16F02B                                                                  
T16F02   TITLE 'SRGAP02 ($GAP) - WB MEDIACOM REQUEST'                           
T16F02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**$GP2**,RA,RR=RE                                    
         USING WORKD,RC            RC = A(WORKING STORAGE)                      
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         L     RE,0(R1)                                                         
         MVC   SRPARS,0(RE)        SAVE S/R PARAMETER LIST                      
SRPARMSD USING SRPARMD,SRPARS                                                   
*                                                                               
         L     R9,SRPARMSD.SRQASYSF                                             
         USING SYSFACD,R9          R9 = A(SYSTEM FACILITIES)                    
         L     RF,VSYSFAC2         GET THE SPOT SYSFAC                          
         USING SPSYSFAC,RF                                                      
         MVC   VRECUP,SRECUP                                                    
         DROP  RF                                                               
*                                                                               
         MVC   AUTL,SRPARMSD.SRQAUTL                                            
         L     RF,AUTL                                                          
         MVC   ATBUFF,TBUFF-UTLD(RF)  GETTING OUR MSG FROM TSAR BUFFER          
         L     RF,ATBUFF                                                        
         SHI   RF,2                                                             
         XR    RE,RE                                                            
         ICM   RE,3,0(RF)          GET THE LENGTH OF THE MESSAGE                
         STCM  RE,3,MQMSGLEN                                                    
*                                                                               
MAIN10   BAS   RE,INITLIZE         INITIALIZE COMMON VARIABLES                  
*                                                                               
         BAS   RE,WORKITIN         PROCESS WHAT WAS PASSED IN TBUFF             
         BNE   NO                                                               
*                                                                               
YES      SR    RC,RC               SET CC TO EQ                                 
NO       LTR   RC,RC               SET CC TO NEQ                                
XIT      XIT1                      RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZES COMMON VARIABLES                                                  
***********************************************************************         
INITLIZE NTR1                                                                   
         MVI   BITFLAG1,0                                                       
*                                                                               
         LR    R7,RC                                                            
         AHI   R7,IOA1-WORKD                                                    
         ST    R7,AIO1                                                          
*                                                                               
         LR    R7,RC                                                            
         AHI   R7,IOA2-WORKD                                                    
         ST    R7,AIO2                                                          
*                                                                               
         LR    R7,RC                                                            
         AHI   R7,SPULAREA-WORKD                                                
         ST    R7,ASPLAREA                                                      
*                                                                               
         MVC   AWRKRBUF,SRPARMSD.SRQATIA                                        
*                                                                               
         L     R8,VSSB             R8 = A(SSB)                                  
         USING SSBD,R8                                                          
         NI    SSBJFLAG,255-SSBJFWKR STOP ABEND LOOPS                           
         MVC   SYSNAME,SSBSYSNA                                                 
         MVC   SYSN1,SSBSYSN1                                                   
         MVC   RECLEN,SSBTWAL      SAVE TEMPSTR TWA RECORD LENGTH               
         DROP  R8                                                               
*                                                                               
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         OI    TPRGIND,X'14'       SET CONVERTED MAX IOS                        
         MVC   TERMNUM,TNUM        SAVE TERMINAL NUMBER                         
         MVC   USERNUM,TUSER       SAVE USER ID NUMBER                          
         DROP  R1                                                               
***************                                                                 
* COMFACS STUFF                                                                 
***************                                                                 
         L     R1,SRPARMSD.SRQACOMF    R1 = A(COMFACS)                          
         USING COMFACSD,R1                                                      
         MVC   VADDAY,CADDAY                                                    
         MVC   VDATCON,CDATCON                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VLOCKET,CLOCKET                                                  
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VPARSNIP,CPARSNIP                                                
         MVC   VREQTWA,CREQTWA                                                  
         DROP  R1                                                               
***************                                                                 
* CORERES STUFF                                                                 
***************                                                                 
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4,=X'D9000A0C'    SPOOL                                     
         GOTO1 VCALLOV,DMCB                                                     
         MVC   ASPOOL,DMCB                                                      
*                                                                               
         MVI   DMCB+7,X'14'           CLPACK                                    
         GOTO1 VCALLOV,DMCB                                                     
         MVC   VCLPACK,DMCB                                                     
*                                                                               
         MVI   DMCB+7,X'15'           CLUNPK                                    
         GOTO1 VCALLOV,DMCB                                                     
         MVC   VCLUNPK,DMCB                                                     
*                                                                               
         MVI   DMCB+7,X'7A'           STAPACK                                   
         GOTO1 VCALLOV,DMCB                                                     
         MVC   ASTAPACK,DMCB                                                    
***************                                                                 
* GET TODAY'S DATE                                                              
***************                                                                 
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,BTODAY)                                 
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    INIT10                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,WORK,WORK,F'1'                                       
*                                                                               
INIT10   GOTO1 VDATCON,DMCB,(3,BTODAY),(15,JDTTODAY)                            
*                                                                               
INITX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DETERMINES WHICH TYPE OF GAP MESSAGE WE HAVE AND THEN ROUTES IT TO            
* THE CORRECT PROCESSING ROUTINE                                                
***********************************************************************         
WORKITIN NTR1                                                                   
*****                                                                           
* PUT 255 NULLS AFTER THE MESSAGE SO WE DON'T PROCESS AS IF THERE WAS           
* RELEVANT DATA THERE                                                           
*****                                                                           
WITIN10  L     R7,ATBUFF                                                        
         SHI   R7,2                                                             
         XR    RE,RE                                                            
         ICM   RE,3,0(R7)          LENGTH OF THE MSG IN BUFFER                  
         LA    RE,2(R7,RE)         POINT AFTER THE MESSAGE                      
         XC    0(255,RE),0(RE)                                                  
*****                                                                           
         L     R7,ATBUFF                                                        
         SHI   R7,2                                                             
         XR    RE,RE                                                            
         ICM   RE,3,0(R7)          LENGTH OF THE MSG IN BUFFER                  
         SHI   RE,8                TAKE OFF 8 FOR THE WBRQUEST                  
*                                                                               
         LA    R7,2(R7)                                                         
         USING EVNTDSCT,R7         DSECT STARTS AT "WBRQUEST"                   
*                                                                               
         CHI   RE,255              CAN'T HANDLE MORE THAN 255 CHAR FOR          
         BNH   *+6                    MESSAGE BODY AS PARSNIP DEFINES           
         DC    H'0'                   1 BYTE FOR THE INPUT LENGTH               
*                                                                               
         XC    DMCB,DMCB                                                        
         LA    RF,EVMSGBDY                                                      
         ST    RF,DMCB                                                          
         STC   RE,DMCB                                                          
         GOTO1 VPARSNIP,DMCB,,(0,BLOCK),(X'08',0)                               
*                                                                               
         LA    R2,BLOCK                                                         
         USING PSND,R2                                                          
         MVI   QTEST,0                                                          
         XC    QNVALUES(QNVALSL),QNVALUES                                       
         XC    QPVALUES(QPVALSL),QPVALUES                                       
*                                                                               
WITIN20  CLI   PSNTAG,0            ANY MORE COMPONENTS?                         
         BE    WITIN50             NO MORE                                      
*                                                                               
         CLI   PSNTAG,PSNFLDQ      C'F' - FIELD COMPONENT?                      
         BE    WITIN30                                                          
*                                                                               
WITIN25  ICM   R2,15,PSNFLD        POINT TO THE NEXT FIELD COMPONENT            
         BZ    WITIN50             IF NONE, THEN WE'RE READY TO REQUEST         
         B     WITIN20                                                          
***************                                                                 
* FIELD COMPONENT                                                               
***************                                                                 
WITIN30  L     RF,AUTL             POINT TO THE FIELD                           
         USING UTLD,RF                                                          
         ICM   R3,15,PSNCOMP       POINT TO THE FIELD                           
         LA    RE,SPTKYWDS                                                      
         MVI   TPRG,X'04'          SPT/WRI                                      
         CLI   TSYS,2                                                           
         BE    WITIN33                                                          
         LA    RE,NETKYWDS                                                      
         MVI   TPRG,X'20'          NET/WRI                                      
         CLI   TSYS,3                                                           
         BE    WITIN33                                                          
         LA    RE,PRTKYWDS                                                      
         MVI   TPRG,X'05'          PRT/WRI                                      
         DROP  RF                                                               
*                                                                               
WITIN33  CLI   0(RE),0             END OF KEYWORDS                              
         BNE   WITIN36             NO                                           
         CLC   =C'TEST',0(R3)      TEST, DO NOT CREATE SOON JCL?                
         BNE   WITIN25                                                          
         MVI   QTEST,C'Y'          YES, ONLY A TEST                             
         B     WITIN25             THEN SKIP THIS FIELD                         
*                                                                               
WITIN36  LLC   RF,PSNLEN           TRY TO MATCH ON A KEYWORD                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(RE)                                                    
         BE    WITIN40                                                          
*                                                                               
         LA    RE,14(RE)           NEXT KEYWORD FOR THAT SYSTEM                 
         B     WITIN33                                                          
*                                                                               
WITIN40  ICM   RE,15,10(RE)        GET ADDRESS OF EBCDIC FIELD                  
         AR    RE,RB       <======   DISPLACED FROM BEGINNING OF CSECT          
         LA    RE,2(RE)                                                         
*                                                                               
         ICM   RF,15,PSNVAL           RF=A(VALUE COMPONENT)                     
         ICM   R3,15,PSNCOMP-PSND(RF) R3 POINTS TO ACTUAL VALUE                 
         LLC   RF,PSNLEN-PSND(RF)                                               
         SHI   RF,3                REDUCE FOR "" AND 1 MORE FOR EX              
         EX    RF,*+8                                                           
         B     WITIN25                                                          
         MVC   0(0,RE),1(R3)                                                    
***********************************                                             
* LOAD THE SCREEN FOR REQTWA TO USE                                             
***********************************                                             
WITIN50  CLI   QTEST,C'Y'           DO NOT CREATE SOON JCL?                     
         BE    WITINYES             YES, THIS IS ONLY A TEST                    
*                                                                               
         L     R3,SRPARMSD.SRQATWA  R3 = A(TWA)                                 
         USING T204FFD,R3                                                       
         XC    DMCB(24),DMCB                                                    
         XC    6(4,R3),6(R3)        CLEAR LIMIT ACCESS AS SERVICE REQ           
         XC    12(2,R3),12(R3)          & AUTHORISATION VALUES                  
*                                                                               
         L     R4,AUTL              POINT TO THE FIELD                          
         USING UTLD,R4                                                          
         CLI   TSYS,2                                                           
         BNE   WITIN60                                                          
********                                                                        
* SYS=SPOT                                                                      
********                                                                        
         MVC   DMCB+4(4),=X'D90204FF'                                           
         GOTO1 VCALLOV,DMCB,(0,64(R3)),,0                                       
         MVC   DMCB+4(4),=X'D90204F1'                                           
         GOTO1 VCALLOV,DMCB,(0,CONTAGH),,0                                      
*                                                                               
         GOTO1 PUTTOTWA,SPWRIWB                                                 
         LA    R2,WRITITH                                                       
*                                                                               
         CLC   MQROUTG+2(16),=CL16' '                                           
         BNH   WITIN90             NO MQROUTING, THEN QUALIFIER                 
         MVC   8(16,R2),MQROUTG+2      MEANS NOTHING                            
         MVI   5(R2),16                                                         
         CLC   QUALIFR+2(16),=CL16' '                                           
         BNH   WITIN90                                                          
         LA    RE,8+16(R2)                                                      
         MVC   0(16,RE),QUALIFR+2                                               
         MVI   5(R2),32                                                         
         B     WITIN90                                                          
********                                                                        
* SYS=NET                                                                       
********                                                                        
WITIN60  CLI   TSYS,3                                                           
         BNE   WITIN70                                                          
         USING NE$T320FFD,R3                                                    
         MVC   DMCB+4(4),=X'D90320FF'                                           
         GOTO1 VCALLOV,DMCB,(0,64(R3)),,0                                       
         MVC   DMCB+4(4),=X'D90320E0'                                           
         GOTO1 VCALLOV,DMCB,(0,NE$CONTAGH),,0                                   
*                                                                               
         GOTO1 PUTTOTWA,NEWRIWB                                                 
*                                                                               
         CLC   NWBFLID+2(10),=CL16' '  DO WE HAVE A FLIGHT ID TO USE?           
         BNH   WITIN90                                                          
         LA    R1,NE$SPLOTH           PUT FLTID IN THE 'OTHER' FIELD            
         MVC   0(6,R1),=C'FLTID='     OPTION FIELD WILL CHANGE                  
         MVC   6(10,R1),NWBFLID+2                                               
*                                                                               
         LA    RE,5+10(R1)         NOW CALC ADJUSTED L(OPTION FIELD)            
WITIN61  CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,WITIN61                                                       
*                                                                               
         LA    RE,1(RE)                                                         
         LA    R1,NE$SPLOTH                                                     
         SR    RE,R1                                                            
         STC   RE,NE$SPLOTHH+5                                                  
*                                                                               
         LA    R2,NE$SPLTITLH                                                   
         CLC   MQROUTG+2(16),=CL16' '                                           
         BNH   WITIN90             NO MQROUTING, THEN QUALIFIER                 
         MVC   8(16,R2),MQROUTG+2      MEANS NOTHING                            
         MVI   5(R2),16                                                         
         CLC   QUALIFR+2(16),=CL16' '                                           
         BNH   WITIN90                                                          
         LA    RE,8+16(R2)                                                      
         MVC   0(16,RE),QUALIFR+2                                               
         MVI   5(R2),32                                                         
         B     WITIN90                                                          
********                                                                        
* SYS=PRT                                                                       
********                                                                        
         USING PR$T405FFD,R3                                                    
WITIN70  MVC   DMCB+4(4),=X'D90405FF'                                           
         GOTO1 VCALLOV,DMCB,(0,64(R3)),,0                                       
         MVC   DMCB+4(4),=X'D90405F1'                                           
         GOTO1 VCALLOV,DMCB,(0,PR$CONTAGH),,0                                   
*                                                                               
         XC    TACCS,TACCS         CLEAR THIS SO THAT DDMASTC DOESN'T           
         XC    TAUTH,TAUTH           PULL BOGUS VALUES                          
*                                                                               
         GOTO1 PUTTOTWA,PRWRIWB                                                 
*                                                                               
         CLI   PWBMED+2,C'I'       INTERACTIVE?                                 
         BNE   WITIN70M                                                         
         GOTO1 PUTTOTWA,PIMRCSEC                                                
         B     WITIN71                                                          
*                                                                               
WITIN70M CLI   PWBMED+2,C'M'       MAGAZINE?                                    
         BNE   WITIN70N                                                         
         GOTO1 PUTTOTWA,PMMRCSEC                                                
         B     WITIN71                                                          
*                                                                               
WITIN70N CLI   PWBMED+2,C'N'       NEWSPAPER?                                   
         BNE   WITIN70O                                                         
         GOTO1 PUTTOTWA,PNMRCSEC                                                
         B     WITIN71                                                          
*                                                                               
WITIN70O GOTO1 PUTTOTWA,POMRCSEC   OUTDOOR                                      
*                                                                               
WITIN71  DS    0H                                                               
         LA    R2,PR$WRITITH                                                    
         CLC   MQROUTG+2(16),=CL16' '                                           
         BNH   WITIN90             NO MQROUTING, THEN QUALIFIER                 
         MVC   8(16,R2),MQROUTG+2      MEANS NOTHING                            
         MVI   5(R2),16                                                         
         CLC   QUALIFR+2(16),=CL16' '                                           
         BNH   WITIN90                                                          
         LA    RE,8+16(R2)                                                      
         MVC   0(16,RE),QUALIFR+2                                               
         MVI   5(R2),32                                                         
         DROP  R3,R4                                                            
***********************************                                             
* SET UP THE REQUEST CARD(S)                                                    
***********************************                                             
WITIN90  L     R3,AUTL                                                          
         USING UTLD,R3                                                          
         XC    TMPSPOOK,TMPSPOOK                                                
         LA    R2,TMPSPOOK                                                      
         USING SPOOK,R2                                                         
         MVC   SPOOKUID,TUSER                                                   
         MVC   SPOOKDES,TUSER                                                   
         MVC   SPOOKAGY,TAGY                                                    
         MVC   SPOOKDID,=C'DDS'    USER INITIALS  (SOON,DDS)                    
         MVI   SPOOKWEN,2          SET SOON STATUS                              
         MVC   SPOOKSYS,=C'SP'     SPOT SYSTEM                                  
         MVC   SPOOKEOD,=C'RW'     THESE ARE DIFF FOR EACH SYS                  
         MVC   SPOOKJCL,=C'RW'                                                  
         CLI   TSYS,2                                                           
         BE    WITIN95                                                          
         MVC   SPOOKSYS,=C'NE'     NET SYSTEM                                   
         MVC   SPOOKEOD,=C'W2'                                                  
         MVC   SPOOKJCL,=C'W2'                                                  
         CLI   TSYS,3                                                           
         BE    WITIN95                                                          
         MVC   SPOOKSYS,=C'PP'     PRINT SYSTEM                                 
         MVC   SPOOKEOD,=C'WR'                                                  
         MVC   SPOOKJCL,=C'WR'                                                  
         DROP  R3                                                               
*                                                                               
WITIN95  L     R3,SRPARMSD.SRQATWA  R3 = A(TWA)                                 
         L     R4,SRPARMSD.SRQACOMF R4 = A(COMFACS)                             
*&&DO                                                                           
         GOTO1 VREQTWA,DMCB,(0,(R3)),REQCARD1,(0,VDATAMGR),(R4),(X'C0',X        
               (R2)),(0,=CL80' ')                                               
*&&                                                                             
         GOTO1 VREQTWA,DMCB,(0,(R3)),REQCARD1,(0,VDATAMGR),(R4),(X'80',X        
               (R2)),(0,=CL80' ')                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(38),=C'REPORT XXX,9999 WILL BE PROCESSED SOON'              
         L     RE,8(R1)                                                         
         MVC   WORK+7(3),2(RE)                                                  
         LH    RF,6(RE)                                                         
         CVD   RF,WORK+47(8)                                                    
         OI    WORK+54,X'0F'                                                    
         UNPK  WORK+11(4),WORK+47(8)                                            
*                                                                               
WITINYES B     YES                                                              
*                                                                               
WITINNO  B     NO                                                               
***************                                                                 
* REQUEST ELEMENTS - BYTE 0 = NTH INPUT FIELD (HAVE TO BE IN ORDER)             
*                    BYTE 1 = L(TEXT) ADJUSTED FOR 'EX' INSTR                   
***************                                                                 
SPWRIWB  DS    0C                                                               
         DC    AL1(02),AL1(5),C'WRITER'     RECORD                              
         DC    AL1(03),AL1(5),C'REPORT'     ACTION                              
         DC    AL1(05),AL1(7),C'SOON,DDS'   PRINT                               
         DC    AL1(06),AL1(3),C'DOWN'       OUTPUT                              
         DC    AL1(09),AL1(6),C'WB TEST'    NAME                                
*                                                                               
* THESE FIELDS WILL BE OVERWRITTEN BUT BE WARY OF THE LENGTHS                   
SWBRQID  DC    AL1(10),AL1(26),CL27' '      DESC                                
         DC    AL1(11),AL1(1),C'  '         REQ FLT                             
SWBMED   DC    AL1(12),AL1(0),CL1' '        MEDIA                               
SWBCLT   DC    AL1(13),AL1(7),CL8' '        CLIENT                              
SWBPRD   DC    AL1(14),AL1(7),CL8' '        PRD/GRP                             
SWBEST   DC    AL1(15),AL1(8),CL9' '        EST                                 
SWBMKT   DC    AL1(16),AL1(8),CL9' '        MKT/GRP                             
SWBSTA   DC    AL1(17),AL1(7),CL8' '        STATION                             
SWBPERD  DC    AL1(18),AL1(16),CL17' '      PERIOD                              
* OPTIONS SECTION                                                               
         DC    AL1(20),AL1(37)                                                  
         DC    C'WIDE,DOWNHEAD,DOWN,DOWNFIX,NOHEAD,FILE'                        
* HEADERS SECTION                                                               
         DC    AL1(22),AL1(6),C'MEDCODE'                                        
         DC    AL1(23),AL1(6),C'CLTCODE'                                        
         DC    AL1(24),AL1(6),C'CLTNAME'                                        
         DC    AL1(25),AL1(6),C'PRDCODE'                                        
         DC    AL1(26),AL1(6),C'PRDNAME'                                        
         DC    AL1(27),AL1(6),C'ESTCODE'                                        
* MIDLINE SECTION                                                               
         DC    AL1(28),AL1(5),C'EDATES'                                         
* ROWS    SECTION                                                               
         DC    AL1(29),AL1(6),C'UCOM=E1'                                        
         DC    AL1(30),AL1(6),C'MKTCODE'                                        
         DC    AL1(31),AL1(2),C'STA'                                            
         DC    AL1(32),AL1(2),C'LEN'                                            
         DC    AL1(33),AL1(2),C'DPT'                                            
         DC    AL1(34),AL1(4),C'DPTCD'                                          
* COLUMNS SECTION                                                               
         DC    AL1(35),AL1(4),C'SLSPT'       A                                  
         DC    AL1(36),AL1(4),C'SPOTS'       B                                  
         DC    AL1(37),AL1(4),C'SLDEM'       C                                  
         DC    AL1(38),AL1(5),C'BYPDEM'      D                                  
         DC    AL1(39),AL1(5),C'SLDEM2'      E                                  
         DC    AL1(40),AL1(6),C'BYPDEM2'     F                                  
         DC    AL1(41),AL1(5),C'SLDEM3'      G                                  
         DC    AL1(42),AL1(6),C'BYPDEM3'     H                                  
         DC    AL1(43),AL1(5),C'SLDEM4'      I                                  
         DC    AL1(44),AL1(6),C'BYPDEM4'     J                                  
         DC    AL1(45),AL1(4),C'SLDOL'       K                                  
         DC    AL1(46),AL1(4),C'BYDOL'       L                                  
         DC    AL1(47),AL1(4),C'BYNET'       M                                  
         DC    X'00'                                                            
***************                                                                 
* REQUEST ELEMENTS - BYTE 0 = NTH INPUT FIELD                                   
*                    BYTE 1 = L(TEXT) ADJUSTED FOR 'EX' INSTR                   
***************                                                                 
NEWRIWB  DS    0C                                                               
         DC    AL1(02),AL1(5),C'WRITER'     RECORD                              
         DC    AL1(03),AL1(5),C'REPORT'     ACTION                              
         DC    AL1(05),AL1(7),C'SOON,DDS'   PRINT                               
         DC    AL1(06),AL1(3),C'DOWN'       OUTPUT                              
         DC    AL1(09),AL1(7),C'XREF-NET'   NAME                                
* THESE FIELDS WILL BE OVERWRITTEN BUT BE WARY OF THE LENGTHS                   
NWBRQID  DC    AL1(10),AL1(25),CL26' '      DESC                                
         DC    AL1(11),AL1(2),C'   '        REQ FLT                             
NWBCLT   DC    AL1(12),AL1(7),CL8' '        CLIENT                              
NWBPRD   DC    AL1(13),AL1(7),CL8' '        PRODUCT                             
NWBEST   DC    AL1(14),AL1(7),CL8' '        ESTIMATE                            
NWBNTWK  DC    AL1(15),AL1(7),CL8' '        NETWORK                             
NWBDPT   DC    AL1(16),AL1(7),CL8' '        DAYPART                             
NWBPKG   DC    AL1(17),AL1(7),CL8' '        PACKAGE                             
NWBSDT   DC    AL1(18),AL1(7),CL8' '        START                               
NWBEDT   DC    AL1(19),AL1(7),CL8' '        END                                 
         DC    AL1(20),AL1(0),C'M'          FLAVOR                              
* L HEADS SECTION                                                               
         DC    AL1(23),AL1(4),C'MEDIA'                                          
         DC    AL1(24),AL1(6),C'CLIENTC'                                        
         DC    AL1(25),AL1(6),C'CLINAME'                                        
         DC    AL1(26),AL1(4),C'PRODC'                                          
* R HEADS SECTION                                                               
         DC    AL1(27),AL1(7),C'PRODNAME'                                       
         DC    AL1(28),AL1(3),C'ESTC'                                           
         DC    AL1(29),AL1(6),C'ESTNAME'                                        
* DETAILS SECTION                                                               
         DC    AL1(31),AL1(6),C'ESTDATE'                                        
         DC    AL1(32),AL1(4),C'FLTID'                                          
         DC    AL1(33),AL1(6),C'NETWORK'                                        
         DC    AL1(34),AL1(2),C'LEN'                                            
         DC    AL1(35),AL1(6),C'DPTCODE'                                        
         DC    AL1(36),AL1(6),C'PROGRAM'                                        
* COLUMNS SECTION                                                               
         DC    AL1(43),AL1(4),C'UNITS'                  A                       
         DC    AL1(44),AL1(2),C'ERT'                    B                       
         DC    AL1(45),AL1(2),C'EIT'                    C                       
         DC    AL1(46),AL1(2),C'ERH'                    D                       
         DC    AL1(47),AL1(2),C'EIH'                    E                       
         DC    AL1(48),AL1(3),C'ERT2'                   F                       
         DC    AL1(49),AL1(3),C'EIT2'                   G                       
         DC    AL1(50),AL1(18),C'ACT,H=TIME,H2=GROSS'   H                       
         DC    AL1(51),AL1(9),C'NET,H=TIME'             I                       
         DC    AL1(52),AL1(2),C'NIN'                    J                       
         DC    AL1(53),AL1(3),C'ORSN'                   K                       
* OPTIONS SECTION                                                               
         DC    AL1(55),AL1(42)                                                  
         DC    C'DOWN,DOWNHEADX,DOWNHEAD,DOWNFIX,NOHEAD,FILE'                   
NWBOTHR  DC    AL1(57),AL1(19),CL20' '                                          
         DC    X'00'                                                            
* NOT REALLY SCREEN FIELDS SO 1ST BYTE MEANS NOTHING                            
NWBFLID  DC    AL1(55),AL1(09),CL10' '                                          
MQROUTG  DC    AL1(55),AL1(15),CL16' '                                          
QUALIFR  DC    AL1(55),AL1(15),CL16' '                                          
***************                                                                 
* REQUEST ELEMENTS - BYTE 0 = NTH INPUT FIELD (HAVE TO BE IN ORDER)             
*                    BYTE 1 = L(TEXT) ADJUSTED FOR 'EX' INSTR                   
***************                                                                 
PRWRIWB  DS    0C                                                               
         DC    AL1(02),AL1(5),C'WRITER'     RECORD                              
         DC    AL1(03),AL1(5),C'REPORT'     ACTION                              
         DC    AL1(05),AL1(7),C'SOON,DDS'   PRINT                               
         DC    AL1(06),AL1(3),C'DOWN'       OUTPUT                              
         DC    AL1(09),AL1(5),C'WB-INT'     NAME                                
*                                                                               
* THESE FIELDS WILL BE OVERWRITTEN BUT BE WARY OF THE LENGTHS                   
PWBRQID  DC    AL1(10),AL1(26),CL27' '      DESC                                
         DC    AL1(11),AL1(2),C'   '        REQ FLT                             
PWBMED   DC    AL1(12),AL1(0),CL1' '        MEDIA                               
PWBCLT   DC    AL1(13),AL1(6),CL7' '        CLT                                 
PWBPRD   DC    AL1(14),AL1(6),CL7' '        PROD                                
PWBEST   DC    AL1(15),AL1(8),CL9' '        ESTIMATE                            
PWBPUB   DC    AL1(16),AL1(13),CL14' '      PUB                                 
PWBDIV   DC    AL1(17),AL1(2),CL3' '        DIV                                 
PWBREQ   DC    AL1(18),AL1(6),CL7' '        REQ                                 
PWBDIST  DC    AL1(19),AL1(6),CL7' '        DIST                                
PWBADCD  DC    AL1(20),AL1(5),CL6' '        ADCODE                              
PWBDTYP  DC    AL1(21),AL1(1),CL2' '        DATE TYPE                           
PWBPERD  DC    AL1(22),AL1(16),CL17' '      PERIOD                              
* OPTIONS SECTION                                                               
         DC    AL1(24),AL1(37)                                                  
         DC    C'DOWN,DOWNHEAD,DOWNFIX,WIDE,NOHEAD,FILE'                        
* HEADERS SECTION                                                               
         DC    AL1(26),AL1(6),C'MEDCODE'                                        
         DC    AL1(27),AL1(2),C'CLI'                                            
         DC    AL1(28),AL1(6),C'CLINAME'                                        
         DC    AL1(29),AL1(6),C'PRDCODE'                                        
         DC    AL1(30),AL1(6),C'PRDNAME'                                        
         DC    AL1(31),AL1(5),C'ESTNUM'                                         
* MIDLINE SECTION                                                               
         DC    AL1(32),AL1(7),C'ESTDATES'                                       
         DC    X'00'                                                            
********                                                                        
* PRINT'S INTERACTIVE  MIDLINE, ROWS, AND COLUMN'S SECTION                      
********                                                                        
PIMRCSEC DS    0X                                                               
* ROWS    SECTION                                                               
         DC    AL1(33),AL1(18),C'UCOM=E1,H=WB FLT ID'                           
         DC    AL1(34),AL1(05),C'PUBNUM'                                        
         DC    AL1(35),AL1(07),C'PUBZNENA'                                      
         DC    AL1(36),AL1(06),C'FREQNCY'                                       
* COLUMNS SECTION                                                               
         DC    AL1(39),AL1(06),C'INSDATE'       A                               
         DC    AL1(40),AL1(05),C'CPMACT'        B                               
         DC    AL1(41),AL1(05),C'CPMEST'        C                               
         DC    AL1(42),AL1(03),C'IMPS'          D                               
         DC    AL1(43),AL1(04),C'$ORDN'         E                               
         DC    AL1(44),AL1(04),C'$ORDG'         F                               
         DC    AL1(45),AL1(06),C'BUYCOMS'       G                               
         DC    X'00'                                                            
********                                                                        
* PRINT'S MAGAZINE  MIDLINE, ROWS, AND COLUMN'S SECTION                         
********                                                                        
PMMRCSEC DS    0X                                                               
* ROWS    SECTION                                                               
         DC    AL1(33),AL1(18),C'UCOM=E1,H=WB FLT ID'                           
         DC    AL1(34),AL1(05),C'PUBNUM'                                        
         DC    AL1(35),AL1(06),C'INSDATE'                                       
         DC    AL1(36),AL1(05),C'ONSALE'                                        
         DC    AL1(37),AL1(04),C'SPACE'                                         
         DC    AL1(38),AL1(06),C'FREQNCY'                                       
* COLUMNS SECTION                                                               
         DC    AL1(39),AL1(04),C'$ORDN'      A                                  
         DC    AL1(40),AL1(04),C'$ORDG'      B                                  
         DC    AL1(41),AL1(03),C'CIRC'       C                                  
         DC    AL1(42),AL1(04),C'ICCOM'      D                                  
         DC    X'00'                                                            
********                                                                        
* PRINT'S NEWSPAPER MIDLINE, ROWS, AND COLUMN'S SECTION                         
********                                                                        
PNMRCSEC DS    0X                                                               
* ROWS    SECTION                                                               
         DC    AL1(33),AL1(18),C'UCOM=E1,H=WB FLT ID'                           
         DC    AL1(34),AL1(05),C'PUBNUM'                                        
         DC    AL1(35),AL1(06),C'FREQNCY'                                       
         DC    AL1(36),AL1(05),C'ONSALE'                                        
         DC    AL1(37),AL1(05),C'MARKET'                                        
         DC    AL1(38),AL1(06),C'INSDATE'                                       
* COLUMNS SECTION                                                               
         DC    AL1(39),AL1(19),C'CC=LINEAGE,H=LINEAGE'        A                 
         DC    AL1(40),AL1(24),C'CC=BW/COLOR,10,H=BW/COLOR'   B                 
         DC    AL1(41),AL1(04),C'$ORDN'                       C                 
         DC    AL1(42),AL1(04),C'$ORDG'                       D                 
         DC    AL1(43),AL1(06),C'BUYCOMS'                     E                 
         DC    X'00'                                                            
********                                                                        
* PRINT'S OUTDOOR   MIDLINE, ROWS, AND COLUMN'S SECTION                         
********                                                                        
POMRCSEC DS    0X                                                               
* ROWS    SECTION                                                               
         DC    AL1(33),AL1(18),C'UCOM=E1,H=WB FLT ID'                           
         DC    AL1(34),AL1(05),C'MARKET'                                        
         DC    AL1(35),AL1(05),C'PUBNUM'                                        
         DC    AL1(36),AL1(06),C'INSDATE'                                       
         DC    AL1(37),AL1(06),C'FREQNCY'                                       
* COLUMNS SECTION                                                               
         DC    AL1(39),AL1(06),C'REGULAR'                     A                 
         DC    AL1(40),AL1(04),C'ILLUM'                       B                 
         DC    AL1(41),AL1(03),C'SHOW'                        C                 
         DC    AL1(42),AL1(04),C'$ORDN'                       D                 
         DC    AL1(43),AL1(06),C'OUTSPC2'                     E                 
         DC    AL1(44),AL1(04),C'$ORDG'                       E                 
         DC    X'00'                                                            
*                                                                               
REQCARD1 DC    CL106''                                                          
REQCARD2 DC    CL106''                                                          
*                                                                               
SPTKYWDS DS    0C                                                               
         DC    CL10'REQUESTID',AL4(SWBRQID-T16F02)                              
         DC    CL10'MEDIA',AL4(SWBMED-T16F02)                                   
         DC    CL10'CLIENT',AL4(SWBCLT-T16F02)                                  
         DC    CL10'PRODUCT',AL4(SWBPRD-T16F02)                                 
         DC    CL10'ESTIMATE',AL4(SWBEST-T16F02)                                
         DC    CL10'MARKET',AL4(SWBMKT-T16F02)                                  
         DC    CL10'STATION',AL4(SWBSTA-T16F02)                                 
         DC    CL10'PERIOD',AL4(SWBPERD-T16F02)                                 
         DC    CL10'MQROUTING',AL4(MQROUTG-T16F02)  NOT A SCREEN FIELD          
         DC    CL10'QUALIFIER',AL4(QUALIFR-T16F02)                              
         DC    X'00'                                                            
*                                                                               
NETKYWDS DS    0C                                                               
         DC    CL10'REQUESTID',AL4(NWBRQID-T16F02)                              
         DC    CL10'CLIENT',AL4(NWBCLT-T16F02)                                  
         DC    CL10'PRODUCT',AL4(NWBPRD-T16F02)                                 
         DC    CL10'ESTIMATE',AL4(NWBEST-T16F02)                                
         DC    CL10'NETWORK',AL4(NWBNTWK-T16F02)                                
         DC    CL10'DAYPART',AL4(NWBDPT-T16F02)                                 
         DC    CL10'PACKAGE',AL4(NWBPKG-T16F02)                                 
         DC    CL10'START',AL4(NWBSDT-T16F02)                                   
         DC    CL10'END',AL4(NWBEDT-T16F02)                                     
         DC    CL10'FLIGHTID',AL4(NWBFLID-T16F02)  NOT A SCREEN FIELD           
         DC    CL10'MQROUTING',AL4(MQROUTG-T16F02)  NOT A SCREEN FIELD          
         DC    CL10'QUALIFIER',AL4(QUALIFR-T16F02)                              
         DC    X'00'                                                            
*                                                                               
PRTKYWDS DS    0C                                                               
         DC    CL10'REQUESTID',AL4(PWBRQID-T16F02)                              
         DC    CL10'MEDIA',AL4(PWBMED-T16F02)                                   
         DC    CL10'CLIENT',AL4(PWBCLT-T16F02)                                  
         DC    CL10'PRODUCT',AL4(PWBPRD-T16F02)                                 
         DC    CL10'ESTIMATE',AL4(PWBEST-T16F02)                                
         DC    CL10'PUBLICATION',AL4(PWBPUB-T16F02)                             
         DC    CL10'DIVISION',AL4(PWBDIV-T16F02)                                
         DC    CL10'REGION',AL4(PWBREQ-T16F02)                                  
         DC    CL10'DISTRICT',AL4(PWBDIST-T16F02)                               
         DC    CL10'ADCODE',AL4(PWBADCD-T16F02)                                 
         DC    CL10'DATETYPE',AL4(PWBDTYP-T16F02)                               
         DC    CL10'PERIOD',AL4(PWBPERD-T16F02)                                 
         DC    CL10'MQROUTING',AL4(MQROUTG-T16F02)  NOT A SCREEN FIELD          
         DC    CL10'QUALIFIER',AL4(QUALIFR-T16F02)                              
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* PUTS INFORMATION OUT TO THE TWA (SCREENS LOADED BASED ON SYSTEM)              
*                                                                               
* ON ENTRY:    R1                  A(FIELD TABLE)                               
***********************************************************************         
PUTTOTWA NTR1                                                                   
         L     R3,SRPARMSD.SRQATWA  R3 = 1ST FIELD IN A(TWA)                    
         LA    R3,64(R3)                                                        
*                                                                               
         MVI   INPCNTR,0            INPUT FIELD COUNTER                         
         XR    RF,RF                RF WILL BE USED TO BUMP INP CNTR            
*                                                                               
PT2TWA10 CLI   0(R3),0             END OF TWA?                                  
         BE    PT2TWAX             WE'RE DONE                                   
*                                                                               
         TM    1(R3),X'20'         PROTECTED FIELD?                             
         BNZ   PT2TWA15            YES, CHECK NEXT FIELD                        
         LA    RF,1(RF)                                                         
         STC   RF,INPCNTR          SAVE THE COUNTER                             
*                                                                               
         CLC   INPCNTR,0(R1)       MATCH ENTRY IN FIELD TABLE?                  
         BE    PT2TWA20                                                         
PT2TWA15 LLC   R0,0(R3)            R3 = A(NEXT FIELD) IN TWA                    
         AR    R3,R0                                                            
         LLC   RF,INPCNTR          RELOAD INPUT COUNTER                         
         B     PT2TWA10                                                         
*                                                                               
PT2TWA20 LLC   RE,1(R1)            R1 = L(INPUT STRING) FOR 'EX' INSTR          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),2(R1)       COPY INPUT STRING TO TWA INPUT FIELD         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R3),ALLSPCES    ANYTHING IN INPUT FIELD?                     
         BNH   PT2TWA40            NOTHING, SKIP THIS INPUT                     
*                                                                               
         LA    RF,8(RE,R3)                                                      
PT2TWA25 CLI   0(RF),C' '                                                       
         BH    PT2TWA30                                                         
         BCT   RF,PT2TWA25                                                      
*                                                                               
PT2TWA30 LA    R0,8(R3)            SET LENGTH IN FIELD HEADER                   
         SR    RF,R0                                                            
         LA    RF,1(RF)                                                         
         STC   RF,5(R3)                                                         
         STC   RF,7(R3)                                                         
         OI    6(R3),X'80'                                                      
*                                                                               
PT2TWA40 LA    R1,2+1(R1,RE)       R1 = A(NEXT INPUT STRING)                    
         B     PT2TWA15                                                         
*                                                                               
PT2TWAX  B     YES                                                              
         LTORG                                                                  
* FIELDS THAT WON'T FIT ON THE SCREEN FIELD                                     
*                                                                               
MSGSTARL DC    H'80'                                                            
         DC    80C'*'                                                           
MSGPROBL DC    H'80'                                                            
         DC    CL49' '                                                          
         DC    CL31' '                                                          
MSG1L    DC    H'80'                                                            
MSG1     DC    CL80' '                                                          
*                                                                               
MSG2L    DC    H'80'                                                            
MSG2     DC    CL80' '                                                          
*                                                                               
ALLSPCES DC    132C' '                                                          
         SPACE 1                                                                
DMRDHI   DC    C'DMRDHI '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMADD    DC    C'DMADD  '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMUNLK   DC    C'DMUNLK '                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC'                                                        
*                                                                               
*                                                                               
GFILE    DC    CL8'GFILE'                                                       
PRTQUE   DC    CL8'PRTQUE'                                                      
*                                                                               
SPTFILE  DC    C'SPTFILE'                                                       
SPTDIR   DC    C'SPTDIR'                                                        
CTFILE   DC    C'CTFILE'                                                        
STATION  DC    C'STATION'                                                       
XSPDIR   DC    CL8'XSPDIR'                                                      
XSPFILE  DC    CL8'XSPFIL'                                                      
*========================================================                       
* SPTDIR COMMANDS                                                               
*========================================================                       
         SPACE 1                                                                
ADDDIR   DS    0H                                                               
         BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(SPTDIR)                                                      
*                                                                               
ADDXKEY  DS    0H                                                               
         BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(XSPDIR)                                                      
*                                                                               
HIGH     MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(SPTDIR)                                                      
*                                                                               
HIGHXSP  MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(XSPDIR)                                                      
*                                                                               
SEQ      BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(SPTDIR)                                                      
*                                                                               
SEQXSP   BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(XSPDIR)                                                      
*                                                                               
WRITE    BRAS  R1,GODIR                                                         
         DC    AL4(DMWRT)                                                       
         DC    AL4(SPTDIR)                                                      
*                                                                               
HIGHCT   MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(CTFILE)                                                      
*                                                                               
WRTCT    BRAS  R1,GODIR                                                         
         DC    AL4(DMWRT)                                                       
         DC    AL4(CTFILE)                                                      
*                                                                               
ADDCT    BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(CTFILE)                                                      
*                                                                               
GODIR    NTR1  BASE=*,LABEL=*                                                   
         ICM   RE,15,0(R1)         COMMAND                                      
         A     RE,RELO                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
*                                                                               
         ICM   RE,15,4(R1)         FILE NAME                                    
         A     RE,RELO                                                          
         ST    RE,DMCB+4                                                        
         CLC   =C'CTFILE',0(RE)    ARE WE LOOKING AT THE CONTROL FILE           
         BNE   GODIR10                                                          
         LA    RE,KEY                                                           
         ST    RE,DMCB+8                                                        
         L     RE,AIO                                                           
         ST    RE,DMCB+12                                                       
         B     GODIR20                                                          
*                                                                               
GODIR10  LA    RE,KEYSAVE                                                       
         ST    RE,DMCB+8                                                        
         LA    RE,KEY                                                           
         ST    RE,DMCB+12                                                       
*                                                                               
GODIR20  GOTO1 VDATAMGR,DMCB                                                    
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   DMINBTS,0           RESET                                        
         XIT1                                                                   
         LTORG                                                                  
         SPACE 1                                                                
*========================================================                       
* FILE COMMANDS                                                                 
*========================================================                       
         SPACE 1                                                                
GET      BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'               DSPL TO DISK ADDRESS                         
         SPACE 1                                                                
GETXSP   BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(XSPFILE)                                                     
         DC    H'36'               DSPL TO DISK ADDRESS                         
*                                                                               
PUT      BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'                                                            
         SPACE 1                                                                
PUTXSP   BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(XSPFILE)                                                     
         DC    H'36'               DSPL TO DISK ADDRESS                         
*                                                                               
ADD      BRAS  R1,GOFILE                                                        
         DC    AL4(ADDREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'                                                            
*                                                                               
GOFILE   NTR1  BASE=*,LABEL=*                                                   
         ICM   RE,15,0(R1)                                                      
         A     RE,RELO                                                          
         ST    RE,DMCB             SET COMMAND ADDRESS                          
         MVC   DMCB(1),DMINBTS                                                  
*                                                                               
         ICM   RE,15,4(R1)                                                      
         A     RE,RELO                                                          
         ST    RE,DMCB+4           SET FILENAME ADDRESS                         
*                                                                               
         LA    RE,KEY                                                           
         AH    RE,8(R1)            GET DSPL OF DISK ADDRESS IN KEY              
         ST    RE,DMCB+8                                                        
*                                                                               
         MVC   DMCB+12(4),AIO                                                   
*                                                                               
         LA    RE,DMWORK                                                        
         ST    RE,DMCB+16                                                       
*                                                                               
         GOTO1 VDATAMGR,DMCB                                                    
         TM    8(R1),X'02'         DELETED RECORD?                              
         BNZ   GOFILX              YEAH, WE DON'T WANNA DIE                     
         TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GOFILX   MVI   DMINBTS,0           RESET                                        
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FACIDTAB                                                       
       ++INCLUDE FACIDTABD                                                      
         SPACE                                                                  
*                                                                               
EVNTDSCT DSECT              *** WE DO NOT GET THE FP=AGY=SJSYS=SPT              
EVSRVHDR DS    CL8                 'WBRQUEST'                                   
EVMSGBDY DS    0C                  BODY OF MESSAGE                              
***                                  NAME="VALUE",NAME="VALUE",ETC.             
                                                                                
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
SRPARS   DS    0XL32                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
SRPAR7   DS    A                                                                
SRPAR8   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
*                                                                               
RELO     DS    A                                                                
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
AUTL     DS    A                                                                
ATBUFF   DS    A                                                                
DAFILT   DS    A                   DISK ADDRESS FILTER                          
DSKAD    DS    A                   LAST PAGE'S LAST DISK ADDRESS                
AIO      DS    A                                                                
AIO1     DS    A                   A(IOAREA #1)                                 
AIO2     DS    A                   A(IOAREA #2)                                 
ASPLAREA DS    A                   A(SPOOL AREA)                                
AWRKRIOA DS    A                   A(IO AREA USED BY EDICT)                     
AWRKRBUF DS    A                   A(WORKER BUFFER AREA)                        
*                                                                               
VADDAY   DS    V                                                                
VDATCON  DS    V                                                                
VGETFACT DS    V                                                                
VHELLO   DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VLOCKET  DS    V                                                                
VRECUP   DS    V                                                                
VCLUNPK  DS    V                                                                
VMSPACK  DS    V                                                                
VMSUNPK  DS    V                                                                
VSWITCH  DS    V                                                                
VCLPACK  DS    V                                                                
VPARSNIP DS    V                                                                
VREQTWA  DS    V                                                                
*                                                                               
ASPOOL   DS    A                                                                
ASTAPACK DS    A                                                                
AREPFACS DS    A                                                                
*                                                                               
DMCB     DS    0XL(6*4)                                                         
         DS    6F                                                               
FULL     DS    F                                                                
SVORDDA  DS    A                   SAVED DISK ADDRESS OF ORDER                  
SVMKNDA  DS    A                   SAVED DISK ADDRESS OF MKGD NOTICE            
*                                                                               
DMINBTS  DS    X                                                                
RECLEN   DS    H                                                                
TERMNUM  DS    H                   TERMINAL NUMBER                              
USERNUM  DS    H                   USER ID NUMBER                               
TRNNUM   DS    H                   TRANSACTION COUNT                            
HALF     DS    H                                                                
DATADISP DS    H                                                                
MQMSGLEN DS    H                                                                
*                                                                               
AGENCY   DS    XL2                 AGENCY POWER CODE                            
SIGNON2H DS    XL2                 2 BYTE HEX AGENCY ID                         
USERID   DS    CL10                CONTROL USER ID                              
BAGYMD   DS    XL1                 BINARY AGENCY/MEDIA                          
BCLT     DS    XL2                 BINARY CLIENT CODE                           
BEST     DS    XL1                 BINARY ESTIMATE                              
BMKTSTA  DS    XL5                 BINARY MARKET STATION                        
BPRD     DS    XL1                                                              
SVAPRF07 DS    XL1                 SAVED COPY OF APROF+07                       
SVCPRF00 DS    XL1                 SAVED COPY OF CPROF+00                       
SVCOFFC  DS    XL1                 CLIENT OFFICE                                
         DS    0F                                                               
PACKOF4B DS    PL4                 PACKED NUMBER OF 4 BYTES                     
JDTTODAY DS    PL4                 JULIAN DATE OF TODAY                         
SVDATTIM DS    CL5                 SAVED DATE AND TIME OF MG CANCEL             
***************                                                                 
QTEST    DS    CL1                 TEST - DO NOT CREATE SOON JCL                
QRQSTID  DS    CL27                REQUEST ID PASSED FROM PC                    
***************                                                                 
* NET SYSTEM KEYWORDS                                                           
***************                                                                 
QNVALUES DS    0C                                                               
QNCLT    DS    CL8                 EBCDIC CLIENT                                
QNPRD    DS    CL8                        PRODUCT                               
QNEST    DS    CL9                        ESTIMATE                              
QNNTWK   DS    CL9                        NETWORK                               
QNDPT    DS    CL8                        DAYPART                               
QNPKG    DS    CL8                        PACKAGE                               
QNPDST   DS    CL8                        PERIOD START                          
QNPDND   DS    CL8                        PERIOD END                            
QNVALSL  EQU   *-QNVALUES                                                       
***************                                                                 
* PRINT SYSTEM KEYWORDS                                                         
***************                                                                 
QPVALUES DS    0C                                                               
QPCLT    DS    CL8                 EBCDIC CLIENT                                
QPPRD    DS    CL8                        PRODUCT                               
QPPUB    DS    CL9                        PUB                                   
QPPERIOD DS    CL17                       PERIOD                                
QPVALSL  EQU   *-QPVALUES                                                       
*                                                                               
SYSSENUM DS    XL1                 SPOT SYSTEM SENUM                            
BYTE     DS    C                                                                
*                                                                               
PIGPRD   DS    XL1                 PIGGYBACK PRODUCT BINARY CODE                
PIGEST   DS    XL1                 PIGGYBACK ESTIMATE                           
*                                                                               
BITFLAG1 DS    XL1                 VARIOUS BIT FLAGS                            
*                                                                               
BTODAY   DS    XL3                 TODAY'S DATE IN BINARY (YMD)                 
PRIORDAT DS    XL3                 YESTERDAY'S OR SOME PRIOR DAY'S DATE         
SYSNAME  DS    CL3                 3 CHR FACPAK NAME                            
SYSN1    DS    CL1                 1 CHR FACPAK NAME                            
WRKFILNO DS    XL2                 WORK FILE NUMBER                             
WRKRECNO DS    F                   WORKER FILE RECORD NUMBER                    
*                                                                               
WORK     DS    XL64                                                             
KEY      DS    XL64                                                             
KEYSAVE  DS    XL64                                                             
DKEY     DS    XL64                KEY OF LAST DA RECORD ADDED                  
HEADER   DS    XL32                                                             
DMWORK   DS    12D                                                              
*                                                                               
FILENAME DS    CL7                 DMGR FILENAME                                
LASTFILE DS    CL1                 PREVIOUS FILE NUMBER                         
DALINK   DS    CL4                 DISK ADDRESS OF LAST ADDREC                  
DDA      DS    CL4                 DISK ADDRESS OF GETREC                       
DALAST   DS    CL1                 LAST DA FILE ADDED TO                        
RACTN    DS    CL1                 RECORD TYPE (COPY CHNG ADD)                  
LASTACTN DS    CL1                 LAST TYPE (COPY CHNG ADD)                    
DLNFRID  DS    XL2                 DELIVERY NOTICE SENDER ID NUM                
ROUTNGCD DS    CL5                 ROUTING CODE                                 
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
ELCODE   DS    XL1                                                              
REVISION DS    XL1                 REVISION NUMBER                              
ORDAUDTR DS    X                   ORDER AUDIT TRAIL ACTION                     
REPORDTP DS    XL1                 REP ORDER TYPE                               
COPYQ    EQU   X'01'                                                            
CHANGEQ  EQU   X'02'                                                            
ADDQ     EQU   X'03'                                                            
NUMSPOTS DS    XL1                                                              
INPCNTR  DS    XL1                 INPUT FIELD COUNTER                          
*                                                                               
TRDEMTHD DS    CL1                 TRADE METHOD                                 
TRDEDATA DS    CL8                 DATA TO DETERMINE TRADE                      
*                                                                               
RECCOUNT DS    F                   RECORD COUNT                                 
*                                                                               
DEMSAVE  DS    XL6                 DEMO EXIT SAVE AREA FOR DEMPROS              
*                                                                               
SAVEBUFF DS    0XL12                                                            
SBVPQTAB DS    A                   A(PQTAB)                                     
SBVPQFIL DS    A                   A(PQFILE DTF)                                
SBSAVE   DS    A                   DISPLACEMENT TO START OF SAVE AREA           
*                                                                               
SAVESIN  DS    F                                                                
NEXTSIN  DS    F                                                                
QSIN     DS    CL6                                                              
PRTQID   DS    CL8                                                              
*                                                                               
REMUSER  DS    CL3                                                              
BIGSPLKY DS    CL144                                                            
*                                                                               
ACURELEM DS    F                   ADDRESS OF CURRENT ELEMENT                   
ELEM     DS    CL256                                                            
*                                                                               
BLOCK    DS    480C                                                             
*                                                                               
TMPSPOOK DS    XL(SPOOKXTX-SPOOK)  TEMPORARY SPOOK AREA                         
*                                                                               
IOA1     DS    4000C               I/O AREA                                     
IOA2     DS    4000C               I/O AREA                                     
*                                                                               
SPULAREA DS    XL3200                                                           
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
* DMWRKRD                                                                       
* DMWRKRK                                                                       
* FADSECTS                                                                      
* FAPQPL                                                                        
* CTGENFILE                                                                     
* CTGENDARE                                                                     
* CTGENRAD                                                                      
* DMGREQUS                                                                      
* DMPRTQD                                                                       
* DMPRTQS                                                                       
* DMPRTQK   <--- PREFIXED WITH 'SR'                                             
* DDCOMFACS                                                                     
* DMFILTABD                                                                     
* TASYSWORKD                                                                    
* DMREQHDRA                                                                     
* DDEDICTFIL                                                                    
* FAFACTS                                                                       
* SPTRPAT                                                                       
* SPTRSHIP                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE FADSECTS                                                       
         PRINT OFF                                                              
       ++INCLUDE FAPQPL                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENDARE                                                      
       ++INCLUDE CTGENRAD                                                       
       ++INCLUDE DMGREQUS                                                       
       ++INCLUDE DMPRTQD                                                        
       ++INCLUDE DMPRTQS                                                        
*PREFIX=SR                                                                      
       ++INCLUDE DMPRTQK                                                        
*PREFIX=                                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMFILTABD                                                      
       ++INCLUDE TASYSWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
RQHHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
       ++INCLUDE DMWRKFL                                                        
*********INCLUDE DMWRKFK                                                        
       ++INCLUDE DDEDICTFIL                                                     
       ++INCLUDE DDPARSNIPD                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPTRPAT                                                        
       ++INCLUDE SPTRSHIP                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPSYSFAC                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOK                                                        
         EJECT                                                                  
       ++INCLUDE SPWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
*PREFIX=NE$                                                                     
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE0D                                                       
         EJECT                                                                  
*PREFIX=PR$                                                                     
       ++INCLUDE PRWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PRWRIF1D                                                       
*PREFIX=                                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SRGAP02   02/15/13'                                      
         END                                                                    
