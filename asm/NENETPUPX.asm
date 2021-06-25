*          DATA SET NENETPUPX  AT LEVEL 017 AS OF 05/01/02                      
*          DATA SET NENETPUP   AT LEVEL 010 AS OF 05/28/99                      
*PHASE T00A55A,+0                                                               
*INCLUDE NETINTG                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE NETWEEK                                                                
         TITLE 'T00A55 - PUP READING MODULE FOR NETWORK'                        
NETPUP   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PUPWRKX-PUPWRK,**NTPP**,RR=R2                                    
         USING NETPUP,RB,R8                                                     
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         USING MYD,RC                                                           
         ST    R2,RELO                                                          
         MVC   USERRD,4(RD)                                                     
         MVC   APUPBLK,0(R1)                                                    
         L     R9,0(R1)                                                         
         USING NETPUPD,R9                                                       
         L     RA,NPANTBLK                                                      
         USING NETBLOCK,RA                                                      
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
*--CLEAR DEMO BLOCK                                                             
         TM    NPPUPARM,NPOVDEMO   DEMOS PASSED TO ROUTINE                      
         BO    MAINLOOP                                                         
*                                                                               
         L     R5,NBADEM           ADDRESS OF DEMO BLOCK                        
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING NDDEMBLK,R5                                                      
         XC    NDDEMOS,NDDEMOS                                                  
         XC    NDESTDEM,NDESTDEM                                                
         XC    NDESTWUN,NDESTWUN                                                
         XC    NDWGTPOS(3),NDWGTPOS                                             
         XC    NDWGTLST,NDWGTLST                                                
         DROP  R5                                                               
         EJECT                                                                  
*--MAIN LOOP                                                                    
MAINLOOP BAS   RE,RDPLAN                                                        
         BNZ   MAINLP80                                                         
         BAS   RE,EXTPLAN                                                       
*                                                                               
MAINLP20 BAS   RE,RDPROG                                                        
         BNZ   MAINLP80                                                         
MAINLP30 BAS   RE,EXTPROG                                                       
         OC    NPUPUNIT,NPUPUNIT   DONT OUTPUT ZERO UNITS                       
         BZ    MAINLP40                                                         
*                                                                               
         TM    NPPUPARM,NPEXBOPT                                                
         BZ    *+8                                                              
         BAS   RE,EXTBLOCK         MOVE INFO INTO NETBLOCK                      
         BAS   RE,GOHOOK                                                        
         XC    NBINTEG,NBINTEG                                                  
*                                                                               
         BAS   RE,EXTDEMO                                                       
         GOTO1 =A(OVFLRTN),DMCB,(4,DUB),(RC),RR=RELO CLRDEMOS                   
*                                                                               
         BAS   RE,EXTBUDG                                                       
         GOTO1 =A(OVFLRTN),DMCB,(4,DUB),(RC),RR=RELO CLRDEMOS                   
*--SEE IF MORE LENGTHS IN THIS PROGRAM RECORD                                   
MAINLP40 OC    NPSELLEN,NPSELLEN   WAS SECIFIC LENGTH REQUSTED                  
         BNZ   MAINLP20            YES READ NEXT PROGRAM RECORD                 
         ZIC   RE,NPUPLNCT                                                      
         LA    RE,1(RE)                                                         
         STC   RE,NPUPLNCT                                                      
         CLC   NPUPLNCT,NPUPNLEN   ANY MORE LENGTHS ON THIS PROGRAM             
         BNH   MAINLP30            YES GET LENGTHS INFO                         
         MVI   NPUPLNCT,1          SET TO FIRST LENGTH                          
         B     MAINLP20            READ NEXT PROGRAM                            
*                                                                               
MAINLP80 B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
RDPLAN   NTR1                                                                   
         OC    NPPLANKY,NPPLANKY   WAS PLAN ALREADY READ                        
         BZ    RDPL10              NO                                           
         XC    KEY,KEY                                                          
         MVC   KEY(15),NPPLANKY                                                 
         GOTO1 READ                                                             
         B     RDPL40                                                           
* RE-READ PLAN RECORD                                                           
RDPL10   LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPLRECD,R4                                                       
         MVI   NPLKTYPE,X'20'      FILL PLAN KEY                                
         MVC   NPLKAM,NBACTAM                                                   
         MVC   NPLKCLT,NPSELCLI                                                 
         MVC   NPLKNET,NPSELNET                                                 
         MVC   NPLKDPT,NPSELDP                                                  
         MVC   NPLKPLAN,NPSELPLN                                                
RDPL20   GOTO1 HIGH                                                             
         B     RDPL60                                                           
         SPACE 1                                                                
RDPL40   GOTO1 SEQ                                                              
         SPACE 1                                                                
RDPL60   CLC   KEY(2),KEYSAVE      AGY                                          
         BNE   RDPLX                                                            
         LA    R4,KEY                                                           
         CLI   NPSELCLI,0                                                       
         BE    *+14                                                             
         CLC   NPSELCLI,KEY+2                                                   
         BNE   RDPL40                                                           
         CLI   NPSELNET,0                                                       
         BE    *+14                                                             
         CLC   NPSELNET,KEY+5                                                   
         BNE   RDPL40                                                           
         CLI   NPSELDP,0                                                        
         BE    *+14                                                             
         CLC   NPSELDP,KEY+10                                                   
         BNE   RDPL40                                                           
         OC    NPSELPLN,NPSELPLN                                                
         BZ    *+14                                                             
         CLC   NPSELPLN,KEY+11                                                  
         BNE   RDPL40                                                           
*                                                                               
RDPL80   L     R4,NPPLANAD                                                      
         GOTO1 GETPLAN                                                          
         BAS   RE,GETSTAT          READ STATION REC FOR MEDIA TYPE              
*                                                                               
RDPL100  BAS   RE,CHKFILT                                                       
         BNE   RDPL40                                                           
         MVC   NPPLANKY,KEY                                                     
         SR    RE,RE                                                            
*                                                                               
RDPLX    LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
CHKFILT  NTR1                                                                   
         L     R4,NPPLANAD                                                      
         USING NPLRECD,R4                                                       
*-LENGTH FILTER                                                                 
         OC    NPSELLEN,NPSELLEN                                                
         BZ    CHK80                                                            
         XC    NPUPALEN,NPUPALEN                                                
         LA    R0,4                                                             
         LA    R3,NPLNLENS         LIST OF LENGTHS ON PLAN                      
         LA    R5,NPUPALEN                                                      
         LA    RE,1                                                             
*                                                                               
CHK60    CLC   NPSELLEN,0(R3)                                                   
         BE    CHK70                                                            
         LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CHK60                                                         
         B     CHKNO                                                            
*                                                                               
CHK70    MVC   0(1,R5),NPSELLEN    SET LENGTH IN TABLE                          
         STC   RE,NPUPALEN         SET ALL LENGTHS                              
         STC   RE,NPUPLNCT         SET CURRENT LENGTH                           
*                                                                               
CHK80    OC    NPSELPFT,NPSELPFT     ARE WE FILTERING ON PLAN                   
         BZ    CHK200                NO / CHECK MEDIA FILTER                    
         LA    R3,NPSELPFT                                                      
         LA    R5,NPLNFILT                                                      
         LA    R0,3                                                             
         SPACE 1                                                                
CHK100   CLI   0(R3),C'*'          WILD                                         
         BE    CHK180                                                           
         CLI   0(R3),0                                                          
         BE    CHK180                                                           
         TM    0(R3),X'40'                                                      
         BZ    CHK140                                                           
         CLC   0(1,R3),0(R5)       FILTER                                       
         BNE   CHKNO                                                            
         B     CHK180                                                           
         SPACE 1                                                                
CHK140   MVC   BYTE,0(R5)                                                       
         NI    BYTE,X'FF'-X'40'     TURN OFF X'40' BIT                          
         CLC   0(1,R3),BYTE         NEGATIVE FILTER                             
         BE    CHKNO                                                            
         SPACE 1                                                                
CHK180   LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,CHK100                                                        
*                                                                               
CHK200   OC    NPSELMED,NPSELMED     ARE WE FILTERING MEDIA                     
         BZ    CHKYES                NO / SO EXIT WITH YES                      
         CLC   NPSELMED,NPUPMEDT                                                
         BNE   CHKNO                                                            
         B     CHKYES                                                           
*                                                                               
CHKYES   SR    R1,R1               PASSED FILTERS                               
         B     *+8                                                              
CHKNO    LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* READ STATION RECORD                                                           
*                                                                               
GETSTAT  NTR1                                                                   
         MVC   WORK(30),KEY        SAVE CURRENT KEY                             
         XC    KEY,KEY                                                          
*                                                                               
         MVC   KEY(17),=C'SNABC NAA00000000'                                    
         USING STAREC,R6                                                        
         LA    R6,KEY                                                           
         MVC   STAKCALL(4),WORK+5                                               
         MVC   STAKAGY,NPSELAGY                                                 
         GOTO1 STREAD              GET STATION RECORD                           
*        PACK  DUB,SMKT            CONVERT 'MARKET' NUMBER                      
*        CVB   R1,DUB                                                           
*        STH   R1,DUB                                                           
*        MVC   NPUPMKTN,DUB                                                     
         MVC   NPUPMEDT,STYPE                                                   
         MVC   NPUPPSTT,SPTYPE                                                  
         MVC   NPUPNTIS,SNTISTA                                                 
         XC    FILE,FILE                                                        
         MVC   KEY(30),WORK        RESET KEY                                    
         GOTO1 HIGH                                                             
         GOTO1 GETPLAN             REREAD PLAN RECORD                           
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*    READ PROGRAM RECORDS                                                       
RDPROG   NTR1                                                                   
RDPR10   OC    NPPROG12,NPPROG12   MIDDLE OF RECORD                             
         BNZ   RDPR80              YES GET NEXT ELEMENT                         
*                                                                               
         OC    NPPROGKY,NPPROGKY   WAS PLAN ALREADY READ                        
         BZ    RDPR20              NO                                           
         XC    KEY,KEY                                                          
         MVC   KEY(15),NPPROGKY                                                 
         GOTO1 READ                                                             
         B     RDPR30                                                           
*                                                                               
RDPR20   LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPURECD,R5          FILL PROGRAM KEY                             
         LA    R4,NPPLANKY         FROM PLANKEYS                                
         USING NPLRECD,R4                                                       
         MVI   NPUKTYPE,X'22'                                                   
         MVC   NPUKAM,NPLKAM                                                    
         MVC   NPUKCLT,NPLKCLT                                                  
         MVC   NPUKNET,NPLKNET                                                  
         MVC   NPUKDPT,NPLKDPT                                                  
         MVC   NPUKPLAN,NPLKPLAN                                                
         GOTO1 HIGH                                                             
         B     RDPR40                                                           
         SPACE 1                                                                
RDPR30   GOTO1 SEQ                                                              
         SPACE 1                                                                
RDPR40   CLC   KEY(13),KEYSAVE     MUST MATCH ON PLAN                           
         BNE   RDPR140                                                          
RDPR60   GOTO1 GETPROG                                                          
         OI    NPSTATUS,NPNPROG    SET NEW RECORD SWITCH                        
*                                                                               
*--CHECK FOR PROGRAM FILTER                                                     
         L     R5,NPPROGAD                                                      
*-CHEKFILT                                                                      
         GOTO1 =A(OVFLRTN),DMCB,(5,DUB),(RC),NPSELUFT,NPGDFILT,RR=RELO          
         CLI   4(R1),X'FF'                                                      
         BE    RDPR30                                                           
*                                                                               
*--CHECK FOR PROGRAM ELEMENTS                                                   
         L     R6,NPPROGAD                                                      
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BE    RDPR120                                                          
         B     RDPR30                                                           
*                                                                               
RDPR80   L     R6,NPPROG12         POINTER TO LAST X'12'ELEMENT                 
         MVI   ELCODE,X'12'                                                     
         BAS   RE,NEXTEL                                                        
         BE    RDPR120                                                          
         XC    NPPROG12,NPPROG12   ELEMENT NOT FOUND                            
         B     RDPR30                                                           
RDPR120  STCM  R6,15,NPPROG12      SAVE ELEMENT ADDRESS                         
*        GOTO1 =V(PRNTBL),DMCB,=C'PLAN2',KEY,C'DUMP',100,=C'1D'                 
         B     RDPRGD                                                           
         SPACE 1                                                                
RDPR140  XC    NPPROGKY,NPPROGKY                                                
         XC    NPPROG12,NPPROG12                                                
         BAS   RE,RDPLAN           READ NEXT PLAN RECORD                        
         BNZ   RDPRX               END OF PLANS                                 
         BAS   RE,EXTPLAN          EXTRACT PLAN RECORD                          
         B     RDPR10                                                           
         SPACE 1                                                                
RDPRGD   SR    RE,RE                                                            
RDPRX    LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
*              PLAN RECORD EXPANSION                                            
         SPACE 3                                                                
EXTPLAN  NTR1                                                                   
         L     R4,NPPLANAD         FROM PLANKEYS                                
         USING NPLRECD,R4                                                       
         MVI   NPUPNWPL,X'FF'      PLAN SWITCH                                  
         GOTO1 NBCLUNPK,DMCB,NPLKCLT,NPUPCL3    CLIENT                          
         MVC   NPUPNET,NPLKNET     NETWORK                                      
         MVC   NPUPDPT,NPLKDPT     DAYPART                                      
         MVC   NPUPPLAN,NPLKPLAN   PLAN                                         
         MVC   NPUPLNNM,NPLNNAME   PLAN NAME                                    
         MVC   NPUPLNYR,NPLNYEAR   PLAN YEAR                                    
         MVC   NPUPLFLT,NPLNFILT   PLAN FILTER                                  
         OC    NPUPLFLT,=XL3'404040'  BLANK UNUSED COLUMNS                      
         MVC   NPUPPTYP,NPLNPERT   PERIOD TYPE                                  
         MVC   NPUPGCPM,NPLNGCPM   GUARANTEED CPM FOR TARGET                    
         MVC   NPUPPKGA,NPLNADJP   PACKAGE ADJUSTMENT                           
         OC    NPSELLEN,NPSELLEN   SET IF LENGTH FILTER SET                     
         BNZ   *+20                YES DONT LOAD DEFAULTS                       
         MVC   NPUPALEN,NPLNLENS   LENGTH VALUES                                
         MVC   NPUPNLEN,NPLNNLEN   NUMBER OF LENGTHS                            
         MVI   NPUPLNCT,1          SET FOR FIRST LENGTH                         
         TM    NPPUPARM,NPHPLAN    PLAN IN HEAD CHANGE DEMOS                    
         BO    *+14                                                             
         OC    NPUPDEMS,NPUPDEMS   DEMOS                                        
         BNZ   *+16                                                             
         MVC   NPUPDEMS,NPLNDEMS                                                
         MVC   NPUPDMCT,NPLNNDEM                                                
*                                                                               
         XC    NPUPPINT,NPUPPINT                                                
         L     R6,NPPLANAD                                                      
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   NPUPPINT,2(R6)      PLAN INTEGRATION                             
*                                                                               
         XC    NPUPDEMA,NPUPDEMA                                                
         L     R6,NPPLANAD                                                      
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   NPUPDEMA,2(R6)      DEMO ADJUSTMENT                              
*                                                                               
         XC    NPUPTVQB,NPUPTVQB                                                
         L     R6,NPPLANAD                                                      
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   NPUPTVQB,6(R6)      TVQ BOOK                                     
*                                                                               
         XC    BUDGTAB,BUDGTAB                                                  
         L     R6,NPPLANAD                                                      
         LA    R5,BUDGTAB                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         B     EXTPL120                                                         
EXTPL100 BAS   RE,NEXTEL                                                        
EXTPL120 BNE   EXTPL140                                                         
         MVC   0(2,R5),2(R6)       QUARTER                                      
         MVC   4(1,R5),4(R6)       SECONDS LENGTH                               
         MVC   5(4,R5),8(R6)       BUDGET                                       
         LA    R5,9(R5)                                                         
         B     EXTPL100                                                         
*                                                                               
EXTPL140 GOTO1 =A(OVFLRTN),DMCB,(3,DUB),(RC),RR=RELO SETBDATE                   
*                                                                               
         L     R6,NPPLANAD         FORMAT UNIVERSE ELEMENT                      
         MVI   ELCODE,X'02'                                                     
         MVC   PUEL(3),=X'31F344'                                               
         XC    UNIVS,UNIVS                                                      
         BAS   RE,GETEL                                                         
         BNE   EXTPL180                                                         
         USING NPUELD,R6                                                        
         ZIC   R1,NPULEN                                                        
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   UNIVS(0),NPUNIVS                                                 
         CLI   NPUPPSTT,C'S'       SYNDICATION                                  
         BE    EXTPL180                                                         
         CLI   NPUPPSTT,C'N'       OR MAIN NETWORK                              
         BE    EXTPL180                                                         
         LA    R2,UNIVS                                                         
         LA    R3,60                                                            
         SPACE 1                                                                
EXTPL160 L     R1,0(R2)            ADJUST UNIVS FOR NON NETWORK                 
         M     R0,=F'10'                                                        
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R3,EXTPL160                                                      
         B     EXTPL180                                                         
*                                                                               
*-SET UP DEMO BLOCK                                                             
EXTPL180 L     R5,NBADEM           ADDRESS OF DEMO BLOCK                        
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING NDDEMBLK,R5                                                      
*                                                                               
         TM    NPPUPARM,NPOVDEMO   DEMOS PASSED TO ROUTINE                      
         BO    EXTPL200                                                         
*                                                                               
         TM    NPPUPARM,NPHPLAN    PLAN IN HEAD CHANGE DEMOS                    
         BO    *+14                                                             
         OC    NDDEMOS,NDDEMOS     DO ONLY FOR FIRST PLAN                       
         BNZ   EXTPLEX                                                          
         MVC   NDDEMOS(18),NPUPDEMS                                             
         MVC   NDNDEMOS,NPUPDMCT                                                
*                                                                               
EXTPL200 GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(RC),RR=RELO BLDDEMIN                   
*        GOTO1 =V(PRNTBL),DMCB,=C'PLAN',NPUPPLAN,C'DUMP',4,=C'1D'               
*        GOTO1 =V(PRNTBL),DMCB,=C'DEMO',NDDEMOS,C'DUMP',60,=C'1D'               
EXTPLEX  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              PROGRAM RECORD EXPANSION                                         
         SPACE 3                                                                
EXTPROG  NTR1                                                                   
         L     R5,NPPROGAD         FROM PLANKEYS                                
         USING NPURECD,R5                                                       
         MVC   NPUPPROG,NPUKPROG   PROGRAM CODE                                 
         MVC   NPUPRGNM,NPGDNAME   PROGRAM NAME                                 
         MVC   NPUPDAY,NPGDDAY     DAY                                          
         MVC   NPUPNTI,NPGDNTI     NTI                                          
         MVC   NPUPTIME,NPGDTIME   PLAN NAME                                    
         MVC   NPUPPRFL,NPGDFILT   PROGRAM FILTER                               
*-GET CURRENT LENGTH                                                            
         LA    R6,NPUPALEN         ALL LENGTHS                                  
         ZIC   RE,NPUPLNCT         LENGTH POSITION                              
         BCTR  RE,0                                                             
         AR    R6,RE                                                            
         MVC   NPUPLEN,0(R6)                                                    
*                                                                               
         XC    NPUPUNIT,NPUPUNIT                                                
         L     R6,NPPROG12         ADDERSS OF CURRENT ELEMENT                   
         USING NPUBD,6                                                          
*                                                                               
         MVC   NPUPPERD,NPUBPER    PERIOD                                       
         GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(RC),RR=RELO GETPDATE                   
*        GOTO1 =V(PRNTBL),DMCB,=C'ACTD',NBACTDAT,C'DUMP',2,=C'1D'               
*        GOTO1 =V(PRNTBL),DMCB,=C'COMD',NPSELSDT,C'DUMP',4,=C'1D'               
         CLC   NBACTDAT,NPSELSDT                                                
         BL    XIT                                                              
         CLC   NBACTDAT,NPSELSDT+2                                              
         BH    XIT                                                              
*                                                                               
         ZIC   RE,NPUPLNCT         LENGTH OFFSET                                
         BCTR  RE,0                                                             
         LA    RF,NPUBUNS                                                       
         AR    RF,RE                                                            
         MVC   NPUPUNIT+1(1),0(RF) NUMBER OF UNITS FOR THIS LENGTH              
         MVC   NPUPSHR(6),NPUBSHR  SHARE/HUT/RATING                             
*        GOTO1 =V(PRNTBL),DMCB,=C'DATE',NPUBPER,C'DUMP',2,=C'1D'                
*        GOTO1 =V(PRNTBL),DMCB,=C'UNIT',NPUPUNIT,C'DUMP',2,=C'1D'               
*        GOTO1 =V(PRNTBL),DMCB,=C'LENC',NPUPLNCT,C'DUMP',1,=C'1D'               
         BAS   RE,EQUIVU                                                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
EXTDEMO  NTR1                                                                   
         TM    NPSTATUS,NPNPROG                                                 
         BZ    *+8                 INFO FROM X'02' ALREADY GENERATED            
         BAS   RE,EXTDEM02                                                      
         BAS   RE,EXTDEM12                                                      
         BAS   RE,GETDEM                                                        
         BAS   RE,PASSDEM                                                       
         NI    NPSTATUS,X'7F'      RE-SET NEW RECORD SWITCH                     
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO EXTRACT VPHS FROM THE X'02' ELEMENT                   
         SPACE 3                                                                
EXTDEM02 NTR1                                                                   
         XC    PBEL,PBEL                                                        
         L     R6,NPPROGAD                                                      
         MVI   ELCODE,X'5D'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PBEL,0(R6)          SET UP BOOK ELEMENT                          
*                                                                               
         L     R6,NPPROGAD                                                      
         XC    PVEL,PVEL                                                        
         MVC   PVEL(3),=X'33F342'                                               
         XC    PREL,PREL                                                        
         XC    OVERAREA,OVERAREA                                                
         MVC   PREL(3),=X'350902'                                               
         MVI   ELCODE,X'12'                                                     
         USING NPUAD,R6                                                         
         BAS   RE,GETEL                                                         
         B     E02DM4                                                           
E02DM2   BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
E02DM4   BNE   XIT                                                              
         SPACE 1                                                                
         MVC   RATING,NPUARTG                                                   
         MVC   HUT,NPUAHUT                                                      
         MVC   SHARE,NPUASHR                                                    
         XC    VPHS,VPHS                                                        
         MVC   VPHS(48),NPUAVPHS                                                
         CLI   NPUALEN,72                                                       
         BE    E02DM6                                                           
         MVC   VPHS(64),NPUAVPHS                                                
         CLI   NPUALEN,88                                                       
         BE    E02DM6                                                           
         MVC   VPHS,NPUAVPHS                                                    
         SPACE 1                                                                
E02DM6   MVC   WORK,VPHS           CHANGE VPH SCALE                             
         LA    R2,WORK                                                          
         LA    R3,VPHS                                                          
         LA    R0,64                                                            
         SPACE 1                                                                
E02DM8   ZIC   R1,0(R2)                                                         
         MH    R1,=H'10'                                                        
         STH   R1,0(R3)                                                         
         LA    R2,1(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,E02DM8                                                        
         SPACE 3                                                                
         TM    NPUAOVRD,X'10'                                                   
         BNO   XIT                                                              
         MVC   OVEREL(2),=X'DD06'                                               
         MVI   OVERDEMO,C'V'                                                    
         MVC   OVERDEMO+1(1),NPUPDEMS+2  TARGET DEMO                            
         MVC   OVERAMNT,NPUAVOVR                                                
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO EXTRACT FROM NEW ELEMENTS                             
         SPACE 3                                                                
EXTDEM12 NTR1                                                                   
         USING NPUBD,R6                                                         
         L     R6,NPPROG12         X'12' ELEMENT                                
         MVC   RATING,NPUBRTG                                                   
         MVC   HUT,NPUBHUT                                                      
         MVC   SHARE,NPUBSHR                                                    
         OC    NPUBBOOK,NPUBBOOK   IF A BOOK IS IN ELEMENT                      
         BZ    *+10                                                             
         MVC   PBEL+5(2),NPUBBOOK  USE IT                                       
*                                                                               
         XC    OVERAREA,OVERAREA                                                
         LA    R2,NPUPDEMS         LOOK FOR ANY VPH OVERRIDES                   
         LA    R3,NPUBVOVR                                                      
         LA    R1,OVERAREA                                                      
         LA    R0,6                (MAX 6)                                      
         SPACE 1                                                                
E12DM2   OC    0(2,R3),0(R3)                                                    
         BZ    E12DM4                                                           
         MVC   0(2,R1),=X'DD06'    YES - SO BUILD AN OVERRIDE ELEMENT           
         MVI   2(R1),C'V'                VPH FOR                                
         MVC   3(1,R1),2(R2)             DEMO NUMBER                            
         MVC   4(2,R1),0(R3)             AND AMOUNT                             
         LA    R1,6(R1)                                                         
         SPACE 1                                                                
E12DM4   LA    R2,3(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,E12DM2                                                        
         SPACE 1                                                                
E12DM6   XC    VPHS,VPHS                                                        
         MVC   WORK(1),NPUBLNK     LINK NUMBER TO VPH ELEMENTS                  
         MVI   ELCODE,X'14'                                                     
         SPACE 1                                                                
E12DM10  BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING NPUCD,R6                                                         
         CLC   NPUCLNK,WORK        FIND MATCHING VPH ELEMENT                    
         BNE   E12DM10                                                          
         ZIC   R1,NPUCLEN                                                       
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VPHS(0),NPUCVPHS                                                 
         B     XIT                                                              
         EJECT                                                                  
*              GET DEMOS VALUES FOR PROG/PERIOD(/LENGTH)                        
         SPACE 3                                                                
*              INPUTS              GDDEMO SET TO REQUIRED DEMO NUMBER           
*              OUTPUTS             FILLS IN GD FIELDS                           
         SPACE 1                                                                
GETDEM   NTR1                                                                   
         BAS   RE,SETDB                                                         
         L     R5,NBADEM           ADDRESS OF DEMO BLOCK                        
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING NDDEMBLK,R5                                                      
         L     R6,NDARAWEX         ADDRESS OF RAW DEMOS                         
         USING RAWDATA,R6                                                       
         PRINT GEN                                                              
         GOTO1 NBDEMOUT,DMCB,(C'L',DIALIST),DBLOCK,DOALIST                      
         PRINT NOGEN                                                            
*                                                                               
         GOTO1 =A(OVFLRTN),DMCB,(1,DUB),(RC),RR=RELO MOVESTDM                   
*        GOTO1 =V(PRNTBL),DMCB,=C'DEMOS',NDESTDEM,C'DUMP',8,=C'1D'              
*                                                                               
         BAS   RE,COMPDEM          CONVERT GDIMP TO DEMO IMPRESSIONS            
*                                                                               
         BAS   RE,ANYADJ           CHECK FOR PACKAGE OR DEMO ADJUST             
         BAS   RE,ANYADJH          CHECK FOR PACK OR DEMO ADJUST HOMES          
*--SET UP RAW HOMES                                                             
         MVC   RWESTHOM(2),NBESTHOM                                             
         MVC   RWESTHOM+2(2),NBESTHOM+2                                         
         MVC   RWESTHOM+4(4),NBESTHOM+4                                         
*                                                                               
         LA    R2,NBESTHOM+4                                                    
         BAS   RE,EQUIVI                                                        
         TM    NBINDS,X'40'                                                     
         BNZ   *+8                                                              
         BAS   RE,FRRND                                                         
*                                                                               
         LA    R2,FULL                                                          
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),NBESTHOM+2                                             
         BAS   RE,EQUIVR                                                        
         MVC   NBESTHOM+2(2),FULL+2                                             
*                                                                               
         SPACE 1                                                                
         BAS   RE,GETDEMLN         GET DEMOS FOR THAT LENGTH                    
         SPACE 1                                                                
         BAS   RE,BKRND            SECOND IMPRESSION ROUND                      
         BAS   RE,BKRNDH           SECOND HOMES IMPRESSION ROUND                
         B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
*              ADD TOTAL AND EQUIV DEMOS FOR LENGTH                             
         SPACE 3                                                                
GETDEMLN NTR1                                                                   
         L     R3,NBADEM                                                        
         USING NDDEMBLK,R3                                                      
         L     R4,NDARAWEX                                                      
         USING RAWDATA,R4                                                       
*--RAW CATEGORIES                                                               
         MVC   RWESTDEM,NDESTDEM                                                
         LA    R2,RWESTDEM+4                                                    
         ZIC   R5,NDNDEMOS                                                      
*--ROUND THE RAW IMPRESSIONS                                                    
GETDML20 BAS   RE,FRRND                                                         
         LA    R2,8(R2)                                                         
         BCT   R5,GETDML20                                                      
*--SET UP EQUIVALENCED AND ROUND THE IMPRESSIONS                                
         LA    R2,FULL                                                          
         ZIC   R5,NDNDEMOS         NUMBER OF DEMO SETS                          
         LA    R6,NDESTDEM+2       RAW DEMOS                                    
*                                                                               
GETDML50 XC    FULL,FULL                                                        
         MVC   FULL+2(2),0(R6)     DO THE RATINGS                               
         BAS   RE,EQUIVR           ROUND EQUIV RATINGS                          
         MVC   0(2,R6),FULL+2                                                   
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(4),2(R6)       DO THE IMPRESSION                            
         BAS   RE,EQUIVI                                                        
         TM    NBINDS,X'40'                                                     
         BNZ   *+8                                                              
         BAS   RE,FRRND                                                         
         MVC   2(4,R6),FULL                                                     
*                                                                               
         LA    R6,8(R6)            BUMP TO NEXT DEMO SET                        
         BCT   R5,GETDML50                                                      
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR GETDEM                                   
         SPACE 3                                                                
COMPDEM  NTR1                                                                   
         L     R5,NBADEM                                                        
         USING NDDEMBLK,R5                                                      
         LA    R3,NDESTDEM         DEMO VALUES                                  
         LA    R6,NDDEMOS          DEMO CODES                                   
         ZIC   RF,NDNDEMOS                                                      
*                                                                               
CPDM20   CLI   2(R6),1             DONT COMPUTE HOMES                           
         BE    CPDM40                                                           
*                                                                               
CPDM30   SR    R0,R0                                                            
         ICM   R0,3,0(R3)          LOAD VPH'S                                   
         ICM   R1,15,NBESTHOM+4    LOAD HOME IMPS                               
         LA    R2,4(R3)                                                         
         BAS   RE,FRRND            ROUND THE HOMES (CONVERTS TO 000)            
         MR    R0,R0                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         STCM  R1,15,4(R3)         RETURN DEMO IMPRESSIONS (IN 00)              
CPDM40   LA    R3,8(R3)                                                         
         LA    R6,3(R6)                                                         
         BCT   RF,CPDM20                                                        
         B     XIT                                                              
         DROP  R5                                                               
         SPACE 1                                                                
EQUIVR   NTR1                                                                   
*        ZIC   RF,NBN2B1           EQUIVALENCE RATING                           
         LA    RF,30               ALWAYS BASE 30                               
         B     EQUIVALL                                                         
         SPACE 1                                                                
EQUIVI   NTR1                                                                   
*        ZIC   RF,NBN0B2           EQUIVALENCE IMPS                             
         LA    RF,30               ALWAYS BASE 30                               
         B     EQUIVALL                                                         
         SPACE 1                                                                
EQUIVALL LTR   RF,RF               EQUIVALENCE RATINGS                          
         BZ    XIT                 (IF NECESSARY)                               
         L     R1,0(R2)            R2=A(IMPS OR RATINGS)                        
         ZIC   R0,NPUPLEN          CURRENT LENGTH                               
         MR    R0,R0                                                            
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R2)                                                         
         B     XIT                                                              
         SPACE 1                                                                
*--CALCULATE EQUIVALENCED UNITS                                                 
EQUIVU   NTR1                                                                   
         OC    NPUPUNIT,NPUPUNIT                                                
         BZ    XIT                                                              
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,NPUPUNIT       NUMBER OF UNITS                              
         M     R0,=F'10'                                                        
         STCM  R1,3,NPUPUNIT                                                    
         ZIC   R0,NPUPLEN          CURRENT LENGTH                               
         MR    R0,R0                                                            
         SLDA  R0,1                                                             
         D     R0,=F'30'                                                        
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STCM  R1,3,NPUPEUNT                                                    
         B     XIT                                                              
         SPACE 1                                                                
*--FRONT END ROUND                                                              
FRRND    NTR1                                                                   
         L     R1,0(R2)            R2=A(IMPRESSIONS TO BE ROUNDED)              
         CLI   NPUPPSTT,C'S'       SYNDICATION                                  
         BE    *+8                                                              
         CLI   NPUPPSTT,C'N'       OR MAIN NETWORK                              
         BNE   FRRND2                                                           
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'10'          (RETURN MAIN IN 000 TO NEAREST                
         ST    R1,0(R2)                                   10000)                
         B     XIT                                                              
         SPACE 1                                                                
FRRND2   AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         ST    R1,0(R2)                                                         
         B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*--BACK END ROUND                                                               
BKRND    NTR1                                                                   
         L     R3,NBADEM                                                        
         USING NDDEMBLK,R3                                                      
         LA    R4,NDESTDEM+2                                                    
         ZIC   R5,NDNDEMOS                                                      
         B     BKRND2                                                           
*                                                                               
BKRNDH   NTR1                                                                   
         L     R3,NBADEM                                                        
         USING NDDEMBLK,R3                                                      
         LA    R4,NBESTHOM+2                                                    
         LA    R5,1                                                             
*-ADJUST CABLE/OTHER IMPRESSIONS                                                
BKRND2   CLI   NPUPPSTT,C'C'       CABLE                                        
         BE    *+12                                                             
         CLI   NPUPPSTT,C'O'       OTHER                                        
         BNE   BKRND5                                                           
         L     R1,2(R4)                                                         
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         ST    R1,2(R4)                                                         
BKRND3   LA    R4,8(R4)                                                         
         BCT   R5,BKRND2                                                        
         B     BKRNDEX                                                          
*-ADJUST NETWORK/SYNDICATION RATING IF CABLE PRECISION IS REQUESTED             
BKRND5   CLI   NBPREOPT,C'Y'                                                    
         BNE   BKRND3                                                           
         SR    R1,R1                                                            
         ICM   R1,3,0(R4)                                                       
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         STCM  R1,3,0(R4)                                                       
         B     BKRND3                                                           
*                                                                               
BKRNDEX  B     XIT                                                              
         EJECT                                                                  
*              PASS DEMO VALUES TO DRIVER                                       
         SPACE 3                                                                
PASSDEM  NTR1                                                                   
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R5,3,NBSPCHRG       MOVE UNIT COUNT TO R5                        
*--CLEAR COLUMN FIELDS                                                          
         XC    NBSPCHRG,NBSPCHRG   CLEAR UNIT COUNT                             
         L     R4,NBADEM           ADDRESS OF DEMO BLOCK                        
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING NDDEMBLK,R4                                                      
         TM    NBINDS,X'40'                                                     
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
**************************************                                          
         L     R1,NDARAWEX                                                      
         USING RAWDATA,R1                                                       
         MVC   RWACTHOM,RWESTHOM   FOR NEW DEMO EXTENSION DSECT                 
         MVC   RWACTDEM,RWESTDEM   EST=RAW=EQU IF NO FILTERING                  
*                                                                               
         MVC   RQESTHOM,NBESTHOM                                                
         MVC   RQESTDEM,NDESTDEM                                                
         MVC   RQACTHOM,RQESTHOM                                                
         MVC   RQACTDEM,RQESTDEM                                                
*                                                                               
***      GET ADDRESSABILITY LATER                                               
***      TM    NDANYDEM,NN+NQ      SKIP DEM/PKG GUAR ?                          
***      BZ    PASSD40             NO                                           
         LA    RE,RAWDATA          YES                                          
         LA    RF,NNDATA                                                        
         LA    R1,NNDATA-RAWDATA   LENGTH OF ACT + EST DEMO AREA                
         MOVE  ((RF),(R1)),(RE)    MOVE TO NO PKG/DEM GUAR AREA                 
         DROP  R1                                                               
PASSD40  EQU   *                                                                
***************************************                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'DATE',NBACTDAT,C'DUMP',2,=C'1D'               
*        GOTO1 =V(PRNTBL),DMCB,=C'BLOCK',NDESTDEM,C'DUMP',160,=C'1D'            
PASSD50  BAS   RE,GOHOOK                                                        
         BCT   R5,PASSD50                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
IMPDIV10 NTR1                           GREATER PRECISION IS 7 DIGITS           
*                                       NORMAL COMES IN AS 5 DIGITS             
*                                       DROP LAST 2 ZEROS HERE                  
*                                       DO FINAL ROUND IN NEWRIDRIVE            
*                                                                               
         L     R4,NBADEM           ADDRESS OF DEMO BLOCK                        
         USING NDDEMBLK,R4                                                      
         L     R5,NDARAWEX                                                      
         USING RAWDATA,R5                                                       
         CLI   NBPOSTYP,C'C'       IF CABLE                                     
         BE    XIT                 LEAVE AS IS                                  
         LA    R2,NBESTHOM+4       DO HOMES FIRST                               
         BAS   RE,DIV100                                                        
         LA    R2,NDESTDEM        ESTIMATE IMPRESSIONS                          
         LA    R2,4(R2)           POINT TO IMPS                                 
         LA    R3,20                                                            
IMPD10   BAS   RE,DIV100                                                        
         LA    R2,8(R2)                                                         
         BCT   R3,IMPD10                                                        
*                                                                               
*--RWESTHOM AND RWESTDEM ARE ROUNDED HERE FOR PUP                               
*                                                                               
         LA    R2,RWESTHOM+4       DO HOMES FIRST                               
         BAS   RE,DIV100                                                        
         LA    R2,RWESTDEM        ESTIMATE IMPRESSIONS                          
         LA    R2,4(R2)           POINT TO IMPS                                 
         LA    R3,20                                                            
IMPD40   BAS   RE,DIV100                                                        
         LA    R2,8(R2)                                                         
         BCT   R3,IMPD40                                                        
         B     XIT                                                              
*                                                                               
DIV100   DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R1,15,0(R2)                                                      
         LTR   R1,R1                                                            
         BZ    DIV10X                                                           
         D     R0,=F'100'                                                       
         ST    R1,0(R2)                                                         
DIV10X   BR    RE                                                               
         DROP  R4,R5                                                            
         EJECT                                                                  
*           INITIALIZE DBLOCK FOR EVN                                           
         SPACE 3                                                                
SETDB    NTR1                                                                   
         BAS   RE,GETTVQS                                                       
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK FOR EVN                    
         MVC   DBCOMFCS,NBACOM     COMFACS                                      
         MVC   DBFILE,=C'EVN'                                                   
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'V'                                                    
         MVC   DBSELAGY,NPSELAGY                                                
         MVI   DBFUNCT,DBGETDEM                                                 
         LA    R1,PIO                                                           
         ST    R1,DBAREC                                                        
         LA    R1,PIO+22                                                        
         ST    R1,DBAQUART                                                      
*  SET FOR TVQ LOOKUP                                                           
         XC    DBEXTEND,DBEXTEND                                                
         CLI   GOTUTYPE,1                                                       
         BNE   SETDB10                                                          
         MVC   BTUEXT,=C'UFIL'                                                  
         MVC   BTUEX2,BTUDB+4                                                   
         LA    RE,BTUEXT                                                        
         ST    RE,DBEXTEND                                                      
*--CHECK CABLE PRECISSION                                                       
SETDB10  CLI   NBPREOPT,C'Y'                                                    
         BNE   SETDBEX                                                          
*                                                                               
         LA    R1,WORK                                                          
         USING DBXNTID,R1                                                       
         ST    R1,DBEXTEND                                                      
*                                                                               
         XC    DBXNID(128),DBXNID                                               
         MVC   DBXNID(4),=CL4'NETW'                                             
         MVI   DBXNCR2,C'Y'                                                     
         MVI   DBXNNR2,C'Y'                                                     
*                                                                               
SETDBEX  B     XIT                                                              
         EJECT                                                                  
* READ FOR USER DEFINED ADJUSTMENT FACTORS - TVQ                                
GETTVQS  NTR1                                                                   
         MVI   GOTUTYPE,0                                                       
         XC    BTUEXT,BTUEXT                                                    
         XC    BTUEX1,BTUEX1                                                    
         XC    BTUEX2,BTUEX2                                                    
*                                                                               
         LA    RE,DIADMSET                                                      
         LA    RF,20                                                            
GTTVQ10  CLI   0(RE),171            TVQ CATEGORY                                
         BE    GTTVQ20              YES CONTINUE                                
         LA    RE,3(RE)                                                         
         BCT   RF,GTTVQ10                                                       
         B     BTUXIT               NO EXIT                                     
*                                                                               
GTTVQ20  OC    NPUPTVQB,NPUPTVQB    ANY BOOK INPUTTED                           
         BZ    BTUXIT               NO EXIT                                     
*                                                                               
         OC    NPUPNTI,NPUPNTI                                                  
         BZ    BTUXIT                                                           
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK FOR TVQ                    
         LA    R6,DBLOCK                                                        
         USING DBLOCK,R6                                                        
         MVI   DBMODE,DBMDSEQ                                                   
         MVC   BTUDB,DBLOCK                                                     
         LA    R6,BTUDB                                                         
         MVC   DBCOMFCS,NBACOM     COMFACS                                      
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'V'                                                    
         MVC   DBSELAGY,NPSELAGY                                                
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBMODE,DBMFRST                                                   
         MVI   DBFUNCT,DBGETNTI                                                 
         L     RE,DBAREC                                                        
         USING PMKEY,RE                                                         
         MVC   DBSELPRG,NPUPNTI                                                 
         MVC   DBSELSTA,NPUPNET                                                 
         CLI   NPUPNTIS,X'40'                                                   
         BNH   *+10                                                             
         MVC   DBSELSTA,NPUPNTIS                                                
         MVC   DBSELSTA+4(1),NPUPMEDT                                           
         CLC   DBSELSTA+3(2),=C'PN'                                             
         BNE   *+8                                                              
         MVI   DBSELSTA+3,C' '                                                  
         CLI   DBSELSTA+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'N'                                                  
         CLI   DBSELSTA+4,C'S'                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'M'                                                  
         MVI   DBBTYPE,C'U'                                                     
         MVI   DBSELDAY,0                                                       
         XC    DBSELTIM(4),DBSELTIM                                             
         MVI   DBSELDUR,X'FF'      ALL DURATIONS                                
         MVI   DBSELDAY,0                                                       
         XC    DBSELTIM(4),DBSELTIM                                             
         MVI   DBPRGDUR,C'Y'                                                    
         MVI   DBBEST,C'A'                                                      
         MVI   DBSELDUR,X'FF'                                                   
*********                                                                       
         GOTO1 NBDATCON,DMCB,(2,NPUPTVQB),(0,WORK+3)                            
         GOTO1 =V(NETWEEK),DMCB,WORK+3,NBGETDAY,NBADDAY                         
         MVC   DBSELBK(1),4(R1)                                                 
         MVC   DBSELBK+1(1),8(R1)                                               
*                                                                               
         LA    RE,TVQDATES         YES - CONVET TO TVQ BOOK                     
BTUR2    CLI   0(RE),X'FF'         NOT FOUND - ALLOW A MISS                     
         BE    BTUR4                                                            
         CLC   DBSELBK,2(RE)       CURRENT OUTSIDE RANGE                        
         BH    BTUR3               GET NEXT                                     
         CLC   DBSELBK,0(RE)       TOTALLY OUTSIDE RANGE - ALLOW MISS           
         BL    BTUR4                                                            
         MVC   DBSELBK(2),4(RE)    FOUND THE EQUATE - USE IT                    
         B     BTUR4                                                            
BTUR3    LA    RE,6(RE)                                                         
         B     BTUR2                                                            
*                                                                               
BTUR4    XC    DBAQUART,DBAQUART                                                
         XC    BTUAQ,BTUAQ                                                      
         LA    RE,BTUIO                                                         
         ST    RE,DBAREC                                                        
         L     RF,=F'1000'                                                      
         XCEF                                                                   
         DROP  R6,RE                                                            
*                                                                               
         PRINT GEN                                                              
         GOTO1 NBDEMAND,BTUDMCB,BTUDB,BTUHK                                     
         PRINT NOGEN                                                            
*                                                                               
         LA    RF,BTUDB                                                         
         USING DBLOCK,RF                                                        
         OC    DBAQUART(4),DBAQUART                                             
         BNZ   BTUXIT                                                           
         CLI   GOTUTYPE,1                                                       
         BNE   BTUXIT                                                           
         MVC   DBAQUART(4),BTUAQ                                                
         B     BTUXIT                                                           
BTUHK    LA    RF,BTUDB                                                         
         MVC   BTUAQ,8(RF)                                                      
         MVI   GOTUTYPE,1                                                       
         BR    RE                                                               
BTUXIT   B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ADJUST DEMOS BY GUARANTEED FACTORS                               
         SPACE 3                                                                
ANYADJ   NTR1                                                                   
         L     R3,NBADEM                                                        
         USING NDDEMBLK,R3                                                      
         LA    R3,NDDEMOS                                                       
         LA    R4,NDESTDEM+2                                                    
         ZIC   R5,NDNDEMOS                                                      
         B     ANYADJ1                                                          
*                                                                               
ANYADJH  NTR1                                                                   
         L     R3,NBADEM                                                        
         USING NDDEMBLK,R3                                                      
         LA    R3,NDDEMOS                                                       
         LA    R4,NBESTHOM+2                                                    
         LA    R5,1                                                             
*                                                                               
*        GOTO1 =V(PRNTBL),DMCB,=C'IMPE',NBINDS,C'DUMP',1,=C'1D'                 
ANYADJ1  TM    NBINDS,X'40'        CHECK FOR INCREASED PRECISSION               
         BZ    ANYADJ1A                                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'IMPE',2(R4),C'DUMP',4,=C'1D'                  
         SR    R0,R0                                                            
         L     R1,2(R4)                                                         
         M     R0,=F'100'                                                       
         ST    R1,2(R4)                                                         
*                                                                               
ANYADJ1A ICM   R2,15,NPUPPKGA      PICK UP ALL ADJUSTMENT FACTOR                
         CLC   2(1,R3),NPUPDEMS+2  OR IF SELECTED DEMO IS TARGET                
         BNE   ANYADJ2                                                          
         OC    NPUPDEMA,NPUPDEMA   AND THERE IS A DEMO FACTOR                   
         BZ    ANYADJ2                                                          
         ICM   R2,15,NPUPDEMA      DEMO ADJUSTMENT FACTOR                       
*                                                                               
ANYADJ2  LTR   R2,R2                                                            
         BZ    ANYADJ6                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,0(R4)          ADJUST GRPS                                  
         MR    R0,R2                                                            
         D     R0,=F'500000'                                                    
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STCM  R1,3,0(R4)                                                       
         SPACE 1                                                                
ANYADJ4  L     R1,2(R4)            ADJUST IMPRESSIONS                           
         MR    R0,R2                                                            
         D     R0,=F'500000'                                                    
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,2(R4)                                                         
ANYADJ6  LA    R3,3(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R5,ANYADJ1                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*--MOVE PUP INFORMATION TO DEMO BLOCK                                           
EXTBLOCK NTR1                                                                   
         MVC   NBAPRD,NPPLANAD     ADD OF PLAN RECORD                           
         MVC   NBLEN,NPUPLEN                                                    
         MVC   NBACTNET,NPUPNET                                                 
         MVC   NBACTDP,NPUPDPT                                                  
         MVC   NBCLICOD,NPUPCL3                                                 
         MVC   NBMGFPCD(4),NPUPPLAN                                             
         MVC   NBMGFPNM,NPUPLNNM                                                
         MVC   NBACTPRG,NPUPPROG                                                
         MVC   NBPROGNM,NPUPRGNM                                                
         MVC   NBDAY,NPUPDAY                                                    
         MVC   NBNTI,NPUPNTI                                                    
         MVC   NBTIME,NPUPTIME                                                  
         MVC   NBPAKCPM,NPUPGCPM                                                
         MVC   NBNGUFAC,NPUPPKGA                                                
         MVC   NBPAKCST,NPUPDEMA                                                
         MVC   NBSTATYP,NPUPMEDT                                                
         MVC   NBPOSTYP,NPUPPSTT                                                
         MVC   NBSURVEY,NPUPPSTT                                                
         MVC   NBSPCHRG,NPUPUNIT                                                
         MVC   NBPRFILT,NPUPPRFL                                                
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         LH    R1,NBSPCHRG                                                      
         D     R0,=F'10'                                                        
         STH   R1,NBSPCHRG         IN PROPER PRECISSION                         
         MVC   NBPKFILT(3),NPUPLFLT                                             
         XC    NBACTUAL,NBACTUAL                                                
         XC    NBINTEG,NBINTEG                                                  
*CALCULATE INTEGRATION COST                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,NPUPUNIT       NUMBER OF UNITS                              
         LTR   R1,R1                                                            
         BZ    EXTBLK30                                                         
         D     R0,=F'10'                                                        
         ICM   RE,15,NPUPPINT                                                   
         LTR   RE,RE                                                            
         BZ    EXTBLK30                                                         
         MR    R0,RE                                                            
         ST    R1,NBINTEG                                                       
*        GOTO1 =V(PRNTBL),DMCB,=C'LENG',NBLEN,C'DUMP',4,=C'1D'                  
*        GOTO1 =V(PRNTBL),DMCB,=C'UNIT',NBSPCHRG,C'DUMP',4,=C'1D'               
*                                                                               
EXTBLK30 GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(RC),RR=RELO GETPDATE                   
*--GET DAY NAME                                                                 
         LA    RE,10                                                            
         LA    RF,DAYTAB                                                        
EXTBLK50 CLC   NBDAY,0(RF)                                                      
         BE    EXTBLK60                                                         
         LA    RF,4(RF)                                                         
         BCT   RE,EXTBLK50                                                      
         DC    H'0'                                                             
EXTBLK60 MVC   NBDAYNAM,1(RF)                                                   
         B     XIT                                                              
DAYTAB   DC    XL1'00',CL3'M-F'                                                 
         DC    XL1'01',CL3'MON'                                                 
         DC    XL1'02',CL3'TUE'                                                 
         DC    XL1'03',CL3'WED'                                                 
         DC    XL1'04',CL3'THU'                                                 
         DC    XL1'05',CL3'FRI'                                                 
         DC    XL1'06',CL3'SAT'                                                 
         DC    XL1'07',CL3'SUN'                                                 
         DC    XL1'08',CL3'M-S'                                                 
         DC    XL1'09',CL3'VAR'                                                 
         EJECT                                                                  
*--MOVE BUDGET INFORMATION TO DEMO BLOCK                                        
EXTBUDG  NTR1                                                                   
         CLI   NPUPNWPL,X'FF'      SEE IF FIRST PASS FOR PLAN                   
         BNE   EXTBDEX                                                          
         MVC   NBCALCOS,NPUPGCPM   CPM GUARANTEE FROM THE PLAN                  
         MVI   NPUPNWPL,0                                                       
         XC    NBSPCHRG,NBSPCHRG   CLEAR UNIT COUNT                             
*                                                                               
         LA    R5,BUDGTAB                                                       
         LA    R6,NPUPALEN                                                      
         LA    RF,16                                                            
*                                                                               
EXTBD030 OC    NPSELSDT(4),NPSELSDT   DATE FILTER                               
         BZ    EXTBD040                                                         
         CLC   2(2,R5),NPSELSDT                                                 
         BL    EXTBD060                                                         
         CLC   2(2,R5),NPSELEDT                                                 
         BH    EXTBD060                                                         
EXTBD040 CLI   NPSELLEN,0                                                       
         BE    EXTBD150                                                         
         CLC   NPSELLEN,4(R5)                                                   
         BE    EXTBD150                                                         
EXTBD060 LA    R5,9(R5)                                                         
         BCT   RF,EXTBD030                                                      
*                                                                               
EXTBDEX  B     XIT                                                              
*                                                                               
EXTBD150 MVC   NPUPBUDD(7),2(R5)                                                
         TM    NPPUPARM,NPEXBOPT                                                
         BZ    EXTBD170                                                         
*        CLI   NPUPBUDD+2,0        CHECK 0 LEN                                  
*        BE    EXTBD060                                                         
*        TM    NPPUPARM,NPBUDGT    DETAIL BUDGET INFO                           
*        BZ    *+10                                                             
         MVC   NBACTPRG,=CL6'******'                                            
         MVC   NBPROGNM(16),=CL16'* PLAN BUDGETS *'                             
         XC    NBPRFILT,NBPRFILT                                                
         MVC   NBACTDAT,NPUPBUDD   DATE                                         
         MVC   NBSDAFDT(3),NPUPBUDD  DATE/LENGTH                                
         MVC   NBACTUAL,NPUPBUDG     BUDGET DOLLARS                             
*        MVC   NBLEN,NPUPBUDD+2    LENGTH                                       
         XC    NBTIME,NBTIME       CLEAR TIME                                   
EXTBD170 BAS   RE,GOHOOK                                                        
         XC    NBCALCOS,NBCALCOS                                                
         B     EXTBD060                                                         
         EJECT                                                                  
HIGH     NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=CL8'DMRDHI'          HANDLE COMMANDS                    
         B     DIRALL                                                           
         SPACE 1                                                                
SEQ      NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     DIRALL                                                           
         SPACE 1                                                                
STREAD   NTR1                                                                   
         MVC   COMMAND,=CL8'DMREAD'                                             
         MVC   FILE,=C'STATION'                                                 
         B     DIRALL2                                                          
         SPACE 1                                                                
READ     NTR1                                                                   
         MVC   COMMAND,=CL8'DMREAD'                                             
         SPACE 1                                                                
DIRALL   MVC   FILE(8),=C'UNTDIR  '         DIRECTORIES                         
DIRALL2  GOTO1 NBDM,DMCB,COMMAND,FILE,KEY,KEY,0                                 
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
         SPACE 1                                                                
GETPLAN  NTR1                                                                   
         L     R2,NPPLANAD                                                      
         B     GETREC1                                                          
GETPROG  NTR1                                                                   
         L     R2,NPPROGAD                                                      
         B     GETREC1                                                          
         SPACE 3                                                                
GETREC1  LA    R3,KEY+21                                                        
         MVC   FILE(8),=C'UNTFILE '     FILE                                    
         MVC   NPDTADSP,=H'27'                                                  
         SPACE 1                                                                
GETREC2  GOTO1 NBDM,DMCB,(X'00',=C'GETREC'),FILE,(R3),(R2),DMWORK               
         BAS   RE,DMCHECK                                                       
         B     XIT                                                              
         SPACE 1                                                                
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
         GETEL (R6),NPDTADSP,ELCODE                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO TEST MASK                                             
         SPACE 3                                                                
TESTMASK NTR1                                                                   
*                                  R1=TEST NUMBER                               
*                                  R2=A(MASK)                                   
         SR    R0,R0                                                            
         SLDL  R0,29                                                            
         SRL   R1,29                                                            
         AR    R2,R0                                                            
         LA    R1,BITLIST(R1)                                                   
         MVC   BITTEST,0(R2)                                                    
         NC    BITTEST,0(R1)                                                    
         CLI   BITTEST,0                                                        
         BE    NO                                                               
         B     YES                                                              
         SPACE 1                                                                
BITLIST  DC    X'8040201008040201'                                              
         SPACE 1                                                                
GOHOOK   NTR1                                                                   
         L     RF,NPAHOOK                                                       
         L     RE,USERRD           USERS RD                                     
         LM    R0,RC,20(RE)        RESTORE USERS R0-RC                          
         BASR  RE,RF               RF CONTAINED A(HOOK ROUTINE)                 
         XIT1                                                                   
         SPACE 1                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*--OVERFLOW ROUTINES                                                            
*                                                                               
         DS    0F                                                               
         DROP  RB                                                               
OVFLRTN  NMOD1 0,**A55OV*                                                       
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         USING OVFLRTN+4096,R8                                                  
         L     RC,4(R1)                                                         
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRANCH(RF)                                                     
*                                                                               
OVBRANCH B     BLDDEMIN                                                         
         B     MOVESTDM                                                         
         B     GETPDATE                                                         
         B     SETBDATE                                                         
         B     CLRDEMOS                                                         
         B     CHEKFILT                                                         
         EJECT                                                                  
******************************************************                          
* NVU.1                                                                         
* SUBROUTINE BLDDEMIN - BUILD INPUT LIST FOR DEMOUT.                            
*   TAKES 3-CHAR DEMO CODE REQUESTS FROM NETBLOCK AND INSERTS                   
*   THE APPROPRIATE CHARACTER TO REQUEST VPHS, GRPS AND IMPS.                   
*                                                                               
*  CALLED FROM: DODEMS - FILL IN ALL DEMO STUFF IN NETBLOCK           *         
*                                                                     *         
*  INPUTS: NDDEMOS - CONTAINS THE 3-CHAR DEMOS REQUESTED              *         
*                                                                     *         
*  OUTPUTS: DIALIST - LIST USED AS INPUT TO DEMOUT.                   *         
*           DIULIST - LIST OF UNIVS ALSO USED AS DEMOUT INPUT         *         
*           NDNDEMOS -THE NUMBER OF 3-CHAR DEMOS                      *         
*           NDWGTPOS - POSITION (OFFSET IN NDDEMOS) OF WEIGHTED DEMO  *         
*                                                                     *         
*  LOCALS:  R2 - LOCATION IN DIALIST TO PUT 3-BYTE DEMO REQUEST       *         
*           R3 - COUNTER OF CURRENT OFFSET IN NDDEMOS                 *         
*           R4 - LOCATION OF 3-BYTE DEMO REQUEST IN NETBLOCK          *         
*           R6 - LOCATION IN DIULIST TO PUT 3-BYTE UNIV REQUEST       *         
*           R7 - ADDRESS OF DEMO BLOCK                                          
*                                                                     *         
***********************************************************************         
*                                                                               
BLDDEMIN LA    RF,DIALEN                                                        
         XCEF  DIALIST,(RF)                                                     
         MVI   DIASHARE+1,C'S'     DEMO CODE FOR SHARE                          
         MVI   DIASHARE+2,X'01'                                                 
         MVI   DIAHUT+1,C'P'       DEMO CODE FOR PUT                            
         MVI   DIAHUT+2,X'01'                                                   
         MVI   DIAVHOME+1,C'V'     DEMO CODE FOR TOT HOMES                      
         MVI   DIAVHOME+2,X'01'    DEMO CODE FOR TOT HOMES                      
         MVI   DIAGHOME+1,C'R'     DEMO CODE FOR HOMES GRP                      
         MVI   DIAGHOME+2,X'01'                                                 
         MVI   DIAIHOME+1,C'T'     DEMO CODE FOR HOMES IMPRESSION               
         MVI   NBHUNOPT,C'Y'       DONE FOR PRECISSION                          
         CLI   NPUPPSTT,C'C'       EXCEPT FOR MAJOR NETWORK                     
         BE    BLDDEM2                                                          
         CLI   NPUPPSTT,C'O'       EXCEPT FOR SYNDICATION                       
         BE    BLDDEM2                                                          
         MVI   DIAIHOME+1,C'H'     NET IN HUNDREDS                              
         SPACE 1                                                                
BLDDEM2  MVI   DIAIHOME+2,X'01'                                                 
         MVI   DIUHOME+1,C'U'      DEMO CODE FOR HOMES UNIVERSE                 
         MVI   DIUHOME+2,X'01'                                                  
*                                                                               
         MVI   DIADMSET,X'FF'      START WITH EMPTY DIADMSET                    
         MVI   DIUDMSET,X'FF'      START WITH EMPTY DIUDMSET                    
         OC    NBADEM,NBADEM       DEMO BLOCK SPECIFIED?                        
         BZ    XITBLD                                                           
*                                                                               
         LA    R2,DIADMSET                                                      
         LA    R6,DIUDMSET                                                      
         L     R7,NBADEM                                                        
         USING NDDEMBLK,R7                                                      
         SR    R3,R3                                                            
         LA    R4,NDDEMOS                                                       
         MVI   NDWGTPOS,0          INITIALIZE IN CASE NO WEIGHTS                
*                                                                               
*                                  DO FOR EACH 3-BYTE DEMO CODE                 
BLDLOOP  OC    0(3,R4),0(R4)       TEST FOR END OF LIST                         
         BZ    ENDBLOOP                                                         
         CLI   0(R4),X'FF'                                                      
         BE    ENDBLOOP                                                         
         CLI   1(R4),X'21'         IF A USER DEMO                               
         BE    BLDD2               DON'T ADD PREFIXES                           
         CLI   1(R4),63            IF A WEIGHTED DEMO                           
         BNE   FILLTBL             DON'T ADD PREFIXES AND                       
         STC   R3,NDWGTPOS         STORE OFFSET IN NDWGTPOS                     
         SPACE 1                                                                
BLDD2    MVC   0(3,R2),0(R4)       PUT UNPREFIXED DEMO IN DIADMSET              
         MVC   3(3,R2),0(R4)       DEMOUT WILL RETURN 0S FOR THESE              
         MVC   6(3,R2),0(R4)       DEMOS                                        
         MVC   0(3,R6),0(R4)       UNIVERSE                                     
         B     NEXTDEM                                                          
*                                                                               
FILLTBL  MVC   0(3,R2),0(R4)       PUT 3-BYTE DEMO CODE                         
         MVI   1(R2),C'V'          VPH PREFIX                                   
         MVC   3(3,R2),0(R4)                                                    
         MVI   4(R2),C'R'          GRP  PREFIX                                  
         MVC   6(3,R2),0(R4)                                                    
         MVI   7(R2),C'T'          IMPRESSION PREFIX (THOUS)                    
*        CLI   NBHUNOPT,C'Y'                                                    
         BNE   FILLTBL2                                                         
         CLI   NPUPPSTT,C'C'                                                    
         BE    FILLTBL2                                                         
         CLI   NPUPPSTT,C'O'                                                    
         BE    FILLTBL2                                                         
         MVI   7(R2),C'H'          NET IN HUNDS                                 
         SPACE 1                                                                
FILLTBL2 MVC   0(3,R6),0(R4)                                                    
         MVI   1(R6),C'U'          UNIVERSE PREFIX                              
*                                                                               
NEXTDEM  LA    R2,9(R2)            GET READY FOR NEXT 3-BYTE DEMO CODE          
         LA    R6,3(R6)                                                         
         LA    R3,1(R3)                                                         
         LA    R1,NDMAXDEM-1       CHECK IF LIST FULL                           
         CR    R3,R1                                                            
         BNL   ENDBLOOP                                                         
         LA    R4,3(R4)            GET NEXT                                     
         B     BLDLOOP                                                          
*                                                                               
ENDBLOOP MVI   0(R2),X'FF'         MARK END OF INPUT LIST                       
         MVI   0(R6),X'FF'                                                      
         STC   R3,NDNDEMOS                                                      
XITBLD   B     OVXIT                                                            
         DROP  R7                                                               
************************************************************                    
         EJECT                                                                  
************************* HIPO ****************************************         
*  NV1.8.6                                                                      
*  TITLE: SUBROUTINE MOVESTDM - PUT ESTIMATE DEMOS IN BLOCK.                    
*           CONVERT THEM FROM THE DEMOUT OUTPUT LIST                            
*               TO THE COMPRESSED FORMAT OF NETBLOCK.                           
*                                                                     *         
*  CALLED FROM: STPUTEM - ANY TIME DEMOS ARE REQUESTED                          
*                                                                     *         
*  INPUTS: DOALIST - CONTAINS THE 1 WORD DEMO VALUES GIVEN BY DEMOUT. *         
*                                                                               
*  OUTPUTS: NETBLOCK -                                                          
*           NDDEMBLK - DEMO OUTPUT BLOCK.                                       
*                                                                     *         
*  LOCALS: R2 - ADDRESS OF CURRENT DEMO VALUE IN DOALIST              *         
*          R3 - COUNTER USED TO DO EACH REQUESTED DEMO.               *         
*          R6 - ADDRESS OF NET DEMO UNIVERSE BLOCK                              
*          R7 - ADDRESS OF NET DEMO BLOCK                                       
*                                                                     *         
***** NOTE: ALMOST IDENTICAL TO MOVACTDM                                        
*                                                                     *         
***********************************************************************         
*                                                                               
MOVESTDM LA    R4,NBESTSHR                                                      
         MVC   0(2,R4),DOASHARE+2  USE LEAST SIG 2-BYTES OF SHARE               
         MVC   2(2,R4),DOAHUT+2    USE LEAST SIG 2-BYTES OF HUT                 
         MVC   6(2,R4),DOAVHOME+2  USE LEAST SIG 2-BYTES OF HOMES               
         MVC   8(2,R4),DOAGHOME+2  USE LEAST SIG 2-BYTES OF HOME GRP            
         MVC   10(4,R4),DOAIHOME   USE ALL 4 BYTES OF HOME IMPRESSION           
*                                                                               
*        GOTO1 =V(PRNTBL),DMCB,=C'DEMOS',NBESTSHR,C'DUMP',16,=C'1D'             
*                                                                               
         OC    NBADEM,NBADEM       DOES NET DEMO BLOCK EXIST?                   
         BZ    XITMESTD                                                         
         L     R7,NBADEM                                                        
         USING NDDEMBLK,R7                                                      
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,NDNDEMOS       ALSO SETS COND CODE                          
         BZ    XITMESTD            NO DEMOS REQUESTED                           
         LA    R4,NDESTDEM         POINT R4 TO NEXT DEMOS                       
         LA    R2,DOADMSET                                                      
*                                                                               
*                                  DO FOR EACH DEMO REQUESTED                   
MOVEM    MVC   0(2,R4),2(R2)       USE LEAST SIG 2-BYTES OF VPH                 
         MVC   2(2,R4),6(R2)       USE LEAST SIG 2-BYTES OF GRP                 
         MVC   4(4,R4),8(R2)       USE ALL 4 BYTES OF IMPRESSION                
*                                                                               
         LA    R4,8(R4)            NEXT SET OF DEMO VALUES                      
         LA    R2,12(R2)                                                        
         BCT   R3,MOVEM                                                         
*                                                                               
         MVC   NDESTWUN,DOAWUNIV   MOVE WEIGHTD UNIVERSE                        
*                                                                               
         OC    NDAUBLOK,NDAUBLOK   DOES UNIV BLOCK EXIST?                       
         BZ    XITMESTD                                                         
         L     R6,NDAUBLOK                                                      
         USING NDUNBLOK,R6                                                      
         MVC   NDUEHOME,DOUHOME     MOVE UNIVERSES                              
         ZIC   R1,NDNDEMOS                                                      
         SLL   R1,2                X 4 FOR L'VALUE                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NDUEDEM(0),DOUDMSET                                              
*                                                                               
XITMESTD B     OVXIT                                                            
         DROP  R6                                                               
         DROP  R7                                                               
         EJECT                                                                  
GETPDATE L     R9,APUPBLK                                                       
         USING NETPUPD,R9                                                       
         MVC   NBACTDAT,NPUPPERD                                                
*        GOTO1 =V(PRNTBL),DMCB,=C'DATE',NBACTDAT,C'DUMP',2,=C'1D'               
         CLI   NPUPPTYP,C'W'                                                    
         BE    OVXIT                                                            
*                                                                               
         LA    RE,MONTHTAB                                                      
         CLI   NPUPPTYP,C'M'                                                    
         BE    GETPD050                                                         
*                                                                               
         LA    RE,QUARTTAB                                                      
*                                                                               
GETPD050 XC    DUB,DUB                                                          
         MVC   DUB(1),NPUPPERD     PUT BINARY YEAR IN DUB                       
         PRINT GEN                                                              
         ZIC   RF,NPUPPERD+1                                                    
         PRINT NOGEN                                                            
*                                                                               
GETPD060 MVC   DUB+1(2),0(RE)      MOVE CONVERTED MONTH/QUARTER                 
         LA    RE,3(RE)                                                         
         BCT   RF,GETPD060                                                      
         S     RE,=F'3'                                                         
         CLI   2(RE),X'FF'                                                      
         BNE   GETPD100                                                         
*                                                                               
*--DO SPECIAL 4TH QTR HANDLING                                                  
         CLC   NPUPLNYR,DUB                                                     
         BE    GETPD100                                                         
         MVC   DUB+1(2),=XL2'0918' FOURTH QTR                                   
*                                                                               
GETPD100 GOTO1 NBDATCON,DMCB,(3,DUB),(2,NBACTDAT)                               
         B     OVXIT                                                            
         DROP  R9                                                               
         EJECT                                                                  
SETBDATE LA    R7,16               LOOP COUNT                                   
         LA    R9,BUDGTAB                                                       
*                                                                               
SETBD050 XC    DUB,DUB                                                          
         CLI   0(R9),0                                                          
         BE    SETBEX                                                           
         MVC   DUB(1),0(R9)        PUT BINARY YEAR IN DUB                       
         ZIC   RF,1(R9)            QUARTER NUMBER                               
*                                                                               
         LA    RE,QUARTTAB                                                      
SETBD060 MVC   DUB+1(2),0(RE)      MOVE CONVERTED MONTH/QUARTER                 
         LA    RE,3(RE)                                                         
         BCT   RF,SETBD060                                                      
*                                                                               
         GOTO1 NBDATCON,DMCB,(3,DUB),(2,2(R9))                                  
*                                                                               
         LA    R9,9(R9)            BUMP TO NEXT ENTRY                           
         BCT   R7,SETBD050                                                      
*                                                                               
SETBEX   B     OVXIT                                                            
*                                                                               
MONTHTAB DC    XL3'010F00'                                                      
         DC    XL3'020F00'                                                      
         DC    XL3'030F00'                                                      
         DC    XL3'040F00'                                                      
         DC    XL3'050F00'                                                      
         DC    XL3'060F00'                                                      
         DC    XL3'070F00'                                                      
         DC    XL3'080F00'                                                      
         DC    XL3'0907FF'                                                      
         DC    XL3'0A0F00'                                                      
         DC    XL3'0B0F00'                                                      
         DC    XL3'0C0F00'                                                      
*                                                                               
QUARTTAB DC    XL3'010F00'                                                      
         DC    XL3'040F00'                                                      
         DC    XL3'070F00'                                                      
         DC    XL3'0A0F00'                                                      
         EJECT                                                                  
************************* HIPO ****************************************         
*  NV1.8.6                                                                      
*  TITLE: SUBROUTINE CLRDEMOS - CLEAR THE DEMO BLOCKS                           
*                                                                     *         
***********************************************************************         
*                                                                               
CLRDEMOS L     R3,NBADEM                                                        
         USING NDDEMBLK,R3                                                      
         XC    NBESTSHR,NBESTSHR   EST SHARE                                    
         XC    NBESTHUT,NBESTHUT   EST HUT                                      
         XC    NBESTHOM,NBESTHOM   EST HOMES                                    
*                                                                               
         L     R4,NDARAWEX                                                      
         USING RAWDATA,R4                                                       
         XC    RWESTHOM,RWESTHOM   RAW HOME                                     
         XC    RWESTDEM,RWESTDEM   RAW ESTMITED DEMOS                           
         XC    RWACTHOM,RWACTHOM   RAW HOME                                     
         XC    RWACTDEM,RWACTDEM   RAW ACT DEMOS                                
         XC    RQESTHOM,RQESTHOM   RAW HOME                                     
         XC    RQESTDEM,RQESTDEM   RAW ESTMITED DEMOS                           
         XC    RQACTHOM,RQACTHOM   RAW HOME                                     
         XC    RQACTDEM,RQACTDEM   RAW ACT DEMOS                                
         DROP  R4                                                               
*                                                                               
         OC    NDAUBLOK,NDAUBLOK   DOES UNIV BLOCK EXIST?                       
         BZ    CLRDEM60                                                         
         L     R4,NDAUBLOK                                                      
         USING NDUNBLOK,R4                                                      
         XC    NDUEHOME,NDUEHOME    HOME UNIVERSE                               
         XC    NDUEDEM(80),NDUEDEM  EST UNIVERSES                               
*                                                                               
CLRDEM60 XC    NDESTDEM,NDESTDEM   EST DEMOS                                    
         XC    NDESTWUN,NDESTWUN   WEIGHTED DEMO UNIVERSE                       
         B     OVXIT                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
************************* HIPO ****************************************         
*  CHECK PROGRAM FILTERS ON PLAN RECORD                               *         
*                                                                     *         
***********************************************************************         
*                                                                               
CHEKFILT L     R2,8(R1)            OPTION FILTER                                
         L     R3,12(R1)           PROGRAM RECORD FILTER                        
*                                                                               
         MVI   NEGFLT,C'N'                                                      
*                                                                               
         OC    0(4,R2),0(R2)       ARE ANY SPECIFIED                            
         BE    CHEFYES             NO SO ITS OK                                 
         CLI   0(R2),C'-'          NEGATIVE FILTERING                           
         BNE   CHEF4               NO                                           
         LA    R2,1(R2)            BUMP TO NEXT SPOT                            
         MVI   NEGFLT,C'Y'         SET NEGATIVE FILTERING                       
*                                                                               
CHEF4    LA    R0,3                                                             
*                                                                               
CHEF6    CLI   0(R2),C'*'          * IS WILD                                    
         BE    CHEF10                                                           
         CLI   0(R2),0             SO IS ZERO                                   
         BE    CHEF10                                                           
         CLI   0(R2),C'?'          QUESTION SIGN IS SPECIAL CHAR MATCH          
         BE    CHEF8                                                            
         CLC   0(1,R2),0(R3)       MUST MATCH                                   
         BNE   CHEFNCK                                                          
         B     CHEF10                                                           
*                                                                               
CHEF8    CLI   0(R3),C' '          MATCH ON ANY SPECIAL CHARACTER               
         BH    CHEFNCK             INCLUDING SPACE AND BINARY ZERO              
         SPACE 1                                                                
CHEF10   LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,CHEF6                                                         
         B     CHEFYCK                                                          
*                                                                               
CHEFNCK  CLI   NEGFLT,C'Y'         IF NEGATIVE FILTER THEN NO MEANS YES         
         BE    CHEFYES                                                          
CHEFNO   MVI   4(R1),X'FF'                                                      
         B     OVXIT                                                            
         SPACE 1                                                                
CHEFYCK  CLI   NEGFLT,C'Y'         IF NEGATIVE FILTER THEN YES MEANS NO         
         BE    CHEFNO                                                           
CHEFYES  MVI   4(R1),X'00'                                                      
         B     OVXIT                                                            
         EJECT                                                                  
OVXIT    XMOD1 1                                                                
         LTORG                                                                  
*              DSECTS ETC                                                       
         SPACE 3                                                                
       ++INCLUDE DETVQDATE                                                      
*                                                                               
MYD      DSECT                                                                  
PUPWRK   DS    0C                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
WORK     DS    CL200                                                            
PARAS    DS    0F                                                               
DMCB     DS    6F                                                               
USERRD   DS    F                                                                
APUPBLK  DS    F                                                                
BYTE     DS    CL1                                                              
KEY      DS    CL48                                                             
KEYSAVE  DS    CL48                                                             
ELCODE   DS    CL1                                                              
BITTEST  DS    CL1                                                              
NEGFLT   DS    CL1                 YES = NEGATIVE PROGRAM FILTER                
TVQSW    DS    CL1                 YES = LOOK UP TVQ RECORD                     
COMMAND  DS    CL8                                                              
FILE     DS    CL8                                                              
DMWORK   DS    CL96                                                             
RELO     DS    A                                                                
BUDGTAB  DS    CL144                                                            
*                                                                               
INTGTBL  DS    0CL17               AREA FOR INTEGRATION LOOKUP                  
INTGREAD DS    X                                                                
INTGAIO  DS    A                                                                
INTGSTDT DS    H                                                                
INTGEDDT DS    H                                                                
INTHLDIT DS    A                                                                
INTDKA   DS    A                                                                
*                                                                               
DIALIST  DS    0C                  INPUT TO DEMOUT                              
DIASHARE DS    CL3                                                              
DIAHUT   DS    CL3                                                              
DIAVHOME DS    CL3                                                              
DIAGHOME DS    CL3                                                              
DIAIHOME DS    CL3                                                              
DIADMSET DS    (20*3)CL3           UP TO 20 SETS OF 3 DEMO REQUESTS             
         DS    CL1                 MAY BE NEEDED TO MARK END-OF-LIST            
DIULIST  DS    0C                   UNIVERSE LIST                               
DIUHOME  DS    CL3                                                              
DIUDMSET DS    20CL3                                                            
         DS    CL1                  E-O-L                                       
DIALEN   EQU   *-DIALIST           LENGTH OF DIALIST                            
*                                                                               
DOALIST  DS    0F                  OUTPUT FROM DEMOUT                           
DOASHARE DS    F                                                                
DOAHUT   DS    F                                                                
DOAVHOME DS    F                                                                
DOAGHOME DS    F                                                                
DOAIHOME DS    F                                                                
DOADMSET DS    (20*3)F             UP TO 20 SETS OF 3 DEMOS EACH                
DOAWUNIV DS    F                   WEIGHTED UNIVERSE                            
DOULIST  DS    0F                   UNIVERSE LIST                               
DOUHOME  DS    F                                                                
DOUDMSET DS    20F                                                              
DOALEN   EQU   *-DOALIST                                                        
*                                                                               
         SPACE 2                                                                
*--AREA TO BUILD EVN RECORD                                                     
PIO      DS    0C                                                               
EVNKEY   DS    CL22                (PHONY KEY)                                  
         SPACE 1                                                                
PUEL     DS    0CL243              UNIVERSE ELEMENT                             
         DS    XL3                 X'31F344'                                    
UNIVS    DS    CL240               UNIVERSES                                    
         SPACE 1                                                                
PVEL     DS    0CL243              VPH ELEMENT                                  
         DS    XL3                 X'33F302'                                    
VPHS     DS    CL240               VPHS                                         
         SPACE 1                                                                
PREL     DS    0CL9                RATING/HUT/SHARE ELEMENT                     
         DS    XL3                 X'350902'                                    
RATING   DS    XL2                 RATING                                       
HUT      DS    XL2                 HUT                                          
SHARE    DS    XL2                 SHARE                                        
         SPACE 1                                                                
PBEL     DS    CL7                 BOOK ELEMENT                                 
         SPACE 1                                                                
OVERAREA DS    0CL64               UP TO 10 OVERRIDES PLUS EOR                  
OVEREL   DS    0CL6                OVERRIDE ELEMENT                             
         DS    CL2                 X'DD06'                                      
OVERDEMO DS    CL2                                                              
OVERAMNT DS    CL2                                                              
         DS    CL58                                                             
         SPACE 2                                                                
*  TVQ BLOCK AREA                                                               
GOTUTYPE DS    CL1                                                              
*                                                                               
         DS    0F                                                               
BTUEXT   DS    CL4                                                              
BTUEX1   DS    A(0)                                                             
BTUEX2   DS    A(0)                                                             
*                                                                               
BTUAQ    DS    F                                                                
BTUDMCB  DS    10F                                                              
BTUDB    DS    XL256                                                            
         DS    CL4                                                              
BTUIO    DS    1000C                                                            
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
PUPWRKX  EQU   *                                                                
         EJECT                                                                  
LISTD    DSECT                     PRODUCT LIST ENTRY                           
LISTNO   DS    XL1                 PRODUCT NUMBER                               
LISTDIV  DS    XL2                 DIVISION NUMBER                              
LISTPRD  DS    CL3                 PRODUCT ALPHA                                
LISTTARG DS    XL1                 TARGET DEMO NUMBER                           
LISTUSER DS    XL1                 USER NUMBER                                  
LISTDEMO DS    CL16                UP TO 8 ADDITIONAL DEMOS                     
         EJECT                                                                  
       ++INCLUDE NETBLOCKD                                                      
         EJECT                                                                  
       ++INCLUDE NENETPUPD                                                      
         EJECT                                                                  
       ++INCLUDE NEGENPLAN                                                      
         EJECT                                                                  
       ++INCLUDE NEGENPUA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE NENETDEMT                                                      
         EJECT                                                                  
       ++INCLUDE NETUNIVD                                                       
         EJECT                                                                  
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017NENETPUPX 05/01/02'                                      
         END                                                                    
