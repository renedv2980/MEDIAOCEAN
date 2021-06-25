*          DATA SET CTREP5002A AT LEVEL 045 AS OF 05/01/02                      
*PHASE CT5002A,+0                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE CLUNPK                                                                 
         TITLE 'ID REPORT'                                                      
*                                                                               
*        QOPT2  Y=PROGRAM EXCEPTION IDS ONLY                                    
*                                                                               
CT5002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**IDS***,RR=R2                                                 
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING CT5002+4096,R9                                                   
         ST    R2,RELO                                                          
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         CLI   MODE,RUNFRST                                                     
         BNE   TRA                                                              
         L     R8,=A(BUFFALOC)                                                  
         A     R8,RELO                                                          
         GOTO1 BUFFALO,DMCB,=C'SET',(R8)                                        
         B     XIT                                                              
         SPACE 2                                                                
TRA      CLI   MODE,REQFRST                                                     
         BNE   TRC                                                              
         MVI   RCSUBPRG,1                                                       
         CLI   QOPT1,C' '                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,3                                                       
         BAS   RE,SYSHEAD                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         L     R2,=A(SYSBUFF)      NEED SYSTEM LIST RECORD                      
         USING CTWKEY,R2                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R2),(R2),0                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
PIDFIL   EQU   *                   PROCESS PRINCIPLE ID FILTER                  
         XC    PIDNUM,PIDNUM                                                    
         CLC   QID,SPACES                                                       
         BE    XIT                                                              
         OC    QID,QID                                                          
         BZ    XIT                                                              
         L     R2,=A(PIDBUFF)                                                   
         USING CTIKEY,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,QID                                                       
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R2),(R2),0                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,CTIDATA(R2)                                                   
         SR    R1,R1                                                            
PIDF010  CLI   0(RF),0                                                          
         BE    XIT                                                              
         CLI   0(RF),X'03'                                                      
         BE    PIDF020                                                          
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     PIDF010                                                          
PIDF020  MVC   PIDNUM,2(RF)                                                     
PIDFILX  EQU   *                                                                
         B     XIT                                                              
         SPACE 2                                                                
TRC      CLI   MODE,REQLAST                                                     
         BNE   TRD                                                              
         BAS   RE,PROGXREF                                                      
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS AN ID RECORD                                             
         SPACE 3                                                                
TRD      CLI   MODE,PROCID                                                      
         BNE   XIT                                                              
         MVI   FORCEHED,C'Y'                                                    
         L     R2,ADRECORD                                                      
         USING CTIREC,R2                                                        
         CLI   CTIKID,0                                                         
         BE    XIT                 IGNORE PASSIVES                              
**NEW 12/9/88                                                                   
         CLI   QOPT2,C'Y'          EXCEPTION ID'S ONLY                          
         BNE   TRD3                                                             
         L     R4,ADRECORD                                                      
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                 NO EXCEPTIONS SKIP THIS ID                   
**NEW 12/9/88                                                                   
TRD3     EQU   *                                                                
*                                                                               
         OC    PIDNUM,PIDNUM       PRINCIPLE ID FILTER                          
         BZ    TRD3A                                                            
         L     R4,ADRECORD                                                      
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         CLC   PIDNUM,2(R4)                                                     
         BNE   XIT                                                              
*                                                                               
TRD3A    EQU   *                                                                
         OC    QAGENCY(2),QAGENCY   AGENCY ALPHA ID FILTER                      
         BZ    TRD3B                                                            
         CLC   QAGENCY(2),SPACES                                                
         BE    TRD3B                                                            
         L     R4,ADRECORD                                                      
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING CTAGYD,R4                                                        
         CLC   CTAGYID,QAGENCY                                                  
         BNE   XIT                                                              
         DROP  R4                                                               
*                                                                               
TRD3B    EQU   *                                                                
         MVC   HEAD4+19(10),CTIKID                                              
         L     R4,ADACTIV                                                       
         USING CTACTD,R4                                                        
         GOTO1 DATCON,DMCB,(3,CTACTDT),(8,HEAD5+94)                             
         L     R4,ADDESC                                                        
         LTR   R4,R4                                                            
         BZ    TR2                                                              
         MVC   HEAD5+19(13),SPACES                                              
         USING CTDSCD,R4                                                        
         EDIT  (2,CTDSC),(4,HEAD5+19),ALIGN=LEFT                                
         SPACE 2                                                                
TR2      L     R4,ADRECORD                                                      
         MVC   SYSAGA,SPACES                                                    
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   TR3                                                              
         USING CTAGYD,R4                                                        
         MVC   SYSAGA,CTAGYID                                                   
         SPACE 2                                                                
TR3      L     R4,ADRECORD                                                      
         CLI   QOPT1,C' '          OPTION TO SKIP INTERNAL STUFF                
         BNE   TR9                                                              
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         B     TR6                                                              
         SPACE 2                                                                
TR4      BAS   RE,NEXTEL                                                        
         SPACE 2                                                                
TR6      BNE   TR8                                                              
         BAS   RE,CLEAR                                                         
         BAS   RE,AUTH                                                          
         B     TR4                                                              
         SPACE 2                                                                
TR8      BAS   RE,CLEAR                                                         
         SPACE 2                                                                
TR9      MVI   ELCODE,X'20'        PROCESS ID LIST                              
         L     R4,ADRECORD                                                      
         BAS   RE,GETEL                                                         
         BNE   TR15                                                             
         B     TR12                                                             
         SPACE 2                                                                
TR10     BAS   RE,NEXTEL                                                        
         SPACE 2                                                                
TR12     BNE   TR14                                                             
         BAS   RE,IDLIST                                                        
         B     TR10                                                             
         SPACE 2                                                                
TR14     MVC   P+1(7),=C'ID LIST'                                               
         MVC   PSECOND+1(7),=26C'-'                                             
         GOTO1 =V(SQUASHER),DMCB,AREA,1000,RR=RB                                
         GOTO1 CHOPPER,DMCB,(250,AREA),(60,P+41),(C'P',4)                       
         GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         SPACE 2                                                                
TR15     L     R4,ADRECORD                                                      
         BAS   RE,CLEAR                                                         
         MVI   ELCODE,X'34'        LIST OF VALID DESTINATIONS                   
         BAS   RE,GETEL                                                         
         BNE   TR21                                                             
         B     TR18                                                             
         SPACE 2                                                                
TR16     BAS   RE,NEXTEL                                                        
         SPACE 2                                                                
TR18     BNE   TR20                                                             
         BAS   RE,VALDEST                                                       
         B     TR16                                                             
         SPACE 2                                                                
TR20     MVC   P+1(26),=C'LIST OF VALID DESTINATIONS'                           
         MVC   PSECOND+1(26),=26C'-'                                            
         GOTO1 =V(SQUASHER),DMCB,AREA,250,RR=RB                                 
         GOTO1 CHOPPER,DMCB,(250,AREA),(60,P+41),(C'P',4)                       
         GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         SPACE 2                                                                
TR21     L     R4,ADRECORD                                                      
         BAS   RE,CLEAR                                                         
         MVI   ELCODE,X'3A'        LIST OF VALID PRINTERS                       
         BAS   RE,GETEL                                                         
         BNE   TR28                                                             
         B     TR24                                                             
         SPACE 2                                                                
TR22     BAS   RE,NEXTEL                                                        
         SPACE 2                                                                
TR24     BNE   TR26                                                             
         BAS   RE,VALPRINT                                                      
         B     TR22                                                             
         SPACE 2                                                                
TR26     MVC   P+1(22),=C'LIST OF VALID PRINTERS'                               
         MVC   PSECOND+1(22),=26C'-'                                            
         GOTO1 =V(SQUASHER),DMCB,AREA,1000,RR=RB                                
         GOTO1 CHOPPER,DMCB,(250,AREA),(60,P+41),(C'P',4)                       
         GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         EJECT                                                                  
*              HANDLE DESTINATION & ORIGIN DETAILS                              
         SPACE 3                                                                
TR28     L     R4,ADRECORD                                                      
         MVI   ELCODE,X'36'                                                     
         BAS   RE,GETEL                                                         
         BNE   TR30                                                             
         USING CTORGD,R4                                                        
         MVC   P+1(33),=C'DETAILS OF THIS ORIGIN       NAME'                    
         MVC   PSECOND+1(37),=C'----------------------       ADDRESS '          
         MVI   SPACING,2                                                        
         MVC   P+41(33),CTORGNAM                                                
         MVC   PSECOND+41(33),CTORGADD                                          
         GOTO1 REPORT                                                           
         SPACE 2                                                                
TR30     L     R4,ADRECORD                                                      
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         USING CTDSTD,R4                                                        
         BNE   PL2                                                              
         MVC   P+1(33),=C'DETAILS OF THIS DESTINATION  NAME'                    
         MVC   PSECOND+1(37),=C'---------------------------  ADDRESS '          
         MVI   SPACING,2                                                        
         MVC   P+41(33),CTDSTNAM                                                
         MVC   PSECOND+41(33),CTDSTADD                                          
         CLI   CTDSTLEN,166                                                     
         BL    TR32                                                             
         MVC   PTHIRD+41(33),CTDSTAD2                                           
         MVC   PFOURTH+41(33),CTDSTAD3                                          
         SPACE 2                                                                
TR32     GOTO1 REPORT                                                           
         MVC   P+30(5),=C'LOGOS'                                                
         MVI   PSECOND,0                                                        
         MVC   PTHIRD+30(10),=C'POWER CODE'                                     
         MVC   P+41(7),CTDSTLG1                                                 
         MVC   P+49(7),CTDSTLG2                                                 
         MVC   PTHIRD+41(L'CTDSTPOW),CTDSTPOW                                   
         GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         EJECT                                                                  
*              ROUTINE FOR PROGRAM EXCEPTION LIST                               
         SPACE 3                                                                
PL2      L     R4,ADRECORD                                                      
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   SH2                                                              
         USING CTPRGD,R4                                                        
         MVC   P+1(22),=C'PROGRAM EXCEPTION LIST'                               
         MVC   PSECOND+1(22),=22C'-'                                            
         BAS   RE,CLEAR                                                         
         LA    R5,AREA                                                          
         SPACE 2                                                                
PL4      MVC   0(4,R5),CTPRGRAM                                                 
         MVI   4(R5),C'='                                                       
         MVC   5(1,R5),CTPRGTST                                                 
         MVC   WORK(10),CTIKID                                                  
         LA    R1,WORK                                                          
         SPACE 2                                                                
PL6      CLI   0(R1),C' '                                                       
         BE    PL8                                                              
         LA    R1,1(R1)                                                         
         B     PL6                                                              
         SPACE 2                                                                
PL8      MVI   0(R1),C'='                                                       
         MVC   1(1,R1),CTPRGTST                                                 
         BAS   RE,EXOUT                                                         
         LA    R5,7(R5)                                                         
         BAS   RE,NEXTEL                                                        
         BE    PL4                                                              
         GOTO1 =V(SQUASHER),DMCB,AREA,250,RR=RB                                 
         GOTO1 CHOPPER,DMCB,(250,AREA),(60,P+41),(C'P',4)                       
         GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         B     SH2                                                              
         SPACE 2                                                                
SPLASH   NTR1                                                                   
         CLC   AREA(32),SPACES                                                  
         BE    XIT                                                              
         GOTO1 =V(SQUASHER),DMCB,AREA,1000,RR=RB                                
         GOTO1 CHOPPER,DMCB,(250,AREA),(108,P+1),(C'P',3)                       
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR SHIPPING UNIT/ROUTE                                  
         SPACE 2                                                                
SH2      L     R4,ADRECORD                                                      
         MVI   ELCODE,X'4C'                                                     
         BAS   RE,GETEL                                                         
         BNE   AT2                                                              
         USING CTSHPD,R4                                                        
         SR    R5,R5                                                            
         IC    R5,CTSHPLEN                                                      
         SH    R5,=H'6'                                                         
         MVC   P+1(23),=C'SHIPPING UNIT AND ROUTE'                              
         MVC   PSECOND+1(23),=23C'-'                                            
         GOTO1 CHOPPER,DMCB,((R5),CTSHPINS),(60,P+41),(C'P',4)                  
         GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         B     AT2                                                              
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
ELCODE   DC    X'00'                                                            
         DC    X'00'                                                            
         EJECT                                                                  
*              ROUTINE TO LIST ATTENTION DETAILS                                
         SPACE 3                                                                
AT2      SR    R3,R3                                                            
         L     R4,ADRECORD                                                      
         MVI   ELCODE,X'31'                                                     
         BAS   RE,GETEL                                                         
         B     AT6                                                              
         SPACE 2                                                                
AT4      BAS   RE,NEXTEL                                                        
         SPACE 2                                                                
AT6      BNE   ATEND                                                            
         USING CTATTND,R4                                                       
         MVC   P+41(3),CTATTTYP                                                 
         LA    R6,P+43                                                          
         CLI   0(R6),C' '                                                       
         BNE   AT8                                                              
         BCTR  R6,0                                                             
         CLI   0(R6),C' '                                                       
         BNE   AT8                                                              
         BCTR  R6,0                                                             
         SPACE 2                                                                
AT8      MVI   1(R6),C'='          SHOW EXPANSION OF ATTENTION TYPE             
         SR    R5,R5               XXX=AAAAA ETC                                
         IC    R5,CTATTLEN                                                      
         SH    R5,=H'6'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R6),CTATTDET                                                 
         LA    R3,1(R3)                                                         
         CH    R3,=H'2'                                                         
         BH    AT12                                                             
         BE    AT10                                                             
         MVC   P+1(25),=C'ATTENTION CODE EXPANSIONS'                            
         B     AT12                                                             
         SPACE 2                                                                
AT10     MVC   P+1(25),=25C'-'                                                  
         SPACE 2                                                                
AT12     GOTO1 REPORT                                                           
         B     AT4                                                              
         SPACE 2                                                                
ATEND    CH    R3,=H'1'                                                         
         BL    XIT                                                              
         BE    ATEND2                                                           
         GOTO1 REPORT                                                           
         B     XIT                                                              
         SPACE 2                                                                
ATEND2   MVC   P+1(25),=25C'-'                                                  
         GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         SPACE 2                                                                
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO GET SYSTEM AND PROGRAM NAME FROM SELIST               
         SPACE 3                                                                
GETPROG  NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
         MVC   0(20,R4),=CL20'UNKNOWN   UNKNOWN   '                             
         L     R5,SELIST                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         SPACE 2                                                                
GP2      CLC   SEOVSYS,0(R2)                                                    
         BE    GP4                                                              
         BXLE  R5,R6,GP2                                                        
         B     XIT                                                              
         SPACE 2                                                                
GP4      MVC   0(7,R4),SENAME                                                   
         LA    R7,6(R4)                                                         
         LA    R6,7                                                             
         SPACE 2                                                                
GP6      CLI   0(R7),C'Z'                                                       
         BH    GP8                                                              
         BCTR  R7,0                                                             
         BCT   R6,GP6                                                           
         B     GP10                                                             
         SPACE 2                                                                
GP8      MVI   0(R7),C' '          GET RID OF TRAILING NUMBER                   
         SPACE 2                                                                
GP10     L     R5,SEPGMS                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
         SPACE 2                                                                
GP12     CLC   PGMNUM,0(R3)                                                     
         BE    GP14                                                             
         BXLE  R5,R6,GP12                                                       
         B     XIT                                                              
         SPACE 2                                                                
GP14     MVC   10(7,R4),PGMNAME                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CLEAR AND ADD IDS                                     
         SPACE 3                                                                
CLEAR    MVI   AREA,C' '                                                        
         MVC   AREA+1(249),AREA                                                 
         MVC   AREA+250(250),AREA+000                                           
         MVC   AREA+500(250),AREA+250                                           
         MVC   AREA+750(250),AREA+500                                           
         BR    RE                                                               
         SPACE 2                                                                
IDLIST   NTR1                                                                   
         LA    R5,AREA                                                          
         SPACE 2                                                                
ID2      CLC   0(10,R5),SPACES                                                  
         BE    ID4                                                              
         LA    R5,11(R5)                                                        
         B     ID2                                                              
         SPACE 2                                                                
ID4      DS    0H                                                               
         USING CTIDD,R4                                                         
         MVC   0(10,R5),CTID                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD TO DESTINATION LIST                               
         SPACE 3                                                                
VALDEST  NTR1                                                                   
         LA    R5,AREA                                                          
         SPACE 2                                                                
VD2      CLC   0(16,R5),SPACES                                                  
         BE    VD4                                                              
         LA    R5,17(R5)                                                        
         B     VD2                                                              
         SPACE 2                                                                
         USING CTVALD,R4                                                        
VD4      MVC   0(10,R5),CTVALDST                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD TO PRINTER LIST                                   
         SPACE 3                                                                
VALPRINT NTR1                                                                   
         LA    R5,AREA                                                          
         SPACE 2                                                                
VP2      CLC   0(15,R5),SPACES                                                  
         BE    VP4                                                              
         LA    R5,17(R5)                                                        
         B     VP2                                                              
         SPACE 2                                                                
         USING CTPRND,R4                                                        
VP4      MVC   0(4,R5),CTPRNLIN                                                 
         CLI   3(R5),C' '                                                       
         BNE   *+8                                                              
         SH    R5,=H'1'                                                         
         MVC   5(4,R5),CTPRNADD                                                 
         MVI   4(R5),C'-'                                                       
         EDIT  (1,CTPRNNUM),(5,9(R5)),BRACKET=YES                               
         CLI   12(R5),C' '                                                      
         BNE   *+8                                                              
         MVI   12(R5),0                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE AUTHORIZATIONS                                 
         SPACE 2                                                                
AUTH     NTR1                                                                   
         USING CTSYSD,R4                                                        
         MVI   BYTE,2                                                           
         GOTO1 GETPROG,DMCB,CTSYSNUM,BYTE,WORK                                  
         MVC   P+1(7),WORK                                                      
         GOTO1 HEXOUT,DMCB,CTSYSNUM,P+11,1,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,CTSYSSE,P+19                                         
         BAS   RE,GETFNAME         EXPAND FILE NAME                             
         GOTO1 HEXOUT,DMCB,CTSYSAGB,P+28                                        
         MVI   P+11,C' '                                                        
         MVC   P+35(2),SYSAGA                                                   
         BAS   RE,DISPAC           DISPLAY ACCESS LIMIT                         
         MVC   AREA(15),=C'DEFAULT=X''0000'''                                   
         GOTO1 HEXOUT,DMCB,CTSYSALL,AREA+10,2                                   
         LA    R2,CTSYSPGM                                                      
         LA    R3,AREA+16                                                       
         ZIC   R6,CTSYSLEN                                                      
         SH    R6,=H'16'                                                        
         BZ    AUTH6                                                            
         SPACE 2                                                                
AUTH2    MVC   BYTE(1),0(R2)       COPY THE PROGRAM NUMBER                      
         GOTO1 GETPROG,DMCB,CTSYSNUM,BYTE,WORK                                  
         CLC   WORK+10(7),=C'UNKNOWN'                                           
         BE    AUTH4                                                            
         MVC   0(4,R3),WORK+10                                                  
         MVC   4(8,R3),=C'=X''0000'''                                           
         GOTO1 HEXOUT,DMCB,1(R2),7(R3),2                                        
         CLC   4(8,R3),=C'=X''0000'''                                           
         BNE   *+10                                                             
         MVC   4(8,R3),=C'=N      '                                             
         LA    R3,13(R3)                                                        
         SPACE 2                                                                
AUTH4    LA    R2,3(R2)            3 BYTES PER NON-DEFAULT PROGRAM              
         SH    R6,=H'3'                                                         
         BNZ   AUTH2                                                            
         SPACE 2                                                                
AUTH6    GOTO1 CHOPPER,DMCB,(252,AREA),(60,P+51),(C'P',4)                       
         MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXPAND FILE NAME FROM CTSYSSE                         
         SPACE 3                                                                
GETFNAME NTR1                                                                   
         USING CTSYSD,R4                                                        
         L     R2,=A(SYSBUFF)                                                   
         USING CTWREC,R2                                                        
         LA    R2,CTWDATA                                                       
         SPACE 1                                                                
GETFN2   CLI   0(R2),X'A4'                                                      
         BNE   GETFN4                                                           
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLC   CTSYSSE,11(R2)      CHECK MATCH ON NUMBER                        
         BNE   GETFN4                                                           
         MVC   P+17(7),3(R2)       FOUND DISPLAY NAME                           
         B     XIT                                                              
         SPACE 1                                                                
GETFN4   ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         B     GETFN2                                                           
         EJECT                                                                  
*                                                                               
*              ROUTINE TO DISPLAY ACCESS LIMIT                                  
*                                                                               
DISPAC   NTR1                                                                   
         OC    CTSYSLMT,CTSYSLMT                                                
         BZ    DISPACX                                                          
         MVC   P+42(L'CTSYSLMT),CTSYSLMT                                        
*                                                                               
         CLI   CTSYSNUM,9          MEDIABASE                                    
         BNE   DISPAC10                                                         
         GOTO1 =V(HEXOUT),DMCB,CTSYSLMT,P+42,4,=C'N'                            
*                                                                               
DISPAC10 DS    0H                                                               
*                                                                               
*&&UK                                                                           
         CLI   CTSYSNUM,4          TEST UK/MEDIA                                
         BNE   DISPAC40                                                         
         OC    CTSYSLMT,CTSYSLMT                                                
         BZ    DISPACX                                                          
         LA    RE,P+42                                                          
         LA    R5,CTSYSLMT                                                      
         LA    R7,4                                                             
DISPAC20 SR    R0,R0                255,255,99,99 MAX VALUES                    
         IC    R0,0(R5)                                                         
         EDIT  (R0),(3,0(RE)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    RE,R0     '                                                      
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         LA    R5,1(R5)                                                         
         BCT   R7,DISPAC20                                                      
         BCTR  RE,0                                                             
         MVI   0(RE),C' '                                                       
         B     DISPACX                                                          
*&&                                                                             
*&&US                                                                           
         CLI   CTSYSNUM,3          TEST NETWORK                                 
         BE    *+12                                                             
         CLI   CTSYSNUM,2          TEST SPOT                                    
         BNE   DISPAC30                                                         
*                                                                               
         GOTO1 =V(CLUNPK),DMCB,CTSYSLMT,P+42                                    
*                                                                               
DISPAC30 CLI   CTSYSNUM,6          TEST ACC                                     
         BNE   DISPAC40                                                         
         CLC   CTSYSLMT+3(3),=C'TAL'                                            
         BE    *+14                                                             
         CLC   CTSYSLMT+4(2),=C'TL'                                             
         BNE   DISPACX                                                          
         MVC   P+42(2),=C'T='      FORMAT T=ULCC (DPS TALENT)                   
         MVC   P+42+2(2),CTSYSLMT                                               
         LA    RE,CTSYSLMT+1       UNIT T HEXOUT PARAMS                         
         LA    RF,P+42+3                                                        
         LA    R0,2                                                             
         CLI   P+42+2,C'T'                                                      
         BE    *+16                                                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         LA    R0,1                                                             
         STM   RE,R0,DMCB                                                       
         GOTO1 =V(HEXOUT),DMCB                                                  
         CLC   CTSYSLMT+3(3),=C'TAL' OLD FORMAT                                 
         BE    DISPACX                                                          
         CLI   CTSYSLMT+3,C' '     IS OFFICE PRESENT                            
         BE    DISPACX                                                          
         LA    RF,P+42+6                                                        
         CLI   P+42+2,C'T'                                                      
         BNE   *+8                                                              
         LA    RF,1(RF)            FOR UNIT T STRING IS LONGER                  
         MVI   0(RF),C'*'                                                       
         MVC   1(1,RF),CTSYSLMT+3                                               
         TM    1(RF),X'40'                                                      
         BO    *+12                                                             
         OI    1(RF),X'40'                                                      
         MVI   0(RF),C'$'          SET OFF LIST IF BIT WAS OFF                  
         B     DISPACX                                                          
*&&                                                                             
DISPAC40 CLI   CTSYSNUM,14         TEST PERSONNEL                               
         BNE   DISPACX                                                          
         OC    CTSYSLMT+2(2),CTSYSLMT+2                                         
         BZ    DISPACX                                                          
         LA    RE,P+42             FORMAT N(NN)-N(NN)                           
         ZIC   R0,CTSYSLMT+2                                                    
         EDIT  (R0),(3,0(RE)),ALIGN=LEFT                                        
         AR    RE,R0                                                            
         MVI   0(RE),C'-'                                                       
         ZIC   R0,CTSYSLMT+3                                                    
         EDIT  (R0),(3,1(RE)),ALIGN=LEFT                                        
*                                                                               
DISPACX  XIT1                                                                   
*                                                                               
*              ROUTINE TO WRITE BUFFALO RECORDS FOR SYSTEM/PROGRAMS             
         SPACE 3                                                                
SYSHEAD  NTR1                                                                   
         L     R8,=A(BUFFALOC)                                                  
         A     R8,RELO                                                          
         GOTO1 BUFFALO,DMCB,=C'RESET',(R8)                                      
         LA    R2,2                                                             
SYSHEAD2 ST    R2,DUB                                                           
         GOTO1 GETPROG,DMCB,DUB+3,1,WORK                                        
         CLC   WORK(7),=C'UNKNOWN'                                              
         BE    SYSHEAD8                                                         
         XC    BUFFIO,BUFFIO                                                    
         MVI   BUFFIO,1                                                         
         MVC   BUFFIO+1(1),WORK                                                 
         MVC   BUFFIO+3(7),WORK                                                 
         BAS   RE,BUFFOUT          SYSTEM HEADER                                
         MVI   BUFFIO+2,X'FF'                                                   
         BAS   RE,BUFFOUT                 TRAILER                               
         LA    R3,1                                                             
         LA    R4,31                                                            
         SPACE 2                                                                
SYSHEAD4 STM   R2,R3,DUB                                                        
         GOTO1 GETPROG,DMCB,DUB+3,DUB+7,WORK                                    
         CLC   WORK+10(7),=C'UNKNOWN'                                           
         BE    SYSHEAD6                                                         
         STM   R2,R3,DUB                                                        
         GOTO1 HEXOUT,DMCB,DUB+3,BUFFIO+2,1,=C'TOG'                             
         GOTO1 HEXOUT,DMCB,DUB+7,BUFFIO+4,1,=C'TOG'                             
         MVI   BUFFIO+2,C'T'                                                    
         MVI   BUFFIO+6,0                                                       
         MVC   BUFFIO+7(7),WORK+10                                              
         BAS   RE,BUFFOUT                                                       
         MVI   BUFFIO+6,X'FF'                                                   
         BAS   RE,BUFFOUT                                                       
         SPACE 2                                                                
SYSHEAD6 LA    R3,1(R3)                                                         
         BCT   R4,SYSHEAD4                                                      
         SPACE 2                                                                
SYSHEAD8 CH    R2,=H'15'                                                        
         BE    XIT                                                              
         LA    R2,1(R2)                                                         
         B     SYSHEAD2                                                         
         SPACE 2                                                                
BUFFOUT  NTR1                                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',(R8),BUFFIO                                 
         B     XIT                                                              
         SPACE 2                                                                
BUFFIO   DS    CL32                                                             
         EJECT                                                                  
*              ROUTINE TO WRITE OUT EXCEPTIONS                                  
         SPACE 3                                                                
EXOUT    NTR1                                                                   
         L     R8,=A(BUFFALOC)                                                  
         A     R8,RELO                                                          
         USING CTPRGD,R4                                                        
         XC    BUFFIO,BUFFIO                                                    
         MVI   BUFFIO,1                                                         
         MVC   BUFFIO+1(1),CTPRGRAM                                             
         MVC   BUFFIO+2(4),CTPRGRAM                                             
         MVC   BUFFIO+6(10),WORK                                                
         CLI   BUFFIO+1,C'T'                                                    
         BE    EXOUT2                                                           
         BAS   RE,BUFFOUT                                                       
         MVI   BUFFIO+6,0                                                       
         MVC   BUFFIO+7(10),=CL10'OFFLINE'                                      
         BAS   RE,BUFFOUT                                                       
         MVI   BUFFIO+6,X'FF'                                                   
         BAS   RE,BUFFOUT                                                       
         B     XIT                                                              
         SPACE 2                                                                
EXOUT2   MVI   DUB,X'F0'                                                        
         MVC   DUB+1(1),BUFFIO+3                                                
         GOTO1 =V(HEXIN),DMCB,DUB,DUB+2,2,RR=RB                                 
         GOTO1 GETPROG,DMCB,DUB+2,1,WORK                                        
         MVC   BUFFIO+1(1),WORK                                                 
         BAS   RE,BUFFOUT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT XREF OF PROGRAMS                                
         SPACE 3                                                                
PROGXREF NTR1                                                                   
         L     R8,=A(BUFFALOC)                                                  
         A     R8,RELO                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         XC    BUFFIO,BUFFIO                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R8),BUFFIO,0                              
         B     PX4                                                              
         SPACE 2                                                                
PX2      GOTO1 BUFFALO,DMCB,=C'SEQ',(R8),BUFFIO,0                               
         SPACE 2                                                                
PX4      TM    DMCB+8,X'80'                                                     
         BO    PX5                                                              
         CLI   BUFFIO,1                                                         
         BE    PX6                                                              
         SPACE 1                                                                
PX5      MVI   RCSUBPRG,1                                                       
         B     XIT                                                              
         SPACE 2                                                                
PX6      CLI   BUFFIO+2,0          SYSTEM HEADER                                
         BNE   PX8                                                              
         MVC   P+1(7),BUFFIO+3                                                  
         B     PX2                                                              
         SPACE 2                                                                
PX8      CLI   BUFFIO+2,X'FF'      SYSTEM TRAILER                               
         BNE   PX10                                                             
         GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         B     PX2                                                              
         SPACE 2                                                                
PX10     CLI   BUFFIO+6,0          PROGRAM HEADER                               
         BNE   PX12                                                             
         MVC   P+15(4),BUFFIO+2                                                 
         MVC   P+28(7),BUFFIO+7                                                 
         BAS   RE,CLEAR                                                         
         B     PX2                                                              
         SPACE 2                                                                
PX12     CLI   BUFFIO+6,X'FF'      PROGRAM TRAILER                              
         BNE   PX14                                                             
         GOTO1 =V(SQUASHER),DMCB,AREA,1000,RR=RB                                
         GOTO1 CHOPPER,DMCB,(250,AREA),(60,P+44),(C'P',4)                       
         GOTO1 REPORT                                                           
         B     PX2                                                              
         SPACE 2                                                                
PX14     LA    R2,AREA                                                          
         SPACE 2                                                                
PX15     CLC   0(10,R2),SPACES                                                  
         BE    PX16                                                             
         LA    R2,11(R2)                                                        
         B     PX15                                                             
         SPACE 2                                                                
PX16     MVC   0(10,R2),BUFFIO+6                                                
         B     PX2                                                              
         LTORG                                                                  
SYSAGA   DS    CL2                                                              
RELO     DS    A                                                                
PIDNUM   DS    XL2                                                              
         SPACE 2                                                                
AREA     DC    2000C' '                                                         
         BUFF  LINES=1000,ROWS=0,COLUMNS=0,FLAVOR=DATA,KEYLIST=(16,A)           
         SPACE 1                                                                
SYSBUFF  DS    1000C               SYSTEM LIST RECORD HERE                      
PIDBUFF  DS    1000C               PRINCIPLE ID RECORD HERE                     
         EJECT                                                                  
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAPGMLST                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045CTREP5002A05/01/02'                                      
         END                                                                    
