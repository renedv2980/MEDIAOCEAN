*          DATA SET REREP8502  AT LEVEL 086 AS OF 04/22/15                      
*PHASE RE8502A,+0                                                               
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH                                                                
***********************************************************************         
*          REREP8502 - BUSINESS OPPORTUNITY REPORT - RE85             *         
*                                                                     *         
*---------------------------------------------------------------------*         
* MOD LOG:                                                            *         
*                                                                     *         
* ???????  (???) --- HISTORY LOST                                     *         
*                                                                     *         
* 25JAN91  (EFJ) --- UPDATED TO HANDLE BOP DEMOS VALIDATED BY DEMOVAL *         
*                     IN CONTRACT                                     *         
*                                                                     *         
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                  *         
*                                                                     *         
* FEB26/98 (BU ) --- 4K CONTRACTS REGENALL1 REGENALL1A                *         
*                                                                     *         
* APR05/99 (AST) --- DEMOCON FIX                                      *         
*                                                                     *         
***********************************************************************         
         TITLE 'BUSINESS OPPORUNITY REPORT - RE85'                              
RE8502   CSECT                                                                  
         ENTRY DEMTAB                                                           
         ENTRY SORTC                                                            
         PRINT NOGEN                                                            
         NMOD1 0,**RE8502,RR=RE                                                 
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING RE8502+4096,R9                                                   
         ST    RE,RELO                                                          
         EJECT                                                                  
         CLI   MODE,REQFRST                                                     
         BNE   BOP20                                                            
****                                                                            
* DEMOCON FIX                                                                   
         GOTOX LOADER,DMCB,=CL8'T00AE0',0                                       
         MVC   VDEMOCON,4(R1)                                                   
         OC    VDEMOCON,VDEMOCON                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
****                                                                            
         SPACE 1                                                                
         L     R7,=V(BINSRCH)                                                   
         A     R7,RELO                                                          
         ST    R7,ABINSRCH                                                      
         L     R7,=A(DEMTAB)                                                    
         A     R7,RELO                                                          
         ST    R7,ADEMTAB                                                       
         XC    0(4,R7),0(R7)                                                    
         SPACE 1                                                                
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   DEMOSW,QOPTION1                                                  
         SPACE 1                                                                
         BAS   RE,CLRP             CLEAR PRINT WORK                             
         XC    CNTL,CNTL                                                        
         MVC   DTLN,SPACES                                                      
         XC    DTES(3),DTES        DATE CONTROL                                 
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(3,DTES)                                  
         MVC   DTES+3(3),DTES                                                   
         CLC   QSTART,SPACES                                                    
         BE    BOP10                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(3,DTES)                                  
         MVC   DTES+3(3),DTES                                                   
         SPACE 1                                                                
BOP10    CLC   QEND,SPACES                                                      
         BE    BOP11                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(3,DTES+3)                                  
         SPACE 1                                                                
BOP11    GOTO1 DATCON,DMCB,(3,DTES),(8,DTLN+6)                                  
         MVC   DTLN(4),=C'DATE'                                                 
         CLC   DTES(3),DTES+3                                                   
         BE    BOP16                                                            
         SPACE 1                                                                
         MVI   DTLN+4,C'S'                                                      
         MVI   DTLN+14,C'-'                                                     
         GOTO1 DATCON,DMCB,(3,DTES+3),(8,DTLN+15)                               
         SPACE 1                                                                
BOP16    CLI   INITSW,C'Y'         IS SORT INITIALIZED                          
         BE    BOPOUT                                                           
         L     R3,=A(SORTC)                                                     
         A     R3,RELO                                                          
         GOTO1 =V(SORTER),DMCB,SRTFLD,RECTYP,(40,(R3)),RR=RELO                  
         MVI   INITSW,C'Y'                                                      
         B     BOPOUT                                                           
         EJECT                                                                  
BOP20    CLI   MODE,PROCCONT                                                    
         BNE   BOP40                                                            
         SR    R3,R3                                                            
         LA    R4,RCONELEM                                                      
         SPACE 1                                                                
BOP21    CLI   0(R4),0             LOOK FOR BOP ELEMENT                         
         BE    BOPOUT                                                           
         CLI   0(R4),X'10'                                                      
         BE    BOP23                                                            
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     BOP21                                                            
         SPACE 1                                                                
         USING RCONBPEL,R4                                                      
         USING RCONBTYP,R5                                                      
BOP23    LA    R5,KEY                                                           
         CLC   RCONBDTE,DTES       USE KEY DATE                                 
         BL    BOPOUT                                                           
         CLC   RCONBDTE,DTES+3                                                  
         BH    BOPOUT                                                           
         DROP  R5                                                               
         SPACE 1                                                                
         CLC   7(4,R5),RCONKADV   THEY CHANGED ADVERTISER AND/OR                
         BNE   BOPOUT              STATION AFTER ADDING BOP.                    
         CLC   18(5,R5),RCONKSTA   I HAVE ANOTHER POINTER FOR                   
         BNE   BOPOUT              CORRECT ADV/STA.                             
         SPACE 1                                                                
         OC    CNTL,CNTL           ANYTHING IN CONTROL                          
         BZ    BLDBOP              NOTHING, SO BUILD CONTROL                    
         CLC   CNTL(4),RCONKADV    SAME ADVERTISER                              
         BNE   BOP30               NO, PRINT OLD                                
         CLC   CNTL+4(3),RCONBPDT  DATE                                         
         BNE   BOP30                                                            
         CLC   CNTL+7(4),RCONBPRF  REFERENCE                                    
         BNE   BOP30                                                            
         SPACE 1                                                                
         DROP  R4                                                               
         SPACE 1                                                                
         LA    R3,LINES            SAME BOP                                     
         LA    R4,PB1+84           LOOK FOR NEXT MARKET AREA                    
         LA    RE,6                                                             
         SR    RF,RF                                                            
BOP25    LA    R2,2                2 FIT PER LINE FOR 1ST 7 LINES               
BOP25K   CLC   0(20,R4),SPACES                                                  
         BE    BOP27               FOUND A SPACE                                
         CLC   0(20,R4),RSTAMKT    IS MARKET ALREADY LISTED                     
         BE    BOP27                                                            
         LA    R4,21(R4)                                                        
         BCT   R2,BOP25K           END OF LINE                                  
         SPACE 1                                                                
         LA    RF,1(RF)                                                         
         CR    RF,RE                                                            
         BH    BOP26                                                            
         LA    R4,90(R4)           NEXT LINE -FAR RIGHT                         
         BCT   R3,BOP25                                                         
         SPACE 1                                                                
BOP26    LA    R4,27(R4)           NEXT LINE - FAR LEFT                         
         LA    R2,5                NOW 5 MKTS TO A LINE                         
         BCT   R3,BOP25K                                                        
         DC    H'0'                NO MORE ROOM                                 
         SPACE 1                                                                
BOP27    MVC   0(20,R4),RSTAMKT                                                 
         MVC   SORTMKT,0(R4)                                                    
         B     BLD14                                                            
         SPACE 1                                                                
BOP30    CLI   QOPTION2,C'N'                                                    
         BE    *+8                                                              
         BAS   RE,PRTBOP           PRINT THE BLOCK                              
         BAS   RE,CLRP                                                          
         XC    CNTL,CNTL                                                        
         B     BLDBOP                                                           
         SPACE 1                                                                
CLRP     LA    R3,LINES                                                         
         LA    RF,PRTW                                                          
         MVC   0(132,RF),SPACES                                                 
         LA    RF,132(RF)                                                       
         BCT   R3,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
BOP40    CLI   MODE,REQLAST                                                     
         BNE   BOPOUT                                                           
         CLC   PRTW(10),SPACES     ANYTHING IN BLOCK                            
         BE    BOP41                                                            
         CLI   QOPTION2,C'N'                                                    
         BE    *+8                                                              
         BAS   RE,PRTBOP           PRINT IT                                     
         SPACE 1                                                                
BOP41    MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         MVC   PAGE,=H'1'                                                       
         CLI   INITSW,C'Y'                                                      
         BNE   BOP41A                                                           
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         MVI   INITSW,C'N'      RE-INITIALIZE SORTER NEXT TIME THROUGH          
         B     BOPOUT              NOTHING TO SORT                              
*                                                                               
BOP41A   XC    SVMKT,SVMKT                                                      
         SPACE 1                                                                
BOP42    GOTO1 =V(SORTER),DMCB,=C'GET',RR=RELO                                  
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    BOP50                                                            
         OC    SVMKT,SVMKT                                                      
         BZ    BOP43                                                            
         CLC   SVMKT(20),0(R2)       MARKET                                     
         BE    BOP45                                                            
         MVI   SPACING,1                                                        
         BAS   RE,RPORT                                                         
BOP43    MVC   P(20),0(R2)          NEW MARKET                                  
         SPACE 1                                                                
BOP45    MOVE  (SRTRC,335),(R2)                                                 
         MVC   P(20),SORTMKT                                                    
*                                                                               
         MVC   P+21(20),SORTADV                                                 
         MVC   PSECOND+21(20),SORTPRD                                           
         MVC   PTHIRD+21(8),SORTSCED                                            
         MVC   PTHIRD+31(2),SORTWKS                                             
         MVC   PTHIRD+34(3),=C'WKS'                                             
         CLC   PTHIRD+31(2),=C' 1'                                              
         BNE   *+8                                                              
         MVI   PTHIRD+36,C' '                                                   
*                                                                               
         MVC   P+42(20),SORTAGY                                                 
         MVC   PSECOND+42(2),SORTOFF                                            
         MVC   PTHIRD+42(20),SORTSAL                                            
*                                                                               
         MVC   P+63(20),SORTTIME                                                
         MVC   PSECOND+63(20),SORTOBJ                                           
         MVC   PTHIRD+64(1),SORTMER                                             
         MVC   PTHIRD+71(1),SORTCTYP     Y=NETWORK                              
         MVC   PTHIRD+79(1),SORTORD                                             
*                                                                               
         MVC   P+84(17),SORTAUD                                                 
         MVC   PSECOND+84(9),SORTRAT                                            
         MVC   PTHIRD+84(6),SORTBKS                                             
*                                                                               
         MVC   PFOURTH+26(35),SORTCOMM                                          
         MVC   PFOURTH+62(35),SORTCOMM+35                                       
         CLC   SORTCOMM+70(70),SPACES           IF 3RD OR 4TH LINE              
         BE    BOP47                                                            
         BAS   RE,RPORT            PRINT 1ST 4 LINES OF DATA                    
         MVC   P+26(35),SORTCOMM+70    THEN PRINT REST OF COMMENT               
         MVC   P+62(35),SORTCOMM+105                                            
         SPACE 1                                                                
BOP47    BAS   RE,RPORT                                                         
         MVC   SVMKT(20),0(R2)                                                  
         B     BOP42                                                            
         SPACE 1                                                                
BOP50    MVI   RCSUBPRG,2          DEMO SUMMARY                                 
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         GOTO1 =V(SORTER),DMCB,=C'END'       ALWAYS END THE SORT                
         L     R7,ADEMTAB                                                       
         L     R3,0(R7)                                                         
         LTR   R3,R3                                                            
         BZ    BOPOUT                                                           
         SPACE 1                                                                
         LA    R7,8(R7)                                                         
BOP55    MVC   P+1(7),0(R7)        DEMO                                         
         EDIT  (P4,7(R7)),(5,P+22),ZERO=BLANK                                   
         EDIT  (P4,11(R7)),(5,P+36),ZERO=BLANK                                  
         MVI   SPACING,2                                                        
         BAS   RE,RPORT                                                         
         LA    R7,15(R7)                                                        
         BCT   R3,BOP55                                                         
         B     BOPOUT                                                           
         EJECT                                                                  
         USING RCONBPEL,R4                                                      
BLDBOP   MVC   CNTL,RCONKADV       NEW CONTROL                                  
         MVC   CNTL+4(3),RCONBPDT                                               
         MVC   CNTL+7(4),RCONBPRF                                               
         SPACE 1                                                                
         MVC   SORTADV,RADVNAME    ADVERTISER NAME                              
         MVC   PB1(20),RADVNAME                                                 
         MVC   SORTAGY,RAGYNAM1    AGENCY NAME                                  
         MVC   PB1+21(20),RAGYNAM1                                              
         MVC   PB2(20),RPRDNAME           PRODUCT                               
         MVC   SORTPRD,RPRDNAME                                                 
         GOTO1 DATCON,DMCB,(3,RCONDATE),(8,SORTSCED)                            
         MVC   PB3(8),SORTSCED            SCHEDULE                              
         MVC   SORTWKS,SPACES                WEEKS                              
         CLI   RCONBAWK,C' '                                                    
         BE    BLD2                                                             
         EDIT  (1,RCONBAWK),(2,SORTWKS)                                         
         MVC   PB3+10(2),SORTWKS                                                
         MVC   PB3+13(3),=C'WKS'                                                
         CLI   RCONBAWK,1                                                       
         BNE   *+8                                                              
         MVI   PB3+15,C' '                                                      
         SPACE 1                                                                
BLD2     MVC   PB2+21(2),RCONKOFF            OFFICE                             
         MVC   SORTOFF,RCONKOFF                                                 
         MVC   PB3+21(20),RSALNAME           SALESPERSON                        
         MVC   SORTSAL,RSALNAME                                                 
         MVC   PB1+42(20),RCONBPTM           TIMES                              
         MVC   SORTTIME,RCONBPTM                                                
         MVC   PB2+42(20),RCONBPOB           OBJECTIVES                         
         MVC   SORTOBJ,RCONBPOB                                                 
         MVC   PB3+43(1),RCONBMER                                               
         MVC   SORTMER,RCONBMER    MERCHANDISING                                
         MVC   PB3+58(1),RCONBORD                                               
         MVC   SORTORD,RCONBORD    ORDER PLACED                                 
         MVC   PB3+63(6),RCONBBKS  BOOK                                         
         MVC   SORTBKS,RCONBBKS                                                 
         LA    R1,RATTAB                     RATING SERVICE                     
         SPACE 1                                                                
BLD4     CLI   0(R1),X'FF'                   LOOK UP TABLE                      
         BE    BLD6                                                             
         CLC   0(1,R1),RCONRTGS                                                 
         BE    BLD6                                                             
         LA    R1,18(R1)                                                        
         B     BLD4                                                             
         SPACE 1                                                                
BLD6     MVC   PB2+63(14),4(R1)                                                 
         MVC   PB2+78(5),RCONBPMK                                               
         MVC   SORTRAT(3),1(R1)                                                 
         GOTO1 SQUASHER,DMCB,PB2+63,20                                          
         MVI   SORTRAT+3,C' '                                                   
         MVC   SORTRAT+4(5),RCONBPMK                                            
*                                                                               
         LA    R1,RCONBPDM                   DEMOS                              
         LA    R0,20                                                            
         CLI   0(R1),X'FF'         VALIDATED BY DEMOVAL?                        
         BNE   BLD8                                                             
* 01JAN91  ***START***                                                          
*                                                                               
* CONVERT TO TEXT                                                               
*                                                                               
* OPEN CTFILE FOR DEMOCON                                                       
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'REP',=C'NCTFILE X'                      
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         DROP  RF                                                               
         MVI   DBSELMED,C'R'                                                    
         SPACE 1                                                                
         LA    R2,WORK                                                          
         XC    WORK(30),WORK                                                    
         MVC   0(L'RCONBPDM-1,R2),RCONBPDM+1     DEMOS + ENDING ZERO            
         LA    R3,2                                                             
*                                                                               
         CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         LA    R2,3(R2)                                                         
         BCT   R3,*-16                                                          
*                                                                               
         SPACE 1                                                                
         GOTO1 VDEMOCON,DMCB,(2,WORK),(9,PB1+63),(0,DBLOCK)                     
         LA    R0,20                                                            
         LA    R1,PB1+63                                                        
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,*-16                                                          
         GOTO1 CHOPPER,DMCB,(20,PB1+63),(17,SORTAUD),1                          
         B     BLD9                                                             
* 01JAN91  ****END****                                                          
         SPACE 1                                                                
BLD8     CLI   0(R1),C','                    TAKE OUT COMMAS                    
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,BLD8                                                          
         MVC   PB1+63(20),RCONBPDM                                              
         GOTO1 CHOPPER,DMCB,(20,RCONBPDM),(17,SORTAUD),1                        
         SPACE 1                                                                
         DROP  R4                                                               
         SPACE 1                                                                
BLD9     MVC   SORTMKT(20),RSTAMKT            MARKET                            
         MVC   PB1+84(20),SORTMKT                                               
*                                                                               
         LA    R6,RCONELEM                   HANDLE COMMENTS                    
         LA    R7,PB4                                                           
         LA    R8,SORTCOMM                                                      
         MVI   SORTCOMM,C' '                                                    
         MVC   SORTCOMM+1(139),SORTCOMM                                         
         SPACE 1                                                                
BLD12    ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    BLD14                                                            
         CLI   0(R6),X'11'                                                      
         BNE   BLD12                                                            
         IC    R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   26(0,R7),2(R6)                                                   
         LA    R7,132(R7)                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),2(R6)                                                    
         LA    R8,35(R8)                                                        
         B     BLD12                                                            
         SPACE 1                                                                
BLD14    MVI   SORTCTYP,C'N'       Y=NETWORK CONTRACT                           
         MVI   PB3+50,C'N'                                                      
         CLI   RCONTYPE,C'N'                                                    
         BNE   BLD20                                                            
         MVI   SORTCTYP,C'Y'                                                    
         MVI   PB3+50,C'Y'                                                      
         SPACE 1                                                                
BLD20    GOTO1 =V(SORTER),DMCB,=C'PUT',SRTRC,RR=RELO                            
         MVI   INITSW,C'N'                                                      
         CLI   DEMOSW,C'Y'       DEMO SUMMARY                                   
         BNE   BOPOUT                                                           
         SPACE 1                                                                
         XR    R1,R1                                                            
         IC    R1,RCONWKS                                                       
         LA    R4,RCONELEM                                                      
         XR    R3,R3               GET AIR WEEKS FROM                           
BLAWK    CLI   0(R4),X'10'         BOP ELEMENT                                  
         BNE   BLAWK2                                                           
         USING RCONBPEL,R4                                                      
         CLI   RCONBAWK,X'40'                                                   
         BE    BLAWK3                                                           
         IC    R1,RCONBAWK                                                      
         B     BLAWK3                                                           
         SPACE 1                                                                
BLAWK2   IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     BLAWK                                                            
BLAWK3   CVD   R1,DUB                                                           
         SPACE 1                                                                
         LA    R2,PRTW                                                          
         LA    R3,2                                                             
         LA    R5,PB1+63                                                        
         SPACE 1                                                                
DEMADD   CLC   0(7,R5),SPACES                                                   
         BE    BOPOUT                                                           
         MVC   WORK(7),SPACES                                                   
         LA    R7,WORK                                                          
         SPACE 1                                                                
DEMADD1  MVC   0(1,R7),0(R5)                                                    
         LA    R5,1(R5)                                                         
         LA    R7,1(R7)                                                         
         CLI   0(R5),C' '                                                       
         BNE   DEMADD1                                                          
         SPACE 1                                                                
         ZAP   WORK+7(4),=P'0'                                                  
         ZAP   WORK+11(4),=P'0'                                                 
         L     R7,ADEMTAB                                                       
         L     R4,0(R7)            COUNT                                        
         L     R8,4(R7)            MAX                                          
         LA    R7,8(R7)            TABLE                                        
         SPACE 1                                                                
         GOTO1 ABINSRCH,DMCB,(1,WORK),(R7),(R4),15,(0,7),(R8)                   
         SH    R7,=H'8'                                                         
         MVC   0(4,R7),DMCB+8      COUNT                                        
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         L     R7,DMCB                                                          
         LA    R7,7(R7)            FIRST DEMO                                   
         CH    R3,=H'2'                                                         
         BE    *+8                 YES                                          
         LA    R7,4(R7)            NO, MUST BE SECOND                           
         AP    0(4,R7),DUB                                                      
         LA    R5,1(R5)                                                         
         BCT   R3,DEMADD           DO NEXT                                      
         B     BOPOUT                                                           
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
PRTBOP   NTR1                                                                   
         BAS   RE,RPORT                                                         
         LA    R2,PRTW                                                          
         AH    R2,=Y(LASTPLIN)     COUNT LINES TO BE PRINTED                    
         LA    R3,LINES                                                         
PRTB1    CLC   0(132,R2),SPACES                                                 
         BNE   PRTB2                                                            
         SH    R2,=H'132'                                                       
         BCT   R3,PRTB1                                                         
         SPACE 1                                                                
PRTB2    SR    R1,R1                                                            
         IC    R1,LINE                                                          
         LA    R1,1(R3,R1)                                                      
         STC   R1,WORK                                                          
         CLC   WORK(1),MAXLINES                                                 
         BL    PRTB2A                                                           
         MVI   FORCEHED,C'Y'       GET IT ALL ON 1 PAGE                         
         BAS   RE,RPORT                                                         
         SPACE 1                                                                
PRTB2A   LA    R4,P                                                             
         LA    R2,PRTW                                                          
PRTB3    LA    R5,4                DO 4 AT A TIME                               
PRTB4    MVC   0(132,R4),0(R2)                                                  
         CLC   0(132,R4),SPACES                                                 
         BNE   *+8                                                              
         MVI   0(R4),0                                                          
         LA    R4,132(R4)                                                       
         LA    R2,132(R2)                                                       
         BCT   R3,PRTB5            REDUCE NUMBER TO PRINT                       
         MVI   SPACING,1           ALL DONE                                     
         BAS   RE,RPORT                                                         
         B     BOPOUT                                                           
         SPACE 1                                                                
PRTB5    BCT   R5,PRTB4                                                         
         BAS   RE,RPORT            PRINT THE FOUR                               
         LA    R4,P                                                             
         B     PRTB3               GO BACK FOR MORE                             
         SPACE 2                                                                
RPORT    NTR1                                                                   
* SPECIAL FIX FOR BLAIR RADIO -PUT REP SPEC BACK IN 01 WHEN DONE                
         MVC   HEAD3(14),=C'REPRESENTATIVE'                                     
         CLC   QREP,=C'BL'                                                      
         BNE   *+10                                                             
         MVC   HEAD3+15(33),=CL33'BANNER RADIO'                                 
*                                                                               
         CLC   QREP,=C'TO'                                                      
         BNE   *+10                                                             
         MVC   HEAD3+15(33),=CL33'TORBET RADIO'                                 
*                                                                               
         CLC   QREP,=C'JB'                                                      
         BNE   *+10                                                             
         MVC   HEAD3+15(33),=CL33'SELECT RADIO'                                 
*                                                                               
         MVC   HEAD3+108(25),DTLN                                               
         CLC   QPROG,=C'86'                                                     
         BE    *+16                                                             
         MVC   HEAD4+108(9),=C'REQUESTOR'                                       
         MVC   HEAD4+118(12),QUESTOR                                            
         GOTO1 REPORT                                                           
BOPOUT   XIT1                                                                   
         EJECT                                                                  
RELO     DS    A                                                                
ABINSRCH DS    A                                                                
ADEMTAB  DS    A                                                                
VDEMOCON DS    A                   * NEW CODE FOR DEMOCON FIX                   
CNTL     DS    CL11                4 ADV, 3 DATE, 4 REFERENCE                   
DTES     DS    CL6                                                              
DTLN     DS    CL25                                                             
INITSW   DC    C'N'                                                             
DEMOSW   DS    CL1                                                              
SVMKT    DS    CL20                LAST MARKET                                  
         SPACE 2                                                                
SRTRC    DS    0C                  SORTRECORD FOR MARKET RECAP                  
SORTMKT  DS    CL20                                                             
SORTADV  DS    CL20                                                             
SORTAGY  DS    CL20                                                             
SORTPRD  DS    CL20                                                             
SORTOFF  DS    CL02                                                             
SORTSAL  DS    CL20                                                             
SORTSCED DS    CL08                                                             
SORTWKS  DS    CL02                                                             
SORTTIME DS    CL20                                                             
SORTRAT  DS    CL09                                                             
SORTAUD  DS    CL17                                                             
SORTOBJ  DS    CL20                                                             
SORTMER  DS    CL01                                                             
SORTCTYP DS    CL01                                                             
SORTORD  DS    CL01                                                             
SORTBKS  DS    CL06                                                             
SORTCOMM DS    CL140                                                            
         DS    CL8                                                              
         SPACE 2                                                                
SRTFLD   DC    CL80'SORT FIELDS=(1,80,A),FORMAT=BI,WORK=1'                      
RECTYP   DC    CL80'RECORD TYPE=F,LENGTH=(335)'                                 
         SPACE 1                                                                
RATTAB   DS    0H                                                               
         DC    CL18'AARBARB'                                                    
         DC    CL18'BBIRBIRCH'                                                  
         DC    CL18'BBURBURKE'   *** NOT USED?***                               
         DC    CL18'MMEDMEDIA TRENDS'                                           
         DC    CL18'NNSINSI'                                                    
         DC    CL18'RRAMRAM'                                                    
         DC    CL18'TTR7TRAC-7'                                                 
         DC    X'FF'                                                            
         DC    CL17' '                                                          
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
       ++INCLUDE DEDBLOCK                                                       
         SPACE 1                                                                
PRTW     DS    0D                                                               
PB1      DS    CL132                                                            
PB2      DS    CL132                                                            
PB3      DS    CL132                                                            
PB4      DS    CL132                                                            
         DS    32CL132                                                          
LASTPLIN EQU   (*-PRTW)-132        DISPLACEMENT TO LAST LINE                    
LINES    EQU   (*-PRTW)/132        NUMBER OF PRINT LINES IN BLOCK               
         SPACE 1                                                                
DEMTAB   DS    0D                                                               
         DS    F                   NUMBER IN TABLE                              
         DC    F'200'              MAX                                          
         DS    200CL15             7 DEMO/4P FIRST/4P SECOND/                   
         SPACE 1                                                                
SORTC    DS    0D                                                               
         DS    41000C                                                           
         PRINT OFF                                                              
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086REREP8502 04/22/15'                                      
         END                                                                    
